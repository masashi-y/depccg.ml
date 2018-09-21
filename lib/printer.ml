
open Grammar
open Utils
open Attributes

module StateM =
struct
    include StateM
    let pop' () = StateM.(get >>= function
        | _, [] -> invalid_arg "pop'"
        | i, (x :: xs) ->
                put (i+1, xs) >>= fun () ->
                return (i, x))
end

module type PRINTER =
   functor (Grammar : GRAMMAR) ->
sig
    open Grammar
    val show_tree : Attributes.t -> Tree.t -> string

    val show_conll_like : Attributes.t -> Tree.t -> string

    val show_ptb : int -> Tree.t -> string

    val show_derivation : Attributes.t -> Tree.t -> string

    val show_html_trees : Tree.scored list -> Xml.xml

    val show_html_trees_separated : (Tree.scored -> string * string) ->
        string option list -> Tree.scored list -> unit

    val output_results : string -> string option list -> Tree.scored list -> unit
end

module ParsePrinter (Grammar : GRAMMAR) =
struct

    open Grammar
    open Tree

    let show_tree attrs tree =
        let rec aux = AttributeM.(function
            | {cat; str; children=[]} ->
               pop () >>= fun attr ->
               let cat = Cat.show cat in
               let pos = Attribute.pos ~def:"POS" attr in
               return (!%"(<L %s %s %s %s %s>)" cat pos pos str cat)
            | {cat; children} ->
                   mapM aux children >>= fun cs ->
                   let w = String.concat " " cs in
                   let n_child = List.length children in
                   let head_is_left = 0 in     (* do not care *)
                   return (!%"(<T %s %i %i> %s )" (Cat.show cat) head_is_left n_child w)) in
        AttributeM.eval (aux tree) attrs

    let show_conll_like attrs tree =
        let rec aux = AttributeM.(function
            | {cat; str; children=[]} ->
                   popi () >>= fun (i, attr) ->
                   let cat = Cat.show cat in
                   let pos = Attribute.pos ~def:"POS" attr in
                   return (!%"%i\t%s\t_\t%s\t%s\t_\t_\t_\t_\t%s" i str pos pos cat)
            | {cat; children} ->
                   mapM aux children >>= fun cs ->
                   return (String.concat "\n" cs)) in
        (AttributeM.eval (aux tree) (1, attrs)) ^ "\n"

    let rec show_ptb depth = function
        | {cat; str; children=[]} -> !%"(%s %s)" (Cat.show cat) str
        | {cat; children} ->
               let w = String.concat " " (List.map (show_ptb (depth+1)) children) in
               !%"(%s %s)" (Cat.show cat) w

    let cat_with_polarity cat =
        AttributeM.(popn () >>= fun attr ->
        Cat.show cat |> fun cat ->
        return (match NodeAttribute.polarity_opt attr with
            | Some p -> cat ^ " " ^ p
            | None -> cat))

    let show_derivation attrs tree =
        let open String in
        let space n = make n ' ' in
        let rec terminal_string = AttributeM.(function
            | {cat; str; children=[]} ->
                cat_with_polarity cat >>= fun cat ->
                let nextlen = 2 + max (length cat) (length str) in
                let lcatlen = (nextlen - length cat) / 2 in
                let rcatlen = lcatlen + (nextlen - length cat) mod 2 in
                let lwordlen = (nextlen - length str) / 2 in
                let rwordlen = lwordlen + (nextlen - length str) mod 2 in
                return (space lcatlen ^ cat ^ space rcatlen,
                        space lwordlen ^ str ^ space rwordlen)
            | {children} ->
                popn () >>= fun _ -> (* discard here *)
                mapM terminal_string children >>= fun children ->
                    let cs, ws = List.split children in
                    return (String.concat "" cs,
                            String.concat "" ws)) in
        let buf = Buffer.create 100 in
        let rec show lwidth = AttributeM.(function
            | {cat; str; children=[]} ->
                cat_with_polarity cat >>= fun cat ->
                return @@ max lwidth (2 + lwidth + max (length cat) (length str))
            | {cat; op; children} ->
                cat_with_polarity cat >>= fun cat ->
                fold_leftM (fun lw child ->
                    show lw child >>= fun rw ->
                    return @@ max lw rw)
                lwidth children >>= fun rwidth ->
                let pad = max 0 ((rwidth - lwidth - (length cat)) / 2 + lwidth) in
                Printf.bprintf buf "%s%s%s\n%s%s\n" (space lwidth) (make (rwidth - lwidth) '-')
                       (Rules.show op) (space pad) cat;
               return rwidth
        ) in
        let (cs, ws) = AttributeM.eval (terminal_string tree) attrs in
        Printf.bprintf buf "%s\n" ws;
        Printf.bprintf buf "%s\n" cs;
        ignore @@ AttributeM.eval (show 0 tree) attrs;
        Buffer.contents buf

    let show_html =
        let cstr cat =
            Xml.Element ("mi", [
                ("mathvariant", "italic");
                ("mathsize", "1.0");
                ("mathcolor", "Red")], [ PCData cat ])

        and fstr cat_elem feat = 
            Xml.(Element ("msub", [], [
                cat_elem;
                Element ("mrow", [], [
                    Element ("mi", [
                        ("mathvariant", "italic");
                        ("mathsize", "0.8");
                        ("mathcolor", "Purple")],
                            [ PCData feat ]) ]) ]))

        in
        let add_feat c f =
            let res = cstr c in
             match Feature.show f with
            | "" -> res
            | s  -> fstr res s in

        let rec show_html_cat = function
            | `S f  -> [add_feat "S" f]
            | `N f  -> [add_feat "N" f]
            | `NP f -> [add_feat "NP" f]
            | `PP f -> [add_feat "PP" f]
            | `Punct s -> [cstr s]
            | `Fwd (x, y) ->
                (cstr "(" :: show_html_cat x) @ (cstr "/" :: show_html_cat y) @ [cstr ")"]
            | `Bwd (x, y) ->
                (cstr "(" :: show_html_cat x) @ (cstr "\\" :: show_html_cat y) @ [cstr ")"]
            | `Either (x, y) ->
                (cstr "(" :: show_html_cat x) @ (cstr "|" :: show_html_cat y) @ [cstr ")"]

        in
        let rec show_html_tree = function
        | {cat; str; children=[]} ->
            Xml.(Element ("mrow", [], [
                Element ("mfrac", [("linethickness", "2px")], [
                    Element ("mtext",  [("mathsize", "1.0"); ("mathcolor", "Black")], [PCData str]);
                    Element ("mstyle", [("mathcolor", "Red")], show_html_cat cat)
                ]);
                Element ("mtext", [("mathsize", "0.8"); ("mathcolor", "Black")], [PCData "lex"]) ]))
        | {cat; op; children=cs} ->
            Xml.(Element ("mrow", [], [
                Element ("mfrac", [("linethickness", "2px")], [
                    Element ("mrow", [], List.map show_html_tree cs);
                    Element ("mstyle", [("mathcolor", "Red")], show_html_cat cat)
                ]);
                Element ("mtext", [("mathsize", "0.8"); ("mathcolor", "Black")], [PCData (Rules.show op)])
            ]))
        in show_html_tree

    let show_html_trees tss =
        let f i ts =
            let _, t = List.hd ts in
            let raw_sent = String.concat " " (Tree.terminals t) in
            Xml.(Element ("p", [], [PCData (!%"ID=%d: %s" i raw_sent)]) ::
            CCList.flat_map (fun (p, t) -> [
                    Element ("p", [], [PCData (!%"Log prob=%f" p)]);
                    Element ("math", [("xmlns", "http://www.w3.org/1998/Math/MathML")], [show_html t])
                ]) ts) in
        Xml.(Element ("html", [("lang", "en")], [
            Element ("head", [], [
                Element ("meta", [("charset", "UTF-8")], []);
                Element ("style", [], [ PCData "body { font-size: 1em; }" ]);
                Element ("script", [
                    ("type", "text/javascript");
                    ("src", "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")], [ PCData "" ])
            ]);
            Element ("body", [], List.(flatten @@ mapi f tss))
        ]))


    let show_html_trees_separated show_fun names tss =
        let dir = !% "results_%s" (current_time_string ()) in
        let rec new_fname suffix i n res =
            if Sys.file_exists (dir </> res) then
            new_fname suffix (i+1) n (!%"%s.%i.%s" n i suffix)
            else res in
        let f i (name, ts) =
            let res, suffix = show_fun ts in
            let hd = snd (List.hd ts) in
            let sent = String.concat " " (Tree.terminals hd) in
            let fname = match name with
                | None -> !%"%d.%s" i suffix
                | Some n -> new_fname suffix 1 n (!%"%s.%s" n suffix) in
            write_file (dir </> fname) res;
            fname, sent in
        let () = Unix.mkdir dir 0o744 in
        let filenames = List.mapi f (List.combine names tss) in
        write_file (dir </> "index.html")
            Xml.(to_string_fmt (Element ("html", [("lang", "en")], [
                Element ("head", [], [
                    Element ("meta", [("charset", "UTF-8")], []);
                    Element ("style", [], [ PCData "body { font-size: 1em; }" ]);
                    Element ("script", [
                        ("type", "text/javascript");
                        ("src", "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")], [ PCData "" ])
                    ]);
                Element ("body", [], [
                    Element ("table", [("border", "1")],
                        Element ("tr", [], [
                            Element ("td", [], [ PCData "id" ]);
                            Element ("td", [], [ PCData "file" ]);
                            Element ("td", [], [ PCData "sentence" ]);
                        ]) ::
                        List.mapi (fun i (fname, sent) ->
                            Element ("tr", [], [
                                Element ("td", [], [ PCData (string_of_int i) ]);
                                Element ("td", [], [
                                    Element ("a", [("href", fname)], [ PCData fname ])
                                ]);
                                Element ("td", [], [ PCData sent ]);
                            ])
                        ) filenames
                    )
                ])
            ])));
        Printf.eprintf "write results to directory: %s\n" dir

    module Svg : sig
        val pp : Format.formatter -> Tree.t -> unit

        val show : Tree.t -> string

        val pp_scored : Format.formatter -> Tree.scored -> unit

        val show_scored : Tree.scored -> string

        val run_graphviz : Tree.scored -> string * string
    end = struct
        let escape_str s =
            let open Buffer in
            let buf = create 10 in
            let f = function
                | '\\' -> add_string buf "\\\\"
                | c -> add_char buf c in
            String.iter f s;
            contents buf

        let show_cat out (id, cat) =
            Format.fprintf out
"e%d [fontname=\"Helvetica,sans-Serif\", fontsize=12, shape = plain, label = \"%s\"]\n"
            id (escape_str @@ Cat.show cat)

        let show_word out (id, word) =
            Format.fprintf out
"e%d [margin=0.001, height=0.2, color = \"#d3d3d3\", fillcolor = \"#d3d3d3\", shape = rect, style=\"filled\", label = \"%s\"]\n"
            id word

        let header =
"digraph G { 
rankdir = TB;
splines = polyline;
subgraph {
    edge [
        dir = none;
        sametail=h1
    ]\n"

        let footer out same_rank =
            let same_rank = String.concat "; " same_rank in
            Format.fprintf out
"{rank = same; %s;}
    }
}" same_rank

        let incr = StateM.(
            get >>= fun i ->
            put (i + 1) >>= fun () ->
            return i)

        let pp out tree =
            let rec aux parent = StateM.(function
                | {cat; str; children=[]} ->
                    incr >>= fun cat_id ->
                    incr >>= fun word_id ->
                    Format.fprintf out
                    "e%d -> e%d\ne%d -> e%d\n%a\n%a\n"
                        parent cat_id cat_id word_id
                        show_cat (cat_id, cat) show_word (word_id, str);
                    return ["e" ^ string_of_int word_id]
                | {cat; children} ->
                    incr >>= fun cat_id ->
                    Format.fprintf out "e%d -> e%d\n%a\n"
                        parent cat_id show_cat (cat_id, cat);
                    mapM (aux cat_id) children >>= fun word_ids ->
                    return (List.flatten word_ids)
            ) in
            Format.pp_print_string out header;
            Format.pp_print_string out
            "e0 [fontname=\"Helvetica,sans-Serif\", fontsize=12, shape = plain, label = \"root\"]\n";
            footer out (StateM.eval (aux 0 tree) 1)

        let show tree = Format.asprintf "%a" pp tree

        let pp_scored out = function
            | (_, tree) :: _ -> pp out tree
            | _ -> invalid_arg "failed in Svg.pp_scored"

        let show_scored tree = Format.asprintf "%a" pp_scored tree

        let warned = ref false

        let run_graphviz tree =
            let dot = show_scored tree in
            try
                Shexp_process.(eval Infix.(echo dot |- run "dot" ["-Tsvg"] |- read_all)), "svg"
            with Failure log ->
                if not !warned then begin
                    prerr_endline ("[parser] error during running dot: " ^ log);
                    prerr_endline "[parser] Saving dot files instead.";
                    warned := true;
                end;
                dot, "dot"
    end

    let output_results fmt names res =
        let f printfun i =
            List.iter (fun (_, t) -> p "ID=%i\n%s\n" i (printfun t)) in
        let g printfun i = function
            | [] -> failwith "invalid argument in output_results"
            | ((_, t) :: _) as lst ->
                    let attrs = Attributes.default () in
                    List.iter (fun (_, t) -> p "ID=%i\n%s\n" i (printfun attrs t)) lst in
        match fmt with
        | "auto"  -> List.iteri (g show_tree) res
        | "conll" -> List.iteri (g show_conll_like) res
        | "deriv" -> List.iteri (f (show_derivation (Attributes.default ()))) res
        | "ptb"   -> List.iteri (f (show_ptb 0)) res
        | "html"  -> pr (Xml.to_string_fmt (show_html_trees res))
        | "htmls" -> show_html_trees_separated
                    (fun ts -> (Xml.to_string_fmt @@ show_html_trees [ts], "html")) names res
        | "svg"   -> show_html_trees_separated (fun ts -> Svg.run_graphviz ts) names res
        | _ -> invalid_arg (!%"Not accepted output format: %s\n" fmt)
end

module EnglishPrinter = struct
    include ParsePrinter (EnglishGrammar)
    open EnglishGrammar
    open Tree
    open Rules
    open Cat
    open Notat

    module Prolog : sig
        val show : int -> Attributes.t -> Tree.t -> string
    end = struct
        let escape_str s =
            let open Buffer in
            let buf = create 10 in
            let f = function
                | '\'' -> add_string buf "\\'"
                | c -> add_char buf c in
            String.iter f s;
            contents buf

        let rec remove_noisy_rules ({op; cat; str; children} as self) = 
            let children = List.map remove_noisy_rules children in
            match children with [{cat=lcat}; {cat=rcat}] -> begin
                match op with
                | `CommaVPtoADV | `ParentDirect | `RP when is_punct lcat -> 
                        let child = Tree.make ~cat:rcat ~op:`RP ~children in
                        Tree.make ~cat ~op:`Unary ~children:[child]
                | `Conj when lcat =:= (!:"conj") (* conj2 *)
                          && rcat =:= (np |: np)
                          &&  cat =:= np ->
                            let cat' = rcat |: rcat in
                            let child = Tree.make ~cat:cat' ~op:`Conj ~children in
                            Tree.make ~cat ~op:`Unary ~children:[child]
                | _ -> Tree.make ~cat ~op ~children
            end
            | [_] -> Tree.make ~cat ~op ~children
            | []  -> self
            | _ -> invalid_arg (!%"Unexpected number of children: %d\n" (List.length children))

        let show_cat =
            let show_atom c f = match Feature.show f with
                | "" -> c
                | s  -> let s' = String.sub s 1 (String.length s - 2)
                        in !%"%s:%s" c s'
            in
            let rec f = function
            | `S f -> show_atom "s" f
            | `N f -> show_atom "n" f
            | `NP f -> show_atom "np" f
            | `PP f -> show_atom "pp" f
            | `Fwd (x, y) -> !%"(%s/%s)" (f x) (f y)
            | `Bwd (x, y) -> !%"(%s\\%s)" (f x) (f y)
            | `Either (x, y) -> !%"(%s|%s)" (f x) (f y)
            | `Punct s -> begin match s with
                | "." -> "period"
                | "," -> "comma"
                | ":" -> "colon"
                | ";" -> "semicolon"
                | s -> String.lowercase_ascii s
            end
            in f

        let show_rule = function
            | `FwdApp    -> "fa"
            | `BwdApp    -> "ba"
            | `FwdCmp    -> "fc"
            | `BwdCmp    -> "bxc"
            | `GenFwdCmp -> "gfc"
            | `GenBwdCmp -> "gbx"
            | `Conj      -> "conj"
            | `RP        -> "rp"
            | _  -> invalid_arg "Not supported combinatory rule"

        let get_arg = function
            | `Fwd (_, y)
            | `Bwd (_, y) -> y
            | _ -> invalid_arg "failed in get_arg"

        let show i attribs tree =
            let open Attributes in
            let mk_indent depth = String.make depth ' ' in
            let rec f depth = 
                let indent = mk_indent depth in
                let f' c = f (depth + 1) c in AttributeM.(function
                | {cat; str; children=[]} -> do_;
                    att <-- pop ();
                    let lemma = Attribute.lemma ~def:"X" att in
                    let pos = Attribute.pos ~def:"X" att in
                    let entity = Attribute.entity ~def:"X" att in
                    let chunk = Attribute.chunk ~def:"X" att in
                    return (!% "%st(%s, '%s', '%s', '%s', '%s' '%s')"
                               indent (show_cat cat) (escape_str str)
                               (escape_str lemma) pos chunk entity)
                | {cat; children=[c]} -> do_;
                    child <-- f' c;
                    return (!% "%slx(%s, %s,\n%s)" indent (show_cat cat) (show_cat c.cat) child)
                | {cat; op=`Conj; children=[c1; c2]} -> do_;
                    child1 <-- f' c1;
                    child2 <-- f' c2;
                    return (!% "%sconj(%s, %s,\n%s,\n%s)" indent (show_cat cat)
                                (show_cat @@ get_arg cat) child1 child2)
                | {cat; op; children=[c1; c2]} -> do_;
                    child1 <-- f' c1;
                    child2 <-- f' c2;
                    return (!% "%s%s(%s,\n%s,\n%s)" indent (show_rule op)
                                (show_cat cat) child1 child2)
                | _ -> invalid_arg "failed in Prolog.show"
            ) in
            !%"ccg(%d,\n%s).\n" i (AttributeM.eval (f 1 @@ remove_noisy_rules tree) attribs)
    end

    module XML : sig
        (* TODO *)
        val show : Attributes.t -> Tree.t -> Xml.xml
    end = struct
        let show_xml_rule_type tree op =
            match Tree.match_with_type_raised tree with
            | Some _ -> "tr"
            | None -> match tree with
            | {children=[_]} -> "lex"
            | {children=[{cat};_]} -> begin match op with
                | `FwdApp       -> "fa"
                | `BwdApp       -> "ba"
                | `FwdCmp       -> "fc"
                | `BwdCmp       -> "bx"
                | `GenFwdCmp    -> "gfc"
                | `GenBwdCmp    -> "gbx"
                | `Conj         -> "conj"
                | `RP when is_punct cat -> "lp"
                | `RP           -> "rp"
                | `CommaVPtoADV -> "lp"
                | `ParentDirect -> "lp"
                | _ -> "other"
            end
            | _ -> invalid_arg (!%"unexpected rule in show_xml_rule_type: %s\n" (Rules.show op))

        let pop_polarity = AttributeM.(
            popni ~incr:false () >>= fun (_, attr) ->
            return (match NodeAttribute.polarity_opt attr with
                | Some v -> [("mono", v)]
                | None -> [])
        )

        let show attr tree = 
            let open Attributes in
            let rec f = AttributeM.(function
                | {cat; str; children=[]} -> 
                    popi () >>= fun (start, attr) ->
                    pop_polarity >>= fun mono ->
                    let fields = [
                        ("start", string_of_int start);
                        ("word",  str);
                        ("lemma", Attribute.lemma ~def:"X" attr);
                        ("pos",   Attribute.pos ~def:"X" attr);
                        ("entity", Attribute.entity ~def:"X" attr);
                        ("cat", Cat.show cat) ] in
                    return @@ Xml.Element ("lf", fields @ mono, [])
                | {cat; op; children} as tree -> 
                    mapM f children >>= fun children ->
                    pop_polarity >>= fun mono ->
                    let fields = [
                        ("type", show_xml_rule_type tree op);
                        ("cat", Cat.show cat) ] in
                    return @@ Xml.Element ("rule", fields @ mono, children)
            ) in
            AttributeM.eval (f tree) (0, attr)
    end

        let show_xml_trees attribs tss =
            let f attr = function
                | (_, t) :: _ -> Xml.Element ("ccg", [], [XML.show attr t])
                | _ -> invalid_arg "failed in show_xml_trees"
            in (Xml.to_string_fmt (
                Xml.Element ("candc", [], List.map2 f attribs tss)))

    let show_prolog attribs trees = 
        let warn = let did = ref false in fun () ->
            if not !did then
                prerr_endline "WARNING: only the one best parses are output in prolog format";
                did := true
        in
        pr (":- op(601, xfx, (/)).\n"        ^
            ":- op(601, xfx, (\\)).\n"       ^
            ":- multifile ccg/2, id/2.\n"    ^
            ":- discontiguous ccg/2, id/2.\n");
        let f i = function
            attrib, (_, t) :: rest -> pr (Prolog.show (i + 1) attrib t);
                              if (List.length rest > 0) then warn ()
            | _ -> invalid_arg ""
        in List.iteri f (List.combine attribs trees)

    let output_results fmt names attribs res =
        let f printfun i =
            List.iter (fun (_, t) -> p "ID=%i\n%s\n" i (printfun t)) in
        let g printfun i attrs =
            List.iter (fun (_, t) -> p "ID=%i\n%s\n" i (printfun attrs t)) in
        match fmt with
        | "auto"  -> CCList.iteri2 (g show_tree) attribs res
        | "conll" -> CCList.iteri2 (g show_conll_like) attribs res
        | "deriv" -> CCList.iteri2 (g show_derivation) attribs res
        | "ptb"   -> List.iteri (f (show_ptb 0)) res
        | "html"  -> pr (Xml.to_string_fmt (show_html_trees res))
        | "htmls" -> show_html_trees_separated
                    (fun ts -> (Xml.to_string_fmt @@ show_html_trees [ts], "html")) names res
        | "svg"   -> show_html_trees_separated (fun ts -> Svg.run_graphviz ts) names res
        | "xml"  -> pr (show_xml_trees attribs res)
        | "prolog" -> show_prolog attribs res
        | _ -> invalid_arg (!%"Not accepted output format: %s\n" fmt)
end

let sample_tree =
    let open EnglishGrammar in
    let open Notat in
    Tree.of_view
        (N (lex, s, [
            T (np, "Hanako");
            N (lex, (s |: np), [
                T ((s |: np) /: np, "beats");
                T              (np, "Taro" )
            ])
        ]))

