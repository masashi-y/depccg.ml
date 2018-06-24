
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

    val show_derivation : Tree.t -> string

    val show_html_trees : Tree.scored list -> Xml.xml

    val show_html_trees_separated : string option list -> Tree.scored list -> unit

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

    let show_derivation tree =
        let open String in
        let space n = make n ' ' in
        let rec terminal_string = function
            | [] -> ("", "")
            | (c, w) :: rest
                -> let nextlen = 2 + max (length c) (length w) in
                   let lcatlen = (nextlen - length c) / 2 in
                   let rcatlen = lcatlen + (nextlen - length c) mod 2 in
                   let lwordlen = (nextlen - length w) / 2 in
                   let rwordlen = lwordlen + (nextlen - length w) mod 2 in
                   let (cs, ws) = terminal_string rest in
                   (space lcatlen ^ c ^ space rcatlen ^ cs,
                       space lwordlen ^ w ^ space rwordlen ^ ws)
        in let buf = Buffer.create 100 in
        let rec show lwidth = function
            | {cat; str; children=[]}
                -> max lwidth (2 + lwidth + max (length (Cat.show cat)) (length str))
            | {cat; op; children}
                -> let rwidth = ListLabels.fold_left children ~init:lwidth
                        ~f:(fun lw child -> let rw = show lw child in max lw rw) in
                   let pad = max 0 ((rwidth - lwidth - (length (Cat.show cat))) / 2 + lwidth) in
                   Printf.bprintf buf "%s%s%s\n%s%s\n" (space lwidth) (make (rwidth - lwidth) '-')
                           (Rules.show op) (space pad) (Cat.show cat);
                   rwidth in
        let ws = terminals tree in
        let cs = List.map Cat.show (preterminals tree) in
        let (cs', ws') = terminal_string (List.combine cs ws) in
        Printf.bprintf buf "%s\n" ws';
        Printf.bprintf buf "%s\n" cs';
        let _ = show 0 tree in
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


    let show_html_trees_separated names tss =
        let dir = !% "results_%s" (current_time_string ()) in
        let rec new_fname i n res =
            if Sys.file_exists (dir </> res) then
            new_fname (i+1) n (!%"%s.%i.html" n i)
            else res in
        let f i (name, ts) =
            let res = show_html_trees [ts] in
            let hd = snd (List.hd ts) in
            let sent = String.concat " " (Tree.terminals hd) in
            let fname = match name with
                | None -> !%"%d.html" i
                | Some n -> new_fname 1 n (!%"%s.html" n) in
            write_file (dir </> fname) (Xml.to_string_fmt res);
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
        | "deriv" -> List.iteri (f show_derivation) res
        | "ptb"   -> List.iteri (f (show_ptb 0)) res
        | "html"  -> pr (Xml.to_string_fmt (show_html_trees res))
        | "htmls" -> show_html_trees_separated names res
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

        let escape_str s =
            let open Buffer in
            let buf = create 10 in
            let f = function
                | '\'' -> add_string buf "\\'"
                | c -> add_char buf c
        in String.iter f s;
        contents buf

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

        let show attr tree = 
            let open Attributes in
            let rec f = AttributeM.(function
                | {cat; str; children=[]} -> 
                    popi () >>= fun (start, att) ->
                    return @@ Xml.Element ("lf", [
                        ("start", string_of_int start);
                        ("word",  str);
                        ("lemma", Attribute.lemma ~def:"X" att);
                        ("pos",   Attribute.pos ~def:"X" att);
                        ("entity", Attribute.entity ~def:"X" att);
                        ("cat", Cat.show cat)], [])
                | {cat; op; children} as tree -> 
                    mapM f children >>= fun children ->
                    return @@ Xml.Element ("rule", 
                        [("type", show_xml_rule_type tree op);
                         ("cat", Cat.show cat)], children)
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
        | "deriv" -> List.iteri (f show_derivation) res
        | "ptb"   -> List.iteri (f (show_ptb 0)) res
        | "html"  -> pr (Xml.to_string_fmt (show_html_trees res))
        | "htmls" -> show_html_trees_separated names res
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

