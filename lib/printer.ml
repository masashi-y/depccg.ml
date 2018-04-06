
open Grammar
open Utils

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
    val show_tree : Tree.t -> string

    val show_ptb : int -> Tree.t -> string

    val show_derivation : Tree.t -> string

    val show_html_trees : Tree.scored list -> string

    val show_html_trees_separated : string option list -> Tree.scored list -> unit

    val sample_tree : Tree.t

    val output_results : string -> string option list -> Tree.scored list -> unit
end

module ParsePrinter (Grammar : GRAMMAR) =
struct

    open Grammar
    open Tree

    let rec show_tree = function
        | {cat; str; children=[]}
            -> let c' = Cat.show cat in
               !%"(<L %s POS POS %s %s>)" c' str c'
        | {cat; children}
            -> let w = String.concat " " (List.map show_tree children) in
               let n_child = List.length children in
               let head_is_left = 0 in     (* do not care *)
               !%"(<T %s %i %i> %s )" (Cat.show cat) head_is_left n_child w

    let is_terminal = function
        | {children=[]} -> true
        | _ -> false

    let rec show_ptb depth = function
        | {cat; str; children=[]}
            -> !%"(%s %s)" (Cat.show cat) str
        | {cat; children}
            -> let w = String.concat " " (List.map (show_ptb (depth+1)) children) in
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
                   rwidth
        in
        let ws = terminals tree in
        let cs = List.map Cat.show (preterminals tree) in
        let (cs', ws') = terminal_string (List.combine cs ws) in
        Printf.bprintf buf "%s\n" ws';
        Printf.bprintf buf "%s\n" cs';
        let _ = show 0 tree in
        Buffer.contents buf

    let show_html =
        let cstr = format_of_string "<mi mathvariant='italic'
          mathsize='1.0' mathcolor='Red'>%s</mi>"
        and fstr = format_of_string "<msub>%s<mrow>
    <mi mathvariant='italic'
        mathsize='0.8' mathcolor='Purple'>%s</mi>
          </mrow></msub>"
        in
        let add_feat c f = let res = !% cstr c in
                           match Feature.show f with
                               | "" -> res
                               | s -> !% fstr res s
        in
        let rec show_html_cat = function
            | `S f  -> add_feat "S" f
            | `N f  -> add_feat "N" f
            | `NP f -> add_feat "NP" f
            | `PP f -> add_feat "PP" f
            | `Punct s -> !% cstr s
            | `Fwd (x, y) -> (!% cstr "(") ^ (show_html_cat x) ^
                             (!% cstr "/") ^ (show_html_cat y) ^ (!% cstr ")")
            | `Bwd (x, y) -> (!% cstr "(") ^ (show_html_cat x) ^
                             (!% cstr "\\") ^ (show_html_cat y) ^ (!% cstr ")")

        in
        let rec show_html_tree = function
        | {cat; str; children=[]} ->
            (!%"<mrow>
  <mfrac linethickness='2px'>
    <mtext mathsize='1.0' mathcolor='Black'>%s</mtext>
    <mstyle mathcolor='Red'>%s</mstyle>
  </mfrac>
  <mtext mathsize='0.8' mathcolor='Black'>lex</mtext>
</mrow>" str (show_html_cat cat))
        | {cat; op; children} ->
            (!%"<mrow>
  <mfrac linethickness='2px'>
    <mrow>%s</mrow>
    <mstyle mathcolor='Red'>%s</mstyle>
  </mfrac>
  <mtext mathsize='0.8' mathcolor='Black'>%s</mtext>
</mrow>" (String.concat "" @@ List.map show_html_tree children)
         (show_html_cat cat)
         (escape_html_simple @@ Rules.show op))
        in show_html_tree

    let show_html_trees tss =
        let f i ts =
            let _, t = List.hd ts in
            let show = String.concat "" @@ List.map (fun (p, t) ->
                !%"<p>Log prob=%f</p>
    <math xmlns='http://www.w3.org/1998/Math/MathML'>%s</math>"
     p (show_html t))
                ts in
            (!% "<p>ID=%d: %s</p>%s"
                i (String.concat " " @@ terminals t) show)
        in (!%"<!doctype html>
<html lang='en'>
<head>
  <meta charset='UTF-8'>
  <style>
    body {
      font-size: 1em;
    }
  </style>
  <script type=\"text/javascript\"
     src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\">
  </script>
</head>
<body>%s</body></html>" (String.concat "" @@ List.mapi f tss))

    let show_html_trees_separated names tss =
        let dir = !% "results_%s" (current_time_string ()) in
        let rec new_fname i n res =
            if Sys.file_exists (dir </> res) then
            new_fname (i+1) n (!%"%s.%i.html" n i)
            else res in
        let f i (name, ts) =
            let res = show_html_trees [ts] in
            let fname = match name with
                None -> !%"%d.html" i
                | Some n -> new_fname 1 n (!%"%s.html" n) in
            write_file (dir </> fname) res;
            fname in
        let () = Unix.mkdir dir 0o744 in
        let filenames = List.mapi f (List.combine names tss) in
        write_file (dir </> "index.html")
(!%"<!doctype html>
<html lang='en'>
<head>
  <meta charset='UTF-8'>
  <title>results</title>
  <style>
    body {
      font-size: 1.5em;
    }
  </style>
</head>
<body>
<table border='1'>
<tr>
  <td>id</td>
  <td>tree</td>
</tr>%s
</table>
</body>
</html>" (String.concat "" @@ List.mapi (fun i fname ->
    !% "
<tr>
  <td>%d</td>
  <td><a href=\"%s\">%s</a></td>
</tr>" i fname fname) filenames));
  Printf.eprintf "write results to directory: %s\n" dir

    let sample_tree = let open Cat in
                make ~cat:s ~op:Rules.intro ~children:
                [terminal np "Hanako";
                make ~cat:(s |: np) ~op:Rules.intro ~children:
                         [terminal ((s |: np) /: np) "beats";
                          terminal np                "Taro"
                         ]
                ]

    let output_results fmt names res =
        let f printfun i = function
            | [] -> ()
            | lst -> List.iter (fun (_, t) ->
                     p "ID=%i\n%s\n" i (printfun t)) lst
        in match fmt with
        | "auto"  -> List.iteri (f show_tree) res
        | "deriv" -> List.iteri (f show_derivation) res
        | "ptb"   -> List.iteri (f (show_ptb 0)) res
        | "html"  -> pr (show_html_trees res)
        | "htmls" -> show_html_trees_separated names res
        | _ -> invalid_arg (!%"Not accepted output format: %s\n" fmt)

end

module EnglishPrinter = struct
    include ParsePrinter (EnglishGrammar)
    open EnglishGrammar
    open Tree
    open Rules
    open Cat

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
                let f' c = f (depth + 1) c in StateM.(function
                | {cat; str; children=[]} -> do_;
                    att <-- pop ();
                    let lemma = Option.get "X" (Attribute.lemma att) in
                    let pos = Option.get "X" (Attribute.pos att) in
                    let entity = Option.get "X" (Attribute.entity att) in
                    let chunk = Option.get "X" (Attribute.chunk att) in
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
            !%"ccg(%d,\n%s).\n" i (StateM.eval (f 1 @@ remove_noisy_rules tree) attribs)
    end

    module XML : sig
        (* TODO *)
        val show : Attributes.t -> Tree.t -> string
    end = struct
        let show_xml_rule_type tree op = match tree with
            | {cat; children=[{cat=cat'}]}
                when Cat.is_type_raised cat &&
                (cat' =:= Cat.np || cat' =:= Cat.pp)
                -> "tr"
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
            let rec f tree = StateM.(match tree with
                | {cat; str; children=[]} -> pop' () >>= fun (start, att) ->
                        let lemma = Option.get "X" (Attribute.lemma att) in
                        let pos = Option.get "X" (Attribute.pos att) in
                        let entity = Option.get "X" (Attribute.entity att) in
                        return (!%
                "<lf start=\"%d\" span=\"1\" word=\"%s\" lemma=\"%s\" pos=\"%s\" entity=\"%s\" cat=\"%s\" />"
                    start (escape_html_simple str) (escape_html_simple lemma) pos entity (Cat.show cat))
                | {cat; op; children} -> 
                        let rule_type = show_xml_rule_type tree op in
                        mapM f children >>= fun children ->
                        return (!%"<rule type=\"%s\" cat=\"%s\">\n%s\n</rule>"
                        rule_type (Cat.show cat) (String.concat "\n" children))
            )
            in StateM.eval (f tree) (0, attr)
    end

        let show_xml_trees attribs tss =
            let f attr ts = match ts with
                | (_, t) :: _ -> !%"<ccg>\n%s\n</ccg>" (XML.show attr t)
                | _ -> invalid_arg "failed in show_xml_trees"
            in (!%"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<?xml-stylesheet type=\"text/xsl\" href=\"candc.xml\"?>
<candc>\n%s\n</candc>" (String.concat "\n\n" @@ List.map2 f attribs tss))

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
        let f printfun i = function
            | [] -> ()
            | lst -> List.iter (fun (_, t) ->
                     p "ID=%i\n%s\n" i (printfun t)) lst
        in match fmt with
        | "auto"  -> List.iteri (f show_tree) res
        | "deriv" -> List.iteri (f show_derivation) res
        | "ptb"   -> List.iteri (f (show_ptb 0)) res
        | "html"  -> pr (show_html_trees res)
        | "htmls" -> show_html_trees_separated names res
        | "xml"  -> pr (show_xml_trees attribs res)
        | "prolog" -> show_prolog attribs res
        | _ -> invalid_arg (!%"Not accepted output format: %s\n" fmt)
end
