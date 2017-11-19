
open Grammar
open Utils

module Tree (Grammar : GRAMMAR) =
struct

    module Cat = Grammar.Cat
    module Feat = Grammar.Feature

    type t = {cat      : Cat.t;
              op       : Grammar.t;
              children : t list;
              str      : string}

    let make ~cat ~op ~children =
        {cat=cat; op=op; children=children; str=""}

    let terminal cat s =
        {cat=cat; op=Grammar.intro; children=[]; str=s}

    let rec terminals = function
        | {str=""; children} -> List.flatten (List.map terminals children)
        | {str=s} -> [s]

    let rec preterminals = function
        | {cat; children=[]} -> [cat]
        | {children} -> List.flatten (List.map preterminals children)

    let rec show_tree = function
        | {cat; str; children=[]}
            -> let c' = Cat.show cat in
               !%"(<L %s POS POS %s %s>)" c' str c'
        | {cat; children}
            -> let w = String.concat " " (List.map show_tree children) in
               let n_child = List.length children in
               let head_is_left = 0 in     (* do not care *)
               !%"(<T %s %i %i> %s )" (Cat.show cat) head_is_left n_child w

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
                           (Grammar.show op) (space pad) (Cat.show cat);
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
                           match Feat.show f with
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
         (escape_html_simple @@ Grammar.show op))
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

    let sample_tree = let open Cat in
                make ~cat:s ~op:Grammar.intro ~children:
                [terminal np "Hanako";
                make ~cat:(s |: np) ~op:Grammar.intro ~children:
                         [terminal ((s |: np) /: np) "beats";
                          terminal np                "Taro"
                         ]
                ]

    let fail = terminal Cat.np "FAILED"

end

