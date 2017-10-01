
open Utils

type t = {cat      : Cat.t;
          op       : Combinator.t;
          children : t list;
          str      : string}

let make ~cat ~op ~children =
    {cat=cat; op=op; children=children; str=""}

let terminal cat s =
    {cat=cat; op=`Intro; children=[]; str=s}

let rec terminals = function
    | {str=""; children=ch} -> List.flatten (List.map terminals ch)
    | {str=s} -> [s]

let rec preterminals = function
    | {cat=c; op=`Intro} -> [c]
    | {children=ch} -> List.flatten (List.map preterminals ch)

let rec show_tree = function
    | {cat=c; str=w; op=`Intro}
        -> let c' = Cat.show_cat c in
           !%"(<L %s POS POS %s %s>)" c' w c'
    | {cat=c; children=ch}
        -> let w = String.concat " " (List.map show_tree ch) in
           let n_child = List.length ch in
           let head_is_left = 1 in     (* do not care *)
           !%"(<T %s %i %i> %s)" (Cat.show_cat c) head_is_left n_child w

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
        | {cat=c; str=w; op=`Intro}
            -> max lwidth (2 + lwidth + max (length (Cat.show_cat c)) (length w))
        | {cat=c; op=p; children=ch}
            -> let rwidth = ListLabels.fold_left ch ~init:lwidth
                    ~f:(fun lw child -> let rw = show lw child in max lw rw) in
               let pad = max 0 ((rwidth - lwidth - (length (Cat.show_cat c))) / 2 + lwidth) in
               Printf.bprintf buf "%s%s%s\n%s%s\n" (space lwidth) (make (rwidth - lwidth) '-')
                       (Combinator.show_combinator p) (space pad) (Cat.show_cat c);
               rwidth
    in
    let ws = terminals tree in
    let cs = List.map Cat.show_cat (preterminals tree) in
    let (cs', ws') = terminal_string (List.combine cs ws) in
    Printf.bprintf buf "%s\n" ws';
    Printf.bprintf buf "%s\n" cs';
    let _ = show 0 tree in
    Buffer.contents buf

let sample_tree = let open Cat in
            make ~cat:s ~op:`FwdApp ~children:
            [terminal np "Hanako";
            make ~cat:(s |: np) ~op:`FwdApp ~children:
                     [terminal ((s |: np) /: np) "beats";
                      terminal np                "Taro"
                     ]
            ]

let fail = terminal Cat.np "FAILED"
