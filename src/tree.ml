
module Cat = Cat.EnglishCategories
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
    | {str=""; children} -> List.flatten (List.map terminals children)
    | {str=s} -> [s]

let rec preterminals = function
    | {cat; op=`Intro} -> [cat]
    | {children} -> List.flatten (List.map preterminals children)

let rec show_tree = function
    | {cat; str; op=`Intro}
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
        | {cat; str; op=`Intro}
            -> max lwidth (2 + lwidth + max (length (Cat.show cat)) (length str))
        | {cat; op; children}
            -> let rwidth = ListLabels.fold_left children ~init:lwidth
                    ~f:(fun lw child -> let rw = show lw child in max lw rw) in
               let pad = max 0 ((rwidth - lwidth - (length (Cat.show cat))) / 2 + lwidth) in
               Printf.bprintf buf "%s%s%s\n%s%s\n" (space lwidth) (make (rwidth - lwidth) '-')
                       (Combinator.show op) (space pad) (Cat.show cat);
               rwidth
    in
    let ws = terminals tree in
    let cs = List.map Cat.show (preterminals tree) in
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
