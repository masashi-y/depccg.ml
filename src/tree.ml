
open Utils

type node = {cat: Cat.t;
             op: Combinator.t;
             start: int;
             length: int;
             children: t list}
and t = Leaf of string | Node of node

let make ~cat ~op ~start ~length ~children =
    Node({cat=cat; op=op; start=start; length=length; children=children})

let terminal cat s start = make ~cat ~op:`Intro
                                ~start ~length:1 ~children:[Leaf s]

let rec terminals = function
    | Leaf s -> [s]
    | Node n -> List.flatten (List.map terminals n.children)

let rec preterminals = function
    | Node {cat=c; children=[Leaf _]} -> [c]
    | Node n -> List.flatten (List.map preterminals n.children)
    | _ -> failwith "error in preterminals"


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
        | Node {cat=c; children=[Leaf w]}
            -> max lwidth (2 + lwidth + max (length (Cat.show_cat c)) (length w))
        | Node {cat=c;op=p; children=children}
            -> let rwidth = ListLabels.fold_left children ~init:lwidth
                    ~f:(fun lw child -> let rw = show lw child in max lw rw) in
               let pad = (rwidth - lwidth - (length (Cat.show_cat c))) / 2 + lwidth in
               Buffer.add_string buf (space lwidth ^ make (rwidth - lwidth) '-'
                 ^ Combinator.show_combinator p ^ "\n" ^ space pad ^ Cat.show_cat c ^ "\n");
               rwidth
        | _ -> failwith "error in show_derivation"
    in
    let ws = terminals tree in
    let cs = List.map Cat.show_cat (preterminals tree) in
    let (cs', ws') = terminal_string (List.combine cs ws) in
    Buffer.add_string buf cs';
    Buffer.add_char buf '\n';
    Buffer.add_string buf ws';
    Buffer.add_char buf '\n';
    (show 0 tree);
    Buffer.contents buf

let () =
    let open Cat in
    let tree = Node ({cat=s; op=`BwdApp; start=1; length=1;
                    children=[Node ({cat=np; op=`Intro; start=1; length=1;
                        children=[Leaf "Hanako"]});
                    Node ({cat=(s |: np); op=`FwdApp; start=1; length=1;
                        children=[Node ({cat=((s |: np) /: np); op=`Intro; start=1; length=1;
                            children=[Leaf "beats"]});
                            Node ({cat=np; op=`Intro; start=1; length=1; children=[Leaf "Taro"]})
                        ]})
                    ]}) in
    let res = terminals tree in
    let res1 = preterminals tree in
    let res2 = List.map show_cat res1 in
    pr (show_derivation tree)
