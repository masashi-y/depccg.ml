
open Utils

module K = struct
    type t = int
    let compare (a:int) b = compare a b
end

module P = struct
    type t = {id: int;
              score: float;
              cat: Cat.t;
              tree: Tree.t}

    let make ~id ~score ~cat ~tree = {id=id; score=score; cat=cat; tree=tree}

    (* Give higher priority to ones with higher scores *)
    let compare {id=i1;score=s1} {id=i2;score=s2} =
        let c = compare s2 s1 in
        if c = 0 then compare i1 i2 else c
end

module Q =Psq.Make (K) (P)

let read_proto_matrix n_cats = 
    let open Ccg_seed_types in
    let error () = failwith "error in read_proto_matrix" in
    let convert = function
        | {values=v; shape=[i; j]} -> Matrix.reshape (Matrix.of_list v) (i, j)
        | _ -> error ()
    in function
    | {sentence=s;
       cat_probs=Some c;
       dep_probs=Some p} -> (s, convert c, convert p)
    | _ -> error ()

let compute_outside_scores scores length =
    let from_left = Array.make (length + 1) 0.0
    and from_right = Array.make (length + 1) 0.0 in
    for i = 0 to length - 2 do
        let j = length - i in
        from_left.(i+1) <- from_left.(i) +. scores.(i);
        from_right.(j-1) <- from_right.(j) +. scores.(j-1);
    done;
    Matrix.init (length+1, length+1) (fun i j -> from_left.(i) +. from_right.(j))

module T = Tree
module L = List
module LL = ListLabels
module M = Matrix
module H = Hashtbl

let new_item cat op ~id ~score ~children = 
    let (start, length) = match children with
    | [T.Node {start=s; length=l}] -> (s, l)
    | [T.Node {start=s; length=l}; T.Node {start=s'; length=l'}] -> (s, l + l')
    | _ -> failwith "error in new_item"
    in
    let tree = T.make ~cat ~op ~start ~length ~children in
    let elt = P.make ~id ~score ~cat ~tree
    in (id, elt)

let parse (sentence, cat_scores, dep_scores)
        ~cat_list
        ~combinatory_rules
        ~unary_rules
        ~seen_rules
        ~rule_cache
        ?(unary_penalty=0.1)
        ?(beta=0.000001) () =
    let n_words = L.length sentence
    and n_cats = L.length cat_list in
    let index = let i = ref 0 in fun () -> i := succ !i; !i in
    let indexed_sentence = enumerate sentence
    and best_dep_scores = Array.make n_words neg_infinity
    and best_cat_scores = Array.make n_words neg_infinity in
    let init_queue = LL.fold_right indexed_sentence ~init:Q.empty
    ~f:(fun (w_i, w) q ->
        best_dep_scores.(w_i) <- M.max_along_row dep_scores w_i;
        LL.fold_right cat_list ~init:q
        ~f:(fun (c_i, cat) q ->
            let score = M.get cat_scores (w_i, c_i) in
            if score > best_cat_scores.(w_i) then
                best_cat_scores.(w_i) <- score;
            let id = index () in
            let v = P.make ~score ~cat ~id ~tree:(T.terminal cat w w_i)
            in Q.add id v q
            )
        ) in
    let cat_out_scores = compute_outside_scores best_cat_scores n_words in
    let dep_out_scores = compute_outside_scores best_dep_scores n_words in
    let dep_out_score_leaf = Array.fold_left (+.) 0.0 best_dep_scores in
    let queue = Q.fold (fun k ({P.score=s; tree=T.Node t} as p) q ->
        let w_i = t.Tree.start in
        let threshold = (exp best_cat_scores.(w_i)) *. beta in
        if exp s < threshold then q
        else
        let score = s
                 +. M.get cat_out_scores (w_i, w_i + 1)
                 +. dep_out_score_leaf in
        Q.add k {p with P.score=score} q) Q.empty init_queue in

    let apply_unaries q = function
        | {P.tree=Node {T.length=l}} when l = n_words -> q
        | {P.score=s; cat=c; tree=t} ->
             LL.fold_right (H.find_all unary_rules c)
                ~init:q ~f:(fun c q ->
                    let (id, elt) = new_item c `Unary ~id:(index())
                        ~children:[t]
                        ~score:(s -. unary_penalty)
                    in Q.add id elt q)
        | _ -> invalid_arg "apply_unaries" in

    let get_rules cats = if H.mem rule_cache cats then
                             H.find rule_cache cats
                         else
                             (let rules = Combinator.apply_rules cats combinatory_rules
                             in H.add rule_cache cats rules; rules) in

    let is_seen (c1, c2) = let prep = Cat.remove_some_feat ["X"; "nb"] in
                           H.mem seen_rules (prep c1, prep c2) in

    let apply_binaries q s1 s2 = function
        | {P.score=s1; tree=T.Node {cat=c1; start=head; length=l1} as t1},
          {P.score=s2; tree=T.Node {cat=c2; start=dep;  length=l2} as t2} ->
              if not @@ is_seen (c1, c2) then q
              else LL.fold_right (get_rules (c1, c2))
                  ~init:q ~f:(fun (rule, c) q ->
                      let span = l1 + l2 in
                      let score = s1 +. s2 +. M.get dep_scores (dep, head+1)
                               +. M.get cat_out_scores (head, head + span)
                               +. M.get dep_out_scores (head, head + span)
                               +. best_dep_scores.(head) in
                      let (id, elt) = new_item c rule ~id:(index())
                          ~children:[t1; t2] ~score
                      in Q.add id elt q)
        | _ -> invalid_arg "apply_binaries" in

    let rec loop q = match Q.pop q with
    | None -> ()
    | Some ((_, {P.tree=Tree.Node {Tree.cat=c; children=[Tree.Leaf w]}; score=s}), q')
        -> p "%s\t%s\n%e\n\n" (Cat.show_cat c) w (exp s); loop q'
    in
    let chart = Chart.make n_words in

    (* let rec astar q = match Q.pop q with                             *)
    (* | None -> q                                                      *)
    (* | Some ((_, {P.tree=T.Node t; score=s; cat=c} as p), q')         *)
    (*        -> let cell = (t.T.start, t.T.length - 1) in              *)
    (*           if Chart.update chart cell c p then                    *)
    (*             (* |> apply_unaries q' p *)                          *)
    (*         Chart.fold_along_row chart (t.T.start + t.T.length)      *)
    (*             (fun _ {P.tree=T.Node t'} -> apply_binaries q' t t') *)
    (*         Chart.fold_along_col chart (t.T.start)                   *)
    (*             (fun _ {P.tree=T.Node t'} -> apply_binaries q' t' t) *)
        loop queue

let unary = "/Users/masashi-y/depccg/models/tri_headfirst/unary_rules.txt"
let binary = "/Users/masashi-y/depccg/models/tri_headfirst/seen_rules.txt"

let () =
    let file = Sys.argv.(1) in
    let res = Cat.read_ccgseeds file in
    let open Ccg_seed_types in
    let seed = List.hd res.seeds in
    let cat_list = enumerate (List.map Cat.parse_cat res.categories)
    and combinatory_rules = [`FwdApp; `BwdApp; `FwdCmp; `BwdCmp; `GenFwdCmp;
                    `GenBwdCmp; `Conj; `RP; `CommaVPtoADV; `ParentDirect]
    and unary_rules = Cat.read_unary_rules unary
    and seen_rules = Cat.read_binary_rules binary
    and rule_cache = Hashtbl.create 100
    and conved = read_proto_matrix (List.length res.categories) seed in
    let res = parse conved ~cat_list ~unary_rules ~seen_rules ~rule_cache
        ~combinatory_rules ~beta:0.00000001 () in ()



(* let () =                                                                  *)
(*     let q = Q.of_list [(1, {P.id=1;P.score=7.0});                         *)
(*                        (2, {P.id=2;P.score=2.0});                         *)
(*                        (3, {P.id=3;P.score=4.0});                         *)
(*                        (4, {P.id=4;P.score=4.0});                         *)
(*                        (5, {P.id=5;P.score=4.0})]                         *)
(*     in let rec loop qq = match Q.pop qq with                              *)
(*             | None -> ()                                                  *)
(*             | Some ((i, {P.score=s}), q') -> p "%i --> %f\n" i s; loop q' *)
(*     in loop q;                                                            *)

