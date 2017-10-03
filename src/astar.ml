
open Utils
module T = Tree
module L = List
module LL = ListLabels
module M = Matrix
module H = Hashtbl

module P = Cell

module Log = P.Log

module K = struct
    type t = int
    let compare (a:int) b = compare a b
end

module Q = Psq.Make (K) (P)

let rec fold_queue_at_most n f init q =
    if n <= 0 then init else
    match Q.pop q with
    | None -> init
    | Some ((k, v), q') -> fold_queue_at_most (n-1) f (f k v init) q'

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

let combinatory_rules = [`FwdApp; `BwdApp; `FwdCmp; `BwdCmp; `GenFwdCmp;
                        `GenBwdCmp; `Conj; `RP; `CommaVPtoADV; `ParentDirect]

let possible_root_cats =
    [`S (`Con "dcl"); `S (`Con "wq"); `S (`Con "q"); `S (`Con "qem"); `NP `None; `N `None]

let en_is_acceptable_unary c r = r <> `RP || Cat.is_type_raised c


let parse (sentence, cat_scores, dep_scores)
        ~cat_list
        ~unary_rules
        ~seen_rules
        ~rule_cache
        ~nbest
        ?cat_dict
        ?(prune_size=50)
        ?(unary_penalty=0.1)
        ?(beta=0.000001) () =
    let n_words = L.length sentence in
    let index = let i = ref 0 in fun () -> i := succ !i; !i in
    let new_item ?(final=false) tree ~in_score ~out_score ~start ~length = 
        let id = index () in
        let elt = P.make ~id ~in_score ~out_score ~start ~length ~tree ~final
        in (id, elt)
    in
    let cat_dict = match cat_dict with
        | Some d -> (fun w i -> try
                    let arr = Hashtbl.find d w in
                    arr.(i)
                with Not_found -> true)
        | None -> (fun _ _ -> true)
    in
    let best_dep_scores = Array.make n_words neg_infinity
    and best_cat_scores = Array.make n_words neg_infinity in
    let init_queues = LL.fold_left (enumerate sentence) ~init:[]
    ~f:(fun lst (w_i, w) ->
        best_dep_scores.(w_i) <- M.max_along_row dep_scores w_i;
        let seen_cats_for_w = cat_dict w in
        let q = LL.fold_left cat_list ~init:Q.empty
        ~f:(fun q (c_i, cat) ->
            if seen_cats_for_w c_i then begin
                let in_score = M.get cat_scores (w_i, c_i) in
                if in_score > best_cat_scores.(w_i) then
                    best_cat_scores.(w_i) <- in_score;
                let tree = T.terminal cat w in
                let (id, elt) = new_item tree ~in_score ~out_score:0.0
                              ~start:w_i ~length:1
                in Q.add id elt q
            end else q)
            in q :: lst
        ) in
    let cat_out_scores = compute_outside_scores best_cat_scores n_words in
    let dep_out_scores = compute_outside_scores best_dep_scores n_words in
    let dep_out_score_leaf = Array.fold_left (+.) 0.0 best_dep_scores in

    (* main queue *)
    let queue = LL.fold_left init_queues ~init:Q.empty
        ~f:(fun q w_q ->
            Log.onebest (Q.min w_q);
            fold_queue_at_most prune_size
                (fun k (P.{score=s; start=w_i} as p) q ->
                let threshold = (exp best_cat_scores.(w_i)) *. beta in
                if exp s < threshold then q
                else
                let out_score = M.get cat_out_scores (w_i, w_i + 1)
                             +. dep_out_score_leaf in
                let score = s +. out_score in
                Q.add k {p with P.score=score; out_score=out_score} q)
            q w_q)
    in
    (* apply a set of combinatory rules to a pair of categories & caches the rules *)
    let get_rules (c1, c2) = let prep = Cat.remove_some_feat [`Nb] in
                             let cats = (prep c1, prep c2) in
                             try H.find rule_cache cats with Not_found ->
                             let rules = Combinator.apply_rules cats combinatory_rules in
                             H.add rule_cache cats rules;
                             rules
    in
    (* check a pair of cats is seen in a dictionary *)
    let is_seen (c1, c2) = let prep = Cat.remove_some_feat [`Var 0; `Nb] in
                           H.mem seen_rules (prep c1, prep c2)
    in
    (* apply unary rules to a subtree & insert them into the chart *)
    let apply_unaries p q = match p with
        | P.{length} when length = n_words -> q
        | P.{in_score=s; out_score; start; length; tree={T.cat=c; op} as t} ->
             LL.fold_right (H.find_all unary_rules c)
                ~init:q ~f:(fun cat q ->
                    if en_is_acceptable_unary cat op then
                    (let tree = T.make ~cat ~op:`Unary ~children:[t] in
                     let (id, elt) = new_item tree
                        ~in_score:(s -. unary_penalty)
                        ~out_score ~start ~length
                    in 
                    Log.unary elt;
                    Q.add id elt q) else q)
        in

    (* apply binary rules to subtrees & insert them into the chart *)
    let apply_binaries ps q = match ps with
        | P.{in_score=s1; start=head; length=l1; tree=T.{cat=c1} as t1},
          P.{in_score=s2; start=dep;  length=l2; tree=T.{cat=c2} as t2}
          when is_seen (c1, c2) ->
                  Log.cand (fst ps);
                  LL.fold_right (get_rules (c1, c2))
                  ~init:q ~f:(fun (op, cat) q ->
                      let length = l1 + l2 in
                      let in_score = s1 +. s2 +. M.get dep_scores (dep, head+1)
                      and out_score = M.get cat_out_scores (head, head + length)
                                   +. M.get dep_out_scores (head, head + length)
                                   -. best_dep_scores.(head) in
                      let tree = T.make ~cat ~op ~children:[t1; t2] in
                      let (id, elt) = new_item tree ~in_score ~out_score ~start:head
                                               ~length
                      in
                      Log.binary elt;
                      Q.add id elt q)
        | _ -> q
        in

    (* main chart *)
    let chart = Chart.make n_words
    and goal = Chart.make 1
    in
    let check_root_cell p q = match p with
    | P.{in_score=score; start; length; tree=T.{cat} as tree}
        when length = n_words && L.mem cat possible_root_cats ->
            let dep_score = M.get dep_scores (start, 0) in
            let in_score = score +. dep_score in
            let (id, elt) = new_item tree ~in_score ~out_score:0.0 ~start
                                   ~length ~final:true in
            Q.add id elt q
    | _ -> q
    in
    (* main A* loop *)
    let rec astar q =
    if Chart.n_complete_parses goal >= nbest then
        `OK (List.sort P.compare @@ Chart.complete_parses goal)
    else match Q.pop q with
    | None -> `Fail
    | Some ((_, (P.{start=s; length=l; tree=T.{cat}} as p)), q')
           -> Log.pop p;
              if p.P.final then let _ = Chart.update goal (0, 0) cat p in astar q'
              else
              let cell = (s, l - 1) in
              if not @@ Chart.update chart cell cat p then astar q' else
                (q' |> check_root_cell p |> apply_unaries p
                |> Chart.fold_along_row chart (s + l)
                    (fun _ p' q -> apply_binaries (p, p') q)
                |> Chart.fold_along_diag chart (s - 1)
                    (fun _ p' q -> apply_binaries (p', p) q)
                |> astar)
    in
    match astar queue with
    | `OK parses -> List.map (fun P.{score; tree} -> (score, tree)) parses
    | `Fail      -> [(nan, Tree.fail)]

