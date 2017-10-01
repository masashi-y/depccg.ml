
open Utils
module T = Tree
module L = List
module LL = ListLabels
module M = Matrix
module H = Hashtbl

module K = struct
    type t = int
    let compare (a:int) b = compare a b
end

module P = struct
    type t = {id        : int;
              in_score  : float;
              out_score : float;
              score     : float;
              start     : int;
              length    : int;
              tree      : Tree.t}

    let make ~id ~in_score ~out_score ~start ~length ~tree =
                {id=id; in_score=in_score;
                out_score=out_score; score=in_score +. out_score;
                start=start; length=length; tree=tree}

    (* Give higher priority to ones with higher scores *)
    let compare {id=i1;score=s1} {id=i2;score=s2} =
        let c = compare s2 s1 in
        if c = 0 then compare i1 i2 else c
end

module Q = Psq.Make (K) (P)

module Log =
struct
    let do_log = false

    let blue : (_,_,_,_,_,_) format6 = "\027[34m%s\027[93m"
    let red  : (_,_,_,_,_,_) format6 = "\027[31m%s\027[93m"

    let bar () = 
        if do_log then prerr_endline
            ("\027[34m" ^ (String.make 10 '#') ^ "\027[93m\n")
        else ()

    let pop {P.score=s; in_score=in_s; out_score=out_s; id=id; tree=t} =
        if do_log then Printf.eprintf (blue ^^ "\nID: %i\n%s\ns: %e\nh: %e\ns+h: %e\n")
            "POPPED" id (T.show_derivation t) in_s out_s s; bar ()

    let unary {P.tree=t} = if do_log then
        Printf.eprintf (red ^^ "\n%s\n") "UNARY" (T.show_derivation t); bar()

    let cand {P.tree=t} = if do_log then
        Printf.eprintf (red ^^ "\n%s\n") "CAND" (T.show_derivation t); bar()

    let binary {P.tree=t} = if do_log then
        Printf.eprintf (red ^^ "\n%s\n") "BINARY" (T.show_derivation t); bar()
end

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

let possible_root_cats = [`S (`Con "dcl"); `S (`Con "wq"); `S (`Con "q"); `S (`Con "qem"); `NP `None]

let parse (sentence, cat_scores, dep_scores)
        ~cat_list
        ~unary_rules
        ~seen_rules
        ~rule_cache
        ~nbest
        ?(prune_size=50)
        ?(unary_penalty=0.1)
        ?(beta=0.000001) () =
    let n_words = L.length sentence in
    let index = let i = ref 0 in fun () -> i := succ !i; !i in
    let new_item ~in_score ~out_score ~start ~length ~tree = 
        let id = index () in
        let elt = P.make ~id ~in_score ~out_score ~start ~length ~tree
        in (id, elt)
    in
    let best_dep_scores = Array.make n_words neg_infinity
    and best_cat_scores = Array.make n_words neg_infinity in
    let init_queues = LL.fold_left (enumerate sentence) ~init:[]
    ~f:(fun lst (w_i, w) ->
        best_dep_scores.(w_i) <- M.max_along_row dep_scores w_i;
        let q = LL.fold_left cat_list ~init:Q.empty
        ~f:(fun q (c_i, cat) ->
            let in_score = M.get cat_scores (w_i, c_i) in
            if in_score > best_cat_scores.(w_i) then
                best_cat_scores.(w_i) <- in_score;
            let tree = T.terminal cat w in
            let (id, elt) = new_item ~in_score ~out_score:0.0
                          ~start:w_i ~length:1 ~tree
            in Q.add id elt q
            )
            in q :: lst
        ) in
    let cat_out_scores = compute_outside_scores best_cat_scores n_words in
    let dep_out_scores = compute_outside_scores best_dep_scores n_words in
    let dep_out_score_leaf = Array.fold_left (+.) 0.0 best_dep_scores in
    (* main queue *)

    let queue = LL.fold_left init_queues ~init:Q.empty
        ~f:(fun q w_q ->
            fold_queue_at_most prune_size
                (fun k ({P.score=s; start=w_i} as p) q ->
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
    let get_rules cats = try H.find rule_cache cats with Not_found ->
                        let rules = Combinator.apply_rules cats combinatory_rules in
                        H.add rule_cache cats rules;
                        rules in

    (* check a pair of cats is seen in a dictionary *)
    let is_seen (c1, c2) = let prep = Cat.remove_some_feat [`Var 0; `Nb] in
                           H.mem seen_rules (prep c1, prep c2) in

    let en_is_acceptable_unary c r = r <> `RP || Cat.is_type_raised c in

    (* apply unary rules to a subtree & insert them into the chart *)
    let apply_unaries p q = match p with
        | {P.length=l} when l = n_words -> q
        | {P.in_score=s; out_score=out_score; start=start;
                        length=length; tree={T.cat=c; op=r} as t} ->
             LL.fold_right (H.find_all unary_rules c)
                ~init:q ~f:(fun cat q ->
                    if en_is_acceptable_unary cat r then
                    (let tree = T.make ~cat ~op:`Unary ~children:[t] in
                     let (id, elt) = new_item
                        ~in_score:(s -. unary_penalty)
                        ~out_score ~start ~length ~tree
                    in 
                    Log.unary elt;
                    Q.add id elt q) else q)
        in

    (* apply binary rules to subtrees & insert them into the chart *)
    let apply_binaries ps q = match ps with
        | {P.in_score=s1; start=head; length=l1; tree={T.cat=c1} as t1},
          {P.in_score=s2; start=dep;  length=l2; tree={T.cat=c2} as t2} ->
              if not @@ is_seen (c1, c2) then q
              else
                  (Log.cand (fst ps); LL.fold_right (get_rules (c1, c2))
                  ~init:q ~f:(fun (op, cat) q ->
                      let length = l1 + l2 in
                      let in_score = s1 +. s2 +. M.get dep_scores (dep, head+1)
                      and out_score = M.get cat_out_scores (head, head + length)
                                   +. M.get dep_out_scores (head, head + length)
                                   -. best_dep_scores.(head) in
                      let tree = T.make ~cat ~op ~children:[t1; t2] in
                      let (id, elt) = new_item ~in_score ~out_score ~start:head
                                               ~length ~tree
                      in
                      Log.binary elt;
                      Q.add id elt q))
        in

    (* main chart *)
    let chart = Chart.make n_words
    and goal = Chart.make 1 in

    let check_root_cell p q = match p with
    | {P.in_score=score; start=s; length=l; tree={T.cat=c} as t}
        when l = n_words && L.mem c possible_root_cats ->
            let dep_score = M.get dep_scores (s, 0) in
            let in_score = score +. dep_score in
            let (id, elt) = new_item ~in_score ~out_score:0.0 ~start:s
                                   ~length:l ~tree:t in
            Q.add id elt q
    | _ -> q
    in
    (* main A* loop *)
    let rec astar q =
    if Chart.n_complete_parses chart >= nbest then
        `OK (List.sort P.compare @@ Chart.complete_parses chart)
    else match Q.pop q with
    | None -> `Fail
    | Some ((_, ({P.start=s; length=l; tree={T.cat=c}} as p)), q')
           -> Log.pop p;
              let cell = (s, l - 1) in
              if not @@ Chart.update chart cell c p then astar q' else
                (q'
                |> check_root_cell p |> apply_unaries p
                |> Chart.fold_along_row chart (s + l)
                    (fun _ p' q -> apply_binaries (p, p') q)
                |> Chart.fold_along_diag chart (s - 1)
                    (fun _ p' q -> apply_binaries (p', p) q)
                |> astar)
    in
    match astar queue with
    | `OK parses -> List.map (fun {P.score=s; tree=t} -> (s, t)) parses
    | `Fail      -> [(nan, Tree.fail)]

(* let unary = "/Users/masashi-y/depccg/models/tri_headfirst/unary_rules.txt" *)
(* let binary = "/Users/masashi-y/depccg/models/tri_headfirst/seen_rules.txt" *)

(* let aaa () =                                                              *)
(*     let file = Sys.argv.(1) in                                            *)
(*     let res = Cat.read_ccgseeds file in                                   *)
(*     let open Ccg_seed_types in                                            *)
(*     let seed = List.hd res.seeds in                                       *)
(*     let cat_list = enumerate (List.map Cat.parse_cat res.categories)      *)
(*     and unary_rules = Cat.read_unary_rules unary                          *)
(*     and seen_rules = Cat.read_binary_rules binary                         *)
(*     and rule_cache = Hashtbl.create 100                                   *)
(*     and nbest = 1                                                         *)
(*     and beta = 0.00000001                                                 *)
(*     and conved = read_proto_matrix (List.length res.categories) seed in   *)
(*     let res = parse conved ~cat_list ~unary_rules ~seen_rules ~rule_cache *)
(*         ~nbest ~beta () in                                                *)
(*     match res with                                                        *)
(*     | [(s, p)] -> Utils.p "%s\n" (T.show_derivation p)                    *)
(*     | _ -> print_endline "aaaaaaaaa";                                     *)
(*     Utils.p "rule_cache size: %i\n" (H.length rule_cache)                 *)
    



