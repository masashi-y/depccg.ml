
open Utils
open Grammar
open Printer
module L = List
module LL = ListLabels
module M = Matrix
module H = Hashtbl


module Agenda (P : Psq.Ordered) =
struct
    module K = struct
        type t = int
        let compare (a:int) b = compare a b
    end
    include Psq.Make (K) (P)

    let rec fold_at_most n f init q =
        if n <= 0 then init else
        match pop q with
        | None -> init
        | Some ((k, v), q') -> fold_at_most (n-1) f (f k v init) q'
end


let compute_outside_scores scores length =
    let from_left = Array.make (length + 1) 0.0
    and from_right = Array.make (length + 1) 0.0 in
    for i = 0 to length - 2 do
        let j = length - i in
        from_left.(i+1) <- from_left.(i) +. scores.(i);
        from_right.(j-1) <- from_right.(j) +. scores.(j-1);
    done;
    Matrix.init (length+1, length+1) (fun i j -> from_left.(i) +. from_right.(j))


module MakeAStarParser (Grammar : GRAMMAR) =
struct

    open Grammar
    module Cell = struct
        type t = {
           id: int;
           in_score: float;
           out_score: float;
           score: float;
           start: int;
           length: int;
           final: bool;
           tree: Tree.t
        }

        (* Give higher priority to ones with higher scores *)
        let compare { id = i1 ;score = s1 } { id = i2 ;score = s2 } =
            let c = compare s2 s1 in
            if c = 0 then compare i1 i2 else c
    end
    module Q = Agenda (Cell)

    module Log = struct
        module Printer = ParsePrinter (Grammar)
        let do_log = false
        let blue = format_of_string "\027[34m%s\027[93m"
        let red  = format_of_string "\027[31m%s\027[93m"

        let bar () = prerr_endline ("\027[34m" ^ (String.make 10 '#') ^ "\027[93m\n")

        let pop Cell.{score; in_score; out_score; id; tree} =
            if do_log then (Printf.eprintf (blue ^^ "\nID: %i\n%s\ns: %e\nh: %e\ns+h: %e\n")
                "POPPED" id (Printer.show_derivation (Attributes.default ()) tree) in_score out_score score; bar ())

        let onebest = function
            | None -> ()
            | Some (_, Cell.{tree=Tree.{cat; str}}) -> if do_log then
                Printf.eprintf "%s\t-->\t%s\n" str (Cat.show cat)

        let log msg Cell.{tree} = if do_log then
            (Printf.eprintf (red ^^ "\n%s\n") msg (Printer.show_derivation (Attributes.default ()) tree); bar())

        let (unary, cand, binary) = (log "UNARY", log "CAND", log "BINARY")

        let show_derivation tree = Printer.show_derivation (Attributes.default ()) tree
    end

    let fail = Tree.terminal Cat.np "FAILED"

    let rule_cache = ref @@ H.create 1000

    let overlap (start1, length1) (start2, length2) =
        let end1 = start1 + length1 - 1 in
        let end2 = start2 + length2 - 1 in
        (start2 <= end1 && end1 <= end2 && start1 < start2 && start2 < end1)
            || (start2 < start1 && start1 < end2 && start1 <= end2 && end2 <= end1)


    let parse (sentence, cat_scores, dep_scores, constraints)
            ~cat_list
            ~unary_rules
            ~nbest
            ?(seen_rules=None)
            ?(cat_dict=None)
            ?(prune_size=50)
            ?(unary_penalty=0.1)
            ?(beta=0.000001) () =
        let n_words = L.length sentence in
        let index = let i = ref 0 in fun () -> incr i; !i in
        let new_item ?(final=false) tree ~in_score ~out_score ~start ~length = 
            let id = index () in
            let score=in_score +. out_score in
            let elt = Cell.{id; in_score; out_score; score; start; length; final; tree}
            in (id, elt)
        in
        let is_seen cs = match seen_rules with
            | Some rules -> is_seen rules cs
            | None -> true
        in
        let cat_dict w i = match cat_dict with
            | Some d ->
                (try (H.find d w).(i)
                with Not_found -> true)
            | None -> true
        in
        let enqueue = match constraints with
            | [] -> Q.add
            | cs ->
                fun id (Cell.{start; length; tree=Tree.{cat}} as elt) queue ->
                    if List.exists (fun (cat0, start0, length0) ->
                        overlap (start, length) (start0, length0)
                        || (start, length) = (start0, length0)
                           && cat <> cat0
                           && not @@ H.mem unary_rules cat) constraints
                    then queue
                    else Q.add id elt queue
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
                    let tree = Tree.terminal cat w in
                    let (id, elt) = new_item tree ~in_score ~out_score:0.0
                                  ~start:w_i ~length:1
                    in enqueue id elt q
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
                Q.fold_at_most prune_size
                    (fun k (Cell.{score=s; start=w_i} as p) q ->
                    let threshold = (exp best_cat_scores.(w_i)) *. beta in
                    if exp s < threshold then q
                    else
                    let out_score = M.get cat_out_scores (w_i, w_i + 1)
                                 +. dep_out_score_leaf in
                    let score = s +. out_score in
                    enqueue k {p with Cell.score=score; out_score=out_score} q)
                q w_q)
        in
        (* apply unary rules to a subtree & insert them into the chart *)
        let apply_unaries p q = match p with
            | Cell.{length} when length = n_words -> q
            | Cell.{in_score=s; out_score; start; length; tree={Tree.cat=c; op} as t} ->
                LL.fold_right (H.find_all unary_rules c)
                    ~init:q ~f:(fun cat q ->
                        if is_acceptable_unary cat op then
                        (let tree = Tree.make ~cat ~op:Rules.unary ~children:[t] in
                         let (id, elt) = new_item tree
                            ~in_score:(s -. unary_penalty)
                            ~out_score ~start ~length
                        in 
                        Log.unary elt;
                        enqueue id elt q) else q)
        in
        (* apply binary rules to subtrees & insert them into the chart *)
        let apply_binaries ps q = match ps with
            | Cell.{in_score=s1; start=st1; length=l1; tree=Tree.{cat=c1} as t1},
              Cell.{in_score=s2; start=st2; length=l2; tree=Tree.{cat=c2} as t2}
              when is_seen (c1, c2) ->
                      Log.cand (fst ps);
                      LL.fold_right (apply_rules_with_cache !rule_cache (c1, c2))
                      ~init:q ~f:(fun (op, cat) q ->
                          let length = l1 + l2 in
                          let (head, dep) = resolve_dependency (st1, st2) (l1, l2) in
                          let in_score = s1 +. s2 +. M.get dep_scores (dep, head+1)
                          and out_score = M.get cat_out_scores (st1, st1 + length)
                                       +. M.get dep_out_scores (st1, st1 + length)
                                       -. best_dep_scores.(head) in
                          let tree = Tree.make ~cat ~op ~children:[t1; t2] in
                          let (id, elt) = new_item tree ~in_score ~out_score ~start:st1 ~length
                          in
                          Log.binary elt;
                          enqueue id elt q)
            | _ -> q
        in

        (* main chart *)
        let chart = Chart.make n_words nbest
        and goal = Chart.make 1 nbest
        in
        let check_root_cell p q = match p with
        | Cell.{in_score=score; start; length; tree=Tree.{cat} as tree}
            when length = n_words && L.mem cat possible_root_cats ->
                let dep_score = M.get dep_scores (start, 0) in
                let in_score = score +. dep_score in
                let (id, elt) = new_item tree ~in_score ~out_score:0.0 ~start
                                       ~length ~final:true in
                enqueue id elt q
        | _ -> q
        in
        (* main A* loop *)
        let rec astar q =
        if Chart.n_complete_parses goal >= nbest then `OK
        else match Q.pop q with
        | None when Chart.n_complete_parses goal > 0 -> `OK
        | None -> `Fail
        | Some ((_, (Cell.{start=s; length=l; tree=Tree.{cat}} as p)), q')
               -> Log.pop p;
                  if p.Cell.final then let _ = Chart.update goal (0, 0) cat p in astar q'
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
        | `OK   -> let parses = L.sort Cell.compare @@ Chart.complete_parses goal
                   in L.map (fun Cell.{score; tree} -> (score, tree)) parses
        | `Fail -> [(nan, fail)]

end

module EnAstarParser = MakeAStarParser (EnglishGrammar)
module JaAstarParser = MakeAStarParser (JapaneseGrammar)
