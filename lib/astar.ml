
open Utils
open Grammar
open Printer


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
           in_score: float;
           out_score: float;
           score: float;
           start: int;
           length: int;
           final: bool;
           cat: Cat.t;
           tree: Tree.t
        }

        let make tree
                ?(final=false)
                ~in_score
                ~out_score
                ~start
                ~length = {
            in_score;
            out_score;
            score = in_score +. out_score;
            start;
            length;
            final;
            cat = Tree.(tree.cat);
            tree
        }

        let to_scored {score; tree} = (score, tree)

        (* Give higher priority to ones with higher scores *)
        let compare {score = s1} {score = s2} = compare s2 s1
    end
    module Queue = Agenda.Make (Cell)

    module Log = struct
        module Printer = ParsePrinter (Grammar)
        open Cell

        let blue = format_of_string "\027[34m%s\027[93m"
        let red  = format_of_string "\027[31m%s\027[93m"

        let logging f =
            if false then f ()

        let bar () =
            prerr_endline ("\027[34m" ^ (String.make 10 '#') ^ "\027[93m\n")

        let pop {score; in_score; out_score; tree} =
            logging (fun () ->
                Printf.eprintf (blue ^^ "\n%s\ns: %e\nh: %e\ns+h: %e\n")
                    "POPPED"
                    (Printer.show_derivation (Attributes.default ()) tree)
                    in_score out_score score;
                bar ())

        let onebest = function
            | None -> ()
            | Some {tree=Tree.{cat; str}} ->
                logging (fun () ->
                    Printf.eprintf "%s\t-->\t%s\n" str (Cat.show cat))

        let log msg {tree} =
            logging (fun () ->
                Printf.eprintf
                    (red ^^ "\n%s\n")
                    msg
                    (Printer.show_derivation (Attributes.default ()) tree);
                bar())

        let unary = log "UNARY"
        let cand = log "CAND"
        let binary = log "BINARY"

        let show_derivation tree =
            Printer.show_derivation (Attributes.default ()) tree
    end

    let fail = [nan, Tree.terminal Cat.np "FAILED"]

    let rule_cache = ref (Hashtbl.create 1000)

    let overlap (start1, length1) (start2, length2) =
        let end1 = start1 + length1 - 1 in
        let end2 = start2 + length2 - 1 in
        (start2 <= end1 && end1 < end2 && start1 < start2 && start2 <= end1)
            || (start2 < start1 && start1 <= end2 && start1 <= end2 && end2 < end1)

    let build_constraints ~unary_rules constraints =
        let aux (terminals, nonterminals) = function
            | Partial.N (Some category, start, length) ->
                    let cat = Cat.parse category in
                    let constraint_ (cat0, start0, length0) =
                        overlap (start, length) (start0, length0)
                            || (start, length) = (start0, length0)
                               && not Cat.(cat =:= cat0)
                               && not @@ Hashtbl.mem unary_rules cat0 in
                    (terminals, constraint_ :: nonterminals)
            | Partial.N (None, start, length) ->
                    let constraint_ (_, start0, length0) =
                        overlap (start, length) (start0, length0) in
                    (terminals, constraint_ :: nonterminals)
            | Partial.T (category, start) ->
                    let category = Cat.parse category in
                    ((start, category) :: terminals, nonterminals)
        in
        let terminals, nonterminals = List.fold_left aux ([], []) constraints in
        let enqueue_fun = match nonterminals with
            | [] -> Queue.add
            | constraints ->
                fun (Cell.{start; length; cat} as cell) queue ->
                    if List.exists (fun constraint_ -> constraint_ (cat, start, length)) constraints
                    then queue
                    else Queue.add cell queue
        in
        enqueue_fun, terminals

    let build_seen_rules = function
        | Some rules -> Grammar.is_seen rules
        | None -> fun _ -> true

    let build_cat_dict = function
        | Some d ->
            fun w i -> begin
                try (Hashtbl.find d w).(i)
                with Not_found -> true
            end
        | None ->
            fun _ _ -> true

    let setup_per_word_queues
            ~enqueue
            ~best_dep_scores
            ~best_cat_scores
            ~terminal_constraints
            ~cat_dict
            ~cat_list
            ~cat_scores
            ~dep_scores
            sentence =
        let f word_i lst word =
            best_dep_scores.(word_i) <- Matrix.max_along_row dep_scores word_i;
            let meet_constraint =
                match List.assoc_opt word_i terminal_constraints with
                | Some cat0 -> fun cat -> Cat.(cat0 =:= cat)
                | None -> fun _ -> true in
            let seen_cats_for_w = cat_dict word in
            let queue =
                ListLabels.fold_left cat_list
                    ~init:(Queue.empty ())
                    ~f:(fun queue (cat_i, cat) ->
                    if not (seen_cats_for_w cat_i && meet_constraint cat) then queue
                    else begin
                        let in_score = Matrix.get cat_scores (word_i, cat_i) in
                        if in_score > best_cat_scores.(word_i) then
                            best_cat_scores.(word_i) <- in_score;
                        let tree = Tree.terminal cat word in
                        let cell = Cell.make tree ~final:false ~in_score ~out_score:0.0 ~start:word_i ~length:1 in
                        enqueue cell queue
                    end) in
            queue :: lst in
        Utils.fold_lefti sentence ~init:[] ~f

    let initialize_queue
             ~enqueue
             ~prune_size
             ~beta
             ~cat_out_scores
             ~dep_out_scores
             ~best_cat_scores
             ~best_dep_scores =
        let dep_out_score_leaf = Array.fold_left (+.) 0.0 best_dep_scores in
        let aux queue word_queue =
            Log.onebest (Queue.min word_queue);
            Queue.fold_at_most prune_size
                (fun (Cell.{score; start} as cell) queue ->
                    let threshold = exp best_cat_scores.(start) *. beta in
                    if exp score < threshold then queue
                    else
                    let out_score = Matrix.get cat_out_scores (start, start + 1)
                                  +. dep_out_score_leaf in
                    let score = score +. out_score in
                    enqueue Cell.{cell with score = score; out_score = out_score} queue)
                queue word_queue in
        List.fold_left aux (Queue.empty ())

    let apply_unary_rules
            ~enqueue
            ~n_words
            ~unary_penalty
            ~unary_rules
            Cell.{in_score; out_score; start; length; tree={Tree.cat=c; op} as tree} queue =
        if length = n_words then queue
        else 
            let aux cat queue =
            if not (is_acceptable_unary cat op) then queue
            else begin
                let tree = Tree.make ~cat ~op:Rules.unary ~children:[tree] in
                let in_score = in_score -. unary_penalty in
                let cell = Cell.make tree ~final:false ~in_score ~out_score ~start ~length in 
                Log.unary cell;
                enqueue cell queue
            end in
        List.fold_right aux (Hashtbl.find_all unary_rules c) queue

    let apply_binary_rules
            ~enqueue
            ~is_seen
            ~cat_scores
            ~dep_scores
            ~cat_out_scores
            ~dep_out_scores
            ~best_cat_scores
            ~best_dep_scores
            Cell.{in_score=s1; start=st1; length=l1; tree=t1; cat=c1}
            Cell.{in_score=s2; start=st2; length=l2; tree=t2; cat=c2}
            queue =
        if not (is_seen (c1, c2)) then queue
        else begin
            let aux (op, cat) queue =
                let length = l1 + l2 in
                let head, dep = resolve_dependency (st1, st2) (l1, l2) in
                let in_score = s1 +. s2 +. Matrix.get dep_scores (dep, head + 1) in
                let out_score = Matrix.get cat_out_scores (st1, st1 + length)
                             +. Matrix.get dep_out_scores (st1, st1 + length)
                             -. best_dep_scores.(head) in
                let tree = Tree.make ~cat ~op ~children:[t1; t2] in
                let cell = Cell.make tree ~final:false ~in_score ~out_score ~start:st1 ~length
                in
                Log.binary cell;
                enqueue cell queue in
            List.fold_right aux (apply_rules_with_cache !rule_cache (c1, c2)) queue
        end

    let check_root_cell
            ~enqueue
            ~n_words
            ~possible_root_cats
            ~dep_scores
            ~cat_scores
            Cell.{in_score; start; length; tree; cat}
            queue =
        if not (length = n_words && List.mem cat possible_root_cats) then queue
        else begin
            let dep_score = Matrix.get dep_scores (start, 0) in
            let in_score = in_score +. dep_score in
            let out_score = 0.0 in
            let cell = Cell.make tree ~final:true ~in_score ~out_score ~start ~length in
            enqueue cell queue
        end

    let parse (sentence, cat_scores, dep_scores, constraints)
            ~cat_list
            ~unary_rules
            ~nbest
            ?(seen_rules=None)
            ?(cat_dict=None)
            ?(prune_size=50)
            ?(unary_penalty=0.1)
            ?(beta=0.000001) () =
        let n_words = List.length sentence in
        let is_seen = build_seen_rules seen_rules in
        let cat_dict = build_cat_dict cat_dict in

        let enqueue, terminal_constraints =
            build_constraints ~unary_rules constraints in

        let best_dep_scores = Array.make n_words neg_infinity in
        let best_cat_scores = Array.make n_words neg_infinity in

        let init_queues = setup_per_word_queues
                ~enqueue ~best_dep_scores ~best_cat_scores ~terminal_constraints
                ~cat_dict ~cat_list ~cat_scores ~dep_scores sentence in

        (* compute the estimates of outside probabilities *)
        let cat_out_scores = compute_outside_scores best_cat_scores n_words in
        let dep_out_scores = compute_outside_scores best_dep_scores n_words in

        (* main queue *)
        let queue = initialize_queue
                ~enqueue ~prune_size ~beta ~cat_out_scores ~dep_out_scores
                ~best_cat_scores ~best_dep_scores init_queues in

        (* apply unary rules to a subtree & insert them into the chart *)
        let apply_unary_rules = apply_unary_rules
                ~enqueue ~n_words ~unary_penalty ~unary_rules in

        (* apply binary rules to subtrees & insert them into the chart *)
        let apply_binary_rules = apply_binary_rules
                ~enqueue ~is_seen ~cat_scores ~dep_scores ~cat_out_scores
                ~dep_out_scores ~best_cat_scores ~best_dep_scores in

        let check_root_cell = check_root_cell
                ~enqueue ~n_words ~possible_root_cats
                ~dep_scores ~cat_scores in

        (* main chart *)
        let chart = Chart.make n_words nbest in
        let goal = Chart.make 1 nbest in

        (* main A* loop *)
        let rec astar queue =
            let completed = Chart.n_complete_parses goal in
            if completed >= nbest || Queue.is_empty queue && completed > 0
            then begin
                Chart.complete_parses goal
                |> List.sort Cell.compare |> List.map Cell.to_scored
            end
            else search queue

        and search queue =
            match Queue.pop queue with
            | None -> fail
            | Some (cell, queue) ->
                Log.pop cell;
                let Cell.{final; start; length; cat} = cell in
                if final then
                    let _ = Chart.update goal (0, 0) cat cell in
                    astar queue
                else begin
                    if not (Chart.update chart (start, length - 1) cat cell)
                    then astar queue
                    else (queue
                        |> check_root_cell cell
                        |> apply_unary_rules cell
                        |> Chart.fold_along_row chart (start + length)
                            (fun _ other queue -> apply_binary_rules cell other queue)
                        |> Chart.fold_along_diag chart (start - 1)
                            (fun _ other queue -> apply_binary_rules other cell queue)
                        |> astar)
                end in
        astar queue
end

module EnAstarParser = MakeAStarParser (EnglishGrammar)
module JaAstarParser = MakeAStarParser (JapaneseGrammar)
