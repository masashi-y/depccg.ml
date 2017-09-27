
open Ccg_seed_types
open Utils

let (</>) = Filename.concat

let paths = ref []
and nbest = ref 1
and beta = ref 0.00000001
and out = ref "auto"

let spec = 
    [("-nbest",  Arg.Set_int nbest,   "output nbest parses")
    ;("-beta",   Arg.Set_float beta,  "beta value for pruning")
    ;("-format", Arg.Set_string out,  "beta value for pruning")
    ]

let usage = !%"\n%sUsage: thorn [-nbest] [-beta] [-format] model seeds"
            (Tree.show_derivation Tree.sample_tree)

let valid_format s = List.mem s ["auto"; "deriv"]

let output_results res =
    match !out with
    | "auto"  -> List.iteri (fun i [(_, t)] -> p "ID=%i\n%s\n" i (Tree.show_tree t)) res
    | "deriv" -> List.iteri (fun i [(_, t)] -> p "ID=%i\n%s\n" i (Tree.show_derivation t)) res
    | _ -> invalid_arg "output_results"

let () =
    let () = Arg.parse spec (fun s -> paths := s :: !paths) usage in
    let (model, seeds) = match !paths with
        |[m; s] -> (m, s)
        | _ -> Arg.usage spec usage; exit 1;
    in
    let ss = Cat.read_ccgseeds seeds in
    let cat_list = Utils.enumerate (List.map Cat.parse_cat ss.categories)
    and n_cats = (List.length ss.categories)
    and unary_rules = Cat.read_unary_rules (model </> "unary_rules.txt")
    and seen_rules = Cat.read_binary_rules (model </> "seen_rules.txt")
    and rule_cache = Hashtbl.create 1000 in
    let res = ListLabels.map ss.seeds
            ~f:(fun s -> Astar.parse (Astar.read_proto_matrix n_cats s)
            ~cat_list ~unary_rules ~seen_rules ~rule_cache
            ~nbest:(!nbest) ~beta:(!beta) ~unary_penalty:0.1 ())
    in output_results res
