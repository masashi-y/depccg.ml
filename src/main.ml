
open Ccg_seed_types
open Utils
module Cat = Cat.EnglishCategories

module EnAstarParser = Astar.MakeAStarParser (Astar.EnglishGrammar)

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

let status =
"[parser] Camelthorn CCG Parser\n"              ^^
"[parser] input document size:\t%i\n"           ^^
"[parser] number of terminal categories:\t%i\n" ^^
"[parser] unary rule size:\t%i\n"               ^^
"[parser] seen rule size:\t%i\n"                ^^
"[parser] category dictionary size:\t%i\n"      ^^
"[parser] nbest:\t%i\n"                         ^^
"[parser] beta:\t%e\n"                          ^^
"[parser] output format:\t%s\n"

let progress_map ~f lst =
    let size = List.length lst in
    let f' (i, x) = let y = f x in
        Printf.eprintf "\b\r[parser] %i/%i" (i+1) size;
        flush stderr;
        y
    in let res = List.map f' (Utils.enumerate lst) in
    Printf.eprintf "\b\r[parser] done         \n";
    res

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

let () =
    let () = Arg.parse spec (fun s -> paths := s :: !paths) usage in
    let (model, seeds) = match !paths with
        |[m; s] -> (m, s)
        | _ -> Arg.usage spec usage; exit 1;
    in
    let ss = Cat.read_ccgseeds seeds in
    let cat_list = Utils.enumerate (List.map Cat.parse ss.categories)
    and n_cats = (List.length ss.categories)
    and unary_rules = Cat.read_unary_rules (model </> "unary_rules.txt")
    and seen_rules = Cat.read_binary_rules (model </> "seen_rules.txt") in
    let cat_dict = Cat.read_cat_dict ss.categories (model </> "cat_dict.txt") in
    Printf.eprintf status (List.length ss.seeds)
        n_cats (Hashtbl.length unary_rules) (Hashtbl.length seen_rules)
        (Hashtbl.length cat_dict) !nbest !beta !out;
    flush stderr;
    let t = Sys.time () in
    let res = progress_map ss.seeds
            ~f:(fun s -> EnAstarParser.parse (read_proto_matrix n_cats s)
            ~cat_list ~unary_rules ~seen_rules ~cat_dict
            ~nbest:(!nbest) ~beta:(!beta) ~unary_penalty:0.1 ()) in
    Printf.eprintf "\nExecution time: %fs\n" (Sys.time() -. t);
    output_results res
