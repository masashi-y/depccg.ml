
open Ccg_seed_types
open Utils
module Category = Cat.EnglishCategories
module Grammar = Grammar.EnglishGrammar
module EnAstarParser = Astar.MakeAStarParser (Grammar)
module Tree = EnAstarParser.Tree
module L =
struct
    include Reader.Loader (Category)
    open Category

    let read_binary_rules file =
        let res = Hashtbl.create 2000 in
        let aaa : Category.feature list = [`Var; `Nb] in
        let parse' c = Category.remove_some_feat aaa (Category.parse c) in
        let add_entry k v = Hashtbl.add res (parse' k, parse' v) true in
        let scan l = Scanf.sscanf l "%s %s" add_entry in
        read_lines file |> List.filter not_comment |> List.iter scan;
        res
end

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

let () =
    let () = Arg.parse spec (fun s -> paths := s :: !paths) usage in
    let (model, seeds) = match !paths with
        |[m; s] -> (m, s)
        | _ -> Arg.usage spec usage; exit 1;
    in
    let ss = L.read_ccgseeds seeds in
    let cat_list = Utils.enumerate (List.map Category.parse ss.categories)
    and n_cats = (List.length ss.categories)
    and unary_rules = L.read_unary_rules (model </> "unary_rules.txt")
    and seen_rules = L.read_binary_rules (model </> "seen_rules.txt") in
    let cat_dict = L.read_cat_dict ss.categories (model </> "cat_dict.txt") in
    Printf.eprintf status (List.length ss.seeds)
        n_cats (Hashtbl.length unary_rules) (Hashtbl.length seen_rules)
        (Hashtbl.length cat_dict) !nbest !beta !out;
    flush stderr;
    let t = Sys.time () in
    let res = progress_map ss.seeds
            ~f:(fun s -> EnAstarParser.parse (Reader.read_proto_matrix n_cats s)
            ~cat_list ~unary_rules ~seen_rules ~cat_dict
            ~nbest:(!nbest) ~beta:(!beta) ~unary_penalty:0.1 ()) in
    Printf.eprintf "\nExecution time: %fs\n" (Sys.time() -. t);
    output_results res
