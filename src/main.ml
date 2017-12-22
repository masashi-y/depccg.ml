
open Ccg_seed_types
open Utils

module EnCat = Cat.EnglishCategories
module EnGrammar = Grammar.EnglishGrammar
module EnAstarParser = Astar.MakeAStarParser (EnGrammar)
module EnPrinter = Printer.ParsePrinter (EnGrammar)
module EnLoader = Reader.EnglishLoader


module JaCat = Cat.JapaneseCategories
module JaGrammar = Grammar.JapaneseGrammar
module JaAstarParser = Astar.MakeAStarParser (JaGrammar)
module JaPrinter = Printer.ParsePrinter (JaGrammar)
module JaLoader = Reader.JapaneseLoader


let (</>) = Filename.concat

let paths = ref []
and nbest = ref 1
and beta  = ref 0.00000001
and out   = ref "auto"
and lang  = ref "en"

let spec = 
    [("-nbest",  Arg.Set_int nbest,   "output nbest parses")
    ;("-beta",   Arg.Set_float beta,  "beta value for pruning")
    ;("-format", Arg.Set_string out,  "beta value for pruning")
    ;("-lang",   Arg.Set_string lang, "language [en, ja]")
    ]

let usage = !%"\n%sUsage: thorn [-nbest] [-beta] [-format] [-lang] model seeds"
            EnPrinter.(show_derivation sample_tree)

let valid_format s = List.mem s ["auto"; "deriv"; "html"; "ptb"]

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
    let f' i x = let y = f x in
        Printf.eprintf "\b\r[parser] %i/%i" (i+1) size;
        flush stderr;
        y
    in let res = List.mapi f' lst in
    Printf.eprintf "\b\r[parser] done         \n";
    res

let try_load name f =
    try let res = f () in
        (Some res, Hashtbl.length res)
    with Sys_error _ ->
        prerr_endline ("[parser] " ^ name ^ " not found. Using empty one");
        (None, 0)

(* main function for English parsing *)
let main_en model seeds =
    let ss = EnLoader.read_ccgseeds seeds in
    let cat_list = Utils.enumerate (List.map EnCat.parse ss.categories)
    and n_cats = (List.length ss.categories)
    and unary_rules = EnLoader.read_unary_rules (model </> "unary_rules.txt") in
    let seen_rules, seen_rules_size = 
        try_load "seen rules" (fun () -> EnLoader.read_binary_rules (model </> "seen_rules.txt")) in
    let cat_dict, cat_dict_size =
        try_load "cat dict" (fun () -> EnLoader.read_cat_dict ss.categories (model </> "cat_dict.txt")) in
    Printf.eprintf status (List.length ss.seeds)
            n_cats (Hashtbl.length unary_rules)
            seen_rules_size cat_dict_size !nbest !beta !out;
    flush stderr;
    let t = Sys.time () in
    let res = progress_map ss.seeds
            ~f:(fun s -> EnAstarParser.parse (Reader.read_proto_matrix n_cats s)
            ~cat_list ~unary_rules ~seen_rules ~cat_dict
            ~nbest:(!nbest) ~beta:(!beta) ~unary_penalty:0.1 ()) in
    Printf.eprintf "\nExecution time: %fs\n" (Sys.time() -. t);
    EnPrinter.output_results !out res


(* main function for Japanese parsing *)
let main_ja model seeds =
    let ss = JaLoader.read_ccgseeds seeds in
    let cat_list = Utils.enumerate (List.map JaCat.parse ss.categories)
    and n_cats = (List.length ss.categories)
    and unary_rules = JaLoader.read_unary_rules (model </> "unary_rules.txt") in
    let seen_rules, seen_rules_size = 
        try_load "seen rules" (fun () -> JaLoader.read_binary_rules (model </> "seen_rules.txt")) in
    Printf.eprintf status (List.length ss.seeds)
            n_cats (Hashtbl.length unary_rules)
            seen_rules_size 0 !nbest !beta !out;
    flush stderr;
    let t = Sys.time () in
    let res = progress_map ss.seeds
            ~f:(fun s -> JaAstarParser.parse (Reader.read_proto_matrix n_cats s)
            ~cat_list ~unary_rules ~seen_rules ~cat_dict:None
            ~nbest:(!nbest) ~beta:(!beta) ~unary_penalty:0.1 ()) in
    Printf.eprintf "\nExecution time: %fs\n" (Sys.time() -. t);
    JaPrinter.output_results !out res


let () =
    let () = Arg.parse spec (fun s -> paths := s :: !paths) usage in
    let (model, seeds) = match !paths with
        |[m; s] -> (m, s)
        | _ -> Arg.usage spec usage; exit 1;
    in
    match !lang with
    | "en" -> main_en model seeds
    | "ja" -> main_ja model seeds
    | _ -> failwith (!%"Not supported language option: %s\n" !lang)
