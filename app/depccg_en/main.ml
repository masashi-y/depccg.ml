
open Thorn
open Ccg_seed_types
open Utils

module EnCat = Cat.EnglishCategories
module EnGrammar = Grammar.EnglishGrammar
module EnAstarParser = Astar.MakeAStarParser (EnGrammar)
module EnPrinter = Printer.EnglishPrinter
module EnLoader = Reader.EnglishLoader


let parse_format s =
    if List.mem s ["auto"; "deriv"; "html"; "ptb"; "prolog"; "htmls"]
    then s else raise (Invalid_argument s)

type cfg = {
    input : string;        [@short "-i"]
        (** input file (seed file (.seeds) *)

    model : string;        [@short "-m"]
        (** path to model directory *)

    nbest : int;           [@short "-n"]
        (** output nbest parses *)

    beta : float;          [@short "-b"]
        (** beta value for pruning *)

    format : string;       [@short "-f"] [@parse parse_format]
        (** output format: [auto, deriv, html, ptb, prolog, htmls] *)

    socket : string option;[@short "-S"]
        (** use socket to contact with tagger *)

    ncores : int           [@short "-c"]
        (** the number of cores to parallelize A* decoders *)
} [@@deriving argparse]

let default = {
    input = "";
    model = "";
    nbest = 1;
    beta = 0.00000001;
    format = "auto";
    socket = None;
    ncores = 4;
}

let status =
"[parser] Camelthorn CCG Parser\n"              ^^
"[parser] input document size:\t%i\n"           ^^
"[parser] number of terminal categories:\t%i\n" ^^
"[parser] unary rule size:\t%i\n"               ^^
"[parser] seen rule size:\t%i\n"                ^^
"[parser] category dictionary size:\t%i\n"      ^^
"[parser] nbest:\t%i\n"                         ^^
"[parser] beta:\t%e\n"                          ^^
"[parser] output format:\t%s\n"                 ^^
"[parser] the number of cores:\t%d\n"

let progress_map ncores ~f lst =
    let size = List.length lst in
    let f' i x = let y = f x in
        Printf.eprintf "\b\r[parser] %i/%i" (i+1) size;
        flush stderr;
        y
    in let res = Parmap.(parmapi ~ncores f' (L lst)) in
    Printf.eprintf "\b\r[parser] done         \n";
    res

let try_load name f =
    try let res = f () in
        (Some res, Hashtbl.length res)
    with Sys_error _ ->
        prerr_endline ("[parser] " ^ name ^ " not found. Using empty one");
        (None, 0)

let tag ~lib ~model sents =
    let sents = String.concat "\n" sents in
    Shexp_process.(eval Infix.(echo sents |- 
        run "env" ["PYTHONPATH=" ^ lib; "python"; "-W"; "ignore"; lib </> "tagger.py"; model] |- read_all))


let () =
    let {input; model; socket; nbest; beta; format; ncores}, _ = argparse_cfg default "depccg_en" Sys.argv in
    let ParserConfig.{model = def_model; lib} = ParserConfig.load_en () in
    let model = match model with
        | "" -> def_model
        | _ -> model in
    let sents = match input with
        | "" -> Utils.read_stdin ()
        | v -> Utils.read_lines input in
    let res = tag ~lib ~model sents in
    let ss = Ccg_seed_pb.decode_ccgseeds (Pbrt.Decoder.of_bytes (Bytes.of_string res)) in
    let cat_list = Utils.enumerate (List.map EnCat.parse ss.categories)
    and n_cats = (List.length ss.categories)
    and unary_rules = EnLoader.read_unary_rules (model </> "unary_rules.txt") in
    let seen_rules, seen_rules_size = 
        try_load "seen rules" (fun () -> EnLoader.read_binary_rules (model </> "seen_rules.txt")) in
    let cat_dict, cat_dict_size =
        try_load "cat dict" (fun () -> EnLoader.read_cat_dict ss.categories (model </> "cat_dict.txt")) in
    Printf.eprintf status (List.length ss.seeds)
            n_cats (Hashtbl.length unary_rules)
            seen_rules_size cat_dict_size nbest beta format ncores;
    flush stderr;
    let t = Sys.time () in
    let attribs = List.map Attributes.of_protobuf ss.seeds in
    let names = List.map (fun s -> s.id) ss.seeds in
    let res = progress_map ncores ss.seeds
            ~f:(fun s -> EnAstarParser.parse (Reader.read_proto_matrix n_cats s)
            ~cat_list ~unary_rules ~seen_rules ~cat_dict
            ~nbest ~beta ~unary_penalty:0.1 ()) in
    Printf.eprintf "\nExecution time: %fs\n" (Sys.time() -. t);
    EnPrinter.output_results format names attribs res

