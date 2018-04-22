
open Thorn
open Ccg_seed_types
open Utils
open Common

module JaCat = Cat.JapaneseCategories
module JaGrammar = Grammar.JapaneseGrammar
module JaAstarParser = Astar.MakeAStarParser (JaGrammar)
module JaPrinter = Printer.ParsePrinter (JaGrammar)
module JaLoader = Reader.JapaneseLoader


let parse_format s =
    if List.mem s ["auto"; "conll"; "deriv"; "html"; "ptb"; "htmls"]
    then s else raise (Invalid_argument s)

type cfg = {
    input : string option; [@short "-i"]
        (** input file (txt file or seed file (.seeds) *)

    model : string option; [@short "-m"]
        (** path to model directory *)

    nbest : int;           [@short "-n"]
        (** output nbest parses *)

    beta : float;          [@short "-b"]
        (** beta value for pruning *)

    format : string;       [@short "-f"] [@parse parse_format]
        (** output format: [auto, deriv, html, ptb, htmls, conll] *)

    socket : string option;[@short "-S"]
        (** use socket to contact with tagger *)

    ncores : int;          [@short "-c"]
        (** the number of cores to parallelize A* decoders *)

    verbose : bool;        [@short "-v"]
        (** show all messages *)
} [@@deriving argparse]

let default = {
    input = None;
    model = None;
    nbest = 1;
    beta = 0.00000001;
    format = "auto";
    socket = None;
    ncores = 4;
    verbose = false;
}

let status =
"[parser] ***** depccg *****\n"                 ^^
"[parser] input document size:\t%i\n"           ^^
"[parser] number of terminal categories:\t%i\n" ^^
"[parser] unary rule size:\t%i\n"               ^^
"[parser] seen rule size:\t%i\n"                ^^
"[parser] category dictionary size:\t%i\n"      ^^
"[parser] nbest:\t%i\n"                         ^^
"[parser] beta:\t%e\n"                          ^^
"[parser] output format:\t%s\n"                 ^^
"[parser] the number of cores:\t%d\n%!"

let tag ~lib ~model ~warn sents =
    let script = [lib </> "tagger.py"] in
    let warn = if warn then [] else ["-W"; "ignore"] in
    let args = [model] in
    let sents = String.concat "\n" sents in
    Shexp_process.(eval Infix.(echo sents |- 
        run "env" (["PYTHONPATH=" ^ lib; "python"] @ warn @ script @ args) |- read_all))
    |> Bytes.of_string |> Pbrt.Decoder.of_bytes |> Ccg_seed_pb.decode_ccgseeds
    


let () =
    let {input; model; socket; nbest;
            beta; format; ncores; verbose}, _ = argparse_cfg default "depccg_ja" Sys.argv in
    let {ParserConfig.model = def_model; lib} = ParserConfig.load_ja () in
    let model = CCOpt.get_or ~default:def_model model in
    let warn = verbose in
    let ss = match input with
        | None -> tag ~lib ~model ~warn (Utils.read_stdin ())
        | Some i -> begin match socket with
            | Some s ->
                Printf.eprintf "[parser] connecting socket %s\n%!" s;
                JaLoader.read_ccgseeds_socket s i
            | None -> begin
                if CCString.suffix ~suf:".seeds" i then
                   (Printf.eprintf "[parser] reading seed file %s\n%!" i;
                   JaLoader.read_ccgseeds i)
                else
                (Printf.eprintf "[parser] tagging inputs\n%!";
                tag ~lib ~model ~warn (Utils.read_lines i))
            end
        end in
    let cat_list = Utils.enumerate (List.map JaCat.parse ss.categories)
    and n_cats = (List.length ss.categories)
    and unary_rules = JaLoader.read_unary_rules (model </> "unary_rules.txt") in
    let seen_rules, seen_rules_size = 
        try_load "seen rules" (fun () -> JaLoader.read_binary_rules (model </> "seen_rules.txt")) in
    Printf.eprintf status (List.length ss.seeds)
            n_cats (Hashtbl.length unary_rules)
            seen_rules_size 0 nbest beta format ncores;
    flush stderr;
    let t = Sys.time () in
    let names = List.map (fun s -> s.id) ss.seeds in
    let res = progress_map ncores ss.seeds
            ~f:(fun s -> JaAstarParser.parse (Reader.read_proto_matrix n_cats s)
            ~cat_list ~unary_rules ~seen_rules ~cat_dict:None
            ~nbest ~beta ~unary_penalty:0.1 ()) in
    Printf.eprintf "\nExecution time: %fs\n" (Sys.time () -. t);
    JaPrinter.output_results format names res


