
open Thorn
open Ccg_seed_types
open Utils
open Common

module EnCat = Cat.EnglishCategories
module EnGrammar = Grammar.EnglishGrammar
module EnAstarParser = Astar.MakeAStarParser (EnGrammar)
module EnPrinter = Printer.EnglishPrinter
module EnLoader = Reader.EnglishLoader


let parse_format s =
    if List.mem s ["auto"; "conll"; "deriv"; "html"; "ptb"; "prolog"; "htmls"; "xml"]
    then s else raise (Invalid_argument s)

let parse_annotator s =
    if List.mem s ["candc"; "spacy"]
    then Some s else raise (Invalid_argument s)

type cfg = {
    input : string option; [@short "-i"]
        (** input file (txt file or seed file (.seeds) *)

    model : string option; [@short "-m"]
        (** path to model directory *)

    annotator : string option; [@short "-a"] [@parse parse_annotator]
        (** assign POS, NER-tags and lemma using annotator [candc, spacy] *)

    nbest : int;           [@short "-n"]
        (** output nbest parses *)

    beta : float;          [@short "-b"]
        (** beta value for pruning *)

    format : string;       [@short "-f"] [@parse parse_format]
        (** output format: [auto, deriv, html, ptb, prolog, htmls, conll, xml] *)

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
    annotator = None;
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

let tag ~lib ~model ~warn ~annotator sents =
    let script = [lib </> "tagger.py"] in
    let warn = if warn then [] else ["-W"; "ignore"] in
    let args = match annotator with
        | None -> [model] 
        | Some v -> ["--annotator"; v; model] in
    let sents = String.concat "\n" sents in
    Shexp_process.(eval Infix.(echo sents |- 
        run "env" (["PYTHONPATH=" ^ lib; "python"] @ warn @ script @ args) |- read_all))
    |> Bytes.of_string |> Pbrt.Decoder.of_bytes |> Ccg_seed_pb.decode_ccgseeds
    


let () =
    let {input; model; annotator; socket; nbest;
        beta; format; ncores; verbose}, _ = argparse_cfg default "depccg_en" Sys.argv in
    let {ParserConfig.model = def_model; lib} = ParserConfig.load_en () in
    let model = CCOpt.get_or ~default:def_model model in
    let ss = match input with
        | None -> tag ~lib ~model ~annotator ~warn:verbose (Utils.read_stdin ())
        | Some i -> begin match socket with
            | Some s ->
                Printf.eprintf "[parser] connecting socket %s\n%!" s;
                EnLoader.read_ccgseeds_socket s i
            | None -> begin
                if CCString.suffix ~suf:".seeds" i then
                   (Printf.eprintf "[parser] reading seed file %s\n%!" i;
                   EnLoader.read_ccgseeds i)
                else
                (Printf.eprintf "[parser] tagging inputs\n%!";
                tag ~lib ~model ~annotator ~warn:verbose (Utils.read_lines i))
            end
        end in
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
    let t = Sys.time () in
    let attribs = List.map Attributes.of_protobuf ss.seeds in
    let names = List.map (fun s -> s.id) ss.seeds in
    let res = progress_map ncores ss.seeds
            ~f:(fun s -> EnAstarParser.parse (Reader.read_proto_matrix n_cats s)
            ~cat_list ~unary_rules ~seen_rules ~cat_dict
            ~nbest ~beta ~unary_penalty:0.1 ()) in
    Printf.eprintf "\nExecution time: %fs\n" (Sys.time() -. t);
    EnPrinter.output_results format names attribs res

