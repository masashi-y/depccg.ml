
open Thorn
open Ccg_seed_types
open Utils

module EnCat = Cat.EnglishCategories
module EnGrammar = Grammar.EnglishGrammar
module EnAstarParser = Astar.MakeAStarParser (EnGrammar)
module EnPrinter = Printer.EnglishPrinter
module EnLoader = Reader.EnglishLoader

let parse_format s =
    if List.mem s ["auto"; "deriv"; "html"; "ptb"; "prolog"; "htmls"; "translation"; "conll"]
    then s else raise (Invalid_argument s)
   

type cfg = {
    format : string [@short "-f"] [@parse parse_format]
        (** output format: [auto, deriv, html, ptb, prolog, htmls, translation, conll] *)
} [@@deriving argparse {
    positional = ["dirs ...", "directories to traverse"];
    description = "parse CCGBank and output formatted trees"
}] 


let make_translations attrs parses =
    let f attr = function
        | [] -> failwith "unexpected zero length parses in make_translations"
        | (_, p) :: _ ->
            let words = String.concat " " (EnGrammar.Tree.terminals p) in
            Printf.printf "<s> %s </s> ||| <s> %s </s>\n" words (EnPrinter.show_tree attr p) in
    List.iter2 f attrs parses


let write_all out names attribs parses = match out with
    | "translation" -> make_translations attribs parses
    | out -> EnPrinter.output_results out names attribs parses

let main out dir =
    let paths = Utils.walk_directory_tree dir ".*\\.auto" in
    let names, attrs, parses = List.fold_left (fun (names, attrs, parses) p ->
        let name, attr, parse = Reader.CCGBank.parse_file p in
        (name @ names, attr @ attrs, parse @ parses)) ([], [], []) paths in
    write_all out names attrs (List.map EnGrammar.Tree.make_scored parses)

let () =
    let { format }, dirs = argparse_cfg { format = "auto" } "ccgbank" Sys.argv in
    List.iter (main format) (Array.to_list dirs)
