
open Thorn
open Ccg_seed_types
open Utils

module EnCat = Cat.EnglishCategories
module EnGrammar = Grammar.EnglishGrammar
module EnAstarParser = Astar.MakeAStarParser (EnGrammar)
module EnPrinter = Printer.EnglishPrinter
module EnLoader = Reader.EnglishLoader

let parse_format s =
    if List.mem s ["auto"; "deriv"; "html"; "ptb"; "prolog"; "htmls"; "translation"]
    then s else raise (Invalid_argument s)
   

type cfg = {
    format : string [@short "-f"] [@parse parse_format]
        (** output format: [auto, deriv, html, ptb, prolog, htmls, translation] *)
} [@@deriving argparse {
    positional = ["dirs ...", "directories to traverse"];
    description = "parse CCGBank and output formatted trees"
}] 


let make_translations =
    let f = function
        | [] -> failwith "unexpected zero length parses in make_translations"
        | (_, p) :: _ ->
            let words = String.concat " " (EnGrammar.Tree.terminals p) in
            Printf.printf "<s> %s </s> ||| <s> %s </s>\n" words (EnPrinter.show_tree p) in
    List.iter f


let write_all out names attribs parses = match out with
    | "translation" -> make_translations parses
    | out -> EnPrinter.output_results out names attribs parses

let main out dir =
    let paths = Utils.walk_directory_tree dir ".*\\.auto" in
    let names, parses = List.fold_right (fun p (names, parses) ->
        let name, parse = Reader.CCGBank.parse_file p in
        (name @ names, parse @ parses)) paths ([], []) in
    let attribs = List.map Attributes.default parses in
    write_all out names attribs
    (List.map EnGrammar.Tree.make_scored parses)

let () =
    let { format }, dirs = argparse_cfg { format = "auto" } "ccgbank" Sys.argv in
    List.iter (main format) (Array.to_list dirs)
