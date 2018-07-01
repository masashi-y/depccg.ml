
open Depccg
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
    positional = ["paths ...", "directories to traverse (either directory or file)"];
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

let parse_xml_with_dummy_name file =
    let attrs, parses = Reader.CAndCXML.parse_file file in
    let names = List.map (fun _ -> None) parses in
    (names, attrs, parses)

let main out path =
    let paths = if Sys.is_directory path then
            Utils.walk_directory_tree path ".*\\.\\(auto\\|xml\\)"
        else [path] in
    let names, attrs, parses = List.fold_left (fun (names, attrs, parses) p ->
        let name, attr, parse =
            if CCString.suffix ~suf:"auto" p then
                Reader.CCGBank.parse_file p
            else if CCString.suffix ~suf:"xml" p then
                parse_xml_with_dummy_name p
            else
                invalid_arg (!% "non-supported extension %s: " p)
    in
        (name @ names, attr @ attrs, parse @ parses)) ([], [], []) paths in
    write_all out names attrs (List.map EnGrammar.Tree.make_scored parses)

let () =
    let { format }, paths = argparse_cfg { format = "auto" } "ccgbank" Sys.argv in
    List.iter (main format) (Array.to_list paths)
