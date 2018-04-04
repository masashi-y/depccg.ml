
open Thorn
open Ccg_seed_types
open Utils

module EnCat = Cat.EnglishCategories
module EnGrammar = Grammar.EnglishGrammar
module EnAstarParser = Astar.MakeAStarParser (EnGrammar)
module EnPrinter = Printer.EnglishPrinter
module EnLoader = Reader.EnglishLoader

let dirs = ref []
and out  = ref "auto"

let spec = [("-format", Arg.Set_string out,  "beta value for pruning")]

let valid_format s = List.mem s ["auto"; "deriv"; "html"; "ptb"; "prolog"; "htmls"; "translation"]

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

let usage = !%"\n%sUsage: ccgbank [-f format] [dir ...]"
            EnPrinter.(show_derivation sample_tree)

let main dir =
    let paths = Utils.walk_directory_tree dir ".*\\.auto" in
    let names, parses = List.fold_right (fun p (names, parses) ->
        let name, parse = Reader.CCGBank.parse_file p in
        (name @ names, parse @ parses)) paths ([], []) in
    let attribs = List.map Attributes.default parses in
    write_all !out names attribs
    (List.map EnGrammar.Tree.make_scored parses)

let () =
    let () = Arg.parse spec (fun s -> dirs := s :: !dirs) usage in
    if List.length !dirs = 0 || not (valid_format !out)
        then (Arg.usage spec usage; exit 1)
    else List.iter main (List.rev !dirs)
