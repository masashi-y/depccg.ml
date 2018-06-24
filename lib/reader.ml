
open Utils
open Cat
open Attributes

type file = string

module type LOADER =
sig
    type cat

    val read_ccgseeds : string -> Ccg_seed_types.ccgseeds

    val read_ccgseeds_bytes : Bytes.t -> Ccg_seed_types.ccgseeds

    val load_ccgseeds : file -> Ccg_seed_types.ccgseeds

    val load_ccgseeds_socket : file -> file -> Ccg_seed_types.ccgseeds

    val read_cats : file -> cat list

    val read_unary_rules : file -> (cat, cat) Hashtbl.t

    val read_cat_dict : string list -> file -> (string, bool array) Hashtbl.t

    val read_binary_rules : file -> (cat * cat, bool) Hashtbl.t
end

module Loader (Cat : CATEGORIES) =
struct
    type cat = Cat.t

    let read_ccgseeds_bytes bytes =
        Ccg_seed_pb.decode_ccgseeds (Pbrt.Decoder.of_bytes bytes)

    let read_ccgseeds string =
        read_ccgseeds_bytes (Bytes.of_string string)

    let load_ccgseeds file =
        let bytes = 
            let ic = open_in file in 
            let len = in_channel_length ic in 
            let bytes = Bytes.create len in 
            really_input ic bytes 0 len; 
            close_in ic; 
            bytes 
        in read_ccgseeds_bytes bytes

    let recvall sock =
        let len = input_binary_int sock in
        let buf = Bytes.create len in
        really_input sock buf 0 len;
        buf

    let load_ccgseeds_socket socket filename =
        let lines = String.concat "\n" (read_lines filename) in
        let input = Bytes.of_string lines in
        let sock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
        let addr = Unix.ADDR_UNIX socket in
        Unix.connect sock addr;
        let sock_out = Unix.out_channel_of_descr sock in
        let sock_in = Unix.in_channel_of_descr sock in
        let input_length = Bytes.length input in
        output_binary_int sock_out input_length;
        output_bytes sock_out input;
        flush sock_out;
        let bytes = recvall sock_in in
        Ccg_seed_pb.decode_ccgseeds (Pbrt.Decoder.of_bytes bytes)

    let read_cats file =
        let scan l = Scanf.sscanf l "%s %i" (fun s _ -> (Cat.parse s))
        in List.map scan (read_lines file)

    let not_comment s = if String.length s = 0 then false
        else match s.[0] with
        | '#' -> false
        | _ -> true

    let read_unary_rules file =
        let res = Hashtbl.create 20 in
        let add_entry k v = Hashtbl.add res (Cat.parse k) (Cat.parse v) in
        let parse l = Scanf.sscanf l "%s %s" add_entry in
        read_lines file |> List.filter not_comment |> List.iter parse;
        res

    let read_cat_dict cat_list file =
        let res = Hashtbl.create 7000 in
        let scan l = match String.split_on_char '\t' l with
        | w :: cs -> let v = List.map (fun c -> List.mem c cs) cat_list in
                     Hashtbl.add res w (Array.of_list v)
        | _ -> invalid_arg "read_cat_dict" in
        read_lines file |> List.filter not_comment |> List.iter scan;
        res

    let read_binary_rules file =
        let res = Hashtbl.create 2000 in
        let add_entry k v = Hashtbl.add res (Cat.parse k, Cat.parse v) true in
        let scan l = Scanf.sscanf l "%s %s" add_entry in
        read_lines file |> List.filter not_comment |> List.iter scan;
        res
end

module EnglishLoader =
struct
    module Cat = EnglishCategories
    include Loader (Cat)

    let read_binary_rules file =
        let res = Hashtbl.create 2000 in
        let parse' c = Cat.remove_some_feat [`Var; `Nb] (Cat.parse c) in
        let add_entry k v = Hashtbl.add res (parse' k, parse' v) true in
        let scan l = Scanf.sscanf l "%s %s" add_entry in
        read_lines file |> List.filter not_comment |> List.iter scan;
        res
end

module JapaneseLoader = Loader (JapaneseCategories)

type matrix = float Matrix.t

let read_proto_matrix n_cats = 
    let open Ccg_seed_types in
    let error () = failwith "error in read_proto_matrix" in
    let convert = function
        | {values=v; shape=[i; j]} -> Matrix.reshape (Matrix.of_list v) (i, j)
        | _ -> error ()
    in fun {sentence; cat_probs; dep_probs;}
        -> (sentence, convert cat_probs, convert dep_probs)


module CCGBank = struct
    open Grammar
    open EnglishGrammar

    let preprocess s =
        Str.(let regex = regexp "[<>]" in
            let s' = global_replace regex " " s in
            split (regexp " +") s')

    let parse_line str =
        let error () = invalid_arg (!%"failed to parse %s\n" str) in
        let rec parse' stack poss = function
            | [] -> begin match stack with
               | [`Close x] -> (Attributes.of_list @@ List.rev poss), x
               | _ -> error ()
            end
            | "(" :: "L" :: cat :: pos :: _ :: word :: _ :: ")" :: rest ->
               let n = Tree.terminal (Cat.parse cat) word in
               parse' (`Close n :: stack) (Attribute.default ~pos () :: poss) rest
            | "(" :: "T" :: cat :: _ :: _ :: rest ->
                let n children = Tree.make ~cat:(Cat.parse cat) ~op:`Intro ~children in
                parse' (`Open n :: stack) poss rest
            | ")" :: rest -> begin match stack with
                | `Close l2 :: `Close l1 :: `Open t :: ss ->
                   let n = t [l1; l2] in
                   parse' (`Close n :: ss) poss rest
                | `Close l :: `Open t :: ss ->
                   let n = t [l] in
                   parse' (`Close n :: ss) poss rest
                | _ -> error ()
            end
            | _ -> error () in
        parse' [] [] (preprocess str)

    let parse_file file =
        let rec aux ((names, attrs, parses) as acc) = function
            | [] -> acc
            | name :: line :: rest ->
                Scanf.sscanf name "ID=%s@ " (fun name ->
                    let names, attrs, parses = aux acc rest in
                    let line = Str.(global_replace (regexp "\\[\\([a-z]+\\)\\]\\[[a-z]+\\]") "[\\1]" line) in
                    let line = Str.(global_replace (regexp ")\\[conj\\]") ")" line) in
                    let attr, parse = try parse_line line
                        with Parse_error s -> invalid_arg (!%"%s :%s" name s) in
                    (Some name :: names, attr :: attrs, parse :: parses))
            | _ -> invalid_arg "CCGBank.parse_file"
        in aux ([], [], []) (read_lines file)
end

module CAndCXML =
struct
    open Grammar
    open EnglishGrammar

    let parse_type = function
        | "fa"   -> `FwdApp
        | "ba"   -> `BwdApp
        | "fc"   -> `FwdCmp
        | "bx"   -> `BwdCmp
        | "gfc"  -> `GenFwdCmp
        | "gbx"  -> `GenBwdCmp
        | "conj" -> `Conj
        | "tr" | "lex" -> `Unary
        | "rp" | "lp" -> `RP
        | s -> failwith (!%"unknown type in ccg %s" s)

    let rec parse_terminal xml_node =
        let f (attr, cat, word) = function
            | ("lemma",  v) -> (Attribute.update attr (`Lemma  v), cat, word)
            | ("pos",    v) -> (Attribute.update attr (`Pos    v), cat, word)
            | ("chunk",  v) -> (Attribute.update attr (`Chunk  v), cat, word)
            | ("entity", v) -> (Attribute.update attr (`Entity v), cat, word)
            | ("cat",    v) -> (attr, Some (EnglishCategories.parse v), word)
            | ("word",   v) -> (attr, cat, Some v)
            | _ -> (attr, cat, word) in
        Xml.attribs xml_node |>
        List.fold_left f (Attribute.default (), None, None) |> AttributeM.(function
            | attr, Some cat, Some word ->
                push attr >>= fun () ->
                    return @@ Tree.terminal cat word
            | _ -> failwith "failed to parse terminal node")

    and parse_non_terminal xml_node =
        let f (cat, op) = function
            | ("cat",  v) -> (Some (EnglishCategories.parse v), op)
            | ("type", v) -> (cat, Some v)
            | _ -> (cat, op) in
        Xml.attribs xml_node |>
        List.fold_left f (None, None) |> AttributeM.(function
            | (Some cat, Some op) ->
                parse_type op |> fun op ->
                mapM parse_node (Xml.children xml_node) >>= fun children ->
                    return @@ Tree.make ~cat ~op ~children
            | _ -> failwith "failed to parse non-terminal node")

    and parse_node xml_node =
        Xml.tag xml_node |> function
        | "lf"   -> parse_terminal xml_node
        | "rule" -> parse_non_terminal xml_node
        | s -> failwith (!%"unknown xml node in ccg: %s" s)

    let parse_tree = function
        | Xml.Element ("ccg", _, [node]) ->
            let t, attr = AttributeM.run (parse_node node) (Attributes.default ()) in
            (t, Attributes.rev attr)
        | _ -> failwith "unknown xml node in candc"

    let parse_file file =
        match Xml.parse_file file with
        | Xml.Element ("candc", _, nodes) ->
            let f node (ts, attrs) =
                let t, attr = parse_tree node in
                (t :: ts, attr :: attrs) in
            List.fold_right f nodes ([], [])
        | _ -> failwith "unknown xml node in candc"

end
(*
let () =
    let paths = read_lines "path" in
    let names, parses = List.fold_right (fun p (names, parses) ->
        let name, parse = CCGBank.parse_file p in
        (name @ names, parse @ parses)) paths ([], []) in
    let attribs = List.map (fun _ -> [None]) names in
    Printer.EnglishPrinter.output_results "htmls" names attribs
     (List.map Grammar.EnglishGrammar.Tree.make_scored parses)
*)

(*
let () =
    let trees, attrs = CAndCXML.parse_file "res.xml" in
    Printer.EnglishPrinter.output_results "xml" [] attrs
     (List.map Grammar.EnglishGrammar.Tree.make_scored trees)
*)
