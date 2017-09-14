

module S = String
module SS = Str
module L = List
open Utils

type feat = string

type cat = S of feat
         | N of feat
         | NP of feat
         | PP of feat
         | Fwd of cat * cat
         | Bwd of cat * cat
         | Punct of string

let string_feat = function
    | "" -> ""
    | f  -> !%"[%s]" f

let rec string_cat ?(bracket=false) = function
      S (feat)       -> "S" ^ string_feat feat
    | N (feat)       -> "N" ^ string_feat feat
    | NP (feat)      -> "NP" ^ string_feat feat
    | PP (feat)      -> "PP" ^ string_feat feat
    | Fwd (res, arg)
    -> !%(if bracket then "(%s/%s)" else "%s/%s")  (string_cat ~bracket:true res) (string_cat ~bracket:true arg)
    | Bwd (res, arg)
    -> !%(if bracket then "(%s\\%s)" else "%s\\%s") (string_cat ~bracket:true res) (string_cat ~bracket:true arg)
    | Punct (str)    -> str

type token = Cat of cat
           | Slash of (cat -> cat -> cat)

exception Parse_error of string

let normalize s = let regex = SS.regexp "\\([]\\[()/\\\\]\\)" in
                  let s' = SS.global_replace regex " \\1 " s in
                  SS.split (SS.regexp " +") s'

let atom c f = match c with
    | "S"   -> S f
    | "N"   -> N f
    | "NP"  -> NP f
    | "PP"  -> PP f
    | _ -> raise (Parse_error c)

let parse_cat str =
    let fwd res arg = Fwd (res, arg)
    and bwd res arg = Bwd (res, arg) in
    (* shift-reduce parser *)
    let rec parse stack = function
        | [] -> begin
           (* consumed all the input *)
           match stack with
           | [Cat res] -> res
           | [Cat arg; Slash f; Cat res] -> f res arg
           | _ -> raise (Parse_error str)
        end
        | head :: rest -> begin
            match head with
            | "," | "." | ";" | ":" | "LRB" | "RRB" | "conj" as s
                -> parse (Cat (Punct s) :: stack) rest
            | "S" | "N" | "NP" | "PP" as s
            -> begin
                (* see if a feature value follows *)
                match rest with
                "[" :: feat :: "]" :: rest'
                    -> parse (Cat (atom s feat) :: stack) rest'
                | _ -> parse (Cat (atom s "") :: stack) rest
               end
            | "(" -> parse stack rest
            | ")" -> begin
                (* reduce top three items into a cat *)
                match stack with
                  Cat arg :: Slash f :: Cat res :: ss
                    -> parse ((Cat (f res arg)) :: ss) rest
                | _ -> raise (Parse_error str)
               end
            | "/"  -> parse (Slash fwd :: stack) rest
            | "\\" -> parse (Slash bwd :: stack) rest
            | _ -> raise (Parse_error str)
        end
    in parse [] (normalize str)

let read_cats file =
    let parse l = Scanf.sscanf l "%s %i" (fun s _ -> (parse_cat s))
    in L.map parse (read_lines file)

let cats = read_cats "/Users/masashi-y/depccg/models/tri_headfirst/target.txt"

let is_type_raised = function
    | Fwd (res, arg) -> res = arg
    | Bwd (res, arg) -> res = arg
    | _ -> false

let is_punct = function
    | Punct _ -> true
    | _ -> false

type combinator =
      FwdApp
    | BwdApp
    | FwdCmp
    | BwdCmp
    | GenFwdCmp
    | GenBwdCmp
    | CNJ            (* Conjunction *)
    | LRP            (* LeftRemovePunctuation *)
    | RRP            (* RightRemovePunctuation *)
    | CommaVPtoADV   (* CommaAndVerbPhraseToAdverb *)
    | Intro
    | Unary

let string_combinator = function
    | FwdApp       -> ">"
    | BwdApp       -> "<"
    | FwdCmp       -> ">B"
    | BwdCmp       -> "<B"
    | GenFwdCmp    -> ">B1"
    | GenBwdCmp    -> "<B1"
    | CNJ          -> "<P>"
    | LRP          -> "<rp>"
    | RRP          -> "<rp>"
    | CommaVPtoADV -> "<*>"
    | Intro        -> "<*>"
    | Unary        -> "<u>"

let rec remove_feat = function
    | S _            -> S ""
    | N _            -> N ""
    | NP _           -> NP ""
    | PP _           -> PP ""
    | Fwd (res, arg) -> Fwd (remove_feat res, remove_feat arg)
    | Bwd (res, arg) -> Bwd (remove_feat res, remove_feat arg)
    | c              -> c

let () =
    (* L.iter (fun c -> string_cat c |> print_endline) cats *)
    let bytes = 
        let ic = open_in "ccg.seeds" in 
        let len = in_channel_length ic in 
        let bytes = Bytes.create len in 
        really_input ic bytes 0 len; 
        close_in ic; 
        bytes 
    in 
    let res = Ccg_seed_pb.decode_ccgseeds (Pbrt.Decoder.of_bytes bytes) in
    print_endline res.lang;
    print_endline (S.concat " " res.categories);
    L.iter (fun x -> print_endline (!%"%f" x)) (let h = L.hd res.seeds in h.cat_probs);
    (* L.iter print_endline res.categories *)
