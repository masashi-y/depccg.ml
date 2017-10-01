

open Utils

module Feature =
struct
    type t = [ `None
             | `Nb
             | `Var of int
             | `Con of string]

    let equal = function
        | (`None | `Nb), _
        | _, (`None | `Nb) -> true
        | `Var i, `Var i'  -> i = i'
        | `Con x, `Con y   -> x = y
        | _ -> false

    let show = function
        | `None  -> ""
        | `Nb    -> "[nb]"
        | `Var _ -> "[X]" (* TODO *)
        | `Con f -> !%"[%s]" f

    let parse = function
        | "[" :: feat :: "]" :: rest
            -> let feat' = begin match feat with
                | "nb" -> `Nb
                | "X"  -> `Var 0
                | f    -> `Con f
               end in (feat', rest)
        | rest -> (`None, rest)

end


type t = [ `S of Feature.t
         | `N of Feature.t
         | `NP of Feature.t
         | `PP of Feature.t
         | `Fwd of t * t
         | `Bwd of t * t
         | `Punct of string]

let s = `S `None
and n = `N `None
and np = `NP `None
and pp = `PP `None
and (/:) x y = `Fwd (x, y)
and (|:) x y = `Bwd (x, y)

let rec (=:=) a b = match (a, b) with
    | `Fwd (x, y), `Fwd (x', y')
    | `Bwd (x, y), `Bwd (x', y') -> x =:= x' && y =:= y'
    | `S f       , `S f'
    | `N f       , `N f'
    | `NP f      , `NP f'
    | `PP f      , `PP f' -> Feature.equal (f, f')
    | `Punct s   , `Punct s' -> s = s'
    | _ -> false

let rec show_cat ?(bracket=false) = function
      `S feat  -> "S" ^ Feature.show feat
    | `N feat  -> "N" ^ Feature.show feat
    | `NP feat -> "NP" ^ Feature.show feat
    | `PP feat -> "PP" ^ Feature.show feat
    | `Fwd (x, y)
    -> !%(if bracket then "(%s/%s)" else "%s/%s")  (show_cat ~bracket:true x) (show_cat ~bracket:true y)
    | `Bwd (x, y)
    -> !%(if bracket then "(%s\\%s)" else "%s\\%s") (show_cat ~bracket:true x) (show_cat ~bracket:true y)
    | `Punct (str)    -> str

type token = Cat of t
           | Slash of (t -> t -> t)

exception Parse_error of string

let preprocess s = let regex = Str.regexp "\\([]\\[()/\\\\]\\)" in
                   let s' = Str.global_replace regex " \\1 " s in
                   Str.split (Str.regexp " +") s'

(* let preprocess s =                                    *)
(*     let open Buffer in                                *)
(*     let buf = create 50 in                            *)
(*     let add = add_char buf in                         *)
(*     let len = String.length s in                      *)
(*     let rec iter i = begin                            *)
(*             match s.[i] with                          *)
(*             | '(' | '[' -> add ' '                    *)
(*             | ')' | ']' | '\\' | '/' as c ->          *)
(*                 add ' '; add c; add ' '               *)
(*             | c -> add c                              *)
(*         end;                                          *)
(*         if i + 1 < len then iter (i+1);               *)
(*     in iter 0;                                        *)
(*     let res = String.split_on_char ' ' (contents buf) *)
(*     in List.iter (p "%s ") res; res                   *)

let atom c f = match c with
    | "S"   -> `S f
    | "N"   -> `N f
    | "NP"  -> `NP f
    | "PP"  -> `PP f
    | _ -> raise (Parse_error c)

(* (X\Y)/Y (X/Y)\Y *)
let is_type_raised = function
    | `Fwd (`Bwd (x, y), y')
    | `Bwd (`Fwd (x, y), y') -> y = y'
    | _ -> false

let is_punct = function
    | `Punct _ -> true
    | _ -> false

let rec remove_all_feat = function
    | `S _        -> s
    | `N _        -> n
    | `NP _       -> np
    | `PP _       -> pp
    | `Fwd (x, y) -> remove_all_feat x /: remove_all_feat y
    | `Bwd (x, y) -> remove_all_feat x |: remove_all_feat y
    | c           -> c

let rec remove_some_feat feats = 
    let open List in function
    | `S f when mem f feats  -> s
    | `N f when mem f feats  -> n
    | `NP f when mem f feats -> np
    | `PP f when mem f feats -> pp
    | `Fwd (x, y)
        -> remove_some_feat feats x /: remove_some_feat feats y
    | `Bwd (x, y)
        -> remove_some_feat feats x |: remove_some_feat feats y
    | c -> c

let parse_cat str =
    (* shift-reduce parser *)
    let rec parse stack = function
        | [] -> begin
           (* consumed all the input *)
           match stack with
           | [Cat x] -> x
           | [Cat y; Slash f; Cat x] -> f x y
           | _ -> raise (Parse_error str)
        end
        | head :: rest -> begin
            match head with
            | "," | "." | ";" | ":" | "LRB" | "RRB" | "conj" as s
                -> parse (Cat (`Punct s) :: stack) rest
            | "S" | "N" | "NP" | "PP" as s
            -> (* see if a feature value follows *)
                let (feat, rest') = Feature.parse rest in
                parse (Cat (atom s feat) :: stack) rest'
            | "(" -> parse stack rest
            | ")" -> begin
                (* reduce top three items into a cat *)
                match stack with
                  Cat y :: Slash f :: Cat x :: ss
                    -> parse ((Cat (f x y)) :: ss) rest
                | _ -> raise (Parse_error str)
               end
            | "/"  -> parse (Slash (/:) :: stack) rest
            | "\\" -> parse (Slash (|:) :: stack) rest
            | _ -> raise (Parse_error str)
        end
    in parse [] (preprocess str)

let read_ccgseeds file =
    let bytes = 
        let ic = open_in file in 
        let len = in_channel_length ic in 
        let bytes = Bytes.create len in 
        really_input ic bytes 0 len; 
        close_in ic; 
        bytes 
    in Ccg_seed_pb.decode_ccgseeds (Pbrt.Decoder.of_bytes bytes)

let read_cats file =
    let parse l = Scanf.sscanf l "%s %i" (fun s _ -> (parse_cat s))
    in List.map parse (read_lines file)

let not_comment s = if String.length s = 0 then false
    else match s.[0] with
    | '#' -> false
    | _ -> true

let read_unary_rules file =
    let res = Hashtbl.create 20 in
    let add_entry k v = Hashtbl.add res (parse_cat k) (parse_cat v) in
    let parse l = Scanf.sscanf l "%s %s" add_entry in
    read_lines file |> List.filter (not_comment) |> List.iter parse;
    res

let read_binary_rules file =
    let res = Hashtbl.create 2000 in
    let parse_cat' c = remove_some_feat [`Var 0; `Nb] (parse_cat c) in
    let add_entry k v = Hashtbl.add res (parse_cat' k, parse_cat' v) true in
    let parse l = Scanf.sscanf l "%s %s" add_entry in
    read_lines file |> List.filter (not_comment) |> List.iter parse;
    res

