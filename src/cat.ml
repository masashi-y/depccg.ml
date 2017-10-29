

open Utils
open Feat


exception Parse_error of string

module type CATEGORIES =
sig
    type feat_t
    type t

    val s : t
    val n : t
    val np : t
    val pp : t
    val (/:) : t -> t -> t
    val (|:) : t -> t -> t
    val (=:=) : t -> t -> bool
    val show : bracket:bool -> t -> string

    val is_type_raised : t -> bool

    val is_punct : t -> bool
    val is_modifier : t -> bool
    val remove_all_feat : t -> t
    val remove_some_feat : t -> feat_t -> t
    val unify : t -> t -> t -> t
    val parse : string -> t
end

module Categories (Feature : FEATURE) =
struct

    (* type feat_t = [`None | Feature.t] *)
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


    let rec show ?(bracket=false) = function
          `S f  -> "S" ^ Feature.show f
        | `N f  -> "N" ^ Feature.show f
        | `NP f -> "NP" ^ Feature.show f
        | `PP f -> "PP" ^ Feature.show f
        | `Fwd (x, y) -> !%(if bracket then "(%s/%s)" else "%s/%s")
                                (show ~bracket:true x) (show ~bracket:true y)
        | `Bwd (x, y) -> !%(if bracket then "(%s\\%s)" else "%s\\%s")
                                (show ~bracket:true x) (show ~bracket:true y)
        | `Punct str  -> str


    let preprocess s = let regex = Str.regexp "\\([]\\[()/\\\\]\\)" in
                       let s' = Str.global_replace regex " \\1 " s in
                       Str.split (Str.regexp " +") s'

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

    let is_modifier = function
        | `Fwd (x, y)
        | `Bwd (x, y) -> x = y
        | _ -> false

    let rec remove_all_feat = function
        | `S _        -> s
        | `N _        -> n
        | `NP _       -> np
        | `PP _       -> pp
        | `Fwd (x, y) -> remove_all_feat x /: remove_all_feat y
        | `Bwd (x, y) -> remove_all_feat x |: remove_all_feat y
        | c           -> c

    let rec remove_some_feat feats = function
        | `S f when List.mem f feats  -> s
        | `N f when List.mem f feats  -> n
        | `NP f when List.mem f feats -> np
        | `PP f when List.mem f feats -> pp
        | `Fwd (x, y)
            -> remove_some_feat feats x /: remove_some_feat feats y
        | `Bwd (x, y)
            -> remove_some_feat feats x |: remove_some_feat feats y
        | c -> c

    let rec get_unification a b =
        let error () = failwith
            (!%"error in get_unification: (%s, %s)" (show a) (show b))
        in match (a, b) with
        | `S f,  `S f'
        | `N f,  `N f'
        | `NP f, `NP f'
        | `PP f, `PP f' -> Feature.unify (f, f')
        | `Punct _ , `Punct _ -> None
        | `Fwd (x, y), `Fwd (x', y')
        | `Bwd (x, y), `Bwd (x', y')
            -> let sub1 = get_unification x x'
               and sub2 = get_unification y y' in
               begin match sub1, sub2 with
                   | None, Some f
                   | Some f, None -> Some f
                   | None, None   -> None
                   | Some f, Some f' -> Some f (* discarding here f' ... *)
               end
        | _ -> error ()

    let unify c a b =
        let rec unify_feat c f = match c with
            | `S f' when Feature.is_var f' -> `S f
            | `N f' when Feature.is_var f' -> `N f
            | `NP f' when Feature.is_var f' -> `NP f
            | `PP f' when Feature.is_var f' -> `PP f
            | `Fwd (x, y) -> unify_feat x f /: unify_feat y f
            | `Bwd (x, y) -> unify_feat x f |: unify_feat y f
            | _ -> c
        in match get_unification a b with
        | Some f -> unify_feat c f
        | None -> c

    let parse str =
        (* shift-reduce parser *)
        let rec parse' stack = function
            | [] -> begin
               (* consumed all the input *)
               match stack with
               | [`Cat x] -> x
               | [`Cat y; `Slash f; `Cat x] -> f x y
               | _ -> raise (Parse_error str)
            end
            | head :: rest -> begin
                match head with
                | "," | "." | ";" | ":" | "LRB" | "RRB"
                | "conj" | "*START*" | "*END*" as s
                    -> parse' (`Cat (`Punct s) :: stack) rest
                | "S" | "N" | "NP" | "PP" as s
                -> (* see if a feature value follows *)
                    let (feat, rest') = Feature.parse rest in
                    parse' (`Cat (atom s feat) :: stack) rest'
                | "(" -> parse' stack rest
                | ")" -> begin
                    (* reduce top three items into a cat *)
                    match stack with
                      `Cat y :: `Slash f :: `Cat x :: ss
                        -> parse' ((`Cat (f x y)) :: ss) rest
                    | _ -> raise (Parse_error str)
                   end
                | "/"  -> parse' (`Slash (/:) :: stack) rest
                | "\\" -> parse' (`Slash (|:) :: stack) rest
                | _ -> raise (Parse_error str)
            end
        in parse' [] (preprocess str)

end

module EnglishCategories = Categories (EnglishFeature)

module JapaneseCategories = Categories (JapaneseFeature)

(* open JapaneseCategories *)
let ja_possible_root_cats : JapaneseCategories.t list =
    [ `NP (`NPf (`NC, `NM, `F))
    ; `NP (`NPf (`NC, `NM, `T))
    ; `S  (`Sf (`NM, `ATTR, `T))
    ; `S  (`Sf (`NM, `BASE, `F))
    ; `S  (`Sf (`NM, `BASE, `T))
    ; `S  (`Sf (`NM, `CONT, `F))
    ; `S  (`Sf (`NM, `CONT, `T))
    ; `S  (`Sf (`NM, `DA,   `F))
    ; `S  (`Sf (`NM, `DA,   `T))
    ; `S  (`Sf (`NM, `HYP,  `T))
    ; `S  (`Sf (`NM, `IMP,  `F))
    ; `S  (`Sf (`NM, `IMP,  `T))
    ; `S  (`Sf (`NM, `R,    `T))
    ; `S  (`Sf (`NM, `S,    `T))
    ; `S  (`Sf (`NM, `STEM, `F))
    ; `S  (`Sf (`NM, `STEM, `T)) ]


