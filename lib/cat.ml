

open Utils
open Feat


exception Parse_error of string

module type CATEGORIES =
sig
    type feature
    type t = [ `S of feature
             | `N of feature
             | `NP of feature
             | `PP of feature
             | `Fwd of t * t
             | `Bwd of t * t
             | `Either of t * t
             | `Punct of string]

    val s : t
    val n : t
    val np : t
    val pp_ : t
    val (/:) : t -> t -> t
    val (|:) : t -> t -> t
    val (||:) : t -> t -> t
    val (!:) : string -> t
    val (=:=) : t -> t -> bool

    val is_type_raised : t -> bool
    val is_functor : t -> bool
    val is_punct : t -> bool
    val is_modifier : t -> bool
    val remove_all_feat : t -> t
    val remove_some_feat : feature list -> t -> t
    val unify : t -> t -> t -> t
    val parse : string -> t
    val show : ?bracket:bool -> t -> string
    val pp : Format.formatter -> t -> unit
end


module Categories (Feature : FEATURE) =
struct

    type feature = Feature.t
    type t = [ `S of feature
             | `N of feature
             | `NP of feature
             | `PP of feature
             | `Fwd of t * t
             | `Bwd of t * t
             | `Either of t * t
             | `Punct of string]


    let s = `S Feature.none
    and n = `N Feature.none
    and np = `NP Feature.none
    and pp_ = `PP Feature.none
    and (/:) x y = `Fwd (x, y)
    and (|:) x y = `Bwd (x, y)
    and (||:) x y = `Either (x, y)
    and (!:) x = `Punct x

    let rec (=:=) a b = match (a, b) with
        | `Fwd (x, y), `Fwd (x', y')
        | `Bwd (x, y), `Bwd (x', y')
        | `Fwd (x, y), `Either (x', y')
        | `Bwd (x, y), `Either (x', y')
        | `Either (x, y), `Fwd (x', y')
        | `Either (x, y), `Bwd (x', y') -> x =:= x' && y =:= y'
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
        | `Either (x, y) -> !%(if bracket then "(%s|%s)" else "%s|%s")
                                (show ~bracket:true x) (show ~bracket:true y)
        | `Punct str  -> str

    let pp out cat =
        Format.pp_print_string out (show cat)

    let preprocess s =
        s |> Str.(global_replace (regexp "\\([]\\[()/\\\\|]\\)") " \\1 ")
          |> Str.(split (regexp " +"))

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

    let is_functor = function
        | `Bwd _ | `Fwd _ | `Either _ -> true
        | _ -> false

    let is_punct = function
        | `Punct _ -> true
        | _ -> false

    let is_modifier = function
        | `Fwd (x, y) | `Bwd (x, y) | `Either (x, y) -> x = y
        | _ -> false

    let rec remove_all_feat = function
        | `S _        -> s
        | `N _        -> n
        | `NP _       -> np
        | `PP _       -> pp_
        | `Fwd (x, y) -> remove_all_feat x /: remove_all_feat y
        | `Bwd (x, y) -> remove_all_feat x |: remove_all_feat y
        | `Either (x, y) -> remove_all_feat x ||: remove_all_feat y
        | c           -> c

    let rec remove_some_feat feats = function
        | `S f when List.mem f feats  -> s
        | `N f when List.mem f feats  -> n
        | `NP f when List.mem f feats -> np
        | `PP f when List.mem f feats -> pp_
        | `Fwd (x, y)
            -> remove_some_feat feats x /: remove_some_feat feats y
        | `Bwd (x, y)
            -> remove_some_feat feats x |: remove_some_feat feats y
        | `Either (x, y)
            -> remove_some_feat feats x ||: remove_some_feat feats y
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
        | `Fwd (x, y), `Either (x', y')
        | `Bwd (x, y), `Either (x', y')
        | `Either (x, y), `Fwd (x', y')
        | `Either (x, y), `Bwd (x', y')
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
            | `Either (x, y) -> unify_feat x f ||: unify_feat y f
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
            | head :: rest -> begin match head with
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
                | "|" -> parse' (`Slash (||:) :: stack) rest
                |  s -> parse' (`Cat (`Punct s) :: stack) rest
            end
        in parse' [] (preprocess str)

end

module EnglishCategories =
struct
    include Categories (EnglishFeature)

    module type NOTAT =
    sig
        val s : t
        val n : t
        val np : t
        val pp : t
        val s_ : feature -> t
        val n_ : feature -> t
        val np_ : feature -> t
        val pp_ : feature -> t
        val (/:) : t -> t -> t
        val (|:) : t -> t -> t
        val (||:) : t -> t -> t
        val (!:) : string -> t
        val (=:=) : t -> t -> bool
    end

    module Notat =
    struct
        let s = s
        let n = n
        let np = np
        let pp = pp_
        let s_ f = `S f
        let n_ f = `N f
        let np_ f = `NP f
        let pp_ f = `PP f
        let (/:) = (/:)
        let (|:) = (|:)
        let (||:) = (||:)
        let (!:) = (!:)
        let (=:=) = (=:=)
    end
end


module JapaneseCategories =
struct
    include Categories (JapaneseFeature)

    module type NOTAT =
    sig
        type feature = f_value * f_value * f_value

        val s_ : feature -> t
        val np_ : feature -> t
        val (/:) : t -> t -> t
        val (|:) : t -> t -> t
        val (||:) : t -> t -> t
        val (!:) : string -> t
        val (=:=) : t -> t -> bool
    end

    module Notat : NOTAT =
    struct
        type feature = f_value * f_value * f_value

        let s_ f = `S (`Sf f)
        let np_ f = `NP (`NPf f)
        let (/:) = (/:)
        let (|:) = (|:)
        let (||:) = (||:)
        let (!:) = (!:)
        let (=:=) = (=:=)
    end
end

