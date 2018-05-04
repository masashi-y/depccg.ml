
open Feat

exception Parse_error of string

module type CATEGORIES =
sig
    type feature
    type t = [ `X
             | `S of feature
             | `N of feature
             | `NP of feature
             | `PP of feature
             | `Fwd of t * t
             | `Bwd of t * t
             | `Punct of string]

    val x : t
    val s : t
    val n : t
    val np : t
    val pp : t
    val (/:) : t -> t -> t
    val (|:) : t -> t -> t
    val (!:) : string -> t
    val (=:=) : t -> t -> bool
    val show : ?bracket:bool -> t -> string

    val is_type_raised : t -> bool
    val is_functor : t -> bool
    val is_punct : t -> bool
    val is_modifier : t -> bool
    val is_variable : t -> bool
    val remove_all_feat : t -> t
    val remove_some_feat : feature list -> t -> t
    val unify : t -> t -> t -> t
    val parse : string -> t
end


module Categories (Feature : FEATURE) : CATEGORIES with type feature = Feature.t

module EnglishCategories : CATEGORIES with type feature = en_feature

module JapaneseCategories : CATEGORIES with type feature = ja_feature

