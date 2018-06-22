
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
    val pp : t
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
end


module Categories (Feature : FEATURE) : CATEGORIES with type feature = Feature.t

module EnglishCategories :
sig
    include CATEGORIES with type feature = en_feature

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

    module Notat : NOTAT
end

module JapaneseCategories :
sig
    include CATEGORIES with type feature = ja_feature

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

    module Notat : NOTAT
end

