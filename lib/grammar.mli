
open Cat
open Feat
open Utils


module type RULES =
sig
    type t

    val intro : t
    val unary : t
    val to_list : t list
    val show : t -> string
end


module type TREE =
sig
    type cat
    type op
    type t = {
        cat : cat;
        op : op;
        children : t list;
        str: string
    }
    type scored = (float * t) list

    type view = T of cat * string
              | N of op * cat * view list
              | P of t

    val make : cat:cat -> op:op -> children:(t list) -> t
    val terminal : cat -> string -> t
    val make_scored : ?score:float -> t -> scored
    val is_terminal : t -> bool
    val terminals : t -> string list
    val preterminals : t -> cat list
    val length : t -> int
    val view : t -> view
    val view2 : t -> view
    val of_view : view -> t
    val left_child : t -> t
    val right_child : t -> t
    val match_with_unary : t -> t option
    val match_with_binary : t -> (t * t) option
    val match_with_type_raised : t -> (cat * t) option
end


module Tree (Cat : CATEGORIES) (Rules : RULES) : TREE


module type GRAMMAR =
sig
    type feature
    type rules

    module Rules : RULES with type t = rules
    module Feature : FEATURE with type t = feature
    module Cat : CATEGORIES with type feature = Feature.t
    module Tree : TREE with type cat = Cat.t and type op = rules

    val apply : Cat.t * Cat.t -> rules -> (rules * Cat.t) list
    val apply_rules : Cat.t * Cat.t -> (rules * Cat.t) list

    val possible_root_cats : Cat.t list
    val is_acceptable_unary : Cat.t -> rules -> bool
    (* TODO: generalize to work on tree object *)
    val resolve_dependency : int * int -> int * int -> int * int

    val apply_rules_with_cache
        : (Cat.t * Cat.t, (rules * Cat.t) list) Hashtbl.t -> Cat.t * Cat.t -> (rules * Cat.t) list

    (* check a pair of cats is seen in a dictionary *)
    val is_seen : (Cat.t * Cat.t, bool) Hashtbl.t -> Cat.t * Cat.t -> bool
end


type base_rules = [ `FwdApp | `BwdApp | `Intro | `Unary]


type en_rules = [ `FwdCmp | `BwdCmp | `GenFwdCmp | `GenBwdCmp
                | `Conj | `RP | `CommaVPtoADV | `ParentDirect | base_rules]


module EnglishGrammar :
sig
    include GRAMMAR with type feature = en_feature
                    and type rules = en_rules

    module Notat :
    sig
        include EnglishFeature.NOTAT
        include EnglishCategories.NOTAT

        val lex : rules
        val fa : rules
        val ba : rules
        val un : rules
        val fc : rules
        val bx : rules
        val gfc : rules
        val gbx : rules
        val conj : rules
        val rp : rules
    end
end

type ja_rules = [ `FwdCmp | `BwdCmp | `GenBwdCmp2 | `GenBwdCmp3
                | `GenBwdCmp4 | `CrsFwdCmp1 | `CrsFwdCmp2
                | `CrsFwdCmp3 | `Conj | base_rules]

module JapaneseGrammar :
sig
    include GRAMMAR with type feature = ja_feature
                    and type rules = ja_rules

    module Notat :
    sig
        include JapaneseFeatureValue.NOTAT
        include JapaneseCategories.NOTAT

        val lex : rules
        val fa : rules
        val ba : rules
        val un : rules
        val fc : rules
        val bc : rules
        val gbc2 : rules
        val gbc3 : rules
        val gbc4 : rules
        val xfc1 : rules
        val xfc2 : rules
        val xfc3 : rules
        val conj : rules
    end
end
