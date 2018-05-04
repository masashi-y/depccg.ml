
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
    
    val make : cat:cat -> op:op -> children:(t list) -> t
    val terminal : cat -> string -> t
    val make_scored : ?score:float -> t -> scored
    val terminals : t -> string list
    val preterminals : t -> cat list
    val length : t -> int
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


type en_rules = [ `FwdCmp | `BwdCmp | `GenFwdCmp | `GenBwdCmp | `Conj
                | `RP | `CommaVPtoADV | `ParentDirect | `Variable | base_rules]

module EnglishGrammar : GRAMMAR with type feature = en_feature
                                and type rules = en_rules

type ja_rules = [ `FwdCmp | `BwdCmp | `GenBwdCmp2 | `GenBwdCmp3
                | `GenBwdCmp4 | `CrsFwdCmp1 | `CrsFwdCmp2
                | `CrsFwdCmp3 | `Conj | base_rules]

module JapaneseGrammar : GRAMMAR with type feature = ja_feature
                                 and type rules = ja_rules
