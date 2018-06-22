
module type FEATURE =
sig
    type t

    val none : t
    val equal : t * t -> bool
    val show : t -> string
    val of_string : string -> t
    val parse : string list -> t * string list
    val unify : t * t -> t option
    val is_var : t -> bool
end

type en_feature = [ `None | `Nb | `Var
                  | `ADJ | `AS | `ASUP | `B | `BEM
                  | `DCL | `EM | `EXPL | `FOR | `FRG
                  | `INTJ | `INV | `NG | `NUM
                  | `POSS | `PSS | `PT | `Q | `QEM
                  | `THR | `TO | `WQ | `CONJ ]


module EnglishFeature :
sig
    include FEATURE with type t = en_feature

    module type NOTAT =
    sig
        val nb : t
        val var : t
        val adj : t
        val as_ : t
        val asup : t
        val b : t
        val bem : t
        val dcl : t
        val em : t
        val expl : t
        val for_ : t
        val frg : t
        val intj : t
        val inv : t
        val ng : t
        val num : t
        val poss : t
        val pss : t
        val pt : t
        val q : t
        val qem : t
        val thr : t
        val to_ : t
        val wq : t
        val conj : t
    end

    module Notat : NOTAT
end


type f_value = [ `X1 | `X2 | `X3 | `GA | `NC | `NI | `O | `TO
               | `ADN | `ADV | `NM | `R | `S | `IMP
               | `ATTR | `BASE | `CONT | `HYP | `STEM | `DA | `NEG
               | `T | `F]

module JapaneseFeatureValue :
sig
    type t = f_value

    module type NOTAT =
    sig
        val x1 : t
        val x2 : t
        val x3 : t
        val ga : t
        val nc : t
        val ni : t
        val o : t
        val to_ : t
        val adn : t
        val adv : t
        val nm : t
        val r : t
        val s : t
        val imp : t
        val attr : t
        val base : t
        val cont : t
        val hyp : t
        val stem : t
        val da : t
        val neg : t
        val t : t
        val f : t
    end

    module Notat : NOTAT
end

type ja_feature = [ `None
                  | `Sf of f_value * f_value * f_value
                  | `NPf of f_value * f_value * f_value]


module JapaneseFeature : FEATURE with type t = ja_feature
