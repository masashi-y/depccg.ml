
module type FEATURE =
sig
    type t

    val none : t
    val equal : t * t -> bool
    val show : t -> string
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


module EnglishFeature : FEATURE with type t = en_feature


type f_value = [ `X1 | `X2 | `X3 | `GA | `NC | `NI | `O | `TO
               | `ADN | `ADV | `NM | `R | `S | `IMP
               | `ATTR | `BASE | `CONT | `HYP | `STEM | `DA | `NEG
               | `T | `F]


type ja_feature = [ `None
                  | `Sf of f_value * f_value * f_value
                  | `NPf of f_value * f_value * f_value]


module JapaneseFeature : FEATURE with type t = ja_feature
