
open Utils

module type FEATURE =
sig
    type t

    val equal : t * t -> bool
    val show : t -> string
    val parse : string list -> t * string list
    val unify : t * t -> t option
    val is_var : t -> bool
end



type en_feat_t = [ `None | `Nb | `Var
                     | `ADJ | `AS | `ASUP | `B | `BEM
                     | `DCL | `EM | `EXPL | `FOR | `FRG
                     | `INTJ | `INV | `NG | `NUM
                     | `POSS | `PSS | `PT | `Q | `QEM
                     | `THR | `TO | `WQ ]


module EnglishFeature : FEATURE with type t = en_feat_t =
struct
    type t = en_feat_t

    let equal = function
        | (`None | `Nb | `Var), _
        | _, (`None | `Nb | `Var) -> true
        | x, y -> x = y

    let show = function
        | `None  -> ""       | `Nb    -> "[nb]"
        | `Var   -> "[X]"    | `ADJ   -> "[adj]"
        | `AS    -> "[as]"   | `ASUP  -> "[asup]"
        | `B     -> "[b]"    | `BEM   -> "[bem]"
        | `DCL   -> "[dcl]"  | `EM    -> "[em]"
        | `EXPL  -> "[expl]" | `FOR   -> "[for]"
        | `FRG   -> "[frg]"  | `INTJ  -> "[intj]"
        | `INV   -> "[inv]"  | `NG    -> "[ng]"
        | `NUM   -> "[num]"  | `POSS  -> "[poss]"
        | `PSS   -> "[pss]"  | `PT    -> "[pt]"
        | `Q     -> "[q]"    | `QEM   -> "[qem]"
        | `THR   -> "[thr]"  | `TO    -> "[to]"
        | `WQ    -> "[wq]"

    let parse = function
        | "[" :: feat :: "]" :: rest
            -> let feat' = match feat with
                | "nb"   -> `Nb     | "X"    -> `Var
                | "adj"  -> `ADJ    | "as"   -> `AS
                | "asup" -> `ASUP   | "b"    -> `B
                | "bem"  -> `BEM    | "dcl"  -> `DCL
                | "em"   -> `EM     | "expl" -> `EXPL
                | "for"  -> `FOR    | "frg"  -> `FRG
                | "intj" -> `INTJ   | "inv"  -> `INV
                | "ng"   -> `NG     | "num"  -> `NUM
                | "poss" -> `POSS   | "pss"  -> `PSS
                | "pt"   -> `PT     | "q"    -> `Q
                | "qem"  -> `QEM    | "thr"  -> `THR
                | "to"   -> `TO     | "wq"   -> `WQ
                | _ -> invalid_arg feat
               in (feat', rest)
        | rest -> (`None, rest)

    let unify = function
        | (`None | `Var), (`None | `Var) -> None
        | (`None | `Var), f | f, (`None | `Var) -> Some f
        | _ -> None

    let is_var = function
        | `Var -> true
        | _ -> false
end


type f_value = [ `X1 | `X2 | `GA | `NC | `NI | `O | `TO
               | `ADN | `ADV | `NM | `R | `S | `IMP
               | `ATTR | `BASE | `CONT | `HYP | `STEM | `DA | `NEG
               | `T | `F]


type ja_feat_t = [ `None
                 | `Sf of f_value * f_value * f_value
                 | `NPf of f_value * f_value * f_value]


module JapaneseFeatureValue =
struct
    type t = f_value

    let equal = function
        | (`X1 | `X2), _
        | _, (`X1 | `X2) -> true
        | (f, g) -> f = g

    let show = function
        | `X1   -> "X1"     | `X2   -> "X2"
        | `GA   -> "ga"     | `NC   -> "nc"
        | `NI   -> "ni"     | `O    -> "o"
        | `TO   -> "to"     | `ADN  -> "adn"
        | `ADV  -> "adv"    | `NM   -> "nm"
        | `R    -> "r"      | `S    -> "s"
        | `IMP  -> "imp"    | `ATTR -> "attr"
        | `BASE -> "base"   | `CONT -> "cont"
        | `HYP  -> "hyp"    | `STEM -> "stem"
        | `DA   -> "da"     | `NEG  -> "neg"
        | `T    -> "t"      | `F    -> "f"

    let parse = function
        | "X1"   -> `X1     | "X2"   -> `X2
        | "ga"   -> `GA     | "nc"   -> `NC
        | "ni"   -> `NI     | "o"    -> `O
        | "to"   -> `TO     | "adn"  -> `ADN
        | "adv"  -> `ADV    | "nm"   -> `NM
        | "r"    -> `R      | "s"    -> `S
        | "imp"  -> `IMP    | "attr" -> `ATTR
        | "base" -> `BASE   | "cont" -> `CONT
        | "hyp"  -> `HYP    | "stem" -> `STEM
        | "da"   -> `DA     | "neg"  -> `NEG
        | "t"    -> `T      | "f"    -> `F
        | rest -> invalid_arg (!%"parse: %s" rest)

    let unify = function
        | (`X1 | `X2), (`X1 | `X2) -> None
        | (`X1 | `X2), f | f, (`X1 | `X2) -> Some f
        | _ -> None

    let is_var = function
        | (`X1 | `X2) -> true
        | _ -> false

end


module JapaneseFeature : FEATURE with type t = ja_feat_t =
struct
    module V = JapaneseFeatureValue

    type t = ja_feat_t

    let equal = function
        | `Sf (f1, f2, f3), `Sf (g1, g2, g3)
        | `NPf (f1, f2, f3), `NPf (g1, g2, g3) ->
                V.equal (f1, g1) && V.equal (f2, g2) && V.equal (f3, g3)
        | _ -> false

    let show = function
        | `Sf (f1, f2, f3)  ->
            !%"[mod=%s,form=%s,fin=%s]" (V.show f1) (V.show f2) (V.show f3)
        | `NPf (f1, f2, f3) ->
            !%"[case=%s,mod=%s,fin=%s]" (V.show f1) (V.show f2) (V.show f3)
        | `None -> ""

    let parse =
        let s_f f1 f2 f3  = `Sf (V.parse f1, V.parse f2, V.parse f3)
        and np_f f1 f2 f3 = `NPf (V.parse f1, V.parse f2, V.parse f3)
        in function
        | "[" :: feat :: "]" :: rest
            -> let feat' = begin match String.sub feat 0 3 with
                | "cas" -> Scanf.sscanf feat "case=%s@,mod=%s@,fin=%s" np_f
                | "mod" -> Scanf.sscanf feat "mod=%s@,form=%s@,fin=%s" s_f
                | _ -> invalid_arg (!%"parse: %s" feat)
               end in (feat', rest)
        | rest -> (`None, rest)

    let unify = function
        | `Sf (`X1, `X2, f3), (`Sf (_, _, g3) as f)
        | (`Sf (_, _, g3) as f), `Sf (`X1, `X2, f3)
        | `NPf (`X1, `X2, f3), (`NPf (_, _, g3) as f)
        | (`NPf (_, _, g3) as f), `NPf (`X1, `X2, f3)
                -> if V.equal (f3, g3) then Some f else None
        | _ -> None

    let is_var = function
        | `Sf (f1, f2, f3) | `NPf (f1, f2, f3) ->
                V.is_var f1 || V.is_var f2 || V.is_var f3
        | _ -> false
end

