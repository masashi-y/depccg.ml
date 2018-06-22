
open Utils

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


module EnglishFeature =
struct
    type t = en_feature

    let none = `None

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
        | `WQ    -> "[wq]"   | `CONJ  -> "[conj]"

    let of_string = function
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
        | "conj" -> `CONJ
        | feat -> invalid_arg feat

    let parse = function
        | "[" :: feat :: "]" :: rest -> (of_string feat, rest)
        | rest -> (`None, rest)

    let unify = function
        | (`None | `Var), (`None | `Var) -> None
        | (`None | `Var), f | f, (`None | `Var) -> Some f
        | _ -> None


    let is_var = function
        | `Var -> true
        | _ -> false

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

    module Notat =
    struct
        let nb = `Nb
        let var = `Var
        let adj = `ADJ
        let as_ = `AS
        let asup = `ASUP
        let b = `B
        let bem = `BEM
        let dcl = `DCL 
        let em = `EM 
        let expl = `EXPL 
        let for_ = `FOR 
        let frg = `FRG
        let intj = `INTJ 
        let inv = `INV 
        let ng = `NG 
        let num = `NUM
        let poss = `POSS 
        let pss = `PSS 
        let pt = `PT 
        let q = `Q 
        let qem = `QEM
        let thr = `THR 
        let to_ = `TO 
        let wq = `WQ 
        let conj = `CONJ
    end
end


type f_value = [ `X1 | `X2 | `X3 | `GA | `NC | `NI | `O | `TO
               | `ADN | `ADV | `NM | `R | `S | `IMP
               | `ATTR | `BASE | `CONT | `HYP | `STEM | `DA | `NEG
               | `T | `F]


type ja_feature = [ `None
                  | `Sf of f_value * f_value * f_value
                  | `NPf of f_value * f_value * f_value]


module JapaneseFeatureValue =
struct
    type t = f_value

    let equal = function
        | (`X1 | `X2 | `X3), _
        | _, (`X1 | `X2 | `X3) -> true
        | (f, g) -> f = g


    let show = function
        | `X1   -> "X1"   | `X2   -> "X2"
        | `X3   -> "X3"   | `GA   -> "ga"
        | `NC   -> "nc"   | `NI   -> "ni"
        | `O    -> "o"    | `TO   -> "to"
        | `ADN  -> "adn"  | `ADV  -> "adv"
        | `NM   -> "nm"   | `R    -> "r"
        | `S    -> "s"    | `IMP  -> "imp"
        | `ATTR -> "attr" | `BASE -> "base"
        | `CONT -> "cont" | `HYP  -> "hyp"
        | `STEM -> "stem" | `DA   -> "da"
        | `NEG  -> "neg"  | `T    -> "t"
        | `F    -> "f"


    let parse = function
        | "X1"   -> `X1   | "X2"   -> `X2
        | "X3"   -> `X3   | "ga"   -> `GA
        | "nc"   -> `NC   | "ni"   -> `NI
        | "o"    -> `O    | "to"   -> `TO
        | "adn"  -> `ADN  | "adv"  -> `ADV
        | "nm"   -> `NM   | "r"    -> `R
        | "s"    -> `S    | "imp"  -> `IMP
        | "attr" -> `ATTR | "base" -> `BASE
        | "cont" -> `CONT | "hyp"  -> `HYP
        | "stem" -> `STEM | "da"   -> `DA
        | "neg"  -> `NEG  | "t"    -> `T
        | "f"    -> `F
        | rest -> invalid_arg (!%"parse: %s" rest)


    let unify = function
        | `X1, `X1 -> `X1
        | `X2, `X2 -> `X2
        | `X3, `X3 -> `X3
        | (`X1 | `X2 | `X3), f
        | f, (`X1 | `X2 | `X3) -> f
        | f, g when equal (f, g) -> f
        | f, g -> failwith
            (!%"trying to unify features not allowed: (%s, %s)"
            (show f) (show g))


    let is_var = function
        | (`X1 | `X2 | `X3) -> true
        | _ -> false

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

    module Notat =
    struct
        let x1 = `X1
        let x2 = `X2
        let x3 = `X3
        let ga = `GA
        let nc = `NC
        let ni = `NI
        let o = `O
        let to_ = `TO
        let adn = `ADN
        let adv = `ADV
        let nm = `NM
        let r = `R
        let s = `S
        let imp = `IMP
        let attr = `ATTR
        let base = `BASE
        let cont = `CONT
        let hyp = `HYP
        let stem = `STEM
        let da = `DA
        let neg = `NEG 
        let t = `T
        let f = `F
    end
end


module JapaneseFeature =
struct
    module V = JapaneseFeatureValue

    type t = ja_feature

    let none = `None

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


    let of_string str = 
        let s_f f1 f2 f3  = `Sf (V.parse f1, V.parse f2, V.parse f3) in
        let np_f f1 f2 f3 = `NPf (V.parse f1, V.parse f2, V.parse f3) in
        match String.sub str 0 3 with
        | "cas" -> Scanf.sscanf str "case=%s@,mod=%s@,fin=%s" np_f
        | "mod" -> Scanf.sscanf str "mod=%s@,form=%s@,fin=%s" s_f
        | _ -> invalid_arg (!%"parse: %s" str)

    let parse = function
        | "[" :: feat :: "]" :: rest -> (of_string feat, rest)
        | rest -> (`None, rest)


    let unify = function
        | `Sf (f1, f2, f3), `Sf (g1, g2, g3)
                -> Some (`Sf (V.unify (f1, g1), V.unify (f2, g2), V.unify (f3, g3)))
        | `NPf (f1, f2, f3), `NPf (g1, g2, g3)
                -> Some (`NPf (V.unify (f1, g1), V.unify (f2, g2), V.unify (f3, g3)))
        | _ -> None


    let is_var = function
        | `Sf (f1, f2, f3) | `NPf (f1, f2, f3) ->
                V.is_var f1 || V.is_var f2 || V.is_var f3
        | _ -> false
end

