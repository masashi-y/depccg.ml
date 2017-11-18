
(* open Cat.EnglishCategories *)
open Cat
open Feat
open Utils
(* open Cell *)

module type GRAMMAR =
sig
    type feature

    module Feature : FEATURE with type t = feature
    module Cat : CATEGORIES with type feature = Feature.t

    type t
    type rule = Cat.t * Cat.t -> Cat.t list

    val intro : t
    val unary : t

    val combinatory_rules : t list
    val show : t -> string
    val apply : Cat.t * Cat.t -> t -> (t * Cat.t) list
    val apply_rules : Cat.t * Cat.t -> (t * Cat.t) list

    val possible_root_cats : Cat.t list
    val is_acceptable_unary : Cat.t -> t -> bool
    val resolve_dependency : int * int -> int * int (* TODO *)

    val apply_rules_with_cache : (Cat.t * Cat.t, (t * Cat.t) list) Hashtbl.t -> Cat.t * Cat.t -> (t * Cat.t) list

    (* check a pair of cats is seen in a dictionary *)
    val is_seen : (Cat.t * Cat.t, bool) Hashtbl.t -> Cat.t * Cat.t -> bool
end

module BaseGrammar (Cat : CATEGORIES) =
struct
    open Cat

    type t = [ `FwdApp | `BwdApp | `Intro | `Unary]

    let show = function
        | `FwdApp  -> ">"
        | `BwdApp  -> "<"
        | `Intro   -> "<*>"
        | `Unary   -> "<u>"

    (* X/Y Y --> X *)
    let forward_application = function
        | `Fwd (x, y), y' when y =:= y' -> [unify (if x = y then y' else x) y y']
        | _ -> []

    (* X Y\X --> Y *)
    let backward_application = function
        | x, `Bwd (y, x') when x =:= x' -> [unify (if y = x' then x else y) x' x]
        | _ -> []

    (* X/Y Y/Z --> X/Z *)
    let forward_composition = function
        | `Fwd (x, y), `Fwd (y', z) when y =:= y'
            -> [unify (if x = y then y /: z else x /: z) y y']
        | _ -> []

    (* X/Y Z\X --> Z/Y *)
    let backward_composition = function
        | `Fwd (x, y), `Bwd (z, x') when x =:= x'
            -> [unify (if z = x' then x /: y else z /: y) x x']
        | _ -> []

    (* X/Y (Y/Z)|W --> (X/Z)|W *)
    let generalized_forward_composition = function
        | `Fwd (x, y), `Fwd (`Fwd (y', z), w) when y =:= y'
            -> let x' = if x = y then y' else x in [unify ((x' /: z) /: w) y y']
        | `Fwd (x, y), `Bwd (`Fwd (y', z), w) when y =:= y'
            -> let x' = if x = y then y' else x in [unify ((x' /: z) |: w) y y']
        | _ -> []

    (* (X\Y)|Z W\X --> (W\Y)|Z *)
    let generalized_backward_composition = function
        | `Bwd (`Bwd (x, y), z), `Bwd (w, x') when x =:= x'
            -> let w' = if w = x' then x else w in [unify ((w' |: y) |: z) x x']
        | `Fwd (`Bwd (x, y), z), `Bwd (w, x') when x =:= x'
            -> let w' = if w = x' then x else w in [unify ((w' |: y) /: z) x x']
        | _ -> []

    (* ((X\Y)|Z)|W U\X --> ((U\Y)|Z)|W *)
    let generalized_backward_composition2 = function
        | `Bwd (`Bwd (`Bwd (x, y), z), w), `Bwd (u, x') when x =:= x'
        -> let u' = if u = x' then x else u in [unify (((u' |: y) |: z) |: w) x x']
        | `Fwd (`Bwd (`Bwd (x, y), z), w), `Bwd (u, x') when x =:= x'
        -> let u' = if u = x' then x else u in [unify (((u' |: y) |: z) /: w) x x']
        | `Fwd (`Fwd (`Bwd (x, y), z), w), `Bwd (u, x') when x =:= x'
        -> let u' = if u = x' then x else u in [unify (((u' |: y) /: z) /: w) x x']
        | `Bwd (`Fwd (`Bwd (x, y), z), w), `Bwd (u, x') when x =:= x'
        -> let u' = if u = x' then x else u in [unify (((u' |: y) /: z) |: w) x x']
        | _ -> []

    (* (((X\Y)|Z)|W)|U S\X --> (((S\Y)|Z)|W)|U *)
    let generalized_backward_composition3 = function
        | `Bwd (`Bwd (`Bwd (`Bwd (x, y), z), w), u), `Bwd (s, x') when x =:= x'
    -> let s' = if s = x' then x else s in [unify ((((s' |: y) |: z) |: w) |: u) x x']
        | `Fwd (`Bwd (`Bwd (`Bwd (x, y), z), w), u), `Bwd (s, x') when x =:= x'
    -> let s' = if s = x' then x else s in [unify ((((s' |: y) |: z) |: w) /: u) x x']
        | `Bwd (`Fwd (`Bwd (`Bwd (x, y), z), w), u), `Bwd (s, x') when x =:= x'
    -> let s' = if s = x' then x else s in [unify ((((s' |: y) |: z) /: w) |: u) x x']
        | `Fwd (`Fwd (`Bwd (`Bwd (x, y), z), w), u), `Bwd (s, x') when x =:= x'
    -> let s' = if s = x' then x else s in [unify ((((s' |: y) |: z) /: w) /: u) x x']
        | `Bwd (`Bwd (`Fwd (`Bwd (x, y), z), w), u), `Bwd (s, x') when x =:= x'
    -> let s' = if s = x' then x else s in [unify ((((s' |: y) /: z) |: w) |: u) x x']
        | `Fwd (`Bwd (`Fwd (`Bwd (x, y), z), w), u), `Bwd (s, x') when x =:= x'
    -> let s' = if s = x' then x else s in [unify ((((s' |: y) /: z) |: w) /: u) x x']
        | `Bwd (`Fwd (`Fwd (`Bwd (x, y), z), w), u), `Bwd (s, x') when x =:= x'
    -> let s' = if s = x' then x else s in [unify ((((s' |: y) /: z) /: w) |: u) x x']
        | `Fwd (`Fwd (`Fwd (`Bwd (x, y), z), w), u), `Bwd (s, x') when x =:= x'
    -> let s' = if s = x' then x else s in [unify ((((s' |: y) /: z) /: w) /: u) x x']
        | _ -> []


    (* X/Y Y\Z --> X\Z *)
    let cross_forward_composition = function
        | `Fwd (x, y), `Bwd (y', z) when y =:= y'
            -> let x' = if x = y then y else x in [unify (x' |: z) y y']
        | _ -> []

    (* X/Y (Y\Z)|W --> (X\Z)|W *)
    let cross_forward_composition1 = function
        | `Fwd (x, y), `Fwd (`Bwd (y', z), w) when y =:= y'
            -> let x' = if x = y then y' else x in [unify ((x' |: z) /: w) y y']
        | `Fwd (x, y), `Bwd (`Bwd (y', z), w) when y =:= y'
            -> let x' = if x = y then y' else x in [unify ((x' |: z) |: w) y y']
        | _ -> []

    (* X/Y ((Y\Z)|W)|U --> ((X\Z)|W)|U *)
    let cross_forward_composition2 = function
        | `Fwd (x, y), `Fwd (`Fwd (`Bwd (y', z), w), u) when y =:= y'
        -> let x' = if x = y then y' else x in [unify (((x' |: z) /: w) /: u) y y']
        | `Fwd (x, y), `Bwd (`Fwd (`Bwd (y', z), w), u) when y =:= y'
        -> let x' = if x = y then y' else x in [unify (((x' |: z) /: w) |: u) y y']
        | `Fwd (x, y), `Fwd (`Bwd (`Bwd (y', z), w), u) when y =:= y'
        -> let x' = if x = y then y' else x in [unify (((x' |: z) |: w) /: u) y y']
        | `Fwd (x, y), `Bwd (`Bwd (`Bwd (y', z), w), u) when y =:= y'
        -> let x' = if x = y then y' else x in [unify (((x' |: z) |: w) |: u) y y']
        | _ -> []

end


module EnglishGrammar : GRAMMAR
    with type feature = en_feature =
struct
    type feature = en_feature
    module Feature = EnglishFeature
    module Cat = Categories (Feature)
    module Base = BaseGrammar (Cat)
    open Cat

    type t = [ `FwdCmp
             | `BwdCmp
             | `GenFwdCmp
             | `GenBwdCmp
             | `Conj           (* Conjunction *)
             | `RP             (* RemovePunctuation *)
             | `CommaVPtoADV   (* CommaAndVerbPhraseToAdverb *)
             | `ParentDirect   (* ParentheticalDirectSpeech *)
             | Base.t]
    type rule = Cat.t * Cat.t -> Cat.t list

    let possible_root_cats = [ `S `DCL
                             ; `S `WQ
                             ; `S `Q
                             ; `S `QEM
                             ; `NP `None]

    let combinatory_rules = [`FwdApp; `BwdApp; `FwdCmp; `BwdCmp; `GenFwdCmp;
        `GenBwdCmp; `Conj; `RP; `CommaVPtoADV; `ParentDirect]

    let intro = `Intro
    let unary = `Unary

    let show = function
        | `FwdCmp       -> ">B"
        | `BwdCmp       -> "<B"
        | `GenFwdCmp    -> ">B1"
        | `GenBwdCmp    -> "<B1"
        | `Conj         -> "<P>"
        | `RP           -> "<rp>"
        | `CommaVPtoADV -> "<*>"
        | `ParentDirect -> "<*>"
        | #Base.t as t  -> Base.show t

    (* S[dcl] S[em]\S[em] --> S[em] *)
    let backward_application = function
        | `S `DCL, `Bwd (`S `EM, `S `EM) -> [`S `EM]
        | x, `Bwd (y, x') when x =:= x' -> [unify (if y = x' then x else y) x' x]
        | _ -> []

    let conjunction cs =
        let conj1 =
            let puncts = [","; ";"; "conj"] in function
            | _, `Bwd (`NP _, `NP _)
            | _, `N _  -> []
            | `Punct x, y when List.mem x puncts
                         && not (is_punct y)
                         && not (is_type_raised y) -> [y |: y]
            | _ -> [] 
        in
        let conj2 = function
            | `Punct "conj", `Bwd (`NP `None, `NP `None) -> [np]
            | _, `Bwd (`NP _, `NP _) -> []
            | _, `N _ -> []
            | `Punct ",", y when not (is_punct y)
                              && not (is_type_raised y) -> [y]
            | _ -> [] 
        in
        let conj3 = function
            | `NP `None, `Bwd (`NP `None, `NP `None) -> [np]
            | _ -> [] 
        in List.flatten [conj1 cs; conj2 cs; conj3 cs]

    (* , S[ng|pss]\NP --> (S\NP)\(S\NP) *)
    let comma_vp_to_adv = function
        | `Punct ",", `Bwd (`S f, `NP `None) when f = `NG || f = `PSS
            -> [(s |: np) |: (s |: np)]
        | _ -> []

    (* , S[dcl]/S[dcl] --> (S\NP)/(S\NP) *)
    let parenthetical_direct_speech = function
        | `Punct ",", `Fwd (`S `DCL, `S `DCL)
            -> [(s |: np) /: (s |: np)]
        | _ -> []

    (* PUNCT x --> x *)  (* x PUNCT --> x *)
    let remove_punctuation = function
        | `Punct _, x -> [x]
        | x, `Punct _ -> [x]
        | _ -> []

    let apply cs rule =
        let p f = List.map (fun c -> (rule, c)) (f cs)
        in match rule with
        | `FwdApp       -> p Base.forward_application
        | `BwdApp       -> p backward_application
        | `FwdCmp       -> p Base.forward_composition
        | `BwdCmp       -> p Base.backward_composition
        | `GenFwdCmp    -> p Base.generalized_forward_composition
        | `GenBwdCmp    -> p Base.generalized_backward_composition
        | `Conj         -> p conjunction
        | `RP           -> p remove_punctuation
        | `CommaVPtoADV -> p comma_vp_to_adv
        | `ParentDirect -> p parenthetical_direct_speech
        | (`Intro | `Unary) -> invalid_arg "apply"

    let apply_rules cs =
        List.flatten @@ List.map (apply cs) combinatory_rules

    let resolve_dependency (head, dep) = (head, dep)

    let is_acceptable_unary c r = r <> `RP || Cat.is_type_raised c

    let apply_rules_with_cache cache (c1, c2) =
                             let prep = Cat.remove_some_feat [`Nb] in
                             let cats = (prep c1, prep c2) in
                             try Hashtbl.find cache cats with Not_found ->
                                 let rules = apply_rules cats in
                                 Hashtbl.add cache cats rules;
                                 rules

    let is_seen seen_rules (c1, c2) = let prep = Cat.remove_some_feat [`Var; `Nb]
                                      in Hashtbl.mem seen_rules (prep c1, prep c2)
end


module JapaneseGrammar : GRAMMAR
    with type feature = ja_feature =
struct
    type feature = ja_feature
    module Feature = JapaneseFeature
    module Cat = Categories (Feature)
    module Base = BaseGrammar (Cat)
    open Cat

    type t = [ `FwdCmp
             | `BwdCmp
             | `GenBwdCmp2
             | `GenBwdCmp3
             | `GenBwdCmp4
             | `CrsFwdCmp1
             | `CrsFwdCmp2
             | `CrsFwdCmp3
             | `Conj
             | Base.t]

    let combinatory_rules = [`FwdApp
                            ; `BwdApp
                            ; `FwdCmp
                            ; `BwdCmp
                            ; `GenBwdCmp2
                            ; `GenBwdCmp3
                            ; `GenBwdCmp4
                            ; `CrsFwdCmp1
                            ; `CrsFwdCmp2
                            ; `CrsFwdCmp3
                            ; `Conj]

    type rule = Cat.t * Cat.t -> Cat.t list

    let possible_root_cats =
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

    let intro = `Intro
    let unary = `Unary

    let show = function
        | `FwdCmp      -> ">B"
        | `BwdCmp      -> "<B1"
        | `GenBwdCmp2  -> "<B2"
        | `GenBwdCmp3  -> "<B3"
        | `GenBwdCmp4  -> "<B4"
        | `CrsFwdCmp1  -> ">Bx1"
        | `CrsFwdCmp2  -> ">Bx2"
        | `CrsFwdCmp3  -> ">Bx3"
        | `Conj        -> "<P>"
        | #Base.t as t  -> Base.show t

    let conjoin : rule = function
        | x, y when List.mem x possible_root_cats
                && x = y && Cat.is_functor x -> [y]
        | _ -> []

    let apply cs rule =
        let p f = List.map (fun c -> (rule, c)) (f cs)
        in match rule with
        | `FwdApp      -> p Base.forward_application
        | `BwdApp      -> p Base.backward_application
        | `FwdCmp      -> p Base.forward_composition
        | `BwdCmp      -> p Base.backward_composition
        | `GenBwdCmp2  -> p Base.generalized_backward_composition
        | `GenBwdCmp3  -> p Base.generalized_backward_composition2
        | `GenBwdCmp4  -> p Base.generalized_backward_composition3
        | `CrsFwdCmp1  -> p Base.cross_forward_composition
        | `CrsFwdCmp2  -> p Base.cross_forward_composition1
        | `CrsFwdCmp3  -> p Base.cross_forward_composition2
        | `Conj        -> p conjoin
        | (`Intro | `Unary) -> invalid_arg "apply"

    let apply_rules cs =
        List.flatten @@ List.map (apply cs) combinatory_rules

    let resolve_dependency (dep, head) = (head, dep)

    let is_acceptable_unary _ _ = true

    let apply_rules_with_cache cache cats =
                            try Hashtbl.find cache cats with Not_found ->
                                let rules = apply_rules cats in
                                Hashtbl.add cache cats rules;
                                rules

    (* check a pair of cats is seen in a dictionary *)
    let is_seen seen_rules cats = Hashtbl.mem seen_rules cats

end

