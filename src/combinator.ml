
open Cat.EnglishCategories
open Utils

type t =
    [ `FwdApp
    | `BwdApp
    | `FwdCmp
    | `BwdCmp
    | `GenFwdCmp
    | `GenBwdCmp
    | `Conj           (* Conjunction *)
    | `RP             (* RemovePunctuation *)
    | `CommaVPtoADV   (* CommaAndVerbPhraseToAdverb *)
    | `ParentDirect   (* ParentheticalDirectSpeech *)
    | `Intro
    | `Unary]

let show = function
    | `FwdApp       -> ">"
    | `BwdApp       -> "<"
    | `FwdCmp       -> ">B"
    | `BwdCmp       -> "<B"
    | `GenFwdCmp    -> ">B1"
    | `GenBwdCmp    -> "<B1"
    | `Conj         -> "<P>"
    | `RP           -> "<rp>"
    | `CommaVPtoADV -> "<*>"
    | `ParentDirect -> "<*>"
    | `Intro        -> "<*>"
    | `Unary        -> "<u>"

(* X/Y Y --> X *)
let forward_application = function
    | `Fwd (x, y), y' when y =:= y' -> [unify (if x = y then y' else x) y y']
    | _ -> []

(* X Y\X --> Y *)  (* S[dcl] S[em]\S[em] --> S[em] *)
let backward_application = function
    | `S `DCL, `Bwd (`S `EM, `S `EM) -> [`S `EM]
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
        -> [unify (if x = y then (y' /: z) /: w else (x /: z) /: w) y y']
    | `Fwd (x, y), `Bwd (`Fwd (y', z), w) when y =:= y'
        -> [unify (if x = y then (y' /: z) |: w else (x /: z) |: w) y y']
    | _ -> []

let generalized_backward_composition = function
    | `Bwd (`Bwd (x, y), z), `Bwd (w, x') when x =:= x'
        -> [unify (if w = x' then (x |: y) |: z else (w |: y) |: z) x x']
    | `Fwd (`Bwd (x, y), z), `Bwd (w, x') when x =:= x'
        -> [unify (if w = x' then (x |: y) /: z else (w |: y) /: z) x x']
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


let get_left = function
    | `Fwd (x, _) | `Bwd (x, _) -> Some x
    | _ -> None

(* PUNCT x --> x\x *)
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
    | `FwdApp       -> p forward_application
    | `BwdApp       -> p backward_application
    | `FwdCmp       -> p forward_composition
    | `BwdCmp       -> p backward_composition
    | `GenFwdCmp    -> p generalized_forward_composition
    | `GenBwdCmp    -> p generalized_backward_composition
    | `Conj         -> p conjunction
    | `RP           -> p remove_punctuation
    | `CommaVPtoADV -> p comma_vp_to_adv
    | `ParentDirect -> p parenthetical_direct_speech
    | (`Intro | `Unary) -> invalid_arg "apply"

let apply_rules cs rules =
    List.flatten @@ List.map (apply cs) rules

let combinators = [`FwdApp; `BwdApp; `FwdCmp; `BwdCmp; `GenFwdCmp;
    `GenBwdCmp; `Conj; `RP; `CommaVPtoADV; `ParentDirect]

module JC = Cat.JapaneseCategories
module Test =
struct
    open Cat.JapaneseCategories
    let forward_application = function
        | `Fwd (x, y), y' when y =:= y' -> [unify (if x = y then y' else x) y y']
        | _ -> []
end
let () =
    let c1 = JC.parse "(S[mod=adn,form=attr,fin=f]/NP[case=ga,mod=nm,fin=f])" in
    let c2 = JC.parse "NP[case=ga,mod=nm,fin=f]" in
    print_endline (JC.show c1);
    print_endline (JC.show c2);
    match Test.forward_application (c1, c2) with
    | [c] -> print_endline (JC.show c)
    | _ -> ()
