
open Cat
open Utils

let rec get_unification a b =
    let error () = failwith
        (!%"error in get_unification: (%s, %s)" (Cat.show a) (Cat.show b))
    in match (a, b) with
    | `S f,  `S f'
    | `N f,  `N f'
    | `NP f, `NP f'
    | `PP f, `PP f' -> begin
            match (f, f') with
            | (`None | `Var _), (`None | `Var _) -> None
            | (`None | `Var _), f''
            | f'', (`None | `Var _) -> Some f''
            | _ -> None
            end
    | `Punct _ , `Punct _ -> None
    | `Fwd (x, y), `Fwd (x', y')
    | `Bwd (x, y), `Bwd (x', y')
        -> let sub1 = get_unification x x'
           and sub2 = get_unification y y' in
           begin match sub1, sub2 with
               | None, Some f
               | Some f, None -> Some f
               | None, None   -> None
               | Some f, Some f' -> Some f (* discarding here f' ... *)
           end
    | _ -> error ()

let rec unify_feat c f = match c with
    | `S (`Var _) -> `S f
    | `N (`Var _) -> `N f
    | `NP (`Var _) -> `NP f
    | `PP (`Var _) -> `PP f
    | `Fwd (x, y) -> unify_feat x f /: unify_feat y f
    | `Bwd (x, y) -> unify_feat x f |: unify_feat y f
    | _ -> c

let unify c a b =
    match get_unification a b with
    | Some f -> unify_feat c f
    | None -> c

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
    | `S (`Con "dcl"), `Bwd (`S (`Con "em"), `S (`Con "em")) -> [`S (`Con "em")]
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

(* (X\Y)|Z W\X --> (W\Y)|Z *)
let generalized_backward_composition = function
    | `Bwd (`Bwd (x, y), z), `Bwd (w, x') when x =:= x'
        -> [unify (if w = x' then (x |: y) |: z else (w |: y) |: z) x x']
    | `Fwd (`Bwd (x, y), z), `Bwd (w, x') when x =:= x'
        -> [unify (if w = x' then (x |: y) /: z else (w |: y) /: z) x x']
    | _ -> []

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
    | `Punct ",", `Bwd (`S (`Con f), `NP `None) when f = "ng" || f = "pss"
        -> [(s |: np) |: (s |: np)]
    | _ -> []

(* , S[dcl]/S[dcl] --> (S\NP)/(S\NP) *)
let parenthetical_direct_speech = function
    | `Punct ",", `Fwd (`S (`Con "dcl"), `S (`Con "dcl"))
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

