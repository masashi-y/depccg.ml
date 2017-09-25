
open Cat
open Utils

module Matrix = Utils.Matrix

let rec get_unification a b =
    let error () = failwith
        (!%"error in get_unification: (%s, %s)" (show_cat a) (show_cat b))
    in match (a, b) with
    | `S f,  `S f'
    | `N f,  `N f'
    | `NP f, `NP f'
    | `PP f, `PP f' -> begin
            match (f, f') with
            | "X", "X" -> None
            | "X", f''
            | f'', "X" -> if f'' = "" then None else Some f''
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
               | Some f, Some f' -> if f <> f' then error () else Some f
           end
    | _ -> error ()

let rec unify_feat c f = match c with
    | `S "X" -> `S f
    | `N "X" -> `N f
    | `NP "X" -> `NP f
    | `PP "X" -> `PP f
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

let show_combinator = function
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
    | `Fwd (x, y), y' when y =:= y' -> [unify x y y']
    | _ -> []

(* X Y\X --> Y *)  (* S[dcl] S[em]\S[em] --> S[em] *)
let backward_application = function
    | `S "dcl", `Bwd (`S "em", `S "em") -> [`S "em"]
    | x, `Bwd (y, x') when x =:= x' -> [unify y x' x]
    | _ -> []

(* X/Y Y/Z --> X/Z *)
let forward_composition = function
    | `Fwd (x, y), `Fwd (y', z) when y =:= y' -> [unify (x /: z) y y']
    | _ -> []

(* X/Y Z\X --> Z/Y *)
let backward_composition = function
    | `Fwd (x, y), `Bwd (z, x') when x =:= x' -> [unify (z /: y) x x']
    | _ -> []

(* X/Y (Y/Z)/W --> (X/Z)/W *)
let generalized_forward_composition = function
    | `Fwd (x, y), `Fwd (`Fwd (y', z), w)
        when y =:= y' -> [unify ((x /: z) /: w) y y']
    | _ -> []

(* (X\Y)\Z W\X --> (W\Y)\Z *)
let generalized_backward_composition = function
    | `Bwd (`Bwd (x, y), z), `Bwd (w, x')
        when x =:= x' -> [unify ((w |: y) |: z) x x']
    | _ -> []

(* PUNCT x --> x\x *)
let conjunction =
    let puncts = [","; ";"; "conj"] in function
    | `Punct "conj", `Bwd ((`NP _) as y, `NP _) -> [y]
    | `NP _, `Bwd (`NP _, `NP _) -> [np]
    | _    , `Bwd (`NP _, `NP _) -> []
    | _    , `N _ -> []
    | `Punct x, y when List.mem x puncts
                 && not (is_punct y)
                 && not (is_type_raised y) -> [y |: y]
    | _ -> [] 

(* , S[ng|pss]\NP --> (S\NP)\(S\NP) *)
let comma_vp_to_adv = function
    | `Punct ",", `Bwd (`S f, `NP _) when f = "ng" || f = "pss"
        -> [(s |: np) |: (s |: np)]
    | _ -> []

(* , S[dcl]/S[dcl] --> (S\NP)\(S\NP) *)
let parenthetical_direct_speech = function
    | `Punct ",", `Fwd (`S "dcl", `S "dcl")
        -> [(s |: np) |: (s |: np)]
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

let cats = read_cats "/Users/masashi-y/depccg/models/tri_headfirst/target.txt"


let () =
    let file = Sys.argv.(1) in
    let res = read_ccgseeds file in
    print_endline res.Ccg_seed_types.lang;
    print_endline (String.concat " " res.Ccg_seed_types.categories);
    let combinators = [`FwdApp; `BwdApp; `BwdCmp] in
    let cat1 = parse_cat "NP[aa]" in
    let cat2 = parse_cat "(S\\NP[X])\\NP[X]" in
    let cat3 = parse_cat "(S\\NP)/NP" in
    let cat4 = parse_cat "(S\\NP[aa])/NP" in
    let cat5 = parse_cat "(S\\NP[X])\\(S\\NP[X])" in
(* X/Y Z\X --> Z/Y *)
    let test c1 c2 =
        let res = List.map (fun c -> apply (c1, c2) c) combinators in
        match flatten res with
        | [(_, c3)] -> p "%s %s --> %s\n" (show_cat c1) (show_cat c2) (show_cat c3)
        | _ -> p "%s %s --> nothing\n" (show_cat c1) (show_cat c2)
    in
    test cat1 cat2;
    test cat3 cat1;
    test cat4 cat5;
    let open Ccg_seed_types in
    let seed = List.hd res.seeds in
    ()
