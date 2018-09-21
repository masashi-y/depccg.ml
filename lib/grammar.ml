
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

module Tree (Cat : CATEGORIES) (Rules : RULES) = 
struct
    type cat = Cat.t
    type op = Rules.t
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

    let make ~cat ~op ~children =
        {cat=cat; op=op; children=children; str=""}

    let terminal cat s =
        {cat=cat; op=Rules.intro; children=[]; str=s}

    let make_scored ?(score=0.0) t = [(score, t)]

    let is_terminal = function
        | {children=[]} -> true
        | _ -> false

    let rec terminals = function
        | {str=""; children} -> List.flatten (List.map terminals children)
        | {str=s} -> [s]

    let rec preterminals = function
        | {cat; children=[]} -> [cat]
        | {children} -> List.flatten (List.map preterminals children)

    let length t = List.length (terminals t)

(*
      some    dogs
       N/N      N
    fa--------------
             N

==> N (`FwdApp, `N _, [
        T (`Fwd (`N _, `N _), "some");
        T (`N _, "dogs")
    ])
*)
    let view {cat; op; children} =
        let cats = List.map (
            fun {cat; op; str} -> T (cat, str)) children in
        N (op, cat, cats)

(*
      some    dogs     run
       N/N      N     S\N
    fa--------------
             N
    ba---------------------
                S

==> N (`BwdApp, `S _, [ 
        N (`FwdApp, `N _, [
            T (`Fwd (`N _, `N _), "some");
            T (`N _, "dogs")
        ]);
        T (`Bwd (`S _, `N), "run")
    ])
*)
    let view2 {cat; op; children} =
        let subview = List.map (
            fun {op; cat; children} ->
            let subsub = List.map (
                fun {op; cat; str} ->
                    T (cat, str)) children in
            N (op, cat, subsub)) children in
        N (op, cat, subview)

    let rec of_view = function
        | T (cat, str) -> terminal cat str
        | N (op, cat, subviews) ->
            make ~cat ~op ~children:(List.map of_view subviews)
        | P parse -> parse

    let left_child = function
        | {children=[c1; _]} -> c1
        | _ -> invalid_arg "left_child"

    let right_child = function
        | {children=[_; c2]} -> c2
        | _ -> invalid_arg "right_child"

    let match_with_type_raised = function
        | {cat; children=[{cat=cat'} as child]}
            when Cat.is_type_raised cat &&
            Cat.(cat' =:= np || cat' =:= pp_) -> Some (cat, child)
        | _ -> None

    let match_with_unary = function
        | {children=[c1]} -> Some c1
        | _ -> None

    let match_with_binary = function
        | {children=[c1; c2]} -> Some (c1, c2)
        | _ -> None
end


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

module BaseGrammar (Cat : CATEGORIES) =
struct
    open Cat

    type t = base_rules

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

type en_rules = [ `FwdCmp
                | `BwdCmp
                | `GenFwdCmp
                | `GenBwdCmp
                | `Conj           (* Conjunction *)
                | `RP             (* RemovePunctuation *)
                | `CommaVPtoADV   (* CommaAndVerbPhraseToAdverb *)
                | `ParentDirect   (* ParentheticalDirectSpeech *)
                | base_rules]

module EnglishGrammar =
struct
    type feature = en_feature
    module Feature = EnglishFeature
    module Cat = Categories (Feature)
    module Base = BaseGrammar (Cat)

    type rules = en_rules

    module Rules = struct
        type t = rules

        let to_list =
            [`FwdApp; `BwdApp; `FwdCmp; `BwdCmp;
           `GenFwdCmp; `GenBwdCmp; `Conj; `RP;
           `CommaVPtoADV; `ParentDirect]

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
    end

    module Tree = Tree (Cat) (Rules)

    open Cat

    module Notat =
    struct
        include EnglishCategories.Notat
        include EnglishFeature.Notat

        let lex = `Intro
        let fa = `FwdApp
        let ba = `BwdApp
        let un = `Unary
        let fc = `FwdCmp
        let bx = `BwdCmp
        let gfc = `GenFwdCmp
        let gbx = `GenBwdCmp
        let conj = `Conj
        let rp = `RP
    end

    open Notat

    let possible_root_cats = [s_(dcl); s_(wq); s_(q); s_(qem); np]

    (* S[dcl] S[em]\S[em] --> S[em] *)
    let backward_application = function
        | `S `DCL, `Bwd (`S `EM, `S `EM) -> [s_(em)]
        | x, `Bwd (y, x') when x =:= x' -> [unify (if y = x' then x else y) x' x]
        | _ -> []

    let conjunction cs =
        let puncts = [","; ";"; "conj"] in 
        let conj1 = function
            | _, `Bwd (`NP _, `NP _)
            | _, `N _  -> []
            | `Punct x, y when List.mem x puncts
                         && not (is_punct y)
                         (* && not (is_type_raised y) *) -> [y |: y]
            | _ -> [] 
        in
        let conj2 = function
            | `Punct "conj", `Bwd (`NP `None, `NP `None) -> [np]
            | _ -> [] 
        in conj1 cs @ conj2 cs

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
        List.flatten @@ List.map (apply cs) Rules.to_list

    let resolve_dependency (head, dep) _ = (head, dep)

    let is_acceptable_unary c r = r <> `RP || Cat.is_type_raised c

    let apply_rules_with_cache cache (c1, c2) =
        let prep = Cat.remove_some_feat [`Nb] in
        let cats = (prep c1, prep c2) in
        try Hashtbl.find cache cats with Not_found ->
            let rules = apply_rules cats in
            Hashtbl.add cache cats rules;
            rules

    let is_seen seen_rules (c1, c2) =
        let prep = Cat.remove_some_feat [`Var; `Nb] in
        Hashtbl.mem seen_rules (prep c1, prep c2)
end

type ja_rules = [ `FwdCmp
                | `BwdCmp
                | `GenBwdCmp2
                | `GenBwdCmp3
                | `GenBwdCmp4
                | `CrsFwdCmp1
                | `CrsFwdCmp2
                | `CrsFwdCmp3
                | `Conj
                | base_rules]

module JapaneseGrammar =
struct
    type feature = ja_feature
    module Feature = JapaneseFeature
    module Cat = Categories (Feature)
    module Base = BaseGrammar (Cat)

    type rules = ja_rules

    module Rules = struct

        type t = rules

        let to_list =
            [`FwdApp; `BwdApp; `FwdCmp; `BwdCmp;
           `GenBwdCmp2; `GenBwdCmp3; `GenBwdCmp4;
           `CrsFwdCmp1; `CrsFwdCmp2; `CrsFwdCmp3; `Conj]

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
    end

    module Tree = Tree (Cat) (Rules)

    open Cat

    module Notat =
    struct
        include JapaneseFeatureValue.Notat
        include JapaneseCategories.Notat

        let lex = `Intro
        let fa = `FwdApp
        let ba = `BwdApp
        let un = `Unary
        let fc = `FwdCmp
        let bc = `BwdCmp
        let gbc2 = `GenBwdCmp2
        let gbc3 = `GenBwdCmp3
        let gbc4 = `GenBwdCmp4
        let xfc1 = `CrsFwdCmp1
        let xfc2 = `CrsFwdCmp2
        let xfc3 = `CrsFwdCmp3
        let conj = `Conj
    end

    open Notat

    let possible_root_cats = [
        np_(nc, nm, f);  np_(nc, nm, t);
        s_(nm, attr, t); s_(nm, base, f);
        s_(nm, base, t); s_(nm, cont, f);
        s_(nm, cont, t); s_(nm, da, f);
        s_(nm, da, t);   s_(nm, hyp, t);
        s_(nm, imp, f);  s_(nm, imp, t);
        s_(nm, r, t);    s_(nm, s, t);
        s_(nm, stem, f); s_(nm, stem, t) ]

    let conjoin = function
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
        List.flatten @@ List.map (apply cs) Rules.to_list

    let resolve_dependency (t1, t2) (len1, len2) = (t2 + len2 - 1, t1 + len1 - 1)

    let is_acceptable_unary _ _ = true

    let apply_rules_with_cache cache cats =
        try Hashtbl.find cache cats with Not_found ->
            let rules = apply_rules cats in
            Hashtbl.add cache cats rules;
            rules

    (* check a pair of cats is seen in a dictionary *)
    let is_seen seen_rules cats = Hashtbl.mem seen_rules cats

end

