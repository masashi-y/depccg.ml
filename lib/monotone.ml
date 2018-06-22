
include Utils

module Cat = Grammar.EnglishGrammar.Cat
module Rules = Grammar.EnglishGrammar.Rules
module Tree = Grammar.EnglishGrammar.Tree
module Notat = Grammar.EnglishGrammar.Notat

module Fix =
struct
    open Tree
    open Rules
    open Notat
(*
    BEFORE:
      most    dogs
       N/N      N
    fa--------------
             N
    lex-------------
             NP

    AFTER:
      most NP/N     dogs N
    fa----------------------
               NP
*) 
    let rec most ({children} as t) =
        match Tree.view2 t, children with
        N (`Unary, `NP _, [
            N (`FwdApp, `N _, [
                T (`Fwd (`N _, `N _), "most");
                T (`N _, _)
            ]) ]), [_; c2] ->
            Tree.of_view (
                N (`FwdApp, np, [
                    T (np /: n, "most");
                    P c2 ]))
        | _ -> {t with children=List.map most children}

(*
    BEFORE:
    no        young man    that     every woman hits OR eats pizzas
    NP/N          N    (NP\NP)/(S\/NP)       S/NP or S\NP
    -----------------    ---------------------------------
          NP                         NP\NP
         -------------------------------------
                             NP

    AFTER:
                          that     every woman hits OR eats pizzas
          young man  (NP\NP)/(S\/NP)       S/NP or S\NP
          ---------  ----------------------------------
             N                    NP\NP
            --------- lex
     no          NP
    ----       ---------------------------------- ba
    NP/N                         NP
                     ---------------------- unlex
                                 N
    ------------------------------------ fa
                   NP
*)
    let relatives = ["that"; "which"; "who"]

    let rec relative_clause ({children} as t) = 
        let open Cat in
        match Tree.view2 t, children with
        N (`BwdApp, `NP _, [
            N (`FwdApp, `NP _, [
                T (`Fwd (`NP _, `N _), w1);
                T (`N _, w2)
            ]);
            N (_, `Bwd (`NP _, `NP _), [
                T (`Fwd (`Bwd (`NP _, `NP _), cat1), w3);
                T (cat2, w4)
            ])
        ]), [c1; c2] when List.mem w3 relatives
                    && (cat1 =:= (s /: np) || cat1 =:= (s |: np))
                    && (cat1 =:= cat2) ->
            Tree.of_view (N (`FwdApp, np, [
                T (np /: n, w1);
                N (`Unary, n, [ (* TODO: unlex *)
                    N (`FwdApp, np, [
                        P (right_child c1);
                        P c2
                    ]) ]) ]))
        | _ -> {t with children=List.map relative_clause children}
end

module Direction =
struct
    type t = Up | Down | Unknown

    let up = Up
    let down = Down
    let unknown = Unknown

    let rec show = function
        | Up -> "↑" 
        | Down -> "↓"
        | Unknown -> "?"

    let of_word = function
        | "who" | "that" | "which" -> up
        | "and" | "or" -> up
        | "but" -> down
        | _ -> unknown
end

module Polarity =
struct
    type t = Plus | Minus | Dot

    let p = Plus
    let m = Minus
    let dot = Dot

    let show = function
        | Plus -> "+"
        | Minus -> "-"
        | Dot -> ""
end

module SemanticType =
struct
    open Notat
    type t = Entity
           | Truth
           | Fun of t * t * Polarity.t
           | Noise of string

    let e = Entity
    let t = Truth
    let (+~>) x y = Fun (x, y, Polarity.p)
    let (-~>) x y = Fun (x, y, Polarity.m)
    let (|~>) x y = Fun (x, y, Polarity.dot)
    let (|?~>) x y z = Fun (x, y, z)
    let s = t
    let n = e |~> t
    let np = n |~> t
    let np_plus = n +~> t
    let np_minus = n -~> t
    let noise s = Noise s
    let ( ^ ) x p = match x with
        | Fun (x, y, _) -> Fun (x, y, p)
        | _ -> x

    let rec show = function
        | Entity -> "e"
        | Truth  -> "t"
        | Fun (x, y, polarity) ->
                !%"(%s, %s%s)" (show x) (Polarity.show polarity) (show y)
        | Noise s -> !%"**(%s)" s

    let rec of_category = function
        | `S _  -> t
        | `N _  -> e |~> t
        | `NP _ -> (e |~> t) |~> t
        | `PP _ -> noise "pp"
        | `Fwd (x, y)
        | `Bwd (x, y)
        | `Either (x, y) -> of_category x |~> of_category y
        | `Punct _  -> noise "conj"

    let of_terminal = function
        | _, "if", _ -> (t +~> t) -~> t
        | `Fwd (`NP _, `N _), ("some" | "a" | "an"), _ -> n +~> np_plus
        | `Fwd (`NP _, `N _), ("every" | "all"), _     -> n -~> np_plus
        | `Fwd (`NP _, `N _), "no", _                  -> n -~> np_minus
        | `Fwd (`NP _, `N _), "most", _                -> n |~> np_plus
        | `Bwd (`Bwd (`S _, `NP _), `Bwd (`S _, `NP _)), ("not" | "n't"), _ -> Notat.(of_category (s |: np) -~> of_category (s |: np))
        | (`Fwd (x, y) | `Bwd (x, y)), _,     "JJ"  -> of_category x +~> of_category y
        | (`Fwd (x, y) | `Bwd (x, y)), "then", _    -> of_category x +~> of_category y
        | `Fwd (`Bwd (`S _, `NP _), `NP _), _, "VB" -> np_plus +~> (np_plus +~> s)
        | `Bwd (`S _, `NP _), _, "VB" -> np +~> s
        | `NP _, _, _ -> np_plus
        | c, _, _ -> of_category c

    let of_nonterminal tree child_types =
        let f (ty1, ty2) = Polarity.(function
            | Dot, Dot -> ty1 |~> ty2
            | Plus, Plus | Minus, Minus -> ty1 +~> ty2
            | _ ->  ty1 -~> ty2) in
        match Tree.view tree, child_types with
        | N (`Unary, _, _), [types] -> types
        | N (`FwdApp, _, _), [Fun (_, types, _); _] -> types
        | N (`BwdApp, _, _), [_; Fun (_, types, _)] -> types
        | N (`FwdCmp, _, _), [Fun (ty1, _, p1); Fun (_, ty2, p2)]
        | N (`BwdCmp, _, _), [Fun (_, ty1, p1); Fun (ty2, _, p2)] -> f (ty1, ty2) (p1, p2)
        | N (`RP, x, [T (y, _); _]), [ty1; _]
        | N (`RP, x, [_; T (y, _)]), [_; ty1] when Cat.(x =:= y) -> ty1
        | _ -> failwith ""
end

module PolarizedTree =
struct
    open Attributes
    open SemanticType
    type t = {
        cat: Cat.t;
        types: SemanticType.t;
        orig: Tree.t;
        children: t list;
    }

    let rec show t = 
        let rec aux depth {cat; types; orig; children} =
            let str = Tree.(if is_terminal orig then orig.str else "") in
            let indent = String.make (depth * 2) ' ' in
            let children = String.concat "" (List.map (aux (depth + 1)) children) in
            !%"%s%s, %s, %s\n%s" indent (Cat.show cat) (SemanticType.show types) str children in
        aux 0 t

    let polarize attrs tree =
        let rec aux = AttributeM.(fun (Tree.{cat; str; children} as t) ->
            if Tree.is_terminal t then
                pop () >>= fun attr ->
                    let pos = Attribute.pos attr in
                    let types = SemanticType.of_terminal (cat, (String.lowercase_ascii str), pos) in
                    return {
                        cat;
                        types;
                        orig = t;
                        children = [];
                    }
            else
                mapM aux children >>= fun children ->
                    let child_types = List.map (fun c -> c.types) children in
                    let types = SemanticType.of_nonterminal t child_types in
                    return {
                        cat;
                        types;
                        orig = t;
                        children;
                    }
        ) in
        AttributeM.eval (aux tree) attrs
end

let () =
    let example = SemanticType.(e +~> t) in
    print_endline (SemanticType.show example);
    print_endline Direction.(show Up)


open Printer

let example =
    let open Grammar.EnglishGrammar.Notat in
    let attributes = Attributes.(of_list
        [
            Attribute.default ~pos:"DT" ();
            Attribute.default ~pos:"JJ" ();
            Attribute.default ~pos:"N" ();
            Attribute.default ~pos:"RB" ();
            Attribute.default ~pos:"DT" ();
            Attribute.default ~pos:"JJ" ();
            Attribute.default ~pos:"N" ();
            Attribute.default ~pos:"VB" ();
            Attribute.default ~pos:"VB" ();
        ]) in
    let tree = Tree.of_view (
    N (ba, s, [
        N (ba, np, [
            N (fa, np_(nb), [
                T (np_(nb) /: n, "Every");
                N (fa, n, [
                    T (n /: n, "young");
                    T (n, "man")
                ])
            ]);
            N (fa, np |: np, [
                T ((np |: np) /: (s/: np), "that");
                N (fc, s /: np, [
                    N (un, s /: (s |: np), [
                        N (fa, np_(nb), [
                            T (np_(nb) /: n, "no");
                            N (fa, n, [
                                T (n /: n, "young");
                                T (n, "woman")
                            ])
                        ])
                    ]);
                    T ((s |: np) /: np, "hits")
                ]);
            ])
        ]);
        T (s_(dcl) |: np, "cried")
    ])) in
    print_endline (EnglishPrinter.show_derivation tree);
    print_endline (EnglishPrinter.show_derivation (Fix.relative_clause tree));
    print_endline (PolarizedTree.(show (polarize attributes tree)))


let example =
    let open Grammar.EnglishGrammar.Notat in
    let attributes = Attributes.(of_list
        [
            Attribute.default ~pos:"DT" ();
            Attribute.default ~pos:"N" ();
            Attribute.default ~pos:"VB" ();
            Attribute.default ~pos:"DT" ();
            Attribute.default ~pos:"N" ();
            Attribute.default ~pos:"N" ();
        ]) in
    let tree = Tree.of_view (
        N (rp, s, [
            N (ba, s, [
                N (fa, np, [
                    T (np_(nb) /: n, "Every");
                    T (n, "dog");
                ]);
                N (fa, s |: np, [
                    T ((s_(dcl) |: np) /: np, "hit");
                    N (fa, np, [
                        T (np_(nb) /: n, "some");
                        T (n, "cat");
                    ]);
                ]);
            ]);
            T (!:".", ".");
        ])
    ) in
    print_endline (EnglishPrinter.show_derivation tree);
    print_endline (EnglishPrinter.show_derivation (Fix.relative_clause tree));
    print_endline (PolarizedTree.(show (polarize attributes tree)))
