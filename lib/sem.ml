
include Grammar

module Cat = EnglishGrammar.Cat
module Rules = EnglishGrammar.Rules

module Condition =
struct
    type t = [ `rule of Rules.t
             | `root of string
             | `coq_type of int
             | `pos of string
             | `surf of string
             | `base of string
             | `chlid_surf of int * string
             | `chlid_category of int * Cat.t
             | `child_any_pos of string
             | `child_any_base of string
             | `child_any_category of Cat.t ]

    let compare = compare
end

module SemanticRule =
struct
    include Map.Make (Condition)
end


module SemanticTemplate =
struct
    type t = {
        semantics : int;
        category : Cat.t;
        (* semantic_rule : SemanticRule.t *)
    }
end
