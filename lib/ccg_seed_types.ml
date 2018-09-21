[@@@ocaml.warning "-27-30-39"]


type terminal_constraint = {
  category : string;
  start : int;
}

type non_terminal_constraint = {
  category : string option;
  start : int;
  length : int;
}

type constraint_ =
  | Terminal of terminal_constraint
  | Nonterminal of non_terminal_constraint

type attribute = {
  lemma : string option;
  pos : string option;
  chunk : string option;
  entity : string option;
}

type matrix = {
  values : float list;
  shape : int list;
}

type ccgseed = {
  id : string option;
  sentence : string list;
  cat_probs : matrix;
  dep_probs : matrix;
  attribs : attribute list;
  constraints : constraint_ list;
}

type ccgseeds = {
  lang : string;
  categories : string list;
  seeds : ccgseed list;
}

let rec default_terminal_constraint 
  ?category:((category:string) = "")
  ?start:((start:int) = 0)
  () : terminal_constraint  = {
  category;
  start;
}

let rec default_non_terminal_constraint 
  ?category:((category:string option) = None)
  ?start:((start:int) = 0)
  ?length:((length:int) = 0)
  () : non_terminal_constraint  = {
  category;
  start;
  length;
}

let rec default_constraint_ () : constraint_ = Terminal (default_terminal_constraint ())

let rec default_attribute 
  ?lemma:((lemma:string option) = None)
  ?pos:((pos:string option) = None)
  ?chunk:((chunk:string option) = None)
  ?entity:((entity:string option) = None)
  () : attribute  = {
  lemma;
  pos;
  chunk;
  entity;
}

let rec default_matrix 
  ?values:((values:float list) = [])
  ?shape:((shape:int list) = [])
  () : matrix  = {
  values;
  shape;
}

let rec default_ccgseed 
  ?id:((id:string option) = None)
  ?sentence:((sentence:string list) = [])
  ?cat_probs:((cat_probs:matrix) = default_matrix ())
  ?dep_probs:((dep_probs:matrix) = default_matrix ())
  ?attribs:((attribs:attribute list) = [])
  ?constraints:((constraints:constraint_ list) = [])
  () : ccgseed  = {
  id;
  sentence;
  cat_probs;
  dep_probs;
  attribs;
  constraints;
}

let rec default_ccgseeds 
  ?lang:((lang:string) = "")
  ?categories:((categories:string list) = [])
  ?seeds:((seeds:ccgseed list) = [])
  () : ccgseeds  = {
  lang;
  categories;
  seeds;
}
