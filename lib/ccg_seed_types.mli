(** ccg_seed.proto Types *)



(** {2 Types} *)

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


(** {2 Default values} *)

val default_terminal_constraint : 
  ?category:string ->
  ?start:int ->
  unit ->
  terminal_constraint
(** [default_terminal_constraint ()] is the default value for type [terminal_constraint] *)

val default_non_terminal_constraint : 
  ?category:string option ->
  ?start:int ->
  ?length:int ->
  unit ->
  non_terminal_constraint
(** [default_non_terminal_constraint ()] is the default value for type [non_terminal_constraint] *)

val default_constraint_ : unit -> constraint_
(** [default_constraint_ ()] is the default value for type [constraint_] *)

val default_attribute : 
  ?lemma:string option ->
  ?pos:string option ->
  ?chunk:string option ->
  ?entity:string option ->
  unit ->
  attribute
(** [default_attribute ()] is the default value for type [attribute] *)

val default_matrix : 
  ?values:float list ->
  ?shape:int list ->
  unit ->
  matrix
(** [default_matrix ()] is the default value for type [matrix] *)

val default_ccgseed : 
  ?id:string option ->
  ?sentence:string list ->
  ?cat_probs:matrix ->
  ?dep_probs:matrix ->
  ?attribs:attribute list ->
  ?constraints:constraint_ list ->
  unit ->
  ccgseed
(** [default_ccgseed ()] is the default value for type [ccgseed] *)

val default_ccgseeds : 
  ?lang:string ->
  ?categories:string list ->
  ?seeds:ccgseed list ->
  unit ->
  ccgseeds
(** [default_ccgseeds ()] is the default value for type [ccgseeds] *)
