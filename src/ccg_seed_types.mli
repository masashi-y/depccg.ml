(** ccg_seed.proto Types *)



(** {2 Types} *)

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
}

type ccgseeds = {
  lang : string;
  categories : string list;
  seeds : ccgseed list;
}


(** {2 Default values} *)

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
