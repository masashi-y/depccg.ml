(** ccg_seed.proto Types *)



(** {2 Types} *)

type matrix = {
  values : float list;
  shape : int list;
}

type ccgseed = {
  id : int;
  sentence : string list;
  cat_probs : matrix option;
  dep_probs : matrix option;
}

type ccgseeds = {
  lang : string;
  categories : string list;
  seeds : ccgseed list;
}


(** {2 Default values} *)

val default_matrix : 
  ?values:float list ->
  ?shape:int list ->
  unit ->
  matrix
(** [default_matrix ()] is the default value for type [matrix] *)

val default_ccgseed : 
  ?id:int ->
  ?sentence:string list ->
  ?cat_probs:matrix option ->
  ?dep_probs:matrix option ->
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
