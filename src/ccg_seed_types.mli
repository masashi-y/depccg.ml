(** ccg_seed.proto Types *)



(** {2 Types} *)

type ccgseed = {
  id : int32;
  sentence : string list;
  cat_probs : float list;
  dep_probs : float list;
}

type ccgseeds = {
  lang : string;
  categories : string list;
  seeds : ccgseed list;
}


(** {2 Default values} *)

val default_ccgseed : 
  ?id:int32 ->
  ?sentence:string list ->
  ?cat_probs:float list ->
  ?dep_probs:float list ->
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
