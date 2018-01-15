[@@@ocaml.warning "-27-30-39"]


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
  () : ccgseed  = {
  id;
  sentence;
  cat_probs;
  dep_probs;
  attribs;
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
