[@@@ocaml.warning "-27-30-39"]


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

let rec default_matrix 
  ?values:((values:float list) = [])
  ?shape:((shape:int list) = [])
  () : matrix  = {
  values;
  shape;
}

let rec default_ccgseed 
  ?id:((id:int) = 0)
  ?sentence:((sentence:string list) = [])
  ?cat_probs:((cat_probs:matrix option) = None)
  ?dep_probs:((dep_probs:matrix option) = None)
  () : ccgseed  = {
  id;
  sentence;
  cat_probs;
  dep_probs;
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
