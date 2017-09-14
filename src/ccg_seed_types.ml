[@@@ocaml.warning "-27-30-39"]


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

let rec default_ccgseed 
  ?id:((id:int32) = 0l)
  ?sentence:((sentence:string list) = [])
  ?cat_probs:((cat_probs:float list) = [])
  ?dep_probs:((dep_probs:float list) = [])
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
