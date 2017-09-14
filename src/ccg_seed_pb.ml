[@@@ocaml.warning "-27-30-39"]

type ccgseed_mutable = {
  mutable id : int32;
  mutable sentence : string list;
  mutable cat_probs : float list;
  mutable dep_probs : float list;
}

let default_ccgseed_mutable () : ccgseed_mutable = {
  id = 0l;
  sentence = [];
  cat_probs = [];
  dep_probs = [];
}

type ccgseeds_mutable = {
  mutable lang : string;
  mutable categories : string list;
  mutable seeds : Ccg_seed_types.ccgseed list;
}

let default_ccgseeds_mutable () : ccgseeds_mutable = {
  lang = "";
  categories = [];
  seeds = [];
}


let rec decode_ccgseed d =
  let v = default_ccgseed_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.dep_probs <- List.rev v.dep_probs;
      v.cat_probs <- List.rev v.cat_probs;
      v.sentence <- List.rev v.sentence;
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.id <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(ccgseed), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.sentence <- (Pbrt.Decoder.string d) :: v.sentence;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(ccgseed), field(2)" pk
    | Some (3, Pbrt.Bits64) -> begin
      v.cat_probs <- (Pbrt.Decoder.float_as_bits64 d) :: v.cat_probs;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(ccgseed), field(3)" pk
    | Some (4, Pbrt.Bits64) -> begin
      v.dep_probs <- (Pbrt.Decoder.float_as_bits64 d) :: v.dep_probs;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(ccgseed), field(4)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Ccg_seed_types.id = v.id;
    Ccg_seed_types.sentence = v.sentence;
    Ccg_seed_types.cat_probs = v.cat_probs;
    Ccg_seed_types.dep_probs = v.dep_probs;
  } : Ccg_seed_types.ccgseed)

let rec decode_ccgseeds d =
  let v = default_ccgseeds_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.seeds <- List.rev v.seeds;
      v.categories <- List.rev v.categories;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.lang <- Pbrt.Decoder.string d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(ccgseeds), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.categories <- (Pbrt.Decoder.string d) :: v.categories;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(ccgseeds), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.seeds <- (decode_ccgseed (Pbrt.Decoder.nested d)) :: v.seeds;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(ccgseeds), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Ccg_seed_types.lang = v.lang;
    Ccg_seed_types.categories = v.categories;
    Ccg_seed_types.seeds = v.seeds;
  } : Ccg_seed_types.ccgseeds)

let rec encode_ccgseed (v:Ccg_seed_types.ccgseed) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Ccg_seed_types.id encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  ) v.Ccg_seed_types.sentence;
  List.iter (fun x -> 
    Pbrt.Encoder.key (3, Pbrt.Bits64) encoder; 
    Pbrt.Encoder.float_as_bits64 x encoder;
  ) v.Ccg_seed_types.cat_probs;
  List.iter (fun x -> 
    Pbrt.Encoder.key (4, Pbrt.Bits64) encoder; 
    Pbrt.Encoder.float_as_bits64 x encoder;
  ) v.Ccg_seed_types.dep_probs;
  ()

let rec encode_ccgseeds (v:Ccg_seed_types.ccgseeds) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Ccg_seed_types.lang encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  ) v.Ccg_seed_types.categories;
  List.iter (fun x -> 
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_ccgseed x) encoder;
  ) v.Ccg_seed_types.seeds;
  ()
