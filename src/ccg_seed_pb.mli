(** ccg_seed.proto Binary Encoding *)


(** {2 Protobuf Encoding} *)

val encode_matrix : Ccg_seed_types.matrix -> Pbrt.Encoder.t -> unit
(** [encode_matrix v encoder] encodes [v] with the given [encoder] *)

val encode_ccgseed : Ccg_seed_types.ccgseed -> Pbrt.Encoder.t -> unit
(** [encode_ccgseed v encoder] encodes [v] with the given [encoder] *)

val encode_ccgseeds : Ccg_seed_types.ccgseeds -> Pbrt.Encoder.t -> unit
(** [encode_ccgseeds v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_matrix : Pbrt.Decoder.t -> Ccg_seed_types.matrix
(** [decode_matrix decoder] decodes a [matrix] value from [decoder] *)

val decode_ccgseed : Pbrt.Decoder.t -> Ccg_seed_types.ccgseed
(** [decode_ccgseed decoder] decodes a [ccgseed] value from [decoder] *)

val decode_ccgseeds : Pbrt.Decoder.t -> Ccg_seed_types.ccgseeds
(** [decode_ccgseeds decoder] decodes a [ccgseeds] value from [decoder] *)
