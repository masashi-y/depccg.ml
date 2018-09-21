(** ccg_seed.proto Binary Encoding *)


(** {2 Protobuf Encoding} *)

val encode_terminal_constraint : Ccg_seed_types.terminal_constraint -> Pbrt.Encoder.t -> unit
(** [encode_terminal_constraint v encoder] encodes [v] with the given [encoder] *)

val encode_non_terminal_constraint : Ccg_seed_types.non_terminal_constraint -> Pbrt.Encoder.t -> unit
(** [encode_non_terminal_constraint v encoder] encodes [v] with the given [encoder] *)

val encode_constraint_ : Ccg_seed_types.constraint_ -> Pbrt.Encoder.t -> unit
(** [encode_constraint_ v encoder] encodes [v] with the given [encoder] *)

val encode_attribute : Ccg_seed_types.attribute -> Pbrt.Encoder.t -> unit
(** [encode_attribute v encoder] encodes [v] with the given [encoder] *)

val encode_matrix : Ccg_seed_types.matrix -> Pbrt.Encoder.t -> unit
(** [encode_matrix v encoder] encodes [v] with the given [encoder] *)

val encode_ccgseed : Ccg_seed_types.ccgseed -> Pbrt.Encoder.t -> unit
(** [encode_ccgseed v encoder] encodes [v] with the given [encoder] *)

val encode_ccgseeds : Ccg_seed_types.ccgseeds -> Pbrt.Encoder.t -> unit
(** [encode_ccgseeds v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_terminal_constraint : Pbrt.Decoder.t -> Ccg_seed_types.terminal_constraint
(** [decode_terminal_constraint decoder] decodes a [terminal_constraint] value from [decoder] *)

val decode_non_terminal_constraint : Pbrt.Decoder.t -> Ccg_seed_types.non_terminal_constraint
(** [decode_non_terminal_constraint decoder] decodes a [non_terminal_constraint] value from [decoder] *)

val decode_constraint_ : Pbrt.Decoder.t -> Ccg_seed_types.constraint_
(** [decode_constraint_ decoder] decodes a [constraint_] value from [decoder] *)

val decode_attribute : Pbrt.Decoder.t -> Ccg_seed_types.attribute
(** [decode_attribute decoder] decodes a [attribute] value from [decoder] *)

val decode_matrix : Pbrt.Decoder.t -> Ccg_seed_types.matrix
(** [decode_matrix decoder] decodes a [matrix] value from [decoder] *)

val decode_ccgseed : Pbrt.Decoder.t -> Ccg_seed_types.ccgseed
(** [decode_ccgseed decoder] decodes a [ccgseed] value from [decoder] *)

val decode_ccgseeds : Pbrt.Decoder.t -> Ccg_seed_types.ccgseeds
(** [decode_ccgseeds decoder] decodes a [ccgseeds] value from [decoder] *)
