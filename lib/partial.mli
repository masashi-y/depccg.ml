
type constraint_ = N of string option * int * int
                 | T of string * int

type constraints = constraint_ list

val parse : string -> constraints * string list

val show : constraint_ list -> string

val to_protobuf : constraint_ -> Ccg_seed_types.constraint_
