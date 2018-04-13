
module Attribute :
sig
    type t

    val lemma  : ?def:string -> t -> string
    val pos    : ?def:string -> t -> string
    val chunk  : ?def:string -> t -> string
    val entity : ?def:string -> t -> string

    val default : ?lemma:string
               -> ?pos:string
               -> ?chunk:string
               -> ?entity:string
               -> unit -> t
end

type t = Attribute.t list

val from_protobuf : Ccg_seed_types.ccgseed -> t

val default : int -> t
