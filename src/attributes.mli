
module Attribute :
sig
    type t

    val lemma : t -> string option
    val pos : t -> string option
    val chunk : t -> string option
    val entity : t -> string option

    val default : ?lemma:string option
               -> ?pos:string option
               -> ?chunk:string option
               -> ?entity:string option
               -> unit -> t
end

type t

