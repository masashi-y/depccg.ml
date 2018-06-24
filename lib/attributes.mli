
module Attribute :
sig
    type t

    type label = [ `Lemma of string
                 | `Pos of string
                 | `Chunk of string
                 | `Entity of string ]

    val lemma  : ?def:string -> t -> string
    val pos    : ?def:string -> t -> string
    val chunk  : ?def:string -> t -> string
    val entity : ?def:string -> t -> string

    val lemma_opt  : t -> string option
    val pos_opt    : t -> string option
    val chunk_opt  : t -> string option
    val entity_opt : t -> string option

    val default : ?lemma:string
               -> ?pos:string
               -> ?chunk:string
               -> ?entity:string
               -> unit -> t

    val update : t -> label -> t

end

module NodeAttribute :
sig
    type t

    type label = [ `Polarity of string ]

    val polarity : ?def:string -> t -> string

    val polarity_opt : t -> string option

    val default : ?polarity:string -> unit -> t

    val update : t -> label -> t
end

type t

type attrs = t

val of_lists : ?leaves:(Attribute.t list)
            -> ?nodes:(NodeAttribute.t list)
            -> unit -> t

val to_lists : t -> Attribute.t list * NodeAttribute.t list

val rev : t -> t

val of_protobuf : Ccg_seed_types.ccgseed -> t

val default : unit -> t

val is_empty : t -> bool

val update : ?leaves:(Attribute.t list)
            -> ?nodes:(NodeAttribute.t list)
            -> t -> t

module AttributeM : 
sig
    type ('a, 'b) t

    val bind : ('a, 'c) t -> ('a -> ('b, 'c) t) -> ('b, 'c) t
    val (>>=) : ('a, 'c) t -> ('a -> ('b, 'c) t) -> ('b, 'c) t

    (* these two functions does not pass state transitions *)
    val (&&&) : ('a -> ('b, 'c) t) -> ('e -> ('d, 'c) t) -> (('a * 'e) -> (('b * 'd), 'c) t)
    val sequence' : ('a, 'b) t list -> ('a list, 'b) t

    val return : 'a -> ('a, 'b) t

    val get : ('a, 'a) t
    val put : 'a -> (unit, 'a) t

    val run : ('a, 'b) t -> 'b -> 'a * 'b
    val eval : ('a, 'b) t -> 'b -> 'a
    val exec : ('a, 'b) t -> 'b -> 'b

    val push : Attribute.t -> (unit, attrs) t
    val pushn : NodeAttribute.t -> (unit, attrs) t
    val pop : unit -> (Attribute.t, attrs) t
    val popn : unit -> (NodeAttribute.t, attrs) t
    val popi : ?incr:bool -> unit -> (int * Attribute.t, int * attrs) t
    val popni : ?incr:bool -> unit -> (int * NodeAttribute.t, int * attrs) t

    val mapM : ('a -> ('b, 'c) t) -> 'a list -> ('b list, 'c) t
    val rev_mapM : ('a -> ('b, 'c) t) -> 'a list -> ('b list, 'c) t
    val fold_leftM : ('a -> 'b -> ('a, 'c) t) -> 'a -> 'b list -> ('a, 'c) t
end
