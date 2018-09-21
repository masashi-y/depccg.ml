
module type Ordered =
sig
    type t
   
    val compare : t -> t -> int
end

module type S =
sig
    type t

    type elt

    val length : t -> int

    val is_empty : t -> bool

    val empty : unit -> t

    val add : elt -> t -> t

    val singleton : elt -> t

    val min : t -> elt option

    val pop : t -> (elt * t) option

    val fold_at_most : int -> (elt -> 'a -> 'a) -> 'a -> t -> 'a
end

module Make (Item : Ordered) : S with type elt = Item.t
