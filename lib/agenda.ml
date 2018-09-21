
open Ocaml_base

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

module Make (Item : Ordered) =
struct

    type elt = Item.t

    type t = elt PriorityQueue.t

    let length = PriorityQueue.length

    let is_empty = PriorityQueue.is_empty

    let empty () =
        PriorityQueue.make (fun x y -> Item.compare x y <= 0)

    let add item queue =
        PriorityQueue.add queue item;
        queue

    let singleton item =
        add item (empty ())

    let min queue =
       try Some (PriorityQueue.first queue)
       with Failure _ -> None

    let pop queue =
        try
            let first = PriorityQueue.first queue in
            PriorityQueue.remove_first queue;
            Some (first, queue)
        with Failure _ ->
            None

    let rec fold_at_most n f init queue =
        if n <= 0 then init else
        match pop queue with
        | None -> init
        | Some (v, queue) -> fold_at_most (n-1) f (f v init) queue

end

