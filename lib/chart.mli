
type ('a, 'b) t

val make : int -> int -> ('a, 'b) t

val update : ('a, 'b) t -> int * int -> 'a -> 'b -> bool

val iter_at : ('a, 'b) t -> int * int -> ('a -> 'b -> unit) -> unit

val iter_along_row : ('a, 'b) t -> int -> ('a -> 'b -> unit) -> unit 

val iter_along_col : ('a, 'b) t -> int -> ('a -> 'b -> unit) -> unit 

(*        traverse like this         *)
(*           * * * * * *             *)
(*           o o o o o *             *)
(*           * * * * * *             *)
(*           o o o * * *             *)
(*           * * * * * *             *)
(*           * * * * * *             *)
val fold_along_row : ('a, 'b) t -> int -> ('a -> 'b -> 'c -> 'c) -> 'c -> 'c

(*        traverse like this         *)
(*           * * * o * o             *)
(*           * * o * o *             *)
(*           * o * o * *             *)
(*           o * o * * *             *)
(*           * o * * * *             *)
(*           o * * * * *             *)
val fold_along_diag : ('a, 'b) t -> int -> ('a -> 'b -> 'c -> 'c) -> 'c -> 'c

val complete_parses : ('a, 'b) t -> 'b list

val n_complete_parses : ('a, 'b) t -> int
