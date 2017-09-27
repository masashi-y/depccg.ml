
module H = Hashtbl

type ('a, 'b) cell = Empty
                   | Filled of ('a, 'b) H.t

type index = (int * int -> int)

type ('a, 'b) t = {cells:('a, 'b) cell array; index:index; length:int}

let make n = {cells=Array.make (n * n) Empty; index=(fun (i, j) -> i * n + j); length=n}

let update {cells=chart; index=idx} i k v =
    let update_with_check cell =
        if H.mem cell k then false else (H.add cell k v; true)
    in match chart.(idx i) with
    | Empty -> let cell = H.create 10 in
               let () = H.add cell k v in
               chart.(idx i) <- Filled cell;
               true
    | Filled cell -> update_with_check cell

let get {cells=chart; index=idx} i = chart.(idx i)

let iter_at {cells=chart; index=idx} i f =
    match chart.(idx i) with
    | Empty -> ()
    | Filled cell -> H.iter f cell

let iter_along g {cells=chart; index=idx; length=n} i f =
    let rec iter j = if j >= n then () else
        match chart.(idx (g i j)) with
        | Empty -> iter (j+1)
        | Filled cell -> H.iter f cell; iter (j+1)
    in iter 0

let row = (fun i j -> (i, j))
and column = (fun i j -> (j, i))

let iter_along_row c = iter_along row c
and iter_along_col c = iter_along column c

(*        traverse like this         *)
(*           * * * * * *             *)
(*           o o o o o *             *)
(*           * * * * * *             *)
(*           o o o * * *             *)
(*           * * * * * *             *)
(*           * * * * * *             *)
let fold_along_row {cells=chart; index=idx; length=n} i f init =
    let rec iter j res =
        if j >= (n - i) then res
        else match chart.(idx (i, j)) with
        | Empty -> iter (j+1) res
        | Filled cell -> iter (j+1) (H.fold f cell res)
    in iter 0 init

(*        traverse like this         *)
(*           * * * o * o             *)
(*           * * o * o *             *)
(*           * o * o * *             *)
(*           o * o * * *             *)
(*           * o * * * *             *)
(*           o * * * * *             *)
let fold_along_diag {cells=chart; index=idx; length=n} i f init =
    let rec iter (i, j) res = 
        if j >= n || i < 0 then res
        else match chart.(idx (i, j)) with
        | Empty -> iter (i-1, j+1) res
        | Filled cell -> iter (i-1, j+1) (H.fold f cell res)
    in iter (i, 0) init

let complete_parses {cells=chart; index=idx; length=n} =
    let f _ elt l = elt :: l in
    match chart.(idx (0, n - 1)) with
    | Empty       -> []
    | Filled cell -> H.fold f cell []

let n_complete_parses {cells=chart; index=idx; length=n} =
    match chart.(idx (0, n - 1)) with
    | Empty       -> 0
    | Filled cell -> H.fold (fun _ _ n -> n + 1) cell 0
