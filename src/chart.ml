
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

let goes_along_row = (fun i j -> (i, j))
and goes_along_col = (fun i j -> (j, i))

let iter_along_row c = iter_along goes_along_row c
and iter_along_col c = iter_along goes_along_col c

let fold_along g {cells=chart; index=idx; length=n} ~index ~f ~init =
    let rec iter j res = if j >= n then res
        else match chart.(idx (g index j)) with
        | Empty -> iter (j+1) res
        | Filled cell -> iter (j+1) (H.fold f cell res)
    in iter 0 init

let fold_along_row c = fold_along goes_along_row c
and fold_along_col c = fold_along goes_along_col c

