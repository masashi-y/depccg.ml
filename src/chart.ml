

type ('a, 'b) cell = Empty
                   | Filled of ('a, 'b) Hashtbl.t

type index = int * int -> int

type ('a, 'b) t = {cells  : ('a, 'b) cell array;
                   index  : index;
                   length : int;
                   nbest  : int}

let make n nbest = {cells  = Array.make (n * n) Empty;
                    index  = (fun (i, j) -> i * n + j);
                    length = n;
                    nbest  = nbest}

let update {cells; index; nbest} i k v =
    let update_with_check cell =
        let check = match nbest with
            | 1 -> Hashtbl.mem cell k
            | n -> let res = Hashtbl.find_all cell k
                   in n <= (List.length res)
        in
        if check then false else (Hashtbl.add cell k v; true)
    in match cells.(index i) with
    | Empty -> let cell = Hashtbl.create 10 in
               let () = Hashtbl.add cell k v in
               cells.(index i) <- Filled cell;
               true
    | Filled cell -> update_with_check cell

let get {cells; index} i = cells.(index i)

let iter_at {cells; index} i f =
    match cells.(index i) with
    | Empty -> ()
    | Filled cell -> Hashtbl.iter f cell

let iter_along g {cells; index; length} i f =
    let rec iter j = if j >= length then () else
        match cells.(index (g i j)) with
        | Empty -> iter (j+1)
        | Filled cell -> Hashtbl.iter f cell; iter (j+1)
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
let fold_along_row {cells; index; length} i f init =
    let rec iter j res =
        if j >= (length - i) then res
        else match cells.(index (i, j)) with
        | Empty -> iter (j+1) res
        | Filled cell -> iter (j+1) (Hashtbl.fold f cell res)
    in iter 0 init

(*        traverse like this         *)
(*           * * * o * o             *)
(*           * * o * o *             *)
(*           * o * o * *             *)
(*           o * o * * *             *)
(*           * o * * * *             *)
(*           o * * * * *             *)
let fold_along_diag {cells; index; length} i f init =
    let rec iter (i, j) res = 
        if j >= length || i < 0 then res
        else match cells.(index (i, j)) with
        | Empty -> iter (i-1, j+1) res
        | Filled cell -> iter (i-1, j+1) (Hashtbl.fold f cell res)
    in iter (i, 0) init

let complete_parses {cells; index; length} =
    let f _ elt l = elt :: l in
    match cells.(index (0, length - 1)) with
    | Empty       -> []
    | Filled cell -> Hashtbl.fold f cell []

let n_complete_parses {cells; index; length} =
    match cells.(index (0, length - 1)) with
    | Empty       -> 0
    | Filled cell -> Hashtbl.fold (fun _ _ n -> n + 1) cell 0
