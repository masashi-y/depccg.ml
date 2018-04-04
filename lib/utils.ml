open Unix

let (</>) = Filename.concat

let p = Printf.printf

let pr = print_endline

let (!%) = Printf.sprintf

let list_init n ~f =
    if n < 0 then raise (Invalid_argument "list_init");
    let rec aux i accum =
        if i = 0 then accum
        else aux (i-1) (f (i-1) :: accum) in
    aux n []

let walk_directory_tree dir pattern =
    let re = Str.regexp pattern in
    let select str = Str.string_match re str 0 in
    let rec walk acc = function
    | [] -> acc
    | dir::tail ->
        let contents = Array.to_list (Sys.readdir dir) in
        let contents = List.rev_map (Filename.concat dir) contents in
        let dirs, files =
            List.fold_left (fun (dirs,files) f ->
                match (stat f).st_kind with
                | S_REG -> (dirs, f::files)
                | S_DIR -> (f::dirs, files)
                | _ -> (dirs, files)
            ) ([],[]) contents
        in
        let matched = List.filter (select) files in
        walk (matched @ acc) (dirs @ tail)
    in
    walk [] [dir]

let read_lines file =
    let ch = open_in file in
    let rec parse () =
        let one = try Some (input_line ch)
                with End_of_file -> close_in_noerr ch; None
        in match one with
            | Some s -> s :: parse ()
            | None -> []
    in parse ()

let string_null s = String.length s = 0

let rec flatten = function
    [] -> []
    | head :: tail -> head @ flatten tail

let (--) a b =
    let rec iter store a bi =
        if a = bi then bi::store
        else iter (bi::store) a (bi - 1)
    in
    if a <= b then iter [] a b
    else List.rev @@ iter [] b a

let enumerate l = List.mapi (fun i elt -> (i, elt)) l

let escape_html_simple s =
    let open Buffer in
    let buf = create 10 in
    let f = function
        | '&' -> add_string buf "&amp;"
        | '<' -> add_string buf "&lt;"
        | '>' -> add_string buf "&gt;"
        | ' ' -> add_string buf "&nbsp;"
        | c -> add_char buf c
    in String.iter f s;
    contents buf

let current_time_string () =
    let Unix.{tm_sec; tm_min; tm_hour; tm_mday;
         tm_mon; tm_year } = Unix.gmtime (Unix.time ()) in
    !% "%d:%d:%d:%d:%d:%d" tm_year (tm_mon + 1) tm_mday tm_hour tm_min tm_sec

let write_file filename str =
    let oc = open_out filename in
    output_string oc str;
    close_out oc

module Matrix =
struct
    type shape = int * int
    type 'a t = 'a array * shape

    let make (i, j) n = (Array.make (i*j) n, (i, j))

    (* let zeros s = make s 0.0 *)

    let of_list l = (Array.of_list l, (List.length l, 1))

    let length (_, (i, j)) = i * j

    let reshape (m, (i, j)) (i',j') =
        if i * j <> i' * j'
        then failwith (!%"reshape error: (%i, %i) to (%i, %i)" i j i' j')
        else (m, (i', j'))

    let get (m, (_, i)) (x, y) = m.(i * x + y)

    let set (m, (_, i)) (x, y) v = m.(i * x + y) <- v

    let init (i, j) f =
        let res = make (i, j) (f 0 0) in
        for i' = 0 to pred i do
            for j' = 0 to pred j do
                set res (i', j') (f i' j')
            done
        done;
        res

    let max_along_row (m, (_, i) as mat) x =
        let res = ref (get mat (x, 0)) in
        for idx = 1 to pred i do
            let v = get mat (x, idx) in
            match compare !res v with
            1 -> () | _ -> res := v
        done;
        !res
end


module type TypedMonadType =
sig
    type 'a t
    val empty : 'a t
    val singleton : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
end


module TypedMonad (S:TypedMonadType) =
struct
    open S
    let (>>=) = bind
    let (=<<) f m = m >>= f
    let fail = empty
    let return = singleton
    let rec sequence x = function
        | [] -> x
        | f::t -> sequence (x >>= f) t
    let sequence_ m l = ignore (sequence m l)
end

module Option =
struct
    module Core =
    struct
        type 'a t = 'a option
        let compare = compare
        let empty = None
        let singleton x = Some x
        let add x _ = Some x
        let bind x f = match x with
            | None -> None
            | Some x -> f x
        let to_list = function
            | None -> []
            | Some x -> [x]
        let fold f x e = match x with
            | None -> e
            | Some x -> f x e
    end

    include Core
    include TypedMonad(Core)

    let get default = function
        | Some x -> x
        | None -> default
end

module StateM : sig
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
    val pop : unit -> ('a, 'a list) t
    val mapM : ('a -> ('b, 'c) t) -> 'a list -> ('b list, 'c) t
end = struct
    type ('a, 'b) t = State of ('b -> 'a * 'b)

    let value (State s) = s
    let bind m f = State (fun s -> let a, s' = (value m) s in ((value (f a)) s'))
    let (>>=) = bind
    let return x = State (fun s -> (x, s))

    let get = State (fun s -> (s, s))
    let put s = State (fun _ -> ((), s))

    let (&&&) f1 f2 (t1, t2) = get >>= fun s ->
        f1 t1 >>= fun r1 ->
        put s >>= fun () ->
        f2 t2 >>= fun r2 ->
        put s >>= fun () ->
        return (r1, r2)

    let rec sequence' = function
        | [] -> return []
        | hd :: rest -> 
            get >>= fun s ->
            hd >>= fun hd ->
            put s >>= fun () ->
            sequence' rest >>= fun rest ->
            put s >>= fun () ->
            return (hd :: rest)

    let run (State m) a = m a
    let eval m a = fst (run m a)
    let exec m a = snd (run m a)

    let pop () =
        get >>= fun s ->
        match s with
            | [] -> failwith "pop failed in Attributes.StateM.pop"
            | x :: xs -> 
                put xs >>= fun () ->
                return x

    let rec mapM f = function
        | [] -> return []
        | x :: xs ->
            f x >>= fun x ->
            mapM f xs >>= fun xs ->
            return (x :: xs)
end
