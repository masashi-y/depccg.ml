

(*
   Parse partially annotated sentence (e.g. span and its root category).
   Example: "<NP NP/NP|Michael NP|Jordan> <(S[dcl]\NP)/NP is> a professor at <NP University of California , Berkeley> .|."
     ---> [T ("NP/NP", 0); T ("NP", 1); N ("NP", 0, 2); T ("(S[dcl]\NP)/NP", 2) N ("NP", 6, 2); T (".", 8)]
*)

open Utils

type constraint_ = N of string option * int * int
                 | T of string * int

type constraints = constraint_ list

let rec string_to_chars = function
    | "" -> []
    | s -> String.get s 0 :: string_to_chars (String.sub s 1 (String.length s - 1))

let rec string_of_chars chars =
    let buf = Buffer.create 16 in
    List.iter (Buffer.add_char buf) chars;
    Buffer.contents buf

(*
let lexer str =
    let reduce stack res = match stack with
        | [] -> res
        | stack -> (string_of_chars @@ List.rev stack) :: res in
    let aux (state, stack, res) char =
        match char, state with
        | '(', `Cat  -> (`Cat, '(' :: stack, res)
        | ')', `Cat  -> (`Cat, ')' :: stack, res)
        | ' ', `Cat  -> (`Word, [], reduce stack res)
        | chr, `Cat  -> (`Cat, chr :: stack, res)
        | '(', `Word -> (`Cat, stack, "(" :: res)
        | ')', `Word -> (`None, [], ")" :: reduce stack res)
        | ' ', `Word -> (`Word, [], reduce stack res)
        | chr, `Word -> (`Word, chr :: stack, res)
        | '(', `None -> (`Cat, stack, "(" :: res)
        | ')', `None -> (`None, [], ")" :: reduce stack res)
        | ' ', `None -> (`None, [], reduce stack res)
        | chr, `None -> (`None, chr :: stack, res) in
    let state, stack, res = List.fold_left aux (`None, [], []) (string_to_chars str) in
    assert (state = `None);
    let res = List.rev (reduce stack res) in
    print_endline (String.concat "; " res);
    res
*)

let preprocess s =
    s |> Str.(global_replace (regexp "\\([<>]\\)") " \\1 ")
      |> Str.(split (regexp " +"))

let parse str =
    let error () = invalid_arg (!%"failed to parse %s\n" str) in
    let rec aux ptr stack = function
        | [] -> ([], [])
        | "<" :: cat :: rest ->
            let cat = match cat with
                | "X" -> None
                | _ -> Some cat in
            aux ptr ((cat, ptr) :: stack) rest
        | ">" :: rest -> begin match stack with
            | (cat, start) :: stack ->
                let constraints, words = aux ptr stack rest in
                (N (cat, start, ptr - start) :: constraints, words)
            | [] -> error ()
        end
        | word :: rest ->
            let constraints, words = aux (ptr + 1) stack rest in
            let word, constraints = match String.split_on_char '|' word with
            | [cat; word] -> (word, T (cat, ptr) :: constraints)
            | _ -> (word, constraints) in
            (constraints, word :: words)
    in
    aux 0 [] (preprocess str)

let show_one = function
    | N (Some cat, start, length) -> !%"%s(%d, %d)" cat start length
    | N (None, start, length) -> !%"X(%d, %d)" start length
    | T (cat, start) -> !%"%s(%d)" cat start

let show cs =
    String.concat ", " (List.map show_one cs)

let to_protobuf = function
    | N (category, start, length) ->
            Ccg_seed_types.Nonterminal {category; start; length}
    | T (category, start) ->
            Ccg_seed_types.Terminal {category; start}
(*
let () =
    let example = "<NP NP/NP|Michael NP|Jordan> <(S[dcl]\\NP)/NP is> a <NP professor> at <NP University of California , Berkeley> .|." in
    let res, words = parse example in
    print_endline (String.concat " " words);
    print_endline (show res);
    let example = "<S[dcl] <S[dcl] <NP NP/NP|Michael NP|Jordan> <S[dcl]\\NP (S[dcl]\\NP)/NP|is <NP <NP[nb] NP[nb]/N|a N|professor> <NP\\NP (NP\\NP)/NP|at <NP University of California , Berkeley>>>>> .|.>" in
    let res, words = parse example in
    print_endline (String.concat " " words);
    print_endline (show res);
    let example = "<NP <NP/NP Michael> <NP Jordan>> is a professor at <NP Berkeley university> <. .>" in
    let res, words = parse example in
    print_endline (String.concat " " words);
    print_endline (show res);
    let example = "<NP Michael Jordan> is a professor at <NP Berkeley university> <. .>" in
    let res, words = parse example in
    print_endline (String.concat ", " words);
    print_endline (show res);
    let example = "Michael Jordan is a professor at Berkeley university ." in
    let res, words = parse example in
    print_endline (String.concat " " words);
    print_endline (show res)
*)
