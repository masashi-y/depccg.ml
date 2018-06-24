
open Utils

module S = Ccg_seed_types

module Attribute =
struct
    type t = S.attribute

    type label = [ `Lemma of string
                 | `Pos of string
                 | `Chunk of string
                 | `Entity of string ]

    let get name def = function
        | Some v -> v
        | None -> match def with
            | Some v -> v
            | None -> failwith (
                !%"fail in Attribute.%s without defaut value" name)

    let lemma ?def t = get "lemma" def t.S.lemma
    let pos ?def t = get "pos" def t.S.pos
    let chunk ?def t = get "chunk" def t.S.chunk
    let entity ?def t = get "entity" def t.S.entity

    let update attr = Ccg_seed_types.(function
        | `Lemma s -> {attr with lemma = Some s}
        | `Pos   s -> {attr with pos = Some s}
        | `Chunk s -> {attr with chunk = Some s}
        | `Entity s -> {attr with entity = Some s})

    let default ?lemma ?pos ?chunk ?entity () =
        Ccg_seed_types.default_attribute ~lemma ~pos ~chunk ~entity ()
end

type t = Attribute.t list option

type attrs = t

let of_list s = Some s

let to_list = function
    | None -> []
    | Some v -> v

let rev = function
    | None -> None
    | Some v -> Some (List.rev v)

let of_protobuf s = match s.S.attribs with
    | [] -> None
    | v -> Some v

let default () = (* TODO *)
    None
    (* let length = List.length (Grammar.EnglishGrammar.Tree.terminals t) in *)
    (* CCList.init length (fun _ -> Attribute.default ()) *)

let is_empty = function
    | None -> true
    | Some _ -> false

module AttributeM =
struct
    include StateM

    let push attr = get >>= function
        | None -> put (Some [attr])
        | Some ls -> put (Some (attr :: ls))

    let pop () = get >>= function
        | None -> return (Attribute.default ())
        | Some [] -> invalid_arg "AttributeM.pop"
        | Some (x :: xs) -> 
            put (Some xs) >>= fun () ->
            return x

    let popi () = get >>= function
        | i, None -> 
            put (i+1, None) >>= fun () ->
            return (i, Attribute.default ())
        | _, Some [] -> invalid_arg "AttributeM.popi"
        | i, Some (x :: xs) ->
            put (i+1, Some xs) >>= fun () ->
            return (i, x)
end
