
open Utils

module S = Ccg_seed_types

module Attribute =
struct
    type t = S.attribute

    let get name def = function
        | Some v -> v
        | None -> begin match def with
            | Some v -> v
            | None -> failwith (!%"fail in Attribute.%s without defaut value" name)
        end

    let lemma ?def t = get "lemma" def t.S.lemma
    let pos ?def t = get "pos" def t.S.pos
    let chunk ?def t = get "chunk" def t.S.chunk
    let entity ?def t = get "entity" def t.S.entity

    let default ?lemma ?pos ?chunk ?entity () =
        Ccg_seed_types.default_attribute ~lemma ~pos ~chunk ~entity ()
end

type t = Attribute.t list option

type attrs = t

let of_list s = Some s

let of_protobuf s = match s.S.attribs with
    | [] -> None
    | v -> Some v

let default () = (* TODO *)
    None
    (* let length = List.length (Grammar.EnglishGrammar.Tree.terminals t) in *)
    (* CCList.init length (fun _ -> Attribute.default ()) *)

module AttributeM =
struct
    include StateM

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
