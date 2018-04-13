
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

type t = Attribute.t list

let from_protobuf s = s.S.attribs

let default length = (* TODO *)
    (* let length = List.length (Grammar.EnglishGrammar.Tree.terminals t) in *)
    CCList.init length (fun _ -> Attribute.default ())
