
open Utils

module S = Ccg_seed_types

module Attribute =
struct
    type t = S.attribute

    let lemma t = t.S.lemma
    let pos t = t.S.pos
    let chunk t = t.S.chunk
    let entity t = t.S.entity

    let default
        ?(lemma = None)
        ?(pos = None)
        ?(chunk = None)
        ?(entity = None)
        () = Ccg_seed_types.default_attribute ~lemma ~pos ~chunk ~entity ()
end

type t = Attribute.t list

let from_protobuf s = s.S.attribs

let default t = (* TODO *)
    let length = List.length (Grammar.EnglishGrammar.Tree.terminals t) in
    list_init length ~f:(fun _ -> Attribute.default ())
