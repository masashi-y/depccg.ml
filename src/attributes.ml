
module Attribute =
struct
    module S = Ccg_seed_types
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

