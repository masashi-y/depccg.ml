
open Utils

module Attribute =
struct
    type t = Ccg_seed_types.attribute

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

    let lemma ?def t = get "lemma" def Ccg_seed_types.(t.lemma)
    let pos ?def t = get "pos" def Ccg_seed_types.(t.pos)
    let chunk ?def t = get "chunk" def Ccg_seed_types.(t.chunk)
    let entity ?def t = get "entity" def Ccg_seed_types.(t.entity)

    let lemma_opt Ccg_seed_types.{ lemma } = lemma
    let pos_opt Ccg_seed_types.{ pos } = pos
    let chunk_opt Ccg_seed_types.{ chunk } = chunk
    let entity_opt Ccg_seed_types.{ entity } = entity

    let update attr = Ccg_seed_types.(function
        | `Lemma s -> {attr with lemma = Some s}
        | `Pos   s -> {attr with pos = Some s}
        | `Chunk s -> {attr with chunk = Some s}
        | `Entity s -> {attr with entity = Some s})

    let default ?lemma ?pos ?chunk ?entity () =
        Ccg_seed_types.default_attribute ~lemma ~pos ~chunk ~entity ()
end

module NodeAttribute =
struct
    type t = {
        polarity: string option;
    }

    type label = [ `Polarity of string ]

    let polarity ?def { polarity } = match polarity with
        | Some v -> v
        | None -> match def with
            | Some v -> v
            | None -> failwith (
                !%"fail in NodeAttribute.polarity without defaut value")

    let polarity_opt { polarity } = polarity

    let default ?polarity () = { polarity }

    let update attr = function
        | `Polarity s -> {polarity = Some s}
end

type t = {
    leaves: Attribute.t list option;
    nodes: NodeAttribute.t list option;
}

type attrs = t

let of_lists ?leaves ?nodes () = {
    leaves; nodes
}

let to_lists {leaves; nodes} =
    let aux = function
        | Some v -> v
        | None -> [] in
    (aux leaves, aux nodes)

let rev { leaves; nodes } =
    let aux = function
        | None -> None
        | Some v -> Some (List.rev v) in
    { leaves = aux leaves; nodes = aux nodes }


let of_protobuf s =
    let leaves =
        match Ccg_seed_types.(s.attribs) with
        | [] -> None
        | v -> Some v in
    {leaves; nodes = None}

let default () = { leaves = None; nodes = None }

let is_empty = function
    | { leaves = None; nodes = None } -> true
    | _ -> false

let update ?leaves ?nodes attrs =
    let aux def = function
        | Some v -> Some v
        | None -> def in {
    leaves = aux attrs.leaves leaves;
    nodes = aux attrs.nodes nodes;
}


module AttributeM =
struct
    include StateM

    let push attr = get >>= fun attrs ->
        let leaves = match attrs.leaves with
            | None    -> [attr]
            | Some ls -> attr :: ls in
        put (update ~leaves attrs)

    let pushn attr = get >>= fun attrs ->
        let nodes = match attrs.nodes with
            | None    -> [attr]
            | Some ls -> attr :: ls in
        put (update ~nodes attrs)

    let pop () = get >>= fun attrs ->
        match attrs.leaves with
        | Some [] -> invalid_arg "AttributeM.pop"
        | None -> return (Attribute.default ())
        | Some (x :: leaves) -> 
            put (update ~leaves attrs) >>= fun () ->
            return x

    let popn () = get >>= fun attrs ->
        match attrs.nodes with
        | Some [] -> invalid_arg "AttributeM.popn"
        | None -> return (NodeAttribute.default ())
        | Some (x :: nodes) -> 
            put (update ~nodes attrs) >>= fun () ->
            return x

    let popi ?(incr=true) () =
        get >>= fun (i, attrs) ->
        let res, state = match attrs.leaves with
            | Some [] -> invalid_arg "AttributeM.popi"
            | None -> (Attribute.default (), attrs)
            | Some (x :: xs) -> (x, update ~leaves:xs attrs) in
        let j = if incr then i + 1 else i in
        put (j, state) >>= fun () ->
        return (i, res)

    let popni ?(incr=true) () =
        get >>= fun (i, attrs) ->
        let res, state = match attrs.nodes with
            | Some [] -> invalid_arg "AttributeM.popni"
            | None -> (NodeAttribute.default (), attrs)
            | Some (x :: xs) -> (x, update ~nodes:xs attrs) in
        let j = if incr then i + 1 else i in
        put (j, state) >>= fun () ->
        return (i, res)
end
