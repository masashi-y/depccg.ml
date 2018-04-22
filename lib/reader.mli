
open Utils
open Cat

type file = string

module type LOADER =
sig
    type cat

    val read_ccgseeds : string -> Ccg_seed_types.ccgseeds

    val read_ccgseeds_bytes : Bytes.t -> Ccg_seed_types.ccgseeds

    val load_ccgseeds : file -> Ccg_seed_types.ccgseeds

    val load_ccgseeds_socket : file -> file -> Ccg_seed_types.ccgseeds

    val read_cats : file -> cat list

    val read_unary_rules : file -> (cat, cat) Hashtbl.t

    val read_cat_dict : string list -> file -> (string, bool array) Hashtbl.t

    val read_binary_rules : file -> (cat * cat, bool) Hashtbl.t
end

module EnglishLoader : LOADER with type cat = EnglishCategories.t

module JapaneseLoader : LOADER with type cat = JapaneseCategories.t

type matrix = float Matrix.t

val read_proto_matrix : int -> Ccg_seed_types.ccgseed -> string list * matrix * matrix


module CCGBank :
sig
    open Grammar

    open EnglishGrammar

    val parse_line : file -> Attributes.t * Tree.t

    (* optionally output names for parses from e.g. ID=1 *)
    val parse_file : file -> string option list * Attributes.t list * Tree.t list
end

