
open Grammar
open Utils

module type PRINTER =
   functor (Grammar : GRAMMAR) ->
sig
    open Grammar
    val show_tree : Attributes.t -> Tree.t -> string

    val show_conll_like : Attributes.t -> Tree.t -> string

    val show_ptb : int -> Tree.t -> string

    val show_derivation : Tree.t -> string

    val show_html_trees : Tree.scored list -> string

    val show_html_trees_separated : string option list -> Tree.scored list -> unit
    val output_results : string -> string option list -> Tree.scored list -> unit
end

module ParsePrinter : PRINTER

module EnglishPrinter : sig
    open EnglishGrammar

    module Prolog : sig
        val show : int -> Attributes.t -> Tree.t -> string
    end

    module XML : sig
        val show : Attributes.t -> Tree.t -> string
    end

    val show_tree : Attributes.t -> Tree.t -> string

    val show_conll_like : Attributes.t -> Tree.t -> string

    val show_ptb : int -> Tree.t -> string

    val show_derivation : Tree.t -> string

    val show_html_trees : Tree.scored list -> string

    val show_html_trees_separated : string option list -> Tree.scored list -> unit
    val show_prolog : Attributes.t list -> Tree.scored list -> unit

    val show_xml_trees : Attributes.t list -> Tree.scored list -> string

    val output_results : string -> string option list -> Attributes.t list -> Tree.scored list -> unit

end
