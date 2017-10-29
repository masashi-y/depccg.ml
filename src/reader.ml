
open Utils
open Cat

module type CATEGORIES =
sig
    type t
    val parse : string -> t
end

module Loader (Cat : CATEGORIES) =
struct
    let read_ccgseeds file =
        let bytes = 
            let ic = open_in file in 
            let len = in_channel_length ic in 
            let bytes = Bytes.create len in 
            really_input ic bytes 0 len; 
            close_in ic; 
            bytes 
        in Ccg_seed_pb.decode_ccgseeds (Pbrt.Decoder.of_bytes bytes)

    let read_cats file =
        let scan l = Scanf.sscanf l "%s %i" (fun s _ -> (Cat.parse s))
        in List.map scan (read_lines file)

    let not_comment s = if String.length s = 0 then false
        else match s.[0] with
        | '#' -> false
        | _ -> true

    let read_unary_rules file =
        let res = Hashtbl.create 20 in
        let add_entry k v = Hashtbl.add res (Cat.parse k) (Cat.parse v) in
        let parse l = Scanf.sscanf l "%s %s" add_entry in
        read_lines file |> List.filter not_comment |> List.iter parse;
        res

    let read_cat_dict cat_list file =
        let res = Hashtbl.create 7000 in
        let scan l = match String.split_on_char '\t' l with
        | w :: cs -> let v = List.map (fun c -> List.mem c cs) cat_list in
                     Hashtbl.add res w (Array.of_list v)
        | _ -> invalid_arg "read_cat_dict" in
        read_lines file |> List.filter not_comment |> List.iter scan;
        res

    let read_binary_rules file =
        let res = Hashtbl.create 2000 in
        let add_entry k v = Hashtbl.add res (Cat.parse k, Cat.parse v) true in
        let scan l = Scanf.sscanf l "%s %s" add_entry in
        read_lines file |> List.filter not_comment |> List.iter scan;
        res
end

module EnglishLoader =
struct
    module EC = EnglishCategories
    include Loader (EC)

    let read_binary_rules file =
        let res = Hashtbl.create 2000 in
        let parse' c = EC.remove_some_feat [`Var; `Nb] (EC.parse c) in
        let add_entry k v = Hashtbl.add res (parse' k, parse' v) true in
        let scan l = Scanf.sscanf l "%s %s" add_entry in
        read_lines file |> List.filter not_comment |> List.iter scan;
        res
end


let read_proto_matrix n_cats = 
    let open Ccg_seed_types in
    let error () = failwith "error in read_proto_matrix" in
    let convert = function
        | {values=v; shape=[i; j]} -> Matrix.reshape (Matrix.of_list v) (i, j)
        | _ -> error ()
    in function
    | {sentence=s;
       cat_probs=Some c;
       dep_probs=Some p} -> (s, convert c, convert p)
    | _ -> error ()

