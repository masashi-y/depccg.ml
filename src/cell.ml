
(* chart cell *)

module T = Tree
module Cat = Cat.EnglishCategories

type t = {id        : int;
          in_score  : float;
          out_score : float;
          score     : float;
          start     : int;
          length    : int;
          final     : bool;
          tree      : Tree.t}

let make ~id ~in_score ~out_score ~start ~length ~tree ~final =
            {id=id; in_score=in_score; out_score=out_score;
            score=in_score +. out_score; start=start;
            length=length; final=final; tree=tree}

(* Give higher priority to ones with higher scores *)
let compare {id=i1;score=s1} {id=i2;score=s2} =
    let c = compare s2 s1 in
    if c = 0 then compare i1 i2 else c


module Log = struct
    let do_log = false

    let blue : (_,_,_,_,_,_) format6 = "\027[34m%s\027[93m"
    let red  : (_,_,_,_,_,_) format6 = "\027[31m%s\027[93m"

    let bar () = 
        if do_log then prerr_endline
            ("\027[34m" ^ (String.make 10 '#') ^ "\027[93m\n")
        else ()

    let pop {score; in_score; out_score; id; tree} =
        if do_log then Printf.eprintf (blue ^^ "\nID: %i\n%s\ns: %e\nh: %e\ns+h: %e\n")
            "POPPED" id (T.show_derivation tree) in_score out_score score; bar ()

    let onebest = function
        | None -> ()
        | Some (_, {tree=T.{cat; str}}) -> if do_log then
            Printf.eprintf "%s\t-->\t%s\n" str (Cat.show cat)

    let unary {tree} = if do_log then
        Printf.eprintf (red ^^ "\n%s\n") "UNARY" (T.show_derivation tree); bar()

    let cand {tree} = if do_log then
        Printf.eprintf (red ^^ "\n%s\n") "CAND" (T.show_derivation tree); bar()

    let binary {tree} = if do_log then
        Printf.eprintf (red ^^ "\n%s\n") "BINARY" (T.show_derivation tree); bar()
end


