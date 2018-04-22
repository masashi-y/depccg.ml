
open Depccg
open Reader

let progress_map ncores ~f lst =
    let size = List.length lst in
    let f' i x = let y = f x in
        Printf.eprintf "\b\r[parser] %i/%i" (i+1) size;
        flush stderr;
        y in
    let res = Parmap.(parmapi ~ncores f' (L lst)) in
    Printf.eprintf "\b\r[parser] done         \n";
    res

let try_load name f =
    try let res = f () in
        (Some res, Hashtbl.length res)
    with Sys_error _ ->
        prerr_endline ("[parser] " ^ name ^ " not found. Using empty one");
        (None, 0)

let load_seeds ~tagger ~loader ~socket input =
    let module Loader = (val loader : LOADER) in
    match input with
    | None -> (let input = Utils.read_stdin () in
              try begin let res = Loader.read_ccgseeds (CCString.unlines input) in
                  Printf.eprintf "[parser] read ccg_seeds protobuf from stdin%!";
                  res end
              with _ -> begin
                  Printf.eprintf "[parser] running tagger on the text from stdin%!";
                  tagger input end)
    | Some i -> begin match socket with
        | Some s ->
            Printf.eprintf "[parser] connecting socket %s\n%!" s;
            Loader.load_ccgseeds_socket s i
        | None -> begin
            if CCString.(suffix ~suf:".seed" i || suffix ~suf:".seeds" i) then
               (Printf.eprintf "[parser] reading seed file %s\n%!" i;
               Loader.load_ccgseeds i)
            else
            (Printf.eprintf "[parser] tagging inputs\n%!";
            tagger (Utils.read_lines i))
        end
    end
