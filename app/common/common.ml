
open Depccg
open Reader

type input_format = Raw | Partial

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

let read_input_and_tag ~verbose ~input_format ~tagger (inputs : string list) =
    let open Ccg_seed_types in
    match input_format with (* this part will be extended to support more input types *)
    | Raw -> tagger inputs
    | Partial ->
        let constraints, sentences = List.split @@ List.map PartialParse.parse inputs in
        if verbose then begin
            Printf.eprintf "[parser] CONSTRAINED PARSING: reading partially annotated trees\n%!";
            Printf.eprintf "[parser] SENTENCE_ID: CATEGORY(START, LENGTH)\n%!";
            List.iteri (fun i constraint_ ->
                Printf.eprintf "[parser] %d: %s\n%!" i (PartialParse.show constraint_)) constraints
        end;
        let tagged = tagger @@ List.map (String.concat " ") sentences in
        let aux seed constraints =
            match constraints, seed.constraints with
            | [], _ -> seed
            | cs, [] ->
                let constraints = List.map
                    (fun (category, start, length) -> {category; start; length}) cs in
                {seed with constraints}
            | _ -> invalid_arg "conflict occured: both input text and seed contains partial annotation" in
    {tagged with seeds = List.map2 aux tagged.seeds constraints}

let load_seeds ~verbose ~input_format ~tagger ~loader ~socket input =
    let module Loader = (val loader : LOADER) in
    match input with
    | None -> (let input = Utils.read_stdin () in
              try let res = Loader.read_ccgseeds (String.concat "\n" input) in
                  Printf.eprintf "[parser] read ccg_seeds protobuf from stdin\n%!";
                  res
              with _ -> begin
                  Printf.eprintf "[parser] running tagger on the text from stdin\n%!";
                  read_input_and_tag ~verbose ~input_format ~tagger input end)
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
            read_input_and_tag ~verbose ~input_format ~tagger (Utils.read_lines i))
        end
    end
