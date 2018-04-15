
open Sexplib
open Std
open Utils

type config = {
    lib : string;
    share : string;
    model : string;
} [@@deriving sexp]

let home =
    try
        Sys.getenv "HOME"
    with Not_found ->
        try
            (Unix.getpwuid (Unix.getuid ())).Unix.pw_dir
        with Unix.Unix_error _ | Not_found ->
            if Sys.win32 then
            try
                Sys.getenv "AppData"
            with Not_found ->
                  ""
            else
                  ""

type t = [`en of config | `ja of config] [@@deriving sexp]

let rec lookup lang = function
    | [] -> prerr_endline "Failed to load .depccgrc. Abort."; exit 2
    | `Result (`en v) :: _ when lang = "en" -> v
    | `Result (`ja v) :: _ when lang = "ja" -> v
    | _ :: vs -> lookup lang vs

let load () =
    try Sexp.load_sexps_conv (home </> ".depccgrc") t_of_sexp
    with _ -> prerr_endline "Failed to load .depccgrc. Abort."; exit 2

let load_en () = lookup "en" (load ())
let load_ja () = lookup "ja" (load ())
