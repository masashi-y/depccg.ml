
let (!%) = Printf.sprintf

let read_lines file =
    let ch = open_in file in
    let rec parse () =
        let one = try Some (input_line ch)
                with End_of_file -> close_in_noerr ch; None
        in match one with
            | Some s -> s :: parse ()
            | None -> []
    in parse ()

