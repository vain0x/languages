open Stdio
module L = Location
module ST = Syntax_token

let () =
  let rec loop () =
    printf "> ";
    flush stdout;

    match In_channel.input_line stdin with
    | None -> exit 0
    | Some line ->
        let line = line ^ "\r\n  // newline\n// a  \n" in
        ST.tokenize line |> List.iter (fun t -> printf "%s\n" (ST.dump t));
        flush stdout;
        loop ()
  in
  loop ()
