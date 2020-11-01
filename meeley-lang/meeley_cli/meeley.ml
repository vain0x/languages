open Stdio

let () =
  let s = In_channel.input_all stdin in
  printf "len = %d\n" (String.length s)
