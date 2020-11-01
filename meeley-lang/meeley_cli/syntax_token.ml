module L = Location

type token =
  | T_eof
  | T_bad
  | T_newline
  | T_blank
  | T_comment
  | T_number
  | T_string
  | T_ident
  | T_keyword
  | T_pun

type token_data = token * string * L.range

type lookahead =
  | L_eof
  | L_bad
  | L_newline
  | L_blank
  | L_comment
  | L_number
  | L_string
  | L_ident
  | L_pun

let keywords =
  [ "true"; "false"; "if"; "else"; "do"; "fn"; "type"; "let"; "rec" ]

let punctuations =
  [
    "(";
    ")";
    "[";
    "]";
    "{";
    "}";
    "<=";
    "<";
    ">=";
    ">";
    "==";
    "=";
    "!=";
    ".";
    ",";
    ":";
    ";";
    "+";
    "-";
    "*";
    "/";
    "%";
    "|";
    "||";
    "&";
    "&&";
  ]

let char_is_newline c = c = '\r' || c = '\n'

let char_is_blank c = c = ' ' || c = '\t'

let char_is_digit c = '0' <= c && c <= '9'

let char_is_ident c =
  ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') || char_is_digit c || c = '_'

let str_slice l r s =
  let n = String.length s in

  String.sub s (l |> max 0 |> min n) (r - l |> max 0 |> min n)

let str_contains_at i t s =
  let sn = String.length s in
  let tn = String.length t in
  i + tn <= sn && String.sub s i tn = t

let str_count_from i pred s =
  let n = String.length s in
  let rec go i = if i < n && pred s.[i] then go (i + 1) else i in

  go i - i

let look_pun s i =
  punctuations
  |> List.find_map (fun pun ->
         if s |> str_contains_at i pun then Some (L_pun, String.length pun)
         else None)

let lookahead (s : string) (i : int) : lookahead * int =
  let ( let* ) x f = match x with Some x -> x | None -> f () in

  let check kind pred = if pred s.[i] then Some (kind, 1) else None in

  let* () = if i >= String.length s then Some (L_eof, 0) else None in

  let* () = if str_contains_at i "//" s then Some (L_comment, 2) else None in

  let* () = check L_newline char_is_newline in
  let* () = check L_blank char_is_blank in
  let* () = check L_number char_is_digit in
  let* () = check L_ident char_is_ident in
  let* () = look_pun s i in
  (L_bad, 1)

let tokenize (source_code : string) : token_data list =
  let s = source_code in
  let rec go i =
    let do_take kind_fn len =
      let n = len in
      Stdio.printf "n=%d, l=%d, r=%d\n" i n (i + n);
      let r = i + n in
      let text = s |> str_slice i r in
      (kind_fn text, text, L.Range.of_string s i r) :: go r
    in
    let take_while kind len pred =
      let len = len + (s |> str_count_from (i + len) pred) in
      do_take (fun _ -> kind) len
    in
    let take_pun len = do_take (fun _ -> T_pun) len in
    let take_ident len =
      let len = len + (s |> str_count_from (i + len) char_is_ident) in
      let kind text =
        if keywords |> List.exists (( = ) text) then T_keyword else T_ident
      in
      do_take kind len
    in

    let kind, n = lookahead s i in
    match kind with
    | L_eof -> [ (T_eof, "", L.Range.of_string s i i) ]
    | L_newline -> take_while T_newline n char_is_newline
    | L_blank -> take_while T_blank n char_is_blank
    | L_comment -> take_while T_comment n (fun c -> c |> char_is_newline |> not)
    | L_number -> take_while T_number n char_is_ident
    | L_string -> failwith "unimplemented"
    | L_ident -> take_ident n
    | L_pun -> take_pun n
    | L_bad ->
        take_while T_bad n (fun c -> not (char_is_newline c || char_is_blank c))
  in

  go 0

let dump (t : token_data) =
  let kind, text, range = t in
  let escape s =
    s ^ " `" ^ String.escaped text ^ "` " ^ L.Range.to_string range
  in
  let raw s = s ^ " `" ^ text ^ "` " ^ L.Range.to_string range in
  match kind with
  | T_eof -> "EOF"
  | T_bad -> escape "BAD"
  | T_newline -> escape "NEWLINE"
  | T_blank -> raw "BLANK"
  | T_comment -> raw "COMMENT"
  | T_number -> raw "NUMBER"
  | T_string -> escape "STRING"
  | T_ident -> raw "IDENT"
  | T_keyword -> raw "KEYWORD"
  | T_pun -> raw "PUN"
