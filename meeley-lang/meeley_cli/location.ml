(* -------------------------------------------- *)
(* Pos *)
(* -------------------------------------------- *)

(* Position. *)
type pos = { pos_index : int; pos_row : int; pos_column : int }

module Pos : sig
  val of_string : string -> pos

  val add : pos -> pos -> pos

  val to_string : pos -> string
end = struct
  let of_string s : pos =
    let rec go i y =
      try
        let i = String.index_from s i '\n' in
        go (i + 1) (y + 1)
      with Not_found ->
        { pos_index = i; pos_row = y; pos_column = String.length s - i }
    in
    go 0 0

  let add l r : pos =
    let i = l.pos_index + r.pos_index in
    if r.pos_row = 0 then
      {
        pos_index = i;
        pos_row = l.pos_row;
        pos_column = l.pos_column + r.pos_column;
      }
    else
      {
        pos_index = i;
        pos_row = l.pos_row + r.pos_row;
        pos_column = r.pos_column;
      }

  let to_string p =
    Int.to_string (p.pos_row + 1) ^ "." ^ Int.to_string (p.pos_column + 1)
end

(* -------------------------------------------- *)
(* Range *)
(* -------------------------------------------- *)

type range = pos * pos

module Range : sig
  val of_string : string -> int -> int -> range

  val to_string : range -> string
end = struct
  let of_string s l r =
    let start = Pos.of_string (String.sub s 0 l) in
    let len = Pos.of_string (String.sub s l (r - l)) in
    (start, Pos.add start len)

  let to_string s =
    let l, r = s in
    Pos.to_string l ^ ".." ^ Pos.to_string r
end

(* Document. A source file or an editor tab. *)
type doc = {
  doc_path : string option;
  doc_name : string;
  doc_text : string;
  doc_version : int;
}

(* Location. *)
type loc = doc * pos
