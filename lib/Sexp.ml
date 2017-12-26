(**
 * Super simple sexp generation.
 *)

type t =
  | S of string
  | N of float
  | I of string
  | L of t list

type item =
  | Value of t
  | Comment of string

type doc = item list

let render doc =
  let buf = Buffer.create 1024 in
  let emit = Buffer.add_string buf in
  let rec emitAtom = function
  | S v -> emit "\""; emit v; emit "\"";
  | N v -> emit (string_of_float v)
  | I v -> emit v
  | L v ->
    let emitListElement item = emitAtom item; emit " " in
    emit "("; List.iter emitListElement v; emit ")"
  in
  let emitItem = function
  | Value a ->
    emitAtom a;
    emit "\n"
  | Comment comment ->
    emit "; "; emit comment;
    emit "\n"
  in
  List.iter emitItem doc;
  Buffer.contents buf
