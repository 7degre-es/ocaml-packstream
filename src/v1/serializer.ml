open Common
open Faraday

let null t = write_char t '\xC0'

let boolean t v =
  let c = if v then '\xC3' else '\xC2' in
  write_char t c

let int_8 t v =
  let open Signed.Int8 in
  let c = Char.unsafe_chr (to_int v) in
  if compare v (of_int (-16)) >= 0 && compare v (of_int 127) <= 0 then
    write_char t c
  else write_char t '\xC8';
  write_char t c

let sized_int :
    type a. int -> (module Unsigned.S with type t = a) -> t -> a -> unit =
 fun sz (module S_int) t v ->
  let byte_mask = S_int.of_int 0xff in
  let rec helper i =
    if i >= 0 then (
      let shift_by = i lsl 3 in
      let c =
        S_int.Infix.((v lsr shift_by) land byte_mask)
        |> S_int.to_int |> Char.unsafe_chr
      in
      write_char t c;
      helper (i - 1))
  in
  helper (sz - 1)

let int_16 t v =
  write_char t '\xC9';
  sized_int 2 (module Signed.Int16) t v

let int_32 t v =
  write_char t '\xCA';
  sized_int 4 (module Signed.Int32) t v

let int_64 t v =
  write_char t '\xCB';
  sized_int 8 (module Signed.Int64) t v

let float t v =
  write_char t '\xC1';
  let b = Int64.bits_of_float v in
  sized_int 8 (module Signed.Int64) t b

let bytes t v =
  let l = String.length v in
  let () =
    if l <= 255 then (
      let c = Char.unsafe_chr l in
      write_char t '\xCC';
      write_char t c)
    else if l <= 65_535 then (
      write_char t '\xCD';
      sized_int 2 (module Signed.Int16) t (Signed.Int16.of_int l))
    else (
      write_char t '\xCE';
      sized_int 4 (module Signed.Int32) t (Signed.Int32.of_int l))
  in
  write_string t v

let string t v =
  let l = String.length v in
  let () =
    if l <= 15 then
      let c = Char.unsafe_chr (0x80 + l) in
      write_char t c
    else if l <= 255 then (
      let c = Char.unsafe_chr l in
      write_char t '\xD0';
      write_char t c)
    else if l <= 65_535 then (
      write_char t '\xD1';
      sized_int 2 (module Signed.Int16) t (Signed.Int16.of_int l))
    else (
      write_char t '\xD2';
      sized_int 4 (module Signed.Int32) t (Signed.Int32.of_int l))
  in
  write_string t v

let rec value ?(structure_lut = Structure.Lut.empty) t
    (Value.T { typ; value = v }) =
  match typ with
  | Null -> null t
  | Boolean -> boolean t v
  | Int_8 -> int_8 t v
  | Int_16 -> int_16 t v
  | Int_32 -> int_32 t v
  | Int_64 -> int_64 t v
  | Float -> float t v
  | Bytes -> bytes t v
  | String -> string t v
  | List -> list structure_lut t (v : Value.List.t)
  | Dictionary -> dictionary structure_lut t (v : Value.Dictionary.t)
  | Structure s -> structure structure_lut t s v

and list structure_lut t v =
  let rec helper l k = function
    | [] -> (l, k)
    | v :: rest ->
        helper (l + 1)
          (fun () ->
            k ();
            value ~structure_lut t v)
          rest
  in
  let l, f = helper 0 Fun.id v in
  let () =
    if l <= 15 then
      let c = Char.unsafe_chr (0x90 + l) in
      write_char t c
    else if l <= 255 then (
      let c = Char.unsafe_chr l in
      write_char t '\xD4';
      write_char t c)
    else if l <= 65_535 then (
      write_char t '\xD5';
      sized_int 2 (module Unsigned.UInt16) t (Unsigned.UInt16.of_int l))
    else (
      write_char t '\xD6';
      sized_int 4 (module Unsigned.UInt32) t (Unsigned.UInt32.of_int l))
  in
  f ()

and dictionary structure_lut t v =
  let l, f =
    String_map.fold
      (fun key v (l, k) ->
        ( l + 1,
          fun () ->
            k ();
            string t key;
            value ~structure_lut t v ))
      v (0, Fun.id)
  in
  let () =
    if l <= 15 then
      let c = Char.unsafe_chr (0xA0 + l) in
      write_char t c
    else if l <= 255 then (
      let c = Char.unsafe_chr l in
      write_char t '\xD8';
      write_char t c)
    else if l <= 65_535 then (
      write_char t '\xD9';
      sized_int 2 (module Unsigned.UInt16) t (Unsigned.UInt16.of_int l))
    else (
      write_char t '\xDA';
      sized_int 4 (module Unsigned.UInt32) t (Unsigned.UInt32.of_int l))
  in
  f ()

and structure : type a. Structure.Lut.t -> t -> a Structure.t -> a -> unit =
 fun structure_lut t s v ->
  let l = Structure.size s in
  let list = Structure.to_list s v in
  let c = Char.unsafe_chr (0xB0 + l) in
  write_char t c;
  List.iter (value ~structure_lut t) list
