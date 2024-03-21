open Common
open Angstrom

module Result_let = Monad.Binary.Make_let (struct
  include Result

  let return = ok
end)

let ( >|= ) = ( >>| )

let ( >>@= ) (lhs : ('a, 'b) result t) (rhs : 'a -> ('c, 'b) result t) =
  lhs >>= function Ok v -> rhs v | Error _ as e -> return e

let ( >|@= ) (lhs : ('a, 'b) result t) (rhs : 'a -> ('c, 'b) result) =
  lhs >|= function Ok v -> rhs v | Error _ as e -> e

let ( >>@ ) (lhs : ('a, 'b) result t) (rhs : 'a -> 'c t) =
  lhs >>= function Ok v -> rhs v >|= Result.ok | Error _ as e -> return e

let ( >|@ ) (lhs : ('a, 'b) result t) (rhs : 'a -> 'c) =
  lhs >|= function Ok v -> Ok (rhs v) | Error _ as e -> e

let marker structure_lut =
  any_char >>= fun byte ->
  match byte with
  | '\xC0' -> return (Ok (Marker.T { byte; typ = Null }))
  | '\xC2' | '\xC3' -> return (Ok (Marker.T { byte; typ = Boolean }))
  | '\xF0' .. '\xFF' | '\x00' .. '\x7F' | '\xC8' ->
      return (Ok (Marker.T { byte; typ = Int_8 }))
  | '\xC9' -> return (Ok (Marker.T { byte; typ = Int_16 }))
  | '\xCA' -> return (Ok (Marker.T { byte; typ = Int_32 }))
  | '\xCB' -> return (Ok (Marker.T { byte; typ = Int_64 }))
  | '\xC1' -> return (Ok (Marker.T { byte; typ = Float }))
  | '\xCC' .. '\xCE' -> return (Ok (Marker.T { byte; typ = Bytes }))
  | '\x80' .. '\x8F' | '\xD0' .. '\xD2' ->
      return (Ok (Marker.T { byte; typ = String }))
  | '\x90' .. '\x9F' | '\xD4' .. '\xD6' ->
      return (Ok (Marker.T { byte; typ = List }))
  | '\xA0' .. '\xAF' | '\xD8' .. '\xDA' ->
      return (Ok (Marker.T { byte; typ = Dictionary }))
  | '\xB0' .. '\xBF' ->
      any_char >|= fun tag_byte ->
      let open Result_let in
      let+ (E s) =
        Structure.Lut.find tag_byte structure_lut
        |> Option.to_result ~none:(`Unknown_structure_tag tag_byte)
      in
      Marker.T { byte; typ = Structure s }
  | _ -> return (Error (`Unknown_marker_byte byte))

let sized_int :
    type a. int -> (module Unsigned.S with type t = a) -> a Angstrom.t =
 fun sz (module S_int) ->
  let rec helper i acc =
    if i = 0 then return acc
    else
      any_char >>= fun c ->
      let open S_int in
      let v = of_int (Char.code c) in
      let acc = Infix.((acc lsl 8) lor v) in
      helper (i - 1) acc
  in
  helper sz S_int.zero

let int_8 = function
  | ('\xF0' .. '\xFF' | '\x00' .. '\x7F') as b ->
      return (Ok (Value.Int_8.int_8_of_char b))
  | '\xC8' -> sized_int 1 (module Signed.Int8) >|= Result.ok
  | b -> return (Error (`Non_int_8_marker_byte b))

let int_16 = function
  | '\xC9' -> sized_int 2 (module Signed.Int16) >|= Result.ok
  | b -> return (Error (`Non_int_16_marker_byte b))

let int_32 = function
  | '\xCA' -> sized_int 4 (module Signed.Int32) >|= Result.ok
  | b -> return (Error (`Non_int_32_marker_byte b))

let int_64 = function
  | '\xCB' -> sized_int 8 (module Signed.Int64) >|= Result.ok
  | b -> return (Error (`Non_int_64_marker_byte b))

let float = function
  | '\xC1' ->
      sized_int 8 (module Signed.Int64) >|= Int64.float_of_bits >|= Result.ok
  | b -> return (Error (`Non_float_marker_byte b))

let int_uint_8 = sized_int 1 (module Unsigned.UInt8) >|= Unsigned.UInt8.to_int

let int_uint_16 =
  sized_int 2 (module Unsigned.UInt16) >|= Unsigned.UInt16.to_int

let int_uint_32 =
  sized_int 4 (module Unsigned.UInt32) >|= Unsigned.UInt32.to_int

let bytes c =
  (match c with
  | '\xCC' -> int_uint_8 >|= Result.ok
  | '\xCD' -> int_uint_16 >|= Result.ok
  | '\xCE' -> int_uint_32 >|= Result.ok
  | b -> return (Error (`Non_bytes_marker_byte b)))
  >>@ take

let string c =
  (match c with
  | '\x80' .. '\x8F' as b -> return (Ok (Char.code b - 0x80))
  | '\xD0' ->
      sized_int 1 (module Unsigned.UInt8)
      >|= Unsigned.UInt8.to_int >|= Result.ok
  | '\xD1' ->
      sized_int 2 (module Unsigned.UInt16)
      >|= Unsigned.UInt16.to_int >|= Result.ok
  | '\xD2' ->
      sized_int 4 (module Unsigned.UInt32)
      >|= Unsigned.UInt32.to_int >|= Result.ok
  | b -> return (Error (`Non_string_marker_byte b)))
  >>@ take

let rec value ?(structure_lut = Structure.Lut.empty) () =
  marker structure_lut >>@= fun (T { typ; byte }) ->
  let helper (type a) (typ : a Type.t) : (a, _) result t =
    match typ with
    | Null -> return (Ok ())
    | Boolean -> return (Value.Boolean.of_marker_byte byte)
    | Int_8 -> int_8 byte
    | Int_16 -> int_16 byte
    | Int_32 -> int_32 byte
    | Int_64 -> int_64 byte
    | Float -> float byte
    | String -> string byte
    | Bytes -> bytes byte
    | Dictionary -> dictionary structure_lut byte
    | List -> list structure_lut byte
    | Structure s -> structure structure_lut s byte
  in
  helper typ >|@ Value.make typ

and dictionary : Structure.Lut.t -> char -> (Value.Dictionary.t, _) result t =
 fun structure_lut c ->
  let rec helper i acc =
    if i = 0 then return (Ok acc)
    else
      any_char >>= string >>@= fun key ->
      value ~structure_lut () >>@= fun v ->
      let acc = String_map.add key v acc in
      helper (i - 1) acc
  in
  (match c with
  | '\xA0' .. '\xAF' as b -> return (Ok (Char.code b - 0xA0))
  | '\xD8' -> int_uint_8 >|= Result.ok
  | '\xD9' -> int_uint_16 >|= Result.ok
  | '\xDA' -> int_uint_32 >|= Result.ok
  | b -> return (Error (`Non_dictionary_marker_byte b)))
  >>@= fun len -> helper len String_map.empty

and list : Structure.Lut.t -> char -> (Value.List.t, _) result t =
 fun structure_lut c ->
  (match c with
  | '\x90' .. '\x9F' as b -> return (Ok (Char.code b - 0x90))
  | '\xD4' -> int_uint_8 >|= Result.ok
  | '\xD5' -> int_uint_16 >|= Result.ok
  | '\xD6' -> int_uint_32 >|= Result.ok
  | b -> return (Error (`Non_list_marker_byte b)))
  >>@= fun len -> n_fields structure_lut len

and structure :
    type a. Structure.Lut.t -> a Structure.t -> char -> (a, _) result t =
 fun structure_lut structure c ->
  (match c with
  | '\xB0' .. '\xBF' as c -> return (Ok (Char.code c - 0xB0))
  | b -> return (Error (`Non_structure_marker_byte b)))
  >>@= fun len ->
  let structure_len = Structure.size structure in
  (if Int.equal structure_len len then n_fields structure_lut len
   else return (Error (`Not_enough_fields len)))
  >|@= fun l -> Structure.of_list structure l

and n_fields structure_lut n : (Value.List.t, _) result t =
  let rec helper i acc =
    if i = 0 then return (Ok acc)
    else value ~structure_lut () >>@= fun v -> helper (i - 1) (v :: acc)
  in
  helper n [] >|@ List.rev
