type error = [ `Unknown_marker_byte of char ]
type t = T : { byte : char; typ : 'a Type.t } -> t

let of_char byte =
  match byte with
  | '\xF0' .. '\xFF' | '\x00' .. '\x7F' | '\xC8' -> Ok (T { byte; typ = Int_8 })
  | '\xC9' -> Ok (T { byte; typ = Int_16 })
  | '\xCA' -> Ok (T { byte; typ = Int_32 })
  | '\xCB' -> Ok (T { byte; typ = Int_64 })
  | '\xC1' -> Ok (T { byte; typ = Float })
  | '\xCC' .. '\xCE' -> Ok (T { byte; typ = Bytes })
  | '\x80' .. '\x8F' | '\xD0' .. '\xD2' -> Ok (T { byte; typ = String })
  | '\x90' .. '\x9F' | '\xD4' .. '\xD6' -> Ok (T { byte; typ = List })
  | '\xA0' .. '\xAF' | '\xD8' .. '\xDA' -> Ok (T { byte; typ = Dictionary })
  | _ -> Error (`Unknown_marker_byte byte)
