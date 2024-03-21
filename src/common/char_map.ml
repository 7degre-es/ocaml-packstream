include Map.Make (struct
  type t = char

  let compare = Char.compare
  let pp = Fmt.char
end)
