include Map.Make (struct
  type t = string

  let compare = String.compare
  let pp = Fmt.string
end)
