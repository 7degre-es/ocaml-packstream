module type IN = sig
  include Stdlib.Map.OrderedType

  val pp : t Fmt.t
end

module type S = sig
  include Stdlib.Map.S

  val pp : 'a Fmt.t -> Format.formatter -> 'a t -> unit
end

module Make (In : IN) : S with type key = In.t = struct
  include Stdlib.Map.Make (In)

  let pp pp_v fmt t =
    Fmt.(
      braces
        (seq ~sep:comma (fun fmt (key, v) ->
             pair ~sep:(any " => ") In.pp pp_v fmt (key, v))))
      fmt (to_seq t)
end
