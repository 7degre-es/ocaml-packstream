open Common

module rec Type : sig
  type _ t =
    | Null : unit t
    | Boolean : bool t
    | Int_8 : Signed.Int8.t t
    | Int_16 : Signed.Int16.t t
    | Int_32 : Signed.Int32.t t
    | Int_64 : Signed.Int64.t t
    | Float : float t
    | Bytes : string t
    | String : string t
    | List : Value.List.t t
    | Dictionary : Value.Dictionary.t t
    | Structure : 'a Structure.t -> 'a t

  val pp : 'a t Fmt.t
  val pp_v : 'a t -> 'a Fmt.t
  val equal : 'a t -> 'b t -> bool
  val equal_t : 'a t -> 'b t -> ('a, 'b) Stdlib.Type.eq option
  val equal_v : 'a t -> 'a -> 'a -> bool
end = struct
  type _ t =
    | Null : unit t
    | Boolean : bool t
    | Int_8 : Signed.Int8.t t
    | Int_16 : Signed.Int16.t t
    | Int_32 : Signed.Int32.t t
    | Int_64 : Signed.Int64.t t
    | Float : float t
    | Bytes : string t
    | String : string t
    | List : Value.List.t t
    | Dictionary : Value.Dictionary.t t
    | Structure : 'a Structure.t -> 'a t

  let pp : type a. a t Fmt.t =
   fun fmt -> function
    | Null -> Fmt.pf fmt "Null"
    | Boolean -> Fmt.pf fmt "Boolean"
    | Int_8 -> Fmt.pf fmt "Int_8"
    | Int_16 -> Fmt.pf fmt "Int_16"
    | Int_32 -> Fmt.pf fmt "Int_32"
    | Int_64 -> Fmt.pf fmt "Int_64"
    | Float -> Fmt.pf fmt "Float"
    | Bytes -> Fmt.pf fmt "Bytes"
    | String -> Fmt.pf fmt "String"
    | List -> Fmt.pf fmt "List"
    | Dictionary -> Fmt.pf fmt "Dictionary"
    | Structure s -> Fmt.pf fmt "Structure(%c)" (Structure.tag s)

  let pp_v : type a. a t -> a Fmt.t =
   fun t fmt v ->
    match t with
    | Null -> Fmt.pf fmt "()"
    | Boolean -> Fmt.bool fmt v
    | Int_8 -> Signed.Int8.pp fmt v
    | Int_16 -> Signed.Int16.pp fmt v
    | Int_32 -> Signed.Int32.pp fmt v
    | Int_64 -> Signed.Int64.pp fmt v
    | Float -> Fmt.float fmt v
    | Bytes -> Fmt.Dump.string fmt v
    | String -> Fmt.Dump.string fmt v
    | List -> Fmt.Dump.list Value.pp fmt v
    | Dictionary -> String_map.pp Value.pp fmt v
    | Structure s -> Structure.pp_v s fmt v

  let equal_t : type a b. a t -> b t -> (a, b) Stdlib.Type.eq option =
   fun a b ->
    match (a, b) with
    | Null, Null -> Some Equal
    | Boolean, Boolean -> Some Equal
    | Int_8, Int_8 -> Some Equal
    | Int_16, Int_16 -> Some Equal
    | Int_32, Int_32 -> Some Equal
    | Int_64, Int_64 -> Some Equal
    | Float, Float -> Some Equal
    | Bytes, Bytes -> Some Equal
    | String, String -> Some Equal
    | List, List -> Some Equal
    | Dictionary, Dictionary -> Some Equal
    | Structure s0, Structure s1 -> Structure.equal_t s0 s1
    | _ -> None

  let equal_v : type a. a t -> a -> a -> bool = function
    | Null -> fun () () -> true
    | Boolean -> Bool.equal
    | Int_8 -> Signed.Int8.equal
    | Int_16 -> Signed.Int16.equal
    | Int_32 -> Signed.Int32.equal
    | Int_64 -> Signed.Int64.equal
    | Float -> Float.equal
    | Bytes -> String.equal
    | String -> String.equal
    | List -> List.equal Value.equal
    | Dictionary -> String_map.equal Value.equal
    | Structure s -> Structure.equal_v s

  let equal : type a b. a t -> b t -> bool =
   fun t0 t1 -> Option.is_some (equal_t t0 t1)
end

and Value : sig
  type t = T : { typ : 'a Type.t; value : 'a } -> t

  val make : 'a Type.t -> 'a -> t
  val pp : t Fmt.t
  val equal : t -> t -> bool

  module Boolean : sig
    type error = [ `Non_boolean_marker_byte of char ]

    val of_marker_byte : char -> (bool, [> error ]) result
  end

  module Int_8 : sig
    type error = [ `Non_int_8_marker_byte of char ]

    val int_8_of_char : char -> Signed.Int8.t
  end

  module Dictionary : sig
    type nonrec t = t String_map.t
  end

  module List : sig
    type nonrec t = t list
  end
end = struct
  type t = T : { typ : 'a Type.t; value : 'a } -> t

  let make : type a. a Type.t -> a -> t = fun typ value -> T { typ; value }

  let pp fmt (T { typ; value }) =
    (fun (type a) (typ : a Type.t) (value : a) ->
      Fmt.Dump.(
        record
          [
            field "typ" (fun () -> typ) Type.pp;
            field "value" (fun () -> value) (Type.pp_v typ);
          ])
        fmt ())
      typ value

  let equal (T { typ = t0; value = v0 }) (T { typ = t1; value = v1 }) =
    match Type.equal_t t0 t1 with
    | Some Equal -> Type.equal_v t0 v0 v1
    | None -> false

  module Boolean = struct
    type error = [ `Non_boolean_marker_byte of char ]

    let of_marker_byte = function
      | '\xC2' -> Ok false
      | '\xC3' -> Ok true
      | b -> Error (`Non_boolean_marker_byte b)
  end

  module Int_8 = struct
    type error = [ `Non_int_8_marker_byte of char ]

    let int_8_of_char c = Signed.Int8.of_int (Char.code c)
  end

  module Dictionary = struct
    type nonrec t = t String_map.t
  end

  module List = struct
    type nonrec t = t list
  end
end

and Structure : sig
  type error =
    [ `Incompatible_types
    | `Not_enough_values
    | `Too_many_values
    | `Structure_specific of string ]

  type is_empty = private EMPTY
  type is_nonempty = private NON_EMPTY

  module Deserialize : sig
    module Spec : sig
      type (_, _, _) t

      val nil : (('a, string) result, 'a, is_empty) t
      val cons : 'a Type.t -> ('b, 'c, 'd) t -> ('a -> 'b, 'c, is_nonempty) t

      module Infix : sig
        val ( @> ) :
          'a Type.t -> ('b, 'c, 'd) t -> ('a -> 'b, 'c, is_nonempty) t

        val ( ~: ) : 'a Type.t -> ('a -> ('b, string) result, 'b, is_nonempty) t
      end
    end
  end

  module Serialize : sig
    module Hlist : sig
      type (_, _) t

      val nil : (('a, string) result, is_empty) t
      val cons : 'a -> ('b, 'c) t -> ('a -> 'b, is_nonempty) t

      module Infix : sig
        val ( @> ) : 'a -> ('b, 'c) t -> ('a -> 'b, is_nonempty) t
        val ( ~: ) : 'a -> ('a -> ('b, string) result, is_nonempty) t
      end
    end

    type ('a, 'b) t = 'a -> ('b, is_nonempty) Hlist.t
  end

  type 'a t
  type ex = E : _ t -> ex

  val make :
    ?equal:('a -> 'a -> bool) ->
    ?pp:'a Fmt.t ->
    tag:char ->
    spec:('deser, 'a, is_nonempty) Deserialize.Spec.t ->
    deserialize:'deser ->
    serialize:('a, 'deser) Serialize.t ->
    unit ->
    'a t

  val tag : 'a t -> char
  val size : 'a t -> int
  val equal_t : 'a t -> 'b t -> ('a, 'b) Stdlib.Type.eq option
  val equal_v : 'a t -> 'a -> 'a -> bool
  val pp_v : 'a t -> 'a Fmt.t
  val of_list : 'a t -> Value.List.t -> ('a, [> error ]) result
  val to_list : 'a t -> 'a -> Value.List.t

  module Lut : sig
    type 'a structure = 'a t
    type t

    val empty : t
    val add : 'a structure -> t -> t
    val find : char -> t -> ex option
  end
end = struct
  type error =
    [ `Incompatible_types
    | `Not_enough_values
    | `Too_many_values
    | `Structure_specific of string ]

  type is_empty = private EMPTY
  type is_nonempty = private NON_EMPTY

  module R_infix = Monad.Binary.Make_infix (struct
    include Result

    let return = ok
  end)

  module Deserialize = struct
    module Spec = struct
      type (_, _, _) t =
        | Nil : (('a, string) result, 'a, is_empty) t
        | Cons : 'a Type.t * ('b, 'c, _) t -> ('a -> 'b, 'c, is_nonempty) t

      let rec length : type a b c. (a, b, c) t -> int = function
        | Nil -> 0
        | Cons (_, rest) -> 1 + length rest

      let nil = Nil
      let cons l r = Cons (l, r)

      module Infix = struct
        let ( @> ) l r = Cons (l, r)
        let ( ~: ) v = Cons (v, Nil)
      end
    end
  end

  module Serialize = struct
    module Hlist = struct
      type (_, _) t =
        | Nil : ((_, string) result, is_empty) t
        | Cons : 'a * ('b, _) t -> ('a -> 'b, is_nonempty) t

      let nil = Nil
      let cons l r = Cons (l, r)

      module Infix = struct
        let ( @> ) l r = Cons (l, r)
        let ( ~: ) l = Cons (l, Nil)
      end
    end

    type ('a, 'b) t = 'a -> ('b, is_nonempty) Hlist.t
  end

  type _ t =
    | T : {
        len : int;
        tag : char;
        equal : 'a -> 'a -> bool;
        pp : 'a Fmt.t;
        t_id : 'a Stdlib.Type.Id.t;
        spec : ('deser, 'a, is_nonempty) Deserialize.Spec.t;
        deser : 'deser;
        ser : ('a, 'deser) Serialize.t;
      }
        -> 'a t

  type ex = E : _ t -> ex

  let make ?(equal = ( = )) ?(pp = Pp.opaque) ~tag ~spec ~deserialize:deser
      ~serialize:ser () =
    let len = Deserialize.Spec.length spec in
    T { len; tag; equal; pp; spec; ser; deser; t_id = Stdlib.Type.Id.make () }

  let tag : type a. a t -> char = fun (T { tag; _ }) -> tag
  let size : type a. a t -> int = fun (T { len; _ }) -> len

  let equal_t : type a b. a t -> b t -> (a, b) Stdlib.Type.eq option =
   fun (T { t_id = t0; _ }) (T { t_id = t1; _ }) ->
    Stdlib.Type.Id.provably_equal t0 t1

  let equal_v : type a. a t -> a -> a -> bool = fun (T { equal; _ }) -> equal
  let pp_v : type a. a t -> a Fmt.t = fun (T { pp; _ }) -> pp

  let type_equal_t_res t0 t1 =
    Type.equal_t t0 t1 |> Option.to_result ~none:`Incompatible_types

  let rec of_list' :
      type a b c.
      Value.List.t ->
      (a -> b, c, is_nonempty) Deserialize.Spec.t ->
      (a -> b) ->
      (c, [> error ]) result =
   fun values (Deserialize.Spec.Cons (typ, tl)) f ->
    let open R_infix in
    match (tl, values) with
    | Nil, [ (T { typ = typ'; value } as v) ] ->
        Fmt.pr "%a %a\n%!" Type.pp typ Value.pp v;
        type_equal_t_res typ typ' >>= fun Equal ->
        let x : (c, error) result =
          f value |> Result.map_error (fun e -> `Structure_specific e)
        in
        x
    | Nil, _ -> Error `Too_many_values
    | (Cons _ as tl), (T { typ = typ'; value } as v) :: rest ->
        Fmt.pr "%a\n%!" Value.pp v;
        type_equal_t_res typ typ' >>= fun Equal ->
        let f = f value in
        of_list' rest tl f
    | Cons _, [] -> Error `Not_enough_values

  let of_list (T { spec = Cons _ as spec; deser; _ }) values =
    of_list' values spec deser |> Result.map_error (function #error as e -> e)

  let rec to_list' :
      type a b c d.
      (a, b, c) Deserialize.Spec.t -> (a, d) Serialize.Hlist.t -> Value.List.t =
   fun spec ser ->
    match (spec, ser) with
    | Cons (typ, tl_spec), Cons (value, tl_ser) ->
        Value.T { typ; value } :: to_list' tl_spec tl_ser
    | Nil, Nil -> []

  let to_list (T { spec = Cons _ as spec; ser; _ }) value =
    to_list' spec (ser value)

  module Lut = struct
    type 'a structure = 'a t
    type t = ex Char_map.t

    let empty = Char_map.empty
    let add (T { tag; _ } as structure) t = Char_map.add tag (E structure) t
    let find char t = Char_map.find_opt char t
  end
end
