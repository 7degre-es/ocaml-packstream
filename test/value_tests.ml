open Common
open Alcotest
open Packstream

module Structures = struct
  open Structure

  type singleton = { foo : int }
  type three = { one : string; two : float; three : unit }

  let pp_singleton fmt { foo } = Fmt.int fmt foo

  let pp_three =
    Fmt.Dump.(
      record
        [
          field "one" (fun t -> t.one) string;
          field "two" (fun t -> t.two) Fmt.float;
          field "three" (fun t -> t.three) (Fmt.any "()");
        ])

  let singleton =
    make ~pp:pp_singleton ~tag:'S'
      ~spec:Deserialize.Spec.Infix.(~:Type.Int_8)
      ~deserialize:(fun i -> Ok { foo = Signed.Int8.to_int i })
      ~serialize:(fun { foo } ->
        Serialize.Hlist.Infix.(~:(Signed.Int8.of_int foo)))
      ()

  let three =
    make ~pp:pp_three ~tag:'T'
      ~spec:Deserialize.Spec.Infix.(String @> Float @> ~:Null)
      ~deserialize:(fun one two three -> Ok { one; two; three })
      ~serialize:(fun { one; two; three } ->
        Serialize.Hlist.Infix.(one @> two @> ~:three))
      ()
end

let structure_lut =
  let open Structure.Lut in
  empty |> add Structures.singleton |> add Structures.three

let of_string str =
  Angstrom.parse_string ~consume:All (Parser.value ~structure_lut ()) str
  |> Result.map_error (fun str -> `Angstrom str)
  |> Result.join

module Parser = struct
  let test_ext blob expected =
    let res = of_string blob in
    check Testable.(result value (error ())) "true" expected res

  let test_truth () =
    test_ext [%blob "test/truth.bin"]
      (Ok (Value.T { typ = Boolean; value = true }))

  let test_untruth () =
    test_ext [%blob "test/untruth.bin"]
      (Ok (Value.T { typ = Boolean; value = false }))

  let test_string () =
    test_ext [%blob "test/string.bin"]
      (Ok (Value.T { typ = String; value = "Hello world" }))

  let test_float () =
    test_ext [%blob "test/float.bin"]
      (Ok (Value.T { typ = Float; value = 1.234 }))

  let test_nan () =
    test_ext [%blob "test/nan.bin"]
      (Ok (Value.T { typ = Float; value = Float.nan }))

  let test_inf () =
    test_ext [%blob "test/inf.bin"]
      (Ok (Value.T { typ = Float; value = Float.infinity }))

  let test_neg_inf () =
    test_ext [%blob "test/neg_inf.bin"]
      (Ok (Value.T { typ = Float; value = Float.neg_infinity }))

  let test_serially_increasing_list () =
    let l =
      List.init 100_000 (fun i ->
          if i <= 127 then Value.T { typ = Int_8; value = Signed.Int8.of_int i }
          else if i <= 32_767 then
            T { typ = Int_16; value = Signed.Int16.of_int i }
          else T { typ = Int_32; value = Signed.Int32.of_int i })
    in
    test_ext [%blob "test/serially_increasing_list.bin"]
      (Ok (T { typ = List; value = l }))

  let test_small_dictionary () =
    let d =
      String_map.(
        singleton "pack" (Value.T { typ = String; value = "stream" })
        |> add "12" (Value.T { typ = Int_8; value = Signed.Int8.of_int 34 })
        |> add "true" (Value.T { typ = Boolean; value = false }))
    in
    test_ext [%blob "test/small_dictionary.bin"]
      (Ok (T { typ = Dictionary; value = d }))

  let test_singleton () =
    let str = "\xB1S\x01" in
    let res = of_string str in
    check
      Testable.(result value (error ()))
      "same value"
      (Ok
         (Packstream.Value.T
            { typ = Structure Structures.singleton; value = { foo = 1 } }))
      res
end

let () =
  run "Value"
    [
      ( "Parser",
        [
          test_case "truth" `Quick Parser.test_truth;
          test_case "untruth" `Quick Parser.test_untruth;
          test_case "string" `Quick Parser.test_string;
          test_case "float" `Quick Parser.test_float;
          test_case "nan" `Quick Parser.test_nan;
          test_case "inf" `Quick Parser.test_inf;
          test_case "neg_inf" `Quick Parser.test_neg_inf;
          test_case "serially_increasing_list" `Quick
            Parser.test_serially_increasing_list;
          test_case "small_dictionary" `Quick Parser.test_small_dictionary;
          test_case "Structure.singleton" `Quick Parser.test_singleton;
        ] );
    ]
