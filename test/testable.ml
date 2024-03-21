(**

( Packstream.Value.t,
  [> `Angstrom of string
  | `Incompatible_types
  | `Non_boolean_marker_byte of char
  | `Non_bytes_marker_byte of char
  | `Non_dictionary_marker_byte of char
  | `Non_float_marker_byte of char
  | `Non_int_16_marker_byte of char
  | `Non_int_32_marker_byte of char
  | `Non_int_64_marker_byte of char
  | `Non_int_8_marker_byte of char
  | `Non_list_marker_byte of char
  | `Non_string_marker_byte of char
  | `Non_structure_marker_byte of char
  | `Not_enough_fields of int
  | `Not_enough_values
  | `Structure_specific of string
  | `Too_many_values
  | `Unknown_marker_byte of char
  | `Unknown_structure_tag of char ] )
result
*)

let value = Alcotest.testable Packstream.Value.pp Packstream.Value.equal

let error () =
  Alcotest.testable
    (fun fmt -> function
      | `Angstrom str -> Fmt.string fmt str
      | `Incompatible_types -> Fmt.pf fmt "Incompatible_types"
      | `Non_structure_marker_byte b ->
          Fmt.pf fmt "Non_structure_marker_byte(%C)" b
      | `Non_boolean_marker_byte b -> Fmt.pf fmt "Non_boolean_marker_byte(%C)" b
      | `Unknown_marker_byte b -> Fmt.pf fmt "Unknown_marker_byte(%C)" b
      | s -> Fmt.any "<opaque>" fmt s)
    ( = )
