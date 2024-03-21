(lang dune 3.14)
(name packstream)

(source (github 7degre-es/ocaml-packstream))

(generate_opam_files true)

(version 0.0.1)

(authors "Zach Baylin <zbaylin@gmail.com>")
(maintainers "Zach Baylin <zbaylin@gmail.com>")

(package
  (name packstream)
  (synopsis "Neo4j Packstream library for OCaml")
  (depends
    (angstrom (>= 0.15.0))
    (angstrom (>= 0.8.2))
    (fmt (and (>= 0.9.0) (< 1.0.0)))))
