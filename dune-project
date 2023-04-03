(lang dune 3.4)

(name ocamlunix)

(generate_opam_files true)

(source
 (github henrytill/ocamlunix))

(authors "Henry Till <henrytill@gmail.com>")

(maintainers "Henry Till <henrytill@gmail.com>")

(package
 (name ocamlunix)
 (synopsis "Unix system programming in OCaml")
 (description
  "Work from Leroy and R\195\169my's 'Unix system programming in OCaml'")
 (depends
  (ocaml
   (>= 4.13.0))
  dune
  (alcotest :with-test)))
