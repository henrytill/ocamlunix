(lang dune 3.4)

(name ocamlunix)

(generate_opam_files true)

(source
 (sourcehut henrytill/ocamlunix))

(authors "Henry Till <henrytill@gmail.com>")

(maintainers "Henry Till <henrytill@gmail.com>")

(package
 (name ocamlunix)
 (synopsis "Unix system programming in OCaml")
 (description "Work from Leroy and Rémy's 'Unix system programming in OCaml'")
 (depends
  (ocaml (>= 4.13.0))
  dune
  (alcotest :with-test)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
