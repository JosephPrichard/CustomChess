(lang dune 3.8)

(name customchess)

(generate_opam_files true)

(source
 (github JosephPrichard/CustomChess))

(authors "Joseph Prichard")

(package
 (name customchess)
 (synopsis "Play Chess with custom pieces, rules, and variation")
 (depends ocaml dune ppx_deriving))