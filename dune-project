(lang dune 2.9)

(name opal)
(version 0.1.1)

(generate_opam_files false)

(source
 (github pyrocat101/opal))

(authors "Linjie Ding <i@pyroc.at>")

(maintainers "Linjie Ding <i@pyroc.at>")

(license MIT)

(documentation https://github.com/pyrocat101/opal)

(package
 (name opal)
 (synopsis "Self-contained monadic parser combinators for OCaml")
 (description "Opal is a minimum collection of useful parsers and combinators (~150 loc of OCaml) that makes writing parsers easier. It is designed to be small, self-contained, pure-functional, and only includes most essential parsers, so that one could include single file in the project or just embed it in other OCaml source code files.")
 (depends camlp-streams ocamlfind)
 (tags
  (topics "parser" "combinator")))
