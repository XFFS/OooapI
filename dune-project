(lang dune 2.9)
(name oooapi)
(cram enable)
(generate_opam_files true)

(license MIT)
(authors "Shon Feder")
(maintainers "Shon Feder")
(source (github shonfeder/ooopai))

(package
 (name oooapi)
 (synopsis "Short description")
 (description "Longer description")
 (depends
  (dune (> 2.9))
  (ocaml (>= 5.0))

  (atdgen (>= 2.11.0))
  (ppxlib (>= 0.28.0))
  ; Currently just used for its dependencies
  (json-data-encoding (>= 0.12.1))

  (ppx_expect (and :with-test (>= v0.15.1)))
  (tezt :with-test)))
