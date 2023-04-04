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

   json-data-encoding
   ppxlib
   atdgen

  (ppx_expect (and :with-test (>= v0.15.1)))
  (tezt :with-test)))
