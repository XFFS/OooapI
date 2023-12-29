(lang dune 3.7)
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

  openapi_spec

  (ppxlib (>= 0.28.0))
  (camelsnakekebab (>= 0.4))

  (multipart_form (>= 0.5.0))
  (multipart_form-lwt (>= 0.5.0))
  (multipart_form-cohttp-lwt (>= 0.5.0))
  uuidm
  cohttp-lwt-unix
  tls
  tls-lwt

  (ppx_deriving_yojson :with-test)
  (ppx_deriving :with-test)

  (ppx_expect (and :with-test (>= v0.15.1)))
  (tezt :with-test)))

;  TODO rm unneeded deps
(package
 (name openapi_spec)
 (synopsis "Short description")
 (description "Longer description")
 (depends
  (dune (> 2.9))

  (atdgen (>= 2.11.0))
  (ppxlib (>= 0.28.0))
  (camelsnakekebab (>= 0.4))
  ; Currently just used for its dependencies, because we
  ; are vendoring the source
  ; (json-data-encoding (>= 0.12.1))

  (ppx_expect (and :with-test (>= v0.15.1)))
  (tezt :with-test)))
