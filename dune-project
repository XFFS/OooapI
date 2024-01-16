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

  ;  TODO Remove uneeded deps
  (multipart_form (>= 0.5.0))
  (multipart_form-lwt (>= 0.5.0))
  (multipart_form-cohttp-lwt (>= 0.5.0))
  (uuidm (>= 0.9.8))
  (cohttp-lwt-unix (>= 5.2.0))
  (http (>= 6.0.0~alpha2))
  (tls (>= 0.17.0))
  (tls-lwt (>= 0.17.0))

  ; Test deps
  (ppx_deriving_yojson :with-test)
  (ppx_deriving :with-test)

  (ppx_expect (and :with-test (>= v0.15.1)))
  (tezt :with-test)))

(package
 ;  TODO Which name convention to use here?
 (name oooapi_lib)
 (synopsis "Short description")
 (description "Longer description")
 (depends
  (dune (> 2.9))

  openapi_spec

  (ppxlib (>= 0.28.0))
  (camelsnakekebab (>= 0.4))

  ;  TODO Remove uneeded deps
  (multipart_form (>= 0.5.0))
  (multipart_form-lwt (>= 0.5.0))
  (multipart_form-cohttp-lwt (>= 0.5.0))
  (uuidm (>= 0.9.8))
  (cohttp-lwt-unix (>= 5.2.0))
  (http (>= 6.0.0))
  (tls (>= 0.17.0))
  (tls-lwt (>= 0.17.0))

  ; Test deps
  (ppx_deriving_yojson :with-test)
  (ppx_deriving :with-test)

  (ppx_expect (and :with-test (>= v0.15.1)))
  (tezt :with-test)))

(package
 (name openapi_spec)
 (synopsis "Short description")
 (description "Longer description")
 (depends
  (dune (> 2.9))

  (atdgen (>= 2.11.0))

  (ppx_expect (and :with-test (>= v0.15.1)))
  (tezt :with-test)))
