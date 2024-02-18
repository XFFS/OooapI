(lang dune 3.7)
(name oooapi)
(cram enable)
(generate_opam_files true)

(license MIT)
(authors "Shon Feder")
(maintainers "Shon Feder")
(source (github shonfeder/ooopai))

; Defined in ./lib and ./bin
(package
 (name oooapi)
 (synopsis "OooapI is OCaml of OpenAPI")
 (description "An executable to generate OCaml HTTP clients from OpenAPI specs")
 (depends
  (dune (> 2.9))

  openapi_spec

  (ocamlformat :with-test)))

(package
 (name oooapi_lib)
 (synopsis "A library abstracting calls to HTTP clients for OooapI")
 (description
   "A library abstracting calls to HTTP clients and providing all \
 dependencies needed by OooapI generated clients")
 (depends
  (dune (> 2.9))

  openapi_spec

  (multipart_form (>= 0.5.0))
  (multipart_form-lwt (>= 0.5.0))
  (multipart_form-cohttp-lwt (>= 0.5.0))
  (uuidm (>= 0.9.8))
  (cohttp-lwt-unix (>= 5.2.0))
  (http (>= 6.0.0~alpha1))
  (tls (>= 0.17.0))
  (tls-lwt (>= 0.17.0))
  (camelsnakekebab (>= 0.4))

  (ppx_deriving_yojson (>= 3.7.0))
  (ppx_deriving (>= 5.2.1))

  (ppx_expect (and :with-test (>= v0.15.1)))
  (tezt :with-test)))

(package
 (name openapi_spec)
 (synopsis "A representation of OpenAPI in OCaml data types")
 (description
   "ATD-based specification and deserializers of the OpenAPI scheme.")
 (depends
  (dune (> 2.9))

  (uri (>= 4.2.0))

  (atdgen (and :with-test (>= 2.11.0))) ; Only needed for generation of ATD sources
  (ppx_expect (and :with-test (>= v0.15.1)))
  (tezt :with-test)))
