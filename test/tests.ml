open Tezt

module _ = Test_parse_openapi_spec
module _ = Test_oooapi_gen
module _ = Test_dag

let () =
  Test.run ()
