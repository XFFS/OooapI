These tests simulate the use oooapi in the context of an ocaml project.
See the ./dune file for the build configuration.

Generate the library and build the test utility, based on the dune file
  $ dune build

Test that we can hit a few endpoints.
NOTE: Remove the grep to see the results.

  $ dune exec test_openai
  list_models: OK
  create_completion: OK

