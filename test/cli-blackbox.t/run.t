Error reported when source file cannot be found

  $ oooapi non-existent-file.json
  error: could not parse spec non-existent-file.json
  File non-existent-file.json does not exist or cannot be read
  [1]

Can generate an implementation for the tictactoe spec

$ oooapi tictactoe.json

Can generate and then compile the tictactoe spec, as specified in the dune file

  $ dune build
