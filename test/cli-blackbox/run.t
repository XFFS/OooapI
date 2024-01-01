Error reported when source file cannot be found

  $ oooapi non-existent-file.json
  error: could not parse spec non-existent-file.json
  File non-existent-file.json does not exist or cannot be read
  [1]

Can generate and then compile the tictactoe spec
from https://github.com/OAI/learn.openapis.org/blob/main/examples/tictactoe.yaml

  $ oooapi tictactoe.json > tictactoe_api.ml
  $ dune build
