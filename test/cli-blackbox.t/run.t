Error reported when source file cannot be found

  $ oooapi non-existent-file.json
  error: could not parse spec
  File non-existent-file.json does not exist or cannot be read
  [1]

Error reported when source file does not have a .json suffix

  $ touch foo.yml
  $ oooapi foo.yml
  error: could not parse spec
  Only JSON input is supported, expecting .json file but found foo.yml
  [1]

Can generate an implementation for the tictactoe spec

  $ oooapi tictactoe.json | tail -n +2
  let __TITLE__ = "Tic Tac Toe"
  let __API_VERSION__ = "1.0.0"
  module Ooo = Oooapi_lib
  let base_url = "/"
  module Data =
    struct
      module Mark =
        struct
          type t = string[@@deriving yojson { strict = false }][@@ocaml.doc
                                                                 "Possible values for a board square. `.` means empty square."]
        end
      module Board =
        struct
          type t = Mark.t list list[@@deriving yojson { strict = false }]
        end
      module Winner =
        struct
          type t = string[@@deriving yojson { strict = false }][@@ocaml.doc
                                                                 "Winner of the game. `.` means nobody has won yet."]
        end
      module Coordinate =
        struct type t = int[@@deriving yojson { strict = false }] end
      module ErrorMessage =
        struct
          type t = string[@@deriving yojson { strict = false }][@@ocaml.doc
                                                                 "A text message describing an error"]
        end
      module Status =
        struct
          type t =
            {
            winner: Winner.t option
              [@yojson.key "winner"][@yojson.default None];
            board: Board.t option [@yojson.key "board"][@yojson.default None]}
          [@@deriving (make, (yojson { strict = false }))]
        end
    end
  module Make(Client:Oooapi_lib.Client)(Config:Oooapi_lib.Config) =
    struct
      module Client = (Client)(Config)
      let get_square ~column  ~row  () =
        let _ooo_path = ["board"; row; column] in
        let _ooo_params = List.filter_map Fun.id [] in
        let _ooo_headers = [] in
        let _ooo_decode =
          ((`Code 200), (Ooo.of_json_string Data.Mark.of_yojson)) in
        let _ooo_data = None in
        Client.make_request `GET ~base_url ~path:_ooo_path ~params:_ooo_params
          ~headers:_ooo_headers ~decode:_ooo_decode ?data:_ooo_data[@@ocaml.doc
                                                                     "Get a single board square\n\nRetrieves the requested square."]
      let put_square ~column  ~row  data =
        let _ooo_path = ["board"; row; column] in
        let _ooo_params = List.filter_map Fun.id [] in
        let _ooo_headers = [] in
        let _ooo_decode =
          ((`Code 200), (Ooo.of_json_string Data.Status.of_yojson)) in
        let _ooo_data = Some (`Json (Data.Mark.to_yojson data)) in
        Client.make_request `PUT ~base_url ~path:_ooo_path ~params:_ooo_params
          ~headers:_ooo_headers ~decode:_ooo_decode ?data:_ooo_data[@@ocaml.doc
                                                                     "Set a single board square\n\nPlaces a mark on the board and retrieves the whole board and the winner (if any)."]
      let get_board () =
        let _ooo_path = ["board"] in
        let _ooo_params = List.filter_map Fun.id [] in
        let _ooo_headers = [] in
        let _ooo_decode =
          ((`Code 200), (Ooo.of_json_string Data.Status.of_yojson)) in
        let _ooo_data = None in
        Client.make_request `GET ~base_url ~path:_ooo_path ~params:_ooo_params
          ~headers:_ooo_headers ~decode:_ooo_decode ?data:_ooo_data[@@ocaml.doc
                                                                     "Get the whole board\n\nRetrieves the current state of the board and the winner."]
    end

Can read input from stdin

  $ oooapi < tictactoe.json | tail -1
    end

Report error when trying to parse invalid json

  $ echo "not json" | oooapi
  error: could not parse spec
  Line 1, bytes 0-9:
  Expected '{' but found 'not json
  '
  Input was not JSON conforming to the OpenaAPI format.
  [1]

Can generate and then compile the tictactoe spec, as specified in the dune file

  $ dune build
