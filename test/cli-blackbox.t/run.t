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
  open Oooapi_lib[@@warning "-33"]
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
      open Client
      let get_square ~column  ~row  () =
        let path = ["board"; row; column] in
        let params = List.filter_map Fun.id [] in
        let headers = [] in
        let decode = of_json_string Data.Mark.of_yojson in
        let data = None in
        make_request `GET ~base_url ~path ~params ~headers ~decode ?data
        [@@ocaml.doc
          "Get a single board square\n\nRetrieves the requested square."]
      let put_square ~column  ~row  data =
        let path = ["board"; row; column] in
        let params = List.filter_map Fun.id [] in
        let headers = [] in
        let decode = of_json_string Data.Status.of_yojson in
        let data = Some (`Json (Data.Mark.to_yojson data)) in
        make_request `PUT ~base_url ~path ~params ~headers ~decode ?data
        [@@ocaml.doc
          "Set a single board square\n\nPlaces a mark on the board and retrieves the whole board and the winner (if any)."]
      let get_board () =
        let path = ["board"] in
        let params = List.filter_map Fun.id [] in
        let headers = [] in
        let decode = of_json_string Data.Status.of_yojson in
        let data = None in
        make_request `GET ~base_url ~path ~params ~headers ~decode ?data
        [@@ocaml.doc
          "Get the whole board\n\nRetrieves the current state of the board and the winner."]
    end

Can read input from stdin

  $ oooapi < tictactoe.json | tail -1
    end

Can generate and then compile the tictactoe spec, as specified in the dune file

  $ dune build
