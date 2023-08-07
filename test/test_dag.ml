open Tezt
open Tezt.Base
open Lib

let test ?(tags = [ "dag" ]) title f = Test.register ~__FILE__ ~title ~tags f;;

test "add_arcs ~src [] ensures src is added to graph" @@ fun () ->
let module G = Oooapi.DAG.Make (Int) in
let nodes =
  G.empty |> G.add_arcs ~src:0 [] |> G.nodes |> G.Nodes.to_seq |> List.of_seq
in
Check.(
  (nodes = [ 0 ])
    (list int)
    ~error_msg:"expected the node 0 to be the sole node in the graph");
unit
;;

test "can topo sort dag" @@ fun () ->
let module G = Oooapi.DAG.Make (Int) in
(*
0 -> 1 -> 2 -> 3
          ^
4 -> 5 --/
*)
let edges = [ (0, 1); (1, 2); (2, 3); (4, 5); (5, 2) ] in
let actual =
  edges
  |> List.fold_left (fun g (src, dst) -> G.add_arc ~src ~dst g) G.empty
  |> G.topological_sort
in
Check.(
  (actual = [ 4; 0; 5; 1; 2; 3 ])
    (list int)
    ~error_msg:"expected nodes to be topologically sorted");
unit
