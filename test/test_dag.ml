open Tezt
open Tezt.Base

let test ?(tags = [ "dag" ]) title f = Test.register ~__FILE__ ~title ~tags f;;

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
