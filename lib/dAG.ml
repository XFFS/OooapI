module Make (O : Map.OrderedType) = struct
  type node = O.t

  module Edges = Map.Make (O)
  module Nodes = Set.Make (O)

  type t = Nodes.t Edges.t

  let empty = Edges.empty
  let is_empty t = Edges.is_empty t
  let nodes t = t |> Edges.to_seq |> Seq.map (fun (k, _) -> k) |> Nodes.of_seq

  let roots t =
    let child_nodes =
      Edges.fold (fun _ cs acc -> Nodes.union acc cs) t Nodes.empty
    in
    nodes t |> Nodes.filter (fun n -> not (Nodes.mem n child_nodes))

  let arcs_to ~src ~dst t =
    t
    |> Edges.find_opt src
    |> Option.map (fun nodes -> Nodes.mem dst nodes)
    |> Option.value ~default:false

  let children a t =
    t |> Edges.find_opt a |> Option.value ~default:Nodes.empty |> Nodes.to_seq

  let add_node a t =
    t
    |> Edges.update a (function
           | None -> Some Nodes.empty
           | Some ns -> Some ns)

  let add_arc ~src ~dst t =
    t
    |> add_node dst
    |> Edges.update src (function
           | None -> Some (Nodes.singleton dst)
           | Some cs -> Some (Nodes.add dst cs))

  let add_arcs ~src dsts t =
    match dsts with
    | [] -> add_node src t
    | _ :: _ ->
        dsts
        |> ListLabels.fold_left ~init:t ~f:(fun g dst -> add_arc ~src ~dst g)

  let remove_arc ~src ~dst t =
    t |> Edges.update src (Option.map (Nodes.remove dst))

  let remove_node a t = t |> Edges.remove a |> Edges.map (Nodes.remove a)

  (* exception Cycle_found of node *)

  (* DFS algorithm from https://en.wikipedia.org/wiki/Topological_sorting *)
  let topological_sort t =
    let rec aux acc g =
      if is_empty g then
        (* All nodes have been accounted for *)
        List.of_seq acc |> List.rev
      else
        (* Get all nodes with no incoming edges *)
        let deg0s = roots g in
        (* Add them to our sorted nodes *)
        let acc' = Seq.append (Nodes.to_seq deg0s) acc in
        (* Remove them from the graph *)
        let g' = Nodes.fold remove_node deg0s g in
        aux acc' g'
    in
    aux Seq.empty t
end
