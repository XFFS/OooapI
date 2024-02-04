module Make (O : Map.OrderedType) = struct
  type node = O.t

  module Edges = Map.Make (O)
  module Nodes = Set.Make (O)

  type t = Nodes.t Edges.t

  let empty = Edges.empty
  let is_empty t = Edges.is_empty t
  let nodes t = t |> Edges.to_seq |> Seq.map (fun (k, _) -> k) |> Nodes.of_seq

  (* Nodes with no incoming edges *)
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

  exception Cycle_found of node list

  let raise_if_cycle_found node t: t =
    let rec find_trail n trail =
      match Edges.find_opt n t with
      | None          -> ()
      | Some children ->
        if Nodes.mem node children then
          raise (Cycle_found (List.rev (node :: trail)))
        else
          Nodes.iter (fun n' -> find_trail n' (n' :: trail)) children
    in
    find_trail node [node]; t

  let add_node a t =
    t
    |> Edges.update a (function
           | None    -> Some Nodes.empty
           | Some ns -> Some ns)

  let add_arc ~src ~dst t =
    t
    |> add_node dst
    |> Edges.update src (function
        | None    -> Some (Nodes.singleton dst)
        | Some cs -> Some (Nodes.add dst cs))
    |> raise_if_cycle_found src

  let add_arcs ~src dsts t =
    match dsts with
    | [] -> add_node src t
    | _ :: _ ->
        dsts
        |> ListLabels.fold_left ~init:t ~f:(fun g dst -> add_arc ~src ~dst g)

  let remove_arc ~src ~dst t =
    t |> Edges.update src (Option.map (Nodes.remove dst))

  let remove_node a t = t |> Edges.remove a |> Edges.map (Nodes.remove a)

  let topological_sort t =
    let rec aux acc g =
      if is_empty g then
        (* All nodes have been accounted for *)
        List.of_seq acc |> List.rev
      else
        (* Get all nodes with no incoming edges *)
        let deg0s = roots g in
        (* If all nodes have incoming edges, there's a cycle.
           I.e., we don't have a DAG, but this is impossible to construct. *)
        if Nodes.is_empty deg0s then failwith "non DAG";
        (* Add them to our sorted nodes *)
        let acc' = Seq.append (Nodes.to_seq deg0s) acc in
        (* Remove them from the graph *)
        let g' = Nodes.fold remove_node deg0s g in
        aux acc' g'
    in
    aux Seq.empty t

  let pp_trail pp_node fmt trail =
    let pp_trail = Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt " -> ")
        pp_node
    in
    Format.fprintf fmt "%a" pp_trail trail
end
