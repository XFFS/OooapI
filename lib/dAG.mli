(* A simple DAG library, used for dependency resolution  *)

module Make : functor (O : Map.OrderedType) -> sig
  type t
  type node = O.t

  module Nodes : module type of Set.Make (O)

  val empty : t
  val nodes : t -> Nodes.t
  val children : node -> t -> node Seq.t
  val add_node : node -> t -> t
  val arcs_to : src:node -> dst:node -> t -> bool
  val add_arc : src:node -> dst:node -> t -> t

  val add_arcs : src:node -> node list -> t -> t
  (** [acc_arcs ~src dsts g] is a new graph built by ensuring [src] is a node in
      [g] and that arcs from [src] to each node in [dsts] are added to [g]. *)

  val remove_arc : src:node -> dst:node -> t -> t
  val remove_node : node -> t -> t
  val topological_sort : t -> node list
end
