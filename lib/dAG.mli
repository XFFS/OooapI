module Make :
  functor (O : Map.OrderedType) ->
    sig
      type t
      type node = O.t
      module Nodes : module type of Set.Make (O)
      val empty : t
      val nodes : t -> Nodes.t
      val children : node -> t -> node Seq.t
      val add_node : node -> t -> t
      val arcs_to : src:node -> dst:node -> t -> bool
      val add_arc : src:node -> dst:node -> t -> t
      val remove_arc : src:node -> dst:node -> t -> t
      val remove_node : node -> t -> t
      val topological_sort : t -> node list
    end
