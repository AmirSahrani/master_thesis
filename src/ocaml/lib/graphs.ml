open Utils

module Node = struct
  type t = int list

  let compare = compare
  let hash = Hashtbl.hash
  let equal = ( = )
end

module Edge = struct
  type t = int

  let compare = compare
  let default = 0
end

module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (Node) (Edge)

module Weight = struct
  type edge = G.E.t
  type t = int

  let weight (_, e, _) = e
  let compare = Stdlib.compare
  let add = ( + )
  let zero = 0
end

module Dijkstra = Graph.Path.Dijkstra (G) (Weight)

module Dot = Graph.Graphviz.Dot (struct
  include G (* Use the graph module from above *)

  let edge_attributes (_, _, _) = []

  (* Convert edge label to string *)
  let default_edge_attributes _ = []
  let get_subgraph _ = None

  let vertex_attributes v =
    [ `Shape `Box; `Label (string_of_list_pref v string_of_int) ]

  let vertex_name v = String.concat "" (List.map string_of_int v)
  (* Convert vertex to string *)

  let default_vertex_attributes _ = []
  let graph_attributes _ = [ `Rankdir `LeftToRight ]
end)

let buildGraph p edge_constructor =
  let all_nodes = permutations p in
  (* Create a graph with all vertices *)
  let g = List.fold_left (fun acc n -> G.add_vertex acc n) G.empty all_nodes in
  (* Add edges between every combination of vertices *)
  let g =
    List.fold_left
      (fun acc n1 ->
        List.fold_left
          (fun acc n2 ->
            if n1 <> n2 then
              (* Add an edge if dpBetween returns true *)
              let valid_edge =
                not @@ List.exists (edge_constructor n1 n2) all_nodes
              in
              if valid_edge then G.add_edge_e acc (n1, 1, n2) else acc
            else acc)
          acc all_nodes)
      g all_nodes
  in
  g

let shortest_path graph source target =
  match Dijkstra.shortest_path graph source target with
  | _, distance -> distance
