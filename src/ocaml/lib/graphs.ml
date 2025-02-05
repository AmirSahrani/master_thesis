open Utils

module ProfileNode = struct
  type t = int list list

  let compare = compare
  let hash = Hashtbl.hash
  let equal = ( = )

  let print x =
    print_list x string_of_int;
    print_char '\n'
end

module PreferenceNode = struct
  type t = int

  let compare = compare
  let hash = Hashtbl.hash
  let equal = ( = )
  let name x = string_of_int x
end

module Edge = struct
  type t = int

  let compare = compare
  let default = 0
end

module ProfileGraph =
  Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (ProfileNode) (Edge)

module PreferenceGraph =
  Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (PreferenceNode) (Edge)

module Weight = struct
  type edge = ProfileGraph.E.t
  type t = int

  let weight (_, e, _) = e
  let compare = Stdlib.compare
  let add = ( + )
  let zero = 0
end

module Dijkstra = Graph.Path.Dijkstra (ProfileGraph) (Weight)

module Dot = Graph.Graphviz.Dot (struct
  include ProfileGraph (* Use the graph module from above *)

  let edge_attributes (_, _, _) = []

  (* Convert edge label to string *)
  let default_edge_attributes _ = []
  let get_subgraph _ = None

  let vertex_attributes v =
    [ `Shape `Box; `Label (string_of_list_pref v string_of_int) ]

  let vertex_name v =
    String.concat ">"
      (List.map
         (fun inner ->
           "( " ^ String.concat ", " (List.map string_of_int inner) ^ " )")
         v)
  (* Convert vertex to string *)

  let default_vertex_attributes _ = []
  let graph_attributes _ = [ `Rankdir `LeftToRight ]
end)

module DotPref = Graph.Graphviz.Dot (struct
  include PreferenceGraph (* Use the graph module from above *)

  let edge_attributes (_, _, _) = []

  (* Convert edge label to string *)
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes x = [ `Shape `Box; `Label (string_of_int x) ]
  let vertex_name x = string_of_int x
  (* Convert vertex to string *)

  let default_vertex_attributes _ = []
  let graph_attributes _ = [ `Rankdir `LeftToRight ]
end)

let buildMajorityGraph maj =
  let g =
    Seq.fold_left
      (fun acc (start, _) -> PreferenceGraph.add_vertex acc start)
      PreferenceGraph.empty (Hashtbl.to_seq_keys maj)
  in
  Hashtbl.fold
    (fun (x, y) count acc ->
      (* Printf.printf "%d > %d: %d\n" x y count; *)
      if Hashtbl.find maj (y, x) < count then
        PreferenceGraph.add_edge_e acc (x, 1, y)
      else acc)
    maj g

let has_condorcet profile =
  let maj_pref = profile |> maj in
  let alternatives = List.flatten @@ List.hd profile in
  List.exists
    (fun cand ->
      List.for_all
        (fun cand' ->
          cand' = cand
          || Hashtbl.find maj_pref (cand, cand')
             > Hashtbl.find maj_pref (cand', cand))
        alternatives)
    alternatives

let is_cyclic profile =
  let graph = profile |> maj |> buildMajorityGraph in
  (* DotPref.output_graph (open_out "figures/test.dot") graph; *)
  let visited = Hashtbl.create 16 in
  let rec_stack = Hashtbl.create 16 in

  let rec dfs node =
    if Hashtbl.mem rec_stack node then true
      (* Cycle detected: node is in recursion stack *)
    else if not (Hashtbl.mem visited node) then (
      (* Mark the current node as visited and add to recursion stack *)
      Hashtbl.replace visited node true;
      Hashtbl.replace rec_stack node true;

      (* Visit all neighbors *)
      let neighbors = PreferenceGraph.succ graph node in
      let cycle_found = List.exists dfs neighbors in

      (* Remove the node from the recursion stack before returning *)
      Hashtbl.remove rec_stack node;

      cycle_found)
    else false (* Node already visited, no cycle detected *)
  in

  (* Run DFS for every unvisited node *)
  PreferenceGraph.fold_vertex
    (fun node acc -> acc || ((not (Hashtbl.mem visited node)) && dfs node))
    graph false

let buildGraph p set_between =
  let all_nodes = all_profiles_weak p in
  (* Create a graph with all vertices *)
  let g =
    List.fold_left
      (fun acc n -> ProfileGraph.add_vertex acc n)
      ProfileGraph.empty all_nodes
  in
  (* Add edges between every combination of vertices *)
  let g =
    List.fold_left
      (fun acc n1 ->
        List.fold_left
          (fun acc n2 ->
            if n1 <> n2 then
              let valid_edge =
                not @@ List.exists (set_between n1 n2) all_nodes
              in
              if valid_edge then ProfileGraph.add_edge_e acc (n1, 1, n2)
              else acc
            else acc)
          acc all_nodes)
      g all_nodes
  in
  (* Dot.output_graph (open_out "figures/test.dot") g; *)
  g

let shortest_path graph source target =
  if source = target then 0
  else
    match Dijkstra.shortest_path graph source target with
    | _, distance -> distance
