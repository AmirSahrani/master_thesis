open Utils
open In_channel

module ProfileNode = struct
  type t = int list list

  let compare = compare
  let hash = Hashtbl.hash
  let equal = ( = )

  let print x =
    print_list x string_of_int;
    print_char '\n'
end

module GenericNode = struct
  type t = int

  let compare = compare
  let hash = Hashtbl.hash
  let equal = ( = )
  let label x = x
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

module GenericGraph =
  Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (GenericNode) (Edge)

module Weight = struct
  type edge = ProfileGraph.E.t
  type t = int

  let weight (_, e, _) = e
  let compare = Stdlib.compare
  let add = ( + )
  let zero = 0
end

module Dijkstra = Graph.Path.Dijkstra (ProfileGraph) (Weight)

module LabelVE = struct
  let node (n : GenericGraph.V.label) : Graph.Gml.value_list =
    [ (string_of_int n, Int n) ]

  let edge (e : GenericGraph.E.label) : Graph.Gml.value_list =
    [ (string_of_int e, Int e) ]
end

module GmlGenerator = Graph.Gml.Print (GenericGraph) (LabelVE)

let read_adjacency_matrix filename =
  let ic = open_in filename in
  let data = input_lines ic in
  List.filter_map
    (fun line ->
      match String.split_on_char ' ' line with
      | [ s; t ] -> Some (int_of_string s, int_of_string (String.trim t))
      | [] -> None
      | _ -> failwith "File improperly formatted")
    data

let write_adjacency_matrix graph filename =
  let oc = open_out filename in
  let edges =
    GenericGraph.fold_edges_e
      (fun e acc ->
        let src = GenericGraph.E.src e in
        let dst = GenericGraph.E.dst e in
        (string_of_int src ^ " " ^ string_of_int dst) :: acc)
      graph []
  in
  String.concat "\n" edges |> output_string oc;
  close_out oc

let save_matrix_adjacency matrix filename =
  let open Owl in
  let oc = open_out filename in
  let rows, cols = Mat.shape matrix in
  (* Write header row - useful for Gephi *)
  Printf.fprintf oc "Source,Target,Weight\n";

  (* Iterate through the matrix *)
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      let weight = Mat.get matrix i j in
      if weight <> 0. then (* Only write non-zero entries *)
        Printf.fprintf oc "%d,%d,%f\n" i j weight
    done
  done;

  close_out oc;
  Printf.printf "Matrix saved to %s\n" filename

let adjacency_matrix_from graph =
  let node_count = GenericGraph.nb_vertex graph in
  let adjacency_matrix = Owl.Mat.zeros node_count node_count in
  let rename_table = Hashtbl.create node_count in
  List.iteri
    (fun i node -> Hashtbl.add rename_table node i)
    (GenericGraph.fold_vertex (fun node lst -> node :: lst) graph []);
  GenericGraph.iter_vertex
    (fun source ->
      List.iter
        (fun target ->
          let source_label = Hashtbl.find rename_table source in
          let target_label = Hashtbl.find rename_table target in
          Owl.Mat.set adjacency_matrix source_label target_label 1.)
        (GenericGraph.succ graph source))
    graph;
  adjacency_matrix

module Dot = Graph.Graphviz.Dot (struct
  include ProfileGraph (* Use the graph module from above *)

  let edge_attributes (_, _, _) = []

  (* Convert edge label to string *)
  let default_edge_attributes _ = []
  let get_subgraph _ = None

  let vertex_attributes v =
    [ `Shape `Box; `Label (string_of_list_pref v string_of_int) ]

  let vertex_name v =
    "\""
    ^ String.concat "_pref_"
        (List.map
           (fun inner -> String.concat ", " (List.map string_of_int inner))
           v)
    ^ "\""
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
  print_string "Saving Graph.\n";
  (* Dot.output_graph (open_out "figures/dpGraph.dot") g; *)
  g

let shortest_path graph source target =
  if source = target then 0
  else
    match Dijkstra.shortest_path graph source target with
    | _, distance -> distance

(* let forest_fire_sample graph target_num p_forward p_backward =
  (*
      Sample from a graph using a spreading "fire".
      Initiallize by picking random node. This node can burn each of its edges,
      if a edge gets burned, the neighbor on the other side of the edge "catches" fire, and can burn its own links.

      !Note burning an edge, is how an edge gets sampled. Thus the final graph is a graph of all the burned edges between nodes.
  *)
  let initial_node = Random.int (GenericGraph.nb_vertex graph) in
  let ambassador_node = Random.int (GenericGraph.nb_vertex graph) in
  let visited = IntSet.empty in
  let visited = IntSet.add initial_node visited in
  let queue = Queue.create () in
  let _ = Queue.add initial_node queue in

  let rec spread ambassador source_node graph' v =
    if GenericGraph.nb_vertex graph' >= target_num then (graph', visited)
    else
      let node =
        if Queue.is_empty queue then Random.int (GenericGraph.nb_vertex graph)
        else Queue.take queue
      in
      let updated_graph =
        if source_node <> node then
          GenericGraph.add_vertex graph' node |> fun g ->
          GenericGraph.add_edge g source_node node
        else graph'
      in
      let visited = IntSet.add node v in
      let x = Owl_stats_dist.binomial_sf 1 p_forward  
      

      let spread_neighbors =
        GenericGraph.succ graph node GenericGraph.succ graph ambassador
        |> List.filter (fun _ -> Random.float 1. < p_forward)
      in
      let spread_neighbors_backwards =
        GenericGraph.pred graph ambassador
        |> List.filter (fun _ -> Random.float 1. < p_backward)
      in
      let all_spread_neighbors =
        spread_neighbors @ spread_neighbors_backwards
      in
      List.iter
        (fun new_neighbor -> Queue.add new_neighbor queue)
        all_spread_neighbors;

      (* let ambassador_node' = Random.int (GenericGraph.nb_vertex graph) in *)
      spread source_node updated_graph visited
  in
  let sampled_graph, _ = spread initial_node GenericGraph.empty visited in
  sampled_graph *)

let ties_sampling graph n =
  let rec sampling_edges nodes =
    if List.length nodes = n then nodes
    else
      let sampled_edge = Random.int (GenericGraph.nb_edges graph) in
      let _, sampled_nodes =
        GenericGraph.fold_edges
          (fun v1 v2 (i, lst) ->
            if i = sampled_edge && v1 <> v2 then (i + 1, (v1, v2) :: lst)
            else (i + 1, lst))
          graph (0, [])
      in
      match sampled_nodes with
      | [ (v1, v2) ] -> sampling_edges (v1 :: v2 :: nodes)
      | _ -> sampling_edges nodes
  in
  let sampled_nodes = sampling_edges [] in
  let induced_graph =
    List.fold_left
      (fun g' v1 ->
        List.fold_left
          (fun g'' v2 ->
            if GenericGraph.mem_edge graph v1 v2 then
              GenericGraph.add_edge g'' v1 v2
            else g'')
          g' sampled_nodes)
      GenericGraph.empty sampled_nodes
  in
  induced_graph
