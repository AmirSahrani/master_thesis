open Graphs
open Utils

(*

TODO 
   - Move towards consensus
   - Proximity to single peakedness 
   - Proximity to single plateauedness

*)

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

let is_transitive profile =
  let maj_pref = profile |> maj in
  let alternatives = List.flatten @@ List.hd profile in
  List.for_all
    (fun x ->
      List.for_all
        (fun y ->
          List.for_all
            (fun z ->
              if x = y || y = z || x = z then true
              else
                let x_pref_y =
                  Hashtbl.find maj_pref (x, y) > Hashtbl.find maj_pref (y, x)
                in
                let y_pref_z =
                  Hashtbl.find maj_pref (y, z) > Hashtbl.find maj_pref (z, y)
                in
                let x_pref_z =
                  Hashtbl.find maj_pref (x, z) > Hashtbl.find maj_pref (z, x)
                in

                (not (x_pref_y && y_pref_z)) || x_pref_z)
            alternatives)
        alternatives)
    alternatives

let n_unique_preferences profile = List.length @@ unique_preferences profile

let distance_to_consensus profile distance =
  if n_unique_preferences profile = 1 then 1.
  else
    let dist_to_profiles =
      List.map
        (fun cand_pref ->
          List.fold_left
            (fun dist pref -> dist +. distance cand_pref pref)
            0.
            (all_profiles_weak (List.hd profile)))
        profile
    in
    List.fold_left
      (fun min_found curr -> if curr < min_found then curr else min_found)
      (List.hd dist_to_profiles) dist_to_profiles

let get_all_evals () =
  ( [
      "cyclic_start";
      "condorcet_start";
      "unique_start";
      "intransative_start";
      "consensus_dist_start";
      "cyclic_end";
      "condorcet_end";
      "unique_end";
      "intransative_end";
      "consensus_dist_end";
    ],
    [
      (fun prof _ -> string_of_bool @@ is_cyclic prof);
      (fun prof _ -> string_of_bool @@ has_condorcet prof);
      (fun prof _ -> string_of_int @@ n_unique_preferences prof);
      (fun prof _ -> string_of_bool @@ not @@ is_transitive prof);
      (fun prof distance ->
        string_of_float @@ distance_to_consensus prof distance);
    ] )
