open Graphs
open Utils

(*

TODO 
   - Proximity to single peakedness 
   - Proximity to single plateauedness

*)

let is_single_peaked profile =
  (* 
     Algorithm to determine is single peaked, runs in O(nm) adapted from the presentation by Elkind, who formulated it based on the algortihm by Doignon and Falmagne (1994), adding ideas from Escoffier et al. (2008)
     *)
  let rec stage_1 left right a' =
    if a' = [] || right <> [||] then (left, right, a')
    else
      let b = bottom profile a' in
      if List.length b > 2 then ([||], [||], a')
      else
        let a'' = List.filter (fun alt -> not (List.mem alt b)) a' in
        match b with
        | x :: y :: _ ->
            stage_1
              (Array.concat [ left; [| x |] ])
              (Array.concat [ [| y |]; right ])
              a''
        | x :: _ -> stage_1 (Array.concat [ left; [| x |] ]) right a''
        | _ -> failwith "Not bottom alternatives found"
  in
  let left, right, a' = stage_1 [||] [||] (List.hd profile) in
  if left = [||] && right = [||] then None
  else
    let rec part_2 left right a' =
      if List.length a' < 2 then
        Some (Array.to_list left @ a' @ Array.to_list right)
      else
        let l = left.(Array.length left - 1) in
        let r = right.(0) in
        let b = bottom profile a' in
        if List.length b > 2 then None
        else
          let l' =
            List.filter
              (fun m ->
                List.exists
                  (fun p -> between l m r p)
                  (List.filter (fun p -> List.hd p <> m) profile))
              b
          in
          let r' =
            List.filter
              (fun m ->
                List.exists
                  (fun p -> between r m l p)
                  (List.filter (fun p -> List.hd p <> m) profile))
              b
          in
          if List.length l' > 1 || List.length r' > 1 then None
          else if intersection l' r' <> [] then None
          else
            let rest =
              List.filter (fun elem -> not (List.mem elem (union l' r'))) b
            in
            (* assert (List.length rest <= 1); *)
            let l' = if List.length l' = 0 then rest @ l' else l' in
            let r' = if List.length r' = 0 then rest @ r' else r' in
            part_2
              (Array.append left (Array.of_list l'))
              (Array.append (Array.of_list r') right)
              (List.filter (fun elem -> not (List.mem elem b)) a')
    in
    part_2 left right a'

(* Split the societal order into the part left of candidate and the part right of candidate.
   The candidate must occur in the list; otherwise, we return None. 
   (The left part is built up in the order encountered, so it will be in the same order as in [order].) *)
let rec split candidate order =
  match order with
  | [] -> None
  | x :: xs -> (
      if x = candidate then Some ([], xs)
      else
        match split candidate xs with
        | None -> None
        | Some (l, r) -> Some (x :: l, r))

(* Given the two boundaries (lists of candidates that are next in line on the left and right)
   and the remaining voter ranking (pref), check that each next candidate is exactly the
   candidate at one of the two ends.
   
   Here, left_bound is the list of candidates to the left of the peak in *inward order*.
   That is, if societal order is [a; b; c; d; e] and the peak is c, then the left side 
   (the candidates to the left of c) is [a; b]. But the candidate immediately adjacent to c 
   on the left is b. So we want left_bound to be [b; a] (i.e. “closer” candidates first). *)
let rec check_single_peaked remaining_pref left_bound right_bound =
  match remaining_pref with
  | [] -> true
  | c :: cs ->
      let left_ok = match left_bound with [] -> false | x :: _ -> x = c in
      let right_ok = match right_bound with [] -> false | x :: _ -> x = c in
      if left_ok then check_single_peaked cs (List.tl left_bound) right_bound
      else if right_ok then
        check_single_peaked cs left_bound (List.tl right_bound)
      else false

(* Main function: checks if a voter's preference [pref] is single peaked with respect to [order]. *)
let is_single_peaked_wrt order pref =
  if List.length pref > List.length @@ List.flatten pref then false
  else
    match pref with
    | [] -> true (* trivial case *)
    | peak :: cs -> (
        match split peak order with
        | None -> false (* peak not in societal order *)
        | Some (left, right) ->
            (* The left list produced by [split] is in the order encountered,
             which is the same as [order] from left to right.
             However, the candidate immediately adjacent to the peak on the left
             is the *last* element of that list. So we reverse it to obtain the left boundary. *)
            let left_bound = match left with [] -> [] | _ -> List.rev left in
            check_single_peaked cs left_bound right)

let fraction_strongly_single_peaked prof =
  let orders = all_profiles (List.hd prof) in
  let closeness =
    List.map
      (fun ord ->
        List.map (is_single_peaked_wrt ord) prof
        |> List.map int_of_bool
        |> List.fold_left (fun sum x -> x + sum) 0)
      orders
  in
  let max_closeness =
    List.fold_left
      (fun acc x -> if acc > x then acc else x)
      (List.hd closeness) closeness
  in
  float_of_int max_closeness /. float_of_int (List.length prof)

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
      "proximity_to_sp_start";
      "cyclic_end";
      "condorcet_end";
      "unique_end";
      "intransative_end";
      "consensus_dist_end";
      "proximity_to_sp_end";
    ],
    [
      (fun prof _ -> string_of_bool @@ is_cyclic prof);
      (fun prof _ -> string_of_bool @@ has_condorcet prof);
      (fun prof _ -> string_of_int @@ n_unique_preferences prof);
      (fun prof _ -> string_of_bool @@ not @@ is_transitive prof);
      (fun prof distance ->
        string_of_float @@ distance_to_consensus prof distance);
      (fun prof _ -> string_of_float @@ fraction_strongly_single_peaked prof);
    ] )
