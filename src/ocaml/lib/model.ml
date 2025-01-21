type voter = { preference : int list; bias : float }

let objectiveFun voter1 voter2 distMeasure updatedProfile =
  let r = voter1.bias in
  let r' = 1. -. r in
  let p1 = voter1.preference in
  let p2 = voter2.preference in
  let lhs = Float.pow (distMeasure p1 updatedProfile) 2.0 in
  let rhs = Float.pow (distMeasure p2 updatedProfile) 2.0 in
  Float.sqrt ((r *. lhs) +. (r' *. rhs))

let judgementSet p =
  let alternatives = List.init (List.length p) (fun x -> x + 1) in
  let combinations =
    List.map
      (fun x ->
        List.map (fun y -> if x <> y then Some (x, y) else None) alternatives)
      alternatives
    |> List.flatten
    |> List.filter_map (fun x -> x)
  in
  List.map
    (fun (x, y) ->
      if List.find_index (Int.equal x) p > List.find_index (Int.equal y) p then
        1
      else -1)
    combinations

let ksDistance p p' =
  (* Kemeny Snell distance, it converts both preferences into judgement sets,
     and computes the minimum number of binary changes needed to get from one
     set to another *)
  let j1 = judgementSet p in
  let j2 = judgementSet p' in
  List.map2 (fun x y -> if x = y then 0. else 1.) j1 j2
  |> List.fold_left (fun x acc -> acc +. x) 0.

let csDistance p p' =
  (* calculates the distance between to profiles as the differ in rank of each alternative *)
  let alternatives = List.init (List.length p) (fun x -> x + 1) in
  List.fold_left
    (fun acc x ->
      match (List.find_index (( = ) x) p, List.find_index (( = ) x) p') with
      | Some r, Some r' -> acc +. (float_of_int @@ abs (r - r'))
      | _ -> acc)
    0. alternatives

let rec permutations lst =
  match lst with
  | [] ->
      [ [] ]
      (* Base case: the only permutation of an empty list is the empty list *)
  | _ ->
      List.flatten
        (List.map
           (fun x ->
             let rest = List.filter (( <> ) x) lst in
             List.map (fun perm -> x :: perm) (permutations rest))
           lst)

let update_profile v1 v2 distance =
  if v1 = v2 then v1
  else
    let obj = objectiveFun v1 v2 distance in
    let alternatives = List.init (List.length v1.preference) (fun x -> x + 1) in
    let profiles = permutations alternatives in
    let scores = List.map obj profiles in
    let min =
      List.fold_right
        (fun x acc -> if x < acc then x else acc)
        scores (List.nth scores 0)
    in
    let i =
      match List.find_index (( = ) min) scores with
      | None -> failwith ("Could not find index of: " ^ string_of_float min)
      | Some idx -> idx
    in
    let new_voter = { preference = List.nth profiles i; bias = v1.bias } in
    new_voter

let deliberate voters rounds distance =
  let announce voters announcer =
    List.map (fun voter -> update_profile voter announcer distance) voters
  in
  let rec round acc voters =
    match voters with [] -> acc | hd :: tl -> round (announce acc hd) tl
  in
  let rec aux vs r = if r >= rounds then vs else aux (round vs vs) (r + 1) in
  aux voters 0

let print_voter voter =
  let p = List.map string_of_int voter.preference |> String.concat " > " in
  Printf.printf "Voter : %s\n" p
