open Utils
open Distances

let objectiveFun v1 v2 distMeasure updatedProfile =
  let r = v1.bias in
  let r' = 1.0 -. r in
  let p1 = v1.preference in
  let p2 = v2.preference in
  let d1 = distMeasure p1 updatedProfile in
  let d2 = distMeasure updatedProfile p2 in
  (* assert (Float.compare d1 1.0 <> -1 || Float.compare d2 1.0 <> -1); *)
  let lhs = Float.pow d1 2.0 in
  let rhs = Float.pow d2 2.0 in
  Float.sqrt ((r *. lhs) +. (r' *. rhs))

let update_profile v1 v2 distance between =
  if v1.preference = v2.preference then v1
  else
    let obj = objectiveFun v1 v2 distance in
    let alternatives = List.init (List.length v1.preference) (fun x -> x + 1) in
    let profiles = all_profiles alternatives in

    let profiles = List.filter (between v1.preference v2.preference) profiles in
    let profiles = v1.preference :: v2.preference :: profiles in
    Printf.printf "num profiles: %d\n" (List.length profiles);
    let scores = List.map obj profiles in
    if List.length scores = 0 then v1
    else
      let _, i =
        List.fold_left
          (fun (min_val, min_idx) (x, i) ->
            if x < min_val then (x, i) else (min_val, min_idx))
          (List.hd scores, 0)
          (List.mapi (fun i x -> (x, i)) (List.tl scores))
      in
      let new_voter = { preference = List.nth profiles i; bias = v1.bias } in
      (* print_endline "-----------------------";
      print_float (obj v1.preference);
      print_voter v1;

      print_float (obj v2.preference);
      print_voter v2;

      print_float (obj new_voter.preference);
      print_voter new_voter; *)
      new_voter

let deliberate voters rounds space =
  let p = (List.nth voters 0).preference in
  let distance, between =
    match space with
    | KS -> (ksDistance, ksBetween)
    | CS -> (csDistance, csBetween)
    | DP -> (dpDistance p, dpBetween)
  in

  let announce voters announcer =
    List.map
      (fun voter -> update_profile voter announcer distance between)
      voters
  in

  let rec round acc voters =
    match voters with [] -> acc | hd :: tl -> round (announce acc hd) tl
  in

  let rec aux vs r =
    (* print_endline "-------------------------------";
       print_profile (List.map (fun v -> v.preference) vs); *)
    if r >= rounds then vs
    else
      let vs = shuffle vs in
      aux (round vs vs) (r + 1)
  in
  aux voters 0

(* let print_voter voter =
   let p = List.map string_of_int voter.preference |> String.concat " > " in
   Printf.printf "Voter : %s\n" p *)
