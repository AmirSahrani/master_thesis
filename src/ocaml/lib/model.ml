open Utils
open Distances

let objectiveFun voter1 voter2 distMeasure updatedProfile =
  let r = voter1.bias in
  let r' = 1. -. r in
  let p1 = voter1.preference in
  let p2 = voter2.preference in
  let lhs = Float.pow (distMeasure p1 updatedProfile) 2.0 in
  let rhs = Float.pow (distMeasure p2 updatedProfile) 2.0 in
  Float.sqrt ((r *. lhs) +. (r' *. rhs))

let update_profile v1 v2 distance between =
  if v1 = v2 then v1
  else
    let obj = objectiveFun v1 v2 distance in
    let alternatives = List.init (List.length v1.preference) (fun x -> x + 1) in
    let profiles = permutations alternatives in

    let profiles = List.filter (between v1.preference v2.preference) profiles in
    let scores = List.map obj profiles in
    if List.length scores = 0 then v1
    else
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
  let rec aux vs r = if r >= rounds then vs else aux (round vs vs) (r + 1) in
  aux voters 0

let print_voter voter =
  let p = List.map string_of_int voter.preference |> String.concat " > " in
  Printf.printf "Voter : %s\n" p
