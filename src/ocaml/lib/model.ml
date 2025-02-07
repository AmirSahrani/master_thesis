open Utils

let objectiveFun v1 v2 distMeasure updatedProfile =
  let r = v1.bias in
  let r' = 1.0 -. r in
  let p1 = v1.preference in
  let p2 = v2.preference in
  let d1 = distMeasure p1 updatedProfile in
  let d2 = distMeasure p2 updatedProfile in
  assert (
    Float.compare d1 1.0 <> -1
    || Float.compare d2 1.0 <> -1
    || v1.preference = v2.preference);
  let lhs = Float.pow d1 2.0 in
  let rhs = Float.pow d2 2.0 in
  (r *. lhs) +. (r' *. rhs)

let update_profile v1 v2 distance between =
  let obj = objectiveFun v1 v2 distance in
  let alternatives =
    List.init (List.length v1.preference) (fun x -> [ x + 1 ])
  in
  let profiles = all_profiles alternatives in

  let profiles = List.filter (between v1.preference v2.preference) profiles in
  let profiles = v1.preference :: v2.preference :: profiles in
  let scores = List.map obj profiles in
  let _, i =
    List.fold_left
      (fun (min_val, min_idx) (x, i) ->
        if x < min_val then (x, i) else (min_val, min_idx))
      (List.hd scores, 0)
      (List.mapi (fun i x -> (x, i)) scores)
  in
  let new_voter =
    {
      preference = List.nth profiles i;
      bias = v1.bias;
      announced = v1.announced;
    }
  in
  new_voter

let deliberate voters rounds distance between =
  let announce listeners announcer =
    List.map
      (fun voter ->
        if voter = announcer then
          {
            preference = voter.preference;
            bias = voter.bias;
            announced = voter.announced + 1;
          }
        else
          let nw = update_profile voter announcer distance between in
          nw)
      listeners
  in

  let rec round updated_voters unnannounced_voters =
    match unnannounced_voters with
    | [] -> updated_voters
    | announcer :: listeners ->
        round (announce updated_voters announcer) listeners
  in

  let rec aux vs r =
    (*
    print_profile (List.map (fun v -> v.preference) vs);
    Printf.printf "\nUnique preferences: %d\n" (unique_preferences vs); *)
    if
      r >= rounds
      || unique_preferences (List.map (fun v -> v.preference) vs) = 1
    then vs
    else
      let vs = shuffle vs in
      aux (round vs vs) (r + 1)
  in
  aux voters 0
