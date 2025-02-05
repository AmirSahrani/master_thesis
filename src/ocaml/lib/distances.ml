open Utils
open Graphs

let ksDistance p p' =
  (* Kemeny Snell distance, it converts both preferences into judgement sets,
     and computes the minimum number of binary changes needed to get from one
     set to another *)
  let j1 = judgementSet p in
  let j2 = judgementSet p' in
  List.map2 (fun x y -> if x = y then 0. else 1.) j1 j2
  |> List.fold_left (fun x acc -> acc +. x) 0.

let ksBetween p p' np =
  let j1 = judgementSet p in
  let j2 = judgementSet p' in
  let j3 = judgementSet np in
  let combination = List.combine j1 j2 in
  List.for_all2 (fun (l, r) m -> l = m || r = m) combination j3
  && j1 <> j3 && j3 <> j2

let dpBetween = ksBetween

let dpDistance p =
  let g = buildGraph p dpBetween in
  let d p p' =
    let distance = shortest_path g p p' in
    float_of_int distance
  in
  d

let csBetween p p' np =
  List.for_all
    (fun x ->
      let np_x_indx =
        List.find_index (fun r -> List.mem x r) np |> Option.get
      in
      let p_x_indx = List.find_index (fun r -> List.mem x r) p |> Option.get in
      let p'_x_indx =
        List.find_index (fun r -> List.mem x r) p' |> Option.get
      in
      (p_x_indx <= np_x_indx && np_x_indx <= p'_x_indx)
      || (p'_x_indx <= np_x_indx && np_x_indx <= p_x_indx))
    (List.flatten np)

let csDistance p p' =
  (* calculates the distance between two profiles as the difference in rank of each alternative *)
  let alternatives = List.init (List.length p) (fun x -> x + 1) in
  List.fold_left
    (fun acc x ->
      match
        ( List.find_index (fun rank -> List.mem x rank) p,
          List.find_index (fun rank -> List.mem x rank) p' )
      with
      | Some r, Some r' -> acc +. (float_of_int @@ abs (r - r'))
      | _ -> acc)
    0. alternatives
