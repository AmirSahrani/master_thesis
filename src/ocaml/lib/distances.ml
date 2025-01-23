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
  let p = judgementSet p in
  let p' = judgementSet p' in
  let np = judgementSet np in
  let combination = List.combine p p' in
  List.for_all2 (fun (l, r) m -> l = m || r = m) combination np
  && not (p = p' || p = np || np = p')

let dpBetween p p' = ksBetween p p'

let dpDistance p p' =
  let g = buildGraph p dpBetween in
  shortest_path g p p' |> float_of_int

let csBetween p p' np =
  let combination = List.combine p p' in
  List.for_all2 (fun (l, r) m -> l = m || r = m) combination np && p <> p'

let csDistance p p' =
  (* calculates the distance between to profiles as the differ in rank of each alternative *)
  let alternatives = List.init (List.length p) (fun x -> x + 1) in
  List.fold_left
    (fun acc x ->
      match (List.find_index (( = ) x) p, List.find_index (( = ) x) p') with
      | Some r, Some r' -> acc +. (float_of_int @@ abs (r - r'))
      | _ -> acc)
    0. alternatives
