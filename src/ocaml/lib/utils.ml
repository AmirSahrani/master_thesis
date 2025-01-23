type voter = { preference : int list; bias : float }

let rec permutations lst =
  match lst with
  | [] -> [ [] ]
  | _ ->
      List.flatten
        (List.map
           (fun x ->
             let rest = List.filter (( <> ) x) lst in
             List.map (fun perm -> x :: perm) (permutations rest))
           lst)

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

let pyList_toInt py_list = Py.List.to_list_map Py.Int.to_int py_list

let parse_tuple py_tuple =
  let int_list = Py.Tuple.get_item py_tuple 0 |> pyList_toInt in
  let float_val = Py.Tuple.get_item py_tuple 1 |> Py.Float.to_float in
  (int_list, float_val)

let tupleToVoters (p, b) = { preference = p; bias = b }

let parse_pyVoters pv =
  Py.List.to_list_map parse_tuple pv |> List.map tupleToVoters

let string_of_list lst convert =
  Printf.sprintf "\"%s\"" (String.concat " > " (List.map convert lst))

let string_of_list_pref lst convert =
  Printf.sprintf "$ %s $" (String.concat " \\pref " (List.map convert lst))

let print_list lst convert =
  Printf.printf "[ %s ]" (String.concat " ; " (List.map convert lst))
