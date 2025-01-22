type voter = { preference : int list; bias : float }

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

let dpDistnace = ksDistance

let ksBetween p p' np =
  let p = judgementSet p in
  let p' = judgementSet p' in
  let np = judgementSet np in
  let combination = List.combine p p' in
  List.for_all2 (fun (l, r) m -> l = m || r = m) combination np && p <> p'

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

let initPython () =
  (* Dynamically determine the project root directory *)
  let project_root = Sys.getcwd () in
  let venv_path = Filename.concat project_root ".venv" in
  let python_scripts_dir = Filename.concat project_root "src/python" in

  (* Set the Python virtual environment variables *)
  Unix.putenv "VIRTUAL_ENV" venv_path;
  Unix.putenv "PATH" (venv_path ^ "/bin:" ^ Sys.getenv "PATH");

  (* Initialize the Python interpreter *)
  let () =
    Py.initialize ~interpreter:(Filename.concat venv_path "bin/python") ()
  in

  (* Add the directory containing your Python script to sys.path *)
  let sys = Py.Import.import_module "sys" in

  match Py.Object.get_attr_string sys "path" with
  | Some sys_path -> (
      match Py.Object.get_attr_string sys_path "append" with
      | Some append_method ->
          let directory = Py.String.of_string python_scripts_dir in
          Py.Object.call append_method
            (Py.Tuple.of_array [| directory |]) (* Arguments as a tuple *)
            Py.null
      | None -> failwith "Error: sys.path.append method not found")
  | None -> failwith "Error: sys.path not found"

let pyList_toInt py_list = Py.List.to_list_map Py.Int.to_int py_list

let parse_tuple py_tuple =
  let int_list = Py.Tuple.get_item py_tuple 0 |> pyList_toInt in
  let float_val = Py.Tuple.get_item py_tuple 1 |> Py.Float.to_float in
  (int_list, float_val)

let tupleToVoters (p, b) = { preference = p; bias = b }

let parse_pyVoters pv =
  Py.List.to_list_map parse_tuple pv |> List.map tupleToVoters
