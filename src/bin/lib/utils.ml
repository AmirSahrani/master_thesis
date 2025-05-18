type preference = int list list
type profile = preference list
type voter = { preference : preference; bias : float; announced : int }
type timeRange = float list
type spaces = KS | CS | DP
type alternativeGenerators = Random | Voter | SampleVoters

type output = {
  original_opinion : Owl.Mat.mat;
  simulated_opinion : Owl.Mat.mat;
  true_opinion : Owl.Mat.mat;
  original_preferences : profile;
  simulated_preferences : profile;
  true_preferences : profile;
  trust_start : Owl.Mat.mat;
  trust_current : Owl.Mat.mat;
}

let filter_odd = function
    | `Int x ->
        if x mod 2 = 0 then `Int (x + [| -1; 1 |].(Random.int 1)) else `Int x
    | _ -> failwith "Not an interger"

let experiments = function
    | "rad" -> `Rad
    | "degroot" -> `DeGroot
    | "sens" -> `Sensitivity
    | "test" -> `Testing
    | _ -> failwith "Provide valid experiment type"

let alternativeGenerator_of = function
    | "Random" -> Random
    | "Voter" -> Voter
    | "SampleVoters" -> SampleVoters
    | _ -> failwith "Provide candidate sampling type"

let string_of_space = function KS -> "KS" | DP -> "DP" | CS -> "CS"

let string_of_sampler = function
    | Random -> "Random"
    | SampleVoters -> "Sample"
    | Voter -> "Voter"

module IntSet = Set.Make (struct
  type t = int

  let compare = compare
end)

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

let cartesian_product l1 l2 =
    List.concat_map (fun x -> List.map (fun y -> y @ x) l2) l1

let int_of_bool b = match b with true -> 1 | false -> 0

let all_profiles_weak _ =
    [
      [ [ 1; 2; 3 ] ];
      [ [ 1 ]; [ 2 ]; [ 3 ] ];
      [ [ 1; 2 ]; [ 3 ] ];
      [ [ 1 ]; [ 2; 3 ] ];
      [ [ 1 ]; [ 3 ]; [ 2 ] ];
      [ [ 1; 3 ]; [ 2 ] ];
      [ [ 2; 3 ]; [ 1 ] ];
      [ [ 2 ]; [ 3; 1 ] ];
      [ [ 2 ]; [ 1 ]; [ 3 ] ];
      [ [ 2 ]; [ 3 ]; [ 1 ] ];
      [ [ 3 ]; [ 2; 1 ] ];
      [ [ 3 ]; [ 2 ]; [ 1 ] ];
      [ [ 3 ]; [ 1 ]; [ 2 ] ];
    ]

let all_profiles p = permutations p
(* let all_profiles = all_profiles_weak *)

let rec shuffle = function
    | [] -> []
    | [ single ] -> [ single ]
    | list ->
        let before, after = List.partition (fun _ -> Random.bool ()) list in
            List.rev_append (shuffle before) (shuffle after)

let arange start stop step =
    let rec aux l curr =
        if curr > stop then List.rev l else aux (curr :: l) (curr +. step)
    in
        aux [] start

let range start stop step =
    let rec aux l curr =
        if curr > stop then List.rev l else aux (curr :: l) (curr + step)
    in
        aux [] start

let pairs p =
    let alternatives = List.init (List.length p) (fun x -> x + 1) in
        List.map
          (fun x ->
            List.map
              (fun y -> if x <> y then Some (x, y) else None)
              alternatives)
          alternatives
        |> List.flatten
        |> List.filter_map (fun x -> x)

let judgementSet p =
    let combinations = pairs @@ List.flatten p in
        List.map
          (fun (x, y) ->
            if List.find_index (List.mem x) p <= List.find_index (List.mem y) p
            then 1
            else -1)
          combinations

let unique lst =
    let rec aux l l' =
        match l with
        | [] -> l'
        | hd :: tl -> if List.mem hd l' then aux tl l' else aux tl (hd :: l')
    in
        aux lst []

let print_list lst convert =
    Printf.printf "[ %s ]" (String.concat " ; " (List.map convert lst))

let print_mat mat = mat |> Owl_pretty.dsnda_to_string |> print_endline

let print_shape mat =
    let n_row, n_cols = Owl.Mat.shape mat in
        Printf.printf "Matrix of shape %d %d\n%!" n_row n_cols

let string_of_list lst convert sep =
    Printf.sprintf "\"%s\"" (String.concat sep (List.map convert lst))

let string_of_list_pref lst convert =
    Printf.sprintf "%s"
      (String.concat " ≻ "
         (List.map
            (fun inner ->
              Printf.sprintf "(%s)"
                (String.concat ", " (List.map convert inner)))
            lst))

let print_profile prof =
    List.iter
      (fun p ->
        print_endline
        @@ String.concat " > "
             (List.map
                (fun inner ->
                  "( "
                  ^ String.concat ", " (List.map string_of_int inner)
                  ^ " )")
                p))
      prof

let print_voter v =
    Printf.printf "Bias: %.2f " v.bias;
    print_profile [ v.preference ]

let print_param_value = function
    | `Int i -> Printf.sprintf "Int(%d)" i
    | `Float f -> Printf.sprintf "Float(%f)" f
    | `Method m -> Printf.sprintf "Method(%s)" (string_of_sampler m)

let print_param_list params =
    let param_strs = List.map print_param_value params in
        Printf.sprintf "[%s]" (String.concat "; " param_strs)

let print_raw_product raw_product =
    Printf.printf "Raw Product (Total configurations: %d):\n"
      (List.length raw_product);
    List.iteri
      (fun i params ->
        Printf.printf "  Config %d: %s\n" (i + 1) (print_param_list params))
      raw_product

let print_judgementset s p =
    List.iter
      (fun (sign, (x, y)) ->
        if sign = 1 then Printf.printf "(%d, %d) " x y
        else Printf.printf "-(%d, %d) " x y)
      (List.combine s (pairs p))

let maj profile =
    let combinations = pairs (List.flatten @@ List.nth profile 0) in
    let tbl = Hashtbl.create (List.length combinations) in
        List.iter
          (fun (x, y) ->
            let count =
                List.fold_left
                  (fun count p ->
                    let ix =
                        List.find_index (fun ranking -> List.mem x ranking) p
                        |> Option.get
                    in
                    let iy =
                        List.find_index (fun ranking -> List.mem y ranking) p
                        |> Option.get
                    in
                        if ix < iy then count + 1 else count)
                  0 profile
            in
                Hashtbl.add tbl (x, y) count)
          combinations;
        tbl

let unique_preferences profiles =
    let rec aux unique orig =
        match orig with
        | [] -> unique
        | hd :: tl ->
            aux (if List.mem hd unique then unique else hd :: unique) tl
    in
        aux [] profiles

let profile_sub p a = List.filter (fun elem -> List.mem elem a) p
let wrap_in_list lst = List.map (fun x -> [ x ]) lst
let unique_length lst = unique lst |> List.length

let intersection s1 s2 =
    List.filter (fun elem -> List.mem elem s1 && List.mem elem s2) (s1 @ s2)
    |> List.sort_uniq Stdlib.compare

let union s1 s2 =
    List.filter (fun elem -> List.mem elem s1 || List.mem elem s2) (s1 @ s2)
    |> List.sort_uniq Stdlib.compare

let bottom p a =
    List.map (fun pref -> List.hd @@ List.rev @@ profile_sub pref a) p |> unique

let get_index e p = List.find_index (( = ) e) p |> Option.value ~default:0

let between l m r p =
    get_index l p < get_index m p && get_index m p < get_index r p

let pyList_toInt py_list =
    Py.List.to_list_map Py.Int.to_int py_list |> List.map (fun x -> [ x ])

let parse_tuple py_tuple =
    let int_list = Py.Tuple.get_item py_tuple 0 |> pyList_toInt in
    let float_val = Py.Tuple.get_item py_tuple 1 |> Py.Float.to_float in
        (int_list, float_val)

let extract_preferences voters = List.map (fun v -> v.preference) voters
let tupleToVoters (p, b) = { preference = p; bias = b; announced = 0 }

let parse_pyVoters pv =
    Py.List.to_list_map parse_tuple pv |> List.map tupleToVoters

let parse_yaml file =
    let ic = open_in file in
    let contents = In_channel.input_lines ic |> String.concat "\n" in

    match Yaml.of_string contents with
    | Ok v -> v
    | Error (`Msg m) -> failwith ("YAML parse error: " ^ m)
