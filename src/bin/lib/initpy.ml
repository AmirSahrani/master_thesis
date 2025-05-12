let initPython () =
    print_endline "\nInitializing python bindings";
    (* Dynamically determine the project root directory *)
    let project_root = Sys.getcwd () in
    let venv_path = Filename.concat project_root ".venv" in
    let python_scripts_dir = Filename.concat project_root "src/scripts" in
        print_string "Project root directory:\t\t";
        print_endline project_root;
        print_string "Python interpreter location:\t";
        print_endline venv_path;
        print_string "Script location:\t\t";
        print_endline python_scripts_dir;
        print_endline "";

        (* Set the Python virtual environment variables *)
        Unix.putenv "VIRTUAL_ENV" venv_path;
        Unix.putenv "PATH" (venv_path ^ "/bin:" ^ Sys.getenv "PATH");

        (* Initialize the Python interpreter *)
        Py.initialize ~interpreter:(Filename.concat venv_path "bin/python") ();

        (* Add the directory containing your Python script to sys.path *)
        let sys = Py.Import.import_module "sys" in

        match Py.Object.get_attr_string sys "path" with
        | Some sys_path -> (
            match Py.Object.get_attr_string sys_path "append" with
            | Some append_method ->
                let directory = Py.String.of_string python_scripts_dir in
                    Py.Object.call append_method
                      (Py.Tuple.of_array [| directory |])
                      (* Arguments as a tuple *)
                      Py.null
            | None -> failwith "Error: sys.path.append method not found")
        | None -> failwith "Error: sys.path not found"

let _ = initPython ()

let owl_to_np_NDArray matrix =
    let arrays = Owl.Mat.to_arrays matrix in
    (* Convert 2D array to Python-compatible array *)
    let py_arr =
        Py.List.of_array_map (Py.List.of_array_map Py.Float.of_float) arrays
    in
    (* Convert to NumPy array *)
    let np = Py.import "numpy" in
    let out =
        Py.Object.call
          (Py.Object.get_attr_string np "array" |> Option.get)
          (Py.Tuple.of_array [| py_arr |])
          Py.null
    in
        out

let profile_to_np_NDArray (prof : Utils.profile) =
    let arrays =
        List.map
          (fun v ->
            List.map
              (fun x ->
                match x with
                | x :: [] -> x
                | _ -> failwith "Only strict preferences allowed")
              v)
          prof
    in
    (* Convert 2D array to Python-compatible array *)
    let py_arr =
        Py.List.of_list_map (Py.List.of_list_map Py.Int.of_int) arrays
    in
    (* Convert to NumPy array *)
    let np = Py.import "numpy" in
    let out =
        Py.Object.call
          (Py.Object.get_attr_string np "array" |> Option.get)
          (Py.Tuple.of_array [| py_arr |])
          Py.null
    in
        out

(** Module wrapper for python script containing sklearn models needed.
    ```voter_stats_models``` contains the following functions: predict function,
    given a model and data set, this will predict the labels of the data
    transform function, given a model and a data set, this will transform the
    data into the components of that model procrustes function, given two
    dataset, x1 and x2, it will return x1 and x2' where x2' is rotated to best
    correspond to x1 using the procrustes method fit_TSNE function, given a
    dataset, a number of components, it will fit a model labelling the data
    according to the number of components fit_SpectralClustering function, given
    a dataset, a number of clusters, and an affinity method, this will cluster
    the data into the number of specified clusters. *)

module WrappedModels = struct
  let vs = Py.Import.import_module "voter_stats_models"

  let align_voters_to_graph ~data1 ~data2 () =
      let result =
          Py.Module.get_function vs "map_voters_to_nodes_on_graph"
            [| data1; data2 |]
      in
      let order = Utils.pyList_toInt result |> List.flatten in
          order

  let adjacency_to_distance data1 =
      let result =
          Py.Module.get_function vs "adjancy_to_distance" [| data1 |]
      in
          result
end

module WrappedEvals = struct
  let ev = Py.Import.import_module "election_evals"

  let k_alternative_deletion profile =
      Py.Module.get_function ev "get_max_num_cands_for_single_peakedness"
        [| profile |]

  let k_voter_deletion profile =
      Py.Module.get_function ev "get_max_num_voters_for_single_peakedness"
        [| profile |]
end

module WrappedSensitivity = struct
  let sa = Py.Import.import_module "sensitivity_analysis"
  let get_params () = Py.Module.get_function sa "get_analysis_inputs" [||]

  let analyse problem values =
      Py.Module.get_function sa "run_analysis" [| problem; values |]
end
