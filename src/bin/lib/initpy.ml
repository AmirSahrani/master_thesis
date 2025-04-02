let initPython () =
  (* Dynamically determine the project root directory *)
  let project_root = Sys.getcwd () in
  let venv_path = Filename.concat project_root ".venv" in
  let python_scripts_dir = Filename.concat project_root "src/scripts" in
  print_endline project_root;
  print_endline venv_path;
  print_endline python_scripts_dir;

  (* Set the Python virtual environment variables *)
  Unix.putenv "VIRTUAL_ENV" venv_path;
  Unix.putenv "PATH" (venv_path ^ "/bin:" ^ Sys.getenv "PATH");

  (* Initialize the Python interpreter *)
  Py.initialize ~interpreter:(Filename.concat venv_path "bin/python") ();

  (* Add the directory containing your Python script to sys.path *)
  let _ = Py.Import.import_module "math" in
  print_endline "s6";
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

let _ = initPython ()

(* let owl_matrix_to_numpy (matrix : Owl.Mat.mat) : Py.Object.t =
   let rows, cols = Owl.Mat.shape matrix in

   (* Create a Python list of lists *)
   let np_array =
     Py.Array.numpy
     @@ Float.Array.map
          (fun i -> Py.Array.numpy @@ Owl.Mat.row matrix (int_of_float i))
          (Float.Array.init rows (fun x -> float_of_int x))
   in
   np_array *)
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

(* let np_to_owl matrix =
  let fArray = Py.Array.numpy_get_array matrix in
  let out =
    Owl.Mat.of_arrays
    @@ Float.Array.map_to_array (Float.Array.map_to_array Fun.id) fArray
  in
  out *)

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

  (* Common utility functions *)
  let predict data model = Py.Module.get_function vs "predict" [| data; model |]

  let transform data model =
    Py.Module.get_function vs "transform" [| data; model |]

  module SpectralClustering = struct
    let fit ~data ~n_clusters ~affinity () =
      Py.Module.get_function vs "fit_SpectralClustering"
        [| data; Py.Int.of_int n_clusters; Py.String.of_string affinity |]

    let fit_transform ~data ~n_clusters ~affinity () =
      let model = fit ~data ~n_clusters ~affinity () in
      transform data model
  end

  module TSNE = struct
    let create ?(n_components = 2) ?(perplexity = 30.0) () =
      (n_components, perplexity)

    let fit ~data ~n_components () =
      Py.Module.get_function vs "fit_TSNE"
        [| data; Py.Int.of_int n_components |]

    let fit_transform ~data ~n_components () =
      let model = fit ~data ~n_components () in
      transform data model
  end

  let procrustes ~data1 ~data2 () =
    let result = Py.Module.get_function vs "procrustes" [| data1; data2 |] in
    let mtx1 = Py.Tuple.get_item result 0 in
    let mtx2 = Py.Tuple.get_item result 1 in
    let disparity = Py.Tuple.get_item result 2 in
    (mtx1, mtx2, disparity)
end
