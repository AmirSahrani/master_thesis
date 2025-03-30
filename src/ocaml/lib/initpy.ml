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

module WrappedModels = struct
  module SpectralClustering = struct
    let create ?(n_clusters = 8) ?(affinity = "nearest_neighbors")
        ?(random_state = None) () =
      let py = Py.Import.import_module "sklearn.cluster" in
      let sc_class = Py.Module.get py "SpectralClustering" in
      let kwargs = Py.Dict.create () in
      Py.Dict.set_item kwargs
        (Py.String.of_string "n_clusters")
        (Py.Int.of_int n_clusters);
      Py.Dict.set_item kwargs
        (Py.String.of_string "affinity")
        (Py.String.of_string affinity);
      match random_state with
      | Some state ->
          Py.Dict.set_item kwargs
            (Py.String.of_string "random_state")
            (Py.Int.of_int state)
      | None ->
          ();
          Py.Object.call sc_class [||] kwargs

    let fit_predict ~x model =
      let np = Py.Import.import_module "numpy" in

      (* Convert Owl array to NumPy array *)
      let x_np =
        match Bigarray.Genarray.kind x with
        | Bigarray.Float32 -> Numpy.of_bigarray x
        | Bigarray.Float64 -> Numpy.of_bigarray x
        | _ -> failwith "Unsupported array type"
      in

      (* Call fit_predict on the model *)
      let fit_predict_method =
        Py.Object.get_attr_string_exn model "fit_predict"
      in
      let result = Py.Object.call_function fit_predict_method [| x_np |] in

      (* Convert result back to OCaml array *)
      let result_array = Py.Array.to_array Py.Int.to_int result in
      result_array
  end

  module TSNE = struct
    let create ?(n_components = 2) ?(perplexity = 30.0)
        ?(early_exaggeration = 12.0) ?(learning_rate = 200.0)
        ?(random_state = None) () =
      let py = Py.Import.import_module "sklearn.manifold" in
      let tsne_class = Py.Module.get py "TSNE" in
      let kwargs = Py.Dict.create () in
      Py.Dict.set_item kwargs
        (Py.String.of_string "n_components")
        (Py.Int.of_int n_components);
      Py.Dict.set_item kwargs
        (Py.String.of_string "perplexity")
        (Py.Float.of_float perplexity);
      Py.Dict.set_item kwargs
        (Py.String.of_string "early_exaggeration")
        (Py.Float.of_float early_exaggeration);
      Py.Dict.set_item kwargs
        (Py.String.of_string "learning_rate")
        (Py.Float.of_float learning_rate);
      match random_state with
      | Some state ->
          Py.Dict.set_item kwargs
            (Py.String.of_string "random_state")
            (Py.Int.of_int state)
      | None ->
          ();
          Py.Object.call tsne_class [||] kwargs

    let fit_transform ~x model =
      (* Convert Owl array to NumPy array *)
      let x_np =
        match Bigarray.Genarray.kind x with
        | Bigarray.Float32 -> Numpy.of_bigarray x
        | Bigarray.Float64 -> Numpy.of_bigarray x
        | _ -> failwith "Unsupported array type"
      in

      (* Call fit_transform on the model *)
      let fit_transform_method =
        Py.Object.get_attr_string_exn model "fit_transform"
      in
      let result = Py.Object.call_function fit_transform_method [| x_np |] in

      (* Convert result back to Owl array *)
      let result_array = Numpy.to_bigarray result in
      result_array
  end
end

(* Procrustes analysis module *)
let procrustes ~data1 ~data2 ?(scale = true) () =
  (* Initialize Python if not already initialized *)
  let scipy_spatial = Py.Import.import_module "scipy.spatial" in
  let procrustes_fn = Py.Module.get scipy_spatial "procrustes" in

  (* Convert Owl arrays to NumPy arrays *)
  let data1_np =
    match Bigarray.Genarray.kind data1 with
    | Bigarray.Float32 -> Numpy.of_bigarray data1
    | Bigarray.Float64 -> Numpy.of_bigarray data1
    | _ -> failwith "Unsupported array type"
  in

  let data2_np =
    match Bigarray.Genarray.kind data2 with
    | Bigarray.Float32 -> Numpy.of_bigarray data2
    | Bigarray.Float64 -> Numpy.of_bigarray data2
    | _ -> failwith "Unsupported array type"
  in

  (* Call procrustes function *)
  let kwargs = Py.Dict.create () in
  Py.Dict.set_item kwargs (Py.String.of_string "scale") (Py.Bool.of_bool scale);

  let result = Py.Object.call procrustes_fn [| data1_np; data2_np |] kwargs in

  (* Extract the results - Python returns a tuple of (mtx1, mtx2, disparity) *)
  let result_tuple = Py.Tuple.to_array result in
  let mtx1 = Numpy.to_bigarray result_tuple.(0) in
  let mtx2 = Numpy.to_bigarray result_tuple.(1) in
  let disparity = Py.Float.to_float result_tuple.(2) in

  (mtx1, mtx2, disparity)
