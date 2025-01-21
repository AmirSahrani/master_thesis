open Deliberation_model.Model

let testDeliberate =
  let voters =
    [
      { preference = [ 3; 2; 1 ]; bias = 0.3 };
      { preference = [ 1; 2; 3 ]; bias = 0.5 };
      { preference = [ 2; 3; 1 ]; bias = 0.8 };
    ]
  in
  let out_voters = deliberate voters 1 csDistance in
  List.iter print_voter out_voters

let () = testDeliberate
(* let () =
   (* Dynamically determine the project root directory *)
   let current_dir = Sys.getcwd () in
   let project_root = Filename.dirname (Filename.dirname current_dir) in
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

   let _ =
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
   in

   (* Import your Python script *)
   let analysis_module = Py.Import.import_module "sensitivity_analysis" in

   (* Call the `run_analysis` function *)
   let result =
     Py.Module.get_function analysis_module "run_analysis" [||]
     |> Py.Dict.to_bindings_string
   in

   (* Process and display the results *)
   List.iter
     (fun (key, value) ->
       Printf.printf "%s: %s\n" key (Py.Object.to_string value))
     result;

   (* Finalize the Python interpreter *)
   Py.finalize ();
   Printf.printf "Python interpreter finalized.\n%!" *)
