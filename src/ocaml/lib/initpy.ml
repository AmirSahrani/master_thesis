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
