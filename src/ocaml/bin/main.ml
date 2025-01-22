open Deliberation_model.Model
open Deliberation_model.Utils

let testDeliberate voters =
  let out_voters = deliberate voters 1 csDistance csBetween in
  List.iter print_voter out_voters

let () =
  let _ = initPython () in
  (* Import your Python script *)
  let vg = Py.Import.import_module "voterGenerator" in

  (* Call the `run_analysis` function *)
  let open Pyops in
  let voters =
    vg.&("generateVoters")
      [| Py.Int.of_int 2; Py.Int.of_int 5; Py.Float.of_float 0.5 |]
    |> parse_pyVoters
  in

  testDeliberate voters;

  (* Finalize the Python interpreter *)
  Py.finalize ()
