open Deliberation_model.Model
open Deliberation_model.Utils
open Deliberation_model.Initpy

let testDeliberate voters =
  List.iter print_voter voters;
  print_endline "---------";
  let out_voters = deliberate voters 1 KS in
  List.iter print_voter out_voters

let () =
  (* let g = buildGraph [ 1; 2; 3 ] dpBetween in *)
  (* Dot.output_graph stdout g; *)
  let _ = initPython () in
  (* Import your Python script *)
  let vg = Py.Import.import_module "voterGenerator" in

  (* Call the `run_analysis` function *)
  let open Pyops in
  let voters =
    vg.&("generateVoters")
      [|
        Py.Int.of_int 10 (* number of voters *);
        Py.Int.of_int 5 (* number of alternatives*);
        Py.Float.of_float 0.9;
      |]
    |> parse_pyVoters
  in

  (* let voters =
       [
         { preference = [ 1; 2; 3 ]; bias = 0.5 };
         { preference = [ 2; 1; 3 ]; bias = 0.5 };
         { preference = [ 3; 2; 1 ]; bias = 0.5 };
       ]
     in *)
  testDeliberate voters;

  (* Finalize the Python interpreter *)
  Py.finalize ()
