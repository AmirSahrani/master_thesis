open Deliberation_model.Model
open Deliberation_model.Utils
open Deliberation_model.Evaluations
open Deliberation_model.Distances
open Deliberation_model.Initpy
open Pyops

(* Function to run a single experiment *)
let run_experiment nVoters nAlternatives space distance between trial bias
    nDeliberationsteps evals =
  let vg = Py.Import.import_module "voterGenerator" in
  (* Generate fresh voter set for each bias in every trial *)
  let voters =
    vg.&("generateVoters")
      [|
        Py.Int.of_int nVoters;
        Py.Int.of_int nAlternatives;
        Py.Float.of_float bias;
      |]
    |> parse_pyVoters
  in
  (* Process the voters *)
  let original_profile = extract_preferences voters in
  let outcome = deliberate voters nDeliberationsteps distance between in
  let updated_profile = extract_preferences outcome in

  (* Convert to CSV row format *)
  [ string_of_float bias; string_of_int trial; string_of_space space ]
  @ List.map (fun eval -> eval original_profile distance) evals
  @ List.map (fun eval -> eval updated_profile distance) evals

let param_grid nVoters nAlternatives spaces trials biases nDeliberationsteps
    evals =
  List.concat_map
    (fun space ->
      let p = List.init nAlternatives (fun x -> [ x + 1 ]) in
      let distance, between =
        match space with
        | KS ->
            print_endline "Testing KS";
            (ksDistance, ksBetween)
        | CS ->
            print_endline "Testing DP";
            (csDistance, csBetween)
        | DP ->
            print_endline "Testing CS";
            (dpDistance p, dpBetween)
      in
      let distance = distance in
      let distTbl = Hashtbl.create (List.length p * List.length p) in
      let profiles = all_profiles_weak p in

      List.iter
        (fun p ->
          List.iter
            (fun p' ->
              if not (Hashtbl.mem distTbl (p', p)) then (
                let d = distance p p' in
                Hashtbl.add distTbl (p, p') d;
                Hashtbl.add distTbl (p', p) d)
              else ())
            profiles)
        profiles;
      let distance p p' = Hashtbl.find distTbl (p, p') in
      List.concat_map
        (fun trial ->
          List.map
            (fun bias ->
              run_experiment nVoters nAlternatives space distance between trial
                bias nDeliberationsteps evals)
            biases)
        (List.init trials Fun.id))
    spaces

let main () =
  let _ = initPython () in
  let biases = arange 0.45 0.99 0.01 in
  let num_experiments = 1000 in
  let nVoters = 51 in
  let nAlternatives = 3 in
  let nDeliberationSteps = 5 in
  (* Open CSV file *)
  let oc = open_out "results/data___.csv" in
  let titles, evals = get_all_evals () in

  (* Prepare header row *)
  Csv.output_all (Csv.to_channel oc)
    [ [ "bias"; "trial"; "metric_space" ] @ titles ];

  (* Run experiments *)
  let results =
    param_grid nVoters nAlternatives [ KS; DP; CS ] num_experiments biases
      nDeliberationSteps evals
  in

  (* Write results *)
  Csv.output_all (Csv.to_channel oc) results;

  (* Close CSV file *)
  close_out oc;

  print_endline "Finished experiments";

  (* Finalize Python *)
  Py.finalize ()

let () =
  Printexc.record_backtrace true;
  try
    main ();
    let _ = Sys.command "notify-send 'Simulations finished'" in
    ()
  with
  | Not_found ->
      Printexc.print_backtrace stderr;
      prerr_endline "Caught Not_found!";
      exit 1
  | e ->
      Printf.eprintf "Uncaught exception: %s\n%s\n" (Printexc.to_string e)
        (Printexc.get_backtrace ());
      exit 1
