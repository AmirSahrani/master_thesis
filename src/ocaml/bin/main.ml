open Deliberation_model.Model
open Deliberation_model.Utils
open Deliberation_model.Graphs
open Deliberation_model.Initpy
open Pyops

(* Function to run a single experiment *)
let run_experiment nVoters nAlternatives space trial bias nDeliberationsteps =
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
  let original_preferences = extract_preferences voters in
  let original_cyclic = is_cyclic original_preferences in
  let original_condorcet = has_condorcet original_preferences in
  let n_original_profiles = unique_preferences original_preferences in
  let outcome = deliberate voters nDeliberationsteps space in
  let preferences = extract_preferences outcome in
  let cyclic = is_cyclic preferences in
  let condorcet = has_condorcet preferences in
  let n_profiles = unique_preferences preferences in
  (* Convert to CSV row format *)
  [
    string_of_float bias;
    string_of_int trial;
    string_of_bool original_cyclic;
    string_of_bool cyclic;
    string_of_bool original_condorcet;
    string_of_bool condorcet;
    string_of_int n_original_profiles;
    string_of_int n_profiles;
    string_of_space space;
  ]

let param_grid nVoters nAlternatives spaces trials biases nDeliberationsteps =
  List.concat_map
    (fun space ->
      List.concat_map
        (fun trial ->
          List.map
            (fun bias ->
              run_experiment nVoters nAlternatives space trial bias
                nDeliberationsteps)
            biases)
        (List.init trials Fun.id)
      (* Generates trials as [0; 1; 2; ...] *))
    spaces

let main () =
  (* let all_preferences = permutations [ 1; 2; 3 ] in
  List.iter (fun l -> print_list l string_of_int) all_preferences;
  let d = dpDistance [ 1; 2; 3 ] in
  List.iter
    (fun pref ->
      List.iter
        (fun pref2 -> Printf.printf "%d " (d pref pref2 |> int_of_float))
        all_preferences;
      print_endline "")
    all_preferences; *)
  let _ = initPython () in
  let biases = arange 0.2 1.0 0.03 in
  let num_experiments = 10 in
  let nVoters = 51 in
  let nAlternatives = 3 in
  let nDeliberationSteps = 1 in

  print_endline "Starting experiments";

  (* Open CSV file *)
  let oc = open_out "results/data.csv" in

  (* Prepare header row *)
  Csv.output_all (Csv.to_channel oc)
    [
      [
        "bias";
        "trial";
        "cyclic_start";
        "cyclic_end";
        "condorcet_start";
        "condorcet_end";
        "unique_start";
        "unique_end";
        "metric_space";
      ];
    ];

  (* Run experiments *)
  let results =
    param_grid nVoters nAlternatives [ KS; CS ] num_experiments biases
      nDeliberationSteps
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
  try main () with
  | Not_found ->
      Printexc.print_backtrace stderr;
      prerr_endline "Caught Not_found!";
      exit 1
  | e ->
      Printf.eprintf "Uncaught exception: %s\n%s\n" (Printexc.to_string e)
        (Printexc.get_backtrace ());
      exit 1
