open Deliberation_model.Model
open Deliberation_model.Utils
open Deliberation_model.Graphs
open Deliberation_model.Initpy

let () =
  let _ = initPython () in
  let open Pyops in
  let vg = Py.Import.import_module "voterGenerator" in

  let biases = arange 0.0 1.0 0.03 in
  let num_experiments = 100 in
  let nVoters = 51 in
  let nAlternatives = 3 in

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
        "unique_start";
        "unique_end";
      ];
    ];

  (* Run experiments *)
  let results =
    List.flatten
      (List.init num_experiments (fun trial ->
           List.map
             (fun bias ->
               (* Generate fresh voter set for each bias in every trial *)
               let voters =
                 vg.&("generateVoters")
                   [|
                     Py.Int.of_int nVoters;
                     (* number of voters *)
                     Py.Int.of_int nAlternatives;
                     (* number of alternatives *)
                     Py.Float.of_float bias;
                   |]
                 |> parse_pyVoters
               in
               (* Process the voters *)
               let original_preferences = extract_preferences voters in
               let original_cylic = is_cyclic original_preferences in
               let n_original_profiles =
                 unique_preferences original_preferences
               in
               let outcome = deliberate voters 2 DP in
               let preferences = extract_preferences outcome in
               let cyclic = is_cyclic preferences in
               let n_profiles = unique_preferences preferences in
               (* Convert to CSV row format *)
               [
                 string_of_float bias;
                 string_of_int trial;
                 (if original_cylic then "1" else "0");
                 (if cyclic then "1" else "0");
                 string_of_int n_original_profiles;
                 string_of_int n_profiles;
               ])
             biases))
  in

  (* Write results *)
  Csv.output_all (Csv.to_channel oc) results;

  (* Close CSV file *)
  close_out oc;

  print_endline "Finished experiments";

  (* Finalize Python *)
  Py.finalize ()
