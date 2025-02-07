open Deliberation_model.Model
open Deliberation_model.Utils
open Deliberation_model.Graphs
open Deliberation_model.Distances
open Deliberation_model.Initpy
open Pyops

(* Function to run a single experiment *)
let run_experiment nVoters nAlternatives space distance between trial bias
    nDeliberationsteps =
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
  let outcome = deliberate voters nDeliberationsteps distance between in
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
      let p = List.init nAlternatives (fun x -> [ x + 1 ]) in
      let distance, between =
        match space with
        | KS ->
            print_endline "Testing KS";
            (ksDistance, ksBetween)
        | DP ->
            print_endline "Testing DP";
            (csDistance, csBetween)
        | CS ->
            print_endline "Testing CS";
            (dpDistance p, dpBetween)
      in
      let distTbl = Hashtbl.create (List.length p * List.length p) in
      let profiles = all_profiles p in

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
                bias nDeliberationsteps)
            biases)
        (List.init trials Fun.id))
    spaces

let main () =
  let _ = initPython () in
  let biases = arange 0.4 0.99 0.03 in
  let num_experiments = 100 in
  let nVoters = 51 in
  let nAlternatives = 3 in
  let nDeliberationSteps = 5 in
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
    param_grid nVoters nAlternatives [ KS; DP; CS ] num_experiments biases
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
  let prefs =
    [
      [ [ 3 ]; [ 1 ]; [ 2 ] ]; [ [ 2 ]; [ 1 ]; [ 3 ] ]; [ [ 2 ]; [ 3 ]; [ 1 ] ];
    ]
  in
  let biases = [ 0.77; 0.77; 0.77 ] in
  (* let biases = [ 0.5; 0.5; 0.5 ] in *)
  let voters =
    List.map2
      (fun p b -> { preference = p; bias = b; announced = false })
      prefs biases
  in
  print_endline "----------------";
  List.iter print_voter voters;
  let updated = deliberate voters 4 ksDistance ksBetween in
  print_endline "----------------";
  List.iter print_voter updated;
  print_endline "----------------";

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
