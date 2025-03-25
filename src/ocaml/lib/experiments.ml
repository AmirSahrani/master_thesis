open Model
open Utils
open Evaluations
open Query
open Distances
open Graphs
open Initpy
open Pyops

(* Function to run a single experiment *)
let run_experiment_rad nVoters nAlternatives space distance between trial bias
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
        | DP ->
            print_endline "Testing DP";
            (dpDistance p, dpBetween)
        | CS ->
            print_endline "Testing CS";
            (csDistance, csBetween)
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
              run_experiment_rad nVoters nAlternatives space distance between
                trial bias nDeliberationsteps evals)
            biases)
        (List.init trials Fun.id))
    spaces

let rad_roy_bias_experiment () =
  let _ = initPython () in
  let biases = arange 0.45 0.99 0.01 in
  let num_experiments = 100 in
  let nVoters = 51 in
  let nAlternatives = 3 in
  let nDeliberationSteps = 2 in
  (* Open CSV file *)
  let oc = open_out "results/data_consensus.csv" in
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

  (* Finalize Python *)
  Py.finalize ()

let deGroot_experiment () =
  let edges = read_adjacency_matrix "graphs/soc-academia.edges" in
  let graph =
    List.fold_left
      (fun g (l, r) -> GenericGraph.add_edge g l r)
      GenericGraph.empty edges
  in
  let out_file = "graphs/soc-academia_test.edges" in
  let out_file_weighted = "graphs/soc-academia_sampled_weighted.edges" in
  let out_graph = ties_sampling graph 50 in
  (* let db = open_db "data/a1r.db" in
  let opinions = () in *)
  write_adjacency_matrix out_graph out_file;
  let trust_matrix = out_graph |> adjacency_matrix_from in
  let trust_matrix =
    add_self_bias trust_matrix 3.0 |> randomize_matrix |> normalize_matrix
  in
  let final_trust, _ = deGroot trust_matrix 10. in
  (* print_mat final_trust; *)
  save_matrix_adjacency final_trust out_file_weighted

let test () =
  let db = open_db "data/a1r.db" in
  let columns = questions in
  let table = tables `Response_pre in
  let join = inner_join (tables `Voter_info) table (voter_info `ID) in
  let where =
    join_where
      [
        condition_sub (tables `Voter_info) (voter_info `Condition) (comp `Equal)
          "1";
      ]
  in
  let opinions = get_voters_opinions db columns table join where in
  let opinion_matrix = Owl.Mat.of_arrays opinions in
  print_mat opinion_matrix
