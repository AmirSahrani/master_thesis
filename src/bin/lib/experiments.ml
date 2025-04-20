(* open ABMmodel *)
open DeGrootmodel
open Radmodel
open Utils
open Evaluations
open Query
open Initpy
open Distances
open Graphs
open Pyops

[@@@ocaml.warning "-26-27"]

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
                    run_experiment_rad nVoters nAlternatives space distance
                      between trial bias nDeliberationsteps evals)
                  biases)
              (List.init trials Fun.id))
      spaces

let rad_roy_bias_experiment () =
    (* Duct tape fix*)
    let _ = WrappedModels.SpectralClustering.fit in
    let biases = arange 0.45 0.99 0.01 in
    let num_experiments = 100 in
    let nVoters = 51 in
    let nAlternatives = 3 in
    let nDeliberationSteps = 2 in
    (* Open CSV file *)
    let oc = open_out "results/data_cnsus.csv" in
    let titles, evals = get_all_evals_rad () in

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

let load_data limit_n cond q =
    let db = open_db "data/a1r.db" in
    let table_pre = tables `Response_pre in
    let table_post = tables `Response_post in
    let voter_info_tbl = tables `Voter_info in

    let join_table table = inner_join voter_info_tbl table (voter_info `ID) in
    let join_pre, join_post = (join_table table_pre, join_table table_post) in
    let join_pk_pre =
        inner_join (tables `Response_pk) table_pre (voter_info `ID)
    in
    let join_pk_post =
        inner_join (tables `Response_pk) table_post (voter_info `ID)
    in
    let limit_str = limit limit_n in
    let where_condition cond_value =
        join_where
          [
            condition_sub voter_info_tbl (voter_info `Condition) (comp `Equal)
              cond_value;
          ]
    in

    let get_data table join where lim =
        get_voters_opinions db q table join where lim |> Owl.Mat.of_arrays
    in

    let where = where_condition cond in
        ( get_data table_pre [ join_pre; join_pk_pre ] where limit_str,
          get_data table_post [ join_post; join_pk_post ] where limit_str )

(** [deGroot_experiment] samples a graph of academic papers using the TIES ()
    algorithm according to the number of data points provided. The graph is then
    embedded using LaPlacian eigenmaps into the same N-dimnsional space as the
    data. Finally Procrustes analysis is used to align then nodes on the graph
    with the data points.

    Once the voters have been place on the graph, a final opinion matrix is
    generated by left multiplying the matrix of opnions with a Trust matrix.
    This trust matrix is based on the graph, but has added noise and is
    normailzed such that the sum of the weights of all incoming edges in a node
    is exactly 1. *)

let run_deGroot_experiment pre_data post_data graph num_voters num_candidates
    time methd bias =
    let max_idx = min (Owl.Mat.row_num pre_data) (Owl.Mat.row_num post_data) in
    let indices = Owl.Stats.shuffle (Array.init max_idx Fun.id) in
    let voter_indices = Array.sub indices 0 num_voters in
    let pre_data = Owl.Mat.rows pre_data voter_indices in
    let post_data = Owl.Mat.rows post_data voter_indices in
    let out_graph = ties_sampling graph num_voters in
    let voter_mapping =
        WrappedModels.(
          align_voters_to_graph
            ~data1:
              (owl_to_np_NDArray
                 (opinion_to_dist pre_data (fun x y -> Owl.Mat.(x - y |> sum'))))
            ~data2:
              (adjacency_to_distance
                 (owl_to_np_NDArray (adjacency_matrix_from out_graph)))
            ())
    in
    let conf =
        {
          seed = None;
          pre_data;
          post_data;
          graph = out_graph;
          (* parameters to experiment with*)
          n_voters = num_voters;
          n_candidates = num_candidates;
          timesteps = time;
          cand_method = methd;
          bias_factor = bias;
        }
    in
        deGroot conf

let deGroot_experiment () =
    let titles, evals = get_all_evals_degroot () in

    let oc = open_out "results/data_degroot_mapping_delib.csv" in

    (* Prepare header row *)
    Csv.output_all (Csv.to_channel oc)
      [
        [
          "bias";
          "cand_sampler";
          "n_voters";
          "n_candidates";
          "time_steps";
          "trial";
        ]
        @ titles;
      ];

    let pre_delib, post_delib = load_data 100000 "1" questions_without_pk in
    let edges = read_adjacency_matrix "graphs/soc-astro.edges" in
    let graph =
        List.fold_left
          (fun g (l, r) -> GenericGraph.add_edge g l r)
          GenericGraph.empty edges
    in

    let num_voters_range =
        List.init 4 (fun i -> 9 + (i * 2)) |> List.map (fun x -> [ `Int x ])
    in
    let num_candidates_range =
        List.init 3 (fun i -> 5 + (i * 2)) |> List.map (fun x -> [ `Int x ])
    in
    let bias_range = arange 0.1 1.5 0.1 |> List.map (fun x -> [ `Float x ]) in
    let cand_methds =
        [ Random; SampleVoters; Voter ] |> List.map (fun x -> [ `Method x ])
    in
    let timesteps_range =
        [ 1.; 5.; 10.; 50. ] |> List.map (fun x -> [ `Float x ])
    in
    let product =
        cartesian_product num_candidates_range num_voters_range
        |> cartesian_product bias_range
        |> cartesian_product cand_methds
        |> cartesian_product timesteps_range
    in
    let total = List.length product in

    Printf.printf "Running %d simulations\n" total;
    let n_trials = 3 in
    let results =
        List.mapi
          (fun i c ->
            match c with
            | [
             `Int voters;
             `Int candidates;
             `Float bias;
             `Method meth;
             `Float steps;
            ] ->
                Printf.printf "\027[2K\r%.2f%% done%!%!"
                  (float_of_int i /. float_of_int total *. 100.);
                List.map
                  (fun i ->
                    let ( (sim_opinion, true_opinion),
                          (original_prof, sim_prof, true_prof) ) =
                        run_deGroot_experiment pre_delib post_delib graph voters
                          candidates steps meth bias
                    in
                        [
                          string_of_float bias;
                          string_of_sampler meth;
                          string_of_int voters;
                          string_of_int candidates;
                          string_of_float steps;
                          string_of_int i;
                        ]
                        @ List.map (fun eval -> eval original_prof ()) evals
                        @ List.map (fun eval -> eval sim_prof ()) evals
                        @ List.map (fun eval -> eval true_prof ()) evals)
                  (List.init n_trials (fun x -> x + 1))
            | _ -> failwith "Unexpected pattern")
          product
        |> List.concat
    in

    Csv.output_all (Csv.to_channel oc) results;

    (* Close CSV file *)
    close_out oc;
    ()

let test () =
    let d =
        Owl.Mat.of_arrays
          [| [| 1.; 2.; 3. |]; [| 0.; 0.; 0. |]; [| 0.; 0.; 0. |] |]
    in
    let s =
        Owl.Mat.of_arrays
          [| [| 0.; 0.; 0. |]; [| 0.; 0.; 0. |]; [| 1.; 1.; 1. |] |]
    in

    print_mat d;
    print_mat s;

    let f = greedy_mapping d s in

    print_mat (apply_bijection d f);

    let perm =
        WrappedModels.align_voters_to_graph ~data1:(owl_to_np_NDArray d)
          ~data2:(owl_to_np_NDArray s) ()
    in
        print_list perm string_of_int;
        print_mat (apply_bijection d (bijection (Array.of_list perm)));

        ()
