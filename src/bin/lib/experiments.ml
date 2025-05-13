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

let load_data loc limit_n cond group q =
    let db = open_db loc in
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
    let where_condition cond_value group_value =
        join_where
          (condition_sub voter_info_tbl (voter_info `Condition) (comp `Equal)
             cond_value
          ::
          (match group_value with
          | Some v ->
              [
                condition_sub voter_info_tbl (voter_info `Group) (comp `Equal) v;
              ]
          | None -> []))
    in

    let get_data table join where lim =
        get_voters_opinions db q table join where lim |> Owl.Mat.of_arrays
    in

    let where = where_condition cond group in
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
    normalized such that the sum of the weights of all incoming edges in a node
    is exactly 1. *)

let run_deGroot_experiment ~pre_data ~post_data ~credibility_bool
    ~knowledge_bool ~meta_bool ~substantive_bool ~knowledge_scores ~graph
    ~num_voters ~num_candidates ~timesteps ~methd ~bias ~grouped ~sparse
    ~self_knowledge ~self_ego =
    let max_idx = min (Owl.Mat.row_num pre_data) (Owl.Mat.row_num post_data) in
    let indices = Owl.Stats.shuffle (Array.init max_idx Fun.id) in
    let num_voters = if not grouped then num_voters else max_idx in
    let voter_indices = Array.sub indices 0 num_voters in
    let pre_data = Owl.Mat.rows pre_data voter_indices in
    let post_data = Owl.Mat.rows post_data voter_indices in
    let knowledge_data = Owl.Mat.rows knowledge_scores voter_indices in
    let out_graph = ties_sampling graph num_voters in
    let voter_mapping =
        if sparse then
          WrappedModels.(
            align_voters_to_graph
              ~data1:
                (owl_to_np_NDArray
                   (opinion_to_dist pre_data (fun x y ->
                        Owl.Mat.(x - y |> sum'))))
              ~data2:
                (adjacency_to_distance
                   (owl_to_np_NDArray (adjacency_matrix_from out_graph)))
              ())
        else List.init num_voters Fun.id
    in
    let b = bijection (Array.of_list voter_mapping) in

    let knowledge_data =
        apply_bijection knowledge_data b |> Owl.Mat.transpose
    in
    let pre_data = apply_bijection pre_data b in
    let post_data = apply_bijection post_data b in

    let conf =
        {
          seed = None;
          pre_data;
          post_data;
          knowledge_data;
          knowledge_bool;
          credibility_bool;
          meta_bool;
          substantive_bool;
          self_knowledge;
          self_ego;
          graph = out_graph;
          n_voters = num_voters;
          n_candidates = num_candidates;
          timesteps;
          cand_method = methd;
          bias_factor = bias;
        }
    in
        deGroot conf

type job = {
  credibility_bool : bool;
  knowledge_bool : bool;
  meta_bool : bool;
  substantive_bool : bool;
  self_knowledge : bool;
  self_ego : bool;
  num_voters : int;
  num_candidates : int;
  time : timeRange;
  bias : float;
  methd : alternativeGenerators;
  trial_id : int;
}

let run_one_job ~pre_data ~post_data ~knowledge_scores ~graph ~evals ~grouped
    ~sparse (job : job) : string list list =
    (* unwrap the job *)
    let {
      credibility_bool;
      knowledge_bool;
      meta_bool;
      substantive_bool;
      self_knowledge;
      self_ego;
      num_voters;
      num_candidates;
      time;
      bias;
      methd;
      trial_id;
    } =
        job
    in
    let num_voters =
        if not grouped then num_voters else Owl.Mat.row_num pre_data
    in
    let out =
        run_deGroot_experiment ~pre_data ~post_data ~credibility_bool
          ~knowledge_bool ~meta_bool ~substantive_bool ~knowledge_scores ~graph
          ~num_voters ~num_candidates ~timesteps:time ~methd ~bias ~grouped
          ~sparse ~self_knowledge ~self_ego
    in
        List.mapi
          (fun j
               ( (sim_opinion, true_opinion),
                 (original_prof, sim_prof, true_prof, trust, trust_start) ) ->
            [
              string_of_float bias;
              string_of_sampler methd;
              string_of_int num_voters;
              string_of_int num_candidates;
              string_of_float (List.nth time j);
              string_of_int trial_id;
              string_of_bool sparse;
              string_of_bool grouped;
              string_of_bool credibility_bool;
              string_of_bool knowledge_bool;
              string_of_bool self_ego;
              string_of_bool meta_bool;
              string_of_bool substantive_bool;
            ]
            @ List.map
                (fun eval ->
                  eval original_prof sim_prof true_prof trust trust_start)
                evals)
          out

let run_parallel_simulations product pre_data post_data knowledge_scores graph
    n_trials evals grouped sparse =
    let jobs =
        product
        |> List.concat_map (function
             | [
                 `Bool cred;
                 `Bool knowledge;
                 `Bool meta;
                 `Bool substantive;
                 `Bool self_knowledge_bool;
                 `Bool self_ego;
                 `Int v;
                 `Int c;
                 `TimeRange timesteps;
                 `Float b;
                 `Method m;
               ] ->
                 List.init n_trials (fun t ->
                     {
                       credibility_bool = cred;
                       knowledge_bool = knowledge;
                       meta_bool = meta;
                       substantive_bool = substantive;
                       self_knowledge = self_knowledge_bool;
                       self_ego;
                       num_voters = v;
                       num_candidates = c;
                       time = timesteps;
                       bias = b;
                       methd = m;
                       trial_id = t + 1;
                     })
             | _ -> failwith "bad product")
    in
    let total = List.length jobs in
    let cores = Domain.recommended_domain_count () in

    Printf.printf "Running %d simulations in parallel, using %d cores\n" total
      cores;

    (* 2. Progress tracking *)
    let mutex = Mutex.create () in
    let completed = ref 0 in

    (* 3. Worker: plain function over a single job *)
    let worker job =
        let result =
            run_one_job ~pre_data ~post_data ~knowledge_scores ~graph ~evals
              ~grouped ~sparse job
        in
            result
    in

    (* 4. Fire up Parmap: one process per core, mapping over jobs *)
    let partials = Parmap.parmap ~ncores:cores worker (Parmap.L jobs) in

    Printf.printf "\nSimulations complete!\n%!";
    (* 5. Flatten your list of lists *)
    List.flatten partials

let run_and_write data_loc graph_loc file_out sparse group_bool condition
    n_trials product evaluations =
    let titles, evals = evaluations in
    let group =
        if group_bool then Some (string_of_int @@ Random.int 33) else None
    in
    let pre_data, post_data =
        load_data data_loc 10000 condition group questions_without_pk
    in
    let knowledge_data =
        fst @@ load_data data_loc 10000 condition group pk_score
    in

    let edges =
        if sparse then read_adjacency_matrix graph_loc
        else
          let voter_list = List.init 50 Fun.id in
              List.map
                (fun i -> List.map (fun j -> (i, j)) voter_list)
                voter_list
              |> List.flatten
    in
    let graph =
        List.fold_left
          (fun g (l, r) -> if l <> r then GenericGraph.add_edge g l r else g)
          GenericGraph.empty edges
    in

    let oc = open_out file_out in

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
          "sparse";
          "grouped";
          "credibility";
          "knowledge";
          "ego";
          "meta";
          "substantative";
        ]
        @ titles;
      ];

    let total = List.length product in
    let results =
        run_parallel_simulations product pre_data post_data knowledge_data graph
          n_trials evals group_bool sparse
    in

    Csv.output_all (Csv.to_channel oc) results;

    (* Close CSV file *)
    close_out oc;
    ()

let method_of_float f = if f > 0.5 then SampleVoters else Voter

let list_to_config = function
    | [
        knowledge_bool;
        credibility_bool;
        meta_bool;
        substantive_bool;
        self_knowledge_bool;
        self_ego;
        n_voters;
        n_candidates;
        timesteps;
        cand_method;
        bias_factors;
      ] ->
        [
          `Bool (knowledge_bool > 0.5);
          `Bool (credibility_bool > 0.5);
          `Bool (meta_bool > 0.5);
          `Bool (substantive_bool > 0.5);
          `Bool (self_knowledge_bool > 0.5);
          `Bool (self_ego > 0.5);
          `Int (int_of_float n_voters) |> filter_odd;
          `Int (int_of_float n_candidates);
          `TimeRange [ timesteps ];
          `Float bias_factors;
          `Method (method_of_float cand_method);
        ]
    | _ -> failwith "wrong number of elements"

let deGroot_experiment () =
    let ( file_out,
          graph_loc,
          data_loc,
          condition,
          sparse,
          group_bool,
          n_trials,
          product,
          get_evals ) =
        parse_yaml Sys.argv.(2) |> yaml_to_config_generator
    in

    let _ =
        run_and_write data_loc graph_loc file_out sparse group_bool condition
          n_trials product (get_evals ())
    in
        ()

let sensitivity_analysis () =
    let ( file_out,
          graph_loc,
          data_loc,
          condition,
          sparse,
          group_bool,
          n_trials,
          _,
          _ ) =
        parse_yaml Sys.argv.(2) |> yaml_to_config_generator
    in

    let pyParams = WrappedSensitivity.get_params (Py.Int.of_int n_trials) () in
    let product =
        (Py.List.to_list_map (Py.Tuple.to_list_map Py.Float.to_float)) pyParams
        |> List.map list_to_config
    in
    let _ =
        run_and_write data_loc graph_loc file_out sparse group_bool condition 1
          product
          (Evaluations.get_all_evals_sensitivity ())
    in
        ()

let test () =
    let t =
        Owl.Mat.of_arrays
          [| [| 0.; 1.; 1. |]; [| 1.; 0.; 0. |]; [| 0.; 1.; 0. |] |]
    in
    let ego_mat = add_ego_bias t in
        print_mat ego_mat;
        ()
