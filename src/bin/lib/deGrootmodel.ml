open Utils
open Graphs

type rangeParameterFloat = { start : float; stop : float; step : float }
[@@deriving of_yaml]

type rangeParameterInt = { istart : int; istop : int; istep : int }
[@@deriving of_yaml]

type config = {
  pre_data : Owl.Mat.mat;
  post_data : Owl.Mat.mat;
  graph : GenericGraph.t;
  timesteps : float list;
  n_voters : int;
  n_candidates : int;
  bias_factor : float;
  cand_method : alternativeGenerators;
  seed : int Option.t;
}

type degroot_yaml = {
  file_out : string;
  data_loc : string;
  graph : string;
  condition : string;
  cand_method : string list;
  n_trials : int;
  n_voters : rangeParameterInt;
  n_candidates : rangeParameterInt;
  timesteps : rangeParameterFloat;
  bias : rangeParameterFloat;
}
[@@deriving of_yaml]

let yaml_to_config_generator yaml_value =
    let res =
        match degroot_yaml_of_yaml yaml_value with
        | Ok r -> r
        | Error _ -> failwith "could not parse file"
    in
    let params_ranges_float = [ res.bias ] in
    let params_ranges_int = [ res.n_voters; res.n_candidates ] in

    let all_params_float =
        List.map (fun x -> arange x.start x.stop x.step) params_ranges_float
        |> List.map (fun x -> `Float x)
    in
    let all_params_int =
        List.map (fun x -> range x.istart x.istop x.istep) params_ranges_int
        |> List.map (fun x -> `Int x)
    in
    let all_params_method =
        [
          `Method
            (List.map (fun x -> alternativeGenerator_of x) res.cand_method);
        ]
    in
    let all_params = all_params_int @ all_params_float @ all_params_method in

    let all_params_processed =
        List.rev_map
          (function
            | `Int values -> List.map (fun v -> `Int v) values
            | `Float values -> List.map (fun v -> `Float v) values
            | `Method values -> List.map (fun v -> `Method v) values)
          all_params
    in

    let raw_product =
        List.fold_left
          (fun acc lst ->
            List.concat_map (fun x -> List.map (fun y -> y :: x) lst) acc)
          [ [] ] all_params_processed
    in

    ( ( res.file_out,
        res.graph,
        res.data_loc,
        res.condition,
        res.n_trials,
        (fun x -> arange x.start x.stop x.step) res.timesteps ),
      raw_product )

let normalize_matrix adjacency_matrix =
    let row_sums = Owl.Mat.sum_cols adjacency_matrix in
    let row_sums_fix =
        Owl.Mat.map (fun sum -> if sum <> 0. then sum else 1.) row_sums
    in
        Owl.Mat.div adjacency_matrix row_sums_fix

(** Add bias to a voter, bias is defined in terms of a factor x, where voter
    having bias x means they weight their opinion x times more than that of the
    other voters together. In the case that x = 1, the voter values their
    opinion equally to that of all their neighbors. *)
let add_self_bias adjacency_matrix factor =
    let rows = Owl.Mat.row_num adjacency_matrix in
        for i = 0 to rows - 1 do
          Owl.Mat.set adjacency_matrix i i
            (Owl.Mat.(sum' @@ row adjacency_matrix i) *. factor)
        done;
        adjacency_matrix

let randomize_matrix adjcency_matrix _ =
    let open Owl.Mat in
    let rows, col = Owl.Mat.shape adjcency_matrix in
    let random_mat = Owl.Mat.uniform ~a:1. ~b:10. rows col in
    let out_mat = random_mat * adjcency_matrix in
        out_mat

(** Edge weights equal to the credibility of each voter, where credibility is
    defined as the number of out going edges from voter i*)
let credibility_matrix adjcency_matrix _ =
    let rows, cols = Owl.Mat.shape adjcency_matrix in
    let out_mat = Owl.Mat.empty rows cols in
        for i = 0 to rows - 1 do
          for j = 0 to cols - 1 do
            let credibility = Owl.Mat.sum' @@ Owl.Mat.col adjcency_matrix j in
                if Owl.Mat.get adjcency_matrix i j <> 0. && i <> j then (
                  Owl.Mat.set out_mat i j credibility;
                  Owl.Mat.set out_mat j i credibility)
          done
        done;
        out_mat

let abs_diff_sum mat mat2 = Owl.Mat.(mat - mat2) |> Owl.Mat.abs |> Owl.Mat.sum'

let apply_bijection matrix mapping =
    let n, _ = Owl.Mat.shape matrix in
    let out = Owl.Mat.empty n n in
        for i = 0 to n - 1 do
          for j = 0 to n - 1 do
            let new_i = mapping i in
            let new_j = mapping j in
            let new_val = Owl.Mat.get matrix i j in
                Owl.Mat.set out new_i new_j new_val
          done
        done;
        out

let bijection m x = m.(x)

let greedy_mapping dist_matrix shortest_path_matrix =
    let n = Owl.Mat.row_num dist_matrix in
    (* 0…n−1 are all initially available *)
    let available = Array.init n (fun i -> i) in
    let nodes = Array.init n (fun i -> i) in
    let mapping = Array.init n (fun i -> i) in

    (* start identity *)
    Array.iter
      (fun node ->
        (* pick best unused voter for this node *)
        let best =
            Array.fold_left
              (fun acc voter ->
                if voter = -1 then acc
                else
                  let potential = Array.copy mapping in
                      Array.set potential node voter;
                      let new_dist =
                          apply_bijection dist_matrix (bijection potential)
                      in
                      let score = abs_diff_sum new_dist shortest_path_matrix in
                          match acc with
                          | None -> Some (voter, score)
                          | Some (_, s) when score < s -> Some (voter, score)
                          | _ -> acc)
              None available
        in
            match best with
            | None -> failwith ("no voter left for node " ^ string_of_int node)
            | Some (v, _) ->
                (* record the assignment, mark voter used *)
                mapping.(node) <- v;
                available.(v) <- -1)
      nodes;

    bijection mapping

(** Edge weights equal to the similarity between voter i and j if they have a
    link in *)
let similarity_matrix adjcency_matrix distance_matrix =
    let rows, cols = Owl.Mat.shape adjcency_matrix in
    let out_mat = Owl.Mat.empty rows cols in
        for i = 0 to rows - 1 do
          for j = 0 to cols - 1 do
            let distance = Owl.Mat.get distance_matrix i j in
                if Owl.Mat.get adjcency_matrix i j <> 0. && i <> j then (
                  Owl.Mat.set out_mat i j distance;
                  Owl.Mat.set out_mat j i distance)
          done
        done;
        out_mat

let opinion_to_dist matrix norm =
    let n, _m = Owl.Mat.shape matrix in
    let dist = Owl.Mat.empty n n in
        for i = 0 to n - 1 do
          for j = 0 to i do
            let row_i = Owl.Mat.row matrix i in
            let row_j = Owl.Mat.row matrix j in
            let distance = norm row_i row_j in
                Owl.Mat.set dist i j distance;
                Owl.Mat.set dist j i distance
          done
        done;
        dist

(** Generate an alternative based on the opinions of the voter, opinions
    represented in [matrix] of shape n_voters x n_opion_items.

    Options: Random -> Generate a random alternative, highest and lowest support
    values hard coded at 0 and 9 Voter -> Sample a random voter to be a
    candidate SampleVoters -> Sample [sample_size] voters, candidate become mean
    of the opinions *)
let gen_alterantive methd matrix sample_size =
    let rows, cols = Owl.Mat.shape matrix in
        match methd with
        | Random ->
            let random_mat = Owl.Mat.uniform ~a:(-0.5) ~b:9.5 1 cols in
                Owl.Mat.round_ random_mat;
                random_mat
        | Voter ->
            let voter = Random.int rows in
                Owl.Mat.row matrix voter
        | SampleVoters ->
            let voters = Array.init sample_size (fun _ -> Random.int rows) in
            let opinions = Owl.Mat.rows matrix voters in
            let cand = Owl.Mat.mean ~axis:0 opinions in
                Owl.Mat.round_ cand;
                cand

let opinion_to_pref pref candidates =
    let distances =
        List.map (fun cand -> Owl.Mat.(sum' @@ abs (pref - cand))) candidates
    in
        candidates
        |> List.mapi (fun i _ -> (i + 1, List.nth distances i))
        |> List.sort (fun (_, d1) (_, d2) -> compare d1 d2)
        |> List.map (fun tup -> [ fst tup ])

let create_trust_matrix graph bias_factor =
    let trust_matrix = graph |> adjacency_matrix_from in
        trust_matrix
        |> (fun m -> credibility_matrix m ())
        |> (fun m -> add_self_bias m bias_factor)
        |> normalize_matrix

(** [deGroot] takes in a configuration to simulate a deGroot learning process on
    the supplied input data, return the simulated final opinions, as well as the
    supplied true final options, and the trust matrix *)
let deGroot config =
    let {
      pre_data;
      post_data;
      graph;
      n_voters;
      timesteps;
      n_candidates;
      cand_method;
      bias_factor;
      seed;
    } =
        config
    in

    (match seed with None -> () | Some s -> Random.init s);
    let n_policies = Owl.Mat.col_num pre_data in

    (* First we create the proper trust matrix*)
    let trust_matrix = create_trust_matrix graph bias_factor in

    (* list of mat *)
    let candidates =
        List.init n_candidates (fun _ ->
            gen_alterantive cand_method pre_data n_candidates)
    in

    let candidates_arr =
        Array.of_list candidates
        |> Owl.Dense.Ndarray.D.stack ~axis:0
        |> Owl.Arr.squeeze ~axis:[| 1 |]
    in

    let v_candidates_arr = Array.init n_voters (fun _ -> candidates_arr) in
    let cand_ndarray = Owl.Dense.Ndarray.D.stack ~axis:0 v_candidates_arr in
    let shpe = Owl.Dense.Ndarray.D.shape cand_ndarray in
    let cand_noisy =
        Owl.Dense.Ndarray.D.(cand_ndarray + gaussian ~mu:0. ~sigma:2. shpe)
    in
        (* let estimated_candidates = *)
        List.map
          (fun t ->
            let trust = Owl.Mat.(trust_matrix **@ t) in
            let final_opinion = Owl.Mat.(trust *@ pre_data) in
            let cand_noisy_2d =
                Owl.Arr.reshape cand_noisy
                  [| n_voters; n_candidates * n_policies |]
            in

            (* Perform matrix multiplication *)
            let final_est_2d = Owl.Mat.(trust *@ cand_noisy_2d) in

            (* Reshape the result back to 3D *)
            let final_est =
                Owl.Arr.reshape final_est_2d
                  [| n_voters; n_candidates; n_policies |]
            in

            let extract_candidate_policies voter_idx tens =
                (* Try a simpler approach - reshape the data *)
                let n_candidates = (Owl.Arr.shape tens).(1) in
                let n_policies = (Owl.Arr.shape tens).(2) in

                (* Extract each candidate's policy row one by one *)
                Array.init n_candidates (fun c_idx ->
                    let policy_array =
                        Array.init n_policies (fun p_idx ->
                            Owl.Arr.get cand_noisy [| voter_idx; c_idx; p_idx |])
                    in
                        Owl.Mat.of_array policy_array 1 n_policies)
                |> Array.to_list
            in
            let original_prefs =
                List.mapi
                  (fun _ i ->
                    opinion_to_pref (Owl.Mat.row pre_data i)
                      (extract_candidate_policies i cand_noisy))
                  (List.init n_voters Fun.id)
            in

            let simulated_prefs =
                List.mapi
                  (fun _ i ->
                    opinion_to_pref
                      (Owl.Mat.row final_opinion i)
                      (extract_candidate_policies i final_est))
                  (List.init n_voters Fun.id)
            in

            let true_prefs =
                List.mapi
                  (fun _ i ->
                    opinion_to_pref (Owl.Mat.row post_data i)
                      (extract_candidate_policies i final_est))
                  (List.init (Owl.Mat.row_num post_data) Fun.id)
            in
                ( (final_opinion, post_data),
                  (original_prefs, simulated_prefs, true_prefs) ))
          timesteps
