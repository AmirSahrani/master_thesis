open Utils
open Graphs

type config = {
  pre_data : Owl.Mat.mat;
  post_data : Owl.Mat.mat;
  graph : GenericGraph.t;
  timesteps : float;
  n_voters : int;
  n_candidates : int;
  bias_factor : float;
  cand_method : alternativeGenerators;
  seed : int Option.t;
}

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
  |> List.mapi (fun i _ -> (i, List.nth distances i))
  |> List.sort (fun (_, d1) (_, d2) -> compare d1 d2)
  |> List.map fst

let create_trust_matrix graph bias_factor =
  let trust_matrix = graph |> adjacency_matrix_from in
  trust_matrix
  |> (fun m -> credibility_matrix m ())
  |> (fun m -> add_self_bias m bias_factor)
  |> normalize_matrix

(** [deGroot] takes in a trust matrix and a number of steps, and returns the *)
let deGroot config =
  let open Owl.Mat in
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

  (* First we create the proper trust matrix*)
  let trust_matrix = create_trust_matrix graph bias_factor in

  let candidates =
    List.init n_candidates (fun _ -> gen_alterantive cand_method pre_data 10)
  in

  let trust = trust_matrix **@ timesteps in
  let final_opinion = Owl.Mat.(trust *@ pre_data) in
  let simulated_prefs =
    List.mapi
      (fun _ i -> opinion_to_pref (Owl.Mat.row final_opinion i) candidates)
      (List.init n_voters Fun.id)
  in
  let true_prefs =
    List.mapi
      (fun _ i -> opinion_to_pref (Owl.Mat.row post_data i) candidates)
      (List.init (Owl.Mat.row_num post_data) Fun.id)
  in
  ((final_opinion, post_data), (simulated_prefs, true_prefs))
