open Utils
open Initpy

let objectiveFun v1 v2 distMeasure updatedProfile =
  let r = v1.bias in
  let r' = 1.0 -. r in
  let p1 = v1.preference in
  let p2 = v2.preference in
  let d1 = distMeasure p1 updatedProfile in
  let d2 = distMeasure p2 updatedProfile in
  (* assert (
     Float.compare d1 1.0 <> -1 || Float.compare d2 1.0 <> -1
     || v1.preference = v2.preference); *)
  let lhs = Float.pow d1 2.0 in
  let rhs = Float.pow d2 2.0 in
  (r *. lhs) +. (r' *. rhs)

let update_profile v1 v2 distance between =
  let obj = objectiveFun v1 v2 distance in
  let alternatives =
    List.init (List.length v1.preference) (fun x -> [ x + 1 ])
  in
  let profiles = all_profiles alternatives in

  let profiles = List.filter (between v1.preference v2.preference) profiles in
  let profiles = v1.preference :: v2.preference :: profiles in
  let scores = List.map obj profiles in
  let _, i =
    List.fold_left
      (fun (min_val, min_idx) (x, i) ->
        if x < min_val then (x, i) else (min_val, min_idx))
      (List.hd scores, 0)
      (List.mapi (fun i x -> (x, i)) scores)
  in
  let new_voter =
    {
      preference = List.nth profiles i;
      bias = v1.bias;
      announced = v1.announced;
    }
  in
  new_voter

let deliberate ?(should_shuffle = true) voters rounds distance between =
  let announce listeners announcer =
    List.map
      (fun voter ->
        if voter = announcer then
          {
            preference = voter.preference;
            bias = voter.bias;
            announced = voter.announced + 1;
          }
        else
          let nw = update_profile voter announcer distance between in
          nw)
      listeners
  in

  let rec round updated_voters round_num =
    let unnannounced_voters =
      List.filter (fun v -> v.announced < round_num) updated_voters
    in
    match unnannounced_voters with
    | [] -> updated_voters
    | announcer :: _ -> round (announce updated_voters announcer) round_num
  in

  let rec aux vs r =
    if
      r >= rounds
      || List.length @@ unique_preferences (List.map (fun v -> v.preference) vs)
         = 1
    then vs
    else
      let vs = if should_shuffle then shuffle vs else vs in
      aux (round vs (r + 1)) (r + 1)
  in
  aux voters 0

let normalize_matrix adjacency_matrix =
  let row_sums = Owl.Mat.sum_cols adjacency_matrix in
  let row_sums_fix =
    Owl.Mat.map (fun sum -> if sum <> 0. then sum else 1.) row_sums
  in
  Owl.Mat.div adjacency_matrix row_sums_fix

let add_self_bias adjacency_matrix factor =
  Owl.Mat.add_diag adjacency_matrix factor

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

let perform_spectral_clustering adjacency_matrix n_clusters =
  (* Create the spectral clustering model *)
  (* let model =
       WrappedModels.SpectralClustering.fit adjacency_matrix n_clusters
         "nearest_neighbours"
     in *)

  (* Fit the model to the adjacency matrix *)
  let numpy_matrix = owl_to_np_NDArray adjacency_matrix in
  let labels =
    WrappedModels.SpectralClustering.fit ~data:numpy_matrix ~n_clusters
      ~affinity:"" ()
  in
  labels

let perform_tsne opinion_distance_matrix n_components =
  (* Create the spectral clustering model *)

  (* Fit the model to the adjacency matrix *)
  let numpy_matrix = owl_to_np_NDArray opinion_distance_matrix in
  let transform_matrix =
    WrappedModels.TSNE.fit_transform ~data:numpy_matrix ~n_components ()
  in
  transform_matrix

let align_voter_graph adjacency_matrix opinion_matrix =
  (* let numpy_adjacency_matrix = owl_to_np_NDArray adjacency_matrix in
     let numpy_opinion_matrix = owl_to_np_NDArray opinion_matrix in *)
  let order =
    WrappedModels.align_voters_to_graph ~data1:adjacency_matrix
      ~data2:opinion_matrix ()
  in
  order

(** [deGroot] takes in a trust matrix and a number of steps, and returns the *)
let deGroot trust_matrix t =
  let open Owl.Mat in
  let evolved_trust_matrix = trust_matrix **@ t in
  assert (sum_cols evolved_trust_matrix =~ ones (row_num trust_matrix) 1);
  (evolved_trust_matrix, evolved_trust_matrix)
