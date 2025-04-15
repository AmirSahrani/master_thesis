open Initpy

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
