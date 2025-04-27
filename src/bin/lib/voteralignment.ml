open Initpy

let align_voter_graph adjacency_matrix opinion_matrix =
    let order =
        WrappedModels.align_voters_to_graph ~data1:adjacency_matrix
          ~data2:opinion_matrix ()
    in
        order
