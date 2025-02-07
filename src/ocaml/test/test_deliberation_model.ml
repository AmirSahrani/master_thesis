open Alcotest
open Deliberation_model
(* Replace with the actual module name containing is_single_peaked *)

(* Define test cases *)

(* Convert is_single_peaked output to boolean *)
let is_single_peaked_bool profile =
  match Dimensions.is_single_peaked profile with
  | None -> false
  | Some _ -> true

(* Define the test function *)
let test_is_single_peaked () =
  let profiles_tests =
    [
      [ [ 5; 4; 2; 3; 1 ]; [ 5; 4; 3; 2; 1 ]; [ 4; 5; 3; 2; 1 ] ];
      [ [ 5; 4; 2; 3; 1 ]; [ 5; 4; 2; 3; 1 ]; [ 5; 4; 2; 3; 1 ] ];
      [ [ 5; 4; 3; 1; 2 ]; [ 5; 4; 2; 3; 1 ]; [ 5; 4; 2; 1; 3 ] ];
    ]
  in

  let expected_results = [ true; true; false ] in
  List.iter2
    (fun profile expected ->
      let result = is_single_peaked_bool profile in
      check bool "Single-peaked test" expected result)
    profiles_tests expected_results

let test_DPDistance () =
  let preference = [ [ 1 ]; [ 2 ]; [ 3 ] ] in
  let preference2 = [ [ 3 ]; [ 2 ]; [ 1 ] ] in
  let d = Deliberation_model.Distances.dpDistance preference in
  let distance = d preference preference2 in
  check (float 0.001) "DP Distance test" 4.0 distance

let test_KSDistance () =
  let preference = [ [ 1 ]; [ 2 ]; [ 3 ] ] in
  let preference2 = [ [ 3 ]; [ 2 ]; [ 1 ] ] in
  let d = Deliberation_model.Distances.ksDistance in
  let distance = d preference preference2 in
  let distance_rev = d preference2 preference in
  let distance_self = d preference preference in
  check (float 0.001) "KS Distance test Opposing profiles" 6.0 distance;
  check (float 0.001) "KS Distance test Reverse is equal" 6.0 distance_rev;
  check (float 0.001) "KS Distance test distance to oneself is 0" 0.0
    distance_self

let test_deliberation () =
  let profile =
    [
      [ [ 2 ]; [ 1 ]; [ 3 ] ];
      [ [ 2 ]; [ 3 ]; [ 1 ] ];
      [ [ 2 ]; [ 3 ]; [ 1 ] ];
      [ [ 2 ]; [ 3 ]; [ 1 ] ];
      [ [ 2 ]; [ 3 ]; [ 1 ] ];
    ]
  in
  let biases = List.init (List.length profile) (fun _ -> 0.5) in
  let voters =
    List.map2
      (fun p b -> { Utils.preference = p; Utils.bias = b; Utils.announced = 0 })
      profile biases
  in
  let updated =
    Model.deliberate voters 1 Distances.ksDistance Distances.ksBetween
  in
  check bool "All voters announce exactly once per round" true
    (List.for_all (fun v -> v.Utils.announced = 1) updated)

(* Define the test suite *)
let () =
  run "Deliberation model Testing"
    [
      ( "is_single_peaked",
        [
          test_case "Check single-peaked profiles" `Quick test_is_single_peaked;
        ] );
      ( "dpDistance",
        [ test_case "Check DP distance measure" `Quick test_DPDistance ] );
      ( "ksDistance",
        [ test_case "Check KS distance measure" `Quick test_KSDistance ] );
      ( "Deliberation",
        [ test_case "Check valid deliberation" `Quick test_deliberation ] );
    ]
