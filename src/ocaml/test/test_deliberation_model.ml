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
  let preference3 = [ [ 3 ]; [ 2; 1 ] ] in
  let d = Deliberation_model.Distances.ksDistance in
  let distance = d preference preference2 in
  let distance_rev = d preference2 preference in
  let distance_self = d preference preference in
  let distance_weak = d preference2 preference3 in
  check (float 0.001) "KS Distance test Opposing profiles" 6.0 distance;
  check (float 0.001) "KS Distance test Reverse is equal" 6.0 distance_rev;
  check (float 0.001) "KS Distance test distance to oneself is 0" 0.0
    distance_self;
  check (float 0.001) "KS Distance test distance to oneself is 0" 1.0
    distance_weak

let test_KSBetween () =
  let p = [ [ 1 ]; [ 2 ]; [ 3 ] ] in
  let p2 = [ [ 3 ]; [ 1 ]; [ 2 ] ] in
  let p3 = [ [ 2 ]; [ 3 ]; [ 1 ] ] in
  let p4 = [ [ 3 ]; [ 2 ]; [ 1 ] ] in
  let p_eq = [ [ 3; 2; 1 ] ] in
  let b = Deliberation_model.Distances.ksBetween in
  check bool "Testing if (a = b = c) is not between a > b > c and c > b > a"
    false (b p p2 p_eq);
  check bool "Testing if (a = b = c) is not between a > b > c and b > c > a"
    false (b p2 p3 p_eq);
  check bool "Testing if (a = b = c) is not between a > b > c and b > c > a"
    true (b p p4 p_eq)

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
  let biases = List.init (List.length profile) (fun _ -> 0.7) in
  let voters =
    List.map2
      (fun p b -> { Utils.preference = p; Utils.bias = b; Utils.announced = 0 })
      profile biases
  in
  let updated =
    Model.deliberate voters 1 Distances.csDistance Distances.csBetween
      ~should_shuffle:false
  in

  check bool "All voters announce exactly once per round" true
    (List.for_all (fun v -> v.Utils.announced = 1) updated);
  check bool "Voters do not change their mind with bias > 0.7" true
    (List.for_all2
       (fun v vu -> v.Utils.preference = vu.Utils.preference)
       voters updated)

let test_deliberation_ks () =
  let profile =
    [
      [ [ 1 ]; [ 2 ]; [ 3 ] ]; [ [ 3 ]; [ 1 ]; [ 2 ] ]; [ [ 2 ]; [ 3 ]; [ 1 ] ];
    ]
  in
  let biases = List.init (List.length profile) (fun _ -> 0.85) in
  let voters =
    List.map2
      (fun p b -> { Utils.preference = p; Utils.bias = b; Utils.announced = 0 })
      profile biases
  in
  let updated =
    Model.deliberate voters 1 Distances.ksDistance Distances.ksBetween
      ~should_shuffle:false
  in

  check bool "Voters do not change their mind with bias > 0.7" true
    (Evaluations.is_cyclic @@ Utils.extract_preferences updated)

let test_transitive () =
  let profile_intransative =
    [
      [ [ 1 ]; [ 2 ]; [ 3 ] ]; [ [ 3 ]; [ 1 ]; [ 2 ] ]; [ [ 2 ]; [ 3 ]; [ 1 ] ];
    ]
  in
  let profile_transative =
    [
      [ [ 1 ]; [ 2 ]; [ 3 ] ]; [ [ 1 ]; [ 2 ]; [ 3 ] ]; [ [ 1 ]; [ 2 ]; [ 3 ] ];
    ]
  in
  let result_intransative = Evaluations.is_transitive profile_intransative in
  let result_transative = Evaluations.is_transitive profile_transative in
  check bool "Correctly determines if intransative profile is not transative"
    false result_intransative;
  check bool "Correctly determines if transative profile is transative" true
    result_transative

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
      ( "ksBetween",
        [ test_case "Check KS betweenness measure" `Quick test_KSBetween ] );
      ( "Deliberation",
        [
          test_case "Check valid deliberation" `Quick test_deliberation;
          test_case "Check if cyclic profile stays cyclic in KS" `Quick
            test_deliberation_ks;
        ] );
      ("Transtive", [ test_case "Check transitivity" `Quick test_transitive ]);
    ]
