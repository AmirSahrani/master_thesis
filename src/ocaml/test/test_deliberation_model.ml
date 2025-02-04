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

(* Define the test suite *)
let () =
  run "Single-peaked Tests"
    [
      ( "is_single_peaked",
        [
          test_case "Check single-peaked profiles" `Quick test_is_single_peaked;
        ] );
    ]
