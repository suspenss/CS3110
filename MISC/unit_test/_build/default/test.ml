open OUnit2
open Src

let make_sum_test name expected_output input =
  name >:: fun _ -> assert_equal expected_output (sum input) ~printer:string_of_int
;;

let _tests =
  "test suite for sum"
  >::: [ make_sum_test "empty" 0 []
       ; make_sum_test "singleton" 1 [ 1 ]
       ; make_sum_test "two_elements" 3 [ 1; 2 ]
       ]
;;

let _ = run_test_tt_main _tests

let tests =
  "test suite for next_weekday"
  >::: [ ("tue_after_mon" >:: fun _ -> assert_equal Tuesday (next_weekday Monday)) ]
;;

let _ = run_test_tt_main tests
