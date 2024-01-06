open OUnit2 
(* open Calculator.Ast *)
open Calculator.Main

(* [make_test name num str] is a test unit have [name] and assert whether [num]
is equal to [iterp str] *)
let make_test name num expr_str = 
  name >:: (fun _ -> assert_equal (string_of_int num) (interp expr_str))
;;

let tests = "interp tests suite" >::: [
  make_test "int"             22 "22";
  make_test "addition"        22 "11+11";
  make_test "Multiplication"  22 "11*2";
  make_test "mult of mult"    44 "11*2*2";
  make_test "add and mult"    24 "2+11*2";
  make_test "mult and add"    24 "11*2+2";
  make_test "mult and add"    44 "11 * (2 + 2)";
]

let _ = run_test_tt_main tests

