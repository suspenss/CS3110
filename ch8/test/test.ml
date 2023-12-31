open OUnit2
open Ds.Lmap

let bindings_test name input output =
  name >:: fun _ -> assert_equal output (ListMap.bindings input)
;;

let assoc_tests =
  let open ListMap in
  [ bindings_test "empty bindings" empty []
  ; (let lst = [ "hello", 12 ] in
     bindings_test "singleton list has 1 binding" (of_list lst) lst)
  ]
;;

let suite = "maps suite" >::: assoc_tests
let _ = run_test_tt_main suite
