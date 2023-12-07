open OUnit2
open Ch3

let test_ch3 =
  "test suite for ch3"
  >::: [ ("test for product function"
          >:: fun _ ->
          assert_equal (product [ 1; 3; 9 ]) 27;
          assert_equal (product []) 1)
       ; ("test for pattern three function"
          >:: fun _ ->
          assert (fst_eq_snd_elem [ 2; 2 ]);
          assert_equal (fst_eq_snd_elem []) false;
          assert_equal (fst_string_is_bigred []) false;
          assert (fst_string_is_bigred [ "bigred" ]);
          assert (len_is_two_or_four [ 1 ] |> not);
          assert (len_is_two_or_four [ 3; 4 ]);
          assert (len_is_two_or_four [ 2; 3; 2; 3 ]))
       ; ("test for library "
          >:: fun _ ->
          assert_equal (fifth_element [ 1; 2; 3; 4; 10 ]) 10;
          assert_equal (fifth_element [ 1; 2; 3; 4 ]) 0;
          assert_equal (sort_by_desecnding [ 1; 3; 9; 100 ]) [ 100; 9; 3; 1 ];
          assert_equal (sort_by_desecnding []) [])
       ; ("test for library puzzle "
          >:: fun _ -> assert_equal (last_element [ 2; 5; 7; 0 ]) 0)
       ]
;;

(* let test_ch3 : test list = 
  [
    "test for ch3" >::: [

    ]
  ] *)

let _ = run_test_tt_main test_ch3
