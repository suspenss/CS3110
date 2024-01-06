exception OhNo of string

let x = OhNo "oops"

let safty_div x y =
  try x / y with
  | Division_by_zero -> 0
;;

let _ =
  match List.hd [] with
  | [] -> "empty"
  | _ :: _ -> "nonempty"
  | exception Failure x -> x
;;

(*
   (* test exception *)
   open OUnit2

   let tests =
   "suite"
   >::: [ ("empty"
          >:: fun _ -> assert_raises (Failure "hd") (fun () -> List.hd []))
       ]
   ;;

   let _ = run_test_tt_main tests
*)
