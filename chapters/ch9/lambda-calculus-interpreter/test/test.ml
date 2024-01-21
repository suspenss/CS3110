open OUnit2
open Lc
open Main

(** [make n s1 s2] makes an OUnit test named [n] that expects
    [s2] to evalute to [s1]. *)
let make n s1 s2 = n >:: fun _ -> assert_equal (parse s1) (interp s2)

(** [make_unbound_err n s] makes an OUnit test named [n] that
    expects [s] to produce an unbound variable error. *)
let make_unbound_err n s =
  n >:: fun _ -> assert_raises (Failure unbound_var_err) (fun () -> interp s)
;;

let tests =
  [ make "reduce correct" "^y.y" "(^x.x) (^y.y)"
  ; make "shadowing correct" "^a.^b.b" "(^x.^x.x) (^a.^b.a) (^a.^b.b)"
    (* ; make_unbound_err "capture avoiding correct" "((^x.(^z.x)) z) (^x.x)" *)
  ]
;;

let _ = run_test_tt_main ("suite" >::: tests)
