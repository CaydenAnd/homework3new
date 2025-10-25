open OUnit2
open Interp
open Ast
open Main

let make_i n i s =
  n >:: (fun _ -> assert_equal (Int i) (interp s))

let make_b n b s =
  n >:: (fun _ -> assert_equal (Bool b) (interp s))

let make_t n err s =
  n >:: (fun _ -> assert_raises (Failure err) (fun () -> interp s))

let tests = [
  make_i "nested_lets" 45 "let x = 5 in let y = x * 9 in y";
  make_b "complex_bool" true "1+2 <= 3+4";
  make_i "if_nested" 10 "if 2<=3 then let z = 10 in z else 0";
  make_t "invalid_nested_if" if_guard_err "if 10 then true else false";
  make_t "mixed_error" bop_err "true + (1<=2)";
]

let _ = run_test_tt_main ("student_tests" >::: tests)
