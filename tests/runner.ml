open Pprint_ast

exception Unimplemented_testcase

let pass = ref 0
let fail = ref 0
let unimplemented = ref 0

let pprint_diff_verbose _formatter results =
  let (actual, expect) = results in
  let actual' = string_of_program actual ~padding:2 in
  let expect' = string_of_program expect ~padding:2 in
  let expect_string = Printf.sprintf
      "\n\x1b[32m┌──\x1b[39m\x1b[42;1m Expect \x1b[49;0m\n\x1b[32m:\x1b[39m\n\x1b[90m%s\x1b[39m"
      expect' in
  let actual_string = Printf.sprintf
      "\x1b[31m:\x1b[39m\n\x1b[31m├──\x1b[39m\x1b[41;1m Actual \x1b[49;0m\n\x1b[31m:\x1b[39m\n\x1b[90m%s\x1b[39m"
      actual' in
  print_endline expect_string;
  print_endline actual_string;
  print_endline "\x1b[31m: \x1b[39m";
  print_endline "\x1b[31m└──┐ \x1b[39m";
  print_endline "\x1b[31m   ↓ \x1b[39m"

let pprint_diff_quiet _formatter results = ()

let pprint_diff = if Settings.use_verbose_tests then pprint_diff_verbose else pprint_diff_quiet

let run test name desc =
  try
    test ();
    print_endline ("\x1b[32m∗ \x1b[39m " ^ name);
    pass := !pass + 1
  with
  | Unimplemented_testcase ->
    unimplemented := !unimplemented + 1;
    print_endline ("\x1b[33m↻ \x1b[39m " ^ name ^ " \x1b[90m" ^ desc ^ "\x1b[39m")

  | _ ->
    fail := !fail + 1;
    print_endline ("\x1b[31m⊘ \x1b[39m " ^ name ^ " \x1b[90m" ^ desc ^ "\x1b[39m")
