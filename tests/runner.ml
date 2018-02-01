open Pprint_ast

let pprint_diff _formatter results =
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
  print_endline "\x1b[31m└─┐ \x1b[39m";
  print_endline "\x1b[31m  ↓ \x1b[39m"

let run test name desc =
  try
    test ();
    print_endline ("\x1b[32m✓ success\x1b[39m " ^ name)
  with
    | _ -> print_endline ("\x1b[31m✕ failure\x1b[39m " ^ name ^ " \x1b[90m" ^ desc ^ "\x1b[39m")
