let pprint_diff _formatter results =
  let (actual, expect) = results in
  let actual_string = Printf.sprintf
    "\x1b[41;1m Actual \x1b[49;0m\n\x1b[90m%s\x1b[39m"
    actual in
  let expect_string = Printf.sprintf
    "\x1b[42;1m Expect \x1b[49;0m\n\x1b[90m%s\x1b[39m"
    expect in
  print_endline "\n\x1b[1mSomething went wrong and your test failed, here is a diff:\x1b[0m";
  print_endline expect_string;
  print_endline actual_string;
  print_endline ""

let run test name desc =
  try
    test ();
    print_endline ("\x1b[32m✓ success\x1b[39m " ^ name)
  with
    | _ -> print_endline ("\x1b[31m✕ failure\x1b[39m " ^ name ^ " \x1b[90m" ^ desc ^ "\x1b[39m")
