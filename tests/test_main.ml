open Ast
open Compiler

let () =
  let start = Unix.gettimeofday () in
  print_endline ("\n\x1b[1mRUNNING TESTS:\x1b[0m \x1b[90m" ^ Unix.getcwd () ^ "/test_main.ml\x1b[39m");
  Test_uniquify.main ();
  Test_flatten.main ();
  print_endline ("\n\x1b[1mTEST SUMMARY:\x1b[0m \x1b[90m" ^ Unix.getcwd () ^ "/test_main.ml\x1b[39m");
  Printf.printf " •\x1b[32m %d tests passed\x1b[39m" !Runner.pass;
  if (!Runner.fail > 0) then
    Printf.printf "\n •\x1b[31m %d tests failed\x1b[39m" !Runner.fail
  else
    ();
  Printf.printf "\n • Ran in %f seconds\n\n" ((Unix.gettimeofday ()) -. start)
