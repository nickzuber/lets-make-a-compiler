open Ast
open Compiler

let () =
  print_endline ("\n\x1b[1mRUNNING TESTS:\x1b[0m \x1b[90m" ^ Unix.getcwd () ^ "/test_main.ml\x1b[39m");
  Test_uniquify.main ();
  Test_flatten.main ();
  print_endline "";
