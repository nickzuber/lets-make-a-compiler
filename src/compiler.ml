open Ast
open Pprint_ast

(* The core compiling routine *)
let compile (prog : program) =
  let prog' = Uniquify.transform prog in
  let output = string_of_program prog'
  in output

(* Run the executable x86-64 binary *)
let run assembly : unit =
  print_endline assembly;
  print_endline ""

(* Runs and compiles the program *)
let compile_and_run (prog : program) =
  let assembly = compile prog
  in run assembly
