open Ast
open Pprint_ast

(* The core compiling routine *)
let compile (prog : program) : program =
  prog
    |> Uniquify.transform
    |> Flatten.transform
    |> Selectify.transform
    |> Assignify.transform

let compile_and_debug (prog : program) : program =
  print_endline "\n\x1b[90m[DEBUG] Compiling program...\x1b[39m";
  prog
    |> Uniquify.transform
    |> display_output "Uniquify"
    |> Flatten.transform
    |> display_output "Flatten"
    |> Selectify.transform
    |> display_output "Select"
    |> Assignify.transform
    |> display_output "Assign"

(* Run the executable x86-64 binary *)
let run assembly : unit =
  print_endline "\n\x1b[90mRunning program...\x1b[39m\n"

(* Runs and compiles the program *)
let compile_and_run (prog : program) =
  let assembly = compile prog
  in run assembly
