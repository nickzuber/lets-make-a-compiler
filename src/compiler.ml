open Ast
open Pprint_ast

(* The core compiling routine *)
let compile (prog : program) : program =
  prog |> Uniquify.transform
    |> Flatten.transform
    |> Selectify.transform
    |> Assignify.transform

(* Compiles the program decorated with print messages *)
let compile_and_debug (prog : program) : unit =
  print_endline "\n\x1b[90m[DEBUG] Compiling program...\x1b[39m";
  let _ = prog |> Uniquify.transform
    |> display_output "Uniquify"
    |> Flatten.transform
    |> display_output "Flatten"
    |> Selectify.transform
    |> display_output "Select"
    |> Assignify.transform
    |> display_output "Assign"
  in ()

let run (assembly : program) : unit =
  let assembly_filename = "assembly" in
  let runtime_filename = "basics" in
  let assembly_string = Assembler.string_of_assembly assembly in
  Core.Std.Out_channel.write_all (assembly_filename ^ ".s") ~data:assembly_string;
  let _build_runtime = Unix.system
    (Printf.sprintf "cc -c runtime/%s.c -o runtime/%s.o" runtime_filename runtime_filename) in
  let _build_program = Unix.system
    (Printf.sprintf "cc runtime/%s.o assembly.s -o program" runtime_filename)
  in ()

(* Runs and compiles the program *)
let compile_and_run (prog : program) : unit =
  let assembly = compile prog
  in run assembly
