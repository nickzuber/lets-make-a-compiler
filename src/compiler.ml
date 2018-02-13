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
  display "Compiling each step in debug mode";
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
  (* Create assembly file *)
  Core.Std.Out_channel.write_all (assembly_filename ^ ".s") ~data:assembly_string;
  print_endline "\n\x1b[32m∙\x1b[39m  building with runtime";
  (* Create runtime object file *)
  let _build_runtime = Unix.system
      (Printf.sprintf "cc -c runtime/%s.c -o runtime/%s.o" runtime_filename runtime_filename) in
  print_endline "\x1b[33m↻\x1b[39m  compiling program";
  (* Create executable with runtime and assembly file *)
  let _build_program = Unix.system
      (Printf.sprintf "cc runtime/%s.o assembly.s -o program" runtime_filename) in
  print_endline "\x1b[32m∙\x1b[39m  executing program";
  print_endline "\n\x1b[36m=-=-\x1b[39m \x1b[1mOutput of program\x1b[0m \x1b[36m=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=\x1b[39m";
  (* Execute the program *)
  let _run_program = Unix.system "./program" in
  print_endline ""

(* Runs and compiles the program *)
let compile_and_run (prog : program) : unit =
  let assembly = compile prog in
  let _ = display "Building and running program"
  in run assembly
