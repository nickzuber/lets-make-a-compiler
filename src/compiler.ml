open Ast
open Pprint_ast

(* The core compiling routine *)
let compile (prog : program) : program =
  prog |> Uniquify.transform
  |> Flatten.transform
  |> Selectify.transform
  |> Assignify.transform

(* Compiles the program decorated with print messages *)
let compile_and_debug (prog : program) : program =
  Printf.printf "%s" (create_title "Compiling each step in debug mode");
  prog |> Uniquify.transform
  |> display_title "Uniquify"
  |> Flatten.transform
  |> display_title "Flatten"
  |> Selectify.transform
  |> display_title "Select"
  |> Assignify.transform
  |> display_title "Assign"

let build_runtime runtime_filename =
  let build_runtime = Unix.system
      (Printf.sprintf "gcc -c runtime/%s.c -o runtime/%s.o" runtime_filename runtime_filename) in
  let msg = match build_runtime with
    | Unix.WEXITED n when n = 0 -> "\x1b[32m∗\x1b[39m  built runtime object file\t\t" ^ "[\x1b[1mruntime/" ^ runtime_filename ^ ".o\x1b[0m]"
    | _ -> "\x1b[31m⊘\x1b[39m  failed build runtime object file\t" ^ "[\x1b[1mruntime/" ^ runtime_filename ^ ".o\x1b[0m]"
  in print_endline msg

let build_program runtime_filename =
  let build_program = Unix.system
      (Printf.sprintf "gcc runtime/%s.o assembly.s -o program" runtime_filename) in
  let msg = match build_program with
    | Unix.WEXITED n when n = 0 -> "\x1b[32m∗\x1b[39m  generated executable program\t\t" ^ "[\x1b[1mruntime/" ^ runtime_filename ^ ".o\x1b[0m]"
    | _ -> "\x1b[31m⊘\x1b[39m  failed generate executable program\t" ^ "[\x1b[1mruntime/" ^ runtime_filename ^ ".o\x1b[0m]"
  in print_endline msg

let run (assembly : program) : unit =
  print_endline (create_title "Building and running program");
  let runtime_filename = "basics" in
  let assembly_filename = "assembly" in
  let assembly_string = Assembler.string_of_assembly assembly in
  (* Create assembly file *)
  Core.Std.Out_channel.write_all (assembly_filename ^ ".s") ~data:assembly_string;
  print_endline ("\x1b[32m∗\x1b[39m  created assembly file\t\t[\x1b[1m" ^ assembly_filename ^ ".s\x1b[0m]");
  (* Create runtime object file *)
  build_runtime runtime_filename;
  (* Create executable with runtime and assembly file *)
  build_program runtime_filename;
  (* Execute the program *)
  print_endline "\x1b[32m∗\x1b[39m  attempting to execute program\t[\x1b[1mprogram.exe\x1b[0m]";
  print_endline (create_title "Output of program");
  let exit_code = Unix.system "./program" in
  let result_message = match exit_code with
    | Unix.WEXITED n when n = 0 -> "Done."
    | _ -> "Failed."
  in Printf.printf "\n%s\n" result_message

(* Runs and compiles the program *)
let compile_and_run (prog : program) : unit =
  let assembly = compile prog in
  run assembly
