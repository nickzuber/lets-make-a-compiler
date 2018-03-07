open Ast

let run (assembly : program) : string =
  let runtime_filename = "control" in
  let assembly_filename = "assembly" in
  let assembly_string = Assembler.string_of_assembly assembly in
  (* Create assembly file *)
  Core.Std.Out_channel.write_all (assembly_filename ^ ".s") ~data:assembly_string [@warning "-3"];
  (* Create runtime object file *)
  let _build_runtime = Unix.system
      (Printf.sprintf "gcc -c runtime/%s.c -o runtime/%s.o" runtime_filename runtime_filename) in
  (* Create executable with runtime and assembly file *)
  let _build_program = Unix.system
      (Printf.sprintf "gcc runtime/%s.o assembly.s -o testing_program" runtime_filename) in
  let ans = Unix.open_process_in "./testing_program" in
  input_line ans

let compile_and_run (prog : program) : string =
  let assembly = Compiler.compile prog in
  run assembly
