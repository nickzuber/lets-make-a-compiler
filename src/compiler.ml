open Ast
open Pprint_ast

(* The core compiling routine *)
let compile (prog : program) : program =
  prog |> Macros.transform
  |> Definify.transform
  |> Uniquify.transform
  |> Typecheck.transform
  |> Expose.transform
  |> Flatten.transform
  |> Selectify.transform
  |> Selectify.remove_unused_variables
  |> Assignify.transform ~quiet:true

(* Compiles the program decorated with print messages *)
let compile_and_debug (prog : program) : program =
  Printf.printf "%s" (create_title "Compiling each step in debug mode");
  prog |> Macros.transform
  |> Definify.transform |> display_title "Definify"
  |> Uniquify.transform |> display_title "Uniquify"
  |> Typecheck.transform |> display_title "Typecheck"
  |> Expose.transform |> display_title "Expose"
  |> Flatten.transform |> display_title "Flatten"
  |> Selectify.transform |> display_title "Select"
  |> Selectify.remove_unused_variables |> display_title "Select (cleaned)"
  |> Assignify.transform |> display_title "Assign"

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
  let runtime_filename = "gc" in
  let assembly_filename = "assembly" in
  let assembly_string = Assembler.string_of_assembly assembly in
  (* Create assembly file *)
  Core.Std.Out_channel.write_all (assembly_filename ^ ".s") ~data:assembly_string [@warning "-3"];
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
    | Unix.WEXITED n -> Printf.sprintf "Done with exit code %s." (string_of_int n)
    | _ -> "Failed."
  in Printf.printf "\n%s\n" result_message

(* Runs and compiles the program *)
let compile_and_run (prog : program) : unit =
  let assembly = compile prog in
  run assembly

let create_define_assembly (name : string) (definition : define) =
  let (name, params_with_types, body_expr, return_type) = definition in
  let body_expr_as_program = Program ([], body_expr) in
  let body_assembly_program = body_expr_as_program
                              |> Macros.transform |> display_title "Macros"
                              |> Uniquify.transform_function ~function_name:name |> display_title "Uniquify"
                              |> Typecheck.transform |> display_title "Typecheck"
                              |> Expose.transform |> display_title "Expose"
                              |> Flatten.transform_function ~function_name:name |> display_title "Flatten"
                              |> Selectify.transform |> display_title "Selectify"
                              |> Selectify.remove_unused_variables |> display_title "Selectify (cleaned)"
                              |> Assignify.transform_function ~function_name:name ~quiet:true |> display_title "Assignify"
  in
  let body_instructions = match body_assembly_program with
    | AssemblyProgram (t, instrs) -> instrs
    | _ -> raise (Not_found)
  in
  Printf.sprintf "\t.globl %s\n\t%s:%s\n"
    name
    name
    (Pprint_ast.string_of_assembly_instructions body_instructions ~padding:(6))

(* Pass the program right through without touching it, but generate the function definitions in the
 * assembly before you do this. *)
let compile_functions prog =
  let defines_strings = Hashtbl.fold (fun k v acc -> acc ^ (create_define_assembly k v)) Assembler.defines "" in
  Hashtbl.add Assembler.function_definitions "functions" defines_strings;
  prog
