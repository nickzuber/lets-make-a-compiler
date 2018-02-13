open Ast

let rec string_of_assembly node : string =
  match node with
  | AssemblyProgram instructions ->
    let instructions_string = Pprint_ast.string_of_assembly_instructions instructions ~padding:(2) in
    Printf.sprintf "    .globl _asm_main\n_asm_main:%s\n"
      instructions_string
  | _ -> ""
