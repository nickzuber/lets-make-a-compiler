open Ast
open Ast.Assembly

exception Incorrect_step of string
exception Unexpected_argument

let build_variable_to_offset_mapping (vars : string list) : (string, int) Hashtbl.t =
  let variable_size = List.length vars in
  let mapping = Hashtbl.create variable_size in
  let rec assign_offset vars i =
    match vars with
    | [] -> ()
    | var :: rest ->
        Hashtbl.add mapping var (i * -8);
        assign_offset rest (i + 1)
  in assign_offset vars 0;
  mapping

(* Given a variable and offset mappings, produce the offset stackpointer register for it. *)
let rsp_register_of_variable (mapping : (string, int) Hashtbl.t) (var : string) : Assembly.arg =
  let offset = Hashtbl.find mapping var in
  let register_string = Printf.sprintf "rsp(%d)" offset in
  REGISTER register_string

let arg_of_select_arg (mapping : (string, int) Hashtbl.t) (arg : Select.arg) : Assembly.arg =
  match arg with
  | Select.INT n -> INT n
  | Select.REGISTER r -> REGISTER r
  | Select.VARIABLE v -> rsp_register_of_variable mapping v

let assign (instructions : Select.instruction list) (mapping : (string, int) Hashtbl.t) : Assembly.instruction list =
  [RETQ (INT 2)]

(* NOTE: `retq` should always return $rax, otherwise you have an error. *)
(* Given a program of variables and assembly instructions, produce a valid assembly program. *)
let transform (prog : program) : program =
  let instructions = match prog with
    | SelectProgram (vars, instructions, final_instruction) ->
        let variable_size = List.length vars in
        let mapping = build_variable_to_offset_mapping vars in
        (* Push stack pointer down far enough to store a variable in each memory location. *)
        let prepare_memory = SUBQ (INT (8 * variable_size), REGISTER "rsp") in
        let instructions = assign instructions mapping in
        let prepare_return = (match final_instruction with
          | Select.RETQ arg ->
            let arg' = arg_of_select_arg mapping arg in
            [MOVQ (arg', REGISTER "rax");
             ADDQ (INT (8 * variable_size), REGISTER "rps");
             RETQ (REGISTER "rax")]
          | _ -> raise (Unexpected_argument)) in
        prepare_memory :: instructions @ prepare_return
    | _ -> raise (Incorrect_step "expected type SelectProgram") in
  AssemblyProgram instructions
