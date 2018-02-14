open Ast
open Ast.Assembly

exception Incorrect_step of string
exception Unexpected_argument

(* List of general purpose registers we can use to store variables. *)
let valid_registers =
  [REGISTER "rcx";
   REGISTER "rdx";
   REGISTER "rsi";
   REGISTER "rdi";
   REGISTER "r8";
   REGISTER "r9";
   REGISTER "r10";
   REGISTER "r11"]

(* Create a mapping from variables to registers, then return the leftover variables and the mapping. *)
let build_variable_to_register_mapping (vars : string list) : string list * (string, Assembly.arg) Hashtbl.t =
  let variable_size = List.length vars in
  let mapping = Hashtbl.create variable_size in
  (* Assign registers to variables and return the list of unassigned variables. *)
  let rec assign_mapping vars regs : string list =
    match (vars, regs) with
    | ([], _) -> []
    | (unassigned_vars, []) -> unassigned_vars
    | (var :: rest_of_vars, reg :: rest_of_regs) ->
      Hashtbl.add mapping var reg;
      assign_mapping rest_of_vars rest_of_regs in
  let unassigned_variables = assign_mapping vars valid_registers in
  (unassigned_variables, mapping)

(* Create a mapping of spilled variable to locations in memory off the stack base pointer. *)
let build_spilled_variable_to_offset_mapping (mapping : (string, Assembly.arg) Hashtbl.t) (vars : string list) : (string, Assembly.arg) Hashtbl.t =
  let spilled_variable_size = List.length vars in
  (* At the point of a `call`, the %rsp base pointer register must be divisibly by 16.
     https://stackoverflow.com/questions/43354658/os-x-x64-stack-not-16-byte-aligned-error#comment73772561_43354658 *)
  let rsp_offset_starting_point = if spilled_variable_size mod 2 = 0 then 1 else 2 in
  let rec assign_offset vars i =
    match vars with
    | [] -> ()
    | var :: rest ->
      let offset = (i * -8) in
      let rsp_register_with_offset = REFERENCE ("rbp", offset) in
      Hashtbl.add mapping var rsp_register_with_offset;
      assign_offset rest (i + 1)
  in assign_offset vars rsp_offset_starting_point;
  mapping

(* Given a variable and offset mappings, produce the offset stackpointer register for it. *)
let register_of_variable (mapping : (string, Assembly.arg) Hashtbl.t) (var : string) : Assembly.arg =
  Hashtbl.find mapping var

(* Take a Select arg and creates a Assembly arg from it. This accounts for the variable mapping. *)
let arg_of_select_arg (mapping : (string, Assembly.arg) Hashtbl.t) (arg : Select.arg) : Assembly.arg =
  match arg with
  | Select.INT n -> INT n
  | Select.REGISTER r -> REGISTER r
  | Select.VARIABLE v -> register_of_variable mapping v

(* If both arguments reference memory, make a fix so that doesn't happen. *)
let fix_double_memory_references (instruction : Assembly.instruction) : Assembly.instruction list =
  match instruction with
  | ADDQ (REFERENCE (src, src_offset), REFERENCE (dest, dest_offset)) ->
    [MOVQ (REFERENCE (src, src_offset), REGISTER "rax");
     ADDQ (REGISTER "rax", REFERENCE (dest, dest_offset))]
  | SUBQ (REFERENCE (src, src_offset), REFERENCE (dest, dest_offset)) ->
    [MOVQ (REFERENCE (src, src_offset), REGISTER "rax");
     SUBQ (REGISTER "rax", REFERENCE (dest, dest_offset))]
  | MOVQ (REFERENCE (src, src_offset), REFERENCE (dest, dest_offset)) ->
    [MOVQ (REFERENCE (src, src_offset), REGISTER "rax");
     MOVQ (REGISTER "rax", REFERENCE (dest, dest_offset))]
  | _ -> [instruction]

let assign_single_instruction (mapping : (string, Assembly.arg) Hashtbl.t) (instruction : Select.instruction) : Assembly.instruction list =
  match instruction with
  | Select.ADDQ (src, dest) ->
    let src' = arg_of_select_arg mapping src in
    let dest' = arg_of_select_arg mapping dest in
    ADDQ (src', dest') |> fix_double_memory_references
  | Select.SUBQ (src, dest) ->
    let src' = arg_of_select_arg mapping src in
    let dest' = arg_of_select_arg mapping dest in
    SUBQ (src', dest') |> fix_double_memory_references
  | Select.MOVQ (src, dest) ->
    let src' = arg_of_select_arg mapping src in
    let dest' = arg_of_select_arg mapping dest in
    MOVQ (src', dest') |> fix_double_memory_references
  | Select.CALLQ label -> [CALLQ label]
  | Select.NEGQ src ->
    let src' = arg_of_select_arg mapping src in
    [NEGQ src']
  | Select.RETQ src ->
    let src' = arg_of_select_arg mapping src in
    [RETQ src']
  | Select.PUSHQ src ->
    let src' = arg_of_select_arg mapping src in
    [PUSHQ src']
  | Select.POPQ src ->
    let src' = arg_of_select_arg mapping src in
    [POPQ src']

let rec assign (mapping : (string, Assembly.arg) Hashtbl.t) (instructions : Select.instruction list) : Assembly.instruction list =
  match instructions with
  | [] -> []
  | instruction :: [] -> assign_single_instruction mapping instruction
  | instruction :: rest -> (assign_single_instruction mapping instruction) @ (assign mapping rest)

(* NOTE: `retq` should always return $rax, otherwise you have an error. *)
(* Given a program of variables and assembly instructions, produce a valid assembly program. *)
let transform (prog : program) : program =
  let instructions = match prog with
    | SelectProgram (vars, instructions, final_instruction) ->
      (* Map as many variables to registers as we can *)
      let unassigned_vars, unfinished_mapping = build_variable_to_register_mapping vars in
      let spilled_variable_size = List.length unassigned_vars in
      (* At the point of a `call`, the %rsp base pointer register must be divisibly by 16.
         https://stackoverflow.com/questions/43354658/os-x-x64-stack-not-16-byte-aligned-error#comment73772561_43354658 *)
      let align_base_pointer_offset = if spilled_variable_size mod 2 = 0 then 0 else 1 in
      (* Map the rest of the variables to memory *)
      let mapping = build_spilled_variable_to_offset_mapping unfinished_mapping unassigned_vars in
      (* Push stack pointer down far enough to store a variable in each memory location. *)
      let prepare_memory =
        [(PUSHQ (REGISTER "rbp"));
         (MOVQ ((REGISTER "rsp"), (REGISTER "rbp")));
         (SUBQ (INT (8 * (spilled_variable_size + align_base_pointer_offset)), REGISTER "rsp"))] in
      let instructions = assign mapping instructions in
      let prepare_return = (match final_instruction with
          | Select.RETQ arg ->
            let arg' = arg_of_select_arg mapping arg in
            [MOVQ (arg', REGISTER "rax");
             LEAVEQ;  (* This fixes the base pointer, replaces something like `ADDQ (INT (8 * spilled_variable_size), REGISTER "rsp")` *)
             RETQ (REGISTER "rax")]
          | _ -> raise (Unexpected_argument)) in
      prepare_memory @ instructions @ prepare_return
    | _ -> raise (Incorrect_step "expected type SelectProgram") in
  AssemblyProgram instructions
