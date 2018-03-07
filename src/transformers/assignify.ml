open Ast
open Ast.Assembly

exception Incorrect_step of string
exception Unexpected_argument

(* Global ID generator module. *)
module Dangerous_guid = struct
  let _id = ref (-1)
  let get () = _id := (!_id + 1); !_id
end

(* Given a variable and offset mappings, produce the offset stackpointer register for it. *)
let register_of_variable (mapping : (string, Assembly.arg) Hashtbl.t) (var : string) : Assembly.arg =
  Hashtbl.find mapping var

(* Call the correct function based on the return type of the program. *)
let get_print_function (t : Ast.t) : Assembly.instruction =
  match t with
  | T_BOOL -> CALLQ "_print_bool"
  | T_INT -> CALLQ "_print_int"

(* Take a Select arg and creates a Assembly arg from it. This accounts for the variable mapping. *)
let arg_of_select_arg (mapping : (string, Assembly.arg) Hashtbl.t) (arg : Select.arg) : Assembly.arg =
  match arg with
  | Select.INT n -> INT n
  | Select.REGISTER r -> REGISTER r
  | Select.VARIABLE v -> register_of_variable mapping v
  | Select.BYTE_REGISTER r -> BYTE_REGISTER r

let cc_of_select_cc (cc : Select.cc) : Assembly.cc =
  match cc with
  | Select.E -> E
  | Select.G -> G
  | Select.L -> L
  | Select.GE -> GE
  | Select.LE -> LE
  | Select.Always -> Always

(* If both arguments reference memory, make a fix so that doesn't happen.
 * If we try to compare two INT's, we need to make the last arg a register.
 * CMP must be [ANY], REGISTER
 * XOR must be [ANY], REGISTER *)
let fix_illegal_instruction_combinations (instruction : Assembly.instruction) : Assembly.instruction list =
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
  | CMPQ (INT n, INT m) ->
    [MOVQ (INT m, REGISTER "rax");
     CMPQ (INT n, REGISTER "rax")]
  | CMPQ (reg, INT m) ->
    [MOVQ (INT m, REGISTER "rax");
     CMPQ (reg, REGISTER "rax")]
  | XORQ (INT n, INT m) ->
    [MOVQ (INT m, REGISTER "rax");
     XORQ (INT n, REGISTER "rax")]
  | XORQ (reg, INT m) ->
    [XORQ (INT m, reg)]
  | _ -> [instruction]

let rec assign_single_instruction (mapping : (string, Assembly.arg) Hashtbl.t) (instruction : Select.instruction) (count : int) : Assembly.instruction list =
  match instruction with
  | Select.ADD (src, dest) ->
    let src' = arg_of_select_arg mapping src in
    let dest' = arg_of_select_arg mapping dest in
    ADDQ (src', dest') |> fix_illegal_instruction_combinations
  | Select.SUB (src, dest) ->
    let src' = arg_of_select_arg mapping src in
    let dest' = arg_of_select_arg mapping dest in
    SUBQ (src', dest') |> fix_illegal_instruction_combinations
  | Select.MOV (src, dest) ->
    let src' = arg_of_select_arg mapping src in
    let dest' = arg_of_select_arg mapping dest in
    MOVQ (src', dest') |> fix_illegal_instruction_combinations
  | Select.CALL label ->
    (* NOTE: The amount you push and pop here is relative to the spill size.
     * Push/pop needs to be 16 byte aligned. It's like each push/pop adds 8 bytes.
     * So spill size + push/pop must be divisible by 16.
     * Example: spill size is 1 and push/pop is 2, they equal 3 together and therefore is offset.
     *          spill size is 2 and push/pop is 2, they equal 4 together and therefore is fine.
     * IMPORTANT: Since the registers we push/pop here are constant, we don't actually need to care about
     * this, since the rsp offset will always align itself to 16 byte assuming the amount of registers we
     * push/pop here is an even number. *)
    (* In reality, you should do a separate pass that injects these push/pops based on the liveness. maybe *)
    [PUSHQ (REGISTER "rcx");
     PUSHQ (REGISTER "rdx");
     PUSHQ (REGISTER "r8");
     PUSHQ (REGISTER "r9");
     PUSHQ (REGISTER "r10");
     PUSHQ (REGISTER "r11");
     CALLQ label;
     POPQ (REGISTER "r11");
     POPQ (REGISTER "r10");
     POPQ (REGISTER "r9");
     POPQ (REGISTER "r8");
     POPQ (REGISTER "rdx");
     POPQ (REGISTER "rcx");]
  | Select.NEG src ->
    let src' = arg_of_select_arg mapping src in
    [NEGQ src']
  | Select.RET src ->
    let src' = arg_of_select_arg mapping src in
    [RETQ src']
  | Select.PUSH src ->
    let src' = arg_of_select_arg mapping src in
    [PUSHQ src']
  | Select.POP src ->
    let src' = arg_of_select_arg mapping src in
    [POPQ src']
  | Select.XORQ (src, dest) ->
    let src' = arg_of_select_arg mapping src in
    let dest' = arg_of_select_arg mapping dest in
    XORQ (src', dest') |> fix_illegal_instruction_combinations
  | Select.CMPQ (src, dest) ->
    let src' = arg_of_select_arg mapping src in
    let dest' = arg_of_select_arg mapping dest in
    CMPQ (src', dest') |> fix_illegal_instruction_combinations
  | Select.SET (cc, arg) ->
    let cc' = cc_of_select_cc cc in
    let arg' = arg_of_select_arg mapping arg in
    SET (cc', arg') |> fix_illegal_instruction_combinations
  | Select.JUMP (cc, label) ->
    let cc' = cc_of_select_cc cc in
    JUMP (cc', label) |> fix_illegal_instruction_combinations
  | Select.MOVZBQ (src, dest) ->
    let src' = arg_of_select_arg mapping src in
    let dest' = arg_of_select_arg mapping dest in
    MOVZBQ (src', dest') |> fix_illegal_instruction_combinations
  | Select.LABEL label -> [LABEL label]
  | Select.IF_STATEMENT (t, c, a) ->
    let count = Dangerous_guid.get () in
    let t_instr = assign_single_instruction mapping t count in
    let then_instrs = assign mapping c count in
    let else_instrs = assign mapping a count in
    let label_then = Printf.sprintf "then%d" count in
    let label_end = Printf.sprintf "if_end%d" count in
    t_instr
    @ [(JUMP (E, label_then))]
    @ else_instrs
    @ [JUMP (Always, label_end)]
    @ [LABEL label_then]
    @ then_instrs
    @ [LABEL label_end]

and assign (mapping : (string, Assembly.arg) Hashtbl.t) (instructions : Select.instruction list) (count : int) : Assembly.instruction list =
  match instructions with
  | [] -> []
  | instruction :: [] -> assign_single_instruction mapping instruction count
  | instruction :: rest -> (assign_single_instruction mapping instruction count) @ (assign mapping rest count)

(* NOTE: `retq` should always return $rax, otherwise you have an error. *)
(* Given a program of variables and assembly instructions, produce a valid assembly program. *)
let transform (prog : program) : program =
  let (t, instructions) = match prog with
    | SelectProgram (t, vars, instructions, final_instruction) ->
      (* The spilled variable size is used for offsetting the stack pointer. *)
      let mapping, spilled_variable_size = Mapping.create vars instructions in
      let align_base_pointer_offset = if spilled_variable_size mod 2 = 0 then 0 else 1 in
      (* Push stack pointer down far enough to store a variable in each memory location. *)
      let prepare_memory =
        [(PUSHQ (REGISTER "rbp"));
         (MOVQ ((REGISTER "rsp"), (REGISTER "rbp")));
         (SUBQ (INT (8 * (spilled_variable_size + align_base_pointer_offset)), REGISTER "rsp"))] in
      let instructions = assign mapping instructions 0 in
      let print_function_callq = get_print_function t in
      let prepare_return = (match final_instruction with
          | Select.RET arg ->
            let arg' = arg_of_select_arg mapping arg in
            [MOVQ (arg', REGISTER "rax");
             MOVQ (REGISTER "rax", REGISTER "rdi");
             print_function_callq;
             MOVQ (INT 0, REGISTER "rax");
             LEAVEQ;  (* This fixes the base pointer, replaces something like `ADDQ (INT (8 * spilled_variable_size), REGISTER "rsp")` *)
             RETQ (REGISTER "rax")]
          | _ -> raise (Unexpected_argument)) in
      (t, prepare_memory @ instructions @ prepare_return)
    | _ -> raise (Incorrect_step "expected type SelectProgram") in
  AssemblyProgram (t, instructions)
