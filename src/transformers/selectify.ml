open Ast
open Ast.Select
open Polyfill

exception Incorrect_step of string
exception Program_error of string

let extra_variables : ((string, Ast.t) Hashtbl.t) = Hashtbl.create 53

(* Mapping of int to caller save registers. *)
let int_to_callersave_register = Hashtbl.create 6
let _ = List.iter (fun (i, reg) -> Hashtbl.add int_to_callersave_register i reg)
    [ (0, REGISTER "rsi")   (* 2st function argument *)
    ; (1, REGISTER "rdx")   (* 3st function argument *)
    ; (2, REGISTER "rcx")   (* 4st function argument *)
    ; (3, REGISTER "r8")    (* 5th function argument *)
    ; (4, REGISTER "r9") ]  (* 6st function argument *)

let find_reg_for_arg i =
  try
    (* Use a register. Note the ordering is important. *)
    Hashtbl.find int_to_callersave_register i
  with
    Not_found ->
    (* Use stack. *)
    raise (Program_error "Too many arguments applied to function.")

let arg_of_flat_argument (arg : Flat.argument) : Select.arg =
  match arg with
  | Flat.Int n -> INT n
  | Flat.Variable v -> VARIABLE v
  | Flat.FunctionReference n -> TAG n  (* Treat as tag and indirect call later *)

let rec select_single_statement (stmt : Flat.statement) : Select.instruction list = Flat.(
    match stmt with
    | Assignment (var, expr) -> (match expr with
        | Apply (caller, arguments) ->
          let var' = VARIABLE var in
          let caller' = arg_of_flat_argument caller in
          let args_in_regs_list = List.mapi (fun index arg ->
              let reg = find_reg_for_arg index in
              let arg' = arg_of_flat_argument arg in
              MOV (arg', reg)) arguments in
          [ LEAQ (caller', var')
          ; MOV (REGISTER rootstack_ptr_reg, REGISTER "rdi") ]
          @ args_in_regs_list
          @ [ INDIRECT_CALL (var')
            ; MOV (REGISTER "rax", var') ]
        | Allocate (gs, t, n) ->
          (* Length of vector * 8 for items + 8 for the size of the tag + 8 for length of items. *)
          let size_in_bytes = ((n * 8) + 8 + 8) in
          let free_ptr = REGISTER free_ptr_reg in
          let free_ptr_tag_deref = REFERENCE (free_ptr_reg, 0) in
          let free_ptr_len_deref = REFERENCE (free_ptr_reg, 8) in
          (* recall that `n` is already size_in_bytes from expose pass *)
          let var' = VARIABLE var in
          let guid = Dangerous_guid.get () in
          let ptr_var_name = Printf.sprintf "ty_vector_ptr_var%d" guid in
          Hashtbl.add extra_variables ptr_var_name t;
          [ MOV (GLOBAL "free_ptr", var')
          (* ; CALL ("_show_freeptr") *)
          ; ADD (INT size_in_bytes, GLOBAL "free_ptr")
          ; MOV (var', free_ptr)
          ; LEAQ (TAG gs, VARIABLE ptr_var_name)
          ; MOV (VARIABLE ptr_var_name, free_ptr_tag_deref)
          ; MOV (INT n, free_ptr_len_deref) ]
        | VectorRef (vec, i) ->
          let vec' = arg_of_flat_argument vec in
          let free_ptr = REGISTER free_ptr_reg in
          (* field + tag + len *)
          let offset = (i * 8) + 8 + 8 in
          let free_ptr_deref = REFERENCE (free_ptr_reg, offset) in
          let var' = VARIABLE var in
          [ MOV (vec', free_ptr)
          ; MOV (free_ptr_deref, var') ]
        | VectorSet (vec, i, body) ->
          let vec' = arg_of_flat_argument vec in
          let body' = arg_of_flat_argument body in
          let free_ptr = REGISTER free_ptr_reg in
          (* field + tag *)
          let offset = (i * 8) + 8 + 8 in
          let free_ptr_deref = REFERENCE (free_ptr_reg, offset) in
          let var' = VARIABLE var in
          [ MOV (vec', free_ptr)
          ; MOV (body', free_ptr_deref)
          ; MOV (INT 0, var') ]
        | Void ->
          let var' = VARIABLE var in
          [ MOV (INT 0, var') ]
        | Global s ->
          let var' = VARIABLE var in
          [ MOV (GLOBAL s, var') ]
        | Argument arg ->
          let src = arg_of_flat_argument arg in
          let dest = VARIABLE var in
          [MOV (src, dest)]
        | Read ->
          let src = REGISTER "rax" in
          let dest = VARIABLE var in
          [CALL ("_read_int");
           MOV (src, dest)]
        | UnaryExpression (op, arg) -> (match op with
            | Not ->
              let src = arg_of_flat_argument arg in
              let dest = VARIABLE var in
              [MOV (src, dest);
               XOR (dest, INT 1)]
            | Minus ->
              let src = arg_of_flat_argument arg in
              let dest = VARIABLE var in
              [MOV (src, dest);
               NEG (dest)])
        | BinaryExpression (op, lhs, rhs) -> (match op with
            | Plus ->
              let lhs' = arg_of_flat_argument lhs in
              let rhs' = arg_of_flat_argument rhs in
              let var' = VARIABLE var in
              [MOV (lhs', var');
               ADD (rhs', var')]
            | And ->
              let lhs' = arg_of_flat_argument lhs in
              let rhs' = arg_of_flat_argument rhs in
              let var' = VARIABLE var in
              [CMP (rhs', lhs');
               SET (E, (BYTE_REGISTER "al"));
               MOVZB ((BYTE_REGISTER "al"), var')]
            | Or ->
              (* Not done, same as AND at the moment. *)
              let lhs' = arg_of_flat_argument lhs in
              let rhs' = arg_of_flat_argument rhs in
              let var' = VARIABLE var in
              [CMP (rhs', lhs');
               SET (E, (BYTE_REGISTER "al"));
               MOVZB ((BYTE_REGISTER "al"), var')]
            | Compare cmp -> (match cmp with
                | Equal ->
                  let lhs' = arg_of_flat_argument lhs in
                  let rhs' = arg_of_flat_argument rhs in
                  let var' = VARIABLE var in
                  [CMP (rhs', lhs');
                   SET (E, (BYTE_REGISTER "al"));
                   MOVZB ((BYTE_REGISTER "al"), var')]
                | GreaterThan ->
                  let lhs' = arg_of_flat_argument lhs in
                  let rhs' = arg_of_flat_argument rhs in
                  let var' = VARIABLE var in
                  [CMP (rhs', lhs');
                   SET (G, (BYTE_REGISTER "al"));
                   MOVZB ((BYTE_REGISTER "al"), var')]
                | LessThan ->
                  let lhs' = arg_of_flat_argument lhs in
                  let rhs' = arg_of_flat_argument rhs in
                  let var' = VARIABLE var in
                  [CMP (rhs', lhs');
                   SET (L, (BYTE_REGISTER "al"));
                   MOVZB ((BYTE_REGISTER "al"), var')])))
    | IfStatement (test, consequent_instrs, alternate_instrs) -> (match test with
        | BinaryExpression ((Compare cmp), lhs, rhs) ->
          let lhs' = arg_of_flat_argument lhs in
          let rhs' = arg_of_flat_argument rhs in
          let then_instructions = select consequent_instrs  in
          let else_instructions = select alternate_instrs in
          [IF_STATEMENT (CMP (rhs', lhs'), then_instructions, else_instructions)]
        | _ -> raise (Program_error "Unhandled if expression"))
    | Collect ->
      let rootstack_ptr = REGISTER rootstack_ptr_reg in
      let function_argument_reg = REGISTER "rdi" in
      [ MOV (rootstack_ptr, function_argument_reg)
      (* if `collect` took size, we'd pass that into %rsi here *)
      ; CALL ("_collect") ])

and select (stmts : Flat.statement list) : Select.instruction list =
  match stmts with
  | [] -> []
  | stmt :: [] -> select_single_statement stmt
  | stmt :: rest -> (select_single_statement stmt) @ (select rest)

(* Given a flattened program, produce a program that's like assembly, but we still have variable names. *)
let transform (prog : program) : program =
  let (t, (vars, instructions, final_instruction)) = match prog with
    | FlatProgram (vars, stmts, arg, t) ->
      let instrs = select stmts in
      (* The final flat program argument is the result of running this program. *)
      let final_instr = arg_of_flat_argument arg in
      Hashtbl.iter (fun k v ->
          Hashtbl.add vars k v) extra_variables;
      (t, (vars, instrs, RET final_instr))
    | _ -> raise (Incorrect_step "expected type FlatProgram")
  in
  SelectProgram (t, vars, instructions, final_instruction)

(* if a variable is marked as unused, remove it *)
let rec remove_unused_variables instrs =
  match instrs with
  | [] -> []
  | instr :: [] -> remove_unused_variable instr
  | instr :: rest -> (remove_unused_variable instr) @ (remove_unused_variables rest)

(* if a variable is marked as unused, remove it *)
and remove_unused_variable instr =
  match instr with
  | MOV (INT _, VARIABLE a) when a.[0] = '_' -> []
  | _ -> [instr]

(* if a variable is marked as unused, remove it *)
let remove_unused_vars vars =
  let tbl = Hashtbl.create 53 in
  Hashtbl.iter (fun k v ->
      if k.[0] = '_' then () else Hashtbl.add tbl k v) vars;
  tbl

(* remove all instructions that try to move around unused variables *)
let remove_unused_variables prog =
  let (t, v, i, f) = match prog with
    | SelectProgram (t, vars, instructions, final_instruction) ->
      let instructions' = remove_unused_variables instructions in
      let vars' = remove_unused_vars vars in
      (* assumes final instruction is a used variable/argument *)
      (t, vars, instructions', final_instruction)
    | _ -> raise (Incorrect_step "expected type FlatProgram")
  in
  SelectProgram (t, v, i, f)
