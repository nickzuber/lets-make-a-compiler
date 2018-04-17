open Ast
open Ast.Select

exception Incorrect_step of string
exception Unhandled_if_test_expression

let int_of_tag = function
  | T_INT -> 1
  | T_BOOL -> 2
  | T_VECTOR _ -> 3
  | T_VOID -> 4

let arg_of_flat_argument (arg : Flat.argument) : Select.arg =
  match arg with
  | Flat.Int n -> INT n
  | Flat.Variable v -> VARIABLE v

let rec select_single_statement (stmt : Flat.statement) : Select.instruction list = Flat.(
    match stmt with
    | Assignment (var, expr) -> (match expr with
        | Allocate (t, n) ->
          let free_ptr = REGISTER free_ptr_reg in
          let free_ptr_deref = REFERENCE (free_ptr_reg, 0) in
          (* recall that `n` is already size_in_bytes from expose pass *)
          let var' = VARIABLE var in
          [ MOV (GLOBAL "free_ptr", var')
          ; ADD (INT n, GLOBAL "free_ptr")
          ; MOV (var', free_ptr)
          ; MOV (INT (int_of_tag (T_VECTOR [])), free_ptr_deref) ]
        | VectorRef (vec, i) ->
          let vec' = arg_of_flat_argument vec in
          let free_ptr = REGISTER free_ptr_reg in
          (* field + tag *)
          let offset = i * 8 + 8 in
          let free_ptr_deref = REFERENCE (free_ptr_reg, offset) in
          let var' = VARIABLE var in
          [ MOV (vec', free_ptr)
          ; MOV (free_ptr_deref, var') ]
        | VectorSet (vec, i, body) ->
          let vec' = arg_of_flat_argument vec in
          let body' = arg_of_flat_argument body in
          let free_ptr = REGISTER free_ptr_reg in
          (* field + tag *)
          let offset = i * 8 + 8 in
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
        | _ -> raise Unhandled_if_test_expression)
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
      (t, (vars, instrs, RET final_instr))
    | _ -> raise (Incorrect_step "expected type FlatProgram") in
  SelectProgram (t, vars, instructions, final_instruction)
