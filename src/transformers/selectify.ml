open Ast
open Ast.Select

exception Incorrect_step of string
exception Unhandled_if_test_expression

let arg_of_flat_argument (arg : Flat.argument) : Select.arg =
  match arg with
  | Flat.Int n -> INT n
  | Flat.Variable v -> VARIABLE v

let rec select_single_statement (stmt : Flat.statement) (count : int) : Select.instruction list = Flat.(
    match stmt with
    | Assignment (var, expr) -> (match expr with
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
               XORQ (dest, INT 1)]
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
              [CMPQ (lhs', rhs');
               SET (E, (BYTE_REGISTER "al"));
               MOVZBQ ((BYTE_REGISTER "al"), var')]
            | Or ->
              (* Not done, same as AND at the moment. *)
              let lhs' = arg_of_flat_argument lhs in
              let rhs' = arg_of_flat_argument rhs in
              let var' = VARIABLE var in
              [CMPQ (lhs', rhs');
               SET (E, (BYTE_REGISTER "al"));
               MOVZBQ ((BYTE_REGISTER "al"), var')]
            | Compare cmp -> (match cmp with
                | Equal ->
                  let lhs' = arg_of_flat_argument lhs in
                  let rhs' = arg_of_flat_argument rhs in
                  let var' = VARIABLE var in
                  [CMPQ (lhs', rhs');
                   SET (E, (BYTE_REGISTER "al"));
                   MOVZBQ ((BYTE_REGISTER "al"), var')]
                | GreaterThan ->
                  let lhs' = arg_of_flat_argument lhs in
                  let rhs' = arg_of_flat_argument rhs in
                  let var' = VARIABLE var in
                  [CMPQ (rhs', lhs');
                   SET (G, (BYTE_REGISTER "al"));
                   MOVZBQ ((BYTE_REGISTER "al"), var')]
                | LessThan ->
                  let lhs' = arg_of_flat_argument lhs in
                  let rhs' = arg_of_flat_argument rhs in
                  let var' = VARIABLE var in
                  [CMPQ (rhs', lhs');
                   SET (L, (BYTE_REGISTER "al"));
                   MOVZBQ ((BYTE_REGISTER "al"), var')])))
    | IfStatement (test, consequent_instrs, alternate_instrs) -> (match test with
        | BinaryExpression ((Compare cmp), lhs, rhs) ->
          (* I'm going to assume cmp is always Equal since `if` is implemented with naive method. *)
          let lhs' = arg_of_flat_argument lhs in
          let rhs' = arg_of_flat_argument rhs in
          let then_instructions = select consequent_instrs (count + 1) in
          let else_instructions = select alternate_instrs (count + 1) in
          let label_then = Printf.sprintf "IF_THEN_%d" count in
          let label_end = Printf.sprintf "IF_END_%d" count in
          [CMPQ (lhs', rhs');
           (JUMP (E, label_then))]
          @ else_instructions
          @ [JUMP (Always, label_end)]
          @ [LABEL label_then]
          @ then_instructions
          @ [LABEL label_end]
        | _ -> raise Unhandled_if_test_expression))

and select (stmts : Flat.statement list) (count : int) : Select.instruction list =
  match stmts with
  | [] -> []
  | stmt :: [] -> select_single_statement stmt count
  | stmt :: rest -> (select_single_statement stmt count) @ (select rest count)

(* Given a flattened program, produce a program that's like assembly, but we still have variable names. *)
let transform (prog : program) : program =
  let (t, (vars, instructions, final_instruction)) = match prog with
    | FlatProgram (vars, stmts, arg, t) ->
      let instrs = select stmts 0 in
      (* The final flat program argument is the result of running this program. *)
      let final_instr = arg_of_flat_argument arg in
      (t, (vars, instrs, RET final_instr))
    | _ -> raise (Incorrect_step "expected type FlatProgram") in
  SelectProgram (t, vars, instructions, final_instruction)
