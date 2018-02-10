open Ast
open Ast.Select

exception Incorrect_step of string

let arg_of_flat_argument (arg : Flat.argument) : Select.arg =
  match arg with
  | Flat.Int n -> INT n
  | Flat.Variable v -> VARIABLE v

let select_single_statement (stmt : Flat.statement) : Select.instruction list = Flat.(
  match stmt with
  | Assignment (var, expr) -> (match expr with
      | Argument arg ->
          let src = arg_of_flat_argument arg in
          let dest = VARIABLE var in
          [MOVQ (src, dest)]
      | Read ->
          let src = REGISTER "rax" in
          let dest = VARIABLE var in
          [CALLQ ("_read");
           MOVQ (src, dest)]
      | UnaryExpression (op, arg) -> (match op with
          | Minus ->
              let src = arg_of_flat_argument arg in
              let dest = VARIABLE var in
              [MOVQ (src, dest);
               NEGQ (dest)])
      | BinaryExpression (op, lhs, rhs) -> (match op with
          | Plus ->
              let lhs' = arg_of_flat_argument lhs in
              let rhs' = arg_of_flat_argument rhs in
              let var' = VARIABLE var in
              [MOVQ (lhs', var');
               ADDQ (rhs', var')])))

let rec select (stmts : Flat.statement list) : Select.instruction list =
  match stmts with
  | [] -> []
  | stmt :: [] -> select_single_statement stmt
  | stmt :: rest -> (select_single_statement stmt) @ (select rest)

(* Given a flattened program, produce a program that's like assembly, but we still
   have variable names. *)
let transform (prog : program) : program =
  let (vars, instructions, final_instruction) = match prog with
    | FlatProgram (vars, stmts, arg) ->
        let instrs = select stmts in
        (* The final flat program argument is the result of running this program. *)
        let final_instr = arg_of_flat_argument arg in
        (vars, instrs, RETQ final_instr)
    | _ -> raise (Incorrect_step "expected type FlatProgram") in
  SelectProgram (vars, instructions, final_instruction)
