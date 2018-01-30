open Ast
open Ast.Flat

exception Illegal_variable_reference of string
exception Incorrect_step

(* Given a standard AST, we want to convert this to a new AST that omits all nesting. The end result
 * is essentially a list of statements that we want to pass on. Variable names will be converted
 * from their previous names (`x_0`, `y_2`, etc.) into something more meaningful (`let_binding_3`,
 * `add_lhs_2`, etc.). When these name changes happen, we will map the previous name to the newer
 * name so that other expressions that reference these variables will be able to correct their name. *)
let rec flatten (expr : Standard.expression) (count : int) : string list * Flat.statement list * Flat.argument =
  match expr with
  | Standard.LetExpression (name, binding, body) ->
      let (vars, statements, argument) = flatten binding count in
      let (vars', statements', argument') = flatten body count in
      let assign = Assignment (name, Argument argument) in
      (vars @ vars' @ [name],
      statements @ [assign] @ statements',
      argument')
  | Standard.UnaryExpression (op, expr) ->
      let name = "unaryExpr_" ^ (string_of_int count) in
      let (vars, statements, argument) = flatten expr (count + 1) in
      let assign = Assignment (name, UnaryExpression (Flat.Minus, argument)) in
      (vars @ [name], statements @ [assign], Variable name)
  | Standard.BinaryExpression (op, lhs, rhs) ->
      let name = "binaryExpr_" ^ (string_of_int count) in
      let (vars_lhs, statements_lhs, argument_lhs) = flatten lhs (count + 1) in
      let (vars_rhs, statements_rhs, argument_rhs) = flatten rhs (count + 1) in
      let assign = Assignment (name, BinaryExpression (Flat.Plus, argument_rhs, argument_lhs)) in
      (vars_lhs @ vars_rhs @ [name],
      statements_lhs @ statements_rhs @ [assign],
      Variable name)
  | Standard.Read ->
      let name = "read_x" in
      let assign = Assignment (name, Read) in
      ([name], [assign], Variable name)
  | Standard.Variable v -> ([], [], (Variable v))
  | Standard.Int n -> ([], [], Int n)

(*  *)
let transform (prog : program) : program =
  let count = 0 in
  let (vars, statements, argument) = match prog with
    | Program expr -> flatten expr count
    | _ -> raise Incorrect_step in
  FlatProgram (vars, statements, argument)
