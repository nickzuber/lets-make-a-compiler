open Ast
open Ast.Flat

exception Illegal_variable_reference of string
exception Incorrect_step

(* Given a standard AST, we want to convert this to a new AST that omits all nesting. The end result
 * is essentially a list of statements that we want to pass on. Newly created variables will be given
 * the name of something more meaningful (`binary_expression_3`, `unary_expression_2`, etc.). *)
let rec flatten (expr : Standard.expression) (count : int) : string list * Flat.statement list * Flat.argument =
  match expr with
  | Standard.LetExpression (name, binding, body) ->
      let (vars_binding, statements_binding, argument_binding) = flatten binding count in
      let (vars_body, statements_body, argument_body) = flatten body count in
      let assign = Assignment (name, Argument argument_binding) in
      (vars_binding @ vars_body @ [name],
      statements_binding @ [assign] @ statements_body,
      argument_body)
  | Standard.UnaryExpression (op, expr) ->
      let name = "unary_expression_" ^ (string_of_int count) in
      let (vars, statements, argument) = flatten expr (count + 1) in
      let assign = Assignment (name, UnaryExpression (Flat.Minus, argument)) in
      (vars @ [name], statements @ [assign], Variable name)
  | Standard.BinaryExpression (op, lhs, rhs) ->
      let name = "binary_expression_" ^ (string_of_int count) in
      let (vars_lhs, statements_lhs, argument_lhs) = flatten lhs (count + 1) in
      let (vars_rhs, statements_rhs, argument_rhs) = flatten rhs (count + 1) in
      let assign = Assignment (name, BinaryExpression (Flat.Plus, argument_lhs, argument_rhs)) in
      (vars_lhs @ vars_rhs @ [name],
      statements_lhs @ statements_rhs @ [assign],
      Variable name)
  | Standard.Read ->
      let name = "read_" ^ (string_of_int count) in
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
