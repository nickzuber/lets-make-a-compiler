open Ast
open Ast.Flat

exception Illegal_variable_reference of string
exception Incorrect_step of string

let flat_unop_of_typed_standard_unop = function
  | TypedStandard.Minus -> Minus
  | TypedStandard.Not -> Not

let flat_binop_of_typed_standard_binop = function
  | TypedStandard.Plus -> Plus
  | TypedStandard.And -> And
  | TypedStandard.Or -> Or
  | TypedStandard.Compare cmp ->
    Compare (match cmp with
        | TypedStandard.Equal -> Equal
        | TypedStandard.GreaterThan -> GreaterThan
        | TypedStandard.LessThan -> LessThan)

(* Given a standard AST, we want to convert this to a new AST that omits all nesting. The end result
 * is essentially a list of statements that we want to pass on. Newly created variables will be given
 * the name of something more meaningful (`binary_expression_3`, `unary_expression_2`, etc.).
 * In this phase, booleans are converted to their integer counterparts. *)
let rec flatten (expr : TypedStandard.expression) (count : int) : int * string list * Flat.statement list * Flat.argument =
  match expr with
  | TypedStandard.LetExpression (name, binding, body) ->
    let (tb, binding) = binding in
    let (td, body) = body in
    let (count', vars_binding, statements_binding, argument_binding) = flatten binding count in
    let (count'', vars_body, statements_body, argument_body) = flatten body count' in
    let assign = Assignment (name, Argument argument_binding) in
    (count'',
     vars_binding @ vars_body @ [name],
     statements_binding @ [assign] @ statements_body,
     argument_body)
  | TypedStandard.UnaryExpression (op, expr) ->
    let (t, expr) = expr in
    let var_unexp = "unary_expression_" ^ (string_of_int count) in
    let (count', vars, statements, argument) = flatten expr (count + 1) in
    let op' = flat_unop_of_typed_standard_unop op in
    let assign = Assignment (var_unexp, UnaryExpression (op', argument)) in
    (count',
     vars @ [var_unexp],
     statements @ [assign],
     Variable var_unexp)
  | TypedStandard.BinaryExpression (op, lhs, rhs) ->
    let (tl, lhs) = lhs in
    let (tr, rhs) = rhs in
    let var_binexp = "binary_expression_" ^ (string_of_int count) in
    let (count', vars_lhs, statements_lhs, argument_lhs) = flatten lhs (count + 1) in
    let (count'', vars_rhs, statements_rhs, argument_rhs) = flatten rhs (count' + 1) in
    let op' = flat_binop_of_typed_standard_binop op in
    let assign = Assignment (var_binexp, BinaryExpression (op', argument_lhs, argument_rhs)) in
    (count'',
     vars_lhs @ vars_rhs @ [var_binexp],
     statements_lhs @ statements_rhs @ [assign],
     Variable var_binexp)
  | TypedStandard.IfExpression (test, consequent, alternate) ->
    let (tt, test) = test in
    let (tc, consequent) = consequent in
    let (ta, alternate) = alternate in
    let var_if = "if_statement_" ^ (string_of_int count) in
    let (count', vars_t, statements_t, argument_t) = flatten test (count + 1) in
    let (count'', vars_c, statements_c, argument_c) = flatten consequent (count' + 1) in
    let (count''', vars_a, statements_a, argument_a) = flatten alternate (count'' + 1) in
    let if_statement =
      IfStatement
        ((BinaryExpression
            ((Compare Equal),
             (Int 1),
             (argument_t))),
         statements_c @ [Assignment (var_if, (Argument argument_c))],
         statements_a @ [Assignment (var_if, (Argument argument_a))]) in
    (count''',
     [var_if] @ vars_t @ vars_c @ vars_a,
     statements_t @ [if_statement],
     Variable var_if)
  | TypedStandard.Read ->
    let var_read = "read_" ^ (string_of_int count) in
    let assign = Assignment (var_read, Read) in
    (count,
     [var_read],
     [assign],
     Variable var_read)
  | TypedStandard.Variable v -> (count, [], [], (Variable v))
  | TypedStandard.Int n -> (count, [], [], Int n)
  | TypedStandard.True -> (count, [], [], Int 1)
  | TypedStandard.False -> (count, [], [], Int 0)
  | _ -> (count, [], [], Int (-1)) (* TODO: Vector, VectorRef, VectorSet *)

(* Given a typed program, transform it into a flat program such that all forms of nesting is removed. *)
let transform (prog : program) : program =
  let count = 0 in
  let ((_, vars, statements, argument), argument_type) = match prog with
    | ProgramTyped (t, expr) -> ((flatten expr count), t)
    | _ -> raise (Incorrect_step "expected type ProgramTyped") in
  FlatProgram (vars, statements, argument, argument_type)
