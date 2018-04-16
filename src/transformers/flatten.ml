open Ast
open Ast.Flat

exception Illegal_variable_reference of string
exception Incorrect_step of string

let flat_unop_of_typed_standard_unop = function
  | InternalTypedStandard.Minus -> Minus
  | InternalTypedStandard.Not -> Not

let flat_binop_of_typed_standard_binop = function
  | InternalTypedStandard.Plus -> Plus
  | InternalTypedStandard.And -> And
  | InternalTypedStandard.Or -> Or
  | InternalTypedStandard.Compare cmp ->
    Compare (match cmp with
        | InternalTypedStandard.Equal -> Equal
        | InternalTypedStandard.GreaterThan -> GreaterThan
        | InternalTypedStandard.LessThan -> LessThan)

(* Given a standard AST, we want to convert this to a new AST that omits all nesting. The end result
 * is essentially a list of statements that we want to pass on. Newly created variables will be given
 * the name of something more meaningful (`binary_expression_3`, `unary_expression_2`, etc.).
 * In this phase, booleans are converted to their integer counterparts. *)
let rec flatten (expr : InternalTypedStandard.expression) (count : int) (env : (string, Ast.t) Hashtbl.t)
  : int * (string, Ast.t) Hashtbl.t * Flat.statement list * Flat.argument =
  match expr with
  | InternalTypedStandard.LetExpression (name, (tb, binding), (td, body)) ->
    let (count', vars_binding, statements_binding, argument_binding) = flatten binding count env in
    let (count'', vars_body, statements_body, argument_body) = flatten body count' env in
    let assign = Assignment (name, Argument argument_binding) in
    Hashtbl.add env name td;
    (count'',
     env,  (* vars_binding @ vars_body @ [name], *)
     statements_binding @ [assign] @ statements_body,
     argument_body)
  | InternalTypedStandard.UnaryExpression (op, (t, expr)) ->
    let var_unexp = "unary_expression_" ^ (string_of_int count) in
    let (count', vars, statements, argument) = flatten expr (count + 1) env in
    let op' = flat_unop_of_typed_standard_unop op in
    let assign = Assignment (var_unexp, UnaryExpression (op', argument)) in
    Hashtbl.add env var_unexp t;
    (count',
     env,  (* vars @ [var_unexp] *)
     statements @ [assign],
     Variable var_unexp)
  | InternalTypedStandard.BinaryExpression (op, (tl, lhs), (tr, rhs)) ->
    let var_binexp = "binary_expression_" ^ (string_of_int count) in
    let (count', vars_lhs, statements_lhs, argument_lhs) = flatten lhs (count + 1) env in
    let (count'', vars_rhs, statements_rhs, argument_rhs) = flatten rhs (count' + 1) env in
    let op' = flat_binop_of_typed_standard_binop op in
    let assign = Assignment (var_binexp, BinaryExpression (op', argument_lhs, argument_rhs)) in
    Hashtbl.add env var_binexp T_INT;
    (count'',
     env,  (* vars_lhs @ vars_rhs @ [var_binexp] *)
     statements_lhs @ statements_rhs @ [assign],
     Variable var_binexp)
  | InternalTypedStandard.IfExpression ((tt, test), (tc, consequent), (ta, alternate)) ->
    let var_if = "if_statement_" ^ (string_of_int count) in
    let (count', vars_t, statements_t, argument_t) = flatten test (count + 1) env in
    let (count'', vars_c, statements_c, argument_c) = flatten consequent (count' + 1) env in
    let (count''', vars_a, statements_a, argument_a) = flatten alternate (count'' + 1) env in
    let if_statement =
      IfStatement
        ((BinaryExpression
            ((Compare Equal),
             (Int 1),
             (argument_t))),
         statements_c @ [Assignment (var_if, (Argument argument_c))],
         statements_a @ [Assignment (var_if, (Argument argument_a))]) in
    Hashtbl.add env var_if tc;  (* tc or ta since both same type *)
    (count''',
     env,  (* [var_if] @ vars_t @ vars_c @ vars_a *)
     statements_t @ [if_statement],
     Variable var_if)
  | InternalTypedStandard.Read ->
    let var_read = "read_" ^ (string_of_int count) in
    let assign = Assignment (var_read, Read) in
    Hashtbl.add env var_read T_INT;
    (count,
     env,  (* [var_read] *)
     [assign],
     Variable var_read)
  | InternalTypedStandard.Variable v -> (count, env, [], (Variable v))
  | InternalTypedStandard.Int n -> (count, env, [], Int n)
  | InternalTypedStandard.True -> (count, env, [], Int 1)
  | InternalTypedStandard.False -> (count, env, [], Int 0)
  | _ -> (count, env, [], Int (-1)) (* TODO: Vector, VectorRef, VectorSet *)

(* Given a typed program, transform it into a flat program such that all forms of nesting is removed. *)
let transform (prog : program) : program =
  let count = 0 in
  let env = Hashtbl.create 53 in
  let ((_, vars, statements, argument), argument_type) = match prog with
    | InternalProgramTyped (t, expr) -> ((flatten expr count env), t)
    | _ -> raise (Incorrect_step "expected type ProgramTyped") in
  FlatProgram (vars, statements, argument, argument_type)
