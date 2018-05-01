open Ast
open Ast.Flat

exception Illegal_variable_reference of string
exception Incorrect_step of string
exception Found_unused_expression_type

let hashtbl_add_safe tbl name value =
  try
    let _ = Hashtbl.find tbl name in
    ()
  with
  | Not_found ->
    Hashtbl.add tbl name value

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
let rec flatten (expr : TypedStandard.typed_expression) (count : int) (env : (string, Ast.t) Hashtbl.t)
  : int * (string, Ast.t) Hashtbl.t * Flat.statement list * Flat.argument =
  match expr with
  | (t, TypedStandard.LetExpression (name, (tb, binding), (td, body))) ->
    let (count', vars_binding, statements_binding, argument_binding) = flatten (tb, binding) count env in
    let (count'', vars_body, statements_body, argument_body) = flatten (td, body) count' env in
    let assign = Assignment (name, Argument argument_binding) in
    hashtbl_add_safe env name td;
    (count'',
     env,  (* vars_binding @ vars_body @ [name], *)
     statements_binding @ [assign] @ statements_body,
     argument_body)
  | (t, TypedStandard.UnaryExpression (op, (tt, expr))) ->
    let var_unexp = "unary_expression" ^ (string_of_int count) in
    let (count', vars, statements, argument) = flatten (tt, expr) (count + 1) env in
    let op' = flat_unop_of_typed_standard_unop op in
    let assign = Assignment (var_unexp, UnaryExpression (op', argument)) in
    hashtbl_add_safe env var_unexp tt;
    (count',
     env,  (* vars @ [var_unexp] *)
     statements @ [assign],
     Variable var_unexp)
  | (t, TypedStandard.BinaryExpression (op, (tl, lhs), (tr, rhs))) ->
    let var_binexp = "binary_expression_" ^ (string_of_int count) in
    let (count', vars_lhs, statements_lhs, argument_lhs) = flatten (tl, lhs) (count + 1) env in
    let (count'', vars_rhs, statements_rhs, argument_rhs) = flatten (tr, rhs) (count' + 1) env in
    let op' = flat_binop_of_typed_standard_binop op in
    let assign = Assignment (var_binexp, BinaryExpression (op', argument_lhs, argument_rhs)) in
    hashtbl_add_safe env var_binexp T_INT;
    (count'',
     env,  (* vars_lhs @ vars_rhs @ [var_binexp] *)
     statements_lhs @ statements_rhs @ [assign],
     Variable var_binexp)
  | (t, TypedStandard.IfExpression ((tt, test), (tc, consequent), (ta, alternate))) ->
    let var_if = "if_statement_" ^ (string_of_int count) in
    let (count', vars_t, statements_t, argument_t) = flatten (tt, test) (count + 1) env in
    let (count'', vars_c, statements_c, argument_c) = flatten (tc, consequent) (count' + 1) env in
    let (count''', vars_a, statements_a, argument_a) = flatten (ta, alternate) (count'' + 1) env in
    let if_statement =
      IfStatement
        ((BinaryExpression
            ((Compare Equal),
             (Int 1),
             (argument_t))),
         statements_c @ [Assignment (var_if, (Argument argument_c))],
         statements_a @ [Assignment (var_if, (Argument argument_a))]) in
    hashtbl_add_safe env var_if tc;  (* tc or ta since both same type *)
    (count''',
     env,  (* [var_if] @ vars_t @ vars_c @ vars_a *)
     statements_t @ [if_statement],
     Variable var_if)
  | (t, TypedStandard.Read) ->
    let var_read = "read" ^ (string_of_int count) in
    let assign = Assignment (var_read, Read) in
    hashtbl_add_safe env var_read T_INT;
    (count,
     env,  (* [var_read] *)
     [assign],
     Variable var_read)
  | (t, TypedStandard.Variable v) -> (count, env, [], (Variable v))
  | (t, TypedStandard.FunctionReference n) -> (count, env, [], (FunctionReference n))
  | (t, TypedStandard.Int n) -> (count, env, [], Int n)
  | (t, TypedStandard.True) -> (count, env, [], Int 1)
  | (t, TypedStandard.False) -> (count, env, [], Int 0)
  | (t, TypedStandard.Global s) ->
    let global_variable = "global" ^ (string_of_int count) in
    let assign = Assignment (global_variable, Global s) in
    hashtbl_add_safe env global_variable T_INT;
    (count,
     env,
     [assign],
     Variable global_variable)
  | (t, TypedStandard.Void) -> (count, env, [], Int 0)
  | (t, TypedStandard.Allocate (gs, tt, n)) ->
    let vector_variable = "allocate_" ^ gs ^ "_" ^ (string_of_int count) in
    let assign = Assignment (vector_variable, Allocate (gs, tt, n)) in
    hashtbl_add_safe env vector_variable t;
    (count,
     env,
     [assign],
     Variable vector_variable)
  | (t, TypedStandard.VectorSet ((tv, vec_expr), i, (tt, body_expr))) ->
    let vectorset_variable = "vs" ^ (string_of_int count) in
    let (count', vars_vec, statements_vec, argument_vec) = flatten (tv, vec_expr) (count + 1) env in
    let (count'', vars_body, statements_body, argument_body) = flatten (tt, body_expr) (count' + 1) env in
    let assign = Assignment (vectorset_variable, VectorSet (argument_vec, i, argument_body)) in
    hashtbl_add_safe env vectorset_variable t;
    (count'',
     env,
     statements_vec @ statements_body @ [assign],
     Variable vectorset_variable)
  | (t, TypedStandard.VectorRef ((tv, vec_expr), i)) ->
    let vectorref_variable = "vr" ^ (string_of_int count) in
    let (count', vars_vec, statements_vec, argument_vec) = flatten (tv, vec_expr) (count + 1) env in
    let assign = Assignment (vectorref_variable, VectorRef (argument_vec, i)) in
    hashtbl_add_safe env vectorref_variable t;
    (count',
     env,
     statements_vec @ [assign],
     Variable vectorref_variable)
  | (t, TypedStandard.Collect) ->
    let collect_variable = "collect" ^ (string_of_int count) in
    hashtbl_add_safe env collect_variable t;
    (count,
     env,
     [Collect],
     Variable collect_variable)
  | (t, TypedStandard.Apply ((caller_t, caller), arguments)) ->
    let var_apply = "apply_expression_" ^ (string_of_int count) in
    let (count', vars_caller, statements_caller, argument_caller) = flatten (caller_t, caller) (count + 1) env in
    let (count'', statements_all_arguments, argument_all_arguments) = List.fold_left (fun acc arg ->
        let (arg_t, arg) = arg in
        let (prev_count, prev_statements, prev_arguments) = acc in
        let (new_count, vars_arg, statements_arg, argument_arg) = flatten (arg_t, arg) (prev_count + 1) env in
        ( new_count
        , statements_arg @ prev_statements
        , [argument_arg] @ prev_arguments )) (count', [], []) arguments
    in
    let assign = Assignment (var_apply, Apply (argument_caller, argument_all_arguments)) in
    hashtbl_add_safe env var_apply T_INT;
    (count'',
     env,  (* vars_caller @ vars_rhs @ [var_apply] *)
     statements_caller @ statements_all_arguments @ [assign],
     Variable var_apply)
  | (t, TypedStandard.Unless _)
  | (t, TypedStandard.Vector _)
  | (t, TypedStandard.When _)
  | (t, TypedStandard.Begin _) -> raise Found_unused_expression_type

(* Given a typed program, transform it into a flat program such that all forms of nesting is removed. *)
let transform (prog : program) : program =
  let count = 0 in
  let env = Hashtbl.create 53 in
  let ((_, vars, statements, argument), argument_type) = match prog with
    | ProgramTyped (t, expr) -> ((flatten (t, expr) count env), t)
    | _ -> raise (Incorrect_step "expected type ProgramTyped") in
  FlatProgram (vars, statements, argument, argument_type)
