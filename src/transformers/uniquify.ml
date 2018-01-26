open Ast
open Mapping

exception Illegal_variable_reference of string

(* Actually does the unnique naming transformation on a given program. This is done by
 * passing around a mapping of all the variable names we've seen so far, and either:
 *  - throwing an exception if we're trying to reference an undefined variable
 *  - adjusts the name of the variable if it's been seen before
 * We use the `count` variable to append a unique number on each variable name
 * we decide that we want to change. *)
let rec uniquify (expr : expression) (env : env) (count : int) : expression =
  match expr with
  | Variable name ->
    let name' = Printf.sprintf "%s_%d" name count in
    if exists_in_env env name' then
      Variable name'
    else
      raise (Illegal_variable_reference name)
  | LetExpression (name, binding, body) ->
    let count' = count + 1 in
    let name' = Printf.sprintf "%s_%d" name count' in
    let mapping = (name', binding) in
    let env' = extend_env env mapping in
    let binding' = uniquify binding env' count' in
    let body' = uniquify body env' count' in
    LetExpression (name', binding', body')
  | BinaryExpression (op, lhs, rhs) ->
    let lhs' = uniquify lhs env count in
    let rhs' = uniquify rhs env count in
    BinaryExpression (op, lhs', rhs')
  | UnaryExpression (op, operand) ->
    let operand' = uniquify operand env count in
    UnaryExpression (op, operand')
  | Int n -> Int n
  | Read -> Read

(* Given a program, removes any instances of shadowing by providing each variable
 * with a unique name. A feature of this transformation is any variables out referenced
 * out of scope will be caught and an exception will be thrown.
 * The approach here is to extract the program body and uniquify that expression, since
 * the recursive algorithm operates on expressions anyways. *)
let transform (prog : program) : program =
  let env = [] in
  let count = 0 in
  let uniquified_body = match prog with
    | Program expr -> uniquify expr env count in
  Program uniquified_body
