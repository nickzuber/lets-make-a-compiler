open Ast
open Ast.Standard

exception Illegal_variable_reference of string
exception Incorrect_step

(* Return the amount of times we've seen a variable so far. Since the variables
 * are stored in the hashtable, a failed lookup means we've seen it zero times so
 * we return that as the count. *)
let get_count tbl key =
  try Hashtbl.find tbl key
  with _ -> 0

(* Actually does the unnique naming transformation on a given program. This is done by
 * passing around a mapping of all the variable names we've seen so far, and either:
 *  - throwing an exception if we're trying to reference an undefined variable
 *  - adjusts the name of the variable if it's been seen before
 * We use the `count` variable to append a unique number on each variable name
 * we decide that we want to change. *)
let rec uniquify (expr : expression) (env : (string, int) Hashtbl.t) : expression =
  match expr with
  | Variable name ->
      let count = get_count env name in
      if count > 0 then
        let name' = Printf.sprintf "%s_%d" name count in
        Variable name'
      else
        raise (Illegal_variable_reference name)
  | LetExpression (name, binding, body) ->
    let count = (get_count env name) + 1 in
    let env' = Hashtbl.copy env in
    Hashtbl.add env' name count;
    let binding' = uniquify binding env' in
    let body' = uniquify body env' in
    let name' = Printf.sprintf "%s_%d" name count in
    LetExpression (name', binding', body')
  | BinaryExpression (op, lhs, rhs) ->
    let lhs' = uniquify lhs env in
    let rhs' = uniquify rhs env in
    BinaryExpression (op, lhs', rhs')
  | UnaryExpression (op, operand) ->
    let operand' = uniquify operand env in
    UnaryExpression (op, operand')
  | Int n -> Int n
  | Read -> Read

(* Given a program, removes any instances of shadowing by providing each variable
 * with a unique name. A feature of this transformation is any variables out referenced
 * out of scope will be caught and an exception will be thrown.
 * The approach here is to extract the program body and uniquify that expression, since
 * the recursive algorithm operates on expressions anyways. *)
let transform (prog : program) : program =
  let env = Hashtbl.create 20 in
  let uniquified_body = match prog with
    | Program expr -> uniquify expr env
    | _ -> raise Incorrect_step in
  Program uniquified_body
