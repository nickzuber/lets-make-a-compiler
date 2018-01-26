open Ast
open Ast.Flat
open Ast.Standard
open Mapping

exception Illegal_variable_reference of string
exception Invalid_program

(* *)
let rec flatten (expr : expression) (env : env) (count : int) : expression =
  match expr with
  | Variable name ->
    Read
  | LetExpression (name, binding, body) ->
    Read
  | BinaryExpression (op, lhs, rhs) ->
    Read
  | UnaryExpression (op, operand) ->
    Read
  | Int n -> Int n
  | Read -> Read

(*  *)
let transform (prog : program) : program =
  let env = [] in
  let count = 0 in
  let flattened_body = match prog with
    | Program expr -> flatten expr env count
    | _ -> raise Invalid_program in
  Program flattened_body
