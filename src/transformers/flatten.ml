open Ast
open Ast.Flat

exception Illegal_variable_reference of string
exception Invalid_program

(* *)
let rec flatten (expr : Standard.expression) (env : (string, int) Hashtbl.t) : Flat.statement list =
  match expr with
  | _ -> [
    Assignment ("a", Argument (Int 1));
    Assignment ("b", Argument (Int 2));
    Assignment ("c", Argument (Int 3));
    Assignment ("d", Argument (Int 4));
    Return  (Variable "d");
  ]
  (* | Variable name ->
  | LetExpression (name, binding, body) ->
  | BinaryExpression (op, lhs, rhs) ->
  | UnaryExpression (op, operand) ->
  | Int n -> Int n
  | Read -> Read *)

(*  *)
let transform (prog : program) : program =
  let env = Hashtbl.create 20 in
  let flattened_body = match prog with
    | Program expr -> flatten expr env
    | _ -> raise Invalid_program in
  FlatProgram ([], flattened_body)
