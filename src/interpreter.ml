open Ast
open Ast.Standard

exception Bad_program
exception Incorrect_step of string

type result =
  | R_INT of int
  | R_BOOL of bool

let rec interp (expr : expression) (env : (string, result) Hashtbl.t) : result =
  match expr with
  | Variable name -> Hashtbl.find env name
  | LetExpression (name, binding, body) ->
    let binding' = interp binding env in
    Hashtbl.add env name binding';
    interp body env
  | IfExpression (test, consequent, alternate) ->
    let test' = interp test env in
    let consequent' = interp consequent env in
    let alternate' = interp alternate env in
    (match (test', consequent', alternate') with
     | ((R_BOOL true), a, b) -> a
     | ((R_BOOL false), a, b) -> b
     | _ -> raise Bad_program)
  | BinaryExpression (op, lhs, rhs) ->
    let lhs' = interp lhs env in
    let rhs' = interp rhs env in
    (match (op, lhs', rhs') with
     | (Plus, (R_INT n), (R_INT m)) -> R_INT (n + m)
     | (And, (R_BOOL a), (R_BOOL b)) -> R_BOOL (a && b)
     | (Or, (R_BOOL a), (R_BOOL b)) -> R_BOOL (a || b)
     | (Compare cmp, (R_BOOL a), (R_BOOL b)) -> (match cmp with
         | Equal -> R_BOOL (a = b)
         | GreaterThan -> R_BOOL (a > b)
         | LessThan -> R_BOOL (a < b))
     | _ -> raise Bad_program
    )
  | UnaryExpression (op, operand) ->
    let operand' = interp operand env in
    (match (op, operand') with
     | (Minus, (R_INT n)) -> R_INT ((-1) * n)
     | (Not, (R_BOOL b)) -> R_BOOL (not b)
     | _ -> raise Bad_program)
  | Int n -> R_INT n
  | Read -> R_INT 0
  | True -> R_BOOL true
  | False -> R_BOOL false

(* An optimisic Program interpreter. We expect all programs to be well typed. *)
let parse (prog : program) : string =
  let env = Hashtbl.create 53 in
  let (ans, expr) = match prog with
    | Program expr -> ((interp expr env), expr)
    | _ -> raise (Incorrect_step "expected type Program") in
  match ans with
  | R_BOOL b -> string_of_bool b
  | R_INT n -> string_of_int n
