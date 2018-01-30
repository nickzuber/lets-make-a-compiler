module rec Standard : sig
  type unops =
    | Minus
  type binops =
    | Plus
  type expression =
    | Read
    | Int of int
    | Variable of string
    | UnaryExpression of unops * expression
    | BinaryExpression of binops * expression * expression
    | LetExpression of string * expression * expression
end = Standard

module rec Flat : sig
  type unops =
    | Minus
  type binops =
    | Plus
  type argument =
    | Int of int
    | Variable of string
  type expression =
    | Read
    | Argument of argument
    | UnaryExpression of unops * argument
    | BinaryExpression of binops * argument * argument
  type statement =
    | Assignment of string * expression
end = Flat

type program =
  | Program of Standard.expression
  | FlatProgram of string list * Flat.statement list * Flat.argument
