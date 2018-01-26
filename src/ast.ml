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
  type var = string

  type unops =
    | Minus

  type binops =
    | Plus

  type argument =
    | Int of int
    | Var of var

  type expression =
    | Read
    | Int of int
    | Variable of string
    | UnaryExpression of unops * argument
    | BinaryExpression of binops * argument * argument

  type statement =
    | Assignment of var * expression
    | Return of argument
end = Flat

type program =
  | Program of Standard.expression
  | FlatProgram of Flat.var list * Flat.statement list
