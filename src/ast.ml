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

type program =
  | Program of expression
