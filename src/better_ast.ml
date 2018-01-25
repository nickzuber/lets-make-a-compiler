(* Usage:
```
let ex1 = Better_ast.Program.(
  Expression.BinaryExpression({
    BinaryExpression.
    left = Expression.Number 1;
    operator = BinaryOperator.Plus;
    right = Expression.Number 2;
  })
)
``` *)

module rec Program : sig
  type t = Expression of Expression.t
end = Program

and UnaryOperator : sig
  type t =
    | Minus
end = UnaryOperator

and BinaryOperator : sig
  type t =
    | Plus
end = BinaryOperator

and Expression : sig
  type t =
    | Read
    | Number of int
    | Variable of Identifier.t
    | UnaryExpression of UnaryExpression.t
    | BinaryExpression of BinaryExpression.t
    | LetExpression of LetExpression.t
end = Expression

and Identifier : sig
  type t = string
end = Identifier

and UnaryExpression : sig
  type t = {
    operator: UnaryOperator.t;
    operand: Expression.t;
  }
end = UnaryExpression

and BinaryExpression : sig
  type t = {
    left: Expression.t;
    operator: BinaryOperator.t;
    right: Expression.t;
  }
end = BinaryExpression

and LetExpression : sig
  type t = {
    binding: Identifier.t * Expression.t;
    body: Expression.t;
  }
end = LetExpression
