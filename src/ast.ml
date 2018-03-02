module rec Standard : sig
  type cmps =
    | Equal
    | GreaterThan
    | LessThan
  type unops =
    | Minus
    | Not
  type binops =
    | Plus
    | And
    | Or
    | Compare of cmps
  type expression =
    | Read
    | True
    | False
    | Int of int
    | Variable of string
    | UnaryExpression of unops * expression
    | BinaryExpression of binops * expression * expression
    | LetExpression of string * expression * expression
    | IfExpression of expression * expression * expression
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

module rec Select : sig
  type reg = string
  type arg =
    | INT of int
    | VARIABLE of string
    | REGISTER of string
  type instruction =
    | ADD of arg * arg
    | SUB of arg * arg
    | MOV of arg * arg
    | CALL of string  (* label *)
    | NEG of arg
    | RET of arg
    | PUSH of arg
    | POP of arg
end = Select

module rec Assembly : sig
  type arg =
    | INT of int
    | REGISTER of string
    | REFERENCE of string * int
  type instruction =
    | ADDQ of arg * arg
    | SUBQ of arg * arg
    | MOVQ of arg * arg
    | CALLQ of string  (* label *)
    | NEGQ of arg
    | RETQ of arg
    | PUSHQ of arg
    | POPQ of arg
    | LEAVEQ
end = Assembly

type t =
  | T_BOOL
  | T_INT

type program =
  | Program of Standard.expression
  | FlatProgram of string list * Flat.statement list * Flat.argument
  | SelectProgram of string list * Select.instruction list * Select.instruction
  | AssemblyProgram of Assembly.instruction list
