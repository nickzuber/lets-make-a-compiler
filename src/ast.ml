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
    | Vector of expression list
    | VectorRef of expression * int
    | VectorSet of expression * int * expression
    | Read
    | True
    | False
    | Int of int
    | Variable of string
    | UnaryExpression of unops * expression
    | BinaryExpression of binops * expression * expression
    | LetExpression of string * expression * expression
    | IfExpression of expression * expression * expression
    | Void
    (* macros *)
    | Begin of expression list
    | When of expression * expression list
end = Standard

(* Note: not really flat anymore with if statements. *)
module rec Flat : sig
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
    | IfStatement of expression * statement list * statement list
end = Flat

module rec Select : sig
  type cc =
    | E
    | G
    | L
    | GE
    | LE
    | Always
  type arg =
    | INT of int
    | VARIABLE of string
    | REGISTER of string
    | BYTE_REGISTER of string
  type instruction =
    | IF_STATEMENT of instruction * instruction list * instruction list
    | ADD of arg * arg
    | SUB of arg * arg
    | MOV of arg * arg
    | CALL of string (* label *)
    | NEG of arg
    | RET of arg
    | PUSH of arg
    | POP of arg
    | XOR of arg * arg
    | CMP of arg * arg
    | SET of cc * arg (* cc, byte-register *)
    | MOVZB of arg * arg (* byte-register, register *)
    | JUMP of cc * string (* cc, label *)
    | LABEL of string (* label *)
end = Select

module rec Assembly : sig
  type cc =
    | E
    | G
    | L
    | GE
    | LE
    | Always
  type arg =
    | INT of int
    | REGISTER of string
    | REFERENCE of string * int
    | BYTE_REGISTER of string
  type instruction =
    | ADDQ of arg * arg
    | SUBQ of arg * arg
    | MOVQ of arg * arg
    | CALLQ of string  (* label *)
    | NEGQ of arg
    | RETQ of arg
    | PUSHQ of arg
    | POPQ of arg
    | XORQ of arg * arg
    | CMPQ of arg * arg
    | SET of cc * arg (* cc, byte-register *)
    | MOVZBQ of arg * arg (* byte-register, register *)
    | JUMP of cc * string (* cc, label *)
    | LABEL of string (* label *)
    | LEAVEQ
end = Assembly

type t =
  | T_BOOL
  | T_INT
  | T_VOID
  | T_VECTOR of t list

type program =
  | Program of Standard.expression
  | ProgramTyped of t * Standard.expression
  | FlatProgram of string list * Flat.statement list * Flat.argument * t
  | SelectProgram of t * string list * Select.instruction list * Select.instruction
  | AssemblyProgram of t * Assembly.instruction list
