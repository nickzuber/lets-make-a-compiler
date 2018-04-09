type t =
  | T_BOOL
  | T_INT
  | T_VOID
  | T_VECTOR of t list

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
    | Void
    | Vector of expression list
    | VectorRef of expression * int
    | VectorSet of expression * int * expression
    (* macros *)
    | Begin of expression list
    | When of expression * expression list
end = Standard

module rec TypedStandard : sig
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
  type typed_expression = t * expression
  (* Exact copy of Standard expression but with types and no macros. *)
  and expression =
    | Read
    | True
    | False
    | Int of int
    | Variable of string
    | UnaryExpression of unops * typed_expression
    | BinaryExpression of binops * typed_expression * typed_expression
    | LetExpression of string * typed_expression * typed_expression
    | IfExpression of typed_expression * typed_expression * typed_expression
    | Void
    | Vector of typed_expression list
    | VectorRef of typed_expression * int
    | VectorSet of typed_expression * int * typed_expression
end = TypedStandard

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

type program =
  | Program of Standard.expression
  | ProgramTyped of TypedStandard.typed_expression
  | FlatProgram of (string, t) Hashtbl.t * Flat.statement list * Flat.argument * t
  | SelectProgram of t * (string, t) Hashtbl.t * Select.instruction list * Select.instruction
  | AssemblyProgram of t * Assembly.instruction list
