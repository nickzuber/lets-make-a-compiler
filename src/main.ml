open Ast

let prog = Program
  (BinaryExpression
    (Plus,
    (UnaryExpression
      (Minus,
      (Int 1))),
    (Int 2)))

let () =
  Compiler.run prog
