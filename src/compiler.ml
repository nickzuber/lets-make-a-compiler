open Ast

let compile prog =
  42

let int_of_program = function
  Program prog -> compile prog

let ex1 = Program
  (BinaryExpression
    (Plus,
    (UnaryExpression
      (Minus,
      (Int 1))),
    (Int 2)))

let () = ex1
  |> int_of_program
  |> string_of_int
  |> print_endline
