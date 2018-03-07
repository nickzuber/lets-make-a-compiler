open OUnit

open Ast
open Ast.Standard
open Pprint_ast

let test_if_true () =
  Program
    (IfExpression
       (True,
        Int 1,
        Int 2))
  |> Runner.test_output

let test_if_false () =
  Program
    (IfExpression
       (False,
        Int 1,
        Int 2))
  |> Runner.test_output

let test_if_not_false () =
  Program
    (IfExpression
       ((UnaryExpression (Not, False)),
        Int 1,
        Int 2))
  |> Runner.test_output

let test_if_not_true () =
  Program
    (IfExpression
       (UnaryExpression (Not, True),
        Int 1,
        Int 2))
  |> Runner.test_output

let test_nested_if_true () =
  Program
    (IfExpression
       (True,
        Int 1,
        (IfExpression
           (False,
            Int 3,
            Int 4))))
  |> Runner.test_output

let test_nested_if_true2 () =
  Program
    (IfExpression
       ((BinaryExpression
           ((Compare LessThan),
            (Int 9),
            (Int 10))),
        (Int 1),
        (Int 0)))
  |> Runner.test_output

let test_nested_if_false () =
  Program
    (IfExpression
       (UnaryExpression (Not, True),
        Int 1,
        (IfExpression
           (True,
            Int 3,
            Int 4))))
  |> Runner.test_output

let test_really_nested_if () =
  Program
    (IfExpression
       (True,
        (IfExpression
           (False,
            Int 1,
            (IfExpression
               (False,
                Int 2,
                Int 3)))),
        (IfExpression
           (False,
            Int 4,
            Int 5))))
  |> Runner.test_output

let main () = Runner.(
    print_endline ("\n[\x1b[1mcontrol\x1b[0m]");
    run test_if_true "if true" "";
    run test_if_false "if false" "";
    run test_if_not_true "if not true" "";
    run test_if_not_false "if not false" "";
    run test_nested_if_true "nested if true (int)" "";
    run test_nested_if_true2 "nested if true (bool)" "";
    run test_nested_if_false "nested if false" "";
    run test_really_nested_if "very nested if" "";
  )
