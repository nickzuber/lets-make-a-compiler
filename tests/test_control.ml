open OUnit

open Ast
open Ast.Standard
open Pprint_ast

let test_if_true () =
  let expect = "1" in
  let prog =
    Program
      (IfExpression
         (True,
          Int 1,
          Int 2)) in
  let ans = Interpreter.parse prog in
  let msg = Printf.sprintf "expected %s but got %s" expect ans in
  assert_equal ans expect ~msg:msg

let test_if_false () =
  let expect = "2" in
  let prog =
    Program
      (IfExpression
         (False,
          Int 1,
          Int 2)) in
  let ans = Interpreter.parse prog in
  let msg = Printf.sprintf "expected %s but got %s" expect ans in
  assert_equal ans expect ~msg:msg

let test_if_not_false () =
  let expect = "1" in
  let prog =
    Program
      (IfExpression
         ((UnaryExpression (Not, False)),
          Int 1,
          Int 2)) in
  let ans = Interpreter.parse prog in
  let msg = Printf.sprintf "expected %s but got %s" expect ans in
  assert_equal ans expect ~msg:msg

let test_if_not_true () =
  let expect = "2" in
  let prog =
    Program
      (IfExpression
         (UnaryExpression (Not, True),
          Int 1,
          Int 2)) in
  let ans = Interpreter.parse prog in
  let msg = Printf.sprintf "expected %s but got %s" expect ans in
  assert_equal ans expect ~msg:msg

let test_nested_if_true () =
  let expect = "1" in
  let prog =
    Program
      (IfExpression
         (True,
          Int 1,
          (IfExpression
             (False,
              Int 3,
              Int 4)))) in
  let ans = Interpreter.parse prog in
  let msg = Printf.sprintf "expected %s but got %s" expect ans in
  assert_equal ans expect ~msg:msg

let test_nested_if_false () =
  let expect = "3" in
  let prog =
    Program
      (IfExpression
         (UnaryExpression (Not, True),
          Int 1,
          (IfExpression
             (True,
              Int 3,
              Int 4)))) in
  let ans = Interpreter.parse prog in
  let msg = Printf.sprintf "expected %s but got %s" expect ans in
  assert_equal ans expect ~msg:msg

let test_really_nested_if () =
  let expect = "3" in
  let prog =
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
              Int 5)))) in
  let ans = Interpreter.parse prog in
  let msg = Printf.sprintf "expected %s but got %s" expect ans in
  assert_equal ans expect ~msg:msg

let main () = Runner.(
    print_endline ("\n[\x1b[1mcontrol\x1b[0m]");
    run test_if_true "if true" "";
    run test_if_false "if false" "";
    run test_if_not_true "if not true" "";
    run test_if_not_false "if not false" "";
    run test_nested_if_true "nested if true" "";
    run test_nested_if_false "nested if false" "";
    run test_really_nested_if "very nested if" "";
  )
