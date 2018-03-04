open OUnit

open Ast
open Compiler
open Pprint_ast

let test_int () =
  let actual = Ast.Standard.(
      ProgramTyped
        (T_INT,
         Int 2)
    ) |> Flatten.transform in
  let expect = Ast.Flat.(
      FlatProgram
        ([],
         [],
         Int 2,
         T_INT)
    ) in
  assert_equal actual expect ~pp_diff:Runner.pprint_diff

let test_read () =
  let actual = Ast.Standard.(
      ProgramTyped
        (T_INT,
         Read)
    ) |> Flatten.transform in
  let expect = Ast.Flat.(
      FlatProgram
        (["read_0"],
         [(Assignment ("read_0", Read))],
         Variable "read_0",
         T_INT)
    ) in
  assert_equal actual expect ~pp_diff:Runner.pprint_diff

let test_binop1 () =
  let actual = Ast.Standard.(
      ProgramTyped
        (T_INT,
         BinaryExpression
           (Plus,
            (Int 1),
            (Int 2)))
    ) |> Flatten.transform in
  let expect = Ast.Flat.(
      FlatProgram
        (["binary_expression_0"],
         [(Assignment
             ("binary_expression_0",
              (BinaryExpression
                 (Plus,
                  (Int 1),
                  (Int 2)))))],
         Variable "binary_expression_0",
         T_INT)
    ) in
  assert_equal actual expect ~pp_diff:Runner.pprint_diff

let test_binop2 () =
  let actual = Ast.Standard.(
      ProgramTyped
        (T_INT,
         BinaryExpression
           (Plus,
            (BinaryExpression
               (Plus,
                (Int 1),
                (BinaryExpression
                   (Plus,
                    (Int 1),
                    (Int 2))))),
            (Int 2)))
    ) |> Flatten.transform in
  let expect = Ast.Flat.(
      FlatProgram
        (["binary_expression_3"; "binary_expression_1"; "binary_expression_0"],
         [(Assignment
             ("binary_expression_3",
              (BinaryExpression
                 (Plus,
                  (Int 1),
                  (Int 2)))));
          (Assignment
             ("binary_expression_1",
              (BinaryExpression
                 (Plus,
                  (Int 1),
                  (Variable "binary_expression_3")))));
          (Assignment
             ("binary_expression_0",
              (BinaryExpression
                 (Plus,
                  (Variable "binary_expression_1"),
                  (Int 2)))))],
         Variable "binary_expression_0",
         T_INT)
    ) in
  assert_equal actual expect ~pp_diff:Runner.pprint_diff

let test_unnop () =
  let actual = Ast.Standard.(
      ProgramTyped
        (T_INT,
         UnaryExpression
           (Minus,
            (Int 3)))
    ) |> Flatten.transform in
  let expect = Ast.Flat.(
      FlatProgram
        (["unary_expression_0"],
         [(Assignment
             ("unary_expression_0",
              (UnaryExpression
                 (Minus,
                  (Int 3)))))],
         Variable "unary_expression_0",
         T_INT)
    ) in
  assert_equal actual expect ~pp_diff:Runner.pprint_diff

let test_unnop_nested () =
  let actual = Ast.Standard.(
      ProgramTyped
        (T_INT,
         UnaryExpression
           (Minus,
            (UnaryExpression
               (Minus,
                (Int 3)))))
    ) |> Flatten.transform in
  let expect = Ast.Flat.(
      FlatProgram
        (["unary_expression_1"; "unary_expression_0"],
         [(Assignment
             ("unary_expression_1",
              (UnaryExpression
                 (Minus,
                  (Int 3)))));
          (Assignment
             ("unary_expression_0",
              (UnaryExpression
                 (Minus,
                  (Variable "unary_expression_1")))))],
         Variable "unary_expression_0",
         T_INT)
    ) in
  assert_equal actual expect ~pp_diff:Runner.pprint_diff

let test_letexpr1 () =
  let actual = Ast.Standard.(
      ProgramTyped
        (T_INT,
         LetExpression
           ("x_1",
            (Int 1),
            (Int 2)))
    ) |> Flatten.transform in
  let expect = Ast.Flat.(
      FlatProgram
        (["x_1"],
         [(Assignment
             ("x_1",
              (Argument (Int 1))))],
         Int 2,
         T_INT)
    ) in
  assert_equal actual expect ~pp_diff:Runner.pprint_diff

let test_letexpr2 () =
  let actual = Ast.Standard.(
      ProgramTyped
        (T_INT,
         LetExpression
           ("x_1",
            (BinaryExpression
               (Plus,
                (Int 1),
                (Int 2))),
            (Int 2)))
    ) |> Flatten.transform in
  let expect = Ast.Flat.(
      FlatProgram
        (["binary_expression_0"; "x_1"],
         [(Assignment
             ("binary_expression_0",
              (BinaryExpression
                 (Plus,
                  (Int 1),
                  (Int 2)))));
          (Assignment
             ("x_1",
              (Argument (Variable "binary_expression_0"))))],
         Int 2,
         T_INT)
    ) in
  assert_equal actual expect ~pp_diff:Runner.pprint_diff

let test_letexpr_nested_diff_name () =
  let actual = Ast.Standard.(
      ProgramTyped
        (T_INT,
         LetExpression
           ("x_1",
            (BinaryExpression
               (Plus,
                (UnaryExpression
                   (Minus,
                    (Int 3))),
                (Int 2))),
            (LetExpression
               ("y_1",
                (Int 1),
                (Variable "x_1")))))
    ) |> Flatten.transform in
  let expect = Ast.Flat.(
      FlatProgram
        (["unary_expression_1"; "binary_expression_0"; "y_1"; "x_1"],
         [(Assignment
             ("unary_expression_1",
              (UnaryExpression
                 (Minus,
                  (Int 3)))));
          (Assignment
             ("binary_expression_0",
              (BinaryExpression
                 (Plus,
                  (Variable "unary_expression_1"),
                  (Int 2)))));
          (Assignment
             ("x_1",
              (Argument (Variable "binary_expression_0"))));
          (Assignment
             ("y_1",
              (Argument (Int 1))))],
         Variable "x_1",
         T_INT)
    ) in
  assert_equal actual expect ~pp_diff:Runner.pprint_diff

let test_complex () =
  let actual = Ast.Standard.(
      ProgramTyped
        (T_INT,
         BinaryExpression
           (Plus,
            (LetExpression
               ("x_1",
                (BinaryExpression
                   (Plus,
                    (UnaryExpression
                       (Minus,
                        (Int 3))),
                    (Int 2))),
                (LetExpression
                   ("y_1",
                    (Int 1),
                    (Variable "x_1"))))),
            (UnaryExpression
               (Minus,
                (UnaryExpression
                   (Minus,
                    (BinaryExpression
                       (Plus,
                        (UnaryExpression
                           (Minus,
                            (Int 3))),
                        (Int 2)))))))))
    ) |> Flatten.transform in
  let expect = Ast.Flat.(
      FlatProgram
        (["unary_expression_2";
          "binary_expression_1";
          "y_1";
          "x_1";
          "unary_expression_8";
          "binary_expression_7";
          "unary_expression_6";
          "unary_expression_5";
          "binary_expression_0";],
         [(Assignment
             ("unary_expression_2",
              (UnaryExpression
                 (Minus,
                  (Int 3)))));
          (Assignment
             ("binary_expression_1",
              (BinaryExpression
                 (Plus,
                  (Variable "unary_expression_2"),
                  (Int 2)))));
          (Assignment
             ("x_1",
              (Argument (Variable "binary_expression_1"))));
          (Assignment
             ("y_1",
              (Argument (Int 1))));
          (Assignment
             ("unary_expression_8",
              (UnaryExpression
                 (Minus,
                  (Int 3)))));
          (Assignment
             ("binary_expression_7",
              (BinaryExpression
                 (Plus,
                  (Variable "unary_expression_8"),
                  (Int 2)))));
          (Assignment
             ("unary_expression_6",
              (UnaryExpression
                 (Minus,
                  (Variable "binary_expression_7")))));
          (Assignment
             ("unary_expression_5",
              (UnaryExpression
                 (Minus,
                  (Variable "unary_expression_6")))));
          (Assignment
             ("binary_expression_0",
              (BinaryExpression
                 (Plus,
                  (Variable "x_1"),
                  (Variable "unary_expression_5")))));

         ],
         Variable "binary_expression_0",
         T_INT)
    ) in
  assert_equal actual expect ~pp_diff:Runner.pprint_diff

let main () = Runner.(
    print_endline ("\n[\x1b[1mflatten\x1b[0m]");
    run test_int "int" "Should have nothing and return int";
    run test_read "read" "Should make and return variable for read";
    run test_binop1 "binary expression" "Should make and return variable for binary expression";
    run test_binop2 "nested binary expression" "Should make all binary expressions";
    run test_unnop "unary expression" "Should make and return variable for unary expression";
    run test_unnop_nested "nested unary expression" "Should make and return variable for unary expression";
    run test_letexpr1 "let expression simple" "Should make let expression and return second expr argument";
    run test_letexpr2 "let expression binop" "Should make let expression and return second expr argument";
    run test_letexpr_nested_diff_name "let expression x and y" "Should make let expression and return second expr argument";
    run test_complex "complex expression" "Should flatten accordingly";
  )
