open OUnit

open Ast
open Compiler
open Pprint_ast
let test_int () =
  let actual = Ast.Flat.(
      FlatProgram
        ([],
         [],
         Int 2)
    ) |> Selectify.transform in
  let expect = Ast.Select.(
      SelectProgram
        ([],
         [],
         RET (INT 2))
    ) in
  assert_equal actual expect ~pp_diff:Runner.pprint_diff

let test_read () =
  let actual = Ast.Flat.(
      FlatProgram
        (["read_0"],
         [Assignment ("read_0", Read)],
         Variable "read_0")
    ) |> Selectify.transform in
  let expect = Ast.Select.(
      SelectProgram
        (["read_0"],
         [CALL "_read_int";
          MOV ((REGISTER "rax"), (VARIABLE "read_0"))],
         RET (VARIABLE "read_0"))
    ) in
  assert_equal actual expect ~pp_diff:Runner.pprint_diff

let test_binop1 () =
  let actual = Ast.Flat.(
      FlatProgram
        (["read_1";
          "binary_expression_0"],
         [Assignment ("read_1", Read);
          Assignment ("binary_expression_0",
                      (BinaryExpression
                         (Plus,
                          (Variable "read_1"),
                          (Int 2))))],
         Variable "binary_expression_0")
    ) |> Selectify.transform in
  let expect = Ast.Select.(
      SelectProgram
        (["read_1";
          "binary_expression_0"],
         [CALL "_read_int";
          MOV ((REGISTER "rax"), (VARIABLE "read_1"));
          MOV ((VARIABLE "read_1"), (VARIABLE "binary_expression_0"));
          ADD ((INT 2), (VARIABLE "binary_expression_0"))],
         RET (VARIABLE "binary_expression_0"))
    ) in
  assert_equal actual expect ~pp_diff:Runner.pprint_diff

let test_binop2 () =
  raise Runner.Unimplemented_testcase

let test_unnop () =
  raise Runner.Unimplemented_testcase

let test_unnop_nested () =
  raise Runner.Unimplemented_testcase

let test_letexpr1 () =
  raise Runner.Unimplemented_testcase

let test_letexpr2 () =
  raise Runner.Unimplemented_testcase

let test_letexpr_nested_diff_name () =
  raise Runner.Unimplemented_testcase

let test_complex () =
  raise Runner.Unimplemented_testcase


let main () = Runner.(
    print_endline ("\n[\x1b[1mselect\x1b[0m]");
    run test_int "int" "Should have no instructions besides returning 2";
    run test_read "read" "Should call read and store it";
    run test_binop1 "binary expression" "";
    run test_binop2 "nested binary expression" "";
    run test_unnop "unary expression" "";
    run test_unnop_nested "nested unary expression" "";
    run test_letexpr1 "let expression simple" "";
    run test_letexpr2 "let expression binop" "";
    run test_letexpr_nested_diff_name "let expression x and y" "";
    run test_complex "complex expression" "";
  )
