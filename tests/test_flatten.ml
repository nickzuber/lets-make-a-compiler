open OUnit

open Ast
open Compiler
open Pprint_ast

let test_int () =
  let actual = Ast.Standard.(
    Program
      (Int 2)
  ) |> Flatten.transform
    |> string_of_program in
  let expect = Ast.Flat.(
    FlatProgram
      ([],
       [],
       Int 2)
    ) |> string_of_program in
  assert_equal actual expect ~pp_diff:Runner.pprint_diff

let test_read () =
  let actual = Ast.Standard.(
    Program
      (Read)
  ) |> Flatten.transform
    |> string_of_program in
  let expect = Ast.Flat.(
    FlatProgram
      ([],
       [],
       Variable "read_0")
    ) |> string_of_program in
  assert_equal actual expect ~pp_diff:Runner.pprint_diff

let main () = Runner.(
  print_endline ("\n\x1b[1mflatten\x1b[0m");
  run test_int "int" "Should have nothing and return int";
  run test_read "read" "Should make and return variable for read";
)
