open Ast
open Ast.Standard
open Pprint_ast
open Uniquify

let prog = Program
  (LetExpression
    ("x",
    (Int 1),
    (BinaryExpression
      (Plus,
      (Variable "x"),
      (LetExpression
        ("x",
        (Int 1),
        (LetExpression
          ("y",
          (Int 1),
          (BinaryExpression
            (Plus,
            (UnaryExpression
              (Minus,
              (Int 1))),
            (Variable "x")))))))))))

let _ =
  try
    display_input prog;
    Compiler.compile_and_run prog
  with
    | Illegal_variable_reference name ->
      let msg = "variable \x1b[33m" ^ name ^ "\x1b[39m was referenced out of scope." in
      display_error "Illegal_variable_reference" msg
        |> print_endline
    | _ ->
      display_error "Unknown_error" "Caught an unhandled error"
        |> print_endline
