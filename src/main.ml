open Ast
open Ast.Standard
open Pprint_ast
open Uniquify
open Typecheck

let rec pow2 n =
  if n = 0 then
    (Int 1)
  else
    (BinaryExpression
       (Plus,
        (pow2 (n - 1)),
        (pow2 (n - 1))))

let prog = Program
    (LetExpression
       ("y",
        (LetExpression
           ("x",
            (Int 11),
            (Variable "x"))),
        (BinaryExpression
           (Plus,
            (UnaryExpression
               (Minus,
                (Int 1))),
            (Variable "y")))))

let compare_expr =
  (BinaryExpression
     ((Compare Equal),
      (Read),
      (Int 1)))

let if_expr =
  (IfExpression
     ((BinaryExpression
         ((Compare LessThan),
          (Int 9),
          (Int 10))),
      (Int 11),
      (Int 22)))

let prog2 = Program
    (IfExpression
       (compare_expr,
        if_expr,
        (Int 9000)))

let prog_tons_of_variables = Program
    (pow2 3)

let _ =
  try
    let prog' = prog2 in
    if Settings.debug_mode then
      (display "Current program representation";
       prog' |> display_title "Input" |> Compiler.compile_and_debug |> Compiler.run)
    else
      Compiler.compile_and_run prog'
  with
  | Illegal_variable_reference name ->
    let msg = "variable \x1b[33m" ^ name ^ "\x1b[39m was referenced out of scope." in
    display_error "Illegal Variable Reference" msg
    |> print_endline
  | Type_error reason ->
    display_error "Type Error" reason
    |> print_endline
  | _ as e ->
    display_error "Unknown_error" "Caught an unhandled error"
    |> print_endline;
    (raise e)
