open Ast
open Ast.Standard
open Pprint_ast
open Uniquify

let rec pow2 n =
  if n = 0 then
    (Int 1)
  else
    (BinaryExpression
       (Plus,
        (pow2 (n - 1)),
        (pow2 (n - 1))))

let prog = Program
    (BinaryExpression
       (Plus,
        (Read),
        (Int 1)))

let prog_tons_of_variables = Program
    (pow2 3)

let _ =
  try
    let prog' = prog_tons_of_variables in
    if Settings.debug_mode then
      (display "Current program representation";
       prog' |> display_title "Input" |> Compiler.compile_and_debug |> Compiler.run)
    else
      Compiler.compile_and_run prog'
  with
  | Illegal_variable_reference name ->
    let msg = "variable \x1b[33m" ^ name ^ "\x1b[39m was referenced out of scope." in
    display_error "Illegal_variable_reference" msg
    |> print_endline
  | _ ->
    display_error "Unknown_error" "Caught an unhandled error"
    |> print_endline
