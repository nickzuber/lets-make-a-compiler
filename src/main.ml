open Ast
open Pprint_ast
open Uniquify

let bxp = BinaryExpression
  (Plus,
  (Int 1),
  (Int 1))

let lxp = LetExpression
  ("y",
  (Int 1),
  bxp)

let uxp = UnaryExpression
  (Minus,
  (Int 1))

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
        lxp))))))

let display_input (prog : program) : unit =
  print_endline "\n──< \x1b[1mInput\x1b[0m >─────────────────────────────────────────\n";
  print_endline (string_of_program prog ~padding:1);
  print_endline "\n──< \x1b[1mOutput\x1b[0m >────────────────────────────────────────\n"

let display_error title msg : string =
  Printf.sprintf "\x1b[31m✗\x1b[39m \x1b[4m%s\x1b[0m\n\
    \n  \x1b[31m●\x1b[39m %s\n"
    title msg

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
