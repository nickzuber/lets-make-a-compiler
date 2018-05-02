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

let prog1 = Program
    ( []
    , (LetExpression
         ("x",
          Int 1,
          BinaryExpression
            ( Plus
            , Variable "x"
            , Int 1 ))))

let prog2 = Program
    ( []
    , LetExpression
        ("x",
         (Vector
            [ Int 1
            ; (Vector
                 [ (Vector
                      [ False ])
                 ; True
                 ])
            ; Void
            ]),
         Variable "x"))

let prog3 = Program
    ( []
    , VectorRef
        (Vector (
            [ Int 123
            ; True
            ; Void ])
        , 0))

let prog4 = Program
    ( []
    , LetExpression
        ("x"
        , (Vector (
              [ Int 321
              ; False
              ; Int 123 ]))
        , (LetExpression
             ("x"
             , (Vector (
                   [ Int 321
                   ; False
                   ; Int 123 ]))
             , (LetExpression
                  ("x"
                  , (Vector (
                        [ Int 321
                        ; False
                        ; Int 123 ]))
                  , (LetExpression
                       ("x"
                       , (Vector (
                             [ Int 321
                             ; False
                             ; Int 123 ]))
                       , (LetExpression
                            ("x"
                            , (Vector (
                                  [ Int 321
                                  ; False
                                  ; Int 123 ]))
                            , Variable "x"))))))))))

let local_defines =
  [ ( "add_nums"
    , [ ("arg1", T_INT)
      ; ("arg2", T_INT) ]
    , (BinaryExpression (Plus, Variable "arg1", Variable "arg2"))
    , T_INT)
  ; ( "max"
    , [ ("arg1", T_INT)
      ; ("arg2", T_INT) ]
    , (IfExpression
         ( (BinaryExpression (Compare GreaterThan, Variable "arg1", Variable "arg2"))
         , Variable "arg1"
         , Variable "arg2"))
    , T_INT)
  ; ( "incorrectly_defined_max"
    , [ ("arg1", T_INT)
      ; ("arg2", T_INT) ]
    , (BinaryExpression (Compare GreaterThan, Variable "arg1", Variable "arg2"))
    , T_INT) ]

let prog5 = Program
    ( local_defines
    , (BinaryExpression
         ( Plus
         , Variable "add_nums"
         , Int 1)))

let prog6 = Program
    ( local_defines
    , (Begin
         [ (Apply
              ( Variable "add_nums"
              , [Int 1; Int 2]))
         ; (Vector (
               [ Int 321
               ; False
               ; Int 123 ])) ]))

let prog7 = Program
    ( local_defines
    , (Apply
         ( Variable "add_nums"
         , [Int 2; Int 5])))

let prog8 = Program
    ( local_defines
    , (LetExpression
         ( "x"
         , (Apply
              ( Variable "add_nums"
              , [Int 2; Int 5]))
         , (Variable "x"))))

let prog9 = Program
    ( local_defines
    , (LetExpression
         ( "x"
         , (Apply
              ( Variable "add_nums"
              , [Int 2; Int 5]))
         , (BinaryExpression
              ( Plus
              , Variable "x"
              , Int 1)))))

let prog10 = Program
    ( local_defines
    , (Begin
         [ (Apply
              ( Variable "add_nums"
              , [Int 1; Int 2]))
         ; (Apply
              ( Variable "max"
              , [Int 1; Int 2]))
         ; (Vector (
               [ Int 321
               ; False
               ; Int 123 ])) ]))

let prog11 = Program
    ( local_defines
    , Apply
        ( Variable "incorrectly_defined_max"
        , [Int 1; Int 2]))

let prog12 = Program
    ( local_defines
    , Apply
        ( Variable "max"
        , [Int 10; Int 5]))

let () =
  try
    let _ = prog1 in      (* old stuff still works fine *)
    let _ = prog2 in      (* first class nested vector *)
    let _ = prog3 in      (* vector reference *)
    let _ = prog4 in      (* lots of vectors w/ gc example *)
    let _ = prog5 in      (* [RAISE] illegal function argument *)
    let _ = prog6 in      (* function and vectors together *)
    let _ = prog7 in      (* function application *)
    let _ = prog8 in      (* function application in let expression *)
    let _ = prog9 in      (* function application in variable and binexp *)
    let _ = prog10 in     (* multiple functions with vectors *)
    let _ = prog11 in     (* [RAISE] illegal fuction definition *)
    let _ = prog12 in     (*  *)
    let prog' = prog12 in
    if Settings.debug_mode then begin
      display "Current program representation";
      prog' |> display_title "Input" |> Compiler.compile_and_debug |> Compiler.compile_functions |> Compiler.run;
    end else
      Compiler.compile_and_run prog'
  with
  | Illegal_variable_reference name ->
    let msg = "variable \x1b[33m" ^ name ^ "\x1b[39m was referenced out of scope." in
    display_error "Illegal Variable Reference" msg
    |> print_endline
  | Type_error reason ->
    display_error "Type Error" reason
    |> print_endline
  | Probably_bad_stuff reason ->
    display_error "Probably Bad Stuff" reason
    |> print_endline
  | _ as e ->
    display_error "Unknown_error" "Caught an unhandled error"
    |> print_endline;
    (raise e)
