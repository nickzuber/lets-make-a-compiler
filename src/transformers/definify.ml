open Ast
open Ast.Standard

exception Incorrect_step of string
exception Program_error of string
exception Encountered_undesugared_macro

let rec definify (defines : define list) (expr : expression) : expression =
  match expr with
  | Variable name ->
    let is_a_function_reference = List.exists (fun define ->
        let (define_name, _, _, _) = define in
        if define_name = name then begin
          Hashtbl.add Assembler.defines name define;
          true
        end else
          false) defines in
    if is_a_function_reference then
      INTERNAL_FunctionVariable name
    else
      Variable name
  | LetExpression (name, binding, body) ->
    let binding' = definify defines binding in
    let body' = definify defines body in
    LetExpression (name, binding', body')
  | IfExpression (test, consequent, alternate) ->
    let test' = definify defines test in
    let consequent' = definify defines consequent in
    let alternate' = definify defines alternate in
    IfExpression (test', consequent', alternate')
  | BinaryExpression (op, lhs, rhs) ->
    let lhs' = definify defines lhs in
    let rhs' = definify defines rhs in
    BinaryExpression (op, lhs', rhs')
  | UnaryExpression (op, operand) ->
    let operand' = definify defines operand in
    UnaryExpression (op, operand')
  | Int n -> Int n
  | Read -> Read
  | True -> True
  | False -> False
  | Void -> Void
  | Vector exprs ->
    let exprs' = List.fold_left (fun acc e -> (definify defines e) :: acc) [] exprs in
    Vector exprs'
  | VectorRef (vec, index) ->
    let vec' = definify defines vec in
    VectorRef (vec', index)
  | VectorSet (vec, index, value) ->
    let vec' = definify defines vec in
    let value' = definify defines value in
    VectorSet (vec', index, value')
  | INTERNAL_FunctionVariable _ -> raise (Program_error "You can't manually call to internals.")
  | Apply (caller, arguments) ->
    let caller' = definify defines caller in
    let arguments' = List.map (fun arg -> definify defines arg) arguments in
    Apply (caller', arguments')
  | Begin _
  | When _ -> raise Encountered_undesugared_macro

let transform (prog : program) : program =
  let (defines, expr) = match prog with
    | Program (defines, expr) -> (defines, (definify defines expr))
    | _ -> raise (Incorrect_step "expected type Program") in
  Program (defines, expr)
