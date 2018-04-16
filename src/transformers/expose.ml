open Ast
open Ast.TypedStandard

exception Illegal_variable_reference of string
exception Incorrect_step of string

(* *)
let rec expose (t : Ast.t) (expr : expression) (count : int) : typed_expression =
  match expr with
  | Vector exprs ->
    let count = ref 1 in
    let exprs_with_vars = List.fold_left (fun acc expr ->
        let name = Printf.sprintf "x_%d" !count in
        count := (!count) + 1;
        let binding = expr in
        let body = (T_VOID, Void) in
        (T_VOID, LetExpression (name, binding, body)) :: acc) [] exprs
    in
    let sugared = (T_VOID, Begin exprs_with_vars) in
    let expr = Macros.desugar_typed sugared in
    (t, Collect)
  | _ -> (t, Collect)

(* *)
let transform (prog : program) : program =
  let count = 0 in
  let (t, expr) = match prog with
    | ProgramTyped (t, expr) -> expose t expr count
    | _ -> raise (Incorrect_step "expected type ProgramTyped")
  in
  ProgramTyped (t, expr)
