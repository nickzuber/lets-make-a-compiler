open Ast
open Ast.Standard

exception Illegal_variable_reference of string
exception Incorrect_step of string

(* Given an expression, remove any macros/sugar from the expression. *)
let rec desugar (expr : expression) : expression =
  match expr with
  | When (cond, exprs) ->
    let exprs' = desugar (Begin exprs) in
    (IfExpression
       (cond,
        exprs',
        Void))
  | Begin ([]) -> Void
  | Begin (expr :: []) ->
    (LetExpression
       ("_", expr, Void))
  | Begin (expr :: rest) ->
    let rest' = desugar (Begin rest) in
    (LetExpression
       ("_", expr, rest'))
  | _ -> expr

(* Remove all macros/sugar from the given program. *)
let transform (prog : program) : program =
  let desugared_body = match prog with
    | Program expr -> desugar expr
    | _ -> raise (Incorrect_step "expected type Program") in
  Program desugared_body
