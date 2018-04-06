open Ast
open Ast.Standard

exception Type_error of string
exception Incorrect_step of string
exception Attempted_to_typecheck_macro

(* Derive the type from a Standard expression. *)
let rec typecheck (expr : expression) (env : (string, Ast.t) Hashtbl.t) : Ast.t =
  match expr with
  | Variable name -> Hashtbl.find env name
  | LetExpression (name, binding, body) ->
    let binding_type = typecheck binding env in
    Hashtbl.add env name binding_type;
    typecheck body env
  | IfExpression (test, consequent, alternate) ->
    let test_type = typecheck test env in
    let consequent_type = typecheck consequent env in
    let alternate_type = typecheck alternate env in
    (match (test_type, consequent_type, alternate_type) with
     | (T_BOOL, c, a) when c = a -> c
     | (T_BOOL, c, a) ->
       raise (Type_error
                (Printf.sprintf "This expression was given types \x1b[1m%s\x1b[0m and \x1b[1m%s\x1b[0m but expected those types to be the same.\n\x1b[91m%s\x1b[49m"
                   (Pprint_ast.string_of_type c)
                   (Pprint_ast.string_of_type a)
                   (Pprint_ast.string_of_expression expr ~padding:4)))
     | (t, _, _) ->
       raise (Type_error
                (Printf.sprintf "This expression was tested with type \x1b[1m%s\x1b[0m but expected type bool.\n\x1b[91m%s\x1b[49m"
                   (Pprint_ast.string_of_type t)
                   (Pprint_ast.string_of_expression expr ~padding:4))))
  | BinaryExpression (op, lhs, rhs) ->
    let lhs_type = typecheck lhs env in
    let rhs_type = typecheck rhs env in
    (match op with
     | Plus ->
       if lhs_type = T_INT && rhs_type = T_INT then
         T_INT
       else
         raise (Type_error
                  (Printf.sprintf "This expression was given types \x1b[1m%s\x1b[0m and \x1b[1m%s\x1b[0m but expected types int and int.\n\x1b[91m%s\x1b[49m"
                     (Pprint_ast.string_of_type lhs_type)
                     (Pprint_ast.string_of_type rhs_type)
                     (Pprint_ast.string_of_expression expr ~padding:4)))
     | And
     | Or ->
       if lhs_type = T_BOOL && rhs_type = T_BOOL then
         T_BOOL
       else
         raise (Type_error
                  (Printf.sprintf "This expression was given types \x1b[1m%s\x1b[0m and \x1b[1m%s\x1b[0m but expected types bool and bool.\n\x1b[91m%s\x1b[49m"
                     (Pprint_ast.string_of_type lhs_type)
                     (Pprint_ast.string_of_type rhs_type)
                     (Pprint_ast.string_of_expression expr ~padding:4)))
     | Compare cmp ->
       if lhs_type = T_INT && rhs_type = T_INT then
         T_BOOL
       else
         raise (Type_error
                  (Printf.sprintf "This expression was given types \x1b[1m%s\x1b[0m and \x1b[1m%s\x1b[0m but expected types int and int.\n\x1b[91m%s\x1b[49m"
                     (Pprint_ast.string_of_type lhs_type)
                     (Pprint_ast.string_of_type rhs_type)
                     (Pprint_ast.string_of_expression expr ~padding:4))))
  | UnaryExpression (op, operand) ->
    let operand_type = typecheck operand env in
    (match op with
     | Minus ->
       if operand_type = T_INT then
         T_INT
       else
         raise (Type_error
                  (Printf.sprintf "This expression was given type \x1b[1m%s\x1b[0m but expected type int.\n\x1b[91m%s\x1b[49m"
                     (Pprint_ast.string_of_type operand_type)
                     (Pprint_ast.string_of_expression expr ~padding:4)))
     | Not ->
       if operand_type = T_BOOL then
         T_BOOL
       else
         raise (Type_error
                  (Printf.sprintf "This expression was given type \x1b[1m%s\x1b[0m but expected type bool.\n\x1b[91m%s\x1b[49m"
                     (Pprint_ast.string_of_type operand_type)
                     (Pprint_ast.string_of_expression expr ~padding:4))))
  | Int n -> T_INT
  | Read -> T_INT
  | True -> T_BOOL
  | False -> T_BOOL
  | Void -> T_VOID
  | Vector exprs -> T_VECTOR [T_INT; T_INT; T_BOOL]
  | VectorRef (vec, index) ->
    let vec' = match vec with
      | Vector exprs -> exprs
      | _ -> (raise Type_error "Type_error") in
    typecheck (List.nth vec' index) env
  | VectorSet (vec, index, value) -> T_VOID
  | _ -> (raise Attempted_to_typecheck_macro)

(* *)
let transform (prog : program) : program =
  (* Env is a mapping from variable name to type. *)
  let env = Hashtbl.create 53 in
  let (expr_type, expr) = match prog with
    | Program expr -> ((typecheck expr env), expr)
    | _ -> raise (Incorrect_step "expected type Program") in
  ProgramTyped (expr_type, expr)
