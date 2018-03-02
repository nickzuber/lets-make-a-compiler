open Ast
open Ast.Standard

exception Type_error of string
exception Incorrect_step of string

(* *)
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
                (Printf.sprintf "IfExpression was given types %s and %s but expected those types to be the same."
                   (Pprint_ast.string_of_type c)
                   (Pprint_ast.string_of_type a)))
     | (t, _, _) ->
       raise (Type_error
                (Printf.sprintf "IfExpression was tested with type %s but expected type bool."
                   (Pprint_ast.string_of_type t))))
  | BinaryExpression (op, lhs, rhs) ->
    let lhs_type = typecheck lhs env in
    let rhs_type = typecheck rhs env in
    (match op with
     | Plus ->
       if lhs_type = T_INT && rhs_type = T_INT then
         T_INT
       else
         raise (Type_error
                  (Printf.sprintf "BinaryExpression/Plus was given types %s and %s but expected types int and int."
                     (Pprint_ast.string_of_type lhs_type)
                     (Pprint_ast.string_of_type rhs_type)))
     | And
     | Or ->
       if lhs_type = T_BOOL && rhs_type = T_BOOL then
         T_BOOL
       else
         raise (Type_error
                  (Printf.sprintf "BinaryExpression/Or was given types %s and %s but expected types bool and bool."
                     (Pprint_ast.string_of_type lhs_type)
                     (Pprint_ast.string_of_type rhs_type)))
     | Compare cmp ->
       if lhs_type = T_INT && rhs_type = T_INT then
         T_BOOL
       else
         raise (Type_error
                  (Printf.sprintf "BinaryExpression/Compare was given types %s and %s but expected types int and int."
                     (Pprint_ast.string_of_type lhs_type)
                     (Pprint_ast.string_of_type rhs_type))))
  | UnaryExpression (op, operand) ->
    let operand_type = typecheck operand env in
    (match op with
     | Minus ->
       if operand_type = T_INT then
         T_INT
       else
         raise (Type_error
                  (Printf.sprintf "UnaryExpression/Minus was given type %s but expected type int."
                     (Pprint_ast.string_of_type operand_type)))
     | Not ->
       if operand_type = T_BOOL then
         T_BOOL
       else
         raise (Type_error
                  (Printf.sprintf "UnaryExpression/Not expression was given type %s but expected type bool."
                     (Pprint_ast.string_of_type operand_type))))
  | Int n -> T_INT
  | Read -> T_INT
  | True -> T_BOOL
  | False -> T_BOOL

(* *)
let transform (prog : program) : Ast.t =
  (* Env is a mapping from variable name to type. *)
  let env = Hashtbl.create 53 in
  match prog with
  | Program expr -> typecheck expr env
  | _ -> raise (Incorrect_step "expected type Program")
