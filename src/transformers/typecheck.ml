open Ast
open Ast.Standard

exception Type_error of string
exception Program_error of string
exception Incorrect_step of string
exception Attempted_to_typecheck_macro

let hashtbl_find_with_error tbl key ?(error="") =
  try
    Hashtbl.find tbl key
  with
  | Not_found -> raise (Type_error error)

let typed_cmp_of_standard_cmp cmp =
  let open Ast.TypedStandard in
  match cmp with
  | Standard.Equal -> Equal
  | Standard.GreaterThan -> GreaterThan
  | Standard.LessThan -> LessThan

(* Derive the type from a Standard expression. *)
let rec get_typed_expression (expr : expression) (env : (string, Ast.t) Hashtbl.t) : Ast.TypedStandard.typed_expression =
  let open Ast.TypedStandard in
  match expr with
  | Standard.Variable name ->
    let expr' = Variable name in
    let error = Printf.sprintf "Could not find variable definition for \x1b[1m%s\x1b[0m within our environment." name in
    ((hashtbl_find_with_error env name ~error:error), expr')
  | Standard.LetExpression (name, binding, body) ->
    let (tb, typed_binding) = get_typed_expression binding env in
    Hashtbl.add env name tb;
    let (td, typed_body) = get_typed_expression body env in
    (td, LetExpression (name, (tb, typed_binding), (td, typed_body)))
  | Standard.IfExpression (test, consequent, alternate) ->
    let (test_type, typed_test) = get_typed_expression test env in
    let (consequent_type, typed_consequent) = get_typed_expression consequent env in
    let (alternate_type, typed_alternate) = get_typed_expression alternate env in
    (match (test_type, consequent_type, alternate_type) with
     | (T_BOOL, c, a) when c = a ->
       (consequent_type,
        IfExpression
          ((test_type, typed_test),
           (consequent_type, typed_consequent),
           (alternate_type, typed_alternate)))
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
  | Standard.BinaryExpression (op, lhs, rhs) ->
    let (lhs_type, typed_lhs) = get_typed_expression lhs env in
    let (rhs_type, typed_rhs) = get_typed_expression rhs env in
    (match op with
     | Standard.Plus ->
       if lhs_type = T_INT && rhs_type = T_INT then
         (T_INT,
          BinaryExpression
            (Plus,
             (lhs_type, typed_lhs),
             (rhs_type, typed_rhs)))
       else
         raise (Type_error
                  (Printf.sprintf "This expression was given types \x1b[1m%s\x1b[0m and \x1b[1m%s\x1b[0m but expected types int and int.\n\x1b[91m%s\x1b[49m"
                     (Pprint_ast.string_of_type lhs_type)
                     (Pprint_ast.string_of_type rhs_type)
                     (Pprint_ast.string_of_expression expr ~padding:4)))
     | Standard.And
     | Standard.Or ->
       if lhs_type = T_BOOL && rhs_type = T_BOOL then
         (T_BOOL,
          BinaryExpression
            (Or,
             (lhs_type, typed_lhs),
             (rhs_type, typed_rhs)))
       else
         raise (Type_error
                  (Printf.sprintf "This expression was given types \x1b[1m%s\x1b[0m and \x1b[1m%s\x1b[0m but expected types bool and bool.\n\x1b[91m%s\x1b[49m"
                     (Pprint_ast.string_of_type lhs_type)
                     (Pprint_ast.string_of_type rhs_type)
                     (Pprint_ast.string_of_expression expr ~padding:4)))
     | Standard.Compare cmp ->
       if lhs_type = T_INT && rhs_type = T_INT then
         (T_BOOL,
          BinaryExpression
            (Compare (typed_cmp_of_standard_cmp cmp),
             (lhs_type, typed_lhs),
             (rhs_type, typed_rhs)))
       else
         raise (Type_error
                  (Printf.sprintf "This expression was given types \x1b[1m%s\x1b[0m and \x1b[1m%s\x1b[0m but expected types int and int.\n\x1b[91m%s\x1b[49m"
                     (Pprint_ast.string_of_type lhs_type)
                     (Pprint_ast.string_of_type rhs_type)
                     (Pprint_ast.string_of_expression expr ~padding:4))))
  | Standard.UnaryExpression (op, operand) ->
    let (operand_type, typed_operand) = get_typed_expression operand env in
    (match op with
     | Standard.Minus ->
       if operand_type = T_INT then
         (T_INT,
          UnaryExpression
            (Minus,
             (operand_type, typed_operand)))
       else
         raise (Type_error
                  (Printf.sprintf "This expression was given type \x1b[1m%s\x1b[0m but expected type int.\n\x1b[91m%s\x1b[49m"
                     (Pprint_ast.string_of_type operand_type)
                     (Pprint_ast.string_of_expression expr ~padding:4)))
     | Standard.Not ->
       if operand_type = T_BOOL then
         (T_BOOL,
          UnaryExpression
            (Not,
             (operand_type, typed_operand)))
       else
         raise (Type_error
                  (Printf.sprintf "This expression was given type \x1b[1m%s\x1b[0m but expected type bool.\n\x1b[91m%s\x1b[49m"
                     (Pprint_ast.string_of_type operand_type)
                     (Pprint_ast.string_of_expression expr ~padding:4))))
  | Standard.Int n -> (T_INT, Int n)
  | Standard.Read -> (T_INT, Read)
  | Standard.True -> (T_BOOL, True)
  | Standard.False -> (T_BOOL, False)
  | Standard.Void -> (T_VOID, Void)
  | Standard.Vector exprs ->
    let typed_exprs = List.fold_left (fun acc e -> (get_typed_expression e env) :: acc) [] exprs in
    let types = List.map (fun te -> match te with (t, _) -> t) typed_exprs in
    (T_VECTOR types, Vector typed_exprs)
  | Standard.VectorRef (vec_expr, index) ->
    let vec = match vec_expr with
      | Standard.Vector exprs -> exprs
      | _ ->
        let (t, _) = get_typed_expression vec_expr env in
        raise (Type_error
                 (Printf.sprintf "Attempted to vector reference \x1b[1m%s\x1b[0m instead of a vector."
                    (Pprint_ast.string_of_type t)))
    in
    let len = List.length vec in
    let adjusted_index = (len - 1) - index in
    let typed_vectorref = get_typed_expression vec_expr env in
    let (vecref_type, _typed_vectorref) = get_typed_expression (List.nth vec adjusted_index) env in
    (vecref_type, VectorRef (typed_vectorref, adjusted_index))
  | Standard.VectorSet (vec_expr, index, value) ->
    let _ = match vec_expr with
      | Standard.Vector exprs -> exprs
      | _ ->
        let (t, _) = get_typed_expression vec_expr env in
        raise (Type_error
                 (Printf.sprintf "Attempted to vector \x1b[1m%s\x1b[0m instead of a vector."
                    (Pprint_ast.string_of_type t)))
    in
    let (tvs_t, typed_vectorset_vec) = get_typed_expression vec_expr env in
    let (value_type, typed_value) = get_typed_expression value env in
    (T_VOID, VectorSet ((tvs_t, typed_vectorset_vec), index, (value_type, typed_value)))
  | INTERNAL_FunctionVariable name ->
    let error = Printf.sprintf "Could not find function definition for \x1b[1m%s\x1b[0m within the list of defines." name in
    let (_, params_with_types, _, return_type) = hashtbl_find_with_error Assembler.defines name ~error:error in
    let param_types = List.map (fun (param, param_t) -> param_t) params_with_types in
    (T_FUNCTION (param_types, return_type), FunctionReference name)
  | Apply (call_expr, args) ->
    let name = match call_expr with
      | INTERNAL_FunctionVariable name -> name
      | Variable name -> raise (Type_error "Attempted to call a regular variable as a function.")
      | _ -> raise (Type_error "Attempted to call something that wasn't a function as a function.")
    in
    let error = Printf.sprintf "When calling apply, we could not find function definition for \x1b[1m%s\x1b[0m within the list of defines." name in
    let (_, param_and_type_list, body, return_type) = hashtbl_find_with_error Assembler.defines name ~error:error in
    (* Create a new env for the function which includes all of the previous scope and its parameters as variables. *)
    let function_env = Hashtbl.copy env in
    List.iter (fun (param, param_t) -> Hashtbl.add function_env param param_t) param_and_type_list;
    let (body_type, typed_body) = get_typed_expression body function_env in
    let typed_call_expr = get_typed_expression call_expr env in
    (* Confirm that argument types match parameter signature. *)
    let typed_args = List.map2 (fun param_with_type arg ->
        let (param, param_t) = param_with_type in
        let (arg_t, arg) = get_typed_expression arg env in
        if arg_t <> param_t then
          raise (Type_error (Printf.sprintf "The function \x1b[1m%s\x1b[0m was called with an argument of the wrong type.\
                                             \n    Got \x1b[1m%s\x1b[0m but expected \x1b[1m%s\x1b[0m."
                               name (Pprint_ast.string_of_type arg_t) (Pprint_ast.string_of_type param_t)))
        else
          (arg_t, arg)
      ) param_and_type_list args in
    (* Confirm that return type matches body expression. *)
    if body_type <> return_type then
      raise (Type_error (Printf.sprintf "The function body of \x1b[1m%s\x1b[0m claimed to have type \x1b[1m%s\x1b[0m but we got \x1b[1m%s\x1b[0m."
                           name (Pprint_ast.string_of_type body_type) (Pprint_ast.string_of_type return_type)))
    else
      (return_type, Apply (typed_call_expr, typed_args))
  | Begin _
  | When _ -> (raise Attempted_to_typecheck_macro)

(* *)
let transform (prog : program) : program =
  (* Env is a mapping from variable name to type. *)
  let env = Hashtbl.create 53 in
  (* Since this program is in the scope of a function, we add parameters as variables. *)
  Hashtbl.iter (fun _ definition ->
      let (_, params_with_types, _, _) = definition in
      List.iter (fun (param, t) ->
          let uniquified_name = param ^ "_1" in
          Hashtbl.add env uniquified_name t) params_with_types) Assembler.defines;
  let (expr_type, expr) = match prog with
    | Program (defines, expr) -> get_typed_expression expr env
    | _ -> raise (Incorrect_step "expected type Program") in
  ProgramTyped (expr_type, expr)

let safe_transform prog =
  try transform prog
  with _ -> raise (Program_error "Error occured Typecheck")
