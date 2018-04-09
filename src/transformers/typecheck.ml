open Ast
open Ast.Standard

exception Type_error of string
exception Incorrect_step of string
exception Attempted_to_typecheck_macro

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
    ((Hashtbl.find env name), expr')
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
  | Standard.VectorRef (expr, index) ->
    let vec = match expr with
      | Standard.Vector exprs -> exprs
      | _ ->
        let (t, _) = get_typed_expression expr env in
        raise (Type_error
                 (Printf.sprintf "Attempted to dereference \x1b[1m%s\x1b[0m instead of a vector."
                    (Pprint_ast.string_of_type t))) in
    let (vec_type, typed_vectorref) = get_typed_expression (List.nth vec index) env in
    (vec_type, VectorRef ((vec_type, typed_vectorref), index))
  | Standard.VectorSet (vec, index, value) ->
    let vec = match expr with
      | Standard.Vector exprs -> exprs
      | _ ->
        let (t, _) = get_typed_expression expr env in
        raise (Type_error
                 (Printf.sprintf "Attempted to dereference \x1b[1m%s\x1b[0m instead of a vector."
                    (Pprint_ast.string_of_type t))) in
    let (vec_type, typed_vectorref) = get_typed_expression (List.nth vec index) env in
    let (value_type, typed_value) = get_typed_expression value env in
    (T_VOID, VectorSet ((vec_type, typed_vectorref), index, (value_type, typed_value)))
  | _ -> (raise Attempted_to_typecheck_macro)

(* *)
let transform (prog : program) : program =
  (* Env is a mapping from variable name to type. *)
  let env = Hashtbl.create 53 in
  let (expr_type, expr) = match prog with
    | Program expr -> get_typed_expression expr env
    | _ -> raise (Incorrect_step "expected type Program") in
  ProgramTyped (expr_type, expr)
