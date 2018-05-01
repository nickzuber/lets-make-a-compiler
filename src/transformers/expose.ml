open Ast
open Ast.TypedStandard
open Polyfill

exception Illegal_variable_reference of string
exception Incorrect_step of string
exception Encountered_a_macro
exception Program_error of string
exception Unsupported of string

let rec int_of_typed_expression typed_expr =
  let (t, expr) = typed_expr in
  match expr with
  | Int n -> n
  | False -> 0
  | Void -> 0
  | True -> 1
  | LetExpression (_v, binding, _body) ->
    int_of_typed_expression binding
  | _ -> raise (Unsupported (Printf.sprintf "we don't support %s as a argument for a vector yet"
                               (Pprint_ast.string_of_typed_expression typed_expr)))

let generic_name_of_type = function
  | T_VOID -> "tag_void"
  | T_BOOL -> "tag_bool"
  | T_INT -> "tag_int"
  | T_VECTOR _ -> "tag_vector"
  | T_FUNCTION _ -> raise (Program_error "Shouldn't be trying to create a generic name for a function.")

(* *)
let rec expose (expr : typed_expression) : string option * typed_expression =
  match expr with
  | (t, Vector exprs) ->
    let names_exprs_vars = List.fold_left (fun acc expr ->
        let local_uid = Dangerous_guid.get () in
        let vector_expression_name = Printf.sprintf "ve%d" local_uid in
        let (name, binding) = expose expr in
        let (binding_t, _) = binding in
        let typed_vector_name_expr = (binding_t, Variable vector_expression_name) in
        (name, binding_t, LetExpression (vector_expression_name, binding, typed_vector_name_expr)) :: acc) [] exprs
    in
    let exprs_with_vars = List.map (fun (_n, t, e) -> (t, e)) names_exprs_vars in
    (* Length of vector * 8 + 8 for the size of the tag + 8 for length of items. *)
    let len = List.length exprs_with_vars in
    let size_plus_free_ptr =
      (T_BOOL, BinaryExpression
         ((Plus),
          (T_INT, Global "free_ptr"),
          (T_INT, Int len)))
    in
    let greater_than_fromspace_end =
      (T_BOOL, BinaryExpression
         ((Compare GreaterThan),
          size_plus_free_ptr,
          (T_INT, Global "fromspace_end")))
    in
    let uid = Dangerous_guid.get () in
    let collect_variable_name = Printf.sprintf "maybe_collect%d" uid in
    let allocate_variable_name = Printf.sprintf "allocate%d" uid in
    let allocate_variable_wrapper_name = Printf.sprintf "_UNUNSED_allocatewrapper%d" uid in
    let names_of_vec_exprs = List.map (fun typed_expr_with_name ->
        let (maybe_name, t, expr) = typed_expr_with_name in
        let name = match (maybe_name, t) with
          | (Some name, T_VECTOR _) ->  (* Vectors have already had their tags written. *)
            name
          | (Some name, _) ->  (* If not a vector, then we write the tag manually here. *)
            let items =
              [ string_of_int (int_of_tag t)
              ; string_of_int (int_of_typed_expression (t, expr)) ]
            in
            Hashtbl.add Assembler.vectors name items;
            name
          | (None, _) -> raise (Unsupported "when getting vector expression names, an expression didn't have a name. \
                                             This expression probably isn't supported (not atomic).")
        in
        name) names_exprs_vars
    in
    let vector_name = Printf.sprintf "%s%d" (generic_name_of_type t) uid in
    let items =
      [ string_of_int (int_of_tag t)
      ; string_of_int len ]
      @ names_of_vec_exprs
    in
    Hashtbl.add Assembler.vectors vector_name items;
    let exposed_expr =
      (T_VOID, Begin
         (exprs_with_vars  (* create a list of the vector expressions, like to remove them from the vector *)
          @
          [ T_VOID, LetExpression  (* check for garbage collection *)
              ((collect_variable_name),
               (T_VOID, IfExpression
                  ((greater_than_fromspace_end),
                   (T_VOID, Collect),
                   (T_VOID, Void))),
               (T_VOID, Void))
          ; t, LetExpression  (* assign the allocate call *)
              ((allocate_variable_name),
               (t, Allocate (vector_name, t, len)),
               (t, Variable allocate_variable_name))
          ]))
      (* @ vector_set_expressions))
         set all the elements of allocate to the vector expressions
         We don't need to add this any more because the fields of a vector
         as "set" when we define it up in the `.data` section of the assembly.
         We might defer writing all this `.data` writing till later and use these
         vector sets to our advantage in doing that the right way.
      *)
    in
    (* The type being returned here is T_VOID *)
    let exposed_expr' = Macros.desugar_typed exposed_expr in
    let exposed_typed_expr =
      (t, LetExpression
         ((allocate_variable_wrapper_name),
          (exposed_expr'),
          (t, Variable allocate_variable_name)))
    in
    (Some vector_name, exposed_typed_expr)
  | (t, LetExpression (name, binding, body)) ->
    let (_name, binding') = expose binding in
    let (_name, body') = expose body in
    let exposed_typed_expr = (t, LetExpression (name, binding', body')) in
    (None, exposed_typed_expr)
  | (t, Apply (caller, arguments)) ->
    let (_name, caller') = expose caller in
    let arguments' = List.fold_left (fun acc arg ->
        let (_name, arg') = expose arg in
        arg' :: acc) [] arguments in
    let exposed_typed_expr = (t, Apply (caller', arguments')) in
    (None, exposed_typed_expr)
  | (t, IfExpression (test, consequent, alternate)) ->
    let (_name, test') = expose test in
    let (_name, consequent') = expose consequent in
    let (_name, alternate') = expose alternate in
    let exposed_typed_expr = (t, IfExpression (test', consequent', alternate')) in
    (None, exposed_typed_expr)
  | (t, BinaryExpression (op, lhs, rhs)) ->
    let (_name, lhs') = expose lhs in
    let (_name, rhs') = expose rhs in
    let exposed_typed_expr = (t, BinaryExpression (op, lhs', rhs')) in
    (None, exposed_typed_expr)
  | (t, UnaryExpression (op, operand)) ->
    let (_name, operand') = expose operand in
    let exposed_typed_expr = (t, UnaryExpression (op, operand')) in
    (None, exposed_typed_expr)
  | (t, VectorRef (vec, index)) ->
    let (_name, vec') = expose vec in
    let exposed_typed_expr = (t, VectorRef (vec', index)) in
    (None, exposed_typed_expr)
  | (t, VectorSet (vec, index, value)) ->
    let (_name, vec') = expose vec in
    let (_name, value') = expose value in
    let exposed_typed_expr = (t, VectorSet (vec', index, value')) in
    (None, exposed_typed_expr)
  | (t, Global (str)) -> (None, (t, Global (str)))
  | (t, Collect) -> (None, (t, Collect))
  | (t, Allocate (gs, tt, len)) -> (None, (t, Allocate (gs, tt, len)))
  | (t, Read) -> (None, (t, Read))
  | (t, Variable s) -> (None, (t, Variable s))
  | (t, FunctionReference s) -> (None, (t, FunctionReference s))
  | (t, Int n) ->
    let exposed_typed_expr = (t, Int n) in
    let local_uid = Dangerous_guid.get () in
    let expr_name = Printf.sprintf "%s%d" (generic_name_of_type t) local_uid in
    (Some expr_name, exposed_typed_expr)
  | (t, True) ->
    let exposed_typed_expr = (t, True) in
    let local_uid = Dangerous_guid.get () in
    let expr_name = Printf.sprintf "%s%d" (generic_name_of_type t) local_uid in
    (Some expr_name, exposed_typed_expr)
  | (t, False) ->
    let exposed_typed_expr = (t, False) in
    let local_uid = Dangerous_guid.get () in
    let expr_name = Printf.sprintf "%s%d" (generic_name_of_type t) local_uid in
    (Some expr_name, exposed_typed_expr)
  | (t, Void) ->
    let exposed_typed_expr = (t, Void) in
    let local_uid = Dangerous_guid.get () in
    let expr_name = Printf.sprintf "%s%d" (generic_name_of_type t) local_uid in
    (Some expr_name, exposed_typed_expr)
  | (t, When _)
  | (t, Unless _)
  | (t, Begin _) -> raise Encountered_a_macro

(* Expose allocations and replace calls to vectors with explicit calls to allocate. *)
let transform (prog : program) : program =
  let (_name, (t, expr)) = match prog with
    | ProgramTyped typed_expr -> expose typed_expr
    | _ -> raise (Incorrect_step "expected type ProgramTyped")
  in
  ProgramTyped (t, expr)
