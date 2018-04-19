open Ast
open Ast.TypedStandard
open Polyfill

exception Illegal_variable_reference of string
exception Incorrect_step of string
exception Not_a_LetExpression
exception Encountered_a_macro
exception Unsupported of string

let rec int_of_typed_expression typed_expr =
  let (t, expr) = typed_expr in
  match expr with
  | Int n -> n
  | False -> 0
  | True -> 1
  | LetExpression (_v, binding, _body) ->
    int_of_typed_expression binding
  | _ -> raise (Unsupported (Printf.sprintf "we don't support %s as a argument for a vector yet"
                               (Pprint_ast.string_of_typed_expression typed_expr)))

let generic_name_of_type = function
  | T_VOID -> "ty_void"
  | T_BOOL -> "ty_bool"
  | T_INT -> "ty_int"
  | T_VECTOR _ -> "ty_vector"

(* *)
let rec expose (expr : typed_expression) : typed_expression =
  match expr with
  | (t, Vector exprs) ->
    let exprs_with_vars = List.fold_left (fun acc expr ->
        let local_uid = Dangerous_guid.get () in
        let vector_expression_name = Printf.sprintf "ve%d" local_uid in
        let binding = expose expr in
        let (binding_t, _) = binding in
        let typed_vector_name_expr = (binding_t, Variable vector_expression_name) in
        (binding_t, LetExpression (vector_expression_name, binding, typed_vector_name_expr)) :: acc) [] exprs
    in
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
    let vector_set_expressions = List.mapi (fun i expr ->
        match expr with
        | (t_let, LetExpression (name, binding, body)) ->
          let local_uid = Dangerous_guid.get () in
          let vector_set_name = Printf.sprintf "_UNUNSED_vs%d" local_uid in
          (T_VOID, LetExpression
             ((vector_set_name),
              (t_let, VectorSet
                 ((t, Variable allocate_variable_name),
                  (i),
                  (t_let, Variable name))),
              (T_VOID, Void)))
        | _ -> raise Not_a_LetExpression) exprs_with_vars
    in
    let names_of_vec_exprs = List.map (fun typed_expr ->
        let (t, expr) = typed_expr in
        let local_uid = Dangerous_guid.get () in
        let expr_name = Printf.sprintf "%s%d" (generic_name_of_type t) local_uid in
        let items =
          [ string_of_int (int_of_tag t)
          ; string_of_int (int_of_typed_expression typed_expr) ]
        in
        Hashtbl.add Assembler.vectors expr_name items;
        expr_name) exprs_with_vars
    in
    let vector_name = Printf.sprintf "%s%d" (generic_name_of_type t) uid in
    let items =
      [ string_of_int (int_of_tag t)
      ; string_of_int len ]
      @
      names_of_vec_exprs
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
      (* @
         vector_set_expressions))
      *)
      (* set all the elements of allocate to the vector expressions
         We don't need to add this any more because the fields of a vector
         as "set" when we define it up in the `.data` section of the assembly.
         We might defer writing all this `.data` writing till later and use these
         vector sets to our advantage in doing that the right way.
      *)
    in
    (* The type being returned here is T_VOID *)
    let exposed_expr' = Macros.desugar_typed exposed_expr in
    (t, LetExpression
       ((allocate_variable_wrapper_name),
        (exposed_expr'),
        (t, Variable allocate_variable_name)))
  | (t, LetExpression (name, binding, body)) ->
    let binding' = expose binding in
    let body' = expose body in
    (t, LetExpression (name, binding', body'))
  | (t, IfExpression (test, consequent, alternate)) ->
    let test' = expose test in
    let consequent' = expose consequent in
    let alternate' = expose alternate in
    (t, IfExpression (test', consequent', alternate'))
  | (t, BinaryExpression (op, lhs, rhs)) ->
    let lhs' = expose lhs in
    let rhs' = expose rhs in
    (t, BinaryExpression (op, lhs', rhs'))
  | (t, UnaryExpression (op, operand)) ->
    let operand' = expose operand in
    (t, UnaryExpression (op, operand'))
  | (t, VectorRef (vec, index)) ->
    let vec' = expose vec in
    (t, VectorRef (vec', index))
  | (t, VectorSet (vec, index, value)) ->
    let vec' = expose vec in
    let value' = expose value in
    (t, VectorSet (vec', index, value'))
  | (t, Global (str)) -> (t, Global (str))
  | (t, Collect) -> (t, Collect)
  | (t, Allocate (gs, tt, len)) -> (t, Allocate (gs, tt, len))
  | (t, Read) -> (t, Read)
  | (t, Variable s) -> (t, Variable s)
  | (t, Int n) -> (t, Int n)
  | (t, True) -> (t, True)
  | (t, False) -> (t, False)
  | (t, Void) -> (t, Void)
  | _ -> raise Encountered_a_macro

(* Expose allocations and replace calls to vectors with explicit calls to allocate. *)
let transform (prog : program) : program =
  let (t, expr) = match prog with
    | ProgramTyped typed_expr -> expose typed_expr
    | _ -> raise (Incorrect_step "expected type ProgramTyped")
  in
  ProgramTyped (t, expr)
