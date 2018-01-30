open Ast
open Ast.Flat

exception Illegal_variable_reference of string
exception Incorrect_step

(* Given a standard AST, we want to convert this to a new AST that omits all nesting. The end result
 * is essentially a list of statements that we want to pass on. Variable names will be converted
 * from their previous names (`x_0`, `y_2`, etc.) into something more meaningful (`let_binding_3`,
 * `add_lhs_2`, etc.). When these name changes happen, we will map the previous name to the newer
 * name so that other expressions that reference these variables will be able to correct their name. *)
let rec flatten (expr : Standard.expression) (env : (string, string) Hashtbl.t) : Flat.statement list =
  [
    Assignment ("a", Argument (Int 1));
    Assignment ("b", Argument (Int 2));
    Assignment ("c", Argument (Int 3));
    Assignment ("d", Argument (Int 4));
    Return  (Variable "d");
  ]
  (*match expr with
   | Standard.LetExpression (name, binding, body) ->
    let
  | _ -> *)


(*
*)

(*  *)
let transform (prog : program) : program =
  let env = Hashtbl.create 20 in
  let flattened_body = match prog with
    | Program expr -> flatten expr env
    | _ -> raise Incorrect_step in
  FlatProgram ([], flattened_body)
