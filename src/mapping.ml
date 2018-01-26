module Ast = Ast.Standard
open Ast

type mapping = string * int

type env = mapping list

let extend_env (env : env) (mapping : mapping) : env =
  mapping :: env

let exists_in_env (env : env) (name : string) : bool =
  List.exists (fun mapping ->
    let name', _ = mapping in
    name = name') env

let get_val (env : env) (name : string) : int =
  try
    let (_, count) = List.find (fun mapping ->
      let name', _ = mapping in
      name = name') env in
    count
  with _ -> 0
