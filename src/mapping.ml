open Ast

type mapping = string * expression

type env = mapping list

let extend_env (env : env) (mapping : mapping) : env =
  mapping :: env

let exists_in_env (env : env) (name : string) : bool =
  List.exists (fun mapping ->
    let name', _ = mapping in
    name = name') env
