open Ast
open Polyfill

type items = string list
let vectors : ((string, items) Hashtbl.t) = Hashtbl.create 53

let create_vector_assembly (name : string) (items : string list) =
  let items = List.rev items in
  let items_string = List.fold_right (fun item acc ->
      let item_string = Printf.sprintf "\t\t.quad %s\n" item in
      acc ^ item_string) items "" in
  Printf.sprintf "\t%s:\n%s"
    name
    items_string

let rec string_of_assembly node : string =
  match node with
  | AssemblyProgram (t, instructions) ->
    let vectors_string = Hashtbl.fold (fun k v acc -> acc ^ (create_vector_assembly k v)) vectors "" in
    let instructions_string = Pprint_ast.string_of_assembly_instructions instructions ~padding:(2) in
    Printf.sprintf ".data\n%s.text\n\t.globl _main\n_main:%s\n"
      vectors_string
      instructions_string
  | _ -> ""
