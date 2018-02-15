open Ast
open Ast.Assembly

(* List of general purpose registers we can use to store variables. *)
let valid_registers =
  [REGISTER "rcx";
   REGISTER "rdx";
   REGISTER "rsi";
   REGISTER "rdi";
   REGISTER "r8";
   REGISTER "r9";
   REGISTER "r10";
   REGISTER "r11"]

(* Create a mapping from variables to registers, then return the leftover variables and the mapping. *)
let build_variable_to_register_mapping (vars : string list) : string list * (string, Assembly.arg) Hashtbl.t =
  let variable_size = List.length vars in
  let mapping = Hashtbl.create variable_size in
  (* Assign registers to variables and return the list of unassigned variables. *)
  let rec assign_mapping vars regs : string list =
    match (vars, regs) with
    | ([], _) -> []
    | (unassigned_vars, []) -> unassigned_vars
    | (var :: rest_of_vars, reg :: rest_of_regs) ->
      Hashtbl.add mapping var reg;
      assign_mapping rest_of_vars rest_of_regs in
  let unassigned_variables = assign_mapping vars valid_registers in
  (unassigned_variables, mapping)

(* Create a mapping of spilled variable to locations in memory off the stack base pointer. *)
let build_spilled_variable_to_offset_mapping (mapping : (string, Assembly.arg) Hashtbl.t) (vars : string list) : (string, Assembly.arg) Hashtbl.t =
  let spilled_variable_size = List.length vars in
  (* At the point of a `call`, the %rsp base pointer register must be divisibly by 16.
     https://stackoverflow.com/questions/43354658/os-x-x64-stack-not-16-byte-aligned-error#comment73772561_43354658 *)
  let rsp_offset_starting_point = if spilled_variable_size mod 2 = 0 then 1 else 2 in
  let rec assign_offset vars i =
    match vars with
    | [] -> ()
    | var :: rest ->
      let offset = (i * -8) in
      let rsp_register_with_offset = REFERENCE ("rbp", offset) in
      Hashtbl.add mapping var rsp_register_with_offset;
      assign_offset rest (i + 1)
  in assign_offset vars rsp_offset_starting_point;
  mapping


let create (vars : string list) : (string, Assembly.arg) Hashtbl.t * int =
  (* Map as many variables to registers as we can *)
  let unassigned_vars, unfinished_mapping = build_variable_to_register_mapping vars in
  let spilled_variable_size = List.length unassigned_vars in
  (* At the point of a `call`, the %rsp base pointer register must be divisibly by 16.
     https://stackoverflow.com/questions/43354658/os-x-x64-stack-not-16-byte-aligned-error#comment73772561_43354658 *)
  (* Map the rest of the variables to memory *)
  let mapping = build_spilled_variable_to_offset_mapping unfinished_mapping unassigned_vars in
  (mapping, spilled_variable_size)
