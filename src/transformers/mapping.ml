open Ast
open Ast.Assembly
open Polyfill

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

(* Get the arguments that are considered for writes, regardless of being a variable or not. *)
let get_write_args (instr : Select.instruction) : Select.arg list =
  match instr with
  | Select.SUBQ (src, dest) -> [dest]
  | Select.ADDQ (src, dest) -> [dest]
  | Select.MOVQ (src, dest) -> [dest]
  | Select.NEGQ (arg) -> [arg]
  | Select.PUSHQ arg -> []
  | Select.POPQ arg -> []
  | Select.CALLQ label -> []
  | Select.RETQ arg -> []

(* Get the arguments that are considered for reads, regardless of being a variable or not. *)
let get_read_args (instr : Select.instruction) : Select.arg list =
  match instr with
  | Select.SUBQ (src, dest) -> [src; dest]
  | Select.ADDQ (src, dest) -> [src; dest]
  | Select.MOVQ (src, dest) -> [src]
  | Select.NEGQ (arg) -> [arg]
  | Select.PUSHQ arg -> []
  | Select.POPQ arg -> []
  | Select.CALLQ label -> []
  | Select.RETQ arg -> []

(* Get the arguments that are considered for writes that are variables. *)
let get_write_variables (instr : Select.instruction) : Select.arg Set.t =
  instr |> get_write_args |> List.filter (fun arg ->
      match arg with
      | Select.VARIABLE _ -> true
      | _ -> false) |> Set.set_of_list

(* Get the arguments that are considered for reads that are variables. *)
let get_read_variables (instr : Select.instruction) : Select.arg Set.t =
  instr |> get_read_args |> List.filter (fun arg ->
      match arg with
      | Select.VARIABLE _ -> true
      | _ -> false) |> Set.set_of_list

let build_liveness_mapping (instructions : Select.instruction list) : (Select.instruction, Select.arg Set.t) Hashtbl.t =
  let variable_size = List.length instructions in
  let empty_liveness = Set.create ~n:0 in
  let mapping = Hashtbl.create variable_size in
  let rec assign instrs previous_liveness : unit =
    match instrs with
    | [] -> ()
    | instr :: rest ->
      let write_variables = get_write_variables instr in
      let read_variables = get_read_variables instr in
      (* Take previous liveness, subtract the stuff we write, add the stuff we read *)
      let liveness = Set.union (Set.difference previous_liveness write_variables) read_variables in
      Hashtbl.add mapping instr liveness;
      assign rest liveness in
  (* We want to iterate backwards to compute liveness. *)
  let reversed_instructions = List.rev instructions in
  assign reversed_instructions empty_liveness;
  mapping

(* Create a mapping from variables to registers, then return the leftover variables and the mapping. *)
let build_variable_to_register_mapping (vars : string list) : string list * (string, Assembly.arg) Hashtbl.t =
  let variable_size = List.length vars in
  let mapping = Hashtbl.create variable_size in
  (* Assign registers to variables and return the list of unassigned variables. *)
  let rec assign vars regs : string list =
    match (vars, regs) with
    | ([], _) -> []
    | (unassigned_vars, []) -> unassigned_vars
    | (var :: rest_of_vars, reg :: rest_of_regs) ->
      Hashtbl.add mapping var reg;
      assign rest_of_vars rest_of_regs in
  let unassigned_variables = assign vars valid_registers in
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

let create (vars : string list) (instructions : Select.instruction list) : (string, Assembly.arg) Hashtbl.t * int =
  print_endline "";
  let liveness_mapping = build_liveness_mapping instructions in
  (* Map as many variables to registers as we can *)
  let unassigned_vars, unfinished_mapping = build_variable_to_register_mapping vars in
  let spilled_variable_size = List.length unassigned_vars in
  (* At the point of a `call`, the %rsp base pointer register must be divisibly by 16.
     https://stackoverflow.com/questions/43354658/os-x-x64-stack-not-16-byte-aligned-error#comment73772561_43354658 *)
  (* Map the rest of the variables to memory *)
  let mapping = build_spilled_variable_to_offset_mapping unfinished_mapping unassigned_vars in
  (mapping, spilled_variable_size)
