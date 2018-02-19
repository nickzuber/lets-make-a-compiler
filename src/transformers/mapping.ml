open Ast
open Ast.Assembly
open Polyfill

let naive_interference_ts = ref 0.

module rec Liveness_mapping : sig
  type t = (Select.instruction, Select.arg Set.t) Hashtbl.t
end = Liveness_mapping

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

let caller_save_registers =
  [REGISTER "rax";
   REGISTER "rdx";
   REGISTER "rcx";
   REGISTER "rsi";
   REGISTER "rdi";
   REGISTER "r8";
   REGISTER "r9";
   REGISTER "r10";
   REGISTER "r11"]

let callee_save_registers =
  [REGISTER "rsp";
   REGISTER "rbp";
   REGISTER "rbx";
   REGISTER "r12";
   REGISTER "r13";
   REGISTER "r14";
   REGISTER "r15"]

(* Get the arguments that are considered for writes, regardless of being a variable or not. *)
let get_write_args (instr : Select.instruction) : Select.arg list =
  match instr with
  | Select.SUB (src, dest) -> [dest]
  | Select.ADD (src, dest) -> [dest]
  | Select.MOV (src, dest) -> [dest]
  | Select.NEG (arg) -> [arg]
  | Select.PUSH arg -> []
  | Select.POP arg -> []
  | Select.CALL label -> []
  | Select.RET arg -> []

(* Get the arguments that are considered for reads, regardless of being a variable or not. *)
let get_read_args (instr : Select.instruction) : Select.arg list =
  match instr with
  | Select.SUB (src, dest) -> [src; dest]
  | Select.ADD (src, dest) -> [src; dest]
  | Select.MOV (src, dest) -> [src]
  | Select.NEG (arg) -> [arg]
  | Select.PUSH arg -> []
  | Select.POP arg -> []
  | Select.CALL label -> []
  | Select.RET arg -> []

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

let build_liveness_mapping (instructions : Select.instruction list) : Liveness_mapping.t =
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

let build_liveness_matrix_naive (vars : string list) (mapping : Liveness_mapping.t) =
  let start = Unix.gettimeofday () in
  let n = List.length vars in
  let vars_array = Array.of_list vars in
  (* Create a matrix with unique references as elements. *)
  let matrix = Array.init n (fun i -> Array.init n (fun i -> ref 0)) in
  let rec loop u v =
    (* Check for interference. *)
    Hashtbl.iter (fun _instr liveness ->
        let u_key = Select.VARIABLE vars_array.(u) in
        let v_key = Select.VARIABLE vars_array.(v) in
        if Set.exists liveness u_key && Set.exists liveness v_key && u <> v then
          matrix.(u).(v) := !(matrix.(u).(v)) + 1) mapping;
    if (v + 1) < n then
      loop (u) (v + 1)
    else if (u + 1) < n then
      loop (u + 1) (0)
  in loop 0 0;
  naive_interference_ts := ((Unix.gettimeofday ()) -. start);
  matrix

let print_matrix m =
  if Settings.debug_mode <> true then () else
    (Printf.printf "\n[\x1b[1mInterference Matrix\x1b[0m]";
     if Array.length m.(0) > 100 then
       print_endline "\nToo long to show."
     else
       (let spacing = String.make (Array.length m.(0) * 2 + 1) ' ' in
        (Printf.printf "\n┌%s┐\n" spacing;
         Array.iter (fun row ->
             Printf.printf "│ ";
             Array.iter (fun elem -> Printf.printf "%d " !elem) row;
             Printf.printf "│\n") m;
         Printf.printf "└%s┘\n" spacing));
     if Settings.debug_mode then Printf.printf "\x1b[90m(naive) %s\x1b[39m\n" (Time.format !naive_interference_ts))

let create (vars : string list) (instructions : Select.instruction list) : (string, Assembly.arg) Hashtbl.t * int =
  let liveness_mapping = build_liveness_mapping instructions in
  let liveness_matrix = build_liveness_matrix_naive vars liveness_mapping in
  print_matrix liveness_matrix;
  (* Map as many variables to registers as we can. *)
  let unassigned_vars, unfinished_mapping = build_variable_to_register_mapping vars in
  let spilled_variable_size = List.length unassigned_vars in
  (* At the point of a `call`, the %rsp base pointer register must be divisibly by 16.
     https://stackoverflow.com/questions/43354658/os-x-x64-stack-not-16-byte-aligned-error#comment73772561_43354658 *)
  (* Map the rest of the variables to memory *)
  let mapping = build_spilled_variable_to_offset_mapping unfinished_mapping unassigned_vars in
  (mapping, spilled_variable_size)
