open Ast
open Ast.Assembly
open Polyfill

exception Debugging of string

module Bfs = Interference_graph.Bfs

let naive_interference_ts = ref 0.
let graph_interference_ts = ref 0.

module rec Liveness_mapping : sig
  (* An instruction mapped to a set of variables that are live at that instruction. *)
  type t = (Select.instruction, Liveness.t) Hashtbl.t
end = Liveness_mapping

and Liveness : sig
  (* A set of variables which are considered live. *)
  type t = Select.arg Set.t
end = Liveness

(* Mapping of int to general purpose registers. *)
let int_to_valid_register = Hashtbl.create 8
let _ = List.iter (fun (i, reg) -> Hashtbl.add int_to_valid_register i reg)
    [ (0, REGISTER "r8")
    ; (1, REGISTER "r9")
    ; (2, REGISTER "r10")
    ; (3, REGISTER "rdx")
    ; (4, REGISTER "rcx") ]

(* Caller-save registers should be pushed onto the stack BEFORE a function is called,
 * and restored AFTER it's done. When you do this, you need to make sure the stack is aligned
 * before you make the call instruction. *)
let caller_save_registers =
  [ REGISTER "rdi"  (* 1st function argument *)
  ; REGISTER "rsi"  (* 2st function argument *)
  ; REGISTER "rdx"  (* 3st function argument *)
  ; REGISTER "rcx"  (* 4st function argument *)
  ; REGISTER "r8"   (* 5th function argument *)
  ; REGISTER "r9"   (* 6st function argument *)
  ; REGISTER "rax"
  ; REGISTER "r10"
  ; REGISTER "r11" ]

(* Callee-save registers should be pushed onto the stack AFTER a function is called,
 * like when you're inside of the function block, and restored BEFORE it's done. *)
let callee_save_registers =  (** unused *)
  [ REGISTER "rsp"
  ; REGISTER "rbp"
  ; REGISTER "rbx"
  ; REGISTER "r12"
  ; REGISTER "r13"
  ; REGISTER "r14"
  ; REGISTER "r15" ]

(* Get the arguments that are considered for writes, regardless of being a variable or not. *)
let get_write_args (instr : Select.instruction) : Select.arg list =
  match instr with
  | Select.SUB (src, dest) -> [dest]
  | Select.ADD (src, dest) -> [dest]
  | Select.CMP (src, dest) -> [dest]
  | Select.MOV (src, dest) -> [dest]
  | Select.LEAQ (src, dest) -> [dest]
  | Select.NEG (arg) -> [arg]
  | _ -> []

(* Get the arguments that are considered for reads, regardless of being a variable or not. *)
let get_read_args (instr : Select.instruction) : Select.arg list =
  match instr with
  | Select.SUB (src, dest) -> [src; dest]
  | Select.ADD (src, dest) -> [src; dest]
  | Select.CMP (src, dest) -> [src; dest]
  | Select.MOV (src, dest) -> [src]
  | Select.LEAQ (src, dest) -> [src]
  | Select.NEG (arg) -> [arg]
  | _ -> []

(* Get the arguments that are considered for writes that are variables. *)
let get_write_variables (instr : Select.instruction) : Liveness.t =
  instr |> get_write_args |> List.filter (fun arg ->
      match arg with
      | Select.VARIABLE _ -> true
      | _ -> false) |> Set.set_of_list

(* Get the arguments that are considered for reads that are variables. *)
let get_read_variables (instr : Select.instruction) : Liveness.t =
  instr |> get_read_args |> List.filter (fun arg ->
      match arg with
      | Select.VARIABLE _ -> true
      | _ -> false) |> Set.set_of_list

(* Compute the liveness at some isntruction and add it to the given mapping. *)
let rec compute_liveness (instr : Select.instruction) (mapping : (Select.instruction, Liveness.t) Immutable_hashtbl.t) (previous_liveness : Liveness.t) =
  match instr with
  | Select.IF_STATEMENT (t, c_instrs, a_instrs) ->
    let (c_mapping, c_previous_liveness) = build_liveness_mapping c_instrs previous_liveness in
    let (a_mapping, a_previous_liveness) = build_liveness_mapping a_instrs previous_liveness in
    let liveness = Set.union c_previous_liveness a_previous_liveness in
    let new_mapping = Immutable_hashtbl.combine c_mapping a_mapping in
    let new_mapping_with_original = Immutable_hashtbl.combine new_mapping mapping in
    let new_mapping_with_original' = Immutable_hashtbl.add new_mapping_with_original instr liveness in
    (new_mapping_with_original', liveness)
  | _ ->
    let write_variables = get_write_variables instr in
    let read_variables = get_read_variables instr in
    (* Take previous liveness, subtract the stuff we write, add the stuff we read *)
    let liveness = Set.union (Set.difference previous_liveness write_variables) read_variables in
    let mapping' = Immutable_hashtbl.add mapping instr liveness in
    (mapping', liveness)

and build_liveness_mapping instructions previous_liveness =
  let reversed_instructions = List.rev instructions in
  let size = List.length instructions in
  let mapping = Immutable_hashtbl.create size in
  (* Iterate through instructions, return the final previous_liveness at the end. *)
  let rec assign instrs mapping previous_liveness =
    match instrs with
    | [] -> (mapping, previous_liveness)
    | instr :: rest ->
      (* Printf.printf "=>> %s\n<<= %d\n\n" (Pprint_ast.string_of_instruction instr) (Immutable_hashtbl.length mapping); *)
      let (mapping', previous_liveness') = compute_liveness instr mapping previous_liveness in
      assign rest mapping' previous_liveness' in
  let (mapping', final_liveness) = assign reversed_instructions mapping previous_liveness in
  (mapping', final_liveness)

let register_of_variable (var : Select.arg) rsp_offset coloring : Assembly.arg =
  let i = Hashtbl.find coloring var in
  try
    (* Use a register. *)
    Hashtbl.find int_to_valid_register i
  with
    Not_found ->
    (* Use main memory. *)
    let starting_point = i - (Hashtbl.length int_to_valid_register) in
    (* At the point of a `call`, the %rsp base pointer register must be divisibly by 16.
       https://stackoverflow.com/questions/43354658/os-x-x64-stack-not-16-byte-aligned-error#comment73772561_43354658 *)
    let offset = ((starting_point + rsp_offset) * -8) in
    let rsp_register_with_offset = REFERENCE ("rbp", offset) in
    rsp_register_with_offset

(* Create a mapping from variables to registers, then return the leftover variables and the mapping. *)
let build_variable_to_register_mapping (vars : string list) (coloring : (Select.arg, int) Hashtbl.t) : (string, Assembly.arg) Hashtbl.t * int =
  let variable_size = List.length vars in
  let mapping = Hashtbl.create variable_size in
  (* How many colors point to main memory. *)
  let spill_size = max ((count_unique coloring) - (Hashtbl.length int_to_valid_register)) 0 in
  let rsp_offset = if spill_size mod 2 = 0 then 1 else 2 in
  (* Assign registers to variables and return the list of unassigned variables. *)
  List.iter (fun name ->
      let v = Select.VARIABLE name in
      let reg = register_of_variable v rsp_offset coloring in
      Hashtbl.add mapping name reg) vars;
  (mapping, spill_size)

let build_liveness_matrix (vars : string list) (mapping : Liveness_mapping.t) =
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

let attempt_to_add_edge liveness args d graph vt =
  match d with
  | Select.VARIABLE _ ->
    Set.for_each liveness (fun v ->
        let should_add_edge = List.for_all (fun arg -> arg <> v) args in
        if should_add_edge then
          (let v' = Hashtbl.find vt v and d' = Hashtbl.find vt d in
           Interference_graph.G.add_edge graph d' v'))
  | _ -> ()  (* if dest isn't a variable, we don't consider for reg mapping *)

let build_liveness_graph (vars : string list) (mapping : Liveness_mapping.t) : Interference_graph.G.t =
  let start = Unix.gettimeofday () in
  let vars' = List.map (fun var -> Select.VARIABLE var) vars in
  let (graph, vt) = Interference_graph.init vars' in
  (* print_liveness_mapping mapping; *)
  Hashtbl.iter (fun instr liveness ->
      match instr with
      | Select.PUSH d -> attempt_to_add_edge liveness [d] d graph vt
      | Select.POP s -> attempt_to_add_edge liveness [s] s graph vt
      | Select.MOV (s, d) -> attempt_to_add_edge liveness [s; d] d graph vt
      | Select.ADD (_s, d) -> attempt_to_add_edge liveness [d] d graph vt
      | Select.SUB (_s, d) -> attempt_to_add_edge liveness [d] d graph vt
      | Select.NEG d -> attempt_to_add_edge liveness [d] d graph vt
      | Select.XOR (s, d) -> ()
      | Select.CMP (s, d) -> attempt_to_add_edge liveness [d] d graph vt
      | Select.SET (_cc, d) -> ()
      | Select.MOVZB (s, d) -> ()
      | _ -> ()) mapping;
  graph_interference_ts := ((Unix.gettimeofday ()) -. start);
  graph

(* From a hashmap of variables to saturation sets, find the variable with the highest saturation. *)
let get_variable_with_max_saturation (var_to_sat_and_adj : (Select.arg, int Set.t * Liveness.t) Hashtbl.t) : Select.arg * (int Set.t * Liveness.t) =
  let maybe_max = Hashtbl.fold (fun k v prev ->
      match prev with
      | Some (k', (sat', adj')) ->
        (let (sat, adj) = v in
         let size_v' = Set.size sat' in
         let size_v = Set.size sat in
         if size_v > size_v' then Some (k, v) else Some (k', (sat', adj')))
      | None -> Some (k, v)) var_to_sat_and_adj None in
  match maybe_max with
  | Some (k, v) -> (k, v)
  | None -> (raise Not_found)

(* Given a set, find the lowest positive integer that is not an element of the set. *)
let find_lowest_num_not_in_set set : int =
  let rec loop i = if Set.exists set i then loop (i + 1) else i in
  loop 0

(* Given an interference graph, return a mapping of ints to variables. *)
let saturate (graph : Interference_graph.G.t) : (Select.arg, int) Hashtbl.t =
  let var_to_sat_and_adj = Hashtbl.create 53 in
  let coloring = Hashtbl.create 53 in
  (* Load up the map with variables mapped to empty satuation sets. *)
  let rec loop it =
    let v = Bfs.get it in
    let var = Interference_graph.G.V.label v in
    let sat = Set.create 53 in
    let v_adj_list = Interference_graph.G.pred graph v in
    let adj_list = List.map (fun v -> Interference_graph.G.V.label v) v_adj_list in
    let adj = Set.set_of_list adj_list in
    Hashtbl.add var_to_sat_and_adj var (sat, adj);
    loop (Bfs.step it)
  in
  (try loop (Bfs.start graph) with Exit -> ());
  (* Create a copy of our var to sat/adj map so we can remove elements we've seen without
   * actually damaging the integrity of the original mapping. *)
  let vertices = Hashtbl.copy var_to_sat_and_adj in
  (* Saturation algorithm *)
  while Hashtbl.length vertices > 0 do
    let (var, (sat, adj)) = get_variable_with_max_saturation vertices in
    let color = find_lowest_num_not_in_set sat in
    Hashtbl.add coloring var color;
    (* Adjust the saturation of the adjacent vertices. *)
    Set.for_each adj (fun v ->
        let (sat, _adj) = Hashtbl.find var_to_sat_and_adj v in
        Set.add sat color);
    Hashtbl.remove vertices var
  done;
  coloring

(* Create a mapping between variable names (strings) to registers (memory offsets are included).
 * This is used by assign to turn our Select program into an Assembly program. *)
let create ?(quiet=false) (vars : (string, Ast.t) Hashtbl.t) (instructions : Select.instruction list) : (string, Assembly.arg) Hashtbl.t * int =
  (* @TEMP after we changed vars from list to map *)
  let vars = Hashtbl.fold (fun k v acc -> k :: acc) vars [] in
  (* We want to iterate backwards to compute liveness. *)
  let empty_liveness = Set.create 0 in
  let (liveness_mapping, _final_liveness) = build_liveness_mapping instructions empty_liveness in
  let liveness_graph = build_liveness_graph vars liveness_mapping in
  let coloring = saturate liveness_graph in
  (* Map the rest of the variables to memory *)
  let mapping, spill_size = build_variable_to_register_mapping vars coloring in
  (* [DEBUG] Used just for debugging. *)
  if Settings.compute_liveness_matrix then
    (let liveness_matrix = build_liveness_matrix vars liveness_mapping in
     Pprint_ast.print_matrix liveness_matrix !naive_interference_ts);
  if Settings.debug_mode = false || quiet = true then () else
    Pprint_ast.print_graph liveness_graph coloring (List.length vars) !graph_interference_ts;
  (mapping, spill_size)
