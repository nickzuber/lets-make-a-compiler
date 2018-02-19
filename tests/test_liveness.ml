open OUnit

open Ast
open Mapping
open Pprint_ast
open Polyfill

exception Unequal_sets

let rec pow2 n = Ast.Standard.(
    if n = 0 then
      (Int 1)
    else
      (BinaryExpression
         (Plus,
          (pow2 (n - 1)),
          (pow2 (n - 1)))))

let prog_tons_of_variables = Program
    (pow2 4)

let compare_liveness_mappings h1 h2 =
  try
    Hashtbl.iter (fun instr h1_liveness ->
        let h2_liveness = Hashtbl.find h2 instr in
        if h1_liveness <> h2_liveness then (raise Unequal_sets)) h1;
    true
  with
  | Unequal_sets
  | Not_found -> false

let print_liveness_mapping liveness_mapping =
  Hashtbl.iter (fun instr liveness ->
      Printf.printf "\n\x1b[90m[%s]\x1b[39m: " (string_of_instruction instr);
      Hashtbl.iter (fun arg _ ->
          match arg with
          | Select.VARIABLE name -> Printf.printf "\n  %s" name
          | _ -> ()) liveness;
      print_endline ""
    ) liveness_mapping

let iter_select_instructions_of_program fn prog =
  let select_prog = prog |> Uniquify.transform
                    |> Flatten.transform
                    |> Selectify.transform in
  match select_prog with
  | SelectProgram (_, instrs, _) ->
    let mapping = build_liveness_mapping instrs in
    fn mapping
  | _ -> ()

let pprint_liveness_diff_verbose _formatter results =
  let (actual, expect) = results in
  Printf.printf "\n\x1b[32m┌──\x1b[39m\x1b[42;1m Expect \x1b[49;0m\n\x1b[32m:\x1b[39m\n\x1b[90m";
  print_liveness_mapping expect;
  Printf.printf "\x1b[39m";
  Printf.printf "\x1b[31m:\x1b[39m\n\x1b[31m├──\x1b[39m\x1b[41;1m Actual \x1b[49;0m\n\x1b[31m:\x1b[39m\n\x1b[90m";
  print_liveness_mapping actual;
  Printf.printf "\x1b[39m";
  print_endline "\x1b[31m: \x1b[39m";
  print_endline "\x1b[31m└──┐ \x1b[39m";
  print_endline "\x1b[31m   ↓ \x1b[39m"

let pprint_diff = if Settings.use_verbose_tests then pprint_liveness_diff_verbose else Runner.pprint_diff_quiet

let test_class_example () = Ast.Select.(
    let actual =
      [MOVQ ((INT 1), (VARIABLE "v"));
       MOVQ ((INT 46), (VARIABLE "w"));
       MOVQ ((VARIABLE "v"), (VARIABLE "x"));
       ADDQ ((INT 7), (VARIABLE "x"));
       MOVQ ((VARIABLE "x"), (VARIABLE "y"));
       ADDQ ((INT 4), (VARIABLE "y"));
       MOVQ ((VARIABLE "x"), (VARIABLE "z"));
       ADDQ ((VARIABLE "w"), (VARIABLE "z"));
       MOVQ ((VARIABLE "y"), (VARIABLE "t1"));
       NEGQ (VARIABLE "t1");
       MOVQ ((VARIABLE "z"), (VARIABLE "t2"));
       ADDQ ((VARIABLE "t1"), (VARIABLE "t2"));
       MOVQ ((VARIABLE "t2"), (REGISTER "rax"))] in
    let actual_mapping = build_liveness_mapping actual in
    let expect_mapping = Hashtbl.create 13 in
    Hashtbl.add expect_mapping
      (MOVQ ((INT 1), (VARIABLE "v")))
      (Set.set_of_list []);
    Hashtbl.add expect_mapping
      (MOVQ ((INT 46), (VARIABLE "w")))
      (Set.set_of_list [VARIABLE "v"]);
    Hashtbl.add expect_mapping
      (MOVQ ((VARIABLE "v"), (VARIABLE "x")))
      (Set.set_of_list [VARIABLE "w"; VARIABLE "v"]);
    Hashtbl.add expect_mapping
      (ADDQ ((INT 7), (VARIABLE "x")))
      (Set.set_of_list [VARIABLE "w"; VARIABLE "x";]);
    Hashtbl.add expect_mapping
      (MOVQ ((VARIABLE "x"), (VARIABLE "y")))
      (Set.set_of_list [VARIABLE "w"; VARIABLE "x"]);
    Hashtbl.add expect_mapping
      (ADDQ ((INT 4), (VARIABLE "y")))
      (Set.set_of_list [VARIABLE "w"; VARIABLE "x"; VARIABLE "y"]);
    Hashtbl.add expect_mapping
      (MOVQ ((VARIABLE "x"), (VARIABLE "z")))
      (Set.set_of_list [VARIABLE "w"; VARIABLE "x"; VARIABLE "y"]);
    Hashtbl.add expect_mapping
      (ADDQ ((VARIABLE "w"), (VARIABLE "z")))
      (Set.set_of_list [VARIABLE "w"; VARIABLE "z"; VARIABLE "y"]);
    Hashtbl.add expect_mapping
      (MOVQ ((VARIABLE "y"), (VARIABLE "t1")))
      (Set.set_of_list [VARIABLE "z"; VARIABLE "y"]);
    Hashtbl.add expect_mapping
      (NEGQ (VARIABLE "t1"))
      (Set.set_of_list [VARIABLE "z"; VARIABLE "t1"]);
    Hashtbl.add expect_mapping
      (MOVQ ((VARIABLE "z"), (VARIABLE "t2")))
      (Set.set_of_list [VARIABLE "z"; VARIABLE "t1"]);
    Hashtbl.add expect_mapping
      (ADDQ ((VARIABLE "t1"), (VARIABLE "t2")))
      (Set.set_of_list [VARIABLE "t2"; VARIABLE "t1"]);
    Hashtbl.add expect_mapping
      (MOVQ ((VARIABLE "t2"), (REGISTER "rax")))
      (Set.set_of_list [VARIABLE "t2"]);
    assert_equal actual_mapping expect_mapping ~pp_diff:pprint_diff ~cmp:compare_liveness_mappings
  )

let test_custom () =
  try
    (* Max live variables is equal to the power of 2 we use. (example) pow2 5, max is 5 *)
    let max = 4 in
    let has_failed = ref false in
    iter_select_instructions_of_program (fun mapping ->
        Hashtbl.iter (fun _instr liveness ->
            let size = Set.length liveness in
            let test = (size <= max) in
            if test <> true then has_failed := true;
            let desc = Printf.sprintf ", liveness set was larger than expected (was %d, max %d)" size max in
            assert_bool desc test) mapping) prog_tons_of_variables
  with
  | _ as e ->
    (* If this test fails, we print all of the liveness mappings to debug. Kind of messy but does the trick. *)
    iter_select_instructions_of_program print_liveness_mapping prog_tons_of_variables;
    (raise e)

let main () = Runner.(
    print_endline ("\n[\x1b[1mliveness\x1b[0m]");
    run test_class_example "class example" "";
    run test_custom "many vars, low overlap" "Should be small";
  )
