open Ast

let padding_offset = Settings.padding_offset_for_pprint_ast

let build_offset padding : string =
  String.make padding ' '

let rec string_of_program ?(padding=0) node : string =
  match node with
  | Program expr ->
    let str = string_of_expression expr ~padding:(padding + padding_offset) in
    Printf.sprintf "%sProgram\n%s" (build_offset padding) str
  | FlatProgram (vars, stmts, arg) ->
    let vars_string = string_of_variables vars ~padding:(padding + padding_offset) in
    let stmts_string = string_of_statements stmts ~padding:(padding + padding_offset) in
    let arg_string = string_of_argument arg in
    Printf.sprintf "%sFlatProgram\n%s\n%s\n%sArgument:\n%s%s"
      (build_offset padding)
      vars_string
      stmts_string
      (build_offset (padding + padding_offset))
      (build_offset (padding + (padding_offset * 2)))
      arg_string
  | SelectProgram (_vars, instructions, final_instruction) ->
    let instructions_string = string_of_instructions instructions ~padding:(padding) in
    Printf.sprintf "%sSelectProgram:%s\n%s%s"
      (build_offset padding)
      instructions_string
      (build_offset (padding + padding_offset))
      (string_of_instruction final_instruction)
  | AssemblyProgram instructions ->
    let instructions_string = string_of_assembly_instructions instructions ~padding:(padding) in
    Printf.sprintf "%sAssignProgram:%s"
      (build_offset padding)
      instructions_string

and string_of_expression ?(padding=0) node : string = Ast.Standard.(
    match node with
    | Read -> Printf.sprintf "%sRead" (build_offset padding)
    | Int n -> Printf.sprintf "%sInt: %d" (build_offset padding) n
    | Variable name -> Printf.sprintf "%sVariable: %s" (build_offset padding) name
    | BinaryExpression (op, lhs, rhs) ->
      Printf.sprintf "%sBinaryExpression\n%s\n%s\n%s"
        (build_offset padding)
        (string_of_binop op ~padding:(padding + padding_offset))
        (string_of_expression lhs ~padding:(padding + padding_offset))
        (string_of_expression rhs ~padding:(padding + padding_offset))
    | UnaryExpression (op, operand) ->
      Printf.sprintf "%sUnaryExpression\n%s\n%s"
        (build_offset padding)
        (string_of_unop op ~padding:(padding + padding_offset))
        (string_of_expression operand ~padding:(padding + padding_offset))
    | LetExpression (v, binding, expr) ->
      Printf.sprintf "%sLetExpression\n%s\n%s\n%s"
        (build_offset padding)
        (build_offset (padding + padding_offset) ^ v)
        (string_of_expression binding ~padding:(padding + padding_offset))
        (string_of_expression expr ~padding:(padding + padding_offset)))

and string_of_binop ?(padding=0) node : string = Ast.Standard.(
    match node with
    | Plus -> Printf.sprintf "%sPlus" (build_offset padding))

and string_of_unop ?(padding=0) node : string = Ast.Standard.(
    match node with
    | Minus -> Printf.sprintf "%sMinus" (build_offset padding))

and string_of_statements ?(padding=0) stmts : string = Ast.Flat.(
    let start = (build_offset padding) ^ "Statements:" in
    let statements = List.fold_left (fun acc stmt ->
        let str = string_of_statement stmt ~padding:(padding + padding_offset) in
        acc ^ "\n" ^ str) "" stmts in
    start ^ statements
  )

and string_of_instructions ?(padding=0) instructions : string = Ast.Select.(
    let instructions_string = List.fold_left (fun acc instr ->
        let str = string_of_instruction instr ~padding:(padding + padding_offset) in
        acc ^ "\n" ^ str) "" instructions in
    (build_offset padding) ^ instructions_string
  )

and string_of_assembly_instructions ?(padding=0) instructions : string = Ast.Assembly.(
    let instructions_string = List.fold_left (fun acc instr ->
        let str = string_of_assembly_instruction instr ~padding:(padding + padding_offset) in
        acc ^ "\n" ^ str) "" instructions in
    (build_offset padding) ^ instructions_string
  )

and string_of_variables ?(padding=0) vars : string = Ast.Flat.(
    let start = (build_offset padding) ^ "Variables:" in
    let variables = List.fold_left (fun acc var ->
        let str = build_offset(padding + padding_offset) ^ var in
        acc ^ "\n" ^ str) "" vars in
    start ^ variables
  )

and string_of_statement ?(padding=0) node : string = Ast.Flat.(
    match node with
    | Assignment (name, expr) ->
      Printf.sprintf "%s%s := %s"
        (build_offset padding)
        (name)
        (string_of_flat_expression expr)
  )

and string_of_instruction ?(padding=0) instruction : string = Ast.Select.(
    match instruction with
    | ADD (a, b) ->
      Printf.sprintf "%saddq %s, %s"
        (build_offset padding)
        (string_of_arg a)
        (string_of_arg b)
    | MOV (a, b) ->
      Printf.sprintf "%smovq %s, %s"
        (build_offset padding)
        (string_of_arg a)
        (string_of_arg b)
    | CALL l ->
      Printf.sprintf "%scallq %s"
        (build_offset padding)
        l
    | NEG a ->
      Printf.sprintf "%snegq %s"
        (build_offset padding)
        (string_of_arg a)
    | RET a ->
      Printf.sprintf "%sretq %s"
        (build_offset padding)
        (string_of_arg a)
    | PUSH a ->
      Printf.sprintf "%spushq %s"
        (build_offset padding)
        (string_of_arg a)
    | POP a ->
      Printf.sprintf "%spopq %s"
        (build_offset padding)
        (string_of_arg a)
    | SUB (a, b) ->
      Printf.sprintf "%ssubq %s, %s"
        (build_offset padding)
        (string_of_arg a)
        (string_of_arg b)
  )

and string_of_assembly_instruction ?(padding=0) instruction : string = Ast.Assembly.(
    match instruction with
    | LEAVEQ ->
      Printf.sprintf "%sleaveq"
        (build_offset padding)
    | ADDQ (a, b) ->
      Printf.sprintf "%saddq \t%s, %s"
        (build_offset padding)
        (string_of_assembly_arg a)
        (string_of_assembly_arg b)
    | MOVQ (a, b) ->
      Printf.sprintf "%smovq \t%s, %s"
        (build_offset padding)
        (string_of_assembly_arg a)
        (string_of_assembly_arg b)
    | CALLQ l ->
      Printf.sprintf "%scallq \t%s"
        (build_offset padding)
        l
    | NEGQ a ->
      Printf.sprintf "%snegq \t%s"
        (build_offset padding)
        (string_of_assembly_arg a)
    | RETQ a ->
      Printf.sprintf "%sretq"
        (build_offset padding)
    | PUSHQ a ->
      Printf.sprintf "%spushq \t%s"
        (build_offset padding)
        (string_of_assembly_arg a)
    | POPQ a ->
      Printf.sprintf "%spopq \t%s"
        (build_offset padding)
        (string_of_assembly_arg a)
    | SUBQ (a, b) ->
      Printf.sprintf "%ssubq \t%s, %s"
        (build_offset padding)
        (string_of_assembly_arg a)
        (string_of_assembly_arg b)
  )

and string_of_arg ?(padding=0) arg : string = Ast.Select.(
    match arg with
    | INT n -> Printf.sprintf "%d" n
    | VARIABLE v -> Printf.sprintf "%s" v
    | REGISTER r -> Printf.sprintf "%%%s" r
  )

and string_of_assembly_arg ?(padding=0) arg : string = Ast.Assembly.(
    match arg with
    | INT n -> Printf.sprintf "$%d" n
    | REGISTER r -> Printf.sprintf "%%%s" r
    | REFERENCE (r, offset) -> Printf.sprintf "%d(%%%s)" offset r
  )

and string_of_argument ?(padding=0) node : string = Ast.Flat.(
    match node with
    | Int n -> Printf.sprintf "Int(%d)" n
    | Variable name -> Printf.sprintf "%s" name
  )

and string_of_flat_expression ?(padding=0) node : string = Ast.Flat.(
    match node with
    | Read -> "Read"
    | Argument arg -> string_of_argument arg
    | UnaryExpression (op, arg) ->
      Printf.sprintf "%s%s"
        (string_of_flat_unop op)
        (string_of_argument arg)
    | BinaryExpression (op, lhs, rhs) ->
      Printf.sprintf "%s %s %s"
        (string_of_argument lhs)
        (string_of_flat_binop op)
        (string_of_argument rhs)
  )

and string_of_flat_binop ?(padding=0) node : string = Ast.Flat.(
    match node with
    | Plus -> Printf.sprintf "%s+" (build_offset padding))

and string_of_flat_unop ?(padding=0) node : string = Ast.Flat.(
    match node with
    | Minus -> Printf.sprintf "%s(-)" (build_offset padding))

let display_title (title : string) (prog : program) : program =
  let prog_string = string_of_program prog ~padding:1 in
  print_endline ("\n[\x1b[1m" ^ title ^ "\x1b[0m]");
  if String.length prog_string > Settings.max_characters_to_show then
    print_endline "Too long to show."
  else
    print_endline prog_string;
  prog

let display_error title msg : string =
  Printf.sprintf "\n\x1b[31m✗\x1b[39m \x1b[4m%s\x1b[0m\n\n  \x1b[31m●\x1b[39m %s\n"
    title msg

let make_dashes len : string =
  let rec loop str i =
    if i > 0 then loop (str ^ "-=") (i - 1) else str
  in loop "" len

let display title : unit =
  let title_len = String.length title in
  let amt_of_dashes = 35 - (title_len / 2) in
  let end_dashes = make_dashes amt_of_dashes in
  Printf.printf "\n\x1b[36m=-=-\x1b[39m \x1b[1m%s\x1b[0m \x1b[36m%s\x1b[39m %s" title end_dashes Emoji.herb

let create_title title : string =
  let title_len = String.length title in
  let amt_of_dashes = 35 - (title_len / 2) in
  let end_dashes = make_dashes amt_of_dashes in
  Printf.sprintf "\n\x1b[36m=-=-\x1b[39m \x1b[1m%s\x1b[0m \x1b[36m%s\x1b[39m %s" title end_dashes Emoji.herb

let print_matrix m ts=
  Printf.printf "\n[\x1b[1mInterference Matrix\x1b[0m]";
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
  Printf.printf "\x1b[90m(naive) %s\x1b[39m\n" (Time.format ts)

let print_string_of_graph g ts = Polyfill.(
    let i = ref 0 in
    let colors =
      [|"\x1b[31m";
        "\x1b[32m";
        "\x1b[33m";
        "\x1b[34m";
        "\x1b[35m";
        "\x1b[91m";
        "\x1b[92m";
        "\x1b[93m";
        "\x1b[94m";
        "\x1b[95m";
        "\x1b[41m";
        "\x1b[42m";
        "\x1b[44m";
        "\x1b[45m";
        "\x1b[101m";
        "\x1b[102m";
        "\x1b[104m";
        "\x1b[105m";
        "\x1b[90m"|] in
    let tbl = Hashtbl.create (Array.length colors) in
    InterferenceGraph.G.iter_edges (fun v1 v2 ->
        let l1 = string_of_arg (InterferenceGraph.G.V.label v1)
        and l2 = string_of_arg (InterferenceGraph.G.V.label v2) in
        let color1 =
          try Hashtbl.find tbl l1 with | Not_found ->
            (if !i < (Array.length colors) then
               (let c = colors.(!i) in
                i := !i + 1;
                Hashtbl.add tbl l1 c;
                c)
             else
               "")
        and color2 = try Hashtbl.find tbl l2 with | Not_found ->
          (if !i < (Array.length colors) then
             (let c = colors.(!i) in
              i := !i + 1;
              Hashtbl.add tbl l2 c;
              c)
           else
             "") in
        Printf.printf "%s%s\x1b[39;49m <-> %s%s\x1b[39;49m\n" color1 l1 color2 l2) g)

let print_graph g n ts = Polyfill.(
    Printf.printf "\n[\x1b[1mInterference Graph Edges\x1b[0m]\n";
    if n > 50 then
      print_endline "Too long to show."
    else
      (print_string_of_graph g ts);
    Printf.printf "\x1b[90m(graph) %s\x1b[39m\n" (Time.format ts))
