open Ast

exception Bad_set_instruction
exception Probably_bad_stuff of string

let padding_offset = Settings.padding_offset_for_pprint_ast

let build_offset padding : string =
  String.make padding ' '

let rec string_of_program ?(padding=0) node : string =
  match node with
  | Program expr ->
    let str = string_of_expression expr ~padding:(padding + padding_offset) in
    Printf.sprintf "%sProgram\n%s" (build_offset padding) str
  | ProgramTyped (t, expr) ->
    let str = string_of_typed_expression (t, expr) ~padding:(padding + padding_offset) in
    Printf.sprintf "%sProgramTyped \x1b[90m: %s\x1b[0m\n%s"
      (build_offset padding)
      (string_of_type t)
      str
  | FlatProgram (vars, stmts, arg, arg_t) ->
    let vars_string = string_of_variables vars ~padding:(padding + padding_offset) in
    let stmts_string = string_of_statements stmts ~padding:(padding + padding_offset) in
    let arg_string = string_of_argument arg in
    Printf.sprintf "%sFlatProgram\n%s\n%s\n%s\x1b[4mArgument:\x1b[0m\n%s%s \x1b[90m: %s\x1b[39m"
      (build_offset padding)
      vars_string
      stmts_string
      (build_offset (padding + padding_offset))
      (build_offset (padding + (padding_offset * 2)))
      arg_string
      (string_of_type arg_t)
  | SelectProgram (t, _vars, instructions, final_instruction) ->
    let instructions_string = string_of_instructions instructions ~padding:(padding) in
    Printf.sprintf "%sSelectProgram:%s\n%s%s"
      (build_offset padding)
      instructions_string
      (build_offset (padding + padding_offset))
      (string_of_instruction final_instruction)
  | AssemblyProgram (t, instructions) ->
    let instructions_string = string_of_assembly_instructions instructions ~padding:(padding) in
    Printf.sprintf "%sAssignProgram:%s"
      (build_offset padding)
      instructions_string

and string_of_expression ?(padding=0) node : string = Ast.Standard.(
    match node with
    | Begin _ -> Printf.sprintf "%sBegin _" (build_offset padding)
    | When _ -> Printf.sprintf "%sWhen _" (build_offset padding)
    | Vector exprs ->
      Printf.sprintf "%sVector%s"
        (build_offset padding)
        (List.fold_left
           (fun acc e -> Printf.sprintf "\n%s"
               (string_of_expression e ~padding:(padding + padding_offset)) ^ acc)
           ""
           exprs)
    | VectorRef (expr, i) ->
      Printf.sprintf "%sVectorRef (i: %d)\n%s"
        (build_offset padding)
        i
        (string_of_expression expr ~padding:(padding + padding_offset))
    | VectorSet _ -> Printf.sprintf "%sVectorSet TODO" (build_offset padding)
    | Void -> Printf.sprintf "%sVoid" (build_offset padding)
    | Read -> Printf.sprintf "%sRead" (build_offset padding)
    | Int n -> Printf.sprintf "%sInt: %d" (build_offset padding) n
    | True -> Printf.sprintf "%sTrue" (build_offset padding)
    | False -> Printf.sprintf "%sFalse" (build_offset padding)
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
        (string_of_expression expr ~padding:(padding + padding_offset))
    | IfExpression (t, c, a) ->
      Printf.sprintf "%sIfExpression\n%s\n%s\n%s"
        (build_offset padding)
        (string_of_expression t ~padding:(padding + padding_offset))
        (string_of_expression c ~padding:(padding + padding_offset))
        (string_of_expression a ~padding:(padding + padding_offset)))

and string_of_typed_expression ?(padding=0) node : string = Ast.TypedStandard.(
    let (t, expr) = node in
    match expr with
    | Vector exprs ->
      Printf.sprintf "%sVector \x1b[90m: %s\x1b[39m%s"
        (build_offset padding)
        (string_of_type t)
        (List.fold_left
           (fun acc e -> Printf.sprintf "\n%s"
               (string_of_typed_expression e ~padding:(padding + padding_offset)) ^ acc)
           ""
           exprs)
    | VectorRef (expr, i) ->
      Printf.sprintf "%sVectorRef (i: %d)\n%s"
        (build_offset padding)
        i
        (string_of_typed_expression expr ~padding:(padding + padding_offset))
    | VectorSet _ -> Printf.sprintf "%sVectorSet TODO" (build_offset padding)
    | Void -> Printf.sprintf "%sVoid \x1b[90m: %s\x1b[39m" (build_offset padding) (string_of_type t)
    | Read -> Printf.sprintf "%sRead \x1b[90m: %s\x1b[39m" (build_offset padding) (string_of_type t)
    | Int n -> Printf.sprintf "%sInt: %d \x1b[90m: %s\x1b[39m" (build_offset padding) n (string_of_type t)
    | True -> Printf.sprintf "%sTrue \x1b[90m: %s\x1b[39m" (build_offset padding) (string_of_type t)
    | False -> Printf.sprintf "%sFalse \x1b[90m: %s\x1b[39m" (build_offset padding) (string_of_type t)
    | Variable name -> Printf.sprintf "%sVariable: %s \x1b[90m: %s\x1b[39m" (build_offset padding) name (string_of_type t)
    | BinaryExpression (op, lhs, rhs) ->
      Printf.sprintf "%sBinaryExpression\n%s\n%s\n%s"
        (build_offset padding)
        (string_of_typed_binop op ~padding:(padding + padding_offset))
        (string_of_typed_expression lhs ~padding:(padding + padding_offset))
        (string_of_typed_expression rhs ~padding:(padding + padding_offset))
    | UnaryExpression (op, operand) ->
      Printf.sprintf "%sUnaryExpression\n%s\n%s"
        (build_offset padding)
        (string_of_typed_unop op ~padding:(padding + padding_offset))
        (string_of_typed_expression operand ~padding:(padding + padding_offset))
    | LetExpression (v, binding, expr) ->
      Printf.sprintf "%sLetExpression \x1b[90m: %s\x1b[0m\n%s\n%s\n%s"
        (build_offset padding)
        (string_of_type t)
        (build_offset (padding + padding_offset) ^ v)
        (string_of_typed_expression binding ~padding:(padding + padding_offset))
        (string_of_typed_expression expr ~padding:(padding + padding_offset))
    | IfExpression (t, c, a) ->
      Printf.sprintf "%sIfExpression\n%s\n%s\n%s"
        (build_offset padding)
        (string_of_typed_expression t ~padding:(padding + padding_offset))
        (string_of_typed_expression c ~padding:(padding + padding_offset))
        (string_of_typed_expression a ~padding:(padding + padding_offset))

    | Allocate (gs, tt, len) -> Printf.sprintf "%sAllocate \"%s\" %s (%d) \x1b[90m: %s\x1b[39m" (build_offset padding) gs (string_of_type tt) len (string_of_type t)
    | Collect -> Printf.sprintf "%sCollect \x1b[90m: %s\x1b[39m" (build_offset padding) (string_of_type t)
    | Global s -> Printf.sprintf "%sGlobal %s \x1b[90m: %s\x1b[39m" (build_offset padding) s (string_of_type t)
    | _ -> "some sugar\n")

and string_of_binop ?(padding=0) node : string = Ast.Standard.(
    match node with
    | Plus -> Printf.sprintf "%sPlus" (build_offset padding)
    | And -> Printf.sprintf "%sAnd" (build_offset padding)
    | Or -> Printf.sprintf "%sOr" (build_offset padding)
    | Compare cmp -> Printf.sprintf "%s%s" (build_offset padding) (string_of_cmps cmp))

and string_of_typed_binop ?(padding=0) node : string = Ast.TypedStandard.(
    match node with
    | Plus -> Printf.sprintf "%sPlus" (build_offset padding)
    | And -> Printf.sprintf "%sAnd" (build_offset padding)
    | Or -> Printf.sprintf "%sOr" (build_offset padding)
    | Compare cmp -> Printf.sprintf "%s%s" (build_offset padding) (string_of_typed_cmps cmp))

and string_of_cmps ?(padding=0) node : string = Ast.Standard.(
    match node with
    | Equal -> Printf.sprintf "%sEqual" (build_offset padding)
    | GreaterThan -> Printf.sprintf "%sGreaterThan" (build_offset padding)
    | LessThan -> Printf.sprintf "%sLessThan" (build_offset padding))

and string_of_typed_cmps ?(padding=0) node : string = Ast.TypedStandard.(
    match node with
    | Equal -> Printf.sprintf "%sEqual" (build_offset padding)
    | GreaterThan -> Printf.sprintf "%sGreaterThan" (build_offset padding)
    | LessThan -> Printf.sprintf "%sLessThan" (build_offset padding))

and string_of_unop ?(padding=0) node : string = Ast.Standard.(
    match node with
    | Minus -> Printf.sprintf "%sMinus" (build_offset padding)
    | Not -> Printf.sprintf "%sNot" (build_offset padding))

and string_of_typed_unop ?(padding=0) node : string = Ast.TypedStandard.(
    match node with
    | Minus -> Printf.sprintf "%sMinus" (build_offset padding)
    | Not -> Printf.sprintf "%sNot" (build_offset padding))

and string_of_statements ?(padding=0) ?(title="Statements") stmts : string = Ast.Flat.(
    let start = if title = "Statements" then
        Printf.sprintf "%s\x1b[4m%s:\x1b[0m" (build_offset padding) title
      else
        Printf.sprintf "\x1b[90m%s%s:\x1b[39m" (build_offset padding) title in
    let statements = List.fold_left (fun acc stmt ->
        let str = string_of_statement stmt ~padding:(padding + padding_offset) in
        acc ^ "\n" ^ str) "" stmts in
    start ^ statements)

and string_of_instructions ?(padding=0) ?(title="") instructions : string = Ast.Select.(
    let start = if title = "" then
        ""
      else
        Printf.sprintf "\x1b[90m%s%s:\x1b[39m" (build_offset padding) title in
    let instructions_string = List.fold_left (fun acc instr ->
        let str = string_of_instruction instr ~padding:(padding + padding_offset) in
        acc ^ "\n" ^ str) "" instructions in
    (build_offset padding) ^ start ^ instructions_string)

and string_of_assembly_instructions ?(padding=0) instructions : string = Ast.Assembly.(
    let instructions_string = List.fold_left (fun acc instr ->
        let str = string_of_assembly_instruction instr ~padding:(padding + padding_offset) in
        acc ^ "\n" ^ str) "" instructions in
    (build_offset padding) ^ instructions_string)

and string_of_variables ?(padding=0) vars : string = Ast.Flat.(
    let start = Printf.sprintf "%s\x1b[4mVariables:\x1b[0m" (build_offset padding) in
    let variables = Hashtbl.fold (fun k v acc ->
        acc ^ (Printf.sprintf "\n%s%s \x1b[90m: %s\x1b[39m"
                 (build_offset (padding + padding_offset)) k (string_of_type v))) vars "" in
    start ^ variables)

and string_of_statement ?(padding=0) node : string = Ast.Flat.(
    match node with
    | Collect ->
      Printf.sprintf "%sCollect"
        (build_offset padding)
    | Assignment (name, expr) ->
      Printf.sprintf "%sAssignment => %s, %s"
        (build_offset padding)
        (name)
        (string_of_flat_expression expr)
    | IfStatement (test, consequent, alternate) ->
      Printf.sprintf "%sIfStatement\n%s\x1b[90mtest:\x1b[39m\n%s%s\n%s\n%s"
        (build_offset padding)
        (build_offset (padding))
        (build_offset (padding + padding_offset)) (string_of_flat_expression test ~padding:(padding))
        (string_of_statements consequent ~title:"then" ~padding:(padding))
        (string_of_statements alternate ~title:"else" ~padding:(padding)))

and string_of_instruction ?(padding=0) instruction : string = Ast.Select.(
    match instruction with
    | IF_STATEMENT (t, c, a) ->
      Printf.sprintf "%sIF_STATEMENT\n\
                      %s\x1b[90mtest:\x1b[39m\n%s\
                      %s\n%s\x1b[90mthen:\x1b[39m%s\
                      %s\n%s\x1b[90melse:\x1b[39m%s\
                      %s"
        (build_offset padding)
        (build_offset padding)
        (build_offset (padding + padding_offset)) (string_of_instruction t)
        (build_offset padding)
        (build_offset padding)
        (string_of_instructions c ~padding:(padding))
        (build_offset padding)
        (build_offset padding)
        (string_of_instructions a ~padding:(padding))
    | ADD (a, b) ->
      Printf.sprintf "%sADD \t%s, %s"
        (build_offset padding)
        (string_of_arg a)
        (string_of_arg b)
    | MOV (a, b) ->
      Printf.sprintf "%sMOV \t%s, %s"
        (build_offset padding)
        (string_of_arg a)
        (string_of_arg b)
    | LEAQ (a, b) ->
      Printf.sprintf "%sLEA \t%s, %s"
        (build_offset padding)
        (string_of_arg a)
        (string_of_arg b)
    | CALL l ->
      Printf.sprintf "%sCALL \t%s"
        (build_offset padding)
        l
    | NEG a ->
      Printf.sprintf "%sNEG \t%s"
        (build_offset padding)
        (string_of_arg a)
    | RET a ->
      Printf.sprintf "%sRET \t%s"
        (build_offset padding)
        (string_of_arg a)
    | PUSH a ->
      Printf.sprintf "%sPUSH \t%s"
        (build_offset padding)
        (string_of_arg a)
    | POP a ->
      Printf.sprintf "%sPOP \t%s"
        (build_offset padding)
        (string_of_arg a)
    | SUB (a, b) ->
      Printf.sprintf "%sSUB \t%s, %s"
        (build_offset padding)
        (string_of_arg a)
        (string_of_arg b)
    | XOR (a, b) ->
      Printf.sprintf "%sXOR \t%s, %s"
        (build_offset padding)
        (string_of_arg a)
        (string_of_arg b)
    | CMP (a, b) ->
      Printf.sprintf "%sCMP \t%s, %s"
        (build_offset padding)
        (string_of_arg a)
        (string_of_arg b)
    | MOVZB (a, b) ->
      Printf.sprintf "%sMOVZB \t%s, %s"
        (build_offset padding)
        (string_of_arg a)
        (string_of_arg b)
    | JUMP (cc, l) ->
      Printf.sprintf "%s%s %s"
        (build_offset padding)
        (string_of_jump_instr cc)
        l
    | SET (cc, r) ->
      Printf.sprintf "%s%s %s"
        (build_offset padding)
        (string_of_set_instr cc)
        (string_of_arg r)
    | LABEL l ->
      Printf.sprintf "%s:"
        l)

and string_of_set_instr cc = Ast.Select.(
    match cc with
    | E -> "SET[E]"
    | G -> "SET[G]"
    | L -> "SET[L]"
    | GE -> "SET[GE]"
    | LE -> "SET[LE]"
    | Always -> (raise Bad_set_instruction))

and string_of_jump_instr cc = Ast.Select.(
    match cc with
    | E -> "JUMP[E]"
    | G -> "JUMP[G]"
    | L -> "JUMP[L]"
    | GE -> "JUMP[GE]"
    | LE -> "JUMP[LE]"
    | Always -> "JUMP[-]")

and string_of_assembly_instruction ?(padding=0) instruction : string = Ast.Assembly.(
    match instruction with
    | LEAVEQ ->
      Printf.sprintf "%sleaveq"
        (build_offset padding)
    | LEAQ (a, b) ->
      Printf.sprintf "%sleaq \t%s, %s"
        (build_offset padding)
        (string_of_assembly_arg a)
        (string_of_assembly_arg b)
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
    | XORQ (a, b) ->
      Printf.sprintf "%sxorq \t%s, %s"
        (build_offset padding)
        (string_of_assembly_arg a)
        (string_of_assembly_arg b)
    | CMPQ (a, b) ->
      Printf.sprintf "%scmpq \t%s, %s"
        (build_offset padding)
        (string_of_assembly_arg a)
        (string_of_assembly_arg b)
    | MOVZBQ (a, b) ->
      Printf.sprintf "%smovzbq \t%s, %s"
        (build_offset padding)
        (string_of_assembly_arg a)
        (string_of_assembly_arg b)
    | JUMP (cc, l) ->
      Printf.sprintf "%s%s \t%s"
        (build_offset padding)
        (string_of_jump_assembly_instr cc)
        l
    | SET (cc, r) ->
      Printf.sprintf "%s%s \t%s"
        (build_offset padding)
        (string_of_set_assembly_instr cc)
        (string_of_assembly_arg r)
    | LABEL l ->
      Printf.sprintf "%s:"
        l)

and string_of_set_assembly_instr cc = Ast.Assembly.(
    match cc with
    | E -> "sete"
    | G -> "setg"
    | L -> "setl"
    | GE -> "setge"
    | LE -> "setle"
    | Always -> (raise Bad_set_instruction))

and string_of_jump_assembly_instr cc = Ast.Assembly.(
    match cc with
    | E -> "je\t"
    | G -> "jg\t"
    | L -> "jl\t"
    | GE -> "jge"
    | LE -> "jle"
    | Always -> "jmp\t")

and string_of_arg ?(padding=0) arg : string = Ast.Select.(
    match arg with
    | INT n -> Printf.sprintf "%d" n
    | VARIABLE v -> Printf.sprintf "%s" v
    | GLOBAL s -> Printf.sprintf "GLOBAL %s" s
    | TAG s -> Printf.sprintf "TAG %s" s
    | BYTE_REGISTER r -> Printf.sprintf "%s" r
    | REGISTER r -> Printf.sprintf "%%%s" r
    | REFERENCE (r, offset) -> Printf.sprintf "%d(%%%s)" offset r)

and string_of_assembly_arg ?(padding=0) arg : string = Ast.Assembly.(
    match arg with
    | INT n -> Printf.sprintf "$%d" n
    | REGISTER r -> Printf.sprintf "%%%s" r
    | GLOBAL s -> Printf.sprintf "_%s(%%rip)" s
    | TAG s -> Printf.sprintf "%s(%%rip)" s
    | BYTE_REGISTER r -> Printf.sprintf "%%%s" r
    | REFERENCE (r, offset) -> Printf.sprintf "%d(%%%s)" offset r)

and string_of_argument ?(padding=0) node : string = Ast.Flat.(
    match node with
    | Int n -> Printf.sprintf "Int(%d)" n
    | Variable name -> Printf.sprintf "%s" name)

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
    | Allocate (gs, t, n) ->
      Printf.sprintf "Allocate \"%s\" %s (%d)"
        gs
        (string_of_type t)
        (n)
    | VectorRef (arg, i) ->
      Printf.sprintf "%s[%d]"
        (string_of_argument arg)
        (i)
    | VectorSet (v, i, b) ->
      Printf.sprintf "%s[%d] <- %s"
        (string_of_argument v)
        (i)
        (string_of_argument b)
    | Void -> "Void"
    | Global s -> Printf.sprintf "Global %s" s)

and string_of_flat_binop ?(padding=0) node : string = Ast.Flat.(
    match node with
    | Plus -> Printf.sprintf "%s+" (build_offset padding)
    | And -> Printf.sprintf "%s&&" (build_offset padding)
    | Or -> Printf.sprintf "%s||" (build_offset padding)
    | Compare cmp -> Printf.sprintf "%s%s" (build_offset padding) (string_of_cmp cmp))

and string_of_flat_unop ?(padding=0) node : string = Ast.Flat.(
    match node with
    | Not -> Printf.sprintf "%s!" (build_offset padding)
    | Minus -> Printf.sprintf "%s-" (build_offset padding))

and string_of_cmp ?(padding=0) node : string = Ast.Flat.(
    match node with
    | Equal -> Printf.sprintf "%s===" (build_offset padding)
    | GreaterThan -> Printf.sprintf "%s>" (build_offset padding)
    | LessThan -> Printf.sprintf "%s<" (build_offset padding))

and string_of_type ?(padding=0) node : string = Ast.(
    match node with
    | T_VOID -> Printf.sprintf "%svoid" (build_offset padding)
    | T_VECTOR ts ->
      Printf.sprintf "%s(%s)" (build_offset padding)
        (List.fold_right (fun t acc -> match acc with
             | "" -> Printf.sprintf "%s" (string_of_type t)
             | _ -> Printf.sprintf "%s * %s" acc (string_of_type t)) ts "")
    | T_INT -> Printf.sprintf "%sint" (build_offset padding)
    | T_BOOL -> Printf.sprintf "%sbool" (build_offset padding))

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
  let amt_of_dashes = Settings.length_of_partition_bar - (title_len / 2) in
  let end_dashes = make_dashes amt_of_dashes in
  Printf.printf "\n\x1b[36m=-=-\x1b[39m \x1b[1m%s\x1b[0m \x1b[36m%s\x1b[39m %s" title end_dashes Emoji.herb

let create_title title : string =
  let title_len = String.length title in
  let amt_of_dashes = Settings.length_of_partition_bar - (title_len / 2) in
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

let print_string_of_graph g c ts = Polyfill.(
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
    Interference_graph.G.iter_edges (fun v1 v2 ->
        let a1 = (Interference_graph.G.V.label v1) in
        let a2 = (Interference_graph.G.V.label v2) in
        let l1 = string_of_arg a1 in
        let l2 = string_of_arg a2 in
        let c1 = Hashtbl.find c a1 in
        let c2 = Hashtbl.find c a2 in
        let color1 = if Settings.use_color_coded_graph = false then "" else colors.(c1) in
        let color2 = if Settings.use_color_coded_graph = false then "" else colors.(c2) in
        Printf.printf "%s%s\x1b[39;49m <-> %s%s\x1b[39;49m\n" color1 l1 color2 l2) g)

let print_graph g c n ts = Polyfill.(
    Printf.printf "\n[\x1b[1mInterference Graph Edges\x1b[0m]\n";
    Printf.printf "\x1b[90m(coloring) %d\x1b[39m\n" (count_unique c);
    if n > 50 then
      print_endline "Too long to show."
    else
      (try
         (print_string_of_graph g c ts)
       with Invalid_argument _ ->
         let s = count_unique c in
         let msg = Printf.sprintf "With the tests you're running, the \
                                   coloring should never get this high (it was %d). \
                                   Something is probably wrong with your saturation." s in
         raise (Probably_bad_stuff msg));
    Printf.printf "\x1b[90m(graph) %s\x1b[39m\n" (Time.format ts))
