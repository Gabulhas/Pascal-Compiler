open Ast
open Format
open X86_64

(*
- os seus parâmetros efectivos

- um apontador para a tabela de activação do seu procedimento pai (pode ser quem o chamou ou pode ser uma função recursiva que espera pelo resultado)
(que existe, em virtude do resultado anterior !)

- um apontador para a tabela de activação do procedimento caller (ou seja, de quem o chamou)

- o endereço de retorno

- as suas variáveis locais




o caso interessante é o de uma variável x
sejam l o seu nível e ofs a sua posição na tabela de activação
para encontrar a tabela de activação de x, devemos seguir lvl - l vezes
o apontador de activação do pai

ou seja, se o pai está em 5, e a variável foi chamada em 9, então temos de dar 4 passos para trás para aceder à variável
ou seja, temos de dar load ao ponteiro para o pai, ir para o sítio, dar load ao ponteiro do pai, etc etc...
*)

(*Move this to different Utils file*)
exception VariableNotDeclared of string
exception SubprogramNotDeclared of string
exception TypeMismatch of string
exception ParametersMismatch of string
exception NotAnArray of string

module Smap = Map.Make (String)

(*offset, type*)
type local_env = (int * pascaltype) Smap.t

(*
    level
    function variable allocation size
    local variable names (with offsets)
    parent function (if any)
    return type
*)

type sbpAlloc = int * int * local_env * string * pascaltype
type sbpDefs = sbpAlloc Smap.t

let label_index = ref 0

let new_label_index () =
  label_index := !label_index + 1;
  !label_index

let popn n = addq (imm n) (reg rsp)
let pushn n = subq (imm n) (reg rsp)

let rec frame_walk n code =
  if n = 0 then nop else code ++ frame_walk (n - 1) code

let type_size = function
  | TypeInteger -> 8
  | TypeBoolean -> 8
  | _ -> raise (Invalid_argument "Not sizeable variable")

let rec var_offsets_and_size vars env offset_direction varstack_size =
  match vars with
  | h :: tl -> (
      match h with
      | VariableDeclaration (a, b) ->
          let size =
            (b |> Typechecker.get_simpletype |> type_size) + varstack_size
          in
          let new_env = Smap.add a (size * offset_direction, b) env in
          var_offsets_and_size tl new_env offset_direction size)
  | [] -> (env, varstack_size)

let unwrap_subprogram = function
  | ProcedureDeclaration (name, parameters, block) ->
      (name, parameters, block, SimpleType TypeNull)
  | FunctionDeclaration (name, parameters, block, tp) ->
      (name, parameters, block, tp)

let unwrap_block = function Block (a, b, c) -> (a, b, c)

let rec get_subprogram_tree parent level sbp : sbpAlloc Smap.t =
  let name, parameters, block, tp = unwrap_subprogram sbp in
  let vars, funcs, _ = unwrap_block block in

  let merge_conflict k a b =
    match (a, b) with Some x, Some y -> Some x | None, b -> b | a, None -> a
  in

  let env, stack_size = var_offsets_and_size vars Smap.empty (-1) 0 in
  let env, _ = var_offsets_and_size parameters env 1 0 in
  let thisDef = (level, stack_size, env, parent, tp) in
  let starting_env = Smap.empty |> Smap.add name thisDef in
  List.fold_left
    (fun accum x ->
      let new_env = get_subprogram_tree name (level + 1) x in
      Smap.merge merge_conflict new_env accum)
    starting_env funcs

let find_subprogram (sb_tr : sbpDefs) subprogram =
  match Smap.find_opt subprogram sb_tr with
  | Some a -> a
  | None ->
      SubprogramNotDeclared
        (Printf.sprintf "Couldn't find subprogram %s" subprogram)
      |> raise

let get_level sbp_tree name =
  let a, _, _, _, _ = find_subprogram sbp_tree name in
  a

let get_allocation_size sbp_tree name =
  let _, a, _, _, _ = find_subprogram sbp_tree name in
  a

(*Returns level * (offset, type)*)
let rec get_closest_declaration sb_tr sbp_name varname =
  match sbp_name with
  | "" ->
      VariableNotDeclared (Printf.sprintf "Couldn't find variable %s" sbp_name)
      |> raise
  | _ -> (
      let level, _, env, paren, _ = find_subprogram sb_tr sbp_name in
      match Smap.find_opt varname env with
      | Some a -> (level, a)
      | None -> get_closest_declaration sb_tr paren varname)

let print_env =
  Smap.iter (fun varname def ->
      let offset, tp = def in
      Printf.printf "\nVar: %s| Type: %s | Offset: %d" varname
        (Typechecker.type_to_string tp)
        offset)

let print_def name df =
  let level, sz, env, parent, rt = df in
  Printf.printf
    "\n----Name: %s | Level: %d | Size: %d | Parent: %s | ReturnType: %s" name
    level sz parent
    (Typechecker.type_to_string rt);
  print_env env

let print_tree tree = Smap.iter print_def tree

(*Only used for write*)

let exp_type sbtr = function
  | SUM (_, _) | SUB (_, _) | MUL (_, _) | DIV (_, _) | Integer _ ->
      SimpleType TypeInteger
  | Equ (_, _)
  | LE (_, _)
  | LT (_, _)
  | GE (_, _)
  | GT (_, _)
  | NOT _
  | AND (_, _)
  | OR (_, _)
  | B _ ->
      SimpleType TypeBoolean
  | CALL (a, _) ->
      let _, _, _, _, tp = find_subprogram sbtr a in
      tp
  (*TODO acabar*)
  | _ -> SimpleType TypeInteger

let subprogram_call sb_tr sb_name called_name comp_fun el =
  let lvl = get_level sb_tr called_name in
  let l = get_level sb_tr sb_name in
  Printf.printf "Got %d and %d" lvl l;
  let ammount_to_pop = 8 + get_allocation_size sb_tr called_name in

  printf "\npopping %d\n" ammount_to_pop;
  (*TODO if lvl is > l, porque podem ser chamadas de dentro e fora*)
  List.fold_left (fun acc e -> acc ++ comp_fun e ++ pushq (reg rdi)) nop el
  ++ movq (reg rbp) (reg rsi)
  ++ frame_walk (lvl - l) (movq (ind ~ofs:16 rsi) (reg rsi))
  ++ pushq (reg rsi)
  ++ call called_name

(*Assume-se que todos os resultados das expressões são postos no RDI*)
let rec compile_expr sb_tr sb_name e =
  (*left -> rsi | right -> rdi*)
  let binop a b e =
    compile_expr sb_tr sb_name a
    ++ pushq (reg rdi)
    ++ compile_expr sb_tr sb_name b
    ++ popq rsi ++ e
  in

  match e with
  | Integer a -> movq (imm a) (reg rdi)
  | B b -> movq (imm (if b then 1 else 0)) (reg rdi)
  | Var a ->
      let varname =
        match a with
        | EntireVar a -> a
        | IndexedVar (a, b) -> Invalid_argument "Not IMPLEMENTED yet" |> raise
      in
      let l, (ofs, tp) = get_closest_declaration sb_tr sb_name varname in
      let lvl = get_level sb_tr sb_name in
      assert (l <= lvl);
      movq (reg rbp) (reg rsi)
      ++ frame_walk (lvl - l) (movq (ind ~ofs:16 rsi) (reg rsi))
      ++ movq (ind ~ofs rsi) (reg rdi)
  | SUM (a, b) -> binop a b (addq (reg rsi) (reg rdi))
  | SUB (a, b) -> binop b a (subq (reg rsi) (reg rdi))
  | MUL (a, b) -> binop a b (imulq (reg rsi) (reg rdi))
  (*
    mov rdx, 0        ; clear dividend
    mov rax, rsi   ; dividend
    mov rcx, rdi    ; divisor
    div ecx           ; EAX = 0x80, EDX = 0x3

   *)
  | DIV (a, b) ->
      binop a b
        (movq (imm 0) (reg rdx)
        ++ movq (reg rsi) (reg rax)
        ++ movq (reg rdi) (reg rcx)
        ++ idivq (reg rcx)
        ++ movq (reg rax) (reg rdi))
  | Equ (a, b) ->
      binop a b
        (cmpq (reg rdi) (reg rsi) ++ sete (reg dil) ++ movzbq (reg dil) rdi)
  | LE (a, b) ->
      binop a b
        (cmpq (reg rdi) (reg rsi) ++ setle (reg dil) ++ movzbq (reg dil) rdi)
  | LT (a, b) ->
      binop a b
        (cmpq (reg rdi) (reg rsi) ++ setl (reg dil) ++ movzbq (reg dil) rdi)
  | GE (a, b) ->
      binop a b
        (cmpq (reg rdi) (reg rsi) ++ setge (reg dil) ++ movzbq (reg dil) rdi)
  | GT (a, b) ->
      binop a b
        (cmpq (reg rdi) (reg rsi) ++ setg (reg dil) ++ movzbq (reg dil) rdi)
  | NOT a ->
      compile_expr sb_tr sb_name a
      ++ movq (imm 1) (reg rsi)
      ++ cmpq (reg rdi) (reg rsi)
      ++ setne (reg dil)
      ++ movzbq (reg dil) rdi
  | AND (a, b) ->
      let i = new_label_index () in
      let end_lbl = sprintf "AND_END_%d" i in
      let false_result = end_lbl ^ "FALSE_RESULT" in

      compile_expr sb_tr sb_name a
      ++ movq (imm 1) (reg rsi)
      ++ cmpq (reg rdi) (reg rsi)
      ++ jne false_result
      ++ compile_expr sb_tr sb_name b
      ++ movq (imm 1) (reg rsi)
      ++ cmpq (reg rdi) (reg rsi)
      ++ je end_lbl ++ label false_result
      ++ movq (imm 0) (reg rdi)
      ++ label end_lbl
  | OR (a, b) ->
      let i = new_label_index () in
      let end_lbl = sprintf "OR_END_%d" i in

      compile_expr sb_tr sb_name a
      ++ movq (imm 1) (reg rsi)
      ++ cmpq (reg rdi) (reg rsi)
      ++ je end_lbl
      ++ compile_expr sb_tr sb_name b
      ++ movq (imm 1) (reg rsi)
      ++ cmpq (reg rdi) (reg rsi)
      ++ je end_lbl
      ++ movq (imm 0) (reg rdi)
      ++ label end_lbl
  | CALL (name, exp_list) ->
      (*TODO add something to retrieve return value*)
      comment (sprintf "---CALLING %s ----" name)
      ++ subprogram_call sb_tr sb_name name
           (compile_expr sb_tr sb_name)
           exp_list
      ++ comment (sprintf "###CALLING %s ####" name)
  | _ -> raise (Invalid_argument "NOT IMPLEMENTED YET")

let rec compile_stmt sb_tr sb_name = function
  | STMTAss (var, exp) ->
      let varname =
        match var with
        | EntireVar a -> a
        | IndexedVar (a, b) -> Invalid_argument "NOT IMPLEMENTED" |> raise
      in
      if varname = sb_name then nop (*TODO this one should be the return*)
      else
        let l, (ofs, tp) = get_closest_declaration sb_tr sb_name varname in
        let lvl = get_level sb_tr sb_name in
        compile_expr sb_tr sb_name exp
        ++ movq (reg rbp) (reg rsi)
        ++ frame_walk (lvl - l) (movq (ind ~ofs:16 rsi) (reg rsi))
        ++ movq (reg rdi) (ind ~ofs rsi)
  | STMTBlock stmt_list ->
      List.fold_left
        (fun accum x -> accum ++ compile_stmt sb_tr sb_name x)
        nop stmt_list
  | STMTFor (varname, fr, t, stmt) ->
      let fake_var = EntireVar varname in
      let fake_initial_assign = STMTAss (fake_var, fr) in
      let while_condition = LE (Var fake_var, t) in
      let while_stmt =
        STMTBlock [ stmt; STMTAss (fake_var, SUM (Var fake_var, Integer 1)) ]
      in
      let fake_stmt =
        STMTBlock
          [ fake_initial_assign; STMTWhile (while_condition, while_stmt) ]
      in
      compile_stmt sb_tr sb_name fake_stmt
      (*
            for a := 1 to 10 do 
                STMT
        
        -----------------------
        -   is the same as    -
        -----------------------
          
            a := 1;
            while a < 10 do
                STMT
                a:= a + 1;

           *)
  | STMTIf (cond, then_part, else_part) ->
      let i = new_label_index () in
      let compiled_then = compile_stmt sb_tr sb_name then_part in
      let if_lbl = sprintf "IF_%d" i in
      let if_end = if_lbl ^ "_END" in
      let if_else = if_lbl ^ "_else" in
      compile_expr sb_tr sb_name cond
      ++ movq (imm 1) (reg rsi)
      ++ cmpq (reg rdi) (reg rsi)
      ++ (match else_part with
         | None -> jne if_end ++ compiled_then
         | Some a ->
             jne if_else ++ compiled_then ++ jmp if_end ++ label if_else
             ++ compile_stmt sb_tr sb_name a)
      ++ label if_end
  (*Super Hyper Mega Important*)
  | STMTSubprogramCall (name, params_exps) ->
      comment (sprintf "---CALLING %s ----" name)
      ++ subprogram_call sb_tr sb_name name
           (compile_expr sb_tr sb_name)
           params_exps
      ++ comment (sprintf "###CALLING %s ####" name)
  | STMTWhile (cond, stmt) ->
      let i = new_label_index () in
      let while_lbl = sprintf "while_%d" i in
      let compiled_stmt = compile_stmt sb_tr sb_name stmt in
      let while_start = while_lbl ^ "START" in
      let while_end = while_lbl ^ "END" in
      label while_start
      ++ compile_expr sb_tr sb_name cond
      ++ movq (imm 1) (reg rsi)
      ++ cmpq (reg rdi) (reg rsi)
      ++ jne while_end ++ compiled_stmt ++ jmp while_start ++ label while_end
  | STMTRead var ->
      let varname =
        match var with
        | EntireVar a -> a
        | IndexedVar (a, b) -> Invalid_argument "NOT IMPLEMENTED" |> raise
      in
      if varname = sb_name then nop (*TODO this one should be the return*)
      else
        let l, (ofs, tp) = get_closest_declaration sb_tr sb_name varname in
        let lvl = get_level sb_tr sb_name in
        movq (reg rbp) (reg rsi)
        ++ frame_walk (lvl - l) (movq (ind ~ofs:16 rsi) (reg rsi))

        ++ pushq (reg rsi)
        ++ call "scan_int" ++ popq rsi
        ++ movq (reg rdi) (ind ~ofs rsi)
  (*Separado por virgulas, em que devemos ver qual o tipo de cada parametro e fazer print_int ou print dependendo disso*)
  | STMTWrite exp_list ->
      (*Adicionar suporte para strings*)
      List.fold_left
        (fun accum x ->
          comment "PRINT" ++ compile_expr sb_tr sb_name x ++ call "print_int")
        nop exp_list
  | STMTEmpty -> nop

let rec compile_subprogram sbp_tree sbp =
  let name, parameters, block, tp = unwrap_subprogram sbp in
  let _, nested_sbps, stmt = unwrap_block block in
  let label_func = label name in
  let allocation_size = get_allocation_size sbp_tree name in
  let pointer_setup = pushq (reg rsp) ++ movq (reg rsp) (reg rbp) in
  let allocate_variables = pushn allocation_size in
  let compiled_stmt = compile_stmt sbp_tree name stmt in
  let end_subprogram = popn allocation_size ++ popq rbp ++ ret in
  let compiled_subprogram =
    label_func ++ pointer_setup ++ allocate_variables ++ compiled_stmt
    ++ end_subprogram
  in
  List.fold_left
    (fun accum x -> accum ++ compile_subprogram sbp_tree x)
    compiled_subprogram nested_sbps

let helper_routines =
  let h_print_int =
    label "print_int"
    ++ movq (reg rdi) (reg rsi)
    ++ movq (ilab ".Sprint_int") (reg rdi)
    ++ movq (imm 0) (reg rax)
    ++ call "printf" ++ ret
  in
  let h_scan_int =
    label "scan_int" ++ pushn 8
    ++ xorl (reg eax) (reg eax)
    ++ movq (ilab ".SScan_int") (reg rdi)
    ++ movq (reg rsp) (reg rsi)
    ++ call "scanf" ++ popq rdi ++ ret
  in

  nop ++ h_print_int ++ h_scan_int

let helper_data =
  label ".Sprint_int" ++ string "%d\n" ++ label ".SScan_int" ++ string "%d"

let generation_pipeline program =
  let program_name, blk = match program with Program (a, b) -> (a, b) in
  let main_sbp = ProcedureDeclaration (program_name, [], blk) in
  let sbp_tree = get_subprogram_tree "" 0 main_sbp in
  let sbp_mc = compile_subprogram sbp_tree main_sbp in
  let p =
    {
      text =
        globl "main" ++ label "main"
        ++ movq (reg rsp) (reg rbp)
        ++ call program_name
        ++ movq (imm 0) (reg rax)
        (* exit *)
        ++ ret
        ++ helper_routines ++ sbp_mc;
      data = helper_data;
    }
  in
  p
