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

let popn n = addq (imm n) (reg rsp)
let pushn n = subq (imm n) (reg rsp)

let type_size = function
  | TypeInteger -> 8
  | TypeBoolean -> 1
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
  List.fold_left (fun accum x ->
      let new_env = get_subprogram_tree name (level + 1) x in
      Smap.merge merge_conflict new_env accum
  ) starting_env funcs


let find_subprogram (sb_tr : sbpDefs) subprogram =
  match Smap.find_opt subprogram sb_tr with
  | Some a -> a
  | None ->
      SubprogramNotDeclared
        (Printf.sprintf "Couldn't find subprogram %s" subprogram)
      |> raise

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

(*
let rec compile_expr sb_tr = function
  | Integer a ->  pushq (imm a)
  | B b -> pushq (imm (if b then 1 else 0))
  | Var a ->
  | SUM(a, b) ->
  | SUB(a, b) ->
  | MUL(a, b) ->
  | DIV(a, b) ->
  | Equ(a, b) ->
  | LE(a, b) ->
  | LT(a, b) ->
  | GE(a, b) ->
  | GT(a, b) ->
  | NOT a
  | AND(a, b) ->
  | OR(a, b) ->
  | CALL of ident * exp list
  |_ -> raise (Invalid_argument "NOT IMPLEMENTED YET")

let rec compile_stmt sb_tr =
  function
  | STMTAss(var, exp) ->
  | STMTBlock(stmt_list) ->
  | STMTFor(varname, from, to, stmt) ->
  | STMTIf(cond, then_part, else_part) ->

  (*Super Hyper Mega Important*)
  | STMTSubprogramCall(sb_name, params_exps) ->
  | STMTWhile(cond, stmt) ->
  | STMTRead(var) -> 
(*Separado por virgulas, em que devemos ver qual o tipo de cada parametro e fazer print_int ou print dependendo disso*)
  | STMTWrite(exp_list)-> let all_types = List.map exp_type exp_list
  | STMTEmpty

*)


let rec compile_subprogram sbp_tree sbp =
  let name, parameters, block, tp = unwrap_subprogram sbp in
  let _, nested_sbps, stmt = unwrap_block block in
  let label_func = label name in
  let pointer_setup = 
  let allocate_variables = 
  let compiled_stmt = compile_stmt sbp_tree stmt in
  let compiled_subprogram = label_func ++ allocate_variables ++ compiled_stmt in


let generation_pipeline program =
  let program_name, blk = match program with Program (a, b) -> (a, b) in
  let main_sbp = ProcedureDeclaration (program_name, [], blk) in
  let sbp_tree = get_subprogram_tree "" 0 main_sbp in
  let sbp_mc = compile_subprogram sbp_tree main_sbp in
  sbp_mc
(*call program_name*)
