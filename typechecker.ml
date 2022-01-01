open Ast

module VariableMap = Map.Make (struct
  type t = ident

  let compare = compare
end)
module ParameterMap = Map.Make (struct
  type t = ident

  let compare = compare
end)

exception VariableNotDeclared of string
exception SubprogramNotDeclared of string
exception TypeMismatch of string
exception ParametersMismatch of string
exception NotAnArray of string

let rec simpletype_to_string = function
  | TypeInteger -> "Integer"
  | TypeReal -> "Real"
  | TypeBoolean -> "Boolean"
  | TypeString -> "String"
  | TypeChar -> "Char"
  | TypeNull -> "Null"

let rec type_to_string = function
  | SimpleType a -> simpletype_to_string a
  | ArrayType (a, b, c) ->
      Printf.sprintf "Array[%d..%d] of %s" a b (simpletype_to_string c)

let raise_variable_not_declared i =
  raise (VariableNotDeclared (Printf.sprintf "Variable %s not found!" i))

let raise_subprogram_not_declared i =
  raise
    (SubprogramNotDeclared
       (Printf.sprintf "Function/Procedure \"%s\" not found!" i))

let raise_parameters_mismatch i params calls =
  let params_to_string p =
    [ "("; String.concat "," (List.map type_to_string params); ")" ]
    |> String.concat ""
  in
  let params_string = params_to_string params in
  let calls_string = params_to_string calls in

  raise
    (ParametersMismatch
       (Printf.sprintf "While calling %s: %s != %s" i params_string calls_string))

let raise_type_mismatched a b =
  let a_string = type_to_string a in
  let b_string = type_to_string b in
  raise
    (TypeMismatch (Printf.sprintf "Expected %s but got %s" a_string b_string))

let raise_simpletype_mismatched a b =
  let a_string = simpletype_to_string a in
  let b_string = simpletype_to_string b in
  raise
    (TypeMismatch (Printf.sprintf "Expected %s but got %s" a_string b_string))

let raise_not_an_array i =
  raise (VariableNotDeclared (Printf.sprintf "%s is not an array" i))

let get_array_element_type v t =
  match t with ArrayType (_, _, v) -> SimpleType(v) | SimpleType _ -> raise_not_an_array v

let get_var_type_from_ident i map =
  match VariableMap.find_opt i map with
  | Some a -> a
  | None -> raise_variable_not_declared i

let get_var_type_from_var v map =
  match v with
  | EntireVar a -> get_var_type_from_ident a map
  | IndexedVar (a, _) -> get_var_type_from_ident a map

let rec add_vars_to_map varlist varmap =
  match varlist with
  | VariableDeclaration (i, t) :: tl ->
      add_vars_to_map tl (VariableMap.add i t varmap)
  | [] -> varmap

let rec add_function_returns_to_map subprogram_list varmap =
  match subprogram_list with
  | FunctionDeclaration (i, _, _, rtrn) :: tl ->
      add_function_returns_to_map tl (VariableMap.add i rtrn varmap)
  | ProcedureDeclaration (i, _, _) :: tl ->
      add_function_returns_to_map tl
        (VariableMap.add i (SimpleType TypeNull) varmap)
  | [] -> varmap

let get_sympletype = function SimpleType a -> a | ArrayType (_, _, a) -> a


let compare_pascaltypes a b =
  match (a, b) with
  | SimpleType x, SimpleType y -> x == y
  | ArrayType (_, _, x), ArrayType (_, _, y) -> x == y
  | _ -> false

let is_type_expected expected operands =
  List.iter
    (fun x ->
      if not (compare_pascaltypes expected x) then
        raise_type_mismatched expected x)
    operands;
  expected

let are_parameters_expected funname paramlist explist =
  if List.equal (fun x y -> compare_pascaltypes x y) paramlist explist then ()
  else raise_parameters_mismatch funname paramlist explist

let rec functions_to_parameters_map funlist paramsmap =
  match funlist with
  | ProcedureDeclaration (i, params, _s) :: tl ->
      functions_to_parameters_map tl (ParameterMap.add i params paramsmap)
  | FunctionDeclaration (i, params, _, _) :: tl ->
      functions_to_parameters_map tl (ParameterMap.add i params paramsmap)
  | [] -> paramsmap

let rec check_expression expression varmap paramsmap =
  let check_complex expected operands =
    is_type_expected expected
      (List.map (fun x -> check_expression x varmap paramsmap) operands)
  in
  match expression with
  | Integer _ -> SimpleType TypeInteger
  | PString _ -> SimpleType TypeString
  | PChar a -> SimpleType TypeChar
  | B _ -> SimpleType TypeBoolean
  (*this should include arrays*)
  | Var a ->( 
        match a with
        | EntireVar a -> get_var_type_from_ident a varmap
        | IndexedVar (a, b) ->
            let index_exp = check_expression b varmap paramsmap in
            is_type_expected (SimpleType TypeInteger) [ index_exp ] |> ignore;
            get_array_element_type a (get_var_type_from_ident a varmap)
  )

  | SUM (a, b) -> check_complex (SimpleType TypeInteger) [ a; b ]
  | SUB (a, b) -> check_complex (SimpleType TypeInteger) [ a; b ]
  | MUL (a, b) -> check_complex (SimpleType TypeInteger) [ a; b ]
  | DIV (a, b) -> check_complex (SimpleType TypeInteger) [ a; b ]
  | Equ (a, b) ->
      check_complex (SimpleType TypeInteger) [ a; b ] |> ignore;
      SimpleType TypeBoolean
  | LE (a, b) ->
      check_complex (SimpleType TypeInteger) [ a; b ] |> ignore;
      SimpleType TypeBoolean
  | LT (a, b) ->
      check_complex (SimpleType TypeInteger) [ a; b ] |> ignore;
      SimpleType TypeBoolean
  | GE (a, b) ->
      check_complex (SimpleType TypeInteger) [ a; b ] |> ignore;
      SimpleType TypeBoolean
  | GT (a, b) ->
      check_complex (SimpleType TypeInteger) [ a; b ] |> ignore;
      SimpleType TypeBoolean
  | NOT a -> check_complex (SimpleType TypeBoolean) [ a ]
  | AND (a, b) -> check_complex (SimpleType TypeBoolean) [ a ]
  | OR (a, b) -> check_complex (SimpleType TypeBoolean) [ a ]
  | CALL (a, b) ->
      let returntype_opt = VariableMap.find_opt a varmap in
      let returntype =
        match returntype_opt with
        | Some a -> a
        | None -> raise_subprogram_not_declared a
      in

      let exp_types =
        List.map (fun x -> check_expression x varmap paramsmap) b
      in
      let paramslist_opt = ParameterMap.find_opt a paramsmap in
      let paramslist =
        match paramslist_opt with
        | Some a ->
            List.map
              (fun x -> match x with VariableDeclaration (_, tp) -> tp)
              a
        | None -> []
      in
      are_parameters_expected a paramslist exp_types;
      returntype

let rec check_statements statement varmap paramsmap =
  match statement with
  | STMTAss (a, e) ->
      let i, isIndexed =
        match a with
        | EntireVar a -> (a, false)
        | IndexedVar (a, b) ->
            let index_exp = check_expression b varmap paramsmap in
            is_type_expected (SimpleType TypeInteger) [ index_exp ] |> ignore;
            (a, true)
      in
      let e_type = check_expression e varmap paramsmap in
      let a_type =
        if isIndexed then
          get_array_element_type i (get_var_type_from_ident i varmap)
        else get_var_type_from_ident i varmap
      in
      is_type_expected a_type [ e_type ] |> ignore
  | STMTBlock stmt_list ->
      List.iter (fun x -> check_statements x varmap paramsmap) stmt_list
  | STMTFor (a, b, c, d) ->
      let a_type = get_var_type_from_ident a varmap in
      is_type_expected (SimpleType TypeInteger)
        [
          a_type;
          check_expression b varmap paramsmap;
          check_expression c varmap paramsmap;
        ]
      |> ignore;
      check_statements d varmap paramsmap
  | STMTIf (e, tn, el) -> (
      is_type_expected (SimpleType TypeBoolean)
        [ check_expression e varmap paramsmap ]
      |> ignore;
      check_statements tn varmap paramsmap;
      match el with Some a -> check_statements a varmap paramsmap | None -> ())
  (*find a way to get the return value type*)
  (*Compare exp list to argument list types*)
  | STMTSubprogramCall (i, e) -> (
      let returntype_opt = VariableMap.find_opt i varmap in
      let returntype =
        match returntype_opt with
        | Some a -> a
        | None -> raise_subprogram_not_declared i
      in
      is_type_expected (SimpleType(TypeNull)) [returntype] |> ignore;

          

      let exp_types =
        List.map (fun x -> check_expression x varmap paramsmap) e
      in

      let paramslist_opt = ParameterMap.find_opt i paramsmap in
      let paramslist =
        match paramslist_opt with
        | Some a ->
            List.map
              (fun x -> match x with VariableDeclaration (_, tp) -> tp)
              a
        | None -> raise_subprogram_not_declared i
      in
      are_parameters_expected i paramslist exp_types;


  )
  | STMTWhile (e, st) ->
      is_type_expected (SimpleType TypeBoolean)
        [ check_expression e varmap paramsmap ]
      |> ignore;
      check_statements st varmap paramsmap
  | STMTRead _ -> ()
  | STMTWrite el ->
      List.iter (fun e -> check_expression e varmap paramsmap |> ignore) el
      |> ignore
  | STMTEmpty -> ()

let check_subprogram name globalvarsmap paramsmap localparmslist localvarslist
    statement returntype =
 
  let add_local_map map = 
    map
    |> add_vars_to_map localparmslist
    |> add_vars_to_map localvarslist
    |> VariableMap.add name returntype
  in
  let local_map =
    VariableMap.empty |> add_local_map
  in
  let totalvars =
    globalvarsmap |> add_local_map
  in
  check_statements statement totalvars paramsmap;
  local_map


(*
    parentvars: current scope of the parent
    parentparams: current function scope of the parents (including their params list)

 *)
let rec read_subprogram subprogram parentvars parentparams =
  let name, params, block, returntype = 
        match subprogram with
        | ProcedureDeclaration (name, prms, block) -> name, prms, block, (SimpleType TypeNull)
        | FunctionDeclaration (name, prms, block, t) -> name, prms, block, t
  in
  let varlist, sublist, stm =
    match block with Block (a, b, c) -> (a, b, c)
  in
  
  let localvars =
    add_vars_to_map varlist parentvars
    |> add_vars_to_map params
    |> add_function_returns_to_map sublist
  in
  let localparams =
    functions_to_parameters_map sublist parentparams in

  check_statements stm localvars localparams;
  List.iter (fun x -> read_subprogram x localvars localparams) sublist


let type_check_program my_program= 
    let _, main_block = (match my_program with | Program(a,b) -> (a,b))
    in 
    (*Main is a procedure*)
    read_subprogram (ProcedureDeclaration("main", [], main_block)) (VariableMap.empty) (ParameterMap.empty);

