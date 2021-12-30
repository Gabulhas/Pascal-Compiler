open Ast

module VariableMap = Map.Make(struct type t = ident let compare = compare end)
module ParameterMap = Map.Make(struct type t = ident let compare = compare end)

exception VariableNotDeclared of string
exception TypeMismatch of string


let rec simpletype_to_string = function
    | TypeInteger -> "Integer"
                   | TypeReal -> "Real"
                   | TypeBoolean -> "Boolean"
                   | TypeString -> "String"
                   | TypeChar -> "Char"

let rec type_to_string =
    function | Simpletype a -> simpletype_to_string a
                   | ArrayType(a,b,c) -> Printf.sprintf "Array[%d..%d] of %s" a b (simpletype_to_string c)

let raise_variable_not_declared i =
    raise (VariableNotDeclared (Printf.sprintf "Variable %s not found!" i))

let raise_type_mismatched a b =
    let a_string = type_to_string a in
    let b_string = type_to_string b in
    raise (TypeMismatch (Printf.sprintf "Expected %s bot got %s" a_string b_string))

let raise_simpletype_mismatched a b =
    let a_string = simpletype_to_string a in
    let b_string = simpletype_to_string b in
    raise (TypeMismatch (Printf.sprintf "Expected %s bot got %s" a_string b_string))

let get_var_type_from_ident i map=
    match VariableMap.find_opt i map with
    | Some a -> a
    | None ->  raise_variable_not_declared i

let get_var_type_from_var v map=
    match v with
    | EntireVar a -> get_var_type_from_ident a map
    | IndexedVar(a,_)-> get_var_type_from_ident a map

let rec add_vars_to_map varlist varmap =
    match varlist with
    | VariableDeclaration(i, t) :: tl -> add_vars_to_map tl (VariableMap.add i t varmap)
    | [] -> varmap

let get_sympletype =
    function
        | Simpletype a -> a
    | ArrayType (_,_,a) -> a

let compare_pascaltypes a b=
    match a, b with
    | Simpletype x, Simpletype y ->  x == y
    | ArrayType(_,_,x), ArrayType(_,_,y) -> x == y
    | _ -> false

let is_simpletype_expected expected operands =
    List.iter (fun x -> if expected  == x then raise_simpletype_mismatched expected x) operands;
    expected

let rec functions_to_parameters_map funlist paramsmap=
    match funlist with
    | ProcedureDeclaration(i, params,_,_) :: tl ->
            functions_to_parameters_map tl (ParameterMap.add i params paramsmap)
    | FunctionDeclaration (i, params,_,_, _) :: tl ->
            functions_to_parameters_map tl (ParameterMap.add i params paramsmap)
    | [] -> paramsmap

    (*Add get function return type*)
let rec check_program my_program =
    let varlist, sublist, stm = match my_program with | Program(_, a,b,c) -> (a,b, c) in
    let global_map = add_vars_to_map varlist (VariableMap.empty) in
    let params_map = functions_to_parameters_map sublist (ParameterMap.empty) in
    global_map



let rec check_expression expression varmap paramsmap=
    let check_complex expected operands=
        is_simpletype_expected expected (List.map (fun x -> check_expression x varmap paramsmap) operands)
    in
    match expression with
    | Integer (_) -> TypeInteger
    | PString (_) -> TypeString
    | PChar (a) -> TypeChar
    | B (_) -> TypeBoolean
    | Var (a) -> get_var_type_from_var a varmap |> get_sympletype
    | SUM (a , b) -> check_complex TypeInteger [a;b]
    | SUB (a , b) -> check_complex TypeInteger [a;b]
    | MUL (a , b) -> check_complex TypeInteger [a;b]
    | DIV (a , b) -> check_complex TypeInteger [a;b]
    | Equ (a , b) -> check_complex TypeInteger [a;b] |> ignore; TypeBoolean
    | LE ( a , b) -> check_complex TypeInteger [a;b] |> ignore; TypeBoolean
    | LT ( a , b) -> check_complex TypeInteger [a;b] |> ignore; TypeBoolean
    | GE ( a , b) -> check_complex TypeInteger [a;b] |> ignore; TypeBoolean
    | GT ( a , b) -> check_complex TypeInteger [a;b] |> ignore; TypeBoolean

    | NOT (a) ->     check_complex TypeBoolean [a]
    | AND (a , b) -> check_complex TypeBoolean [a]
    | OR (a , b) ->  check_complex TypeBoolean [a]

    (*TODO complete tomorrow*)
    | CALL (a , b) -> check_expression (List.hd b) varmap paramsmap

let rec check_statements statement varmap paramsmap=
    match statement with
                | STMTAss(a, e) -> (let e_type = check_expression e varmap paramsmap in
                let a_type =  get_var_type_from_var a varmap |> get_sympletype in
                is_simpletype_expected a_type [e_type] |> ignore

                )
                | STMTBlock(stmt_list) -> List.iter (fun x -> check_statements x varmap paramsmap) stmt_list
                | STMTFor(a, b, c, d) -> (
                    let a_type =  get_var_type_from_ident a varmap |> get_sympletype in
                    is_simpletype_expected TypeInteger [a_type; check_expression b varmap paramsmap; check_expression c varmap paramsmap]
                |> ignore;
                                    check_statements d varmap paramsmap;
                )
                | STMTIf(e, tn, el) -> (
                    is_simpletype_expected TypeBoolean [check_expression e varmap paramsmap] |> ignore;
                    check_statements tn varmap paramsmap;
                    match el with
                    | Some a -> check_statements a varmap paramsmap;
                    | None -> ()
                    )
                (*find a way to get the return value type*)
                (*Compare exp list to argument list types*)
                    | STMTSubprogramCall(_, _)   -> ()
                    | STMTWhile(e, st) -> (
                        is_simpletype_expected TypeBoolean [check_expression e varmap paramsmap] |> ignore;
                    check_statements st varmap paramsmap;
                    )
                    | STMTRead (_) -> ()
                    | STMTWrite(el) -> List.iter (fun e -> check_expression e varmap paramsmap |> ignore) el|>ignore ;
                | STMTEmpty -> ()


                (*

let rec check_subprogram subprogram map =
    *)
