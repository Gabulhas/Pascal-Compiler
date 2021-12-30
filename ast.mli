(* based on https://web.archive.org/web/20081028194213/https://www.cs.helsinki.fi/u/vihavain/k06/okk/items/minipascalsyntax.html *)

type ident = string

(*PROCEDURE (ident: name) (variable_declaration list: arguments) ....*)
and program = Program of string * variable_declaration list * subprogram_declaration list * statement


(*-------------------Variable declaration part----------------------------------------------*)

(*<variable declaration> ::=	<identifier > { , <identifier> } : <type>*)
and variable_declaration = VariableDeclaration of ident * pascaltype

(* <type> ::=	<simple type> | <array type> *)
(* <array type> ::=	array [ <index range> ] of <simple type> *)
and pascaltype = Simpletype of simpletype | ArrayType of int * int * simpletype

and simpletype  =  | TypeInteger 
                   | TypeReal 
                   | TypeBoolean 
                   | TypeString 
                   | TypeChar 
(*------------------------------------------------------------------------------------------*)



(*-------------------Procedure declaration part---------------------------------------------*)

(* <procedure declaration> ::=	procedure <identifier> ; <block> *)
and subprogram_declaration =
    | ProcedureDeclaration of ident * variable_declaration list * variable_declaration list * statement

    (* Last value is the return type*)
    | FunctionDeclaration of ident * variable_declaration list * variable_declaration list * statement
 * pascaltype
(*------------------------------------------------------------------------------------------*)


(*-------------------Statement declaration part---------------------------------------------*)

                
and statement = | STMTAss of variable * exp (*Begin...End*)
                | STMTBlock of statement list
                | STMTFor of ident * exp * exp * statement
                (*<if statement> ::=	if <expression> then <statement> | if <expression> then <statement> else <statement>  option because it might have an else statement or not *)
                | STMTIf of exp * statement * statement option
                (*example batata(4, a, 9*2) *)
                | STMTSubprogramCall of ident * exp list 
                | STMTWhile of exp * statement
                | STMTRead of variable
                | STMTWrite of exp list
                | STMTEmpty

and exp = 
    | Integer of int
    | PString of string
    | PChar of char
    | B of bool
    | Var of variable
    | SUM of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | Equ of exp * exp
    | LE of  exp * exp
    | LT of  exp * exp
    | GE of  exp * exp
    | GT of  exp * exp
    | NOT of exp
    | AND of exp * exp
    | OR of exp * exp
    | CALL of ident * exp list

(*EntireVar -> batata(a) *)
(*IndexedVar -> batata(a[1]) *)
and variable = EntireVar of ident | IndexedVar of ident * exp
