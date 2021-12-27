(* based on https://web.archive.org/web/20081028194213/https://www.cs.helsinki.fi/u/vihavain/k06/okk/items/minipascalsyntax.html *)

(*TODO remove RECs and Vector/Matrix stuff*)

type ident = Ident of string

(*PROCEDURE (ident: name) (variable_declaration list: arguments) ....*)
and program = Program of string *  block

and block = Block of variable_declaration list * subprogam_declaration list * statement

(*-------------------Variable declaration part----------------------------------------------*)

(*<variable declaration> ::=	<identifier > { , <identifier> } : <type>*)
and variable_declaration = VariableDeclaration of ident * pascaltype

(* <type> ::=	<simple type> | <array type> *)
(* <array type> ::=	array [ <index range> ] of <simple type> *)
and pascaltype = Simpletype of simpletype | PArrayType of int * simpletype

and simpletype  =  | TypeInteger 
                   | TypeReal 
                   | TypeBoolean 
                   | TypeString 
                   | TypeChar 
(*------------------------------------------------------------------------------------------*)



(*-------------------Procedure declaration part---------------------------------------------*)

(* <procedure declaration> ::=	procedure <identifier> ; <block> *)
and subprogam_declaration =
    | ProcedureDeclartion of ident * variable_declaration list * block 
    (* Last value is the return type*)
    | FunctionDeclaration of ident * variable_declaration list * block * pascaltype
(*------------------------------------------------------------------------------------------*)


(*-------------------Statement declaration part---------------------------------------------*)

                
and statement = | STMTAss of variable * exp
                (*Begin...End*)
                | STMTBlock of statement list
                | STMTFor of ident * arithexp * arithexp * statement
                (*<if statement> ::=	if <expression> then <statement> | if <expression> then <statement> else <statement>  option because it might have an else statement or not *)
                | STMTIf of booleanexp * statement * statement option
                (*example batata(4, a, 9*2) *)
                | STMTSubprogramCall of ident * exp list 
                | STMTWhile of booleanexp * statement

                | STMTRead of variable
                | STMTWrite of exp list

and exp = 
    | ArithExp of arithexp 
    | BooleanExp of booleanexp 
    | VariableExp of variable
    | MiscExp of miscexp

and arithexp =
    | Integer of int
	| Real of float
    | NumVar of ident
    | SUM of arithexp * arithexp
    | SUB of arithexp * arithexp
    | MUL of arithexp * arithexp
    | DIV of arithexp * arithexp

and booleanexp =
    | B of bool
    | BoolVar of ident
    | Equ of arithexp * arithexp
    | LE of  arithexp * arithexp
    | LT of  arithexp * arithexp
    | GE of  arithexp * arithexp
    | GT of  arithexp * arithexp
    | NOT of booleanexp
    | AND of booleanexp * booleanexp
    | OR of booleanexp * booleanexp

and miscexp =
    | PString of string
    | PChar of string
	| VAR of ident

(*EntireVar -> batata(a) *)
(*IndexedVar -> batata(a[1]) *)
and variable = EntireVar of ident | IndexedVar of ident * arithexp
