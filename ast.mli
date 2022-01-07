(* based on https://web.archive.org/web/20081028194213/https://www.cs.helsinki.fi/u/vihavain/k06/okk/items/minipascalsyntax.html *)

type ident = string

(*PROCEDURE (ident: name) (variable_declaration list: arguments) ....*)
and program = Program of string * block

and block =
  | Block of variable_declaration list * subprogram_declaration list * statement
(*-------------------Variable declaration part----------------------------------------------*)

(*<variable declaration> ::=	<identifier > { , <identifier> } : <type>*)
and variable_declaration = VariableDeclaration of ident * pascaltype

(* <type> ::=	<simple type> | <array type> *)
(* <array type> ::=	array [ <index range> ] of <simple type> *)
and pascaltype =
  | SimpleType of simpletype
  | ArrayType of int * int * simpletype

and simpletype =
  | TypeInteger
  | TypeReal
  | TypeBoolean
  | TypeString
  | TypeChar
  | TypeNull
(*------------------------------------------------------------------------------------------*)

(*-------------------Procedure declaration part---------------------------------------------*)

(* <procedure declaration> ::=	procedure <identifier> ; <block> *)
and subprogram_declaration =
  | ProcedureDeclaration of ident * variable_declaration list * block
  (* Last value is the return type*)
  | FunctionDeclaration of
      ident * variable_declaration list * block * pascaltype
(*------------------------------------------------------------------------------------------*)

(*-------------------Statement declaration part---------------------------------------------*)

and statement =
  | STMTAss of variable * exp (*Begin...End*)
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
  | LE of exp * exp
  | LT of exp * exp
  | GE of exp * exp
  | GT of exp * exp
  | NOT of exp
  | AND of exp * exp
  | OR of exp * exp
  | CALL of ident * exp list

(*EntireVar -> batata(a) *)
(*IndexedVar -> batata(a[1]) *)
and variable = EntireVar of ident | IndexedVar of ident * exp

(*Compile Statement*)
type cstatement =
  | CSTMTAss of variable * exp (*Begin...End*)
  | CSTMTBlock of statement list
  | CSTMTFor of ident * exp * exp * statement
  (*<if statement> ::=	if <expression> then <statement> | if <expression> then <statement> else <statement>  option because it might have an else statement or not *)
  | CSTMTIf of exp * statement * statement option
  (*example batata(4, a, 9*2) *)
  | CSTMTSubprogramCall of ident * exp list
  | CSTMTWhile of exp * statement
  | CSTMTRead of variable
  | CSTMTWrite of exp list
  | CSTMTEmpty

(*Allocate Expression*)
and aexp =
  | ConstInteger of int
  | ConstString of string
  | ConstChar of char
  | ConstB of bool
  | LVar of aident
  | GVar of string
  | ASUM of aexp * aexp
  | ASUB of aexp * aexp
  | AMUL of aexp * aexp
  | ADIV of aexp * aexp
  | AEqu of aexp * aexp
  | ALE of aexp * aexp
  | ALT of aexp * aexp
  | AGE of aexp * aexp
  | AGT of aexp * aexp
  | ANOT of aexp
  | AAND of aexp * aexp
  | AOR of aexp * aexp
  | ACALL of ident * aexp list


and aident = int
