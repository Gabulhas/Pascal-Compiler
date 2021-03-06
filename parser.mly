%{ (* HEADER *)

open Ast;;



let vars_to_list var_ides var_type =
    List.map (fun x -> VariableDeclaration(x, var_type)) var_ides

%}

%token <string>   IDE
%token <int>      INT
%token <bool>     PBOOLEAN
%token <string>   PSTRING
%token <char>     PCHAR

%token            TRUE FALSE

%token            PROGRAM VAR ARRAY OF TINT TBOOLEAN TCHAR TSTRING PROCEDURE FUNCTION BEGIN END
%token            IF THEN ELSE WHILE DO FOR TO WRITE READ DOWNTO

%token            PLUS MINUS TIMES DIVISION EQUAL LESSEQUAL LESS AND OR NOT MODU
%token            ASSIGN

%token            SEMICOLON COLON COMMA

%token            LS RS LP RP

%token            GREATEREQUAL GREATER

%token            DOT
%token            EOF

%left OR
%left AND
%nonassoc NOT
%nonassoc LESS LESSEQUAL GREATER GREATEREQUAL EQUAL NOTEQUAL
%left MINUS PLUS
%left TIMES DIVISION MODU
%nonassoc LS
%nonassoc THEN
%nonassoc ELSE



%start program
%type <Ast.program> program
%%

(*
PROGRAM ProgramName (FileList);

CONST
  (* Constant declarations *)

TYPE
  (* Type declarations *)

VAR
  (* Variable declarations *)

(* Subprogram definitions *)

BEGIN
  (* Executable statements *)
END.

 *)

(*-------------------Program part-----------------------------------------------------------*)
(* program <identifier> ; <block> .*)
program:
     |PROGRAM PSTRING block EOF { Program($2, $3) }
     ;

block: 
    | opt_variable_declaration_list opt_subprogram_list statement_part {Block($1, $2, $3)}
    ;

(*------------------------------------------------------------------------------------------*)
(*-------------------Variable declaration part----------------------------------------------*)
opt_variable_declaration_list: 
    |                                             { [] }
    | VAR variable_declaration_list                   { $2 }
    ;

variable_declaration_list:
    | variable_field                     { $1 }
    | variable_field  variable_declaration_list { $1@$2 }
    ;

variable_field:
    | separated_nonempty_list(COMMA, ide) COLON ptype SEMICOLON {vars_to_list $1 $3}
(*------------------------------------------------------------------------------------------*)


(*-------------------Statement declaration part---------------------------------------------*)

statement_part:
    | BEGIN statement_list END DOT {STMTBlock($2)}

statement:
    (*Mudar este ide para outra coisa, caso se use arrays*)
    | variable ASSIGN exp SEMICOLON                       { STMTAss($1,$3) }
    | BEGIN statement_list END SEMICOLON              { STMTBlock($2) }
    | FOR ide ASSIGN exp TO exp DO statement    { STMTFor($2, $4, $6, $8, true) }
    | FOR ide ASSIGN exp DOWNTO exp DO statement    { STMTFor($2, $4, $6, $8, false) }
    | IF exp THEN statement ELSE statement         { STMTIf($2,$4,Some $6) }
    | IF exp THEN statement                        { STMTIf($2,$4,None) }
    | ide LP separated_list(COMMA, exp) RP SEMICOLON                    { STMTSubprogramCall($1,$3) }
    | WHILE exp DO statement                       { STMTWhile($2,$4) }
    | WRITE LP separated_list(COMMA, exp) RP SEMICOLON                  { STMTWrite($3) }
    | READ LP variable RP SEMICOLON                       { STMTRead($3) }
    ;

statement_list
    : statement                                     { [$1] }
    | statement  statement_list                     { $1::$2 }
    ;
(*------------------------------------------------------------------------------------------*)
(*-------------------Expression declaration part--------------------------------------------*)


exp:
    | INT          {Integer($1)}
    | PSTRING {PString($1)}
    | TRUE {B(true)}
    | FALSE {B(false)}
    | ide {Var(EntireVar($1))}
    | ide LS exp RS  {Var(IndexedVar($1,$3 ))}
    | exp PLUS exp {SUM($1,$3)}
    | exp MINUS exp {SUB($1,$3)}
    | MINUS exp {SUB(Integer(0),$2)}
    | exp TIMES exp {MUL($1,$3)}
    | exp DIVISION exp {DIV($1,$3)}
    | exp MODU exp {MOD($1,$3)}
    | exp EQUAL exp {Equ($1, $3)}
    | exp LESSEQUAL exp {LE($1, $3)}
    | exp LESS exp {LT($1, $3)}
    | exp GREATEREQUAL exp {GE($1, $3)}
    | exp GREATER exp {GT($1, $3)}
    | NOT exp {NOT($2)}
    | exp AND exp {AND($1,$3)}
    | exp OR exp {OR($1,$3)}
    | ide LP separated_list(COMMA, exp) RP { CALL($1,$3) }
    | LP exp RP             { $2 }


(*------------------------------------------------------------------------------------------*)
(*-----------------------------Subprogram stuff :) STUFF :)-----------------------------------------------*)
opt_subprogram_list
    :                                             { [] }
    | subprogram_list                                   { $1 }
    ;

subprogram_list
    : subprogram                                                 { [$1] }
    | subprogram  subprogram_list                    { $1::$2 }
    ;

subprogram
    : PROCEDURE ide LP flatten(separated_list(COMMA, parameter)) RP SEMICOLON block 
	                                          { ProcedureDeclaration($2,$4,$7) }
    | FUNCTION ide LP flatten(separated_list(COMMA, parameter)) RP COLON ptype SEMICOLON block
	                                          { FunctionDeclaration($2,$4,$9, $7) }
    ;

parameter:
    | separated_nonempty_list(COMMA, ide) COLON ptype {vars_to_list $1 $3};

(*------------------------------------------------------------------------------------------*)
(*-----------------------------OTHER STUFF :)-----------------------------------------------*)
ptype: 
    | stype {SimpleType($1)}
    | ARRAY LS INT DOT DOT INT RS OF stype {ArrayType($3, $6, $9)}

stype:
    | TINT {TypeInteger}
    | TBOOLEAN {TypeBoolean}
    | TSTRING {TypeString}
    | TCHAR {TypeChar}

variable:
    | ide LS exp RS {IndexedVar($1, $3)}
    | ide {EntireVar($1)}

ide:
    | IDE {$1}
;
(*------------------------------------------------------------------------------------------*)


