%{ (* HEADER *)

open Ast;;

let vars_to_list var_ides var_type =
    List.map (fun x -> VariableDeclaration(x, var_type)) var_ides

%}

%token <string>   IDE
%token <int>      INT
%token <float>    REAL
%token <bool>     PBOOLEAN
%token <string>   PSTRING
%token <char>     PCHAR

%token            TRUE FALSE

%token            PROGRAM VAR ARRAY OF TINT TREAL TBOOLEAN TCHAR TSTRING PROCEDURE FUNCTION BEGIN END
%token            IF THEN ELSE WHILE DO FOR TO WRITE READ

%token            PLUS MINUS TIMES DIVISION EQUAL LESSEQUAL LESS AND OR NOT
%token            ASSIGN

%token            SEMICOLON COLON COMMA

%token            LS RS LP RP QUOTE

%token            GREATEREQUAL GREATER

%token            DOT
%token            EOF



%left OR
%left AND
%nonassoc NOT
%nonassoc LT LE GT GE EQ NE
%left MINUS PLUS
%left TIMES DIVISION
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
     |PROGRAM PSTRING opt_variable_declaration_list opt_subprogram_list statement_part EOF { Program($2, $3, $4, $5) }
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
    separated_nonempty_list(COMMA, ide) COLON ptype SEMICOLON {vars_to_list $1 $3};
(*------------------------------------------------------------------------------------------*)


(*-------------------Statement declaration part---------------------------------------------*)

statement_part:
    | BEGIN statement_list END DOT {STMTBlock($2)}

statement:
    (*Mudar este ide para outra coisa, caso se use arrays*)
    | variable ASSIGN exp SEMICOLON                       { STMTAss($1,$3) }
    | BEGIN statement_list END SEMICOLON              { STMTBlock($2) }
    | FOR ide ASSIGN arithexp TO arithexp DO statement    { STMTFor($2, $4, $6, $8) }
    | IF LP booleanexp RP THEN statement ELSE statement         { STMTIf($3,$6,Some $8) }
    | IF LP booleanexp RP THEN statement                        { STMTIf($3,$6,None) }
    | ide LP separated_list(COMMA, exp) RP SEMICOLON                    { STMTSubprogramCall($1,$3) }
    | WHILE booleanexp DO statement                       { STMTWhile($2,$4) }
    | WRITE LP separated_list(COMMA, exp) RP SEMICOLON                  { STMTWrite($3) }
    | READ LP variable RP SEMICOLON                       { STMTRead($3) }
    ;

statement_list
    : statement                                     { [$1] }
    | statement  statement_list                     { $1::$2 }
    ;
(*------------------------------------------------------------------------------------------*)
(*-------------------Expression declaration part--------------------------------------------*)


exp_list:
    |exp                                           {[$1]}
    |exp exp_list                                   {$1::$2}

exp:
    | arithexp      {ArithExp($1)}
    | booleanexp    {BooleanExp($1)}
    | miscexp       {MiscExp($1)}


(*
arithvalue:
| ide     {NumVar($1)}
| arithexp     {$1} 
*)

arithexp:
    | INT          {Integer($1)}
    | REAL         {Real($1)}
    | ide {NumVar($1)}
    | arithexp PLUS arithexp {SUM($1,$3)}
    | arithexp MINUS arithexp {SUB($1,$3)}
    | arithexp TIMES arithexp {MUL($1,$3)}
    | arithexp DIVISION arithexp {DIV($1,$3)}
    | LP arithexp RP                 { $2 }

booleanexp:
    | TRUE {B(true)}
    | FALSE {B(false)}
    | ide {BoolVar($1)}
    | arithexp EQUAL arithexp {Equ($1, $3)}
    | arithexp LESSEQUAL arithexp {LE($1, $3)}
    | arithexp LESS arithexp {LT($1, $3)}
    | arithexp GREATEREQUAL arithexp {GE($1, $3)}
    | arithexp GREATER arithexp {GT($1, $3)}
    | NOT booleanexp {NOT($2)}
    | booleanexp AND booleanexp {AND($1,$3)}
    | booleanexp OR booleanexp {OR($1,$3)}
    | LP booleanexp RP             { $2 }

miscexp:
    | PSTRING {PString($1)}


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
    : PROCEDURE ide LP flatten(separated_list(COMMA, parameter)) RP SEMICOLON opt_variable_declaration_list statement_part 
	                                          { ProcedureDeclaration($2,$4,$7, $8) }
    | FUNCTION ide LP flatten(separated_list(COMMA, parameter)) RP COLON ptype SEMICOLON opt_variable_declaration_list statement_part
	                                          { FunctionDeclaration($2,$4,$9,$10, $7) }
    ;

parameter:
    | separated_nonempty_list(COMMA, ide) COLON ptype {vars_to_list $1 $3};

(*------------------------------------------------------------------------------------------*)
(*-----------------------------OTHER STUFF :)-----------------------------------------------*)
ptype: 
    | stype {Simpletype($1)}


stype:
    | TINT {TypeInteger}
    | TREAL {TypeReal}
    | TBOOLEAN {TypeBoolean}
    | TSTRING {TypeString}
    | TCHAR {TypeChar}

variable:
    | ide LS arithexp RS {IndexedVar($1, $3)}
    | ide {EntireVar($1)}

ide:
    | IDE {Ident($1)}
;
(*------------------------------------------------------------------------------------------*)


