%{ (* HEADER *)

open Ast;;

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
%left MINUS
%left PLUS
%left DIVISION
%left TIMES


%start program
%type <Ast.program> program
%%


(*-------------------Program part-----------------------------------------------------------*)
(* program <identifier> ; <block> .*)
program:
     |PROGRAM PSTRING block DOT EOF { Program($2, $3) }

block:
     | opt_variable_declaration_list; statement;{ Block($1, [], $2) }
    ;
(*------------------------------------------------------------------------------------------*)

(*-------------------Variable declaration part----------------------------------------------*)
opt_variable_declaration_list: 
    |                                             { [] }
    | variable_declaration_list                   { $1 }
    ;

variable_declaration_list:
    | variable_field                     { [$1] }
    | variable_field  variable_declaration_list { $1::$2 }
    ;

variable_field:
    VAR ide COLON ptype SEMICOLON {VariableDeclaration($2, $4)};
(*------------------------------------------------------------------------------------------*)


(*-------------------Statement declaration part---------------------------------------------*)


statement:
    (*Mudar este ide para outra coisa, caso se use arrays*)
    | variable ASSIGN exp SEMICOLON                       { STMTAss($1,$3) }
    | BEGIN opt_statement_list END SEMICOLON              { STMTBlock($2) }
    | FOR ide ASSIGN arithexp TO arithexp DO statement    { STMTFor($2, $4, $6, $8) }
    | IF booleanexp THEN statement opt_else               { STMTIf($2,$4,$5) }
    | ide LP opt_exp_list RP SEMICOLON                    { STMTSubprogramCall($1,$3) }
    | WHILE booleanexp DO statement                       { STMTWhile($2,$4) }
    | WRITE LP opt_exp_list RP SEMICOLON                  { STMTWrite($3) }
    | READ LP variable RP SEMICOLON                       { STMTRead($3) }
    ;


opt_else:
    | {None}
    | ELSE statement {Some $2}


opt_statement_list:
    |                                               { [] }
    | statement_list                                { $1 }
    ;

statement_list
    : statement                                     { [$1] }
    | statement  statement_list                     { $1::$2 }
    ;
(*------------------------------------------------------------------------------------------*)
(*-------------------Expression declaration part--------------------------------------------*)


opt_exp_list:
    |                                           {[]}
    |exp_list                                   {$1}

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
    (*| variable     {NumVar($1})*)
    | arithexp PLUS arithexp {SUM($1,$3)}
    | arithexp MINUS arithexp {SUB($1,$3)}
    | arithexp TIMES arithexp {MUL($1,$3)}
    | arithexp DIVISION arithexp {DIV($1,$3)}
    | LP arithexp RP                 { $2 }

booleanexp:
    | TRUE {B(true)}
    | FALSE {B(false)}
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
