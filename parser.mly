%{ (* HEADER *)

open Ast;;

%}

%token <string>   IDE

%token <int>      NUM
%token <float>    REAL
%token <bool>     PBOOLEAN
%token <string>   PSTRING
%token <char>     PCHAR

%token            TRUE FALSE

%token            PROGRAM VAR ARRAY OF INT FLOAT BOOLEAN CHAR STRING PROCEDURE FUNCTION BEGIN END
%token            IF THEN ELSE WHILE DO REPEAT UNTIL FOR TO WRITE CALL

%token            PLUS MINUS TIMES DIVISION EQUAL LESSEQUAL LESS AND OR NOT
%token            ASSIGN

%token            SEMICOLON COLON COMMA

%token            LS RS LP RP QUOTE

%token            GREATEREQUAL GREATER

%token            RECORD DOT

%token            EOF

%start program
%type <Ast.program> program
%%


(* program <identifier> ; <block> .*)
program:
     |PROGRAM QUOTE ide QUOTE block EOF { Program($3, $5) }

block:
     | variable_declaration_list; { Block($1, [], []) }
     (*| variable_declaration_list; procedure_declaration_list; statement_list { Block($1, $2, $3) }*)
    ;


variable_declaration_list:
    VAR separated_list(COMMA, variable_field)    { $2 } ;

variable_field:
      VAR ide COLON ptype SEMICOLON {VariableDeclaration($2, $4)};

ptype: 
    | stype {Simpletype($1)}


stype:
    | INT {TypeInteger}
    | REAL {TypeReal}
    | BOOLEAN {TypeBoolean}
    | STRING {TypeString}
    | CHAR {TypeChar}

ide
    : IDE {Ident($1)}
;
