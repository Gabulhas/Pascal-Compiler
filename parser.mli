
(* The type of tokens. *)

type token = 
  | WRITE
  | WHILE
  | VAR
  | TSTRING
  | TRUE
  | TREAL
  | TO
  | TINT
  | TIMES
  | THEN
  | TCHAR
  | TBOOLEAN
  | SEMICOLON
  | RS
  | RP
  | REAL of (float)
  | READ
  | QUOTE
  | PSTRING of (string)
  | PROGRAM
  | PROCEDURE
  | PLUS
  | PCHAR of (char)
  | PBOOLEAN of (bool)
  | OR
  | OF
  | NOT
  | MINUS
  | LS
  | LP
  | LESSEQUAL
  | LESS
  | INT of (int)
  | IF
  | IDE of (string)
  | GREATEREQUAL
  | GREATER
  | FUNCTION
  | FOR
  | FALSE
  | EQUAL
  | EOF
  | END
  | ELSE
  | DOT
  | DO
  | DIVISION
  | COMMA
  | COLON
  | BEGIN
  | ASSIGN
  | ARRAY
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.program)
