{ (* HEADER *)

open Parser;;

exception UnknownChar;;

let remove_quotes my_str =
    String.sub my_str 1 ((String.length my_str) -2)

}

(* REGULAR DEFINITIONS *)

let integer = '-'? ['0'-'9']+
let string = '"' [^'"']* '"'
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let ident = letter (letter | digit | '_')*


let space = [' ' '\t']
let comment = "#" [^'\n']*


(* RULES *)

rule lex =
  parse integer         { INT (int_of_string (Lexing.lexeme lexbuf))    }
      | string          { PSTRING (remove_quotes (Lexing.lexeme lexbuf)) }

      | "true"          { TRUE }
      | "false"         { FALSE }

      | "program"       { PROGRAM }
      | "var"           { VAR }
      | "Array"         { ARRAY }
      | "of"            { OF }
      | "int"           { TINT }
      | "boolean"       { TBOOLEAN }
      | "char"          { TCHAR }
      | "str"           { TSTRING }
      | "procedure"     { PROCEDURE }
      | "function"      { FUNCTION }
      | "begin"         { BEGIN }
      | "end"           { END }
      | "if"            { IF }
      | "then"          { THEN }
      | "else"          { ELSE }
      | "while"         { WHILE }
      | "for"           { FOR }
      | "to"            { TO }
      | "do"            { DO}
      | "write"         { WRITE }
      | "read"         { READ }
      (*| "call"           { CALL }*)

      | ident           { IDE (Lexing.lexeme lexbuf) }

      | "+"             { PLUS }
      | "-"             { MINUS }
      | "*"             { TIMES }
      | "/"             { DIVISION }

      | "="             { EQUAL }
      | "<="            { LESSEQUAL }
      | "<"             { LESS }
      | ">="            { GREATEREQUAL } 
      | ">"             { GREATER }

      | "&"             { AND }
      | "|"             { OR }
      | "!"             { NOT }
      | ":="            { ASSIGN }

      | ";"             { SEMICOLON }
      | ":"             { COLON }
      | ","             { COMMA }

      | "("             { LP }
      | ")"             { RP }
      | "["             { LS }
      | "]"             { RS }

      | "."             { DOT }

      | [' ' '\t' '\n'] { lex lexbuf }
      | eof             { EOF }

      | _               { raise UnknownChar }
