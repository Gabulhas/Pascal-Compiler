
let load_lex () = Lexing.from_channel stdin
let generate_ast lexbuf = 
    Parser.program Lexer.lex lexbuf
