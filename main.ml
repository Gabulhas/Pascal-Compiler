
let pipeline filename =
    let lexbuf = Lexing.from_channel (open_in filename) in
    let ast = Parser.program Lexer.lex lexbuf in
    Codegeneration.from_ast ast


let load_lex () = Lexing.from_channel stdin
let generate_ast lexbuf = 
    Parser.program Lexer.lex lexbuf
