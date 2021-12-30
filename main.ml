let pipeline filename =
  let lexbuf = Lexing.from_channel (open_in filename) in
  let ast = Parser.program Lexer.lex lexbuf in
  Codegeneration.from_ast ast

let print_var_list varlist =
  List.iter
    (fun ele ->
      Typechecker.VariableMap.iter
        (fun k v -> Printf.printf "%s: %s\n" k (Typechecker.type_to_string v))
        ele;
      print_newline ())
    varlist

let load_lex () = Lexing.from_channel stdin
let generate_ast lexbuf = Parser.program Lexer.lex lexbuf
