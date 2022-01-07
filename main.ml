let pipeline filename =
  open_in filename
  |> Lexing.from_channel
  |> Parser.program Lexer.lex
  |> Typechecker.type_check_program
  |> Codegeneration.generation_pipeline



