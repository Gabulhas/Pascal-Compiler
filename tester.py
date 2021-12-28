from os import listdir
from os.path import isfile, join
mypath = "tests/"
onlyfiles = sorted([f for f in listdir(mypath) if isfile(join(mypath, f))])
result_string = ""
for file in onlyfiles:
    result_string = result_string + f"let lexbuf = Lexing.from_channel (open_in \"./tests/{file}\");;\n Parser.program Lexer.lex lexbuf;;\nLexer.lex lexbuf;;\n"


result_string = result_string + "\nexit 1\n"

print(result_string)
