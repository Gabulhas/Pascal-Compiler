# Pascal Compiler to X86_64

## How to run

### Using the compile.sh script
- run `./compile.sh [PROGRAM NAME]` to  compile, assemble and execute a program.

### Step by Step
- run the `make` command to build the Compiler.
- run `pascaml [PROGRAM NAME]` to compile a pascal file to X86_64 assembly.
- run `gcc -no-pie -g test.s -o [OUTPUT NAME]` to assemble the file to an executable.
- run the output file.
