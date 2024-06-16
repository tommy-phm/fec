# FEC
A transpiler that translates Rust code into C code. It can accept a small subset of Rust. This project was developed as part of a class assignment for a compiler course. 

## Supported Features
- Error Checking: Lexical, syntax, semantic, data type
- Data Types: boolean, int, double, string
- Structures: Functions, arrays
- Built-in Functions: println!, read!, format!
- Operations: *, +, /, -, &&, ||, !, <, <=, >, >=, ==
- Control Statements: while, for, if-else

## Notes
- Boolean data types are treated as int.
- Macros are treated as functions.
- There is no local scope
- No precedence order of operations.
- 100 statment limit per block


## Main Files
- token: Defines token.
- rustlex (Lexical Analyzer): Converts source code into tokens. Each token has its own regex pattern. Adapoted from [bleibig's lexer.l](https://github.com/bleibig/rust-grammar/blob/master/lexer.l).
- rustgram (Syntax Analyzer): Assembles the stream of tokens into a parse tree. Adapoted from [bleibig's parser-lalr.y](https://github.com/bleibig/rust-grammar/blob/master/parser-lalr.y).
- symtab (Semantic Analysis): Generates symbol tables for the global scope and each function. Performs type checking.
- tac (Code Generator): Generates the C program. Variables are stored in a union called data which can hold int, float, and char[64]. Variables are stored in three arrays: global for global variables, local for function-local variables, and string for constant strings.

## Functions
- read!(var): Returns a string of var.
- format!(string, var1, var2): Returns a formatted string with var1 and var2.
- println!(string, var): Prints to the terminal.

## Usage
### Requirements
- Flex
- Bison

To create the transpiler:
`make`

Example command to generate a test program:
`./fec -c ../test.rs`

### Command-Line Options
- -dot: Generate a DOT file of the parse tree
- -tree: Display the token tree
- -sym: Display the symbol tree
- -code: Display the symbol tree with intermediate code
- -c: Generate a C file and compile it
- input_file: Rust source file (supports multipe files)

## Why "fec"?
The name "fec" stands for "fe compiler," with "fe" being a reference to the Rust programming language. This reflects the project's goal of translating (or compiling) a subset of Rust code into C code.

