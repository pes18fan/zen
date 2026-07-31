% zen(1) | General Commands Manual

NAME
====

zen - a statically typed programming language

SYNOPSIS
====

`zen [script] [--compile] [--dump] [--dump-ast] [--dump-tokens] [--exec CODE] [--log-checker] [--log-gc] [--stress-gc] [--time] [--trace] [--version] [ARGUMENTS]`

DESCRIPTION
====

zen is a statically typed programming language written in Odin, with
Hindley-Milner style type inference. The zen program
is a bytecode interpreter for this language.

It aims to have a familiar syntax while being easy to use with various features.
It supports functional programming paradigms, including first-class functions
and closures. More
information about the language can be found in the documentation at 
`https://github.com/pes18fan/zen/blob/main/DOCUMENTATION.md`.

When run without any arguments, it starts a REPL session, else it evaluates
the file passed to it and passes on the arguments after a `--` to the language
interpreter.

OPTIONS
====

`-h, --help`
:   Show the help message and exit

`--version`
:   Show the current program version and exit

`--time`
:   Record time taken to compile and run

`--compile`
:   Compile only, useful with --dump

`--dump`
:   Dump disassembled bytecode

`--trace`
:   Trace script execution

`--log-checker`
:   Log the type checker

`--log-gc`
:   Log garbage collection

`--stress-gc`
:   Collect garbage on every allocation

`--dump-tokens`
:   Dump tokens from lexer and exit

`--dump-ast`
: Dump the abstract syntax tree from the parser and exit

`--exec <code>`
:   Execute a string of zen code directly

`--script <file>`
:   Input script; omit to use the REPL instead

EXIT STATUS
====

0
: Success

65
: Lex, parse, or compile error

66
: Error reading file

70
: Runtime error

Other exit statuses may be observed as well depending on the executed program,
as zen's exit expression allows ending the program with a custom exit.

BUGS
====

Bugs can be reported and filed at https://www.github.com/pes18fan/zen/issues

SEE ALSO
====

The full language documentation is available at
https://github.com/pes18fan/zen/blob/main/DOCUMENTATION.md
