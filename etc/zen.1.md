% zen(1) | General Commands Manual

NAME
====

zen - a dynamically typed programming language

SYNOPSIS
====

`zen [-hvtCDTLSc?] [--dump-tokens] [FILE] [--] [ARGUMENTS]`

DESCRIPTION
====

zen is a dynamically typed programming language written in Odin. The zen program
is a bytecode interpreter for this language.

It aims to have a familiar syntax while being easy to use with new features.
It supports both object oriented and functional programming paradigms. More
information about the language can be found in the documentation at 
`https://github.com/pes18fan/zen/blob/main/DOCUMENTATION.md`.

When run without any arguments, it starts a REPL session, else it evaluates
the file passed to it and passes on the arguments after a `--` to the language
interpreter.

OPTIONS
====

`-h, -?, --help`

:   Show the help message and exit

`-v, --version`

:   Show the current program version and exit

`-t, --time`

:   Record time taken to compile and run

`-C, --compile`

:   Compile only, useful with -D

`-D, --dump`

:   Dump disassembled bytecode

`-T, --trace`

:   Trace script execution

`-L, --log-gc`

:   Log garbage collection

`-S, --stress-gc`

:   Collect garbage on every allocation

`--dump-tokens`

:   Dump tokens from lexer and exit

BUGS
====

It is known that programs making use of OOP patterns may have unexpected problems
while running. However, its not yet known what exactly the issue is or why it occurs.

Bugs can be reported and filed at https://www.github.com/pes18fan/zen/issues
