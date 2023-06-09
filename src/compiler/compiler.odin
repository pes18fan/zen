package compiler

import "core:fmt"
import "core:strconv"

import dbg "../debug"
import ch "../chunk"
import lx "../lexer"
import val "../value"

/* Maximum limit for a eight bit unsigned integer. */
U8_MAX :: 255

/* The language parser. */
Parser :: struct {
    tokens: []lx.Token,
    curr_idx: int,
    current: lx.Token,
    previous: lx.Token,
    had_error: bool,
    panic_mode: bool,
    compiling_chunk: ^ch.Chunk,
}

/* Expression precedence. */
Precedence :: enum {
    NONE,
    ASSIGNMENT,  // =
    OR,          // or
    AND,         // and
    EQUALITY,    // == !=
    COMPARISON,  // < > <= >=
    TERM,        // + -
    FACTOR,      // * /
    UNARY,       // not -
    CALL,        // . ()
    PRIMARY,
}

/* A function that can parse a certain expression. */
ParseFn :: #type proc (p: ^Parser)

/* 
A rule for parsing an expression. Has a prefix parsing function, an
infix parsing function and a precedence, all of which may be nil or
equivalent to nil.
*/
ParseRule :: struct {
    prefix: ParseFn,
    infix: ParseFn,
    precedence: Precedence,
}

/* Report an error at the provided token with a message. */
@(private)
error_at :: proc (p: ^Parser, token: ^lx.Token, message: string) {
    if p.panic_mode do return
    p.panic_mode = true
    fmt.eprintf("[line %d] Error", token.line)

    if token.type == lx.TokenType.EOF {
        fmt.eprintf(" at end")
    } else if token.type == lx.TokenType.ILLEGAL {
        // nothin'
    } else {
        fmt.eprintf(" at '%s'", token.lexeme)
    }

    fmt.eprintf(": %s\n", message)
    p.had_error = true
}

/* Report an error at the token just parsed. */
@(private)
error :: proc (p: ^Parser, message: string) {
    error_at(p, &p.previous, message)
}

/* Report an error at the current token. */
@(private)
error_at_current :: proc (p: ^Parser, message: string) {
    error_at(p, &p.current, message)
}

/*
Advance to the next token. Reports an error if that token is an
ILLEGAL token variant.
*/
@(private)
advance :: proc (p: ^Parser) #no_bounds_check {
    p.previous = p.current

    for {
        if p.current.type == .EOF do break

        p.current = p.tokens[p.curr_idx]
        p.curr_idx += 1
        if p.current.type != lx.TokenType.ILLEGAL do break

        error_at_current(p, p.current.lexeme)
    }
}

/*
Advance to the next token if it matches the provided type, else
report an error at that token. */
@(private)
consume :: proc (p: ^Parser, type: lx.TokenType, message: string) {
    if p.current.type == type {
        advance(p)
        return
    }

    error_at_current(p, message)
}

/* Write a byte to the chunk being compiled. */
@(private)
emit_byte :: proc (p: ^Parser, byait: byte) {
    ch.write_chunk(p.compiling_chunk, byait, p.previous.line)
}

/* Write two bytes to the current chunk. */
@(private)
emit_bytes :: proc (p: ^Parser, byait1: byte, byait2: byte) {
    emit_byte(p, byait1)
    emit_byte(p, byait2)
}

/* Write a OP_RETURN to the current chunk. */
@(private)
emit_return :: proc (p: ^Parser) {
    emit_byte(p, byte(ch.OpCode.OP_RETURN))
}

/* 
Add a constant to the current chunk's constant pool. Reports an error if
there are too many constants.
TODO: Increase the limit of the constant pool from 255 to 65535.
*/
@(private)
make_constant :: proc (p: ^Parser, value: val.Value) -> byte {
    constant := ch.add_constant(p.compiling_chunk, value)
    if constant > U8_MAX {
        error(p, "Too many constants in one chunk.")
        return 0
    }

    return byte(constant)
}

/* Write a OP_CONSTANT to the current chunk. */
@(private)
emit_constant :: proc (p: ^Parser, value: val.Value) {
    emit_bytes(p, byte(ch.OpCode.OP_CONSTANT), make_constant(p, value))
}

/* Emit a return instruction and decode the RLE-encoded lines. */
@(private)
end_compiler :: proc (p: ^Parser) {
    emit_return(p)

    when ODIN_DEBUG {
        if !p.had_error {
            dbg.disassemble(p.compiling_chunk, "code")
        }
    }
}

/* Get a rule for the provided token type from the rule table. */
@(private)
get_rule :: proc (type: lx.TokenType) -> ^ParseRule {
    return &rules[type]
}

/* Parse a binary expression and emit it to the chunk. */
@(private)
binary :: proc (p: ^Parser) {
    using ch.OpCode

    operator_type := p.previous.type
    rule := get_rule(operator_type)
    parse_precedence(p, Precedence(byte(rule.precedence) + 1))

    #partial switch operator_type {
        case .BANG_EQUAL: emit_bytes(p, byte(OP_EQUAL), byte(OP_NOT))
        case .EQUAL_EQUAL: emit_byte(p, byte(OP_EQUAL))
        case .GREATER: emit_byte(p, byte(OP_GREATER))
        case .GREATER_EQUAL: emit_bytes(p, byte(OP_LESS), byte(OP_NOT))
        case .LESS: emit_byte(p, byte(OP_LESS))
        case .LESS_EQUAL: emit_bytes(p, byte(OP_GREATER), byte(OP_NOT))
        case .MINUS: emit_byte(p, byte(OP_SUBTRACT))
        case .PLUS: emit_byte(p, byte(OP_ADD))
        case .SLASH: emit_byte(p, byte(OP_DIVIDE))
        case .STAR: emit_byte(p, byte(OP_MULTIPLY))
        case: return // Unreachable.
    }
}

/* Parse a literal value and emit it to the chunk. */
@(private)
literal :: proc (p: ^Parser) {
    using lx.TokenType
    using ch.OpCode

    #partial switch p.previous.type {
        case FALSE: emit_byte(p, byte(OP_FALSE))
        case NIL: emit_byte(p, byte(OP_NIL))
        case TRUE: emit_byte(p, byte(OP_TRUE))
        case: return // Unreachable.
    }
}

/* Parse a grouping (parenthesized) expression. */
@(private)
grouping :: proc (p: ^Parser) {
    expression(p)
    consume(p, lx.TokenType.RPAREN, "Expect ')' after expression.")
}

/* 
Parse a number and emit that constant to the chunk. Numbers are currently
only parsed as 64-bit floats, which is subject to change.
*/
@(private)
number :: proc (p: ^Parser) {
    value, ok := strconv.parse_f64(p.previous.lexeme)
    if !ok {
        error(p, 
            fmt.tprintf("Failed to parse '%s' into f64.", p.previous.lexeme))
    }

    emit_constant(p, val.number_val(value))
}

/* Parse a unary expression. */
@(private)
unary :: proc (p: ^Parser) {
    using ch.OpCode
    operator_type := p.previous.type

    // Compile the operand.
    parse_precedence(p, .UNARY)

    // Emit the operator instruction.
    #partial switch operator_type {
        case .NOT: emit_byte(p, byte(OP_NOT))
        case .MINUS: emit_byte(p, byte(OP_NEGATE))
        case: return // Unreachable.
    }
}

/* A table of the parsing rules for all the token types. */
rules: []ParseRule = {
    lx.TokenType.LPAREN        = ParseRule{ grouping, nil,    .NONE },
    lx.TokenType.RPAREN        = ParseRule{ nil,      nil,    .NONE },
    lx.TokenType.LSQUIRLY      = ParseRule{ nil,      nil,    .NONE },
    lx.TokenType.RSQUIRLY      = ParseRule{ nil,      nil,    .NONE },
    lx.TokenType.COMMA         = ParseRule{ nil,      nil,    .NONE },
    lx.TokenType.DOT           = ParseRule{ nil,      nil,    .NONE },
    lx.TokenType.MINUS         = ParseRule{ unary,    binary, .TERM },
    lx.TokenType.PLUS          = ParseRule{ nil,      binary, .TERM },
    lx.TokenType.SEMI          = ParseRule{ nil,      nil,    .NONE },
    lx.TokenType.SLASH         = ParseRule{ nil,      binary, .FACTOR },
    lx.TokenType.STAR          = ParseRule{ nil,      binary, .FACTOR },
    lx.TokenType.BANG_EQUAL    = ParseRule{ nil,      binary, .EQUALITY },
    lx.TokenType.EQUAL         = ParseRule{ nil,      nil,    .NONE },
    lx.TokenType.EQUAL_EQUAL   = ParseRule{ nil,      binary, .EQUALITY },
    lx.TokenType.GREATER       = ParseRule{ nil,      binary, .COMPARISON },
    lx.TokenType.GREATER_EQUAL = ParseRule{ nil,      binary, .COMPARISON },
    lx.TokenType.LESS          = ParseRule{ nil,      binary, .COMPARISON },
    lx.TokenType.LESS_EQUAL    = ParseRule{ nil,      binary, .COMPARISON },
    lx.TokenType.IDENT         = ParseRule{ nil,      nil,    .NONE },
    lx.TokenType.STRING        = ParseRule{ nil,      nil,    .NONE },
    lx.TokenType.NUMBER        = ParseRule{ number,   nil,    .NONE },
    lx.TokenType.AND           = ParseRule{ nil,      nil,    .AND },
    lx.TokenType.BREAK         = ParseRule{ nil,      nil,    .NONE },
    lx.TokenType.ELSE          = ParseRule{ nil,      nil,    .NONE },
    lx.TokenType.FALSE         = ParseRule{ literal,  nil,    .NONE },
    lx.TokenType.FINAL         = ParseRule{ nil,      nil,    .NONE },
    lx.TokenType.FOR           = ParseRule{ nil,      nil,    .NONE },
    lx.TokenType.FUN           = ParseRule{ nil,      nil,    .NONE },
    lx.TokenType.IF            = ParseRule{ nil,      nil,    .NONE },
    lx.TokenType.IMPORT        = ParseRule{ nil,      nil,    .NONE },
    lx.TokenType.IN            = ParseRule{ nil,      nil,    .NONE },
    lx.TokenType.NIL           = ParseRule{ literal,  nil,    .NONE },
    lx.TokenType.NOT           = ParseRule{ unary,    nil,    .NONE },
    lx.TokenType.OR            = ParseRule{ nil,      nil,    .NONE },
    lx.TokenType.PRIVATE       = ParseRule{ nil,      nil,    .NONE },
    lx.TokenType.RETURN        = ParseRule{ nil,      nil,    .NONE },
    lx.TokenType.TRUE          = ParseRule{ literal,  nil,    .NONE },
    lx.TokenType.WRITE         = ParseRule{ nil,      nil,    .NONE },
    lx.TokenType.ILLEGAL       = ParseRule{ nil,      nil,    .NONE },
    lx.TokenType.EOF           = ParseRule{ nil,      nil,    .NONE },
}

/* 
Starting at the current token, parse an expression at the given precedence
level or higher.
*/
@(private)
parse_precedence :: proc (p: ^Parser, precedence: Precedence) {
    advance(p)
    prefix_rule := get_rule(p.previous.type).prefix
    if (prefix_rule == nil) {
        error(p, "Expect expression.")
        return
    }

    prefix_rule(p)

    for precedence <= get_rule(p.current.type).precedence {
        advance(p)
        infix_rule := get_rule(p.previous.type).infix
        infix_rule(p)
    }
}

/* Parse any expression. */
@(private)
expression :: proc (p: ^Parser) {
    parse_precedence(p, .ASSIGNMENT)
}

/* Compile the provided slice of tokens into a bytecode chunk. */
compile :: proc (tokens: []lx.Token, chunk: ^ch.Chunk) -> bool {
    p := Parser{
        tokens = tokens,
        curr_idx = 0,
        had_error = false,
        panic_mode = false,
        compiling_chunk = chunk,
    }

    advance(&p)
    expression(&p)
    consume(&p, lx.TokenType.EOF, "Expect end of expression.")
    end_compiler(&p)

    return !p.had_error
}
