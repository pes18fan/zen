package zen

import "core:fmt"
import "core:strconv"

/* Maximum limit for a eight bit unsigned integer. */
U8_MAX :: 255

/* The language parser. */
Parser :: struct {
    tokens: []Token,
    curr_idx: int,
    current: Token,
    previous: Token,
    had_error: bool,
    panic_mode: bool,
    compiling_chunk: ^Chunk,
    vm: ^VM,
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
ParseFn :: #type proc (p: ^Parser, can_assign: bool)

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
@(private="file")
error_at :: proc (p: ^Parser, token: ^Token, message: string) {
    if p.panic_mode do return
    p.panic_mode = true
    fmt.eprintf("[line %d] Error", token.line)

    if token.type == TokenType.EOF {
        fmt.eprintf(" at end")
    } else if token.type == TokenType.ILLEGAL {
        // nothin'
    } else {
        fmt.eprintf(" at '%s'", token.lexeme)
    }

    fmt.eprintf(": %s\n", message)
    p.had_error = true
}

/* Report an error at the token just parsed. */
@(private="file")
error :: proc (p: ^Parser, message: string) {
    error_at(p, &p.previous, message)
}

/* Report an error at the current token. */
@(private="file")
error_at_current :: proc (p: ^Parser, message: string) {
    error_at(p, &p.current, message)
}

/*
Advance to the next token. Reports an error if that token is an
ILLEGAL token variant.
*/
@(private="file")
advance :: proc (p: ^Parser) #no_bounds_check {
    p.previous = p.current

    for {
        if p.current.type == .EOF do break

        p.current = p.tokens[p.curr_idx]
        p.curr_idx += 1
        if p.current.type != TokenType.ILLEGAL do break

        error_at_current(p, p.current.lexeme)
    }
}

/*
Advance to the next token if it matches the provided type, else
report an error at that token. */
@(private="file")
consume :: proc (p: ^Parser, type: TokenType, message: string) {
    if p.current.type == type {
        advance(p)
        return
    }

    error_at_current(p, message)
}

@(private="file")
check :: proc (p: ^Parser, type: TokenType) -> bool {
    return p.current.type == type
}

@(private="file")
match :: proc (p: ^Parser, type: TokenType) -> bool {
    if !check(p, type) { return false }
    advance(p)
    return true
}

/* Write a byte to the chunk being compiled. */
@(private="file")
emit_byte :: proc (p: ^Parser, byait: byte) {
    write_chunk(p.compiling_chunk, byait, p.previous.line)
}

/* Write two bytes to the current chunk. */
@(private="file")
emit_bytes :: proc (p: ^Parser, byait1: byte, byait2: byte) {
    emit_byte(p, byait1)
    emit_byte(p, byait2)
}

/* Write a OP_RETURN to the current chunk. */
@(private="file")
emit_return :: proc (p: ^Parser) {
    emit_byte(p, byte(OpCode.OP_RETURN))
}

/* 
Add a constant to the current chunk's constant pool. Reports an error if
there are too many constants.
TODO: Increase the limit of the constant pool from 255 to 65535.
*/
@(private="file")
make_constant :: proc (p: ^Parser, value: Value) -> byte {
    constant := add_constant(p.compiling_chunk, value)
    if constant > U8_MAX {
        error(p, "Too many constants in one chunk.")
        return 0
    }

    return byte(constant)
}

/* Write a OP_CONSTANT to the current chunk. */
@(private="file")
emit_constant :: proc (p: ^Parser, value: Value) {
    emit_bytes(p, byte(OpCode.OP_CONSTANT), make_constant(p, value))
}

/* Emit a return instruction and decode the RLE-encoded lines. */
@(private="file")
end_compiler :: proc (p: ^Parser) {
    emit_return(p)

    when ODIN_DEBUG {
        if !p.had_error {
            disassemble(p.compiling_chunk, "code")
        }
    }
}

/* Get a rule for the provided token type from the rule table. */
@(private="file")
get_rule :: proc (type: TokenType) -> ^ParseRule {
    return &rules[type]
}

/* Parse a binary expression and emit it to the chunk. */
@(private="file")
binary :: proc (p: ^Parser, can_assign: bool) {
    using OpCode

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
@(private="file")
literal :: proc (p: ^Parser, can_assign: bool) {
    using TokenType
    using OpCode

    #partial switch p.previous.type {
        case FALSE: emit_byte(p, byte(OP_FALSE))
        case NIL: emit_byte(p, byte(OP_NIL))
        case TRUE: emit_byte(p, byte(OP_TRUE))
        case: return // Unreachable.
    }
}

/* Parse a grouping (parenthesized) expression. */
@(private="file")
grouping :: proc (p: ^Parser, can_assign: bool) {
    expression(p)
    consume(p, TokenType.RPAREN, "Expect ')' after expression.")
}

/* 
Parse a number and emit that constant to the chunk. Numbers are currently
only parsed as 64-bit floats, which is subject to change.
*/
@(private="file")
number :: proc (p: ^Parser, can_assign: bool) {
    value, ok := strconv.parse_f64(p.previous.lexeme)
    if !ok {
        error(p, 
            fmt.tprintf("Failed to parse '%s' into f64.", p.previous.lexeme))
    }

    emit_constant(p, number_val(value))
}

@(private="file")
zstring :: proc (p: ^Parser, can_assign: bool) {
    emit_constant(p, obj_val(
        copy_string(p.vm, p.previous.lexeme[1:len(p.previous.lexeme)-1])))
}

@(private="file")
named_variable :: proc (p: ^Parser, name: Token, can_assign: bool) {
    name := name
    arg := identifier_constant(p, &name)

    if can_assign && match(p, .EQUAL) {
        expression(p)
        emit_bytes(p, byte(OpCode.OP_SET_GLOBAL), arg)
    } else {
        emit_bytes(p, byte(OpCode.OP_GET_GLOBAL), arg)
    }
}

@(private="file")
variable :: proc (p: ^Parser, can_assign: bool) {
    named_variable(p, p.previous, can_assign)
}

/* Parse a unary expression. */
@(private="file")
unary :: proc (p: ^Parser, can_assign: bool) {
    using OpCode
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
    TokenType.LPAREN        = ParseRule{ grouping, nil,    .NONE },
    TokenType.RPAREN        = ParseRule{ nil,      nil,    .NONE },
    TokenType.LSQUIRLY      = ParseRule{ nil,      nil,    .NONE },
    TokenType.RSQUIRLY      = ParseRule{ nil,      nil,    .NONE },
    TokenType.COMMA         = ParseRule{ nil,      nil,    .NONE },
    TokenType.DOT           = ParseRule{ nil,      nil,    .NONE },
    TokenType.MINUS         = ParseRule{ unary,    binary, .TERM },
    TokenType.PLUS          = ParseRule{ nil,      binary, .TERM },
    TokenType.SEMI          = ParseRule{ nil,      nil,    .NONE },
    TokenType.SLASH         = ParseRule{ nil,      binary, .FACTOR },
    TokenType.STAR          = ParseRule{ nil,      binary, .FACTOR },
    TokenType.BANG_EQUAL    = ParseRule{ nil,      binary, .EQUALITY },
    TokenType.EQUAL         = ParseRule{ nil,      nil,    .NONE },
    TokenType.EQUAL_EQUAL   = ParseRule{ nil,      binary, .EQUALITY },
    TokenType.GREATER       = ParseRule{ nil,      binary, .COMPARISON },
    TokenType.GREATER_EQUAL = ParseRule{ nil,      binary, .COMPARISON },
    TokenType.LESS          = ParseRule{ nil,      binary, .COMPARISON },
    TokenType.LESS_EQUAL    = ParseRule{ nil,      binary, .COMPARISON },
    TokenType.IDENT         = ParseRule{ variable, nil,    .NONE },
    TokenType.STRING        = ParseRule{ zstring,  nil,    .NONE },
    TokenType.NUMBER        = ParseRule{ number,   nil,    .NONE },
    TokenType.AND           = ParseRule{ nil,      nil,    .AND },
    TokenType.BREAK         = ParseRule{ nil,      nil,    .NONE },
    TokenType.ELSE          = ParseRule{ nil,      nil,    .NONE },
    TokenType.FALSE         = ParseRule{ literal,  nil,    .NONE },
    TokenType.FINAL         = ParseRule{ nil,      nil,    .NONE },
    TokenType.FOR           = ParseRule{ nil,      nil,    .NONE },
    TokenType.FUN           = ParseRule{ nil,      nil,    .NONE },
    TokenType.IF            = ParseRule{ nil,      nil,    .NONE },
    TokenType.IMPORT        = ParseRule{ nil,      nil,    .NONE },
    TokenType.IN            = ParseRule{ nil,      nil,    .NONE },
    TokenType.NIL           = ParseRule{ literal,  nil,    .NONE },
    TokenType.NOT           = ParseRule{ unary,    nil,    .NONE },
    TokenType.OR            = ParseRule{ nil,      nil,    .NONE },
    TokenType.PRIVATE       = ParseRule{ nil,      nil,    .NONE },
    TokenType.RETURN        = ParseRule{ nil,      nil,    .NONE },
    TokenType.TRUE          = ParseRule{ literal,  nil,    .NONE },
    TokenType.WRITE         = ParseRule{ nil,      nil,    .NONE },
    TokenType.ILLEGAL       = ParseRule{ nil,      nil,    .NONE },
    TokenType.EOF           = ParseRule{ nil,      nil,    .NONE },
}

/* 
Starting at the current token, parse an expression at the given precedence
level or higher.
*/
@(private="file")
parse_precedence :: proc (p: ^Parser, precedence: Precedence) {
    advance(p)
    prefix_rule := get_rule(p.previous.type).prefix
    if (prefix_rule == nil) {
        error(p, "Expect expression.")
        return
    }

    can_assign := precedence <= Precedence.ASSIGNMENT
    prefix_rule(p, can_assign)

    for precedence <= get_rule(p.current.type).precedence {
        advance(p)
        infix_rule := get_rule(p.previous.type).infix
        infix_rule(p, can_assign)
    }

    if can_assign && match(p, .EQUAL) {
        error(p, "Invalid assignment target.")
    }
}

@(private="file")
identifier_constant :: proc (p: ^Parser, name: ^Token) -> u8 {
    return make_constant(p, obj_val(copy_string(p.vm, name.lexeme)))
}

@(private="file")
parse_variable :: proc (p: ^Parser, error_message: string) -> u8 {
    consume(p, .IDENT, error_message)
    return identifier_constant(p, &p.previous)
}

@(private="file")
define_variable :: proc (p: ^Parser, global: u8) {
    emit_bytes(p, byte(OpCode.OP_DEFINE_GLOBAL), global)
}

/* Parse any expression. */
@(private="file")
expression :: proc (p: ^Parser) {
    parse_precedence(p, .ASSIGNMENT)
}

@(private="file")
let_declaration :: proc (p: ^Parser) {
    global := parse_variable(p, "Expect variable name.")

    if match(p, .EQUAL) {
        expression(p)
    } else {
        emit_byte(p, byte(OpCode.OP_NIL))
    }

    consume(p, .SEMI, "Expect ';' after variable declaration.")

    define_variable(p, global)
}

@(private="file")
expression_statement :: proc (p: ^Parser) {
    expression(p)
    consume(p, .SEMI, "Expect ';' after expression.")
    emit_byte(p, byte(OpCode.OP_POP))
}

@(private="file")
write_statement :: proc (p: ^Parser) {
    expression(p)
    consume(p, .SEMI, "Expect ';' after value.")
    emit_byte(p, byte(OpCode.OP_WRITE))
}

@(private="file")
synchronize :: proc (p: ^Parser) {
    p.panic_mode = false

    for p.current.type != .EOF {
        if p.previous.type == .SEMI {
            return
        }

        #partial switch p.current.type {
            case .FUN, .FOR, .IF, .WRITE, .RETURN:
                return
            case: // Do nothing.
        }

        advance(p)
    }
}

@(private="file")
declaration :: proc (p: ^Parser) {
    if match(p, .LET) {
        let_declaration(p)
    } else {
        statement(p)
    }

    if p.panic_mode {
        synchronize(p)
    }
}

@(private="file")
statement :: proc (p: ^Parser) {
    if match(p, .WRITE) {
        write_statement(p)
    } else {
        expression_statement(p)
    }
}

/* Compile the provided slice of tokens into a bytecode chunk. */
compile :: proc (vm: ^VM, tokens: []Token, chunk: ^Chunk) -> bool {
    p := Parser{
        tokens = tokens,
        curr_idx = 0,
        had_error = false,
        panic_mode = false,
        compiling_chunk = chunk,
        vm = vm,
    }

    advance(&p)

    for !match(&p, .EOF) {
        declaration(&p)
    }

    end_compiler(&p)

    return !p.had_error
}
