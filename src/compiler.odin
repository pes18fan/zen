package zen

import "core:fmt"
import "core:strconv"

/* Maximum limit for a eight bit unsigned integer. */
U8_MAX :: 255

/* Maximum limit for a sixteen bit unsigned integer. */
U16_MAX :: 65535

/* Number of eight bit unsigned integers in existence. */
U8_COUNT :: 256

/* The language parser. */
Parser :: struct {
    tokens: []Token,
    curr_idx: int,
    current: Token,
    previous: Token,
    had_error: bool,
    panic_mode: bool,
    compiling_chunk: ^Chunk,
    current_compiler: ^Compiler,
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

/*
A local variable.
*/
Local :: struct {
    name: Token,
    depth: int,
    final: bool,
}

Loop :: struct {
    start: int,
    scope_depth: int,
    breaks: [dynamic]int,
}

/*
A struct that holds variables and scope info.
*/
Compiler :: struct {
    globals: ^Table,
    locals: [U8_COUNT]Local,
    local_count: int,
    loops: [U8_COUNT]Loop,
    loop_count: int,
    scope_depth: int,
}

/* Report an error at the provided token with a message. */
@(private="file")
error_at :: proc (p: ^Parser, token: ^Token, message: string) {
    if p.panic_mode do return
    p.panic_mode = true
    fmt.eprintf("\e[31mcompile error\e[0m")

    if token.type == TokenType.EOF {
        fmt.eprintf(" at end")
    } else if token.type == TokenType.ILLEGAL {
        // nothin'
    } else {
        fmt.eprintf(" at '%s'", token.lexeme)
    }

    fmt.eprintf(": %s\n", message)
    fmt.eprintf("  on [line %d]\n", token.line)
    p.had_error = true
}

/* Report an error at the token just parsed. */
@(private="file")
error :: proc (p: ^Parser, message: string, args: ..any) {
    error_at(p, &p.previous, message)
}

/* Report an error at the current token. */
@(private="file")
error_at_current :: proc (p: ^Parser, message: string, args: ..any) {
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

/* Check if the token to be consumed is of the provided type. */
@(private="file")
check :: proc (p: ^Parser, type: TokenType) -> bool {
    return p.current.type == type
}

/* Consume the next token if it matches the provided type. */
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

emit_loop :: proc (p: ^Parser, loop_start: int) {
    emit_byte(p, byte(OpCode.OP_LOOP))

    offset := len(p.compiling_chunk.code) - loop_start + 2
    if offset > U16_MAX { error(p, "Loop body too large.") }

    emit_byte(p, (u8(offset) >> 8) & 0xff)
    emit_byte(p, u8(offset) & 0xff)
}

/*
Write a jump instruction to the current chunk.
Initially writes only placeholder bytes for the jump offset, which
will be filled in later.
*/
@(private="file")
emit_jump :: proc (p: ^Parser, instruction: byte) -> int {
    emit_byte(p, instruction)
    emit_byte(p, 0xff)
    emit_byte(p, 0xff)
    return len(p.compiling_chunk.code) - 2
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

/*
Patch a jump instruction at the provided offset in the current chunk.
Jump instructions are initially written with placeholder offsets, which
are filled in here, after the amount to jump over is known.
*/
@(private="file")
patch_jump :: proc (p: ^Parser, offset: int) {
    // -2 to adjust for the bytecode for the jump offset itself.
    jump := len(p.compiling_chunk.code) - offset - 2

    if jump > U16_MAX {
        error(p, "Too much code to jump over.")
    }

    p.compiling_chunk.code[offset] = byte((jump >> 8) & 0xff)
    p.compiling_chunk.code[offset + 1] = byte(jump & 0xff)
}

/*
Return a new Compiler struct.
The globals table is inherited from previous runs of the compiler so that
final variables' "finality" can be preserved in the global scope.
*/
init_compiler :: proc (globals: ^Table) -> Compiler {
    return Compiler {
        local_count = 0,
        loop_count = 0,
        scope_depth = 0,
        globals = globals,
    }
}

/*
Free the dynamic array of break jump offsets. 
TODO: Nothing else really needs to be freed, so this stands out. Find a
better way to do this.
*/
@(private="file")
free_loops :: proc (p: ^Parser) {
    for i in p.current_compiler.loops {
        delete(i.breaks)
    }
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

    free_loops(p)
}

/* Begin a new scope. */
@(private="file")
begin_scope :: proc (p: ^Parser) {
    p.current_compiler.scope_depth += 1
}

/* 
End the current scope. 
This also pops off any local variables that were declared in the scope.
*/
@(private="file")
end_scope :: proc (p: ^Parser) {
    p.current_compiler.scope_depth -= 1

    for p.current_compiler.local_count > 0 &&
        p.current_compiler.locals[p.current_compiler.local_count - 1].depth > 
            p.current_compiler.scope_depth {
        emit_byte(p, byte(OpCode.OP_POP))
        p.current_compiler.local_count -= 1
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
        case .BANG_EQUAL:    emit_bytes(p, byte(OP_EQUAL), byte(OP_NOT))
        case .EQUAL_EQUAL:   emit_byte(p, byte(OP_EQUAL))
        case .GREATER:       emit_byte(p, byte(OP_GREATER))
        case .GREATER_EQUAL: emit_bytes(p, byte(OP_LESS), byte(OP_NOT))
        case .LESS:          emit_byte(p, byte(OP_LESS))
        case .LESS_EQUAL:    emit_bytes(p, byte(OP_GREATER), byte(OP_NOT))
        case .MINUS:         emit_byte(p, byte(OP_SUBTRACT))
        case .PLUS:          emit_byte(p, byte(OP_ADD))
        case .SLASH:         emit_byte(p, byte(OP_DIVIDE))
        case .STAR:          emit_byte(p, byte(OP_MULTIPLY))
        case: return // Unreachable.
    }
}

/* Parse a literal value and emit it to the chunk. */
@(private="file")
literal :: proc (p: ^Parser, can_assign: bool) {
    using OpCode

    #partial switch p.previous.type {
        case .FALSE: emit_byte(p, byte(OP_FALSE))
        case .NIL: emit_byte(p, byte(OP_NIL))
        case .TRUE: emit_byte(p, byte(OP_TRUE))
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

or_ :: proc (p: ^Parser, can_assign: bool) {
    else_jump := emit_jump(p, byte(OpCode.OP_JUMP_IF_FALSE))
    end_jump := emit_jump(p, byte(OpCode.OP_JUMP))

    patch_jump(p, else_jump)
    emit_byte(p, byte(OpCode.OP_POP))

    parse_precedence(p, .OR)
    patch_jump(p, end_jump)
}

/* Parse a string and emit that constant to the chunk. */
@(private="file")
zstring :: proc (p: ^Parser, can_assign: bool) {
    emit_constant(p, obj_val(
        copy_string(p.vm, p.previous.lexeme[1:len(p.previous.lexeme)-1])))
}

/*
Parse a previously declared variable. This is used for both reading and
assigning to variables, and for reading constants declared with `final`.
This function will error if there is an attempt to assign to a constant.
*/
@(private="file")
named_variable :: proc (p: ^Parser, name: Token, 
    can_assign: bool) {
    name := name
    get_op, set_op: u8
    arg: u8
    possible_local := resolve_local(p, &name)

    if possible_local != -1 {
        arg = u8(possible_local)
        get_op = byte(OpCode.OP_GET_LOCAL)
        set_op = byte(OpCode.OP_SET_LOCAL)
    } else {
        arg = identifier_constant(p, &name)
        get_op = byte(OpCode.OP_GET_GLOBAL)
        set_op = byte(OpCode.OP_SET_GLOBAL)
    }

    if can_assign && match(p, .EQUAL) {
        if set_op == byte(OpCode.OP_SET_LOCAL) {
            for i := p.current_compiler.local_count - 1; i >= 0; i -= 1 {
                local := &p.current_compiler.locals[i]
                if identifiers_equal(&name, &local.name) && local.final {
                    error(p, 
                        "Can only set a final variable once.")
                }
            }
        } 

        if set_op == byte(OpCode.OP_SET_GLOBAL) {
            global_o_str := copy_string(p.vm, name.lexeme)
            value: Value; ok: bool

            if value, ok := table_get(p.current_compiler.globals, 
                global_o_str); ok {
                if values_equal(value, bool_val(true)) {
                    error(p, 
                        "Can only set a final variable once.")
                }
            } else {
                table_set(p.current_compiler.globals,
                    global_o_str, bool_val(false))
            }
        }

        expression(p)
        emit_bytes(p, set_op, arg)
    } else {
        emit_bytes(p, get_op, arg)
    }
}

/* Parses a variable expression. Also used to parse assignments. */
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
    TokenType.AND           = ParseRule{ nil,      and_,   .AND },
    TokenType.BREAK         = ParseRule{ nil,      nil,    .NONE },
    TokenType.ELSE          = ParseRule{ nil,      nil,    .NONE },
    TokenType.FALSE         = ParseRule{ literal,  nil,    .NONE },
    TokenType.FINAL         = ParseRule{ nil,      nil,    .NONE },
    TokenType.FOR           = ParseRule{ nil,      nil,    .NONE },
    TokenType.FN            = ParseRule{ nil,      nil,    .NONE },
    TokenType.IF            = ParseRule{ nil,      nil,    .NONE },
    TokenType.IMPORT        = ParseRule{ nil,      nil,    .NONE },
    TokenType.IN            = ParseRule{ nil,      nil,    .NONE },
    TokenType.LET           = ParseRule{ nil,      nil,    .NONE },
    TokenType.NIL           = ParseRule{ literal,  nil,    .NONE },
    TokenType.NOT           = ParseRule{ unary,    nil,    .NONE },
    TokenType.OR            = ParseRule{ nil,      or_,    .OR   },
    TokenType.PRINT         = ParseRule{ nil,      nil,    .NONE },
    TokenType.RETURN        = ParseRule{ nil,      nil,    .NONE },
    TokenType.TRUE          = ParseRule{ literal,  nil,    .NONE },
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

    /* Error raised if trying to assign to something that you can't
    assign to. */
    if can_assign && match(p, .EQUAL) {
        error(p, "Invalid assignment target.")
    }
}

/* Parse an identifier name. */
@(private="file")
identifier_constant :: proc (p: ^Parser, name: ^Token) -> u8 {
    return make_constant(p, obj_val(copy_string(p.vm, name.lexeme)))
}

/* Check if two idents are equal. */
@(private="file")
identifiers_equal :: proc (a: ^Token, b: ^Token) -> bool {
    if len(a.lexeme) != len(b.lexeme) { return false }
    return a.lexeme == b.lexeme
}

/* 
Resolve a local name binding from the Compiler struct.
*/
@(private="file")
resolve_local :: proc (p: ^Parser, name: ^Token) -> int {
    for i := p.current_compiler.local_count - 1; i >= 0; i -= 1 {
        local := &p.current_compiler.locals[i]
        if identifiers_equal(name, &local.name) {
            if local.depth == -1 {
                error(p, 
                    "Cannot read local variable in its own initializer.")
            }
            return i
        }
    }

    return -1
}

/* 
Add a local name binding.
Errors if there are too many local variables in the scope already.
*/
@(private="file")
add_local :: proc (p: ^Parser, name: Token, final: bool) {
    if p.current_compiler.local_count == U8_COUNT {
        error(p, "Too many local variables in function.")
        return
    }

    defer p.current_compiler.local_count += 1
    local := &p.current_compiler.locals[p.current_compiler.local_count]
    local.name = name
    local.depth = -1
    local.final = final
}

/* 
Declare a name binding.
Errors if the variable of that name already exists in the scope.
*/
@(private="file")
declare_variable :: proc (p: ^Parser, final: bool) {
    if p.current_compiler.scope_depth == 0 { return }

    name := &p.previous

    for i := p.current_compiler.local_count - 1; i >= 0; i -= 1 {
        local := &p.current_compiler.locals[i]
        if local.depth != -1 && local.depth < p.current_compiler.scope_depth {
            break
        }

        if identifiers_equal(name, &local.name) {
            error(p, 
                "A variable with this name in this scope already exists.")
        }
    }

    add_local(p, name^, final)
}

/* Parse a variable or `final` declaration. */
@(private="file")
parse_variable :: proc (p: ^Parser, error_message: string, 
    final: bool) -> u8 {
    consume(p, .IDENT, error_message)

    declare_variable(p, final)
    if p.current_compiler.scope_depth > 0 { return 0 }

    global_o_str := copy_string(p.vm, p.previous.lexeme)
    value: Value; ok: bool

    if value, ok := table_get(p.current_compiler.globals, global_o_str); ok {
        if values_equal(value, bool_val(true)) {
            error(p, 
                final ? "Cannot redefine a final variable." : 
                    "Cannot redefine a final variable as normal variable.")
        } else if final {
            error(p, "Cannot redefine a variable as final variable.")
        }
    } else {
        table_set(p.current_compiler.globals, global_o_str, bool_val(final))
    }

    return identifier_constant(p, &p.previous)
}

/* Mark a local name binding as initialized. */
@(private="file")
mark_initialized :: proc (p: ^Parser) {
    p.current_compiler.locals[p.current_compiler.local_count - 1].depth = 
        p.current_compiler.scope_depth
}

/* Define a local or global name binding. */
@(private="file")
define_variable :: proc (p: ^Parser, global: u8) {
    if p.current_compiler.scope_depth > 0 { 
        mark_initialized(p)
        return 
    }

    emit_bytes(p, byte(OpCode.OP_DEFINE_GLOBAL), global)
}

and_ :: proc (p: ^Parser, can_assign: bool) {
    end_jump := emit_jump(p, byte(OpCode.OP_JUMP_IF_FALSE))

    emit_byte(p, byte(OpCode.OP_POP))
    parse_precedence(p, .AND)

    patch_jump(p, end_jump)
}

/* Parse any expression. */
@(private="file")
expression :: proc (p: ^Parser) {
    parse_precedence(p, .ASSIGNMENT)
}

/* Parse a block. */
@(private="file")
block :: proc (p: ^Parser) {
    for !check(p, .RSQUIRLY) && !check(p, .EOF) {
        declaration(p)
    }

    consume(p, .RSQUIRLY, "Expect '}' after block.")
}

/* Parse a name binding. */
@(private="file")
let_declaration :: proc (p: ^Parser) {
    final := p.previous.type == .FINAL
    global := parse_variable(p, 
        final ? "Expect final variable name." : "Expect variable name.", final)

    if match(p, .EQUAL) {
        expression(p)
    } else {
        if final {
            error(p, "Final variables must be initialized.")
        } else {
            emit_byte(p, byte(OpCode.OP_NIL))
        }
    }

    consume(p, .SEMI, "Expect ';' after variable declaration.")

    define_variable(p, global)
}

/* Parse an expression statement. */
@(private="file")
expression_statement :: proc (p: ^Parser) {
    expression(p)
    consume(p, .SEMI, "Expect ';' after expression.")
    emit_byte(p, byte(OpCode.OP_POP))
}

/* Parse an if statement. */
@(private="file")
if_statement :: proc (p: ^Parser) {
    expression(p)
    consume(p, .LSQUIRLY, "Expect '{' after if condition.")

    then_jump := emit_jump(p, byte(OpCode.OP_JUMP_IF_FALSE))
    emit_byte(p, byte(OpCode.OP_POP))

    begin_scope(p)
    block(p)
    end_scope(p)

    else_jump := emit_jump(p, byte(OpCode.OP_JUMP))

    patch_jump(p, then_jump)
    emit_byte(p, byte(OpCode.OP_POP))

    if match(p, .ELSE) {
        consume(p, .LSQUIRLY, "Expect '{' after else.")
        block(p)
    }
    patch_jump(p, else_jump)
}

/* Parse a `write` statement. */
@(private="file")
print_statement :: proc (p: ^Parser) {
    expression(p)
    consume(p, .SEMI, "Expect ';' after value.")
    emit_byte(p, byte(OpCode.OP_PRINT))
}

@(private="file")
begin_loop :: proc (p: ^Parser, loop_start: int) {
    if p.current_compiler.loop_count >= U8_COUNT {
        error(p, "Too many nested loops.")
        return
    }

    loop := &p.current_compiler.loops[p.current_compiler.loop_count]
    p.current_compiler.loop_count += 1
    loop.start = loop_start
    loop.scope_depth = p.current_compiler.scope_depth
}

@(private="file")
end_loop :: proc (p: ^Parser) {
    assert(p.current_compiler.loop_count > 0)
    patch_breaks(p)
    p.current_compiler.loop_count -= 1
}


/*
Patch the jump offsets of any break statements present in the current loop.
Called right after a loop is parsed. 
*/
@(private="file")
patch_breaks :: proc (p: ^Parser) {
    loop := &p.current_compiler.loops[p.current_compiler.loop_count - 1]

    for i in loop.breaks {
        patch_jump(p, i)
    }
}

/* Parse a `break` statement. */
@(private="file")
break_statement :: proc (p: ^Parser) {
    if p.current_compiler.loop_count == 0 {
        error(p, "Cannot 'break' outside a loop.")
        return
    }

    consume(p, .SEMI, "Expect ';' after 'break'.")

    loop := &p.current_compiler.loops[p.current_compiler.loop_count - 1]
    append(&loop.breaks, emit_jump(p, byte(OpCode.OP_JUMP)))

    // Discard correct number of values from the stack.
    for i := p.current_compiler.local_count - 1; i >= 0; i -= 1 {
        local := &p.current_compiler.locals[i]
        if local.depth < loop.scope_depth {
            break
        }
    }
}

@(private="file")
continue_statement :: proc (p: ^Parser) {
    if p.current_compiler.loop_count == 0 {
        error(p, "Cannot use 'continue' outside a loop.")
        return
    }

    consume(p, .SEMI, "Expect ';' after 'continue'.")

    loop := &p.current_compiler.loops[p.current_compiler.loop_count - 1]

    // Discard correct number of values from the stack.
    for i := p.current_compiler.local_count - 1; i >= 0; i -= 1 {
        local := &p.current_compiler.locals[i]
        if local.depth < loop.scope_depth {
            break
        }
    }

    emit_loop(p, loop.start)
}

/* Parse a while statment. */
@(private="file")
while_statement :: proc (p: ^Parser) {
    loop_start := len(p.compiling_chunk.code)

    begin_loop(p, loop_start)
    expression(p)
    consume(p, .LSQUIRLY, "Expect '{' after while loop condition.")

    exit_jump := emit_jump(p, byte(OpCode.OP_JUMP_IF_FALSE))
    emit_byte(p, byte(OpCode.OP_POP))

    begin_scope(p)
    block(p)
    end_scope(p)

    emit_loop(p, loop_start)

    patch_jump(p, exit_jump)
    emit_byte(p, byte(OpCode.OP_POP))
    end_loop(p)
}

@(private="file")
for_statement :: proc (p: ^Parser) {
    begin_scope(p)
    if match(p, .SEMI) {
        // No initializer.
    } else if match(p, .LET) {
        let_declaration(p)
    } else {
        expression_statement(p)
    }

    loop_start := len(p.compiling_chunk.code)
    exit_jump := -1
    if !match(p, .SEMI) {
        expression(p)
        consume(p, .SEMI, "Expect ';' after loop condition.")

        // Jump out of the loop if the condition is false.
        exit_jump = emit_jump(p, byte(OpCode.OP_JUMP_IF_FALSE))
        emit_byte(p, byte(OpCode.OP_POP)) // Condition.
    }

    // Jump over the increment, run the body, and jump back to the
    // increment, then go to the next iteration.
    if !match(p, .LSQUIRLY) {
        body_jump := emit_jump(p, byte(OpCode.OP_JUMP))
        increment_start := len(p.compiling_chunk.code)

        expression(p)
        emit_byte(p, byte(OpCode.OP_POP))
        consume(p, .LSQUIRLY, "Expect '{' after for clauses.")

        emit_loop(p, loop_start)
        loop_start = increment_start
        patch_jump(p, body_jump)
    }

    begin_loop(p, loop_start)
    block(p)

    emit_loop(p, loop_start)

    if exit_jump != -1 {
        patch_jump(p, exit_jump)
        emit_byte(p, byte(OpCode.OP_POP)) // Condition.
    }

    end_loop(p)
    end_scope(p)
}

/* 
Discard all tokens until reaching a synchronization point, like a
statement terminator or any tokens that begin a statement. 
*/
@(private="file")
synchronize :: proc (p: ^Parser) {
    p.panic_mode = false

    for p.current.type != .EOF {
        if p.previous.type == .SEMI {
            return
        }

        #partial switch p.current.type {
            case .FN, .FOR, .IF, .LET, .PRINT, .PUB, .RETURN, .WHILE:
                return
            case: // Do nothing.
        }

        advance(p)
    }
}

/* Parse a declaration. */
@(private="file")
declaration :: proc (p: ^Parser) {
    if match(p, .LET) || match(p, .FINAL) {
        let_declaration(p)
    } else {
        statement(p)
    }

    if p.panic_mode {
        synchronize(p)
    }
}

/* Parse a statement. */
@(private="file")
statement :: proc (p: ^Parser) {
    if match(p, .BREAK) {
        break_statement(p)
    } else if match(p, .CONTINUE) {
        continue_statement(p)
    } else if match(p, .PRINT) {
        print_statement(p)
    } else if match(p, .IF) {
        if_statement(p)
    } else if match(p, .WHILE) {
        while_statement(p)
    } else if match(p, .FOR) {
        for_statement(p)
    } else if match(p, .LSQUIRLY) {
        begin_scope(p)
        block(p)
        end_scope(p)
    } else {
        expression_statement(p)
    }
}

/* 
Compile the provided slice of tokens into a bytecode chunk. 
The compile function also takes in a pointer to a globals table. The main 
reason behind this is to keep track of any final variables declared. This is 
useful in the REPL but not so much in a file, since in the REPL the compiler
recompiles every line but that's not necessary in a file.

TODO: Find a better way to store global variables so that this whole
table-passing thing isn't necessary.
*/
compile :: proc (vm: ^VM, tokens: []Token, chunk: ^Chunk, globals: ^Table) -> 
    bool {
    c := init_compiler(globals)
    p := Parser{
        tokens = tokens,
        curr_idx = 0,
        had_error = false,
        panic_mode = false,
        compiling_chunk = chunk,
        current_compiler = &c,
        vm = vm,
    }

    advance(&p)

    for !match(&p, .EOF) {
        declaration(&p)
    }

    end_compiler(&p)

    return !p.had_error
}
