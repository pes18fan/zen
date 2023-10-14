package zen

import "core:fmt"
import "core:strings"
import "core:strconv"

/* Maximum limit for a eight bit unsigned integer. */
U8_MAX :: 255

/* Maximum limit for a sixteen bit unsigned integer. */
U16_MAX :: 65535

/* Number of eight bit unsigned integers in existence. */
U8_COUNT :: 256

/* The language parser. */
Parser :: struct {
	tokens:           []Token,
	curr_idx:         int,
	current:          Token,
	previous:         Token,
	had_error:        bool,
	panic_mode:       bool,
	compiling_chunk:  ^Chunk,
	current_compiler: ^Compiler,
	globals:          ^Table,
	pipeline_state:   PipelineState,
	gc:               ^GC,
	prev_mark_roots:  RootSource,
}

/* Expression precedence. */
Precedence :: enum {
	NONE,
	ASSIGNMENT, // =
	PIPE, // |>
	OR, // or
	AND, // and
	EQUALITY, // == !=
	COMPARISON, // < > <= >=
	TERM, // + -
	FACTOR, // * /
	UNARY, // not -
	CALL, // . ()
	PRIMARY,
}

/* A function that can parse a certain expression. */
ParseFn :: #type proc(p: ^Parser, can_assign: bool)

/* 
A rule for parsing an expression. Has a prefix parsing function, an
infix parsing function and a precedence, all of which may be nil or
equivalent to nil.
*/
ParseRule :: struct {
	prefix:     ParseFn,
	infix:      ParseFn,
	precedence: Precedence,
}

/* Whether a variable is reassignable or not. */
Variability :: enum {
	VAR,
	FINAL,
}

/* Whether a value is currently being piped through functions. */
PipelineState :: enum {
	NONE,
	ACTIVE,
}

/*
A local variable.
*/
Local :: struct {
	name:        Token,
	depth:       int,
	final:       Variability,
	is_captured: bool,
}

/*
An upvalue. References a local variable in an enclosing function.
Each closure keeps an array of upvalues, which include all the variables
that the closure closes over.
*/
Upvalue :: struct {
	index:    int,
	is_local: bool,
}

/*
A loop.
`breaks` refers to an array of bytecode offsets to patch any break statement
jumps to.
*/
Loop :: struct {
	start:       int,
	scope_depth: int,
	breaks:      [dynamic]int,
}

/* 
The type of a function. The "top level" of a program, called "script" here, 
is also considered an implicit function.
*/
FunctionType :: enum {
	LAMBDA,
	FUNCTION,
	SCRIPT,
}

/*
A struct that holds variables and scope info.
Each Compiler struct is associated with a function, and each also points back
to the Compiler struct of the function that encloses it, forming a linked list
of Compiler structs.
*/
Compiler :: struct {
	enclosing:   ^Compiler, // The Compiler for the enclosing function.
	function:    ^ObjFunction, // The function being compiled.
	type:        FunctionType, // The type of function being compiled.
	globals:     ^Table, // Hash table storing global variables.
	locals:      [U8_COUNT]Local, // Array of local variables. 
	local_count: int, // Number of local variables.
	upvalues:    [U8_COUNT]Upvalue, // Array of upvalues.
	loops:       [U8_COUNT]Loop, // Array of loops.
	loop_count:  int, // Number of loops.
	scope_depth: int, // The number of blocks currently in scope.
}

current_chunk :: proc(p: ^Parser) -> ^Chunk {
	return &p.current_compiler.function.chunk
}

/* Report an error at the provided token with a message. */
@(private = "file")
error_at :: proc(p: ^Parser, token: ^Token, message: string) {
	if p.panic_mode do return
	p.panic_mode = true
	fmt.eprintf("%scompile error%s ", COL_RED, RESET)

	if token.type == TokenType.EOF {
		fmt.eprintf("at end")
	} else {
		fmt.eprintf("at '%s'", token.lexeme)
	}

	fmt.eprintf(": %s\n", message)
	fmt.eprintf("  on [line %d]\n", token.line)
	p.had_error = true
}

/* Report an error at the token just parsed. */
@(private = "file")
error :: proc(p: ^Parser, message: string, args: ..any) {
	error_at(p, &p.previous, message)
}

/* Report an error at the current token. */
@(private = "file")
error_at_current :: proc(p: ^Parser, message: string, args: ..any) {
	error_at(p, &p.current, message)
}

/*
Advance to the next token. Reports an error if that token is an
ILLEGAL token variant.
*/
@(private = "file")
advance :: proc(p: ^Parser) #no_bounds_check {
	p.previous = p.current

	if p.current.type == .EOF {return}

	p.current = p.tokens[p.curr_idx]
	p.curr_idx += 1
}

/*
Advance to the next token if it matches the provided type, else
report an error at that token. */
@(private = "file")
consume :: proc(p: ^Parser, type: TokenType, message: string) {
	if p.current.type == type {
		advance(p)
		return
	}

	error_at_current(
		p,
		fmt.tprintf(
			"%s Got %s instead.",
			message,
			"nothing" if p.current.type == .EOF else p.current.lexeme,
		),
	)
}

@(private = "file")
consume_semi :: proc(p: ^Parser, message: string) {
	consume(p, .SEMI, fmt.tprintf("Expect ';' after %s.", message))
}

/* Check if the token to be consumed is of the provided type. */
@(private = "file")
check :: proc(p: ^Parser, type: TokenType) -> bool {
	return p.current.type == type
}

/* Consume the next token if it matches the provided type. */
@(private = "file")
match :: proc(p: ^Parser, type: TokenType) -> bool {
	if !check(p, type) {return false}
	advance(p)
	return true
}

/* Write a byte to the chunk being compiled. */
@(private = "file")
emit_byte :: proc(p: ^Parser, byait: byte) {
	write_chunk(current_chunk(p), byait, p.previous.line)
}

/* Write an opcode as a byte to the chunk being compiled. */
@(private = "file")
emit_opcode :: proc(p: ^Parser, oc: OpCode) {
	write_chunk(current_chunk(p), byte(oc), p.previous.line)
}

/* Write two bytes to the current chunk. */
@(private = "file")
emit_bytes :: proc(p: ^Parser, byait1: byte, byait2: byte) {
	emit_byte(p, byait1)
	emit_byte(p, byait2)
}

/* Write two opcodes to the current chunk. */
@(private = "file")
emit_opcodes :: proc(p: ^Parser, oc1: OpCode, oc2: OpCode) {
	emit_opcode(p, oc1)
	emit_opcode(p, oc2)
}

/* Write a pop instruction to the current chunk. */
emit_pop :: proc(p: ^Parser) {
	emit_opcode(p, .OP_POP)
}

/* Write a noop (no operation) instruction to the current chunk. */
emit_noop :: proc(p: ^Parser) {
	emit_opcode(p, .OP_NOOP)
}

/* Write a loop instruction to the current chunk. */
emit_loop :: proc(p: ^Parser, loop_start: int) {
	emit_opcode(p, .OP_LOOP)

	offset := len(current_chunk(p).code) - loop_start + 2
	if offset > U16_MAX {error(p, "Loop body too large.")}

	emit_byte(p, (u8(offset) >> 8) & 0xff)
	emit_byte(p, u8(offset) & 0xff)
}

/*
Write a jump instruction to the current chunk.
Initially writes only placeholder bytes for the jump offset, which
will be filled in later.
*/
@(private = "file")
emit_jump :: proc(p: ^Parser, instruction: OpCode) -> int {
	emit_opcode(p, instruction)
	emit_byte(p, 0xff)
	emit_byte(p, 0xff)
	return len(current_chunk(p).code) - 2
}

/* 
Write an OP_NIL and OP_RETURN to the current chunk.
OP_NIL is written since a function without a return value implicitly returns
nil.
*/
@(private = "file")
emit_return :: proc(p: ^Parser) {
	emit_opcodes(p, .OP_NIL, .OP_RETURN)
}

/* 
Add a constant to the current chunk's constant pool. Reports an error if
there are too many constants.
TODO: Increase the limit of the constant pool from 255 to 65535.
*/
@(private = "file")
make_constant :: proc(p: ^Parser, value: Value) -> byte {
	constant := add_constant(current_chunk(p), p.gc, value)
	if constant > U8_MAX {
		error(p, "Too many constants in one chunk.")
		return 0
	}

	return byte(constant)
}

/* Write a OP_CONSTANT to the current chunk. */
@(private = "file")
emit_constant :: proc(p: ^Parser, value: Value) {
	emit_bytes(p, byte(OpCode.OP_CONSTANT), make_constant(p, value))
}

/*
Patch a jump instruction at the provided offset in the current chunk.
Jump instructions are initially written with placeholder offsets, which
are filled in here, after the amount to jump over is known.
*/
@(private = "file")
patch_jump :: proc(p: ^Parser, offset: int) {
	// -2 to adjust for the bytecode for the jump offset itself.
	jump := len(current_chunk(p).code) - offset - 2

	if jump > U16_MAX {
		error(p, "Too much code to jump over.")
	}

	current_chunk(p).code[offset] = byte((jump >> 8) & 0xff)
	current_chunk(p).code[offset + 1] = byte(jump & 0xff)
}

/*
Free the dynamic array of break jump offsets. 
TODO: Nothing else really needs to be freed, so this stands out. Find a
better way to do this.
*/
@(private = "file")
free_loops :: proc(p: ^Parser) {
	for i in p.current_compiler.loops {
		delete(i.breaks)
	}
}

/*
Return a new Compiler struct.
The globals table is inherited from previous runs of the compiler so that
final variables' "finality" can be preserved in the global scope.
Also makes sure that the current_compiler is set to the new compiler.
*/
init_compiler :: proc(c: ^Compiler, p: ^Parser, type: FunctionType) {
	c^ = Compiler {
		enclosing   = p.current_compiler,
		type        = type,
		local_count = 0,
		loop_count  = 0,
		scope_depth = 0,
		function    = nil,
		globals     = p.globals,
	}

	/* DON'T FORGET to set current_compiler to the new compiler!
	Took me WAAAAAAAAAAY too long to figure out I was missing this. */
	p.current_compiler = c

	c.function = new_function(p.gc)

	if type != .SCRIPT && type != .LAMBDA {
		c.function.name = copy_string(p.gc, p.previous.lexeme)
	} else if type == .LAMBDA {
		c.function.name = copy_string(p.gc, "lambda")
	}

	// The first slot is always the function itself.
	local := &p.current_compiler.locals[p.current_compiler.local_count]
	c.local_count += 1
	local.depth = 0
	local.is_captured = false // You can't capture the slot zero function.
	local.name.lexeme = ""
}

/* Emit a return instruction and decode the RLE-encoded lines. */
@(private = "file")
end_compiler :: proc(p: ^Parser) -> ^ObjFunction {
	fn := p.current_compiler.function
	if !fn.has_returned {
		emit_return(p)
	}

	if debug_flags.dump_disassembly {
		if !p.had_error {
			disassemble(current_chunk(p), fn.name != nil ? fn.name.chars : "<script>")
		}
	}

	free_loops(p)
	p.current_compiler = p.current_compiler.enclosing

	return fn
}

/* Begin a new scope. */
@(private = "file")
begin_scope :: proc(p: ^Parser) {
	p.current_compiler.scope_depth += 1
}

/* 
End the current scope. 
This also pops off any local variables that were declared in the scope.
*/
@(private = "file")
end_scope :: proc(p: ^Parser) {
	curr := p.current_compiler
	curr.scope_depth -= 1

	for curr.local_count > 0 && curr.locals[curr.local_count - 1].depth > curr.scope_depth {
		/* If the local was captured, close its upvalue instead of popping it
		to allow closures to work. Closing the upvalue requires no operand,
		since the upvalue to close is right on top of the stack at this point. */
		if curr.locals[curr.local_count - 1].is_captured {
			emit_opcode(p, .OP_CLOSE_UPVALUE)
		} else {
			emit_pop(p)
		}
		p.current_compiler.local_count -= 1
	}
}

/* Get a rule for the provided token type from the rule table. */
@(private = "file")
get_rule :: proc(type: TokenType) -> ^ParseRule {
	return &rules[type]
}

/* Parse a binary expression and emit it to the chunk. */
@(private = "file")
binary :: proc(p: ^Parser, can_assign: bool) {
	operator_type := p.previous.type
	rule := get_rule(operator_type)
	parse_precedence(p, Precedence(byte(rule.precedence) + 1))

	#partial switch operator_type {
	case .BANG_EQUAL:
		emit_opcodes(p, .OP_EQUAL, .OP_NOT)
	case .EQUAL_EQUAL:
		emit_opcode(p, .OP_EQUAL)
	case .GREATER:
		emit_opcode(p, .OP_GREATER)
	case .GREATER_EQUAL:
		emit_opcodes(p, .OP_LESS, .OP_NOT)
	case .LESS:
		emit_opcode(p, .OP_LESS)
	case .LESS_EQUAL:
		emit_opcodes(p, .OP_GREATER, .OP_NOT)
	case .MINUS:
		emit_opcode(p, .OP_SUBTRACT)
	case .PLUS:
		emit_opcode(p, .OP_ADD)
	case .SLASH:
		emit_opcode(p, .OP_DIVIDE)
	case .STAR:
		emit_opcode(p, .OP_MULTIPLY)
	case:
		unreachable()
	}
}

/* Parse a function call. */
@(private = "file")
call :: proc(p: ^Parser, can_assign: bool) {
	arg_count: u8 = 0
	if p.pipeline_state == .ACTIVE {
		emit_opcode(p, .OP_GET_IT)
		arg_count += 1
	}

	arg_count += argument_list(p)
	emit_bytes(p, byte(OpCode.OP_CALL), arg_count)
}

/* Parse a literal value and emit it to the chunk. */
@(private = "file")
literal :: proc(p: ^Parser, can_assign: bool) {
	#partial switch p.previous.type {
	case .FALSE:
		emit_opcode(p, .OP_FALSE)
	case .NIL:
		emit_opcode(p, .OP_NIL)
	case .TRUE:
		emit_opcode(p, .OP_TRUE)
	case:
		unreachable()
	}

}

/* Parse a grouping (parenthesized) expression. */
@(private = "file")
grouping :: proc(p: ^Parser, can_assign: bool) {
	expression(p)
	consume(p, .RPAREN, "Expect ')' after expression.")
}

/* 
Parse a number and emit that constant to the chunk. Numbers are currently
only parsed as 64-bit floats, which is subject to change.
*/
@(private = "file")
number :: proc(p: ^Parser, can_assign: bool) {
	value, ok := strconv.parse_f64(p.previous.lexeme)
	if !ok {
		error(p, fmt.tprintf("Failed to parse '%s' into f64.", p.previous.lexeme))
	}

	emit_constant(p, number_val(value))
}

/* Parse an `and` expression. */
and_ :: proc(p: ^Parser, can_assign: bool) {
	end_jump := emit_jump(p, .OP_JUMP_IF_FALSE)

	emit_pop(p)
	parse_precedence(p, .AND)

	patch_jump(p, end_jump)
}

/* Parse an `or` expression. */
or_ :: proc(p: ^Parser, can_assign: bool) {
	else_jump := emit_jump(p, .OP_JUMP_IF_FALSE)
	end_jump := emit_jump(p, .OP_JUMP)

	patch_jump(p, else_jump)
	emit_pop(p)

	parse_precedence(p, .OR)
	patch_jump(p, end_jump)
}

/* 
Add a single byte character to a string.
This function allocates memory.
*/
@(private = "file")
concatenate_byte :: proc(a: string, b: byte) -> string {
	defer delete(a)
	len := len(a) + 1
	res := make([]byte, len)
	i := 0
	i = copy(res, a)
	res[i] = b

	return string(res)
}

/*
Translate escape sequences in a string literal.
This function allocates a string, but doesn't take ownership of the input.

So far, only the newline and tab sequences are supported.
*/
@(private = "file")
add_escape_sequences :: proc(str: string) -> string {
	sequences := map[byte]byte {
		'n' = '\n',
		't' = '\t',
	}
	defer delete(sequences)

	escaped := false
	res := ""
	for i, idx in str {
		if i == '\\' && idx + 1 < len(str) && str[idx + 1] in sequences {
			res = concatenate_byte(res, sequences[str[idx + 1]])
			escaped = true
			continue
		}
		if escaped {
			escaped = false
			continue
		}
		res = concatenate_byte(res, byte(i))
	}

	return res
}

/* Parse a string and emit that constant to the chunk. */
@(private = "file")
zstring :: proc(p: ^Parser, can_assign: bool) {
	str := add_escape_sequences(p.previous.lexeme[1:len(p.previous.lexeme) - 1])
	defer delete(str)

	emit_constant(p, obj_val(copy_string(p.gc, str)))
}

/*
Parse a previously declared variable. This is used for both reading and
assigning to variables, and for reading constants declared with `final`.
This function will error if there is an attempt to assign to a constant.
*/
@(private = "file")
named_variable :: proc(p: ^Parser, name: Token, can_assign: bool) {
	name := name
	get_op, set_op: u8
	arg: u8
	possible_local := resolve_local(p, p.current_compiler, &name)
	possible_upvalue := resolve_upvalue(p, p.current_compiler, &name)

	if possible_local != -1 {
		arg = u8(possible_local)
		get_op = byte(OpCode.OP_GET_LOCAL)
		set_op = byte(OpCode.OP_SET_LOCAL)
	} else if possible_upvalue != -1 {
		arg = u8(possible_upvalue)
		get_op = byte(OpCode.OP_GET_UPVALUE)
		set_op = byte(OpCode.OP_SET_UPVALUE)
	} else {
		arg = identifier_constant(p, &name)
		get_op = byte(OpCode.OP_GET_GLOBAL)
		set_op = byte(OpCode.OP_SET_GLOBAL)
	}

	if can_assign && match(p, .EQUAL) {
		if set_op == byte(OpCode.OP_SET_LOCAL) {
			for i := p.current_compiler.local_count - 1; i >= 0; i -= 1 {
				local := &p.current_compiler.locals[i]
				if identifiers_equal(&name, &local.name) && local.final == .FINAL {
					error(p, "Can only set a final variable once.")
				}
			}
		}

		if set_op == byte(OpCode.OP_SET_GLOBAL) {
			global_o_str := copy_string(p.gc, name.lexeme)
			value: Value;ok: bool

			if value, ok := table_get(p.current_compiler.globals, global_o_str); ok {
				if values_equal(value, bool_val(true)) {
					error(p, "Can only set a final variable once.")
				}
			} else {
				table_set(p.current_compiler.globals, global_o_str, bool_val(false))
			}
		}

		expression(p)
		emit_bytes(p, set_op, arg)
	} else {
		emit_bytes(p, get_op, arg)
	}
}

/* Parses a variable expression. Also used to parse assignments. */
@(private = "file")
variable :: proc(p: ^Parser, can_assign: bool) {
	named_variable(p, p.previous, can_assign)
}

@(private = "file")
it_ :: proc(p: ^Parser, can_assign: bool) {
	if p.pipeline_state == .NONE {
		error(p, "Cannot use 'it' outside of a pipeline.")
	}

	emit_opcode(p, .OP_GET_IT)
}

/* Parses an anonymous function. */
@(private = "file")
lambda :: proc(p: ^Parser, can_assign: bool) {
	function(p, .LAMBDA)
}

/* Parses a unary expression. */
@(private = "file")
unary :: proc(p: ^Parser, can_assign: bool) {
	operator_type := p.previous.type

	// Compile the operand.
	parse_precedence(p, .UNARY)

	// Emit the operator instruction.
	#partial switch operator_type {
	case .NOT:
		emit_opcode(p, .OP_NOT)
	case .MINUS:
		emit_opcode(p, .OP_NEGATE)
	}
}

/* A table of the parsing rules for all the token types. */
rules: []ParseRule = {
	TokenType.LPAREN = ParseRule{grouping, call, .CALL},
	TokenType.RPAREN = ParseRule{nil, nil, .NONE},
	TokenType.LSQUIRLY = ParseRule{nil, nil, .NONE},
	TokenType.RSQUIRLY = ParseRule{nil, nil, .NONE},
	TokenType.COMMA = ParseRule{nil, nil, .NONE},
	TokenType.DOT = ParseRule{nil, nil, .NONE},
	TokenType.MINUS = ParseRule{unary, binary, .TERM},
	TokenType.PLUS = ParseRule{nil, binary, .TERM},
	TokenType.SEMI = ParseRule{nil, nil, .NONE},
	TokenType.SLASH = ParseRule{nil, binary, .FACTOR},
	TokenType.STAR = ParseRule{nil, binary, .FACTOR},
	TokenType.BANG_EQUAL = ParseRule{nil, binary, .EQUALITY},
	TokenType.BAR_GREATER = ParseRule{nil, nil, .NONE},
	TokenType.EQUAL = ParseRule{nil, nil, .NONE},
	TokenType.EQUAL_EQUAL = ParseRule{nil, binary, .EQUALITY},
	TokenType.GREATER = ParseRule{nil, binary, .COMPARISON},
	TokenType.GREATER_EQUAL = ParseRule{nil, binary, .COMPARISON},
	TokenType.LESS = ParseRule{nil, binary, .COMPARISON},
	TokenType.LESS_EQUAL = ParseRule{nil, binary, .COMPARISON},
	TokenType.IDENT = ParseRule{variable, nil, .NONE},
	TokenType.STRING = ParseRule{zstring, nil, .NONE},
	TokenType.NUMBER = ParseRule{number, nil, .NONE},
	TokenType.AND = ParseRule{nil, and_, .AND},
	TokenType.BREAK = ParseRule{nil, nil, .NONE},
	TokenType.ELSE = ParseRule{nil, nil, .NONE},
	TokenType.FALSE = ParseRule{literal, nil, .NONE},
	TokenType.FINAL = ParseRule{nil, nil, .NONE},
	TokenType.FOR = ParseRule{nil, nil, .NONE},
	TokenType.FUNC = ParseRule{lambda, nil, .NONE},
	TokenType.IF = ParseRule{nil, nil, .NONE},
	TokenType.IMPORT = ParseRule{nil, nil, .NONE},
	TokenType.IN = ParseRule{nil, nil, .NONE},
	TokenType.IT = ParseRule{it_, nil, .NONE},
	TokenType.LET = ParseRule{nil, nil, .NONE},
	TokenType.NIL = ParseRule{literal, nil, .NONE},
	TokenType.NOT = ParseRule{unary, nil, .NONE},
	TokenType.OR = ParseRule{nil, or_, .OR},
	TokenType.PRINT = ParseRule{nil, nil, .NONE},
	TokenType.RETURN = ParseRule{nil, nil, .NONE},
	TokenType.TRUE = ParseRule{literal, nil, .NONE},
	TokenType.EOF = ParseRule{nil, nil, .NONE},
}

/* 
Starting at the current token, parse an expression at the given precedence
level or higher.
*/
@(private = "file")
parse_precedence :: proc(p: ^Parser, precedence: Precedence) {
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
@(private = "file")
identifier_constant :: proc(p: ^Parser, name: ^Token) -> u8 {
	return make_constant(p, obj_val(copy_string(p.gc, name.lexeme)))
}

/* Check if two idents are equal. */
@(private = "file")
identifiers_equal :: proc(a: ^Token, b: ^Token) -> bool {
	if len(a.lexeme) != len(b.lexeme) {return false}
	return a.lexeme == b.lexeme
}

/* 
Resolve a local name binding from the Compiler struct.
*/
@(private = "file")
resolve_local :: proc(p: ^Parser, compiler: ^Compiler, name: ^Token) -> int {
	// Look for the name in the local scopes of the current function.
	for i := compiler.local_count - 1; i >= 0; i -= 1 {
		local := &compiler.locals[i]
		if identifiers_equal(name, &local.name) {
			if local.depth == -1 {
				error(p, "Cannot read local variable in its own initializer.")
			}
			return i
		}
	}

	// Not found in the scopes of the current function.
	return -1
}

@(private = "file")
add_upvalue :: proc(p: ^Parser, compiler: ^Compiler, index: u8, is_local: bool) -> int {
	upvalue_count := compiler.function.upvalue_count

	// Check if the function already has the upvalue we're about to add.
	for i in 0 ..< upvalue_count {
		upvalue := &compiler.upvalues[i]
		if upvalue.index == int(index) && upvalue.is_local == is_local {
			return i
		}
	}

	if upvalue_count == U8_COUNT {
		error(p, "Too many closure variables in function.")
		return 0
	}

	// Add the upvalue.
	defer compiler.function.upvalue_count += 1
	compiler.upvalues[upvalue_count].is_local = is_local
	compiler.upvalues[upvalue_count].index = int(index)
	return compiler.function.upvalue_count
}

@(private = "file")
resolve_upvalue :: proc(p: ^Parser, compiler: ^Compiler, name: ^Token) -> int {
	/* Base case 1: We reached the end of the compiler stack, so the name is probably in the
	global scope. */
	if compiler.enclosing == nil {return -1}

	/* Look for the name in the enclosing function's local scope. 
	Base case 2: If we find the name there, return it. */
	local := resolve_local(p, compiler.enclosing, name)
	if local != -1 {
		// Mark the local as captured.
		compiler.enclosing.locals[local].is_captured = true
		/* is_local is true since we're capturing a local variable from the
		immediately enclosing function. */
		return add_upvalue(p, compiler, u8(local), true)
	}

	/* Recursively look for an upvalue in the enclosing function. */
	upvalue := resolve_upvalue(p, compiler.enclosing, name)
	if upvalue != -1 {
		/* Once the local variable is found in the most deeply nested recursive call, 
		which is the outermost function, capture it as an upvalue, add it to the 
		current (outermost) function's upvalue list and return the index. That 
		returns to the inner function's declaration, which captures the upvalue
		from that surrounding function from where we just returned, and so on, 
		until we eventually return to the function declaration where the
		identifier we are looking for appears. */
		/* The boolean is_local flag is false since here, we're capturing an
		upvalue which captures either a local variable of its surrounding
		function or another upvalue. */
		return add_upvalue(p, compiler, u8(upvalue), false)
	}

	// Nope, didn't find anything.
	return -1
}

/* 
Add a local name binding.
Errors if there are too many local variables in the scope already.
*/
@(private = "file")
add_local :: proc(p: ^Parser, name: Token, final: Variability) {
	if p.current_compiler.local_count == U8_COUNT {
		error(p, "Too many local variables in function.")
		return
	}

	defer p.current_compiler.local_count += 1
	local := &p.current_compiler.locals[p.current_compiler.local_count]
	local.name = name
	local.depth = -1
	local.final = final
	local.is_captured = false
}

/* 
Declare a name binding.
Errors if the variable of that name already exists in the scope.
*/
@(private = "file")
declare_variable :: proc(p: ^Parser, final: Variability) {
	if p.current_compiler.scope_depth == 0 {return}

	name := &p.previous

	for i := p.current_compiler.local_count - 1; i >= 0; i -= 1 {
		local := &p.current_compiler.locals[i]
		if local.depth != -1 && local.depth < p.current_compiler.scope_depth {
			break
		}

		if identifiers_equal(name, &local.name) {
			error(p, "A variable with this name in this scope already exists.")
		}
	}

	add_local(p, name^, final)
}

/* Parse a variable or `final` declaration. */
@(private = "file")
parse_variable :: proc(p: ^Parser, error_message: string, final: Variability) -> u8 {
	consume(p, .IDENT, error_message)

	declare_variable(p, final)
	if p.current_compiler.scope_depth > 0 {return 0}

	global_o_str := copy_string(p.gc, p.previous.lexeme)
	value: Value;ok: bool

	if value, ok := table_get(p.current_compiler.globals, global_o_str); ok {
		if values_equal(value, bool_val(true)) {
			error(
				p,
				final == .FINAL \
				? "Cannot redefine a final variable." \
				: "Cannot redefine a final variable as normal variable.",
			)
		} else if final == .FINAL {
			error(p, "Cannot redefine a variable as final variable.")
		}
	} else {
		table_set(p.current_compiler.globals, global_o_str, bool_val(final == .FINAL))
	}

	return identifier_constant(p, &p.previous)
}

/* Mark a local name binding as initialized. */
@(private = "file")
mark_initialized :: proc(p: ^Parser) {
	if p.current_compiler.scope_depth == 0 {return}
	p.current_compiler.locals[p.current_compiler.local_count - 1].depth =
		p.current_compiler.scope_depth
}

/* Define a local or global name binding. */
@(private = "file")
define_variable :: proc(p: ^Parser, global: byte) {
	if p.current_compiler.scope_depth > 0 {
		mark_initialized(p)
		return
	}

	emit_bytes(p, byte(OpCode.OP_DEFINE_GLOBAL), global)
}

/* Parse function arguments. */
@(private = "file")
argument_list :: proc(p: ^Parser) -> u8 {
	arg_count: u8 = 0

	if !check(p, .RPAREN) {
		// Read args as long as we see a comma next.
		for {
			expression(p)
			// Arg count can't be more than 255 since it's stuffed in one byte.
			if arg_count == 255 {
				error(p, "Cannot have more than 255 arguments.")
			}
			arg_count += 1

			if !match(p, .COMMA) {break}
		}
	}

	consume(p, .RPAREN, "Expect ')' after arguments.")
	return arg_count
}

/* Parse any expression. */
@(private = "file")
expression :: proc(p: ^Parser) {
	_expression(p, 0)
}

/* Recursive helper for expression(). */
@(private = "file")
_expression :: proc(p: ^Parser, pipes: u8) -> u8 {
	parse_precedence(p, .ASSIGNMENT)

	if pipes >= U8_MAX {
		error(p, "Cannot have more than 255 pipes.")
		return pipes
	}

	if !match(p, .BAR_GREATER) {
		p.pipeline_state = .NONE
		return pipes
	}

	if p.pipeline_state == .NONE {
		p.pipeline_state = .ACTIVE
	}

	emit_opcode(p, .OP_SET_IT)
	return _expression(p, pipes + 1)
}

/* Parse a block. */
@(private = "file")
block :: proc(p: ^Parser) {
	for !check(p, .RSQUIRLY) && !check(p, .EOF) {
		declaration(p)
	}

	consume(p, .RSQUIRLY, "Expect '}' after block.")
}

@(private = "file")
function :: proc(p: ^Parser, type: FunctionType) {
	compiler: Compiler
	init_compiler(&compiler, p, type)

	/* Note that this scope doesn't need to be explicitly closed, since the 
    Compiler itself is ended when we reach the end of the function body. */
	begin_scope(p)

	// Check if the function is anonymous or not.
	if type != .LAMBDA {
		consume(p, .LPAREN, "Expect '(' after function name.")
	} else {
		consume(p, .LPAREN, "Expect '(' after 'func'.")
	}

	if !check(p, .RPAREN) {
		for {
			p.current_compiler.function.arity += 1
			if p.current_compiler.function.arity > 255 {
				error_at_current(p, "Cannot have more than 255 parameters.")
			}

			// Function parameters are mutable. This may change in the future.
			constant := parse_variable(p, "Expect parameter name.", .VAR)
			define_variable(p, constant)

			if !match(p, .COMMA) {break}
		}
	}

	consume(p, .RPAREN, "Expect ')' after function parameters.")

	if match(p, .FAT_ARROW) {
		arrow_function(p, anonymous = type == .LAMBDA)
	} else if match(p, .LSQUIRLY) {
		block(p)
	} else {
		error(p, "Expect '=>' or '{' after function parameter list.")
	}


	function := end_compiler(p)
	emit_bytes(p, byte(OpCode.OP_CLOSURE), make_constant(p, obj_val(function)))

	/* OP_CLOSURE has a variably sized encoding. For each upvalue captured by
	the closure, there are two single-byte operands: a boolean indicating
	whether the upvalue captures a local variable in the immediately enclosing
	function (1) or one of that function's upvalues (0), and the local slot or upvalue
	index to capture. */
	for i in 0 ..< function.upvalue_count {
		emit_byte(p, 1 if compiler.upvalues[i].is_local else 0)
		emit_byte(p, u8(compiler.upvalues[i].index))
	}
}

/* 
Parse the return value of an arrow function.
As simple as parsing the expression and inserting a return instruction.
*/
@(private = "file")
arrow_function :: proc(p: ^Parser, anonymous: bool) {
	expression(p)
	if !anonymous {
		consume_semi(p, "arrow function")
	}

	emit_opcode(p, .OP_RETURN)

	p.current_compiler.function.has_returned = true
}

/* 
Parse a function declaration. 
Functions are first class, so they are parsed like variables.
*/
@(private = "file")
func_declaration :: proc(p: ^Parser) {
	global := parse_variable(p, "Expect function name.", .VAR)
	mark_initialized(p)
	function(p, .FUNCTION)
	define_variable(p, global)
}

/* Parse a name binding. */
@(private = "file")
let_declaration :: proc(p: ^Parser) {
	final: Variability = .FINAL if p.previous.type == .FINAL else .VAR

	for {
		global := parse_variable(
			p,
			final == .FINAL ? "Expect final variable name." : "Expect variable name.",
			final,
		)

		if match(p, .EQUAL) {
			/* Need to do this to allow anonymous functions to recurse by
			referring to the name they've been bound to. */
			if check(p, .FUNC) {
				mark_initialized(p)
			}
			expression(p)
		} else {
			if final == .FINAL {
				error(p, "Final variables must be initialized.")
			} else {
				emit_byte(p, byte(OpCode.OP_NIL))
			}
		}

		define_variable(p, global)

		if !match(p, .COMMA) {break} 	// This allows for multiple declarations
	}

	consume_semi(p, "variable declaration")
}

/* Parse an expression statement. */
@(private = "file")
expression_statement :: proc(p: ^Parser) {
	expression(p)
	consume_semi(p, "expression")
	emit_pop(p)
}

/* Parse an if statement. */
@(private = "file")
if_statement :: proc(p: ^Parser) {
	expression(p)
	consume(p, .LSQUIRLY, "Expect '{' after if condition.")

	then_jump := emit_jump(p, .OP_JUMP_IF_FALSE)
	emit_pop(p)

	begin_scope(p)
	block(p)
	end_scope(p)

	else_jump := emit_jump(p, .OP_JUMP)

	patch_jump(p, then_jump)
	emit_pop(p)

	if match(p, .ELSE) {
		consume(p, .LSQUIRLY, "Expect '{' after else.")
		block(p)
	}
	patch_jump(p, else_jump)
}

/* Parse a `print` statement. */
@(private = "file")
print_statement :: proc(p: ^Parser) {
	expression(p)
	consume_semi(p, "value")
	emit_opcode(p, .OP_PRINT)
}

/* Parse a `return` statement. */
@(private = "file")
return_statement :: proc(p: ^Parser) {
	if p.current_compiler.type == .SCRIPT {
		// I may allow returning from top-level in the future, since it's a
		// useful way to exit early.
		error(p, "Cannot return from top-level code.")
	}

	if match(p, .SEMI) {
		emit_return(p)
	} else {
		expression(p)
		consume_semi(p, "return value")
		emit_opcode(p, .OP_RETURN)
	}

	/* Set a flag to true if the function returns in its outermost scope.
	This flag is to check if the function needs an implicit return in the end. */
	if p.current_compiler.scope_depth == 0 {
		p.current_compiler.function.has_returned = true
	}
}

/* Add a loop to the current compiler's loop stack. */
@(private = "file")
begin_loop :: proc(p: ^Parser, loop_start: int) {
	if p.current_compiler.loop_count >= U8_COUNT {
		error(p, "Too many nested loops.")
		return
	}

	loop := &p.current_compiler.loops[p.current_compiler.loop_count]
	p.current_compiler.loop_count += 1
	loop.start = loop_start
	loop.scope_depth = p.current_compiler.scope_depth
}

/* Remove a loop from the current compiler's loop stack, and patch its break
statements, if any exist. */
@(private = "file")
end_loop :: proc(p: ^Parser) {
	loop := &p.current_compiler.loops[p.current_compiler.loop_count - 1]

	assert(p.current_compiler.loop_count > 0)
	patch_breaks(p)
	p.current_compiler.loop_count -= 1

	/* Remove all the break statements that were in the loop we just ended. */
	clear(&loop.breaks)
}


/*
Patch the jump offsets of any break statements present in the current loop.
Called right after a loop is parsed. 
*/
@(private = "file")
patch_breaks :: proc(p: ^Parser) {
	loop := &p.current_compiler.loops[p.current_compiler.loop_count - 1]

	for i in loop.breaks {
		patch_jump(p, i)
	}
}

/* 
Parse a `break` statement. Only valid inside loops, a `break` outside a loop
is a compile-time error.
*/
@(private = "file")
break_statement :: proc(p: ^Parser) {
	emit_noop(p)
	if p.current_compiler.loop_count == 0 {
		error(p, "Cannot 'break' outside a loop.")
		return
	}

	consume_semi(p, "break")

	loop := &p.current_compiler.loops[p.current_compiler.loop_count - 1]

	// Discard correct number of values from the stack.
	for i := p.current_compiler.local_count - 1; i >= 0; i -= 1 {
		local := &p.current_compiler.locals[i]
		if local.depth < loop.scope_depth {
			break
		}
		emit_opcode(p, .OP_POP)
	}

	append(&loop.breaks, emit_jump(p, .OP_JUMP))
}

/*
Parse a `continue` statement. Only valid inside loops, a `continue` outside
a loop is a compile-time error.
*/
@(private = "file")
continue_statement :: proc(p: ^Parser) {
	if p.current_compiler.loop_count == 0 {
		error(p, "Cannot use 'continue' outside a loop.")
		return
	}

	consume_semi(p, "continue")

	loop := &p.current_compiler.loops[p.current_compiler.loop_count - 1]

	// Discard correct number of values from the stack.
	for i := p.current_compiler.local_count - 1; i >= 0; i -= 1 {
		local := &p.current_compiler.locals[i]
		if local.depth < loop.scope_depth {
			break
		}
		emit_opcode(p, .OP_POP)
	}

	emit_loop(p, loop.start)
}

/*
Parse a switch statement.
I plan to make these into expressions, but for now they're statements.
*/
@(private = "file")
switch_statement :: proc(p: ^Parser) {
	case_jumps_to_end: [dynamic]int
	has_else_clause := false

	defer delete(case_jumps_to_end)
	expression(p)
	consume(p, .LSQUIRLY, "Expect '{' after switch condition.")

	for i := 0; !match(p, .RSQUIRLY); i += 1 {
		if i >= U8_COUNT {
			error(p, "Too many cases in switch statement.")
			return
		}
		assert(i < U8_COUNT)

		if match(p, .ELSE) {
			has_else_clause = true
			emit_pop(p) // Pop the switch value.

			consume(p, .FAT_ARROW, "Expect '=>' after 'else'.")
			statement(p)
			append(&case_jumps_to_end, emit_jump(p, .OP_JUMP))
			consume(p, .RSQUIRLY, "'else' must be the last case.")
			break
		}

		emit_opcode(p, .OP_DUP)
		expression(p)
		emit_opcode(p, .OP_EQUAL)
		case_jump := emit_jump(p, .OP_JUMP_IF_FALSE)

		// If a case matches, pop out both the residual boolean comparison
		// and the switch value. We can do this since the switch statement
		// is exhaustive.
		emit_pop(p)
		emit_pop(p)
		consume(p, .FAT_ARROW, "Expect '=>' after case.")
		statement(p)
		append(&case_jumps_to_end, emit_jump(p, .OP_JUMP))

		// Patch the case-to-case jump.
		patch_jump(p, case_jump)
		emit_pop(p)
	}

	if !has_else_clause {
		error(p, "Switch statement must have an 'else' clause.")
		return
	}

	// Patch all the case-to-end jumps.
	for jump in case_jumps_to_end {
		patch_jump(p, jump)
	}
}

/* Parse a while statment. */
@(private = "file")
while_statement :: proc(p: ^Parser) {
	emit_noop(p)

	loop_start := len(current_chunk(p).code)

	begin_scope(p)
	begin_loop(p, loop_start)
	expression(p)
	consume(p, .LSQUIRLY, "Expect '{' after while loop condition.")

	exit_jump := emit_jump(p, .OP_JUMP_IF_FALSE)
	emit_pop(p)

	block(p)

	emit_loop(p, loop_start)

	patch_jump(p, exit_jump)
	emit_pop(p)

	end_loop(p)
	end_scope(p)
}

@(private = "file")
for_statement :: proc(p: ^Parser) {
	emit_noop(p)

	begin_scope(p)
	if match(p, .SEMI) {
		// No initializer.
	} else if match(p, .LET) {
		let_declaration(p)
	} else {
		expression_statement(p)
	}

	loop_start := len(current_chunk(p).code)
	exit_jump := -1

	if !match(p, .SEMI) {
		expression(p)
		consume_semi(p, "loop condition")

		// Jump out of the loop if the condition is false.
		exit_jump = emit_jump(p, .OP_JUMP_IF_FALSE)
		emit_pop(p) // Condition.
	}

	// Jump over the increment, run the body, and jump back to the
	// increment, then go to the next iteration.
	if !match(p, .LSQUIRLY) {
		body_jump := emit_jump(p, .OP_JUMP)
		increment_start := len(current_chunk(p).code)

		expression(p)
		emit_pop(p)
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
		emit_pop(p) // Condition.
	}

	end_loop(p)
	end_scope(p)
}

/* 
Discard all tokens until reaching a synchronization point, like a
statement terminator or any tokens that begin a statement. 
*/
@(private = "file")
synchronize :: proc(p: ^Parser) {
	p.panic_mode = false

	for p.current.type != .EOF {
		if p.previous.type == .SEMI {
			return
		}

		#partial switch p.current.type {
		case .BREAK, .CONTINUE, .FUNC, .FOR, .IF, .LET, .PRINT, .SWITCH, .RETURN, .WHILE:
			return
		case: // Do nothing.
		}

		advance(p)
	}
}

/* Parse a declaration. */
@(private = "file")
declaration :: proc(p: ^Parser) {
	switch {
	case match(p, .LET) || match(p, .FINAL):
		let_declaration(p)
	case match(p, .FUNC):
		func_declaration(p)
	case:
		statement(p)
	}

	if p.panic_mode {
		synchronize(p)
	}
}

/* Parse a statement. */
@(private = "file")
statement :: proc(p: ^Parser) {
	switch {
	case match(p, .BREAK):
		break_statement(p)
	case match(p, .CONTINUE):
		continue_statement(p)
	case match(p, .FOR):
		for_statement(p)
	case match(p, .LSQUIRLY):
		begin_scope(p)
		block(p)
		end_scope(p)
	case match(p, .IF):
		if_statement(p)
	case match(p, .PRINT):
		print_statement(p)
	case match(p, .RETURN):
		return_statement(p)
	case match(p, .SWITCH):
		switch_statement(p)
	case match(p, .WHILE):
		while_statement(p)
	case:
		expression_statement(p)
	}
}

restore_gc :: proc(p: ^Parser) {
	p.gc.mark_roots_arg = p.prev_mark_roots
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
compile :: proc(gc: ^GC, tokens: []Token, globals: ^Table) -> (fn: ^ObjFunction, success: bool) {
	c: Compiler
	p := Parser {
		tokens           = tokens,
		curr_idx         = 0,
		had_error        = false,
		panic_mode       = false,
		current_compiler = nil,
		globals          = globals,
		pipeline_state   = .NONE,
		gc               = gc,
		prev_mark_roots  = gc.mark_roots_arg,
	}
	gc.mark_roots_arg = &p
	init_compiler(&c, &p, .SCRIPT)

	advance(&p)

	for !match(&p, .EOF) {
		declaration(&p)
	}

	fn = end_compiler(&p)
	restore_gc(&p)

	return fn, !p.had_error
}
