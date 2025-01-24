package zen

import "core:fmt"
import "core:os"
import "core:path/filepath"
import "core:slice"
import "core:strconv"
import "core:strings"

/* Maximum limit for a eight bit unsigned integer. */
U8_MAX :: 255

/* Maximum limit for a sixteen bit unsigned integer. */
U16_MAX :: 65535

/* Number of eight bit unsigned integers in existence. */
U8_COUNT :: 256

/* Number of sixteen bit unsigned integers in existence. */
U16_COUNT :: 65536

/*
The language parser. Consists of pretty much every value necessary during the
compilation process.
*/
Parser :: struct {
	tokens:           []Token,
	curr_idx:         int,
	current:          Token,
	previous:         Token,
	had_error:        bool,
	panic_mode:       bool,
	compiling_chunk:  ^Chunk,
	current_compiler: ^Compiler,
	current_class:    ^ClassCompiler,
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
	name:             Token,
	depth:            int,
	final:            Variability,

	/* Whether the local variable is captured in a closure. */
	is_captured:      bool,

	/* Whether the variable is a loop variable of a `for` loop. */
	is_loop_variable: bool,
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
	INITIALIZER,
	METHOD,
	SCRIPT,
}

/*
The type of a module imported with `use`, may be a built-in module or one defined
by the user i.e. an imported file. */
ModuleType :: enum {
	BUILTIN,
	USER,
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

/*
A struct representing the current innermost class being compiled.
*/
ClassCompiler :: struct {
	enclosing:      ^ClassCompiler,
	has_superclass: bool,
}

/* Returns the chunk that is currently being compiled. This function is just
for convenience. */
current_chunk :: proc(p: ^Parser) -> ^Chunk {
	return &p.current_compiler.function.chunk
}

/* Report an error at the provided token with a message. */
@(private = "file")
error_at :: proc(p: ^Parser, token: ^Token, message: string) {
	if p.panic_mode do return
	p.panic_mode = true
	color_red(os.stderr, "compile error ")

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
Advance to the next token.
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
report an error at that token.
*/
@(private = "file")
consume :: proc(p: ^Parser, type: TokenType, message: string) {
	if p.current.type == type {
		advance(p)
		return
	}

	error_at_current(p, message)
}

/* Wrapper over `consume()` to consume a semicolon with a certain message. */
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
Emit instructions to make the current function return nil; or if the current
function is an initializer make it return its receiver.
*/
@(private = "file")
emit_return :: proc(p: ^Parser) {
	if p.current_compiler.type == .INITIALIZER {
		emit_opcode(p, .OP_GET_LOCAL)
		emit_byte(p, 0) /* Since the receiver is always stored in slot zero. */
	} else {
		emit_opcode(p, .OP_NIL)
	}

	emit_opcode(p, .OP_RETURN)
}

/* 
Add a constant to the current chunk's constant pool. Reports an error if
there are too many constants.
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

	/* The first slot is the function itself. */
	local := &p.current_compiler.locals[p.current_compiler.local_count]
	c.local_count += 1
	local.depth = 0
	local.is_captured = false /* You cannot capture the slot zero value. */

	/* If the function is a method, the first slot is repurposed to store that
	 * method's receiver instead. */
	if type != .FUNCTION {
		local.name.lexeme = "this"
	} else {
		local.name.lexeme = ""
	}
}

/* Emit a return instruction and decode the RLE-encoded lines. */
@(private = "file")
end_compiler :: proc(p: ^Parser) -> ^ObjFunction {
	fn := p.current_compiler.function
	if !fn.has_returned {
		emit_return(p)
	}

	if config.dump_disassembly {
		if !p.had_error {
			disassemble(current_chunk(p), fn.name != nil ? fn.name.chars : "<script>")
		}
	}

	free_loops(p)
	p.current_compiler = p.current_compiler.enclosing

	return fn
}

/* Begin a new scope when compiling. */
@(private = "file")
begin_scope :: proc(p: ^Parser) {
	p.current_compiler.scope_depth += 1
}

/* 
End the current scope when compiling.
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

/* Subscript a list to get a value out of it. */
@(private = "file")
subscript :: proc(p: ^Parser, can_assign: bool) {
	expression(p)
	consume(p, .RSQUARE, "Expect ']' after index.")

	if match(p, .EQUAL) {
		// Parse right hand side of equals
		expression(p)

		emit_opcode(p, .OP_SUBSCRIPT_SET)
	} else {
		emit_opcode(p, .OP_SUBSCRIPT)
	}
}

/* Access or set a instance's property, or access a value from a module. */
@(private = "file")
dot :: proc(p: ^Parser, can_assign: bool) {
	receiver := p.tokens[p.curr_idx - 3]
	possible_local := resolve_local(p, p.current_compiler, &receiver)
	possible_upvalue, upvalue_final := resolve_upvalue(p, p.current_compiler, &receiver)
	set_op: OpCode

	if possible_local != -1 {
		set_op = .OP_SET_LOCAL
	} else if possible_upvalue != -1 {
		set_op = .OP_SET_UPVALUE
	} else {
		set_op = .OP_SET_GLOBAL
	}

	consume(p, .IDENT, "Expect property name after '.'.")
	name := identifier_constant(p, &p.previous)

	if can_assign && match(p, .EQUAL) {
		if set_op == .OP_SET_LOCAL {
			for i := p.current_compiler.local_count - 1; i >= 0; i -= 1 {
				local := &p.current_compiler.locals[i]
				if identifiers_equal(&receiver, &local.name) && local.final == .FINAL {
					error(p, "Can only set a final variable once.")
				}
			}
		}

		if set_op == .OP_SET_GLOBAL {
			global_o_str := copy_string(p.gc, receiver.lexeme)
			value: Value;ok: bool

			if value, ok := table_get(p.current_compiler.globals, global_o_str); ok {
				if values_equal(value, bool_val(true)) {
					error(p, "Can only set a final variable once.")
				}
			} else {
				table_set(p.current_compiler.globals, global_o_str, bool_val(false))
			}
		}

		if set_op == .OP_SET_UPVALUE {
			if upvalue_final == .FINAL {
				error(p, "Can only set a final variable once.")
			}
		}

		expression(p)
		emit_opcode(p, .OP_SET_PROPERTY)
		emit_byte(p, name)
	} else if match(p, .LPAREN) {
		/* Check if the receiver is a module. */
		arg_count: u8 = 0

		if p.pipeline_state == .ACTIVE {
			emit_opcode(p, .OP_GET_IT)
			arg_count += 1
		}

		/* We have a method call here, so we compile it like a function call. */
		arg_count += argument_list(p)
		emit_opcode(p, .OP_INVOKE)
		emit_byte(p, name)
		emit_byte(p, arg_count)
	} else {
		emit_opcode(p, .OP_GET_PROPERTY)
		emit_byte(p, name)
	}
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

/* Parse a list expression. */
@(private = "file")
list :: proc(p: ^Parser, can_assign: bool) {
	item_count: u8 = 0

	if !check(p, .RSQUARE) {
		// Read args as long as we see a comma next.
		for {
			expression(p)
			/* Maximum arg count that can be stuffed in one byte is 255, and 
			 * the list itself is also a constant so we'll then have 256
			 * constants in one chunk, which is one over the limit. */
			if item_count == 254 {
				error(p, "Can't have more than 254 list items in a literal.")
			}
			item_count += 1

			if !match(p, .COMMA) {break}
		}
	}

	consume(p, .RSQUARE, "Expect ')' after arguments.")

	emit_opcode(p, .OP_LIST)
	emit_byte(p, item_count)
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

Input:
- a: String to concatenate to
- b: The byte to concatenate

Output:
- The concatenated string
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

This function allocates a string, but doesn't take ownership of the input; therefore
the input will still need to be freed if necessary. 

In this compiler, it is used to create an escape-sequenced string out of a slice 
of the program input itself, which should **NOT** be freed until the program ends; 
therefore it is not necessary for it to take ownership.

So far, only the newline and tab sequences are supported.
*/
@(private = "file")
add_escape_sequences :: proc(str: string) -> string {
	sequences := make(map[byte]byte)
	sequences['n'] = '\n'
	sequences['t'] = '\t'
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
assigning to variables, and for reading constants declared with `val`.
This function will error if there is an attempt to assign to a constant.
*/
@(private = "file")
named_variable :: proc(p: ^Parser, name: Token, can_assign: bool) {
	name := name
	get_op, set_op: u8
	arg: u8
	possible_local := resolve_local(p, p.current_compiler, &name)
	possible_upvalue, upvalue_final := resolve_upvalue(p, p.current_compiler, &name)

	if possible_local != -1 {
		arg = u8(possible_local)
		get_op = byte(OpCode.OP_GET_LOCAL)
		set_op = byte(OpCode.OP_SET_LOCAL)
	} else if possible_upvalue != -1 {
		arg = u8(possible_upvalue)
		get_op = byte(OpCode.OP_GET_UPVALUE)
		set_op = byte(OpCode.OP_SET_UPVALUE)
	} else {
		/* The assumption at this point is that the variable is global, since
         * it wasn't found as an upvalue or local variable. In the original
         * clox by Bob Nystrom, there was no way to resolve globals at compile
         * time, however zen allows that so we can check if the variable exists
         * right at compile time. */
		global_o_str := copy_string(p.gc, name.lexeme)
		if _, ok := table_get(p.current_compiler.globals, global_o_str); !ok {
			error(p, fmt.tprintf("Undefined variable '%s'.", name.lexeme))
		}

		arg = identifier_constant(p, &name)
		get_op = byte(OpCode.OP_GET_GLOBAL)
		set_op = byte(OpCode.OP_SET_GLOBAL)
	}

	if can_assign && match(p, .EQUAL) {
		if set_op == byte(OpCode.OP_SET_LOCAL) {
			if p.current_compiler.locals[arg].final == .FINAL {
				error(p, "Can only set a final variable once.")
			}
		}

		if set_op == byte(OpCode.OP_SET_GLOBAL) {
			global_o_str := copy_string(p.gc, name.lexeme)
			value: Value;ok: bool

			/* PERF: This hash table lookup is already done when figuring out if
             * the variable exists earlier in this function, so this is redundant */
			if value, ok := table_get(p.current_compiler.globals, global_o_str); ok {
				if values_equal(value, bool_val(true)) {
					error(p, "Can only set a final variable once.")
				}
			} else {
				table_set(p.current_compiler.globals, global_o_str, bool_val(false))
			}
		}

		if set_op == byte(OpCode.OP_SET_UPVALUE) {
			if upvalue_final == .FINAL {
				error(p, "Can only set a final variable once.")
			}
		}

		expression(p)
		emit_bytes(p, set_op, arg)
	} else {
		/* Lua-like feature to allow function calls without parens if the
         * function has a singular string. */
		/* For a moment I thought of allowing lists here as well, but then
         * remembered that that would break subscripting. */
		if match(p, .STRING) {
			/* Grab the (supposed) function */
			emit_bytes(p, get_op, arg)

			arg_count: u8 = 1

			/* Grab the string. */
			zstring(p, can_assign)

			/* This might be removed. */
			if p.pipeline_state == .ACTIVE {
				emit_opcode(p, .OP_GET_IT)
				arg_count += 1
			}

			emit_bytes(p, byte(OpCode.OP_CALL), arg_count)
		} else {
			emit_bytes(p, get_op, arg)
		}
	}
}

/* Parses a variable expression. Also used to parse assignments. */
@(private = "file")
variable :: proc(p: ^Parser, can_assign: bool) {
	named_variable(p, p.previous, can_assign)
}

/* Create a synthetic token i.e. a token that doesn't actually exist in the
 * source code. Used for `super` and `this`, to create a variable out of them. */
@(private = "file")
synthetic_token :: proc(text: string) -> Token {
	return Token{lexeme = text}
}

/* Parse the `super` keyword. */
@(private = "file")
super_ :: proc(p: ^Parser, can_assign: bool) {
	if p.current_class == nil {
		error(p, "Can't use 'super' outside a class.")
	} else if !p.current_class.has_superclass {
		error(p, "Can't use 'super' in a class with no superclass.")
	}

	consume(p, .DOT, "Expect '.' after 'super'.")
	consume(p, .IDENT, "Expect superclass method name.")
	name := identifier_constant(p, &p.previous)

	/* Place both the current receiver and the superclass on the stack. */
	named_variable(p, synthetic_token("this"), can_assign = false)

	/* Check if the method is immediately invoked or not; since we can apply
	 * an optimization involving no use of bound methods if it is. */
	if match(p, .LPAREN) {
		arg_count := argument_list(p)
		named_variable(p, synthetic_token("super"), can_assign = false)
		emit_opcode(p, .OP_SUPER_INVOKE)
		emit_byte(p, name)
		emit_byte(p, arg_count)
	} else {
		named_variable(p, synthetic_token("super"), can_assign = false)
		emit_opcode(p, .OP_GET_SUPER)
		emit_byte(p, name)
	}
}

/* Parse the `this` keyword. */
@(private = "file")
this_ :: proc(p: ^Parser, can_assign: bool) {
	if p.current_class == nil {
		error(p, "Cannot use 'this' outside a class.")
		return
	}

	/* `this` is treated as a lexically scoped local variable whose value is
	 * somehow magically initialized. Also, can_assign is set to false because
	 * you obviously can't assign to `this`. */
	variable(p, can_assign = false)
}

/* Parse the `it` keyword, used in pipelines. */
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
	TokenType.LPAREN        = ParseRule{grouping, call, .CALL},
	TokenType.RPAREN        = ParseRule{nil, nil, .NONE},
	TokenType.LSQUIRLY      = ParseRule{nil, nil, .NONE},
	TokenType.RSQUIRLY      = ParseRule{nil, nil, .NONE},
	TokenType.LSQUARE       = ParseRule{list, subscript, .CALL},
	TokenType.RSQUARE       = ParseRule{nil, nil, .NONE},
	TokenType.COMMA         = ParseRule{nil, nil, .NONE},
	TokenType.DOT           = ParseRule{nil, dot, .CALL},
	TokenType.MINUS         = ParseRule{unary, binary, .TERM},
	TokenType.PLUS          = ParseRule{nil, binary, .TERM},
	TokenType.SEMI          = ParseRule{nil, nil, .NONE},
	TokenType.SLASH         = ParseRule{nil, binary, .FACTOR},
	TokenType.STAR          = ParseRule{nil, binary, .FACTOR},
	TokenType.BANG_EQUAL    = ParseRule{nil, binary, .EQUALITY},
	TokenType.BAR_GREATER   = ParseRule{nil, nil, .NONE},
	TokenType.EQUAL         = ParseRule{nil, nil, .NONE},
	TokenType.EQUAL_EQUAL   = ParseRule{nil, binary, .EQUALITY},
	TokenType.GREATER       = ParseRule{nil, binary, .COMPARISON},
	TokenType.GREATER_EQUAL = ParseRule{nil, binary, .COMPARISON},
	TokenType.LESS          = ParseRule{nil, binary, .COMPARISON},
	TokenType.LESS_EQUAL    = ParseRule{nil, binary, .COMPARISON},
	TokenType.IDENT         = ParseRule{variable, nil, .NONE},
	TokenType.STRING        = ParseRule{zstring, nil, .NONE},
	TokenType.NUMBER        = ParseRule{number, nil, .NONE},
	TokenType.AND           = ParseRule{nil, and_, .AND},
	TokenType.BREAK         = ParseRule{nil, nil, .NONE},
	TokenType.ELSE          = ParseRule{nil, nil, .NONE},
	TokenType.FALSE         = ParseRule{literal, nil, .NONE},
	TokenType.VAL           = ParseRule{nil, nil, .NONE},
	TokenType.FOR           = ParseRule{nil, nil, .NONE},
	TokenType.FUNC          = ParseRule{lambda, nil, .NONE},
	TokenType.IF            = ParseRule{nil, nil, .NONE},
	TokenType.USE           = ParseRule{nil, nil, .NONE},
	TokenType.IN            = ParseRule{nil, nil, .NONE},
	TokenType.IT            = ParseRule{it_, nil, .NONE},
	TokenType.VAR           = ParseRule{nil, nil, .NONE},
	TokenType.NIL           = ParseRule{literal, nil, .NONE},
	TokenType.NOT           = ParseRule{unary, nil, .NONE},
	TokenType.OR            = ParseRule{nil, or_, .OR},
	TokenType.PRINT         = ParseRule{nil, nil, .NONE},
	TokenType.PUB           = ParseRule{nil, nil, .NONE},
	TokenType.RETURN        = ParseRule{nil, nil, .NONE},
	TokenType.SUPER         = ParseRule{super_, nil, .NONE},
	TokenType.THIS          = ParseRule{this_, nil, .NONE},
	TokenType.TRUE          = ParseRule{literal, nil, .NONE},
	TokenType.EOF           = ParseRule{nil, nil, .NONE},
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

/* Store the lexeme of a token in the constant table. */
@(private = "file")
identifier_constant :: proc(p: ^Parser, name: ^Token) -> u8 {
	return make_constant(p, obj_val(copy_string(p.gc, name.lexeme)))
}

/* Similar to identifier_constant, but you can pass in just a string.
 * Used to implement the modules, as module names are STRING tokens rather than
 * IDENT tokens. */
@(private = "file")
string_constant :: proc(p: ^Parser, text: string) -> u8 {
	return make_constant(p, obj_val(copy_string(p.gc, text)))
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

/* Add an upvalue to the function or return it if it already exists. */
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

/*
Find an upvalue in the function's local scope and scopes above it, and return
the index to its name in the constant table. Also return whether the upvalue was
initially declared with `val` or `var`.
*/
@(private = "file")
resolve_upvalue :: proc(p: ^Parser, compiler: ^Compiler, name: ^Token) -> (int, Variability) {
	/* Base case 1: We reached the end of the compiler stack, so the name is probably in the
	global scope. The .VAR is just a dummy value. */
	if compiler.enclosing == nil {
		return -1, .VAR
	}

	/* Look for the name in the enclosing function's local scope. 
	Base case 2: If we find the name there, return it. */
	local := resolve_local(p, compiler.enclosing, name)
	if local != -1 {
		// Mark the local as captured and see if its a `var` or `val`.
		compiler.enclosing.locals[local].is_captured = true
		final := compiler.enclosing.locals[local].final
		/* is_local is true since we're capturing a local variable from the
		immediately enclosing function. */
		return add_upvalue(p, compiler, u8(local), is_local = true), final
	}

	/* Recursively look for an upvalue in the enclosing function. */
	upvalue, final := resolve_upvalue(p, compiler.enclosing, name)
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
		return add_upvalue(p, compiler, u8(upvalue), is_local = false), final
	}

	// Nope, didn't find anything.
	// The .VAR is just a dummy value.
	return -1, .VAR
}

/* 
Add a local name binding.
Errors if there are too many local variables in the scope already.
*/
@(private = "file")
add_local :: proc(p: ^Parser, name: Token, final: Variability, is_loop_variable: bool = false) {
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
	local.is_loop_variable = is_loop_variable
}

/* 
Declare a name binding.
Errors if the variable of that name already exists in the scope.
*/
@(private = "file")
declare_variable :: proc(p: ^Parser, final: Variability, is_loop_variable: bool = false) {
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

	add_local(p, name^, final, is_loop_variable)
}

/* Parse a variable or `final` declaration. */
@(private = "file")
parse_variable :: proc(
	p: ^Parser,
	error_message: string,
	final: Variability,
	is_loop_variable: bool = false,
) -> u8 {
	consume(p, .IDENT, error_message)

	declare_variable(p, final, is_loop_variable)
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

/* 
Parse function arguments.
TODO: Allow for more than 255 arguments.
*/
@(private = "file")
argument_list :: proc(p: ^Parser) -> u8 {
	arg_count: u8 = 0

	if !check(p, .RPAREN) {
		// Read args as long as we see a comma next.
		for {
			expression(p)
			// Arg count can't be more than 255 since it's stuffed in one byte.
			if arg_count == 255 {
				error(p, "Can't have more than 255 arguments.")
			}
			arg_count += 1

			if !match(p, .COMMA) {break}
		}
	}

	consume(p, .RPAREN, "Expect ')' after arguments.")
	return arg_count
}

/* Parse any expression. Also handles pipelines. */
@(private = "file")
expression :: proc(p: ^Parser) {
	for i := 0;; i += 1 {
		if i >= U8_MAX {
			error(p, "Too many pipes.")
			return
		}

		parse_precedence(p, .ASSIGNMENT)

		if !match(p, .BAR_GREATER) {
			p.pipeline_state = .NONE
			break
		}

		if p.pipeline_state == .NONE {
			p.pipeline_state = .ACTIVE
		}

		emit_opcode(p, .OP_SET_IT)
	}
}

/* Parse a block. */
@(private = "file")
block :: proc(p: ^Parser) {
	for !check(p, .RSQUIRLY) && !check(p, .EOF) {
		declaration(p)
	}

	consume(p, .RSQUIRLY, "Expect '}' after block.")
}

/* Parse a function, either a named or anonymous, including arrow functions. */
@(private = "file")
function :: proc(p: ^Parser, type: FunctionType, public: bool = false) {
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
				error_at_current(p, "Can't have more than 255 parameters.")
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

	/* Emit a final byte indicating whether the function should be available to
     * other files importing the file it is in. */
	emit_byte(p, 1 if public else 0)

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
Compile a method in a class.
After an empty class is created by the OP_CLASS instruction, for each method
in the class an OP_METHOD instruction is emitted that adds the method to that
class, which at that moment is on top of the stack. The OP_METHOD instruction 
has the index to the name of the method as its operand.
*/
@(private = "file")
method :: proc(p: ^Parser) {
	consume(p, .IDENT, "Expect method name.")
	constant := identifier_constant(p, &p.previous)

	type: FunctionType = .METHOD
	/* Check if the method is an initializer. */
	if len(p.previous.lexeme) == 4 && strings.compare(p.previous.lexeme, "init") == 0 {
		type = .INITIALIZER
	}
	function(p, type)

	emit_opcode(p, .OP_METHOD)
	emit_byte(p, constant)
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
Parse a class declaration.
*/
@(private = "file")
class_declaration :: proc(p: ^Parser, public: bool = false) {
	consume(p, .IDENT, "Expect class name.")

	/* The class name is captured to push it back on the stack later on while
	   compiling methods. */
	class_name := p.previous

	name_constant := identifier_constant(p, &p.previous)

	declare_variable(p, .VAR) /* Classes are reassignable, subject to change. */

	global_o_str := copy_string(p.gc, p.previous.lexeme)
	value: Value;ok: bool

	/* Add the value onto the globals table. We don't check if it already exists
     * because classes are, as of now, reassignable. */
	table_set(p.current_compiler.globals, global_o_str, bool_val(false))

	emit_opcode(p, .OP_CLASS)
	emit_byte(p, 1 if public else 0)
	emit_byte(p, name_constant)
	define_variable(p, name_constant)

	/* Push the current class to the linked list of classes. */
	class_compiler: ClassCompiler
	class_compiler.has_superclass = false
	class_compiler.enclosing = p.current_class
	p.current_class = &class_compiler

	/* Check if this class inherits from another. */
	if match(p, .LESS) {
		consume(p, .IDENT, "Expect superclass name.")

		/* Push the superclass on the stack. */
		variable(p, can_assign = false)

		if identifiers_equal(&class_name, &p.previous) {
			error(p, "A class can't inherit from itself.")
		}

		/* Create a local variable as the reference to the superclass. */
		begin_scope(p)
		add_local(p, synthetic_token("super"), .VAR)
		define_variable(p, 0)

		/* Push the inheriting class on the stack. */
		named_variable(p, class_name, can_assign = false)

		emit_opcode(p, .OP_INHERIT)
		class_compiler.has_superclass = true
	}

	/* Note that we don't need to do anything extra to allow method overrides.
	 * The OP_INHERIT instruction which copies all methods of the superclass
	 * into the subclass runs before any methods of the subclass itself are
	 * compiled, so if a method of the same name appears in the subclass it
	 * will override that inherited entry. */

	/* This call to named_variable() pushes the class back on the stack. */
	named_variable(p, class_name, can_assign = false)

	consume(p, .LSQUIRLY, "Expect '{' before class body.")

	/* Compile methods until the final curly brace. The EOF check ensures that
	   the compiler doesn't get stuck in an infinite loop if the closing loop
	   is forgotten.

	   Note that currently, classes can only have method declarations; fields
	   aren't explicitly declared but rather freely added. This may be changed
	   in the future.
	*/
	for !check(p, .RSQUIRLY) && !check(p, .EOF) {
		method(p)

		/* Consume the semi after the method that gets added by automatic
		   semicolon insertion. */
		match(p, .SEMI)
	}

	consume(p, .RSQUIRLY, "Expect '}' after class body.")
	emit_pop(p) /* Pop the class object off. */

	if class_compiler.has_superclass {
		end_scope(p)
	}

	/* Set the current class to its enclosing one. */
	p.current_class = p.current_class.enclosing
}

@(private = "file")
module_declaration :: proc(p: ^Parser) {
	mod_type: ModuleType
	consume(p, .STRING, "Expect module path.")

	path := strings.trim(p.previous.lexeme[1:len(p.previous.lexeme) - 1], " ")
	abs_path := filepath.join([]string{config.__dirname, path})
	defer delete(abs_path)

	// look for the path in the stdlib, if not present look for a file at the path
	found := slice.contains(p.gc.std_modules[:], path)
	if found {
		mod_type = .BUILTIN
	} else {
		found := os.exists(abs_path)
		if !found {
			error(p, fmt.tprintf("Module '%s' not found.", abs_path))
		}

		mod_type = .USER
	}

	mod_name: string
	switch mod_type {
	case .BUILTIN:
		{
			mod_name = path
		}
	case .USER:
		{
			mod_name = filepath.short_stem(path)
		}
	}

	name_constant := string_constant(p, mod_name)

	/* Standard library modules and user modules are implemented differently, so
       we need to emit the correct opcode. */
	switch mod_type {
	case .BUILTIN:
		{
			emit_opcode(p, .OP_MODULE_BUILTIN)
			emit_byte(p, name_constant)
		}
	case .USER:
		{
			/* Provide the name of the module and the path as bytecode args. */
			emit_opcode(p, .OP_MODULE_USER)
			emit_byte(p, name_constant)
			emit_byte(p, string_constant(p, abs_path))
		}
	}

	define_variable(p, name_constant)

	/* Set the module as a global variable for variable existence checks.
     * The `true` is to ensure that the variable cannot be reassigned to. */
	table_set(p.current_compiler.globals, copy_string(p.gc, mod_name), bool_val(true))
}

/* 
Parse a function declaration. 
Functions are first class, so they are parsed like variables.
*/
@(private = "file")
func_declaration :: proc(p: ^Parser, public: bool = false) {
	global := parse_variable(p, "Expect function name.", .VAR)
	mark_initialized(p)
	function(p, .FUNCTION, public)
	define_variable(p, global)
}

/* Parse a name binding. */
@(private = "file")
var_declaration :: proc(p: ^Parser, is_loop_variable: bool = false) {
	final: Variability = .FINAL if p.previous.type == .VAL else .VAR

	for {
		global := parse_variable(
			p,
			final == .FINAL ? "Expect final variable name." : "Expect variable name.",
			final,
			is_loop_variable,
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

	if config.repl {
		/* If in a repl session, print out the expression's value.
		 * The print instruction also pops off the value at the top of the
		 * stack, so no need to add another pop instruction in this case.
         * This is only done when the program is at the top level i.e. the value
         * of current_compiler is SCRIPT. */
		if p.current_compiler.type == .SCRIPT {
			emit_constant(p, obj_val(copy_string(p.gc, "=> ")))
			emit_opcode(p, .OP_PRINT)

			emit_opcode(p, .OP_PRINT)

			/* Add a newline, since OP_PRINT does not append a newline. */
			emit_constant(p, obj_val(copy_string(p.gc, "\n")))
			emit_opcode(p, .OP_PRINT)
		}
	} else {
		emit_pop(p)
	}
}

/* Parse an if statement. */
@(private = "file")
if_statement :: proc(p: ^Parser, ifnt: bool = false) {
	expression(p)
	consume(p, .LSQUIRLY, "Expect '{' after if condition.")

	when CHAOTIC {
		then_jump := emit_jump(p, .OP_JUMP_IF_TRUE)
	} else {
		then_jump := emit_jump(p, .OP_JUMP_IF_FALSE)
	}

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
		if p.current_compiler.type == .INITIALIZER {
			error(p, "Can't return a value from an initializer.")
		}

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
		if local.is_loop_variable {
			continue
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

	/* If there is no switch variable, the value to switch on is assumed to
       be the boolean value true. This is so that the switch statement can
       be used like else if. */
	if match(p, .LSQUIRLY) {
		emit_opcode(p, .OP_TRUE)
	} else {
		expression(p)
		consume(p, .LSQUIRLY, "Expect '{' after switch condition.")
	}

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

			/* Consume a possible residual semi due to automatic semicolon
			 * insertion */
			match(p, .SEMI)

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

		/* Consume a possible residual semi due to automatic semicolon
		* insertion */
		match(p, .SEMI)

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
while_statement :: proc(p: ^Parser, whilent: bool = false) {
	loop_start := len(current_chunk(p).code)

	begin_scope(p)
	begin_loop(p, loop_start)
	expression(p)
	consume(p, .LSQUIRLY, "Expect '{' after while loop condition.")

	when CHAOTIC {
		exit_jump := emit_jump(p, .OP_JUMP_IF_TRUE)
	} else {
		exit_jump := emit_jump(p, .OP_JUMP_IF_FALSE)
	}

	emit_pop(p)

	block(p)

	emit_loop(p, loop_start)

	patch_jump(p, exit_jump)
	emit_pop(p)

	end_scope(p)
	end_loop(p)
}

/* Parse a for loop. */
@(private = "file")
for_statement :: proc(p: ^Parser) {
	begin_scope(p)
	if match(p, .SEMI) {
		// No initializer.
	} else if match(p, .VAR) {
		var_declaration(p, is_loop_variable = true)
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

	end_scope(p)
	end_loop(p)
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
		case .BREAK,
		     .CONTINUE,
		     .FUNC,
		     .FOR,
		     .IF,
		     .IFNT,
		     .VAR,
		     .VAL,
		     .PRINT,
		     .SWITCH,
		     .RETURN,
		     .WHILE,
		     .WHILENT,
		     .USE:
			return
		case: // Do nothing.
		}

		advance(p)
	}
}

/* Parse a declaration of some value. Includes variables, classes, functions
 * and module imports. */
@(private = "file")
declaration :: proc(p: ^Parser) {
	switch {
	case match(p, .VAR) || match(p, .VAL):
		var_declaration(p)
	case match(p, .CLASS):
		class_declaration(p)
	case match(p, .USE):
		module_declaration(p)
	case match(p, .FUNC):
		func_declaration(p)
	case match(p, .PUB):
		{
			if match(p, .FUNC) {
				func_declaration(p, public = true)
			} else if match(p, .CLASS) {
				class_declaration(p, public = true)
			} else {
				error(p, "Only functions or classes can be set as public.")
			}
		}
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
	when CHAOTIC {
		switch {
		case match(p, .IFNT):
			{
				if_statement(p, ifnt = true)
			}
		case match(p, .WHILENT):
			{
				while_statement(p, whilent = true)
			}
		}
	}

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
	case match(p, .SEMI): // Do nothing. This is equivalent to `pass`.
	case match(p, .ELSE):
		error(p, "You can't have an `else` without an `if`.")
	case:
		expression_statement(p)
	}}

/* Restore the GC to its previous state, i.e. change the roots. */
restore_gc :: proc(p: ^Parser) {
	p.gc.mark_roots_arg = p.prev_mark_roots
}

/* 
Compile the provided slice of tokens into a bytecode chunk. 
The compile function also takes in a pointer to a globals table with each key
being the name of every global declared so far and the value being whether it
is declared with `var` or `val`. The main reason behind this is to keep track of
any final variables declared as well as to check if a variable exists. This is 
useful in the REPL but not so much in a file, since in the REPL the compiler
recompiles every line but that's not necessary in a file.

TODO: Find a better way to store global variables so that this whole
table-passing thing isn't necessary.
*/
compile :: proc(gc: ^GC, tokens: []Token, globals: ^Table) -> (fn: ^ObjFunction, success: bool) {
	/* Add all the native function names to the global table, for variable
     * existence checks. */
	for fn_name in gc.global_native_fns {
		table_set(globals, copy_string(gc, fn_name), bool_val(true))
	}

	c: Compiler
	p := Parser {
		tokens           = tokens,
		curr_idx         = 0,
		had_error        = false,
		panic_mode       = false,
		current_compiler = nil,
		current_class    = nil,
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
