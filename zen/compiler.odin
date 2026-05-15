package zen

import "core:fmt"
import "core:os"
import "core:path/filepath"
import "core:slice"
import "core:strings"

/* Maximum limit for a eight bit unsigned integer. */
U8_MAX :: 255

/* Maximum limit for a sixteen bit unsigned integer. */
U16_MAX :: 65535

/* Number of eight bit unsigned integers in existence. */
U8_COUNT :: 256

/* Number of sixteen bit unsigned integers in existence. */
U16_COUNT :: 65536

ErrorMessage :: Maybe(string)

/* Struct holding state of codegen. */
Codegen :: struct {
	current_compiler: ^Compiler,
	current_class:    ^ClassCompiler,
	current_token:    Token,
	globals:          ^Table, // Hash table storing global variables.
	gc:               ^GC,
	prev_mark_roots:  RootSource,
	pipeline_active:  bool,
	had_error:        bool,
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
A local variable.
*/
Local :: struct {
	name:             Token,
	depth:            int,
	is_final:         bool,

	/* Whether the local variable is captured in a closure. */
	is_captured:      bool,

	/* A loop variable is a variable used for operation of a loop itself.
     * In the case of a C-style for loop it is the iteration variable only,
     * and in case of for-in loops it is the iteration variable, and the
     * hidden iterator and index variables. */
	is_loop_variable: bool,
}

/*
An upvalue. References a local variable in an enclosing function, or an upvalue
from one of any functions that enclose the current function.
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
The type of a module imported with `use`, may be a built-in module or one defined
by the user i.e. an imported file. */
ModuleType :: enum {
	BUILTIN,
	USER,
}

/* Returns the chunk that is currently being compiled. This function is just
for convenience. */
@(private = "file")
current_chunk :: proc(cg: ^Codegen) -> ^Chunk {
	return &cg.current_compiler.function.chunk
}

/* 
Add a constant to the current chunk's constant pool. Reports an error if
there are too many constants.
*/
@(private = "file")
@(require_results)
make_constant :: proc(cg: ^Codegen, value: Value) -> (int, ErrorMessage) {
	constant := add_constant(current_chunk(cg), cg.gc, value)
	if constant > U16_MAX {
		return 0, "Too many constants in one chunk."
	}

	return constant, nil
}

/* Store the lexeme of a token in the constant table. */
@(private = "file")
@(require_results)
identifier_constant :: proc(cg: ^Codegen, name: Token) -> (int, ErrorMessage) {
	return make_constant(cg, obj_val(copy_string(cg.gc, name.lexeme)))
}

/* Similar to identifier_constant, but you can pass in just a string.
 * Used to implement the modules, as module names are STRING tokens rather than
 * IDENT tokens. */
@(private = "file")
@(require_results)
string_constant :: proc(cg: ^Codegen, text: string) -> (int, ErrorMessage) {
	return make_constant(cg, obj_val(copy_string(cg.gc, text)))
}

/* Write bytes to the chunk being compiled. */
@(private = "file")
emit_byte :: proc(cg: ^Codegen, bytes: ..byte) {
	for b in bytes {
		write_chunk(current_chunk(cg), b, cg.current_token.line)
	}
}

/* Write opcodes as bytes to the chunk being compiled. */
@(private = "file")
emit_opcode :: proc(cg: ^Codegen, opcodes: ..OpCode) {
	for oc in opcodes {
		write_chunk(current_chunk(cg), byte(oc), cg.current_token.line)
	}
}

@(private = "file")
emit_instruction :: proc(cg: ^Codegen, oc: OpCode, operands: ..byte) {
	emit_opcode(cg, oc)
	emit_byte(cg, ..operands)
}

@(private = "file")
emit_pop :: proc(cg: ^Codegen) {
	emit_opcode(cg, .OP_POP)
}

/* Write a OP_CONSTANT or OP_CONSTANT_LONG to the current chunk. */
@(private = "file")
@(require_results)
emit_constant :: proc(cg: ^Codegen, value: Value) -> ErrorMessage {
	index := make_constant(cg, value) or_return

	if index <= U8_MAX {
		emit_instruction(cg, .OP_CONSTANT, byte(index))
	} else {
		emit_instruction(cg, .OP_CONSTANT_LONG, byte((index >> 8) & 0xff), byte(index & 0xff))
	}

	return nil
}

/* Directly write a constant index to the chunk without any preceding opcode. */
@(private = "file")
emit_constant_only :: proc(cg: ^Codegen, index: int) {
	if index <= U8_MAX {
		emit_byte(cg, byte(index))
	} else {
		emit_byte(cg, byte((index >> 8) & 0xff), byte(index & 0xff))
	}
}

/* 
Write a certain opcode to the current chunk followed by an appropriately sized
constant index.
*/
@(private = "file")
emit_op_with_constant :: proc(cg: ^Codegen, short_op: OpCode, long_op: OpCode, index: int) {
	if index <= U8_MAX {
		emit_instruction(cg, short_op, byte(index))
	} else {
		emit_instruction(cg, long_op, byte((index >> 8) & 0xff), byte(index & 0xff))
	}
}


/*
Write a jump instruction to the current chunk.
Initially writes only placeholder bytes for the jump offset, which
will be filled in later.
*/
@(private = "file")
emit_jump :: proc(cg: ^Codegen, instruction: OpCode) -> int {
	emit_opcode(cg, instruction)
	emit_byte(cg, 0xff)
	emit_byte(cg, 0xff)
	return len(current_chunk(cg).code) - 2
}

/*
Patch a jump instruction at the provided offset in the current chunk.
Jump instructions are initially written with placeholder offsets, which
are filled in here, after the amount to jump over is known.
*/
@(private = "file")
@(require_results)
patch_jump :: proc(cg: ^Codegen, offset: int) -> ErrorMessage {
	// -2 to adjust for the bytecode for the jump offset itself.
	jump := len(current_chunk(cg).code) - offset - 2

	if jump > U16_MAX {
		return "Too much code to jump over."
	}

	current_chunk(cg).code[offset] = byte((jump >> 8) & 0xff)
	current_chunk(cg).code[offset + 1] = byte(jump & 0xff)
	return nil
}

/* 
Emit instructions to make the current function return nil; or if the current
function is an initializer make it return its receiver.
*/
@(private = "file")
emit_return :: proc(cg: ^Codegen) {
	if cg.current_compiler.type == .INITIALIZER {
		emit_opcode(cg, .OP_GET_LOCAL)
		emit_byte(cg, 0) /* Since the receiver is always stored in slot zero. */
	} else {
		emit_opcode(cg, .OP_NIL)
	}

	emit_opcode(cg, .OP_RETURN)
}

/* Write a loop instruction to the current chunk. */
@(require_results)
emit_loop :: proc(cg: ^Codegen, loop_start: int) -> ErrorMessage {
	emit_opcode(cg, .OP_LOOP)

	offset := len(current_chunk(cg).code) - loop_start + 2
	if offset > U16_MAX {
		return "Loop body too large."
	}

	emit_byte(cg, byte((offset >> 8) & 0xff))
	emit_byte(cg, byte(offset & 0xff))
	return nil
}

/*
Free the dynamic array of break jump offsets. 
*/
@(private = "file")
free_loops :: proc(cg: ^Codegen) {
	for i in 0 ..< cg.current_compiler.loop_count {
		loop := &cg.current_compiler.loops[i]
		delete(loop.breaks)
	}
}

/* Begin a new scope when compiling. */
@(private = "file")
begin_scope :: proc(cg: ^Codegen) {
	cg.current_compiler.scope_depth += 1
}

/* 
End the current scope when compiling.
This also pops off any local variables that were declared in the scope.
*/
@(private = "file")
end_scope :: proc(cg: ^Codegen) {
	curr := cg.current_compiler
	curr.scope_depth -= 1

	for curr.local_count > 0 && curr.locals[curr.local_count - 1].depth > curr.scope_depth {
		/* If the local was captured, close its upvalue instead of popping it
		to allow closures to work. Closing the upvalue requires no operand,
		since the upvalue to close is right on top of the stack at this point. */
		if curr.locals[curr.local_count - 1].is_captured {
			emit_opcode(cg, .OP_CLOSE_UPVALUE)
		} else {
			emit_pop(cg)
		}
		cg.current_compiler.local_count -= 1
	}
}

/* Add a loop to the current compiler's loop stack. */
@(private = "file")
@(require_results)
begin_loop :: proc(cg: ^Codegen, loop_start: int) -> ErrorMessage {
	if cg.current_compiler.loop_count >= U8_COUNT {
		return "Too many nested loops."
	}

	loop := &cg.current_compiler.loops[cg.current_compiler.loop_count]
	cg.current_compiler.loop_count += 1
	loop.start = loop_start
	loop.scope_depth = cg.current_compiler.scope_depth
	loop.breaks = make([dynamic]int)
	return nil
}

/* Remove a loop from the current compiler's loop stack, and patch its break
statements, if any exist. */
@(private = "file")
@(require_results)
end_loop :: proc(cg: ^Codegen) -> ErrorMessage {
	loop := &cg.current_compiler.loops[cg.current_compiler.loop_count - 1]

	assert(cg.current_compiler.loop_count > 0, "number of loops should not be less than zero")
	patch_breaks(cg) or_return
	cg.current_compiler.loop_count -= 1

	/* Remove all the break statements that were in the loop we just ended. */
	clear(&loop.breaks)
	return nil
}

/*
Patch the jump offsets of any break statements present in the current loop.
Called right after a loop is parsed. 
*/
@(private = "file")
@(require_results)
patch_breaks :: proc(cg: ^Codegen) -> ErrorMessage {
	loop := &cg.current_compiler.loops[cg.current_compiler.loop_count - 1]

	for i in loop.breaks {
		patch_jump(cg, i) or_return
	}
	return nil
}

/* Check if two idents are equal. */
@(private = "file")
identifiers_equal :: proc(a: Token, b: Token) -> bool {
	if len(a.lexeme) != len(b.lexeme) {return false}
	if a.lexeme == b.lexeme {
		// HACK: temporary fix for making the internal __iter and __idx variables
		// of the for-in loop inaccessible (which itself is a bit of a hack)
		// must be fixed in the AST version
		if len(a.lexeme) >= 2 && a.lexeme[0:2] == "__" {
			return false
		}
		return true
	}
	return false
}

/* Create a synthetic token i.e. a token that doesn't actually exist in the
 * source code. Used for `super` and `this`, to create a variable out of them. */
@(private = "file")
synthetic_token :: proc(text: string) -> Token {
	return Token{lexeme = text}
}

/* 
Resolve a local name binding from the Compiler struct.
*/
@(private = "file")
resolve_local :: proc(cg: ^Codegen, compiler: ^Compiler, name: Token) -> (int, Maybe(string)) {
	// Look for the name in the local scopes of the current function.
	for i := compiler.local_count - 1; i >= 0; i -= 1 {
		local := &compiler.locals[i]
		if identifiers_equal(name, local.name) {
			if local.depth == -1 {
				return -1, "Cannot read local variable in its own initializer."
			}
			return i, nil
		}
	}

	// Not found in the scopes of the current function.
	return -1, nil
}

/* 
Add a local name binding.
Errors if there are too many local variables in the scope already.
*/
@(private = "file")
@(require_results)
add_local :: proc(
	cg: ^Codegen,
	name: Token,
	is_final: bool,
	is_loop_variable: bool = false,
) -> ErrorMessage {
	if cg.current_compiler.local_count == U8_COUNT {
		return "Too many local variables in function."
	}

	defer cg.current_compiler.local_count += 1
	local := &cg.current_compiler.locals[cg.current_compiler.local_count]
	local.name = name
	local.depth = -1
	local.is_final = is_final
	local.is_captured = false
	local.is_loop_variable = is_loop_variable
	return nil
}

/*
Find an upvalue in the function's local scope and scopes above it, and return
the index to its name in the constant table. Also return whether the upvalue was
initially declared with `val` or `var`.
*/
@(private = "file")
@(require_results)
resolve_upvalue :: proc(
	cg: ^Codegen,
	compiler: ^Compiler,
	name: Token,
) -> (
	index: int,
	is_final: bool,
	error: ErrorMessage,
) {
	/* Base case 1: We reached the end of the compiler stack, so the name is probably in the
	global scope. The .VAR is just a dummy value. */
	if compiler.enclosing == nil {
		return -1, false, nil
	}

	/* Look for the name in the enclosing function's local scope. 
	Base case 2: If we find the name there, return it. */
	local := resolve_local(cg, compiler.enclosing, name) or_return
	if local != -1 {
		// Mark the local as captured and see if its a `var` or `val`.
		compiler.enclosing.locals[local].is_captured = true
		final := compiler.enclosing.locals[local].is_final
		/* is_local is true since we're capturing a local variable from the
		immediately enclosing function. */
		idx, err := add_upvalue(cg, compiler, u8(local), is_local = true)
		return idx, final, err
	}

	/* Recursively look for an upvalue in the enclosing function. */
	upvalue, final := resolve_upvalue(cg, compiler.enclosing, name) or_return
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
		idx, err := add_upvalue(cg, compiler, u8(upvalue), is_local = false)
		return idx, final, err
	}

	// Nope, didn't find anything.
	// The `false` is just a dummy value.
	return -1, false, nil
}

/* Add an upvalue to the function or return it if it already exists. */
@(private = "file")
@(require_results)
add_upvalue :: proc(
	p: ^Codegen,
	compiler: ^Compiler,
	index: u8,
	is_local: bool,
) -> (
	int,
	ErrorMessage,
) {
	upvalue_count := compiler.function.upvalue_count

	// Check if the function already has the upvalue we're about to add.
	for i in 0 ..< upvalue_count {
		upvalue := &compiler.upvalues[i]
		if upvalue.index == int(index) && upvalue.is_local == is_local {
			return i, nil
		}
	}

	if upvalue_count == U8_COUNT {
		return 0, "Too many closure variables in function."
	}

	// Add the upvalue.
	defer compiler.function.upvalue_count += 1
	compiler.upvalues[upvalue_count].is_local = is_local
	compiler.upvalues[upvalue_count].index = int(index)
	return compiler.function.upvalue_count, nil
}

/* 
Declare a name binding.
Errors if the variable of that name already exists in the scope.
*/
@(private = "file")
@(require_results)
declare_variable :: proc(
	cg: ^Codegen,
	name: Token,
	is_final: bool,
	is_loop_variable: bool = false,
) -> ErrorMessage {
	if cg.current_compiler.scope_depth == 0 {return nil}

	for i := cg.current_compiler.local_count - 1; i >= 0; i -= 1 {
		local := &cg.current_compiler.locals[i]
		if local.depth != -1 && local.depth < cg.current_compiler.scope_depth {
			break
		}

		if identifiers_equal(name, local.name) {
			return "A variable with this name in this scope already exists."
		}
	}

	return add_local(cg, name, is_final, is_loop_variable)
}

/* Mark a local name binding as initialized. */
@(private = "file")
mark_initialized :: proc(cg: ^Codegen) {
	if cg.current_compiler.scope_depth == 0 {return}
	cg.current_compiler.locals[cg.current_compiler.local_count - 1].depth =
		cg.current_compiler.scope_depth
}

/* Define a local or global name binding. */
@(private = "file")
define_variable :: proc(cg: ^Codegen, global: int) {
	if cg.current_compiler.scope_depth > 0 {
		mark_initialized(cg)
		return
	}

	emit_op_with_constant(cg, .OP_DEFINE_GLOBAL, .OP_DEFINE_GLOBAL_LONG, global)
}

@(private = "file")
global_exists :: proc(cg: ^Codegen, global_o_str: ^ObjString) -> bool {
	if _, ok := table_get(cg.globals, global_o_str); ok {
		return true
	}
	return false
}

@(private = "file")
global_exists_and_is_final :: proc(cg: ^Codegen, global_o_str: ^ObjString) -> bool {
	if value, ok := table_get(cg.globals, global_o_str); ok {
		if values_equal(value, bool_val(true)) {
			return true
		}
	}
	return false
}

@(private = "file")
binding_exists_and_is_final :: proc(cg: ^Codegen, name: Token) -> bool {
	local, _ := resolve_local(cg, cg.current_compiler, name)
	upvalue, _, _ := resolve_upvalue(cg, cg.current_compiler, name)

	if local != -1 {
		return true
	} else if upvalue != -1 {
		return true
	} else {
		global_o_str := copy_string(cg.gc, name.lexeme)
		return global_exists_and_is_final(cg, global_o_str)
	}
}

@(private = "file")
@(require_results)
emit_named_variable :: proc(cg: ^Codegen, name: Token, can_assign: bool) -> ErrorMessage {
	local := resolve_local(cg, cg.current_compiler, name) or_return
	upvalue, _ := resolve_upvalue(cg, cg.current_compiler, name) or_return

	if local != -1 {
		emit_instruction(cg, .OP_GET_LOCAL, byte(local))
	} else if upvalue != -1 {
		emit_instruction(cg, .OP_GET_UPVALUE, byte(upvalue))
	} else {
		global_o_str := copy_string(cg.gc, name.lexeme)
		if !global_exists(cg, global_o_str) {
			return fmt.tprintf("Undefined variable '%s'.", name.lexeme)
		}

		arg := identifier_constant(cg, name) or_return
		emit_op_with_constant(cg, .OP_GET_GLOBAL, .OP_GET_GLOBAL_LONG, arg)
	}

	return nil
}

@(private = "file")
@(require_results)
emit_named_variable_set :: proc(cg: ^Codegen, name: Token) -> ErrorMessage {
	local := resolve_local(cg, cg.current_compiler, name) or_return
	upvalue, upvalue_is_final := resolve_upvalue(cg, cg.current_compiler, name) or_return

	if local != -1 {
		if cg.current_compiler.locals[local].is_final {
			return "Can only set a final variable once."
		}
		emit_instruction(cg, .OP_SET_LOCAL, byte(local))
	} else if upvalue != -1 {
		if upvalue_is_final {
			return "Can only set a final variable once."
		}
		emit_instruction(cg, .OP_SET_UPVALUE, byte(upvalue))
	} else {
		arg := identifier_constant(cg, name) or_return

		global_o_str := copy_string(cg.gc, name.lexeme)
		if global_exists_and_is_final(cg, global_o_str) {
			return "Can only set a final variable once."
		} else {
			table_set(cg.globals, global_o_str, bool_val(false))
		}

		emit_op_with_constant(
			cg,
			arg <= U8_MAX ? .OP_SET_GLOBAL : .OP_SET_GLOBAL_LONG,
			.OP_SET_GLOBAL_LONG,
			arg,
		)
	}

	return nil
}

/* Compile a variable binding. */
@(require_results)
compile_binding :: proc(
	cg: ^Codegen,
	name: Token,
	is_final: bool,
	is_loop_variable: bool,
) -> (
	global: int,
	err: ErrorMessage,
) {
	declare_variable(cg, name, is_final, is_loop_variable) or_return
	if cg.current_compiler.scope_depth > 0 {return 0, nil}

	global_o_str := copy_string(cg.gc, name.lexeme)
	if value, ok := table_get(cg.globals, global_o_str); ok {
		if values_equal(value, bool_val(true)) {
			return 0,
				is_final ? "Cannot redefine a final variable." : "Cannot redefine a final variable as normal variable."
		} else if is_final {
			return 0, "Cannot redefine a variable as final variable."
		}
	} else {
		table_set(cg.globals, global_o_str, bool_val(is_final))
	}

	return identifier_constant(cg, name)
}

@(require_results)
compile_var_declaration :: proc(
	cg: ^Codegen,
	d: ^VarDecl,
	is_loop_variable: bool = false,
) -> bool {
	bindings := d.bindings
	is_final := d.is_final

	for binding in bindings {
		global := try2(cg, compile_binding(cg, binding.name, is_final, is_loop_variable)) or_return
		if binding.initializer == nil {
			if is_final {
				codegen_error(cg, "Final variables must be initialized.")
				return false
			} else {
				emit_opcode(cg, .OP_NIL)
			}
		} else {
			/* Allow anonymous functions to recurse by referring to the name they've
             been bound to. */
			if _, ok := binding.initializer.(^LambdaExpr); ok {
				mark_initialized(cg)
			}
			compile_expression(cg, binding.initializer) or_return
		}

		define_variable(cg, global)
	}

	return true
}

@(require_results)
compile_function :: proc(
	cg: ^Codegen,
	name: Token,
	params: []Token,
	body: FunctionBody,
	type: FunctionType,
	public: bool,
) -> bool {
	compiler: Compiler
	init_compiler(&compiler, cg, name, type)

	/* Note that this scope doesn't need to be explicitly closed, since the 
    Compiler itself is ended when we reach the end of the function body. */
	begin_scope(cg)

	if len(params) > U8_MAX {
		codegen_error(cg, "Cannot have more than 255 parameters.")
		return false
	}
	cg.current_compiler.function.arity = u8(len(params))

	for param in params {
		cg.current_token = param

		/* Function parameters are mutable. This may change in the future. */
		constant := try2(
			cg,
			compile_binding(cg, param, is_final = false, is_loop_variable = false),
		) or_return
		define_variable(cg, constant)
	}

	switch b in body {
	case ^BlockStmt:
		// I'm not handing off to compile_statement() here, because it will
		// create a new scope for the block which will end up allowing function
		// parameter shadowing. Not a bad thing, but not intended behavior for zen.
		cg.current_token = b.token
		for decl in b.declarations {
			compile_declaration(cg, decl) or_return
		}
	case Expr:
		/* This is the case of an arrow function which implicitly returns an
         expression, so we need to add an OP_RETURN here */
		compile_expression(cg, b) or_return
		emit_opcode(cg, .OP_RETURN)
		cg.current_compiler.function.has_returned = true
	}

	function := end_compiler(cg)

	/* Note that OP_CLOSURE always uses a 2-byte constant index. While it would
    be perfectly reasonable to switch between a 1-byte index and 2-byte index
    using separate OP_CLOSURE and custom OP_CLOSURE_LONG opcodes, that would
    further complicate the already complex closure encoding. */
	emit_opcode(cg, .OP_CLOSURE)
	idx := try2(cg, make_constant(cg, obj_val(function))) or_return
	emit_byte(cg, byte((idx >> 8) & 0xff))
	emit_byte(cg, byte(idx & 0xff))

	/* Emit a final byte indicating whether the function should be available to
     * other files importing the file it is in. */
	emit_byte(cg, 1 if public else 0)

	/* OP_CLOSURE has a variably sized encoding. For each upvalue captured by
	the closure, there are two single-byte operands: a boolean indicating
	whether the upvalue captures a local variable in the immediately enclosing
	function (1) or one of that function's upvalues (0), and the local slot or upvalue
	index to capture. */
	for i in 0 ..< function.upvalue_count {
		emit_byte(cg, 1 if compiler.upvalues[i].is_local else 0)
		emit_byte(cg, u8(compiler.upvalues[i].index))
	}

	return true
}

@(require_results)
compile_func_declaration :: proc(cg: ^Codegen, d: ^FuncDecl, public: bool) -> bool {
	name := d.name
	params := d.params
	body := d.body

	global := try2(
		cg,
		compile_binding(cg, name, is_final = false, is_loop_variable = false),
	) or_return
	mark_initialized(cg)
	compile_function(cg, name, params, body, .FUNCTION, public) or_return
	define_variable(cg, global)
	return true
}

@(require_results)
compile_method :: proc(cg: ^Codegen, m: ^FuncDecl) -> bool {
	name := m.name
	params := m.params
	body := m.body
	constant := try2(cg, identifier_constant(cg, name)) or_return

	type: FunctionType = .METHOD
	/* Check if the method is an initializer. */
	if len(name.lexeme) == 4 && strings.compare(name.lexeme, "init") == 0 {
		type = .INITIALIZER
	}
	compile_function(cg, name, params, body, type, public = false) or_return

	emit_op_with_constant(cg, .OP_METHOD, .OP_METHOD_LONG, constant)
	return true
}

@(require_results)
compile_class_declaration :: proc(cg: ^Codegen, d: ^ClassDecl, public: bool) -> bool {
	class_name := d.name
	methods := d.methods
	superclass := d.superclass

	/* The class name is captured to push it back on the stack later on while
	   compiling methods. */
	name_constant := try2(cg, identifier_constant(cg, class_name)) or_return
	try(
		cg,
		declare_variable(cg, class_name, is_final = false, is_loop_variable = true),
	) or_return /* Classes are reassignable. */

	global_o_str := copy_string(cg.gc, class_name.lexeme)
	/* Add the value onto the globals table. */
	if cg.current_compiler.scope_depth == 0 {
		if global_exists(cg, global_o_str) {
			codegen_error(cg, "Cannot redeclare a class.")
			return false
		}
		table_set(cg.globals, global_o_str, bool_val(false))
	}

	if name_constant <= U8_MAX {
		emit_opcode(cg, .OP_CLASS)
		emit_byte(cg, 1 if public else 0)
		emit_byte(cg, byte(name_constant))
	} else {
		emit_opcode(cg, .OP_CLASS_LONG)
		emit_byte(cg, 1 if public else 0)
		emit_byte(cg, byte((name_constant >> 8) & 0xff))
		emit_byte(cg, byte(name_constant & 0xff))
	}
	define_variable(cg, name_constant)

	/* Push the current class to the linked list of classes. */
	class_compiler: ClassCompiler
	class_compiler.has_superclass = false
	class_compiler.enclosing = cg.current_class
	cg.current_class = &class_compiler

	/* Check if this class inherits from another. */
	if superclass != nil {
		superclass := superclass.?
		if identifiers_equal(class_name, superclass) {
			codegen_error(cg, "A class can't inherit from itself.")
			return false
		}

		/* Push the superclass on the stack. */
		try(cg, emit_named_variable(cg, superclass, can_assign = false)) or_return

		/* Create a local variable as the reference to the superclass. */
		begin_scope(cg)
		try(cg, add_local(cg, synthetic_token("super"), is_final = true)) or_return
		define_variable(cg, 0)

		/* Push the inheriting class on the stack. */
		try(cg, emit_named_variable(cg, class_name, can_assign = false)) or_return

		emit_opcode(cg, .OP_INHERIT)
		class_compiler.has_superclass = true
	}

	/* Note that we don't need to do anything extra to allow method overrides.
	 * The OP_INHERIT instruction which copies all methods of the superclass
	 * into the subclass runs before any methods of the subclass itself are
	 * compiled, so if a method of the same name appears in the subclass it
	 * will override that inherited entry. */

	/* This call to named_variable() pushes the class back on the stack. */
	try(cg, emit_named_variable(cg, class_name, can_assign = false)) or_return

	/* Compile methods until the final curly brace. The EOF check ensures that
	   the compiler doesn't get stuck in an infinite loop if the closing brace
	   is forgotten.

	   Note that currently, classes can only have method declarations; fields
	   aren't explicitly declared but rather freely added. This may be changed
	   in the future.
	*/
	for method in methods {
		cg.current_token = method.token
		compile_method(cg, method) or_return
	}

	emit_pop(cg) /* Pop the class object off. */
	if class_compiler.has_superclass {
		end_scope(cg)
	}

	/* Set the current class to its enclosing one. */
	cg.current_class = cg.current_class.enclosing
	return true
}

@(require_results)
compile_module_declaration :: proc(cg: ^Codegen, d: ^ModuleDecl) -> bool {
	path_str := d.path.lexeme

	mod_type: ModuleType

	path := strings.trim(path_str[1:len(path_str) - 1], " ")
	abs_path, err := filepath.join([]string{config.__dirname, path}, context.allocator)
	if err != nil {
		codegen_error(cg, fmt.tprintf("Error when declaring module: %s", os.error_string(err)))
		return false
	}
	defer delete(abs_path)

	// look for the path in the stdlib, if not present look for a file at the path
	builtin_found := slice.contains(cg.gc.std_modules[:], path)
	if builtin_found {
		mod_type = .BUILTIN
	} else {
		user_found := os.exists(abs_path)
		if !user_found {
			codegen_error(cg, fmt.tprintf("Module '%s' not found.", abs_path))
			return false
		}

		mod_type = .USER
	}

	// TODO: allows `use "module" as "m"` notation

	// the mod_name here is path, but will be turned into just the filename
	// in the VM if it is a user module
	mod_name: string
	switch mod_type {
	case .BUILTIN:
		mod_name = path
	case .USER:
		mod_name = filepath.short_stem(path)
	}
	name_constant := try2(cg, string_constant(cg, mod_name)) or_return

	/* Standard library modules and user modules are implemented differently, so
       we need to emit the correct opcode. */
	switch mod_type {
	case .BUILTIN:
		emit_op_with_constant(cg, .OP_MODULE_BUILTIN, .OP_MODULE_BUILTIN_LONG, name_constant)
	case .USER:
		/* Provide the name of the module and the path as bytecode args. */
		path_constant := try2(cg, string_constant(cg, abs_path)) or_return

		/* Use OP_MODULE_USER_LONG if the path_constant needs to be encoded
        as 2 bytes, even if the name_constant fits in 1. */
		if path_constant <= U8_MAX {
			emit_opcode(cg, .OP_MODULE_USER)
			emit_constant_only(cg, name_constant)
			emit_constant_only(cg, path_constant)
		} else {
			emit_opcode(cg, .OP_MODULE_USER_LONG)

			emit_byte(cg, byte((name_constant >> 8) & 0xff))
			emit_byte(cg, byte(name_constant & 0xff))

			emit_byte(cg, byte((path_constant >> 8) & 0xff))
			emit_byte(cg, byte(path_constant & 0xff))
		}
	}
	define_variable(cg, name_constant)

	/* Set the module as a global variable for variable existence checks.
     * The `true` is to ensure that the variable cannot be reassigned to. */
	table_set(cg.globals, copy_string(cg.gc, mod_name), bool_val(true))
	return true
}

@(require_results)
compile_declaration :: proc(cg: ^Codegen, decl: Decl) -> bool {
	if decl == nil {
		return true
	}

	switch d in decl {
	case ^VarDecl:
		cg.current_token = d.token
		compile_var_declaration(cg, d) or_return
	case ^FuncDecl:
		cg.current_token = d.token
		compile_func_declaration(cg, d, public = false) or_return
	case ^ClassDecl:
		cg.current_token = d.token
		compile_class_declaration(cg, d, public = false) or_return
	case ^ModuleDecl:
		cg.current_token = d.token
		compile_module_declaration(cg, d) or_return
	case ^PubDecl:
		cg.current_token = d.token
		#partial switch inner in d.decl {
		case ^VarDecl:
		case ^FuncDecl:
			compile_func_declaration(cg, inner, public = true) or_return
		case ^ClassDecl:
			compile_class_declaration(cg, inner, public = true) or_return
		case:
			codegen_error(cg, "Only functions or classes can be set as public.")
			return false
		}
	case Stmt:
		compile_statement(cg, d) or_return
	}

	return true
}

@(require_results)
compile_if_statement :: proc(cg: ^Codegen, s: ^IfStmt) -> bool {
	condition := s.condition
	then_branch := s.then_branch
	else_branch := s.else_branch

	compile_expression(cg, condition) or_return

	when CHAOTIC {
		then_jump: int
		if s.is_ifnt {
			then_jump = emit_jump(cg, .OP_JUMP_IF_FALSE)
		} else {
			then_jump = emit_jump(cg, .OP_JUMP_IF_TRUE)
		}
	} else {
		then_jump := emit_jump(cg, .OP_JUMP_IF_FALSE)
	}

	emit_pop(cg)

	begin_scope(cg)
	compile_statement(cg, then_branch) or_return
	end_scope(cg)

	else_jump := emit_jump(cg, .OP_JUMP)

	try(cg, patch_jump(cg, then_jump)) or_return
	emit_pop(cg)

	if else_branch != nil {
		compile_statement(cg, else_branch) or_return
	}
	return try(cg, patch_jump(cg, else_jump))
}

compile_switch_statement :: proc(cg: ^Codegen, s: ^SwitchStmt) -> bool {
	condition := s.condition
	cases := s.cases
	else_branch := s.else_branch

	case_jumps_to_end := make([dynamic]int)
	defer delete(case_jumps_to_end)

	/* If there is no switch variable, the value to switch on is assumed to
       be the boolean value true. This is so that the switch statement can
       be used like else if. */
	if condition == nil {
		emit_opcode(cg, .OP_TRUE)
	} else {
		compile_expression(cg, condition) or_return
	}

	for switch_case in cases {
		emit_opcode(cg, .OP_DUP)
		compile_expression(cg, switch_case.condition) or_return
		emit_opcode(cg, .OP_EQUAL)
		case_jump := emit_jump(cg, .OP_JUMP_IF_FALSE)

		// If a case matches, pop out both the residual boolean comparison
		// and the switch value. We can do this since the switch statement
		// is exhaustive.
		emit_pop(cg)
		emit_pop(cg)
		compile_statement(cg, switch_case.body) or_return

		append(&case_jumps_to_end, emit_jump(cg, .OP_JUMP))

		// Patch the case-to-case jump.
		try(cg, patch_jump(cg, case_jump)) or_return
		emit_pop(cg) /* Case condition. */
	}

	emit_pop(cg) // Pop the switch value.
	compile_statement(cg, else_branch) or_return
	// append(&case_jumps_to_end, emit_jump(cg, .OP_JUMP))

	// Patch all the case-to-end jumps.
	for jump in case_jumps_to_end {
		try(cg, patch_jump(cg, jump)) or_return
	}

	return true
}

@(require_results)
compile_while_statement :: proc(cg: ^Codegen, s: ^WhileStmt) -> bool {
	condition := s.condition
	body := s.body

	loop_start := len(current_chunk(cg).code)

	begin_scope(cg)
	try(cg, begin_loop(cg, loop_start)) or_return
	compile_expression(cg, condition) or_return

	when CHAOTIC {
		exit_jump: int
		if s.is_whilent {
			exit_jump = emit_jump(cg, .OP_JUMP_IF_TRUE)
		} else {
			exit_jump = emit_jump(cg, .OP_JUMP_IF_FALSE)
		}
	} else {
		exit_jump := emit_jump(cg, .OP_JUMP_IF_FALSE)
	}

	emit_pop(cg)
	compile_statement(cg, body) or_return
	try(cg, emit_loop(cg, loop_start)) or_return

	try(cg, patch_jump(cg, exit_jump)) or_return
	emit_pop(cg)

	end_scope(cg)
	return try(cg, end_loop(cg))
}


@(require_results)
compile_for_statement :: proc(cg: ^Codegen, s: ^ForStmt) -> bool {
	initializer := s.initializer
	condition := s.condition
	increment := s.increment
	body := s.body

	loop_variable_slot := -1

	begin_scope(cg)
	switch iz in initializer {
	case ^VarDecl:
		compile_var_declaration(cg, iz, is_loop_variable = true) or_return
		loop_variable_slot = cg.current_compiler.local_count - 1
	case ^ExprStmt:
		compile_statement(cg, iz) or_return
	case ^EmptyStmt:
	// do nothing.
	}

	loop_start := len(current_chunk(cg).code)
	exit_jump := -1

	if condition != nil {
		compile_expression(cg, condition) or_return

		// Jump out of the loop if the condition is false.
		exit_jump = emit_jump(cg, .OP_JUMP_IF_FALSE)
		emit_pop(cg) // Condition.
	}

	// Jump over the increment, run the body, and jump back to the
	// increment, then go to the next iteration.
	if increment != nil {
		body_jump := emit_jump(cg, .OP_JUMP)
		increment_start := len(current_chunk(cg).code)

		compile_expression(cg, increment) or_return
		emit_pop(cg)

		try(cg, emit_loop(cg, loop_start)) or_return
		loop_start = increment_start
		try(cg, patch_jump(cg, body_jump)) or_return
	}

	try(cg, begin_loop(cg, loop_start)) or_return
	begin_scope(cg)
	compile_statement(cg, body) or_return
	end_scope(cg)

	if loop_variable_slot != -1 {
		loop_var := &cg.current_compiler.locals[loop_variable_slot]
		if loop_var.is_captured {
			emit_opcode(cg, .OP_CLOSE_LOOP_VAR)
			emit_byte(cg, byte(loop_variable_slot))
		}
	}
	try(cg, emit_loop(cg, loop_start)) or_return

	if exit_jump != -1 {
		try(cg, patch_jump(cg, exit_jump)) or_return
		emit_pop(cg) // Condition.
	}

	end_scope(cg)
	return try(cg, end_loop(cg))
}

@(require_results)
compile_for_in_statement :: proc(cg: ^Codegen, s: ^ForInStmt) -> bool {
	iterable := s.iterable
	var_name := s.var_name
	body := s.body

	begin_scope(cg)

	emit_opcode(cg, .OP_NIL) // assign it as nil to begin with
	try(cg, declare_variable(cg, var_name, is_final = true, is_loop_variable = true)) or_return
	define_variable(cg, 0)
	loop_variable_slot := cg.current_compiler.local_count - 1

	// Push the iterable on the stack
	// TODO: perhaps figure out a way to find out if the iterable is a valid
	// iterable in advance
	compile_expression(cg, iterable) or_return

	try(
		cg,
		add_local(cg, synthetic_token("__iter"), is_final = true, is_loop_variable = true),
	) or_return
	mark_initialized(cg)
	iter_slot := cg.current_compiler.local_count - 1

	try(cg, emit_constant(cg, number_val(0))) or_return
	try(
		cg,
		add_local(cg, synthetic_token("__idx"), is_final = true, is_loop_variable = true),
	) or_return
	mark_initialized(cg)
	idx_slot := cg.current_compiler.local_count - 1

	loop_start := len(current_chunk(cg).code)
	exit_jump := -1

	// Push idx and iter for ITERATE_NEXT.
	emit_opcode(cg, .OP_GET_LOCAL)
	emit_byte(cg, byte(idx_slot))

	emit_opcode(cg, .OP_GET_LOCAL)
	emit_byte(cg, byte(iter_slot))

	// Pops iter and idx and checks idx < len(iter).
	// If true, pushs (iter[idx], idx+1, true) or false.
	emit_opcode(cg, .OP_ITERATE)

	// Jump out of the loop if ITERATE_NEXT said false.
	exit_jump = emit_jump(cg, .OP_JUMP_IF_FALSE)
	emit_pop(cg) // Condition.

	// updated idx is on top, assign it back to hidden __idx
	emit_opcode(cg, .OP_SET_LOCAL)
	emit_byte(cg, byte(idx_slot))
	emit_pop(cg) // idx.

	emit_opcode(cg, .OP_SET_LOCAL)
	emit_byte(cg, byte(loop_variable_slot))
	emit_pop(cg) // iter[idx].

	try(cg, begin_loop(cg, loop_start)) or_return
	begin_scope(cg)
	compile_statement(cg, body) or_return
	end_scope(cg)

	// close the loop var's upvalue if it was captured
	// ensures each iteration's closure gets its own snapshot
	loop_var := &cg.current_compiler.locals[loop_variable_slot]
	if loop_var.is_captured {
		emit_opcode(cg, .OP_CLOSE_LOOP_VAR)
		emit_byte(cg, byte(loop_variable_slot))
	}
	try(cg, emit_loop(cg, loop_start)) or_return

	try(cg, patch_jump(cg, exit_jump)) or_return
	emit_pop(cg) // Condition.

	end_scope(cg)
	return try(cg, end_loop(cg))
}

@(require_results)
compile_break_statement :: proc(cg: ^Codegen, s: ^BreakStmt) -> bool {
	if cg.current_compiler.loop_count == 0 {
		codegen_error(cg, "Cannot break outside a loop.")
		return false
	}

	loop := &cg.current_compiler.loops[cg.current_compiler.loop_count - 1]

	// Discard correct number of values from the stack.
	for i := cg.current_compiler.local_count - 1; i >= 0; i -= 1 {
		local := &cg.current_compiler.locals[i]
		if local.depth < loop.scope_depth {
			break
		}
		emit_pop(cg)
	}

	append(&loop.breaks, emit_jump(cg, .OP_JUMP))
	return true
}

@(require_results)
compile_continue_statement :: proc(cg: ^Codegen, s: ^ContinueStmt) -> bool {
	if cg.current_compiler.loop_count == 0 {
		codegen_error(cg, "Cannot use 'continue' outside a loop.")
		return false
	}

	loop := &cg.current_compiler.loops[cg.current_compiler.loop_count - 1]

	// Discard correct number of values from the stack.
	for i := cg.current_compiler.local_count - 1; i >= 0; i -= 1 {
		local := &cg.current_compiler.locals[i]
		if local.depth < loop.scope_depth {
			break
		}
		if local.is_loop_variable {
			continue
		}
		emit_pop(cg)
	}

	return try(cg, emit_loop(cg, loop.start))
}

@(require_results)
compile_return_statement :: proc(cg: ^Codegen, s: ^ReturnStmt) -> bool {
	value := s.value

	if cg.current_compiler.type == .SCRIPT {
		codegen_error(cg, "Cannot return from the top level.")
		return false
	}

	if s.value != nil {
		if cg.current_compiler.type == .INITIALIZER {
			codegen_error(cg, "Cannot return a value from an initializer.")
			return false
		}

		compile_expression(cg, value) or_return
		emit_opcode(cg, .OP_RETURN)
	} else {
		emit_return(cg)
	}

	/* Set a flag to true if the function returns in its outermost scope.
	This flag is to check if the function needs an implicit return in the end. */
	if cg.current_compiler.scope_depth == 0 {
		cg.current_compiler.function.has_returned = true
	}
	return true
}

@(require_results)
compile_exit_statement :: proc(cg: ^Codegen, s: ^ExitStmt) -> bool {
	code := s.code

	/* A bare exit will exit the program successfully (with status code 0),
     * and you can add a number after it to make it exit with a certain
     * status code. */
	if code != nil {
		compile_expression(cg, s.code) or_return
	} else {
		try(cg, emit_constant(cg, 0)) or_return
	}

	emit_opcode(cg, .OP_EXIT)
	return true
}

@(require_results)
compile_statement :: proc(cg: ^Codegen, stmt: Stmt) -> bool {
	if stmt == nil {
		return true
	}

	switch s in stmt {
	case ^ExprStmt:
		cg.current_token = s.token
		compile_expression(cg, s.expr) or_return
		if config.repl && cg.current_compiler.type == .SCRIPT {
			try(cg, emit_constant(cg, obj_val(copy_string(cg.gc, "=> ")))) or_return
			emit_opcode(cg, .OP_PRINT)

			// print the expression itself
			emit_opcode(cg, .OP_PRINT)

			/* Add a newline, since OP_PRINT does not append a newline. */
			try(cg, emit_constant(cg, obj_val(copy_string(cg.gc, "\n")))) or_return
			emit_opcode(cg, .OP_PRINT)
		} else {
			emit_pop(cg)
		}
	case ^IfStmt:
		cg.current_token = s.token
		compile_if_statement(cg, s) or_return
	case ^WhileStmt:
		cg.current_token = s.token
		compile_while_statement(cg, s) or_return
	case ^BlockStmt:
		cg.current_token = s.token
		begin_scope(cg)
		for decl in s.declarations {
			compile_declaration(cg, decl) or_return
		}
		end_scope(cg)
	case ^BreakStmt:
		cg.current_token = s.token
		compile_break_statement(cg, s) or_return
	case ^ContinueStmt:
		cg.current_token = s.token
		compile_continue_statement(cg, s) or_return
	case ^EmptyStmt:
		return true
	case ^ExitStmt:
		cg.current_token = s.token
		compile_exit_statement(cg, s) or_return
	case ^ForInStmt:
		cg.current_token = s.token
		compile_for_in_statement(cg, s) or_return
	case ^ForStmt:
		cg.current_token = s.token
		compile_for_statement(cg, s) or_return
	case ^PrintStmt:
		cg.current_token = s.token
		compile_expression(cg, s.expr) or_return
		emit_opcode(cg, .OP_PRINT)
	case ^ReturnStmt:
		cg.current_token = s.token
		compile_return_statement(cg, s) or_return
	case ^SwitchStmt:
		cg.current_token = s.token
		compile_switch_statement(cg, s) or_return
	}

	return true
}

@(require_results)
compile_expression :: proc(cg: ^Codegen, expr: Expr) -> bool {
	if expr == nil {
		return true
	}

	switch e in expr {
	case ^AssignExpr:
		cg.current_token = e.token
		compile_expression(cg, e.value) or_return
		try(cg, emit_named_variable_set(cg, e.name)) or_return
	case ^BinaryExpr:
		cg.current_token = e.token
		compile_expression(cg, e.left) or_return
		compile_expression(cg, e.right) or_return

		#partial switch e.operator.type {
		case .PLUS:
			emit_opcode(cg, .OP_ADD)
		case .MINUS:
			emit_opcode(cg, .OP_SUBTRACT)
		case .STAR:
			emit_opcode(cg, .OP_MULTIPLY)
		case .SLASH:
			emit_opcode(cg, .OP_DIVIDE)
		case .PERCENT:
			emit_opcode(cg, .OP_MODULO)
		case .EQUAL_EQUAL:
			emit_opcode(cg, .OP_EQUAL)
		case .BANG_EQUAL:
			emit_opcode(cg, .OP_EQUAL, .OP_NOT)
		case .GREATER:
			emit_opcode(cg, .OP_GREATER)
		case .GREATER_EQUAL:
			emit_opcode(cg, .OP_LESS, .OP_NOT)
		case .LESS:
			emit_opcode(cg, .OP_LESS)
		case .LESS_EQUAL:
			emit_opcode(cg, .OP_GREATER, .OP_NOT)
		case:
			codegen_error(
				cg,
				fmt.tprintf(
					"Invalid binary operator '%s'. This is a compiler bug.",
					e.operator.lexeme,
				),
			)
			return false
		}
	case ^CallExpr:
		cg.current_token = e.token
		arguments := e.arguments
		callee := e.callee

		arg_count := len(arguments)

		if get_expr, ok := e.callee.(^GetExpr); ok {
			// This branch compiles a method invocation.
			compile_expression(cg, get_expr.receiver) or_return
			name := try2(cg, identifier_constant(cg, get_expr.property)) or_return

			if cg.pipeline_active {
				emit_opcode(cg, .OP_GET_IT)
				arg_count += 1
			}

			// Arg count can't be more than 255 since it's stuffed in one byte.
			if len(arguments) > U8_MAX {
				codegen_error(cg, "Cannot have more than 255 arguments.")
				return false
			}

			for arg in e.arguments {
				compile_expression(cg, arg) or_return
			}

			emit_op_with_constant(cg, .OP_INVOKE, .OP_INVOKE_LONG, name)
			emit_byte(cg, u8(arg_count))
		} else {
			// Compile the callee.
			compile_expression(cg, callee) or_return

			// If in a pipeline, fetch `it` as the first argument.
			if cg.pipeline_active {
				emit_opcode(cg, .OP_GET_IT)
				arg_count += 1
			}

			if len(arguments) > U8_MAX {
				codegen_error(cg, "Cannot have more than 255 arguments.")
				return false
			}

			for arg in arguments {
				compile_expression(cg, arg) or_return
			}

			emit_instruction(cg, .OP_CALL, u8(arg_count))
		}
	case ^GetExpr:
		cg.current_token = e.token
		receiver := e.receiver
		property := e.property

		compile_expression(cg, receiver) or_return

		property_name := try2(cg, identifier_constant(cg, property)) or_return
		emit_op_with_constant(cg, .OP_GET_PROPERTY, .OP_GET_PROPERTY_LONG, property_name)
	case ^SetExpr:
		cg.current_token = e.token
		receiver := e.receiver
		property := e.property
		value := e.value

		if r, ok := receiver.(^VariableExpr); ok {
			if binding_exists_and_is_final(cg, r.name) {
				codegen_error(cg, "Can only set a final variable once.")
				return false
			}
		}

		compile_expression(cg, receiver) or_return
		compile_expression(cg, value) or_return

		property_name := try2(cg, identifier_constant(cg, property)) or_return
		emit_op_with_constant(cg, .OP_SET_PROPERTY, .OP_SET_PROPERTY_LONG, property_name)

	// TODO: Writeback for COW, I'll enable it once the rest is stable
	// if var_expr, ok := e.object.(^VariableExpr); ok {
	//     try(cg, emit_named_variable_set(cg, var_expr.name)) or_return
	// }
	case ^GroupingExpr:
		cg.current_token = e.token
		compile_expression(cg, e.expression) or_return
	case ^ItExpr:
		cg.current_token = e.token
		if !cg.pipeline_active {
			codegen_error(cg, "Cannot use 'it' outside of a pipeline.")
			return false
		}

		emit_opcode(cg, .OP_GET_IT)
	case ^LambdaExpr:
		cg.current_token = e.token
		f := e.func_decl

		compile_function(
			cg,
			synthetic_token("lambda"),
			f.params,
			f.body,
			.LAMBDA,
			public = false,
		) or_return
	case ^ListExpr:
		cg.current_token = e.token
		elements := e.elements
		if len(elements) > U8_MAX {
			codegen_error(cg, "Cannot have more than 255 items in a list literal.")
			return false
		}

		for element in elements {
			compile_expression(cg, element) or_return
		}
		emit_instruction(cg, .OP_LIST, u8(len(elements)))
	case ^LiteralExpr:
		cg.current_token = e.token

		switch v in e.value {
		case f64:
			try(cg, emit_constant(cg, number_val(v))) or_return
		case string:
			try(cg, emit_constant(cg, obj_val(copy_string(cg.gc, v)))) or_return
		case bool:
			if v == true {
				emit_opcode(cg, .OP_TRUE)
			} else {
				emit_opcode(cg, .OP_FALSE)
			}
		// nil case
		case:
			emit_opcode(cg, .OP_NIL)
		}
	case ^LogicalExpr:
		cg.current_token = e.token
		compile_expression(cg, e.left) or_return

		#partial switch e.operator.type {
		case .OR:
			end_jump := emit_jump(cg, .OP_JUMP_IF_TRUE)

			emit_pop(cg)
			compile_expression(cg, e.right) or_return
			try(cg, patch_jump(cg, end_jump)) or_return
		case .AND:
			end_jump := emit_jump(cg, .OP_JUMP_IF_FALSE)

			emit_pop(cg)
			compile_expression(cg, e.right) or_return
			try(cg, patch_jump(cg, end_jump)) or_return
		case:
			codegen_error(
				cg,
				fmt.tprintf(
					"Invalid logical operator '%s'. This is a compiler bug.",
					e.operator.lexeme,
				),
			)
			return false
		}
	case ^PipeExpr:
		cg.current_token = e.token
		compile_expression(cg, e.left) or_return
		emit_opcode(cg, .OP_SET_IT)
		old_pipeline := cg.pipeline_active
		cg.pipeline_active = true
		compile_expression(cg, e.right) or_return
		cg.pipeline_active = old_pipeline
	case ^SubscriptExpr:
		cg.current_token = e.token
		receiver := e.receiver
		index := e.index

		compile_expression(cg, receiver) or_return
		compile_expression(cg, index) or_return
		emit_opcode(cg, .OP_SUBSCRIPT)
	case ^SubscriptSetExpr:
		cg.current_token = e.token
		compile_expression(cg, e.receiver) or_return
		compile_expression(cg, e.index) or_return
		compile_expression(cg, e.value) or_return
		emit_opcode(cg, .OP_SUBSCRIPT_SET)
	case ^SuperExpr:
		cg.current_token = e.token
		method := e.method
		method_args := e.method_args

		if cg.current_class == nil {
			codegen_error(cg, "Can't use 'super' outside a class.")
			return false
		} else if !cg.current_class.has_superclass {
			codegen_error(cg, "Can't use 'super' in a class with no superclass.")
			return false
		}

		name := try2(cg, identifier_constant(cg, method)) or_return

		/* Place both the current receiver and the superclass on the stack. */
		try(cg, emit_named_variable(cg, synthetic_token("this"), can_assign = false)) or_return

		/* Check if the method is immediately invoked or not; since we can apply
     	an optimization involving no use of bound methods if it is. */
		if method_args != nil {
			if len(method_args) > U8_COUNT {
				codegen_error(cg, "Cannot have more than 255 arguments.")
				return false
			}
			arg_count := u8(len(method_args))

			for arg in method_args {
				compile_expression(cg, arg) or_return
			}

			try(
				cg,
				emit_named_variable(cg, synthetic_token("super"), can_assign = false),
			) or_return
			emit_op_with_constant(cg, .OP_SUPER_INVOKE, .OP_SUPER_INVOKE_LONG, name)
			emit_byte(cg, arg_count)
		} else {
			try(
				cg,
				emit_named_variable(cg, synthetic_token("super"), can_assign = false),
			) or_return
			emit_op_with_constant(cg, .OP_GET_SUPER, .OP_GET_SUPER_LONG, name)
		}
	case ^ThisExpr:
		cg.current_token = e.token
		if cg.current_class == nil {
			codegen_error(cg, "Cannot use 'this' outside a class.")
			return false
		}

		/* `this` is treated as a lexically scoped local variable whose value is
        somehow magically initialized. Also, can_assign is set to false because
        you obviously can't assign to `this`. */
		try(cg, emit_named_variable(cg, e.token, can_assign = false)) or_return
	case ^UnaryExpr:
		cg.current_token = e.token
		compile_expression(cg, e.right) or_return

		#partial switch e.operator.type {
		case .NOT:
			emit_opcode(cg, .OP_NOT)
		case .MINUS:
			emit_opcode(cg, .OP_NEGATE)
		case:
			codegen_error(
				cg,
				fmt.tprintf(
					"Invalid unary operator '%s'. This is a compiler bug.",
					e.operator.lexeme,
				),
			)
			return false
		}
	case ^VariableExpr:
		cg.current_token = e.token
		try(cg, emit_named_variable(cg, e.token, can_assign = false)) or_return
	}

	return true
}

// Helper to avoid a billion `if err != nil`s everywhere
@(private = "file")
@(require_results)
try :: #force_inline proc(cg: ^Codegen, err: ErrorMessage) -> bool {
	if err != nil {
		codegen_error(cg, err.?)
		return false
	}
	return true
}

// `try` for procedures that return a value and possibly an error
@(private = "file")
@(require_results)
try2 :: #force_inline proc(cg: ^Codegen, ret: $T, err: ErrorMessage) -> (T, bool) {
	if err != nil {
		codegen_error(cg, err.?)
		return ret, false
	}
	return ret, true
}

/* 
Report an error at the current token with a message. This doesn't propagate
errors or stop compilation by itself, so it should either be used with `try`
or a return should be made immediately after calling it.
*/
@(private = "file")
codegen_error :: proc(cg: ^Codegen, message: string) {
	token := cg.current_token
	color_red(os.stderr, "compile error ")

	if token.type == .EOF {
		fmt.eprintf("at end")
	} else {
		fmt.eprintf("at '%s'", token.lexeme)
	}

	fmt.eprintfln(": %s", message)
	fmt.eprintfln("  on [line %d]", token.line)
	cg.had_error = true
}

/* Emit a return instruction and decode the RLE-encoded lines. */
@(private = "file")
end_compiler :: proc(cg: ^Codegen) -> ^ObjFunction {
	fn := cg.current_compiler.function
	if !fn.has_returned {
		emit_return(cg)
	}

	if config.dump_disassembly {
		if !cg.had_error {
			disassemble(current_chunk(cg), fn.name != nil ? fn.name.chars : "<script>")
		}
	}

	free_loops(cg)
	cg.current_compiler = cg.current_compiler.enclosing

	return fn
}

init_compiler :: proc(c: ^Compiler, cg: ^Codegen, name: Token, type: FunctionType) {
	c^ = Compiler {
		enclosing   = cg.current_compiler,
		type        = type,
		local_count = 0,
		loop_count  = 0,
		scope_depth = 0,
		function    = nil,
	}

	/* DON'T FORGET to set current_compiler to the new compiler!
	Took me WAAAAAAAAAAY too long to figure out I was missing this. */
	cg.current_compiler = c
	c.function = new_function(cg.gc)

	if type != .SCRIPT && type != .LAMBDA {
		c.function.name = copy_string(cg.gc, name.lexeme)
	} else if type == .LAMBDA {
		c.function.name = copy_string(cg.gc, "lambda")
	}

	/* The first slot is the function itself. */
	local := &cg.current_compiler.locals[cg.current_compiler.local_count]
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

/* Compile the provided abstract syntax tree (array of declarations) into a bytecode chunk. */
codegen :: proc(gc: ^GC, decls: []Decl, globals: ^Table) -> (fn: ^ObjFunction, success: bool) {
	/* Add all the native function names to the global table, for variable
     * existence checks. */
	for fn_name in gc.global_native_fns {
		table_set(globals, copy_string(gc, fn_name), bool_val(true))
	}

	cg := Codegen {
		globals         = globals,
		gc              = gc,
		prev_mark_roots = gc.mark_roots_arg,
		pipeline_active = false,
		had_error       = false,
	}
	gc.mark_roots_arg = &cg

	c: Compiler
	init_compiler(&c, &cg, synthetic_token(""), .SCRIPT)

	for decl in decls {
		compile_declaration(&cg, decl) or_break
	}

	res_fn := end_compiler(&cg)
	gc.mark_roots_arg = cg.prev_mark_roots
	return res_fn, !cg.had_error
}

@(private = "file")
collect_decl_globals :: proc(globals: ^Table, gc: ^GC, decl: Decl) {
	switch d in decl {
	case ^VarDecl:
	case ^FuncDecl:
		name := copy_string(gc, d.name.lexeme)
		if _, ok := table_get(globals, name); ok {
			return
		}
		table_set(globals, name, bool_val(false))
	case ^ClassDecl:
	case ^PubDecl:
		collect_decl_globals(globals, gc, d.decl)
	case ^ModuleDecl:
	case Stmt:
	}
}

/* Collect all global functions declared in the file and put them into the
`globals` table. */
collect_script_globals :: proc(globals: ^Table, gc: ^GC, decls: []Decl) {
	for fn_name in gc.global_native_fns {
		table_set(globals, copy_string(gc, fn_name), bool_val(true))
	}
	for decl in decls {
		collect_decl_globals(globals, gc, decl)
	}
}
