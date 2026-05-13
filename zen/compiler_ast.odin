package zen

import "core:fmt"

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
	globals:          ^Table,
	gc:               ^GC,
	prev_mark_roots:  RootSource,
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
identifier_constant :: proc(cg: ^Codegen, name: Token) -> (int, ErrorMessage) {
	return make_constant(cg, obj_val(copy_string(cg.gc, name.lexeme)))
}

/* Similar to identifier_constant, but you can pass in just a string.
 * Used to implement the modules, as module names are STRING tokens rather than
 * IDENT tokens. */
@(private = "file")
string_constant :: proc(cg: ^Codegen, text: string) -> (int, ErrorMessage) {
	return make_constant(cg, obj_val(copy_string(cg.gc, text)))
}

/* Write a byte to the chunk being compiled. */
@(private = "file")
emit_byte :: proc(cg: ^Codegen, byait: byte) {
	write_chunk(current_chunk(cg), byait, cg.current_token.line)
}

/* Write two bytes to the current chunk. */
@(private = "file")
emit_bytes :: proc(cg: ^Codegen, byait1: byte, byait2: byte) {
	emit_byte(cg, byait1)
	emit_byte(cg, byait2)
}

/* Write an opcode as a byte to the chunk being compiled. */
@(private = "file")
emit_opcode :: proc(cg: ^Codegen, oc: OpCode) {
	write_chunk(current_chunk(cg), byte(oc), cg.current_token.line)
}

@(private = "file")
emit_opcodes :: proc(cg: ^Codegen, oc1: OpCode, oc2: OpCode) {
	emit_opcode(cg, oc1)
	emit_opcode(cg, oc2)
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
		emit_bytes(cg, byte(OpCode.OP_CONSTANT), byte(index))
	} else {
		emit_opcode(cg, .OP_CONSTANT_LONG)
		emit_byte(cg, byte((index >> 8) & 0xff))
		emit_byte(cg, byte(index & 0xff))
	}

	return nil
}

/* 
Write a certain opcode to the current chunk followed by an appropriately sized
constant index.
*/
@(private = "file")
emit_op_with_constant :: proc(cg: ^Codegen, short_op: OpCode, long_op: OpCode, index: int) {
	if index <= U8_MAX {
		emit_bytes(cg, byte(short_op), byte(index))
	} else {
		emit_opcode(cg, long_op)
		emit_byte(cg, byte((index >> 8) & 0xff))
		emit_byte(cg, byte(index & 0xff))
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
	for i in cg.current_compiler.loops {
		delete(i.breaks)
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
				return 0, "Cannot read local variable in its own initializer."
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
		return idx, is_final, err
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
		idx, err := add_upvalue(cg, compiler, u8(local), is_local = true)
		return idx, is_final, err
	}

	// Nope, didn't find anything.
	// The `false` is just a dummy value.
	return -1, false, nil
}

/* Add an upvalue to the function or return it if it already exists. */
@(private = "file")
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
emit_named_variable :: proc(cg: ^Codegen, name: Token, can_assign: bool) -> ErrorMessage {
	local := resolve_local(cg, cg.current_compiler, name) or_return
	upvalue, _ := resolve_upvalue(cg, cg.current_compiler, name) or_return

	if local != -1 {
		emit_bytes(cg, byte(OpCode.OP_GET_LOCAL), byte(local))
	} else if upvalue != -1 {
		emit_bytes(cg, byte(OpCode.OP_GET_UPVALUE), byte(upvalue))
	} else {
		global_o_str := copy_string(cg.gc, name.lexeme)
		if _, ok := table_get(cg.current_compiler.globals, global_o_str); !ok {
			return fmt.tprintf("Undefined variable '%s'.", name.lexeme)
		}

		arg := identifier_constant(cg, name) or_return
		emit_op_with_constant(
			cg,
			arg <= U8_MAX ? .OP_GET_GLOBAL : .OP_GET_GLOBAL_LONG,
			.OP_GET_GLOBAL_LONG,
			arg,
		)
	}

	return nil
}

emit_named_variable_set :: proc(cg: ^Codegen, name: Token) -> ErrorMessage {
	local := resolve_local(cg, cg.current_compiler, name) or_return
	upvalue, upvalue_is_final := resolve_upvalue(cg, cg.current_compiler, name) or_return

	if local != -1 {
		if cg.current_compiler.locals[local].is_final {
			return "Can only set a final variable once."
		}
		emit_bytes(cg, byte(OpCode.OP_SET_LOCAL), byte(local))
	} else if upvalue != -1 {
		if upvalue_is_final {
			return "Can only set a final variable once."
		}
		emit_bytes(cg, byte(OpCode.OP_SET_UPVALUE), byte(upvalue))
	} else {
		arg := identifier_constant(cg, name) or_return

		global_o_str := copy_string(cg.gc, name.lexeme)
		value: Value; ok: bool
		if value, ok := table_get(cg.current_compiler.globals, global_o_str); ok {
			if values_equal(value, bool_val(true)) {
				return "Can only set a final variable once."
			}
		} else {
			table_set(cg.current_compiler.globals, global_o_str, bool_val(false))
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

@(require_results)
compile_var_declaration :: proc(cg: ^Codegen, decl: ^VarDecl) -> bool {
	unimplemented()
}

@(require_results)
compile_func_declaration :: proc(cg: ^Codegen, decl: ^FuncDecl, public: bool) -> bool {
	unimplemented()
}

@(require_results)
compile_class_declaration :: proc(cg: ^Codegen, decl: ^ClassDecl, public: bool) -> bool {
	unimplemented()
}

@(require_results)
compile_module_declaration :: proc(cg: ^Codegen, decl: ^ModuleDecl) -> bool {
	unimplemented()
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

	unreachable()
}

@(require_results)
compile_if_statement :: proc(cg: ^Codegen, s: ^IfStmt) -> bool {
	condition := s.condition
	then_branch := s.then_branch
	else_branch := s.else_branch

	compile_expression(cg, s.condition) or_return

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
	compile_statement(cg, s.then_branch) or_return
	end_scope(cg)

	else_jump := emit_jump(cg, .OP_JUMP)

	try(cg, patch_jump(cg, then_jump)) or_return
	emit_pop(cg)

	compile_statement(cg, else_branch) or_return
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
		emit_pop(cg)
	}

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
		compile_var_declaration(cg, iz) or_return
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
	#partial switch v in s.iterable {
	case ^ListExpr:
		compile_expression(cg, v) or_return
	case ^LiteralExpr:
		if v.value.type != .STRING {
			codegen_error(cg, "Can only iterate over lists or strings.")
			return false
		}
		compile_expression(cg, v) or_return
	case:
		codegen_error(cg, "Can only iterate over lists or strings.")
		return false
	}

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
		for decl in s.declarations {
			compile_declaration(cg, decl) or_return
		}
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

	unreachable()
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
		emit_named_variable_set(cg, e.name)
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
			emit_opcodes(cg, .OP_EQUAL, .OP_NOT)
		case .GREATER:
			emit_opcode(cg, .OP_GREATER)
		case .GREATER_EQUAL:
			emit_opcodes(cg, .OP_LESS, .OP_NOT)
		case .LESS:
			emit_opcode(cg, .OP_LESS)
		case .LESS_EQUAL:
			emit_opcodes(cg, .OP_GREATER, .OP_NOT)
		}
	case ^CallExpr:
		unimplemented()
	}

	unreachable()
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

@(private = "file")
codegen_error :: proc(cg: ^Codegen, message: string) {
	unimplemented()
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

init_compiler_cg :: proc(c: ^Compiler, cg: ^Codegen, type: FunctionType) {
	c^ = Compiler {
		enclosing   = cg.current_compiler,
		type        = type,
		local_count = 0,
		loop_count  = 0,
		scope_depth = 0,
		function    = nil,
		globals     = cg.globals,
	}

	/* DON'T FORGET to set current_compiler to the new compiler!
	Took me WAAAAAAAAAAY too long to figure out I was missing this. */
	cg.current_compiler = c
	c.function = new_function(cg.gc)

	// if type != .SCRIPT && type != .LAMBDA {
	// 	c.function.name = copy_string(cg.gc, cg.previous.lexeme)
	// } else if type == .LAMBDA {
	// 	c.function.name = copy_string(cg.gc, "lambda")
	// }

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

codegen :: proc(gc: ^GC, decls: []Decl, globals: ^Table) -> (^ObjFunction, bool) {
	cg := Codegen {
		globals         = globals,
		gc              = gc,
		prev_mark_roots = gc.mark_roots_arg,
		had_error       = false,
	}
	gc.mark_roots_arg = &cg

	c: Compiler
	init_compiler_cg(&c, &cg, .SCRIPT)

	for decl in decls {
		compile_declaration(&cg, decl) or_break
	}

	fn := end_compiler(&cg)
	gc.mark_roots_arg = cg.prev_mark_roots
	return fn, !cg.had_error
}
