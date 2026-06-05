package zen

import "core:fmt"
import "core:os"

/*
TODO: Variable resolution / symbol table creation. Needs very careful
design so that it can be used seamlessly in the codegen part without any
awful hacky designs. Some points:
- Classes and OOP SHOULD work fine and without hacks because records will be
     in soon and therefore will need to work nicely, but on that same note
     they're not a huge priority as they'll be replaced by said records
- The upvalue-based approach used in clox and inherited by zen ever since
     its creation can now safely be replaced because zen now has an AST
     and therefore enough context to know what variables are closed over
     and when; the idea now is to create a new ObjCell type to replace
     ObjUpvalue that is nothing but a heap-allocated value used for closed
     over variables. This point needs to be kept in mind for closing over
     loop variables as well.
- Because of the addition of the Hindley-Milner type checker, modules can
     no longer be viably used as values. Why? Well, if we wanted to viably
     use modules as values, we'd need to typecheck them. So they'd be
     some sort of record type. But, these modules have polymorphic functions
     inside of them; for instance the list.pop function which has a quantified
     type of `forall a. List a -> a`. The list module would then be a type
     that looks something like `Record (forall a. List a -> a, ...)`, and
     this directly hits a limit of Hindley-Milner; polymorphic types are
     NOT allowed as type parameters i.e. we can only have rank-1 polymorphism.
     A lot of changes are needed throughout zen for this; some perhaps in the
     semantic analyzer.
*/

SemanticCompiler :: struct {
	enclosing:        ^SemanticCompiler, // The enclosing function.
	func_type:        FunctionType, // Type of the function being checked.

	// Modules that have been imported. This map is nil outside global scope.
	imported_modules: map[string]ModuleImport,
	loop_depth:       int, // How many loops in are we?
	scope_depth:      int, // The number of blocks in scope of this function.
	locals:           [U8_COUNT]Local, // Array of local variables.
	local_count:      int, // Number of local variables.
	capture_count:    int, // Number of variables captured from outer scopes.
}

ModuleImport :: struct {
	name:     string,
	fullpath: string,
	type:     ModuleType,
}

/* Main state for the semantic analysis pass. Holds the current scope,
class context, pipeline state and some other necessary items.
One Semantic instance is created per call to `analyze` and lives until
the caller (codegen) finishes consuming the resolution map. */
Semantic :: struct {
	current_compiler: ^SemanticCompiler,
	current_class:    ^ClassCompiler,
	current_token:    Token,
	had_error:        bool,
	pipeline_active:  bool,
	gc:               ^GC,
	globals:          ^Table,
}

init_semantic_compiler :: proc(sm: ^Semantic, c: ^SemanticCompiler, type: FunctionType) {
	c^ = SemanticCompiler {
		capture_count = 0,
		local_count   = 0,
		scope_depth   = 0,
		loop_depth    = 0,
		enclosing     = sm.current_compiler,
		func_type     = type,
	}

	if type == .SCRIPT {
		c.imported_modules = make(map[string]ModuleImport)
	}

	sm.current_compiler = c
}

// free the whole scope chain
destroy_semantic_compiler :: proc(cmp: ^SemanticCompiler) {
	c := cmp
	for c != nil {
		enclosing := c.enclosing
		if c.imported_modules != nil {delete(c.imported_modules)}
		free(c)
		c = enclosing
	}
}

/* Pop back to the enclosing compiler when we exit a function scope. */
end_semantic_compiler :: proc(sm: ^Semantic) {
	sm.current_compiler = sm.current_compiler.enclosing
}

init_semantic :: proc(gc: ^GC, globals: ^Table) -> Semantic {
	return Semantic {
		current_compiler = nil,
		current_class = nil,
		had_error = false,
		pipeline_active = false,
		gc = gc,
		globals = globals,
	}
}

semantic_error :: proc(sm: ^Semantic, message: string) {
	token := sm.current_token
	color_red(os.stderr, "compile error ")

	if token.type == .EOF {
		fmt.eprint("at end")
	} else if token.type == .NEWLINE {
		fmt.eprint("at end of line")
	} else {
		fmt.eprintf("at '%s'", token.lexeme)
	}

	fmt.eprintfln(": %s", message)
	fmt.eprintfln("  on [line %d]", token.line)
	sm.had_error = true
}

begin_semantic_scope :: proc(sm: ^Semantic) {
	sm.current_compiler.scope_depth += 1
}

/* 
End the current scope. Removes any local variables that were declared in the
scope from the compiler's local table. (The codegen handles emitting the
OP_POP or OP_CLOSE_UPVALUE bytecodes during its own end_scope pass.)
*/
@(private = "file")
end_semantic_scope :: proc(sm: ^Semantic) {
	sm.current_compiler.scope_depth -= 1
}

/* Register names of top-level function declarations and named lambdas in the
globals table, to allow mutual recursion. Native function names are also
registered. */
collect_forward_refs :: proc(sm: ^Semantic, expr: Expr) {
	if expr == nil {return}

	#partial switch e in expr {
	case ^VarDeclExpr:
		for binding in e.bindings {
			if _, ok := binding.initializer.(^FunctionExpr); !ok {
				continue
			}

			name := copy_string(sm.gc, binding.name.lexeme)
			if _, ok := table_get(sm.globals, name); ok {
				continue
			}
			table_set(sm.globals, name, nil_val())
		}
	case ^SequenceExpr:
		collect_forward_refs(sm, e.left)
		// process iteratively instead of recursively to avoid stack overflows
		{
			right := e.right
			for {
				next_seq, ok := right.(^SequenceExpr)
				if !ok {
					collect_forward_refs(sm, right)
					break
				}
				collect_forward_refs(sm, next_seq.left)
				right = next_seq.right
			}
		}
	case:
	// other cases don't matter
	}
}

@(require_results)
resolve_local :: proc(
	compiler: ^SemanticCompiler,
	name: Token,
) -> (
	index: int,
	is_final: bool,
	err: ErrorMessage,
) {
	// Look for the name in the local scopes of the current function.
	for i := compiler.local_count - 1; i >= 0; i -= 1 {
		local := &compiler.locals[i]
		if identifiers_equal(name, local.name) {
			if local.depth == -1 {
				return -1, local.is_final, "Cannot read local variable in its own initializer."
			}
			return i, local.is_final, nil
		}
	}

	// Not found in the scopes of the current function.
	return -1, false, nil
}

@(require_results)
resolve_upvalue :: proc(
	compiler: ^SemanticCompiler,
	name: Token,
) -> (
	index: int,
	is_final: bool,
	error: ErrorMessage,
) {
	/* Base case 1: We reached the end of the compiler stack, so the name is probably in the
	global scope. */
	if compiler.enclosing == nil {
		return -1, false, nil
	}

	/* Look for the name in the enclosing function's local scope. 
	Base case 2: If we find the name there, return it. */
	local, local_is_final := resolve_local(compiler.enclosing, name) or_return
	if local != -1 {
		if compiler.capture_count == U8_COUNT {
			return -1, false, "Too many closure variables in function."
		}

		// Mark the local as captured and see if its a `var` or `val`.
		compiler.enclosing.locals[local].is_captured = true
		compiler.capture_count += 1
		return local, local_is_final, nil
	}

	/* Recursively look for an upvalue in the enclosing function. */
	upvalue, upvalue_is_final := resolve_upvalue(compiler.enclosing, name) or_return
	if upvalue != -1 {
		if compiler.capture_count == U8_COUNT {
			return -1, false, "Too many closure variables in function."
		}

		// Set the found value as a captured local.
		compiler.enclosing.locals[local].is_captured = true
		compiler.capture_count += 1
		return upvalue, upvalue_is_final, nil
	}

	// Nope, didn't find anything.
	return -1, false, nil
}

@(require_results)
add_local :: proc(
	cmp: ^SemanticCompiler,
	name: Token,
	is_final: bool,
	is_loop_variable: bool = false,
) -> ErrorMessage {
	if cmp.local_count == U8_COUNT {
		return "Too many local variables in function."
	}

	defer cmp.local_count += 1
	local := &cmp.locals[cmp.local_count]
	local.name = name
	local.depth = -1
	local.is_final = is_final
	local.is_captured = false
	local.is_loop_variable = is_loop_variable
	return nil
}

@(require_results)
binding_exists :: proc(sm: ^Semantic, name: Token) -> bool {
	local, _, _ := resolve_local(sm.current_compiler, name)
	upvalue, _, _ := resolve_upvalue(sm.current_compiler, name)

	if local != -1 {
		return true
	} else if upvalue != -1 {
		return true
	} else {
		global_o_str := copy_string(sm.gc, name.lexeme)
		if _, ok := table_get(sm.globals, global_o_str); ok {
			return true
		}
		return false
	}
}

// assumes the binding exists
@(require_results)
binding_is_final :: proc(sm: ^Semantic, name: Token) -> bool {
	local, local_is_final, _ := resolve_local(sm.current_compiler, name)
	upvalue, upvalue_is_final, _ := resolve_upvalue(sm.current_compiler, name)

	if local != -1 {
		return local_is_final
	} else if upvalue != -1 {
		return upvalue_is_final
	} else {
		global_o_str := copy_string(sm.gc, name.lexeme)
		if value, ok := table_get(sm.globals, global_o_str); ok {
			if values_equal(value, bool_val(true)) {
				return true
			}
		}
		return false
	}
}

@(require_results)
in_global_scope :: proc(sm: ^Semantic) -> bool {
	return sm.current_compiler.func_type == .SCRIPT && sm.current_compiler.scope_depth == 0
}

@(require_results)
declare_variable :: proc(
	sm: ^Semantic,
	name: Token,
	is_final: bool,
	is_loop_variable: bool = false,
) -> ErrorMessage {
	if in_global_scope(sm) {
		global_o_str := copy_string(sm.gc, name.lexeme)
		if value, ok := table_get(sm.globals, global_o_str); ok && !is_nil(value) {
			if values_equal(value, bool_val(true)) {
				return(
					is_final ? "Cannot redefine a final variable." : "Cannot redefine a final variable as normal variable." \
				)
			} else if is_final {
				return "Cannot redefine a variable as final variable."
			}
		} else {
			table_set(sm.globals, global_o_str, bool_val(is_final))
		}
	}

	for i := sm.current_compiler.local_count - 1; i >= 0; i -= 1 {
		local := &sm.current_compiler.locals[i]
		if local.depth != -1 && local.depth < sm.current_compiler.scope_depth {
			break
		}

		if identifiers_equal(name, local.name) {
			return "A variable with this name in this scope already exists."
		}
	}

	return add_local(sm.current_compiler, name, is_final, is_loop_variable)
}

define_variable :: proc(sm: ^Semantic) {
	if in_global_scope(sm) {
		return
	}

	mark_initialized(sm)
}

mark_initialized :: proc(sm: ^Semantic) {
	if in_global_scope(sm) {return}
	sm.current_compiler.locals[sm.current_compiler.local_count - 1].depth =
		sm.current_compiler.scope_depth
}

// Full semantic analysis phase, done after resolving forward references in the
// global scope.
@(require_results)
_analyze :: proc(sm: ^Semantic, expr: Expr) -> bool {
	if expr == nil {
		return true
	}

	switch e in expr {
	case ^AssignExpr:
		sm.current_token = e.token
		if !binding_exists(sm, e.name) {
			semantic_error(sm, fmt.tprintf("Undefined variable '%v'.", e.name.lexeme))
			return false
		}

		if binding_is_final(sm, e.name) {
			semantic_error(sm, "Can only set a final variable once.")
			return false
		}
		_analyze(sm, e.value) or_return
	case ^BinaryExpr:
		sm.current_token = e.token
		_analyze(sm, e.left) or_return
		_analyze(sm, e.right) or_return
	case ^BlockExpr:
		sm.current_token = e.token
		begin_semantic_scope(sm)
		_analyze(sm, e.expression) or_return
		end_semantic_scope(sm)
	case ^BreakExpr:
		sm.current_token = e.token
		if sm.current_compiler.loop_depth == 0 {
			semantic_error(sm, "Cannot break outside a loop.")
			return false
		}
	case ^CallExpr:
		sm.current_token = e.token
		_analyze(sm, e.callee) or_return

		arg_count := len(e.arguments)
		if sm.pipeline_active {
			arg_count += 1
		}

		// Arg count can't be more than 255 since it's stuffed in one byte.
		if arg_count > U8_MAX {
			semantic_error(sm, "Cannot have more than 255 arguments.")
			return false
		}

		for arg in e.arguments {
			_analyze(sm, arg) or_return
		}
	case ^ClassExpr:
		unimplemented()
	case ^ContinueExpr:
		sm.current_token = e.token
		if sm.current_compiler.loop_depth == 0 {
			semantic_error(sm, "Cannot use 'continue' outside a loop.")
			return false
		}
	case ^DiscardExpr:
		sm.current_token = e.token
		_analyze(sm, e.expression) or_return
	case ^ExitExpr:
		sm.current_token = e.token
		_analyze(sm, e.code) or_return
	case ^ForExpr:
		sm.current_token = e.token

		begin_semantic_scope(sm)

		_analyze(sm, e.initializer) or_return

		loop_depth := 0
		if sm.current_compiler != nil {
			loop_depth = sm.current_compiler.loop_depth
			sm.current_compiler.loop_depth += 1
		}

		_analyze(sm, e.condition) or_return
		_analyze(sm, e.increment) or_return
		_analyze(sm, e.body) or_return

		if sm.current_compiler != nil {
			sm.current_compiler.loop_depth = loop_depth
		}

		end_semantic_scope(sm)
	case ^ForInExpr:
		sm.current_token = e.token
		var_name := e.var_name
		iterable := e.iterable
		body := e.body

		begin_semantic_scope(sm)

		try(sm, declare_variable(sm, var_name, is_final = true, is_loop_variable = true)) or_return
		define_variable(sm)

		_analyze(sm, iterable) or_return

		try(
			sm,
			add_local(
				sm.current_compiler,
				synthetic_token("__iter"),
				is_final = true,
				is_loop_variable = true,
			),
		) or_return
		mark_initialized(sm)

		try(
			sm,
			add_local(
				sm.current_compiler,
				synthetic_token("__idx"),
				is_final = true,
				is_loop_variable = true,
			),
		) or_return
		mark_initialized(sm)

		begin_semantic_scope(sm)
		_analyze(sm, body) or_return
		end_semantic_scope(sm)

		end_semantic_scope(sm)
	case ^GetExpr:
		sm.current_token = e.token
		_analyze(sm, e.receiver) or_return
	case ^GroupingExpr:
		sm.current_token = e.token
		_analyze(sm, e.expression) or_return
	case ^IfExpr:
		sm.current_token = e.token
		_analyze(sm, e.condition) or_return
		_analyze(sm, e.then_branch) or_return
		if e.else_branch != nil {
			_analyze(sm, e.else_branch) or_return
		}
	case ^ItExpr:
		sm.current_token = e.token
		if !sm.pipeline_active {
			semantic_error(sm, "Cannot use 'it' outside of a pipeline.")
			return false
		}
	case ^FunctionExpr:
		unimplemented()
	case ^ListExpr:
		sm.current_token = e.token

		if len(e.elements) > U8_MAX {
			semantic_error(sm, "Cannot have more than 255 items in a list literal.")
			return false
		}

		for element in e.elements {
			_analyze(sm, element) or_return
		}
	case ^LiteralExpr:
		sm.current_token = e.token
	case ^LogicalExpr:
		sm.current_token = e.token
		_analyze(sm, e.left) or_return
		_analyze(sm, e.right) or_return
	case ^PipeExpr:
		sm.current_token = e.token
		_analyze(sm, e.left) or_return
		old_pipeline := sm.pipeline_active
		sm.pipeline_active = true
		_analyze(sm, e.right) or_return
		sm.pipeline_active = old_pipeline
	case ^PrintExpr:
		sm.current_token = e.token
		_analyze(sm, e.expr) or_return
	case ^ReturnExpr:
		sm.current_token = e.token
		if sm.current_compiler.func_type == .SCRIPT {
			semantic_error(sm, "Cannot return from the top level.")
			return false
		}

		if e.value != nil && sm.current_compiler.func_type == .INITIALIZER {
			semantic_error(sm, "Cannot return a value from an initializer.")
			return false
		}

		_analyze(sm, e.value) or_return
	case ^SequenceExpr:
		sm.current_token = e.token
		_analyze(sm, e.left) or_return
		// process iteratively instead of recursively to avoid stack overflows,
		// because SequenceExprs can get extremely deep in large programs
		{
			right := e.right
			for {
				next_seq, ok := right.(^SequenceExpr)
				if !ok {
					_analyze(sm, right) or_return
					break
				}
				sm.current_token = next_seq.token
				_analyze(sm, next_seq.left) or_return
				right = next_seq.right
			}
		}
	case ^SetExpr:
		sm.current_token = e.token
		receiver := e.receiver
		property := e.property
		value := e.value

		if r, ok := receiver.(^VariableExpr); ok {
			if binding_exists(sm, r.name) && binding_is_final(sm, r.name) {
				semantic_error(sm, "Can only set a final variable once.")
				return false
			}
		}

		_analyze(sm, receiver) or_return
		_analyze(sm, value) or_return
	case ^SubscriptExpr:
		sm.current_token = e.token
		_analyze(sm, e.receiver) or_return
		_analyze(sm, e.index) or_return
	case ^SubscriptSetExpr:
		sm.current_token = e.token
		receiver := e.receiver
		index := e.index
		value := e.value

		if r, ok := receiver.(^VariableExpr); ok {
			if binding_exists(sm, r.name) && binding_is_final(sm, r.name) {
				semantic_error(sm, "Can only set a final variable once.")
				return false
			}
		}

		_analyze(sm, receiver) or_return
		_analyze(sm, index) or_return
		_analyze(sm, value) or_return
	case ^SuperExpr:
		sm.current_token = e.token

		if sm.current_class == nil {
			semantic_error(sm, "Can't use 'super' outside a class.")
			return false
		} else if !sm.current_class.has_superclass {
			semantic_error(sm, "Can't use 'super' in a class with no superclass.")
			return false
		}

		if e.method_args != nil {
			if len(e.method_args) > U8_MAX {
				semantic_error(sm, "Cannot have more than 255 arguments.")
				return false
			}

			for arg in e.method_args {
				_analyze(sm, arg) or_return
			}
		}
	case ^SwitchExpr:
		sm.current_token = e.token
		_analyze(sm, e.condition) or_return
		for c in e.cases {
			_analyze(sm, c.condition) or_return
			_analyze(sm, c.body) or_return
		}
		_analyze(sm, e.else_branch) or_return
	case ^ThisExpr:
		sm.current_token = e.token
		if sm.current_class == nil {
			semantic_error(sm, "Cannot use 'this' outside a class.")
			return false
		}
	case ^UnaryExpr:
		sm.current_token = e.token
		_analyze(sm, e.right) or_return
	case ^UseExpr:
		sm.current_token = e.token
		if !in_global_scope(sm) {
			semantic_error(sm, "Can only declare modules at the top level.")
			return false
		}
		unimplemented()
	case ^VariableExpr:
		sm.current_token = e.token
		name := e.name

		if !binding_exists(sm, name) {
			semantic_error(sm, fmt.tprintf("Undefined variable '%v'.", name.lexeme))
			return false
		}
	case ^VarDeclExpr:
		sm.current_token = e.token
		is_final := e.is_final

		for binding in e.bindings {
			sm.current_token = binding.name
			if binding.initializer == nil && is_final {
				semantic_error(sm, "Final variables must be initialized.")
				return false
			}

			try(sm, declare_variable(sm, binding.name, is_final)) or_return

			// allow anonymous fns to recurse
			if _, ok := binding.initializer.(^FunctionExpr); ok {
				mark_initialized(sm)
			}
			_analyze(sm, binding.initializer) or_return
			define_variable(sm)
		}
	case ^WhileExpr:
		sm.current_token = e.token

		loop_depth := 0
		if sm.current_compiler != nil {
			loop_depth = sm.current_compiler.loop_depth
			sm.current_compiler.loop_depth += 1
		}

		_analyze(sm, e.condition) or_return
		_analyze(sm, e.body) or_return

		if sm.current_compiler != nil {
			sm.current_compiler.loop_depth = loop_depth
		}
	}

	return true
}

// Two-pass semantic analyzer
@(require_results)
analyze :: proc(gc: ^GC, expr: Expr, globals: ^Table) -> (cmp: ^SemanticCompiler, success: bool) {
	if expr == nil {
		return nil, true
	}

	// Add native function names to the globals table
	for fn_name in GLOBAL_NATIVE_FN_NAMES {
		table_set(globals, copy_string(gc, fn_name), bool_val(true))
	}

	sm := init_semantic(gc, globals)

	// allocate on the heap, we need this for codegen
	script_compiler := new(SemanticCompiler)
	init_semantic_compiler(&sm, script_compiler, .SCRIPT)
	// Don't end the compiler as it exists for the entire script scope.

	// Pass 1: Collect forward references for top-level function declarations
	// This enables mutual recursion between functions.
	collect_forward_refs(&sm, expr)

	// Pass 2: Full semantic analysis
	ok := _analyze(&sm, expr)
	if !ok {
		destroy_semantic_compiler(script_compiler)
		return nil, false
	}
	return script_compiler, true
}
