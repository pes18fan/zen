package zen

import "core:fmt"
import "core:os"
import "core:path/filepath"
import "core:slice"
import "core:strings"

// TODO: this is an absolute mess of AI slop, clean it up. Use a simple symbol
// table to collect all variable, class and function declarations, and pass
// that onto the compiler

/* ResolutionKind indicates how a variable reference is resolved:
   LOCAL: a local variable in the current or an enclosing scope of the
   current function. 
   UPVALUE: captured from an enclosing function's scope.
   GLOBAL: a top-level variable in the program's global table. */
ResolutionKind :: enum {
	LOCAL,
	UPVALUE,
	GLOBAL,
}

/* ResolutionInfo is the per-expression entry stored in the resolution map
that the codegen reads during bytecode emission. For LOCAL and GLOBAL
variables, index is the slot/constant index. For UPVALUE variables, index
is the upvalue index in the current function's upvalue table, and
upv_slot / upv_is_local are the parameters that codegen passes to its own
add_upvalue call to create the upvalue entry in the runtime compiler. */
ResolutionInfo :: struct {
	kind:         ResolutionKind,
	index:        int, // local slot, upvalue index, or -1 for globals
	is_final:     bool,
	name:         string,
	upv_slot:     int,
	upv_is_local: bool,
}

/* An entry in the current semantic compiler's local variable array.
Mirrors the `Compiler.local` struct in compiler.odin but lives only
for the duration of the analysis pass. */
SemanticLocal :: struct {
	name:        Token,
	depth:       int,
	is_final:    bool,
	is_captured: bool,
}

/* An entry in the current semantic compiler's upvalue array.
Each upvalue represents a variable captured from an enclosing scope. */
SemanticUpvalue :: struct {
	index:    int,
	is_local: bool,
}

/* SemanticCompiler mirrors the Compiler struct from compiler.odin, tracking
the lexical scope stack, locals, and upvalues for one function scope during
the analysis pass. The name resolution results are ultimately stored in
the resolution map, but intermediate per-function state (locals, upvalues)
is maintained here in parallel with codegen's Compiler. */
SemanticCompiler :: struct {
	enclosing:     ^SemanticCompiler,
	func_type:     FunctionType,
	loop_depth:    int,
	scope_depth:   int,
	local_count:   int,
	locals:        [U8_COUNT]SemanticLocal,
	upvalue_count: int,
	upvalues:      [U8_COUNT]SemanticUpvalue,
}

/* Main state for the semantic analysis pass. Holds the current scope,
class context, pipeline state, and the resolution map being populated.
One Semantic instance is created per call to `analyze` and lives until
the caller (codegen) finishes consuming the resolution map. */
Semantic :: struct {
	current_compiler: ^SemanticCompiler,
	current_class:    ^ClassCompiler,
	current_token:    Token,
	had_error:        bool,
	pipeline_active:  bool,
	resolution:       map[uintptr]ResolutionInfo,
	gc:               ^GC,
	globals:          ^Table,
}

/* Initialise a fresh SemanticCompiler for a new function scope, set its
enclosing pointer to the current compiler, and place a synthetic local
at slot 0 ("this" for methods/initializers) to match the codegen's
slot layout. */
init_semantic_compiler :: proc(sm: ^Semantic, c: ^SemanticCompiler, type: FunctionType) {
	c^ = SemanticCompiler {
		scope_depth   = 0,
		loop_depth    = 0,
		local_count   = 0,
		upvalue_count = 0,
		enclosing     = sm.current_compiler,
		func_type     = type,
	}

	local := &c.locals[c.local_count]
	c.local_count += 1
	local.depth = 0
	local.is_captured = false

	if type == .METHOD || type == .INITIALIZER {
		local.name.lexeme = "this"
	} else {
		local.name.lexeme = ""
	}

	sm.current_compiler = c
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
		resolution = make(map[uintptr]ResolutionInfo),
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
	curr := sm.current_compiler
	curr.scope_depth -= 1

	for curr.local_count > 0 && curr.locals[curr.local_count - 1].depth > curr.scope_depth {
		curr.local_count -= 1
	}
}

// === VARIABLE DECLARATION ===

/* 
Declare a name binding.
Errors if the variable of that name already exists in the scope.
*/
@(private = "file")
@(require_results)
declare_variable :: proc(sm: ^Semantic, name: Token, is_final: bool) -> bool {
	comp := sm.current_compiler

	if comp.scope_depth == 0 {
		// Global scope.
		// NOTE: Duplicate declarations at global scope are currently allowed for
		// non-final variables. To disallow them, check table_get(sm.globals, ...)
		// for any existing binding and error with
		// "A variable with name '...' already exists in this scope."
		global_o_str := copy_string(sm.gc, name.lexeme)

		if value, ok := table_get(sm.globals, global_o_str); ok && !is_nil(value) {
			if values_equal(value, bool_val(true)) {
				msg := "Cannot redefine a final variable as normal variable."
				if is_final {
					msg = "Cannot redefine a final variable."
				}
				semantic_error(sm, msg)
				return false
			} else if is_final {
				semantic_error(sm, "Cannot redefine a variable as final variable.")
				return false
			}
		} else {
			table_set(sm.globals, global_o_str, bool_val(is_final))
		}

		return true
	}

	// Local scope: check for duplicates
	for i := comp.local_count - 1; i >= 0; i -= 1 {
		local := &comp.locals[i]
		if local.depth != -1 && local.depth < comp.scope_depth {
			break
		}

		if identifiers_equal(name, local.name) {
			semantic_error(sm, "A variable with this name in this scope already exists.")
			return false
		}
	}

	if comp.local_count == U8_COUNT {
		semantic_error(sm, "Too many local variables in function.")
		return false
	}

	local := &comp.locals[comp.local_count]
	comp.local_count += 1
	local.name = name
	local.depth = -1
	local.is_final = is_final
	local.is_captured = false
	return true
}

/* 
Add a local name binding.
Errors if there are too many local variables in the scope already.
*/
@(private = "file")
@(require_results)
add_local :: proc(sm: ^Semantic, name: Token, is_final: bool) -> bool {
	comp := sm.current_compiler

	if comp.local_count == U8_COUNT {
		semantic_error(sm, "Too many local variables in function.")
		return false
	}

	local := &comp.locals[comp.local_count]
	comp.local_count += 1
	local.name = name
	local.depth = -1
	local.is_final = is_final
	local.is_captured = false
	return true
}

@(private = "file")
mark_initialized :: proc(sm: ^Semantic) {
	if sm.current_compiler.scope_depth == 0 {return}
	sm.current_compiler.locals[sm.current_compiler.local_count - 1].depth =
		sm.current_compiler.scope_depth
}

// === LOCAL RESOLUTION ===

/* Resolve a local name binding from the SemanticCompiler struct. */
@(private = "file")
@(require_results)
resolve_local :: proc(
	sm: ^Semantic,
	compiler: ^SemanticCompiler,
	name: Token,
) -> (
	int,
	ErrorMessage,
) {
	for i := compiler.local_count - 1; i >= 0; i -= 1 {
		local := &compiler.locals[i]
		if identifiers_equal(name, local.name) {
			if local.depth == -1 {
				return -1, "Cannot read local variable in its own initializer."
			}
			return i, nil
		}
	}

	return -1, nil
}

// === UPVALUE MANAGEMENT ===

/* Add an upvalue to the function or return it if it already exists. */
@(private = "file")
@(require_results)
add_upvalue :: proc(
	sm: ^Semantic,
	compiler: ^SemanticCompiler,
	index: int,
	is_local: bool,
) -> (
	int,
	ErrorMessage,
) {
	upvalue_count := compiler.upvalue_count

	for i in 0 ..< upvalue_count {
		upvalue := &compiler.upvalues[i]
		if upvalue.index == index && upvalue.is_local == is_local {
			return i, nil
		}
	}

	if upvalue_count == U8_COUNT {
		return 0, "Too many closure variables in function."
	}

	compiler.upvalues[upvalue_count].is_local = is_local
	compiler.upvalues[upvalue_count].index = index
	defer compiler.upvalue_count += 1
	return compiler.upvalue_count, nil
}

/*
Find an upvalue in the function's local scope and scopes above it, and return
the index to its name in the constant table. Also return whether the upvalue was
initially declared with `val` or `var`.
*/
@(private = "file")
@(require_results)
resolve_upvalue :: proc(
	sm: ^Semantic,
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
	local := resolve_local(sm, compiler.enclosing, name) or_return
	if local != -1 {
		// Mark the local as captured and see if its a `var` or `val`.
		compiler.enclosing.locals[local].is_captured = true
		final := compiler.enclosing.locals[local].is_final
		/* is_local is true since we're capturing a local variable from the
		immediately enclosing function. */
		idx, err := add_upvalue(sm, compiler, local, is_local = true)
		return idx, final, err
	}

	/* Recursively look for an upvalue in the enclosing function. */
	upvalue, final := resolve_upvalue(sm, compiler.enclosing, name) or_return
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
		idx, err := add_upvalue(sm, compiler, upvalue, is_local = false)
		return idx, final, err
	}

	// Nope, didn't find anything.
	return -1, false, nil
}

// === FORWARD REFERENCE COLLECTION ===

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
		// Resolve the assigned variable
		resolved := try2(sm, resolve_variable(sm, e.name)) or_return
		if resolved.is_final {
			semantic_error(sm, "Can only set a final variable once.")
			return false
		}

		_analyze(sm, e.value) or_return
		sm.resolution[uintptr(e)] = resolved
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
		sm.current_token = e.token

		// Register the class name as a variable, then immediately mark it
		// initialized so methods in the body can reference the class name.
		declare_variable(sm, e.name, is_final = false) or_return
		mark_initialized(sm)

		class_compiler: ClassCompiler
		class_compiler.has_superclass = false
		class_compiler.enclosing = sm.current_class
		sm.current_class = &class_compiler

		if e.superclass != nil {
			class_compiler.has_superclass = true
		}

		for method in e.methods {
			_analyze(sm, method) or_return
		}

		sm.current_class = sm.current_class.enclosing
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

		begin_semantic_scope(sm)

		// Declare the loop variable as final
		declare_variable(sm, e.var_name, is_final = true) or_return
		mark_initialized(sm)

		_analyze(sm, e.iterable) or_return

		// Add hidden __iter variable (not accessible by user code)
		add_local(sm, synthetic_token("__iter"), is_final = true) or_return
		mark_initialized(sm)

		// Add hidden __idx variable
		add_local(sm, synthetic_token("__idx"), is_final = true) or_return
		mark_initialized(sm)

		loop_depth := 0
		if sm.current_compiler != nil {
			loop_depth = sm.current_compiler.loop_depth
			sm.current_compiler.loop_depth += 1
		}

		_analyze(sm, e.body) or_return

		if sm.current_compiler != nil {
			sm.current_compiler.loop_depth = loop_depth
		}

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
		sm.current_token = e.token

		if len(e.params) > U8_MAX {
			semantic_error(sm, "Cannot have more than 255 parameters.")
			return false
		}

		c: SemanticCompiler
		if sm.current_class != nil &&
		   sm.current_compiler.scope_depth == 0 &&
		   e.bound_to != nil &&
		   e.bound_to.?.lexeme == "init" {
			init_semantic_compiler(sm, &c, .INITIALIZER)
		} else {
			init_semantic_compiler(sm, &c, .LAMBDA)
		}
		begin_semantic_scope(sm)

		for param in e.params {
			declare_variable(sm, param.token, is_final = false) or_return
			mark_initialized(sm)
		}

		_analyze(sm, e.body) or_return

		end_semantic_compiler(sm)
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

		// If the receiver is a VariableExpr, check that it's not a final binding
		if r, ok := e.receiver.(^VariableExpr); ok {
			sm.current_token = r.token
			resolved := try2(sm, resolve_variable(sm, r.name)) or_return
			if resolved.is_final {
				semantic_error(sm, "Can only set a final variable once.")
				return false
			}
		}

		_analyze(sm, e.receiver) or_return
		_analyze(sm, e.value) or_return
	case ^SubscriptExpr:
		sm.current_token = e.token
		_analyze(sm, e.receiver) or_return
		_analyze(sm, e.index) or_return
	case ^SubscriptSetExpr:
		sm.current_token = e.token
		_analyze(sm, e.receiver) or_return
		if r, ok := e.receiver.(^VariableExpr); ok {
			sm.current_token = r.token
			resolved := try2(sm, resolve_variable(sm, r.name)) or_return
			if resolved.is_final {
				semantic_error(sm, "Can only set a final variable once.")
				return false
			}
		}

		_analyze(sm, e.index) or_return
		_analyze(sm, e.value) or_return
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

		// Pre-register the module name in the globals table so it can be
		// resolved by subsequent variable references.
		path_str := e.path.lexeme
		path := strings.trim(path_str[1:len(path_str) - 1], " ")

		builtin_found := slice.contains(sm.gc.std_modules[:], path)

		mod_name: string
		if builtin_found {
			mod_name = path
		} else {
			abs_path, err := filepath.join([]string{config.__dirname, path}, context.allocator)
			if err != nil {
				semantic_error(
					sm,
					fmt.tprintf("Error when declaring module: %s", os.error_string(err)),
				)
				return false
			}
			defer delete(abs_path)

			if !os.exists(abs_path) {
				semantic_error(sm, fmt.tprintf("Module '%s' not found.", abs_path))
				return false
			}
			mod_name = filepath.short_stem(path)
		}

		declare_variable(sm, synthetic_token(mod_name), is_final = true) or_return
		mark_initialized(sm)
	case ^VariableExpr:
		sm.current_token = e.token
		resolved := try2(sm, resolve_variable(sm, e.name)) or_return
		sm.resolution[uintptr(e)] = resolved
	case ^VarDeclExpr:
		sm.current_token = e.token

		for binding in e.bindings {
			sm.current_token = binding.name
			if binding.initializer == nil {
				if e.is_final {
					semantic_error(sm, "Final variables must be initialized.")
					return false
				}
			}

			declare_variable(sm, binding.name, e.is_final) or_return

			if binding.initializer != nil {
				// Allow anonymous functions to recurse by referring to the name
				// they've been bound to.
				if _, ok := binding.initializer.(^FunctionExpr); ok {
					mark_initialized(sm)
				}

				_analyze(sm, binding.initializer) or_return

				if _, ok := binding.initializer.(^FunctionExpr); !ok {
					mark_initialized(sm)
				}
				// NOTE: Use-before-initialization at global scope is currently
				// not checked. To add it, defer the analysis of initializers
				// that reference their own binding and error with
				// "Cannot read local variable in its own initializer."
			}
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

/* 
Resolve a variable by walking the scope chain and checking the globals table.
Returns ResolutionInfo or an error message.
The resolution order is: local -> upvalue -> global.
For upvalues, the upv_slot/upv_is_local fields store the parameters that
would be passed to add_upvalue in the codegen phase.
*/
@(private = "file")
@(require_results)
resolve_variable :: proc(sm: ^Semantic, name: Token) -> (info: ResolutionInfo, err: ErrorMessage) {
	comp := sm.current_compiler

	// Check locals of the current function
	local, resolve_err := resolve_local(sm, comp, name)
	if resolve_err != nil {
		return {}, resolve_err
	}
	if local != -1 {
		return ResolutionInfo {
				kind = .LOCAL,
				index = local,
				is_final = comp.locals[local].is_final,
				name = name.lexeme,
			},
			nil
	}

	// Check upvalues (walks enclosing functions recursively)
	// After this call, comp.upvalues will have the newly added upvalue
	// with its .index and .is_local fields set correctly.
	upvalue, final, up_err := resolve_upvalue(sm, comp, name)
	if up_err != nil {
		return {}, up_err
	}
	if upvalue != -1 {
		return ResolutionInfo {
				kind = .UPVALUE,
				index = upvalue,
				is_final = final,
				name = name.lexeme,
				upv_slot = comp.upvalues[upvalue].index,
				upv_is_local = comp.upvalues[upvalue].is_local,
			},
			nil
	}

	// Not found in local scopes or upvalues. Check globals.
	global_o_str := copy_string(sm.gc, name.lexeme)
	if _, exists := table_get(sm.globals, global_o_str); exists {
		is_final := false
		if v, ok := table_get(sm.globals, global_o_str); ok {
			is_final = !is_nil(v) && values_equal(v, bool_val(true))
		}

		return ResolutionInfo{kind = .GLOBAL, index = -1, is_final = is_final, name = name.lexeme},
			nil
	}

	return {}, fmt.tprintf("Undefined variable '%s'.", name.lexeme)
}

// Two-pass semantic analyzer
@(require_results)
analyze :: proc(
	gc: ^GC,
	expr: Expr,
	globals: ^Table,
) -> (
	resolution: map[uintptr]ResolutionInfo,
	success: bool,
) {
	if expr == nil {
		return nil, true
	}

	// Add native function names to the globals table
	for fn_name in gc.global_native_fns {
		table_set(globals, copy_string(gc, fn_name), bool_val(true))
	}

	sm := init_semantic(gc, globals)

	// analogous to Codegen's `Compiler`
	script_compiler: SemanticCompiler
	init_semantic_compiler(&sm, &script_compiler, .SCRIPT)
	// Don't end the compiler as it exists for the entire script scope.

	// Pass 1: Collect forward references for top-level function declarations
	// This enables mutual recursion between functions.
	collect_forward_refs(&sm, expr)

	// Pass 2: Full semantic analysis
	ok := _analyze(&sm, expr)
	if !ok {
		if sm.resolution != nil {delete(sm.resolution)}
		return nil, false
	}
	return sm.resolution, true
}
