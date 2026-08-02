package zen

import "core:fmt"
import "core:strings"

// The state of the resolver.
Resolver :: struct #all_or_none {
	file_scopes:         map[string]map[string]^Symbol, // global scopes for each file
	current_local_scope: ^Scope, // chain of local scopes of a file
	builtin_scope:       map[string]^Symbol, // topmost scope; common to all files
	resolution_map:      ResolutionMap,
	current_module:      ^Module,
	current_token:       Token,
}

// Return the global scope of the file that the resolver is currently resolving.
current_file_scope :: #force_inline proc(rs: ^Resolver) -> ^map[string]^Symbol {
	return &rs.file_scopes[rs.current_module.name]
}

// A local scope of bindings
Scope :: struct #all_or_none {
	enclosing:          ^Scope,
	kind:               ScopeKind,
	variables:          map[string]^Symbol,
	current_local_slot: int,
	scope_depth:        int,
}

// Kind of a scope; can be a function scope or local scope. This affects the
// setting of the `is_captured` flag on `Symbol`s.
ScopeKind :: enum {
	FUNCTION,
	BLOCK,
}

// Representation of a variable.
Symbol :: struct #all_or_none {
	name:             string,
	kind:             SymbolKind,
	is_final:         bool,
	is_loop_variable: bool, // not used here but important for codegen
	is_captured:      bool,
	is_module:        bool,
	is_native_value:  bool, // an Odin-implemented native (stdlib) function/value
	is_public:        bool,
	initialized:      bool,
	scope_depth:      int,
	local_index:      int,
}

// Kind of a variable (local or global).
SymbolKind :: enum {
	LOCAL,
	GLOBAL,
}

// Return a pointer to a heap-allocated symbol with the given params.
// The symbol is set to uncaptured and unshadowed by default, with the current
// scope depth and of the resolver and an appropriate local index. The symbol
// is kept uninitialized by default unless it is a module or native value.
symb :: #force_inline proc(
	rs: ^Resolver,
	name: string,
	is_final: bool,
	is_public: bool,
	is_loop_variable: bool = false,
	is_module: bool = false,
	is_native_value: bool = false,
) -> ^Symbol {
	s := new(Symbol)
	s^ = Symbol {
		name             = name,
		kind             = .GLOBAL if in_file_scope(rs) else .LOCAL,
		is_final         = is_final,
		is_loop_variable = is_loop_variable,
		is_captured      = false,
		is_module        = is_module,
		is_native_value  = is_native_value,
		is_public        = is_public,
		initialized      = true if (is_native_value || is_module) else false,
		scope_depth      = 0 if in_file_scope(rs) else rs.current_local_scope.scope_depth,
		local_index      = 0 if in_file_scope(rs) else rs.current_local_scope.current_local_slot,
	}
	return s
}

// Expressions that can refer to a name.
ResolvingExpr :: union #no_nil {
	^AssignExpr,
	^VariableExpr,
	^ModuleAccessExpr,
}

// A map mapping `ResolvingExpr`s to the `Symbol`s they resolve to. It is the
// output of the resolver.
ResolutionMap :: map[ResolvingExpr]^Symbol

// Enter a scope of the given `kind`.
enter_scope :: proc(rs: ^Resolver, kind: ScopeKind) {
	enclosing := rs.current_local_scope
	starting_local_slot: int
	switch kind {
	case .FUNCTION:
		// for functions, start at 1 as each function starts a new callframe
		// its 1 cuz the first slot `0` is the function itself
		starting_local_slot = 1
	case .BLOCK:
		starting_local_slot = 1 if enclosing == nil else enclosing.current_local_slot + 1
	case:
		fmt.panicf("invalid ScopeKind %v", kind)
	}

	fs := new(Scope)
	fs^ = {
		enclosing          = enclosing,
		current_local_slot = starting_local_slot,
		variables          = make(map[string]^Symbol),
		scope_depth        = 0 if enclosing == nil else enclosing.scope_depth + 1,
		kind               = kind,
	}

	rs.current_local_scope = fs
}

// Exit the current local scope.
exit_scope :: proc(rs: ^Resolver) {
	assert(rs.current_local_scope != nil)
	rs.current_local_scope = rs.current_local_scope.enclosing
}

// Are we in the global scope within a module?
@(require_results)
in_file_scope :: proc(rs: ^Resolver) -> bool {
	return rs.current_local_scope.enclosing == nil
}

// Resolve a variable in the entire scope chain. Mark it captured if the variable
// is found outside of the current function scope.
@(require_results)
resolve_local :: proc(fs: ^Scope, name: string, is_upvalue: bool = false) -> (^Symbol, bool) {
	assert(fs != nil)
	if var, ok := fs.variables[name]; ok {
		if is_upvalue {
			var.is_captured = true
		}

		return var, true
	}

	// reached the end of scope chain
	if fs.enclosing == nil {
		return nil, false
	}

	is_upvalue := is_upvalue
	if fs.kind == .FUNCTION {
		// we are past the lowest function scope
		is_upvalue = true
	}

	return resolve_local(fs.enclosing, name, is_upvalue)
}

// Looks up a variable through all the scopes one-by-one: local scope, enclosing
// function scopes, current file's global scope, and finally the builtin value
// scope. Returns the variable if it was found in any of these scopes, else
// it returns `nil` without an error.
// If the variable is uninitialized and the caller has disallowed that by keeping
// the `allow_uninitialized` parameter false, the function returns with the
// relevant error.
@(require_results)
resolve_variable :: proc(
	rs: ^Resolver,
	name: string,
	allow_uninitialized: bool = false,
) -> (
	^Symbol,
	ErrorMessage,
) {
	var: ^Symbol

	if local, local_ok := resolve_local(rs.current_local_scope, name); local_ok {
		var = local
	} else if global, global_ok := current_file_scope(rs)[name]; global_ok {
		var = global
	} else if builtin, builtin_ok := rs.builtin_scope[name]; builtin_ok {
		var = builtin
	} else {
		return nil, nil
	}

	if !allow_uninitialized && !var.initialized {
		return nil, fmt.tprintf("Cannot use uninitialized variable '%v'.", var.name)
	}

	return var, nil
}

// Resolve a variable in a module, erroring out if it is uninitialized (unless
// `allow_uninitialized` is set).
@(require_results)
resolve_variable_in_module :: proc(
	rs: ^Resolver,
	module_name: string,
	variable_name: string,
	allow_uninitialized: bool = false,
) -> (
	^Symbol,
	ErrorMessage,
) {
	file_scope := rs.file_scopes[module_name]

	// look in the file scope
	global, global_ok := file_scope[variable_name]
	if !global_ok {
		return nil, nil
	}

	if !allow_uninitialized && !global.initialized {
		return nil, fmt.tprintf("Cannot use uninitialized module import '%v'.", global.name)
	}

	return global, nil
}

// Attempt to resolve a variable accessed via module access, erroring out if
// it doesn't exist or some other error occured.
@(require_results)
try_resolve_module_variable :: proc(
	rs: ^Resolver,
	module_name: string,
	variable_name: string,
	allow_uninitialized: bool = false,
) -> (
	s: ^Symbol,
	err: ErrorMessage,
) {
	var := resolve_variable_in_module(
		rs,
		module_name,
		variable_name,
		allow_uninitialized,
	) or_return
	if var == nil {
		return nil, fmt.tprintf(
			"Variable '%v' does not exist in module '%v'.",
			variable_name,
			module_name,
		)
	}

	return var, nil
}

// Attempt to resolve a variable, erroring out if it doesn't exist or some
// other error occured.
@(require_results)
try_resolve_variable :: proc(
	rs: ^Resolver,
	name: string,
	allow_uninitialized: bool = false,
) -> (
	s: ^Symbol,
	err: ErrorMessage,
) {
	var := resolve_variable(rs, name, allow_uninitialized) or_return
	if var == nil {
		return nil, fmt.tprintf("Undefined variable '%v'.", name)
	}

	return var, nil
}

// Declare and define a module in the current scope.
@(require_results)
declare_and_define_module :: proc(rs: ^Resolver, name: string, type: ModuleType) -> ErrorMessage {
	if in_file_scope(rs) {
		_, exists := current_file_scope(rs)[name]
		if exists {
			return nil
		}
		new_var := symb(
			rs,
			name,
			is_final = true,
			is_public = false,
			is_module = true,
			is_native_value = true if type == .BUILTIN else false,
		)
		current_file_scope(rs)[name] = new_var
		return nil
	}

	var, exists := rs.current_local_scope.variables[name]
	if exists && var.scope_depth == rs.current_local_scope.scope_depth {
		return "A variable with this name in this scope already exists."
	}

	new_var := symb(
		rs,
		name,
		is_final = true,
		is_public = false,
		is_module = true,
		is_native_value = true if type == .BUILTIN else false,
	)
	rs.current_local_scope.current_local_slot += 1

	// put it in the scope
	rs.current_local_scope.variables[name] = new_var

	return nil
}

// Declare the existence of a variable in the current scope.
@(require_results)
declare_variable :: proc(
	rs: ^Resolver,
	name: string,
	is_final: bool,
	is_public: bool,
	is_loop_variable: bool = false,
) -> ErrorMessage {
	if in_file_scope(rs) {
		var, exists := current_file_scope(rs)[name]
		if exists {
			if !var.initialized {
				return nil
			}

			if var.is_final {
				return(
					is_final ? "Cannot redefine a final variable." : "Cannot redefine a final variable as normal variable." \
				)
			} else if is_final {
				return "Cannot redefine a variable as final variable."
			}
		}

		new_var := symb(rs, name, is_final, is_public, is_loop_variable)
		current_file_scope(rs)[name] = new_var
		return nil
	}

	var, exists := rs.current_local_scope.variables[name]
	if exists && var.scope_depth == rs.current_local_scope.scope_depth {
		return "A variable with this name in this scope already exists."
	}

	new_var := symb(rs, name, is_final, is_public, is_loop_variable)
	rs.current_local_scope.current_local_slot += 1

	// put it in the scope
	rs.current_local_scope.variables[name] = new_var

	return nil
}

// Mark a variable as defined.
define_variable :: proc(rs: ^Resolver, name: string) {
	if in_file_scope(rs) {
		var, ok := current_file_scope(rs)[name]
		if !ok {
			fmt.panicf("no global variable with name %v exists", name)
		}
		var.initialized = true
		return
	}

	var, ok := rs.current_local_scope.variables[name]
	if !ok {
		fmt.panicf("no variable with name %v exists in the function scope", name)
	}
	var.initialized = true
}

// Resolve the module import and accessed value in a `ModuleAccessExpr`.
resolve_module_access_expr :: proc(rs: ^Resolver, e: ^ModuleAccessExpr) -> bool {
	if var_e, ok := e.receiver.(^VariableExpr); ok {
		receiver_var := try2(rs, try_resolve_variable(rs, var_e.token.lexeme)) or_return
		if !receiver_var.is_module {
			resolver_error(
				rs,
				fmt.tprintf(
					"`%v` operator cannot be used on '%v' as it is not a module.",
					e.token.lexeme,
					receiver_var.name,
				),
			)
			return false
		}

		resolved: ^Symbol
		if receiver_var.is_native_value {
			mod, mod_ok := as_builtin_module(receiver_var.name)
			if !mod_ok {
				fmt.panicf("expected '%v' to be a builtin module but it wasn't", receiver_var.name)
			}

			found := function_exists_in_builtin_module(mod, e.property.lexeme)
			if !found {
				resolver_error(
					rs,
					fmt.tprintf(
						"Variable '%v' does not exist in module '%v'.",
						e.property.lexeme,
						receiver_var.name,
					),
				)
				return false
			}

			resolved = symb(
				rs,
				e.property.lexeme,
				is_final = true,
				is_public = false,
				is_native_value = true,
			)
		} else {
			var := try2(
				rs,
				try_resolve_module_variable(rs, var_e.token.lexeme, e.property.lexeme),
			) or_return

			if !var.is_public {
				resolver_error(
					rs,
					fmt.tprintf(
						"Cannot use private variable '%v' of module '%v' outside it.",
						e.property.lexeme,
						var_e.token.lexeme,
					),
					fmt.tprintf("Try marking '%v' as `pub`.", e.property.lexeme),
				)
				return false
			}

			resolved = var
		}

		rs.resolution_map[e] = resolved
		return true
	} else {
		resolver_error(
			rs,
			fmt.tprintf("`%v` operator can only be used on a module.", e.token.lexeme),
		)
		return false
	}
}

// Resolve the names within an arbitrary `Expr`.
@(require_results)
resolve_with_resolver :: proc(rs: ^Resolver, expr: Expr) -> bool {
	if expr == nil {return true}

	switch e in expr {
	case ^AssignExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.value) or_return
		var := try2(
			rs,
			try_resolve_variable(rs, e.name.lexeme, allow_uninitialized = true),
		) or_return

		if var.is_module {
			resolver_error(rs, fmt.tprintf("Cannot reassign module '%v'.", var.name))
			return false
		}

		if var.is_final {
			if var.is_native_value {
				resolver_error(rs, fmt.tprintf("Cannot reassign native value '%v'.", var.name))
			} else {
				resolver_error(rs, "Can only set a final variable once.")
			}
			return false
		}

		var.initialized = true

		rs.resolution_map[e] = var
	case ^BinaryExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.left) or_return
		resolve_with_resolver(rs, e.right) or_return
	case ^BlockExpr:
		rs.current_token = e.token
		enter_scope(rs, .BLOCK)
		defer exit_scope(rs)
		resolve_with_resolver(rs, e.expression) or_return
	case ^BreakExpr:
		rs.current_token = e.token
	case ^CallExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.callee) or_return
		for arg in e.arguments {
			resolve_with_resolver(rs, arg) or_return
		}
	case ^ContinueExpr:
		rs.current_token = e.token
	case ^ExitExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.code) or_return
	case ^ModuleAccessExpr:
		rs.current_token = e.token
		resolve_module_access_expr(rs, e) or_return
	case ^GroupingExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.expression) or_return
	case ^FunctionExpr:
		rs.current_token = e.token
		enter_scope(rs, .FUNCTION)
		defer exit_scope(rs)
		for param in e.params {
			try(
				rs,
				declare_variable(rs, param.name.lexeme, is_final = false, is_public = false),
			) or_return
			define_variable(rs, param.name.lexeme)
		}
		resolve_with_resolver(rs, e.body) or_return
	case ^ForExpr:
		rs.current_token = e.token
		enter_scope(rs, .BLOCK)
		defer exit_scope(rs)
		resolve_with_resolver(rs, e.initializer) or_return
		resolve_with_resolver(rs, e.condition) or_return
		resolve_with_resolver(rs, e.increment) or_return
		resolve_with_resolver(rs, e.body) or_return
	case ^ForInExpr:
		rs.current_token = e.token
		enter_scope(rs, .BLOCK)
		defer exit_scope(rs)
		try(
			rs,
			declare_variable(
				rs,
				e.var_name.lexeme,
				is_final = true,
				is_public = false,
				is_loop_variable = true,
			),
		) or_return
		define_variable(rs, e.var_name.lexeme)
		resolve_with_resolver(rs, e.iterable) or_return
		resolve_with_resolver(rs, e.body) or_return
	case ^IfExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.condition) or_return
		resolve_with_resolver(rs, e.then_branch) or_return
		resolve_with_resolver(rs, e.else_branch) or_return
	case ^ItExpr:
		rs.current_token = e.token
	case ^ListExpr:
		rs.current_token = e.token
		for element in e.elements {
			resolve_with_resolver(rs, element) or_return
		}
	case ^LiteralExpr:
		rs.current_token = e.token
	case ^LogicalExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.left) or_return
		resolve_with_resolver(rs, e.right) or_return
	case ^PipeExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.left) or_return
		resolve_with_resolver(rs, e.right) or_return
	case ^EchoExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.expr) or_return
	case ^ReturnExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.value) or_return
	case ^SubscriptExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.receiver) or_return
		resolve_with_resolver(rs, e.index) or_return
	case ^SubscriptSetExpr:
		rs.current_token = e.token

		if varexpr, ok := e.receiver.(^VariableExpr); ok {
			var := try2(rs, try_resolve_variable(rs, varexpr.name.lexeme)) or_return

			if var.is_module {
				resolver_error(rs, fmt.tprintf("Cannot reassign module '%v'.", var.name))
				return false
			}

			if var.is_final {
				if var.is_native_value {
					resolver_error(rs, fmt.tprintf("Cannot reassign native value '%v'.", var.name))
				} else {
					resolver_error(rs, "Can only set a final variable once.")
				}
				return false
			}
		} else {
			resolve_with_resolver(rs, e.receiver) or_return
		}
		resolve_with_resolver(rs, e.index) or_return
		resolve_with_resolver(rs, e.value) or_return
	case ^SequenceExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.left) or_return
		resolve_with_resolver(rs, e.right) or_return
	case ^SwitchExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.condition) or_return
		for c in e.cases {
			resolve_with_resolver(rs, c.condition) or_return
			resolve_with_resolver(rs, c.body) or_return
		}
		resolve_with_resolver(rs, e.else_branch) or_return
	case ^UnaryExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.right) or_return
	case ^UseExpr:
		rs.current_token = e.token
		name := e.name
		type := e.type
		try(rs, declare_and_define_module(rs, name, type)) or_return
	case ^VariableExpr:
		rs.current_token = e.token
		var := try2(rs, try_resolve_variable(rs, e.name.lexeme)) or_return
		if var.is_module {
			resolver_error(rs, "Cannot use a module as a value.")
			return false
		}

		rs.resolution_map[e] = var
	case ^VarDeclExpr:
		rs.current_token = e.token
		is_final := e.is_final
		bindings := e.bindings
		is_public := e.is_public

		for binding in bindings {
			if _, ok := binding.initializer.?; !ok {
				// if no initializer, just declare the variable and move on
				try(rs, declare_variable(rs, binding.name.lexeme, is_final, is_public)) or_return
				continue
			}

			init := binding.initializer.?
			_, is_fn := init.(^FunctionExpr)

			// if not fn, just declare, resolve, define and move on
			if !is_fn {
				try(rs, declare_variable(rs, binding.name.lexeme, is_final, is_public)) or_return
				resolve_with_resolver(rs, init) or_return
				define_variable(rs, binding.name.lexeme)
				continue
			}

			// if it is fn, check if in global scope
			// if in global scope the thing was pre-declared and defined, just
			// resolve the body. Else (fn in local scope), declare, define (for
			// recursion) then resolve body
			if in_file_scope(rs) {
				resolve_with_resolver(rs, init) or_return
			} else {
				try(rs, declare_variable(rs, binding.name.lexeme, is_final, is_public)) or_return
				define_variable(rs, binding.name.lexeme)
				resolve_with_resolver(rs, init) or_return
			}
		}
	case ^WhileExpr:
		rs.current_token = e.token
		enter_scope(rs, .BLOCK)
		defer exit_scope(rs)
		resolve_with_resolver(rs, e.condition) or_return
		resolve_with_resolver(rs, e.body) or_return
	}

	return true
}

// Inject the native function symbols into a provided map of symbols.
inject_builtin_functions :: #force_inline proc(m: ^map[string]^Symbol) {
	#unroll for fn in GLOBAL_BUILTIN_FUNCTIONS {
		native_var := new(Symbol)
		native_var^ = {
			name             = fn.name,
			kind             = .GLOBAL,
			is_final         = true,
			is_loop_variable = false,
			is_captured      = false,
			is_module        = false,
			is_native_value  = true,
			is_public        = true,
			initialized      = true,
			scope_depth      = 0,
			local_index      = 0,
		}
		m[fn.name] = native_var
	}
}

// Small pre-pass before the resolver, used to hoist global function declarations.
// Useful for allowing freely reorderable code as well as mutual recursion.
@(require_results)
collect_forward_references :: proc(rs: ^Resolver, expr: Expr) -> bool {
	if expr == nil {return true}

	#partial switch e in expr {
	case ^VarDeclExpr:
		rs.current_token = e.token
		is_final := e.is_final
		is_public := e.is_public

		for binding in e.bindings {
			// only hoist functions
			init := binding.initializer.? or_continue
			_ = init.(^FunctionExpr) or_continue

			// ONLY declare the variable; it is defined in the main resolver pass
			try(rs, declare_variable(rs, binding.name.lexeme, is_final, is_public)) or_return
			define_variable(rs, binding.name.lexeme)
		}
	case ^SequenceExpr:
		rs.current_token = e.token
		collect_forward_references(rs, e.left) or_return
		collect_forward_references(rs, e.right) or_return
	// other cases don't matter
	}

	return true
}

resolver_error :: proc(rs: ^Resolver, message: string, details: string = "") {
	token := rs.current_token
	print_error(token, message, file = rs.current_module.fullpath, details = details)
}

// Takes in the module graph, resolves all variables.
// Also returns whether the operation succeeded, while printing out the error
// messages as 'resolution errors' in the process.
// If `repl_scope` is provided, the global scope of the root module is cloned
// from and written back to it, so that the REPL can keep its declarations
// alive from one line to the next.
@(require_results)
resolve :: proc(
	graph: []^Module,
	repl_scope: ^map[string]^Symbol = nil,
) -> (resolutions: ResolutionMap, success: bool) {
	rs := Resolver {
		resolution_map      = make(ResolutionMap),
		file_scopes         = make(map[string]map[string]^Symbol),
		current_local_scope = nil,
		builtin_scope       = make(map[string]^Symbol),
		current_module      = nil,
		current_token       = {},
	}
	inject_builtin_functions(&rs.builtin_scope)

	for module in graph {
		if _, exists := rs.file_scopes[module.name]; exists {
			resolver_error(&rs, fmt.tprintf("Module '%v' is already defined.", module.name))
			return {}, false
		}

		/* The root module is the one whose fullpath is the path of the
		program currently being interpreted. In the REPL, it resolves against
		a private copy of the global scope accumulated over the previous
		lines, which is handed back to the VM once the line resolves; see
		`persist_repl_scope` below and `interpret` in vm.odin. */
		globals: map[string]^Symbol
		if module.fullpath == zen_get_path() && repl_scope != nil {
			globals = copy_repl_scope(repl_scope^)
		} else {
			globals = make(map[string]^Symbol)
		}
		rs.file_scopes[module.name] = globals

		rs.current_module = module
		enter_scope(&rs, .FUNCTION)
		defer exit_scope(&rs)

		collect_forward_references(&rs, module.ast) or_return
		resolve_with_resolver(&rs, module.ast) or_return

		/* Hand the resolved scope back to the VM. Maps are copy-on-write, so
		the changes made during resolution live in a different backing than
		the copy made above, which is discarded. */
		if module.fullpath == zen_get_path() && repl_scope != nil {
			repl_scope^ = rs.file_scopes[module.name]
		}
	}

	return rs.resolution_map, true
}

/* Make a copy of the REPL's global scope on the current allocator for the
 * resolver to work on. This copy dies with the compiler frontend's arena at
 * the end of the line, keeping the VM's copy pristine so that a failed line
 * leaves no trace; see `persist_repl_scope`. */
copy_repl_scope :: proc(scope: map[string]^Symbol) -> map[string]^Symbol {
	scope_copy := make(map[string]^Symbol, len(scope))
	for name, var in scope {
		name_copy := strings.clone(name)
		var_copy := new(Symbol)
		var_copy^ = var^
		var_copy.name = name_copy
		scope_copy[name_copy] = var_copy
	}
	return scope_copy
}

/* Copy the REPL's global scope from the resolver onto the default allocator,
 * where the VM owns it from one line to the next, and free the scope from the
 * previous lines once its declarations live in the new copy. Module imports
 * are left out; they are re-resolved from scratch on every line. */
persist_repl_scope :: proc(vm: ^VM, previous: map[string]^Symbol) {
	old := vm.repl_scope
	vm.repl_scope = make(map[string]^Symbol, len(old))

	for name, var in old {
		if var.is_module {
			continue
		}

		name_copy := strings.clone(name)
		var_copy := new(Symbol)
		var_copy^ = var^
		var_copy.name = name_copy
		vm.repl_scope[name_copy] = var_copy
	}

	delete(old)

	for name, var in previous {
		delete(name)
		free(var)
	}
	delete(previous)
}
