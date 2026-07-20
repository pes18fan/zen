package zen

import "core:fmt"

/*
TODO: Variable resolution / symbol table creation. Needs very careful
design so that it can be used seamlessly in the codegen part without any
awful hacky designs. Some points:
- The upvalue-based approach used in clox and inherited by zen ever since
     its creation can now safely be replaced because zen now has an AST
     and therefore enough context to know what variables are closed over
     and when; the idea now is to create a new ObjCell type to replace
     ObjUpvalue that is nothing but a heap-allocated value used for closed
     over variables. This point needs to be kept in mind for closing over
     loop variables as well. However this change can break a lot of things,
     so for now it is recommended to just get the type checker fully working
     with upvalues themselves then transition to cells.
- Because of the addition of the Hindley-Milner type checker, modules can
     no longer be viably used as values. Why? Well, if we wanted to viably
     use modules as values, we'd need to typecheck them. So they'd be
     some sort of record type. But, these modules have polymorphic functions
     inside of them; for instance the list.pop function which has a quantified
     type of `forall a. List a -> a`. The list module would then be a type
     that looks something like `Record (forall a. List a -> a, ...)`, and
     this directly hits a limit of Hindley-Milner; polymorphic types are
     NOT allowed as type parameters i.e. we can only have rank-1 polymorphism.
*/

Resolver :: struct #all_or_none {
	resolutions:    Resolutions,
	current_module: ^Module,
	current_token:  Token,
}

Resolutions :: struct #all_or_none {
	file_scopes:    map[string]map[string]^Symbol, // global scopes for each file
	function_scope: ^UntypedContext, // chain of local scopes of a file
	builtin_scope:  map[string]^Symbol, // topmost scope; common to all files
	resolution_map: ResolutionMap,
}

current_file_scope :: #force_inline proc(rs: ^Resolver) -> ^map[string]^Symbol {
	return &rs.resolutions.file_scopes[rs.current_module.name]
}

UntypedContext :: struct #all_or_none {
	enclosing:            ^UntypedContext,
	within:               [dynamic]^UntypedContext,
	variables:            map[string]^Symbol,
	total_scope_depth:    int,
	local_count:          int,

	// private
	_current_scope_depth: int,
}

Symbol :: struct #all_or_none {
	shadower:         ^Symbol,
	name:             string,
	kind:             enum {
		LOCAL,
		GLOBAL,
	},
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

ResolvingNode :: union #no_nil {
	^AssignExpr,
	^VariableExpr,
	^GetExpr,
}

ResolutionMap :: map[ResolvingNode]^Symbol

@(require_results)
resolve_local :: proc(fs: ^UntypedContext, name: string) -> (^Symbol, ErrorMessage) {
	var, ok := fs.variables[name]
	if !ok {
		return nil, nil
	}

	actual := var
	for actual.shadower != nil {
		actual = actual.shadower
	}

	if !actual.initialized {
		return nil, "Cannot read local variable in its own initializer."
	}

	return var, nil
}

@(require_results)
resolve_upvalue :: proc(fs: ^UntypedContext, name: string) -> (v: ^Symbol, e: ErrorMessage) {
	// nothing found in function scopes, the thing is probably a global variable
	if fs.enclosing == nil {
		return nil, nil
	}

	// look for a local in the enclosing one, capture if its there
	local := resolve_local(fs.enclosing, name) or_return
	if local != nil {
		local.is_captured = true
		return local, nil
	}

	// recursively look for the value in the enclosing fn
	up := resolve_upvalue(fs.enclosing, name) or_return
	if up != nil {
		return up, nil
	}

	// nothing found at all
	return nil, nil
}

@(require_results)
resolve_variable :: proc(rs: ^Resolver, name: string) -> (^Symbol, bool) {
	var, _ := resolve_local(rs.resolutions.function_scope, name)
	up, _ := resolve_upvalue(rs.resolutions.function_scope, name)
	if var != nil {
		return var, true
	} else if up != nil {
		return up, true
	} else {
		// look in the file scope
		global, global_ok := current_file_scope(rs)[name]
		if global_ok {
			return global, true
		}

		// look in the builtin scope
		builtin, builtin_ok := rs.resolutions.builtin_scope[name]
		if builtin_ok {
			return builtin, true
		}

		// not found anywhere
		return nil, false
	}
}

@(require_results)
resolve_variable_in_module :: proc(
	rs: ^Resolver,
	module_name: string,
	variable_name: string,
) -> (
	^Symbol,
	bool,
) {
	file_scope := rs.resolutions.file_scopes[module_name]

	// look in the file scope
	global, global_ok := file_scope[variable_name]
	if global_ok {
		return global, true
	}

	// not found
	return nil, false
}

@(require_results)
assert_module_variable_exists_and_resolve_it :: proc(
	rs: ^Resolver,
	module_name: string,
	variable_name: string,
) -> (
	^Symbol,
	ErrorMessage,
) {
	var, ok := resolve_variable_in_module(rs, module_name, variable_name)
	if !ok {
		return nil, fmt.tprintf(
			"Variable '%v' does not exist in module '%v'.",
			variable_name,
			module_name,
		)
	}

	return var, nil
}

@(require_results)
assert_variable_exists_and_resolve_it :: proc(
	rs: ^Resolver,
	name: string,
) -> (
	^Symbol,
	ErrorMessage,
) {
	var, ok := resolve_variable(rs, name)
	if !ok {
		return nil, fmt.tprintf("Undefined variable '%v'.", name)
	}

	return var, nil
}

in_file_scope :: proc(rs: ^Resolver) -> bool {
	return(
		rs.resolutions.function_scope.enclosing == nil &&
		rs.resolutions.function_scope._current_scope_depth == 0 \
	)
}

@(require_results)
declare_and_define_module :: proc(rs: ^Resolver, name: string, type: ModuleType) -> ErrorMessage {
	if in_file_scope(rs) {
		_, exists := current_file_scope(rs)[name]
		if exists {
			return nil
		}

		new_var := new(Symbol)
		new_var^ = {
			shadower         = nil,
			name             = fmt.tprint(name),
			kind             = .GLOBAL,
			is_final         = true,
			is_loop_variable = false,
			is_captured      = false,
			is_module        = true,
			is_native_value  = true if type == .BUILTIN else false,
			is_public        = false,
			initialized      = true,
			scope_depth      = 0,
			local_index      = 0,
		}
		current_file_scope(rs)[name] = new_var
		return nil
	}

	var, exists := rs.resolutions.function_scope.variables[name]
	if exists && var.scope_depth == rs.resolutions.function_scope._current_scope_depth {
		return "A variable with this name in this scope already exists."
	}

	new_var := new(Symbol)
	new_var^ = {
		shadower         = nil,
		name             = fmt.tprint(name),
		kind             = .LOCAL,
		is_final         = true,
		is_loop_variable = false,
		is_captured      = false,
		is_module        = true,
		is_native_value  = true if type == .BUILTIN else false,
		is_public        = false,
		initialized      = true,
		scope_depth      = rs.resolutions.function_scope._current_scope_depth,
		local_index      = rs.resolutions.function_scope.local_count,
	}
	rs.resolutions.function_scope.local_count += 1

	// if the variable exists in a different scope we just shadow the thing
	if exists {
		var.shadower = new_var
	} else {
		rs.resolutions.function_scope.variables[fmt.tprint(name)] = new_var
	}

	return nil
}

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

		new_var := new(Symbol)
		new_var^ = {
			shadower         = nil,
			name             = fmt.tprint(name),
			kind             = .GLOBAL,
			is_final         = is_final,
			is_loop_variable = is_loop_variable,
			is_captured      = false,
			is_module        = false,
			is_native_value  = false,
			is_public        = is_public,
			initialized      = false,
			scope_depth      = rs.resolutions.function_scope._current_scope_depth,
			local_index      = 0,
		}

		// do NOT remove the tprint
		key := fmt.tprint(name)
		current_file_scope(rs)[key] = new_var
		return nil
	}

	var, exists := rs.resolutions.function_scope.variables[name]
	if exists && var.scope_depth == rs.resolutions.function_scope._current_scope_depth {
		return "A variable with this name in this scope already exists."
	}

	new_var := new(Symbol)
	new_var^ = {
		shadower         = nil,
		name             = fmt.tprint(name),
		kind             = .LOCAL,
		is_final         = is_final,
		is_loop_variable = is_loop_variable,
		is_captured      = false,
		is_module        = false,
		is_native_value  = false,
		is_public        = is_public,
		initialized      = false,
		scope_depth      = rs.resolutions.function_scope._current_scope_depth,
		local_index      = rs.resolutions.function_scope.local_count,
	}
	rs.resolutions.function_scope.local_count += 1

	// if the variable exists in a different scope we just shadow the thing
	if exists {
		var.shadower = new_var
	} else {
		rs.resolutions.function_scope.variables[fmt.tprint(name)] = new_var
	}

	return nil
}

define_variable :: proc(rs: ^Resolver, name: string) {
	if in_file_scope(rs) {
		var, ok := current_file_scope(rs)[name]
		if !ok {
			fmt.panicf("no global variable with name %v exists", name)
		}
		var.initialized = true
		return
	}

	v, ok := rs.resolutions.function_scope.variables[name]
	if !ok {
		fmt.panicf("no variable with name %v exists in the function scope", name)
	}
	actual := v
	for actual.shadower != nil {
		actual = actual.shadower
	}
	actual.initialized = true
}

resolver_error :: proc(rs: ^Resolver, message: string, details: string = "") {
	token := rs.current_token
	print_error(token, message, file = rs.current_module.fullpath, details = details)
}

@(require_results)
resolve_with_resolver :: proc(rs: ^Resolver, expr: Expr) -> bool {
	if expr == nil {return true}

	switch e in expr {
	case ^AssignExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.value) or_return
		var := try2(rs, assert_variable_exists_and_resolve_it(rs, e.name.lexeme)) or_return

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

		resolved := new_clone(var^)
		resolved.shadower = nil
		rs.resolutions.resolution_map[e] = resolved
	case ^BinaryExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.left) or_return
		resolve_with_resolver(rs, e.right) or_return
	case ^BlockExpr:
		rs.current_token = e.token
		push_block_scope_untyped(rs)
		defer pop_block_scope_untyped(rs)
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
	case ^GetExpr:
		rs.current_token = e.token

		if var_e, ok := e.receiver.(^VariableExpr); ok {
			receiver_var := try2(
				rs,
				assert_variable_exists_and_resolve_it(rs, var_e.token.lexeme),
			) or_return
			if !receiver_var.is_module {
				resolver_error(
					rs,
					fmt.tprintf("Dot-accessed value '%v' must be a module.", receiver_var.name),
				)
				return false
			}

			resolved: ^Symbol
			if receiver_var.is_native_value {
				mod, mod_ok := as_builtin_module(receiver_var.name)
				if !mod_ok {
					fmt.panicf(
						"expected '%v' to be a builtin module but it wasn't",
						receiver_var.name,
					)
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
			} else {
				var := try2(
					rs,
					assert_module_variable_exists_and_resolve_it(
						rs,
						var_e.token.lexeme,
						e.property.lexeme,
					),
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

				resolved = new_clone(var^)
				resolved.shadower = nil
			}

			rs.resolutions.resolution_map[e] = resolved
		} else {
			resolve_with_resolver(rs, e.receiver) or_return
		}
	case ^GroupingExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.expression) or_return
	case ^FunctionExpr:
		rs.current_token = e.token
		push_function_scope_untyped(rs)
		defer pop_function_scope_untyped(rs)
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
		push_block_scope_untyped(rs)
		defer pop_block_scope_untyped(rs)
		resolve_with_resolver(rs, e.initializer) or_return
		resolve_with_resolver(rs, e.condition) or_return
		resolve_with_resolver(rs, e.increment) or_return
		resolve_with_resolver(rs, e.body) or_return
	case ^ForInExpr:
		rs.current_token = e.token
		push_block_scope_untyped(rs)
		defer pop_block_scope_untyped(rs)
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
			var := try2(
				rs,
				assert_variable_exists_and_resolve_it(rs, varexpr.name.lexeme),
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
		var := try2(rs, assert_variable_exists_and_resolve_it(rs, e.name.lexeme)) or_return
		if var.is_module {
			resolver_error(rs, "Cannot use a module as a value.")
			return false
		}

		resolved := new_clone(var^)
		resolved.shadower = nil
		rs.resolutions.resolution_map[e] = resolved
	case ^VarDeclExpr:
		rs.current_token = e.token
		is_final := e.is_final
		bindings := e.bindings
		is_public := e.is_public

		for binding in bindings {
			try(rs, declare_variable(rs, binding.name.lexeme, is_final, is_public)) or_return
			init := binding.initializer.? or_continue

			is_fn := false
			// allow recursion
			if _, ok := init.(^FunctionExpr); ok {
				is_fn = true
				define_variable(rs, binding.name.lexeme)
			}
			resolve_with_resolver(rs, init) or_return
			if !is_fn {
				define_variable(rs, binding.name.lexeme)
			}
		}
	case ^WhileExpr:
		rs.current_token = e.token
		push_block_scope_untyped(rs)
		defer pop_block_scope_untyped(rs)
		resolve_with_resolver(rs, e.condition) or_return
		resolve_with_resolver(rs, e.body) or_return
	}

	return true
}

push_function_scope_untyped :: proc(rs: ^Resolver) {
	fs := new(UntypedContext)
	fs^ = {
		enclosing            = rs.resolutions.function_scope,
		within               = make([dynamic]^UntypedContext),
		total_scope_depth    = 0,
		local_count          = 1, // starts at 1 cuz the first local is the function itself
		variables            = make(map[string]^Symbol),
		_current_scope_depth = 0,
	}

	if rs.resolutions.function_scope != nil {
		append(&rs.resolutions.function_scope.within, fs)
	}
	rs.resolutions.function_scope = fs
}

pop_function_scope_untyped :: proc(rs: ^Resolver) {
	rs.resolutions.function_scope = rs.resolutions.function_scope.enclosing
}

push_block_scope_untyped :: proc(rs: ^Resolver) {
	rs.resolutions.function_scope.total_scope_depth += 1
	rs.resolutions.function_scope._current_scope_depth += 1
}

pop_block_scope_untyped :: proc(rs: ^Resolver) {
	assert(
		rs.resolutions.function_scope._current_scope_depth > 0,
		"cannot have less than zero block scopes",
	)
	depth := rs.resolutions.function_scope._current_scope_depth
	to_delete: [dynamic]string
	for name, var in rs.resolutions.function_scope.variables {
		if var.scope_depth == depth {
			append(&to_delete, name)
		}
	}
	for name in to_delete {
		delete_key(&rs.resolutions.function_scope.variables, name)
	}
	rs.resolutions.function_scope._current_scope_depth -= 1
}

add_native_fns :: #force_inline proc(m: ^map[string]^Symbol) {
	#unroll for fn in GLOBAL_BUILTIN_FUNCTIONS {
		native_var := new(Symbol)
		native_var^ = {
			shadower         = nil,
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

			name := binding.name

			// ONLY declare the variable; it is defined in the main resolver pass
			try(rs, declare_variable(rs, name.lexeme, is_final, is_public)) or_return
		}
	case ^SequenceExpr:
		rs.current_token = e.token
		collect_forward_references(rs, e.left) or_return
		collect_forward_references(rs, e.right) or_return
	// other cases don't matter
	}

	return true
}

// Takes in the module graph, resolves all variables.
// Also returns whether the operation succeeded, while printing out the error
// messages as 'resolution errors' in the process.
@(require_results)
resolve :: proc(graph: []^Module) -> (resolutions: Resolutions, success: bool) {
	rs := Resolver {
		resolutions = Resolutions {
			resolution_map = make(ResolutionMap),
			file_scopes = make(map[string]map[string]^Symbol),
			function_scope = nil,
			builtin_scope = make(map[string]^Symbol),
		},
		current_module = nil,
		current_token = {},
	}
	add_native_fns(&rs.resolutions.builtin_scope)

	for module in graph {
		if _, exists := rs.resolutions.file_scopes[module.name]; exists {
			resolver_error(&rs, fmt.tprintf("Module '%v' is already defined.", module.name))
			return {}, false
		}

		globals := make(map[string]^Symbol)
		rs.resolutions.file_scopes[module.name] = globals

		rs.current_module = module
		push_function_scope_untyped(&rs)
		defer pop_function_scope_untyped(&rs)

		collect_forward_references(&rs, module.ast) or_return
		resolve_with_resolver(&rs, module.ast) or_return
	}

	return rs.resolutions, true
}
