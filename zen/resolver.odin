package zen

import "core:fmt"
import "core:mem"

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
	resolutions:    ResolutionMap,
	globals:        map[string]^UntypedVariable,
	function_scope: ^UntypedContext,
	current_token:  Token,
}

UntypedContext :: struct {
	enclosing:   ^UntypedContext,
	variables:   map[string]^UntypedVariable,
	scope_depth: int,
	local_count: int,
}

UntypedVariable :: struct #all_or_none {
	shadower:         ^UntypedVariable,
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
	initialized:      bool,
	scope_depth:      int,
	local_index:      int,
}

ResolvingNode :: union #no_nil {
	^AssignExpr,
	^VariableExpr,
}

ResolutionMap :: map[ResolvingNode]^UntypedVariable

@(require_results)
resolve_local :: proc(fs: ^UntypedContext, name: string) -> (^UntypedVariable, ErrorMessage) {
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
resolve_upvalue :: proc(
	fs: ^UntypedContext,
	name: string,
) -> (
	v: ^UntypedVariable,
	e: ErrorMessage,
) {
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
resolve_variable :: proc(rs: ^Resolver, name: string) -> (^UntypedVariable, bool) {
	var, _ := resolve_local(rs.function_scope, name)
	up, _ := resolve_upvalue(rs.function_scope, name)
	if var != nil {
		return var, true
	} else if up != nil {
		return up, true
	} else {
		global, ok := rs.globals[name]
		if ok {
			return global, true
		}

		return nil, false
	}
}

@(require_results)
assert_variable_exists_and_resolve_it :: proc(
	rs: ^Resolver,
	name: string,
) -> (
	^UntypedVariable,
	ErrorMessage,
) {
	var, ok := resolve_variable(rs, name)
	if !ok {
		return nil, fmt.tprintf("Undefined variable '%v'.", name)
	}

	return var, nil
}

in_global_scope :: proc(rs: ^Resolver) -> bool {
	return rs.function_scope.enclosing == nil && rs.function_scope.scope_depth == 0
}

@(require_results)
declare_variable :: proc(
	rs: ^Resolver,
	name: string,
	is_final: bool,
	is_loop_variable: bool = false,
) -> ErrorMessage {
	// TODO: handle later
	if in_global_scope(rs) {
		var, exists := rs.globals[name]
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

		new_var := new(UntypedVariable)
		new_var^ = {
			shadower         = nil,
			name             = fmt.tprint(name),
			kind             = .GLOBAL,
			is_final         = is_final,
			is_loop_variable = is_loop_variable,
			is_captured      = false,
			is_module        = false,
			is_native_value  = false,
			initialized      = false,
			scope_depth      = rs.function_scope.scope_depth,
			local_index      = 0,
		}
		// do NOT remove the fmt.tprint, idk why but the REPL won't work without
		// explicitly allocating the key
		key := fmt.tprint(name)
		rs.globals[key] = new_var
		return nil
	}

	var, exists := rs.function_scope.variables[name]
	if exists && var.scope_depth == rs.function_scope.scope_depth {
		return "A variable with this name in this scope already exists."
	}

	new_var := new(UntypedVariable)
	new_var^ = {
		shadower         = nil,
		name             = fmt.tprint(name),
		kind             = .LOCAL,
		is_final         = is_final,
		is_loop_variable = is_loop_variable,
		is_captured      = false,
		is_module        = false,
		is_native_value  = false,
		initialized      = false,
		scope_depth      = rs.function_scope.scope_depth,
		local_index      = rs.function_scope.local_count,
	}
	rs.function_scope.local_count += 1

	// if the variable exists in a different scope we just shadow the thing
	if exists {
		var.shadower = new_var
	} else {
		rs.function_scope.variables[fmt.tprint(name)] = new_var
	}

	return nil
}

define_variable :: proc(rs: ^Resolver, name: string) {
	if in_global_scope(rs) {
		var, ok := rs.globals[name]
		if !ok {
			fmt.panicf("no global variable with name %v exists", name)
		}
		var.initialized = true
		return
	}

	v, ok := rs.function_scope.variables[name]
	if !ok {
		fmt.panicf("no variable with name %v exists in the function scope", name)
	}
	actual := v
	for actual.shadower != nil {
		actual = actual.shadower
	}
	actual.initialized = true
}

resolver_error :: proc(rs: ^Resolver, message: string) {
	token := rs.current_token
	print_error(token, message)
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
		rs.resolutions[e] = resolved
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
		resolve_with_resolver(rs, e.receiver) or_return
	case ^GroupingExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.expression) or_return
	case ^FunctionExpr:
		rs.current_token = e.token
		push_function_scope_untyped(rs)
		defer pop_function_scope_untyped(rs)
		for param in e.params {
			try(rs, declare_variable(rs, param.name.lexeme, false)) or_return
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
		try(rs, declare_variable(rs, e.var_name.lexeme, true, is_loop_variable = true)) or_return
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
		if _, exists := rs.globals[name]; exists {
			resolver_error(rs, fmt.tprintf("Module '%s' is already defined.", name))
			return false
		}
		new_var := new(UntypedVariable)
		new_var^ = {
			shadower         = nil,
			name             = fmt.tprint(name),
			kind             = .GLOBAL,
			is_final         = true,
			is_loop_variable = false,
			is_captured      = false,
			is_module        = true,
			is_native_value  = true if e.type == .BUILTIN else false,
			initialized      = true,
			scope_depth      = 0,
			local_index      = 0,
		}
		rs.globals[fmt.tprint(name)] = new_var
	case ^VariableExpr:
		rs.current_token = e.token
		var := try2(rs, assert_variable_exists_and_resolve_it(rs, e.name.lexeme)) or_return
		resolved := new_clone(var^)
		resolved.shadower = nil
		rs.resolutions[e] = resolved
	case ^VarDeclExpr:
		rs.current_token = e.token
		is_final := e.is_final
		bindings := e.bindings

		for binding in bindings {
			try(rs, declare_variable(rs, binding.name.lexeme, is_final)) or_return
			is_fn := false
			// allow recursion
			if _, ok := binding.initializer.(^FunctionExpr); ok {
				is_fn = true
				define_variable(rs, binding.name.lexeme)
			}
			resolve_with_resolver(rs, binding.initializer) or_return
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
	fs.enclosing = rs.function_scope
	fs.scope_depth = 0
	fs.local_count = 1 // starts at 1 cuz the first local is the function itself
	fs.variables = make(map[string]^UntypedVariable)
	rs.function_scope = fs
}

pop_function_scope_untyped :: proc(rs: ^Resolver) {
	fs := rs.function_scope
	enc := fs.enclosing
	rs.function_scope = enc
}

push_block_scope_untyped :: proc(rs: ^Resolver) {
	rs.function_scope.scope_depth += 1
}

pop_block_scope_untyped :: proc(rs: ^Resolver) {
	assert(rs.function_scope.scope_depth > 0, "cannot have less than zero block scopes")
	depth := rs.function_scope.scope_depth
	to_delete: [dynamic]string
	for name, var in rs.function_scope.variables {
		if var.scope_depth == depth {
			append(&to_delete, name)
		}
	}
	for name in to_delete {
		delete_key(&rs.function_scope.variables, name)
	}
	rs.function_scope.scope_depth -= 1
}

@(require_results)
collect_forward_references :: proc(rs: ^Resolver, expr: Expr) -> bool {
	if expr == nil {return true}

	#partial switch e in expr {
	case ^VarDeclExpr:
		rs.current_token = e.token
		is_final := e.is_final

		for binding in e.bindings {
			// only hoist functions
			if _, ok := binding.initializer.(^FunctionExpr); !ok {continue}
			name := binding.name

			// ONLY declare the variable; it is defined in the main resolver pass
			try(rs, declare_variable(rs, name.lexeme, is_final)) or_return
		}
	case ^SequenceExpr:
		rs.current_token = e.token
		collect_forward_references(rs, e.left) or_return
		collect_forward_references(rs, e.right) or_return
	// other cases don't matter
	}

	return true
}

// Takes in the AST, resolves all variables.
// Also returns whether the operation succeeded, while printing out the error
// messages as 'resolution errors' in the process.
@(require_results)
resolve :: proc(
	expr: Expr,
	existing_globals: ^map[string]^UntypedVariable = nil,
	setup_native_fns: bool = true,
) -> (
	resolutions: ResolutionMap,
	success: bool,
) {
	globals: map[string]^UntypedVariable
	if existing_globals != nil {
		globals = existing_globals^
	} else {
		globals = make(map[string]^UntypedVariable)
	}

	if setup_native_fns {
		add_native_fns_to_variable_map(&globals, context.allocator)
	}

	rs := Resolver {
		resolutions    = nil,
		globals        = globals,
		function_scope = nil,
		current_token  = {},
	}
	push_function_scope_untyped(&rs)
	defer pop_function_scope_untyped(&rs)

	collect_forward_references(&rs, expr) or_return
	resolve_with_resolver(&rs, expr) or_return
	return rs.resolutions, true
}

add_native_fns_to_variable_map :: #force_inline proc(
	m: ^map[string]^UntypedVariable,
	allocator: mem.Allocator,
) {
	#unroll for fn in GLOBAL_BUILTIN_FUNCTIONS {
		native_var := new(UntypedVariable, allocator)
		native_var^ = {
			shadower         = nil,
			name             = fn.name,
			kind             = .GLOBAL,
			is_final         = true,
			is_loop_variable = false,
			is_captured      = false,
			is_module        = false,
			is_native_value  = true,
			initialized      = true,
			scope_depth      = 0,
			local_index      = 0,
		}
		m[fn.name] = native_var
	}
}

@(require_results)
resolve_full :: proc(
	vm: ^VM,
	expr: Expr,
	persistent_globals: ^map[string]^UntypedVariable = nil,
) -> (
	resolutions: ResolutionMap,
	ok: bool,
) {
	out := resolve(
		expr,
		existing_globals = persistent_globals,
		// persistently existing globals are assumed to already have setup their
		// native function definitions
		setup_native_fns = true if persistent_globals == nil else false,
	) or_return

	return out, true
}
