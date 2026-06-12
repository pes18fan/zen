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

Resolver :: struct {
	globals:        map[string]^UntypedVariable,
	function_scope: ^FunctionScope,
	current_token:  Token,
	had_error:      bool,
}

FunctionScope :: struct {
	enclosing:                  ^FunctionScope,
	variables:                  map[string]^UntypedVariable,
	scope_depth:                int,
	local_count_for_each_block: [dynamic]int,
}

UntypedVariable :: struct #all_or_none {
	shadower:         ^UntypedVariable,
	is_final:         bool,
	is_loop_variable: bool, // not used here but important for codegen
	is_captured:      bool,
	initialized:      bool,
	scope_depth:      int,
	local_index:      int,
}

@(require_results)
resolve_local :: proc(fs: ^FunctionScope, name: string) -> (^UntypedVariable, ErrorMessage) {
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
	fs: ^FunctionScope,
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

variable_exists :: proc(rs: ^Resolver, name: string) -> bool {
	var, _ := resolve_local(rs.function_scope, name)
	up, _ := resolve_upvalue(rs.function_scope, name)
	if var != nil {
		return true
	} else if up != nil {
		return true
	} else {
		_, ok := rs.globals[name]
		if ok {
			return true
		}

		return false
	}
}

resolve_variable :: proc(rs: ^Resolver, name: string) -> ^UntypedVariable {
	var, _ := resolve_local(rs.function_scope, name)
	up, _ := resolve_upvalue(rs.function_scope, name)
	if var != nil {
		return var
	} else if up != nil {
		return up
	} else {
		global, ok := rs.globals[name]
		if ok {
			return global
		}

		return nil
	}
}

@(require_results)
assert_variable_exists :: proc(rs: ^Resolver, name: string) -> ErrorMessage {
	if !variable_exists(rs, name) {
		return fmt.tprintf("Undefined variable '%v'.", name)
	}

	return nil
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
			is_final         = is_final,
			is_loop_variable = is_loop_variable,
			is_captured      = false,
			initialized      = false,
			scope_depth      = rs.function_scope.scope_depth,
			local_index      = 0,
		}
		rs.globals[name] = new_var
	}

	var, exists := rs.function_scope.variables[name]
	if exists && var.scope_depth == rs.function_scope.scope_depth {
		return "A variable with this name in this scope already exists."
	}

	new_var := new(UntypedVariable)
	new_var^ = {
		shadower         = nil,
		is_final         = is_final,
		is_loop_variable = is_loop_variable,
		is_captured      = false,
		initialized      = false,
		scope_depth      = rs.function_scope.scope_depth,
		local_index      = rs.function_scope.local_count_for_each_block[rs.function_scope.scope_depth],
	}
	rs.function_scope.local_count_for_each_block[rs.function_scope.scope_depth] += 1

	// if the variable exists in a different scope we just shadow the thing
	if exists {
		var.shadower = new_var
	} else {
		rs.function_scope.variables[name] = new_var
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
	rs.had_error = true
}

@(require_results)
resolve_with_resolver :: proc(rs: ^Resolver, expr: Expr) -> bool {
	if expr == nil {return true}

	switch e in expr {
	case ^AssignExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.value) or_return
		try(rs, assert_variable_exists(rs, e.name.lexeme)) or_return
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
		unimplemented()
	case ^ClassExpr:
		unimplemented()
	case ^DiscardExpr:
		unimplemented()
	case ^ExitExpr:
		unimplemented()
	case ^GetExpr:
		unimplemented()
	case ^GroupingExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.expression) or_return
	case ^FunctionExpr:
		rs.current_token = e.token
		push_function_scope_untyped(rs)
		defer pop_function_scope_untyped(rs)
		for param in e.params {
			try(rs, declare_variable(rs, param.token.lexeme, false)) or_return
			define_variable(rs, param.token.lexeme)
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
		unimplemented()
	case ^ListExpr:
		unimplemented()
	case ^LiteralExpr:
		rs.current_token = e.token
	case ^LogicalExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.left) or_return
		resolve_with_resolver(rs, e.right) or_return
	case ^PipeExpr:
		unimplemented()
	case ^PrintExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.expr) or_return
	case ^ReturnExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.value) or_return
	case ^SubscriptExpr:
		unimplemented()
	case ^SubscriptSetExpr:
		unimplemented()
	case ^SequenceExpr:
		rs.current_token = e.token
		_ = resolve_with_resolver(rs, e.left)
		return resolve_with_resolver(rs, e.right)
	case ^SetExpr:
		unimplemented()
	case ^SuperExpr:
		unimplemented()
	case ^SwitchExpr:
		unimplemented()
	case ^ThisExpr:
		unimplemented()
	case ^UnaryExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.right) or_return
	case ^UseExpr:
		unimplemented()
	case ^VariableExpr:
		rs.current_token = e.token
		try(rs, assert_variable_exists(rs, e.name.lexeme)) or_return
	case ^VarDeclExpr:
		rs.current_token = e.token
		is_final := e.is_final
		bindings := e.bindings

		for binding in bindings {
			try(rs, declare_variable(rs, binding.name.lexeme, is_final)) or_return
			resolve_with_resolver(rs, binding.initializer) or_return
			define_variable(rs, binding.name.lexeme)
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
	fs := new(FunctionScope)
	fs.enclosing = rs.function_scope
	fs.scope_depth = 0
	fs.local_count_for_each_block = make([dynamic]int)
	append(&fs.local_count_for_each_block, 0)
	fs.variables = make(map[string]^UntypedVariable)
	rs.function_scope = fs
}

pop_function_scope_untyped :: proc(rs: ^Resolver) {
	fs := rs.function_scope
	enc := fs.enclosing
	for _, &v in fs.variables {
		free(v)
	}
	delete(fs.variables)
	delete(fs.local_count_for_each_block)
	free(fs)
	rs.function_scope = enc
}

push_block_scope_untyped :: proc(rs: ^Resolver) {
	rs.function_scope.scope_depth += 1
	append(&rs.function_scope.local_count_for_each_block, 0)
}

pop_block_scope_untyped :: proc(rs: ^Resolver) {
	assert(rs.function_scope.scope_depth > 0, "cannot have less than zero block scopes")
	rs.function_scope.scope_depth -= 1
}

// WIP
// Takes in the AST, resolves all variables.
// Also returns whether the operation succeeded, while printing out the error
// messages as 'resolution errors' in the process.
resolve :: proc(expr: Expr) -> bool {
	rs := Resolver {
		globals        = make(map[string]^UntypedVariable),
		function_scope = nil,
		current_token  = {},
		had_error      = false,
	}

	for fn_name in GLOBAL_NATIVE_FN_NAMES {
		native_var := new(UntypedVariable)
		native_var^ = {
			shadower         = nil,
			is_final         = true,
			is_loop_variable = false,
			is_captured      = false,
			initialized      = true,
			scope_depth      = 0,
			local_index      = 0,
		}
		rs.globals[fn_name] = native_var
	}

	defer {
		for _, v in rs.globals {
			free(v)
		}
		delete(rs.globals)
	}
	push_function_scope_untyped(&rs)
	defer pop_function_scope_untyped(&rs)

	return resolve_with_resolver(&rs, expr)
}
