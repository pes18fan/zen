package zen

import "core:fmt"
import "core:os"
import "core:path/filepath"
import "core:slice"

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
	ucx:           ^UntypedContext,
	current_token: Token,
	had_error:     bool,
}

// Similar to `TypeContext` but carries no type information.
// One `UntypedContext` represents one function scope much like the `TypeContext`;
// the recursive `enclosing` field inside it refers to the enclosing function
// scope.
UntypedContext :: struct {
	enclosing:        ^UntypedContext,
	bindings:         [dynamic]UntypedBinding,
	scope_depth:      int, // current scope depth of the context
	scope_boundaries: [dynamic]int,
}

UntypedBinding :: struct {
	name: string,
	var:  UntypedVariable,
}

UntypedVariable :: struct #all_or_none {
	is_final:         bool,
	is_loop_variable: bool, // not used here but important for codegen
	is_captured:      bool,
	initialized:      bool,
	scope_depth:      int,
}

// Plain name comparison without the __ hack from identifiers_equal.
names_equal :: proc(a: string, b: Token) -> bool {
	return a == b.lexeme
}

make_untyped_variable :: proc(
	ucx: ^UntypedContext,
	is_final: bool,
	initialized: bool,
	is_captured: bool = false,
	is_loop_variable: bool = false,
) -> UntypedVariable {
	return UntypedVariable {
		is_final = is_final,
		initialized = initialized,
		is_captured = is_captured,
		is_loop_variable = is_loop_variable,
		scope_depth = ucx.scope_depth,
	}
}

push_function_scope_untyped :: proc(rs: ^Resolver) {
	ucx := new(UntypedContext)
	ucx.bindings = make([dynamic]UntypedBinding)
	ucx.scope_boundaries = make([dynamic]int)
	ucx.enclosing = rs.ucx
	ucx.scope_depth = 0
	rs.ucx = ucx
}

pop_function_scope_untyped :: proc(rs: ^Resolver) {
	old := rs.ucx
	rs.ucx = old.enclosing
	delete(old.bindings)
	delete(old.scope_boundaries)
	free(old)
}

push_scope_untyped :: proc(ucx: ^UntypedContext) {
	append(&ucx.scope_boundaries, len(ucx.bindings))
	ucx.scope_depth += 1
}

pop_scope_untyped :: proc(ucx: ^UntypedContext) {
	assert(ucx.scope_depth > 0, "cannot have less than zero block scopes")
	old_len := pop(&ucx.scope_boundaries)
	resize(&ucx.bindings, old_len)
	ucx.scope_depth -= 1
}

destroy_untyped_context :: proc(ucx: ^UntypedContext) {
	u := ucx
	for u != nil {
		next := u.enclosing
		delete(u.bindings)
		delete(u.scope_boundaries)
		free(u)
		u = next
	}
}

// Takes in an `UntypedContext` and from it allocates a new `TypeContext` by
// setting the type of everything in the context to fresh variables.
// Necessary step to pass information from the resolver through to the type
// checker.
untyped_to_typed_context :: proc(ucx: UntypedContext) -> ^TypeContext {
	unimplemented()
}

/*
Resolve a local name binding from the Compiler struct.
*/
@(private = "file")
@(require_results)
resolve_local :: proc(ucx: ^UntypedContext, name: Token) -> (UntypedVariable, ErrorMessage) {
	// Look for the name in the local scopes of the current function.
	for i := len(ucx.bindings) - 1; i >= 0; i -= 1 {
		b := &ucx.bindings[i]
		if b.name == name.lexeme && b.var.scope_depth <= ucx.scope_depth {
			if !b.var.initialized {
				return {}, "Cannot read variable in its own initializer."
			}
			return b.var, nil
		}
	}

	// Not found in the scopes of the current function.
	return {}, nil
}

/*
Find an upvalue (closure-captured local variable) in the function's local scope
and scopes above it, and return the information about it.
*/
@(private = "file")
@(require_results)
resolve_upvalue :: proc(
	ucx: ^UntypedContext,
	name: Token,
) -> (
	var: UntypedVariable,
	error: ErrorMessage,
) {
	/* Base case 1: We reached the end of the function stack, so the name is probably in the
	global scope. */
	if ucx.enclosing == nil {
		return {}, nil
	}

	/* Look for the name in the enclosing function's local scope.
	If found, mark it as captured and return. */
	for i := len(ucx.enclosing.bindings) - 1; i >= 0; i -= 1 {
		b := &ucx.enclosing.bindings[i]
		if b.name == name.lexeme && b.var.scope_depth <= ucx.enclosing.scope_depth {
			if !b.var.initialized {
				return {}, "Cannot read variable in its own initializer."
			}
			b.var.is_captured = true
			return b.var, nil
		}
	}

	/* Recursively look for an upvalue in enclosing functions.
	The recursive call already marks it as captured where declared;
	just propagate the info back. */
	upvalue := resolve_upvalue(ucx.enclosing, name) or_return
	if upvalue != {} {
		return upvalue, nil
	}

	// Nope, didn't find anything.
	return {}, nil
}

/*
Declare a name binding.
Errors if the variable of that name already exists in the scope, or if a final
variable is attempted to be redefined.
*/
@(private = "file")
@(require_results)
declare_variable :: proc(
	ucx: ^UntypedContext,
	name: Token,
	is_final: bool,
	is_loop_variable: bool = false,
) -> ErrorMessage {
	// Check for duplicates in the current scope.
	for i := len(ucx.bindings) - 1; i >= 0; i -= 1 {
		b := &ucx.bindings[i]
		if b.var.scope_depth < ucx.scope_depth {
			break // reached bindings from outer scopes within this function
		}
		if names_equal(b.name, name) {
			// Forward refs leave bindings uninitialized. If we find one, just
			// update its properties in-place instead of re-declaring.
			if !b.var.initialized {
				b.var.is_final = is_final
				b.var.is_loop_variable = is_loop_variable
				return nil
			}

			// NOTE: redefining variables in the global scope IS allowed with some
			// restrictions, still thinking if i should restrict it everywhere
			// or not
			if !in_global_scope(ucx) {
				return "A variable with this name in this scope already exists."
			}

			if b.var.is_final {
				return(
					is_final ? "Cannot redefine a final variable." : "Cannot redefine a final variable as normal variable." \
				)
			} else if is_final {
				return "Cannot redefine a variable as final variable."
			}

			b.var.initialized = false
			return nil
		}
	}

	append(
		&ucx.bindings,
		UntypedBinding {
			name = name.lexeme,
			var = make_untyped_variable(
				ucx,
				is_final = is_final,
				initialized = false,
				is_loop_variable = is_loop_variable,
			),
		},
	)
	return nil
}

/* Mark the most recently uninitialized binding for this name as initialized. */
@(private = "file")
define_variable :: proc(ucx: ^UntypedContext, name: Token) {
	// Find the most recently declared (still uninitialized) binding for this name
	for i := len(ucx.bindings) - 1; i >= 0; i -= 1 {
		b := &ucx.bindings[i]
		if names_equal(b.name, name) && !b.var.initialized {
			b.var.initialized = true
			return
		}
	}

	fmt.panicf("Internal compiler error: there is no variable to define with name %v", name.lexeme)
}

@(private = "file")
@(require_results)
in_global_scope :: proc(ucx: ^UntypedContext) -> bool {
	return ucx.enclosing == nil
}

// Check if a binding exists in any scope, including uninitialized forward refs.
@(private = "file")
@(require_results)
binding_exists :: proc(ucx: ^UntypedContext, name: Token) -> bool {
	if ucx == nil {
		return false
	}

	for i := len(ucx.bindings) - 1; i >= 0; i -= 1 {
		b := &ucx.bindings[i]
		if b.name == name.lexeme && b.var.scope_depth <= ucx.scope_depth {
			return true
		}
	}

	return binding_exists(ucx.enclosing, name)
}

@(require_results)
assert_binding_exists :: proc(ucx: ^UntypedContext, name: Token) -> ErrorMessage {
	if !binding_exists(ucx, name) {
		return fmt.tprintf("Undefined variable '%v'.", name.lexeme)
	}

	return nil
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

// Pre-scan the top-level AST for global function declarations and hoist them
// into the outermost scope before the main resolution walk, enabling mutual
// recursion between top-level functions.
collect_forward_refs_untyped :: proc(ucx: ^UntypedContext, expr: Expr) {
	if expr == nil {return}

	#partial switch e in expr {
	case ^VarDeclExpr:
		for binding in e.bindings {
			if _, ok := binding.initializer.(^FunctionExpr); ok {
				declare_variable(ucx, binding.name, e.is_final) or_continue
			}
		}
	case ^SequenceExpr:
		collect_forward_refs_untyped(ucx, e.left)
		collect_forward_refs_untyped(ucx, e.right)
	// other cases don't matter
	}
}

@(require_results)
resolve_with_resolver :: proc(rs: ^Resolver, expr: Expr) -> bool {
	if expr == nil {return true}

	switch e in expr {
	case ^AssignExpr:
		rs.current_token = e.token
		name := e.name
		value := e.value

		resolve_with_resolver(rs, value) or_return
		try(rs, assert_binding_exists(rs.ucx, name)) or_return
	case ^BinaryExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.left) or_return
		resolve_with_resolver(rs, e.right) or_return
	case ^BlockExpr:
		push_scope_untyped(rs.ucx)
		resolve_with_resolver(rs, e.expression) or_return
		pop_scope_untyped(rs.ucx)
	case ^BreakExpr:
		rs.current_token = e.token
	case ^CallExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.callee) or_return
		for arg in e.arguments {
			resolve_with_resolver(rs, arg) or_return
		}
	case ^ClassExpr:
		rs.current_token = e.token
		name := e.name
		superclass := e.superclass
		methods := e.methods

		if binding_exists(rs.ucx, name) {
			resolver_error(rs, "Cannot redeclare a class.")
			return false
		}

		try(rs, declare_variable(rs.ucx, name, is_final = false)) or_return
		define_variable(rs.ucx, name)

		has_superclass := false
		if superclass_name, ok := superclass.?; ok {
			has_superclass = true
			try(rs, assert_binding_exists(rs.ucx, superclass_name)) or_return

			push_scope_untyped(rs.ucx)
			super := synthetic_token("super")
			try(rs, declare_variable(rs.ucx, super, is_final = true)) or_return
			define_variable(rs.ucx, super)
		}

		for method in methods {
			rs.current_token = method.token
			resolve_with_resolver(rs, method) or_return
		}

		if has_superclass {
			pop_scope_untyped(rs.ucx)
		}
	case ^ContinueExpr:
		rs.current_token = e.token
	case ^DiscardExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.expression) or_return
	case ^ExitExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.code) or_return
	case ^ForExpr:
		rs.current_token = e.token
		push_scope_untyped(rs.ucx)
		resolve_with_resolver(rs, e.initializer) or_return
		resolve_with_resolver(rs, e.condition) or_return
		resolve_with_resolver(rs, e.increment) or_return
		push_scope_untyped(rs.ucx)
		resolve_with_resolver(rs, e.body) or_return
		pop_scope_untyped(rs.ucx)
		pop_scope_untyped(rs.ucx)
	case ^ForInExpr:
		rs.current_token = e.token
		var_name := e.var_name
		iterable := e.iterable
		body := e.body

		push_scope_untyped(rs.ucx)
		defer pop_scope_untyped(rs.ucx)

		try(
			rs,
			declare_variable(rs.ucx, var_name, is_final = true, is_loop_variable = true),
		) or_return
		define_variable(rs.ucx, var_name)

		resolve_with_resolver(rs, iterable) or_return

		iter := synthetic_token("__iter")
		try(rs, declare_variable(rs.ucx, iter, is_final = true, is_loop_variable = true)) or_return
		define_variable(rs.ucx, iter)

		idx := synthetic_token("__idx")
		try(rs, declare_variable(rs.ucx, idx, is_final = true, is_loop_variable = true)) or_return
		define_variable(rs.ucx, idx)

		push_scope_untyped(rs.ucx)
		defer pop_scope_untyped(rs.ucx)

		resolve_with_resolver(rs, body) or_return
	case ^FunctionExpr:
		rs.current_token = e.token
		params := e.params
		body := e.body

		push_function_scope_untyped(rs)
		for param in params {
			try(rs, declare_variable(rs.ucx, param.token, is_final = false)) or_return
			define_variable(rs.ucx, param.token)
		}
		resolve_with_resolver(rs, body) or_return
		pop_function_scope_untyped(rs)
	case ^GetExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.receiver) or_return
	case ^GroupingExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.expression) or_return
	case ^IfExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.condition) or_return
		resolve_with_resolver(rs, e.then_branch) or_return
		if e.else_branch != nil {
			resolve_with_resolver(rs, e.else_branch) or_return
		}
	case ^ItExpr:
		rs.current_token = e.token
	case ^ListExpr:
		rs.current_token = e.token
		for elem in e.elements {
			resolve_with_resolver(rs, elem) or_return
		}
	case ^LiteralExpr:
		rs.current_token = e.token
	case ^LogicalExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.left) or_return
		resolve_with_resolver(rs, e.right) or_return
	case ^PrintExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.expr) or_return
	case ^PipeExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.left) or_return
		resolve_with_resolver(rs, e.right) or_return
	case ^ReturnExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.value) or_return
	case ^SetExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.receiver) or_return
		resolve_with_resolver(rs, e.value) or_return
	case ^SequenceExpr:
		// resolution errors on the left expression are ignored when resolving
		// a sequence; this is to catch as many errors as possible across expressions
		rs.current_token = e.token
		_ = resolve_with_resolver(rs, e.left)
		return resolve_with_resolver(rs, e.right)
	case ^SubscriptExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.receiver) or_return
		resolve_with_resolver(rs, e.index) or_return
	case ^SubscriptSetExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.receiver) or_return
		resolve_with_resolver(rs, e.index) or_return
		resolve_with_resolver(rs, e.value) or_return
	case ^SuperExpr:
		rs.current_token = e.token
		if e.method_args != nil {
			for arg in e.method_args {
				resolve_with_resolver(rs, arg) or_return
			}
		}
	case ^SwitchExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.condition) or_return
		for c in e.cases {
			resolve_with_resolver(rs, c.condition) or_return
			resolve_with_resolver(rs, c.body) or_return
		}
		resolve_with_resolver(rs, e.else_branch) or_return
	case ^ThisExpr:
		rs.current_token = e.token
	case ^UnaryExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.right) or_return
	case ^UseExpr:
		rs.current_token = e.token
		path_str := e.path.lexeme
		path := path_str[1:len(path_str) - 1] // strip quotes
		abs_path, err := filepath.join([]string{config.__dirname, path}, context.allocator)
		if err != nil {
			resolver_error(
				rs,
				fmt.tprintf("Error when resolving module: %s", os.error_string(err)),
			)
			return false
		}
		defer delete(abs_path)

		mod_name: string
		if slice.contains(STD_MODULES[:], path) {
			mod_name = path
		} else {
			if !os.exists(abs_path) {
				resolver_error(rs, fmt.tprintf("Module '%s' not found.", abs_path))
				return false
			}
			mod_name = filepath.short_stem(path)
		}

		mod_token := synthetic_token(mod_name)
		try(rs, declare_variable(rs.ucx, mod_token, is_final = true)) or_return
		define_variable(rs.ucx, mod_token)
	case ^VarDeclExpr:
		rs.current_token = e.token
		is_final := e.is_final
		bindings := e.bindings

		for binding in bindings {
			rs.current_token = binding.name
			name := binding.name
			initializer := binding.initializer

			if !try(rs, declare_variable(rs.ucx, name, is_final)) {continue}
			resolve_with_resolver(rs, initializer) or_return
			define_variable(rs.ucx, name)
		}
	case ^VariableExpr:
		rs.current_token = e.token
		name := e.name
		try(rs, assert_binding_exists(rs.ucx, name)) or_return
	case ^WhileExpr:
		rs.current_token = e.token
		resolve_with_resolver(rs, e.condition) or_return
		resolve_with_resolver(rs, e.body) or_return
	}

	return true
}

// WIP
// Takes in the AST, resolves all variables and puts the info in an `UntypedContext`.
// Also returns whether the operation succeeded, while printing out the error
// messages as 'resolution errors' in the process.
resolve :: proc(expr: Expr) -> (ucx: ^UntypedContext, success: bool) {
	rs := Resolver {
		ucx           = nil,
		current_token = {},
		had_error     = false,
	}

	// push the topmost (global) scope
	// don't pop it off! we want to return it
	push_function_scope_untyped(&rs)

	// Pre-populate the global scope with native function names.
	for name in GLOBAL_NATIVE_FN_NAMES {
		declare_variable(rs.ucx, synthetic_token(name), is_final = true) or_continue
		define_variable(rs.ucx, synthetic_token(name))
	}

	// Pre-scan for global function forward references
	collect_forward_refs_untyped(rs.ucx, expr)

	_ = resolve_with_resolver(&rs, expr)
	return rs.ucx, !rs.had_error
}
