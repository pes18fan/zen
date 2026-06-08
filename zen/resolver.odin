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
	ucx:           ^UntypedContext,
	current_token: Token,
	had_error:     bool,
}

// Similar to `TypeContext` but carries no type information.
// One `UntypedContext` represents one function scope much like the `TypeContext`;
// the recursive `enclosing` field inside it refers to the enclosing function
// scope.
UntypedContext :: struct {
	enclosing:   ^UntypedContext,
	bindings:    map[string]UntypedVariable,
	scope_depth: int, // current scope depth of the context
}

UntypedVariable :: struct #all_or_none {
	is_final:         bool,
	is_loop_variable: bool, // not used here but important for codegen
	is_captured:      bool,
	initialized:      bool,
	scope_depth:      int,
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
	ucx.bindings = make(map[string]UntypedVariable)
	ucx.enclosing = rs.ucx
	ucx.scope_depth = 0
	rs.ucx = ucx
}

pop_function_scope_untyped :: proc(rs: ^Resolver) {
	rs.ucx = rs.ucx.enclosing
}

push_scope_untyped :: proc(ucx: ^UntypedContext) {
	ucx.scope_depth += 1
}

pop_scope_untyped :: proc(ucx: ^UntypedContext) {
	assert(ucx.scope_depth > 0, "cannot have less than zero block scopes")
	ucx.scope_depth -= 1
}

destroy_untyped_context :: proc(ucx: ^UntypedContext) {
	u := ucx
	for u != nil {
		delete(u.bindings)
		free(u)
		u = u.enclosing
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
resolve_local :: proc(
	ucx: ^UntypedContext,
	name: Token,
) -> (
	var: UntypedVariable,
	err: ErrorMessage,
) {
	// Look for the name in the local scopes of the current function.
	if var, ok := ucx.bindings[name.lexeme]; ok && var.scope_depth <= ucx.scope_depth {
		if !var.initialized {
			return {}, "Cannot read variable in its own initializer."
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
	Base case 2: If we find the name there, return it. */
	local := resolve_local(ucx.enclosing, name) or_return
	if local != {} {
		// Mark the local as captured.
		old := ucx.enclosing.bindings[name.lexeme]
		new := make_untyped_variable(
			ucx,
			old.is_final,
			old.initialized,
			true,
			old.is_loop_variable,
		)
		ucx.enclosing.bindings[name.lexeme] = new
		return new, nil
	}

	/* Recursively look for an upvalue in the enclosing function. */
	upvalue := resolve_upvalue(ucx.enclosing, name) or_return
	if upvalue != {} {
		/* Once the local variable is found in the most deeply nested recursive call, 
		which is the outermost function, set it as captured and return the
        upvalue. That returns to the inner function's declaration, which captures 
        the upvalue from that surrounding function from where we just returned, 
        and so on, until we eventually return to the function declaration where 
        the identifier we are looking for appears. */
		old := ucx.enclosing.bindings[name.lexeme]
		new := make_untyped_variable(
			ucx,
			old.is_final,
			old.initialized,
			true,
			old.is_loop_variable,
		)
		ucx.enclosing.bindings[name.lexeme] = new
		return new, nil
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
	var, ok := ucx.bindings[name.lexeme]
	if ok {
		// redefining variables in the global scope IS allowed with some
		// restrictions
		if !in_global_scope(ucx) {
			return "A variable with this name in this scope already exists."
		}

		if var.is_final {
			return(
				is_final ? "Cannot redefine a final variable." : "Cannot redefine a final variable as normal variable." \
			)
		} else if is_final {
			return "Cannot redefine a variable as final variable."
		}

		return nil
	} else {
		ucx.bindings[name.lexeme] = make_untyped_variable(
			ucx,
			is_final = is_final,
			initialized = false,
			is_loop_variable = is_loop_variable,
		)
		return nil
	}
}

/* Mark a name binding as initialized. */
@(private = "file")
define_variable :: proc(ucx: ^UntypedContext, name: Token) {
	old := ucx.bindings[name.lexeme]
	new := make_untyped_variable(
		ucx,
		is_final = old.is_final,
		initialized = true,
		is_captured = old.is_captured,
		is_loop_variable = old.is_loop_variable,
	)
	ucx.bindings[name.lexeme] = new
}

@(private = "file")
@(require_results)
in_global_scope :: proc(ucx: ^UntypedContext) -> bool {
	return ucx.enclosing == nil
}

@(private = "file")
@(require_results)
binding_exists :: proc(ucx: ^UntypedContext, name: Token) -> bool {
	// went through all the scopes and still found nothing
	if ucx == nil {
		return false
	}

	local, _ := resolve_local(ucx, name)
	if local != {} {
		return true
	}

	return binding_exists(ucx.enclosing, name)
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

// need to ensure parser-like synchronization on errors so that we don't bail
// out on just one error; will help for nicer error messages
resolve_with_resolver :: proc(rs: ^Resolver, expr: Expr) -> bool {
	switch e in expr {}

	return true
}

// Takes in the AST, resolves all variables and puts the info in an `UntypedContext`.
// Also returns whether the operation succeeded, while printing out the error
// messages as 'resolution errors' in the process.
resolve :: proc(expr: Expr) -> (ucx: ^UntypedContext, success: bool) {
	rs := Resolver {
		ucx = nil,
	}

	// push the topmost (global) scope
	// don't pop it off! we want to return it
	push_function_scope_untyped(&rs)

	ok := resolve_with_resolver(&rs, expr)
	return rs.ucx, ok
}
