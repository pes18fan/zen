package zen

import "core:fmt"

TypeChecker :: struct {
	ctx: TypeContext,
}

Type :: union {
	TypeVariable,
	TypeFunctionApplication,
}

TypeVariable :: struct {
	name: string,
}

TypeFunctionApplication :: struct {
	constructor: TypeConstructor,
	args:        []Type,
}

is_type_variable :: #force_inline proc(type: Type) -> bool {
	_, ok := type.(TypeVariable)
	return ok
}

is_type_function_application :: #force_inline proc(type: Type) -> bool {
	_, ok := type.(TypeFunctionApplication)
	return ok
}

as_type_variable :: #force_inline proc(type: Type) -> TypeVariable {
	return type.(TypeVariable)
}

as_type_function_application :: #force_inline proc(type: Type) -> TypeFunctionApplication {
	return type.(TypeFunctionApplication)
}

TypeConstructor :: enum {
	NIL, // perhaps replace with UNIT ()
	BOOL,
	NUMBER,
	STRING,
	FUNCTION,
	LIST,
	RECORD, // to be added
}

type_constructor_string :: proc(c: TypeConstructor) -> string {
	switch c {
	case .NIL:
		return "nil"
	case .BOOL:
		return "boolean"
	case .NUMBER:
		return "number"
	case .STRING:
		return "string"
	case .FUNCTION:
		return "function"
	case .LIST:
		return "list"
	case .RECORD:
		return "record"
	}

	panic("invalid type constructor")
}

TypeScheme :: union {
	Type,
	TypeQuantified,
}

TypeQuantified :: struct {
	bound: []TypeVariable,
	type:  Type,
}

TypeContext :: map[string]TypeScheme

Substitution :: map[TypeVariable]Type

type_assignment_new :: proc(t: ^TypeChecker, name: string, type: TypeScheme) {
	t.ctx[name] = type
}

@(require_results)
type_assignment_exists :: proc(t: ^TypeChecker, name: string) -> bool {
	return name in t.ctx
}

@(require_results)
types_equal :: proc(a: Type, b: Type) -> bool {
	switch t1 in a {
	case TypeVariable:
		if !is_type_variable(b) {
			return false
		}
		t2 := as_type_variable(b)

		if t1.name != t2.name {
			return false
		}
	case TypeFunctionApplication:
		if !is_type_function_application(b) {
			return false
		}
		t2 := as_type_function_application(b)

		if t1.constructor != t2.constructor {
			return false
		}

		if len(t1.args) != len(t2.args) {
			return false
		}

		for i in 0 ..< len(t1.args) {
			types_equal(t1.args[i], t2.args[i]) or_return
		}
	}

	return true
}

// this is more of a hash set, the int is a dummy value
FreeVars :: map[TypeVariable]int

// allocates a map (FreeVars)
free_vars :: proc {
	free_vars_typescheme,
	free_vars_context,
}

free_vars_typescheme :: proc(ts: TypeScheme) -> FreeVars {
	fvs := make(FreeVars)

	switch type in ts {
	case Type:
		switch t in type {
		case TypeVariable:
			fvs[t] = 0
		case TypeFunctionApplication:
			// PERF: O(n^2), kinda sucky
			for arg in t.args {
				arg_fvs := free_vars_typescheme(arg)
				defer delete(arg_fvs)
				for key in arg_fvs {
					fvs[key] = 0
				}
			}
		}
	case TypeQuantified:
		internal_fvs := free_vars_typescheme(type.type)
		defer delete(internal_fvs)
		for key in internal_fvs {
			fvs[key] = 0
		}

		for bound in type.bound {
			delete_key(&fvs, bound)
		}
	}

	return fvs
}

free_vars_context :: proc(ctx: TypeContext) -> FreeVars {
	// context is empty so there's nothing free
	if ctx == nil {
		return nil
	}

	fvs := make(FreeVars)
	for _, typescheme in ctx {
		free_in_scheme := free_vars_typescheme(typescheme)
		defer delete(free_in_scheme)
		for key in free_in_scheme {
			fvs[key] = 0
		}
	}

	return fvs
}

@(require_results)
contains :: proc(container: Type, containee: TypeVariable) -> bool {
	switch t in container {
	case TypeVariable:
		// a (terminal) type variable cannot contain another
		return false
	case TypeFunctionApplication:
		// a nullary type constructor (Int, Bool etc) canot contain a type
		// variable
		if len(t.args) == 0 {
			return false
		}

		for arg in t.args {
			if contains(arg, containee) {
				return true
			}
		}
	}

	return false
}

// allocates a map (Substitution)
// NOTE: order matters, s2 is applied first then s1
combine_substitutions :: proc(s1: Substitution, s2: Substitution) -> Substitution {
	res := make(Substitution)

	// populate with s2 first
	for type_var, type in s2 {
		res[type_var] = type
	}

	// now apply s1
	for type_var, type in s1 {
		switch t in type {
		case TypeVariable:
		case TypeFunctionApplication:
		}
	}
}

// may allocate a map
@(require_results)
unify :: proc(a: Type, b: Type) -> (Substitution, ErrorMessage) {
	if is_type_variable(a) {
		if types_equal(a, b) {
			return nil, nil // nothing to substitute
		}

		if contains(b, as_type_variable(a)) {
			return nil, "occurs check failed, infinite type"
		}

		subst := make(Substitution)
		subst[as_type_variable(a)] = b
		return subst, nil
	}

	if is_type_variable(b) {
		return unify(b, a)
	}

	if is_type_function_application(a) && is_type_function_application(b) {
		t1 := as_type_function_application(a)
		t2 := as_type_function_application(a)

		if t1.constructor != t2.constructor {
			return nil, fmt.tprintf(
				"cannot unify %v with %v",
				type_constructor_string(t1.constructor),
				type_constructor_string(t2.constructor),
			)
		}

		subst := make(Substitution)
		for i in 0 ..< len(t1.args) {

		}
	}

	panic("unreachable point in unify()")
}
