package zen

import "core:fmt"
import "core:os"
import "core:strings"

TypeChecker :: struct {
	ctx:           ^TypeContext,
	typevar_count: int,
	current_token: Token,
	had_error:     bool,
}

TypeContext :: struct {
	bindings:  map[string]TypeScheme,
	enclosing: ^TypeContext,
}

Type :: union #no_nil {
	TypeVariable,
	TypeFunctionApplication,
}

TypeVariable :: struct {
	// identifier for the variable
	idx: int,
}

TypeFunctionApplication :: struct {
	constructor: TypeConstructor,
	args:        []Type,
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
		return "->"
	case .LIST:
		return "list"
	case .RECORD:
		return "record"
	}

	panic("invalid type constructor")
}

fresh :: proc(tc: ^TypeChecker) -> TypeVariable {
	defer tc.typevar_count += 1
	return TypeVariable{tc.typevar_count}
}

@(require_results)
is_type_variable :: #force_inline proc(ty: Type) -> bool {
	_, ok := ty.(TypeVariable)
	return ok
}

@(require_results)
is_type_function_application :: #force_inline proc(ty: Type) -> bool {
	_, ok := ty.(TypeFunctionApplication)
	return ok
}

as_type_variable :: #force_inline proc(ty: Type) -> TypeVariable {
	return ty.(TypeVariable)
}

as_type_function_application :: #force_inline proc(ty: Type) -> TypeFunctionApplication {
	return ty.(TypeFunctionApplication)
}

TypeScheme :: union #no_nil {
	Type,
	TypeQuantified,
}

TypeQuantified :: struct {
	bound: []TypeVariable,
	type:  Type,
}

Substitution :: map[TypeVariable]Type

// Resolve a name - walks the scope chain exactly like resolve_local/upvalue
@(require_results)
resolve_type :: proc(tc: ^TypeChecker, name: string) -> (TypeScheme, ErrorMessage) {
	ctx := tc.ctx
	for ctx != nil {
		if t, ok := ctx.bindings[name]; ok {
			return t, nil
		}
		ctx = ctx.enclosing
	}

	// panic cuz variable resolving is supposed to be done beforehand
	fmt.panicf("undefined variable '%v'", name)
}

bind_type :: proc(ctx: ^TypeContext, name: string, t: TypeScheme) {
	ctx.bindings[name] = t
}

push_scope :: proc(tc: ^TypeChecker) {
	ctx := new(TypeContext)
	ctx.bindings = make(map[string]TypeScheme)
	ctx.enclosing = tc.ctx
	tc.ctx = ctx
}

pop_scope :: proc(tc: ^TypeChecker) {
	ctx := tc.ctx
	tc.ctx = ctx.enclosing
	delete(ctx.bindings)
	free(ctx)
}

@(require_results)
types_equal :: proc(a: Type, b: Type) -> bool {
	switch t1 in a {
	case TypeVariable:
		if !is_type_variable(b) {
			return false
		}
		t2 := as_type_variable(b)

		if t1.idx != t2.idx {
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

// this is more of a hash set, the struct{} is a dummy value
FreeVars :: map[TypeVariable]struct{}

// allocates a map (FreeVars)
free_vars :: proc {
	free_vars_typescheme,
	free_vars_context,
}

free_vars_typescheme :: proc(scheme: TypeScheme) -> FreeVars {
	fvs := make(FreeVars)

	switch type in scheme {
	case Type:
		switch t in type {
		case TypeVariable:
			fvs[t] = {}
		case TypeFunctionApplication:
			// PERF: O(n^2), kinda sucky
			for arg in t.args {
				arg_fvs := free_vars_typescheme(arg)
				defer delete(arg_fvs)
				for key in arg_fvs {
					fvs[key] = {}
				}
			}
		}
	case TypeQuantified:
		internal_fvs := free_vars_typescheme(type.type)
		defer delete(internal_fvs)
		for key in internal_fvs {
			fvs[key] = {}
		}

		for bound in type.bound {
			delete_key(&fvs, bound)
		}
	}

	return fvs
}

free_vars_context :: proc(ctx: ^TypeContext) -> FreeVars {
	fvs := make(FreeVars)
	c := ctx

	for c != nil {
		for _, scheme in c.bindings {
			scheme_fvs := free_vars(scheme)
			defer delete(scheme_fvs)
			for k in scheme_fvs {fvs[k] = {}}
		}
		c = c.enclosing
	}
	return fvs
}

// returns true if the variable `containee` occurs in `container`
// that is, does `containee` appear as a free variable in `container`?
@(require_results)
contains :: proc(container: Type, containee: TypeVariable) -> bool {
	fvs := free_vars(container)
	defer delete(fvs)
	return containee in fvs
}

apply_substitution :: proc {
	apply_substitution_type,
	apply_substitution_quantified,
}

apply_substitution_type :: proc(type: Type, subst: Substitution) -> Type {
	switch t in type {
	case TypeVariable:
		if t in subst {
			return subst[t]
		}
		return t
	case TypeFunctionApplication:
		new_args := make([]Type, len(t.args))
		for i in 0 ..< len(t.args) {
			new_args[i] = apply_substitution_type(t.args[i], subst)
		}
		return TypeFunctionApplication{constructor = t.constructor, args = new_args}
	}

	panic("invalid type kind in apply_substitution_type()")
}

apply_substitution_quantified :: proc(scheme: TypeScheme, subst: Substitution) -> TypeScheme {
	switch type in scheme {
	case Type:
		return apply_substitution_type(type, subst)
	case TypeQuantified:
		// copy the substitution
		applied := make(Substitution)
		defer delete(applied)
		for k, v in subst {
			applied[k] = v
		}

		// remove all quantified variables from the copied one
		for bound in type.bound {
			delete_key(&applied, bound)
		}

		// apply it to the type within
		return apply_substitution(type.type, applied)
	}

	panic("invalid typescheme kind in apply_substitution_quantified()")
}

// allocates a map (Substitution)
// NOTE: order matters, s2 is applied first then s1
combine_substitutions :: proc(s1: Substitution, s2: Substitution) -> Substitution {
	res := make(Substitution)

	for var, ty in s2 {
		res[var] = apply_substitution(ty, s1)
	}

	for var, ty in s1 {
		if var not_in res {
			res[var] = ty
		}
	}

	return res
}

// Replace all bound variables with fresh ones
instantiate :: proc(tc: ^TypeChecker, scheme: TypeScheme) -> Type {
	switch type in scheme {
	case Type:
		return type
	case TypeQuantified:
		subst := make(Substitution)
		defer delete(subst)
		for bound in type.bound {
			subst[bound] = fresh(tc)
		}
		res := apply_substitution(type.type, subst)

		if config.log_type {
			quant := type_string(type)
			reduced := type_string(res)
			fmt.eprintfln("%v instantiated to %v", quant, reduced)
		}

		return res
	}

	panic("reached unreachable point in instantiate()")
}

// Generalize a type over all free vars not in the current ctx
generalize :: proc(tc: ^TypeChecker, ty: Type) -> TypeScheme {
	ctx_fvs := free_vars(tc.ctx)
	defer delete(ctx_fvs)

	ty_fvs := free_vars(ty)
	defer delete(ty_fvs)

	bound := make([dynamic]TypeVariable)
	for fv in ty_fvs {
		if fv not_in ctx_fvs {
			append(&bound, fv)
		}
	}

	if len(bound) == 0 {
		delete(bound)
		return ty
	}

	res := TypeQuantified {
		bound = bound[:],
		type  = ty,
	}

	if config.log_type {
		mono := type_string(ty)
		quant := type_string(res)
		fmt.eprintfln("%v generalized to %v", mono, quant)
	}

	return res
}


// allocates a map
@(require_results)
unify :: proc(a: Type, b: Type) -> (subst: Substitution, err: ErrorMessage) {
	if is_type_variable(a) {
		if types_equal(a, b) {
			return nil, nil // nothing to substitute
		}

		if contains(b, as_type_variable(a)) {
			return nil, "occurs check failed, infinite type"
		}

		s := make(Substitution)
		s[as_type_variable(a)] = b
		return s, nil
	}

	if is_type_variable(b) {
		return unify(b, a)
	}

	if is_type_function_application(a) && is_type_function_application(b) {
		t1 := as_type_function_application(a)
		t2 := as_type_function_application(b)

		if t1.constructor != t2.constructor {
			return nil, fmt.tprintf(
				"cannot unify %v with %v",
				type_constructor_string(t1.constructor),
				type_constructor_string(t2.constructor),
			)
		}

		s := make(Substitution)
		for i in 0 ..< len(t1.args) {
			fst := apply_substitution(t1.args[i], s)
			snd := apply_substitution(t2.args[i], s)
			res := unify(fst, snd) or_return
			defer delete(res)

			old := s
			defer delete(old)
			s = combine_substitutions(s, res)
		}
		return s, nil
	}

	panic("unreachable point in unify()")
}

typecheck_error :: proc(tc: ^TypeChecker, message: string) {
	token := tc.current_token
	color_red(os.stderr, "type error ")

	if token.type == .EOF {
		fmt.eprintf("at end")
	} else {
		fmt.eprintf("at '%s'", token.lexeme)
	}

	fmt.eprintfln(": %s", message)
	fmt.eprintfln("  on [line %d]", token.line)
	tc.had_error = true
}

// wrapper around check_type to emulate algorithm W
@(require_results)
infer_type :: proc(
	tc: ^TypeChecker,
	expr: Expr,
) -> (
	s: Substitution,
	ty: TypeScheme,
	success: bool,
) {
	var := fresh(tc)
	subst := check_type(tc, expr, var) or_return
	return subst, apply_substitution(var, subst), true
}

@(require_results)
check_type :: proc(
	tc: ^TypeChecker,
	expr: Expr,
	ty: Type,
) -> (
	subst: Substitution,
	success: bool,
) {
	return try2(tc, m(tc, expr, ty))
}

// algorithm M
@(require_results)
m :: proc(tc: ^TypeChecker, expr: Expr, ty: Type) -> (subst: Substitution, err: ErrorMessage) {
	unimplemented()
}

type_string :: proc(scheme: TypeScheme) -> string {
	switch type in scheme {
	case Type:
		switch t in type {
		case TypeVariable:
			return fmt.tprintf("t%d", t.idx)
		case TypeFunctionApplication:
			if t.constructor == .FUNCTION {
				assert(len(t.args) == 2)
				return fmt.tprintf(
					"%v %v %v",
					type_string(t.args[0]),
					type_constructor_string(t.constructor),
					type_string(t.args[1]),
				)
			} else {
				sb := strings.builder_make()
				defer strings.builder_destroy(&sb)

				fmt.sbprintf(
					&sb,
					len(t.args) == 0 ? "%v" : "%v ",
					type_constructor_string(t.constructor),
				)
				for arg, idx in t.args {
					arg_str := type_string(arg)
					if idx == len(t.args) - 1 {
						fmt.sbprintf(&sb, "%v", arg_str)
					} else {
						fmt.sbprintf(&sb, "%v ", arg_str)
					}
				}

				return fmt.tprint(strings.to_string(sb))
			}
		}
	case TypeQuantified:
		sb := strings.builder_make()
		defer strings.builder_destroy(&sb)

		for bound in type.bound {
			bound_str := type_string(bound)
			fmt.sbprintf(&sb, "forall %v. ", bound_str)
		}

		type_str := type_string(type.type)
		fmt.sbprint(&sb, type_str)

		return fmt.tprint(strings.to_string(sb))
	}

	panic("invalid typescheme")
}

subst_string :: proc(subst: Substitution) -> string {
	sb := strings.builder_make()
	defer strings.builder_destroy(&sb)

	fmt.sbprint(&sb, "{")
	for var, ty in subst {
		var_string := type_string(var)
		ty_string := type_string(ty)

		fmt.sbprintf(&sb, "%v |-> %v", var_string, ty_string)
	}
	fmt.sbprint(&sb, "}")

	return fmt.tprint(strings.to_string(sb))
}

init_type_checker :: proc() -> TypeChecker {
	tc := TypeChecker {
		ctx           = nil,
		typevar_count = 0,
		had_error     = false,
	}
	push_scope(&tc)
	return tc
}

destroy_type_checker :: proc(tc: ^TypeChecker) {
	ctx := tc.ctx
	for ctx != nil {
		next := ctx.enclosing
		delete(ctx.bindings)
		free(ctx)
		ctx = next
	}
}
