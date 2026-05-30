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
		return "bool"
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

fresh :: #force_inline proc(tc: ^TypeChecker) -> TypeVariable {
	defer tc.typevar_count += 1
	return TypeVariable{tc.typevar_count}
}

nullary :: #force_inline proc(constructor: TypeConstructor) -> TypeFunctionApplication {
	#partial switch constructor {
	case .NUMBER, .STRING, .NIL, .BOOL:
		return TypeFunctionApplication{constructor = constructor, args = nil}
	}

	fmt.panicf("invalid nullary type constructor %v", constructor)
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
resolve_type :: proc(tc: ^TypeChecker, name: string) -> TypeScheme {
	ctx := tc.ctx
	for ctx != nil {
		if t, ok := ctx.bindings[name]; ok {
			return t
		}
		ctx = ctx.enclosing
	}

	// panic cuz variable resolving is supposed to be done beforehand
	fmt.panicf("Internal compiler error: Undefined variable '%v' in type checker", name)
}

bind_type :: proc(ctx: ^TypeContext, name: string, t: TypeScheme) {
	ctx.bindings[name] = t
}

push_scope :: proc(tc: ^TypeChecker) {
	ctx := new(TypeContext, context.temp_allocator)
	ctx.bindings = make(map[string]TypeScheme, context.temp_allocator)
	ctx.enclosing = tc.ctx
	tc.ctx = ctx
}

pop_scope :: proc(tc: ^TypeChecker) {
	ctx := tc.ctx
	tc.ctx = ctx.enclosing
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
	fvs := make(FreeVars, context.temp_allocator)

	switch type in scheme {
	case Type:
		switch t in type {
		case TypeVariable:
			fvs[t] = {}
		case TypeFunctionApplication:
			// PERF: O(n^2), kinda sucky
			for arg in t.args {
				arg_fvs := free_vars(arg)
				defer delete(arg_fvs)
				for key in arg_fvs {
					fvs[key] = {}
				}
			}
		}
	case TypeQuantified:
		internal_fvs := free_vars(type.type)
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
	fvs := make(FreeVars, context.temp_allocator)
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
	apply_substitution_ctx,
}

apply_substitution_type :: proc(subst: Substitution, type: Type) -> Type {
	switch t in type {
	case TypeVariable:
		if t in subst {
			return subst[t]
		}
		return t
	case TypeFunctionApplication:
		new_args := make([]Type, len(t.args), context.temp_allocator)
		for i in 0 ..< len(t.args) {
			new_args[i] = apply_substitution(subst, t.args[i])
		}
		return TypeFunctionApplication{constructor = t.constructor, args = new_args}
	}

	panic("invalid type kind in apply_substitution_type()")
}

apply_substitution_quantified :: proc(subst: Substitution, scheme: TypeScheme) -> TypeScheme {
	switch type in scheme {
	case Type:
		return apply_substitution(subst, type)
	case TypeQuantified:
		// copy the substitution
		applied := make(Substitution, context.temp_allocator)
		for k, v in subst {
			applied[k] = v
		}

		// remove all quantified variables from the copied one
		for bound in type.bound {
			delete_key(&applied, bound)
		}

		// apply it to the type within
		return apply_substitution(applied, type.type)
	}

	panic("invalid typescheme kind in apply_substitution_quantified()")
}

apply_substitution_ctx :: proc(subst: Substitution, ctx: ^TypeContext) {
	for name, scheme in ctx.bindings {
		ctx.bindings[name] = apply_substitution(subst, scheme)
	}
}

// allocates a map (Substitution)
// NOTE: order matters, s2 is applied first then s1
combine_substitutions :: proc(s1: Substitution, s2: Substitution) -> Substitution {
	res := make(Substitution, context.temp_allocator)

	for var, ty in s2 {
		res[var] = apply_substitution(s1, ty)
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
		subst := make(Substitution, context.temp_allocator)
		for bound in type.bound {
			subst[bound] = fresh(tc)
		}
		res := apply_substitution(subst, type.type)

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

	bound := make([dynamic]TypeVariable, context.temp_allocator)
	for fv in ty_fvs {
		if fv not_in ctx_fvs {
			append(&bound, fv)
		}
	}

	if len(bound) == 0 {
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
			return nil, fmt.tprintf(
				"Infinite type: type %v contains %v.",
				type_string(b),
				type_string(a),
			)
		}

		s := make(Substitution, context.temp_allocator)
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
				"Cannot unify %v with %v.",
				type_constructor_string(t1.constructor),
				type_constructor_string(t2.constructor),
			)
		}

		s := make(Substitution, context.temp_allocator)
		for i in 0 ..< len(t1.args) {
			fst := apply_substitution(s, t1.args[i])
			snd := apply_substitution(s, t2.args[i])
			res := unify(fst, snd) or_return
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
	subst: Substitution,
	ty: Type,
	success: bool,
) {
	alpha := fresh(tc)
	s := try2(tc, m(tc, expr, alpha)) or_return
	return s, apply_substitution(s, alpha), true
}

@(require_results)
m :: proc(tc: ^TypeChecker, expr: Expr, type: Type) -> (subst: Substitution, err: ErrorMessage) {
	switch e in expr {
	case ^AssignExpr:
		unimplemented()
	case ^BinaryExpr:
		tc.current_token = e.token

		#partial switch e.operator.type {
		case .PLUS:
			// TODO: figure out how to get this working for both str and num and nothing else
			beta := fresh(tc)
			s1 := m(tc, e.left, beta) or_return
			apply_substitution(s1, tc.ctx)
			s2 := m(tc, e.right, beta) or_return
			apply_substitution(s2, tc.ctx)
			sn := unify(type, beta) or_return
			return combine_substitutions(sn, combine_substitutions(s2, s1)), nil
		case .MINUS, .STAR, .SLASH, .PERCENT:
			num := nullary(.NUMBER)
			s1 := m(tc, e.left, num) or_return
			apply_substitution(s1, tc.ctx)
			s2 := m(tc, e.right, num) or_return
			apply_substitution(s2, tc.ctx)
			sn := unify(type, num) or_return
			return combine_substitutions(sn, combine_substitutions(s2, s1)), nil
		case .EQUAL_EQUAL, .BANG_EQUAL, .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL:
			unimplemented()
		case:
			fmt.panicf("Internal compiler error: Invalid binary operator '%s'.", e.operator.lexeme)
		}
	case ^BlockExpr:
		tc.current_token = e.token
		push_scope(tc)
		s := m(tc, e.expression, type) or_return // infer body with expected type
		pop_scope(tc)
		return s, nil
	case ^BreakExpr:
		unimplemented()
	case ^CallExpr:
		tc.current_token = e.token
		// must curry
		unimplemented()
	case ^ClassExpr:
		unimplemented()
	case ^ContinueExpr:
		unimplemented()
	case ^DiscardExpr:
		unimplemented()
	case ^ExitExpr:
		unimplemented()
	case ^ForExpr:
		unimplemented()
	case ^ForInExpr:
		unimplemented()
	case ^IfExpr:
		unimplemented()
	case ^GetExpr:
		unimplemented()
	case ^SetExpr:
		unimplemented()
	case ^GroupingExpr:
		tc.current_token = e.token
		return m(tc, e.expression, type)
	case ^LogicalExpr:
		tc.current_token = e.token
		s1 := m(tc, e.left, nullary(.BOOL)) or_return
		apply_substitution(s1, tc.ctx)
		s2 := m(tc, e.right, nullary(.BOOL)) or_return
		apply_substitution(s2, tc.ctx)
		sn := unify(type, nullary(.BOOL)) or_return
		return combine_substitutions(sn, combine_substitutions(s2, s1)), nil
	case ^ItExpr:
		unimplemented()
	case ^ListExpr:
		unimplemented()
	case ^PipeExpr:
		unimplemented()
	case ^PrintExpr:
		unimplemented()
	case ^ReturnExpr:
		unimplemented()
	case ^SubscriptExpr:
		unimplemented()
	case ^SubscriptSetExpr:
		unimplemented()
	case ^SuperExpr:
		unimplemented()
	case ^ThisExpr:
		unimplemented()
	case ^LiteralExpr:
		tc.current_token = e.token

		// just unify with the matching literal constructor
		switch l in e.value {
		case f64:
			return unify(type, nullary(.NUMBER))
		case string:
			return unify(type, nullary(.STRING))
		case bool:
			return unify(type, nullary(.BOOL))
		case:
			return unify(type, nullary(.NIL))
		}
	case ^VariableExpr:
		tc.current_token = e.token
		alpha := fresh(tc) // create fresh typevar
		found := resolve_type(tc, e.name.lexeme) // find typescheme in the context
		ty := instantiate(tc, found) // instantiate the found scheme
		return unify(alpha, ty) // unify typevar with the found type
	case ^LambdaExpr:
		tc.current_token = e.token
		// must curry
		unimplemented()
	case ^SequenceExpr:
		beta := fresh(tc)
		s1 := m(tc, e.left, beta) or_return // infer left with fresh var
		apply_substitution(s1, tc.ctx)
		s2 := m(tc, e.right, type) or_return // infer right with expected type
		return combine_substitutions(s2, s1), nil
	case ^SwitchExpr:
		unimplemented()
	case ^UnaryExpr:
		tc.current_token = e.token

		must_unify_with: TypeFunctionApplication
		#partial switch e.operator.type {
		case .MINUS:
			must_unify_with = nullary(.NUMBER)
		case .NOT:
			must_unify_with = nullary(.BOOL)
		case:
			fmt.panicf("Internal compiler error: Unknown unary operator '%s'", e.operator.lexeme)
		}

		s1 := m(tc, e.right, must_unify_with) or_return
		sn := unify(type, must_unify_with) or_return
		return combine_substitutions(sn, s1), nil
	case ^UseExpr:
		unimplemented()
	case ^VarDeclExpr:
		unimplemented()
	case ^WhileExpr:
		unimplemented()
	}

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
				sb := strings.builder_make(context.temp_allocator)

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
		sb := strings.builder_make(context.temp_allocator)

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
	sb := strings.builder_make(context.temp_allocator)

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
	free_all(context.temp_allocator)
}
