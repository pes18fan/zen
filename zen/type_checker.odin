package zen

import "core:fmt"
import vmem "core:mem/virtual"
import "core:os"
import "core:slice"
import "core:strings"

TypeChecker :: struct {
	ctx:           ^TypeContext,
	typevar_count: int,
	current_token: Token,
	had_error:     bool,
	typeid_map:    map[string]Type,
}

TypeContext :: struct {
	bindings:  map[string]TypeScheme,
	enclosing: ^TypeContext,
}

Type :: union #no_nil {
	TypeVariable,
	TypeFunctionApplication,
	TypeAny,
	TypeNever,
}

// unifies with anything, the top type
TypeAny :: struct {}

// unifies with anything, the bottom type
TypeNever :: struct {}

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
		return "func"
	case .LIST:
		return "list"
	case .RECORD:
		return "record"
	}

	fmt.panicf("Internal compiler error: invalid type constructor %v", c)
}

fresh :: #force_inline proc(tc: ^TypeChecker) -> TypeVariable {
	idx := tc.typevar_count
	var := TypeVariable{idx}
	when ODIN_DEBUG {
		if config.log_type {
			fmt.eprintfln("-- create fresh type variable %v", type_string(var))
		}
	}

	tc.typevar_count += 1
	return var
}

nullary :: #force_inline proc(constructor: TypeConstructor) -> TypeFunctionApplication {
	#partial switch constructor {
	case .NUMBER, .STRING, .NIL, .BOOL:
		return TypeFunctionApplication{constructor = constructor, args = nil}
	}

	fmt.panicf("Internal compiler error: invalid nullary type constructor %v", constructor)
}

tapp :: proc(constructor: TypeConstructor, args: []Type = nil) -> TypeFunctionApplication {
	switch constructor {
	case .NUMBER, .STRING, .NIL, .BOOL:
		return nullary(constructor)
	case .FUNCTION:
		assert(args != nil, "cannot have nil type args for a function type")

		// a function type here always takes at least one value and returns exactly
		// one; hence at least two type args. The last one is the return value.
		// even a function like func() {} does that; it takes unit and returns unit
		assert(len(args) > 1, "must have at least two args for a function type")

		// cloning the slice is a bit bad for performance but it keeps `tapp` reliable
		return TypeFunctionApplication{constructor = constructor, args = slice.clone(args)}
	case .LIST:
		assert(args != nil, "cannot have nil type args for a list type")
		assert(len(args) == 1, "must have one arg exactly for a list type")
		return TypeFunctionApplication{constructor = constructor, args = slice.clone(args)}
	case .RECORD:
		unimplemented()
	}

	fmt.panicf("Internal compiler error: invalid type constructor %v", constructor)
}

type_any :: TypeAny{}
type_never :: TypeNever{}

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

@(require_results)
is_type_any :: #force_inline proc(ty: Type) -> bool {
	_, ok := ty.(TypeAny)
	return ok
}

@(require_results)
is_type_never :: #force_inline proc(ty: Type) -> bool {
	_, ok := ty.(TypeNever)
	return ok
}

as_type_variable :: #force_inline proc(ty: Type) -> TypeVariable {
	return ty.(TypeVariable)
}

as_type_function_application :: #force_inline proc(ty: Type) -> TypeFunctionApplication {
	return ty.(TypeFunctionApplication)
}

as_type_any :: #force_inline proc(ty: Type) -> TypeAny {
	return ty.(TypeAny)
}

as_type_never :: #force_inline proc(ty: Type) -> TypeNever {
	return ty.(TypeNever)
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
	fmt.panicf("Internal compiler error: Couldn't resolve variable '%v' in typechecker", name)
}

bind_type :: proc(ctx: ^TypeContext, name: string, scheme: TypeScheme) {
	ctx.bindings[name] = scheme
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
	case TypeAny:
		if !is_type_any(b) {return false}
	case TypeNever:
		if !is_type_never(b) {return false}
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
				arg_fvs := free_vars(arg)
				defer delete(arg_fvs)
				for key in arg_fvs {
					fvs[key] = {}
				}
			}
		// the any and never types are basically nullary type constructors
		case TypeAny:
			return nil
		case TypeNever:
			return nil
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
	apply_substitution_context,
}

apply_substitution_type :: proc(subst: Substitution, type: Type) -> Type {
	switch t in type {
	case TypeVariable:
		result: Type = t
		for {
			tv, tv_ok := result.(TypeVariable)
			if !tv_ok {
				return result
			}
			val, ok := subst[tv]
			if !ok {
				return result
			}
			result = val
		}
	case TypeFunctionApplication:
		new_args := make([]Type, len(t.args))
		for i in 0 ..< len(t.args) {
			new_args[i] = apply_substitution(subst, t.args[i])
		}
		return tapp(t.constructor, new_args)
	case TypeAny:
		return t
	case TypeNever:
		return t
	}

	panic("invalid type kind in apply_substitution_type()")
}

apply_substitution_quantified :: proc(subst: Substitution, scheme: TypeScheme) -> TypeScheme {
	switch type in scheme {
	case Type:
		return apply_substitution(subst, type)
	case TypeQuantified:
		// copy the substitution
		applied := make(Substitution)
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

// applies the substitution over all the scopes of the context
apply_substitution_context :: proc(subst: Substitution, ctx: ^TypeContext) {
	c := ctx
	for c != nil {
		for name, scheme in c.bindings {
			c.bindings[name] = apply_substitution(subst, scheme)
		}
		c = c.enclosing
	}
}

// allocates a map (Substitution)
// NOTE: order matters, s2 is applied first then s1
combine_substitutions :: proc(s1: Substitution, s2: Substitution) -> Substitution {
	res := make(Substitution)

	for var, ty in s2 {
		when ODIN_DEBUG {
			if var in s1 && !types_equal(s1[var], ty) {
				fmt.eprintfln(
					"WARNING: substitutions %v and %v map same variable %v to different values",
					subst_string(s1),
					subst_string(s2),
					type_string(var),
				)
			}
		}

		res[var] = apply_substitution(s1, ty)
	}

	for var, ty in s1 {
		if var not_in res {
			res[var] = apply_substitution(res, ty)
		}
	}

	when ODIN_DEBUG {
		if config.log_type {
			fmt.eprintfln(
				"-- combine subst %v with %v to get %v",
				subst_string(s2),
				subst_string(s1),
				subst_string(res),
			)
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
		for bound in type.bound {
			subst[bound] = fresh(tc)
		}
		res := apply_substitution(subst, type.type)

		when ODIN_DEBUG {
			if config.log_type {
				quant := type_string(type)
				reduced := type_string(res)
				fmt.eprintfln("-- %v instantiated to %v", quant, reduced)
			}
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
		return ty
	}

	res := TypeQuantified {
		bound = bound[:],
		type  = ty,
	}

	when ODIN_DEBUG {
		if config.log_type {
			mono := type_string(ty)
			quant := type_string(res)
			fmt.eprintfln("-- %v generalized to %v", mono, quant)
		}
	}

	return res
}

// allocates a map
@(require_results)
unify :: proc(a: Type, b: Type) -> (subst: Substitution, err: ErrorMessage) {
	if is_type_variable(a) {
		if types_equal(a, b) {
			when ODIN_DEBUG {
				if config.log_type {
					fmt.eprintfln("-- unify %v with %v trivially", type_string(a), type_string(b))
				}
			}

			return nil, nil // nothing to substitute
		}

		if contains(b, as_type_variable(a)) {
			return nil, fmt.tprintf(
				"Infinite type: type %v contains %v.",
				type_string(b),
				type_string(a),
			)
		}

		s := make(Substitution)
		s[as_type_variable(a)] = b

		when ODIN_DEBUG {
			if config.log_type {
				fmt.eprintfln(
					"-- unify %v with %v through %v",
					type_string(a),
					type_string(b),
					subst_string(s),
				)
			}
		}

		return s, nil
	}

	if is_type_variable(b) {
		return unify(b, a)
	}

	if is_type_any(a) {
		// any unifies with anything and returns a substitution that turns the
		// other type into any. What TypeScript does. Unsound, but it works.
		when ODIN_DEBUG {
			if config.log_type {
				fmt.eprintfln("-- unify any with %v trivially", type_string(b))
			}
		}

		if is_type_variable(b) {
			s := make(Substitution)
			s[as_type_variable(b)] = a

			return s, nil
		} else {
			// when unifying with type fn apps or any itself, the substitution is nil
			return nil, nil
		}
	}

	if is_type_any(b) {
		return unify(b, a)
	}

	if is_type_never(a) {
		// never also unifies with anything, but it does NOT turn the other
		// type into never itself
		when ODIN_DEBUG {
			if config.log_type {
				fmt.eprintfln("-- unify ! with %v trivially", type_string(b))
			}
		}

		return nil, nil
	}

	if is_type_never(b) {
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

		s := make(Substitution)
		for i in 0 ..< len(t1.args) {
			fst := apply_substitution(s, t1.args[i])
			snd := apply_substitution(s, t2.args[i])
			res := unify(fst, snd) or_return
			s = combine_substitutions(s, res)
		}

		when ODIN_DEBUG {
			if config.log_type {
				fmt.eprintfln(
					"-- unify %v with %v %s",
					type_string(a),
					type_string(b),
					"trivially" if len(t1.args) == 0 else fmt.tprintf("through %v", subst_string(s)),
				)
			}
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
	err: ErrorMessage,
) {
	alpha := fresh(tc)
	s := check_type(tc, expr, alpha) or_return
	res := apply_substitution(s, alpha)

	when ODIN_DEBUG {
		if config.log_type {
			fmt.eprintfln(
				"-- infer %v as %v via %v",
				type_string(alpha),
				type_string(res),
				subst_string(s),
			)
		}
	}

	return s, res, nil
}

// check if the type of expr is compatible with type
// uses algorithm M
@(require_results)
check_type :: proc(
	tc: ^TypeChecker,
	expr: Expr,
	type: Type,
) -> (
	subst: Substitution,
	err: ErrorMessage,
) {
	// ! is only allowed for expressions that actually do not return.
	if is_type_never(type) {
		s, inferred := infer_type(tc, expr) or_return
		if !is_type_never(inferred) {
			return nil, fmt.tprintf(
				"Expected a diverging expression of type !, got %v.",
				type_string(inferred),
			)
		}
		return s, nil
	}

	switch e in expr {
	case ^AssignExpr:
		tc.current_token = e.token
		s1, t1 := infer_type(tc, e.value) or_return
		apply_substitution(s1, tc.ctx)
		found := resolve_type(tc, e.name.lexeme)
		ty := instantiate(tc, found)
		sn := unify(ty, t1) or_return
		apply_substitution(sn, tc.ctx)
		sn2 := unify(type, apply_substitution(sn, ty)) or_return
		return combine_substitutions(sn2, combine_substitutions(sn, s1)), nil
	case ^BinaryExpr:
		tc.current_token = e.token

		#partial switch e.operator.type {
		case .PLUS:
			s1, s2, sn: Substitution; str_err: ErrorMessage

			// try for strings
			s1, str_err = check_type(tc, e.left, tapp(.STRING))
			if str_err == nil {
				apply_substitution(s1, tc.ctx)
				s2 = check_type(tc, e.right, tapp(.STRING)) or_return
				apply_substitution(s2, tc.ctx)
				sn = unify(type, nullary(.STRING)) or_return
				return combine_substitutions(sn, combine_substitutions(s2, s1)), nil
			}

			// try for numbers
			s1 = check_type(tc, e.left, tapp(.NUMBER)) or_return
			apply_substitution(s1, tc.ctx)
			s2 = check_type(tc, e.right, tapp(.NUMBER)) or_return
			apply_substitution(s2, tc.ctx)
			sn = unify(type, tapp(.NUMBER)) or_return
			return combine_substitutions(sn, combine_substitutions(s2, s1)), nil
		case .MINUS, .STAR, .SLASH, .PERCENT:
			num := tapp(.NUMBER)
			s1 := check_type(tc, e.left, num) or_return
			apply_substitution(s1, tc.ctx)
			s2 := check_type(tc, e.right, num) or_return
			apply_substitution(s2, tc.ctx)
			sn := unify(type, num) or_return
			return combine_substitutions(sn, combine_substitutions(s2, s1)), nil
		case .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL:
			bool_ := tapp(.BOOL)
			num := tapp(.NUMBER)
			s1 := check_type(tc, e.left, num) or_return
			apply_substitution(s1, tc.ctx)
			s2 := check_type(tc, e.right, num) or_return
			apply_substitution(s2, tc.ctx)
			sn := unify(type, bool_) or_return
			return combine_substitutions(sn, combine_substitutions(s2, s1)), nil
		case .EQUAL_EQUAL, .BANG_EQUAL:
			s1, _ := infer_type(tc, e.left) or_return
			apply_substitution(s1, tc.ctx)
			s2, _ := infer_type(tc, e.right) or_return
			apply_substitution(s2, tc.ctx)
			sn := unify(type, tapp(.BOOL)) or_return
			return combine_substitutions(sn, combine_substitutions(s2, s1)), nil
		case:
			fmt.panicf("Internal compiler error: Invalid binary operator '%s'.", e.operator.lexeme)
		}
	case ^BlockExpr:
		tc.current_token = e.token
		push_scope(tc)
		s := check_type(tc, e.expression, type) or_return // infer body with expected type
		pop_scope(tc)
		return s, nil
	case ^BreakExpr:
		tc.current_token = e.token
		return unify(type, type_never)
	case ^CallExpr:
		tc.current_token = e.token
		unimplemented()
	case ^ClassExpr:
		unimplemented()
	case ^ContinueExpr:
		tc.current_token = e.token
		return unify(type, type_never)
	case ^DiscardExpr:
		tc.current_token = e.token
		s1, _ := infer_type(tc, e.expression) or_return // infer inner and discard
		sn := unify(type, tapp(.NIL)) or_return
		return combine_substitutions(sn, s1), nil
	case ^ExitExpr:
		tc.current_token = e.token
		return unify(type, type_never)
	case ^ForExpr:
		unimplemented()
	case ^ForInExpr:
		unimplemented()
	case ^IfExpr:
		tc.current_token = e.token
		s1 := check_type(tc, e.condition, tapp(.BOOL)) or_return
		apply_substitution(s1, tc.ctx)
		s2, then_type := infer_type(tc, e.then_branch.expression) or_return
		apply_substitution(s2, tc.ctx)
		s := combine_substitutions(s2, s1)

		if e.else_branch != nil {
			// both branches must return same type
			s3 := check_type(tc, e.else_branch.expression, then_type) or_return
			apply_substitution(s3, tc.ctx)
			s = combine_substitutions(s3, s)
			s4 := unify(type, apply_substitution(s, then_type)) or_return
			s = combine_substitutions(s4, s)
		} else {
			// evaluate to nil if no else branch
			sn := unify(apply_substitution(s, type), tapp(.NIL)) or_return
			s = combine_substitutions(sn, s)
		}

		return s, nil
	case ^GetExpr:
		unimplemented()
	case ^SetExpr:
		unimplemented()
	case ^GroupingExpr:
		tc.current_token = e.token
		return check_type(tc, e.expression, type)
	case ^LogicalExpr:
		tc.current_token = e.token
		bool_ := tapp(.BOOL)
		s1 := check_type(tc, e.left, bool_) or_return
		apply_substitution(s1, tc.ctx)
		s2 := check_type(tc, e.right, bool_) or_return
		apply_substitution(s2, tc.ctx)
		sn := unify(type, bool_) or_return
		return combine_substitutions(sn, combine_substitutions(s2, s1)), nil
	case ^ItExpr:
		unimplemented()
	case ^ListExpr:
		unimplemented()
	case ^PipeExpr:
		unimplemented()
	case ^PrintExpr:
		tc.current_token = e.token
		s1, t1 := infer_type(tc, e.expr) or_return
		sn := unify(type, t1) or_return // print returns what it printed
		return combine_substitutions(sn, s1), nil
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
			return unify(type, tapp(.NUMBER))
		case string:
			return unify(type, tapp(.STRING))
		case bool:
			return unify(type, tapp(.BOOL))
		case:
			return unify(type, tapp(.NIL))
		}
	case ^VariableExpr:
		tc.current_token = e.token
		found := resolve_type(tc, e.name.lexeme) // find typescheme in the context
		ty := instantiate(tc, found) // instantiate the found scheme
		return unify(type, ty) // unify typevar with the found type
	case ^LambdaExpr:
		tc.current_token = e.token
		unimplemented()
	case ^SequenceExpr:
		tc.current_token = e.token
		beta := fresh(tc)
		s1 := check_type(tc, e.left, beta) or_return // infer left with fresh var
		apply_substitution(s1, tc.ctx)
		if e.right == nil {
			// seq evaluates to nil if there is no right side
			sn := unify(type, tapp(.NIL)) or_return
			return combine_substitutions(sn, s1), nil
		}
		s2 := check_type(tc, e.right, type) or_return // infer right with expected type
		apply_substitution(s2, tc.ctx)
		return combine_substitutions(s2, s1), nil
	case ^SwitchExpr:
		unimplemented()
	case ^UnaryExpr:
		tc.current_token = e.token

		must_unify_with: TypeFunctionApplication
		#partial switch e.operator.type {
		case .MINUS:
			must_unify_with = tapp(.NUMBER)
		case .NOT:
			must_unify_with = tapp(.BOOL)
		case:
			fmt.panicf("Internal compiler error: Unknown unary operator '%s'", e.operator.lexeme)
		}

		s1 := check_type(tc, e.right, must_unify_with) or_return
		sn := unify(type, must_unify_with) or_return
		return combine_substitutions(sn, s1), nil
	case ^UseExpr:
		unimplemented()
	case ^VarDeclExpr:
		tc.current_token = e.token
		s := make(Substitution)
		for binding in e.bindings {
			beta: Type
			if binding.type != nil {
				beta = annotation_to_type(tc, binding.type.?.lexeme) or_return
			} else {
				beta = fresh(tc)
			}

			if binding.initializer != nil {
				s1 := check_type(tc, binding.initializer, beta) or_return
				s = combine_substitutions(s1, s)
				apply_substitution(s, tc.ctx)
				inferred := apply_substitution(s, beta)
				// gen: TypeScheme
				// if is_value(binding.initializer) {
				// 	gen = generalize(tc, inferred)
				// } else {
				// 	gen = inferred
				// }
				gen := generalize(tc, inferred)
				bind_type(tc.ctx, binding.name.lexeme, gen)
			} else {
				bind_type(tc.ctx, binding.name.lexeme, beta)
			}
		}
		sn := unify(type, tapp(.NIL)) or_return // VarDeclExpr itself evaluates to nil
		return combine_substitutions(sn, s), nil
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
				sb := strings.builder_make()
				assert(len(t.args) > 1, "cannot have less than two type args in a function type")

				fmt.sbprintf(&sb, "%v (", type_constructor_string(t.constructor))

				param_count := len(t.args) - 1
				for i in 0 ..< param_count {
					arg := t.args[i]

					arg_str := type_string(arg)
					if i == param_count - 1 {
						fmt.sbprintf(&sb, "%v", arg_str)
					} else {
						fmt.sbprintf(&sb, "%v, ", arg_str)
					}
				}
				fmt.sbprint(&sb, ")")

				// now the return type
				fmt.sbprintf(&sb, " -> %v", type_string(t.args[len(t.args) - 1]))

				return fmt.tprint(strings.to_string(sb))
			} else {
				sb := strings.builder_make()

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
		case TypeAny:
			return "any"
		case TypeNever:
			return "!"
		}
	case TypeQuantified:
		sb := strings.builder_make()

		for bound in type.bound {
			bound_str := type_string(bound)
			fmt.sbprintf(&sb, "forall %v. ", bound_str)
		}

		type_str := type_string(type.type)
		fmt.sbprint(&sb, type_str)

		return fmt.tprint(strings.to_string(sb))
	}

	panic("Internal compiler error: invalid typescheme")
}

subst_string :: proc(subst: Substitution) -> string {
	sb := strings.builder_make()

	sz := len(subst)
	count := 0
	fmt.sbprint(&sb, "{")
	for var, ty in subst {
		var_string := type_string(var)
		ty_string := type_string(ty)

		fmt.sbprintf(&sb, "%v |-> %v%s", var_string, ty_string, ", " if count < sz - 1 else "")
		count += 1
	}
	fmt.sbprint(&sb, "}")

	return fmt.tprint(strings.to_string(sb))
}

annotation_to_type :: proc(tc: ^TypeChecker, annotation: string) -> (Type, ErrorMessage) {
	if annotation in tc.typeid_map {
		return tc.typeid_map[annotation], nil
	}

	return {}, fmt.tprintf("Invalid type %s.", annotation)
}

make_typeid_map :: proc() -> map[string]Type {
	// only including nullary types for now
	typeid_map := make(map[string]Type)
	typeid_map["nil"] = tapp(.NIL)
	typeid_map["bool"] = tapp(.BOOL)
	typeid_map["number"] = tapp(.NUMBER)
	typeid_map["string"] = tapp(.STRING)
	typeid_map["!"] = type_never
	typeid_map["any"] = type_any
	return typeid_map
}

typecheck :: proc(expr: Expr) -> (type: Type, success: bool) {
	// create separate arena to allocate everything for typechecker
	arena: vmem.Arena
	arena_err := vmem.arena_init_growing(&arena)
	ensure(arena_err == nil)
	defer vmem.arena_destroy(&arena)

	arena_alloc := vmem.arena_allocator(&arena)
	context.allocator = arena_alloc

	tc := TypeChecker {
		ctx           = nil,
		typevar_count = 0,
		current_token = {},
		had_error     = false,
		typeid_map    = make_typeid_map(),
	}
	push_scope(&tc)

	_, ty, err := infer_type(&tc, expr)
	if err != nil {
		typecheck_error(&tc, err.?)
		return {}, false
	}

	return ty, true
}
