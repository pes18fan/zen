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
	return_type:   Type,
	pipeline_type: Type,
	typeid_map:    map[string]Type,
	had_error:     bool,
}

TypedBinding :: struct {
	name: string,
	var:  Variable,
}

TypeContext :: struct {
	enclosing:        ^TypeContext,
	bindings:         [dynamic]TypedBinding,
	scope_depth:      int,
	scope_boundaries: [dynamic]int,
}

Variable :: struct {
	using _: UntypedVariable,
	scheme:  TypeScheme,
}

make_variable :: proc(ctx: ^TypeContext, scheme: TypeScheme) -> Variable {
	// NOTE: currently most the fields from `Variable` are unused here
	return Variable{scope_depth = ctx.scope_depth, scheme = scheme}
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
		return "Nil"
	case .BOOL:
		return "Bool"
	case .NUMBER:
		return "Number"
	case .STRING:
		return "String"
	case .FUNCTION:
		return "Func"
	case .LIST:
		return "List"
	case .RECORD:
		return "Record"
	}

	fmt.panicf("Internal compiler error: invalid type constructor %v", c)
}

fresh :: #force_inline proc(tc: ^TypeChecker) -> TypeVariable {
	idx := tc.typevar_count
	var := TypeVariable{idx}
	when ODIN_DEBUG {
		if config.log_type {
			fmt.eprintfln("-- create fresh type variable %v", type_string(var, true))
		}
	}

	tc.typevar_count += 1
	return var
}

tapp :: proc(constructor: TypeConstructor, args: []Type = nil) -> TypeFunctionApplication {
	switch constructor {
	case .NUMBER, .NIL, .BOOL, .STRING:
		return TypeFunctionApplication{constructor = constructor, args = nil}
	case .FUNCTION:
		assert(args != nil, "cannot have nil type args for a function type")

		// a function type here may take zero or more values and returns exactly
		// one; hence at least one type arg.
		// for instance a function like func() {} takes nothing and returns unit
		assert(len(args) > 0, "must have at least one (return type) arg for a function type")

		// cloning the slice is a bit bad for performance but it keeps `tapp` reliable
		return TypeFunctionApplication{constructor = constructor, args = slice.clone(args)}
	case .LIST:
		assert(args != nil, "cannot have nil type args for a list type")
		assert(len(args) == 1, "must have one type arg exactly for a list type")
		return TypeFunctionApplication{constructor = constructor, args = slice.clone(args)}
	case .RECORD:
		unimplemented()
	}

	fmt.panicf("Internal compiler error: invalid type constructor %v", constructor)
}

// not necessary for the type checker as it just uses an arena; but the
// parser needs it because it allocates types for annotations
free_type :: proc(type: ^Type) {
	switch t in type {
	case TypeVariable: // nothing
	case TypeFunctionApplication:
		switch t.constructor {
		case .NUMBER, .STRING, .NIL, .BOOL: // nothing to free
		case .FUNCTION, .LIST, .RECORD:
			delete(t.args)
		}
	case TypeAny: // nothing
	case TypeNever: // nothing
	}
}

free_typescheme :: proc(scheme: ^TypeScheme) {
	switch &type in scheme {
	case Type:
		free_type(&type)
	case TypeQuantified:
		delete(type.bound)
		free_type(&type.type)
	}
}

type_any :: TypeAny{}
type_never :: TypeNever{}

tquant :: proc(bound: []TypeVariable, type: Type) -> TypeQuantified {
	return TypeQuantified{bound = slice.clone(bound), type = type}
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
		for i := len(ctx.bindings) - 1; i >= 0; i -= 1 {
			b := ctx.bindings[i]
			if b.name == name && b.var.scope_depth <= ctx.scope_depth {
				t := b.var.scheme
				when ODIN_DEBUG {
					if config.log_type {
						fmt.eprintfln(
							"-- grab type %v of %s from current context",
							type_string(t, true),
							name,
						)
					}
				}

				return t
			}
		}
		ctx = ctx.enclosing
	}

	// panic cuz variable resolving is supposed to be done beforehand
	fmt.panicf("Internal compiler error: Couldn't resolve variable '%v' in typechecker", name)
}

bind_type :: proc(ctx: ^TypeContext, name: string, scheme: TypeScheme) {
	append(&ctx.bindings, TypedBinding{name = name, var = make_variable(ctx, scheme)})

	when ODIN_DEBUG {
		if config.log_type {
			fmt.eprintfln("-- update current context with %s: %v", name, type_string(scheme, true))
		}
	}
}

when ODIN_DEBUG {
	fn_scope_counter := -1
}

push_function_scope :: proc(tc: ^TypeChecker) {
	ctx := new(TypeContext)
	ctx.bindings = make([dynamic]TypedBinding)
	ctx.scope_boundaries = make([dynamic]int)
	ctx.enclosing = tc.ctx
	ctx.scope_depth = 0
	tc.ctx = ctx

	when ODIN_DEBUG {
		fn_scope_counter += 1
		if config.log_type {
			fmt.eprintfln("\n-- enter fn %d", fn_scope_counter)
		}
	}
}

pop_function_scope :: proc(tc: ^TypeChecker) {
	tc.ctx = tc.ctx.enclosing

	when ODIN_DEBUG {
		if config.log_type {
			fmt.eprintfln("-- exit fn %d\n", fn_scope_counter)
		}
		fn_scope_counter -= 1
	}
}

push_scope :: proc(ctx: ^TypeContext) {
	append(&ctx.scope_boundaries, len(ctx.bindings))
	ctx.scope_depth += 1

	when ODIN_DEBUG {
		if config.log_type {
			fmt.eprintfln("\n-- enter block %d", ctx.scope_depth)
		}
	}
}

pop_scope :: proc(ctx: ^TypeContext) {
	assert(ctx.scope_depth > 0, "cannot have less than zero block scopes")
	old_len := pop(&ctx.scope_boundaries)
	resize(&ctx.bindings, old_len)
	ctx.scope_depth -= 1

	when ODIN_DEBUG {
		if config.log_type {
			fmt.eprintfln("-- exit block %d\n", ctx.scope_depth + 1)
		}
	}
}

destroy_type_context :: proc(ctx: ^TypeContext) {
	c := ctx
	for c != nil {
		for i in 0 ..< len(c.bindings) {
			free_typescheme(&c.bindings[i].var.scheme)
		}
		delete(c.bindings)
		delete(c.scope_boundaries)
		free(c)
		c = c.enclosing
	}
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
		for i in 0 ..< len(c.bindings) {
			scheme := c.bindings[i].var.scheme
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
		new_type := apply_substitution(applied, type.type)

		// now wrap it with the bound to create a new scheme
		return tquant(type.bound, new_type)
	}

	panic("invalid typescheme kind in apply_substitution_quantified()")
}

// applies the substitution over all the scopes of the context
apply_substitution_context :: proc(subst: Substitution, ctx: ^TypeContext) {
	c := ctx
	for c != nil {
		for i in 0 ..< len(c.bindings) {
			b := &c.bindings[i]
			b.var.scheme = apply_substitution(subst, b.var.scheme)
		}
		c = c.enclosing
	}

	// TODO: add a debug log showing how the context got updated
	when ODIN_DEBUG {
		if config.log_type {
			fmt.eprintfln("-- apply subst %v to current context", subst_string(subst, true))
		}
	}
}

// allocates a map (Substitution)
// NOTE: order matters, s2 is applied first then s1
combine_substitutions :: proc(s1: Substitution, s2: Substitution) -> Substitution {
	res := make(Substitution)

	for var, ty in s2 {
		when ODIN_DEBUG {
			if var in s1 && !types_equal(s1[var], ty) {
				color_yellow(os.stderr, "WARNING")
				fmt.eprintfln(
					": substitutions %v and %v map same variable %v to different values",
					subst_string(s1, true),
					subst_string(s2, true),
					type_string(var, true),
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
				subst_string(s2, true),
				subst_string(s1, true),
				subst_string(res, true),
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
				quant := type_string(type, true)
				reduced := type_string(res, true)
				fmt.eprintfln("-- instantiate %v to %v", quant, reduced)
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

	res := tquant(bound[:], ty)

	when ODIN_DEBUG {
		if config.log_type {
			mono := type_string(ty, true)
			quant := type_string(res, true)
			fmt.eprintfln("-- generalize %v to %v", mono, quant)
		}
	}

	return res
}

UnificationError :: enum {
	INFINITE_TYPE,
	MISMATCH,
}

// tries to unify an expected type with another given one
// done to provide nicer error messages as just applying unify() makes it
// unclear which is the expected type
try_unify :: proc(expected: Type, checking: Type) -> (Substitution, ErrorMessage) {
	subst, err := unify(expected, checking)
	if err != nil {
		switch err {
		case .INFINITE_TYPE:
			return nil, fmt.tprintf(
				"Infinite type: type %v contains %v.",
				type_string(checking, false),
				type_string(expected, false),
			)
		case .MISMATCH:
			return nil, fmt.tprintf(
				"Expected an expression of type %v, got %v.",
				type_string(expected, false),
				type_string(checking, false),
			)
		}
	}

	return subst, nil
}

// allocates a map
@(require_results)
unify :: proc(a: Type, b: Type) -> (subst: Substitution, err: Maybe(UnificationError)) {
	if is_type_any(a) {
		// any unifies with anything and returns a substitution that turns the
		// other type into any. What TypeScript does. Unsound, but it works.
		when ODIN_DEBUG {
			if config.log_type {
				fmt.eprintfln("-- unify any with %v trivially", type_string(b, true))
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
		// type into never itself.
		when ODIN_DEBUG {
			if config.log_type {
				fmt.eprintfln("-- unify ! with %v trivially", type_string(b, true))
			}
		}

		return nil, nil
	}

	if is_type_never(b) {
		return unify(b, a)
	}

	if is_type_variable(a) {
		if types_equal(a, b) {
			when ODIN_DEBUG {
				if config.log_type {
					fmt.eprintfln(
						"-- unify %v with %v trivially",
						type_string(a, true),
						type_string(b, true),
					)
				}
			}

			return nil, nil // nothing to substitute
		}

		if contains(b, as_type_variable(a)) {
			return nil, .INFINITE_TYPE
		}

		s := make(Substitution)
		s[as_type_variable(a)] = b

		when ODIN_DEBUG {
			if config.log_type {
				fmt.eprintfln(
					"-- unify %v with %v through %v",
					type_string(a, true),
					type_string(b, true),
					subst_string(s, true),
				)
			}
		}

		return s, nil
	}

	if is_type_variable(b) {
		return unify(b, a)
	}

	if is_type_function_application(a) && is_type_function_application(b) {
		t1 := as_type_function_application(a)
		t2 := as_type_function_application(b)

		if t1.constructor != t2.constructor {
			return nil, .MISMATCH
		}

		if len(t1.args) != len(t2.args) {
			return nil, .MISMATCH
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
					type_string(a, true),
					type_string(b, true),
					"trivially" if len(t1.args) == 0 else fmt.tprintf("through %v", subst_string(s, true)),
				)
			}
		}

		return s, nil
	}

	panic("unreachable point in unify()")
}

type_mismatch_string :: proc(want: Type, got: Type) -> string {
	return fmt.tprintf(
		"Expected expression of type %v, got %v",
		type_string(want, false),
		type_string(got, false),
	)
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

// is the expression a syntactic "value"?
is_value :: proc(expr: Expr) -> bool {
	if expr == nil {return false}

	#partial switch e in expr {
	case ^LiteralExpr, ^FunctionExpr, ^VariableExpr:
		return true
	case ^ListExpr:
		// technically a value but due to its mutability, it is kept as a non-value
		return false
	case:
		return false
	}
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
				type_string(alpha, true),
				type_string(res, true),
				subst_string(s, true),
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
	switch e in expr {
	case ^AssignExpr:
		tc.current_token = e.token
		s1, t1 := infer_type(tc, e.value) or_return
		apply_substitution(s1, tc.ctx)
		found := resolve_type(tc, e.name.lexeme)
		ty := instantiate(tc, found)
		sn := try_unify(ty, t1) or_return
		apply_substitution(sn, tc.ctx)
		sn2 := try_unify(type, apply_substitution(sn, ty)) or_return
		return combine_substitutions(sn2, combine_substitutions(sn, s1)), nil
	case ^BinaryExpr:
		tc.current_token = e.token

		#partial switch e.operator.type {
		case .PLUS, .MINUS, .STAR, .SLASH, .PERCENT:
			num := tapp(.NUMBER)
			s1 := check_type(tc, e.left, num) or_return
			apply_substitution(s1, tc.ctx)
			s2 := check_type(tc, e.right, num) or_return
			apply_substitution(s2, tc.ctx)
			sn := try_unify(type, num) or_return
			return combine_substitutions(sn, combine_substitutions(s2, s1)), nil
		case .DOT_DOT:
			str := tapp(.STRING)
			s1 := check_type(tc, e.left, str) or_return
			apply_substitution(s1, tc.ctx)
			s2 := check_type(tc, e.right, str) or_return
			apply_substitution(s2, tc.ctx)
			sn := try_unify(type, str) or_return
			return combine_substitutions(sn, combine_substitutions(s2, s1)), nil
		case .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL:
			bool_ := tapp(.BOOL)
			num := tapp(.NUMBER)
			s1 := check_type(tc, e.left, num) or_return
			apply_substitution(s1, tc.ctx)
			s2 := check_type(tc, e.right, num) or_return
			apply_substitution(s2, tc.ctx)
			sn := try_unify(type, bool_) or_return
			return combine_substitutions(sn, combine_substitutions(s2, s1)), nil
		case .EQUAL_EQUAL, .BANG_EQUAL:
			s1, _ := infer_type(tc, e.left) or_return
			apply_substitution(s1, tc.ctx)
			s2, _ := infer_type(tc, e.right) or_return
			apply_substitution(s2, tc.ctx)
			sn := try_unify(type, tapp(.BOOL)) or_return
			return combine_substitutions(sn, combine_substitutions(s2, s1)), nil
		case:
			fmt.panicf("Internal compiler error: Invalid binary operator '%s'.", e.operator.lexeme)
		}
	case ^BlockExpr:
		tc.current_token = e.token
		push_scope(tc.ctx)
		s := make(Substitution)
		if e.expression != nil {
			s = check_type(tc, e.expression, type) or_return // infer body with expected type
		} else {
			s = try_unify(type, tapp(.NIL)) or_return // infer body with expected type
		}
		pop_scope(tc.ctx)
		return s, nil
	case ^BreakExpr:
		tc.current_token = e.token
		return try_unify(type, type_never)
	case ^CallExpr:
		tc.current_token = e.token
		callee := e.callee
		arguments := e.arguments

		// build the expected function type
		arg_types: []Type
		if len(arguments) != 0 {
			arg_types = make([]Type, len(arguments))
			for i in 0 ..< len(arguments) {
				arg_types[i] = fresh(tc)
			}
		}

		all_args := make([]Type, len(arg_types) + 1)
		if len(arg_types) != 0 {copy(all_args, arg_types)}
		all_args[len(arg_types)] = type
		func_type := tapp(.FUNCTION, all_args)

		// handle method calls vs regular ones
		s := make(Substitution)

		if get_expr, ok := callee.(^GetExpr); ok {
			s1, _ := infer_type(tc, get_expr.receiver) or_return
			apply_substitution(s1, tc.ctx)
			s = combine_substitutions(s1, s)
		} else {
			s1 := check_type(tc, callee, func_type) or_return
			apply_substitution(s1, tc.ctx)
			s = combine_substitutions(s1, s)
		}

		// typecheck each argument
		for arg, idx in arguments {
			expected := apply_substitution(s, arg_types[idx])
			s1 := check_type(tc, arg, expected) or_return
			apply_substitution(s1, tc.ctx)
			s = combine_substitutions(s1, s)
		}
		return s, nil
	case ^ClassExpr:
		unimplemented()
	case ^ContinueExpr:
		tc.current_token = e.token
		return try_unify(type, type_never)
	case ^DiscardExpr:
		tc.current_token = e.token
		s1, _ := infer_type(tc, e.expression) or_return // infer inner and discard it
		sn := try_unify(type, tapp(.NIL)) or_return
		return combine_substitutions(sn, s1), nil
	case ^ExitExpr:
		tc.current_token = e.token
		return try_unify(type, type_never)
	case ^ForExpr:
		tc.current_token = e.token

		push_scope(tc.ctx)
		s := make(Substitution)
		if e.initializer != nil {
			s_init, _ := infer_type(tc, e.initializer) or_return
			apply_substitution(s_init, tc.ctx)
			s = combine_substitutions(s_init, s)
		}

		if e.condition != nil {
			s_cond, _ := infer_type(tc, e.condition) or_return
			apply_substitution(s_cond, tc.ctx)
			s = combine_substitutions(s_cond, s)
		}

		if e.increment != nil {
			s_inc, _ := infer_type(tc, e.increment) or_return
			apply_substitution(s_inc, tc.ctx)
			s = combine_substitutions(s_inc, s)
		}

		s_body := check_type(tc, e.body.expression, fresh(tc)) or_return
		apply_substitution(s_body, tc.ctx)
		s = combine_substitutions(s_body, s)
		pop_scope(tc.ctx)
		sn := try_unify(type, tapp(.NIL)) or_return
		return combine_substitutions(sn, s), nil
	case ^ForInExpr:
		tc.current_token = e.token
		push_scope(tc.ctx)
		bind_type(tc.ctx, strings.clone(e.var_name.lexeme), fresh(tc)) // fresh typevar for for-in loop variable
		s_iter := check_type(tc, e.iterable, fresh(tc)) or_return // should probably be replaced by a `string | list` union in future
		apply_substitution(s_iter, tc.ctx)
		beta := fresh(tc)
		s_body := check_type(tc, e.body.expression, beta) or_return
		apply_substitution(s_body, tc.ctx)
		pop_scope(tc.ctx)
		sn := try_unify(type, tapp(.NIL)) or_return
		return combine_substitutions(sn, combine_substitutions(s_body, s_iter)), nil
	case ^IfExpr:
		tc.current_token = e.token
		s1, _ := infer_type(tc, e.condition) or_return // the condition can be any value
		apply_substitution(s1, tc.ctx)
		s2, then_type := infer_type(tc, e.then_branch.expression) or_return
		apply_substitution(s2, tc.ctx)
		s := combine_substitutions(s2, s1)

		if e.else_branch != nil {
			// both branches must return same type
			s3 := check_type(tc, e.else_branch.expression, then_type) or_return
			apply_substitution(s3, tc.ctx)
			s = combine_substitutions(s3, s)
			s4 := try_unify(type, apply_substitution(s, then_type)) or_return
			s = combine_substitutions(s4, s)
		} else {
			// evaluate to nil if no else branch
			sn := try_unify(apply_substitution(s, type), tapp(.NIL)) or_return
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
		s1, _ := infer_type(tc, e.left) or_return
		apply_substitution(s1, tc.ctx)
		s2, _ := infer_type(tc, e.right) or_return
		apply_substitution(s2, tc.ctx)
		sn := try_unify(type, tapp(.BOOL)) or_return
		return combine_substitutions(sn, combine_substitutions(s2, s1)), nil
	case ^ItExpr:
		unimplemented()
	case ^ListExpr:
		tc.current_token = e.token
		if len(e.elements) == 0 {
			s := try_unify(type, tapp(.LIST, {fresh(tc)})) or_return
			return s, nil
		}

		elem := fresh(tc)
		s := make(Substitution)
		for element in e.elements {
			s1 := check_type(tc, element, apply_substitution(s, elem)) or_return
			s = combine_substitutions(s1, s)
			apply_substitution(s, tc.ctx)
		}
		sn := try_unify(type, tapp(.LIST, {apply_substitution(s, elem)})) or_return
		return combine_substitutions(sn, s), nil
	case ^PipeExpr:
		unimplemented()
	case ^PrintExpr:
		tc.current_token = e.token
		s1, t1 := infer_type(tc, e.expr) or_return
		sn := try_unify(type, t1) or_return // print returns what it printed
		return combine_substitutions(sn, s1), nil
	case ^ReturnExpr:
		tc.current_token = e.token
		if e.value != nil {
			s1 := check_type(tc, e.value, tc.return_type) or_return
			apply_substitution(s1, tc.ctx)
			sn := try_unify(type, type_never) or_return // return expression itself has type `!`
			return combine_substitutions(sn, s1), nil
		} else {
			s1 := try_unify(tc.return_type, tapp(.NIL)) or_return
			apply_substitution(s1, tc.ctx)
			sn := try_unify(type, type_never) or_return
			return combine_substitutions(sn, s1), nil
		}
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
			return try_unify(type, tapp(.NUMBER))
		case string:
			return try_unify(type, tapp(.STRING))
		case bool:
			return try_unify(type, tapp(.BOOL))
		case:
			return try_unify(type, tapp(.NIL))
		}
	case ^VariableExpr:
		tc.current_token = e.token
		found := resolve_type(tc, e.name.lexeme) // find typescheme in the context
		found_t := instantiate(tc, found) // instantiate the found scheme
		return try_unify(type, found_t) // unify typevar with the found type
	case ^FunctionExpr:
		tc.current_token = e.token
		bound_to := e.bound_to
		params := e.params
		body := e.body
		return_type := e.return_type

		param_types: []Type
		if len(params) != 0 {
			param_types = make([]Type, len(params))

			for param, idx in params {
				param_types[idx] = param.type.? or_else fresh(tc)
			}
		}

		ret_type := return_type.? or_else fresh(tc)

		// last arg is return type
		all_args := make([]Type, len(param_types) + 1)
		if len(param_types) != 0 {copy(all_args, param_types)}
		all_args[len(param_types)] = ret_type
		func_type := tapp(.FUNCTION, all_args)

		// unify with expected type first
		s1 := try_unify(type, func_type) or_return
		apply_substitution(s1, tc.ctx)

		// start the function scope
		push_function_scope(tc)
		if name, ok := bound_to.?; ok {
			bind_type(tc.ctx, strings.clone(name.lexeme), apply_substitution(s1, func_type))
		}

		for param, idx in params {
			bind_type(
				tc.ctx,
				strings.clone(param.token.lexeme),
				apply_substitution(s1, param_types[idx]),
			)
		}

		// set return type context to allow ReturnExpr to check against it
		old_ret := tc.return_type
		tc.return_type = apply_substitution(s1, ret_type)
		defer tc.return_type = old_ret

		s2 := check_type(tc, body, apply_substitution(s1, ret_type)) or_return
		apply_substitution(s2, tc.ctx)
		pop_function_scope(tc)

		return combine_substitutions(s2, s1), nil
	case ^SequenceExpr:
		tc.current_token = e.token
		beta := fresh(tc)
		s1 := check_type(tc, e.left, beta) or_return // infer left with fresh var
		apply_substitution(s1, tc.ctx)
		if e.right == nil {
			// seq evaluates to nil if there is no right side
			sn := try_unify(type, tapp(.NIL)) or_return
			return combine_substitutions(sn, s1), nil
		}
		s2 := check_type(tc, e.right, apply_substitution(s1, type)) or_return // infer right with expected type
		apply_substitution(s2, tc.ctx)
		return combine_substitutions(s2, s1), nil
	case ^SwitchExpr:
		tc.current_token = e.token
		cond_type: Type

		s := make(Substitution)
		if e.condition != nil {
			s_cond, t_cond := infer_type(tc, e.condition) or_return
			s = combine_substitutions(s_cond, s)
			cond_type = t_cond
		} else {
			cond_type = tapp(.BOOL)
		}

		apply_substitution(s, tc.ctx)
		for c in e.cases {
			s1 := check_type(tc, c.condition, apply_substitution(s, cond_type)) or_return
			s = combine_substitutions(s1, s)
			apply_substitution(s, tc.ctx)
			s2 := check_type(tc, c.body, apply_substitution(s, type)) or_return
			s = combine_substitutions(s2, s)
			apply_substitution(s, tc.ctx)
		}
		s_else := check_type(tc, e.else_branch, apply_substitution(s, type)) or_return
		s = combine_substitutions(s_else, s)
		return s, nil
	case ^UnaryExpr:
		tc.current_token = e.token

		#partial switch e.operator.type {
		case .MINUS:
			s1 := check_type(tc, e.right, tapp(.NUMBER)) or_return
			sn := try_unify(type, tapp(.NUMBER)) or_return
			return combine_substitutions(sn, s1), nil
		case .NOT:
			s1, _ := infer_type(tc, e.right) or_return
			sn := try_unify(type, tapp(.BOOL)) or_return
			return combine_substitutions(sn, s1), nil
		case:
			fmt.panicf("Internal compiler error: Unknown unary operator '%s'", e.operator.lexeme)
		}

	case ^UseExpr:
		unimplemented()
	case ^VarDeclExpr:
		tc.current_token = e.token
		s := make(Substitution)
		for binding in e.bindings {
			beta := binding.type.? or_else fresh(tc)

			if binding.initializer != nil {
				// allow recursion for anonymous fns
				if _, ok := binding.initializer.(^FunctionExpr); ok {
					bind_type(tc.ctx, strings.clone(binding.name.lexeme), beta)
				}
				s1 := check_type(tc, binding.initializer, beta) or_return

				s = combine_substitutions(s1, s)
				apply_substitution(s, tc.ctx)
				inferred := apply_substitution(s, beta)

				// only generalize if the thing is a syntactic 'value' (the
				// value restriction)
				gen: TypeScheme
				if is_value(binding.initializer) {
					gen = generalize(tc, inferred)
				} else {
					gen = inferred
				}
				bind_type(tc.ctx, strings.clone(binding.name.lexeme), gen)
			} else {
				bind_type(tc.ctx, strings.clone(binding.name.lexeme), beta)
			}
		}
		sn := try_unify(type, tapp(.NIL)) or_return // VarDeclExpr itself evaluates to nil
		return combine_substitutions(sn, s), nil
	case ^WhileExpr:
		tc.current_token = e.token
		push_scope(tc.ctx)
		s1, _ := infer_type(tc, e.condition) or_return
		apply_substitution(s1, tc.ctx)
		beta := fresh(tc)
		s2 := check_type(tc, e.body.expression, beta) or_return
		apply_substitution(s2, tc.ctx)
		pop_scope(tc.ctx)
		sn := try_unify(type, tapp(.NIL)) or_return
		return combine_substitutions(sn, combine_substitutions(s2, s1)), nil
	}

	unimplemented()
}

TypePrintCtx :: struct {
	names:     map[int]string,
	next:      int,
	debugging: bool,
}

@(rodata)
alphabet := "abcdefghijklmnopqrstuvwxyz"

fresh_type_name :: proc(ctx: ^TypePrintCtx) -> string {
	n := ctx.next
	ctx.next += 1

	base := n % len(alphabet)
	suffix := n / len(alphabet)

	if suffix == 0 {
		return fmt.tprintf("%r", alphabet[base])
	}
	return fmt.tprintf("%r%d", alphabet[base], suffix)
}

type_string :: proc(scheme: TypeScheme, $debugging: bool) -> string {
	ctx := TypePrintCtx {
		names     = make(map[int]string),
		next      = 0,
		debugging = debugging,
	}
	defer delete(ctx.names)

	return type_string_with_ctx(&ctx, scheme)
}

type_var_string :: proc(ctx: ^TypePrintCtx, t: TypeVariable) -> string {
	if ctx.debugging {
		return fmt.tprintf("?%d", t.idx)
	}

	if name, ok := ctx.names[t.idx]; ok {
		return name
	}

	name := fresh_type_name(ctx)
	ctx.names[t.idx] = name
	return name
}

type_string_inner :: proc(ctx: ^TypePrintCtx, type: Type) -> string {
	switch t in type {
	case TypeVariable:
		return type_var_string(ctx, t)
	case TypeFunctionApplication:
		if t.constructor == .FUNCTION {
			assert(len(t.args) > 0, "cannot have less than one type arg in a function type")
			sb := strings.builder_make()
			defer strings.builder_destroy(&sb)

			param_count := len(t.args) - 1
			fmt.sbprint(&sb, "(")
			for i in 0 ..< param_count {
				if i > 0 {
					fmt.sbprint(&sb, ", ")
				}
				fmt.sbprint(&sb, type_string_inner(ctx, t.args[i]))
			}
			fmt.sbprint(&sb, ") -> ")
			fmt.sbprint(&sb, type_string_inner(ctx, t.args[len(t.args) - 1]))
			return strings.to_string(sb)
		} else {
			sb := strings.builder_make()
			defer strings.builder_destroy(&sb)
			fmt.sbprint(&sb, type_constructor_string(t.constructor))

			for arg in t.args {
				fmt.sbprint(&sb, " ")
				fmt.sbprint(&sb, type_string_inner(ctx, arg))
			}

			return strings.to_string(sb)
		}
	case TypeAny:
		return "any"
	case TypeNever:
		return "!"
	}

	panic("Internal compiler error: Invalid type")
}

type_string_with_ctx :: proc(ctx: ^TypePrintCtx, scheme: TypeScheme) -> string {
	switch s in scheme {
	case Type:
		return type_string_inner(ctx, s)
	case TypeQuantified:
		old_names := ctx.names
		old_next := ctx.next
		defer {
			ctx.names = old_names
			ctx.next = old_next
		}

		bound_names := strings.builder_make()
		defer strings.builder_destroy(&bound_names)

		for bound in s.bound {
			name := type_var_string(ctx, bound)

			if len(strings.to_string(bound_names)) > 0 {
				fmt.sbprint(&bound_names, " ")
			}
			fmt.sbprint(&bound_names, name)
		}

		body := type_string_inner(ctx, s.type)
		return fmt.tprintf("forall %s. %s", strings.to_string(bound_names), body)
	}

	panic("Internal compiler error: invalid typescheme")
}

subst_string :: proc(subst: Substitution, $debugging: bool) -> string {
	sb := strings.builder_make()

	sz := len(subst)
	count := 0
	fmt.sbprint(&sb, "{")
	for var, ty in subst {
		var_string := type_string(var, debugging)
		ty_string := type_string(ty, debugging)

		fmt.sbprintf(&sb, "%v |-> %v%s", var_string, ty_string, ", " if count < sz - 1 else "")
		count += 1
	}
	fmt.sbprint(&sb, "}")

	return fmt.tprint(strings.to_string(sb))
}

// only prints the current scope of ctx
ctx_string :: proc(ctx: ^TypeContext, $debugging: bool) -> string {
	sb := strings.builder_make()

	sz := len(ctx.bindings)
	count := 0
	fmt.sbprint(&sb, "{")
	for i in 0 ..< len(ctx.bindings) {
		b := ctx.bindings[i]
		ty_string := type_string(b.var.scheme, debugging)

		fmt.sbprintf(&sb, "%s: %v%s", b.name, ty_string, ", " if count < sz - 1 else "")
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
	typeid_map["Nil"] = tapp(.NIL)
	typeid_map["Bool"] = tapp(.BOOL)
	typeid_map["Number"] = tapp(.NUMBER)
	typeid_map["String"] = tapp(.STRING)
	typeid_map["!"] = type_never
	typeid_map["Any"] = type_any
	return typeid_map
}

typecheck_without_arena :: proc(tc: ^TypeChecker, expr: Expr) -> (type: Type, success: bool) {
	_, ty, err := infer_type(tc, expr)
	if err != nil {
		typecheck_error(tc, err.?)
		return {}, false
	}

	return ty, true
}

bind_type_to_module :: proc(
	ctx: ^TypeContext,
	module: TypeFunctionApplication,
	name: string,
	type: TypeScheme,
) {
	if module.constructor != .RECORD {
		fmt.panicf(
			"Internal compiler error: %v is not a record and thus not a module type",
			module,
		)
	}

	unimplemented()
}

register_builtin_module :: proc(tc: ^TypeChecker, module: string) {
	if !slice.contains(STD_MODULES[:], module) {
		fmt.panicf("Internal compiler error: Invalid builtin module %v", module)
	}

	string_t := tapp(.STRING)
	number_t := tapp(.NUMBER)

	mod := tapp(.RECORD)

	switch module {
	case "time":
		bind_type_to_module(tc.ctx, mod, "clock", tapp(.FUNCTION, {number_t}))
		bind_type_to_module(tc.ctx, mod, "clock_ms", tapp(.FUNCTION, {number_t}))
	case "math":
		bind_type_to_module(tc.ctx, mod, "sin", tapp(.FUNCTION, {number_t, number_t}))
		bind_type_to_module(tc.ctx, mod, "cos", tapp(.FUNCTION, {number_t, number_t}))
		bind_type_to_module(tc.ctx, mod, "tan", tapp(.FUNCTION, {number_t, number_t}))
		bind_type_to_module(tc.ctx, mod, "sqrt", tapp(.FUNCTION, {number_t, number_t}))
		bind_type_to_module(tc.ctx, mod, "ln", tapp(.FUNCTION, {number_t, number_t}))
		bind_type_to_module(tc.ctx, mod, "pow", tapp(.FUNCTION, {number_t, number_t, number_t}))
		bind_type_to_module(tc.ctx, mod, "floor", tapp(.FUNCTION, {number_t, number_t}))
		bind_type_to_module(tc.ctx, mod, "ceil", tapp(.FUNCTION, {number_t, number_t}))
		bind_type_to_module(tc.ctx, mod, "round", tapp(.FUNCTION, {number_t, number_t}))
		bind_type_to_module(tc.ctx, mod, "abs", tapp(.FUNCTION, {number_t, number_t}))
		bind_type_to_module(tc.ctx, mod, "rand", tapp(.FUNCTION, {number_t}))
	case "os":
		bind_type_to_module(tc.ctx, mod, "read", tapp(.FUNCTION, {string_t}))
		bind_type_to_module(tc.ctx, mod, "write", tapp(.FUNCTION, {string_t, string_t, string_t}))
		bind_type_to_module(tc.ctx, mod, "args", tapp(.FUNCTION, {tapp(.LIST, {string_t})}))
	case "list":
		a := fresh(tc)
		bind_type_to_module(
			tc.ctx,
			mod,
			"push",
			tquant({a}, tapp(.FUNCTION, {tapp(.LIST, {a}), tapp(.LIST, {a})})),
		)

		b := fresh(tc)
		bind_type_to_module(
			tc.ctx,
			mod,
			"pop",
			tquant({b}, tapp(.FUNCTION, {tapp(.LIST, {b}), b})),
		)

		c := fresh(tc)
		bind_type_to_module(
			tc.ctx,
			mod,
			"remove_last",
			tquant({c}, tapp(.FUNCTION, {tapp(.LIST, {c}), tapp(.LIST, {c})})),
		)

		d := fresh(tc)
		bind_type_to_module(
			tc.ctx,
			mod,
			"sort",
			tquant({d}, tapp(.FUNCTION, {tapp(.LIST, {d}), tapp(.LIST, {d})})),
		)

		bind_type_to_module(
			tc.ctx,
			mod,
			"sum",
			tapp(.FUNCTION, {tapp(.LIST, {number_t}), number_t}),
		)
	case "string":
		bind_type_to_module(tc.ctx, mod, "chomp", tapp(.FUNCTION, {string_t, string_t}))
		bind_type_to_module(
			tc.ctx,
			mod,
			"replace",
			tapp(.FUNCTION, {string_t, string_t, string_t, string_t}),
		)
		bind_type_to_module(
			tc.ctx,
			mod,
			"slice",
			tapp(.FUNCTION, {string_t, number_t, number_t, string_t}),
		)
		bind_type_to_module(tc.ctx, mod, "upcase", tapp(.FUNCTION, {string_t, string_t}))
		bind_type_to_module(tc.ctx, mod, "downcase", tapp(.FUNCTION, {string_t, string_t}))
		bind_type_to_module(tc.ctx, mod, "reverse", tapp(.FUNCTION, {string_t, string_t}))
		bind_type_to_module(tc.ctx, mod, "asciichar", tapp(.FUNCTION, {string_t, number_t}))
		bind_type_to_module(tc.ctx, mod, "asciinum", tapp(.FUNCTION, {number_t, string_t}))
	}
}

register_builtins :: proc(tc: ^TypeChecker) {
	nil_t := tapp(.NIL)
	string_t := tapp(.STRING)
	number_t := tapp(.NUMBER)
	never_t := type_never

	when ODIN_DEBUG {
		if config.log_type {
			fmt.eprintln("-- registering native function signatures")
		}
	}

	a := fresh(tc)
	bind_type(tc.ctx, "puts", tquant({a}, tapp(.FUNCTION, {a, nil_t})))
	bind_type(tc.ctx, "gets", tapp(.FUNCTION, {string_t}))

	b := fresh(tc)
	bind_type(tc.ctx, "len", tquant({b}, tapp(.FUNCTION, {b, number_t})))

	c := fresh(tc)
	bind_type(tc.ctx, "typeof", tquant({c}, tapp(.FUNCTION, {c, string_t})))

	d := fresh(tc)
	bind_type(tc.ctx, "str", tquant({d}, tapp(.FUNCTION, {d, string_t})))

	bind_type(tc.ctx, "parse", tapp(.FUNCTION, {string_t, number_t}))

	e := fresh(tc)
	bind_type(tc.ctx, "copy", tquant({e}, tapp(.FUNCTION, {e, e})))

	bind_type(tc.ctx, "panic", tapp(.FUNCTION, {string_t, never_t}))
	bind_type(tc.ctx, "dirname", tapp(.FUNCTION, {string_t}))
	bind_type(tc.ctx, "filename", tapp(.FUNCTION, {string_t}))

	when ODIN_DEBUG {
		if config.log_type {
			fmt.eprintln("-- finished registering native function signatures\n")
		}
	}
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
	push_function_scope(&tc)
	defer pop_function_scope(&tc)
	register_builtins(&tc)

	return typecheck_without_arena(&tc, expr)
}
