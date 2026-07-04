package zen

import "core:fmt"
import vmem "core:mem/virtual"
import "core:reflect"
import "core:slice"
import "core:strings"

TypeChecker :: struct {
	ctx:           ^TypeContext,
	resolutions:   ResolutionMap,
	typemap:       TypeMap,
	typevar_count: int,
	current_token: Token,
	return_type:   Type,
	pipeline_type: Type,
}

TypeMap :: map[Expr]TypeScheme

add_to_typemap :: #force_inline proc(tc: ^TypeChecker, expr: Expr, scheme: TypeScheme) {
	tc.typemap[expr] = scheme
}

add_to_typemap_after_substitution :: #force_inline proc(
	tc: ^TypeChecker,
	expr: Expr,
	subst: Substitution,
	typescheme: TypeScheme,
) {
	context.allocator = tc.typemap.allocator
	t := apply_substitution(subst, typescheme)
	add_to_typemap(tc, expr, t)
}

delete_typemap :: proc(typemap: TypeMap) {
	context.allocator = typemap.allocator
	for _, &scheme in typemap {
		free_typescheme(&scheme)
	}
	delete(typemap)
}

TypedBinding :: struct {
	name:        string,
	scope_depth: int,
	is_module:   bool,
	scheme:      TypeScheme,
}

TypeContext :: struct {
	enclosing:        ^TypeContext,
	bindings:         [dynamic]TypedBinding,
	scope_depth:      int,
	scope_boundaries: [dynamic]int,
}

make_typed_binding :: proc(
	ctx: ^TypeContext,
	name: string,
	scheme: TypeScheme,
	is_module: bool = false,
) -> TypedBinding {
	return TypedBinding {
		name = name,
		scope_depth = ctx.scope_depth,
		scheme = scheme,
		is_module = is_module,
	}
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
	RESULT,
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
	case .RESULT:
		return "Result"
	case .RECORD:
		return "Record"
	}

	fmt.panicf("invalid type constructor %v", c)
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
	case .RESULT:
		assert(args != nil, "cannot have nil type args for a result type")
		assert(len(args) == 2, "must have two type args exactly for a result type")
		return TypeFunctionApplication{constructor = constructor, args = slice.clone(args)}
	case .RECORD:
		unimplemented()
	}

	fmt.panicf("invalid type constructor %v", constructor)
}

// not necessary for the type checker as it just uses an arena; but the
// parser needs it because it allocates types for annotations
free_type :: proc(type: ^Type) {
	switch t in type {
	case TypeVariable: // nothing
	case TypeFunctionApplication:
		switch t.constructor {
		case .NUMBER, .STRING, .NIL, .BOOL: // nothing to free
		case .FUNCTION, .LIST, .RESULT, .RECORD:
			for &arg in t.args {
				free_type(&arg)
			}
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

// same as resolve_type except it errors out if the resolved binding is not a variable
@(require_results)
resolve_type_of_variable :: proc(
	tc: ^TypeChecker,
	name: string,
	node: ResolvingNode,
) -> (
	scheme: TypeScheme,
	err: ErrorMessage,
) {
	t, is_module := resolve_type_with_module_info(tc, name, node)
	if is_module {
		return {}, "Cannot use a module as a value."
	}
	return t, nil
}

@(require_results)
resolve_type :: proc(tc: ^TypeChecker, name: string, node: ResolvingNode) -> TypeScheme {
	t, _ := resolve_type_with_module_info(tc, name, node)
	return t
}

@(require_results)
resolve_type_with_module_info :: proc(
	tc: ^TypeChecker,
	name: string,
	node: ResolvingNode,
) -> (
	scheme: TypeScheme,
	is_module: bool,
) {
	// special case for builtin functions, their types are lazily loaded and
	// bound to the context only when they're called
	// we can do this safely at the top of this procedure because the native
	// functions cannot be reassigned
	if fn, ok := as_global_builtin_function(name); ok {
		return get_global_builtin_function_signature(tc, fn), false
	}

	ctx := tc.ctx
	for ctx != nil {
		for i := len(ctx.bindings) - 1; i >= 0; i -= 1 {
			b := ctx.bindings[i]
			if b.name == name && b.scope_depth <= ctx.scope_depth {
				when ODIN_DEBUG {
					if config.log_type {
						fmt.eprintfln(
							"-- grab type %v of %s from current context",
							type_string(b.scheme, true),
							name,
						)
					}
				}

				return b.scheme, b.is_module
			}
		}
		ctx = ctx.enclosing
	}

	// if the variable doesn't exist in the context, there is a chance it is
	// in the global context resolved previously by the resolver; see if its
	// there
	if _, exists := tc.resolutions[node]; exists {
		alpha := fresh(tc)

		// NOTE: technically the type is in the global context, but for now we're
		// just keeping it in whichever context it is required in
		bind_type(tc.ctx, strings.clone(name), alpha)

		// NOTE: is_module is hardcoded to false because this branch is only
		// activated for hoisted global values and modules are currently NOT
		// hoisted, may need changing if I ever decide to hoist modules
		return alpha, false
	}

	// nothing found at all
	fmt.panicf("Couldn't resolve variable '%v' in typechecker", name)
}

bind_type :: proc(ctx: ^TypeContext, name: string, scheme: TypeScheme, is_module: bool = false) {
	append(&ctx.bindings, make_typed_binding(ctx, name, scheme, is_module))

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
			free_typescheme(&c.bindings[i].scheme)
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
			scheme := c.bindings[i].scheme
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

// only the TypeFunctionApplication needs to be cloned as the other types don't
// carry anything allocated on the heap
clone_type :: proc(t: Type) -> Type {
	#partial switch v in t {
	case TypeFunctionApplication:
		new_args := make([]Type, len(v.args))
		defer delete(new_args)
		for arg, i in v.args {
			new_args[i] = clone_type(arg)
		}
		return tapp(v.constructor, new_args)
	}
	return t
}

apply_substitution_type :: proc(subst: Substitution, type: Type) -> Type {
	switch t in type {
	case TypeVariable:
		result: Type = t
		for {
			tv, tv_ok := result.(TypeVariable)
			if !tv_ok {
				return clone_type(result)
			}
			val, ok := subst[tv]
			if !ok {
				return result
			}
			result = val
		}
	case TypeFunctionApplication:
		new_args := make([]Type, len(t.args))
		defer delete(new_args)
		for _, i in t.args {
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
			b.scheme = apply_substitution(subst, b.scheme)
		}
		c = c.enclosing
	}

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
				fmt.eprint(color_yellow("WARNING"))
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
// also handles the Never type which needs to know what is the expected type
// to work soundly
// NOTE: try_unify() is directional and NOT commutative, unlike unify() which is
@(require_results)
try_unify :: proc(
	expected: Type,
	checking: Type,
	expected_expression_name: string,
) -> (
	Substitution,
	ErrorMessage,
) {
	// allow Never to unify with anything if it is the type we're checking
	// for conformation; but disallow that if we are expecting it as a
	// result
	if is_type_never(checking) {
		when ODIN_DEBUG {
			if config.log_type {
				fmt.eprintfln(
					"-- unify %v with %v trivially",
					type_string(expected, true),
					type_string(checking, true),
				)
			}
		}

		return nil, nil
	}

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
			if is_type_function_application(expected) && is_type_function_application(checking) {
				t1 := as_type_function_application(expected)
				t2 := as_type_function_application(checking)

				if t1.constructor == .FUNCTION &&
				   t2.constructor == .FUNCTION &&
				   len(t1.args) != len(t2.args) {
					expected_count := len(t1.args) - 1
					checking_count := len(t2.args) - 1
					name :=
						expected_expression_name if expected_expression_name != "" else "function"

					return nil, fmt.tprintf(
						"Expected %d argument%s for %v but got %d.",
						checking_count,
						"" if checking_count == 1 else "s",
						name,
						expected_count,
					)
				}
			}

			never_string := type_string(type_never, false)
			expected_type_string := type_string(expected, false)

			return nil, fmt.tprintf(
				"Expected %v to be %v, got %v.",
				expected_expression_name if expected_expression_name != "" else "expression",
				fmt.tprintf("a diverging expression of type %v", never_string) if expected_type_string == never_string else fmt.tprintf("of type %v", expected_type_string),
				type_string(checking, false),
			)
		}
	}

	return subst, nil
}

/* Wrapper for `unify` used for branching constructs like `if` and `switch`,
allows for the Never type to stand in any of those branches. */
join :: proc(
	a: Type,
	b: Type,
	expected_expression_name: string,
) -> (
	subst: Substitution,
	type: Type,
	err: ErrorMessage,
) {
	if is_type_never(a) {
		return nil, b, nil
	}

	if is_type_never(b) {
		return nil, a, nil
	}

	s, uni_err := unify(a, b)
	if uni_err != nil {
		switch uni_err {
		case .INFINITE_TYPE:
			return nil, {}, fmt.tprintf("Cannot unify type %v with %v as that would require an infinite type.", type_string(a, false), type_string(b, false))
		case .MISMATCH:
			return nil, {}, fmt.tprintf("Type %v in %v is not compatible with type %v.", type_string(b, false), expected_expression_name if expected_expression_name != "" else "expression", type_string(a, false))
		}
	}
	return s, apply_substitution(s, a), nil
}

// allocates a map
@(require_results)
unify :: proc(a: Type, b: Type) -> (subst: Substitution, err: Maybe(UnificationError)) {
	if is_type_any(a) {
		// The only thing Any cannot unify with is Never.
		if is_type_never(b) {
			return nil, .MISMATCH
		}

		when ODIN_DEBUG {
			if config.log_type {
				fmt.eprintfln(
					"-- unify %v with %v trivially",
					type_string(a, true),
					type_string(b, true),
				)
			}
		}

		// Any unifies with anything and returns a substitution that turns the
		// other type into Any. What TypeScript does. Unsound, but it works.
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

	if is_type_never(a) {
		// Never is treated the exact same way as a nullary type function
		// application in commutative/non-directional unification.
		if !is_type_never(b) {
			return nil, .MISMATCH
		}

		when ODIN_DEBUG {
			if config.log_type {
				fmt.eprintfln(
					"-- unify %v with %v trivially",
					type_string(a, true),
					type_string(b, true),
				)
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

typecheck_error :: proc(tc: ^TypeChecker, message: string) {
	token := tc.current_token
	print_error(token, message)
}

// is the expression a syntactic "value"?
is_value :: proc(expr: Expr) -> bool {
	if expr == nil {return false}

	#partial switch e in expr {
	case ^LiteralExpr, ^FunctionExpr, ^VariableExpr, ^GetExpr:
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
	expected_expression_name: string = "",
) -> (
	subst: Substitution,
	err: ErrorMessage,
) {
	if expr == nil {return nil, nil}

	switch e in expr {
	case ^AssignExpr:
		tc.current_token = e.token
		s1, t1 := infer_type(tc, e.value) or_return
		apply_substitution(s1, tc.ctx)
		found := resolve_type_of_variable(tc, e.name.lexeme, e) or_return
		ty := instantiate(tc, found)
		s2 := try_unify(ty, t1, "assigned value") or_return
		apply_substitution(s2, tc.ctx)
		sn := try_unify(type, apply_substitution(s2, ty), "assignment") or_return

		s := combine_substitutions(sn, combine_substitutions(s2, s1))
		add_to_typemap_after_substitution(tc, expr, s, type)
		return s, nil
	case ^BinaryExpr:
		tc.current_token = e.token
		operator := e.operator
		s := make(Substitution)

		#partial switch operator.type {
		case .PLUS, .MINUS, .STAR, .SLASH, .PERCENT:
			num := tapp(.NUMBER)
			s1 := check_type(
				tc,
				e.left,
				num,
				fmt.tprintf("left operand to '%v'", operator.lexeme),
			) or_return
			apply_substitution(s1, tc.ctx)
			s2 := check_type(
				tc,
				e.right,
				num,
				fmt.tprintf("right operand to '%v'", operator.lexeme),
			) or_return
			apply_substitution(s2, tc.ctx)
			sn := try_unify(type, num, expected_expression_name) or_return
			s = combine_substitutions(sn, combine_substitutions(s2, combine_substitutions(s1, s)))
		case .DOT_DOT:
			str := tapp(.STRING)
			s1 := check_type(tc, e.left, str, "left operand to '..'") or_return
			apply_substitution(s1, tc.ctx)
			s2 := check_type(tc, e.right, str, "right operand to '..'") or_return
			apply_substitution(s2, tc.ctx)
			sn := try_unify(type, str, expected_expression_name) or_return
			s = combine_substitutions(sn, combine_substitutions(s2, combine_substitutions(s1, s)))
		case .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL:
			bool_ := tapp(.BOOL)
			num := tapp(.NUMBER)
			s1 := check_type(
				tc,
				e.left,
				num,
				fmt.tprintf("left operand to '%v'", operator.lexeme),
			) or_return
			apply_substitution(s1, tc.ctx)
			s2 := check_type(
				tc,
				e.right,
				num,
				fmt.tprintf("right operand to '%v'", operator.lexeme),
			) or_return
			apply_substitution(s2, tc.ctx)
			sn := try_unify(type, bool_, expected_expression_name) or_return
			s = combine_substitutions(sn, combine_substitutions(s2, combine_substitutions(s1, s)))
		case .EQUAL_EQUAL, .BANG_EQUAL:
			s1, _ := infer_type(tc, e.left) or_return
			apply_substitution(s1, tc.ctx)
			s2, _ := infer_type(tc, e.right) or_return
			apply_substitution(s2, tc.ctx)
			sn := try_unify(type, tapp(.BOOL), expected_expression_name) or_return
			s = combine_substitutions(sn, combine_substitutions(s2, combine_substitutions(s1, s)))
		case:
			fmt.panicf("Invalid binary operator '%s'.", e.operator.lexeme)
		}

		add_to_typemap_after_substitution(tc, expr, s, type)
		return s, nil
	case ^BlockExpr:
		tc.current_token = e.token
		push_scope(tc.ctx)
		s := make(Substitution)
		if e.expression != nil {
			s = check_type(tc, e.expression, type, expected_expression_name) or_return // infer body with expected type
		} else {
			s = try_unify(type, tapp(.NIL), expected_expression_name) or_return // infer body with expected type
		}
		pop_scope(tc.ctx)

		add_to_typemap_after_substitution(tc, expr, s, type)
		return s, nil
	case ^BreakExpr:
		tc.current_token = e.token
		s := try_unify(type, type_never, expected_expression_name) or_return
		add_to_typemap_after_substitution(tc, expr, s, type)
		return s, nil
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
		ret_type := fresh(tc)
		all_args[len(arg_types)] = ret_type
		func_type := tapp(.FUNCTION, all_args)

		// handle method calls vs regular ones
		s := make(Substitution)

		// check called function's type
		callee_name := "called value"
		#partial switch e in callee {
		case ^VariableExpr:
			callee_name = fmt.tprintf("'%s'", e.name.lexeme)
		case ^FunctionExpr:
			if fn_name, ok := e.bound_to.?; ok {
				callee_name = fmt.tprintf("function '%s'", fn_name.lexeme)
			}
		case ^GetExpr:
			callee_name = fmt.tprintf("'%s'", e.property.lexeme)
		}
		s_callee := check_type(tc, callee, func_type, callee_name) or_return
		apply_substitution(s_callee, tc.ctx)
		s = combine_substitutions(s_callee, s)

		// typecheck each argument
		for arg, idx in arguments {
			expected := apply_substitution(s, arg_types[idx])
			s_arg := check_type(tc, arg, expected, "argument") or_return
			apply_substitution(s_arg, tc.ctx)
			s = combine_substitutions(s_arg, s)
		}
		sn := try_unify(type, apply_substitution(s, ret_type), expected_expression_name) or_return

		s = combine_substitutions(sn, s)
		add_to_typemap_after_substitution(tc, expr, s, type)
		return s, nil
	case ^ContinueExpr:
		tc.current_token = e.token
		s := try_unify(type, type_never, expected_expression_name) or_return
		add_to_typemap_after_substitution(tc, expr, s, type)
		return s, nil
	case ^ExitExpr:
		tc.current_token = e.token
		s1 := check_type(tc, e.code, tapp(.NUMBER), "exit code") or_return
		sn := try_unify(type, type_never, expected_expression_name) or_return
		s := combine_substitutions(sn, s1)
		add_to_typemap_after_substitution(tc, expr, s, type)
		return s, nil
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

		s_body, _ := infer_type(tc, e.body.expression) or_return
		apply_substitution(s_body, tc.ctx)
		s = combine_substitutions(s_body, s)
		pop_scope(tc.ctx)
		sn := try_unify(type, tapp(.NIL), expected_expression_name) or_return

		s = combine_substitutions(sn, s)
		add_to_typemap_after_substitution(tc, expr, s, type)
		return s, nil
	case ^ForInExpr:
		tc.current_token = e.token
		push_scope(tc.ctx)

		loop_var := fresh(tc)
		bind_type(tc.ctx, strings.clone(e.var_name.lexeme), loop_var) // fresh typevar for for-in loop variable
		any_list := tapp(.LIST, {loop_var})
		s_iter := check_type(tc, e.iterable, any_list, "iterable") or_return // for-in only works for lists
		apply_substitution(s_iter, tc.ctx)
		s_body, _ := infer_type(tc, e.body.expression) or_return
		apply_substitution(s_body, tc.ctx)
		pop_scope(tc.ctx)
		sn := try_unify(type, tapp(.NIL), expected_expression_name) or_return

		s := combine_substitutions(sn, combine_substitutions(s_body, s_iter))
		add_to_typemap_after_substitution(tc, expr, s, type)
		return s, nil
	case ^IfExpr:
		tc.current_token = e.token
		s1 := check_type(tc, e.condition, tapp(.BOOL), "if condition") or_return // the condition can be any value
		apply_substitution(s1, tc.ctx)
		s2, then_type := infer_type(tc, e.then_branch.expression) or_return
		apply_substitution(s2, tc.ctx)
		s := combine_substitutions(s2, s1)

		if e.else_branch != nil {
			// both branches must return same type
			s3, else_type := infer_type(tc, e.else_branch.expression) or_return
			apply_substitution(s3, tc.ctx)
			s = combine_substitutions(s3, s)
			s4, joined := join(
				apply_substitution(s, then_type),
				else_type,
				"if expression branch",
			) or_return
			s = combine_substitutions(s4, s)
			sn := try_unify(type, joined, expected_expression_name) or_return
			s = combine_substitutions(sn, s)
		} else {
			// evaluate to nil if no else branch
			sn := try_unify(
				apply_substitution(s, type),
				tapp(.NIL),
				expected_expression_name,
			) or_return
			s = combine_substitutions(sn, s)
		}

		add_to_typemap_after_substitution(tc, expr, s, type)
		return s, nil
	case ^GetExpr:
		tc.current_token = e.property
		receiver := e.receiver
		property := e.property

		s := make(Substitution)
		// handle the module case
		if v, ok := receiver.(^VariableExpr); ok {
			_, is_module := resolve_type_with_module_info(tc, v.name.lexeme, v)
			if !is_module {
				_, t := infer_type(tc, receiver) or_return
				return nil, fmt.tprintf(
					"Expected dot-accessed value to be a module, got %v.",
					type_string(t, false),
				)
			}

			module, ok2 := as_builtin_module(v.name.lexeme)
			if !ok2 {
				fmt.panicf("couldn't find %v in builtin module", v.name.lexeme)
			}

			// the type is lazily resolved from within the module
			poly_sig := get_module_function_signature(tc, module, property.lexeme) or_return
			sig := instantiate(tc, poly_sig) // instantiate the function; cuz it can be polymorphic
			s = try_unify(type, sig, expected_expression_name) or_return
		} else {
			// only reason we're inferring the type at all is to provide the
			// error message some extra context
			_, t := infer_type(tc, receiver) or_return
			return nil, fmt.tprintf(
				"Expected dot-accessed value to be a module, got %v.",
				type_string(t, false),
			)
		}

		add_to_typemap_after_substitution(tc, expr, s, type)
		return s, nil
	case ^GroupingExpr:
		tc.current_token = e.token
		s := check_type(tc, e.expression, type, "grouping expression") or_return
		add_to_typemap_after_substitution(tc, expr, s, type)
		return s, nil
	case ^LogicalExpr:
		tc.current_token = e.token
		bool_ := tapp(.BOOL)
		s1 := check_type(
			tc,
			e.left,
			bool_,
			fmt.tprintf("left operand to '%v'", e.operator.lexeme),
		) or_return
		apply_substitution(s1, tc.ctx)
		s2 := check_type(
			tc,
			e.right,
			bool_,
			fmt.tprintf("right operand to '%v'", e.operator.lexeme),
		) or_return
		apply_substitution(s2, tc.ctx)
		sn := try_unify(type, bool_, expected_expression_name) or_return
		s := combine_substitutions(sn, combine_substitutions(s2, s1))
		add_to_typemap_after_substitution(tc, expr, s, type)
		return s, nil
	case ^ListExpr:
		tc.current_token = e.token
		if len(e.elements) == 0 {
			s := try_unify(type, tapp(.LIST, {fresh(tc)}), expected_expression_name) or_return
			return s, nil
		}

		elem := fresh(tc)
		s := make(Substitution)
		for element in e.elements {
			s1 := check_type(tc, element, apply_substitution(s, elem), "list element") or_return
			s = combine_substitutions(s1, s)
			apply_substitution(s, tc.ctx)
		}
		sn := try_unify(
			type,
			tapp(.LIST, {apply_substitution(s, elem)}),
			expected_expression_name,
		) or_return

		s = combine_substitutions(sn, s)
		add_to_typemap_after_substitution(tc, expr, s, type)
		return s, nil
	case ^ItExpr:
		tc.current_token = e.token

		s := try_unify(type, tc.pipeline_type, expected_expression_name) or_return
		add_to_typemap_after_substitution(tc, expr, s, type)
		return s, nil
	case ^PipeExpr:
		tc.current_token = e.token
		left := e.left
		right := e.right

		s1, t1 := infer_type(tc, left) or_return
		apply_substitution(s1, tc.ctx)
		tc.pipeline_type = t1
		s2, t2 := infer_type(tc, right) or_return
		apply_substitution(s2, tc.ctx)
		tc.pipeline_type = t2
		s := combine_substitutions(s2, s1)
		sn := try_unify(type, t2, expected_expression_name) or_return

		s = combine_substitutions(sn, s)
		add_to_typemap_after_substitution(tc, expr, s, type)
		return s, nil
	case ^EchoExpr:
		tc.current_token = e.token
		s1, t1 := infer_type(tc, e.expr) or_return
		sn := try_unify(type, t1, expected_expression_name) or_return // print returns what it printed

		s := combine_substitutions(sn, s1)
		add_to_typemap_after_substitution(tc, expr, s, type)
		return s, nil
	case ^ReturnExpr:
		tc.current_token = e.token

		s := make(Substitution)
		if e.value != nil {
			s = check_type(tc, e.value, tc.return_type, "return value") or_return
			apply_substitution(s, tc.ctx)
			sn := try_unify(type, type_never, expected_expression_name) or_return // return expression itself has type `!`
			s = combine_substitutions(sn, s)
		} else {
			s = try_unify(tc.return_type, tapp(.NIL), expected_expression_name) or_return
			apply_substitution(s, tc.ctx)
			sn := try_unify(type, type_never, expected_expression_name) or_return
			s = combine_substitutions(sn, s)
		}

		add_to_typemap_after_substitution(tc, expr, s, type)
		return s, nil
	case ^SubscriptExpr:
		tc.current_token = e.token
		receiver := e.receiver
		index := e.index

		// can only subscript lists
		beta := fresh(tc)
		s1 := check_type(tc, receiver, tapp(.LIST, {beta}), "subscripted expression") or_return
		apply_substitution(s1, tc.ctx)
		s2 := check_type(tc, index, tapp(.NUMBER), "subscript index") or_return
		apply_substitution(s2, tc.ctx)
		s := combine_substitutions(s2, s1)
		sn := try_unify(type, apply_substitution(s, beta), expected_expression_name) or_return

		s = combine_substitutions(sn, s)
		add_to_typemap_after_substitution(tc, expr, s, type)
		return s, nil
	case ^SubscriptSetExpr:
		tc.current_token = e.token
		receiver := e.receiver
		index := e.index
		value := e.value

		beta := fresh(tc)
		s1 := check_type(tc, receiver, tapp(.LIST, {beta}), "subscripted expression") or_return
		apply_substitution(s1, tc.ctx)
		s2 := check_type(tc, index, tapp(.NUMBER), "subscript index") or_return
		apply_substitution(s2, tc.ctx)
		s3 := check_type(
			tc,
			value,
			apply_substitution(s2, apply_substitution(s1, beta)),
			"assigned expression",
		) or_return
		apply_substitution(s3, tc.ctx)
		s := combine_substitutions(s3, combine_substitutions(s2, s1))
		sn := try_unify(type, apply_substitution(s, beta), expected_expression_name) or_return

		s = combine_substitutions(sn, s)
		add_to_typemap_after_substitution(tc, expr, s, type)
		return s, nil
	case ^LiteralExpr:
		tc.current_token = e.token

		s := make(Substitution)
		// just unify with the matching literal constructor
		switch l in e.value {
		case f64:
			s = try_unify(type, tapp(.NUMBER), expected_expression_name) or_return
		case string:
			s = try_unify(type, tapp(.STRING), expected_expression_name) or_return
		case bool:
			s = try_unify(type, tapp(.BOOL), expected_expression_name) or_return
		case:
			s = try_unify(type, tapp(.NIL), expected_expression_name) or_return
		}

		add_to_typemap_after_substitution(tc, expr, s, type)
		return s, nil
	case ^VariableExpr:
		tc.current_token = e.token
		found := resolve_type_of_variable(tc, e.name.lexeme, e) or_return // find typescheme in the context (or resolution map)
		found_t := instantiate(tc, found) // instantiate the found scheme
		s := try_unify(type, found_t, expected_expression_name) or_return // unify typevar with the found type
		add_to_typemap_after_substitution(tc, expr, s, type)
		return s, nil
	case ^FunctionExpr:
		tc.current_token = e.token
		bound_to := e.bound_to
		params := e.params
		body := e.body
		return_type := e.return_type

		annotation_types_to_fresh_vars := make(map[TypeVariable]TypeVariable)
		defer delete(annotation_types_to_fresh_vars)

		param_types: []Type
		if len(params) != 0 {
			param_types = make([]Type, len(params))

			for param, idx in params {
				if _, ok := param.type.?; !ok {
					param_types[idx] = fresh(tc)
					continue
				}
				t := param.type.?

				if is_type_variable(t) {
					annotation_tvar := as_type_variable(t)
					if existing, ok := annotation_types_to_fresh_vars[annotation_tvar]; ok {
						param_types[idx] = existing
						continue
					}

					freshified := fresh(tc)
					annotation_types_to_fresh_vars[annotation_tvar] = freshified
					param_types[idx] = freshified
				} else {
					param_types[idx] = t
				}
			}
		}

		ret_type: Type
		if ret, ok := return_type.?; ok {
			if is_type_variable(ret) {
				annotation_tvar := as_type_variable(ret)
				if existing, annot_ok := annotation_types_to_fresh_vars[annotation_tvar];
				   annot_ok {
					ret_type = existing
				} else {
					freshified := fresh(tc)
					annotation_types_to_fresh_vars[annotation_tvar] = freshified
					ret_type = freshified
				}
			} else {
				ret_type = ret
			}
		} else {
			ret_type = fresh(tc)
		}

		// last arg is return type
		all_args := make([]Type, len(param_types) + 1)
		if len(param_types) != 0 {copy(all_args, param_types)}
		all_args[len(param_types)] = ret_type
		func_type := tapp(.FUNCTION, all_args)

		// unify with expected type first
		s1 := try_unify(type, func_type, expected_expression_name) or_return
		apply_substitution(s1, tc.ctx)

		// start the function scope
		push_function_scope(tc)

		// pre-bind the fn name (if it is named) to allow recursion
		if name, ok := bound_to.?; ok {
			bind_type(tc.ctx, strings.clone(name.lexeme), apply_substitution(s1, func_type))
		}

		for param, idx in params {
			bind_type(
				tc.ctx,
				strings.clone(param.name.lexeme),
				apply_substitution(s1, param_types[idx]),
			)
		}

		// set return type context to allow ReturnExpr to check against it
		old_ret := tc.return_type
		tc.return_type = apply_substitution(s1, ret_type)
		defer tc.return_type = old_ret

		s2 := check_type(
			tc,
			body,
			apply_substitution(s1, ret_type),
			fmt.tprintf("return value of function '%s'", bound_to.?.lexeme) if bound_to != nil else "function body",
		) or_return
		apply_substitution(s2, tc.ctx)
		pop_function_scope(tc)

		s := combine_substitutions(s2, s1)
		add_to_typemap_after_substitution(tc, expr, s, type)
		return s, nil
	case ^SequenceExpr:
		tc.current_token = e.token
		s1, _ := infer_type(tc, e.left) or_return // infer left with fresh var
		apply_substitution(s1, tc.ctx)
		if e.right == nil {
			// seq evaluates to nil if there is no right side
			sn := try_unify(type, tapp(.NIL), expected_expression_name) or_return
			return combine_substitutions(sn, s1), nil
		}
		s2 := check_type(
			tc,
			e.right,
			apply_substitution(s1, type),
			expected_expression_name,
		) or_return // infer right with expected type
		apply_substitution(s2, tc.ctx)

		s := combine_substitutions(s2, s1)
		add_to_typemap_after_substitution(tc, expr, s, type)
		return s, nil
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

		switch_result_type: Type = fresh(tc)
		apply_substitution(s, tc.ctx)
		for c in e.cases {
			s1 := check_type(
				tc,
				c.condition,
				apply_substitution(s, cond_type),
				"switch condition",
			) or_return
			s = combine_substitutions(s1, s)
			apply_substitution(s, tc.ctx)
			s2, case_type := infer_type(tc, c.body) or_return
			s = combine_substitutions(s2, s)
			s3, joined := join(switch_result_type, case_type, "switch result") or_return
			switch_result_type = joined
			s = combine_substitutions(s3, s)
			apply_substitution(s, tc.ctx)
		}
		s_else, type_else := infer_type(tc, e.else_branch) or_return
		s = combine_substitutions(s_else, s)
		sn, _ := join(switch_result_type, type_else, "else branch") or_return
		s = combine_substitutions(sn, s)
		add_to_typemap_after_substitution(tc, expr, s, type)
		return s, nil
	case ^UnaryExpr:
		tc.current_token = e.token

		s := make(Substitution)
		#partial switch e.operator.type {
		case .MINUS:
			s = check_type(tc, e.right, tapp(.NUMBER), "operand to '-'") or_return
			sn := try_unify(type, tapp(.NUMBER), expected_expression_name) or_return
			s = combine_substitutions(sn, s)
		case .NOT:
			s = check_type(tc, e.right, tapp(.BOOL), "operand to 'not'") or_return
			sn := try_unify(type, tapp(.BOOL), expected_expression_name) or_return
			s = combine_substitutions(sn, s)
		case:
			fmt.panicf("Unknown unary operator '%s'", e.operator.lexeme)
		}

		add_to_typemap_after_substitution(tc, expr, s, type)
		return s, nil
	case ^UseExpr:
		tc.current_token = e.token
		name := e.name
		mod_type := e.type

		switch mod_type {
		case .BUILTIN:
			// modules are NOT first class values so they have no type
			bind_type(tc.ctx, strings.clone(name), {}, is_module = true)
		case .USER:
			unimplemented()
		}
		s := try_unify(type, tapp(.NIL), expected_expression_name) or_return
		add_to_typemap_after_substitution(tc, expr, s, type)
		return s, nil
	case ^VarDeclExpr:
		tc.current_token = e.token
		s := make(Substitution)
		for binding in e.bindings {
			beta := binding.type.? or_else fresh(tc)

			if binding.initializer != nil {
				s1 := check_type(tc, binding.initializer, beta, "variable initializer") or_return

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
		sn := try_unify(type, tapp(.NIL), expected_expression_name) or_return // VarDeclExpr itself evaluates to nil

		s = combine_substitutions(sn, s)
		add_to_typemap_after_substitution(tc, expr, s, type)
		return s, nil
	case ^WhileExpr:
		tc.current_token = e.token
		push_scope(tc.ctx)
		s1, _ := infer_type(tc, e.condition) or_return
		apply_substitution(s1, tc.ctx)
		s2, _ := infer_type(tc, e.body.expression) or_return
		apply_substitution(s2, tc.ctx)
		pop_scope(tc.ctx)
		sn := try_unify(type, tapp(.NIL), expected_expression_name) or_return

		s := combine_substitutions(sn, combine_substitutions(s2, s1))
		add_to_typemap_after_substitution(tc, expr, s, type)
		return s, nil
	}

	panic("invalid AST node")
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

			if len(t.args) > 0 {
				strings.write_rune(&sb, '[')
				for arg, idx in t.args {
					fmt.sbprint(&sb, type_string_inner(ctx, arg))
					if idx < len(t.args) - 1 {fmt.sbprint(&sb, ", ")}
				}
				strings.write_rune(&sb, ']')
			}

			return strings.to_string(sb)
		}
	case TypeAny:
		return "Any"
	case TypeNever:
		return "Never"
	}

	panic("Invalid type")
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

	panic("invalid typescheme")
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
		ty_string := type_string(b.scheme, debugging)

		fmt.sbprintf(&sb, "%s: %v%s", b.name, ty_string, ", " if count < sz - 1 else "")
		count += 1
	}
	fmt.sbprint(&sb, "}")

	return fmt.tprint(strings.to_string(sb))
}

get_module_function_signature :: proc(
	tc: ^TypeChecker,
	module: BuiltinModule,
	fn_name: string,
) -> (
	scheme: TypeScheme,
	err: ErrorMessage,
) {
	string_t := tapp(.STRING)
	number_t := tapp(.NUMBER)
	bool_t := tapp(.BOOL)

	switch module {
	case .TIME:
		switch fn_name {
		case "clock":
			return tapp(.FUNCTION, {number_t}), nil
		case "clock_ms":
			return tapp(.FUNCTION, {number_t}), nil
		}
	case .MATH:
		switch fn_name {
		case "sin":
			return tapp(.FUNCTION, {number_t, number_t}), nil
		case "cos":
			return tapp(.FUNCTION, {number_t, number_t}), nil
		case "tan":
			return tapp(.FUNCTION, {number_t, number_t}), nil
		case "sqrt":
			return tapp(.FUNCTION, {number_t, number_t}), nil
		case "ln":
			return tapp(.FUNCTION, {number_t, number_t}), nil
		case "pow":
			return tapp(.FUNCTION, {number_t, number_t, number_t}), nil
		case "floor":
			return tapp(.FUNCTION, {number_t, number_t}), nil
		case "ceil":
			return tapp(.FUNCTION, {number_t, number_t}), nil
		case "round":
			return tapp(.FUNCTION, {number_t, number_t}), nil
		case "abs":
			return tapp(.FUNCTION, {number_t, number_t}), nil
		case "rand":
			return tapp(.FUNCTION, {number_t}), nil
		}
	case .OS:
		switch fn_name {
		case "read":
			return tapp(.FUNCTION, {string_t, string_t}), nil
		case "write":
			return tapp(.FUNCTION, {string_t, string_t, string_t, string_t}), nil
		case "args":
			return tapp(.FUNCTION, {tapp(.LIST, {string_t})}), nil
		}
	case .LIST:
		switch fn_name {
		case "push":
			a := fresh(tc)
			return tquant({a}, tapp(.FUNCTION, {tapp(.LIST, {a}), a, tapp(.LIST, {a})})), nil
		case "pop":
			a := fresh(tc)
			return tquant({a}, tapp(.FUNCTION, {tapp(.LIST, {a}), a})), nil
		case "remove_last":
			a := fresh(tc)
			return tquant({a}, tapp(.FUNCTION, {tapp(.LIST, {a}), tapp(.LIST, {a})})), nil
		case "sort":
			a := fresh(tc)
			return tquant({a}, tapp(.FUNCTION, {tapp(.LIST, {a}), tapp(.LIST, {a})})), nil
		case "sum":
			return tapp(.FUNCTION, {tapp(.LIST, {number_t}), number_t}), nil
		}
	case .STRING:
		switch fn_name {
		case "chomp":
			return tapp(.FUNCTION, {string_t, string_t}), nil
		case "replace":
			return tapp(.FUNCTION, {string_t, string_t, string_t, string_t}), nil
		case "slice":
			return tapp(.FUNCTION, {string_t, number_t, number_t, string_t}), nil
		case "index":
			return tapp(.FUNCTION, {string_t, number_t, string_t}), nil
		case "chars":
			return tapp(.FUNCTION, {string_t, tapp(.LIST, {string_t})}), nil
		case "upcase":
			return tapp(.FUNCTION, {string_t, string_t}), nil
		case "downcase":
			return tapp(.FUNCTION, {string_t, string_t}), nil
		case "reverse":
			return tapp(.FUNCTION, {string_t, string_t}), nil
		case "asciichar":
			return tapp(.FUNCTION, {number_t, string_t}), nil
		case "asciinum":
			return tapp(.FUNCTION, {string_t, number_t}), nil
		case "byte_count":
			return tapp(.FUNCTION, {string_t, number_t}), nil
		}
	case .RESULT:
		switch fn_name {
		case "ok?":
			t := fresh(tc)
			e := fresh(tc)
			return tquant({t, e}, tapp(.FUNCTION, {tapp(.RESULT, {t, e}), bool_t})), nil
		case "err?":
			t := fresh(tc)
			e := fresh(tc)
			return tquant({t, e}, tapp(.FUNCTION, {tapp(.RESULT, {t, e}), bool_t})), nil
		case "unwrap":
			t := fresh(tc)
			e := fresh(tc)
			return tquant({t, e}, tapp(.FUNCTION, {tapp(.RESULT, {t, e}), t})), nil
		case "unwrap_or":
			t := fresh(tc)
			e := fresh(tc)
			return tquant({t, e}, tapp(.FUNCTION, {tapp(.RESULT, {t, e}), t, t})), nil
		}
	}

	name, ok := reflect.enum_name_from_value(module)
	if !ok {
		fmt.panicf("unknown builtin module %v", module)
	}
	lower := strings.to_lower(name)
	defer delete(lower)

	return {}, fmt.tprintf("Function '%v' does not exist in builtin module '%v'.", fn_name, lower)
}

get_global_builtin_function_signature :: proc(
	tc: ^TypeChecker,
	fn: GlobalBuiltinFunction,
) -> TypeScheme {
	nil_t := tapp(.NIL)
	string_t := tapp(.STRING)
	number_t := tapp(.NUMBER)
	never_t := type_never

	switch fn {
	case .PRINT:
		a := fresh(tc)
		return tquant({a}, tapp(.FUNCTION, {a, nil_t}))
	case .PUTS:
		a := fresh(tc)
		return tquant({a}, tapp(.FUNCTION, {a, nil_t}))
	case .GETS:
		return tapp(.FUNCTION, {string_t})
	case .PANIC:
		return tapp(.FUNCTION, {string_t, never_t})
	case .ASSERT:
		a := fresh(tc)
		return tquant({a}, tapp(.FUNCTION, {a, nil_t}))
	case .LEN:
		// NOTE: this should be only for strings and lists, not everything;
		// but right now i have no way to differentiate them with standard HM.
		// Someday if I add typeclasses that would be doable
		a := fresh(tc)
		return tquant({a}, tapp(.FUNCTION, {a, number_t}))
	case .TYPEOF:
		// NOTE: I might turn typeof into an operator, so that i can use
		// type_string() to create a string representation of the type directly
		// at compile time
		a := fresh(tc)
		return tquant({a}, tapp(.FUNCTION, {a, string_t}))
	case .STR:
		a := fresh(tc)
		return tquant({a}, tapp(.FUNCTION, {a, string_t}))
	case .PARSE:
		return tapp(.FUNCTION, {string_t, number_t})
	case .COPY:
		a := fresh(tc)
		return tquant({a}, tapp(.FUNCTION, {a, a}))
	case .DIRNAME:
		return tapp(.FUNCTION, {string_t})
	case .FILENAME:
		return tapp(.FUNCTION, {string_t})
	case .OK:
		t := fresh(tc)
		e := fresh(tc)
		return tquant({t, e}, tapp(.FUNCTION, {t, tapp(.RESULT, {t, e})}))
	case .ERR:
		e := fresh(tc)
		t := fresh(tc)
		return tquant({t, e}, tapp(.FUNCTION, {e, tapp(.RESULT, {t, e})}))
	}

	fmt.panicf("undefined global-scoped native function '%v'", fn)
}

typecheck_without_arena :: proc(tc: ^TypeChecker, expr: Expr) -> (type: Type, success: bool) {
	_, ty, err := infer_type(tc, expr)
	if err != nil {
		typecheck_error(tc, err.?)
		return {}, false
	}

	return ty, true
}

typecheck :: proc(expr: Expr, resolutions: ResolutionMap) -> (typemap: TypeMap, success: bool) {
	// create separate arena to allocate everything for typechecker
	arena: vmem.Arena
	arena_err := vmem.arena_init_growing(&arena)
	ensure(arena_err == nil)
	defer vmem.arena_destroy(&arena)

	arena_alloc := vmem.arena_allocator(&arena)
	prev_alloc := context.allocator
	context.allocator = arena_alloc

	tc := TypeChecker {
		ctx           = nil,
		resolutions   = resolutions,
		typemap       = make(TypeMap, prev_alloc),
		typevar_count = 0,
		current_token = {},
		pipeline_type = {},
		return_type   = {},
	}
	push_function_scope(&tc)
	defer pop_function_scope(&tc)

	_, ok := typecheck_without_arena(&tc, expr)
	if !ok {
		delete_typemap(tc.typemap)
		return nil, false
	}
	return tc.typemap, true
}

typecheck_full :: proc(
	vm: ^VM,
	expr: Expr,
	resolutions: ResolutionMap,
) -> (
	typemap: TypeMap,
	success: bool,
) {
	when ODIN_DEBUG {
		if config.log_type {
			fmt.eprintln("-- typechecker begin")
		}
		defer if config.log_type {
			fmt.eprintln("\n-- typechecker end")
		}
	}

	// Use persistent type checker for REPL
	if config.repl {
		if !vm.type_arena_init {
			err := vmem.arena_init_growing(&vm.type_arena)
			ensure(err == nil)
			vm.type_arena_init = true
		}

		prev_alloc := context.allocator
		context.allocator = vmem.arena_allocator(&vm.type_arena)

		if vm.type_checker == nil {
			tc := new(TypeChecker)
			tc^ = TypeChecker {
				ctx           = nil,
				resolutions   = resolutions,
				// the typemap is the only thing that doesn't use the arena;
				// this is because it is returned back by the typechecker
				typemap       = make(TypeMap, prev_alloc),
				typevar_count = 0,
				current_token = {},
				return_type   = {},
				pipeline_type = {},
			}
			push_function_scope(tc)
			vm.type_checker = tc
		}

		vm.type_checker.resolutions = resolutions
		_, ok := typecheck_without_arena(vm.type_checker, expr)
		if !ok {
			delete_typemap(vm.type_checker.typemap)
			vm.type_checker.typemap = make(TypeMap, prev_alloc)
			return nil, false
		}

		result := vm.type_checker.typemap
		vm.type_checker.typemap = make(TypeMap, prev_alloc)
		return result, true
	} else {
		return typecheck(expr, resolutions)
	}
}
