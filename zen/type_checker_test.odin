package zen
import tt "core:testing"

// does the `tapp` shorthand actually produce valid types?
@(test)
test_tapp :: proc(t: ^tt.T) {
	context.allocator = context.temp_allocator
	free_all(context.temp_allocator)

	// Nil
	tt.expect(t, types_equal(tapp(.NIL), TypeFunctionApplication{constructor = .NIL}))

	// Number
	tt.expect(t, types_equal(tapp(.NUMBER), TypeFunctionApplication{constructor = .NUMBER}))

	// String
	tt.expect(t, types_equal(tapp(.STRING), TypeFunctionApplication{constructor = .STRING}))

	// Bool
	tt.expect(t, types_equal(tapp(.BOOL), TypeFunctionApplication{constructor = .BOOL}))

	// List[Number]
	tt.expect(
		t,
		types_equal(
			tapp(.LIST, {tapp(.NUMBER)}),
			TypeFunctionApplication {
				constructor = .LIST,
				args = {TypeFunctionApplication{constructor = .NUMBER}},
			},
		),
	)

	// List[List[Number]]
	tt.expect(
		t,
		types_equal(
			tapp(.LIST, {tapp(.LIST, {tapp(.NUMBER)})}),
			TypeFunctionApplication {
				constructor = .LIST,
				args = {
					TypeFunctionApplication {
						constructor = .LIST,
						args = {TypeFunctionApplication{constructor = .NUMBER}},
					},
				},
			},
		),
	)

	// () -> Nil
	tt.expect(
		t,
		types_equal(
			tapp(.FUNCTION, {tapp(.NIL)}),
			TypeFunctionApplication {
				constructor = .FUNCTION,
				args = {TypeFunctionApplication{constructor = .NIL}},
			},
		),
	)

	// (Number, Number) -> Number
	tt.expect(
		t,
		types_equal(
			tapp(.FUNCTION, {tapp(.NUMBER), tapp(.NUMBER), tapp(.NUMBER)}),
			TypeFunctionApplication {
				constructor = .FUNCTION,
				args = {
					TypeFunctionApplication{constructor = .NUMBER},
					TypeFunctionApplication{constructor = .NUMBER},
					TypeFunctionApplication{constructor = .NUMBER},
				},
			},
		),
	)

	// (Number, String) -> String
	tt.expect(
		t,
		types_equal(
			tapp(.FUNCTION, {tapp(.NUMBER), tapp(.STRING), tapp(.NUMBER)}),
			TypeFunctionApplication {
				constructor = .FUNCTION,
				args = {
					TypeFunctionApplication{constructor = .NUMBER},
					TypeFunctionApplication{constructor = .STRING},
					TypeFunctionApplication{constructor = .NUMBER},
				},
			},
		),
	)

	// (List[Number], Number) -> List[Number]
	tt.expect(
		t,
		types_equal(
			tapp(
				.FUNCTION,
				{tapp(.LIST, {tapp(.NUMBER)}), tapp(.NUMBER), tapp(.LIST, {tapp(.NUMBER)})},
			),
			TypeFunctionApplication {
				constructor = .FUNCTION,
				args = {
					TypeFunctionApplication {
						constructor = .LIST,
						args = {TypeFunctionApplication{constructor = .NUMBER}},
					},
					TypeFunctionApplication{constructor = .NUMBER},
					TypeFunctionApplication {
						constructor = .LIST,
						args = {TypeFunctionApplication{constructor = .NUMBER}},
					},
				},
			},
		),
	)
}

// do variables successfully unify to primitives?
@(test)
test_unify_var_with_primitives :: proc(t: ^tt.T) {
	context.allocator = context.temp_allocator
	defer free_all(context.temp_allocator)
	tc := TypeChecker{}
	push_function_scope(&tc)
	defer pop_function_scope(&tc)

	var := fresh(&tc)

	num_lit := TypeFunctionApplication {
		constructor = .NUMBER,
	}
	bool_lit := TypeFunctionApplication {
		constructor = .BOOL,
	}
	nil_lit := TypeFunctionApplication {
		constructor = .NIL,
	}
	string_lit := TypeFunctionApplication {
		constructor = .STRING,
	}

	num_subst, err1 := unify(var, num_lit)
	if err1 != nil {
		tt.fail(t)
		return
	}
	defer delete(num_subst)

	bool_subst, err2 := unify(var, bool_lit)
	if err2 != nil {
		tt.fail(t)
		return
	}
	defer delete(bool_subst)

	nil_subst, err3 := unify(var, nil_lit)
	if err3 != nil {
		tt.fail(t)
		return
	}
	defer delete(nil_subst)

	string_subst, err4 := unify(var, string_lit)
	if err4 != nil {
		tt.fail(t)
		return
	}
	defer delete(string_subst)

	tt.expect(t, types_equal(apply_substitution(num_subst, var), num_lit))
	tt.expect(t, types_equal(apply_substitution(bool_subst, var), bool_lit))
	tt.expect(t, types_equal(apply_substitution(nil_subst, var), nil_lit))
	tt.expect(t, types_equal(apply_substitution(string_subst, var), string_lit))
}

// do equal types return a nil substitution?
@(test)
test_unify_equal_types_returns_nil :: proc(t: ^tt.T) {
	subst, err := unify(
		TypeFunctionApplication{constructor = .NUMBER},
		TypeFunctionApplication{constructor = .NUMBER},
	)
	defer delete(subst)
	tt.expect(t, err == nil)
	tt.expect(t, subst == nil)
}

// do incampatible type function applications fail to unify?
@(test)
test_unify_mismatched_types :: proc(t: ^tt.T) {
	_, err := unify(
		TypeFunctionApplication{constructor = .NUMBER},
		TypeFunctionApplication{constructor = .BOOL},
	)
	tt.expect(t, err == .MISMATCH)
}

// are infinite types detected correctly?
@(test)
test_unify_occurs_check :: proc(t: ^tt.T) {
	context.allocator = context.temp_allocator
	defer free_all(context.temp_allocator)
	a := TypeVariable {
		idx = 0,
	}
	b := TypeFunctionApplication {
		constructor = .FUNCTION,
		args        = {a, tapp(.BOOL)},
	}
	_, err := unify(a, b)
	tt.expect(t, err == .INFINITE_TYPE)
}

// do function applications unify correctly?
@(test)
test_unify_function_type :: proc(t: ^tt.T) {
	context.allocator = context.temp_allocator
	defer free_all(context.temp_allocator)
	a := TypeVariable {
		idx = 0,
	}
	b := TypeVariable {
		idx = 1,
	}

	fn_type_a := TypeFunctionApplication {
		constructor = .FUNCTION,
		args        = {a, TypeFunctionApplication{constructor = .NUMBER}},
	}
	fn_type_b := TypeFunctionApplication {
		constructor = .FUNCTION,
		args        = {TypeFunctionApplication{constructor = .NUMBER}, b},
	}

	subst, err := unify(fn_type_a, fn_type_b)
	defer delete(subst)
	if err != nil {
		tt.fail(t)
		return
	}

	tt.expect(
		t,
		types_equal(apply_substitution(subst, a), TypeFunctionApplication{constructor = .NUMBER}),
	)
	tt.expect(
		t,
		types_equal(apply_substitution(subst, a), TypeFunctionApplication{constructor = .NUMBER}),
	)
}

// do substitutions combine correctly?
@(test)
test_substitution_combine :: proc(t: ^tt.T) {
	s1 := make(Substitution)
	defer delete(s1)
	s2 := make(Substitution)
	defer delete(s2)

	s1[TypeVariable{idx = 0}] = TypeFunctionApplication {
		constructor = .NUMBER,
	}
	s2[TypeVariable{idx = 1}] = TypeFunctionApplication {
		constructor = .BOOL,
	}

	combined := combine_substitutions(s1, s2)
	defer delete(combined)

	tt.expect(
		t,
		types_equal(
			combined[TypeVariable{idx = 0}],
			TypeFunctionApplication{constructor = .NUMBER},
		),
	)
	tt.expect(
		t,
		types_equal(combined[TypeVariable{idx = 1}], TypeFunctionApplication{constructor = .BOOL}),
	)
}

// do substitutions recursively combine correctly?
@(test)
test_substitution_combine_recursively :: proc(t: ^tt.T) {
	s1 := make(Substitution)
	defer delete(s1)
	s2 := make(Substitution)
	defer delete(s2)

	s1[TypeVariable{idx = 0}] = TypeVariable {
		idx = 1,
	}
	s2[TypeVariable{idx = 1}] = tapp(.BOOL)

	combined := combine_substitutions(s1, s2)
	defer delete(combined)

	tt.expect(t, types_equal(combined[TypeVariable{idx = 0}], tapp(.BOOL)))
}

// is the result of free_vars() on a type variable the variable itself?
@(test)
test_free_vars_type_variable :: proc(t: ^tt.T) {
	fvs := free_vars(TypeVariable{idx = 42})
	defer delete(fvs)
	tt.expect(t, TypeVariable{idx = 42} in fvs)
	tt.expect(t, len(fvs) == 1)
}

// is the result of free_vars() on an application the union of the free_vars() in the args?
@(test)
test_free_vars_type_application :: proc(t: ^tt.T) {
	ty := TypeFunctionApplication {
		constructor = .FUNCTION,
		args        = {TypeVariable{idx = 0}, TypeVariable{idx = 1}},
	}
	fvs := free_vars(ty)
	defer delete(fvs)
	tt.expect(t, TypeVariable{idx = 0} in fvs)
	tt.expect(t, TypeVariable{idx = 1} in fvs)
	tt.expect(t, len(fvs) == 2)
}

// do substitutions apply correctly to type variables?
@(test)
test_apply_substitution_type_variable :: proc(t: ^tt.T) {
	subst := make(Substitution)
	defer delete(subst)
	subst[TypeVariable{idx = 0}] = TypeFunctionApplication {
		constructor = .STRING,
	}

	result := apply_substitution(subst, TypeVariable{idx = 0})
	tt.expect(t, types_equal(result, TypeFunctionApplication{constructor = .STRING}))
}

// do substitutions apply correctly to type function applications?
@(test)
test_apply_substitution_function :: proc(t: ^tt.T) {
	context.allocator = context.temp_allocator
	defer free_all(context.temp_allocator)
	ty := TypeFunctionApplication {
		constructor = .FUNCTION,
		args        = {TypeVariable{idx = 0}, TypeVariable{idx = 1}},
	}
	subst := make(Substitution)
	defer delete(subst)
	subst[TypeVariable{idx = 0}] = TypeFunctionApplication {
		constructor = .NUMBER,
	}
	subst[TypeVariable{idx = 1}] = TypeFunctionApplication {
		constructor = .STRING,
	}

	result := apply_substitution(subst, ty)
	expected := TypeFunctionApplication {
		constructor = .FUNCTION,
		args        = {
			TypeFunctionApplication{constructor = .NUMBER},
			TypeFunctionApplication{constructor = .STRING},
		},
	}
	tt.expect(t, types_equal(result, expected))
}

// are equal types reported as so by types_equal()?
@(test)
test_types_equal_same :: proc(t: ^tt.T) {
	tt.expect(t, types_equal(TypeVariable{idx = 0}, TypeVariable{idx = 0}))
	tt.expect(
		t,
		types_equal(
			TypeFunctionApplication{constructor = .NUMBER},
			TypeFunctionApplication{constructor = .NUMBER},
		),
	)
}

// are different types reported as so by types_equal()?
@(test)
test_types_equal_different :: proc(t: ^tt.T) {
	tt.expect(t, !types_equal(TypeVariable{idx = 0}, TypeVariable{idx = 1}))
	tt.expect(
		t,
		!types_equal(
			TypeFunctionApplication{constructor = .NUMBER},
			TypeFunctionApplication{constructor = .BOOL},
		),
	)
	tt.expect(
		t,
		!types_equal(TypeVariable{idx = 0}, TypeFunctionApplication{constructor = .NUMBER}),
	)
}

// does popping and pushing contexts work correctly?
@(test)
test_scope_push_pop :: proc(t: ^tt.T) {
	context.allocator = context.temp_allocator
	defer free_all(context.temp_allocator)
	tc := TypeChecker{}
	push_function_scope(&tc)
	defer pop_function_scope(&tc)

	bind_type(tc.ctx, "x", TypeFunctionApplication{constructor = .NUMBER})
	push_scope(tc.ctx)
	bind_type(tc.ctx, "y", TypeFunctionApplication{constructor = .BOOL})

	// if any of the resolve_types panic we failed
	_ = resolve_type(&tc, "x", {})
	_ = resolve_type(&tc, "y", {})

	pop_scope(tc.ctx)
	_ = resolve_type(&tc, "x", {})
}

// do generalization and instantiation work correctly?
@(test)
test_generalize_instantiate :: proc(t: ^tt.T) {
	context.allocator = context.temp_allocator
	defer free_all(context.temp_allocator)
	tc := TypeChecker{}
	push_function_scope(&tc)
	defer pop_function_scope(&tc)

	ty := fresh(&tc)
	scheme := generalize(&tc, ty)

	inst := instantiate(&tc, scheme)
	tt.expect(t, is_type_variable(inst))

	inst_tv := as_type_variable(inst)
	tt.expect(t, inst_tv.idx != ty.idx)
}
