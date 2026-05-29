package zen

ErrorMessage :: Maybe(string)

// Helper to avoid a billion `if err != nil`s everywhere
@(require_results)
try :: proc {
	try_codegen,
	try_type_checker,
	try_semantic,
}

@(require_results)
try_codegen :: #force_inline proc(cg: ^Codegen, err: ErrorMessage) -> bool {
	if err != nil {
		codegen_error(cg, err.?)
		return false
	}
	return true
}

@(require_results)
try_type_checker :: #force_inline proc(tc: ^TypeChecker, err: ErrorMessage) -> bool {
	if err != nil {
		typecheck_error(tc, err.?)
		return false
	}
	return true
}

@(require_results)
try_semantic :: #force_inline proc(sm: ^Semantic, err: ErrorMessage) -> bool {
	if err != nil {
		semantic_error(sm, err.?)
		return false
	}
	return true
}

// `try` for procedures that return a value and possibly an error
try2 :: proc {
	try2_codegen,
	try2_type_checker,
	try2_semantic,
}

@(require_results)
try2_codegen :: #force_inline proc(cg: ^Codegen, ret: $T, err: ErrorMessage) -> (T, bool) {
	if err != nil {
		codegen_error(cg, err.?)
		return ret, false
	}
	return ret, true
}

@(require_results)
try2_type_checker :: #force_inline proc(
	tc: ^TypeChecker,
	ret: $T,
	err: ErrorMessage,
) -> (
	T,
	bool,
) {
	if err != nil {
		typecheck_error(tc, err.?)
		return ret, false
	}
	return ret, true
}

@(require_results)
try2_semantic :: #force_inline proc(sm: ^Semantic, ret: $T, err: ErrorMessage) -> (T, bool) {
	if err != nil {
		semantic_error(sm, err.?)
		return ret, false
	}
	return ret, true
}
