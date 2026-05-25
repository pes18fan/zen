package zen

ErrorMessage :: Maybe(string)

// Helper to avoid a billion `if err != nil`s everywhere
@(require_results)
try :: proc {
	try_codegen,
	try_type_checker,
}

try_codegen :: #force_inline proc(cg: ^Codegen, err: ErrorMessage) -> bool {
	if err != nil {
		codegen_error(cg, err.?)
		return false
	}
	return true
}

try_type_checker :: #force_inline proc(tc: ^TypeChecker, err: ErrorMessage) -> bool {
	if err != nil {
		unimplemented()
	}
	return true
}

// `try` for procedures that return a value and possibly an error
@(require_results)
try2 :: proc {
	try2_codegen,
	try2_type_checker,
}

try2_codegen :: #force_inline proc(cg: ^Codegen, ret: $T, err: ErrorMessage) -> (T, bool) {
	if err != nil {
		codegen_error(cg, err.?)
		return ret, false
	}
	return ret, true
}

try2_type_checker :: #force_inline proc(t: ^TypeChecker, ret: $T, err: ErrorMessage) -> (T, bool) {
	if err != nil {
		unimplemented()
	}
	return ret, true
}
