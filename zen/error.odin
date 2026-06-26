package zen

import "core:fmt"
import "core:path/filepath"
ErrorMessage :: Maybe(string)

// Helper to avoid a billion `if err != nil`s everywhere
try :: proc {
	try_codegen,
	try_semantic,
	try_resolver,
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
try_semantic :: #force_inline proc(sm: ^Semantic, err: ErrorMessage) -> bool {
	if err != nil {
		semantic_error(sm, err.?)
		return false
	}
	return true
}

@(require_results)
try_resolver :: #force_inline proc(rs: ^Resolver, err: ErrorMessage) -> bool {
	if err != nil {
		resolver_error(rs, err.?)
		return false
	}

	return true
}

// `try` for procedures that return a value and possibly an error
try2 :: proc {
	try2_codegen,
	try2_semantic,
	try2_resolver,
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
try2_semantic :: #force_inline proc(sm: ^Semantic, ret: $T, err: ErrorMessage) -> (T, bool) {
	if err != nil {
		semantic_error(sm, err.?)
		return ret, false
	}
	return ret, true
}

@(require_results)
try2_resolver :: #force_inline proc(rs: ^Resolver, ret: $T, err: ErrorMessage) -> (T, bool) {
	if err != nil {
		resolver_error(rs, err.?)
		return ret, false
	}

	return ret, true
}

print_error :: #force_inline proc(token: Token, message: string) {
	fmt.eprint(color_red("error"))
	fmt.eprintfln(": %s", style_bold(message))

	fmt.eprintf(" --> %s", "REPL" if config.repl else filepath.base(config.__path))
	if token.type == .EOF {
		fmt.eprintln(" at end of file")
	} else if token.type == .NEWLINE {
		fmt.eprintln(" at end of line %d", token.position.line)
	} else {
		fmt.eprintfln(" at %d:%d", token.position.line, token.position.column)
	}

	source_line := token_line(token)
	fmt.eprintln("  |")
	fmt.eprintfln("%d |\t%s", token.position.line, source_line)

	fmt.eprint("  |\t")
	for _ in 0 ..< token.position.column - 1 {
		fmt.eprint(" ")
	}
	fmt.eprintln(color_red("^"))
}
