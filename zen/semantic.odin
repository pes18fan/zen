package zen

import "core:fmt"
import "core:os"

SemanticCompiler :: struct {
	scope_depth: int,
	loop_depth:  int,
	enclosing:   ^SemanticCompiler,
}

Semantic :: struct {
	current_compiler: ^SemanticCompiler,
	current_class:    ^ClassCompiler,
	current_token:    Token,
	had_error:        bool,
}

init_semantic_compiler :: proc(sm: ^Semantic, c: ^SemanticCompiler) {
	c^ = SemanticCompiler {
		scope_depth = 0,
		loop_depth  = 0,
		enclosing   = sm.current_compiler.enclosing,
	}
	sm.current_compiler = c
}

end_semantic_compiler :: proc(sm: ^Semantic) {
	sm.current_compiler = sm.current_compiler.enclosing
}

init_semantic :: proc() -> Semantic {
	return Semantic{current_compiler = nil, current_class = nil, had_error = false}
}

semantic_error :: proc(sm: ^Semantic, message: string) {
	token := sm.current_token
	color_red(os.stderr, "compile error ")

	if token.type == .EOF {
		fmt.eprint("at end")
	} else if token.type == .NEWLINE {
		fmt.eprint("at end of line")
	} else {
		fmt.eprintf("at '%s'", token.lexeme)
	}

	fmt.eprintfln(": %s", message)
	fmt.eprintfln("  on [line %d]", token.line)
	sm.had_error = true
}

@(require_results)
_analyze :: proc(sm: ^Semantic, expr: Expr) -> bool {
	if expr == nil {
		return true
	}

	switch e in expr {
	case ^AssignExpr:
		sm.current_token = e.token
		_analyze(sm, e.value) or_return
	case ^BinaryExpr:
		sm.current_token = e.token
		_analyze(sm, e.left) or_return
		_analyze(sm, e.right) or_return
	case ^BlockExpr:
		sm.current_token = e.token
		sm.current_compiler.scope_depth += 1
		_analyze(sm, e.expression) or_return
		assert(sm.current_compiler.scope_depth > 0, "can't have less than 0 scopes")
		sm.current_compiler.scope_depth -= 1
	case ^BreakExpr:
		sm.current_token = e.token
		if sm.current_compiler.loop_depth == 0 {
			semantic_error(sm, "Cannot break outside a loop.")
			return false
		}
	case ^CallExpr:
		sm.current_token = e.token
		_analyze(sm, e.callee) or_return
		for arg in e.arguments {
			_analyze(sm, arg) or_return
		}
	case ^ClassExpr:
		unimplemented()
	case ^ContinueExpr:
		sm.current_token = e.token
		if sm.current_compiler.loop_depth == 0 {
			semantic_error(sm, "Cannot use 'continue' outside a loop.")
			return false
		}
	case ^ExitExpr:
		sm.current_token = e.token
		_analyze(sm, e.code) or_return
	case ^ForExpr:
		sm.current_token = e.token
		sm.current_compiler.scope_depth += 1
		_analyze(sm, e.initializer) or_return
		_analyze(sm, e.condition) or_return
		_analyze(sm, e.increment) or_return
		_analyze(sm, e.body) or_return
		assert(sm.current_compiler.scope_depth > 0, "can't have less than 0 scopes")
		sm.current_compiler.scope_depth -= 1
	case ^ForInExpr:
		sm.current_token = e.token
	case ^GetExpr:
		sm.current_token = e.token
	case ^GroupingExpr:
		sm.current_token = e.token
	case ^IfExpr:
		sm.current_token = e.token
	case ^ItExpr:
		sm.current_token = e.token
	case ^LambdaExpr:
		sm.current_token = e.token
		c: SemanticCompiler
		init_semantic_compiler(sm, &c)
		_analyze(sm, e.body) or_return
		end_semantic_compiler(sm)
	case ^ListExpr:
		sm.current_token = e.token
	case ^LiteralExpr:
		sm.current_token = e.token
	case ^LogicalExpr:
		sm.current_token = e.token
	case ^PipeExpr:
		sm.current_token = e.token
	case ^PrintExpr:
		sm.current_token = e.token
	case ^ReturnExpr:
		sm.current_token = e.token
		if sm.current_compiler == nil {
			semantic_error(sm, "Cannot return from the top level.")
			return false
		}
		_analyze(sm, e.value) or_return
	case ^SetExpr:
		sm.current_token = e.token
	case ^SequenceExpr:
		sm.current_token = e.token
	case ^SubscriptExpr:
		sm.current_token = e.token
	case ^SubscriptSetExpr:
		sm.current_token = e.token
	case ^SuperExpr:
		sm.current_token = e.token
	case ^SwitchExpr:
		sm.current_token = e.token
	case ^ThisExpr:
		sm.current_token = e.token
	case ^UnaryExpr:
		sm.current_token = e.token
	case ^UseExpr:
		sm.current_token = e.token
	case ^VariableExpr:
		sm.current_token = e.token
	case ^VarDeclExpr:
		sm.current_token = e.token
	case ^WhileExpr:
		sm.current_token = e.token
	}

	return true
}

@(require_results)
analyze :: proc(expr: Expr) -> bool {
	sm := init_semantic()
	return _analyze(&sm, expr)
}
