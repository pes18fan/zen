package zen

import "core:fmt"
import "core:os"

SemanticCompiler :: struct {
	enclosing:   ^SemanticCompiler, // The enclosing function.
	func_type:   FunctionType, // Type of the function being checked.
	loop_depth:  int, // How many loops in are we?
	scope_depth: int, // The number of blocks in scope of this function.
	local_count: int, // Number of local variables.
}

/* Main state for the semantic analysis pass. Holds the current scope,
class context, pipeline state and some other necessary items.
One Semantic instance is created per call to `semcheck`. */
Semantic :: struct {
	current_compiler: ^SemanticCompiler,
	current_class:    ^ClassCompiler,
	current_token:    Token,
	had_error:        bool,
	pipeline_active:  bool,
}

init_semantic_compiler :: proc(sm: ^Semantic, c: ^SemanticCompiler, type: FunctionType) {
	c^ = SemanticCompiler {
		local_count = 0,
		scope_depth = 0,
		loop_depth  = 0,
		enclosing   = sm.current_compiler,
		func_type   = type,
	}

	sm.current_compiler = c
}

/* Pop back to the enclosing compiler when we exit a function scope. */
end_semantic_compiler :: proc(sm: ^Semantic) {
	sm.current_compiler = sm.current_compiler.enclosing
}

init_semantic :: proc() -> Semantic {
	return Semantic {
		current_compiler = nil,
		current_class = nil,
		had_error = false,
		pipeline_active = false,
	}
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

begin_semantic_scope :: proc(sm: ^Semantic) {
	sm.current_compiler.scope_depth += 1
}

/* 
End the current scope. Removes any local variables that were declared in the
scope from the compiler's local table. (The codegen handles emitting the
OP_POP or OP_CLOSE_UPVALUE bytecodes during its own end_scope pass.)
*/
@(private = "file")
end_semantic_scope :: proc(sm: ^Semantic) {
	sm.current_compiler.scope_depth -= 1
}

@(require_results)
semantic_in_global_scope :: proc(sm: ^Semantic) -> bool {
	return sm.current_compiler.func_type == .SCRIPT && sm.current_compiler.scope_depth == 0
}

@(require_results)
semcheck_function_expr :: proc(sm: ^Semantic, e: ^FunctionExpr, type: FunctionType) -> bool {
	params := e.params
	body := e.body

	compiler: SemanticCompiler
	init_semantic_compiler(sm, &compiler, type)

	begin_semantic_scope(sm)
	if len(params) > U8_MAX {
		semantic_error(sm, "Cannot have more than 255 parameters.")
		return false
	}
	semcheck_expr(sm, body) or_return
	end_semantic_scope(sm)

	end_semantic_compiler(sm)
	return true
}

// Full semantic analysis phase, done after resolving forward references in the
// global scope.
@(require_results)
semcheck_expr :: proc(sm: ^Semantic, expr: Expr) -> bool {
	if expr == nil {
		return true
	}

	switch e in expr {
	case ^AssignExpr:
		sm.current_token = e.token
		semcheck_expr(sm, e.value) or_return
	case ^BinaryExpr:
		sm.current_token = e.token
		semcheck_expr(sm, e.left) or_return
		semcheck_expr(sm, e.right) or_return
	case ^BlockExpr:
		sm.current_token = e.token
		begin_semantic_scope(sm)
		semcheck_expr(sm, e.expression) or_return
		end_semantic_scope(sm)
	case ^BreakExpr:
		sm.current_token = e.token
		if sm.current_compiler.loop_depth == 0 {
			semantic_error(sm, "Cannot break outside a loop.")
			return false
		}
	case ^CallExpr:
		sm.current_token = e.token
		semcheck_expr(sm, e.callee) or_return

		arg_count := len(e.arguments)
		if sm.pipeline_active {
			arg_count += 1
		}

		// Arg count can't be more than 255 since it's stuffed in one byte.
		if arg_count > U8_MAX {
			semantic_error(sm, "Cannot have more than 255 arguments.")
			return false
		}

		for arg in e.arguments {
			semcheck_expr(sm, arg) or_return
		}
	case ^ClassExpr:
		sm.current_token = e.token
		name := e.name
		methods := e.methods
		superclass := e.superclass

		class_compiler: ClassCompiler
		class_compiler.enclosing = sm.current_class
		class_compiler.has_superclass = superclass != nil
		sm.current_class = &class_compiler

		if superclass_name, ok := superclass.?; ok {
			if identifiers_equal(name, superclass_name) {
				semantic_error(sm, "A class can't inherit from itself.")
				return false
			}
		}

		for method in methods {
			assert(method.bound_to != nil, "method must be bound to a name")
			method_name := method.bound_to.?
			if method_name.lexeme == "init" {
				semcheck_function_expr(sm, method, .INITIALIZER) or_return
			} else {
				semcheck_function_expr(sm, method, .METHOD) or_return
			}
		}

		sm.current_class = sm.current_class.enclosing
	case ^ContinueExpr:
		sm.current_token = e.token
		if sm.current_compiler.loop_depth == 0 {
			semantic_error(sm, "Cannot use 'continue' outside a loop.")
			return false
		}
	case ^DiscardExpr:
		sm.current_token = e.token
		semcheck_expr(sm, e.expression) or_return
	case ^ExitExpr:
		sm.current_token = e.token
		semcheck_expr(sm, e.code) or_return
	case ^ForExpr:
		sm.current_token = e.token

		begin_semantic_scope(sm)

		semcheck_expr(sm, e.initializer) or_return

		if sm.current_compiler != nil {
			sm.current_compiler.loop_depth += 1
		}

		semcheck_expr(sm, e.condition) or_return
		semcheck_expr(sm, e.increment) or_return
		semcheck_expr(sm, e.body) or_return

		if sm.current_compiler != nil {
			sm.current_compiler.loop_depth -= 1
		}

		end_semantic_scope(sm)
	case ^ForInExpr:
		sm.current_token = e.token
		iterable := e.iterable
		body := e.body

		begin_semantic_scope(sm)
		semcheck_expr(sm, iterable) or_return

		if sm.current_compiler != nil {
			sm.current_compiler.loop_depth += 1
		}
		begin_semantic_scope(sm)
		semcheck_expr(sm, body) or_return
		end_semantic_scope(sm)

		if sm.current_compiler != nil {
			sm.current_compiler.loop_depth -= 1
		}
		end_semantic_scope(sm)
	case ^GetExpr:
		sm.current_token = e.token
		semcheck_expr(sm, e.receiver) or_return
	case ^GroupingExpr:
		sm.current_token = e.token
		semcheck_expr(sm, e.expression) or_return
	case ^IfExpr:
		sm.current_token = e.token
		semcheck_expr(sm, e.condition) or_return
		semcheck_expr(sm, e.then_branch) or_return
		if e.else_branch != nil {
			semcheck_expr(sm, e.else_branch) or_return
		}
	case ^ItExpr:
		sm.current_token = e.token
		if !sm.pipeline_active {
			semantic_error(sm, "Cannot use 'it' outside of a pipeline.")
			return false
		}
	case ^FunctionExpr:
		sm.current_token = e.token
		if e.bound_to == nil {
			semcheck_function_expr(sm, e, .LAMBDA) or_return
		} else {
			semcheck_function_expr(sm, e, .FUNCTION) or_return
		}
	case ^ListExpr:
		sm.current_token = e.token
		elements := e.elements
		if len(elements) > U8_MAX {
			semantic_error(sm, "Cannot have more than 255 items in a list literal.")
			return false
		}

		for element in e.elements {
			semcheck_expr(sm, element) or_return
		}
	case ^LiteralExpr: // nothing to check
	case ^LogicalExpr:
		sm.current_token = e.token
		semcheck_expr(sm, e.left) or_return
		semcheck_expr(sm, e.right) or_return
	case ^PipeExpr:
		sm.current_token = e.token
		semcheck_expr(sm, e.left) or_return
		old_pipeline := sm.pipeline_active
		sm.pipeline_active = true
		semcheck_expr(sm, e.right) or_return
		sm.pipeline_active = old_pipeline
	case ^PrintExpr:
		sm.current_token = e.token
		semcheck_expr(sm, e.expr) or_return
	case ^ReturnExpr:
		sm.current_token = e.token
		if sm.current_compiler.func_type == .SCRIPT {
			semantic_error(sm, "Cannot return from the top level.")
			return false
		}

		if e.value != nil && sm.current_compiler.func_type == .INITIALIZER {
			semantic_error(sm, "Cannot return a value from an initializer.")
			return false
		}

		semcheck_expr(sm, e.value) or_return
	case ^SequenceExpr:
		sm.current_token = e.token
		semcheck_expr(sm, e.left) or_return
		// process iteratively instead of recursively to avoid stack overflows,
		// because SequenceExprs can get extremely deep in large programs
		{
			right := e.right
			for {
				next_seq, ok := right.(^SequenceExpr)
				if !ok {
					semcheck_expr(sm, right) or_return
					break
				}
				sm.current_token = next_seq.token
				semcheck_expr(sm, next_seq.left) or_return
				right = next_seq.right
			}
		}
	case ^SetExpr:
		sm.current_token = e.token
		receiver := e.receiver
		value := e.value
		semcheck_expr(sm, receiver) or_return
		semcheck_expr(sm, value) or_return
	case ^SubscriptExpr:
		sm.current_token = e.token
		semcheck_expr(sm, e.receiver) or_return
		semcheck_expr(sm, e.index) or_return
	case ^SubscriptSetExpr:
		sm.current_token = e.token
		receiver := e.receiver
		index := e.index
		value := e.value
		semcheck_expr(sm, receiver) or_return
		semcheck_expr(sm, index) or_return
		semcheck_expr(sm, value) or_return
	case ^SuperExpr:
		sm.current_token = e.token

		if sm.current_class == nil {
			semantic_error(sm, "Can't use 'super' outside a class.")
			return false
		} else if !sm.current_class.has_superclass {
			semantic_error(sm, "Can't use 'super' in a class with no superclass.")
			return false
		}

		if e.method_args != nil {
			if len(e.method_args) > U8_MAX {
				semantic_error(sm, "Cannot have more than 255 arguments.")
				return false
			}

			for arg in e.method_args {
				semcheck_expr(sm, arg) or_return
			}
		}
	case ^SwitchExpr:
		sm.current_token = e.token
		semcheck_expr(sm, e.condition) or_return
		for c in e.cases {
			semcheck_expr(sm, c.condition) or_return
			semcheck_expr(sm, c.body) or_return
		}
		semcheck_expr(sm, e.else_branch) or_return
	case ^ThisExpr:
		sm.current_token = e.token
		if sm.current_class == nil {
			semantic_error(sm, "Cannot use 'this' outside a class.")
			return false
		}
	case ^UnaryExpr:
		sm.current_token = e.token
		semcheck_expr(sm, e.right) or_return
	case ^UseExpr:
		sm.current_token = e.token
		if !semantic_in_global_scope(sm) {
			semantic_error(sm, "Can only declare modules at the top level.")
			return false
		}
	case ^VariableExpr: // nothing to check
	case ^VarDeclExpr:
		sm.current_token = e.token
		is_final := e.is_final

		for binding in e.bindings {
			sm.current_token = binding.name
			if binding.initializer == nil && is_final {
				semantic_error(sm, "Final variables must be initialized.")
				return false
			}

			if binding.initializer != nil {
				semcheck_expr(sm, binding.initializer) or_return
			}
		}
	case ^WhileExpr:
		sm.current_token = e.token

		if sm.current_compiler != nil {
			sm.current_compiler.loop_depth += 1
		}

		semcheck_expr(sm, e.condition) or_return
		semcheck_expr(sm, e.body) or_return

		if sm.current_compiler != nil {
			sm.current_compiler.loop_depth -= 1
		}
	}

	return true
}

@(require_results)
semcheck :: proc(expr: Expr) -> (success: bool) {
	if expr == nil {
		return true
	}

	sm := init_semantic()

	// allocate on the heap, we need this for codegen
	script_compiler: SemanticCompiler
	init_semantic_compiler(&sm, &script_compiler, .SCRIPT)
	// Don't end the compiler as it exists for the entire script scope.

	ok := semcheck_expr(&sm, expr)
	if !ok {
		return false
	}
	return true
}

// Temporary, tiny pass right after parsing done to detect if the program
// uses any OOP features. If it does, the typechecker is disabled.
// This function will only exist for the time period between the creation of
// the typechecker and the removal of OOP.
// TODO: handle the case of modules importing classes which are then invoked
// via `GetExpr`
@(require_results)
program_uses_classes :: proc(expr: Expr) -> bool {
	if expr == nil {return false}

	#partial switch e in expr {
	case ^ClassExpr, ^ThisExpr, ^SuperExpr:
		return true
	case ^BlockExpr:
		return program_uses_classes(e.expression)
	case ^GroupingExpr:
		return program_uses_classes(e.expression)
	case ^SequenceExpr:
		return program_uses_classes(e.left) || program_uses_classes(e.right)
	// other cases don't matter
	}

	return false
}
