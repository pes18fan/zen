package zen

import "core:fmt"
import "core:os"

/*
TODO: Variable resolution / symbol table creation. Needs very careful
design so that it can be used seamlessly in the codegen part without any
awful hacky designs. Some points:
- Classes and OOP SHOULD work fine and without hacks because records will be
     in soon and therefore will need to work nicely, but on that same note
     they're not a huge priority as they'll be replaced by said records
- The upvalue-based approach used in clox and inherited by zen ever since
     its creation can now safely be replaced because zen now has an AST
     and therefore enough context to know what variables are closed over
     and when; the idea now is to create a new ObjCell type to replace
     ObjUpvalue that is nothing but a heap-allocated value used for closed
     over variables. This point needs to be kept in mind for closing over
     loop variables as well.
- Because of the addition of the Hindley-Milner type checker, modules can
     no longer be viably used as values. Why? Well, if we wanted to viably
     use modules as values, we'd need to typecheck them. So they'd be
     some sort of record type. But, these modules have polymorphic functions
     inside of them; for instance the list.pop function which has a quantified
     type of `forall a. List a -> a`. The list module would then be a type
     that looks something like `Record (forall a. List a -> a, ...)`, and
     this directly hits a limit of Hindley-Milner; polymorphic types are
     NOT allowed as type parameters i.e. we can only have rank-1 polymorphism.
     A lot of changes are needed throughout zen for this; some perhaps in the
     semantic analyzer.
*/

SemanticCompiler :: struct {
	enclosing:     ^SemanticCompiler, // The enclosing function.
	func_type:     FunctionType, // Type of the function being checked.
	loop_depth:    int, // How many loops in are we?
	scope_depth:   int, // The number of blocks in scope of this function.
	local_count:   int, // Number of local variables.
	capture_count: int, // Number of variables captured from outer scopes.
}

/* Main state for the semantic analysis pass. Holds the current scope,
class context, pipeline state and some other necessary items.
One Semantic instance is created per call to `analyze` and lives until
the caller (codegen) finishes consuming the resolution map. */
Semantic :: struct {
	current_compiler: ^SemanticCompiler,
	current_class:    ^ClassCompiler,
	current_token:    Token,
	had_error:        bool,
	pipeline_active:  bool,
	gc:               ^GC,
	globals:          ^Table,
}

init_semantic_compiler :: proc(sm: ^Semantic, c: ^SemanticCompiler, type: FunctionType) {
	c^ = SemanticCompiler {
		capture_count = 0,
		local_count   = 0,
		scope_depth   = 0,
		loop_depth    = 0,
		enclosing     = sm.current_compiler,
		func_type     = type,
	}

	sm.current_compiler = c
}

/* Pop back to the enclosing compiler when we exit a function scope. */
end_semantic_compiler :: proc(sm: ^Semantic) {
	sm.current_compiler = sm.current_compiler.enclosing
}

init_semantic :: proc(gc: ^GC, globals: ^Table) -> Semantic {
	return Semantic {
		current_compiler = nil,
		current_class = nil,
		had_error = false,
		pipeline_active = false,
		gc = gc,
		globals = globals,
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
in_global_scope :: proc(sm: ^Semantic) -> bool {
	return sm.current_compiler.func_type == .SCRIPT && sm.current_compiler.scope_depth == 0
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

		class_compiler: ClassCompiler
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
		bound_to := e.bound_to
		params := e.params
		body := e.body

		compiler: SemanticCompiler
		init_semantic_compiler(sm, &compiler, .FUNCTION)

		begin_semantic_scope(sm)
		if len(params) > U8_COUNT {
			semantic_error(sm, "Cannot have more than 255 parameters.")
			return false
		}
		semcheck_expr(sm, body) or_return
		end_semantic_scope(sm)
	case ^ListExpr:
		sm.current_token = e.token

		if len(e.elements) > U8_MAX {
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
		if !in_global_scope(sm) {
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

// Two-pass semantic analyzer
@(require_results)
semcheck :: proc(gc: ^GC, expr: Expr, globals: ^Table) -> (success: bool) {
	if expr == nil {
		return true
	}

	// Add native function names to the globals table
	for fn_name in GLOBAL_NATIVE_FN_NAMES {
		table_set(globals, copy_string(gc, fn_name), bool_val(true))
	}

	sm := init_semantic(gc, globals)

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
