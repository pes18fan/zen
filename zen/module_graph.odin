package zen

import "core:container/queue"
import toposort "core:container/topological_sort"
import "core:fmt"
import "core:path/filepath"
import "core:strings"

ModuleResolver :: struct #all_or_none {
	sorter:       toposort.Sorter(^Module),
	module_queue: queue.Queue(^Module),
	resolved:     map[string]^Module,
}

// a module struct has full ownership over the source, tokens and the AST
Module :: struct #all_or_none {
	name:     string,
	fullpath: string,
	source:   string,
	tokens:   []Token,
	ast:      Expr,
}

@(require_results)
add_imports :: proc(mr: ^ModuleResolver, expr: Expr, curr: ^Module) -> bool {
	if expr == nil {return true}

	switch e in expr {
	case ^AssignExpr:
		add_imports(mr, e.value, curr) or_return
	case ^BinaryExpr:
		add_imports(mr, e.left, curr) or_return
		add_imports(mr, e.right, curr) or_return
	case ^BlockExpr:
		add_imports(mr, e.expression, curr) or_return
	case ^BreakExpr, ^ContinueExpr, ^ItExpr, ^LiteralExpr, ^VariableExpr: // do nothing
	case ^CallExpr:
		add_imports(mr, e.callee, curr) or_return
		for arg in e.arguments {
			add_imports(mr, arg, curr) or_return
		}
	case ^ExitExpr:
		add_imports(mr, e.code, curr) or_return
	case ^ForExpr:
		add_imports(mr, e.initializer, curr) or_return
		add_imports(mr, e.condition, curr) or_return
		add_imports(mr, e.increment, curr) or_return
		add_imports(mr, e.body, curr) or_return
	case ^ForInExpr:
		add_imports(mr, e.iterable, curr) or_return
		add_imports(mr, e.body, curr) or_return
	case ^GetExpr:
		add_imports(mr, e.receiver, curr) or_return
	case ^GroupingExpr:
		add_imports(mr, e.expression, curr) or_return
	case ^IfExpr:
		add_imports(mr, e.condition, curr) or_return
		add_imports(mr, e.then_branch, curr) or_return
		add_imports(mr, e.else_branch, curr) or_return
	case ^FunctionExpr:
		add_imports(mr, e.body, curr) or_return
	case ^ListExpr:
		for element in e.elements {
			add_imports(mr, element, curr) or_return
		}
	case ^LogicalExpr:
		add_imports(mr, e.left, curr) or_return
		add_imports(mr, e.right, curr) or_return
	case ^PipeExpr:
		add_imports(mr, e.left, curr) or_return
		add_imports(mr, e.right, curr) or_return
	case ^EchoExpr:
		add_imports(mr, e.expr, curr) or_return
	case ^ReturnExpr:
		add_imports(mr, e.value, curr) or_return
	case ^SequenceExpr:
		add_imports(mr, e.left, curr) or_return
		add_imports(mr, e.right, curr) or_return
	case ^SubscriptExpr:
		add_imports(mr, e.receiver, curr) or_return
		add_imports(mr, e.index, curr) or_return
	case ^SubscriptSetExpr:
		add_imports(mr, e.receiver, curr) or_return
		add_imports(mr, e.index, curr) or_return
		add_imports(mr, e.value, curr) or_return
	case ^SwitchExpr:
		add_imports(mr, e.condition, curr) or_return
		for switch_case in e.cases {
			add_imports(mr, switch_case.condition, curr) or_return
			add_imports(mr, switch_case.body, curr) or_return
		}
		add_imports(mr, e.else_branch, curr) or_return
	case ^UnaryExpr:
		add_imports(mr, e.right, curr) or_return
	case ^UseExpr:
		if e.type == .BUILTIN {
			// not handling builtins here
			return true
		}

		name := e.name
		fullpath := e.fullpath

		if existing, ok := mr.resolved[fullpath]; ok {
			toposort.add_dependency(&mr.sorter, curr, existing)
			return true
		}

		source := read_file(fullpath) or_return
		tokens := lex(source) or_return
		ast := parse(tokens) or_return
		semcheck(ast) or_return

		mod := new(Module)
		mod^ = Module{name, fullpath, source, tokens, ast}
		mr.resolved[fullpath] = mod
		toposort.add_dependency(&mr.sorter, curr, mod)
		queue.enqueue(&mr.module_queue, mod)
	case ^VarDeclExpr:
		for binding in e.bindings {
			add_imports(mr, binding.initializer, curr) or_return
		}
	case ^WhileExpr:
		add_imports(mr, e.condition, curr) or_return
		add_imports(mr, e.body, curr) or_return
	}

	return true
}

// Given a root module with a certain path, source code, lexed tokens and an
// AST, recursively parse all module imports within it and return a topologically
// sorted array of modules, meaning this array is guaranteed to have all
// dependents appearing AFTER their dependencies.
// The created modules take full ownership of the source, tokens and AST; they
// are all freed when the module is freed. They must not be individually freed
// after module resolution.
// Returns the array of modules and `true` on success. Returns `nil` and `false`
// if there was an error lexing, parsing or doing semantic analysis on an inner
// module, or if there was a cyclic import.
create_module_graph :: proc(
	fullpath: string,
	source: string,
	tokens: []Token,
	ast: Expr,
) -> (
	graph: []^Module,
	ok: bool,
) {
	mr := init_module_resolver()
	defer destroy_module_resolver(&mr)

	root_mod := new(Module)
	root_mod^ = {strings.clone(filepath.short_stem(fullpath)), fullpath, source, tokens, ast}
	mr.resolved[fullpath] = root_mod
	toposort.add_key(&mr.sorter, root_mod)
	queue.enqueue(&mr.module_queue, root_mod)

	for queue.len(mr.module_queue) > 0 {
		curr := queue.dequeue(&mr.module_queue)
		add_imports(&mr, curr.ast, curr) or_return
	}

	sorted, cycled := toposort.sort(&mr.sorter)
	if len(cycled) > 0 {
		defer destroy_module_resolver(&mr)

		sb := strings.builder_make()
		defer strings.builder_destroy(&sb)

		start := cycled[0]
		next := cycled[1]
		fmt.sbprintf(&sb, "Module '%v' imported '%v'", start.name, next.name)
		for mod in cycled[2:] {
			fmt.sbprintf(&sb, "\n    which imported '%v'", mod.name)
		}
		fmt.sbprintf(&sb, "\n    which imported '%v'.", start.name)

		print_error(tokens[0], "Cyclic import!", details = strings.to_string(sb))
		return nil, false
	}

	return sorted[:], true
}

init_module_resolver :: proc() -> ModuleResolver {
	// toposorter compares modules by their absolute paths
	sorter: toposort.Sorter(^Module)
	toposort.init(&sorter)

	module_queue: queue.Queue(^Module)
	queue.init(&module_queue)

	return ModuleResolver {
		sorter = sorter,
		module_queue = module_queue,
		resolved = make(map[string]^Module),
	}
}

destroy_module_resolver :: proc(m: ^ModuleResolver) {
	toposort.destroy(&m.sorter)
	queue.destroy(&m.module_queue)
	delete(m.resolved)
}

destroy_module_graph :: proc(graph: []^Module) {
	for &module in graph {
		delete(module.fullpath)
		free_expr(module.ast)
		delete(module.tokens)
		delete(module.source)
		free(module)
	}
	delete(graph)
}
