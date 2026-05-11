package zen

Codegen :: struct {
	current_compiler: ^Compiler,
	current_class:    ^ClassCompiler,
	globals:          ^Table,
	gc:               ^GC,
	prev_mark_roots:  RootSource,
	had_error:        bool,
	current_line:     int,
}

current_chunk :: proc(cg: ^Codegen) -> ^Chunk {
	return &cg.current_compiler.function.chunk
}

/* Write a byte to the chunk being compiled. */
@(private = "file")
emit_byte :: proc(cg: ^Codegen, byait: byte) {
	write_chunk(current_chunk(cg), byait, cg.current_line)
}

/* Write an opcode as a byte to the chunk being compiled. */
@(private = "file")
emit_opcode :: proc(cg: ^Codegen, oc: OpCode) {
	write_chunk(current_chunk(cg), byte(oc), cg.current_line)
}

/* 
Emit instructions to make the current function return nil; or if the current
function is an initializer make it return its receiver.
*/
@(private = "file")
emit_return :: proc(cg: ^Codegen) {
	if cg.current_compiler.type == .INITIALIZER {
		emit_opcode(cg, .OP_GET_LOCAL)
		emit_byte(cg, 0) /* Since the receiver is always stored in slot zero. */
	} else {
		emit_opcode(cg, .OP_NIL)
	}

	emit_opcode(cg, .OP_RETURN)
}

/*
Free the dynamic array of break jump offsets. 
*/
@(private = "file")
free_loops :: proc(cg: ^Codegen) {
	for i in cg.current_compiler.loops {
		delete(i.breaks)
	}
}

compile_var_declaration :: proc(cg: ^Codegen, decl: ^VarDecl) -> bool {
	unimplemented()
}

compile_func_declaration :: proc(cg: ^Codegen, decl: ^FuncDecl, public: bool) -> bool {
	unimplemented()
}

compile_class_declaration :: proc(cg: ^Codegen, decl: ^ClassDecl, public: bool) -> bool {
	unimplemented()
}

compile_module_declaration :: proc(cg: ^Codegen, decl: ^ModuleDecl) -> bool {
	unimplemented()
}

compile_declaration :: proc(cg: ^Codegen, decl: Decl) -> bool {
	if decl == nil {
		return true
	}

	switch d in decl {
	case ^VarDecl:
		cg.current_line = d.token.line
		compile_var_declaration(cg, d) or_return
	case ^FuncDecl:
		cg.current_line = d.token.line
		compile_func_declaration(cg, d, public = false) or_return
	case ^ClassDecl:
		cg.current_line = d.token.line
		compile_class_declaration(cg, d, public = false) or_return
	case ^ModuleDecl:
		cg.current_line = d.token.line
		compile_module_declaration(cg, d) or_return
	case ^PubDecl:
		cg.current_line = d.token.line
		#partial switch inner in d.decl {
		case ^FuncDecl:
			compile_func_declaration(cg, inner, public = true) or_return
		case ^ClassDecl:
			compile_class_declaration(cg, inner, public = true) or_return
		case:
			codegen_error(cg, d.token, "Only functions or classes can be set as public.")
			return false
		}
	case Stmt:
		compile_statement(cg, d) or_return
	}

	unreachable()
}

compile_statement :: proc(cg: ^Codegen, stmt: Stmt) -> bool {
	if stmt == nil {
		return true
	}

	switch s in stmt {
	case ^ExprStmt:
		cg.current_line = s.token.line
		compile_expression(cg, s.expr) or_return
		if config.repl && cg.current_compiler.type == .SCRIPT {
			unimplemented("repl printing logic")
		} else {
			emit_opcode(cg, .OP_POP)
		}
	case ^IfStmt:
		cg.current_line = s.token.line
		compile_expression(cg, s.condition) or_return
		unimplemented()
	case ^WhileStmt:
		cg.current_line = s.token.line
		unimplemented()
	}
}

compile_expression :: proc(cg: ^Codegen, expr: Expr) -> bool {
	unimplemented("expression compliation")
}

@(private = "file")
codegen_error :: proc(cg: ^Codegen, name: Token, message: string) {
	unimplemented()
}

/* Emit a return instruction and decode the RLE-encoded lines. */
@(private = "file")
end_compiler :: proc(cg: ^Codegen) -> ^ObjFunction {
	fn := cg.current_compiler.function
	if !fn.has_returned {
		emit_return(cg)
	}

	if config.dump_disassembly {
		if !cg.had_error {
			disassemble(current_chunk(cg), fn.name != nil ? fn.name.chars : "<script>")
		}
	}

	free_loops(cg)
	cg.current_compiler = cg.current_compiler.enclosing

	return fn
}

init_compiler_cg :: proc(c: ^Compiler, cg: ^Codegen, type: FunctionType) {
	c^ = Compiler {
		enclosing   = cg.current_compiler,
		type        = type,
		local_count = 0,
		loop_count  = 0,
		scope_depth = 0,
		function    = nil,
		globals     = cg.globals,
	}

	/* DON'T FORGET to set current_compiler to the new compiler!
	Took me WAAAAAAAAAAY too long to figure out I was missing this. */
	cg.current_compiler = c
	c.function = new_function(cg.gc)

	// if type != .SCRIPT && type != .LAMBDA {
	// 	c.function.name = copy_string(cg.gc, cg.previous.lexeme)
	// } else if type == .LAMBDA {
	// 	c.function.name = copy_string(cg.gc, "lambda")
	// }

	/* The first slot is the function itself. */
	local := &cg.current_compiler.locals[cg.current_compiler.local_count]
	c.local_count += 1
	local.depth = 0
	local.is_captured = false /* You cannot capture the slot zero value. */

	/* If the function is a method, the first slot is repurposed to store that
	 * method's receiver instead. */
	if type != .FUNCTION {
		local.name.lexeme = "this"
	} else {
		local.name.lexeme = ""
	}
}

codegen :: proc(gc: ^GC, decls: []Decl, globals: ^Table) -> (^ObjFunction, bool) {
	cg := Codegen {
		globals         = globals,
		gc              = gc,
		prev_mark_roots = gc.mark_roots_arg,
		had_error       = false,
	}
	gc.mark_roots_arg = &cg

	c: Compiler
	init_compiler_cg(&c, &cg, .SCRIPT)

	for decl in decls {
		compile_declaration(&cg, decl) or_break
	}

	fn := end_compiler(&cg)
	gc.mark_roots_arg = cg.prev_mark_roots
	return fn, !cg.had_error
}
