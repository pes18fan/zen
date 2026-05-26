package zen

import "core:fmt"
import "core:os"
import "core:strconv"
import "core:strings"

// AST Node Definitions

Expr :: union {
	^AssignExpr,
	^BinaryExpr,
	^BlockExpr,
	^BreakExpr,
	^CallExpr,
	^ContinueExpr,
	^ExitExpr,
	^ForExpr,
	^ForInExpr,
	^GetExpr,
	^GroupingExpr,
	^IfExpr,
	^ItExpr,
	^LambdaExpr,
	^ListExpr,
	^LiteralExpr,
	^LogicalExpr,
	^PipeExpr,
	^PrintExpr,
	^ReturnExpr,
	^SetExpr,
	^SequenceExpr,
	^SubscriptExpr,
	^SubscriptSetExpr,
	^SuperExpr,
	^SwitchExpr,
	^ThisExpr,
	^UnaryExpr,
	^VariableExpr,
	^VarDeclExpr,
	^WhileExpr,
}

Stmt :: union {
	^BlockStmt,
	^BreakStmt,
	^ContinueStmt,
	^EmptyStmt,
	^ExitStmt,
	^ExprStmt,
	^ForInStmt,
	^ForStmt,
	^IfStmt,
	^PrintStmt,
	^ReturnStmt,
	^SwitchStmt,
	^WhileStmt,
}

Decl :: union {
	^ClassDecl,
	^FuncDecl,
	^ModuleDecl,
	^PubDecl,
	^VarDecl,
	Stmt,
}

// Declarations

VarBinding :: struct {
	name:        Token,
	initializer: Expr,
}

VarDecl :: struct {
	token:    Token,
	is_final: bool,
	bindings: []VarBinding,
}

ClassDecl :: struct {
	token:      Token,
	name:       Token,
	superclass: Maybe(Token),
	methods:    []^FuncDecl,
}

ModuleDecl :: struct {
	token: Token,
	path:  Token,
}

FunctionBody :: union #no_nil {
	^BlockStmt,
	Expr,
}

FuncDecl :: struct {
	token:  Token,
	name:   Token,
	params: []Token,
	body:   FunctionBody,
}

PubDecl :: struct {
	token: Token,
	decl:  Decl,
}

// Statements

BlockStmt :: struct {
	token:        Token,
	declarations: []Decl,
}

BreakStmt :: struct {
	token: Token,
}

ContinueStmt :: struct {
	token: Token,
}

EmptyStmt :: struct {
	token: Token,
}

ExitStmt :: struct {
	token: Token,
	code:  Expr,
}

ExprStmt :: struct {
	token: Token,
	expr:  Expr,
}

ForInStmt :: struct {
	token:    Token,
	var_name: Token,
	iterable: Expr,
	body:     ^BlockStmt,
}

ForStmt :: struct {
	token:       Token,
	initializer: union #no_nil {
		^VarDecl,
		^ExprStmt,
		^EmptyStmt,
	},
	condition:   Expr,
	increment:   Expr,
	body:        ^BlockStmt,
}

IfStmt :: struct {
	token:       Token,
	is_ifnt:     bool,
	condition:   Expr,
	then_branch: ^BlockStmt,
	else_branch: ^BlockStmt,
}

PrintStmt :: struct {
	token: Token,
	expr:  Expr,
}

ReturnStmt :: struct {
	token: Token,
	value: Expr,
}

SwitchCase :: struct {
	condition: Expr,
	body:      Stmt,
}

SwitchStmt :: struct {
	token:       Token,
	condition:   Expr,
	cases:       []SwitchCase,
	else_branch: Stmt,
}

WhileStmt :: struct {
	token:      Token,
	is_whilent: bool,
	condition:  Expr,
	body:       ^BlockStmt,
}

// Expressions

AssignExpr :: struct {
	token: Token,
	name:  Token,
	value: Expr,
}

BinaryExpr :: struct {
	token:    Token,
	left:     Expr,
	operator: Token,
	right:    Expr,
}

BlockExpr :: struct {
	token:      Token,
	expression: Expr,
}

BreakExpr :: struct {
	token: Token,
}

CallExpr :: struct {
	token:      Token,
	callee:     Expr,
	rdelimiter: Token,
	arguments:  []Expr,
}

ContinueExpr :: struct {
	token: Token,
}

ExitExpr :: struct {
	token: Token,
	code:  Expr,
}

ForExpr :: struct {
	token:       Token,
	initializer: Expr,
	condition:   Expr,
	increment:   Expr,
	body:        ^BlockExpr,
}

ForInExpr :: struct {
	token:    Token,
	var_name: Token,
	iterable: Expr,
	body:     ^BlockExpr,
}

GetExpr :: struct {
	token:    Token,
	receiver: Expr,
	property: Token,
}

GroupingExpr :: struct {
	token:      Token,
	expression: Expr,
}

IfExpr :: struct {
	token:       Token,
	is_ifnt:     bool,
	condition:   Expr,
	then_branch: ^BlockExpr,
	else_branch: ^BlockExpr,
}

ItExpr :: struct {
	token: Token,
}

LambdaExpr :: struct {
	token:     Token,
	func_decl: ^FuncDecl,
}

ListExpr :: struct {
	token:    Token,
	elements: []Expr,
}

LiteralExpr :: struct {
	token: Token,
	value: PrimitiveValue,
}

/* 
The four primitive values in zen: f64, string, bool and nil (Odin unions are
nilable unless you specify otherwise)
*/
PrimitiveValue :: union {
	f64,
	string,
	bool,
}

LogicalExpr :: struct {
	token:    Token,
	left:     Expr,
	operator: Token,
	right:    Expr,
}

PipeExpr :: struct {
	token:    Token,
	left:     Expr,
	operator: Token,
	right:    Expr,
}

PrintExpr :: struct {
	token: Token,
	expr:  Expr,
}

ReturnExpr :: struct {
	token: Token,
	value: Expr,
}

SequenceExpr :: struct {
	token:    Token,
	left:     Expr,
	operator: Token,
	right:    Expr,
}

SetExpr :: struct {
	token:    Token,
	receiver: Expr,
	property: Token,
	value:    Expr,
}

SubscriptExpr :: struct {
	token:    Token,
	receiver: Expr,
	index:    Expr,
}

SubscriptSetExpr :: struct {
	token:    Token,
	receiver: Expr,
	index:    Expr,
	value:    Expr,
}

SuperExpr :: struct {
	token:       Token,
	method:      Token,
	method_args: []Expr, // nil if the method wasn't directly invoked
}

// Variant of the switch statement whose cases must evaluate to expressions
ExprSwitchCase :: struct {
	condition: Expr,
	body:      Expr,
}

SwitchExpr :: struct {
	token:       Token,
	condition:   Expr,
	cases:       []ExprSwitchCase,
	else_branch: Expr,
}

ThisExpr :: struct {
	token: Token,
}

UnaryExpr :: struct {
	token:    Token,
	operator: Token,
	right:    Expr,
}

VarDeclExpr :: struct {
	token:    Token,
	is_final: bool,
	bindings: []VarBinding,
}

VariableExpr :: struct {
	token: Token,
	name:  Token,
}

WhileExpr :: struct {
	token:      Token,
	is_whilent: bool,
	condition:  Expr,
	body:       ^BlockExpr,
}

Precedence :: enum {
	NONE,
	ASSIGNMENT, // =
	PIPELINE, // |>
	CONDITIONAL, // if switch
	OR, // or
	AND, // and
	EQUALITY, // == !=
	COMPARISON, // < > <= >=
	TERM, // + -
	FACTOR, // * / %
	UNARY, // ! -
	CALL, // . () []
	PRIMARY,
}

ParsePrefixFn :: #type proc(p: ^Parser, can_assign: bool) -> Expr
ParseInfixFn :: #type proc(p: ^Parser, left: Expr, can_assign: bool) -> Expr

ParseRule :: struct {
	prefix:     ParsePrefixFn,
	infix:      ParseInfixFn,
	precedence: Precedence,
}

/* Create a list of parsed declarations forming an abstract syntax tree, from
a list of tokens stored in a parser. */
parse :: proc(p: ^Parser) -> (expr: Expr, success: bool) {
	// declarations := make([dynamic]Decl)
	// for !is_at_end(p) {
	// 	decl := parse_declaration(p)
	// 	append(&declarations, decl)
	//
	// 	if p.panic_mode {
	// 		synchronize(p)
	// 	}
	// }
	//
	// return declarations[:], !p.had_error
	return parse_expression_top(p), !p.had_error
}

parse_declaration :: proc(p: ^Parser) -> Decl {
	switch {
	case match(p, .VAR, .VAL):
		return parse_var_decl(p)
	case match(p, .CLASS):
		return parse_class_decl(p)
	case match(p, .USE):
		return parse_module_decl(p)
	case match(p, .FUNC):
		return parse_func_decl(p, "function")
	case match(p, .PUB):
		return parse_pub_decl(p)
	}
	return parse_statement(p)
}

parse_var_decl :: proc(p: ^Parser) -> ^VarDecl {
	decl := new(VarDecl)
	decl.token = previous(p)
	decl.is_final = decl.token.type == .VAL
	bindings := make([dynamic]VarBinding)

	for {
		binding: VarBinding
		binding.name = consume(p, .IDENT, "Expect variable name.")
		if match(p, .EQUAL) {
			binding.initializer = parse_expression(p)
		}
		append(&bindings, binding)

		if !match(p, .COMMA) {break}
	}
	decl.bindings = bindings[:]

	consume_newline(p, "variable declaration")
	return decl
}

parse_class_decl :: proc(p: ^Parser) -> ^ClassDecl {
	decl := new(ClassDecl)
	decl.token = previous(p)
	decl.name = consume(p, .IDENT, "Expect class name.")
	methods := make([dynamic]^FuncDecl)

	if match(p, .LESS) {
		decl.superclass = consume(p, .IDENT, "Expect superclass name.")
	}

	consume(p, .LSQUIRLY, "Expect '{' before class body.")
	for !check(p, .RSQUIRLY) && !is_at_end(p) {
		append(&methods, parse_method(p))
		if match(p, .SEMI) {}
	}
	decl.methods = methods[:]

	consume(p, .RSQUIRLY, "Expect '}' after class body.")
	return decl
}

parse_method :: proc(p: ^Parser) -> ^FuncDecl {
	name := consume(p, .IDENT, "Expect method name.")
	return parse_func_body(p, name)
}

parse_module_decl :: proc(p: ^Parser) -> ^ModuleDecl {
	decl := new(ModuleDecl)
	decl.token = previous(p)
	decl.path = consume(p, .STRING, "Expect module path.")
	return decl
}

parse_func_decl :: proc(p: ^Parser, kind: string) -> ^FuncDecl {
	token := previous(p)
	name := consume(p, .IDENT, fmt.tprintf("Expect %s name.", kind))
	decl := parse_func_body(p, name)
	decl.token = token
	return decl
}

parse_func_body :: proc(p: ^Parser, name: Token) -> ^FuncDecl {
	decl := new(FuncDecl)
	decl.token = previous(p) // NOTE: this should probably be the keyword and not the name
	decl.name = name
	params := make([dynamic]Token)

	consume(p, .LPAREN, "Expect '(' after function name.")
	if !check(p, .RPAREN) {
		for {
			append(&params, consume(p, .IDENT, "Expect parameter name."))
			if !match(p, .COMMA) {break}
		}
	}
	decl.params = params[:]
	consume(p, .RPAREN, "Expect ')' after function parameters.")

	if match(p, .FAT_ARROW) {
		decl.body = parse_expression(p)
	} else {
		consume(p, .LSQUIRLY, "Expect '=>' or '{' after function parameter list.")
		decl.body = parse_block(p)
	}

	return decl
}

parse_pub_decl :: proc(p: ^Parser) -> ^PubDecl {
	decl := new(PubDecl)
	decl.token = previous(p)
	if match(p, .FUNC) {
		decl.decl = parse_func_decl(p, "function")
	} else if match(p, .CLASS) {
		decl.decl = parse_class_decl(p)
	} else {
		error(p, peek(p), "Only functions or classes can be set as public.")
	}
	return decl
}


get_rule :: proc(type: TokenType) -> ^ParseRule {
	return &rules[type]
}

parse_precedence :: proc(p: ^Parser, precedence: Precedence) -> Expr {
	advance(p)

	// if is_at_end(p) {
	// 	error(p, previous(p), "Expect expression.")
	// 	return nil
	// }

	prefix_rule := get_rule(previous(p).type).prefix
	if prefix_rule == nil {
		error(p, previous(p), "Expect expression.")
		return nil
	}

	can_assign := precedence <= .ASSIGNMENT
	expr := prefix_rule(p, can_assign)

	for precedence <= get_rule(peek(p).type).precedence {
		advance(p)
		infix_rule := get_rule(previous(p).type).infix
		if infix_rule != nil {
			expr = infix_rule(p, expr, can_assign)
		}
	}

	if can_assign && match(p, .EQUAL) {
		error(p, previous(p), "Invalid assignment target.")
	}

	return expr
}

// Statement parsers

parse_statement :: proc(p: ^Parser) -> Stmt {
	when CHAOTIC {
		if match(p, .IFNT) {
			return parse_if_stmt(p, true)
		} else if match(p, .WHILENT) {
			return parse_while_stmt(p, true)
		}
	}

	switch {
	case match(p, .IF):
		return parse_if_stmt(p, false)
	case match(p, .WHILE):
		return parse_while_stmt(p, false)
	case match(p, .BREAK):
		return parse_break_stmt(p)
	case match(p, .CONTINUE):
		return parse_continue_stmt(p)
	case match(p, .FOR):
		return parse_for_stmt(p)
	case match(p, .LSQUIRLY):
		return parse_block(p)
	case match(p, .PRINT):
		return parse_print_stmt(p)
	case match(p, .RETURN):
		return parse_return_stmt(p)
	case match(p, .EXIT):
		return parse_exit_stmt(p)
	case match(p, .SWITCH):
		return parse_switch_stmt(p)
	case match(p, .SEMI):
		stmt := new(EmptyStmt)
		stmt.token = previous(p)
		return stmt
	}

	return parse_expression_stmt(p)
}

parse_if_stmt :: proc(p: ^Parser, is_ifnt: bool) -> ^IfStmt {
	stmt := new(IfStmt)
	stmt.token = previous(p)
	stmt.is_ifnt = is_ifnt
	stmt.condition = parse_expression(p)

	consume(p, .LSQUIRLY, "Expect '{' after condition.")
	stmt.then_branch = parse_block(p)

	if match(p, .ELSE) {
		consume(p, .LSQUIRLY, "Expect '{' after else.")
		stmt.else_branch = parse_block(p)
	}
	return stmt
}

parse_while_stmt :: proc(p: ^Parser, is_whilent: bool) -> ^WhileStmt {
	stmt := new(WhileStmt)
	stmt.token = previous(p)
	stmt.is_whilent = is_whilent
	stmt.condition = parse_expression(p)

	consume(p, .LSQUIRLY, "Expect '{' after condition.")
	stmt.body = parse_block(p)
	return stmt
}

parse_break_stmt :: proc(p: ^Parser) -> ^BreakStmt {
	stmt := new(BreakStmt)
	stmt.token = previous(p)
	consume_newline(p, "break")
	return stmt
}

parse_continue_stmt :: proc(p: ^Parser) -> ^ContinueStmt {
	stmt := new(ContinueStmt)
	stmt.token = previous(p)
	consume_newline(p, "continue")
	return stmt
}

parse_for_stmt :: proc(p: ^Parser) -> Stmt {
	token := previous(p)
	// Differentiate between for-in and classic for loop
	if check(p, .IDENT) && p.tokens[p.current + 1].type == .IN {
		stmt := new(ForInStmt)
		stmt.token = token
		stmt.var_name = advance(p)
		advance(p) // consume IN
		stmt.iterable = parse_expression(p)
		consume(p, .LSQUIRLY, "Expect '{' after iterable.")
		stmt.body = parse_block(p)
		return stmt
	}

	stmt := new(ForStmt)
	stmt.token = token
	if match(p, .SEMI) {
		empty := new(EmptyStmt)
		empty.token = previous(p)
		stmt.initializer = empty
	} else if match(p, .VAR, .VAL) {
		stmt.initializer = parse_var_decl(p)
	} else {
		stmt.initializer = parse_expression_stmt(p)
	}

	if !match(p, .SEMI) {
		stmt.condition = parse_expression(p)
		consume_newline(p, "loop condition")
	}

	if !match(p, .LSQUIRLY) {
		stmt.increment = parse_expression(p)
		consume(p, .LSQUIRLY, "Expect '{' after for clauses.")
	}

	stmt.body = parse_block(p)
	return stmt
}

parse_block :: proc(p: ^Parser) -> ^BlockStmt {
	stmt := new(BlockStmt)
	stmt.token = previous(p)
	declarations := make([dynamic]Decl)

	for !check(p, .RSQUIRLY) && !is_at_end(p) {
		decl := parse_declaration(p)
		append(&declarations, decl)

		if p.panic_mode {
			synchronize(p)
		}
	}
	stmt.declarations = declarations[:]
	consume(p, .RSQUIRLY, "Expect '}' after block.")

	return stmt
}

parse_print_stmt :: proc(p: ^Parser) -> ^PrintStmt {
	stmt := new(PrintStmt)
	stmt.token = previous(p)
	stmt.expr = parse_expression(p)
	consume_newline(p, "value")
	return stmt
}

parse_return_stmt :: proc(p: ^Parser) -> ^ReturnStmt {
	stmt := new(ReturnStmt)
	stmt.token = previous(p)
	if !match(p, .SEMI) {
		stmt.value = parse_expression(p)
		consume_newline(p, "return value")
	}
	return stmt
}

parse_exit_stmt :: proc(p: ^Parser) -> ^ExitStmt {
	stmt := new(ExitStmt)
	stmt.token = previous(p)
	if !match(p, .SEMI) {
		stmt.code = parse_expression(p)
		consume_newline(p, "exit code")
	}
	return stmt
}

parse_switch_stmt :: proc(p: ^Parser) -> ^SwitchStmt {
	stmt := new(SwitchStmt)
	stmt.token = previous(p)
	cases := make([dynamic]SwitchCase)
	has_else_clause := false

	if match(p, .LSQUIRLY) {
		// No condition
	} else {
		stmt.condition = parse_expression(p)
		consume(p, .LSQUIRLY, "Expect '{' after switch condition.")
	}

	for !match(p, .RSQUIRLY) && !is_at_end(p) {
		if match(p, .ELSE) {
			has_else_clause = true
			consume(p, .FAT_ARROW, "Expect '=>' after 'else'.")
			stmt.else_branch = parse_statement(p)
			if check(p, .SEMI) {advance(p)}
			consume(p, .RSQUIRLY, "'else' must be the last case.")
			break
		}

		case_node: SwitchCase
		case_node.condition = parse_expression(p)
		consume(p, .FAT_ARROW, "Expect '=>' after case.")
		case_node.body = parse_statement(p)
		if check(p, .SEMI) {advance(p)}
		append(&cases, case_node)
	}

	if !has_else_clause {
		error(p, peek(p), "Switch statement must have an 'else' case.")
	}

	stmt.cases = cases[:]
	return stmt
}

parse_expression_stmt :: proc(p: ^Parser) -> ^ExprStmt {
	stmt := new(ExprStmt)
	stmt.expr = parse_expression(p)
	stmt.token = previous(p)
	consume_newline(p, "expression")
	return stmt
}

// Expression parsers

// Parse an expression, treating newlines as expression-separating infix operators.
parse_expression_top :: proc(p: ^Parser) -> Expr {
	fst := parse_expression(p)
	if !match(p, .NEWLINE) {
		return fst
	}

	seq := new(SequenceExpr)
	seq.token = previous(p)
	seq.left = fst
	seq.operator = previous(p)
	if is_at_end(p) || check(p, .RSQUIRLY) {
		seq.right = nil
	} else {
		seq.right = parse_expression_top(p)
	}
	return seq
}

// Parse an expression.
parse_expression :: proc(p: ^Parser) -> Expr {
	return parse_precedence(p, .ASSIGNMENT)
}

//---------------------------------------------------------
// Prefix Rules
//---------------------------------------------------------

parse_if_expr :: proc(p: ^Parser, can_assign: bool) -> Expr {
	expr := new(IfExpr)
	expr.token = previous(p)
	expr.is_ifnt = expr.token.type == .IFNT
	expr.condition = parse_expression(p)

	consume(p, .LSQUIRLY, "Expect '{' after condition.")
	expr.then_branch = parse_block_expr(p, can_assign).(^BlockExpr)

	if match(p, .ELSE) {
		consume(p, .LSQUIRLY, "Expect '{' after else.")
		expr.else_branch = parse_block_expr(p, can_assign).(^BlockExpr)
	}
	return expr
}

parse_switch_expr :: proc(p: ^Parser, can_assign: bool) -> Expr {
	expr := new(SwitchExpr)
	expr.token = previous(p)
	cases := make([dynamic]ExprSwitchCase)
	has_else_clause := false

	if match(p, .LSQUIRLY) {
		// No condition
	} else {
		expr.condition = parse_expression(p)
		consume(p, .LSQUIRLY, "Expect '{' after switch condition.")
	}

	for !match(p, .RSQUIRLY) && !is_at_end(p) {
		if match(p, .ELSE) {
			has_else_clause = true
			consume(p, .FAT_ARROW, "Expect '=>' after 'else'.")
			if match(p, .LSQUIRLY) {} 	// for block expressions
			expr.else_branch = parse_expression(p)
			if match(p, .NEWLINE) {}
			consume(p, .RSQUIRLY, "'else' must be the last case.")
			break
		}

		case_node: ExprSwitchCase
		case_node.condition = parse_expression(p)
		consume(p, .FAT_ARROW, "Expect '=>' after case.")
		if match(p, .LSQUIRLY) {} 	// for block expressions
		case_node.body = parse_expression(p)
		if match(p, .NEWLINE) {}
		append(&cases, case_node)
	}

	if !has_else_clause {
		error(p, peek(p), "Switch expression must have an 'else' case.")
	}

	expr.cases = cases[:]
	return expr
}

parse_while :: proc(p: ^Parser, can_assign: bool) -> Expr {
	expr := new(WhileExpr)
	expr.token = previous(p)
	expr.is_whilent = expr.token.type == .WHILENT
	expr.condition = parse_expression(p)

	consume(p, .LSQUIRLY, "Expect '{' after condition.")
	expr.body = parse_block_expr(p, can_assign).(^BlockExpr)
	return expr
}

parse_for :: proc(p: ^Parser, can_assign: bool) -> Expr {
	token := previous(p)

	// Differentiate between for-in and classic for loop
	if check(p, .IDENT) && p.tokens[p.current + 1].type == .IN {
		expr := new(ForInExpr)
		expr.token = token
		expr.var_name = advance(p)
		advance(p) // consume IN
		expr.iterable = parse_expression(p)
		consume(p, .LSQUIRLY, "Expect '{' after iterable.")
		expr.body = parse_block_expr(p, can_assign).(^BlockExpr)
		return expr
	}

	stmt := new(ForExpr)
	stmt.token = token
	if match(p, .SEMI) {
		stmt.initializer = nil
	} else if match(p, .VAR, .VAL) {
		stmt.initializer = parse_var_decl_expression(p, can_assign)
	} else {
		stmt.initializer = parse_expression(p)
	}
	consume(p, .SEMI, "Expect ';' after initializer.")

	if !match(p, .SEMI) {
		stmt.condition = parse_expression(p)
		consume(p, .SEMI, "Expect ';' after loop condition.")
	}

	if !match(p, .LSQUIRLY) {
		stmt.increment = parse_expression(p)
		consume(p, .LSQUIRLY, "Expect '{' after for clauses.")
	}

	stmt.body = parse_block_expr(p, can_assign).(^BlockExpr)
	return stmt
}

parse_var_decl_expression :: proc(p: ^Parser, can_assign: bool) -> Expr {
	expr := new(VarDeclExpr)
	expr.token = previous(p)
	expr.is_final = expr.token.type == .VAL
	bindings := make([dynamic]VarBinding)

	for {
		binding: VarBinding
		binding.name = consume(p, .IDENT, "Expect variable name.")
		if match(p, .EQUAL) {
			binding.initializer = parse_expression(p)
		}
		append(&bindings, binding)

		if !match(p, .COMMA) {break}
	}
	expr.bindings = bindings[:]
	return expr
}

parse_print :: proc(p: ^Parser, can_assign: bool) -> Expr {
	expr := new(PrintExpr)
	expr.token = previous(p)
	expr.expr = parse_expression(p)
	return expr
}

parse_grouping :: proc(p: ^Parser, can_assign: bool) -> Expr {
	token := previous(p)
	expr := parse_expression(p)
	consume(p, .RPAREN, "Expect ')' after expression.")
	grouping := new(GroupingExpr)
	grouping.token = token
	grouping.expression = expr
	return grouping
}

parse_list :: proc(p: ^Parser, can_assign: bool) -> Expr {
	list := new(ListExpr)
	list.token = previous(p)
	elements := make([dynamic]Expr)
	if !check(p, .RSQUARE) {
		for {
			append(&elements, parse_expression(p))
			if !match(p, .COMMA) {break}
		}
	}
	list.elements = elements[:]
	consume(p, .RSQUARE, "Expect ']' after list elements.")
	return list
}

parse_unary :: proc(p: ^Parser, can_assign: bool) -> Expr {
	operator := previous(p)
	right := parse_precedence(p, .UNARY)
	unary := new(UnaryExpr)
	unary.token = operator
	unary.operator = operator
	unary.right = right
	return unary
}

/*
Translate escape sequences in a string literal.

This function allocates a string, but doesn't take ownership of the input; therefore
the input will still need to be freed if necessary. 

In this compiler, it is used to create an escape-sequenced string out of a slice 
of the program input itself, which should **NOT** be freed until the program ends; 
therefore it is not necessary for it to take ownership.

So far, only the newline and tab sequences are supported.
*/
@(private = "file")
add_escape_sequences :: proc(str: string) -> string {
	sequences := make(map[byte]byte)
	sequences['n'] = '\n'
	sequences['t'] = '\t'
	defer delete(sequences)

	sb := strings.builder_make()
	defer strings.builder_destroy(&sb)

	escaped := false
	for i := 0; i < len(str); i += 1 {
		c := str[i]
		if !escaped && c == '\\' && i + 1 < len(str) {
			if replacement, ok := sequences[str[i + 1]]; ok {
				strings.write_byte(&sb, replacement)
				i += 1
				continue
			}
		}
		strings.write_byte(&sb, c)
	}

	return strings.clone(strings.to_string(sb))
}

parse_literal :: proc(p: ^Parser, can_assign: bool) -> Expr {
	literal := new(LiteralExpr)
	literal.token = previous(p)

	#partial switch literal.token.type {
	case .STRING:
		literal.value = add_escape_sequences(literal.token.lexeme[1:len(literal.token.lexeme) - 1])
	case .NUMBER:
		value, ok := strconv.parse_f64(literal.token.lexeme)
		if !ok {
			error(
				p,
				literal.token,
				fmt.tprintf(
					"'%s' is not a valid 64-bit floating point number.",
					literal.token.lexeme,
				),
			)
		}
		literal.value = value
	case .TRUE:
		literal.value = true
	case .FALSE:
		literal.value = false
	case .NIL:
		literal.value = nil
	case:
		error(
			p,
			literal.token,
			fmt.tprintf(
				"'%s' is not a valid literal. This is a compiler bug.",
				literal.token.lexeme,
			),
		)
	}

	return literal
}

parse_variable :: proc(p: ^Parser, can_assign: bool) -> Expr {
	name := previous(p)

	// No-paren string call: `puts "hello"`
	// Only valid as a call, not as an assignment target
	if match(p, .STRING) {
		str_literal := new(LiteralExpr)
		str_literal.token = previous(p)
		str_literal.value = add_escape_sequences(
			str_literal.token.lexeme[1:len(str_literal.token.lexeme) - 1],
		)

		call := new(CallExpr)

		// The `token` and `rparen` fields for this type of call are inconsistent
		// from a normal call since for a normal call those two are `(` and
		// `)` respectively, but in this case there are no parentheses.
		// Therefore, `token` for this case is the function name and `rparen`
		// is the string.
		call.token = name
		call.rdelimiter = previous(p)
		call.arguments = make([]Expr, 1)

		// The no-paren function call is a bit limited as only functions assigned
		// to variables can use the syntax.
		callee := new(VariableExpr)
		callee.token = name
		callee.name = name

		call.arguments[0] = str_literal
		call.callee = callee
		return call
	}

	if can_assign && match(p, .EQUAL) {
		value := parse_expression(p)
		assign := new(AssignExpr)
		assign.token = name
		assign.name = name
		assign.value = value
		return assign
	}

	var_expr := new(VariableExpr)
	var_expr.token = name
	var_expr.name = name
	return var_expr
}

parse_super :: proc(p: ^Parser, can_assign: bool) -> Expr {
	super_expr := new(SuperExpr)
	super_expr.token = previous(p)
	consume(p, .DOT, "Expect '.' after 'super'.")
	super_expr.method = consume(p, .IDENT, "Expect superclass method name.")

	invoked := false
	method_args := make([dynamic]Expr, 0, 1)
	defer if !invoked {
		delete(method_args)
	}

	// was the retrieved method immediately invoked?
	if match(p, .LPAREN) {
		invoked = true
		if !check(p, .RPAREN) {
			for {
				append(&method_args, parse_expression(p))
				if !match(p, .COMMA) {break}
			}
		}
		super_expr.method_args = method_args[:]
		consume(p, .RPAREN, "Expect ')' after method parameters.")
	} else {
		super_expr.method_args = nil
	}

	return super_expr
}

parse_this :: proc(p: ^Parser, can_assign: bool) -> Expr {
	this_expr := new(ThisExpr)
	this_expr.token = previous(p)
	return this_expr
}

parse_it :: proc(p: ^Parser, can_assign: bool) -> Expr {
	it_expr := new(ItExpr)
	it_expr.token = previous(p)
	return it_expr
}

parse_lambda :: proc(p: ^Parser, can_assign: bool) -> Expr {
	lambda := new(LambdaExpr)
	lambda.token = previous(p)
	lambda.func_decl = parse_func_body(p, Token{type = .IDENT, lexeme = "lambda"})
	return lambda
}

parse_block_expr :: proc(p: ^Parser, can_assign: bool) -> Expr {
	expr := new(BlockExpr)
	expr.token = previous(p) // the '{'
	expr.expression = parse_expression_top(p)
	consume(p, .RSQUIRLY, "Expect '}' after block.")
	return expr
}

parse_break :: proc(p: ^Parser, can_assign: bool) -> Expr {
	expr := new(BreakExpr)
	expr.token = previous(p)
	return expr
}

parse_continue :: proc(p: ^Parser, can_assign: bool) -> Expr {
	expr := new(ContinueExpr)
	expr.token = previous(p)
	return expr
}

parse_return :: proc(p: ^Parser, can_assign: bool) -> Expr {
	expr := new(ReturnExpr)
	expr.token = previous(p)
	if !match(p, .NEWLINE) && !check(p, .RSQUIRLY) && !is_at_end(p) {
		expr.value = parse_expression(p)
	}
	return expr
}

parse_exit :: proc(p: ^Parser, can_assign: bool) -> Expr {
	expr := new(ExitExpr)
	expr.token = previous(p)
	if !match(p, .NEWLINE) && !check(p, .RSQUIRLY) && !is_at_end(p) {
		expr.code = parse_expression(p)
	}
	return expr
}

//---------------------------------------------------------
// Infix Rules
//---------------------------------------------------------

parse_pipe :: proc(p: ^Parser, left: Expr, can_assign: bool) -> Expr {
	operator := previous(p)
	rule := get_rule(operator.type)
	right := parse_precedence(p, cast(Precedence)(cast(int)rule.precedence + 1))

	pipe := new(PipeExpr)
	pipe.token = operator
	pipe.left = left
	pipe.operator = operator
	pipe.right = right
	return pipe
}

parse_logical :: proc(p: ^Parser, left: Expr, can_assign: bool) -> Expr {
	operator := previous(p)
	rule := get_rule(operator.type)
	right := parse_precedence(p, cast(Precedence)(cast(int)rule.precedence + 1))

	logical := new(LogicalExpr)
	logical.token = operator
	logical.left = left
	logical.operator = operator
	logical.right = right
	return logical
}

parse_binary :: proc(p: ^Parser, left: Expr, can_assign: bool) -> Expr {
	operator := previous(p)
	rule := get_rule(operator.type)
	// Add 1 to precedence for left-associative operators
	right := parse_precedence(p, cast(Precedence)(cast(int)rule.precedence + 1))

	binary := new(BinaryExpr)
	binary.token = operator
	binary.left = left
	binary.operator = operator
	binary.right = right
	return binary
}

parse_call :: proc(p: ^Parser, left: Expr, can_assign: bool) -> Expr {
	call := new(CallExpr)
	call.token = previous(p) // The '(' token
	call.callee = left
	arguments := make([dynamic]Expr)

	if !check(p, .RPAREN) {
		for {
			append(&arguments, parse_expression(p))
			if !match(p, .COMMA) {break}
		}
	}
	call.arguments = arguments[:]
	call.rdelimiter = consume(p, .RPAREN, "Expect ')' after arguments.")
	return call
}

parse_dot :: proc(p: ^Parser, left: Expr, can_assign: bool) -> Expr {
	dot := previous(p) // The '.' token
	property := consume(p, .IDENT, "Expect property name after '.'.")
	if can_assign && match(p, .EQUAL) {
		equals := previous(p)
		value := parse_expression(p)
		set_expr := new(SetExpr)
		set_expr.token = equals // The '=' token
		set_expr.receiver = left
		set_expr.property = property
		set_expr.value = value
		return set_expr
	}
	get_expr := new(GetExpr)
	get_expr.token = dot
	get_expr.receiver = left
	get_expr.property = property
	return get_expr
}

parse_subscript :: proc(p: ^Parser, left: Expr, can_assign: bool) -> Expr {
	bracket := previous(p) // The '[' token
	index := parse_expression(p)
	consume(p, .RSQUARE, "Expect ']' after index.")
	if can_assign && match(p, .EQUAL) {
		value := parse_expression(p)
		sub_set := new(SubscriptSetExpr)
		sub_set.token = bracket
		sub_set.receiver = left
		sub_set.index = index
		sub_set.value = value
		return sub_set
	}
	sub := new(SubscriptExpr)
	sub.token = bracket
	sub.receiver = left
	sub.index = index
	return sub
}

//---------------------------------------------------------
// Rule Table
//---------------------------------------------------------

rules: [TokenType]ParseRule = {
	.LPAREN        = {parse_grouping, parse_call, .CALL},
	.RPAREN        = {nil, nil, .NONE},
	.LSQUIRLY      = {parse_block_expr, nil, .NONE},
	.RSQUIRLY      = {nil, nil, .NONE},
	.LSQUARE       = {parse_list, parse_subscript, .CALL},
	.RSQUARE       = {nil, nil, .NONE},
	.COMMA         = {nil, nil, .NONE},
	.DOT           = {nil, parse_dot, .CALL},
	.MINUS         = {parse_unary, parse_binary, .TERM},
	.PLUS          = {nil, parse_binary, .TERM},
	.SEMI          = {nil, nil, .NONE},
	.SLASH         = {nil, parse_binary, .FACTOR},
	.STAR          = {nil, parse_binary, .FACTOR},
	.PERCENT       = {nil, parse_binary, .FACTOR},
	.NEWLINE       = {nil, nil, .NONE},
	.BANG_EQUAL    = {nil, parse_binary, .EQUALITY},
	.BAR_GREATER   = {nil, parse_pipe, .PIPELINE},
	.EQUAL         = {nil, nil, .NONE},
	.EQUAL_EQUAL   = {nil, parse_binary, .EQUALITY},
	.FAT_ARROW     = {nil, nil, .NONE},
	.GREATER       = {nil, parse_binary, .COMPARISON},
	.GREATER_EQUAL = {nil, parse_binary, .COMPARISON},
	.LESS          = {nil, parse_binary, .COMPARISON},
	.LESS_EQUAL    = {nil, parse_binary, .COMPARISON},
	.IDENT         = {parse_variable, nil, .NONE},
	.STRING        = {parse_literal, nil, .NONE},
	.NUMBER        = {parse_literal, nil, .NONE},
	.AND           = {nil, parse_logical, .AND},
	.BREAK         = {parse_break, nil, .NONE},
	.CONTINUE      = {parse_continue, nil, .NONE},
	.CLASS         = {nil, nil, .NONE},
	.ELSE          = {nil, nil, .NONE},
	.EXIT          = {parse_exit, nil, .NONE},
	.FALSE         = {parse_literal, nil, .NONE},
	.FOR           = {parse_for, nil, .NONE},
	.FUNC          = {parse_lambda, nil, .NONE},
	.IF            = {parse_if_expr, nil, .CONDITIONAL},
	.IFNT          = {parse_if_expr, nil, .CONDITIONAL},
	.IN            = {nil, nil, .NONE},
	.IT            = {parse_it, nil, .NONE},
	.NIL           = {parse_literal, nil, .NONE},
	.NOT           = {parse_unary, nil, .NONE},
	.OR            = {nil, parse_logical, .OR},
	.PRINT         = {parse_print, nil, .NONE},
	.PUB           = {nil, nil, .NONE},
	.RETURN        = {parse_return, nil, .NONE},
	.SWITCH        = {parse_switch_expr, nil, .CONDITIONAL},
	.SUPER         = {parse_super, nil, .NONE},
	.THIS          = {parse_this, nil, .NONE},
	.TRUE          = {parse_literal, nil, .NONE},
	.USE           = {nil, nil, .NONE},
	.WHILE         = {parse_while, nil, .NONE},
	.WHILENT       = {parse_while, nil, .NONE},
	.VAR           = {parse_var_decl_expression, nil, .NONE},
	.VAL           = {parse_var_decl_expression, nil, .NONE},
	.EOF           = {nil, nil, .NONE},
}

Parser :: struct {
	tokens:     []Token,
	current:    int,
	had_error:  bool,
	panic_mode: bool,
}

init_parser :: proc(tokens: []Token) -> Parser {
	return Parser{tokens = tokens, current = 0, had_error = false, panic_mode = false}
}

error :: proc(p: ^Parser, token: Token, message: string) {
	if p.panic_mode {return}
	p.panic_mode = true

	color_red(os.stderr, "parse error ")

	if token.type == .EOF {
		fmt.eprint("at end")
	} else if token.type == .NEWLINE {
		fmt.eprint("at end of line")
	} else {
		fmt.eprintf("at '%s'", token.lexeme)
	}

	fmt.eprintfln(": %s", message)
	fmt.eprintfln("  on [line %d]", token.line)
	p.had_error = true
}

peek :: proc(p: ^Parser) -> Token {
	return p.tokens[p.current]
}

previous :: proc(p: ^Parser) -> Token {
	return p.tokens[p.current - 1]
}

is_at_end :: proc(p: ^Parser) -> bool {
	return peek(p).type == .EOF
}

check :: proc(p: ^Parser, type: TokenType) -> bool {
	if is_at_end(p) {return false}
	return peek(p).type == type
}

/* Like match() but doesn't advance */
check_any :: proc(p: ^Parser, types: ..TokenType) -> bool {
	if is_at_end(p) {return false}
	for type in types {
		if check(p, type) {return true}
	}
	return false
}

advance :: proc(p: ^Parser) -> Token {
	if !is_at_end(p) {p.current += 1}
	return previous(p)
}

match :: proc(p: ^Parser, types: ..TokenType) -> bool {
	for type in types {
		if check(p, type) {
			advance(p)
			return true
		}
	}
	return false
}

consume :: proc(p: ^Parser, type: TokenType, message: string) -> Token {
	if check(p, type) {return advance(p)}
	error(p, peek(p), message)
	return peek(p)
}

consume_newline :: proc(p: ^Parser, message: string) {
	consume(p, .NEWLINE, fmt.tprintf("Expect newline after %s.", message))
}

synchronize :: proc(p: ^Parser) {
	p.panic_mode = false

	for !is_at_end(p) {
		if previous(p).type == .NEWLINE {return}

		#partial switch peek(p).type {
		case .BREAK,
		     .CONTINUE,
		     .CLASS,
		     .FUNC,
		     .EXIT,
		     .FOR,
		     .IF,
		     .IFNT,
		     .WHILE,
		     .WHILENT,
		     .PRINT,
		     .RETURN,
		     .SWITCH,
		     .PUB,
		     .USE,
		     .VAR,
		     .VAL:
			return
		case: // do nothing.
		}

		advance(p)
	}
}

// AST freeing functions

free_decls :: proc(decls: []Decl) {
	for decl in decls {
		free_decl(decl)
	}
	delete(decls)
}

free_decl :: proc(decl: Decl) {
	if decl == nil {
		return
	}

	switch d in decl {
	case ^ClassDecl:
		for method in d.methods {
			free_decl(method)
		}
		delete(d.methods)
		free(d)
	case ^FuncDecl:
		switch b in d.body {
		case ^BlockStmt:
			free_stmt(b)
		case Expr:
			free_expr(b)
		}
		delete(d.params)
		free(d)
	case ^ModuleDecl:
		free(d)
	case ^PubDecl:
		free_decl(d.decl)
		free(d)
	case ^VarDecl:
		for binding in d.bindings {
			free_expr(binding.initializer)
		}
		delete(d.bindings)
		free(d)
	case Stmt:
		free_stmt(d)
	}
}

free_stmt :: proc(stmt: Stmt) {
	if stmt == nil {
		return
	}

	switch s in stmt {
	case ^BlockStmt:
		free_decls(s.declarations)
		free(s)
	case ^BreakStmt:
		free(s)
	case ^ContinueStmt:
		free(s)
	case ^EmptyStmt:
		free(s)
	case ^ExitStmt:
		free_expr(s.code)
		free(s)
	case ^ExprStmt:
		free_expr(s.expr)
		free(s)
	case ^ForInStmt:
		free_expr(s.iterable)
		free_stmt(s.body)
		free(s)
	case ^ForStmt:
		switch iz in s.initializer {
		case ^VarDecl:
			free_decl(iz)
		case ^ExprStmt:
			free_expr(iz.expr)
			free(iz)
		case ^EmptyStmt:
			free(iz)
		}
		free_decls(s.body.declarations)
		free(s.body)
		free_expr(s.condition)
		free_expr(s.increment)
		free(s)
	case ^IfStmt:
		free_expr(s.condition)
		free_decls(s.then_branch.declarations)
		free(s.then_branch)
		if s.else_branch != nil {
			free_decls(s.else_branch.declarations)
			free(s.else_branch)
		}
		free(s)
	case ^PrintStmt:
		free_expr(s.expr)
		free(s)
	case ^ReturnStmt:
		free_expr(s.value)
		free(s)
	case ^SwitchStmt:
		free_expr(s.condition)
		for c in s.cases {
			free_expr(c.condition)
			free_stmt(c.body)
		}
		delete(s.cases)
		free_stmt(s.else_branch)
		free(s)
	case ^WhileStmt:
		free_expr(s.condition)
		free_decls(s.body.declarations)
		free(s.body)
		free(s)
	}
}

free_expr :: proc(expr: Expr) {
	if expr == nil {
		return
	}

	switch e in expr {
	case ^AssignExpr:
		free_expr(e.value)
		free(e)
	case ^BinaryExpr:
		free_expr(e.left)
		free_expr(e.right)
		free(e)
	case ^BreakExpr:
		free(e)
	case ^BlockExpr:
		free_expr(e.expression)
		free(e)
	case ^ContinueExpr:
		free(e)
	case ^CallExpr:
		for arg in e.arguments {
			free_expr(arg)
		}
		delete(e.arguments)
		free_expr(e.callee)
		free(e)
	case ^ExitExpr:
		free_expr(e.code)
		free(e)
	case ^ForInExpr:
		free_expr(e.iterable)
		free_expr(e.body)
		free(e)
	case ^ForExpr:
		free_expr(e.initializer)
		free_expr(e.body)
		free_expr(e.condition)
		free_expr(e.increment)
		free(e)
	case ^GetExpr:
		free_expr(e.receiver)
		free(e)
	case ^GroupingExpr:
		free_expr(e.expression)
		free(e)
	case ^IfExpr:
		free_expr(e.condition)
		free_expr(e.then_branch)
		free_expr(e.else_branch)
		free(e)
	case ^ItExpr:
		free(e)
	case ^LambdaExpr:
		free_decl(e.func_decl)
		free(e)
	case ^ListExpr:
		for element in e.elements {
			free_expr(element)
		}
		delete(e.elements)
		free(e)
	case ^LiteralExpr:
		if v, ok := e.value.(string); ok {
			/* This was allocated when adding escape sequences */
			delete(v)
		}
		free(e)
	case ^LogicalExpr:
		free_expr(e.left)
		free_expr(e.right)
		free(e)
	case ^PipeExpr:
		free_expr(e.left)
		free_expr(e.right)
		free(e)
	case ^PrintExpr:
		free_expr(e.expr)
		free(e)
	case ^ReturnExpr:
		free_expr(e.value)
		free(e)
	case ^SequenceExpr:
		free_expr(e.left)
		free_expr(e.right)
		free(e)
	case ^SetExpr:
		free_expr(e.receiver)
		free_expr(e.value)
		free(e)
	case ^SubscriptExpr:
		free_expr(e.receiver)
		free_expr(e.index)
		free(e)
	case ^SubscriptSetExpr:
		free_expr(e.receiver)
		free_expr(e.index)
		free_expr(e.value)
		free(e)
	case ^SuperExpr:
		free(e)
	case ^SwitchExpr:
		free_expr(e.condition)
		for c in e.cases {
			free_expr(c.condition)
			free_expr(c.body)
		}
		delete(e.cases)
		free_expr(e.else_branch)
		free(e)
	case ^ThisExpr:
		free(e)
	case ^UnaryExpr:
		free_expr(e.right)
		free(e)
	case ^VarDeclExpr:
		for binding in e.bindings {
			free_expr(binding.initializer)
		}
		delete(e.bindings)
		free(e)
	case ^VariableExpr:
		free(e)
	case ^WhileExpr:
		free_expr(e.body)
		free_expr(e.condition)
		free(e)
	}
}


// AST pretty-printer

// allocates a string
ast_string :: proc(decls: []Decl) -> string {
	b := strings.builder_make()
	defer strings.builder_destroy(&b)

	for decl in decls {
		print_decl(&b, decl, 0)
	}
	return strings.clone(strings.to_string(b))
}

// allocates a string
ast_string_expr :: proc(expr: Expr) -> string {
	b := strings.builder_make()
	defer strings.builder_destroy(&b)

	print_expr(&b, expr, 0)
	return strings.clone(strings.to_string(b))
}

print_indent :: proc(b: ^strings.Builder, indent: int) {
	for i := 0; i < indent; i += 1 {
		strings.write_string(b, "  ")
	}
}

print_decl :: proc(b: ^strings.Builder, decl: Decl, indent: int) {
	if decl == nil {
		return
	}

	switch d in decl {
	case ^ClassDecl:
		print_indent(b, indent)
		fmt.sbprintf(b, "(class %s", d.name.lexeme)
		if d.superclass != nil {
			superclass: Token = d.superclass.?
			fmt.sbprintf(b, " < %s", superclass.lexeme)
		}
		strings.write_string(b, "\n")
		for method in d.methods {
			print_decl(b, method, indent + 1)
		}
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^FuncDecl:
		print_indent(b, indent)
		fmt.sbprintf(b, "(func %s (", d.name.lexeme)
		for param, i in d.params {
			if i > 0 {strings.write_string(b, " ")}
			strings.write_string(b, param.lexeme)
		}
		strings.write_string(b, ")\n")

		#partial switch body in d.body {
		case ^BlockStmt:
			print_stmt(b, body, indent + 1)
		case Expr:
			print_indent(b, indent + 1)
			strings.write_string(b, "=>\n")
			print_expr(b, body, indent + 2)
		}
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^ModuleDecl:
		print_indent(b, indent)
		fmt.sbprintf(b, "(use %s)\n", d.path.lexeme)
	case ^PubDecl:
		print_indent(b, indent)
		strings.write_string(b, "(pub\n")
		print_decl(b, d.decl, indent + 1)
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^VarDecl:
		print_indent(b, indent)
		kind := d.is_final ? "val" : "var"
		fmt.sbprintf(b, "(%s ", kind)
		for binding, i in d.bindings {
			if i > 0 {strings.write_string(b, " ")}
			strings.write_string(b, binding.name.lexeme)
			if binding.initializer != nil {
				init: Expr = binding.initializer
				strings.write_string(b, " =\n")
				print_expr(b, init, indent + 1)
			}
		}
		strings.write_string(b, ")\n")
	case Stmt:
		print_stmt(b, d, indent)
	case:
		fmt.sbprintf(b, "<Unknown Decl %T>\n", d)
	}
}

print_stmt :: proc(b: ^strings.Builder, stmt: Stmt, indent: int) {
	if stmt == nil {
		return
	}

	switch s in stmt {
	case ^BlockStmt:
		print_indent(b, indent)
		strings.write_string(b, "(block\n")
		for d in s.declarations {
			print_decl(b, d, indent + 1)
		}
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^BreakStmt:
		print_indent(b, indent)
		strings.write_string(b, "(break)\n")
	case ^ContinueStmt:
		print_indent(b, indent)
		strings.write_string(b, "(continue)\n")
	case ^EmptyStmt:
		print_indent(b, indent)
		strings.write_string(b, "(empty)\n")
	case ^ExitStmt:
		print_indent(b, indent)
		strings.write_string(b, "(exit")
		if s.code != nil {
			code: Expr = s.code
			strings.write_string(b, "\n")
			print_expr(b, code, indent + 1)
			print_indent(b, indent)
		}
		strings.write_string(b, ")\n")
	case ^ExprStmt:
		print_indent(b, indent)
		strings.write_string(b, "(expr\n")
		print_expr(b, s.expr, indent + 1)
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^ForInStmt:
		print_indent(b, indent)
		fmt.sbprintf(b, "(for-in %s\n", s.var_name.lexeme)
		print_expr(b, s.iterable, indent + 1)
		print_stmt(b, s.body, indent + 1)
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^ForStmt:
		print_indent(b, indent)
		strings.write_string(b, "(for\n")
		if true {
			print_indent(b, indent + 1)
			strings.write_string(b, "init:\n")
			#partial switch init in s.initializer {
			case ^VarDecl:
				print_decl(b, init, indent + 2)
			case ^ExprStmt:
				print_stmt(b, init, indent + 2)
			case ^EmptyStmt:
				print_stmt(b, init, indent + 2)
			}
		}
		if s.condition != nil {
			cond: Expr = s.condition
			print_indent(b, indent + 1)
			strings.write_string(b, "cond:\n")
			print_expr(b, cond, indent + 2)
		}
		if s.increment != nil {
			inc: Expr = s.increment
			print_indent(b, indent + 1)
			strings.write_string(b, "inc:\n")
			print_expr(b, inc, indent + 2)
		}
		print_stmt(b, s.body, indent + 1)
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^IfStmt:
		print_indent(b, indent)
		kind := s.is_ifnt ? "ifn't" : "if"
		fmt.sbprintf(b, "(%s\n", kind)
		print_expr(b, s.condition, indent + 1)
		print_stmt(b, s.then_branch, indent + 1)
		if s.else_branch != nil {
			else_b: ^BlockStmt = s.else_branch
			print_indent(b, indent + 1)
			strings.write_string(b, "else:\n")
			print_stmt(b, else_b, indent + 2)
		}
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^PrintStmt:
		print_indent(b, indent)
		strings.write_string(b, "(print\n")
		print_expr(b, s.expr, indent + 1)
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^ReturnStmt:
		print_indent(b, indent)
		strings.write_string(b, "(return")
		if s.value != nil {
			val: Expr = s.value
			strings.write_string(b, "\n")
			print_expr(b, val, indent + 1)
			print_indent(b, indent)
		}
		strings.write_string(b, ")\n")
	case ^SwitchStmt:
		print_indent(b, indent)
		strings.write_string(b, "(switch")
		if s.condition != nil {
			cond: Expr = s.condition
			strings.write_string(b, "\n")
			print_expr(b, cond, indent + 1)
		} else {
			strings.write_string(b, " true\n")
		}
		for c in s.cases {
			print_indent(b, indent + 1)
			strings.write_string(b, "case:\n")
			print_expr(b, c.condition, indent + 2)
			print_stmt(b, c.body, indent + 2)
		}
		if s.else_branch != nil {
			print_indent(b, indent + 1)
			strings.write_string(b, "else:\n")
			print_stmt(b, s.else_branch, indent + 2)
		}
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^WhileStmt:
		print_indent(b, indent)
		kind := "whilen't" if s.is_whilent else "while"
		fmt.sbprintf(b, "(%s\n", kind)
		print_expr(b, s.condition, indent + 1)
		print_stmt(b, s.body, indent + 1)
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case:
		fmt.sbprintf(b, "<Unknown Stmt %T>\n", s)
	}
}

print_expr :: proc(b: ^strings.Builder, expr: Expr, indent: int) {
	if expr == nil {
		return
	}

	switch e in expr {
	case ^AssignExpr:
		print_indent(b, indent)
		fmt.sbprintf(b, "(assign %s\n", e.name.lexeme)
		print_expr(b, e.value, indent + 1)
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^BinaryExpr:
		print_indent(b, indent)
		fmt.sbprintf(b, "(%s\n", e.operator.lexeme)
		print_expr(b, e.left, indent + 1)
		print_expr(b, e.right, indent + 1)
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^BreakExpr:
		print_indent(b, indent)
		strings.write_string(b, "(break)\n")
	case ^BlockExpr:
		print_indent(b, indent)
		strings.write_string(b, "(block\n")
		print_expr(b, e.expression, indent + 1)
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^CallExpr:
		print_indent(b, indent)
		strings.write_string(b, "(call\n")
		print_expr(b, e.callee, indent + 1)
		for arg in e.arguments {
			print_expr(b, arg, indent + 1)
		}
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^ContinueExpr:
		print_indent(b, indent)
		strings.write_string(b, "(continue)\n")
	case ^ExitExpr:
		print_indent(b, indent)
		strings.write_string(b, "(exit")
		if e.code != nil {
			code: Expr = e.code
			strings.write_string(b, "\n")
			print_expr(b, code, indent + 1)
			print_indent(b, indent)
		}
		strings.write_string(b, ")\n")
	case ^ForInExpr:
		print_indent(b, indent)
		fmt.sbprintf(b, "(for-in %s\n", e.var_name.lexeme)
		print_expr(b, e.iterable, indent + 1)
		print_expr(b, e.body, indent + 1)
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^ForExpr:
		print_indent(b, indent)
		strings.write_string(b, "(for\n")
		print_indent(b, indent + 1)
		strings.write_string(b, "init:\n")
		print_expr(b, e.initializer, indent + 2)
		if e.condition != nil {
			cond: Expr = e.condition
			print_indent(b, indent + 1)
			strings.write_string(b, "cond:\n")
			print_expr(b, cond, indent + 2)
		}
		if e.increment != nil {
			inc: Expr = e.increment
			print_indent(b, indent + 1)
			strings.write_string(b, "inc:\n")
			print_expr(b, inc, indent + 2)
		}
		print_expr(b, e.body, indent + 1)
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^GetExpr:
		print_indent(b, indent)
		fmt.sbprintf(b, "(get %s\n", e.property.lexeme)
		print_expr(b, e.receiver, indent + 1)
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^GroupingExpr:
		print_indent(b, indent)
		strings.write_string(b, "(group\n")
		print_expr(b, e.expression, indent + 1)
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^IfExpr:
		print_indent(b, indent)
		kind := e.is_ifnt ? "ifn't" : "if"
		fmt.sbprintf(b, "(%s\n", kind)
		print_expr(b, e.condition, indent + 1)
		print_expr(b, e.then_branch, indent + 1)
		if e.else_branch != nil {
			else_b: ^BlockExpr = e.else_branch
			print_indent(b, indent + 1)
			strings.write_string(b, "else:\n")
			print_expr(b, else_b, indent + 2)
		}
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^ItExpr:
		print_indent(b, indent)
		strings.write_string(b, "it\n")
	case ^LambdaExpr:
		print_indent(b, indent)
		print_decl(b, e.func_decl, indent)
	case ^ListExpr:
		print_indent(b, indent)
		strings.write_string(b, "(list\n")
		if len(e.elements) == 0 {
			print_indent(b, indent + 1)
			strings.write_string(b, "[]\n")
		}
		for el in e.elements {
			print_expr(b, el, indent + 1)
		}
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^LiteralExpr:
		print_indent(b, indent)
		if e.value == nil {
			fmt.sbprintln(b, "nil")
		} else if v, ok := e.value.(string); ok {
			fmt.sbprintfln(b, "\"%v\"", v)
		} else {
			fmt.sbprintfln(b, "%v", e.value)
		}
	case ^LogicalExpr:
		print_indent(b, indent)
		fmt.sbprintf(b, "(logical %s\n", e.operator.lexeme)
		print_expr(b, e.left, indent + 1)
		print_expr(b, e.right, indent + 1)
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^PipeExpr:
		print_indent(b, indent)
		strings.write_string(b, "(|>\n")
		print_expr(b, e.left, indent + 1)
		print_expr(b, e.right, indent + 1)
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^PrintExpr:
		print_indent(b, indent)
		strings.write_string(b, "(print\n")
		print_expr(b, e.expr, indent + 1)
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^ReturnExpr:
		print_indent(b, indent)
		strings.write_string(b, "(return")
		if e.value != nil {
			val: Expr = e.value
			strings.write_string(b, "\n")
			print_expr(b, val, indent + 1)
			print_indent(b, indent)
		}
		strings.write_string(b, ")\n")
	case ^SequenceExpr:
		print_indent(b, indent)
		strings.write_string(b, "(seq\n")
		print_expr(b, e.left, indent + 1)
		print_expr(b, e.right, indent + 1)
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^SetExpr:
		print_indent(b, indent)
		fmt.sbprintf(b, "(set %s\n", e.property.lexeme)
		print_expr(b, e.receiver, indent + 1)
		print_expr(b, e.value, indent + 1)
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^SubscriptExpr:
		print_indent(b, indent)
		strings.write_string(b, "(subscript\n")
		print_expr(b, e.receiver, indent + 1)
		print_expr(b, e.index, indent + 1)
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^SubscriptSetExpr:
		print_indent(b, indent)
		strings.write_string(b, "(subscript-set\n")
		print_expr(b, e.receiver, indent + 1)
		print_expr(b, e.index, indent + 1)
		print_expr(b, e.value, indent + 1)
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^SuperExpr:
		print_indent(b, indent)
		fmt.sbprintf(b, "(super %s)\n", e.method.lexeme)
	case ^SwitchExpr:
		print_indent(b, indent)
		strings.write_string(b, "(switch")
		if e.condition != nil {
			cond: Expr = e.condition
			strings.write_string(b, "\n")
			print_expr(b, cond, indent + 1)
		} else {
			strings.write_string(b, " true\n")
		}
		for c in e.cases {
			print_indent(b, indent + 1)
			strings.write_string(b, "case:\n")
			print_expr(b, c.condition, indent + 2)
			print_expr(b, c.body, indent + 2)
		}
		if e.else_branch != nil {
			print_indent(b, indent + 1)
			strings.write_string(b, "else:\n")
			print_expr(b, e.else_branch, indent + 2)
		}
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^ThisExpr:
		print_indent(b, indent)
		strings.write_string(b, "this\n")
	case ^UnaryExpr:
		print_indent(b, indent)
		fmt.sbprintf(b, "(unary %s\n", e.operator.lexeme)
		print_expr(b, e.right, indent + 1)
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^VarDeclExpr:
		print_indent(b, indent)
		kind := e.is_final ? "val" : "var"
		fmt.sbprintf(b, "(%s ", kind)
		for binding, i in e.bindings {
			if i > 0 {strings.write_string(b, " ")}
			strings.write_string(b, binding.name.lexeme)
			if binding.initializer != nil {
				init: Expr = binding.initializer
				strings.write_string(b, " =\n")
				print_expr(b, init, indent + 1)
			}
		}
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^VariableExpr:
		print_indent(b, indent)
		fmt.sbprintf(b, "(var %s)\n", e.name.lexeme)
	case ^WhileExpr:
		print_indent(b, indent)
		kind := "whilen't" if e.is_whilent else "while"
		fmt.sbprintf(b, "(%s\n", kind)
		print_expr(b, e.condition, indent + 1)
		print_expr(b, e.body, indent + 1)
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case:
		fmt.sbprintf(b, "<Unknown Expr %T>\n", e)
	}
}
