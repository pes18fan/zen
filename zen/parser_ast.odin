package zen

import "core:fmt"
import "core:os"
import "core:strconv"
import "core:strings"

//---------------------------------------------------------
// AST Node Definitions
//---------------------------------------------------------

Expr :: union {
	^AssignExpr,
	^BinaryExpr,
	^CallExpr,
	^GetExpr,
	^GroupingExpr,
	^ItExpr,
	^LambdaExpr,
	^ListExpr,
	^LiteralExpr,
	^LogicalExpr,
	^PipeExpr,
	^SetExpr,
	^SubscriptExpr,
	^SubscriptSetExpr,
	^SuperExpr,
	^ThisExpr,
	^UnaryExpr,
	^VariableExpr,
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

//---------------------------------------------------------
// Declarations
//---------------------------------------------------------

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

//---------------------------------------------------------
// Statements
//---------------------------------------------------------

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

//---------------------------------------------------------
// Expressions
//---------------------------------------------------------

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

CallExpr :: struct {
	token:     Token,
	callee:    Expr,

	// rparen is a Maybe cuz we can have a paren-less call if the arg is just
	// one string, e.g. `puts "hello!"`
	rparen:    Maybe(Token),
	arguments: []Expr,
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

ThisExpr :: struct {
	token: Token,
}

UnaryExpr :: struct {
	token:    Token,
	operator: Token,
	right:    Expr,
}

VariableExpr :: struct {
	token: Token,
	name:  Token,
}

AstPrecedence :: enum {
	NONE,
	PIPELINE, // |>
	ASSIGNMENT, // =
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

AstParsePrefixFn :: #type proc(p: ^AstParser, can_assign: bool) -> Expr
AstParseInfixFn :: #type proc(p: ^AstParser, left: Expr, can_assign: bool) -> Expr

AstParseRule :: struct {
	prefix:     AstParsePrefixFn,
	infix:      AstParseInfixFn,
	precedence: AstPrecedence,
}

parse :: proc(p: ^AstParser) -> ([]Decl, bool) {
	declarations: [dynamic]Decl
	for !ast_is_at_end(p) {
		decl := parse_declaration(p)
		if p.panic_mode {
			ast_synchronize(p)
		} else {
			append(&declarations, decl)
		}
	}

	return declarations[:], !p.had_error
}

parse_declaration :: proc(p: ^AstParser) -> Decl {
	switch {
	case ast_match(p, .VAR, .VAL):
		return parse_var_decl(p)
	case ast_match(p, .CLASS):
		return parse_class_decl(p)
	case ast_match(p, .USE):
		return parse_module_decl(p)
	case ast_match(p, .FUNC):
		return parse_func_decl(p, "function")
	case ast_match(p, .PUB):
		return parse_pub_decl(p)
	}
	return parse_statement(p)
}

parse_var_decl :: proc(p: ^AstParser) -> ^VarDecl {
	decl := new(VarDecl)
	decl.token = ast_previous(p)
	decl.is_final = decl.token.type == .VAL
	bindings := make([dynamic]VarBinding)

	for {
		binding: VarBinding
		binding.name = ast_consume(p, .IDENT, "Expect variable name.")
		if ast_match(p, .EQUAL) {
			binding.initializer = parse_expression(p)
		}
		append(&bindings, binding)

		if !ast_match(p, .COMMA) {break}
	}
	decl.bindings = bindings[:]

	ast_consume_semi(p, "variable declaration")
	return decl
}

parse_class_decl :: proc(p: ^AstParser) -> ^ClassDecl {
	decl := new(ClassDecl)
	decl.token = ast_previous(p)
	decl.name = ast_consume(p, .IDENT, "Expect class name.")
	methods := make([dynamic]^FuncDecl)

	if ast_match(p, .LESS) {
		decl.superclass = ast_consume(p, .IDENT, "Expect superclass name.")
	}

	ast_consume(p, .LSQUIRLY, "Expect '{' before class body.")
	for !ast_check(p, .RSQUIRLY) && !ast_is_at_end(p) {
		append(&methods, parse_method(p))
	}
	decl.methods = methods[:]

	ast_consume(p, .RSQUIRLY, "Expect '}' after class body.")
	return decl
}

parse_method :: proc(p: ^AstParser) -> ^FuncDecl {
	name := ast_consume(p, .IDENT, "Expect method name.")
	return parse_func_body(p, name)
}

parse_module_decl :: proc(p: ^AstParser) -> ^ModuleDecl {
	decl := new(ModuleDecl)
	decl.token = ast_previous(p)
	decl.path = ast_consume(p, .STRING, "Expect module path.")
	return decl
}

parse_func_decl :: proc(p: ^AstParser, kind: string) -> ^FuncDecl {
	token := ast_previous(p)
	name := ast_consume(p, .IDENT, fmt.tprintf("Expect %s name.", kind))
	decl := parse_func_body(p, name)
	decl.token = token
	return decl
}

parse_func_body :: proc(p: ^AstParser, name: Token) -> ^FuncDecl {
	decl := new(FuncDecl)
	decl.token = ast_previous(p) // NOTE: this should probably be the keyword and not the name
	decl.name = name
	params := make([dynamic]Token)

	ast_consume(p, .LPAREN, "Expect '(' after function name.")
	if !ast_check(p, .RPAREN) {
		for {
			append(&params, ast_consume(p, .IDENT, "Expect parameter name."))
			if !ast_match(p, .COMMA) {break}
		}
	}
	decl.params = params[:]
	ast_consume(p, .RPAREN, "Expect ')' after function parameters.")

	if ast_match(p, .FAT_ARROW) {
		decl.body = parse_expression(p)
		if ast_check(p, .SEMI) {
			ast_advance(p)
		}
	} else {
		ast_consume(p, .LSQUIRLY, "Expect '=>' or '{' after function parameter list.")
		decl.body = parse_block(p)
	}

	return decl
}

parse_pub_decl :: proc(p: ^AstParser) -> ^PubDecl {
	decl := new(PubDecl)
	decl.token = ast_previous(p)
	if ast_match(p, .FUNC) {
		decl.decl = parse_func_decl(p, "function")
	} else if ast_match(p, .CLASS) {
		decl.decl = parse_class_decl(p)
	} else {
		ast_error(p, ast_peek(p), "Only functions or classes can be set as public.")
	}
	return decl
}


ast_get_rule :: proc(type: TokenType) -> ^AstParseRule {
	return &ast_rules[type]
}

ast_parse_precedence :: proc(p: ^AstParser, precedence: AstPrecedence) -> Expr {
	ast_advance(p)
	prefix_rule := ast_get_rule(ast_previous(p).type).prefix
	if prefix_rule == nil {
		ast_error(p, ast_previous(p), "Expect expression.")
		return nil
	}

	can_assign := precedence <= .ASSIGNMENT
	expr := prefix_rule(p, can_assign)

	for precedence <= ast_get_rule(ast_peek(p).type).precedence {
		ast_advance(p)
		infix_rule := ast_get_rule(ast_previous(p).type).infix
		if infix_rule != nil {
			expr = infix_rule(p, expr, can_assign)
		}
	}

	if can_assign && ast_match(p, .EQUAL) {
		ast_error(p, ast_previous(p), "Invalid assignment target.")
	}

	return expr
}

// Statement parsers

parse_statement :: proc(p: ^AstParser) -> Stmt {
	when CHAOTIC {
		if ast_match(p, .IFNT) {
			return parse_if_stmt(p, true)
		} else if ast_match(p, .WHILENT) {
			return parse_while_stmt(p, true)
		}
	}

	switch {
	case ast_match(p, .IF):
		return parse_if_stmt(p, false)
	case ast_match(p, .WHILE):
		return parse_while_stmt(p, false)
	case ast_match(p, .BREAK):
		return parse_break_stmt(p)
	case ast_match(p, .CONTINUE):
		return parse_continue_stmt(p)
	case ast_match(p, .FOR):
		return parse_for_stmt(p)
	case ast_match(p, .LSQUIRLY):
		return parse_block(p)
	case ast_match(p, .PRINT):
		return parse_print_stmt(p)
	case ast_match(p, .RETURN):
		return parse_return_stmt(p)
	case ast_match(p, .EXIT):
		return parse_exit_stmt(p)
	case ast_match(p, .SWITCH):
		return parse_switch_stmt(p)
	case ast_match(p, .SEMI):
		stmt := new(EmptyStmt)
		stmt.token = ast_previous(p)
		return stmt
	}

	return parse_expression_stmt(p)
}

parse_if_stmt :: proc(p: ^AstParser, is_ifnt: bool) -> ^IfStmt {
	stmt := new(IfStmt)
	stmt.token = ast_previous(p)
	stmt.is_ifnt = is_ifnt
	stmt.condition = parse_expression(p)

	ast_consume(p, .LSQUIRLY, "Expect '{' after condition.")
	stmt.then_branch = parse_block(p)

	if ast_match(p, .ELSE) {
		ast_consume(p, .LSQUIRLY, "Expect '{' after else.")
		stmt.else_branch = parse_block(p)
	}
	return stmt
}

parse_while_stmt :: proc(p: ^AstParser, is_whilent: bool) -> ^WhileStmt {
	stmt := new(WhileStmt)
	stmt.token = ast_previous(p)
	stmt.is_whilent = is_whilent
	stmt.condition = parse_expression(p)

	ast_consume(p, .LSQUIRLY, "Expect '{' after condition.")
	stmt.body = parse_block(p)
	return stmt
}

parse_break_stmt :: proc(p: ^AstParser) -> ^BreakStmt {
	stmt := new(BreakStmt)
	stmt.token = ast_previous(p)
	ast_consume_semi(p, "break")
	return stmt
}

parse_continue_stmt :: proc(p: ^AstParser) -> ^ContinueStmt {
	stmt := new(ContinueStmt)
	stmt.token = ast_previous(p)
	ast_consume_semi(p, "continue")
	return stmt
}

parse_for_stmt :: proc(p: ^AstParser) -> Stmt {
	token := ast_previous(p)
	// Differentiate between for-in and classic for loop
	if ast_check(p, .IDENT) && p.tokens[p.current + 1].type == .IN {
		stmt := new(ForInStmt)
		stmt.token = token
		stmt.var_name = ast_advance(p)
		ast_advance(p) // consume IN
		stmt.iterable = parse_expression(p)
		ast_consume(p, .LSQUIRLY, "Expect '{' after iterable.")
		stmt.body = parse_block(p)
		return stmt
	}

	stmt := new(ForStmt)
	stmt.token = token
	if ast_match(p, .SEMI) {
		empty := new(EmptyStmt)
		empty.token = ast_previous(p)
		stmt.initializer = empty
	} else if ast_match(p, .VAR, .VAL) {
		stmt.initializer = parse_var_decl(p)
	} else {
		stmt.initializer = parse_expression_stmt(p)
	}

	if !ast_match(p, .SEMI) {
		stmt.condition = parse_expression(p)
		ast_consume_semi(p, "loop condition")
	}

	if !ast_match(p, .LSQUIRLY) {
		stmt.increment = parse_expression(p)
		ast_consume(p, .LSQUIRLY, "Expect '{' after for clauses.")
	}

	stmt.body = parse_block(p)
	return stmt
}

parse_block :: proc(p: ^AstParser) -> ^BlockStmt {
	stmt := new(BlockStmt)
	stmt.token = ast_previous(p)
	declarations := make([dynamic]Decl)

	for !ast_check(p, .RSQUIRLY) && !ast_is_at_end(p) {
		decl := parse_declaration(p)
		if p.panic_mode {
			ast_synchronize(p)
		} else {
			append(&declarations, decl)
		}
	}
	stmt.declarations = declarations[:]
	ast_consume(p, .RSQUIRLY, "Expect '}' after block.")

	// This is added by ASI after blocks (dunno how to make that not happen yet)
	ast_consume_semi(p, "Expect ';' after block.")
	return stmt
}

parse_print_stmt :: proc(p: ^AstParser) -> ^PrintStmt {
	stmt := new(PrintStmt)
	stmt.token = ast_previous(p)
	stmt.expr = parse_expression(p)
	ast_consume_semi(p, "value")
	return stmt
}

parse_return_stmt :: proc(p: ^AstParser) -> ^ReturnStmt {
	stmt := new(ReturnStmt)
	stmt.token = ast_previous(p)
	if !ast_match(p, .SEMI) {
		stmt.value = parse_expression(p)
		ast_consume_semi(p, "return value")
	}
	return stmt
}

parse_exit_stmt :: proc(p: ^AstParser) -> ^ExitStmt {
	stmt := new(ExitStmt)
	stmt.token = ast_previous(p)
	if !ast_match(p, .SEMI) {
		stmt.code = parse_expression(p)
		ast_consume_semi(p, "exit code")
	}
	return stmt
}

parse_switch_stmt :: proc(p: ^AstParser) -> ^SwitchStmt {
	stmt := new(SwitchStmt)
	stmt.token = ast_previous(p)
	cases := make([dynamic]SwitchCase)
	has_else_clause := false

	if ast_match(p, .LSQUIRLY) {
		// No condition
	} else {
		stmt.condition = parse_expression(p)
		ast_consume(p, .LSQUIRLY, "Expect '{' after switch condition.")
	}

	for !ast_match(p, .RSQUIRLY) && !ast_is_at_end(p) {
		if ast_match(p, .ELSE) {
			has_else_clause = true
			ast_consume(p, .FAT_ARROW, "Expect '=>' after 'else'.")
			stmt.else_branch = parse_statement(p)
			if ast_check(p, .SEMI) {ast_advance(p)}
			ast_consume(p, .RSQUIRLY, "'else' must be the last case.")
			break
		}

		case_node: SwitchCase
		case_node.condition = parse_expression(p)
		ast_consume(p, .FAT_ARROW, "Expect '=>' after case.")
		case_node.body = parse_statement(p)
		if ast_check(p, .SEMI) {ast_advance(p)}
		append(&cases, case_node)
	}

	if !has_else_clause {
		ast_error(p, ast_peek(p), "Switch statement must have an 'else' clause.")
	}

	stmt.cases = cases[:]
	return stmt
}

parse_expression_stmt :: proc(p: ^AstParser) -> ^ExprStmt {
	stmt := new(ExprStmt)
	stmt.expr = parse_expression(p)
	stmt.token = ast_previous(p)
	ast_consume_semi(p, "expression")
	return stmt
}

// Expression parsers

parse_expression :: proc(p: ^AstParser) -> Expr {
	return ast_parse_precedence(p, .PIPELINE)
}

//---------------------------------------------------------
// Prefix Rules
//---------------------------------------------------------

ast_parse_grouping :: proc(p: ^AstParser, can_assign: bool) -> Expr {
	token := ast_previous(p)
	expr := parse_expression(p)
	ast_consume(p, .RPAREN, "Expect ')' after expression.")
	grouping := new(GroupingExpr)
	grouping.token = token
	grouping.expression = expr
	return grouping
}

ast_parse_list :: proc(p: ^AstParser, can_assign: bool) -> Expr {
	list := new(ListExpr)
	list.token = ast_previous(p)
	elements := make([dynamic]Expr)
	if !ast_check(p, .RSQUARE) {
		for {
			append(&elements, parse_expression(p))
			if !ast_match(p, .COMMA) {break}
		}
	}
	list.elements = elements[:]
	ast_consume(p, .RSQUARE, "Expect ']' after list elements.")
	return list
}

ast_parse_unary :: proc(p: ^AstParser, can_assign: bool) -> Expr {
	operator := ast_previous(p)
	right := ast_parse_precedence(p, .UNARY)
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

ast_parse_literal :: proc(p: ^AstParser, can_assign: bool) -> Expr {
	literal := new(LiteralExpr)
	literal.token = ast_previous(p)

	#partial switch literal.token.type {
	case .STRING:
		literal.value = add_escape_sequences(literal.token.lexeme[1:len(literal.token.lexeme) - 1])
	case .NUMBER:
		value, ok := strconv.parse_f64(literal.token.lexeme)
		if !ok {
			ast_error(
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
		ast_error(
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

ast_parse_variable :: proc(p: ^AstParser, can_assign: bool) -> Expr {
	name := ast_previous(p)
	if can_assign && ast_match(p, .EQUAL) {
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

ast_parse_super :: proc(p: ^AstParser, can_assign: bool) -> Expr {
	super_expr := new(SuperExpr)
	super_expr.token = ast_previous(p)
	ast_consume(p, .DOT, "Expect '.' after 'super'.")
	super_expr.method = ast_consume(p, .IDENT, "Expect superclass method name.")

	invoked := false
	method_args := make([dynamic]Expr)
	defer if !invoked {
		delete(method_args)
	}

	// was the retrieved method immediately invoked?
	if ast_match(p, .LPAREN) {
		invoked = true
		if !ast_check(p, .RPAREN) {
			for {
				append(&method_args, parse_expression(p))
				if !ast_match(p, .COMMA) {break}
			}
		}
		super_expr.method_args = method_args[:]
		ast_consume(p, .RPAREN, "Expect ')' after method parameters.")
	} else {
		super_expr.method_args = nil
	}

	return super_expr
}

ast_parse_this :: proc(p: ^AstParser, can_assign: bool) -> Expr {
	this_expr := new(ThisExpr)
	this_expr.token = ast_previous(p)
	return this_expr
}

ast_parse_it :: proc(p: ^AstParser, can_assign: bool) -> Expr {
	it_expr := new(ItExpr)
	it_expr.token = ast_previous(p)
	return it_expr
}

ast_parse_lambda :: proc(p: ^AstParser, can_assign: bool) -> Expr {
	lambda := new(LambdaExpr)
	lambda.token = ast_previous(p)
	lambda.func_decl = parse_func_body(p, Token{type = .IDENT, lexeme = "lambda"})
	return lambda
}

//---------------------------------------------------------
// Infix Rules
//---------------------------------------------------------

ast_parse_binary :: proc(p: ^AstParser, left: Expr, can_assign: bool) -> Expr {
	operator := ast_previous(p)
	rule := ast_get_rule(operator.type)
	// Add 1 to precedence for left-associative operators
	right := ast_parse_precedence(p, cast(AstPrecedence)(cast(int)rule.precedence + 1))

	binary := new(BinaryExpr)
	binary.token = operator
	binary.left = left
	binary.operator = operator
	binary.right = right
	return binary
}

ast_parse_logical :: proc(p: ^AstParser, left: Expr, can_assign: bool) -> Expr {
	operator := ast_previous(p)
	rule := ast_get_rule(operator.type)
	right := ast_parse_precedence(p, cast(AstPrecedence)(cast(int)rule.precedence + 1))

	logical := new(LogicalExpr)
	logical.token = operator
	logical.left = left
	logical.operator = operator
	logical.right = right
	return logical
}

ast_parse_pipe :: proc(p: ^AstParser, left: Expr, can_assign: bool) -> Expr {
	operator := ast_previous(p)
	rule := ast_get_rule(operator.type)
	right := ast_parse_precedence(p, cast(AstPrecedence)(cast(int)rule.precedence + 1))

	pipe := new(PipeExpr)
	pipe.token = operator
	pipe.left = left
	pipe.operator = operator
	pipe.right = right
	return pipe
}

ast_parse_call :: proc(p: ^AstParser, left: Expr, can_assign: bool) -> Expr {
	call := new(CallExpr)
	call.token = ast_previous(p) // The '(' token
	call.callee = left
	arguments := make([dynamic]Expr)

	if !ast_check(p, .RPAREN) {
		for {
			append(&arguments, parse_expression(p))
			if !ast_match(p, .COMMA) {break}
		}
	}
	call.arguments = arguments[:]
	call.rparen = ast_consume(p, .RPAREN, "Expect ')' after arguments.")
	return call
}

ast_parse_dot :: proc(p: ^AstParser, left: Expr, can_assign: bool) -> Expr {
	token := ast_previous(p) // The '.' token
	property := ast_consume(p, .IDENT, "Expect property name after '.'.")
	if can_assign && ast_match(p, .EQUAL) {
		value := parse_expression(p)
		set_expr := new(SetExpr)
		set_expr.token = token
		set_expr.receiver = left
		set_expr.property = property
		set_expr.value = value
		return set_expr
	}
	get_expr := new(GetExpr)
	get_expr.token = token
	get_expr.receiver = left
	get_expr.property = property
	return get_expr
}

ast_parse_subscript :: proc(p: ^AstParser, left: Expr, can_assign: bool) -> Expr {
	bracket := ast_previous(p) // The '[' token
	index := parse_expression(p)
	ast_consume(p, .RSQUARE, "Expect ']' after index.")
	if can_assign && ast_match(p, .EQUAL) {
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

ast_rules: [TokenType]AstParseRule = {
	.LPAREN        = {ast_parse_grouping, ast_parse_call, .CALL},
	.RPAREN        = {nil, nil, .NONE},
	.LSQUIRLY      = {nil, nil, .NONE},
	.RSQUIRLY      = {nil, nil, .NONE},
	.LSQUARE       = {ast_parse_list, ast_parse_subscript, .CALL},
	.RSQUARE       = {nil, nil, .NONE},
	.COMMA         = {nil, nil, .NONE},
	.DOT           = {nil, ast_parse_dot, .CALL},
	.MINUS         = {ast_parse_unary, ast_parse_binary, .TERM},
	.PLUS          = {nil, ast_parse_binary, .TERM},
	.SEMI          = {nil, nil, .NONE},
	.SLASH         = {nil, ast_parse_binary, .FACTOR},
	.STAR          = {nil, ast_parse_binary, .FACTOR},
	.PERCENT       = {nil, ast_parse_binary, .FACTOR},
	.NEWLINE       = {nil, nil, .NONE},
	.BANG_EQUAL    = {nil, ast_parse_binary, .EQUALITY},
	.BAR_GREATER   = {nil, ast_parse_pipe, .PIPELINE},
	.EQUAL         = {nil, nil, .NONE},
	.EQUAL_EQUAL   = {nil, ast_parse_binary, .EQUALITY},
	.FAT_ARROW     = {nil, nil, .NONE},
	.GREATER       = {nil, ast_parse_binary, .COMPARISON},
	.GREATER_EQUAL = {nil, ast_parse_binary, .COMPARISON},
	.LESS          = {nil, ast_parse_binary, .COMPARISON},
	.LESS_EQUAL    = {nil, ast_parse_binary, .COMPARISON},
	.IDENT         = {ast_parse_variable, nil, .NONE},
	.STRING        = {ast_parse_literal, nil, .NONE},
	.NUMBER        = {ast_parse_literal, nil, .NONE},
	.AND           = {nil, ast_parse_logical, .AND},
	.BREAK         = {nil, nil, .NONE},
	.CONTINUE      = {nil, nil, .NONE},
	.CLASS         = {nil, nil, .NONE},
	.ELSE          = {nil, nil, .NONE},
	.EXIT          = {nil, nil, .NONE},
	.FALSE         = {ast_parse_literal, nil, .NONE},
	.FOR           = {nil, nil, .NONE},
	.FUNC          = {ast_parse_lambda, nil, .NONE},
	.IF            = {nil, nil, .NONE},
	.IFNT          = {nil, nil, .NONE},
	.IN            = {nil, nil, .NONE},
	.IT            = {ast_parse_it, nil, .NONE},
	.NIL           = {ast_parse_literal, nil, .NONE},
	.NOT           = {ast_parse_unary, nil, .NONE},
	.OR            = {nil, ast_parse_logical, .OR},
	.PRINT         = {nil, nil, .NONE},
	.PUB           = {nil, nil, .NONE},
	.RETURN        = {nil, nil, .NONE},
	.SWITCH        = {nil, nil, .NONE},
	.SUPER         = {ast_parse_super, nil, .NONE},
	.THIS          = {ast_parse_this, nil, .NONE},
	.TRUE          = {ast_parse_literal, nil, .NONE},
	.USE           = {nil, nil, .NONE},
	.WHILE         = {nil, nil, .NONE},
	.WHILENT       = {nil, nil, .NONE},
	.VAR           = {nil, nil, .NONE},
	.VAL           = {nil, nil, .NONE},
	.EOF           = {nil, nil, .NONE},
}

AstParser :: struct {
	tokens:     []Token,
	current:    int,
	had_error:  bool,
	panic_mode: bool,
}

init_parser :: proc(tokens: []Token) -> AstParser {
	return AstParser{tokens = tokens, current = 0, had_error = false, panic_mode = false}
}

ast_error :: proc(p: ^AstParser, token: Token, message: string) {
	if p.panic_mode {return}
	p.panic_mode = true

	color_red(os.stderr, "parse error ")

	if token.type == TokenType.EOF {
		fmt.eprintf("at end")
	} else {
		fmt.eprintf("at '%s'", token.lexeme)
	}

	fmt.eprintf(": %s\n", message)
	fmt.eprintf("  on [line %d]\n", token.line)
	p.had_error = true
}

ast_peek :: proc(p: ^AstParser) -> Token {
	return p.tokens[p.current]
}

ast_previous :: proc(p: ^AstParser) -> Token {
	return p.tokens[p.current - 1]
}

ast_is_at_end :: proc(p: ^AstParser) -> bool {
	return ast_peek(p).type == .EOF
}

ast_check :: proc(p: ^AstParser, type: TokenType) -> bool {
	if ast_is_at_end(p) {return false}
	return ast_peek(p).type == type
}

ast_advance :: proc(p: ^AstParser) -> Token {
	if !ast_is_at_end(p) {p.current += 1}
	return ast_previous(p)
}

ast_match :: proc(p: ^AstParser, types: ..TokenType) -> bool {
	for type in types {
		if ast_check(p, type) {
			ast_advance(p)
			return true
		}
	}
	return false
}

ast_consume :: proc(p: ^AstParser, type: TokenType, message: string) -> Token {
	if ast_check(p, type) {return ast_advance(p)}
	ast_error(p, ast_peek(p), message)
	return ast_peek(p)
}

ast_consume_semi :: proc(p: ^AstParser, message: string) {
	ast_consume(p, .SEMI, fmt.tprintf("Expect ';' after %s.", message))
}

ast_synchronize :: proc(p: ^AstParser) {
	p.panic_mode = false

	for !ast_is_at_end(p) {
		if ast_previous(p).type == .SEMI {return}

		#partial switch ast_peek(p).type {
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

		ast_advance(p)
	}
}

// AST freeing functions

free_decls :: proc(decls: []Decl) {
	for decl in decls {
		free_decl(decl)
	}
	delete(decls)
}

@(private = "file")
free_decl :: proc(decl: Decl) {
	if decl == nil {
		return
	}

	switch d in decl {
	case ^ClassDecl:
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

@(private = "file")
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
		free_decls(s.else_branch.declarations)
		free(s.else_branch)
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

@(private = "file")
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
	case ^CallExpr:
		for arg in e.arguments {
			free_expr(arg)
		}
		delete(e.arguments)
		free_expr(e.callee)
		free(e)
	case ^GetExpr:
		free_expr(e.receiver)
		free(e)
	case ^GroupingExpr:
		free_expr(e.expression)
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
	case ^ThisExpr:
		free(e)
	case ^UnaryExpr:
		free_expr(e.right)
		free(e)
	case ^VariableExpr:
		free(e)
	}
}


// AST pretty-printer

// allocates a string
ast_string :: proc(decls: []Decl) -> string {
	b := strings.builder_make()
	defer strings.builder_destroy(&b)

	for decl in decls {
		ast_print_decl(&b, decl, 0)
	}
	return strings.clone(strings.to_string(b))
}

ast_print_indent :: proc(b: ^strings.Builder, indent: int) {
	for i := 0; i < indent; i += 1 {
		strings.write_string(b, "  ")
	}
}

ast_print_decl :: proc(b: ^strings.Builder, decl: Decl, indent: int) {
	if decl == nil {
		return
	}

	switch d in decl {
	case ^ClassDecl:
		ast_print_indent(b, indent)
		fmt.sbprintf(b, "(class %s", d.name.lexeme)
		if d.superclass != nil {
			superclass: Token = d.superclass.?
			fmt.sbprintf(b, " < %s", superclass.lexeme)
		}
		strings.write_string(b, "\n")
		for method in d.methods {
			ast_print_decl(b, method, indent + 1)
		}
		ast_print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^FuncDecl:
		ast_print_indent(b, indent)
		fmt.sbprintf(b, "(func %s (", d.name.lexeme)
		for param, i in d.params {
			if i > 0 {strings.write_string(b, " ")}
			strings.write_string(b, param.lexeme)
		}
		strings.write_string(b, ")\n")

		#partial switch body in d.body {
		case ^BlockStmt:
			ast_print_stmt(b, body, indent + 1)
		case Expr:
			ast_print_indent(b, indent + 1)
			strings.write_string(b, "=>\n")
			ast_print_expr(b, body, indent + 2)
		}
		ast_print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^ModuleDecl:
		ast_print_indent(b, indent)
		fmt.sbprintf(b, "(use %s)\n", d.path.lexeme)
	case ^PubDecl:
		ast_print_indent(b, indent)
		strings.write_string(b, "(pub\n")
		ast_print_decl(b, d.decl, indent + 1)
		ast_print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^VarDecl:
		ast_print_indent(b, indent)
		kind := d.is_final ? "val" : "var"
		fmt.sbprintf(b, "(%s ", kind)
		for binding, i in d.bindings {
			if i > 0 {strings.write_string(b, " ")}
			strings.write_string(b, binding.name.lexeme)
			if binding.initializer != nil {
				init: Expr = binding.initializer
				strings.write_string(b, " =\n")
				ast_print_expr(b, init, indent + 1)
			}
		}
		strings.write_string(b, ")\n")
	case Stmt:
		ast_print_stmt(b, d, indent)
	case:
		fmt.sbprintf(b, "<Unknown Decl %T>\n", d)
	}
}

ast_print_stmt :: proc(b: ^strings.Builder, stmt: Stmt, indent: int) {
	if stmt == nil {
		return
	}

	switch s in stmt {
	case ^BlockStmt:
		ast_print_indent(b, indent)
		strings.write_string(b, "(block\n")
		for d in s.declarations {
			ast_print_decl(b, d, indent + 1)
		}
		ast_print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^BreakStmt:
		ast_print_indent(b, indent)
		strings.write_string(b, "(break)\n")
	case ^ContinueStmt:
		ast_print_indent(b, indent)
		strings.write_string(b, "(continue)\n")
	case ^EmptyStmt:
		ast_print_indent(b, indent)
		strings.write_string(b, "(empty)\n")
	case ^ExitStmt:
		ast_print_indent(b, indent)
		strings.write_string(b, "(exit")
		if s.code != nil {
			code: Expr = s.code
			strings.write_string(b, "\n")
			ast_print_expr(b, code, indent + 1)
			ast_print_indent(b, indent)
		}
		strings.write_string(b, ")\n")
	case ^ExprStmt:
		ast_print_indent(b, indent)
		strings.write_string(b, "(expr\n")
		ast_print_expr(b, s.expr, indent + 1)
		ast_print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^ForInStmt:
		ast_print_indent(b, indent)
		fmt.sbprintf(b, "(for-in %s\n", s.var_name.lexeme)
		ast_print_expr(b, s.iterable, indent + 1)
		ast_print_stmt(b, s.body, indent + 1)
		ast_print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^ForStmt:
		ast_print_indent(b, indent)
		strings.write_string(b, "(for\n")
		if true {
			ast_print_indent(b, indent + 1)
			strings.write_string(b, "init:\n")
			#partial switch init in s.initializer {
			case ^VarDecl:
				ast_print_decl(b, init, indent + 2)
			case ^ExprStmt:
				ast_print_stmt(b, init, indent + 2)
			case ^EmptyStmt:
				ast_print_stmt(b, init, indent + 2)
			}
		}
		if s.condition != nil {
			cond: Expr = s.condition
			ast_print_indent(b, indent + 1)
			strings.write_string(b, "cond:\n")
			ast_print_expr(b, cond, indent + 2)
		}
		if s.increment != nil {
			inc: Expr = s.increment
			ast_print_indent(b, indent + 1)
			strings.write_string(b, "inc:\n")
			ast_print_expr(b, inc, indent + 2)
		}
		ast_print_stmt(b, s.body, indent + 1)
		ast_print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^IfStmt:
		ast_print_indent(b, indent)
		kind := s.is_ifnt ? "ifn't" : "if"
		fmt.sbprintf(b, "(%s\n", kind)
		ast_print_expr(b, s.condition, indent + 1)
		ast_print_stmt(b, s.then_branch, indent + 1)
		if s.else_branch != nil {
			else_b: ^BlockStmt = s.else_branch
			ast_print_indent(b, indent + 1)
			strings.write_string(b, "else:\n")
			ast_print_stmt(b, else_b, indent + 2)
		}
		ast_print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^PrintStmt:
		ast_print_indent(b, indent)
		strings.write_string(b, "(print\n")
		ast_print_expr(b, s.expr, indent + 1)
		ast_print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^ReturnStmt:
		ast_print_indent(b, indent)
		strings.write_string(b, "(return")
		if s.value != nil {
			val: Expr = s.value
			strings.write_string(b, "\n")
			ast_print_expr(b, val, indent + 1)
			ast_print_indent(b, indent)
		}
		strings.write_string(b, ")\n")
	case ^SwitchStmt:
		ast_print_indent(b, indent)
		strings.write_string(b, "(switch")
		if s.condition != nil {
			cond: Expr = s.condition
			strings.write_string(b, "\n")
			ast_print_expr(b, cond, indent + 1)
		} else {
			strings.write_string(b, " <implicit true>\n")
		}
		for c in s.cases {
			ast_print_indent(b, indent + 1)
			strings.write_string(b, "case:\n")
			ast_print_expr(b, c.condition, indent + 2)
			ast_print_stmt(b, c.body, indent + 2)
		}
		if s.else_branch != nil {
			ast_print_indent(b, indent + 1)
			strings.write_string(b, "else:\n")
			ast_print_stmt(b, s.else_branch, indent + 2)
		}
		ast_print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^WhileStmt:
		ast_print_indent(b, indent)
		kind := "whilen't" if s.is_whilent else "while"
		fmt.sbprintf(b, "(%s\n", kind)
		ast_print_expr(b, s.condition, indent + 1)
		ast_print_stmt(b, s.body, indent + 1)
		ast_print_indent(b, indent)
		strings.write_string(b, ")\n")
	case:
		fmt.sbprintf(b, "<Unknown Stmt %T>\n", s)
	}
}

ast_print_expr :: proc(b: ^strings.Builder, expr: Expr, indent: int) {
	if expr == nil {
		return
	}

	switch e in expr {
	case ^AssignExpr:
		ast_print_indent(b, indent)
		fmt.sbprintf(b, "(assign %s\n", e.name.lexeme)
		ast_print_expr(b, e.value, indent + 1)
		ast_print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^BinaryExpr:
		ast_print_indent(b, indent)
		fmt.sbprintf(b, "(%s\n", e.operator.lexeme)
		ast_print_expr(b, e.left, indent + 1)
		ast_print_expr(b, e.right, indent + 1)
		ast_print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^CallExpr:
		ast_print_indent(b, indent)
		strings.write_string(b, "(call\n")
		ast_print_expr(b, e.callee, indent + 1)
		for arg in e.arguments {
			ast_print_expr(b, arg, indent + 1)
		}
		ast_print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^GetExpr:
		ast_print_indent(b, indent)
		fmt.sbprintf(b, "(get %s\n", e.property.lexeme)
		ast_print_expr(b, e.receiver, indent + 1)
		ast_print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^GroupingExpr:
		ast_print_indent(b, indent)
		strings.write_string(b, "(group\n")
		ast_print_expr(b, e.expression, indent + 1)
		ast_print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^ItExpr:
		ast_print_indent(b, indent)
		strings.write_string(b, "it\n")
	case ^LambdaExpr:
		ast_print_indent(b, indent)
		ast_print_decl(b, e.func_decl, indent)
	case ^ListExpr:
		ast_print_indent(b, indent)
		strings.write_string(b, "(list\n")
		for el in e.elements {
			ast_print_expr(b, el, indent + 1)
		}
		ast_print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^LiteralExpr:
		ast_print_indent(b, indent)
		fmt.sbprintf(b, "%v\n", e.value)
	case ^LogicalExpr:
		ast_print_indent(b, indent)
		fmt.sbprintf(b, "(logical %s\n", e.operator.lexeme)
		ast_print_expr(b, e.left, indent + 1)
		ast_print_expr(b, e.right, indent + 1)
		ast_print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^PipeExpr:
		ast_print_indent(b, indent)
		strings.write_string(b, "(|>\n")
		ast_print_expr(b, e.left, indent + 1)
		ast_print_expr(b, e.right, indent + 1)
		ast_print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^SetExpr:
		ast_print_indent(b, indent)
		fmt.sbprintf(b, "(set %s\n", e.property.lexeme)
		ast_print_expr(b, e.receiver, indent + 1)
		ast_print_expr(b, e.value, indent + 1)
		ast_print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^SubscriptExpr:
		ast_print_indent(b, indent)
		strings.write_string(b, "(subscript\n")
		ast_print_expr(b, e.receiver, indent + 1)
		ast_print_expr(b, e.index, indent + 1)
		ast_print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^SubscriptSetExpr:
		ast_print_indent(b, indent)
		strings.write_string(b, "(subscript-set\n")
		ast_print_expr(b, e.receiver, indent + 1)
		ast_print_expr(b, e.index, indent + 1)
		ast_print_expr(b, e.value, indent + 1)
		ast_print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^SuperExpr:
		ast_print_indent(b, indent)
		fmt.sbprintf(b, "(super %s)\n", e.method.lexeme)
	case ^ThisExpr:
		ast_print_indent(b, indent)
		strings.write_string(b, "this\n")
	case ^UnaryExpr:
		ast_print_indent(b, indent)
		fmt.sbprintf(b, "(unary %s\n", e.operator.lexeme)
		ast_print_expr(b, e.right, indent + 1)
		ast_print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^VariableExpr:
		ast_print_indent(b, indent)
		fmt.sbprintf(b, "(var %s)\n", e.name.lexeme)
	case:
		fmt.sbprintf(b, "<Unknown Expr %T>\n", e)
	}
}
