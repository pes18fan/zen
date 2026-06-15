package zen

import "core:fmt"
import "core:os"
import "core:path/filepath"
import "core:slice"
import "core:strconv"
import "core:strings"

// AST Node Definitions

Expr :: union #shared_nil {
	^AssignExpr,
	^BinaryExpr,
	^BlockExpr,
	^BreakExpr,
	^CallExpr,
	^ClassExpr,
	^ContinueExpr,
	^DiscardExpr,
	^ExitExpr,
	^ForExpr,
	^ForInExpr,
	^GetExpr,
	^GroupingExpr,
	^IfExpr,
	^ItExpr,
	^FunctionExpr,
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
	^UseExpr,
	^VariableExpr,
	^VarDeclExpr,
	^WhileExpr,
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

// sometimes we want to run an expression for its side effects but discard
// its value itself, in this case the `DiscardExpr` is perfect. It evaluates
// the expression inside of it and returns nil (which will become unit in the
// future)
DiscardExpr :: struct {
	token:      Token,
	expression: Expr,
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

ClassExpr :: struct {
	token:      Token,
	name:       Token,
	superclass: Maybe(Token),
	methods:    []^FunctionExpr,
	public:     bool,
}

UseExpr :: struct {
	token:    Token,
	fullpath: string,
	name:     string,
	type:     ModuleType,

	// for file imports the entire AST of the imported file is directly
	// embedded into the UseExpr
	ast:      Maybe(Expr),
}

FunctionParam :: struct {
	token: Token,
	type:  Maybe(Type),
}

FunctionExpr :: struct {
	token:       Token,
	params:      []FunctionParam,
	body:        Expr,
	bound_to:    Maybe(Token),
	return_type: Maybe(Type),
	public:      bool,
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

VarBinding :: struct {
	name:        Token,
	initializer: Expr,
	type:        Maybe(Type),
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
	CONCATENATION, // ..
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

// Forms the abstract syntax tree from a provided slice of tokens.
// Also performs all module resolution.
Parser :: struct #all_or_none {
	tokens:       []Token,
	current:      int,
	had_error:    bool,
	panic_mode:   bool,
	prev_was_eof: bool,
	source_dir:   string,
	files_parsed: map[string]struct{},
	module_cache: map[string]Expr,
}

init_parser :: proc(tokens: []Token) -> Parser {
	return Parser {
		tokens = tokens,
		current = 0,
		had_error = false,
		panic_mode = false,
		prev_was_eof = false,
		source_dir = config.__dirname,
		files_parsed = make(map[string]struct{}),
		module_cache = make(map[string]Expr),
	}
}

destroy_parser :: proc(p: ^Parser) {
	delete(p.files_parsed)
	delete(p.module_cache)
}

/* Create a list of parsed declarations forming an abstract syntax tree, from
a list of tokens stored in a parser. */
parse :: proc(tokens: []Token) -> (expr: Expr, success: bool) {
	p := init_parser(tokens)
	defer destroy_parser(&p)
	return parse_with_parser(&p)
}

parse_with_parser :: proc(p: ^Parser) -> (expr: Expr, success: bool) {
	if parser_is_at_end(p) {
		return nil, true
	}
	return parse_expression_top(p), !p.had_error
}

parse_method :: proc(p: ^Parser, can_assign: bool) -> ^FunctionExpr {
	name := parser_consume(p, .IDENT, "Expect method name.")
	return parse_lambda(p, can_assign, name).(^FunctionExpr)
}

parser_get_rule :: proc(type: TokenType) -> ^ParseRule {
	return &rules[type]
}

parse_precedence :: proc(p: ^Parser, precedence: Precedence) -> Expr {
	parser_advance(p)

	if p.prev_was_eof {
		parser_error(p, parser_previous(p), "Expect expression.")
		return nil
	}

	prefix_rule := parser_get_rule(parser_previous(p).type).prefix
	if prefix_rule == nil {
		parser_error(p, parser_previous(p), "Expect expression.")
		return nil
	}

	can_assign := precedence <= .ASSIGNMENT
	expr := prefix_rule(p, can_assign)

	for precedence <= parser_get_rule(parser_peek(p).type).precedence {
		parser_advance(p)
		infix_rule := parser_get_rule(parser_previous(p).type).infix
		if infix_rule != nil {
			expr = infix_rule(p, expr, can_assign)
		}
	}

	if can_assign && parser_match(p, .EQUAL) {
		parser_error(p, parser_previous(p), "Invalid assignment target.")
	}

	return expr
}

// Parse an expression, treating newlines and semicolons as expression-separating infix operators.
parse_expression_top :: proc(p: ^Parser) -> Expr {
	fst := parse_expression(p)
	if parser_is_at_end(p) || parser_check(p, .RSQUIRLY) {
		return fst
	}

	if p.panic_mode {
		parser_synchronize(p)
	} else {
		parser_consume_any(p, "Expect newline or semicolon after expression.", .NEWLINE, .SEMI)
	}

	seq := new(SequenceExpr)
	seq.token = parser_previous(p)
	seq.left = fst
	seq.operator = parser_previous(p)
	if parser_is_at_end(p) || parser_check(p, .RSQUIRLY) {
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
	expr.token = parser_previous(p)
	expr.is_ifnt = expr.token.type == .IFNT
	expr.condition = parse_expression(p)

	parser_consume(p, .LSQUIRLY, "Expect '{' after condition.")
	expr.then_branch = parse_block_expr(p, can_assign).(^BlockExpr)

	if parser_match(p, .ELSE) {
		parser_consume(p, .LSQUIRLY, "Expect '{' after else.")
		expr.else_branch = parse_block_expr(p, can_assign).(^BlockExpr)
	}
	return expr
}

parse_switch_expr :: proc(p: ^Parser, can_assign: bool) -> Expr {
	expr := new(SwitchExpr)
	expr.token = parser_previous(p)
	cases := make([dynamic]ExprSwitchCase, 0)
	has_else_clause := false

	if parser_match(p, .LSQUIRLY) {
		// No condition
	} else {
		expr.condition = parse_expression(p)
		parser_consume(p, .LSQUIRLY, "Expect '{' after switch condition.")
	}

	for !parser_match(p, .RSQUIRLY) && !parser_is_at_end(p) {
		if parser_match(p, .ELSE) {
			has_else_clause = true
			parser_consume(p, .FAT_ARROW, "Expect '=>' after 'else'.")
			expr.else_branch = parse_expression(p)
			if parser_match(p, .NEWLINE) {}
			parser_consume(p, .RSQUIRLY, "'else' must be the last case.")
			break
		}

		case_node: ExprSwitchCase
		case_node.condition = parse_expression(p)
		parser_consume(p, .FAT_ARROW, "Expect '=>' after case.")
		case_node.body = parse_expression(p)
		if parser_match(p, .NEWLINE) {}
		append(&cases, case_node)
	}

	if !has_else_clause {
		parser_error(p, parser_peek(p), "Switch expression must have an 'else' case.")
	}

	expr.cases = cases[:]
	return expr
}

parse_while :: proc(p: ^Parser, can_assign: bool) -> Expr {
	expr := new(WhileExpr)
	expr.token = parser_previous(p)
	expr.is_whilent = expr.token.type == .WHILENT
	expr.condition = parse_expression(p)

	parser_consume(p, .LSQUIRLY, "Expect '{' after condition.")
	expr.body = parse_block_expr(p, can_assign).(^BlockExpr)
	return expr
}

parse_for :: proc(p: ^Parser, can_assign: bool) -> Expr {
	token := parser_previous(p)

	// Differentiate between for-in and classic for loop
	if parser_check(p, .IDENT) && p.tokens[p.current + 1].type == .IN {
		expr := new(ForInExpr)
		expr.token = token
		expr.var_name = parser_advance(p)
		parser_advance(p) // parser_consume IN
		expr.iterable = parse_expression(p)
		parser_consume(p, .LSQUIRLY, "Expect '{' after iterable.")
		expr.body = parse_block_expr(p, can_assign).(^BlockExpr)
		return expr
	}

	stmt := new(ForExpr)
	stmt.token = token
	if parser_match(p, .SEMI) {
		stmt.initializer = nil
	} else if parser_match(p, .VAR, .VAL) {
		stmt.initializer = parse_var_decl_expression(p, can_assign)
		parser_consume(p, .SEMI, "Expect ';' after initializer.")
	} else {
		stmt.initializer = parse_expression(p)
		parser_consume(p, .SEMI, "Expect ';' after initializer.")
	}

	if !parser_match(p, .SEMI) {
		stmt.condition = parse_expression(p)
		parser_consume(p, .SEMI, "Expect ';' after loop condition.")
	}

	if !parser_match(p, .LSQUIRLY) {
		stmt.increment = parse_expression(p)
		parser_consume(p, .LSQUIRLY, "Expect '{' after for clauses.")
	}

	stmt.body = parse_block_expr(p, can_assign).(^BlockExpr)
	return stmt
}

parse_class_expr :: proc(p: ^Parser, can_assign: bool) -> Expr {
	expr := new(ClassExpr)
	expr.token = parser_previous(p)
	expr.name = parser_consume(p, .IDENT, "Expect class name.")
	expr.public = false
	methods := make([dynamic]^FunctionExpr, 0)

	if parser_match(p, .LESS) {
		expr.superclass = parser_consume(p, .IDENT, "Expect superclass name.")
	}

	parser_consume(p, .LSQUIRLY, "Expect '{' before class body.")
	for !parser_check(p, .RSQUIRLY) && !parser_is_at_end(p) {
		append(&methods, parse_method(p, can_assign))
		if parser_match(p, .SEMI) || parser_match(p, .NEWLINE) {}
	}
	expr.methods = methods[:]

	parser_consume(p, .RSQUIRLY, "Expect '}' after class body.")
	return expr
}

parse_use_expr :: proc(p: ^Parser, can_assign: bool) -> Expr {
	expr := new(UseExpr)
	expr.token = parser_previous(p)

	relative_path_str := parser_consume(p, .STRING, "Expect module path.").lexeme
	relative_path := strings.trim(relative_path_str[1:len(relative_path_str) - 1], " ")
	abs_path, join_err := filepath.join(
		[]string{config.__dirname, relative_path},
		context.allocator,
	)
	if join_err != nil {
		parser_error(
			p,
			parser_previous(p),
			fmt.tprintf("Error when declaring module: %s", os.error_string(join_err)),
		)
		return nil
	}

	if slice.contains(STD_MODULES[:], relative_path) {
		expr.name = strings.clone(relative_path)
		expr.fullpath = abs_path
		expr.type = .BUILTIN
		expr.ast = nil
		return expr
	}

	// does the file even exist?
	if !os.exists(abs_path) {
		parser_error(p, parser_previous(p), fmt.tprintf("Module '%s' not found.", relative_path))
		return nil
	}

	expr.type = .USER
	expr.fullpath = abs_path
	expr.name = strings.clone(filepath.short_stem(relative_path))

	// check in module cache
	if cached, ok := p.module_cache[abs_path]; ok {
		expr.ast = cached
		return expr
	}

	// detect cycles
	if abs_path in p.files_parsed {
		parser_error(
			p,
			parser_previous(p),
			fmt.tprintf("Cannot cyclically import module '%v'.", relative_path),
		)
		return nil
	}

	// now read the file
	source, read_err := os.read_entire_file(abs_path, context.allocator)
	if read_err != nil {
		parser_error(
			p,
			parser_previous(p),
			fmt.tprintf("Failed to read file: %v", os.error_string(read_err)),
		)
		return nil
	}
	defer delete(source)

	tokens, lx_ok := lex(string(source))
	if !lx_ok {
		parser_error(p, parser_previous(p), "Lexer error occured when parsing module '%v'.")
		return nil
	}
	defer delete(tokens)

	child := Parser {
		tokens       = tokens,
		current      = 0,
		source_dir   = filepath.dir(abs_path),
		files_parsed = p.files_parsed,
		module_cache = p.module_cache,
		panic_mode   = false,
		prev_was_eof = false,
		had_error    = false,
	}
	p.files_parsed[abs_path] = {}
	defer delete_key(&p.files_parsed, abs_path)

	mod_ast, ps_ok := parse_with_parser(&child)
	if !ps_ok {
		p.had_error = true
		return nil
	}

	p.module_cache[abs_path] = mod_ast
	expr.ast = mod_ast
	return expr
}

parse_pub :: proc(p: ^Parser, can_assign: bool) -> Expr {
	if parser_match(p, .FUNC) {
		expr := parse_function(p, can_assign)
		if var_e, v_ok := expr.(^VarDeclExpr); v_ok {
			if len(var_e.bindings) > 0 {
				if lambda, l_ok := var_e.bindings[0].initializer.(^FunctionExpr); l_ok {
					lambda.public = true
				}
			}
		}
		return expr
	} else if parser_match(p, .CLASS) {
		expr := parse_class_expr(p, can_assign)
		if class_e, ok := expr.(^ClassExpr); ok {
			class_e.public = true
		}
		return expr
	} else {
		parser_error(p, parser_peek(p), "Only functions or classes can be set as public.")
		return nil
	}
}

// NOTE: Very important to note that this function allocates the type arguments
// on the general-purpose allocator used for the overall AST (unlike the
// typechecker which uses its own arena) and therefore the types
// created here MUST be freed when the AST nodes are being freed
parse_type_annotation :: proc(p: ^Parser) -> Type {
	if parser_check(p, .IDENT) {
		type: Type

		constructor := parser_advance(p)
		switch constructor.lexeme {
		case "Nil":
			type = tapp(.NIL)
		case "Bool":
			type = tapp(.BOOL)
		case "Number":
			type = tapp(.NUMBER)
		case "String":
			type = tapp(.STRING)
		case "List":
			inner_type := parse_type_annotation(p)
			type = tapp(.LIST, {inner_type})
		case "Any":
			type = type_any
		case "Never":
			type = type_never
		case:
			parser_error(p, parser_previous(p), "Invalid type annotation.")
			return {}
		}

		return type
	} else if parser_check(p, .LPAREN) {
		parser_advance(p) // consume the paren

		// we have a function type here, parse the args recursively
		// TODO: Technically record types also begin with a ( but that's for later
		arg_types := make([dynamic]Type, 0)
		defer delete(arg_types)
		if !parser_check(p, .RPAREN) {
			for {
				append(&arg_types, parse_type_annotation(p))
				parser_match(p, .COMMA) or_break
			}
		}

		parser_consume(p, .RPAREN, "Expect ')' after function type parameters.")
		parser_consume(p, .ARROW, "Expect '->' after function type parameter list.")

		return_type := parse_type_annotation(p)

		all_args := make([]Type, len(arg_types) + 1)
		defer delete(all_args)
		if len(arg_types) != 0 {copy(all_args, arg_types[:])}
		all_args[len(arg_types)] = return_type

		func_type := tapp(.FUNCTION, all_args)
		return func_type
	}

	parser_error(p, parser_peek(p), "Expect type annotation.")
	return {}
}

parse_var_decl_expression :: proc(p: ^Parser, can_assign: bool) -> Expr {
	expr := new(VarDeclExpr)
	expr.token = parser_previous(p)
	expr.is_final = expr.token.type == .VAL
	bindings := make([dynamic]VarBinding, 0)

	for {
		binding: VarBinding
		binding.name = parser_consume(p, .IDENT, "Expect variable name.")
		if parser_match(p, .COLON) {
			// currently the type is just a single token, will be changed later on
			binding.type = parse_type_annotation(p)
		} else {
			binding.type = nil
		}

		if parser_match(p, .EQUAL) {
			binding.initializer = parse_expression(p)
		}

		// to allow named lambdas to recurse when we get to resolving and
		// typechecking
		if fn, ok := binding.initializer.(^FunctionExpr); ok {
			fn.bound_to = binding.name
		}

		append(&bindings, binding)

		parser_match(p, .COMMA) or_break
	}
	expr.bindings = bindings[:]
	return expr
}

parse_print :: proc(p: ^Parser, can_assign: bool) -> Expr {
	expr := new(PrintExpr)
	expr.token = parser_previous(p)
	expr.expr = parse_expression(p)
	return expr
}

parse_grouping :: proc(p: ^Parser, can_assign: bool) -> Expr {
	token := parser_previous(p)
	expr := parse_expression(p)
	parser_consume(p, .RPAREN, "Expect ')' after expression.")
	grouping := new(GroupingExpr)
	grouping.token = token
	grouping.expression = expr
	return grouping
}

parse_list :: proc(p: ^Parser, can_assign: bool) -> Expr {
	list := new(ListExpr)
	list.token = parser_previous(p)
	elements := make([dynamic]Expr, 0)
	if !parser_check(p, .RSQUARE) {
		for {
			append(&elements, parse_expression(p))
			if !parser_match(p, .COMMA) {break}
		}
	}
	list.elements = elements[:]
	parser_consume(p, .RSQUARE, "Expect ']' after list elements.")
	return list
}

parse_unary :: proc(p: ^Parser, can_assign: bool) -> Expr {
	operator := parser_previous(p)
	right := parse_precedence(p, .UNARY)
	unary := new(UnaryExpr)
	unary.token = operator
	unary.operator = operator
	unary.right = right
	return unary
}

/*
Translate escape sequences in a string literal. This function allocates a new
string.

This function doesn't take ownership of the input; therefore the input will 
still need to be freed if necessary. 

In this compiler, it is used to create an escape-sequenced string out of a slice 
of the program input itself, which should **NOT** be freed until the program ends; 
which is why it does not take ownership.

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
	literal.token = parser_previous(p)

	#partial switch literal.token.type {
	case .STRING:
		literal.value = add_escape_sequences(literal.token.lexeme[1:len(literal.token.lexeme) - 1])
	case .NUMBER:
		value, ok := strconv.parse_f64(literal.token.lexeme)
		if !ok {
			parser_error(
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
		parser_error(
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
	name := parser_previous(p)

	// No-paren string call: `puts "hello"`
	// Only valid as a call, not as an assignment target
	if parser_match(p, .STRING) {
		str_literal := new(LiteralExpr)
		str_literal.token = parser_previous(p)
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
		call.rdelimiter = parser_previous(p)
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

	if can_assign && parser_match(p, .EQUAL) {
		operator := parser_previous(p)
		value := parse_expression(p)
		assign := new(AssignExpr)
		assign.token = operator
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
	super_expr.token = parser_previous(p)
	parser_consume(p, .DOT, "Expect '.' after 'super'.")
	super_expr.method = parser_consume(p, .IDENT, "Expect superclass method name.")
	method_args := make([dynamic]Expr, 0, 1)

	// was the retrieved method immediately invoked?
	if parser_match(p, .LPAREN) {
		if !parser_check(p, .RPAREN) {
			for {
				append(&method_args, parse_expression(p))
				parser_match(p, .COMMA) or_break
			}
		}
		super_expr.method_args = method_args[:]
		parser_consume(p, .RPAREN, "Expect ')' after method parameters.")
	} else {
		super_expr.method_args = nil
		delete(method_args)
	}

	return super_expr
}

parse_this :: proc(p: ^Parser, can_assign: bool) -> Expr {
	this_expr := new(ThisExpr)
	this_expr.token = parser_previous(p)
	return this_expr
}

parse_it :: proc(p: ^Parser, can_assign: bool) -> Expr {
	it_expr := new(ItExpr)
	it_expr.token = parser_previous(p)
	return it_expr
}

parse_discard :: proc(p: ^Parser, can_assign: bool) -> Expr {
	discard := new(DiscardExpr)
	discard.token = parser_previous(p)
	discard.expression = parse_expression(p)
	return discard
}

/* Handles both anonymous functions (FunctionExpr) and function declarations,
which are interpreted as a syntactic sugar on top of a VarDeclExpr */
parse_function :: proc(p: ^Parser, can_assign: bool) -> Expr {
	if !parser_check(p, .IDENT) {
		// just a good ol' anonymous function
		return parse_lambda(p, can_assign, nil)
	} else {
		// a function declaration. We interpret it as syntactic sugar over a
		// VarDeclExpr
		expr := new(VarDeclExpr)
		expr.token = parser_previous(p)
		expr.is_final = true // func decls are NOT reassignable
		bindings := make([dynamic]VarBinding, 0)

		func_binding: VarBinding
		func_binding.name = parser_consume(p, .IDENT, "Expect function name.")
		func_binding.initializer = parse_lambda(p, can_assign, func_binding.name)
		append(&bindings, func_binding)
		expr.bindings = bindings[:]
		return expr
	}
}

parse_lambda :: proc(p: ^Parser, can_assign: bool, bound_to: Maybe(Token)) -> Expr {
	lambda := new(FunctionExpr)
	lambda.token = parser_previous(p)
	lambda.bound_to = bound_to
	params := make([dynamic]FunctionParam, 0)

	// TODO: improve this error message
	parser_consume(
		p,
		.LPAREN,
		fmt.tprintf("Expect '(' after %s.", bound_to.?.lexeme if bound_to != nil else "'func'"),
	)

	if !parser_check(p, .RPAREN) {
		for {
			param: FunctionParam
			param.token = parser_consume(p, .IDENT, "Expect parameter name.")
			if parser_match(p, .COLON) {
				param.type = parse_type_annotation(p)
			}
			append(&params, param)
			parser_match(p, .COMMA) or_break
		}
	}
	lambda.params = params[:]
	parser_consume(p, .RPAREN, "Expect ')' after function parameters.")
	if parser_match(p, .COLON) {
		lambda.return_type = parse_type_annotation(p)
	}

	if parser_match(p, .FAT_ARROW) {
		// as parse_expression also parses blocks, you can technically have
		// something like func() => {}, which means func() {} is essentially
		// syntactic sugar
		lambda.body = parse_expression(p)
	} else {
		parser_consume(p, .LSQUIRLY, "Expect '=>' or '{' after function parameter list.")
		lambda.body = parse_block_expr(p, can_assign)
	}

	return lambda
}

parse_block_expr :: proc(p: ^Parser, can_assign: bool) -> Expr {
	expr := new(BlockExpr)
	expr.token = parser_previous(p) // the '{'
	for parser_match(p, .NEWLINE) {}
	if !parser_check(p, .RSQUIRLY) && !parser_is_at_end(p) {
		expr.expression = parse_expression_top(p)
	}
	parser_consume(p, .RSQUIRLY, "Expect '}' after block.")
	return expr
}

parse_break :: proc(p: ^Parser, can_assign: bool) -> Expr {
	expr := new(BreakExpr)
	expr.token = parser_previous(p)
	return expr
}

parse_continue :: proc(p: ^Parser, can_assign: bool) -> Expr {
	expr := new(ContinueExpr)
	expr.token = parser_previous(p)
	return expr
}

parse_return :: proc(p: ^Parser, can_assign: bool) -> Expr {
	expr := new(ReturnExpr)
	expr.token = parser_previous(p)
	if !parser_check_any(p, .NEWLINE, .SEMI, .RSQUIRLY) && !parser_is_at_end(p) {
		expr.value = parse_expression(p)
	}
	return expr
}

parse_exit :: proc(p: ^Parser, can_assign: bool) -> Expr {
	expr := new(ExitExpr)
	expr.token = parser_previous(p)
	if !parser_check_any(p, .NEWLINE, .SEMI, .RSQUIRLY) && !parser_is_at_end(p) {
		expr.code = parse_expression(p)
	}
	return expr
}

//---------------------------------------------------------
// Infix Rules
//---------------------------------------------------------

parse_pipe :: proc(p: ^Parser, left: Expr, can_assign: bool) -> Expr {
	operator := parser_previous(p)
	rule := parser_get_rule(operator.type)
	right := parse_precedence(p, cast(Precedence)(cast(int)rule.precedence + 1))

	pipe := new(PipeExpr)
	pipe.token = operator
	pipe.left = left
	pipe.operator = operator

	// insert `it` into the first arg of the call
	if call, ok := right.(^CallExpr); ok {
		new_args := make([dynamic]Expr)
		if len(call.arguments) != 0 {copy(new_args[:], call.arguments)}
		inserted_it := new(ItExpr)
		inserted_it.token = call.token

		append(&new_args, inserted_it)
		for arg in call.arguments {
			append(&new_args, arg)
		}

		delete(call.arguments)
		call.arguments = new_args[:]
	}
	pipe.right = right

	return pipe
}

parse_logical :: proc(p: ^Parser, left: Expr, can_assign: bool) -> Expr {
	operator := parser_previous(p)
	rule := parser_get_rule(operator.type)
	right := parse_precedence(p, cast(Precedence)(cast(int)rule.precedence + 1))

	logical := new(LogicalExpr)
	logical.token = operator
	logical.left = left
	logical.operator = operator
	logical.right = right
	return logical
}

parse_binary :: proc(p: ^Parser, left: Expr, can_assign: bool) -> Expr {
	operator := parser_previous(p)
	rule := parser_get_rule(operator.type)
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
	call.token = parser_previous(p) // The '(' token
	call.callee = left
	arguments := make([dynamic]Expr, 0)

	if !parser_check(p, .RPAREN) {
		for {
			append(&arguments, parse_expression(p))
			if !parser_match(p, .COMMA) {break}
		}
	}
	call.arguments = arguments[:]
	call.rdelimiter = parser_consume(p, .RPAREN, "Expect ')' after arguments.")
	return call
}

parse_dot :: proc(p: ^Parser, left: Expr, can_assign: bool) -> Expr {
	dot := parser_previous(p) // The '.' token
	property := parser_consume(p, .IDENT, "Expect property name after '.'.")
	if can_assign && parser_match(p, .EQUAL) {
		equals := parser_previous(p)
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
	bracket := parser_previous(p) // The '[' token
	index := parse_expression(p)
	parser_consume(p, .RSQUARE, "Expect ']' after index.")
	if can_assign && parser_match(p, .EQUAL) {
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
	.COLON         = {nil, nil, .NONE},
	.DOT           = {nil, parse_dot, .CALL},
	.DOT_DOT       = {nil, parse_binary, .CONCATENATION},
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
	.ARROW         = {nil, nil, .NONE},
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
	.CLASS         = {parse_class_expr, nil, .NONE},
	.DISCARD       = {parse_discard, nil, .NONE},
	.ELSE          = {nil, nil, .NONE},
	.EXIT          = {parse_exit, nil, .NONE},
	.FALSE         = {parse_literal, nil, .NONE},
	.FOR           = {parse_for, nil, .NONE},
	.FUNC          = {parse_function, nil, .NONE},
	.IF            = {parse_if_expr, nil, .CONDITIONAL},
	.IFNT          = {parse_if_expr, nil, .CONDITIONAL},
	.IN            = {nil, nil, .NONE},
	.IT            = {parse_it, nil, .NONE},
	.NIL           = {parse_literal, nil, .NONE},
	.NOT           = {parse_unary, nil, .NONE},
	.OR            = {nil, parse_logical, .OR},
	.PRINT         = {parse_print, nil, .NONE},
	.PUB           = {parse_pub, nil, .NONE},
	.RETURN        = {parse_return, nil, .NONE},
	.SWITCH        = {parse_switch_expr, nil, .CONDITIONAL},
	.SUPER         = {parse_super, nil, .NONE},
	.THIS          = {parse_this, nil, .NONE},
	.TRUE          = {parse_literal, nil, .NONE},
	.USE           = {parse_use_expr, nil, .NONE},
	.WHILE         = {parse_while, nil, .NONE},
	.WHILENT       = {parse_while, nil, .NONE},
	.VAR           = {parse_var_decl_expression, nil, .NONE},
	.VAL           = {parse_var_decl_expression, nil, .NONE},
	.EOF           = {nil, nil, .NONE},
}

parser_error :: proc(p: ^Parser, token: Token, message: string) {
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

parser_peek :: proc(p: ^Parser) -> Token {
	return p.tokens[p.current]
}

parser_previous :: proc(p: ^Parser) -> Token {
	return p.tokens[p.current - 1]
}

parser_is_at_end :: proc(p: ^Parser) -> bool {
	return parser_peek(p).type == .EOF
}

parser_check :: proc(p: ^Parser, type: TokenType) -> bool {
	if parser_is_at_end(p) {return false}
	return parser_peek(p).type == type
}

parser_check_any :: proc(p: ^Parser, types: ..TokenType) -> bool {
	if parser_is_at_end(p) {return false}
	for type in types {
		if parser_check(p, type) {return true}
	}
	return false
}

parser_advance :: proc(p: ^Parser) -> Token {
	p.prev_was_eof = parser_is_at_end(p)
	if !p.prev_was_eof {p.current += 1}
	return parser_previous(p)
}

parser_match :: proc(p: ^Parser, types: ..TokenType) -> bool {
	for type in types {
		if parser_check(p, type) {
			parser_advance(p)
			return true
		}
	}
	return false
}

parser_consume :: proc(p: ^Parser, type: TokenType, message: string) -> Token {
	if parser_check(p, type) {return parser_advance(p)}
	parser_error(p, parser_peek(p), message)
	return parser_peek(p)
}

parser_consume_any :: proc(p: ^Parser, message: string, types: ..TokenType) -> Token {
	if parser_check_any(p, ..types) {return parser_advance(p)}
	parser_error(p, parser_peek(p), message)
	return parser_peek(p)
}

parser_synchronize :: proc(p: ^Parser) {
	p.panic_mode = false

	for !parser_is_at_end(p) {
		if parser_previous(p).type == .NEWLINE || parser_previous(p).type == .SEMI {return}

		#partial switch parser_peek(p).type {
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

		parser_advance(p)
	}
}

// AST freeing functions
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
	case ^ClassExpr:
		for method in e.methods {
			free_expr(method)
		}
		delete(e.methods)
		free(e)
	case ^DiscardExpr:
		free_expr(e.expression)
		free(e)
	case ^ExitExpr:
		free_expr(e.code)
		free(e)
	case ^ForInExpr:
		free_expr(e.iterable)
		if e.body != nil {free_expr(e.body)}
		free(e)
	case ^ForExpr:
		free_expr(e.initializer)
		if e.body != nil {free_expr(e.body)}
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
		if e.then_branch != nil {free_expr(e.then_branch)}
		if e.else_branch != nil {free_expr(e.else_branch)}
		free(e)
	case ^ItExpr:
		free(e)
	case ^FunctionExpr:
		for param in e.params {
			type := param.type.? or_continue
			free_type(&type)
		}

		free_expr(e.body)
		delete(e.params)
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
		if e.method_args != nil {
			for arg in e.method_args {
				free_expr(arg)
			}
			delete(e.method_args)
		}
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
	case ^UseExpr:
		if e.ast != nil {
			free_expr(e.ast.?)
		}
		delete(e.name)
		delete(e.fullpath)
		free(e)
	case ^VariableExpr:
		free(e)
	case ^VarDeclExpr:
		for binding in e.bindings {
			free_expr(binding.initializer)

			type := binding.type.? or_continue
			free_type(&type)
		}
		delete(e.bindings)
		free(e)
	case ^WhileExpr:
		if e.body != nil {free_expr(e.body)}
		free_expr(e.condition)
		free(e)
	}
}


// AST pretty-printer

ast_string :: proc(expr: Expr) -> string {
	b := strings.builder_make()
	defer strings.builder_destroy(&b)

	print_expr(&b, expr, 0)
	return fmt.tprint(strings.to_string(b))
}

print_indent :: proc(b: ^strings.Builder, indent: int) {
	for i := 0; i < indent; i += 1 {
		strings.write_string(b, "  ")
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
	case ^DiscardExpr:
		print_indent(b, indent)
		strings.write_string(b, "(discard\n")
		print_expr(b, e.expression, indent + 1)
		print_indent(b, indent)
		strings.write_string(b, ")\n")
	case ^ClassExpr:
		print_indent(b, indent)
		fmt.sbprintf(b, "(class %s", e.name.lexeme)
		if e.superclass != nil {
			superclass: Token = e.superclass.?
			fmt.sbprintf(b, " < %s", superclass.lexeme)
		}
		strings.write_string(b, "\n")
		for method in e.methods {
			print_expr(b, method, indent + 1)
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
	case ^FunctionExpr:
		print_indent(b, indent)
		fmt.sbprintf(b, "(func (")
		for param, i in e.params {
			if i > 0 {strings.write_string(b, " ")}
			strings.write_string(b, param.token.lexeme)
			if type, ok := param.type.(Type); ok {
				fmt.sbprintf(b, ": %v", type_string(type, false))
			}
		}
		strings.write_string(b, ")\n")
		print_indent(b, indent + 1)
		strings.write_string(b, "=>\n")
		print_expr(b, e.body, indent + 2)
		print_indent(b, indent)
		strings.write_string(b, ")\n")
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
	case ^UseExpr:
		print_indent(b, indent)
		fmt.sbprintf(b, "(use %s", e.name)
		if ast, ok := e.ast.?; ok {
			strings.write_string(b, "\n")
			print_expr(b, ast, indent + 1)
			print_indent(b, indent)
		}
		fmt.sbprintf(b, ")\n")
	case ^VarDeclExpr:
		print_indent(b, indent)
		kind := e.is_final ? "val" : "var"
		fmt.sbprintf(b, "(%s ", kind)
		for binding, i in e.bindings {
			if i > 0 {strings.write_string(b, " ")}
			strings.write_string(b, binding.name.lexeme)
			if binding.type != nil {
				type := binding.type.(Type)
				fmt.sbprintf(b, ": %v", type_string(type, debugging = false))
			}
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
