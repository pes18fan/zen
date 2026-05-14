package zen

import "core:fmt"
import "core:os"
import tt "core:testing"

/* A `Token` or nil. */
@(private = "file")
MaybeToken :: union {
	Token,
}

/* Check if two slices of `Token`s are the same. */
@(private = "file")
expect_tokens_equal :: proc(
	want: []Token,
	got: []Token,
) -> (
	ok: bool,
	err_wanted: MaybeToken,
	err_recieved: MaybeToken,
) {
	got := got
	for i in want {
		if len(got) == 0 do break

		if i != got[0] {
			return false, i, got[0]
		}
		got = got[1:]
	}

	return true, nil, nil
}

@(private = "file")
print_tokens :: proc(f: ^os.File, tokens: []Token) {
	for token in tokens {
		fmt.fprintfln(f, "\t%v", token)
	}
	fmt.fprintln(f)
}

/* A basic test of the lexer. */
@(test)
test_lexer_default :: proc(t: ^tt.T) {
	source := `// this is a comment
func foo() {
    if not false {
        print str
    }
}

func add(a, b) => a + b

func test() {
    foo("just a little lexer exercise")
    println(add(1, 2))
}`

	lx := init_lexer(source)
	got, ok := lex(&lx)
	defer delete(got)

	if !tt.expect(t, ok, "lexer error") { return }

	want := []Token {
		Token{type = .FUNC, lexeme = "func", line = 2},
		Token{type = .IDENT, lexeme = "foo", line = 2},
		Token{type = .LPAREN, lexeme = "(", line = 2},
		Token{type = .RPAREN, lexeme = ")", line = 2},
		Token{type = .LSQUIRLY, lexeme = "{", line = 2},
		Token{type = .IF, lexeme = "if", line = 3},
		Token{type = .NOT, lexeme = "not", line = 3},
		Token{type = .FALSE, lexeme = "false", line = 3},
		Token{type = .LSQUIRLY, lexeme = "{", line = 3},
		Token{type = .PRINT, lexeme = "print", line = 4},
		Token{type = .IDENT, lexeme = "str", line = 4},
		Token{type = .SEMI, lexeme = ";", line = 4},
		Token{type = .RSQUIRLY, lexeme = "}", line = 5},
		Token{type = .SEMI, lexeme = ";", line = 5},
		Token{type = .RSQUIRLY, lexeme = "}", line = 6},
		Token{type = .SEMI, lexeme = ";", line = 6},
		Token{type = .FUNC, lexeme = "func", line = 8},
		Token{type = .IDENT, lexeme = "add", line = 8},
		Token{type = .LPAREN, lexeme = "(", line = 8},
		Token{type = .IDENT, lexeme = "a", line = 8},
		Token{type = .COMMA, lexeme = ",", line = 8},
		Token{type = .IDENT, lexeme = "b", line = 8},
		Token{type = .RPAREN, lexeme = ")", line = 8},
		Token{type = .FAT_ARROW, lexeme = "=>", line = 8},
		Token{type = .IDENT, lexeme = "a", line = 8},
		Token{type = .PLUS, lexeme = "+", line = 8},
		Token{type = .IDENT, lexeme = "b", line = 8},
		Token{type = .SEMI, lexeme = ";", line = 8},
		Token{type = .FUNC, lexeme = "func", line = 10},
		Token{type = .IDENT, lexeme = "test", line = 10},
		Token{type = .LPAREN, lexeme = "(", line = 10},
		Token{type = .RPAREN, lexeme = ")", line = 10},
		Token{type = .LSQUIRLY, lexeme = "{", line = 10},
		Token{type = .IDENT, lexeme = "foo", line = 11},
		Token{type = .LPAREN, lexeme = "(", line = 11},
		Token{type = .STRING, lexeme = "\"just a little lexer exercise\"", line = 11},
		Token{type = .RPAREN, lexeme = ")", line = 11},
		Token{type = .SEMI, lexeme = ";", line = 11},
		Token{type = .IDENT, lexeme = "println", line = 12},
		Token{type = .LPAREN, lexeme = "(", line = 12},
		Token{type = .IDENT, lexeme = "add", line = 12},
		Token{type = .LPAREN, lexeme = "(", line = 12},
		Token{type = .NUMBER, lexeme = "1", line = 12},
		Token{type = .COMMA, lexeme = ",", line = 12},
		Token{type = .NUMBER, lexeme = "2", line = 12},
		Token{type = .RPAREN, lexeme = ")", line = 12},
		Token{type = .RPAREN, lexeme = ")", line = 12},
		Token{type = .SEMI, lexeme = ";", line = 12},
		Token{type = .RSQUIRLY, lexeme = "}", line = 13},
		Token{type = .EOF, lexeme = "", line = 13},
	}

	are_equal, wanted, recieved := expect_tokens_equal(want, got)
	tt.expectf(t, are_equal, "want %v, got %v", wanted, recieved)
}

/* Test chained multiline method calls for the lexer. */
@(test)
test_lexer_chained_calls :: proc(t: ^tt.T) {
	source := `some_string
    .reverse()
    .capitalize()
    `

	lx := init_lexer(source)
	got, ok := lex(&lx)
	defer delete(got)

	if !tt.expect(t, ok, "lexer error") { return }

	want := []Token {
		Token{type = .IDENT, lexeme = "some_string", line = 1},
		Token{type = .DOT, lexeme = ".", line = 2},
		Token{type = .IDENT, lexeme = "reverse", line = 2},
		Token{type = .LPAREN, lexeme = "(", line = 2},
		Token{type = .RPAREN, lexeme = ")", line = 2},
		Token{type = .DOT, lexeme = ".", line = 3},
		Token{type = .IDENT, lexeme = "capitalize", line = 3},
		Token{type = .LPAREN, lexeme = "(", line = 3},
		Token{type = .RPAREN, lexeme = ")", line = 3},
		Token{type = .SEMI, lexeme = ";", line = 3},
		Token{type = .EOF, lexeme = "", line = 4},
	}

	are_equal, wanted, recieved := expect_tokens_equal(want, got)
	tt.expectf(t, are_equal, "want %v, got %v", wanted, recieved)
}

/* Test single-line block ASI for the lexer. */
@(test)
test_lexer_oneline_block :: proc(t: ^tt.T) {
	source := `func a(x) { return x * 2 }`

	lx := init_lexer(source)
	got, ok := lex(&lx)
	defer delete(got)

	if !tt.expect(t, ok, "lexer error") { return }

	want := []Token {
		Token{type = .FUNC, lexeme = "func", line = 1},
		Token{type = .IDENT, lexeme = "a", line = 1},
		Token{type = .LPAREN, lexeme = "(", line = 1},
		Token{type = .IDENT, lexeme = "x", line = 1},
		Token{type = .RPAREN, lexeme = ")", line = 1},
		Token{type = .LSQUIRLY, lexeme = "{", line = 1},
		Token{type = .RETURN, lexeme = "return", line = 1},
		Token{type = .IDENT, lexeme = "x", line = 1},
		Token{type = .STAR, lexeme = "*", line = 1},
		Token{type = .NUMBER, lexeme = "2", line = 1},
		Token{type = .SEMI, lexeme = ";", line = 1},
		Token{type = .RSQUIRLY, lexeme = "}", line = 1},
		Token{type = .EOF, lexeme = "", line = 1},
	}

	are_equal, wanted, recieved := expect_tokens_equal(want, got)
	tt.expectf(t, are_equal, "want %v, got %v", wanted, recieved)
}

/* Test empty source. */
@(test)
test_lexer_empty :: proc(t: ^tt.T) {
	lx := init_lexer("")
	got, ok := lex(&lx)
	defer delete(got)

	if !tt.expect(t, ok, "lexer error on empty source") { return }

	tt.expectf(t, len(got) == 1, "expected 1 token (EOF), got %v", len(got))
	tt.expect_value(t, got[0].type, TokenType.EOF)
}

/* Test comments-only source. */
@(test)
test_lexer_comment_only :: proc(t: ^tt.T) {
	lx := init_lexer("// just a comment\n")
	got, ok := lex(&lx)
	defer delete(got)

	if !tt.expect(t, ok, "lexer error") { return }
	tt.expect_value(t, got[0].type, TokenType.EOF)
}

/* Test various literal types. */
@(test)
test_lexer_literals :: proc(t: ^tt.T) {
	source := `42 3.14 "hello" true false nil`

	lx := init_lexer(source)
	got, ok := lex(&lx)
	defer delete(got)

	if !tt.expect(t, ok, "lexer error") { return }

	want := []Token {
		Token{type = .NUMBER, lexeme = "42", line = 1},
		Token{type = .NUMBER, lexeme = "3.14", line = 1},
		Token{type = .STRING, lexeme = "\"hello\"", line = 1},
		Token{type = .TRUE, lexeme = "true", line = 1},
		Token{type = .FALSE, lexeme = "false", line = 1},
		Token{type = .NIL, lexeme = "nil", line = 1},
		Token{type = .EOF, lexeme = "", line = 1},
	}

	are_equal, wanted, recieved := expect_tokens_equal(want, got)
	tt.expectf(t, are_equal, "want %v, got %v", wanted, recieved)
}

/* Test operators and punctuation. */
@(test)
test_lexer_operators :: proc(t: ^tt.T) {
	source := `+ - * / % == != < > <= >= and or not ( ) [ ] { } . , |> =>`

	lx := init_lexer(source)
	got, ok := lex(&lx)
	defer delete(got)

	if !tt.expect(t, ok, "lexer error") { return }

	want := []Token {
		Token{type = .PLUS, lexeme = "+", line = 1},
		Token{type = .MINUS, lexeme = "-", line = 1},
		Token{type = .STAR, lexeme = "*", line = 1},
		Token{type = .SLASH, lexeme = "/", line = 1},
		Token{type = .PERCENT, lexeme = "%", line = 1},
		Token{type = .EQUAL_EQUAL, lexeme = "==", line = 1},
		Token{type = .BANG_EQUAL, lexeme = "!=", line = 1},
		Token{type = .LESS, lexeme = "<", line = 1},
		Token{type = .GREATER, lexeme = ">", line = 1},
		Token{type = .LESS_EQUAL, lexeme = "<=", line = 1},
		Token{type = .GREATER_EQUAL, lexeme = ">=", line = 1},
		Token{type = .AND, lexeme = "and", line = 1},
		Token{type = .OR, lexeme = "or", line = 1},
		Token{type = .NOT, lexeme = "not", line = 1},
		Token{type = .LPAREN, lexeme = "(", line = 1},
		Token{type = .RPAREN, lexeme = ")", line = 1},
		Token{type = .LSQUARE, lexeme = "[", line = 1},
		Token{type = .RSQUARE, lexeme = "]", line = 1},
		Token{type = .LSQUIRLY, lexeme = "{", line = 1},
		Token{type = .RSQUIRLY, lexeme = "}", line = 1},
		Token{type = .DOT, lexeme = ".", line = 1},
		Token{type = .COMMA, lexeme = ",", line = 1},
		Token{type = .BAR_GREATER, lexeme = "|>", line = 1},
		Token{type = .FAT_ARROW, lexeme = "=>", line = 1},
		Token{type = .EOF, lexeme = "", line = 1},
	}

	are_equal, wanted, recieved := expect_tokens_equal(want, got)
	tt.expectf(t, are_equal, "want %v, got %v", wanted, recieved)
}

/* Test ASI: newline after identifier inserts semicolon. */
@(test)
test_lexer_asi_identifier :: proc(t: ^tt.T) {
	lx := init_lexer("a\nb")
	got, ok := lex(&lx)
	defer delete(got)

	if !tt.expect(t, ok, "lexer error") { return }

	want := []Token {
		Token{type = .IDENT, lexeme = "a", line = 1},
		Token{type = .SEMI, lexeme = ";", line = 1},
		Token{type = .IDENT, lexeme = "b", line = 2},
		Token{type = .EOF, lexeme = "", line = 2},
	}

	are_equal, wanted, recieved := expect_tokens_equal(want, got)
	tt.expectf(t, are_equal, "want %v, got %v", wanted, recieved)
}

/* Test that ASI is suppressed inside brackets for lists. */
@(test)
test_lexer_asi_suppressed_in_list :: proc(t: ^tt.T) {
	lx := init_lexer("[\na\n]")
	got, ok := lex(&lx)
	defer delete(got)

	if !tt.expect(t, ok, "lexer error") { return }

	want := []Token {
		Token{type = .LSQUARE, lexeme = "[", line = 1},
		Token{type = .IDENT, lexeme = "a", line = 2},
		Token{type = .RSQUARE, lexeme = "]", line = 3},
		Token{type = .EOF, lexeme = "", line = 3},
	}

	are_equal, wanted, recieved := expect_tokens_equal(want, got)
	tt.expectf(t, are_equal, "want %v, got %v", wanted, recieved)
}

/* Test that the 'in' keyword suppresses ASI (for for-in loops). */
@(test)
test_lexer_asi_suppressed_in :: proc(t: ^tt.T) {
	lx := init_lexer("a\nin b")
	got, ok := lex(&lx)
	defer delete(got)

	if !tt.expect(t, ok, "lexer error") { return }

	want := []Token {
		Token{type = .IDENT, lexeme = "a", line = 1},
		Token{type = .IN, lexeme = "in", line = 2},
		Token{type = .IDENT, lexeme = "b", line = 2},
		Token{type = .EOF, lexeme = "", line = 2},
	}

	are_equal, wanted, recieved := expect_tokens_equal(want, got)
	tt.expectf(t, are_equal, "want %v, got %v", wanted, recieved)
}

/* Test ASI: newline after closing brace inserts semicolon. */
@(test)
test_lexer_asi_r_squirly :: proc(t: ^tt.T) {
	lx := init_lexer("}\nprint")
	got, ok := lex(&lx)
	defer delete(got)

	if !tt.expect(t, ok, "lexer error") { return }

	want := []Token {
		Token{type = .RSQUIRLY, lexeme = "}", line = 1},
		Token{type = .SEMI, lexeme = ";", line = 1},
		Token{type = .PRINT, lexeme = "print", line = 2},
		Token{type = .EOF, lexeme = "", line = 2},
	}

	are_equal, wanted, recieved := expect_tokens_equal(want, got)
	tt.expectf(t, are_equal, "want %v, got %v", wanted, recieved)
}

/* Test keywords. */
@(test)
test_lexer_keywords :: proc(t: ^tt.T) {
	source := `class func if while for break continue return switch var val pub use exit this super`

	lx := init_lexer(source)
	got, ok := lex(&lx)
	defer delete(got)

	if !tt.expect(t, ok, "lexer error") { return }

	want := []Token {
		Token{type = .CLASS, lexeme = "class", line = 1},
		Token{type = .FUNC, lexeme = "func", line = 1},
		Token{type = .IF, lexeme = "if", line = 1},
		Token{type = .WHILE, lexeme = "while", line = 1},
		Token{type = .FOR, lexeme = "for", line = 1},
		Token{type = .BREAK, lexeme = "break", line = 1},
		Token{type = .CONTINUE, lexeme = "continue", line = 1},
		Token{type = .RETURN, lexeme = "return", line = 1},
		Token{type = .SWITCH, lexeme = "switch", line = 1},
		Token{type = .VAR, lexeme = "var", line = 1},
		Token{type = .VAL, lexeme = "val", line = 1},
		Token{type = .PUB, lexeme = "pub", line = 1},
		Token{type = .USE, lexeme = "use", line = 1},
		Token{type = .EXIT, lexeme = "exit", line = 1},
		Token{type = .THIS, lexeme = "this", line = 1},
		Token{type = .SUPER, lexeme = "super", line = 1},
		Token{type = .EOF, lexeme = "", line = 1},
	}

	are_equal, wanted, recieved := expect_tokens_equal(want, got)
	tt.expectf(t, are_equal, "want %v, got %v", wanted, recieved)
}
