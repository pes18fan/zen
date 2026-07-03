package zen

import "core:fmt"
import "core:os"
import tt "core:testing"

/* Check if two slices of `Token`s are the same. */
/* Does NOT compare the `source` fields. */
@(private = "file")
expect_tokens_equal :: proc(
	want: []Token,
	got: []Token,
) -> (
	ok: bool,
	err_wanted: Maybe(Token),
	err_recieved: Maybe(Token),
) {
	got := got
	for tok in want {
		if len(got) == 0 {break}

		if tok.type != got[0].type {return false, tok, got[0]}
		if tok.lexeme != got[0].lexeme {return false, tok, got[0]}
		if tok.position != got[0].position {return false, tok, got[0]}
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
        echo str;
    }
};

func add(a, b) => a + b;

func test() {
    foo("just a little lexer exercise");
    println(add(1, 2));
};`

	got, ok := lex(source)
	defer delete(got)

	if !tt.expect(t, ok, "lexer error") {return}

	want := []Token {
		Token{type = .FUNC, lexeme = "func", position = Pos{2, 1}},
		Token{type = .IDENT, lexeme = "foo", position = Pos{2, 6}},
		Token{type = .LPAREN, lexeme = "(", position = Pos{2, 9}},
		Token{type = .RPAREN, lexeme = ")", position = Pos{2, 10}},
		Token{type = .LSQUIRLY, lexeme = "{", position = Pos{2, 12}},
		Token{type = .IF, lexeme = "if", position = Pos{3, 5}},
		Token{type = .NOT, lexeme = "not", position = Pos{3, 8}},
		Token{type = .FALSE, lexeme = "false", position = Pos{3, 12}},
		Token{type = .LSQUIRLY, lexeme = "{", position = Pos{3, 18}},
		Token{type = .ECHO, lexeme = "echo", position = Pos{4, 9}},
		Token{type = .IDENT, lexeme = "str", position = Pos{4, 14}},
		Token{type = .SEMI, lexeme = ";", position = Pos{4, 17}},
		Token{type = .RSQUIRLY, lexeme = "}", position = Pos{5, 5}},
		Token{type = .RSQUIRLY, lexeme = "}", position = Pos{6, 1}},
		Token{type = .SEMI, lexeme = ";", position = Pos{6, 2}},
		Token{type = .FUNC, lexeme = "func", position = Pos{8, 1}},
		Token{type = .IDENT, lexeme = "add", position = Pos{8, 6}},
		Token{type = .LPAREN, lexeme = "(", position = Pos{8, 9}},
		Token{type = .IDENT, lexeme = "a", position = Pos{8, 10}},
		Token{type = .COMMA, lexeme = ",", position = Pos{8, 11}},
		Token{type = .IDENT, lexeme = "b", position = Pos{8, 13}},
		Token{type = .RPAREN, lexeme = ")", position = Pos{8, 14}},
		Token{type = .FAT_ARROW, lexeme = "=>", position = Pos{8, 16}},
		Token{type = .IDENT, lexeme = "a", position = Pos{8, 19}},
		Token{type = .PLUS, lexeme = "+", position = Pos{8, 21}},
		Token{type = .IDENT, lexeme = "b", position = Pos{8, 23}},
		Token{type = .SEMI, lexeme = ";", position = Pos{8, 24}},
		Token{type = .FUNC, lexeme = "func", position = Pos{10, 1}},
		Token{type = .IDENT, lexeme = "test", position = Pos{10, 6}},
		Token{type = .LPAREN, lexeme = "(", position = Pos{10, 10}},
		Token{type = .RPAREN, lexeme = ")", position = Pos{10, 11}},
		Token{type = .LSQUIRLY, lexeme = "{", position = Pos{10, 13}},
		Token{type = .IDENT, lexeme = "foo", position = Pos{11, 5}},
		Token{type = .LPAREN, lexeme = "(", position = Pos{11, 8}},
		Token{type = .STRING, lexeme = "\"just a little lexer exercise\"", position = Pos{11, 9}},
		Token{type = .RPAREN, lexeme = ")", position = Pos{11, 39}},
		Token{type = .SEMI, lexeme = ";", position = Pos{11, 40}},
		Token{type = .IDENT, lexeme = "println", position = Pos{12, 5}},
		Token{type = .LPAREN, lexeme = "(", position = Pos{12, 12}},
		Token{type = .IDENT, lexeme = "add", position = Pos{12, 13}},
		Token{type = .LPAREN, lexeme = "(", position = Pos{12, 16}},
		Token{type = .NUMBER, lexeme = "1", position = Pos{12, 17}},
		Token{type = .COMMA, lexeme = ",", position = Pos{12, 18}},
		Token{type = .NUMBER, lexeme = "2", position = Pos{12, 20}},
		Token{type = .RPAREN, lexeme = ")", position = Pos{12, 21}},
		Token{type = .RPAREN, lexeme = ")", position = Pos{12, 22}},
		Token{type = .SEMI, lexeme = ";", position = Pos{12, 23}},
		Token{type = .RSQUIRLY, lexeme = "}", position = Pos{13, 1}},
		Token{type = .SEMI, lexeme = ";", position = Pos{13, 2}},
		Token{type = .EOF, lexeme = "", position = Pos{13, 3}},
	}

	are_equal, wanted, recieved := expect_tokens_equal(want, got)
	tt.expectf(t, are_equal, "want %v, got %v", wanted, recieved)
}

/* Test empty source. */
@(test)
test_lexer_empty :: proc(t: ^tt.T) {
	got, ok := lex("")
	defer delete(got)

	if !tt.expect(t, ok, "lexer error on empty source") {return}

	tt.expectf(t, len(got) == 1, "expected 1 token (EOF), got %v", len(got))
	tt.expect_value(t, got[0].type, TokenType.EOF)
}

/* Test comments-only source. */
@(test)
test_lexer_comment_only :: proc(t: ^tt.T) {
	got, ok := lex("// just a comment\n")
	defer delete(got)

	if !tt.expect(t, ok, "lexer error") {return}
	tt.expectf(t, len(got) == 1, "expected 1 token (EOF), got %v", len(got))
	tt.expect_value(t, got[0].type, TokenType.EOF)
}

/* Test various literal types. */
@(test)
test_lexer_literals :: proc(t: ^tt.T) {
	source := `42 3.14 1e2 "hello" true false nil`

	got, ok := lex(source)
	defer delete(got)

	if !tt.expect(t, ok, "lexer error") {return}

	want := []Token {
		Token{type = .NUMBER, lexeme = "42", position = Pos{1, 1}},
		Token{type = .NUMBER, lexeme = "3.14", position = Pos{1, 4}},
		Token{type = .NUMBER, lexeme = "1e2", position = Pos{1, 9}},
		Token{type = .STRING, lexeme = "\"hello\"", position = Pos{1, 13}},
		Token{type = .TRUE, lexeme = "true", position = Pos{1, 21}},
		Token{type = .FALSE, lexeme = "false", position = Pos{1, 26}},
		Token{type = .NIL, lexeme = "nil", position = Pos{1, 32}},
		Token{type = .EOF, lexeme = "", position = Pos{1, 35}},
	}

	are_equal, wanted, recieved := expect_tokens_equal(want, got)
	tt.expectf(t, are_equal, "want %v, got %v", wanted, recieved)
}

/* Test operators and punctuation. */
@(test)
test_lexer_operators :: proc(t: ^tt.T) {
	source := `+ - * / % == != < > <= >= and or not ( ) [ ] { } . , |> =>`

	got, ok := lex(source)
	defer delete(got)

	if !tt.expect(t, ok, "lexer error") {return}

	want := []Token {
		Token{type = .PLUS, lexeme = "+", position = Pos{1, 1}},
		Token{type = .MINUS, lexeme = "-", position = Pos{1, 3}},
		Token{type = .STAR, lexeme = "*", position = Pos{1, 5}},
		Token{type = .SLASH, lexeme = "/", position = Pos{1, 7}},
		Token{type = .PERCENT, lexeme = "%", position = Pos{1, 9}},
		Token{type = .EQUAL_EQUAL, lexeme = "==", position = Pos{1, 11}},
		Token{type = .BANG_EQUAL, lexeme = "!=", position = Pos{1, 14}},
		Token{type = .LESS, lexeme = "<", position = Pos{1, 17}},
		Token{type = .GREATER, lexeme = ">", position = Pos{1, 19}},
		Token{type = .LESS_EQUAL, lexeme = "<=", position = Pos{1, 21}},
		Token{type = .GREATER_EQUAL, lexeme = ">=", position = Pos{1, 24}},
		Token{type = .AND, lexeme = "and", position = Pos{1, 27}},
		Token{type = .OR, lexeme = "or", position = Pos{1, 31}},
		Token{type = .NOT, lexeme = "not", position = Pos{1, 34}},
		Token{type = .LPAREN, lexeme = "(", position = Pos{1, 38}},
		Token{type = .RPAREN, lexeme = ")", position = Pos{1, 40}},
		Token{type = .LSQUARE, lexeme = "[", position = Pos{1, 42}},
		Token{type = .RSQUARE, lexeme = "]", position = Pos{1, 44}},
		Token{type = .LSQUIRLY, lexeme = "{", position = Pos{1, 46}},
		Token{type = .RSQUIRLY, lexeme = "}", position = Pos{1, 48}},
		Token{type = .DOT, lexeme = ".", position = Pos{1, 50}},
		Token{type = .COMMA, lexeme = ",", position = Pos{1, 52}},
		Token{type = .BAR_GREATER, lexeme = "|>", position = Pos{1, 54}},
		Token{type = .FAT_ARROW, lexeme = "=>", position = Pos{1, 57}},
		Token{type = .EOF, lexeme = "", position = Pos{1, 59}},
	}

	are_equal, wanted, recieved := expect_tokens_equal(want, got)
	tt.expectf(t, are_equal, "want %v, got %v", wanted, recieved)
}

/* Test keywords. */
@(test)
test_lexer_keywords :: proc(t: ^tt.T) {
	source := `func if while for break continue return switch var val pub use exit`

	got, ok := lex(source)
	defer delete(got)

	if !tt.expect(t, ok, "lexer error") {return}

	want := []Token {
		Token{type = .FUNC, lexeme = "func", position = Pos{1, 1}},
		Token{type = .IF, lexeme = "if", position = Pos{1, 6}},
		Token{type = .WHILE, lexeme = "while", position = Pos{1, 9}},
		Token{type = .FOR, lexeme = "for", position = Pos{1, 15}},
		Token{type = .BREAK, lexeme = "break", position = Pos{1, 19}},
		Token{type = .CONTINUE, lexeme = "continue", position = Pos{1, 25}},
		Token{type = .RETURN, lexeme = "return", position = Pos{1, 34}},
		Token{type = .SWITCH, lexeme = "switch", position = Pos{1, 41}},
		Token{type = .VAR, lexeme = "var", position = Pos{1, 48}},
		Token{type = .VAL, lexeme = "val", position = Pos{1, 52}},
		Token{type = .PUB, lexeme = "pub", position = Pos{1, 56}},
		Token{type = .USE, lexeme = "use", position = Pos{1, 60}},
		Token{type = .EXIT, lexeme = "exit", position = Pos{1, 64}},
		Token{type = .EOF, lexeme = "", position = Pos{1, 68}},
	}

	are_equal, wanted, recieved := expect_tokens_equal(want, got)
	tt.expectf(t, are_equal, "want %v, got %v", wanted, recieved)
}
