package zen

import "core:fmt"
import "core:os"
import "core:unicode/utf8"

/* The type of a token. */
TokenType :: enum {
	// single characters
	LPAREN,
	RPAREN,
	LSQUIRLY,
	RSQUIRLY,
	LSQUARE,
	RSQUARE,
	BACKSLASH,
	COMMA,
	DOT,
	MINUS,
	PLUS,
	SEMI,
	SLASH,
	STAR,
	NEWLINE,

	// one or two character tokens
	BANG_EQUAL,
	BAR_GREATER, // |>
	EQUAL,
	EQUAL_EQUAL,
	FAT_ARROW,
	GREATER,
	GREATER_EQUAL,
	LESS,
	LESS_EQUAL,

	// literals
	IDENT,
	STRING,
	NUMBER,

	// keywords
	AND,
	BREAK,
	CONTINUE,
	CLASS,
	ELSE,
	EXPORT,
	FALSE,
	FOR,
	FUNC,
	IF,
	IN,
	IT,
	NIL,
	NOT,
	OR,
	PRINT,
	RETURN,
	SWITCH,
	SUPER,
	THIS,
	TRUE,
	USE,
	WHILE,
	VAR,
	VAL,
	EOF,
}

/* A token in the source. */
Token :: struct {
	type:   TokenType,
	lexeme: string,
	line:   int,
}

/* The lexer. */
Lexer :: struct {
	source:    string,
	start:     int,
	current:   int,
	line:      int,
	had_error: bool,
}

/* Create a new lexer. */
init_lexer :: proc(source: string) -> Lexer {
	return Lexer{source = source, start = 0, current = 0, line = 1, had_error = false}
}

/* 
Reports a syntax error. Assumes that `token` is an illegal token since only
illegal tokens are returned on syntax errors. 
*/
@(private = "file")
syntax_error :: proc(l: ^Lexer, message: string) {
	color_red("syntax error: ", os.stderr)
	fmt.eprintf("%s\n", message)
	fmt.eprintf("  on [line %d]\n", l.line)
	l.had_error = true
}

/* Returns true if `c` is alphanumeric, or a question mark. */
@(private = "file")
is_alphanumeric_or_qn :: proc(c: rune) -> bool {
	return is_alpha(c) || is_digit(c) || c == '?'
}

/* Returns true if `c` is a letter or underscore. */
@(private = "file")
is_alpha :: proc(c: rune) -> bool {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}

/* Returns true if `c` is a digit. */
@(private = "file")
is_digit :: proc(c: rune) -> bool {
	return c >= '0' && c <= '9'
}

/* Returns true if the scanner reached the end of the source. */
@(private = "file")
is_at_end :: proc(l: ^Lexer) -> bool {
	return l.current >= len(l.source)
}

/* Consume the current character and return it. */
@(private = "file")
advance :: proc(l: ^Lexer) -> rune #no_bounds_check {
	defer l.current += 1
	return utf8.rune_at(l.source, l.current)
}

/* Return the current character without consuming it. */
@(private = "file")
peek :: proc(l: ^Lexer) -> rune #no_bounds_check {
	return utf8.rune_at(l.source, l.current)
}

/* Returns the character after the current one. */
@(private = "file")
peek_next :: proc(l: ^Lexer) -> rune {
	if is_at_end(l) {
		return utf8.RUNE_EOF
	}
	return utf8.rune_at(l.source, l.current + 1)
}

/* Returns the previously consumed character. */
@(private = "file")
previous :: proc(l: ^Lexer) -> rune {
	return utf8.rune_at(l.source, l.current - 1)
}

/* Consume the next character if it matches `expected`. */
@(private = "file")
match :: proc(l: ^Lexer, expected: rune) -> bool {
	if is_at_end(l) {return false}
	if peek(l) != expected {
		return false
	}
	defer l.current += 1

	return true
}

/* Create a token of the provided `type`. */
@(private = "file")
make_token :: proc(l: ^Lexer, type: TokenType) -> Token {
	return Token{type = type, lexeme = l.source[l.start:l.current], line = l.line}
}

/*
Insert a "semicolon" at the end of expressions and statements. There aren't
actually any semicolons actually added in, but I'm using that terminology for
familiarity. This is done to make the parser simpler to work with.
This is the way it works:
1. Insert a semi after a line's final token.
    a. ONLY when that token is one of these:
        - identifier
        - literal
        - break
        - return
        - ) }
    b. And ONLY when the next token isn't one of these:
        - infix operator
        - dot
*/
@(private = "file")
insert_semis :: proc(tokens: []Token) -> []Token {
	defer delete(tokens)
	length := len(tokens)
	result := make([dynamic]Token)

	for token, idx in tokens {
		/* Insert a semi after a line's final token. */
		if token.type == .NEWLINE {
			semi := Token {
				type   = .SEMI,
				lexeme = ";",
				line   = token.line,
			}

			/* If the very first token is a newline, ignore it and continue. */
			if idx == 0 {
				continue
			}

			/* No need to check if idx is too big for us to do a tokens[idx + 1],
			 * since the last token will always be EOF and a newline will be at
			 * most the second to last token. */

			/* If the previous token is a backslash, the user is explicitly
			 * continuing the current line, so move on. */
			if tokens[idx - 1].type == .BACKSLASH {
				continue
			}

			/* Check the first and last points; append only if the previous
			  * token IS one of some particular types and if the next token IS 
			  * NOT one of some particular types. */
			#partial switch tokens[idx - 1].type {
			case .IDENT, .STRING, .NUMBER, .TRUE, .THIS, .FALSE, .NIL, .BREAK, .CONTINUE, .RETURN, .RPAREN, .RSQUIRLY, .RSQUARE, .IT:
				#partial switch tokens[idx + 1].type {
				case .IN, .OR, .AND, .DOT, .PLUS, .MINUS, .STAR, .SLASH, .EQUAL, .EQUAL_EQUAL, .BANG_EQUAL, .LESS, .LESS_EQUAL, .GREATER, .GREATER_EQUAL, .BAR_GREATER, .COMMA, .FAT_ARROW:
					continue
				case:
					append(&result, semi)
				}
			}
		} else {
			if token.type != .BACKSLASH {
				append(&result, token)
			}
		}
	}

	return result[:]
}

/*
Ignore any whitespace character (and comment) encountered. Newlines do not
fall in this category, they are handled separately, as they are used for
automatic semicolon insertion.
*/
@(private = "file")
skip_whitespace :: proc(l: ^Lexer) {
	for {
		c := peek(l)

		switch c {
		case '\t', '\v', '\f', '\r', ' ':
			advance(l)
		case '/':
			if peek_next(l) == '/' {
				for peek(l) != '\n' && !is_at_end(l) {
					advance(l)
				}
			} else {
				return
			}
		case:
			return
		}
	}
}

/* Test for the rest of a potential keyword's lexeme. */
@(private = "file")
check_keyword :: proc(l: ^Lexer, start, length: int, rest: string, type: TokenType) -> TokenType {
	if start + length != l.current - l.start {
		return .IDENT
	}

	actual := l.source[l.start + start:l.start + start + length]
	if actual == rest {
		return type
	}

	return .IDENT
}

/* Find the type of an indentifier. */
@(private = "file")
ident_type :: proc(l: ^Lexer) -> TokenType {
	switch utf8.rune_at(l.source, l.start) {
	case 'a':
		return check_keyword(l, 1, 2, "nd", .AND)
	case 'b':
		return check_keyword(l, 1, 4, "reak", .BREAK)
	case 'c':
		{
			if l.current - l.start > 1 {
				switch utf8.rune_at(l.source, l.start + 1) {
				case 'o':
					return check_keyword(l, 2, 6, "ntinue", .CONTINUE)
				case 'l':
					return check_keyword(l, 2, 3, "ass", .CLASS)
				}
			}
		}
	case 'e':
		{
			if l.current - l.start > 1 {
				switch utf8.rune_at(l.source, l.start + 1) {
				case 'l':
					return check_keyword(l, 2, 2, "se", .ELSE)
				case 'x':
					return check_keyword(l, 2, 4, "port", .EXPORT)
				}
			}
		}
	case 'f':
		{
			if l.current - l.start > 1 {
				switch utf8.rune_at(l.source, l.start + 1) {
				case 'a':
					return check_keyword(l, 2, 3, "lse", .FALSE)
				case 'o':
					return check_keyword(l, 2, 1, "r", .FOR)
				case 'u':
					return check_keyword(l, 2, 2, "nc", .FUNC)
				}
			}
		}
	case 'i':
		{
			if l.current - l.start > 1 {
				switch utf8.rune_at(l.source, l.start + 1) {
				case 'f':
					return check_keyword(l, 2, 0, "", .IF)
				case 'n':
					return check_keyword(l, 2, 0, "", .IN)
				case 't':
					return check_keyword(l, 2, 0, "", .IT)
				}
			}
		}
	case 'n':
		{
			if l.current - l.start > 1 {
				switch utf8.rune_at(l.source, l.start + 1) {
				case 'i':
					return check_keyword(l, 2, 1, "l", .NIL)
				case 'o':
					return check_keyword(l, 2, 1, "t", .NOT)
				}
			}
		}
	case 'o':
		return check_keyword(l, 1, 1, "r", .OR)
	case 'p':
		return check_keyword(l, 1, 4, "rint", .PRINT)
	case 'r':
		return check_keyword(l, 1, 5, "eturn", .RETURN)
	case 's':
		{
			if l.current - l.start > 1 {
				switch utf8.rune_at(l.source, l.start + 1) {
				case 'u':
					return check_keyword(l, 2, 3, "per", .SUPER)
				case 'w':
					return check_keyword(l, 2, 4, "itch", .SWITCH)
				}
			}
		}
	case 't':
		{
			if l.current - l.start > 1 {
				switch utf8.rune_at(l.source, l.start + 1) {
				case 'r':
					return check_keyword(l, 2, 2, "ue", .TRUE)
				case 'h':
					return check_keyword(l, 2, 2, "is", .THIS)
				}
			}
		}
	case 'u':
		return check_keyword(l, 1, 2, "se", .USE)
	case 'w':
		return check_keyword(l, 1, 4, "hile", .WHILE)
	case 'v':
		{
			if l.current - l.start > 1 {
				switch utf8.rune_at(l.source, l.start + 1) {
				case 'a':
					if l.current - l.start > 2 {
						switch utf8.rune_at(l.source, l.start + 2) {
						case 'r':
							return check_keyword(l, 3, 0, "", .VAR)
						case 'l':
							return check_keyword(l, 3, 0, "", .VAL)
						}
					}
				}
			}
		}
	}

	return .IDENT
}

/* Consume an identifier. */
@(private = "file")
tok_ident :: proc(l: ^Lexer) -> Token {
	// Consume letters, underscores and question marks.
	for is_alphanumeric_or_qn(peek(l)) {
		advance(l)
	}

	return make_token(l, ident_type(l))
}

/* Consume a number. */
@(private = "file")
tok_number :: proc(l: ^Lexer) -> Token {
	// Consume digits.
	for is_digit(peek(l)) {
		advance(l)
	}

	// Consume the fractional part, if it exists.
	if peek(l) == '.' && is_digit(peek_next(l)) {
		advance(l)

		for is_digit(peek(l)) {
			advance(l)
		}
	}

	return make_token(l, .NUMBER)
}

/* Consume a string. */
@(private = "file")
tok_string :: proc(l: ^Lexer) -> Maybe(Token) {
	// Consume characters until the closing quote.
	for peek(l) != '"' && !is_at_end(l) {
		if peek(l) == '\n' {
			l.line += 1
		}
		advance(l)
	}

	if is_at_end(l) {
		syntax_error(l, "Unterminated string.")
		return nil
	}

	// Consume the closing quote.
	advance(l)
	return make_token(l, .STRING)
}

/* Lex a token. */
@(private = "file")
lex_token :: proc(l: ^Lexer) -> Maybe(Token) {
	skip_whitespace(l)
	l.start = l.current

	if is_at_end(l) {
		return make_token(l, .EOF)
	}

	c := advance(l)
	if is_alpha(c) {return tok_ident(l)}
	if is_digit(c) {return tok_number(l)}

	switch c {
	case '(':
		return make_token(l, .LPAREN)
	case ')':
		return make_token(l, .RPAREN)
	case '{':
		return make_token(l, .LSQUIRLY)
	case '}':
		return make_token(l, .RSQUIRLY)
	case '[':
		return make_token(l, .LSQUARE)
	case ']':
		return make_token(l, .RSQUARE)
	case ';':
		return make_token(l, .SEMI)
	case ',':
		return make_token(l, .COMMA)
	case '.':
		return make_token(l, .DOT)
	case '-':
		return make_token(l, .MINUS)
	case '+':
		return make_token(l, .PLUS)
	case '\\':
		return make_token(l, .BACKSLASH)
	case '/':
		return make_token(l, .SLASH)
	case '*':
		return make_token(l, .STAR)
	case '|':
		if match(l, '>') {
			return make_token(l, .BAR_GREATER)
		}
	case '!':
		if match(l, '=') {
			return make_token(l, .BANG_EQUAL)
		}
	case '=':
		if match(l, '>') {
			return make_token(l, .FAT_ARROW)
		}
		return make_token(l, match(l, '=') ? .EQUAL_EQUAL : .EQUAL)
	case '<':
		return make_token(l, match(l, '=') ? .LESS_EQUAL : .LESS)
	case '>':
		return make_token(l, match(l, '=') ? .GREATER_EQUAL : .GREATER)
	case '"':
		return tok_string(l)
	case '\n':
		/* Line increment deferred, because otherwise the newline will be 
		 * counted as being on the next line. */
		defer l.line += 1
		return make_token(l, .NEWLINE)
	}

	syntax_error(l, fmt.tprintf("Unexpected character '%c'.", previous(l)))
	return nil
}

/*
Lex the tokens. If an ILLEGAL token is found, it is returned as the error.
*/
lex :: proc(l: ^Lexer) -> (tokens: []Token, success: bool) {
	toks := make([dynamic]Token, 0, 0)

	for {
		token, ok := lex_token(l).?

		if !ok {
			delete(toks)
			return nil, false
		}

		append(&toks, token)

		if token.type == TokenType.EOF {break}
	}

	tokens = insert_semis(toks[:])

	if config.dump_tokens {
		fmt.printf("TOKENS:\n")
		for token in tokens {
			fmt.printf("  %v\n", token)
		}
	}

	return tokens, true
}
