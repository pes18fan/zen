package zen

import "core:fmt"
import "core:path/filepath"
import "core:strings"
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
	COMMA,
	COLON,
	DOT,
	MINUS,
	PLUS,
	SEMI,
	SLASH,
	STAR,
	PERCENT,

	// one or two character tokens
	BANG_EQUAL,
	BAR_GREATER, // |>
	DOT_DOT,
	EQUAL,
	EQUAL_EQUAL,
	ARROW,
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
	CATCH,
	CONTINUE,
	ELSE,
	EXIT,
	FALSE,
	FOR,
	FUNC,
	IF,
	IFNT,
	IN,
	IT,
	NIL,
	NOT,
	OR,
	ORELSE,
	PRINT,
	PUB,
	RETURN,
	SWITCH,
	TRUE,
	TRY,
	USE,
	WHILE,
	WHILENT,
	VAR,
	VAL,
	EOF,
}

/* A token in the source. */
Token :: struct {
	type:     TokenType,
	lexeme:   string,
	source:   string,
	position: Pos,
}

token_line :: proc(token: Token) -> string {
	lines := strings.split_lines(token.source)
	defer delete(lines)
	return lines[token.position.line - 1]
}

Pos :: struct #all_or_none {
	line:   int,
	column: int,
}

/* The lexer. */
Lexer :: struct #all_or_none {
	source:         string,
	start:          int,
	current:        int,
	start_position: Pos,
	position:       Pos,
}

/* 
Reports a syntax error. Assumes that `token` is an illegal token since only
illegal tokens are returned on syntax errors. 
*/
@(private = "file")
syntax_error :: proc(l: ^Lexer, message: string) {
	fmt.eprint(color_red("error"))
	fmt.eprintfln(": %s", style_bold(message))
	fmt.eprintfln("  at line %d, column %d", l.position.line, l.position.column)
	fmt.eprintfln("  in %s", "REPL" if config.repl else filepath.base(config.__path))
}

/* Returns true if `c` is alphanumeric, a question mark or an exclamation mark. */
@(private = "file")
is_alphanumeric_or_qn_or_ex :: proc(c: rune) -> bool {
	return is_alpha(c) || is_digit(c) || c == '?' || c == '!'
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
	defer l.position.column += 1
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
	defer l.position.column += 1

	return true
}

/* Create a token of the provided `type`. */
@(private = "file")
make_token :: proc(l: ^Lexer, type: TokenType) -> Token {
	return Token {
		type = type,
		lexeme = l.source[l.start:l.current],
		position = l.start_position,
		source = l.source,
	}
}

/*
Ignore any whitespace character (and comment) encountered. Newlines do not
fall in this category, they are handled separately, as they are used for
automatic semicolon insertion.
*/
@(private = "file")
@(optimization_mode = "favor_size")
skip_whitespace :: proc(l: ^Lexer) {
	for {
		c := peek(l)

		switch c {
		case '\t', '\v', '\f', '\r', ' ':
			advance(l)
		case '\n':
			advance(l)
			l.position.line += 1
			l.position.column = 1
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
				case 'a':
					return check_keyword(l, 2, 3, "tch", .CATCH)
				}
			}
		}
	case 'e':
		{
			if l.current - l.start > 1 {
				switch utf8.rune_at(l.source, l.start + 1) {
				case 'x':
					return check_keyword(l, 2, 2, "it", .EXIT)
				case 'l':
					return check_keyword(l, 2, 2, "se", .ELSE)
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
					if check_keyword(l, 2, 0, "", .IF) == .IF {
						return .IF
					} else {
						return check_keyword(l, 2, 3, "n't", .IFNT)
					}
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
		if l.current - l.start > 1 {
			switch utf8.rune_at(l.source, l.start + 1) {
			case 'r':
				kw := check_keyword(l, 2, 0, "", .OR)
				if kw == .OR {return kw}
				return check_keyword(l, 2, 4, "else", .ORELSE)
			}
		}
	case 'p':
		{
			if l.current - l.start > 1 {
				switch utf8.rune_at(l.source, l.start + 1) {
				case 'r':
					return check_keyword(l, 2, 3, "int", .PRINT)
				case 'u':
					return check_keyword(l, 2, 1, "b", .PUB)
				}
			}
		}
	case 'r':
		return check_keyword(l, 1, 5, "eturn", .RETURN)
	case 's':
		return check_keyword(l, 1, 5, "witch", .SWITCH)
	case 't':
		{
			if l.current - l.start > 1 {
				switch utf8.rune_at(l.source, l.start + 1) {
				case 'r':
					if l.current - l.start > 2 {
						switch utf8.rune_at(l.source, l.start + 2) {
						case 'u':
							return check_keyword(l, 3, 1, "e", .TRUE)
						case 'y':
							return check_keyword(l, 3, 0, "", .TRY)
						}
					}
				}
			}
		}
	case 'u':
		return check_keyword(l, 1, 2, "se", .USE)
	case 'w':
		if check_keyword(l, 1, 4, "hile", .WHILE) == .WHILE {
			return .WHILE
		} else {
			return check_keyword(l, 1, 7, "hilen't", .WHILENT)
		}
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

when CHAOTIC {
	/* Consume letters, underscores, question marks, exclamation marks and 
       apostrophes. For the ifn't and whilen't tokens. */
	@(private = "file")
	is_alphanumeric_or_qn_or_ex_or_apostrophe :: proc(c: rune) -> bool {
		return is_alpha(c) || is_digit(c) || c == '?' || c == '!' || c == '\''
	}
}

/* Consume an identifier. */
@(private = "file")
tok_ident :: proc(l: ^Lexer) -> Token {
	when CHAOTIC {
		// Consume letters, underscores, question marks and apostrophes.
		for is_alphanumeric_or_qn_or_ex_or_apostrophe(peek(l)) {
			advance(l)
		}
	} else {
		// Consume letters, underscores and question marks.
		for is_alphanumeric_or_qn_or_ex(peek(l)) {
			advance(l)
		}
	}

	return make_token(l, ident_type(l))
}

/* Consume a number. */
@(private = "file")
tok_number :: proc(l: ^Lexer) -> Maybe(Token) {
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

	// Consume the exponential part, if it exists.
	// `xey` means `x` times 10 to the power of `y`
	if peek(l) == 'e' {
		advance(l)

		if !is_digit(peek(l)) {
			syntax_error(l, "Invalid number.")
			return nil
		}
	}

	for is_digit(peek(l)) {
		advance(l)
	}

	return make_token(l, .NUMBER)
}

/* Consume a string. */
@(private = "file")
tok_string :: proc(l: ^Lexer, starts_with: rune) -> Maybe(Token) {
	// Consume characters until the closing quote.
	for peek(l) != starts_with && !is_at_end(l) {
		if peek(l) == '\n' {
			l.position.line += 1
			l.position.column = 1
		}

		// don't end at escaped quotes
		if peek(l) == '\\' && peek_next(l) == starts_with {
			advance(l)
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
	l.start_position = l.position

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
	case ':':
		return make_token(l, .COLON)
	case ';':
		return make_token(l, .SEMI)
	case ',':
		return make_token(l, .COMMA)
	case '.':
		return make_token(l, match(l, '.') ? .DOT_DOT : .DOT)
	case '-':
		return make_token(l, match(l, '>') ? .ARROW : .MINUS)
	case '+':
		return make_token(l, .PLUS)
	case '/':
		return make_token(l, .SLASH)
	case '%':
		return make_token(l, .PERCENT)
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
		return tok_string(l, starts_with = '"')
	case '\'':
		return tok_string(l, starts_with = '\'')
	}

	syntax_error(l, fmt.tprintf("Unexpected character '%c'.", previous(l)))
	return nil
}

/*
Lex the tokens. If an error occurs, `success` is false.
*/
lex :: proc(source: string) -> ([]Token, bool) {
	l := Lexer {
		source         = source,
		start          = 0,
		current        = 0,
		start_position = {1, 1},
		position       = {1, 1},
	}

	tokens := make([dynamic]Token)

	for {
		token, ok := lex_token(&l).?
		if !ok {
			delete(tokens)
			return nil, false
		}

		append(&tokens, token)

		if token.type == TokenType.EOF {break}
	}

	return tokens[:], true
}
