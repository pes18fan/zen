package zen

import "core:fmt"
import "core:unicode/utf8"

/* The type of a token. */
TokenType :: enum {
    // single characters
    LPAREN, RPAREN, LSQUIRLY, RSQUIRLY,
    COMMA, DOT, MINUS, PLUS, SEMI, SLASH,
    STAR, NEWLINE,

    // one or two character tokens
    BANG_EQUAL, EQUAL, EQUAL_EQUAL,
    GREATER, GREATER_EQUAL, LESS, LESS_EQUAL,

    // literals
    IDENT, STRING, NUMBER,

    // keywords
    AND, BREAK, ELSE, FALSE, FINAL, FOR, FUN,
    IF, IMPORT, IN, LET, NIL, NOT, OR, PRIVATE,
    RETURN, TRUE, WRITE,

    ILLEGAL, EOF,
}

/* A token in the source. */
Token :: struct {
    type: TokenType,
    lexeme: string,
    line: int,
}

/* The lexer. */
Lexer :: struct {
    source: string,
    start: int,
    current: int,
    line: int,
    had_error: bool,
}

/* A union of nil and Token. Specifically called ErrorToken since it is
used to report potential lex errors. */
ErrorToken :: union { Token }

/* Create a new lexer. */
init_lexer :: proc (source: string) -> Lexer {
    return Lexer {
        source = source,
        start = 0,
        current = 0,
        line = 1,
        had_error = false,
    }
}

/* Returns true if `c` is alphanumeric, or a question mark. */
@(private="file")
is_alphanumeric_or_qn :: proc (c: rune) -> bool {
    return is_alpha(c) || is_digit(c) || c == '?'
}

/* Returns true if `c` is a letter or underscore. */
@(private="file")
is_alpha :: proc (c: rune) -> bool {
    return (c >= 'a' && c <= 'z') ||
            (c >= 'A' && c <= 'Z') ||
            c == '_'
}

/* Returns true if `c` is a digit. */
@(private="file")
is_digit :: proc (c: rune) -> bool {
    return c >= '0' && c <= '9'
}

/* Returns true if the scanner reached the end of the source. */
@(private="file")
is_at_end :: proc (l: ^Lexer) -> bool {
    return l.current >= len(l.source)
}

/* Consume the current character and return it. */
@(private="file")
advance :: proc (l: ^Lexer) -> rune #no_bounds_check {
    defer l.current += 1
    return utf8.rune_at(l.source, l.current)
}

/* Return the current character without consuming it. */
@(private="file")
peek :: proc (l: ^Lexer) -> rune #no_bounds_check {
    return utf8.rune_at(l.source, l.current)
}

/* Returns the character after the current one. */
@(private="file")
peek_next :: proc (l: ^Lexer) -> rune {
    if is_at_end(l) {
        return utf8.RUNE_EOF
    }
    return utf8.rune_at(l.source, l.current + 1)
}

/* Returns the previously consumed character. */
@(private="file")
previous :: proc (l: ^Lexer) -> rune {
    return utf8.rune_at(l.source, l.current - 1)
}

/* Consume the next character if it matches `expected`. */
@(private="file")
match :: proc (l: ^Lexer, expected: rune) -> bool {
    if is_at_end(l) do return false
    if peek(l) != expected {
        return false
    }
    defer l.current += 1

    return true
}

/* Create a token of the provided `type`. */
@(private="file")
make_token :: proc (l: ^Lexer, type: TokenType) -> Token {
    return Token {
        type = type,
        lexeme = l.source[l.start:l.current],
        line = l.line,
    }
}

/* Return an error/"illegal" token. */
@(private="file")
illegal :: proc (l: ^Lexer, message: string) -> Token {
    return Token {
        type = TokenType.ILLEGAL,
        lexeme = message,
        line = l.line,
    }
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
@(private="file")
insert_semis :: proc (tokens: []Token) -> []Token {
    defer delete(tokens)
    result := make([dynamic]Token, 0, 0)

    for i, idx in tokens {
        if i.type != .NEWLINE && i.type != .EOF || i.type == .SEMI {
            append(&result, i)
            continue
        }

        if idx == 0 do continue

        #partial switch tokens[idx - 1].type {
            case .IDENT, .NUMBER, .STRING, .NIL, .TRUE, .FALSE,
                .BREAK, .RETURN, .RPAREN: {
                if i.type != .EOF {
                    #partial switch tokens[idx + 1].type {
                        case .PLUS, .MINUS, .STAR, .SLASH, .BANG_EQUAL, .EQUAL,
                        .EQUAL_EQUAL, .GREATER, .GREATER_EQUAL, .LESS,
                        .LESS_EQUAL, .AND, .OR, .DOT: continue
                    }
                }

                append(&result, Token{
                    type = .SEMI,
                    lexeme = ";",
                    line = tokens[idx - 1].line,
                })
            }
        }

        if i.type == .EOF {
            append(&result, i)
        }
    }

    return result[:]
}

/*
Ignore any whitespace character (and comment) encountered. Newlines do not
fall in this category, th ey are handled separately, as they are used for
automatic semicolon insertion.
*/
@(private="file")
skip_whitespace :: proc (l: ^Lexer) {
    for {
        c := peek(l)

        switch c {
            case ' ':  fallthrough
            case '\r': fallthrough
            case '\t':
                advance(l)
            case '/':
                if peek_next(l) == '/' {
                    for peek(l) != '\n' && !is_at_end(l) {
                        advance(l)
                    }
                } else {
                    return
                }
            case: return
        }
    }
}

/* Test for the rest of a potential keyword's lexeme. */
@(private="file")
check_keyword :: proc (l: ^Lexer, start, length: int, rest: string, 
    type: TokenType) -> TokenType {
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
@(private="file")
ident_type :: proc (l: ^Lexer) -> TokenType {
    switch utf8.rune_at(l.source, l.start) {
        case 'a': return check_keyword(l, 1, 2, "nd", .AND)
        case 'b': return check_keyword(l, 1, 4, "reak", .BREAK)
        case 'e': return check_keyword(l, 1, 3, "lse", .ELSE)
        case 'f': {
            if l.current - l.start > 1 {
                switch utf8.rune_at(l.source, l.start + 1) {
                    case 'a': return check_keyword(l, 2, 3, "lse", .FALSE)
                    case 'i': return check_keyword(l, 2, 3, "nal", .FINAL)
                    case 'o': return check_keyword(l, 2, 1, "r", .FOR)
                    case 'u': return check_keyword(l, 2, 1, "n", .FUN)
                }
            }
        }
        case 'i': {
            if l.current - l.start > 1 {
                switch utf8.rune_at(l.source, l.start + 1) {
                    case 'f': return check_keyword(l, 2, 0, "", .IF)
                    case 'm': return check_keyword(l, 2, 4, "port", .IMPORT)
                    case 'n': return check_keyword(l, 2, 2, "", .IN)
                }
            }
        }
        case 'l': return check_keyword(l, 1, 2, "et", .LET)
        case 'n': {
            if l.current - l.start > 1 {
                switch utf8.rune_at(l.source, l.start + 1) {
                    case 'i': return check_keyword(l, 2, 1, "l", .NIL)
                    case 'o': return check_keyword(l, 2, 1, "t", .NOT)
                }
            }
        }
        case 'o': return check_keyword(l, 1, 1, "r", .OR)
        case 'p': return check_keyword(l, 1, 6, "rivate", .PRIVATE)
        case 'r': return check_keyword(l, 1, 5, "eturn", .RETURN)
        case 't': return check_keyword(l, 1, 3, "rue", .TRUE)
        case 'w': return check_keyword(l, 1, 4, "rite", .WRITE)
    }

    return .IDENT
}

/* Consume an identifier. */
@(private="file")
tok_ident :: proc (l: ^Lexer) -> Token {
    // Consume letters, underscores and question marks.
    for is_alphanumeric_or_qn(peek(l)) {
        advance(l)
    }

    return make_token(l, ident_type(l))
}

/* Consume a number. */
@(private="file")
tok_number :: proc (l: ^Lexer) -> Token {
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
@(private="file")
tok_string :: proc (l: ^Lexer) -> Token {
    // Consume characters until the closing quote.
    for peek(l) != '"' && !is_at_end(l) {
        if peek(l) == '\n' {
            l.line += 1
        }
        advance(l)
    }

    if is_at_end(l) {
        return syntax_error(l, illegal(l, "Unterminated string."))
    }

    // Consume the closing quote.
    advance(l)
    return make_token(l, .STRING)
}

/* Lex a token. */
@(private="file")
lex_token :: proc (l: ^Lexer) -> Token {
    skip_whitespace(l)
    l.start = l.current

    if is_at_end(l) {
        return make_token(l, .EOF)
    }

    c := advance(l)
    if is_alpha(c) do return tok_ident(l)
    if is_digit(c) do return tok_number(l)

    switch c {
        case '(':  return make_token(l, .LPAREN)
        case ')':  return make_token(l, .RPAREN)
        case '{':  return make_token(l, .LSQUIRLY)
        case '}':  return make_token(l, .RSQUIRLY)
        case ';':  return make_token(l, .SEMI)
        case ',':  return make_token(l, .COMMA)
        case '.':  return make_token(l, .DOT)
        case '-':  return make_token(l, .MINUS)
        case '+':  return make_token(l, .PLUS)
        case '/':  return make_token(l, .SLASH)
        case '*':  return make_token(l, .STAR)
        case '\n': {
            l.line += 1
            return make_token(l, .NEWLINE)
        }
        case '!':
            if match(l, '=') {
                return make_token(l, .BANG_EQUAL)
            }
        case '=':
            return make_token(l,
                match(l, '=') ? .EQUAL_EQUAL : .EQUAL)
        case '<':
            return make_token(l,
                match(l, '=') ? .LESS_EQUAL : .LESS)
        case '>':
            return make_token(l,
                match(l, '=') ? .GREATER_EQUAL : .GREATER)
        case '"':
            return tok_string(l)
    }

    return syntax_error(l, 
        illegal(l, "Unexpected character."))
}

/* 
Reports a syntax error. Assumes that `token` is an illegal token since only
illegal tokens are returned on syntax errors. 
*/
@(private="file")
syntax_error :: proc (l: ^Lexer, token: Token) -> Token {
    fmt.printf("[line %d] Syntax error", token.line)
    fmt.printf(": %s\n", token.lexeme)
    l.had_error = true
    return token
}

/*
Lex the tokens. If an ILLEGAL token is found, it is returned as the error.
*/
lex :: proc (l: ^Lexer) -> (tokens: []Token, err: ErrorToken) {
    toks := make([dynamic]Token, 0, 0)

    for {
        token := lex_token(l)

        if l.had_error {
            delete(toks)
            return nil, token
        }

        append(&toks, token)

        if token.type == TokenType.EOF {
            break
        }
    }

    tokens = insert_semis(toks[:])

    return tokens, nil
}
