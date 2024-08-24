const std = @import("std");

pub const TokenType = enum {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // Literals.
    IDENTIFIER,
    STRING,
    NUMBER,

    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FOR,
    FUN,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,
    ERROR,
    EOF,

    pub fn string(self: TokenType) []const u8 {
        return switch (self) {
            TokenType.LEFT_PAREN => "LEFT_PAREN",
            TokenType.RIGHT_PAREN => "RIGHT_PAREN",
            TokenType.LEFT_BRACE => "LEFT_BRACE",
            TokenType.RIGHT_BRACE => "RIGHT_BRACE",
            TokenType.COMMA => "COMMA",
            TokenType.DOT => "DOT",
            TokenType.MINUS => "MINUS",
            TokenType.PLUS => "PLUS",
            TokenType.SEMICOLON => "SEMICOLON",
            TokenType.SLASH => "SLASH",
            TokenType.STAR => "STAR",
            TokenType.BANG => "BANG",
            TokenType.BANG_EQUAL => "BANG_EQUAL",
            TokenType.EQUAL => "EQUAL",
            TokenType.EQUAL_EQUAL => "EQUAL_EQUAL",
            TokenType.GREATER => "GREATER",
            TokenType.GREATER_EQUAL => "GREATER_EQUAL",
            TokenType.LESS => "LESS",
            TokenType.LESS_EQUAL => "LESS_EQUAL",
            TokenType.IDENTIFIER => "IDENTIFIER",
            TokenType.STRING => "STRING",
            TokenType.NUMBER => "NUMBER",
            TokenType.AND => "AND",
            TokenType.CLASS => "CLASS",
            TokenType.ELSE => "ELSE",
            TokenType.FALSE => "FALSE",
            TokenType.FOR => "FOR",
            TokenType.FUN => "FUN",
            TokenType.IF => "IF",
            TokenType.NIL => "NIL",
            TokenType.OR => "OR",
            TokenType.PRINT => "PRINT",
            TokenType.RETURN => "RETURN",
            TokenType.SUPER => "SUPER",
            TokenType.THIS => "THIS",
            TokenType.TRUE => "TRUE",
            TokenType.VAR => "VAR",
            TokenType.WHILE => "WHILE",
            TokenType.ERROR => "ERROR",
            TokenType.EOF => "EOF",
        };
    }
};

pub const Token = struct {
    token_type: TokenType,
    start: []const u8,
    length: usize,
    line: usize,
};

pub const Scanner = struct {
    source: []const u8,
    start: usize,
    current: usize,
    line: usize,

    pub fn init(content: []const u8) Scanner {
        return Scanner{
            .start = 0,
            .current = 0,
            .line = 1,
            .source = content,
        };
    }

    pub fn scan_token(self: *Scanner) Token {
        self.skip_whitespace();

        self.start = self.current;

        if (self.is_at_end()) {
            return self.make_token(TokenType.EOF);
        }

        const c = self.advance();
        if (self.is_alpha(c)) {
            return self.identifier();
        }

        if (self.is_digit(c)) {
            return self.number();
        }

        return switch (c) {
            '(' => self.make_token(TokenType.LEFT_PAREN),
            ')' => self.make_token(TokenType.RIGHT_PAREN),
            '{' => self.make_token(TokenType.LEFT_BRACE),
            '}' => self.make_token(TokenType.RIGHT_BRACE),
            ';' => self.make_token(TokenType.SEMICOLON),
            ',' => self.make_token(TokenType.COMMA),
            '.' => self.make_token(TokenType.DOT),
            '-' => self.make_token(TokenType.MINUS),
            '+' => self.make_token(TokenType.PLUS),
            '/' => self.make_token(TokenType.SLASH),
            '*' => self.make_token(TokenType.STAR),
            '!' => {
                if (self.match('=')) {
                    return self.make_token(TokenType.BANG_EQUAL);
                } else {
                    return self.make_token(TokenType.BANG);
                }
            },
            '=' => {
                if (self.match('=')) {
                    return self.make_token(TokenType.EQUAL_EQUAL);
                } else {
                    return self.make_token(TokenType.EQUAL);
                }
            },
            '<' => {
                if (self.match('=')) {
                    return self.make_token(TokenType.LESS_EQUAL);
                } else {
                    return self.make_token(TokenType.LESS);
                }
            },
            '>' => {
                if (self.match('=')) {
                    return self.make_token(TokenType.GREATER_EQUAL);
                } else {
                    return self.make_token(TokenType.GREATER);
                }
            },
            '"' => return self.string(),
            else => self.error_token("Unexpected character."),
        };
    }

    pub fn is_at_end(self: Scanner) bool {
        return self.source.len == self.current;
    }

    pub fn make_token(self: Scanner, token_type: TokenType) Token {
        return Token{
            .token_type = token_type,
            .start = self.source[self.start..],
            .length = self.current - self.start,
            .line = self.line,
        };
    }

    pub fn error_token(self: Scanner, error_message: []const u8) Token {
        return Token{
            .token_type = TokenType.EOF,
            .start = error_message,
            .length = error_message.len,
            .line = self.line,
        };
    }

    pub fn advance(self: *Scanner) u8 {
        self.current += 1;
        return self.source[self.current - 1];
    }

    pub fn match(self: *Scanner, expected: u8) bool {
        if (self.is_at_end()) {
            return false;
        }

        if (self.source[self.current] != expected) {
            return false;
        }

        self.current += 1;
        return true;
    }

    pub fn skip_whitespace(self: *Scanner) void {
        while (!self.is_at_end()) {
            const c = self.peek();
            switch (c) {
                ' ', '\r', '\t' => {
                    _ = self.advance();
                },
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                '/' => {
                    if (self.peek_next() == '/') {
                        while (self.peek() != '\n' and !self.is_at_end()) {
                            _ = self.advance();
                        }
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }

    pub fn peek(self: *Scanner) u8 {
        if (self.is_at_end()) {
            return 0;
        }

        return self.source[self.current];
    }

    pub fn peek_next(self: *Scanner) u8 {
        if (self.is_at_end()) {
            return 0;
        }

        return self.source[self.current + 1];
    }

    pub fn string(self: *Scanner) Token {
        while (self.peek() != '"' and !self.is_at_end()) {
            if (self.peek() == '\n') {
                self.line += 1;
            }
            _ = self.advance();
        }

        if (self.is_at_end()) {
            return self.error_token("Unterminated string.");
        }

        _ = self.advance();

        return self.make_token(TokenType.STRING);
    }

    pub fn is_digit(self: Scanner, c: u8) bool {
        _ = self;
        return c >= '0' and c <= '9';
    }

    pub fn number(self: *Scanner) Token {
        while (self.is_digit(self.peek())) {
            _ = self.advance();
        }

        if (self.peek() == '.' and self.is_digit(self.peek_next())) {
            // consume the '.'
            _ = self.advance();
        }

        while (self.is_digit(self.peek())) {
            _ = self.advance();
        }

        return self.make_token(TokenType.NUMBER);
    }

    pub fn is_alpha(self: Scanner, c: u8) bool {
        _ = self;
        return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
    }

    pub fn identifier(self: *Scanner) Token {
        while (self.is_alpha(self.peek()) or self.is_digit(self.peek())) {
            _ = self.advance();
        }

        return self.make_token(self.identifier_type());
    }

    pub fn identifier_type(self: *Scanner) TokenType {
        return switch (self.source[self.start]) {
            'a' => self.check_keyword(1, 2, "nd", TokenType.AND),
            'c' => self.check_keyword(1, 4, "class", TokenType.CLASS),
            'e' => self.check_keyword(1, 3, "lse", TokenType.ELSE),
            'f' => if (self.current - self.start > 1) {
                return switch (self.source[self.start + 1]) {
                    'a' => self.check_keyword(2, 3, "lse", TokenType.FALSE),
                    'o' => self.check_keyword(2, 1, "r", TokenType.FOR),
                    'u' => self.check_keyword(2, 1, "n", TokenType.FUN),
                    else => TokenType.IDENTIFIER,
                };
            } else {
                return TokenType.IDENTIFIER;
            },
            'i' => self.check_keyword(1, 1, "f", TokenType.IF),
            'n' => self.check_keyword(1, 2, "il", TokenType.NIL),
            'o' => self.check_keyword(1, 1, "or", TokenType.OR),
            'p' => self.check_keyword(1, 4, "rint", TokenType.PRINT),
            'r' => self.check_keyword(1, 5, "eturn", TokenType.RETURN),
            's' => self.check_keyword(1, 4, "uper", TokenType.SUPER),
            't' => if (self.current - self.start > 1) {
                return switch (self.source[self.start + 1]) {
                    'h' => self.check_keyword(2, 2, "is", TokenType.THIS),
                    'r' => self.check_keyword(2, 2, "ue", TokenType.TRUE),
                    else => TokenType.IDENTIFIER,
                };
            } else {
                return TokenType.IDENTIFIER;
            },
            'v' => self.check_keyword(1, 2, "ar", TokenType.VAR),
            'w' => self.check_keyword(1, 4, "hile", TokenType.WHILE),
            else => TokenType.IDENTIFIER,
        };
    }

    pub fn check_keyword(self: *Scanner, start: usize, length: usize, rest: []const u8, token_type: TokenType) TokenType {
        if (self.current - self.start == start + length and std.mem.eql(u8, rest, self.source[self.start + start .. self.start + start + length])) {
            return token_type;
        }

        return TokenType.IDENTIFIER;
    }
};
