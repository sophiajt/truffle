use crate::errors::{ErrorBatch, ScriptError};

pub struct Lexer {
    source: Vec<u8>,
    span_offset: usize,
    pub errors: ErrorBatch,
}

#[derive(Debug, Clone, Copy)]
pub enum TokenType {
    Number,
    Comma,
    String,
    Dot,
    DotDot,
    Name,
    Pipe,
    PipePipe,
    Colon,
    Semicolon,
    Plus,
    PlusPlus,
    Dash,
    Exclamation,
    Asterisk,
    AsteriskAsterisk,
    ForwardSlash,
    ForwardSlashForwardSlash,
    Equals,
    EqualsEquals,
    EqualsTilde,
    ExclamationTilde,
    ExclamationEquals,
    LParen,
    LSquare,
    LCurly,
    LessThan,
    LessThanEqual,
    RParen,
    RSquare,
    RCurly,
    GreaterThan,
    GreaterThanEqual,
    Ampersand,
    AmpersandAmpersand,

    // Unknown token
    Garbage,
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub token_type: TokenType,
    pub span_start: usize,
    pub span_end: usize,
}

fn is_symbol(b: u8) -> bool {
    [
        b'+', b'-', b'*', b'/', b'.', b',', b'(', b'[', b'{', b'<', b')', b']', b'}', b'>', b':',
        b';', b'=', b'$', b'|', b'!', b'~', b'&', b'\'', b'"',
    ]
    .contains(&b)
}

impl Lexer {
    pub fn new(source: Vec<u8>, span_offset: usize) -> Self {
        Self {
            source,
            span_offset,
            errors: ErrorBatch::empty(),
        }
    }

    pub fn error(&mut self, message: impl Into<String>, span_start: usize, span_end: usize) {
        self.errors.push(ScriptError {
            message: message.into(),
            span_start,
            span_end,
        })
    }

    pub fn lex_quoted_string(&mut self) -> Option<Token> {
        let span_start = self.span_offset;
        let mut current_position = self.span_offset + 1;
        let mut is_escaped = false;
        while current_position < self.source.len() {
            if is_escaped {
                is_escaped = false;
            } else if self.source[current_position] == b'\\' {
                is_escaped = true;
            } else if self.source[current_position] == b'"' {
                current_position += 1;
                break;
            }
            current_position += 1;
        }

        self.span_offset = current_position;

        Some(Token {
            token_type: TokenType::String,
            span_start,
            span_end: self.span_offset,
        })
    }

    pub fn lex_number(&mut self) -> Option<Token> {
        let span_start = self.span_offset;
        let mut current_position = self.span_offset;
        while current_position < self.source.len() {
            if !self.source[current_position].is_ascii_digit() {
                break;
            }
            current_position += 1;
        }

        // Check to see if we have a hex/octal/binary number
        if current_position < self.source.len() && self.source[current_position] == b'x' {
            current_position += 1;
            while current_position < self.source.len() {
                if !self.source[current_position].is_ascii_hexdigit() {
                    break;
                }
                current_position += 1;
            }
        } else if current_position < self.source.len() && self.source[current_position] == b'o' {
            current_position += 1;
            while current_position < self.source.len() {
                if !(self.source[current_position] >= b'0' && self.source[current_position] <= b'7')
                {
                    break;
                }
                current_position += 1;
            }
        } else if current_position < self.source.len() && self.source[current_position] == b'b' {
            current_position += 1;
            while current_position < self.source.len() {
                if !(self.source[current_position] >= b'0' && self.source[current_position] <= b'1')
                {
                    break;
                }
                current_position += 1;
            }
        } else if current_position < self.source.len()
            && self.source[current_position] == b'.'
            && (current_position + 1 < self.source.len())
            && self.source[current_position + 1].is_ascii_digit()
        {
            // Looks like a float
            current_position += 1;
            while current_position < self.source.len() {
                if !self.source[current_position].is_ascii_digit() {
                    break;
                }
                current_position += 1;
            }

            if current_position < self.source.len()
                && (self.source[current_position] == b'e' || self.source[current_position] == b'E')
            {
                current_position += 1;

                if current_position < self.source.len() && self.source[current_position] == b'-' {
                    current_position += 1;
                }

                while current_position < self.source.len() {
                    if !self.source[current_position].is_ascii_digit() {
                        break;
                    }
                    current_position += 1;
                }
            }
        }

        self.span_offset = current_position;

        Some(Token {
            token_type: TokenType::Number,
            span_start,
            span_end: self.span_offset,
        })
    }

    pub fn skip_space(&mut self) {
        let mut current_position = self.span_offset;
        let whitespace: &[u8] = &[b' ', b'\t', b'\r', b'\n'];
        while current_position < self.source.len() {
            if !whitespace.contains(&self.source[current_position]) {
                break;
            }
            current_position += 1;
        }
        self.span_offset = current_position;
    }

    pub fn skip_comment(&mut self) {
        let mut current_position = self.span_offset;
        while current_position < self.source.len() {
            if self.source[current_position] == b'\n' {
                current_position += 1;
                break;
            } else {
                current_position += 1;
            }
        }
        self.span_offset = current_position;
    }

    pub fn lex_name(&mut self) -> Option<Token> {
        let span_start = self.span_offset;
        let mut current_position = self.span_offset;
        while current_position < self.source.len()
            && (self.source[current_position].is_ascii_alphanumeric()
                || self.source[current_position] == b'_')
        {
            current_position += 1;
        }
        self.span_offset = current_position;

        Some(Token {
            token_type: TokenType::Name,
            span_start,
            span_end: self.span_offset,
        })
    }

    pub fn lex_symbol(&mut self) -> Option<Token> {
        let span_start = self.span_offset;

        let result = match self.source[self.span_offset] {
            b'(' => Token {
                token_type: TokenType::LParen,
                span_start,
                span_end: span_start + 1,
            },
            b'[' => Token {
                token_type: TokenType::LSquare,
                span_start,
                span_end: span_start + 1,
            },
            b'{' => Token {
                token_type: TokenType::LCurly,
                span_start,
                span_end: span_start + 1,
            },
            b'<' => {
                if self.source.len() > self.span_offset + 1
                    && self.source[self.span_offset + 1] == b'='
                {
                    Token {
                        token_type: TokenType::LessThanEqual,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::LessThan,
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b')' => Token {
                token_type: TokenType::RParen,
                span_start,
                span_end: span_start + 1,
            },
            b']' => Token {
                token_type: TokenType::RSquare,
                span_start,
                span_end: span_start + 1,
            },
            b'}' => Token {
                token_type: TokenType::RCurly,
                span_start,
                span_end: span_start + 1,
            },
            b'>' => {
                if self.source.len() > self.span_offset + 1
                    && self.source[self.span_offset + 1] == b'='
                {
                    Token {
                        token_type: TokenType::GreaterThanEqual,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::GreaterThan,
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b'+' => {
                if self.source.len() > self.span_offset + 1
                    && self.source[self.span_offset + 1] == b'+'
                {
                    Token {
                        token_type: TokenType::PlusPlus,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Plus,
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b'-' => Token {
                token_type: TokenType::Dash,
                span_start,
                span_end: span_start + 1,
            },
            b'*' => {
                if self.source.len() > self.span_offset + 1
                    && self.source[self.span_offset + 1] == b'*'
                {
                    Token {
                        token_type: TokenType::AsteriskAsterisk,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Asterisk,
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b'/' => {
                if self.source.len() > self.span_offset + 1
                    && self.source[self.span_offset + 1] == b'/'
                {
                    Token {
                        token_type: TokenType::ForwardSlashForwardSlash,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::ForwardSlash,
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b'=' => {
                if self.source.len() > self.span_offset + 1
                    && self.source[self.span_offset + 1] == b'='
                {
                    Token {
                        token_type: TokenType::EqualsEquals,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else if self.source.len() > self.span_offset + 1
                    && self.source[self.span_offset + 1] == b'~'
                {
                    Token {
                        token_type: TokenType::EqualsTilde,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Equals,
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b':' => Token {
                token_type: TokenType::Colon,
                span_start,
                span_end: span_start + 1,
            },
            b';' => Token {
                token_type: TokenType::Semicolon,
                span_start,
                span_end: span_start + 1,
            },
            b'.' => {
                if self.source.len() > self.span_offset + 1
                    && self.source[self.span_offset + 1] == b'.'
                {
                    Token {
                        token_type: TokenType::DotDot,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Dot,
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b'!' => {
                if self.source.len() > self.span_offset + 1
                    && self.source[self.span_offset + 1] == b'='
                {
                    Token {
                        token_type: TokenType::ExclamationEquals,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else if self.source.len() > self.span_offset + 1
                    && self.source[self.span_offset + 1] == b'~'
                {
                    Token {
                        token_type: TokenType::ExclamationTilde,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Exclamation,
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b'|' => {
                if self.source.len() > self.span_offset + 1
                    && self.source[self.span_offset + 1] == b'|'
                {
                    Token {
                        token_type: TokenType::PipePipe,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Pipe,
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b'&' => {
                if self.source.len() > self.span_offset + 1
                    && self.source[self.span_offset + 1] == b'&'
                {
                    Token {
                        token_type: TokenType::AmpersandAmpersand,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Ampersand,
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b',' => Token {
                token_type: TokenType::Comma,
                span_start,
                span_end: span_start + 1,
            },
            x => {
                self.error(
                    format!(
                        "Internal compiler error: symbol character mismatched in lexer: {}",
                        x as char
                    ),
                    span_start,
                    span_start + 1,
                );
                Token {
                    token_type: TokenType::Garbage,
                    span_start,
                    span_end: span_start + 1,
                }
            }
        };

        self.span_offset = result.span_end;
        Some(result)
    }
}

impl Lexer {
    pub fn peek_token(&mut self) -> Option<Token> {
        let prev_offset = self.span_offset;
        let output = self.next_token();
        self.span_offset = prev_offset;

        output
    }

    /// Extract and return the next token from the source
    pub fn next_token(&mut self) -> Option<Token> {
        loop {
            if self.span_offset >= self.source.len() {
                return None;
            } else if self.source[self.span_offset].is_ascii_digit() {
                return self.lex_number();
            } else if self.source[self.span_offset] == b'"' {
                return self.lex_quoted_string();
            } else if self.source[self.span_offset] == b'/'
                && self.source.len() > self.span_offset + 1
                && self.source[self.span_offset + 1] == b'/'
            {
                // Comment
                self.skip_comment();
            } else if is_symbol(self.source[self.span_offset]) {
                return self.lex_symbol();
            } else if self.source[self.span_offset] == b' '
                || self.source[self.span_offset] == b'\t'
                || self.source[self.span_offset] == b'\r'
                || self.source[self.span_offset] == b'\n'
            {
                self.skip_space()
            } else if self.source[self.span_offset].is_ascii_alphanumeric()
                || self.source[self.span_offset] == b'_'
            {
                return self.lex_name();
            } else {
                let span_start = self.span_offset;
                let span_end = self.span_offset + 1;
                self.error(
                    format!(
                        "unsupported character: {}",
                        self.source[self.span_offset] as char
                    ),
                    span_start,
                    span_end,
                );
                self.span_offset += 1;
                return Some(Token {
                    token_type: TokenType::Garbage,
                    span_start,
                    span_end,
                });
            }
        }
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, ErrorBatch> {
        let mut output = vec![];

        while let Some(token) = self.next_token() {
            output.push(token)
        }

        if !self.errors.is_empty() {
            Err(self.errors.clone())
        } else {
            Ok(output)
        }
    }
}
