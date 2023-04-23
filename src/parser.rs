use crate::errors::ScriptError;
use crate::lexer::{Lexer, Token, TokenType};
use crate::parser_delta::ParserDelta;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    pub delta: ParserDelta,
    pub errors: Vec<ScriptError>,
    content_length: usize,
}

#[derive(Debug)]
pub enum NodeType {
    Int,
    Float,
    String,
    Name,
    Variable,

    // Command-specific
    Flag,
    NamedArg,

    // Booleans
    True,
    False,

    // Operators
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    RegexMatch,
    NotRegexMatch,
    Plus,
    Append,
    Minus,
    Multiply,
    Divide,
    In,
    NotIn,
    Modulo,
    FloorDivision,
    And,
    Or,
    Pow,
    BitOr,
    BitXor,
    BitAnd,
    ShiftLeft,
    ShiftRight,
    StartsWith,
    EndsWith,
    Interpolation,

    // Statements
    Let {
        variable_name: NodeId,
        initializer: NodeId,
    },
    LetMut {
        variable_name: NodeId,
        initializer: NodeId,
    },

    // Definitions
    Fn {
        name: NodeId,
        params: NodeId,
        block: NodeId,
    },
    Params(Vec<NodeId>),
    Param {
        name: NodeId,
        ty: Option<NodeId>,
    },

    Closure {
        params: NodeId,
        block: NodeId,
    },

    // Expressions
    Call {
        head: NodeId,
        args: Vec<NodeId>,
    },
    BinaryOp {
        lhs: NodeId,
        op: NodeId,
        rhs: NodeId,
    },
    Range {
        lhs: NodeId,
        rhs: NodeId,
    },
    Block(Vec<NodeId>),
    If {
        condition: NodeId,
        then_block: NodeId,
        else_expression: Option<NodeId>,
    },
    Garbage,
}

impl NodeType {
    pub fn precedence(&self) -> usize {
        match self {
            NodeType::Pow => 100,
            NodeType::Multiply | NodeType::Divide | NodeType::Modulo | NodeType::FloorDivision => {
                95
            }
            NodeType::Plus | NodeType::Minus => 90,
            NodeType::ShiftLeft | NodeType::ShiftRight => 85,
            NodeType::NotRegexMatch
            | NodeType::RegexMatch
            | NodeType::StartsWith
            | NodeType::EndsWith
            | NodeType::LessThan
            | NodeType::LessThanOrEqual
            | NodeType::GreaterThan
            | NodeType::GreaterThanOrEqual
            | NodeType::Equal
            | NodeType::NotEqual
            | NodeType::In
            | NodeType::NotIn
            | NodeType::Append => 80,
            NodeType::BitAnd => 75,
            NodeType::BitXor => 70,
            NodeType::BitOr => 60,
            NodeType::And => 50,
            NodeType::Or => 40,
            _ => 0,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct NodeId(pub usize);

impl<'a> Parser<'a> {
    pub fn new(source: &'a [u8], span_offset: usize, node_id_offset: usize) -> Self {
        let content_length = source.len();

        Self {
            lexer: Lexer::new(source, span_offset),
            delta: ParserDelta::new(node_id_offset),
            errors: vec![],
            content_length,
        }
    }

    fn position(&mut self) -> usize {
        if let Some(Token { span_start, .. }) = self.lexer.peek() {
            span_start
        } else {
            self.content_length
        }
    }

    pub fn parse(&mut self) {
        self.program();
    }

    pub fn program(&mut self) -> NodeId {
        self.code_block(false)
    }

    pub fn has_tokens(&mut self) -> bool {
        self.lexer.peek().is_some()
    }

    pub fn is_operator(&mut self) -> bool {
        match self.lexer.peek() {
            Some(Token {
                token_type,
                contents,
                ..
            }) => match token_type {
                TokenType::Asterisk
                | TokenType::AsteriskAsterisk
                | TokenType::Dash
                | TokenType::EqualsEquals
                | TokenType::EqualsTilde
                | TokenType::ExclamationEquals
                | TokenType::ExclamationTilde
                | TokenType::ForwardSlash
                | TokenType::LessThan
                | TokenType::LessThanEqual
                | TokenType::Plus
                | TokenType::PlusPlus
                | TokenType::GreaterThan
                | TokenType::GreaterThanEqual
                | TokenType::AmpersandAmpersand
                | TokenType::PipePipe => true,

                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_comma(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::Comma,
                ..
            })
        )
    }

    pub fn is_lcurly(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::LCurly,
                ..
            })
        )
    }

    pub fn is_rcurly(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::RCurly,
                ..
            })
        )
    }

    pub fn is_lparen(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::LParen,
                ..
            })
        )
    }

    pub fn is_rparen(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::RParen,
                ..
            })
        )
    }

    pub fn is_lsquare(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::LSquare,
                ..
            })
        )
    }

    pub fn is_rsquare(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::RSquare,
                ..
            })
        )
    }

    pub fn is_less_than(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::LessThan,
                ..
            })
        )
    }

    pub fn is_greater_than(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::GreaterThan,
                ..
            })
        )
    }

    pub fn is_pipe(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::Pipe,
                ..
            })
        )
    }

    pub fn is_double_pipe(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::PipePipe,
                ..
            })
        )
    }

    pub fn is_double_ampersand(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::AmpersandAmpersand,
                ..
            })
        )
    }

    pub fn is_dash(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::Dash,
                ..
            })
        )
    }

    pub fn is_colon(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::Colon,
                ..
            })
        )
    }

    pub fn is_semicolon(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::Semicolon,
                ..
            })
        )
    }

    pub fn is_dot(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::Dot,
                ..
            })
        )
    }

    pub fn is_dotdot(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::DotDot,
                ..
            })
        )
    }

    pub fn is_comment(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::Comment,
                ..
            })
        )
    }

    pub fn is_number(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::Number,
                ..
            })
        )
    }

    pub fn is_string(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::String,
                ..
            })
        )
    }

    pub fn is_keyword(&mut self, keyword: &[u8]) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::Name,
                contents,
                ..
            }) if contents == keyword
        )
    }

    pub fn is_name(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::Name,
                ..
            })
        )
    }

    pub fn is_expression(&mut self) -> bool {
        self.is_simple_expression() || self.is_keyword(b"if") || self.is_keyword(b"where")
    }

    pub fn is_simple_expression(&mut self) -> bool {
        match self.lexer.peek() {
            Some(Token {
                token_type: TokenType::Number,
                ..
            })
            | Some(Token {
                token_type: TokenType::String,
                ..
            })
            | Some(Token {
                token_type: TokenType::LCurly,
                ..
            })
            | Some(Token {
                token_type: TokenType::LSquare,
                ..
            })
            | Some(Token {
                token_type: TokenType::LParen,
                ..
            }) => true,
            Some(Token {
                token_type: TokenType::Name,
                contents,
                ..
            }) if contents == b"true" => true,
            Some(Token {
                token_type: TokenType::Name,
                contents,
                ..
            }) if contents == b"false" => true,
            Some(Token {
                token_type: TokenType::Name,
                ..
            }) => true,
            _ => false,
        }
    }

    pub fn error(&mut self, message: impl Into<String>) -> NodeId {
        if let Some(Token {
            span_start,
            span_end,
            ..
        }) = self.lexer.next()
        {
            let node_id = self.create_node(NodeType::Garbage, span_start, span_end);
            self.errors.push(ScriptError {
                message: message.into(),

                node_id,
            });

            node_id
        } else {
            let node_id =
                self.create_node(NodeType::Garbage, self.content_length, self.content_length);
            self.errors.push(ScriptError {
                message: message.into(),

                node_id,
            });

            node_id
        }
    }

    pub fn create_node(
        &mut self,
        node_type: NodeType,
        span_start: usize,
        span_end: usize,
    ) -> NodeId {
        self.delta.span_start.push(span_start);
        self.delta.span_end.push(span_end);
        self.delta.node_types.push(node_type);

        NodeId(self.delta.span_start.len() - 1 + self.delta.node_id_offset)
    }

    pub fn code_block(&mut self, in_block: bool) -> NodeId {
        let span_start = self.position();
        let mut code_body = vec![];
        if in_block {
            self.lcurly();
        }

        while self.has_tokens() {
            if self.is_rcurly() && in_block {
                self.rcurly();
                break;
            } else if self.is_semicolon() {
                self.lexer.next();
                continue;
            } else if self.is_keyword(b"fn") {
                let result = self.fn_definition();
                code_body.push(result);

                if !self.is_rcurly()
                    && !self.is_rparen()
                    && !self.is_semicolon()
                    && self.has_tokens()
                {
                    let p = self.lexer.peek();
                    self.error(format!("new line or semicolon (found {:?})", p));
                }
            } else {
                code_body.push(self.expression());
            }
        }
        let span_end = self.position();

        self.create_node(NodeType::Block(code_body), span_start, span_end)
    }

    pub fn fn_definition(&mut self) -> NodeId {
        let span_start = self.position();
        self.keyword(b"fn");

        let name = self.name();

        let params = self.params();

        let block = self.code_block(true);

        let span_end = self.position();

        self.create_node(
            NodeType::Fn {
                name,
                params,
                block,
            },
            span_start,
            span_end,
        )
    }

    pub fn expression(&mut self) -> NodeId {
        let mut expr_stack = vec![];

        let mut last_prec = 1000000;

        // // Check for special forms
        // if self.is_keyword(b"if") {
        //     return self.if_expression();
        // } else if self.is_keyword(b"where") {
        //     return self.where_expression();
        // }

        // Otherwise assume a math expression
        let lhs = if self.is_simple_expression() {
            self.simple_expression()
        } else {
            return self.error("incomplete math expression");
        };

        expr_stack.push(lhs);

        while self.has_tokens() {
            if self.is_operator() {
                let op = self.operator();
                let op_prec = self.operator_precedence(&op);

                let rhs = if self.is_simple_expression() {
                    self.simple_expression()
                } else {
                    self.error("incomplete math expression")
                };

                while op_prec <= last_prec && expr_stack.len() > 1 {
                    let rhs = expr_stack
                        .pop()
                        .expect("internal error: expression stack empty");
                    let op = expr_stack
                        .pop()
                        .expect("internal error: expression stack empty");

                    last_prec = self.operator_precedence(&op);

                    if last_prec < op_prec {
                        expr_stack.push(op);
                        expr_stack.push(rhs);
                        break;
                    }

                    let lhs = expr_stack
                        .pop()
                        .expect("internal error: expression stack empty");

                    let (span_start, span_end) = self.spanning(&lhs, &rhs);
                    expr_stack.push(self.create_node(
                        NodeType::BinaryOp { lhs, op, rhs },
                        span_start,
                        span_end,
                    ))
                }

                expr_stack.push(op);
                expr_stack.push(rhs);

                last_prec = op_prec;
            } else {
                break;
            }
        }

        while expr_stack.len() > 1 {
            let rhs = expr_stack
                .pop()
                .expect("internal error: expression stack empty");
            let op = expr_stack
                .pop()
                .expect("internal error: expression stack empty");
            let lhs = expr_stack
                .pop()
                .expect("internal error: expression stack empty");

            let (span_start, span_end) = self.spanning(&lhs, &rhs);

            expr_stack.push(self.create_node(
                NodeType::BinaryOp { lhs, op, rhs },
                span_start,
                span_end,
            ))
        }

        expr_stack
            .pop()
            .expect("internal error: expression stack empty")
    }

    pub fn simple_expression(&mut self) -> NodeId {
        let span_start = self.position();

        let expr = if self.is_lcurly() {
            self.code_block(true)
        } else if self.is_lparen() {
            self.expression()
        } else if self.is_keyword(b"true") || self.is_keyword(b"false") {
            self.boolean()
        } else if self.is_string() {
            self.string()
        } else if self.is_number() {
            self.number()
        } else if self.is_name() {
            self.variable_or_call()
        } else {
            panic!("incomplete: {:?}", self.lexer.peek());
            let bare_string = self.name();
            self.delta.node_types[bare_string.0] = NodeType::String;
            return bare_string;
        };

        if self.is_dotdot() {
            // Range
            self.lexer.next();

            let rhs = self.simple_expression();
            let span_end = self.position();

            self.create_node(NodeType::Range { lhs: expr, rhs }, span_start, span_end)
        } else {
            expr
        }
    }

    pub fn peek(&mut self) -> Option<Token> {
        self.lexer.peek()
    }

    pub fn next(&mut self) -> Option<Token> {
        self.lexer.next()
    }

    pub fn number(&mut self) -> NodeId {
        match self.lexer.peek() {
            Some(Token {
                token_type: TokenType::Number,
                span_start,
                span_end,
                contents,
            }) => {
                self.lexer.next();

                if contents.contains(&b'.') {
                    self.create_node(NodeType::Float, span_start, span_end)
                } else {
                    self.create_node(NodeType::Int, span_start, span_end)
                }
            }
            _ => self.error("expected: number"),
        }
    }

    pub fn boolean(&mut self) -> NodeId {
        match self.lexer.peek() {
            Some(Token {
                token_type: TokenType::Name,
                span_start,
                span_end,
                contents,
            }) if contents == b"true" => {
                self.lexer.next();
                self.create_node(NodeType::True, span_start, span_end)
            }
            Some(Token {
                token_type: TokenType::Name,
                span_start,
                span_end,
                contents,
            }) if contents == b"false" => {
                self.lexer.next();
                self.create_node(NodeType::False, span_start, span_end)
            }
            _ => self.error("expected: boolean"),
        }
    }

    pub fn operator(&mut self) -> NodeId {
        match self.lexer.peek() {
            Some(Token {
                token_type,
                contents,
                span_start,
                span_end,
            }) => match token_type {
                TokenType::Plus => {
                    self.lexer.next();
                    self.create_node(NodeType::Plus, span_start, span_end)
                }
                TokenType::PlusPlus => {
                    self.lexer.next();
                    self.create_node(NodeType::Append, span_start, span_end)
                }
                TokenType::Dash => {
                    self.lexer.next();
                    self.create_node(NodeType::Minus, span_start, span_end)
                }
                TokenType::Asterisk => {
                    self.lexer.next();
                    self.create_node(NodeType::Multiply, span_start, span_end)
                }
                TokenType::ForwardSlash => {
                    self.lexer.next();
                    self.create_node(NodeType::Divide, span_start, span_end)
                }
                TokenType::ForwardSlashForwardSlash => {
                    self.lexer.next();
                    self.create_node(NodeType::FloorDivision, span_start, span_end)
                }
                TokenType::LessThan => {
                    self.lexer.next();
                    self.create_node(NodeType::LessThan, span_start, span_end)
                }
                TokenType::LessThanEqual => {
                    self.lexer.next();
                    self.create_node(NodeType::LessThanOrEqual, span_start, span_end)
                }
                TokenType::GreaterThan => {
                    self.lexer.next();
                    self.create_node(NodeType::GreaterThan, span_start, span_end)
                }
                TokenType::GreaterThanEqual => {
                    self.lexer.next();
                    self.create_node(NodeType::GreaterThanOrEqual, span_start, span_end)
                }
                TokenType::EqualsEquals => {
                    self.lexer.next();
                    self.create_node(NodeType::Equal, span_start, span_end)
                }
                TokenType::ExclamationEquals => {
                    self.lexer.next();
                    self.create_node(NodeType::NotEqual, span_start, span_end)
                }
                TokenType::AsteriskAsterisk => {
                    self.lexer.next();
                    self.create_node(NodeType::Pow, span_start, span_end)
                }
                TokenType::EqualsTilde => {
                    self.lexer.next();
                    self.create_node(NodeType::RegexMatch, span_start, span_end)
                }
                TokenType::ExclamationTilde => {
                    self.lexer.next();
                    self.create_node(NodeType::NotRegexMatch, span_start, span_end)
                }
                TokenType::AmpersandAmpersand => {
                    self.lexer.next();
                    self.create_node(NodeType::And, span_start, span_end)
                }
                TokenType::PipePipe => {
                    self.lexer.next();
                    self.create_node(NodeType::Or, span_start, span_end)
                }
                _ => self.error("expected: operator"),
            },
            _ => self.error("expected: operator"),
        }
    }

    pub fn operator_precedence(&mut self, operator: &NodeId) -> usize {
        self.delta.node_types[operator.0].precedence()
    }

    pub fn spanning(&mut self, from: &NodeId, to: &NodeId) -> (usize, usize) {
        (self.delta.span_start[from.0], self.delta.span_end[to.0])
    }

    pub fn string(&mut self) -> NodeId {
        match self.lexer.peek() {
            Some(Token {
                token_type: TokenType::String,
                span_start,
                span_end,
                ..
            }) => {
                self.lexer.next();
                self.create_node(NodeType::String, span_start, span_end)
            }
            _ => self.error("expected: string"),
        }
    }

    pub fn name(&mut self) -> NodeId {
        match self.lexer.peek() {
            Some(Token {
                token_type: TokenType::Name,
                span_start,
                span_end,
                ..
            }) => {
                self.lexer.next();
                self.create_node(NodeType::Name, span_start, span_end)
            }
            _ => self.error("expect name"),
        }
    }

    pub fn params(&mut self) -> NodeId {
        let span_start = self.position();
        let param_list = {
            self.lparen();
            let output = self.param_list();
            self.rparen();

            output
        };

        let span_end = self.position();

        self.create_node(NodeType::Params(param_list), span_start, span_end)
    }

    pub fn param_list(&mut self) -> Vec<NodeId> {
        let mut params = vec![];
        while self.has_tokens() {
            if self.is_rparen() || self.is_rsquare() || self.is_pipe() {
                break;
            }

            if self.is_comma() {
                self.lexer.next();
                continue;
            }

            // Parse param
            let span_start = self.position();
            let name = self.name();
            if self.is_colon() {
                // Optional type
                self.colon();

                let ty = self.name();

                let span_end = self.position();

                params.push(self.create_node(
                    NodeType::Param { name, ty: Some(ty) },
                    span_start,
                    span_end,
                ))
            } else {
                let span_end = self.position();
                params.push(self.create_node(
                    NodeType::Param { name, ty: None },
                    span_start,
                    span_end,
                ))
            }
        }

        params
    }

    pub fn variable_or_call(&mut self) -> NodeId {
        if self.is_name() {
            let span_start = self.position();

            let name = self
                .next()
                .expect("internal error: missing token that was expected to be there");
            let name_start = name.span_start;
            let name_end = name.span_end;

            if self.is_lparen() {
                let head = self.create_node(NodeType::Name, name_start, name_end);
                // We're a call
                self.lparen();
                let mut args = vec![];
                loop {
                    if self.is_simple_expression() {
                        args.push(self.simple_expression());

                        if self.is_comma() {
                            self.lexer.next();
                            continue;
                        } else if self.is_rparen() {
                            break;
                        } else {
                            args.push(self.error("unexpected value in call arguments"));
                        }
                    }
                }
                self.rparen();

                let span_end = self.position();
                self.create_node(NodeType::Call { head, args }, span_start, span_end)
            } else {
                // We're a variable
                self.create_node(NodeType::Variable, name_start, name_end)
            }
        } else {
            self.error("expected variable or call")
        }
    }

    pub fn keyword(&mut self, keyword: &[u8]) {
        match self.lexer.peek() {
            Some(Token {
                token_type: TokenType::Name,
                contents,
                ..
            }) if contents == keyword => {
                self.lexer.next();
            }
            _ => {
                // self.error(ShellErrorType::Expected(
                //     String::from_utf8_lossy(keyword).to_string(),
                // ));
            }
        }
    }

    pub fn lparen(&mut self) {
        match self.lexer.peek() {
            Some(Token {
                token_type: TokenType::LParen,
                ..
            }) => {
                self.lexer.next();
            }
            _ => {
                self.error("expected: left paren '('");
            }
        }
    }

    pub fn rparen(&mut self) {
        match self.lexer.peek() {
            Some(Token {
                token_type: TokenType::RParen,
                ..
            }) => {
                self.lexer.next();
            }
            _ => {
                self.error("expected: right paren ')'");
            }
        }
    }

    pub fn lcurly(&mut self) {
        match self.lexer.peek() {
            Some(Token {
                token_type: TokenType::LCurly,
                ..
            }) => {
                self.lexer.next();
            }
            _ => {
                self.error("expected: left bracket '{'");
            }
        }
    }

    pub fn rcurly(&mut self) {
        match self.lexer.peek() {
            Some(Token {
                token_type: TokenType::RCurly,
                ..
            }) => {
                self.lexer.next();
            }
            _ => {
                self.error("expected: right bracket '}'");
            }
        }
    }

    pub fn colon(&mut self) {
        match self.lexer.peek() {
            Some(Token {
                token_type: TokenType::Colon,
                ..
            }) => {
                self.lexer.next();
            }
            _ => {
                self.error("expected: colon ':'");
            }
        }
    }
}