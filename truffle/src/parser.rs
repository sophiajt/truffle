use std::fmt::Display;

use crate::errors::{ErrorBatch, ScriptError};
use crate::lexer::{Token, TokenType};

pub struct Parser {
    pub results: ParseResults,
    pub tokens: Vec<Token>,
    pub current_token: usize,
    pub content_length: usize,
    pub errors: ErrorBatch,
}

#[derive(Debug, PartialEq)]
pub enum AstNode {
    Int,
    Float,
    String,
    Name,
    Type,
    Variable,

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
    Plus,
    Append,
    Minus,
    Multiply,
    Divide,
    // Modulo,
    And,
    Or,
    Pow,

    Assignment,
    AddAssignment,
    MinusAssignment,
    MultiplyAssignment,
    DivideAssignment,

    // Statements
    Let {
        variable_name: NodeId,
        ty: Option<NodeId>,
        initializer: NodeId,
        is_mutable: bool,
    },
    While {
        condition: NodeId,
        block: NodeId,
    },
    For {
        variable: NodeId,
        range: NodeId,
        block: NodeId,
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

    // Closure {
    //     params: NodeId,
    //     block: NodeId,
    // },

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
    Await(NodeId),
    Statement(NodeId),
    Garbage,
}

impl AstNode {
    pub fn precedence(&self) -> usize {
        match self {
            AstNode::Pow => 100,
            AstNode::Multiply | AstNode::Divide => 95,
            //AstNode::Modulo => 95,
            AstNode::Plus | AstNode::Minus => 90,
            AstNode::LessThan
            | AstNode::LessThanOrEqual
            | AstNode::GreaterThan
            | AstNode::GreaterThanOrEqual
            | AstNode::Equal
            | AstNode::NotEqual => 80,
            AstNode::And => 50,
            AstNode::Or => 40,
            AstNode::Assignment
            | AstNode::AddAssignment
            | AstNode::MinusAssignment
            | AstNode::MultiplyAssignment
            | AstNode::DivideAssignment => 10,
            _ => 0,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
/// A region of source code
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.start, self.end)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
/// Uniquely identifies a node in the Abstract Syntax Tree
///
/// Used as an index into various vectors in `ParseResults`.
pub struct NodeId(pub usize);

impl Parser {
    pub fn new(tokens: Vec<Token>, source: Vec<u8>, node_id_offset: usize) -> Self {
        let content_length = source.len();
        Self {
            results: ParseResults::new(node_id_offset, source),
            tokens,
            current_token: 0,
            content_length,
            errors: ErrorBatch::empty(),
        }
    }

    fn peek(&self) -> Option<Token> {
        if self.current_token < self.tokens.len() {
            Some(self.tokens[self.current_token])
        } else {
            None
        }
    }

    fn next(&mut self) -> Option<Token> {
        if self.current_token < self.tokens.len() {
            self.current_token += 1;
            Some(self.tokens[self.current_token - 1])
        } else {
            None
        }
    }

    fn position(&mut self) -> usize {
        if let Some(Token { span, .. }) = self.peek() {
            span.start
        } else {
            self.content_length
        }
    }

    pub fn parse(&mut self) -> Result<(), ErrorBatch> {
        self.program();
        if !self.errors.is_empty() {
            Err(self.errors.clone())
        } else {
            Ok(())
        }
    }

    pub fn program(&mut self) -> NodeId {
        self.block(false)
    }

    pub fn has_tokens(&mut self) -> bool {
        self.peek().is_some()
    }

    pub fn is_operator(&mut self) -> bool {
        match self.peek() {
            Some(Token { token_type, .. }) => matches!(
                token_type,
                TokenType::Asterisk
                    | TokenType::AsteriskAsterisk
                    | TokenType::Dash
                    | TokenType::EqualsEquals
                    | TokenType::ExclamationEquals
                    | TokenType::ForwardSlash
                    | TokenType::LessThan
                    | TokenType::LessThanEqual
                    | TokenType::Plus
                    | TokenType::PlusEquals
                    | TokenType::DashEquals
                    | TokenType::AsteriskEquals
                    | TokenType::ForwardSlashEquals
                    | TokenType::GreaterThan
                    | TokenType::GreaterThanEqual
                    | TokenType::AmpersandAmpersand
                    | TokenType::PipePipe
                    | TokenType::Equals
            ),
            _ => false,
        }
    }

    pub fn is_comma(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::Comma,
                ..
            })
        )
    }

    pub fn is_lcurly(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::LCurly,
                ..
            })
        )
    }

    pub fn is_rcurly(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::RCurly,
                ..
            })
        )
    }

    pub fn is_lparen(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::LParen,
                ..
            })
        )
    }

    pub fn is_rparen(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::RParen,
                ..
            })
        )
    }

    // pub fn is_lsquare(&mut self) -> bool {
    //     matches!(
    //         self.peek(),
    //         Some(Token {
    //             token_type: TokenType::LSquare,
    //             ..
    //         })
    //     )
    // }

    pub fn is_rsquare(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::RSquare,
                ..
            })
        )
    }

    // pub fn is_less_than(&mut self) -> bool {
    //     matches!(
    //         self.peek(),
    //         Some(Token {
    //             token_type: TokenType::LessThan,
    //             ..
    //         })
    //     )
    // }

    // pub fn is_greater_than(&mut self) -> bool {
    //     matches!(
    //         self.peek(),
    //         Some(Token {
    //             token_type: TokenType::GreaterThan,
    //             ..
    //         })
    //     )
    // }

    pub fn is_pipe(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::Pipe,
                ..
            })
        )
    }

    // pub fn is_double_pipe(&mut self) -> bool {
    //     matches!(
    //         self.peek(),
    //         Some(Token {
    //             token_type: TokenType::PipePipe,
    //             ..
    //         })
    //     )
    // }

    // pub fn is_double_ampersand(&mut self) -> bool {
    //     matches!(
    //         self.peek(),
    //         Some(Token {
    //             token_type: TokenType::AmpersandAmpersand,
    //             ..
    //         })
    //     )
    // }

    // pub fn is_dash(&mut self) -> bool {
    //     matches!(
    //         self.peek(),
    //         Some(Token {
    //             token_type: TokenType::Dash,
    //             ..
    //         })
    //     )
    // }

    pub fn is_colon(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::Colon,
                ..
            })
        )
    }

    pub fn is_semicolon(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::Semicolon,
                ..
            })
        )
    }

    pub fn is_dot(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::Dot,
                ..
            })
        )
    }

    pub fn is_dotdot(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::DotDot,
                ..
            })
        )
    }

    pub fn is_number(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::Number,
                ..
            })
        )
    }

    pub fn is_string(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::String,
                ..
            })
        )
    }

    pub fn is_keyword(&self, keyword: &[u8]) -> bool {
        if let Some(Token {
            token_type: TokenType::Name,
            span,
        }) = self.peek()
        {
            if keyword == self.results.contents_for_span(span) {
                return true;
            }
        }

        false
    }

    pub fn is_name(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::Name,
                ..
            })
        )
    }

    pub fn is_expression(&mut self) -> bool {
        self.is_simple_expression() || self.is_keyword(b"if") || self.is_keyword(b"where")
    }

    pub fn is_simple_expression(&self) -> bool {
        match self.peek() {
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
                ..
            }) if self.is_keyword(b"true") => true,
            Some(Token {
                token_type: TokenType::Name,
                ..
            }) if self.is_keyword(b"false") => true,
            Some(Token {
                token_type: TokenType::Name,
                ..
            }) => true,
            _ => false,
        }
    }

    pub fn get_span_end(&self, node_id: NodeId) -> usize {
        self.results.spans[node_id.0].end
    }

    pub fn error(&mut self, message: impl Into<String>) -> NodeId {
        if let Some(Token { span, .. }) = self.next() {
            let node_id = self.create_node(AstNode::Garbage, span);
            self.errors.push(ScriptError {
                message: message.into(),
                span,
            });

            node_id
        } else {
            let span = Span {
                start: self.content_length,
                end: self.content_length,
            };
            let node_id = self.create_node(AstNode::Garbage, span);
            self.errors.push(ScriptError {
                message: message.into(),
                span,
            });

            node_id
        }
    }

    pub fn create_node(&mut self, node_type: AstNode, span: Span) -> NodeId {
        self.results.spans.push(span);
        self.results.ast_nodes.push(node_type);

        NodeId(self.results.spans.len() - 1 + self.results.node_id_offset)
    }

    pub fn block(&mut self, expect_parens: bool) -> NodeId {
        let start = self.position();
        let mut end = self.position();

        let mut code_body = vec![];
        if expect_parens {
            self.lcurly();
        }

        while self.has_tokens() {
            if self.is_rcurly() && expect_parens {
                end = self.position() + 1;
                self.rcurly();
                break;
            } else if self.is_semicolon() {
                self.next();
                continue;
            } else if self.is_keyword(b"fn") {
                let result = self.fn_definition();
                code_body.push(result);

                if !self.is_rcurly()
                    && !self.is_rparen()
                    && !self.is_semicolon()
                    && self.has_tokens()
                {
                    let p = self.peek();
                    self.error(format!("expected newline or semicolon but found {:?}", p));
                }
            } else if self.is_keyword(b"let") {
                let result = self.let_statement();
                code_body.push(result);
            } else if self.is_keyword(b"while") {
                let result = self.while_statement();
                code_body.push(result);
            } else if self.is_keyword(b"for") {
                let result = self.for_statement();
                code_body.push(result);
            } else {
                let start = self.position();
                let expression = self.expression();
                let end = self.get_span_end(expression);
                let span = Span { start, end };

                if self.is_semicolon() {
                    // This is a statement, not an expression
                    self.next();
                    code_body.push(self.create_node(AstNode::Statement(expression), span))
                } else {
                    code_body.push(expression);
                }
            }
        }

        if end == start && !code_body.is_empty() {
            end = self.get_span_end(
                *code_body
                    .last()
                    .expect("internal error: expected ast nodes"),
            );
        }

        let span = Span { start, end };
        self.create_node(AstNode::Block(code_body), span)
    }

    pub fn fn_definition(&mut self) -> NodeId {
        let start = self.position();
        self.keyword(b"fn");

        let name = self.name();

        let params = self.params();

        let block = self.block(true);

        let end = self.get_span_end(block);

        let span = Span { start, end };
        self.create_node(
            AstNode::Fn {
                name,
                params,
                block,
            },
            span,
        )
    }

    pub fn method_call(&mut self, start: usize, receiver: NodeId) -> NodeId {
        let method_name = self
            .next()
            .expect("internal error: missing token that was expected to be there");
        let name_span = method_name.span;
        let head = self.create_node(AstNode::Name, name_span);

        self.lparen();
        let mut args = self.args_list();
        args.insert(0, receiver);
        let end = self.position() + 1;
        self.rparen();

        let span = Span { start, end };
        self.create_node(AstNode::Call { head, args }, span)
    }

    pub fn expression(&mut self) -> NodeId {
        let mut expr_stack = vec![];

        let mut last_prec = 1000000;

        // Check for special forms
        if self.is_keyword(b"if") {
            return self.if_expression();
        }

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
                let op_prec = self.operator_precedence(op);

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

                    last_prec = self.operator_precedence(op);

                    if last_prec < op_prec {
                        expr_stack.push(op);
                        expr_stack.push(rhs);
                        break;
                    }

                    let lhs = expr_stack
                        .pop()
                        .expect("internal error: expression stack empty");

                    let span = self.spanning(lhs, rhs);
                    expr_stack.push(self.create_node(AstNode::BinaryOp { lhs, op, rhs }, span))
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

            let span = self.spanning(lhs, rhs);

            expr_stack.push(self.create_node(AstNode::BinaryOp { lhs, op, rhs }, span))
        }

        expr_stack
            .pop()
            .expect("internal error: expression stack empty")
    }

    pub fn simple_expression(&mut self) -> NodeId {
        let start = self.position();

        let expr = if self.is_lcurly() {
            self.block(true)
        } else if self.is_lparen() {
            self.lparen();
            let output = self.expression();
            self.rparen();
            output
        } else if self.is_keyword(b"true") || self.is_keyword(b"false") {
            self.boolean()
        } else if self.is_string() {
            self.string()
        } else if self.is_number() {
            self.number()
        } else if self.is_name() {
            self.variable_or_call()
        } else {
            self.error("incomplete expression")
        };

        if self.is_dotdot() {
            // Range
            self.next();

            let rhs = self.simple_expression();
            let end = self.get_span_end(rhs);
            let span = Span { start, end };

            self.create_node(AstNode::Range { lhs: expr, rhs }, span)
        } else if self.is_dot() {
            self.next();

            if self.is_keyword(b"await") {
                // consume the 'await' keyword
                self.next();

                let end = self.position();
                let span = Span { start, end };
                self.create_node(AstNode::Await(expr), span)
            } else if !self.has_tokens() {
                self.error("missing method call")
            } else {
                self.method_call(start, expr)
            }
        } else {
            expr
        }
    }

    pub fn number(&mut self) -> NodeId {
        match self.peek() {
            Some(Token {
                token_type: TokenType::Number,
                span,
            }) => {
                let contents = self.results.contents_for_span(span);
                let is_float = contents.contains(&b'.');

                self.next();

                if is_float {
                    self.create_node(AstNode::Float, span)
                } else {
                    self.create_node(AstNode::Int, span)
                }
            }
            _ => self.error("expected: number"),
        }
    }

    pub fn boolean(&mut self) -> NodeId {
        match self.peek() {
            Some(Token {
                token_type: TokenType::Name,
                span,
            }) => {
                let contents = &self.results.contents[span.start..span.end];

                if contents == b"true" {
                    self.next();
                    self.create_node(AstNode::True, span)
                } else if contents == b"false" {
                    self.next();
                    self.create_node(AstNode::False, span)
                } else {
                    self.error("expected: boolean")
                }
            }
            _ => self.error("expected: boolean"),
        }
    }

    pub fn operator(&mut self) -> NodeId {
        match self.peek() {
            Some(Token {
                token_type, span, ..
            }) => match token_type {
                TokenType::Plus => {
                    self.next();
                    self.create_node(AstNode::Plus, span)
                }
                TokenType::PlusPlus => {
                    self.next();
                    self.create_node(AstNode::Append, span)
                }
                TokenType::Dash => {
                    self.next();
                    self.create_node(AstNode::Minus, span)
                }
                TokenType::Asterisk => {
                    self.next();
                    self.create_node(AstNode::Multiply, span)
                }
                TokenType::ForwardSlash => {
                    self.next();
                    self.create_node(AstNode::Divide, span)
                }
                TokenType::LessThan => {
                    self.next();
                    self.create_node(AstNode::LessThan, span)
                }
                TokenType::LessThanEqual => {
                    self.next();
                    self.create_node(AstNode::LessThanOrEqual, span)
                }
                TokenType::GreaterThan => {
                    self.next();
                    self.create_node(AstNode::GreaterThan, span)
                }
                TokenType::GreaterThanEqual => {
                    self.next();
                    self.create_node(AstNode::GreaterThanOrEqual, span)
                }
                TokenType::EqualsEquals => {
                    self.next();
                    self.create_node(AstNode::Equal, span)
                }
                TokenType::PlusEquals => {
                    self.next();
                    self.create_node(AstNode::AddAssignment, span)
                }
                TokenType::DashEquals => {
                    self.next();
                    self.create_node(AstNode::MinusAssignment, span)
                }
                TokenType::AsteriskEquals => {
                    self.next();
                    self.create_node(AstNode::MultiplyAssignment, span)
                }
                TokenType::ForwardSlashEquals => {
                    self.next();
                    self.create_node(AstNode::DivideAssignment, span)
                }
                TokenType::ExclamationEquals => {
                    self.next();
                    self.create_node(AstNode::NotEqual, span)
                }
                TokenType::AsteriskAsterisk => {
                    self.next();
                    self.create_node(AstNode::Pow, span)
                }
                TokenType::AmpersandAmpersand => {
                    self.next();
                    self.create_node(AstNode::And, span)
                }
                TokenType::PipePipe => {
                    self.next();
                    self.create_node(AstNode::Or, span)
                }
                TokenType::Equals => {
                    self.next();
                    self.create_node(AstNode::Assignment, span)
                }
                _ => self.error("expected: operator"),
            },
            _ => self.error("expected: operator"),
        }
    }

    pub fn operator_precedence(&mut self, operator: NodeId) -> usize {
        self.results.ast_nodes[operator.0].precedence()
    }

    pub fn spanning(&mut self, from: NodeId, to: NodeId) -> Span {
        let start = self.results.spans[from.0].start;
        let end = self.results.spans[to.0].end;
        Span { start, end }
    }

    pub fn string(&mut self) -> NodeId {
        match self.peek() {
            Some(Token {
                token_type: TokenType::String,
                span,
                ..
            }) => {
                let mut start = span.start;
                let mut end = span.end;

                // Exclude quotation marks
                if self.results.contents[start] == b'"' && self.results.contents[end - 1] == b'"' {
                    start += 1;
                    end -= 1;
                } else {
                    if self.results.contents[start] != b'"' {
                        return self.error("expected: left quotation mark '\"'");
                    }
                    return self.error("expected: right quotation mark '\"'");
                }

                self.next();
                self.create_node(AstNode::String, Span { start, end })
            }
            _ => self.error("expected: string"),
        }
    }

    pub fn name(&mut self) -> NodeId {
        match self.peek() {
            Some(Token {
                token_type: TokenType::Name,
                span,
                ..
            }) => {
                self.next();
                self.create_node(AstNode::Name, span)
            }
            _ => self.error("expect name"),
        }
    }

    pub fn typename(&mut self) -> NodeId {
        match self.peek() {
            Some(Token {
                token_type: TokenType::Name,
                span,
                ..
            }) => {
                self.next();
                self.create_node(AstNode::Type, span)
            }
            _ => self.error("expect name"),
        }
    }

    pub fn params(&mut self) -> NodeId {
        let start = self.position();

        let param_list = {
            self.lparen();
            let output = self.param_list();
            self.rparen();

            output
        };

        let end = self.position();

        let span = Span { start, end };
        self.create_node(AstNode::Params(param_list), span)
    }

    pub fn param_list(&mut self) -> Vec<NodeId> {
        let mut params = vec![];
        while self.has_tokens() {
            if self.is_rparen() || self.is_rsquare() || self.is_pipe() {
                break;
            }

            if self.is_comma() {
                self.next();
                continue;
            }

            // Parse param
            let start = self.position();
            let name = self.name();
            if self.is_colon() {
                // Optional type
                self.colon();

                let ty = self.name();

                let end = self.get_span_end(ty);
                let span = Span { start, end };

                params.push(self.create_node(AstNode::Param { name, ty: Some(ty) }, span))
            } else {
                let end = self.get_span_end(name);
                let span = Span { start, end };
                params.push(self.create_node(AstNode::Param { name, ty: None }, span))
            }
        }

        params
    }

    pub fn args_list(&mut self) -> Vec<NodeId> {
        let mut args = vec![];
        loop {
            if self.is_expression() {
                args.push(self.expression());

                if self.is_comma() {
                    self.next();
                    continue;
                } else if self.is_rparen() {
                    break;
                } else {
                    args.push(self.error("unexpected value in call arguments"));
                }
            } else {
                break;
            }
        }
        args
    }

    /// Parse an if expression, consuming tokens within the lexer, saves an
    /// AstNode for the parsed expression and yields a new NodeID for the parsed
    /// expression.
    pub fn if_expression(&mut self) -> NodeId {
        let start = self.position();
        let end;

        self.keyword(b"if");

        let condition = self.expression();

        let then_block = self.block(true);

        let else_expression = if self.is_keyword(b"else") {
            self.next();
            let expr = self.expression();
            end = self.get_span_end(expr);
            Some(expr)
        } else {
            end = self.get_span_end(then_block);
            None
        };

        let span = Span { start, end };
        self.create_node(
            AstNode::If {
                condition,
                then_block,
                else_expression,
            },
            span,
        )
    }

    pub fn let_statement(&mut self) -> NodeId {
        let mut is_mutable = false;
        let start = self.position();

        self.keyword(b"let");

        if self.is_keyword(b"mut") {
            is_mutable = true;
            self.next();
        }

        let variable_name = self.variable();

        let ty = if self.is_colon() {
            // We have a type
            self.colon();

            Some(self.typename())
        } else {
            None
        };

        self.equals();

        let initializer = self.expression();

        let end = self.get_span_end(initializer);

        let span = Span { start, end };
        self.create_node(
            AstNode::Let {
                variable_name,
                ty,
                initializer,
                is_mutable,
            },
            span,
        )
    }

    pub fn while_statement(&mut self) -> NodeId {
        let start = self.position();
        self.keyword(b"while");

        let condition = self.expression();
        let block = self.block(true);
        let end = self.get_span_end(block);

        let span = Span { start, end };
        self.create_node(AstNode::While { condition, block }, span)
    }

    pub fn for_statement(&mut self) -> NodeId {
        let start = self.position();
        self.keyword(b"for");

        let variable = self.variable();
        self.keyword(b"in");

        let range = self.simple_expression();
        let block = self.block(true);
        let end = self.get_span_end(block);

        let span = Span { start, end };
        self.create_node(
            AstNode::For {
                variable,
                range,
                block,
            },
            span,
        )
    }

    pub fn variable(&mut self) -> NodeId {
        if self.is_name() {
            let name = self
                .next()
                .expect("internal error: missing token that was expected to be there");
            let name_span = name.span;
            self.create_node(AstNode::Variable, name_span)
        } else {
            self.error("expected variable")
        }
    }

    pub fn variable_or_call(&mut self) -> NodeId {
        if self.is_name() {
            let start = self.position();

            let name = self
                .next()
                .expect("internal error: missing token that was expected to be there");

            if self.is_lparen() {
                let head = self.create_node(AstNode::Name, name.span);
                // We're a call
                self.lparen();
                let args = self.args_list();
                let end = self.position() + 1;
                self.rparen();

                self.create_node(AstNode::Call { head, args }, Span { start, end })
            } else {
                // We're a variable
                self.create_node(AstNode::Variable, name.span)
            }
        } else {
            self.error("expected variable or call")
        }
    }

    pub fn keyword(&mut self, keyword: &[u8]) {
        if let Some(Token {
            token_type: TokenType::Name,
            span,
        }) = self.peek()
        {
            let contents = self.results.contents_for_span(span);

            if contents == keyword {
                self.next();
                return;
            }
        }
        self.error(format!(
            "expected keyword: {}",
            String::from_utf8_lossy(keyword)
        ));
    }

    pub fn lparen(&mut self) {
        match self.peek() {
            Some(Token {
                token_type: TokenType::LParen,
                ..
            }) => {
                self.next();
            }
            _ => {
                self.error("expected: left paren '('");
            }
        }
    }

    pub fn rparen(&mut self) {
        match self.peek() {
            Some(Token {
                token_type: TokenType::RParen,
                ..
            }) => {
                self.next();
            }
            _ => {
                self.error("expected: right paren ')'");
            }
        }
    }

    pub fn lcurly(&mut self) {
        match self.peek() {
            Some(Token {
                token_type: TokenType::LCurly,
                ..
            }) => {
                self.next();
            }
            _ => {
                self.error("expected: left bracket '{'");
            }
        }
    }

    pub fn rcurly(&mut self) {
        match self.peek() {
            Some(Token {
                token_type: TokenType::RCurly,
                ..
            }) => {
                self.next();
            }
            _ => {
                self.error("expected: right bracket '}'");
            }
        }
    }

    pub fn equals(&mut self) {
        match self.peek() {
            Some(Token {
                token_type: TokenType::Equals,
                ..
            }) => {
                self.next();
            }
            _ => {
                self.error("expected: equals '='");
            }
        }
    }

    pub fn colon(&mut self) {
        match self.peek() {
            Some(Token {
                token_type: TokenType::Colon,
                ..
            }) => {
                self.next();
            }
            _ => {
                self.error("expected: colon ':'");
            }
        }
    }
}

#[derive(Debug)]
pub struct ParseResults {
    pub node_id_offset: usize,
    pub spans: Vec<Span>,
    pub ast_nodes: Vec<AstNode>,
    pub contents: Vec<u8>,
}

impl ParseResults {
    pub fn new(node_id_offset: usize, contents: Vec<u8>) -> Self {
        Self {
            node_id_offset,
            spans: vec![],
            ast_nodes: vec![],
            contents,
        }
    }

    pub fn print(&self) {
        if self.ast_nodes.is_empty() {
            println!("<empty>");
        } else {
            self.print_helper(&NodeId(self.ast_nodes.len() - 1), 0)
        }
    }

    fn print_helper(&self, node_id: &NodeId, indent: usize) {
        for _ in 0..indent {
            print!(" ")
        }

        match &self.ast_nodes[node_id.0] {
            AstNode::Let {
                variable_name,
                ty,
                initializer,
                is_mutable,
            } => {
                println!("Let ({}, mutable: {}):", self.spans[node_id.0], is_mutable);
                self.print_helper(variable_name, indent + 2);
                if let Some(ty) = ty {
                    self.print_helper(ty, indent + 2);
                }
                self.print_helper(initializer, indent + 2);
            }
            AstNode::Param { name, ty } => {
                println!("Param {}:", self.spans[node_id.0],);
                self.print_helper(name, indent + 2);
                if let Some(ty) = ty {
                    self.print_helper(ty, indent + 2);
                }
            }
            // AstNode::Closure { params, block } => {
            //     println!(
            //         "Closure {}:",
            //         self.spans[node_id.0],
            //     );
            //     self.print_helper(params, indent + 2);
            //     self.print_helper(block, indent + 2);
            // }
            AstNode::Fn {
                name,
                params,
                block,
            } => {
                println!("Fn:",);
                self.print_helper(name, indent + 2);
                self.print_helper(params, indent + 2);
                self.print_helper(block, indent + 2);
            }
            AstNode::Block(nodes) => {
                println!("Block {}:", self.spans[node_id.0],);
                for node in nodes {
                    self.print_helper(node, indent + 2);
                }
            }
            AstNode::Params(nodes) => {
                print!("Params {}:", self.spans[node_id.0],);
                if nodes.is_empty() {
                    println!(" <empty>");
                } else {
                    println!();
                }

                for node in nodes {
                    self.print_helper(node, indent + 2);
                }
            }
            AstNode::Call { head, args } => {
                println!("Call {}:", self.spans[node_id.0],);
                self.print_helper(head, indent + 2);

                for arg in args {
                    self.print_helper(arg, indent + 2);
                }
            }
            AstNode::BinaryOp { lhs, op, rhs } => {
                println!("BinaryOp {}:", self.spans[node_id.0],);

                self.print_helper(lhs, indent + 2);
                self.print_helper(op, indent + 2);
                self.print_helper(rhs, indent + 2)
            }
            AstNode::Range { lhs, rhs } => {
                println!("Range {}:", self.spans[node_id.0],);

                self.print_helper(lhs, indent + 2);
                self.print_helper(rhs, indent + 2)
            }
            AstNode::If {
                condition,
                then_block,
                else_expression,
            } => {
                println!("If {}:", self.spans[node_id.0],);
                self.print_helper(condition, indent + 2);
                self.print_helper(then_block, indent + 2);
                if let Some(else_expression) = else_expression {
                    self.print_helper(else_expression, indent + 2)
                }
            }
            x => {
                println!("{:?} ({})", x, self.spans[node_id.0],)
            }
        }
    }

    #[cfg(feature = "lsp")]
    pub(crate) fn contents_for_node(&self, node_id: NodeId) -> &[u8] {
        let span = self.spans[node_id.0];
        self.contents_for_span(span)
    }

    pub(crate) fn contents_for_span(&self, span: Span) -> &[u8] {
        &self.contents[span.start..span.end]
    }
}
