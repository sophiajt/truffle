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
            AstNode::Assignment => 10,
            _ => 0,
        }
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
        if let Some(Token { span_start, .. }) = self.peek() {
            span_start
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
            span_start,
            span_end,
        }) = self.peek()
        {
            if keyword == &self.results.contents[span_start..span_end] {
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

    pub fn error(&mut self, message: impl Into<String>) -> NodeId {
        if let Some(Token {
            span_start,
            span_end,
            ..
        }) = self.next()
        {
            let node_id = self.create_node(AstNode::Garbage, span_start, span_end);
            self.errors.push(ScriptError {
                message: message.into(),
                span_start,
                span_end,
            });

            node_id
        } else {
            let node_id =
                self.create_node(AstNode::Garbage, self.content_length, self.content_length);
            self.errors.push(ScriptError {
                message: message.into(),
                span_start: self.content_length,
                span_end: self.content_length,
            });

            node_id
        }
    }

    pub fn create_node(
        &mut self,
        node_type: AstNode,
        span_start: usize,
        span_end: usize,
    ) -> NodeId {
        self.results.span_start.push(span_start);
        self.results.span_end.push(span_end);
        self.results.ast_nodes.push(node_type);

        NodeId(self.results.span_start.len() - 1 + self.results.node_id_offset)
    }

    pub fn block(&mut self, expect_parens: bool) -> NodeId {
        let span_start = self.position();
        let mut code_body = vec![];
        if expect_parens {
            self.lcurly();
        }

        while self.has_tokens() {
            if self.is_rcurly() && expect_parens {
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
                let span_start = self.position();
                let expression = self.expression();
                let span_end = self.position();

                if self.is_semicolon() {
                    // This is a statement, not an expression
                    self.next();
                    code_body.push(self.create_node(
                        AstNode::Statement(expression),
                        span_start,
                        span_end,
                    ))
                } else {
                    code_body.push(expression);
                }
            }
        }
        let span_end = self.position();

        self.create_node(AstNode::Block(code_body), span_start, span_end)
    }

    pub fn fn_definition(&mut self) -> NodeId {
        let span_start = self.position();
        self.keyword(b"fn");

        let name = self.name();

        let params = self.params();

        let block = self.block(true);

        let span_end = self.position();

        self.create_node(
            AstNode::Fn {
                name,
                params,
                block,
            },
            span_start,
            span_end,
        )
    }

    pub fn method_call(&mut self, span_start: usize, receiver: NodeId) -> NodeId {
        // Skip the dot
        self.next();

        let method_name = self
            .next()
            .expect("internal error: missing token that was expected to be there");
        let name_start = method_name.span_start;
        let name_end = method_name.span_end;
        let head = self.create_node(AstNode::Name, name_start, name_end);

        self.lparen();
        let mut args = self.args_list();
        args.insert(0, receiver);
        self.rparen();

        let span_end = self.position();
        self.create_node(AstNode::Call { head, args }, span_start, span_end)
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

                    let (span_start, span_end) = self.spanning(lhs, rhs);
                    expr_stack.push(self.create_node(
                        AstNode::BinaryOp { lhs, op, rhs },
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

            let (span_start, span_end) = self.spanning(lhs, rhs);

            expr_stack.push(self.create_node(
                AstNode::BinaryOp { lhs, op, rhs },
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
            let span_end = self.position();

            self.create_node(AstNode::Range { lhs: expr, rhs }, span_start, span_end)
        } else if self.is_dot() {
            self.method_call(span_start, expr)
        } else {
            expr
        }
    }

    pub fn number(&mut self) -> NodeId {
        match self.peek() {
            Some(Token {
                token_type: TokenType::Number,
                span_start,
                span_end,
            }) => {
                let contents = &self.results.contents[span_start..span_end];
                let is_float = contents.contains(&b'.');

                self.next();

                if is_float {
                    self.create_node(AstNode::Float, span_start, span_end)
                } else {
                    self.create_node(AstNode::Int, span_start, span_end)
                }
            }
            _ => self.error("expected: number"),
        }
    }

    pub fn boolean(&mut self) -> NodeId {
        match self.peek() {
            Some(Token {
                token_type: TokenType::Name,
                span_start,
                span_end,
            }) => {
                let contents = &self.results.contents[span_start..span_end];

                if contents == b"true" {
                    self.next();
                    self.create_node(AstNode::True, span_start, span_end)
                } else if contents == b"false" {
                    self.next();
                    self.create_node(AstNode::False, span_start, span_end)
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
                token_type,
                span_start,
                span_end,
                ..
            }) => match token_type {
                TokenType::Plus => {
                    self.next();
                    self.create_node(AstNode::Plus, span_start, span_end)
                }
                TokenType::PlusPlus => {
                    self.next();
                    self.create_node(AstNode::Append, span_start, span_end)
                }
                TokenType::Dash => {
                    self.next();
                    self.create_node(AstNode::Minus, span_start, span_end)
                }
                TokenType::Asterisk => {
                    self.next();
                    self.create_node(AstNode::Multiply, span_start, span_end)
                }
                TokenType::ForwardSlash => {
                    self.next();
                    self.create_node(AstNode::Divide, span_start, span_end)
                }
                TokenType::LessThan => {
                    self.next();
                    self.create_node(AstNode::LessThan, span_start, span_end)
                }
                TokenType::LessThanEqual => {
                    self.next();
                    self.create_node(AstNode::LessThanOrEqual, span_start, span_end)
                }
                TokenType::GreaterThan => {
                    self.next();
                    self.create_node(AstNode::GreaterThan, span_start, span_end)
                }
                TokenType::GreaterThanEqual => {
                    self.next();
                    self.create_node(AstNode::GreaterThanOrEqual, span_start, span_end)
                }
                TokenType::EqualsEquals => {
                    self.next();
                    self.create_node(AstNode::Equal, span_start, span_end)
                }
                TokenType::ExclamationEquals => {
                    self.next();
                    self.create_node(AstNode::NotEqual, span_start, span_end)
                }
                TokenType::AsteriskAsterisk => {
                    self.next();
                    self.create_node(AstNode::Pow, span_start, span_end)
                }
                TokenType::AmpersandAmpersand => {
                    self.next();
                    self.create_node(AstNode::And, span_start, span_end)
                }
                TokenType::PipePipe => {
                    self.next();
                    self.create_node(AstNode::Or, span_start, span_end)
                }
                TokenType::Equals => {
                    self.next();
                    self.create_node(AstNode::Assignment, span_start, span_end)
                }
                _ => self.error("expected: operator"),
            },
            _ => self.error("expected: operator"),
        }
    }

    pub fn operator_precedence(&mut self, operator: NodeId) -> usize {
        self.results.ast_nodes[operator.0].precedence()
    }

    pub fn spanning(&mut self, from: NodeId, to: NodeId) -> (usize, usize) {
        (self.results.span_start[from.0], self.results.span_end[to.0])
    }

    pub fn string(&mut self) -> NodeId {
        match self.peek() {
            Some(Token {
                token_type: TokenType::String,
                span_start,
                span_end,
                ..
            }) => {
                self.next();
                self.create_node(AstNode::String, span_start, span_end)
            }
            _ => self.error("expected: string"),
        }
    }

    pub fn name(&mut self) -> NodeId {
        match self.peek() {
            Some(Token {
                token_type: TokenType::Name,
                span_start,
                span_end,
                ..
            }) => {
                self.next();
                self.create_node(AstNode::Name, span_start, span_end)
            }
            _ => self.error("expect name"),
        }
    }

    pub fn typename(&mut self) -> NodeId {
        match self.peek() {
            Some(Token {
                token_type: TokenType::Name,
                span_start,
                span_end,
                ..
            }) => {
                self.next();
                self.create_node(AstNode::Type, span_start, span_end)
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

        self.create_node(AstNode::Params(param_list), span_start, span_end)
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
            let span_start = self.position();
            let name = self.name();
            if self.is_colon() {
                // Optional type
                self.colon();

                let ty = self.name();

                let span_end = self.position();

                params.push(self.create_node(
                    AstNode::Param { name, ty: Some(ty) },
                    span_start,
                    span_end,
                ))
            } else {
                let span_end = self.position();
                params.push(self.create_node(
                    AstNode::Param { name, ty: None },
                    span_start,
                    span_end,
                ))
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
        let span_start = self.position();
        self.keyword(b"if");

        let condition = self.expression();

        let then_block = self.block(true);

        let else_expression = if self.is_keyword(b"else") {
            self.next();
            Some(self.expression())
        } else {
            None
        };
        let span_end = self.position();

        self.create_node(
            AstNode::If {
                condition,
                then_block,
                else_expression,
            },
            span_start,
            span_end,
        )
    }

    pub fn let_statement(&mut self) -> NodeId {
        let mut is_mutable = false;
        let span_start = self.position();

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

        let span_end = self.position();

        self.create_node(
            AstNode::Let {
                variable_name,
                ty,
                initializer,
                is_mutable,
            },
            span_start,
            span_end,
        )
    }

    pub fn while_statement(&mut self) -> NodeId {
        let span_start = self.position();
        self.keyword(b"while");

        let condition = self.expression();
        let block = self.block(true);
        let span_end = self.position();

        self.create_node(AstNode::While { condition, block }, span_start, span_end)
    }

    pub fn for_statement(&mut self) -> NodeId {
        let span_start = self.position();
        self.keyword(b"for");

        let variable = self.variable();
        self.keyword(b"in");

        let range = self.simple_expression();
        let block = self.block(true);
        let span_end = self.position();

        self.create_node(
            AstNode::For {
                variable,
                range,
                block,
            },
            span_start,
            span_end,
        )
    }

    pub fn variable(&mut self) -> NodeId {
        if self.is_name() {
            let name = self
                .next()
                .expect("internal error: missing token that was expected to be there");
            let name_start = name.span_start;
            let name_end = name.span_end;
            self.create_node(AstNode::Variable, name_start, name_end)
        } else {
            self.error("expected variable")
        }
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
                let head = self.create_node(AstNode::Name, name_start, name_end);
                // We're a call
                self.lparen();
                let args = self.args_list();
                self.rparen();

                let span_end = self.position();
                self.create_node(AstNode::Call { head, args }, span_start, span_end)
            } else {
                // We're a variable
                self.create_node(AstNode::Variable, name_start, name_end)
            }
        } else {
            self.error("expected variable or call")
        }
    }

    pub fn keyword(&mut self, keyword: &[u8]) {
        if let Some(Token {
            token_type: TokenType::Name,
            span_start,
            span_end,
        }) = self.peek()
        {
            let contents = &self.results.contents[span_start..span_end];

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
    pub span_start: Vec<usize>,
    pub span_end: Vec<usize>,
    pub ast_nodes: Vec<AstNode>,
    pub contents: Vec<u8>,
}

impl ParseResults {
    pub fn new(node_id_offset: usize, contents: Vec<u8>) -> Self {
        Self {
            node_id_offset,
            span_start: vec![],
            span_end: vec![],
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
                println!(
                    "Let ({}, {}, mutable: {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0], is_mutable
                );
                self.print_helper(variable_name, indent + 2);
                if let Some(ty) = ty {
                    self.print_helper(ty, indent + 2);
                }
                self.print_helper(initializer, indent + 2);
            }
            AstNode::Param { name, ty } => {
                println!(
                    "Param ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(name, indent + 2);
                if let Some(ty) = ty {
                    self.print_helper(ty, indent + 2);
                }
            }
            // AstNode::Closure { params, block } => {
            //     println!(
            //         "Closure ({}, {}):",
            //         self.span_start[node_id.0], self.span_end[node_id.0],
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
                println!(
                    "Block ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                for node in nodes {
                    self.print_helper(node, indent + 2);
                }
            }
            AstNode::Params(nodes) => {
                print!(
                    "Params ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
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
                println!(
                    "Call ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(head, indent + 2);

                for arg in args {
                    self.print_helper(arg, indent + 2);
                }
            }
            AstNode::BinaryOp { lhs, op, rhs } => {
                println!(
                    "BinaryOp ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );

                self.print_helper(lhs, indent + 2);
                self.print_helper(op, indent + 2);
                self.print_helper(rhs, indent + 2)
            }
            AstNode::Range { lhs, rhs } => {
                println!(
                    "Range ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );

                self.print_helper(lhs, indent + 2);
                self.print_helper(rhs, indent + 2)
            }
            AstNode::If {
                condition,
                then_block,
                else_expression,
            } => {
                println!(
                    "If ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(condition, indent + 2);
                self.print_helper(then_block, indent + 2);
                if let Some(else_expression) = else_expression {
                    self.print_helper(else_expression, indent + 2)
                }
            }
            x => {
                println!(
                    "{:?} ({}, {})",
                    x, self.span_start[node_id.0], self.span_end[node_id.0],
                )
            }
        }
    }
}
