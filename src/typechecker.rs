use std::collections::HashMap;

use crate::{
    delta::EngineDelta,
    errors::ScriptError,
    parser::{AstNode, NodeId},
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TypeId(pub usize);

pub struct Scope<'source> {
    pub variables: HashMap<&'source [u8], NodeId>,
}

impl<'scope> Scope<'scope> {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }
}

pub struct TypeChecker<'source> {
    // Used by TypeId
    pub types: Vec<Type>,

    // Based on NodeId
    pub node_types: Vec<TypeId>,

    // Bindings from use to def
    pub variable_def: HashMap<NodeId, NodeId>,

    pub errors: Vec<ScriptError>,
    pub scope: Vec<Scope<'source>>,
}

pub const UNKNOWN_TYPE: TypeId = TypeId(0);
pub const VOID_TYPE: TypeId = TypeId(1);
pub const I64_TYPE: TypeId = TypeId(2);
pub const F64_TYPE: TypeId = TypeId(3);
pub const BOOL_TYPE: TypeId = TypeId(4);

impl<'source> TypeChecker<'source> {
    pub fn new(node_count: usize) -> Self {
        let node_types = vec![UNKNOWN_TYPE; node_count];

        Self {
            types: vec![Type::Unknown, Type::Void, Type::I64, Type::F64, Type::Bool],
            errors: vec![],

            node_types,
            variable_def: HashMap::new(),

            scope: vec![Scope::new()],
        }
    }

    pub fn error(&mut self, message: impl Into<String>, node_id: NodeId) {
        self.errors.push(ScriptError {
            message: message.into(),
            node_id,
        })
    }

    pub fn typecheck_node(&mut self, node_id: NodeId, delta: &'source EngineDelta) {
        match &delta.ast_nodes[node_id.0] {
            AstNode::Int => {
                self.node_types[node_id.0] = I64_TYPE;
            }
            AstNode::BinaryOp { lhs, op, rhs } => {
                self.typecheck_binop(*lhs, *op, *rhs, node_id, delta);
            }
            AstNode::Statement(node) => {
                self.typecheck_node(*node, delta);
                self.node_types[node_id.0] = VOID_TYPE;
            }
            AstNode::Block(nodes) => {
                if nodes.is_empty() {
                    self.node_types[node_id.0] = VOID_TYPE;
                } else {
                    self.enter_scope();
                    // FIXME: grab the last one if it's an expression
                    let mut type_id = UNKNOWN_TYPE;
                    for node_id in nodes {
                        self.typecheck_node(*node_id, delta);

                        type_id = self.node_types[node_id.0];
                    }
                    self.node_types[node_id.0] = type_id;
                    self.exit_scope();
                }
            }
            AstNode::Type => {
                let span_start = delta.span_start[node_id.0];
                let span_end = delta.span_end[node_id.0];

                let contents = &delta.contents[span_start..span_end];

                match contents {
                    b"i64" => self.node_types[node_id.0] = I64_TYPE,
                    b"f64" => self.node_types[node_id.0] = F64_TYPE,
                    _ => self.error(
                        format!("unknown type: {}", String::from_utf8_lossy(contents)),
                        node_id,
                    ),
                }
            }
            AstNode::Let {
                variable_name,
                ty,
                initializer,
                is_mutable,
            } => {
                self.typecheck_node(*initializer, delta);

                if let Some(ty) = ty {
                    self.typecheck_node(*ty, delta);

                    // TODO make this a compatibility check rather than equality check
                    if self.node_types[ty.0] != self.node_types[initializer.0] {
                        self.error("initializer does not match declared type", *initializer)
                    }
                }

                self.define_variable(*variable_name, delta);

                self.node_types[variable_name.0] = self.node_types[initializer.0];

                self.node_types[node_id.0] = VOID_TYPE;
            }
            AstNode::Variable => self.resolve_variable(node_id, delta),
            AstNode::If {
                condition,
                then_block,
                else_expression,
            } => self.typecheck_if(*condition, *then_block, *else_expression, node_id, delta),
            AstNode::True => self.node_types[node_id.0] = BOOL_TYPE,
            AstNode::False => self.node_types[node_id.0] = BOOL_TYPE,
            AstNode::Range { lhs, rhs } => self.typecheck_range(*lhs, *rhs, node_id, delta),
            _ => self.error("unsupported ast node in typechecker", node_id),
        }
    }

    pub fn typecheck(&mut self, delta: &'source EngineDelta) {
        if !delta.ast_nodes.is_empty() {
            let last = delta.ast_nodes.len() - 1;
            self.typecheck_node(NodeId(last), delta)
        }
    }

    pub fn typecheck_if(
        &mut self,
        condition: NodeId,
        then_block: NodeId,
        else_expression: Option<NodeId>,
        node_id: NodeId,
        delta: &'source EngineDelta,
    ) {
        self.typecheck_node(condition, delta);
        let condition_ty = self.node_types[condition.0];

        if condition_ty != BOOL_TYPE {
            self.error("expected bool for if condition", condition);
        }

        self.typecheck_node(then_block, delta);
        let then_ty = self.node_types[then_block.0];

        if let Some(else_expression) = else_expression {
            self.typecheck_node(else_expression, delta);
            let else_ty = self.node_types[else_expression.0];

            if then_ty != else_ty {
                self.error("then and else output different types", else_expression)
            }
        }

        self.node_types[node_id.0] = then_ty
    }

    pub fn typecheck_binop(
        &mut self,
        lhs: NodeId,
        op: NodeId,
        rhs: NodeId,
        node_id: NodeId, // whole expression NodeId
        delta: &'source EngineDelta,
    ) {
        self.typecheck_node(lhs, delta);
        self.typecheck_node(rhs, delta);

        let lhs_ty = self.node_types[lhs.0];
        let rhs_ty = self.node_types[rhs.0];
        let lhs = &delta.ast_nodes[lhs.0];
        let op = &delta.ast_nodes[op.0];

        match op {
            AstNode::Assignment => {
                // FIXME: replace with compatibility check rather than an equality check
                if lhs_ty != rhs_ty {
                    self.error("mismatched types during assignment", node_id)
                }
                if !matches!(lhs, AstNode::Variable) {
                    self.error("assignment should use a variable on the left side", node_id)
                }
                self.node_types[node_id.0] = VOID_TYPE;
            }
            AstNode::LessThan
            | AstNode::LessThanOrEqual
            | AstNode::GreaterThan
            | AstNode::GreaterThanOrEqual => {
                if (lhs_ty == I64_TYPE && rhs_ty == I64_TYPE)
                    || (lhs_ty == F64_TYPE && rhs_ty == F64_TYPE)
                {
                    self.node_types[node_id.0] = BOOL_TYPE;
                } else {
                    self.error("mismatch types for operation", node_id)
                }
            }
            AstNode::Equal | AstNode::NotEqual => {
                self.node_types[node_id.0] = BOOL_TYPE;
            }
            _ => {
                if lhs_ty == I64_TYPE && rhs_ty == I64_TYPE {
                    self.node_types[node_id.0] = I64_TYPE;
                } else if lhs_ty == F64_TYPE && rhs_ty == F64_TYPE {
                    self.node_types[node_id.0] = F64_TYPE;
                } else {
                    self.error("mismatch types for operation", node_id)
                }
            }
        }
    }

    pub fn typecheck_range(
        &mut self,
        lhs: NodeId,
        rhs: NodeId,
        node_id: NodeId,
        delta: &'source EngineDelta,
    ) {
        self.typecheck_node(lhs, delta);
        self.typecheck_node(rhs, delta);

        let lhs_ty = self.node_types[lhs.0];
        let rhs_ty = self.node_types[rhs.0];

        // For now, require both sides to be i64
        if lhs_ty != I64_TYPE {
            self.error("expected i64 for range", lhs)
        }

        if rhs_ty != I64_TYPE {
            self.error("expected i64 for range", rhs)
        }

        let type_id = self.create_or_find_type(Type::Range(I64_TYPE));

        self.node_types[node_id.0] = type_id
    }

    pub fn define_variable(&mut self, variable_name_node_id: NodeId, delta: &'source EngineDelta) {
        let variable_name = &delta.contents
            [delta.span_start[variable_name_node_id.0]..delta.span_end[variable_name_node_id.0]];
        self.scope
            .last_mut()
            .expect("internal error: missing expected scope frame")
            .variables
            .insert(variable_name, variable_name_node_id);
    }

    pub fn resolve_variable(&mut self, unbound_node_id: NodeId, delta: &'source EngineDelta) {
        let variable_name =
            &delta.contents[delta.span_start[unbound_node_id.0]..delta.span_end[unbound_node_id.0]];

        if let Some(node_id) = self.find_variable(variable_name) {
            self.variable_def.insert(unbound_node_id, node_id);
            self.node_types[unbound_node_id.0] = self.node_types[node_id.0];
        } else {
            self.error("variable not found", unbound_node_id)
        }
    }

    pub fn find_variable(&mut self, variable_name: &[u8]) -> Option<NodeId> {
        for scope in self.scope.iter().rev() {
            if let Some(result) = scope.variables.get(variable_name) {
                return Some(*result);
            }
        }

        None
    }

    pub fn create_or_find_type(&mut self, ty: Type) -> TypeId {
        let mut idx = 0;
        while idx < self.types.len() {
            if self.types[idx] == ty {
                return TypeId(idx);
            }
            idx += 1;
        }
        self.types.push(ty);

        TypeId(self.types.len() - 1)
    }

    pub fn enter_scope(&mut self) {
        self.scope.push(Scope::new());
    }

    pub fn exit_scope(&mut self) {
        self.scope.pop();
    }
}

#[derive(PartialEq)]
pub enum Type {
    Unknown,
    Void,
    I64,
    F64,
    Bool,
    Range(TypeId),
    Fn(Vec<TypeId>, TypeId),
}
