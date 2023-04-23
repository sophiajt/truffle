use crate::{
    delta::EngineDelta,
    errors::ScriptError,
    parser::{AstNode, NodeId},
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TypeId(pub usize);

pub struct TypeChecker {
    pub types: Vec<Type>,
    pub errors: Vec<ScriptError>,

    pub node_types: Vec<TypeId>,
}

pub const UNKNOWN_TYPE: TypeId = TypeId(0);
pub const VOID_TYPE: TypeId = TypeId(1);
pub const I64_TYPE: TypeId = TypeId(2);
pub const F64_TYPE: TypeId = TypeId(3);

impl TypeChecker {
    pub fn new(node_count: usize) -> Self {
        let node_types = vec![UNKNOWN_TYPE; node_count];

        Self {
            types: vec![Type::Unknown, Type::Void, Type::I64, Type::F64],
            errors: vec![],

            node_types,
        }
    }

    pub fn error(&mut self, message: impl Into<String>, node_id: NodeId) {
        self.errors.push(ScriptError {
            message: message.into(),
            node_id,
        })
    }

    pub fn typecheck_node(&mut self, node_id: NodeId, delta: &EngineDelta) {
        match &delta.ast_nodes[node_id.0] {
            AstNode::Int => {
                self.node_types[node_id.0] = I64_TYPE;
            }
            AstNode::BinaryOp { lhs, rhs, .. } => {
                self.typecheck_node(*lhs, delta);
                self.typecheck_node(*rhs, delta);

                let lhs_ty = self.node_types[lhs.0];
                let rhs_ty = self.node_types[rhs.0];

                if lhs_ty == I64_TYPE && rhs_ty == I64_TYPE {
                    self.node_types[node_id.0] = I64_TYPE;
                } else if lhs_ty == F64_TYPE && rhs_ty == F64_TYPE {
                    self.node_types[node_id.0] = F64_TYPE;
                } else {
                    self.error("mismatch types for operation", node_id)
                }
            }
            AstNode::Statement(node) => {
                self.typecheck_node(*node, delta);
                self.node_types[node_id.0] = VOID_TYPE;
            }
            AstNode::Block(nodes) => {
                if nodes.is_empty() {
                    self.node_types[node_id.0] = VOID_TYPE;
                } else {
                    // FIXME: grab the last one if it's an expression
                    let mut type_id = UNKNOWN_TYPE;
                    for node_id in nodes {
                        self.typecheck_node(*node_id, delta);

                        type_id = self.node_types[node_id.0];
                    }
                    self.node_types[node_id.0] = type_id;
                }
            }
            _ => self.error("unsupported ast node in typechecker", node_id),
        }
    }

    pub fn typecheck(&mut self, delta: &EngineDelta) {
        if !delta.ast_nodes.is_empty() {
            let last = delta.ast_nodes.len() - 1;
            self.typecheck_node(NodeId(last), delta)
        }
    }
}

pub enum Type {
    Unknown,
    Void,
    I64,
    F64,
    Fn(Vec<TypeId>, TypeId),
}
