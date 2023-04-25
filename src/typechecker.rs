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

impl<'source> TypeChecker<'source> {
    pub fn new(node_count: usize) -> Self {
        let node_types = vec![UNKNOWN_TYPE; node_count];

        Self {
            types: vec![Type::Unknown, Type::Void, Type::I64, Type::F64],
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
                self.typecheck_node(*lhs, delta);
                self.typecheck_node(*rhs, delta);

                let lhs_ty = self.node_types[lhs.0];
                let rhs_ty = self.node_types[rhs.0];
                let lhs = &delta.ast_nodes[lhs.0];
                let op = &delta.ast_nodes[op.0];

                if op == &AstNode::Assignment {
                    // FIXME: replace with compatibility check rather than an equality check
                    if lhs_ty != rhs_ty {
                        self.error("mismatched types during assignment", node_id)
                    }
                    if !matches!(lhs, AstNode::Variable(_)) {
                        self.error("assignment should use a variable on the left side", node_id)
                    }
                    self.node_types[node_id.0] = VOID_TYPE;
                } else {
                    if lhs_ty == I64_TYPE && rhs_ty == I64_TYPE {
                        self.node_types[node_id.0] = I64_TYPE;
                    } else if lhs_ty == F64_TYPE && rhs_ty == F64_TYPE {
                        self.node_types[node_id.0] = F64_TYPE;
                    } else {
                        self.error("mismatch types for operation", node_id)
                    }
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

                self.define_variable(variable_name, delta);

                self.node_types[variable_name.0] = self.node_types[initializer.0];

                self.node_types[node_id.0] = VOID_TYPE;
            }
            AstNode::Variable(unbound_node_id) => self.resolve_variable(unbound_node_id, delta),
            _ => self.error("unsupported ast node in typechecker", node_id),
        }
    }

    pub fn typecheck(&mut self, delta: &'source EngineDelta) {
        if !delta.ast_nodes.is_empty() {
            let last = delta.ast_nodes.len() - 1;
            self.typecheck_node(NodeId(last), delta)
        }
    }

    pub fn define_variable(&mut self, variable_name_node_id: &NodeId, delta: &'source EngineDelta) {
        let variable_name = &delta.contents
            [delta.span_start[variable_name_node_id.0]..delta.span_end[variable_name_node_id.0]];
        self.scope
            .last_mut()
            .expect("internal error: missing expected scope frame")
            .variables
            .insert(variable_name, *variable_name_node_id);
    }

    pub fn resolve_variable(&mut self, unbound_node_id: &NodeId, delta: &'source EngineDelta) {
        let variable_name =
            &delta.contents[delta.span_start[unbound_node_id.0]..delta.span_end[unbound_node_id.0]];

        if let Some(node_id) = self.find_variable(variable_name) {
            self.variable_def.insert(*unbound_node_id, node_id);
            self.node_types[unbound_node_id.0] = self.node_types[node_id.0];
        } else {
            self.error("variable not found", *unbound_node_id)
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

    pub fn enter_scope(&mut self) {
        self.scope.push(Scope::new());
    }

    pub fn exit_scope(&mut self) {
        self.scope.pop();
    }
}

pub enum Type {
    Unknown,
    Void,
    I64,
    F64,
    Fn(Vec<TypeId>, TypeId),
}
