use std::{collections::HashMap, fmt};

use crate::{
    engine::{ExternalFnRecord, PermanentDefinitions},
    errors::{ErrorBatch, ScriptError},
    parser::{AstNode, NodeId, ParseResults},
    Type, Value,
};

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "lsp", derive(serde::Serialize, serde::Deserialize))]
pub struct TypeId(pub usize);

impl fmt::Debug for TypeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("TypeId").field(&self.0).finish()?;
        match *self {
            F64_TYPE => f.write_str(" (f64)")?,
            I64_TYPE => f.write_str(" (i64)")?,
            BOOL_TYPE => f.write_str(" (bool)")?,
            UNIT_TYPE => f.write_str(" (unit aka `()`)")?,
            STRING_TYPE => f.write_str(" (String)")?,
            UNKNOWN_TYPE => f.write_str(" (Unknown type)")?,
            _ => (),
        }
        Ok(())
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct ScopeId(pub usize);

pub struct Scope {
    pub variables: HashMap<Vec<u8>, NodeId>,
    pub node_id: NodeId,
}

impl Scope {
    pub fn new(node_id: NodeId) -> Self {
        Self {
            variables: HashMap::new(),
            node_id,
        }
    }
}

#[cfg(feature = "async")]
#[derive(Default)]
pub enum Function {
    ExternalFn0(Box<dyn Fn() -> Result<Value, String>>),
    ExternalFn1(Box<dyn Fn(&mut Value) -> Result<Value, String>>),
    ExternalFn2(Box<dyn Fn(&mut Value, &mut Value) -> Result<Value, String>>),
    ExternalFn3(Box<dyn Fn(&mut Value, &mut Value, &mut Value) -> Result<Value, String>>),
    ExternalFn4(
        Box<dyn Fn(&mut Value, &mut Value, &mut Value, &mut Value) -> Result<Value, String>>,
    ),
    ExternalAsyncFn0(fn() -> futures::future::BoxFuture<'static, Result<Value, String>>),
    ExternalAsyncFn1(
        for<'a> fn(&'a mut Value) -> futures::future::BoxFuture<'a, Result<Value, String>>,
    ),
    ExternalAsyncFn2(
        for<'a> fn(
            &'a mut Value,
            &'a mut Value,
        ) -> futures::future::BoxFuture<'a, Result<Value, String>>,
    ),
    ExternalAsyncFn3(
        for<'a> fn(
            &'a mut Value,
            &'a mut Value,
            &'a mut Value,
        ) -> futures::future::BoxFuture<'a, Result<Value, String>>,
    ),
    ExternalAsyncFn4(
        for<'a> fn(
            &'a mut Value,
            &'a mut Value,
            &'a mut Value,
            &'a mut Value,
        ) -> futures::future::BoxFuture<'a, Result<Value, String>>,
    ),
    #[default]
    RemoteFn,
}

#[cfg(not(feature = "async"))]
#[derive(Default)]
pub enum Function {
    ExternalFn0(Box<dyn Fn() -> Result<Value, String>>),
    ExternalFn1(Box<dyn Fn(&mut Value) -> Result<Value, String>>),
    ExternalFn2(Box<dyn Fn(&mut Value, &mut Value) -> Result<Value, String>>),
    ExternalFn3(Box<dyn Fn(&mut Value, &mut Value, &mut Value) -> Result<Value, String>>),
    ExternalFn4(
        Box<dyn Fn(&mut Value, &mut Value, &mut Value, &mut Value) -> Result<Value, String>>,
    ),
    #[default]
    RemoteFn,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "lsp", derive(serde::Serialize, serde::Deserialize))]
pub struct ExternalFunctionId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(pub usize);

pub struct Variable {
    type_id: TypeId,
    is_mutable: bool,
}

pub struct TypeChecker<'permanent> {
    // The globally registered definitions that are available before
    // we start typechecking the current script
    pub permanent_definitions: &'permanent PermanentDefinitions,

    // Based on NodeId
    pub node_types: Vec<TypeId>,

    // Parser results to refer to
    pub parse_results: ParseResults,

    // Bindings from use to def
    pub variable_def_site: HashMap<NodeId, NodeId>,

    // Mapping betwen definition node id and the full variable definition
    pub variable_info: HashMap<NodeId, Variable>,

    // List of local functions
    // note: local functions not yet supported
    // pub local_functions: Vec<ExternalFnRecord>,

    // Call resolution
    pub call_resolution: HashMap<NodeId, ExternalFunctionId>,

    pub errors: ErrorBatch,
    pub scope_stack: Vec<ScopeId>,
    pub scope: Vec<Scope>,
}

// PLEASE NOTE: STRING_TYPE is considered last and any type after this is considered a user-defined datatype
pub const UNIT_TYPE: TypeId = TypeId(0);
pub const I64_TYPE: TypeId = TypeId(1);
pub const F64_TYPE: TypeId = TypeId(2);
pub const BOOL_TYPE: TypeId = TypeId(3);
pub const STRING_TYPE: TypeId = TypeId(4); // <-- last known builtin type id (after this, assume user-defined).
                                           // Please: keep this last and insert new built-in types above it, making sure
                                           // to adjust STRING_TYPE's type id. TODO update this to
                                           // change the assumption to which types live on the heap
                                           // and need to be leaked after usage to prevent drops,
                                           // confirm there is no other behavior based on the
                                           // assumption of last builtin
pub const UNKNOWN_TYPE: TypeId = TypeId(usize::MAX);

impl<'permanent> TypeChecker<'permanent> {
    pub fn new(
        parse_results: ParseResults,
        permanent_definitions: &'permanent PermanentDefinitions,
    ) -> Self {
        Self {
            errors: ErrorBatch::empty(),

            parse_results,

            node_types: vec![],
            variable_def_site: HashMap::new(),
            variable_info: HashMap::new(),

            call_resolution: HashMap::new(),

            scope: vec![],
            scope_stack: vec![],

            permanent_definitions,
        }
    }

    pub fn error(&mut self, message: impl Into<String>, node_id: NodeId) {
        let span = self.parse_results.spans[node_id.0];

        self.errors.push(ScriptError {
            message: message.into(),
            span,
        })
    }

    pub fn reference_of(&self, ref_type_id: TypeId, type_id: TypeId) -> bool {
        if let Some(tid) = self
            .permanent_definitions
            .reference_of_map
            .get(&ref_type_id)
        {
            tid == &type_id
        } else {
            false
        }
    }

    pub fn typecheck_node(&mut self, node_id: NodeId) {
        match &self.parse_results.ast_nodes[node_id.0] {
            AstNode::Int => {
                self.node_types[node_id.0] = I64_TYPE;
            }
            AstNode::Float => {
                self.node_types[node_id.0] = F64_TYPE;
            }
            AstNode::String => {
                self.node_types[node_id.0] = STRING_TYPE;
            }
            AstNode::BinaryOp { lhs, op, rhs } => {
                self.typecheck_binop(*lhs, *op, *rhs, node_id);
            }
            AstNode::Statement(node) => {
                self.typecheck_node(*node);
                self.node_types[node_id.0] = UNIT_TYPE;
            }
            AstNode::Block(nodes) => {
                if nodes.is_empty() {
                    self.node_types[node_id.0] = UNIT_TYPE;
                } else {
                    // FIXME: clone to get around ownership issue
                    let nodes = nodes.clone();

                    self.enter_scope(node_id);
                    // FIXME: grab the last one if it's an expression
                    let mut type_id = UNIT_TYPE;
                    for node_id in nodes {
                        self.typecheck_node(node_id);

                        type_id = self.node_types[node_id.0];
                    }
                    self.node_types[node_id.0] = type_id;
                    self.exit_scope();
                }
            }
            AstNode::Type => {
                let span = self.parse_results.spans[node_id.0];

                let contents = self.parse_results.contents_for_span(span);

                match contents {
                    b"i64" => self.node_types[node_id.0] = I64_TYPE,
                    b"f64" => self.node_types[node_id.0] = F64_TYPE,
                    b"bool" => self.node_types[node_id.0] = BOOL_TYPE,
                    b"String" => self.node_types[node_id.0] = STRING_TYPE,
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
            } => self.typecheck_let(*variable_name, *ty, *initializer, *is_mutable, node_id),
            AstNode::Variable => self.resolve_variable(node_id),
            AstNode::If {
                condition,
                then_block,
                else_expression,
            } => self.typecheck_if(*condition, *then_block, *else_expression, node_id),
            AstNode::While { condition, block } => {
                self.typecheck_while(*condition, *block, node_id)
            }
            // AstNode::For {
            //     variable,
            //     range,
            //     block,
            // } => self.typecheck_for(*variable, *range, *block, node_id, parse_results),
            AstNode::True => self.node_types[node_id.0] = BOOL_TYPE,
            AstNode::False => self.node_types[node_id.0] = BOOL_TYPE,
            // AstNode::Range { lhs, rhs } => self.typecheck_range(*lhs, *rhs, node_id, parse_results),
            AstNode::Call { head, args } => {
                // FIXME: clone to get around ownership issue
                self.typecheck_call(*head, &args.clone(), node_id)
            }
            AstNode::Await(inner_node_id) => {
                let inner_node_id = *inner_node_id;

                println!(
                    "checking: {:?}",
                    self.parse_results.ast_nodes[inner_node_id.0]
                );
                self.typecheck_node(inner_node_id);

                let inner_type_id = self.node_types[inner_node_id.0];

                if let Some(value) = self.permanent_definitions.future_of_map.get(&inner_type_id) {
                    self.node_types[node_id.0] = *value;
                } else {
                    self.error(
                        format!(
                            "expected future type for .await, found {}",
                            &self.permanent_definitions.typenames[inner_type_id.0]
                        ),
                        node_id,
                    )
                }
            }
            _ => self.error("unsupported ast node in typechecker", node_id),
        }
    }

    pub fn typecheck(&mut self) -> Result<(), ErrorBatch> {
        if !self.parse_results.ast_nodes.is_empty() {
            self.node_types = vec![UNKNOWN_TYPE; self.parse_results.ast_nodes.len()];

            let last = self.parse_results.ast_nodes.len() - 1;

            let last_node_id = NodeId(last);

            self.scope.push(Scope::new(last_node_id));
            self.scope_stack.push(ScopeId(0));

            self.typecheck_node(NodeId(last));
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    pub fn typecheck_let(
        &mut self,
        variable_name: NodeId,
        ty: Option<NodeId>,
        initializer: NodeId,
        is_mutable: bool,
        node_id: NodeId,
    ) {
        self.typecheck_node(initializer);

        if let Some(ty) = ty {
            self.typecheck_node(ty);

            // TODO make this a compatibility check rather than equality check
            if self.node_types[ty.0] != self.node_types[initializer.0] {
                self.error("initializer does not match declared type", initializer)
            }
        }

        self.define_variable(
            variable_name,
            if let Some(ty) = ty {
                self.node_types[ty.0]
            } else {
                self.node_types[initializer.0]
            },
            is_mutable,
        );

        self.node_types[variable_name.0] = self.node_types[initializer.0];

        self.node_types[node_id.0] = UNIT_TYPE;
    }

    pub fn typecheck_if(
        &mut self,
        condition: NodeId,
        then_block: NodeId,
        else_expression: Option<NodeId>,
        node_id: NodeId,
    ) {
        self.typecheck_node(condition);
        let condition_ty = self.node_types[condition.0];

        if condition_ty != BOOL_TYPE {
            self.error("expected bool for if condition", condition);
        }

        self.typecheck_node(then_block);
        let then_ty = self.node_types[then_block.0];

        if let Some(else_expression) = else_expression {
            self.typecheck_node(else_expression);
            let else_ty = self.node_types[else_expression.0];

            if then_ty != else_ty {
                self.error("then and else output different types", else_expression)
            }
        }

        self.node_types[node_id.0] = then_ty
    }

    pub fn typecheck_while(&mut self, condition: NodeId, block: NodeId, node_id: NodeId) {
        self.typecheck_node(condition);
        let condition_ty = self.node_types[condition.0];

        if condition_ty != BOOL_TYPE {
            self.error("expected bool for while condition", condition);
        }

        self.typecheck_node(block);

        self.node_types[node_id.0] = UNIT_TYPE;
    }

    // pub fn typecheck_for(
    //     &mut self,
    //     variable_name: NodeId,
    //     range: NodeId,
    //     block: NodeId,
    //     node_id: NodeId,
    //     parse_results: &ParseResults,
    // ) {
    //     self.typecheck_node(range, parse_results);
    //     let range_ty = self.node_types[range.0];

    //     let range_inner_ty = match &self.types[range_ty.0] {
    //         Type::Range(range_inner_ty) => *range_inner_ty,
    //         _ => {
    //             self.error("expected range value in for loop", range);
    //             UNKNOWN_TYPE
    //         }
    //     };

    //     self.typecheck_node(block, parse_results);

    //     self.define_variable(variable_name, parse_results);

    //     self.node_types[variable_name.0] = range_inner_ty;

    //     self.node_types[node_id.0] = VOID_TYPE;
    // }

    pub fn typecheck_binop(
        &mut self,
        lhs: NodeId,
        op: NodeId,
        rhs: NodeId,
        node_id: NodeId, // whole expression NodeId
    ) {
        self.typecheck_node(lhs);
        self.typecheck_node(rhs);

        let lhs_ty = self.node_types[lhs.0];
        let rhs_ty = self.node_types[rhs.0];
        let op_ast = &self.parse_results.ast_nodes[op.0];

        match op_ast {
            AstNode::Assignment => {
                // FIXME: replace with compatibility check rather than an equality check
                if lhs_ty != rhs_ty {
                    self.error("mismatched types during assignment", node_id)
                }
                let lhs_ast = &self.parse_results.ast_nodes[lhs.0];

                if !matches!(lhs_ast, AstNode::Variable) {
                    self.error("assignment should use a variable on the left side", node_id)
                } else if let Some(definition_id) = self.variable_def_site.get(&lhs) {
                    if let Some(variable) = self.variable_info.get(definition_id) {
                        if !variable.is_mutable {
                            self.error("assignment to immutable variable", lhs)
                        }
                    } else {
                        self.error(
                            "internal error: resolved variable missing variable information",
                            node_id,
                        )
                    }
                } else {
                    self.error(
                        "internal error: variable not resolved to a variable definition",
                        node_id,
                    )
                }
                self.node_types[node_id.0] = UNIT_TYPE;
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
            AstNode::And | AstNode::Or => {
                if lhs_ty == BOOL_TYPE && rhs_ty == BOOL_TYPE {
                    self.node_types[node_id.0] = BOOL_TYPE;
                } else {
                    self.error("boolean operator expects boolean types", node_id)
                }
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

    pub fn with<F>(&mut self, f: F)
    where
        F: Fn(&mut TypeChecker),
    {
        f(self)
    }

    // pub fn typecheck_range(
    //     &mut self,
    //     lhs: NodeId,
    //     rhs: NodeId,
    //     node_id: NodeId,
    //     parse_results: &ParseResults,
    // ) {
    //     self.typecheck_node(lhs, parse_results);
    //     self.typecheck_node(rhs, parse_results);

    //     let lhs_ty = self.node_types[lhs.0];
    //     let rhs_ty = self.node_types[rhs.0];

    //     // For now, require both sides to be i64
    //     if lhs_ty != I64_TYPE {
    //         self.error("expected i64 for range", lhs)
    //     }

    //     if rhs_ty != I64_TYPE {
    //         self.error("expected i64 for range", rhs)
    //     }

    //     let type_id = self.create_or_find_type(Type::Range(I64_TYPE));

    //     self.node_types[node_id.0] = type_id
    // }
    // pub fn create_or_find_type(&mut self, ty: std::any::TypeId) -> TypeId {
    //     let mut idx = 0;
    //     while idx < self.permanent_definitions.types.len() {
    //         if self.permanent_definitions.types[idx] == ty {
    //             return TypeId(idx);
    //         }
    //         idx += 1;
    //     }
    //     self.permanent_definitions.types.push(ty);

    //     TypeId(self.permanent_definitions.types.len() - 1)
    // }

    pub fn typecheck_call(&mut self, head: NodeId, args: &[NodeId], node_id: NodeId) {
        for node_id in args {
            self.typecheck_node(*node_id)
        }

        let call_name = self
            .parse_results
            .contents_for_span(self.parse_results.spans[head.0]);

        if let Some(defs) = self.permanent_definitions.external_functions.get(call_name) {
            'outer: for &def in defs {
                let ExternalFnRecord { params, ret, .. } =
                    &self.permanent_definitions.functions[def.0];
                {
                    if args.len() != params.len() {
                        continue;
                    }

                    for idx in 0..params.len() {
                        let param = params[idx];
                        let arg = args[idx];

                        if self.node_types[arg.0] != param
                            && !self.reference_of(param, self.node_types[arg.0])
                        {
                            // Types don't match here, and we don't have a reference we can work with
                            // so we need to continue looking
                            continue 'outer;
                        }
                    }

                    self.node_types[node_id.0] = *ret;
                    self.call_resolution.insert(head, def);
                    self.call_resolution.insert(node_id, def);
                    return;
                }
            }

            let mut sig = String::from_utf8_lossy(call_name).to_string();
            sig.push('(');
            let mut first = true;
            for arg in args {
                if !first {
                    sig.push_str(", ")
                } else {
                    first = false;
                }

                sig.push_str(&self.stringify_type(self.node_types[arg.0]));
            }
            sig.push(')');
            self.error(
                format!("could not find compatible function for {}", sig),
                node_id,
            )
        } else {
            let name = String::from_utf8_lossy(call_name);
            self.error(format!("unknown function '{}'", name), node_id)
        }
    }

    pub fn is_custom_type(type_id: TypeId) -> bool {
        type_id > STRING_TYPE && type_id != UNKNOWN_TYPE
    }

    pub fn define_variable(
        &mut self,
        variable_name_node_id: NodeId,
        type_id: TypeId,
        is_mutable: bool,
    ) {
        let span = self.parse_results.spans[variable_name_node_id.0];
        let variable_name = &self.parse_results.contents[span.start..span.end];

        let current_scope_id = self
            .scope_stack
            .last()
            .expect("internal error: missing scope frame");
        self.scope[current_scope_id.0]
            .variables
            .insert(variable_name.to_vec(), variable_name_node_id);

        // Let the reference of the variable in the declaration also point to this
        // def site. That will allow things like find-all-refs to work later because
        // the variable in the declaration also counds as a use.
        self.variable_def_site
            .insert(variable_name_node_id, variable_name_node_id);

        self.variable_info.insert(
            variable_name_node_id,
            Variable {
                type_id,
                is_mutable,
            },
        );
    }

    pub fn resolve_variable(&mut self, unbound_node_id: NodeId) {
        let span = self.parse_results.spans[unbound_node_id.0];
        let variable_name = self.parse_results.contents_for_span(span);

        if let Some(node_id) = self.find_variable(variable_name) {
            self.variable_def_site.insert(unbound_node_id, node_id);
            if let Some(variable) = self.variable_info.get(&node_id) {
                self.node_types[unbound_node_id.0] = variable.type_id;
            } else {
                self.error(
                    "internal error: resolved variable missing variable information",
                    unbound_node_id,
                )
            }
        } else {
            self.error("variable not found", unbound_node_id)
        }
    }

    pub fn find_variable(&self, variable_name: &[u8]) -> Option<NodeId> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(result) = self.scope[scope.0].variables.get(variable_name) {
                return Some(*result);
            }
        }

        None
    }

    pub fn enter_scope(&mut self, node_id: NodeId) {
        self.scope.push(Scope::new(node_id));
        self.scope_stack.push(ScopeId(self.scope.len() - 1));
    }

    pub fn exit_scope(&mut self) {
        self.scope_stack.pop();
    }

    // Debug functionality
    pub fn print_node_types(&self) {
        let mut idx = 0;
        while idx < self.node_types.len() {
            println!("{}: {}", idx, self.stringify_type(self.node_types[idx]));
            idx += 1;
        }
    }

    pub fn get_type<T>(&self) -> Option<TypeId>
    where
        T: Type,
    {
        // Since the user can't create types in the script (currently), defer
        // type lookup to the permanent state
        self.permanent_definitions.get_type::<T>()
    }

    pub fn stringify_type(&self, type_id: TypeId) -> String {
        if type_id == UNKNOWN_TYPE {
            String::from("<UNKNOWN TYPE>")
        } else {
            self.permanent_definitions.typenames[type_id.0].clone()
        }
    }

    pub fn stringify_function_name(&self, name: &[u8], function_id: ExternalFunctionId) -> String {
        let fun_def = &self.permanent_definitions.functions[function_id.0];

        let mut fun_name = String::from_utf8_lossy(name).to_string();

        for param in &fun_def.params {
            fun_name.push_str("__");
            fun_name.push_str(&self.stringify_type(*param));
        }
        fun_name
    }

    pub fn pretty_function_signature(
        &self,
        name: &[u8],
        function_id: ExternalFunctionId,
    ) -> String {
        let external_fun = &self.permanent_definitions.functions[function_id.0];

        let mut fun_signature = format!("{}(", String::from_utf8_lossy(name));

        let mut first = true;
        for param in &external_fun.params {
            if !first {
                fun_signature.push_str(", ");
            } else {
                first = false;
            }

            fun_signature.push_str(&self.stringify_type(*param));
        }

        fun_signature.push_str(&format!(") -> {}", self.stringify_type(external_fun.ret)));

        fun_signature
    }
}
