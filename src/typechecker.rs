use std::{any::Any, collections::HashMap};

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

pub enum Function {
    // ExternalFn0(Box<dyn Fn() -> Result<Box<dyn Any>, String>>),
    ExternalFn1(Box<dyn Fn(&mut Box<dyn Any>) -> Result<Box<dyn Any>, String>>),
    ExternalFn2(Box<dyn Fn(&mut Box<dyn Any>, &mut Box<dyn Any>) -> Result<Box<dyn Any>, String>>),
    // InternalFn,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(pub usize);

pub struct TypeChecker<'source> {
    // Used by TypeId
    pub types: Vec<std::any::TypeId>,

    // Names of each types
    pub typenames: Vec<String>,

    // Based on NodeId
    pub node_types: Vec<TypeId>,

    // Bindings from use to def
    pub variable_def: HashMap<NodeId, NodeId>,

    // List of all registered functions
    pub functions: Vec<FnRecord>,

    // Call resolution
    pub call_resolution: HashMap<NodeId, FunctionId>,

    // Externally-registered functions
    pub external_functions: HashMap<Vec<u8>, Vec<FunctionId>>,

    pub errors: Vec<ScriptError>,
    pub scope: Vec<Scope<'source>>,
}

pub const VOID_TYPE: TypeId = TypeId(0);
pub const I64_TYPE: TypeId = TypeId(1);
pub const F64_TYPE: TypeId = TypeId(2);
pub const BOOL_TYPE: TypeId = TypeId(3);

impl<'source> TypeChecker<'source> {
    pub fn new() -> Self {
        Self {
            types: vec![
                std::any::TypeId::of::<()>(),
                std::any::TypeId::of::<i64>(),
                std::any::TypeId::of::<f64>(),
                std::any::TypeId::of::<bool>(),
            ],
            typenames: vec![
                "void".into(), //std::any::type_name::<()>().to_string(),
                std::any::type_name::<i64>().to_string(),
                std::any::type_name::<f64>().to_string(),
                std::any::type_name::<bool>().to_string(),
            ],
            errors: vec![],

            node_types: vec![],
            variable_def: HashMap::new(),

            call_resolution: HashMap::new(),
            external_functions: HashMap::new(),

            functions: vec![],

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
            AstNode::Float => {
                self.node_types[node_id.0] = F64_TYPE;
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
                    let mut type_id = VOID_TYPE;
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
                ..
            } => self.typecheck_let(*variable_name, *ty, *initializer, node_id, delta),
            AstNode::Variable => self.resolve_variable(node_id, delta),
            AstNode::If {
                condition,
                then_block,
                else_expression,
            } => self.typecheck_if(*condition, *then_block, *else_expression, node_id, delta),
            AstNode::While { condition, block } => {
                self.typecheck_while(*condition, *block, node_id, delta)
            }
            // AstNode::For {
            //     variable,
            //     range,
            //     block,
            // } => self.typecheck_for(*variable, *range, *block, node_id, delta),
            AstNode::True => self.node_types[node_id.0] = BOOL_TYPE,
            AstNode::False => self.node_types[node_id.0] = BOOL_TYPE,
            // AstNode::Range { lhs, rhs } => self.typecheck_range(*lhs, *rhs, node_id, delta),
            AstNode::Call { head, args } => self.typecheck_call(*head, args, node_id, delta),
            _ => self.error("unsupported ast node in typechecker", node_id),
        }
    }

    pub fn typecheck(&mut self, delta: &'source EngineDelta) {
        if !delta.ast_nodes.is_empty() {
            self.node_types = vec![VOID_TYPE; delta.ast_nodes.len()];

            let last = delta.ast_nodes.len() - 1;
            self.typecheck_node(NodeId(last), delta)
        }
    }

    pub fn typecheck_let(
        &mut self,
        variable_name: NodeId,
        ty: Option<NodeId>,
        initializer: NodeId,
        node_id: NodeId,
        delta: &'source EngineDelta,
    ) {
        self.typecheck_node(initializer, delta);

        if let Some(ty) = ty {
            self.typecheck_node(ty, delta);

            // TODO make this a compatibility check rather than equality check
            if self.node_types[ty.0] != self.node_types[initializer.0] {
                self.error("initializer does not match declared type", initializer)
            }
        }

        self.define_variable(variable_name, delta);

        self.node_types[variable_name.0] = self.node_types[initializer.0];

        self.node_types[node_id.0] = VOID_TYPE;
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

    pub fn typecheck_while(
        &mut self,
        condition: NodeId,
        block: NodeId,
        node_id: NodeId,
        delta: &'source EngineDelta,
    ) {
        self.typecheck_node(condition, delta);
        let condition_ty = self.node_types[condition.0];

        if condition_ty != BOOL_TYPE {
            self.error("expected bool for while condition", condition);
        }

        self.typecheck_node(block, delta);

        self.node_types[node_id.0] = VOID_TYPE;
    }

    // pub fn typecheck_for(
    //     &mut self,
    //     variable_name: NodeId,
    //     range: NodeId,
    //     block: NodeId,
    //     node_id: NodeId,
    //     delta: &'source EngineDelta,
    // ) {
    //     self.typecheck_node(range, delta);
    //     let range_ty = self.node_types[range.0];

    //     let range_inner_ty = match &self.types[range_ty.0] {
    //         Type::Range(range_inner_ty) => *range_inner_ty,
    //         _ => {
    //             self.error("expected range value in for loop", range);
    //             UNKNOWN_TYPE
    //         }
    //     };

    //     self.typecheck_node(block, delta);

    //     self.define_variable(variable_name, delta);

    //     self.node_types[variable_name.0] = range_inner_ty;

    //     self.node_types[node_id.0] = VOID_TYPE;
    // }

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

    // pub fn typecheck_range(
    //     &mut self,
    //     lhs: NodeId,
    //     rhs: NodeId,
    //     node_id: NodeId,
    //     delta: &'source EngineDelta,
    // ) {
    //     self.typecheck_node(lhs, delta);
    //     self.typecheck_node(rhs, delta);

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

    pub fn typecheck_call(
        &mut self,
        head: NodeId,
        args: &[NodeId],
        node_id: NodeId,
        delta: &'source EngineDelta,
    ) {
        let call_name = &delta.contents[delta.span_start[head.0]..delta.span_end[head.0]];

        for node_id in args {
            self.typecheck_node(*node_id, delta)
        }

        if let Some(defs) = self.external_functions.get(call_name) {
            'outer: for def in defs {
                let def = *def;
                match &self.functions[def.0] {
                    // Function::ExternalFn0(..) => {
                    //     if !args.is_empty() {
                    //         self.error("unexpected argument", args[0])
                    //     }
                    // }
                    FnRecord { params, ret, .. } => {
                        if args.len() != params.len() {
                            // self.error(format!("expected {} argument(s)", params.len()), head);
                            // return;
                            continue;
                        }

                        for idx in 0..params.len() {
                            let param = params[idx];
                            let arg = args[idx];

                            if self.node_types[arg.0] != param {
                                // self.error(
                                //     format!(
                                //         "expect {} found {}",
                                //         self.stringify_type(param),
                                //         self.stringify_type(self.node_types[arg.0])
                                //     ),
                                //     args[idx],
                                // );
                                // return;
                                continue 'outer;
                            }
                        }

                        self.node_types[node_id.0] = *ret;
                        self.call_resolution.insert(head, def);
                        return;
                    }
                }
            }

            let name = String::from_utf8_lossy(call_name);
            self.error(format!("could not resolve call to {}", name), node_id)
        } else {
            let name = String::from_utf8_lossy(call_name);
            self.error(format!("unknown function '{}'", name), node_id)
        }
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

    pub fn create_or_find_type(&mut self, ty: std::any::TypeId) -> TypeId {
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

    // Debug functionality
    pub fn print_node_types(&self) {
        let mut idx = 0;
        while idx < self.node_types.len() {
            println!("{}: {}", idx, self.stringify_type(self.node_types[idx]));
            idx += 1;
        }
    }

    pub fn register_type<T>(&mut self) -> TypeId
    where
        T: Any,
    {
        self.types.push(std::any::TypeId::of::<T>());

        TypeId(self.types.len() - 1)
    }

    pub fn get_type<T>(&self) -> Option<TypeId>
    where
        T: Any,
    {
        for (idx, tid) in self.types.iter().enumerate() {
            if tid == &std::any::TypeId::of::<T>() {
                return Some(TypeId(idx));
            }
        }

        None
    }

    pub fn stringify_type(&self, type_id: TypeId) -> String {
        self.typenames[type_id.0].clone()
    }

    pub fn stringify_function_name(&self, name: &[u8], function_id: FunctionId) -> String {
        let fun_def = &self.functions[function_id.0];

        let mut fun_name = String::from_utf8_lossy(name).to_string();

        for param in &fun_def.params {
            fun_name.push_str("__");
            fun_name.push_str(&self.stringify_type(*param));
        }
        fun_name
    }
}

pub struct FnRecord {
    pub params: Vec<TypeId>,
    pub ret: TypeId,
    pub fun: Function,
    pub raw_ptr: Option<*const u8>,
}

pub trait FnRegister<A, RetVal, Args> {
    fn register_fn(&mut self, name: &str, fun: A, fun_ptr: *const u8);
}

impl<'a, 'source, A, T, U> FnRegister<A, U, &'a T> for TypeChecker<'source>
where
    A: 'static + Fn(T) -> U,
    T: Clone + Any,
    U: Any,
{
    fn register_fn(&mut self, name: &str, fun: A, fun_ptr: *const u8) {
        let wrapped: Box<dyn Fn(&mut Box<dyn Any>) -> Result<Box<dyn Any>, String>> =
            Box::new(move |arg: &mut Box<dyn Any>| {
                let inside = (*arg).downcast_mut() as Option<&mut T>;
                match inside {
                    Some(b) => Ok(Box::new(fun(b.clone())) as Box<dyn Any>),
                    None => Err("ErrorFunctionArgMismatch".into()),
                }
            });

        let param1 = if let Some(id) = self.get_type::<T>() {
            id
        } else {
            self.register_type::<T>()
        };

        let ret = if let Some(id) = self.get_type::<U>() {
            id
        } else {
            self.register_type::<U>()
        };

        self.functions.push(FnRecord {
            params: vec![param1],
            ret,
            fun: Function::ExternalFn1(wrapped),
            raw_ptr: Some(fun_ptr),
        });

        let id = self.functions.len() - 1;

        let ent = self
            .external_functions
            .entry(name.as_bytes().to_vec())
            .or_insert(Vec::new());
        (*ent).push(FunctionId(id));
    }
}

impl<'a, 'source, A, T, U, V> FnRegister<A, V, (&'a T, U)> for TypeChecker<'source>
where
    A: 'static + Fn(T, U) -> V,
    T: Clone + Any,
    U: Clone + Any,
    V: Any,
{
    fn register_fn(&mut self, name: &str, fun: A, fun_ptr: *const u8) {
        let wrapped: Box<
            dyn Fn(&mut Box<dyn Any>, &mut Box<dyn Any>) -> Result<Box<dyn Any>, String>,
        > = Box::new(move |arg1: &mut Box<dyn Any>, arg2: &mut Box<dyn Any>| {
            let inside1 = (*arg1).downcast_mut() as Option<&mut T>;
            let inside2 = (*arg2).downcast_mut() as Option<&mut U>;

            match (inside1, inside2) {
                (Some(b), Some(c)) => Ok(Box::new(fun(b.clone(), c.clone())) as Box<dyn Any>),
                _ => Err("ErrorFunctionArgMismatch".into()),
            }
        });

        let param1 = if let Some(id) = self.get_type::<T>() {
            id
        } else {
            self.register_type::<T>()
        };

        let param2 = if let Some(id) = self.get_type::<U>() {
            id
        } else {
            self.register_type::<U>()
        };

        let ret = if let Some(id) = self.get_type::<V>() {
            id
        } else {
            self.register_type::<V>()
        };

        let fn_record = FnRecord {
            params: vec![param1, param2],
            ret,
            fun: Function::ExternalFn2(wrapped),
            raw_ptr: Some(fun_ptr),
        };
        self.functions.push(fn_record);

        let id = self.functions.len() - 1;

        let ent = self
            .external_functions
            .entry(name.as_bytes().to_vec())
            .or_insert(Vec::new());
        (*ent).push(FunctionId(id));
    }
}

// #[derive(PartialEq)]
// pub enum Type {
//     Unknown,
//     Void,
//     I64,
//     F64,
//     Bool,
//     Range(TypeId),
//     // Fn(Vec<TypeId>, TypeId),
// }

#[macro_export]
macro_rules! register_fn {
    ( $typechecker:expr, $name: expr, $fun:expr ) => {{
        $typechecker.register_fn($name, $fun, $fun as *const u8)
    }};
}
