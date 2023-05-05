use std::collections::HashMap;

use cranelift::{
    codegen::ir::FuncRef,
    prelude::{types::F64, *},
};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};

use crate::{
    delta::EngineDelta,
    parser::{AstNode, NodeId},
    typechecker::{FnRecord, FunctionId, TypeChecker, TypeId, I64_TYPE},
    F64_TYPE,
};

pub struct JIT {
    /// The function builder context, which is reused across multiple
    /// FunctionBuilder instances.
    builder_context: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen. Cranelift
    /// separates this from `Module` to allow for parallel compilation, with a
    /// context per thread, though this isn't in the simple demo here.
    ctx: codegen::Context,

    // /// The data context, which is to data objects what `ctx` is to functions.
    // data_ctx: DataContext,
    //
    /// The module, with the jit backend, which manages the JIT'd
    /// functions.
    module: JITModule,
}

impl JIT {
    pub fn init(typechecker: &TypeChecker) -> Self {
        // We have to do this longer version on aarch64 currently
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        flag_builder.set("opt_level", "speed_and_size").unwrap();
        // flag_builder.set("opt_level", "speed").unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        let mut jit_builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

        for fun in &typechecker.external_functions {
            let fun_def = &typechecker.functions[fun.1 .0];

            if let Some(ptr) = fun_def.raw_ptr {
                let fun_name = String::from_utf8_lossy(fun.0);
                jit_builder.symbol(fun_name, ptr);
            }
        }

        let jit_module = JITModule::new(jit_builder);

        let ctx = jit_module.make_context();

        let builder_context = FunctionBuilderContext::new();

        Self {
            builder_context,
            ctx,
            module: jit_module,
        }
    }
}

pub enum OutputFunction {
    I64Fun(Box<fn() -> i64>),
    F64Fun(Box<fn() -> f64>),
}

pub struct FunctionCodegen {
    fun: OutputFunction,
}

impl FunctionCodegen {
    pub fn eval(&self, _functions: &[FnRecord]) -> (i64, TypeId) {
        match &self.fun {
            OutputFunction::I64Fun(fun) => {
                let result = (fun)();

                (result, I64_TYPE)
            }
            OutputFunction::F64Fun(fun) => {
                let result = (fun)();

                let result = unsafe { std::mem::transmute::<f64, i64>(result) };
                (result, F64_TYPE)
            }
        }
    }

    pub fn debug_print(&self, _typechecker: &TypeChecker) {
        println!("cranelift implementation does not currently keep debug state")
    }
}

pub struct Translater {
    var_lookup: HashMap<NodeId, Variable>,
    func_lookup: HashMap<FunctionId, FuncRef>,

    int: Type,
    float: Type,
}

impl Translater {
    pub fn new() -> Translater {
        Self {
            var_lookup: HashMap::new(),
            func_lookup: HashMap::new(),
            int: Type::default(),
            float: Type::default(),
        }
    }

    pub fn translate<'source>(
        &mut self,
        delta: &'source EngineDelta,
        typechecker: &TypeChecker,
    ) -> FunctionCodegen {
        let mut jit = JIT::init(typechecker);

        self.int = jit.module.target_config().pointer_type();
        self.float = F64;

        if let Some(x) = typechecker.node_types.last() {
            if x == &F64_TYPE {
                jit.ctx
                    .func
                    .signature
                    .returns
                    .push(AbiParam::new(self.float));
            } else {
                jit.ctx.func.signature.returns.push(AbiParam::new(self.int));
            }
        } else {
            jit.ctx
                .func
                .signature
                .returns
                .push(AbiParam::new(self.float));
        }

        let mut builder = FunctionBuilder::new(&mut jit.ctx.func, &mut jit.builder_context);

        for external_function in &typechecker.external_functions {
            let fun_def = &typechecker.functions[external_function.1 .0];
            let mut sig = jit.module.make_signature();
            for param in &fun_def.params {
                if param == &F64_TYPE {
                    sig.params.push(AbiParam::new(self.float));
                } else {
                    sig.params.push(AbiParam::new(self.int));
                }
            }
            if fun_def.ret == F64_TYPE {
                sig.returns.push(AbiParam::new(self.float));
            } else {
                sig.returns.push(AbiParam::new(self.int));
            }

            let name = String::from_utf8_lossy(external_function.0);

            let callee = jit
                .module
                .declare_function(&name, Linkage::Import, &sig)
                .expect("problem declaring function");
            let local_callee = jit.module.declare_func_in_func(callee, builder.func);
            self.func_lookup.insert(*external_function.1, local_callee);
        }

        let entry_block = builder.create_block();

        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        if !delta.ast_nodes.is_empty() {
            let block = builder.create_block();
            builder.ins().jump(block, &[]);
            builder.switch_to_block(block);

            let last = delta.ast_nodes.len() - 1;

            let result = self.translate_node(&mut builder, NodeId(last), delta, typechecker);
            builder.seal_block(block);

            let exit_block = builder.create_block();
            // builder.ins().jump(exit_block, &[]);

            builder.ins().return_(&[result]);
            builder.switch_to_block(exit_block);
            builder.seal_block(exit_block);
        }

        // And finished
        builder.finalize();

        // Finish translation
        let id = jit
            .module
            .declare_function(
                "truffle_cranelift",
                Linkage::Export,
                &jit.ctx.func.signature,
            )
            .unwrap();
        let _ = jit.module.define_function(id, &mut jit.ctx).unwrap();

        jit.module.clear_context(&mut jit.ctx);
        let _ = jit.module.finalize_definitions();

        let code_ptr = jit.module.get_finalized_function(id);

        if let Some(x) = typechecker.node_types.last() {
            if x == &F64_TYPE {
                FunctionCodegen {
                    fun: OutputFunction::F64Fun(Box::new(unsafe {
                        std::mem::transmute::<_, fn() -> f64>(code_ptr)
                    })),
                }
            } else {
                FunctionCodegen {
                    fun: OutputFunction::I64Fun(Box::new(unsafe {
                        std::mem::transmute::<_, fn() -> i64>(code_ptr)
                    })),
                }
            }
        } else {
            FunctionCodegen {
                fun: OutputFunction::I64Fun(Box::new(unsafe {
                    std::mem::transmute::<_, fn() -> i64>(code_ptr)
                })),
            }
        }
    }

    pub fn translate_node<'source>(
        &mut self,
        builder: &mut FunctionBuilder,
        node_id: NodeId,
        delta: &'source EngineDelta,
        typechecker: &TypeChecker,
    ) -> Value {
        match &delta.ast_nodes[node_id.0] {
            AstNode::Int => self.translate_int(builder, node_id, delta),
            AstNode::Float => self.translate_float(builder, node_id, delta),
            AstNode::BinaryOp { lhs, op, rhs } => {
                self.translate_binop(builder, *lhs, *op, *rhs, delta, typechecker)
            }
            AstNode::Block(nodes) => self.translate_block(builder, nodes, delta, typechecker),
            AstNode::Let {
                variable_name,
                initializer,
                ..
            } => self.translate_let(builder, *variable_name, *initializer, delta, typechecker),
            AstNode::Statement(node_id) => {
                self.translate_node(builder, *node_id, delta, typechecker)
            }
            AstNode::If {
                condition,
                then_block,
                else_expression,
            } => self.translate_if(
                builder,
                node_id,
                *condition,
                *then_block,
                *else_expression,
                delta,
                typechecker,
            ),
            AstNode::While { condition, block } => {
                self.translate_while(builder, *condition, *block, delta, typechecker)
            }
            AstNode::True => builder.ins().iconst(self.int, 1),
            AstNode::False => builder.ins().iconst(self.int, 0),
            AstNode::Variable => self.translate_variable(builder, node_id, typechecker),
            AstNode::Call { head, args } => {
                self.translate_call(builder, *head, args, delta, typechecker)
            }
            x => panic!("omg: {:?}", x),
        }
    }

    pub fn translate_int<'source>(
        &mut self,
        builder: &mut FunctionBuilder,
        node_id: NodeId,
        delta: &'source EngineDelta,
    ) -> Value {
        let contents = &delta.contents[delta.span_start[node_id.0]..delta.span_end[node_id.0]];

        let constant = i64::from_str_radix(&String::from_utf8_lossy(contents), 10)
            .expect("internal error: int constant could not be parsed");

        builder.ins().iconst(self.int, constant)
    }

    pub fn translate_float<'source>(
        &mut self,
        builder: &mut FunctionBuilder,
        node_id: NodeId,
        delta: &'source EngineDelta,
    ) -> Value {
        let contents = &delta.contents[delta.span_start[node_id.0]..delta.span_end[node_id.0]];

        let constant = String::from_utf8_lossy(contents)
            .parse::<f64>()
            .expect("internal error: float constant could not be parsed");

        println!("float: {}", constant);

        builder.ins().f64const(constant)
    }

    pub fn translate_variable<'source>(
        &mut self,
        builder: &mut FunctionBuilder,
        variable_name: NodeId,
        typechecker: &TypeChecker,
    ) -> Value {
        let def_site = typechecker
            .variable_def
            .get(&variable_name)
            .expect("internal error: resolved variable not found");

        let variable = self
            .var_lookup
            .get(def_site)
            .expect("internal error: resolved variable missing definition");

        builder.use_var(*variable)
    }

    pub fn translate_let<'source>(
        &mut self,
        builder: &mut FunctionBuilder,
        variable_name: NodeId,
        initializer: NodeId,
        delta: &'source EngineDelta,
        typechecker: &TypeChecker,
    ) -> Value {
        let ty = if typechecker.node_types[initializer.0] == F64_TYPE {
            self.float
        } else {
            self.int
        };

        let initializer = self.translate_node(builder, initializer, delta, typechecker);
        let var = Variable::new(self.var_lookup.len());

        builder.declare_var(var, ty);
        self.var_lookup.insert(variable_name, var);
        builder.def_var(var, initializer);

        initializer
    }

    pub fn translate_binop<'source>(
        &mut self,
        builder: &mut FunctionBuilder,
        untranslate_lhs: NodeId,
        op: NodeId,
        untranslate_rhs: NodeId,
        delta: &'source EngineDelta,
        typechecker: &TypeChecker,
    ) -> Value {
        let lhs = self.translate_node(builder, untranslate_lhs, delta, typechecker);
        let rhs = self.translate_node(builder, untranslate_rhs, delta, typechecker);

        match &delta.ast_nodes[op.0] {
            AstNode::Plus => {
                if typechecker.node_types[untranslate_lhs.0] == F64_TYPE {
                    builder.ins().fadd(lhs, rhs)
                } else {
                    builder.ins().iadd(lhs, rhs)
                }
            }
            AstNode::Minus => {
                if typechecker.node_types[untranslate_lhs.0] == F64_TYPE {
                    builder.ins().fsub(lhs, rhs)
                } else {
                    builder.ins().isub(lhs, rhs)
                }
            }
            AstNode::Multiply => {
                if typechecker.node_types[untranslate_lhs.0] == F64_TYPE {
                    builder.ins().fmul(lhs, rhs)
                } else {
                    builder.ins().imul(lhs, rhs)
                }
            }
            AstNode::Divide => {
                if typechecker.node_types[untranslate_lhs.0] == F64_TYPE {
                    builder.ins().fdiv(lhs, rhs)
                } else {
                    builder.ins().sdiv(lhs, rhs)
                }
            }
            AstNode::LessThan => {
                let result = if typechecker.node_types[untranslate_lhs.0] == F64_TYPE {
                    builder.ins().fcmp(FloatCC::LessThan, lhs, rhs)
                } else {
                    builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs)
                };

                builder.ins().sextend(self.int, result)
            }
            AstNode::LessThanOrEqual => {
                let result = if typechecker.node_types[untranslate_lhs.0] == F64_TYPE {
                    builder.ins().fcmp(FloatCC::LessThanOrEqual, lhs, rhs)
                } else {
                    builder.ins().icmp(IntCC::SignedLessThanOrEqual, lhs, rhs)
                };

                builder.ins().sextend(self.int, result)
            }
            AstNode::GreaterThan => {
                let result = if typechecker.node_types[untranslate_lhs.0] == F64_TYPE {
                    builder.ins().fcmp(FloatCC::GreaterThan, lhs, rhs)
                } else {
                    builder.ins().icmp(IntCC::SignedGreaterThan, lhs, rhs)
                };

                builder.ins().sextend(self.int, result)
            }
            AstNode::GreaterThanOrEqual => {
                let result = if typechecker.node_types[untranslate_lhs.0] == F64_TYPE {
                    builder.ins().fcmp(FloatCC::GreaterThanOrEqual, lhs, rhs)
                } else {
                    builder
                        .ins()
                        .icmp(IntCC::SignedGreaterThanOrEqual, lhs, rhs)
                };

                builder.ins().sextend(self.int, result)
            }
            AstNode::Assignment => {
                if let Some(def_node_id) = typechecker.variable_def.get(&untranslate_lhs) {
                    if let Some(variable) = self.var_lookup.get(&def_node_id) {
                        builder.def_var(*variable, rhs);
                    } else {
                        panic!("unsupported operation: assignment missing variable")
                    }
                } else {
                    panic!("unsupported operation: assignment missing variable")
                }

                rhs
            }
            x => panic!("unsupported operation: {:?}", x),
        }
    }

    pub fn translate_if<'source>(
        &mut self,
        builder: &mut FunctionBuilder,
        if_node_id: NodeId,
        condition: NodeId,
        then_node_id: NodeId,
        else_node_id: Option<NodeId>,
        delta: &'source EngineDelta,
        typechecker: &TypeChecker,
    ) -> Value {
        let condition_block = builder.create_block();
        let then_block = builder.create_block();
        let else_block = builder.create_block();
        let exit_block = builder.create_block();

        let output = Variable::new(self.var_lookup.len());

        let ty = if typechecker.node_types[then_node_id.0] == F64_TYPE {
            self.float
        } else {
            self.int
        };
        builder.declare_var(output, ty);
        self.var_lookup.insert(if_node_id, output);

        // Condition
        builder.ins().jump(condition_block, &[]);
        builder.switch_to_block(condition_block);
        builder.seal_block(condition_block);

        let result = self.translate_node(builder, condition, delta, typechecker);

        builder.ins().brif(result, then_block, &[], else_block, &[]);

        // Then
        builder.switch_to_block(then_block);
        let result = self.translate_node(builder, then_node_id, delta, typechecker);
        builder.def_var(output, result);
        builder.seal_block(then_block);
        builder.ins().jump(exit_block, &[]);

        // Else
        builder.switch_to_block(else_block);
        if let Some(else_node_id) = else_node_id {
            let result = self.translate_node(builder, else_node_id, delta, typechecker);
            builder.def_var(output, result);
        } else {
            if typechecker.node_types[then_node_id.0] == F64_TYPE {
                let zero = builder.ins().f64const(0.0);
                builder.def_var(output, zero);
            } else {
                let zero = builder.ins().iconst(self.int, 0);
                builder.def_var(output, zero);
            };
        }
        builder.seal_block(else_block);
        builder.ins().jump(exit_block, &[]);

        builder.switch_to_block(exit_block);
        builder.seal_block(exit_block);

        builder.use_var(output)
    }

    pub fn translate_while<'source>(
        &mut self,
        builder: &mut FunctionBuilder,
        condition: NodeId,
        block_node_id: NodeId,
        delta: &'source EngineDelta,
        typechecker: &TypeChecker,
    ) -> Value {
        let condition_block = builder.create_block();
        let body_block = builder.create_block();
        let exit_block = builder.create_block();

        // Condition
        builder.ins().jump(condition_block, &[]);
        builder.switch_to_block(condition_block);

        let result = self.translate_node(builder, condition, delta, typechecker);

        builder.ins().brif(result, body_block, &[], exit_block, &[]);

        // Body of loop
        builder.switch_to_block(body_block);
        self.translate_node(builder, block_node_id, delta, typechecker);
        builder.seal_block(body_block);
        builder.ins().jump(condition_block, &[]);

        // Exit loop
        builder.switch_to_block(exit_block);

        builder.seal_block(condition_block);
        builder.seal_block(exit_block);

        builder.ins().iconst(self.int, 0)
    }

    pub fn translate_call<'source>(
        &mut self,
        builder: &mut FunctionBuilder,
        head: NodeId,
        args: &[NodeId],
        delta: &'source EngineDelta,
        typechecker: &TypeChecker,
    ) -> Value {
        let mut translated_args = vec![];

        for arg in args {
            let output = self.translate_node(builder, *arg, delta, typechecker);

            translated_args.push(output);
        }

        // FIXME: Hack resolution for the time being. This will be replaced
        // once we have function registration working for cranelift.

        let resolved_function_id = typechecker
            .call_resolution
            .get(&head)
            .expect("internal error: resolved call missing definition");

        let func = self.func_lookup.get(resolved_function_id).unwrap();
        let call = builder.ins().call(*func, &translated_args);
        builder.inst_results(call)[0]
    }

    pub fn translate_block<'source>(
        &mut self,
        builder: &mut FunctionBuilder,
        nodes: &[NodeId],
        delta: &'source EngineDelta,
        typechecker: &TypeChecker,
    ) -> Value {
        if nodes.is_empty() {
            builder.ins().iconst(self.int, 0)
        } else {
            let mut idx = 0;
            loop {
                let output = self.translate_node(builder, nodes[idx], delta, typechecker);
                if idx == (nodes.len() - 1) {
                    return output;
                }
                idx += 1;
            }
        }
    }
}
