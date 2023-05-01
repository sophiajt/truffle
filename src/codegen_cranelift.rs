use std::collections::HashMap;

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};

use crate::{
    delta::EngineDelta,
    parser::{AstNode, NodeId},
    typechecker::{
        Function, FunctionId, TypeChecker, TypeId, BOOL_TYPE, I64_TYPE, UNKNOWN_TYPE, VOID_TYPE,
    },
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
    pub fn init() -> Self {
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
        let jit_builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

        // jit_builder.symbol("push_val", push_val as *const u8);

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

pub struct FunctionCodegen {
    fun: Box<fn() -> i64>,
}

impl FunctionCodegen {
    pub fn eval(&self, functions: &[Function]) -> (i64, TypeId) {
        let result = (self.fun)();

        (result, I64_TYPE)
    }

    pub fn debug_print(&self, typechecker: &TypeChecker) {
        println!("cranelift implementation does not currently keep debug state")
    }
}

pub struct Translater {
    var_lookup: HashMap<NodeId, Variable>,
}

impl Translater {
    pub fn new() -> Translater {
        Self {
            var_lookup: HashMap::new(),
        }
    }

    pub fn translate<'source>(
        &mut self,
        delta: &'source EngineDelta,
        typechecker: &TypeChecker,
    ) -> FunctionCodegen {
        let mut jit = JIT::init();

        let int = jit.module.target_config().pointer_type();

        jit.ctx.func.signature.returns.push(AbiParam::new(int));

        let mut builder = FunctionBuilder::new(&mut jit.ctx.func, &mut jit.builder_context);

        let entry_block = builder.create_block();

        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        if !delta.ast_nodes.is_empty() {
            let block = builder.create_block();
            builder.ins().jump(block, &[]);
            builder.switch_to_block(block);

            let last = delta.ast_nodes.len() - 1;

            let result = self.translate_node(&mut builder, NodeId(last), delta, typechecker, int);
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

        FunctionCodegen {
            fun: Box::new(unsafe { std::mem::transmute::<_, fn() -> i64>(code_ptr) }),
        }
    }

    pub fn translate_node<'source>(
        &mut self,
        builder: &mut FunctionBuilder,
        node_id: NodeId,
        delta: &'source EngineDelta,
        typechecker: &TypeChecker,
        int: Type,
    ) -> Value {
        match &delta.ast_nodes[node_id.0] {
            AstNode::Int => self.translate_int(builder, node_id, delta, int),
            AstNode::BinaryOp { lhs, op, rhs } => {
                self.translate_binop(builder, *lhs, *op, *rhs, delta, typechecker, int)
            }
            AstNode::Block(nodes) => self.translate_block(builder, nodes, delta, typechecker, int),
            AstNode::Let {
                variable_name,
                initializer,
                ..
            } => self.translate_let(
                builder,
                *variable_name,
                *initializer,
                delta,
                typechecker,
                int,
            ),
            AstNode::Variable => self.translate_variable(builder, node_id, typechecker),
            x => panic!("omg: {:?}", x),
        }
    }

    pub fn translate_int<'source>(
        &mut self,
        builder: &mut FunctionBuilder,
        node_id: NodeId,
        delta: &'source EngineDelta,
        int: Type,
    ) -> Value {
        let contents = &delta.contents[delta.span_start[node_id.0]..delta.span_end[node_id.0]];

        let constant = i64::from_str_radix(&String::from_utf8_lossy(contents), 10)
            .expect("internal error: int constant could not be parsed");

        builder.ins().iconst(int, constant)
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
        int: Type,
    ) -> Value {
        let initializer = self.translate_node(builder, initializer, delta, typechecker, int);
        let var = Variable::new(self.var_lookup.len());

        builder.declare_var(var, int);
        self.var_lookup.insert(variable_name, var);
        builder.def_var(var, initializer);

        initializer
    }

    pub fn translate_binop<'source>(
        &mut self,
        builder: &mut FunctionBuilder,
        lhs: NodeId,
        op: NodeId,
        rhs: NodeId,
        delta: &'source EngineDelta,
        typechecker: &TypeChecker,
        int: Type,
    ) -> Value {
        let lhs = self.translate_node(builder, lhs, delta, typechecker, int);
        let rhs = self.translate_node(builder, rhs, delta, typechecker, int);

        match delta.ast_nodes[op.0] {
            AstNode::Plus => builder.ins().iadd(lhs, rhs),
            AstNode::Minus => builder.ins().isub(lhs, rhs),
            AstNode::Multiply => builder.ins().imul(lhs, rhs),
            AstNode::LessThan => builder.ins().icmp(IntCC::UnsignedLessThan, lhs, rhs),
            AstNode::LessThanOrEqual => {
                builder.ins().icmp(IntCC::UnsignedLessThanOrEqual, lhs, rhs)
            }
            AstNode::GreaterThan => builder.ins().icmp(IntCC::UnsignedGreaterThan, lhs, rhs),
            AstNode::GreaterThanOrEqual => {
                builder
                    .ins()
                    .icmp(IntCC::UnsignedGreaterThanOrEqual, lhs, rhs)
            }
            _ => panic!("unsupported operation"),
        }
    }

    pub fn translate_block<'source>(
        &mut self,
        builder: &mut FunctionBuilder,
        nodes: &[NodeId],
        delta: &'source EngineDelta,
        typechecker: &TypeChecker,
        int: Type,
    ) -> Value {
        if nodes.is_empty() {
            builder.ins().iconst(int, 0)
        } else {
            let mut idx = 0;
            loop {
                let output = self.translate_node(builder, nodes[idx], delta, typechecker, int);
                if idx == (nodes.len() - 1) {
                    return output;
                }
                idx += 1;
            }
        }
    }
}
