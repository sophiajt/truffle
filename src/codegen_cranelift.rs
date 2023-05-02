use std::collections::HashMap;

use cranelift::{codegen::ir::FuncRef, prelude::*};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};

use crate::{
    add_int,
    delta::EngineDelta,
    parser::{AstNode, NodeId},
    print_int,
    typechecker::{Function, TypeChecker, TypeId, I64_TYPE},
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
        let mut jit_builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

        // jit_builder.symbol("push_val", push_val as *const u8);

        // FIXME: I don't yet know how to register functions in the
        // rhai style for cranelift. We'll need to explore with the
        // cranelift team if we decide to go with cranelift
        jit_builder.symbol("print_int", print_int as *const u8);
        jit_builder.symbol("add_int", add_int as *const u8);

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
    pub fn eval(&self, _functions: &[Function]) -> (i64, TypeId) {
        let result = (self.fun)();

        (result, I64_TYPE)
    }

    pub fn debug_print(&self, _typechecker: &TypeChecker) {
        println!("cranelift implementation does not currently keep debug state")
    }
}

pub struct Translater {
    var_lookup: HashMap<NodeId, Variable>,
    func_loopup: HashMap<String, FuncRef>,
}

impl Translater {
    pub fn new() -> Translater {
        Self {
            var_lookup: HashMap::new(),
            func_loopup: HashMap::new(),
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

        // FIXME: until we can get lambda registration working for
        // cranelift, use direct registration
        let mut sig = jit.module.make_signature();
        sig.params.push(AbiParam::new(int));
        let callee = jit
            .module
            .declare_function("print_int", Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = jit.module.declare_func_in_func(callee, builder.func);
        self.func_loopup.insert("print_int".into(), local_callee);

        let mut sig = jit.module.make_signature();
        sig.params.push(AbiParam::new(int));
        sig.params.push(AbiParam::new(int));
        sig.returns.push(AbiParam::new(int));
        let callee = jit
            .module
            .declare_function("add_int", Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = jit.module.declare_func_in_func(callee, builder.func);
        self.func_loopup.insert("add_int".into(), local_callee);

        let entry_block = builder.create_block();

        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        if !delta.ast_nodes.is_empty() {
            let block = builder.create_block();
            builder.ins().jump(block, &[]);
            builder.switch_to_block(block);

            let last = delta.ast_nodes.len() - 1;

            let result =
                self.translate_node(&mut builder, NodeId(last as u32), delta, typechecker, int);
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
            AstNode::Statement(node_id) => {
                self.translate_node(builder, *node_id, delta, typechecker, int)
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
                int,
            ),
            AstNode::While { condition, block } => {
                self.translate_while(builder, *condition, *block, delta, typechecker, int)
            }
            AstNode::True => builder.ins().iconst(int, 1),
            AstNode::False => builder.ins().iconst(int, 0),
            AstNode::Variable => self.translate_variable(builder, node_id, typechecker),
            AstNode::Call { head, args } => {
                self.translate_call(builder, *head, args, delta, typechecker, int)
            }
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
        untranslate_lhs: NodeId,
        op: NodeId,
        untranslate_rhs: NodeId,
        delta: &'source EngineDelta,
        typechecker: &TypeChecker,
        int: Type,
    ) -> Value {
        let lhs = self.translate_node(builder, untranslate_lhs, delta, typechecker, int);
        let rhs = self.translate_node(builder, untranslate_rhs, delta, typechecker, int);

        match delta.ast_nodes[op.0] {
            AstNode::Plus => builder.ins().iadd(lhs, rhs),
            AstNode::Minus => builder.ins().isub(lhs, rhs),
            AstNode::Multiply => builder.ins().imul(lhs, rhs),
            AstNode::LessThan => {
                let result = builder.ins().icmp(IntCC::UnsignedLessThan, lhs, rhs);

                builder.ins().sextend(int, result)
            }
            AstNode::LessThanOrEqual => {
                let result = builder.ins().icmp(IntCC::UnsignedLessThanOrEqual, lhs, rhs);

                builder.ins().sextend(int, result)
            }
            AstNode::GreaterThan => {
                let result = builder.ins().icmp(IntCC::UnsignedGreaterThan, lhs, rhs);

                builder.ins().sextend(int, result)
            }
            AstNode::GreaterThanOrEqual => {
                let result = builder
                    .ins()
                    .icmp(IntCC::UnsignedGreaterThanOrEqual, lhs, rhs);

                builder.ins().sextend(int, result)
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
            _ => panic!("unsupported operation"),
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
        int: Type,
    ) -> Value {
        let condition_block = builder.create_block();
        let then_block = builder.create_block();
        let else_block = builder.create_block();
        let exit_block = builder.create_block();

        let output = Variable::new(self.var_lookup.len());
        // FIXME: assume i64 for now
        builder.declare_var(output, int);
        self.var_lookup.insert(if_node_id, output);

        // Condition
        builder.ins().jump(condition_block, &[]);
        builder.switch_to_block(condition_block);
        builder.seal_block(condition_block);

        let result = self.translate_node(builder, condition, delta, typechecker, int);

        builder.ins().brif(result, then_block, &[], else_block, &[]);

        // Then
        builder.switch_to_block(then_block);
        let result = self.translate_node(builder, then_node_id, delta, typechecker, int);
        builder.def_var(output, result);
        builder.seal_block(then_block);
        builder.ins().jump(exit_block, &[]);

        // Else
        builder.switch_to_block(else_block);
        if let Some(else_node_id) = else_node_id {
            let result = self.translate_node(builder, else_node_id, delta, typechecker, int);
            builder.def_var(output, result);
        } else {
            let zero = builder.ins().iconst(int, 0);
            builder.def_var(output, zero);
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
        int: Type,
    ) -> Value {
        let condition_block = builder.create_block();
        let body_block = builder.create_block();
        let exit_block = builder.create_block();

        // Condition
        builder.ins().jump(condition_block, &[]);
        builder.switch_to_block(condition_block);

        let result = self.translate_node(builder, condition, delta, typechecker, int);

        builder.ins().brif(result, body_block, &[], exit_block, &[]);

        // Body of loop
        builder.switch_to_block(body_block);
        self.translate_node(builder, block_node_id, delta, typechecker, int);
        builder.seal_block(body_block);
        builder.ins().jump(condition_block, &[]);

        // Exit loop
        builder.switch_to_block(exit_block);

        builder.seal_block(condition_block);
        builder.seal_block(exit_block);

        builder.ins().iconst(int, 0)
    }

    pub fn translate_call<'source>(
        &mut self,
        builder: &mut FunctionBuilder,
        _head: NodeId,
        args: &[NodeId],
        delta: &'source EngineDelta,
        typechecker: &TypeChecker,
        int: Type,
    ) -> Value {
        let mut translated_args = vec![];

        for arg in args {
            let output = self.translate_node(builder, *arg, delta, typechecker, int);

            translated_args.push(output);
        }

        // FIXME: Hack resolution for the time being. This will be replaced
        // once we have function registration working for cranelift.
        if args.len() == 1 {
            let print_int = self.func_loopup.get("print_int").unwrap();
            let call = builder.ins().call(*print_int, &translated_args);
            let _ = builder.inst_results(call);

            // For now, return this in place of void
            builder.ins().iconst(int, 0)
        } else if args.len() == 2 {
            let print_int = self.func_loopup.get("add_int").unwrap();
            let call = builder.ins().call(*print_int, &translated_args);
            builder.inst_results(call)[0]
        } else {
            panic!("cranelift function resolution incomplete");
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
