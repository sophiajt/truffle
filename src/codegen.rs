use std::collections::HashMap;

use crate::{
    parser::{AstNode, NodeId},
    typechecker::{
        ExternalFunctionId, TypeChecker, TypeId, BOOL_TYPE, I64_TYPE, STRING_TYPE, UNIT_TYPE,
    },
    F64_TYPE,
};

#[derive(Clone, Copy, Debug)]
pub struct InstructionId(pub usize);

#[derive(Clone, Copy, Debug)]
pub struct RegisterId(pub usize);

#[derive(Clone, Copy)]
pub union RegisterValue {
    pub f64: f64,
    pub i64: i64,
    pub bool: bool,
    pub ptr: *const Box<String>,
}

pub struct Value {
    val: RegisterValue,
    ty: TypeId,
}

impl Value {
    pub fn new_i64(val: i64) -> Value {
        Value {
            val: RegisterValue { i64: val },
            ty: I64_TYPE,
        }
    }

    pub fn new_f64(val: f64) -> Value {
        Value {
            val: RegisterValue { f64: val },
            ty: F64_TYPE,
        }
    }

    pub fn new_bool(val: bool) -> Value {
        Value {
            val: RegisterValue { bool: val },
            ty: BOOL_TYPE,
        }
    }

    pub fn new_string(val: String) -> Value {
        let thin_string = Box::new(val);
        let ptr = Box::into_raw(thin_string) as _;
        let val = RegisterValue { ptr };
        let ty = STRING_TYPE;
        Value { val, ty }
    }
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug)]
pub enum Instruction {
    IADD {
        lhs: RegisterId,
        rhs: RegisterId,
        target: RegisterId,
    },
    ISUB {
        lhs: RegisterId,
        rhs: RegisterId,
        target: RegisterId,
    },
    IMUL {
        lhs: RegisterId,
        rhs: RegisterId,
        target: RegisterId,
    },
    IDIV {
        lhs: RegisterId,
        rhs: RegisterId,
        target: RegisterId,
    },

    // Integer comparisons (e.g., ILT = Integer + LessThan)
    ILT {
        lhs: RegisterId,
        rhs: RegisterId,
        target: RegisterId,
    },
    ILTE {
        lhs: RegisterId,
        rhs: RegisterId,
        target: RegisterId,
    },
    IGT {
        lhs: RegisterId,
        rhs: RegisterId,
        target: RegisterId,
    },
    IGTE {
        lhs: RegisterId,
        rhs: RegisterId,
        target: RegisterId,
    },

    // float math
    FADD {
        lhs: RegisterId,
        rhs: RegisterId,
        target: RegisterId,
    },
    FSUB {
        lhs: RegisterId,
        rhs: RegisterId,
        target: RegisterId,
    },
    FMUL {
        lhs: RegisterId,
        rhs: RegisterId,
        target: RegisterId,
    },
    FDIV {
        lhs: RegisterId,
        rhs: RegisterId,
        target: RegisterId,
    },

    // float comparisons (e.g., ILT = Integer + LessThan)
    FLT {
        lhs: RegisterId,
        rhs: RegisterId,
        target: RegisterId,
    },
    FLTE {
        lhs: RegisterId,
        rhs: RegisterId,
        target: RegisterId,
    },
    FGT {
        lhs: RegisterId,
        rhs: RegisterId,
        target: RegisterId,
    },
    FGTE {
        lhs: RegisterId,
        rhs: RegisterId,
        target: RegisterId,
    },

    MOV {
        target: RegisterId,
        source: RegisterId,
    },

    BRIF {
        condition: RegisterId,
        then_branch: InstructionId,
        else_branch: InstructionId,
    },

    JMP(InstructionId),

    EXTERNALCALL {
        head: ExternalFunctionId,
        args: Vec<RegisterId>,
        target: RegisterId,
    },

    RET,
}

pub struct FunctionCodegen {
    pub instructions: Vec<Instruction>,
    // Map InstructionId to NodeId
    pub source_map: Vec<NodeId>,
    pub register_values: Vec<RegisterValue>,
    pub register_types: Vec<TypeId>,

    // TODO: we may want a different permanent home, but this should work for now
    pub span_start: Vec<usize>,
    pub span_end: Vec<usize>,
}

impl FunctionCodegen {
    pub fn new_register_with_value(&mut self, value: Value) -> RegisterId {
        self.register_values.push(value.val);
        self.register_types.push(value.ty);

        RegisterId(self.register_values.len() - 1)
    }

    pub fn add_instruction(&mut self, node_id: NodeId, instruction: Instruction) {
        self.instructions.push(instruction);
        self.source_map.push(node_id);
    }

    pub fn new_register(&mut self, ty: TypeId) -> RegisterId {
        // For now just add in an used value that'll be later replaced
        self.register_values.push(RegisterValue { i64: 0 });
        self.register_types.push(ty);

        RegisterId(self.register_values.len() - 1)
    }

    pub fn i64_const(&mut self, value: i64) -> RegisterId {
        self.new_register_with_value(Value::new_i64(value))
    }

    pub fn f64_const(&mut self, value: f64) -> RegisterId {
        self.new_register_with_value(Value::new_f64(value))
    }

    pub fn bool_const(&mut self, value: bool) -> RegisterId {
        self.new_register_with_value(Value::new_bool(value))
    }

    pub fn string_const(&mut self, value: String) -> RegisterId {
        let value = Value::new_string(value);
        self.new_register_with_value(value)
    }

    pub fn add(&mut self, node_id: NodeId, lhs: RegisterId, rhs: RegisterId) -> RegisterId {
        if self.register_types[lhs.0] == F64_TYPE {
            let target = self.new_register(F64_TYPE);

            self.add_instruction(node_id, Instruction::FADD { lhs, rhs, target });

            target
        } else if self.register_types[lhs.0] == I64_TYPE {
            let target = self.new_register(I64_TYPE);

            self.add_instruction(node_id, Instruction::IADD { lhs, rhs, target });

            target
        } else {
            panic!("unsupport add operation")
        }
    }

    pub fn sub(&mut self, node_id: NodeId, lhs: RegisterId, rhs: RegisterId) -> RegisterId {
        if self.register_types[lhs.0] == F64_TYPE {
            let target = self.new_register(F64_TYPE);

            self.add_instruction(node_id, Instruction::FSUB { lhs, rhs, target });

            target
        } else if self.register_types[lhs.0] == I64_TYPE {
            let target = self.new_register(I64_TYPE);

            self.add_instruction(node_id, Instruction::ISUB { lhs, rhs, target });

            target
        } else {
            panic!("unsupport sub operation")
        }
    }

    pub fn mul(&mut self, node_id: NodeId, lhs: RegisterId, rhs: RegisterId) -> RegisterId {
        if self.register_types[lhs.0] == F64_TYPE {
            let target = self.new_register(F64_TYPE);

            self.add_instruction(node_id, Instruction::FMUL { lhs, rhs, target });

            target
        } else if self.register_types[lhs.0] == I64_TYPE {
            let target = self.new_register(I64_TYPE);

            self.add_instruction(node_id, Instruction::IMUL { lhs, rhs, target });

            target
        } else {
            panic!("unsupport mul operation")
        }
    }

    pub fn div(&mut self, node_id: NodeId, lhs: RegisterId, rhs: RegisterId) -> RegisterId {
        if self.register_types[lhs.0] == F64_TYPE {
            let target = self.new_register(F64_TYPE);

            self.add_instruction(node_id, Instruction::FDIV { lhs, rhs, target });

            target
        } else if self.register_types[lhs.0] == I64_TYPE {
            let target = self.new_register(I64_TYPE);

            self.add_instruction(node_id, Instruction::IDIV { lhs, rhs, target });

            target
        } else {
            panic!("unsupport div operation")
        }
    }

    pub fn lt(&mut self, node_id: NodeId, lhs: RegisterId, rhs: RegisterId) -> RegisterId {
        let target = self.new_register(BOOL_TYPE);

        if self.register_types[lhs.0] == F64_TYPE {
            self.add_instruction(node_id, Instruction::FLT { lhs, rhs, target });

            target
        } else if self.register_types[lhs.0] == I64_TYPE {
            self.add_instruction(node_id, Instruction::ILT { lhs, rhs, target });

            target
        } else {
            panic!("unsupport lt operation")
        }
    }

    pub fn lte(&mut self, node_id: NodeId, lhs: RegisterId, rhs: RegisterId) -> RegisterId {
        let target = self.new_register(BOOL_TYPE);

        if self.register_types[lhs.0] == F64_TYPE {
            self.add_instruction(node_id, Instruction::FLTE { lhs, rhs, target });

            target
        } else if self.register_types[lhs.0] == I64_TYPE {
            self.add_instruction(node_id, Instruction::ILTE { lhs, rhs, target });

            target
        } else {
            panic!("unsupport lte operation")
        }
    }

    pub fn gt(&mut self, node_id: NodeId, lhs: RegisterId, rhs: RegisterId) -> RegisterId {
        let target = self.new_register(BOOL_TYPE);

        if self.register_types[lhs.0] == F64_TYPE {
            self.add_instruction(node_id, Instruction::FGT { lhs, rhs, target });

            target
        } else if self.register_types[lhs.0] == I64_TYPE {
            self.add_instruction(node_id, Instruction::IGT { lhs, rhs, target });

            target
        } else {
            panic!("unsupport gt operation")
        }
    }

    pub fn gte(&mut self, node_id: NodeId, lhs: RegisterId, rhs: RegisterId) -> RegisterId {
        let target = self.new_register(BOOL_TYPE);

        if self.register_types[lhs.0] == F64_TYPE {
            self.add_instruction(node_id, Instruction::FGTE { lhs, rhs, target });

            target
        } else if self.register_types[lhs.0] == I64_TYPE {
            self.add_instruction(node_id, Instruction::IGTE { lhs, rhs, target });

            target
        } else {
            panic!("unsupport gte operation")
        }
    }

    pub fn mov(&mut self, node_id: NodeId, target: RegisterId, source: RegisterId) -> RegisterId {
        if target.0 == source.0 {
            source
        } else {
            self.add_instruction(node_id, Instruction::MOV { target, source });

            target
        }
    }

    pub fn brif(
        &mut self,
        node_id: NodeId,
        target: RegisterId,
        condition: RegisterId,
        then_branch: InstructionId,
        else_branch: InstructionId,
    ) -> RegisterId {
        self.add_instruction(
            node_id,
            Instruction::BRIF {
                condition,
                then_branch,
                else_branch,
            },
        );

        target
    }

    pub fn jmp(&mut self, node_id: NodeId, location: InstructionId) {
        self.add_instruction(node_id, Instruction::JMP(location));
    }

    pub fn ret(&mut self, node_id: NodeId) {
        self.add_instruction(node_id, Instruction::RET);
    }

    pub fn external_call(
        &mut self,
        node_id: NodeId,
        head: ExternalFunctionId,
        args: Vec<RegisterId>,
        target: RegisterId,
    ) {
        self.add_instruction(node_id, Instruction::EXTERNALCALL { head, args, target });
    }

    pub fn next_position(&self) -> usize {
        self.instructions.len()
    }

    pub fn offset_instruction_addresses(&mut self, offset_amount: usize) {
        // This moves all our jump addresses by some offset to allow for linking with other
        // codegen'd functions (think: single vector with all instructions after codegen
        // is complete and before evaluation begins)

        for instr in &mut self.instructions {
            match instr {
                Instruction::BRIF {
                    then_branch,
                    else_branch,
                    ..
                } => {
                    *then_branch = InstructionId(then_branch.0 + offset_amount);
                    *else_branch = InstructionId(else_branch.0 + offset_amount);
                }
                Instruction::JMP(addr) => *addr = InstructionId(addr.0 + offset_amount),
                _ => {}
            }
        }
    }
}

pub struct Translater {
    var_lookup: HashMap<NodeId, RegisterId>,
    pub typechecker: TypeChecker,
}

impl Translater {
    pub fn new(typechecker: TypeChecker) -> Self {
        Translater {
            var_lookup: HashMap::new(),
            typechecker,
        }
    }

    pub fn translate(&mut self) -> FunctionCodegen {
        let mut builder = FunctionCodegen {
            instructions: vec![],
            source_map: vec![],
            register_values: vec![RegisterValue { i64: 0 }],
            register_types: vec![TypeId(0)],
            span_start: self.typechecker.parse_results.span_start.clone(),
            span_end: self.typechecker.parse_results.span_end.clone(),
        };
        if !self.typechecker.parse_results.ast_nodes.is_empty() {
            let last = self.typechecker.parse_results.ast_nodes.len() - 1;
            let result = self.translate_node(&mut builder, NodeId(last));
            builder.mov(NodeId(last), RegisterId(0), result);
            builder.register_types[0] = self.typechecker.node_types[last];
        }

        // TEST: uncomment the below to test the async function calls
        // builder.instructions.push(Instruction::ASYNCCALL {
        //     target: RegisterId(0),
        // });

        // FIXME: for now assume a RET at the end, though this should be inferred earlier in compilation
        builder.ret(NodeId(0)); // we used a dummy NodeId as this should be an infallible call
        builder
    }

    pub fn translate_node(&mut self, builder: &mut FunctionCodegen, node_id: NodeId) -> RegisterId {
        match &self.typechecker.parse_results.ast_nodes[node_id.0] {
            AstNode::Int => self.translate_int(builder, node_id),
            AstNode::Float => self.translate_float(builder, node_id),
            AstNode::BinaryOp { lhs, op, rhs } => self.translate_binop(builder, *lhs, *op, *rhs),
            AstNode::Block(nodes) => {
                // FIXME: clone to get around ownership issue
                let nodes = nodes.clone();

                self.translate_block(builder, &nodes)
            }
            AstNode::True => builder.bool_const(true),
            AstNode::False => builder.bool_const(false),
            AstNode::Let {
                variable_name,
                initializer,
                ..
            } => self.translate_let(builder, *variable_name, *initializer),
            AstNode::Variable => self.translate_variable(node_id),
            AstNode::Statement(node_id) => self.translate_node(builder, *node_id),
            AstNode::If {
                condition,
                then_block,
                else_expression,
            } => self.translate_if(builder, node_id, *condition, *then_block, *else_expression),
            AstNode::While { condition, block } => {
                self.translate_while(builder, *condition, *block)
            }
            AstNode::Call { head, args } => {
                // FIXME: clone to get around ownership issue
                self.translate_call(builder, *head, &args.clone(), node_id)
            }
            AstNode::String => self.translate_string(builder, node_id),
            x => panic!("unsupported translation: {:?}", x),
        }
    }

    pub fn translate_int(&mut self, builder: &mut FunctionCodegen, node_id: NodeId) -> RegisterId {
        let contents =
            &self.typechecker.parse_results.contents[self.typechecker.parse_results.span_start
                [node_id.0]
                ..self.typechecker.parse_results.span_end[node_id.0]];

        let constant = String::from_utf8_lossy(contents)
            .parse::<i64>()
            .expect("internal error: int constant could not be parsed");

        builder.i64_const(constant)
    }

    pub fn translate_float(
        &mut self,
        builder: &mut FunctionCodegen,
        node_id: NodeId,
    ) -> RegisterId {
        let contents =
            &self.typechecker.parse_results.contents[self.typechecker.parse_results.span_start
                [node_id.0]
                ..self.typechecker.parse_results.span_end[node_id.0]];

        let constant = String::from_utf8_lossy(contents)
            .parse::<f64>()
            .expect("internal error: float constant could not be parsed");

        builder.f64_const(constant)
    }

    pub fn translate_string(
        &mut self,
        builder: &mut FunctionCodegen,
        node_id: NodeId,
    ) -> RegisterId {
        let contents =
            &self.typechecker.parse_results.contents[self.typechecker.parse_results.span_start
                [node_id.0]
                ..self.typechecker.parse_results.span_end[node_id.0]];

        let s = String::from_utf8(contents.to_owned())
            .expect("internal error: string literal could not be parsed");

        builder.string_const(s)
    }

    pub fn translate_binop(
        &mut self,
        builder: &mut FunctionCodegen,
        lhs: NodeId,
        op: NodeId,
        rhs: NodeId,
    ) -> RegisterId {
        match self.typechecker.parse_results.ast_nodes[op.0] {
            AstNode::Plus => {
                let lhs = self.translate_node(builder, lhs);
                let rhs = self.translate_node(builder, rhs);
                builder.add(op, lhs, rhs)
            }
            AstNode::Minus => {
                let lhs = self.translate_node(builder, lhs);
                let rhs = self.translate_node(builder, rhs);
                builder.sub(op, lhs, rhs)
            }
            AstNode::Multiply => {
                let lhs = self.translate_node(builder, lhs);
                let rhs = self.translate_node(builder, rhs);
                builder.mul(op, lhs, rhs)
            }
            AstNode::Divide => {
                let lhs = self.translate_node(builder, lhs);
                let rhs = self.translate_node(builder, rhs);
                builder.div(op, lhs, rhs)
            }
            AstNode::LessThan => {
                let lhs = self.translate_node(builder, lhs);
                let rhs = self.translate_node(builder, rhs);
                builder.lt(op, lhs, rhs)
            }
            AstNode::LessThanOrEqual => {
                let lhs = self.translate_node(builder, lhs);
                let rhs = self.translate_node(builder, rhs);
                builder.lte(op, lhs, rhs)
            }
            AstNode::GreaterThan => {
                let lhs = self.translate_node(builder, lhs);
                let rhs = self.translate_node(builder, rhs);
                builder.gt(op, lhs, rhs)
            }
            AstNode::GreaterThanOrEqual => {
                let lhs = self.translate_node(builder, lhs);
                let rhs = self.translate_node(builder, rhs);
                builder.gte(op, lhs, rhs)
            }
            AstNode::Assignment => {
                let lhs = self.translate_node(builder, lhs);
                let rhs = self.translate_node(builder, rhs);
                builder.mov(op, lhs, rhs)
            }
            AstNode::And => {
                // Note: we can't pre-translate lhs and rhs because
                // we need to have boolean shortcircuiting
                let output = builder.new_register(BOOL_TYPE);

                let lhs = self.translate_node(builder, lhs);

                let brif_location = builder.next_position();
                builder.brif(op, output, lhs, InstructionId(0), InstructionId(0));

                let true_branch = InstructionId(builder.next_position());
                let rhs = self.translate_node(builder, rhs);
                builder.mov(op, output, rhs);

                let jmp_location = builder.next_position();
                builder.jmp(op, InstructionId(0));

                let false_branch = InstructionId(builder.next_position());
                let false_const = builder.bool_const(false);
                builder.mov(op, output, false_const);

                let after_false_branch = builder.next_position();
                builder.instructions[jmp_location] =
                    Instruction::JMP(InstructionId(after_false_branch));

                builder.instructions[brif_location] = Instruction::BRIF {
                    condition: lhs,
                    then_branch: true_branch,
                    else_branch: false_branch,
                };

                output
            }
            AstNode::Or => {
                // Note: we can't pre-translate lhs and rhs because
                // we need to have boolean shortcircuiting
                let output = builder.new_register(BOOL_TYPE);

                let lhs = self.translate_node(builder, lhs);

                let brif_location = builder.next_position();
                builder.brif(op, output, lhs, InstructionId(0), InstructionId(0));

                let true_branch = InstructionId(builder.next_position());
                let true_const = builder.bool_const(true);
                builder.mov(op, output, true_const);

                let jmp_location = builder.next_position();
                builder.jmp(op, InstructionId(0));

                let false_branch = InstructionId(builder.next_position());
                let rhs = self.translate_node(builder, rhs);
                builder.mov(op, output, rhs);

                let after_false_branch = builder.next_position();
                builder.instructions[jmp_location] =
                    Instruction::JMP(InstructionId(after_false_branch));

                builder.instructions[brif_location] = Instruction::BRIF {
                    condition: lhs,
                    then_branch: true_branch,
                    else_branch: false_branch,
                };

                output
            }
            _ => panic!("unsupported operation"),
        }
    }

    pub fn translate_let(
        &mut self,
        builder: &mut FunctionCodegen,
        variable_name: NodeId,
        initializer: NodeId,
    ) -> RegisterId {
        let initializer = self.translate_node(builder, initializer);

        self.var_lookup.insert(variable_name, initializer);

        initializer
    }

    pub fn translate_variable(&mut self, variable_name: NodeId) -> RegisterId {
        let def_site = self
            .typechecker
            .variable_def_site
            .get(&variable_name)
            .expect("internal error: resolved variable not found");

        let register_id = self
            .var_lookup
            .get(def_site)
            .expect("internal error: resolved variable missing definition");

        *register_id
    }

    #[allow(clippy::too_many_arguments)]
    pub fn translate_if(
        &mut self,
        builder: &mut FunctionCodegen,
        node_id: NodeId,
        condition: NodeId,
        then_block: NodeId,
        else_expression: Option<NodeId>,
    ) -> RegisterId {
        let output = builder.new_register(self.typechecker.node_types[node_id.0]);
        let condition = self.translate_node(builder, condition);

        let brif_location = builder.next_position();
        builder.brif(
            node_id,
            output,
            condition,
            InstructionId(0),
            InstructionId(0),
        );

        let then_branch = InstructionId(builder.next_position());
        let then_output = self.translate_node(builder, then_block);
        builder.mov(node_id, output, then_output);

        let else_branch = if let Some(else_expression) = else_expression {
            // Create a jump with a temporary location we'll replace when we know the correct one
            // Remember where it is so we can replace it later
            let jmp_location = builder.next_position();
            builder.jmp(node_id, InstructionId(0));

            let else_location = builder.next_position();
            let else_output = self.translate_node(builder, else_expression);
            builder.mov(node_id, output, else_output);

            let after_if = builder.next_position();

            builder.instructions[jmp_location] = Instruction::JMP(InstructionId(after_if));
            InstructionId(else_location)
        } else {
            InstructionId(builder.next_position())
        };

        builder.instructions[brif_location] = Instruction::BRIF {
            condition,
            then_branch,
            else_branch,
        };
        output
    }

    pub fn translate_while(
        &mut self,
        builder: &mut FunctionCodegen,
        condition: NodeId,
        block: NodeId,
    ) -> RegisterId {
        let output = builder.new_register(UNIT_TYPE);

        let top = builder.next_position();
        let condition = self.translate_node(builder, condition);

        let brif_location = builder.next_position();
        builder.brif(block, output, condition, InstructionId(0), InstructionId(0));

        let block_begin = InstructionId(builder.next_position());
        self.translate_node(builder, block);
        builder.jmp(block, InstructionId(top));

        let block_end = InstructionId(builder.next_position());

        builder.instructions[brif_location] = Instruction::BRIF {
            condition,
            then_branch: block_begin,
            else_branch: block_end,
        };

        output
    }

    pub fn translate_block(
        &mut self,
        builder: &mut FunctionCodegen,
        nodes: &[NodeId],
    ) -> RegisterId {
        if nodes.is_empty() {
            builder.new_register(UNIT_TYPE)
        } else {
            let mut idx = 0;

            loop {
                let output = self.translate_node(builder, nodes[idx]);
                if idx == (nodes.len() - 1) {
                    return output;
                }
                idx += 1;
            }
        }
    }

    pub fn translate_call(
        &mut self,
        builder: &mut FunctionCodegen,
        head: NodeId,
        args: &[NodeId],
        node_id: NodeId,
    ) -> RegisterId {
        let output = builder.new_register(self.typechecker.node_types[node_id.0]);

        let mut translated_args = vec![];

        for node_id in args {
            translated_args.push(self.translate_node(builder, *node_id));
        }

        let head = self
            .typechecker
            .call_resolution
            .get(&head)
            .expect("internal error: call should be resolved");

        builder.external_call(node_id, *head, translated_args, output);

        output
    }
}
