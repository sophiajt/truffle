use std::collections::HashMap;

use crate::{
    parser::{AstNode, NodeId, ParseResults},
    typechecker::{ExternalFunctionId, TypeChecker, TypeId, BOOL_TYPE, I64_TYPE, VOID_TYPE},
    F64_TYPE,
};

#[derive(Clone, Copy, Debug)]
pub struct InstructionId(pub usize);

#[derive(Clone, Copy, Debug)]
pub struct RegisterId(pub usize);

#[derive(Debug, PartialEq)]
pub struct Value {
    ty: TypeId,
    val: i64,
}

impl Value {
    // pub fn new_i64(ty: ValueType, val: i64) -> Value {
    //     Value { ty, val }
    // }

    // pub fn unknown() -> Value {
    //     Value {
    //         ty: ValueType::Unknown,
    //         val: 0,
    //     }
    // }
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

    ASYNCCALL {
        target: RegisterId,
    },

    RET,
}

pub struct FunctionCodegen {
    pub instructions: Vec<Instruction>,
    pub register_values: Vec<i64>,
    pub register_types: Vec<TypeId>,
}

impl FunctionCodegen {
    pub fn new_register_with_value(&mut self, value: i64, value_type: TypeId) -> RegisterId {
        self.register_values.push(value);
        self.register_types.push(value_type);

        RegisterId(self.register_values.len() - 1)
    }

    pub fn new_register(&mut self, ty: TypeId) -> RegisterId {
        self.new_register_with_value(0, ty)
    }

    pub fn i64_const(&mut self, value: i64) -> RegisterId {
        self.new_register_with_value(value, I64_TYPE)
    }

    pub fn f64_const(&mut self, value: f64) -> RegisterId {
        self.new_register_with_value(value.to_bits() as i64, F64_TYPE)
    }

    pub fn bool_const(&mut self, value: bool) -> RegisterId {
        if value {
            self.new_register_with_value(1, BOOL_TYPE)
        } else {
            self.new_register_with_value(0, BOOL_TYPE)
        }
    }

    pub fn add(&mut self, lhs: RegisterId, rhs: RegisterId) -> RegisterId {
        if self.register_types[lhs.0] == F64_TYPE {
            let target = self.new_register(F64_TYPE);

            self.instructions
                .push(Instruction::FADD { lhs, rhs, target });

            target
        } else if self.register_types[lhs.0] == I64_TYPE {
            let target = self.new_register(I64_TYPE);

            self.instructions
                .push(Instruction::IADD { lhs, rhs, target });

            target
        } else {
            panic!("unsupport add operation")
        }
    }

    pub fn sub(&mut self, lhs: RegisterId, rhs: RegisterId) -> RegisterId {
        if self.register_types[lhs.0] == F64_TYPE {
            let target = self.new_register(F64_TYPE);

            self.instructions
                .push(Instruction::FSUB { lhs, rhs, target });

            target
        } else if self.register_types[lhs.0] == I64_TYPE {
            let target = self.new_register(I64_TYPE);

            self.instructions
                .push(Instruction::ISUB { lhs, rhs, target });

            target
        } else {
            panic!("unsupport sub operation")
        }
    }

    pub fn mul(&mut self, lhs: RegisterId, rhs: RegisterId) -> RegisterId {
        if self.register_types[lhs.0] == F64_TYPE {
            let target = self.new_register(F64_TYPE);

            self.instructions
                .push(Instruction::FMUL { lhs, rhs, target });

            target
        } else if self.register_types[lhs.0] == I64_TYPE {
            let target = self.new_register(I64_TYPE);

            self.instructions
                .push(Instruction::IMUL { lhs, rhs, target });

            target
        } else {
            panic!("unsupport mul operation")
        }
    }

    pub fn div(&mut self, lhs: RegisterId, rhs: RegisterId) -> RegisterId {
        if self.register_types[lhs.0] == F64_TYPE {
            let target = self.new_register(F64_TYPE);

            self.instructions
                .push(Instruction::FDIV { lhs, rhs, target });

            target
        } else if self.register_types[lhs.0] == I64_TYPE {
            let target = self.new_register(I64_TYPE);

            self.instructions
                .push(Instruction::IDIV { lhs, rhs, target });

            target
        } else {
            panic!("unsupport div operation")
        }
    }

    pub fn lt(&mut self, lhs: RegisterId, rhs: RegisterId) -> RegisterId {
        let target = self.new_register(BOOL_TYPE);

        if self.register_types[lhs.0] == F64_TYPE {
            self.instructions
                .push(Instruction::FLT { lhs, rhs, target });

            target
        } else if self.register_types[lhs.0] == I64_TYPE {
            self.instructions
                .push(Instruction::ILT { lhs, rhs, target });

            target
        } else {
            panic!("unsupport lt operation")
        }
    }

    pub fn lte(&mut self, lhs: RegisterId, rhs: RegisterId) -> RegisterId {
        let target = self.new_register(BOOL_TYPE);

        if self.register_types[lhs.0] == F64_TYPE {
            self.instructions
                .push(Instruction::FLTE { lhs, rhs, target });

            target
        } else if self.register_types[lhs.0] == I64_TYPE {
            self.instructions
                .push(Instruction::ILTE { lhs, rhs, target });

            target
        } else {
            panic!("unsupport lte operation")
        }
    }

    pub fn gt(&mut self, lhs: RegisterId, rhs: RegisterId) -> RegisterId {
        let target = self.new_register(BOOL_TYPE);

        if self.register_types[lhs.0] == F64_TYPE {
            self.instructions
                .push(Instruction::FGT { lhs, rhs, target });

            target
        } else if self.register_types[lhs.0] == I64_TYPE {
            self.instructions
                .push(Instruction::IGT { lhs, rhs, target });

            target
        } else {
            panic!("unsupport gt operation")
        }
    }

    pub fn gte(&mut self, lhs: RegisterId, rhs: RegisterId) -> RegisterId {
        let target = self.new_register(BOOL_TYPE);

        if self.register_types[lhs.0] == F64_TYPE {
            self.instructions
                .push(Instruction::FGTE { lhs, rhs, target });

            target
        } else if self.register_types[lhs.0] == I64_TYPE {
            self.instructions
                .push(Instruction::IGTE { lhs, rhs, target });

            target
        } else {
            panic!("unsupport gte operation")
        }
    }

    pub fn mov(&mut self, target: RegisterId, source: RegisterId) -> RegisterId {
        if target.0 == source.0 {
            source
        } else {
            self.instructions.push(Instruction::MOV { target, source });

            target
        }
    }

    pub fn brif(
        &mut self,
        target: RegisterId,
        condition: RegisterId,
        then_branch: InstructionId,
        else_branch: InstructionId,
    ) -> RegisterId {
        self.instructions.push(Instruction::BRIF {
            condition,
            then_branch,
            else_branch,
        });

        target
    }

    pub fn jmp(&mut self, location: InstructionId) {
        self.instructions.push(Instruction::JMP(location))
    }

    pub fn ret(&mut self) {
        self.instructions.push(Instruction::RET)
    }

    pub fn external_call(
        &mut self,
        head: ExternalFunctionId,
        args: Vec<RegisterId>,
        target: RegisterId,
    ) {
        self.instructions
            .push(Instruction::EXTERNALCALL { head, args, target })
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

#[derive(Default)]
pub struct Translater {
    var_lookup: HashMap<NodeId, RegisterId>,
}

impl Translater {
    pub fn new() -> Self {
        Translater {
            var_lookup: HashMap::new(),
        }
    }

    pub fn translate(
        &mut self,
        parse_results: &ParseResults,
        typechecker: &TypeChecker,
    ) -> FunctionCodegen {
        let mut builder = FunctionCodegen {
            instructions: vec![],
            register_values: vec![0],
            register_types: vec![TypeId(0)],
        };
        if !parse_results.ast_nodes.is_empty() {
            let last = parse_results.ast_nodes.len() - 1;
            let result =
                self.translate_node(&mut builder, NodeId(last), parse_results, typechecker);
            builder.mov(RegisterId(0), result);
            builder.register_types[0] = typechecker.node_types[last];
        }

        // TEST: uncomment the below to test the async function calls
        // builder.instructions.push(Instruction::ASYNCCALL {
        //     target: RegisterId(0),
        // });

        // FIXME: for now assume a RET at the end, though this should be inferred earlier in compilation
        builder.ret();
        builder
    }

    pub fn translate_node(
        &mut self,
        builder: &mut FunctionCodegen,
        node_id: NodeId,
        parse_results: &ParseResults,
        typechecker: &TypeChecker,
    ) -> RegisterId {
        match &parse_results.ast_nodes[node_id.0] {
            AstNode::Int => self.translate_int(builder, node_id, parse_results),
            AstNode::Float => self.translate_float(builder, node_id, parse_results),
            AstNode::BinaryOp { lhs, op, rhs } => {
                self.translate_binop(builder, *lhs, *op, *rhs, parse_results, typechecker)
            }
            AstNode::Block(nodes) => {
                self.translate_block(builder, nodes, parse_results, typechecker)
            }
            AstNode::True => builder.bool_const(true),
            AstNode::False => builder.bool_const(false),
            AstNode::Let {
                variable_name,
                initializer,
                ..
            } => self.translate_let(
                builder,
                *variable_name,
                *initializer,
                parse_results,
                typechecker,
            ),
            AstNode::Variable => self.translate_variable(node_id, typechecker),
            AstNode::Statement(node_id) => {
                self.translate_node(builder, *node_id, parse_results, typechecker)
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
                parse_results,
                typechecker,
            ),
            AstNode::While { condition, block } => {
                self.translate_while(builder, *condition, *block, parse_results, typechecker)
            }
            AstNode::Call { head, args } => {
                self.translate_call(builder, *head, args, node_id, parse_results, typechecker)
            }
            x => panic!("unsupported translation: {:?}", x),
        }
    }

    pub fn translate_int(
        &mut self,
        builder: &mut FunctionCodegen,
        node_id: NodeId,
        parse_results: &ParseResults,
    ) -> RegisterId {
        let contents = &parse_results.contents
            [parse_results.span_start[node_id.0]..parse_results.span_end[node_id.0]];

        let constant = String::from_utf8_lossy(contents)
            .parse::<i64>()
            .expect("internal error: int constant could not be parsed");

        builder.i64_const(constant)
    }

    pub fn translate_float(
        &mut self,
        builder: &mut FunctionCodegen,
        node_id: NodeId,
        parse_results: &ParseResults,
    ) -> RegisterId {
        let contents = &parse_results.contents
            [parse_results.span_start[node_id.0]..parse_results.span_end[node_id.0]];

        let constant = String::from_utf8_lossy(contents)
            .parse::<f64>()
            .expect("internal error: float constant could not be parsed");

        builder.f64_const(constant)
    }

    pub fn translate_binop(
        &mut self,
        builder: &mut FunctionCodegen,
        lhs: NodeId,
        op: NodeId,
        rhs: NodeId,
        parse_results: &ParseResults,
        typechecker: &TypeChecker,
    ) -> RegisterId {
        let lhs = self.translate_node(builder, lhs, parse_results, typechecker);
        let rhs = self.translate_node(builder, rhs, parse_results, typechecker);

        match parse_results.ast_nodes[op.0] {
            AstNode::Plus => builder.add(lhs, rhs),
            AstNode::Minus => builder.sub(lhs, rhs),
            AstNode::Multiply => builder.mul(lhs, rhs),
            AstNode::Divide => builder.div(lhs, rhs),
            AstNode::LessThan => builder.lt(lhs, rhs),
            AstNode::LessThanOrEqual => builder.lte(lhs, rhs),
            AstNode::GreaterThan => builder.gt(lhs, rhs),
            AstNode::GreaterThanOrEqual => builder.gte(lhs, rhs),
            AstNode::Assignment => builder.mov(lhs, rhs),
            _ => panic!("unsupported operation"),
        }
    }

    pub fn translate_let(
        &mut self,
        builder: &mut FunctionCodegen,
        variable_name: NodeId,
        initializer: NodeId,
        parse_results: &ParseResults,
        typechecker: &TypeChecker,
    ) -> RegisterId {
        let initializer = self.translate_node(builder, initializer, parse_results, typechecker);

        self.var_lookup.insert(variable_name, initializer);

        initializer
    }

    pub fn translate_variable(
        &mut self,
        variable_name: NodeId,
        typechecker: &TypeChecker,
    ) -> RegisterId {
        let def_site = typechecker
            .variable_def
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
        parse_results: &ParseResults,
        typechecker: &TypeChecker,
    ) -> RegisterId {
        let output = builder.new_register(typechecker.node_types[node_id.0]);
        let condition = self.translate_node(builder, condition, parse_results, typechecker);

        let brif_location = builder.next_position();
        builder.brif(output, condition, InstructionId(0), InstructionId(0));

        let then_branch = InstructionId(builder.next_position());
        let then_output = self.translate_node(builder, then_block, parse_results, typechecker);
        builder.mov(output, then_output);

        let else_branch = if let Some(else_expression) = else_expression {
            // Create a jump with a temporary location we'll replace when we know the correct one
            // Remember where it is so we can replace it later
            let jmp_location = builder.next_position();
            builder.jmp(InstructionId(0));

            let else_location = builder.next_position();
            let else_output =
                self.translate_node(builder, else_expression, parse_results, typechecker);
            builder.mov(output, else_output);

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
        parse_results: &ParseResults,
        typechecker: &TypeChecker,
    ) -> RegisterId {
        let output = builder.new_register(VOID_TYPE);

        let top = builder.next_position();
        let condition = self.translate_node(builder, condition, parse_results, typechecker);

        let brif_location = builder.next_position();
        builder.brif(output, condition, InstructionId(0), InstructionId(0));

        let block_begin = InstructionId(builder.next_position());
        self.translate_node(builder, block, parse_results, typechecker);
        builder.jmp(InstructionId(top));

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
        parse_results: &ParseResults,
        typechecker: &TypeChecker,
    ) -> RegisterId {
        if nodes.is_empty() {
            builder.new_register(VOID_TYPE)
        } else {
            let mut idx = 0;

            loop {
                let output = self.translate_node(builder, nodes[idx], parse_results, typechecker);
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
        parse_results: &ParseResults,
        typechecker: &TypeChecker,
    ) -> RegisterId {
        let output = builder.new_register(typechecker.node_types[node_id.0]);

        let head = typechecker
            .call_resolution
            .get(&head)
            .expect("internal error: call should be resolved");

        let mut translated_args = vec![];

        for node_id in args {
            translated_args.push(self.translate_node(
                builder,
                *node_id,
                parse_results,
                typechecker,
            ));
        }

        builder.external_call(*head, translated_args, output);

        output
    }
}
