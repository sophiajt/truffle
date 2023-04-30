use std::{any::Any, collections::HashMap};

use crate::{
    delta::EngineDelta,
    parser::{AstNode, NodeId},
    typechecker::{Function, FunctionId, TypeChecker, I64_TYPE},
};

#[derive(Clone, Copy, Debug)]
pub struct InstructionId(usize);

#[derive(Clone, Copy, Debug)]
pub struct RegisterId(usize);

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ValueType {
    Unknown,
    Void,
    I64,
    F64,
    Bool,
}

#[derive(Debug, PartialEq)]
pub struct Value {
    ty: ValueType,
    val: i64,
}

impl Value {
    pub fn new_i64(ty: ValueType, val: i64) -> Value {
        Value { ty, val }
    }

    pub fn unknown() -> Value {
        Value {
            ty: ValueType::Unknown,
            val: 0,
        }
    }
}

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

    EXTERNAL_CALL {
        head: FunctionId,
        args: Vec<RegisterId>,
        target: RegisterId,
    },
}

pub struct FunctionCodegen {
    pub instructions: Vec<Instruction>,
    pub register_values: Vec<i64>,
    pub register_types: Vec<ValueType>,
}

impl FunctionCodegen {
    pub fn new_register_with_value(&mut self, value: i64, value_type: ValueType) -> RegisterId {
        self.register_values.push(value);
        self.register_types.push(value_type);

        RegisterId(self.register_values.len() - 1)
    }

    pub fn new_register(&mut self, ty: ValueType) -> RegisterId {
        self.new_register_with_value(0, ty)
    }

    pub fn i64_const(&mut self, value: i64) -> RegisterId {
        self.new_register_with_value(value, ValueType::I64)
    }

    pub fn bool_const(&mut self, value: bool) -> RegisterId {
        if value {
            self.new_register_with_value(1, ValueType::Bool)
        } else {
            self.new_register_with_value(0, ValueType::Bool)
        }
    }

    pub fn iadd(&mut self, lhs: RegisterId, rhs: RegisterId) -> RegisterId {
        let target = self.new_register(ValueType::I64);

        self.instructions
            .push(Instruction::IADD { lhs, rhs, target });

        target
    }

    pub fn isub(&mut self, lhs: RegisterId, rhs: RegisterId) -> RegisterId {
        let target = self.new_register(ValueType::I64);

        self.instructions
            .push(Instruction::ISUB { lhs, rhs, target });

        target
    }

    pub fn imul(&mut self, lhs: RegisterId, rhs: RegisterId) -> RegisterId {
        let target = self.new_register(ValueType::I64);

        self.instructions
            .push(Instruction::IMUL { lhs, rhs, target });

        target
    }

    pub fn idiv(&mut self, lhs: RegisterId, rhs: RegisterId) -> RegisterId {
        let target = self.new_register(ValueType::I64);

        self.instructions
            .push(Instruction::IDIV { lhs, rhs, target });

        target
    }

    pub fn ilt(&mut self, lhs: RegisterId, rhs: RegisterId) -> RegisterId {
        let target = self.new_register(ValueType::Bool);

        self.instructions
            .push(Instruction::ILT { lhs, rhs, target });

        target
    }

    pub fn ilte(&mut self, lhs: RegisterId, rhs: RegisterId) -> RegisterId {
        let target = self.new_register(ValueType::Bool);

        self.instructions
            .push(Instruction::ILTE { lhs, rhs, target });

        target
    }

    pub fn igt(&mut self, lhs: RegisterId, rhs: RegisterId) -> RegisterId {
        let target = self.new_register(ValueType::Bool);

        self.instructions
            .push(Instruction::IGT { lhs, rhs, target });

        target
    }

    pub fn igte(&mut self, lhs: RegisterId, rhs: RegisterId) -> RegisterId {
        let target = self.new_register(ValueType::Bool);

        self.instructions
            .push(Instruction::IGTE { lhs, rhs, target });

        target
    }

    pub fn mov(&mut self, target: RegisterId, source: RegisterId) -> RegisterId {
        self.instructions.push(Instruction::MOV { target, source });

        target
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

    pub fn external_call(&mut self, head: FunctionId, args: Vec<RegisterId>, target: RegisterId) {
        self.instructions
            .push(Instruction::EXTERNAL_CALL { head, args, target })
    }

    pub fn eval(&mut self, functions: &[Function]) -> (i64, ValueType) {
        let mut instruction_pointer = 0;
        let length = self.instructions.len();

        while instruction_pointer < length {
            match &self.instructions[instruction_pointer] {
                Instruction::IADD { lhs, rhs, target } => {
                    self.register_values[target.0] =
                        self.register_values[lhs.0] + self.register_values[rhs.0];

                    instruction_pointer += 1;
                }
                Instruction::ISUB { lhs, rhs, target } => {
                    let lhs = &self.register_values[lhs.0];
                    let rhs = &self.register_values[rhs.0];

                    self.register_values[target.0] = lhs - rhs;

                    instruction_pointer += 1;
                }
                Instruction::IMUL { lhs, rhs, target } => {
                    let lhs = &self.register_values[lhs.0];
                    let rhs = &self.register_values[rhs.0];

                    self.register_values[target.0] = lhs * rhs;

                    instruction_pointer += 1;
                }
                Instruction::IDIV { lhs, rhs, target } => {
                    let lhs = &self.register_values[lhs.0];
                    let rhs = &self.register_values[rhs.0];

                    self.register_values[target.0] = lhs / rhs;

                    instruction_pointer += 1
                }
                Instruction::ILT { lhs, rhs, target } => {
                    let lhs = &self.register_values[lhs.0];
                    let rhs = &self.register_values[rhs.0];

                    self.register_values[target.0] = (lhs < rhs) as i64;

                    instruction_pointer += 1;
                }
                Instruction::ILTE { lhs, rhs, target } => {
                    let lhs = &self.register_values[lhs.0];
                    let rhs = &self.register_values[rhs.0];

                    self.register_values[target.0] = (lhs <= rhs) as i64;

                    instruction_pointer += 1;
                }
                Instruction::IGT { lhs, rhs, target } => {
                    let lhs = &self.register_values[lhs.0];
                    let rhs = &self.register_values[rhs.0];

                    self.register_values[target.0] = (lhs > rhs) as i64;

                    instruction_pointer += 1;
                }
                Instruction::IGTE { lhs, rhs, target } => {
                    let lhs = &self.register_values[lhs.0];
                    let rhs = &self.register_values[rhs.0];

                    self.register_values[target.0] = (lhs >= rhs) as i64;

                    instruction_pointer += 1;
                }
                Instruction::MOV { target, source } => {
                    self.register_values[target.0] = self.register_values[source.0];
                    instruction_pointer += 1;
                }
                Instruction::BRIF {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    let condition = self.register_values[condition.0];

                    if condition == 0 {
                        instruction_pointer = else_branch.0;
                    } else {
                        instruction_pointer = then_branch.0;
                    }
                }
                Instruction::JMP(location) => {
                    instruction_pointer = location.0;
                }
                Instruction::EXTERNAL_CALL { head, args, target } => {
                    let output = self.eval_external_call(*head, args, *target, functions);

                    self.unbox_to_register(output, *target);
                    instruction_pointer += 1;
                }
            }
        }

        (self.register_values[0], self.register_types[0])
    }

    pub fn eval_external_call(
        &self,
        head: FunctionId,
        args: &[RegisterId],
        target: RegisterId,
        functions: &[Function],
    ) -> Box<dyn Any> {
        match &functions[head.0] {
            Function::ExternalFn0(fun) => fun().unwrap(),
            Function::ExternalFn1(fun) => {
                let mut val = self.box_register(args[0]);

                fun(&mut val).unwrap()
            }
            Function::ExternalFn2(fun) => {
                let mut arg0 = self.box_register(args[0]);
                let mut arg1 = self.box_register(args[1]);

                fun(&mut arg0, &mut arg1).unwrap()
            }
            _ => Box::new(0),
        }
    }

    pub fn box_register(&self, register_id: RegisterId) -> Box<dyn Any> {
        Box::new(self.register_values[register_id.0])
    }

    pub fn unbox_to_register(&mut self, value: Box<dyn Any>, target: RegisterId) {
        if let Ok(value) = value.downcast::<i64>() {
            self.register_values[target.0] = *value;
        }
    }

    pub fn debug_print(&self) {
        println!("instructions:");
        for instr in self.instructions.iter().enumerate() {
            println!("{:?}", instr);
        }
        println!("registers:");
        for (idx, value) in self.register_values.iter().enumerate() {
            println!("{}: {} ({:?})", idx, value, self.register_types[idx]);
        }
    }

    pub fn next_position(&self) -> usize {
        self.instructions.len()
    }
}

pub struct Translater {
    var_lookup: HashMap<NodeId, RegisterId>,
}

impl Translater {
    pub fn new() -> Self {
        Translater {
            var_lookup: HashMap::new(),
        }
    }

    pub fn translate<'source>(
        &mut self,
        delta: &'source EngineDelta,
        typechecker: &TypeChecker,
    ) -> FunctionCodegen {
        let mut builder = FunctionCodegen {
            instructions: vec![],
            register_values: vec![],
            register_types: vec![],
        };
        if !delta.ast_nodes.is_empty() {
            let last = delta.ast_nodes.len() - 1;
            let result = self.translate_node(&mut builder, NodeId(last), delta, typechecker);
            builder.mov(RegisterId(0), result);
        }

        builder
    }

    pub fn translate_node<'source>(
        &mut self,
        builder: &mut FunctionCodegen,
        node_id: NodeId,
        delta: &'source EngineDelta,
        typechecker: &TypeChecker,
    ) -> RegisterId {
        match &delta.ast_nodes[node_id.0] {
            AstNode::Int => self.translate_int(builder, node_id, delta, typechecker),
            AstNode::BinaryOp { lhs, op, rhs } => {
                self.translate_binop(builder, *lhs, *op, *rhs, delta, typechecker)
            }
            AstNode::Block(nodes) => self.translate_block(builder, nodes, delta, typechecker),
            AstNode::True => builder.bool_const(true),
            AstNode::False => builder.bool_const(false),
            AstNode::Let {
                variable_name,
                initializer,
                ..
            } => self.translate_var_decl(builder, *variable_name, *initializer, delta, typechecker),
            AstNode::Variable => self.translate_variable(node_id, typechecker),
            AstNode::Statement(node_id) => {
                self.translate_node(builder, *node_id, delta, typechecker)
            }
            AstNode::If {
                condition,
                then_block,
                else_expression,
            } => self.translate_if(
                builder,
                *condition,
                *then_block,
                *else_expression,
                delta,
                typechecker,
            ),
            AstNode::While { condition, block } => {
                self.translate_while(builder, *condition, *block, delta, typechecker)
            }
            AstNode::Call { head, args } => {
                self.translate_call(builder, *head, args, delta, typechecker)
            }
            x => panic!("unsupported translation: {:?}", x),
        }
    }

    pub fn translate_int<'source>(
        &mut self,
        builder: &mut FunctionCodegen,
        node_id: NodeId,
        delta: &'source EngineDelta,
        typechecker: &TypeChecker,
    ) -> RegisterId {
        let contents = &delta.contents[delta.span_start[node_id.0]..delta.span_end[node_id.0]];

        let constant = i64::from_str_radix(&String::from_utf8_lossy(contents), 10)
            .expect("internal error: int constant could not be parsed");

        builder.i64_const(constant)
    }

    pub fn translate_binop<'source>(
        &mut self,
        builder: &mut FunctionCodegen,
        lhs: NodeId,
        op: NodeId,
        rhs: NodeId,
        delta: &'source EngineDelta,
        typechecker: &TypeChecker,
    ) -> RegisterId {
        let lhs = self.translate_node(builder, lhs, delta, typechecker);
        let rhs = self.translate_node(builder, rhs, delta, typechecker);

        match delta.ast_nodes[op.0] {
            AstNode::Plus => builder.iadd(lhs, rhs),
            AstNode::Minus => builder.isub(lhs, rhs),
            AstNode::Multiply => builder.imul(lhs, rhs),
            AstNode::Divide => builder.idiv(lhs, rhs),
            AstNode::LessThan => builder.ilt(lhs, rhs),
            AstNode::LessThanOrEqual => builder.ilte(lhs, rhs),
            AstNode::GreaterThan => builder.igt(lhs, rhs),
            AstNode::GreaterThanOrEqual => builder.igte(lhs, rhs),
            AstNode::Assignment => builder.mov(lhs, rhs),
            _ => panic!("unsupported operation"),
        }
    }

    pub fn translate_var_decl<'source>(
        &mut self,
        builder: &mut FunctionCodegen,
        variable_name: NodeId,
        initializer: NodeId,
        delta: &'source EngineDelta,
        typechecker: &TypeChecker,
    ) -> RegisterId {
        let initializer = self.translate_node(builder, initializer, delta, typechecker);

        self.var_lookup.insert(variable_name, initializer);

        initializer
    }

    pub fn translate_variable<'source>(
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

    pub fn translate_if<'source>(
        &mut self,
        builder: &mut FunctionCodegen,
        condition: NodeId,
        then_block: NodeId,
        else_expression: Option<NodeId>,
        delta: &'source EngineDelta,
        typechecker: &TypeChecker,
    ) -> RegisterId {
        let output = builder.new_register(ValueType::Unknown);
        let condition = self.translate_node(builder, condition, delta, typechecker);

        let brif_location = builder.next_position();
        builder.brif(output, condition, InstructionId(0), InstructionId(0));

        let then_branch = InstructionId(builder.next_position());
        let then_output = self.translate_node(builder, then_block, delta, typechecker);
        builder.mov(output, then_output);

        let else_branch = if let Some(else_expression) = else_expression {
            // Create a jump with a temporary location we'll replace when we know the correct one
            // Remember where it is so we can replace it later
            let jmp_location = builder.next_position();
            builder.jmp(InstructionId(0));

            let else_location = builder.next_position();
            let else_output = self.translate_node(builder, else_expression, delta, typechecker);
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

    pub fn translate_while<'source>(
        &mut self,
        builder: &mut FunctionCodegen,
        condition: NodeId,
        block: NodeId,
        delta: &'source EngineDelta,
        typechecker: &TypeChecker,
    ) -> RegisterId {
        let output = builder.new_register(ValueType::Void);

        let top = builder.next_position();
        let condition = self.translate_node(builder, condition, delta, typechecker);

        let brif_location = builder.next_position();
        builder.brif(output, condition, InstructionId(0), InstructionId(0));

        let block_begin = InstructionId(builder.next_position());
        self.translate_node(builder, block, delta, typechecker);
        builder.jmp(InstructionId(top));

        let block_end = InstructionId(builder.next_position());

        builder.instructions[brif_location] = Instruction::BRIF {
            condition,
            then_branch: block_begin,
            else_branch: block_end,
        };

        output
    }

    pub fn translate_block<'source>(
        &mut self,
        builder: &mut FunctionCodegen,
        nodes: &[NodeId],
        delta: &'source EngineDelta,
        typechecker: &TypeChecker,
    ) -> RegisterId {
        if nodes.is_empty() {
            return builder.new_register(ValueType::Void);
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

    pub fn translate_call<'source>(
        &mut self,
        builder: &mut FunctionCodegen,
        head: NodeId,
        args: &[NodeId],
        delta: &'source EngineDelta,
        typechecker: &TypeChecker,
    ) -> RegisterId {
        let output = builder.new_register(ValueType::Unknown);

        let head = typechecker
            .call_resolution
            .get(&head)
            .expect("internal error: call should be resolved");

        let mut translated_args = vec![];

        for node_id in args {
            translated_args.push(self.translate_node(builder, *node_id, delta, typechecker));
        }

        builder.external_call(*head, translated_args, output);

        output
    }
}
