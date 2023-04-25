use std::collections::HashMap;

use crate::{
    delta::EngineDelta,
    parser::{AstNode, NodeId},
    typechecker::{TypeChecker, I64_TYPE},
};

#[derive(Clone, Copy, Debug)]
pub struct ValueId(usize);

#[derive(Debug, Clone, PartialEq)]
pub enum ValueType {
    Unknown,
    Void,
    I64,
    F64,
    Bool,
}

#[derive(Debug, Clone, PartialEq)]
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
        lhs: ValueId,
        rhs: ValueId,
        target: ValueId,
    },
    ISUB {
        lhs: ValueId,
        rhs: ValueId,
        target: ValueId,
    },
    IMUL {
        lhs: ValueId,
        rhs: ValueId,
        target: ValueId,
    },
    IDIV {
        lhs: ValueId,
        rhs: ValueId,
        target: ValueId,
    },

    // Integer comparisons (e.g., ILT = Integer + LessThan)
    ILT {
        lhs: ValueId,
        rhs: ValueId,
        target: ValueId,
    },
    ILTE {
        lhs: ValueId,
        rhs: ValueId,
        target: ValueId,
    },
    IGT {
        lhs: ValueId,
        rhs: ValueId,
        target: ValueId,
    },
    IGTE {
        lhs: ValueId,
        rhs: ValueId,
        target: ValueId,
    },

    MOV {
        target: ValueId,
        source: ValueId,
    },
}

pub struct FunctionCodegen {
    pub instructions: Vec<Instruction>,
    pub registers: Vec<Value>,
}

impl FunctionCodegen {
    pub fn new_register_with_value(&mut self, value: Value) -> ValueId {
        self.registers.push(value);

        ValueId(self.registers.len() - 1)
    }

    pub fn new_register(&mut self, ty: ValueType) -> ValueId {
        self.new_register_with_value(Value::new_i64(ty, 0))
    }

    pub fn i64_const(&mut self, value: i64) -> ValueId {
        self.new_register_with_value(Value::new_i64(ValueType::I64, value))
    }

    pub fn bool_const(&mut self, value: bool) -> ValueId {
        if value {
            self.new_register_with_value(Value::new_i64(ValueType::Bool, 1))
        } else {
            self.new_register_with_value(Value::new_i64(ValueType::Bool, 0))
        }
    }

    pub fn iadd(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let target = self.new_register(ValueType::I64);

        self.instructions
            .push(Instruction::IADD { lhs, rhs, target });

        target
    }

    pub fn isub(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let target = self.new_register(ValueType::I64);

        self.instructions
            .push(Instruction::ISUB { lhs, rhs, target });

        target
    }

    pub fn imul(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let target = self.new_register(ValueType::I64);

        self.instructions
            .push(Instruction::IMUL { lhs, rhs, target });

        target
    }

    pub fn idiv(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let target = self.new_register(ValueType::I64);

        self.instructions
            .push(Instruction::IDIV { lhs, rhs, target });

        target
    }

    pub fn ilt(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let target = self.new_register(ValueType::Bool);

        self.instructions
            .push(Instruction::ILT { lhs, rhs, target });

        target
    }

    pub fn ilte(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let target = self.new_register(ValueType::Bool);

        self.instructions
            .push(Instruction::ILTE { lhs, rhs, target });

        target
    }

    pub fn igt(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let target = self.new_register(ValueType::Bool);

        self.instructions
            .push(Instruction::IGT { lhs, rhs, target });

        target
    }

    pub fn igte(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let target = self.new_register(ValueType::Bool);

        self.instructions
            .push(Instruction::IGTE { lhs, rhs, target });

        target
    }

    pub fn mov(&mut self, target: ValueId, source: ValueId) -> ValueId {
        self.instructions.push(Instruction::MOV { target, source });

        target
    }

    pub fn eval(&mut self) -> Value {
        let mut output = ValueId(0);
        for instr in &self.instructions {
            match instr {
                Instruction::IADD { lhs, rhs, target } => {
                    let lhs = &self.registers[lhs.0];
                    let rhs = &self.registers[rhs.0];

                    self.registers[target.0].val = lhs.val + rhs.val;

                    output = *target;
                }
                Instruction::ISUB { lhs, rhs, target } => {
                    let lhs = &self.registers[lhs.0];
                    let rhs = &self.registers[rhs.0];

                    self.registers[target.0].val = lhs.val - rhs.val;

                    output = *target;
                }
                Instruction::IMUL { lhs, rhs, target } => {
                    let lhs = &self.registers[lhs.0];
                    let rhs = &self.registers[rhs.0];

                    self.registers[target.0].val = lhs.val * rhs.val;

                    output = *target;
                }
                Instruction::IDIV { lhs, rhs, target } => {
                    let lhs = &self.registers[lhs.0];
                    let rhs = &self.registers[rhs.0];

                    self.registers[target.0].val = lhs.val / rhs.val;

                    output = *target;
                }
                Instruction::ILT { lhs, rhs, target } => {
                    let lhs = &self.registers[lhs.0];
                    let rhs = &self.registers[rhs.0];

                    self.registers[target.0].val = (lhs.val < rhs.val) as i64;

                    output = *target;
                }
                Instruction::ILTE { lhs, rhs, target } => {
                    let lhs = &self.registers[lhs.0];
                    let rhs = &self.registers[rhs.0];

                    self.registers[target.0].val = (lhs.val <= rhs.val) as i64;

                    output = *target;
                }
                Instruction::IGT { lhs, rhs, target } => {
                    let lhs = &self.registers[lhs.0];
                    let rhs = &self.registers[rhs.0];

                    self.registers[target.0].val = (lhs.val > rhs.val) as i64;

                    output = *target;
                }
                Instruction::IGTE { lhs, rhs, target } => {
                    let lhs = &self.registers[lhs.0];
                    let rhs = &self.registers[rhs.0];

                    self.registers[target.0].val = (lhs.val >= rhs.val) as i64;

                    output = *target;
                }
                Instruction::MOV { target, source } => {
                    self.registers[target.0].val = self.registers[source.0].val;
                }
            }
        }

        self.registers[output.0].clone()
    }

    pub fn debug_print(&self) {
        println!("instructions:");
        for instr in self.instructions.iter().enumerate() {
            println!("{:?}", instr);
        }
        println!("registers:");
        for register in self.registers.iter().enumerate() {
            println!("{:?}", register);
        }
    }
}

pub struct Translater {
    var_lookup: HashMap<NodeId, ValueId>,
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
            registers: vec![],
        };
        if !delta.ast_nodes.is_empty() {
            let last = delta.ast_nodes.len() - 1;
            self.translate_node(&mut builder, NodeId(last), delta, typechecker);
        }

        builder
    }

    pub fn translate_node<'source>(
        &mut self,
        builder: &mut FunctionCodegen,
        node_id: NodeId,
        delta: &'source EngineDelta,
        typechecker: &TypeChecker,
    ) -> ValueId {
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
            x => panic!("unsupported translation: {:?}", x),
        }
    }

    pub fn translate_int<'source>(
        &mut self,
        builder: &mut FunctionCodegen,
        node_id: NodeId,
        delta: &'source EngineDelta,
        typechecker: &TypeChecker,
    ) -> ValueId {
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
    ) -> ValueId {
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
    ) -> ValueId {
        //let ty = typechecker.node_types[variable_name.0];

        let initializer = self.translate_node(builder, initializer, delta, typechecker);

        self.var_lookup.insert(variable_name, initializer);

        initializer
    }

    pub fn translate_variable<'source>(
        &mut self,
        variable_name: NodeId,
        typechecker: &TypeChecker,
    ) -> ValueId {
        let def_site = typechecker
            .variable_def
            .get(&variable_name)
            .expect("internal error: resolved variable not found");

        let value_id = self
            .var_lookup
            .get(def_site)
            .expect("internal error: resolved variable missing definition");

        *value_id
    }

    pub fn translate_block<'source>(
        &mut self,
        builder: &mut FunctionCodegen,
        nodes: &[NodeId],
        delta: &'source EngineDelta,
        typechecker: &TypeChecker,
    ) -> ValueId {
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
}
