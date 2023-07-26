use std::any::Any;

use crate::{
    codegen::{FunctionCodegen, Instruction, InstructionId, RegisterId},
    parser::NodeId,
    typechecker::{ExternalFnRecord, ExternalFunctionId, Function, FunctionId},
    ScriptError, TypeChecker, TypeId, BOOL_TYPE, F64_TYPE, I64_TYPE, VOID_TYPE,
};

#[derive(Clone)]
pub struct StackFrame {
    pub register_values: Vec<i64>,
    pub register_types: Vec<TypeId>,
    pub instruction_pointer: InstructionId,
}

#[derive(Default)]
pub struct Evaluator {
    pub instructions: Vec<Instruction>,
    pub source_map: Vec<NodeId>,
    pub current_frame: usize,

    pub span_start: Vec<usize>,
    pub span_end: Vec<usize>,

    // Indexed by FunctionId
    pub functions: Vec<StackFrame>,

    // The live stack frames during evaluation
    pub stack_frames: Vec<StackFrame>,
}

#[derive(PartialEq, Debug)]
pub enum ReturnValue {
    I64(i64),
    F64(f64),
    Bool(bool),
    Custom { value: i64, type_id: TypeId },
    Error(ScriptError),
}

impl Evaluator {
    pub fn add_function(&mut self, mut function_codegen: FunctionCodegen) {
        let function_entry = self.instructions.len();
        function_codegen.offset_instruction_addresses(function_entry);
        let stack_frame = StackFrame {
            register_values: function_codegen.register_values,
            register_types: function_codegen.register_types,
            instruction_pointer: InstructionId(function_entry),
        };
        self.instructions.append(&mut function_codegen.instructions);
        self.source_map.append(&mut function_codegen.source_map);

        self.span_start = function_codegen.span_start;
        self.span_end = function_codegen.span_end;

        self.functions.push(stack_frame);
    }

    #[cfg(not(feature = "async"))]
    pub fn eval(
        &mut self,
        starting_function: FunctionId,
        external_functions: &[ExternalFnRecord],
    ) -> ReturnValue {
        self.current_frame = self.stack_frames.len();
        self.stack_frames
            .push(self.functions[starting_function.0].clone());
        let mut instruction_pointer = self.stack_frames[self.current_frame].instruction_pointer.0;

        loop {
            if let Some(ret_val) = self.eval_helper(&mut instruction_pointer, external_functions) {
                return ret_val;
            }
        }
    }

    #[cfg(feature = "async")]
    pub async fn eval(
        &mut self,
        starting_function: FunctionId,
        external_functions: &[ExternalFnRecord],
    ) -> ReturnValue {
        self.current_frame = self.stack_frames.len();
        self.stack_frames
            .push(self.functions[starting_function.0].clone());
        let mut instruction_pointer = self.stack_frames[self.current_frame].instruction_pointer.0;

        loop {
            match &self.instructions[instruction_pointer] {
                Instruction::ASYNCCALL { target } => {
                    // TODO: for now, this will just be a test function we run
                    let result = test_async_fn().await;
                    self.stack_frames[self.current_frame].register_values[target.0] = result;

                    instruction_pointer += 1;
                }
                _ => {
                    if let Some(ret_val) =
                        self.eval_helper(&mut instruction_pointer, external_functions)
                    {
                        return ret_val;
                    }
                }
            }
        }
    }

    fn eval_helper(
        &mut self,
        instruction_pointer: &mut usize,
        external_functions: &[ExternalFnRecord],
    ) -> Option<ReturnValue> {
        match &self.instructions[*instruction_pointer] {
            Instruction::IADD { lhs, rhs, target } => {
                self.stack_frames[self.current_frame].register_values[target.0] =
                    self.stack_frames[self.current_frame].register_values[lhs.0]
                        + self.stack_frames[self.current_frame].register_values[rhs.0];

                *instruction_pointer += 1;
            }
            Instruction::ISUB { lhs, rhs, target } => {
                self.stack_frames[self.current_frame].register_values[target.0] =
                    self.stack_frames[self.current_frame].register_values[lhs.0]
                        - self.stack_frames[self.current_frame].register_values[rhs.0];

                *instruction_pointer += 1;
            }
            Instruction::IMUL { lhs, rhs, target } => {
                self.stack_frames[self.current_frame].register_values[target.0] =
                    self.stack_frames[self.current_frame].register_values[lhs.0]
                        * self.stack_frames[self.current_frame].register_values[rhs.0];

                *instruction_pointer += 1;
            }
            Instruction::IDIV { lhs, rhs, target } => {
                if self.stack_frames[self.current_frame].register_values[rhs.0] == 0 {
                    return Some(ReturnValue::Error(
                        self.error("division by zero", self.source_map[*instruction_pointer]),
                    ));
                }
                self.stack_frames[self.current_frame].register_values[target.0] =
                    self.stack_frames[self.current_frame].register_values[lhs.0]
                        / self.stack_frames[self.current_frame].register_values[rhs.0];

                *instruction_pointer += 1
            }
            Instruction::ILT { lhs, rhs, target } => {
                let lhs = self.stack_frames[self.current_frame].register_values[lhs.0];
                let rhs = self.stack_frames[self.current_frame].register_values[rhs.0];

                self.stack_frames[self.current_frame].register_values[target.0] =
                    (lhs < rhs) as i64;

                *instruction_pointer += 1;
            }
            Instruction::ILTE { lhs, rhs, target } => {
                let lhs = self.stack_frames[self.current_frame].register_values[lhs.0];
                let rhs = self.stack_frames[self.current_frame].register_values[rhs.0];

                self.stack_frames[self.current_frame].register_values[target.0] =
                    (lhs <= rhs) as i64;

                *instruction_pointer += 1;
            }
            Instruction::IGT { lhs, rhs, target } => {
                let lhs = self.stack_frames[self.current_frame].register_values[lhs.0];
                let rhs = self.stack_frames[self.current_frame].register_values[rhs.0];

                self.stack_frames[self.current_frame].register_values[target.0] =
                    (lhs > rhs) as i64;

                *instruction_pointer += 1;
            }
            Instruction::IGTE { lhs, rhs, target } => {
                let lhs = self.stack_frames[self.current_frame].register_values[lhs.0];
                let rhs = self.stack_frames[self.current_frame].register_values[rhs.0];

                self.stack_frames[self.current_frame].register_values[target.0] =
                    (lhs >= rhs) as i64;

                *instruction_pointer += 1;
            }
            Instruction::FADD { lhs, rhs, target } => {
                let lhs = f64::from_bits(
                    self.stack_frames[self.current_frame].register_values[lhs.0] as u64,
                );
                let rhs = f64::from_bits(
                    self.stack_frames[self.current_frame].register_values[rhs.0] as u64,
                );

                self.stack_frames[self.current_frame].register_values[target.0] =
                    (lhs + rhs).to_bits() as i64;

                *instruction_pointer += 1;
            }
            Instruction::FSUB { lhs, rhs, target } => {
                let lhs = f64::from_bits(
                    self.stack_frames[self.current_frame].register_values[lhs.0] as u64,
                );
                let rhs = f64::from_bits(
                    self.stack_frames[self.current_frame].register_values[rhs.0] as u64,
                );

                self.stack_frames[self.current_frame].register_values[target.0] =
                    (lhs - rhs).to_bits() as i64;

                *instruction_pointer += 1;
            }
            Instruction::FMUL { lhs, rhs, target } => {
                let lhs = f64::from_bits(
                    self.stack_frames[self.current_frame].register_values[lhs.0] as u64,
                );
                let rhs = f64::from_bits(
                    self.stack_frames[self.current_frame].register_values[rhs.0] as u64,
                );

                self.stack_frames[self.current_frame].register_values[target.0] =
                    (lhs * rhs).to_bits() as i64;

                *instruction_pointer += 1;
            }
            Instruction::FDIV { lhs, rhs, target } => {
                let lhs = f64::from_bits(
                    self.stack_frames[self.current_frame].register_values[lhs.0] as u64,
                );
                let rhs = f64::from_bits(
                    self.stack_frames[self.current_frame].register_values[rhs.0] as u64,
                );

                self.stack_frames[self.current_frame].register_values[target.0] =
                    (lhs / rhs).to_bits() as i64;

                *instruction_pointer += 1;
            }
            Instruction::FLT { lhs, rhs, target } => {
                let lhs = f64::from_bits(
                    self.stack_frames[self.current_frame].register_values[lhs.0] as u64,
                );
                let rhs = f64::from_bits(
                    self.stack_frames[self.current_frame].register_values[rhs.0] as u64,
                );

                self.stack_frames[self.current_frame].register_values[target.0] =
                    (lhs < rhs) as i64;

                *instruction_pointer += 1;
            }
            Instruction::FLTE { lhs, rhs, target } => {
                let lhs = f64::from_bits(
                    self.stack_frames[self.current_frame].register_values[lhs.0] as u64,
                );
                let rhs = f64::from_bits(
                    self.stack_frames[self.current_frame].register_values[rhs.0] as u64,
                );

                self.stack_frames[self.current_frame].register_values[target.0] =
                    (lhs <= rhs) as i64;

                *instruction_pointer += 1;
            }
            Instruction::FGT { lhs, rhs, target } => {
                let lhs = f64::from_bits(
                    self.stack_frames[self.current_frame].register_values[lhs.0] as u64,
                );
                let rhs = f64::from_bits(
                    self.stack_frames[self.current_frame].register_values[rhs.0] as u64,
                );

                self.stack_frames[self.current_frame].register_values[target.0] =
                    (lhs > rhs) as i64;

                *instruction_pointer += 1;
            }
            Instruction::FGTE { lhs, rhs, target } => {
                let lhs = f64::from_bits(
                    self.stack_frames[self.current_frame].register_values[lhs.0] as u64,
                );
                let rhs = f64::from_bits(
                    self.stack_frames[self.current_frame].register_values[rhs.0] as u64,
                );

                self.stack_frames[self.current_frame].register_values[target.0] =
                    (lhs >= rhs) as i64;

                *instruction_pointer += 1;
            }
            Instruction::MOV { target, source } => {
                self.stack_frames[self.current_frame].register_values[target.0] =
                    self.stack_frames[self.current_frame].register_values[source.0];
                *instruction_pointer += 1;
            }
            Instruction::BRIF {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition = self.stack_frames[self.current_frame].register_values[condition.0];

                if condition == 0 {
                    *instruction_pointer = else_branch.0;
                } else {
                    *instruction_pointer = then_branch.0;
                }
            }
            Instruction::JMP(location) => {
                *instruction_pointer = location.0;
            }
            Instruction::EXTERNALCALL { head, args, target } => {
                let target = *target;
                let output = self.eval_external_call(*head, args, external_functions);

                println!("external call into: {:?}", target);
                println!(
                    "target is: {:?}",
                    self.stack_frames[self.current_frame].register_types[target.0]
                );
                self.unbox_to_register(output, target);
                println!(
                    "register is now: {}",
                    self.stack_frames[self.current_frame].register_values[target.0]
                );
                *instruction_pointer += 1;
            }
            Instruction::RET => {
                if self.stack_frames.len() > 1 {
                    self.stack_frames.pop();
                    self.current_frame -= 1;
                    *instruction_pointer =
                        self.stack_frames[self.current_frame].instruction_pointer.0;
                } else {
                    match self.stack_frames[self.current_frame].register_types[0] {
                        BOOL_TYPE => {
                            return Some(ReturnValue::Bool(
                                self.stack_frames[self.current_frame].register_values[0] != 0,
                            ))
                        }
                        I64_TYPE => {
                            return Some(ReturnValue::I64(
                                self.stack_frames[self.current_frame].register_values[0],
                            ))
                        }
                        F64_TYPE => {
                            return Some(ReturnValue::F64(f64::from_bits(
                                self.stack_frames[self.current_frame].register_values[0] as u64,
                            )))
                        }
                        _ => {
                            return Some(ReturnValue::Custom {
                                value: self.stack_frames[self.current_frame].register_values[0],
                                type_id: self.stack_frames[self.current_frame].register_types[0],
                            })
                        }
                    }
                }
            }
            _ => {
                panic!(
                    "Unsupported opcode: {:?}",
                    self.instructions[*instruction_pointer]
                )
            }
        }

        None
    }

    pub fn eval_external_call(
        &self,
        head: ExternalFunctionId,
        args: &[RegisterId],
        functions: &[ExternalFnRecord],
    ) -> Box<dyn Any> {
        match &functions[head.0].fun {
            Function::ExternalFn0(fun) => fun().unwrap(),
            Function::ExternalFn1(fun) => {
                let mut arg0 = self.box_register(args[0]);

                let result = fun(&mut arg0).unwrap();

                if self.is_user_type(args[0]) {
                    // We leak the box here because we manually clean it up later
                    Box::leak(arg0);
                }

                result
            }
            Function::ExternalFn2(fun) => {
                let mut arg0 = self.box_register(args[0]);
                let mut arg1 = self.box_register(args[1]);

                let result = fun(&mut arg0, &mut arg1).unwrap();

                if self.is_user_type(args[0]) {
                    // We leak the box here because we manually clean it up later
                    Box::leak(arg0);
                }
                if self.is_user_type(args[1]) {
                    // We leak the box here because we manually clean it up later
                    Box::leak(arg1);
                }

                result
            }
            Function::ExternalFn3(fun) => {
                let mut arg0 = self.box_register(args[0]);
                let mut arg1 = self.box_register(args[1]);
                let mut arg2 = self.box_register(args[2]);

                let result = fun(&mut arg0, &mut arg1, &mut arg2).unwrap();

                if self.is_user_type(args[0]) {
                    // We leak the box here because we manually clean it up later
                    Box::leak(arg0);
                }
                if self.is_user_type(args[1]) {
                    // We leak the box here because we manually clean it up later
                    Box::leak(arg1);
                }
                if self.is_user_type(args[2]) {
                    // We leak the box here because we manually clean it up later
                    Box::leak(arg2);
                }

                result
            }
        }
    }

    pub fn box_register(&self, register_id: RegisterId) -> Box<dyn Any> {
        if self.stack_frames[self.current_frame].register_types[register_id.0] == F64_TYPE {
            let val = f64::from_bits(
                self.stack_frames[self.current_frame].register_values[register_id.0] as u64,
            );
            Box::new(val)
        } else if self.stack_frames[self.current_frame].register_types[register_id.0] == BOOL_TYPE {
            let val = self.stack_frames[self.current_frame].register_values[register_id.0] != 0;
            Box::new(val)
        } else if self.stack_frames[self.current_frame].register_types[register_id.0] == I64_TYPE {
            Box::new(self.stack_frames[self.current_frame].register_values[register_id.0])
        } else {
            println!(
                "transmuting: {} with {}",
                register_id.0, self.stack_frames[self.current_frame].register_values[register_id.0]
            );
            let boxed = unsafe {
                std::mem::transmute::<i64, Box<Box<dyn Any>>>(
                    self.stack_frames[self.current_frame].register_values[register_id.0],
                )
            };

            let boxed = Box::leak(boxed);

            let leaked = &mut **boxed as *mut dyn Any;

            unsafe { Box::from_raw(leaked) }
        }
    }

    pub fn unbox_to_register(&mut self, value: Box<dyn Any>, target: RegisterId) {
        if self.stack_frames[self.current_frame].register_types[target.0] == F64_TYPE {
            if let Ok(value) = value.downcast::<f64>() {
                let val = (*value).to_bits() as i64;

                self.stack_frames[self.current_frame].register_values[target.0] = val;
            } else {
                panic!("internal error: could not properly handle conversion of register to i64")
            }
        } else if self.stack_frames[self.current_frame].register_types[target.0] == BOOL_TYPE {
            if let Ok(value) = value.downcast::<bool>() {
                let val = *value as i64;

                self.stack_frames[self.current_frame].register_values[target.0] = val;
            } else {
                panic!("internal error: could not properly handle conversion of register to i64")
            }
        } else if self.stack_frames[self.current_frame].register_types[target.0] == VOID_TYPE {
            // Ignore this case, as void creates no changes
        } else if self.stack_frames[self.current_frame].register_types[target.0] == I64_TYPE {
            if let Ok(value) = value.downcast::<i64>() {
                self.stack_frames[self.current_frame].register_values[target.0] = *value;
            } else {
                panic!("internal error: could not properly handle conversion of register to i64")
            }
        } else {
            self.stack_frames[self.current_frame].register_values[target.0] =
                unsafe { std::mem::transmute::<Box<Box<dyn Any>>, i64>(Box::new(value)) };

            println!(
                "setting {} into {}",
                target.0, self.stack_frames[self.current_frame].register_values[target.0]
            )
        }
    }

    pub fn debug_print(&self, typechecker: &TypeChecker) {
        println!("virtual machine:");
        println!("  instructions:");
        for instr in self.instructions.iter().enumerate() {
            println!("    {:?}", instr);
        }
        println!("  registers:");
        for (idx, value) in self.stack_frames[self.current_frame]
            .register_values
            .iter()
            .enumerate()
        {
            if self.stack_frames[self.current_frame].register_types[idx] == F64_TYPE {
                println!(
                    "    {}: {} ({})",
                    idx,
                    f64::from_bits(*value as u64),
                    typechecker
                        .stringify_type(self.stack_frames[self.current_frame].register_types[idx])
                );
            } else {
                println!(
                    "    {}: {} ({})",
                    idx,
                    *value,
                    typechecker
                        .stringify_type(self.stack_frames[self.current_frame].register_types[idx])
                );
            }
        }
    }

    pub fn is_user_type(&self, register_id: RegisterId) -> bool {
        self.stack_frames[self.current_frame].register_types[register_id.0].0 > BOOL_TYPE.0
    }

    pub fn error(&self, msg: impl Into<String>, node_id: NodeId) -> ScriptError {
        let span_start = self.span_start[node_id.0];
        let span_end = self.span_end[node_id.0];

        ScriptError {
            message: msg.into(),
            span_start,
            span_end,
        }
    }
}

// Test function for async until we build out full support
#[cfg(feature = "async")]
async fn test_async_fn() -> i64 {
    42
}
