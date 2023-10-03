use std::any::Any;

use crate::{
    codegen::{FunctionCodegen, Instruction, InstructionId, RegisterId, RegisterValue},
    engine::ExternalFnRecord,
    parser::NodeId,
    typechecker::{ExternalFunctionId, Function, FunctionId, STRING_TYPE},
    ScriptError, TypeChecker, TypeId, BOOL_TYPE, F64_TYPE, I64_TYPE, UNIT_TYPE,
};

#[derive(Clone)]
pub struct StackFrame {
    pub register_values: Vec<RegisterValue>,
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

impl Drop for Evaluator {
    fn drop(&mut self) {
        let num_frames = self.stack_frames.len();
        for frame_id in (0..num_frames).rev() {
            let stack_frame = &self.stack_frames[frame_id];
            let num_registers = stack_frame.register_values.len();

            for register_id in (0..num_registers).map(RegisterId).rev() {
                self.maybe_free_register(register_id);
            }
        }
    }
}

#[derive(Debug)]
pub enum ReturnValue {
    Unit,
    I64(i64),
    F64(f64),
    Bool(bool),
    String(String),
    Custom(Box<dyn Any>),
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

    #[inline]
    pub fn get_reg_i64(&self, register_id: RegisterId) -> i64 {
        unsafe { self.stack_frames[self.current_frame].register_values[register_id.0].i64 }
    }

    #[inline]
    pub fn get_reg_f64(&self, register_id: RegisterId) -> f64 {
        unsafe { self.stack_frames[self.current_frame].register_values[register_id.0].f64 }
    }

    #[inline]
    pub fn get_reg_bool(&self, register_id: RegisterId) -> bool {
        unsafe { self.stack_frames[self.current_frame].register_values[register_id.0].bool }
    }

    #[inline]
    pub fn get_reg_string(&self, register_id: RegisterId) -> Box<String> {
        let ptr =
            unsafe { self.stack_frames[self.current_frame].register_values[register_id.0].ptr };
        unsafe { Box::from_raw(ptr as _) }
    }

    #[inline]
    /// Take a given register's contents, leaving a copy of the pointer in the register. Users must
    /// leak the box to prevent the underlying storage from being freed.
    ///
    /// SAFETY: the underlying storage of the data stored at `register_id` must not have been freed
    /// by a previous access
    ///
    /// TODO: we need to either mark this function unsafe or change how we do this in the future
    pub fn get_user_type(&self, register_id: RegisterId) -> Box<dyn Any> {
        println!(
            "transmuting: {} with {}",
            register_id.0,
            self.get_reg_i64(register_id)
        );
        let boxed = unsafe {
            std::mem::transmute::<i64, Box<Box<dyn Any>>>(
                self.stack_frames[self.current_frame].register_values[register_id.0].i64,
            )
        };

        let boxed = Box::leak(boxed);

        let leaked = &mut **boxed as *mut dyn Any;

        unsafe { Box::from_raw(leaked) }
    }

    #[inline]
    pub fn eval_common_opcode(
        &mut self,
        instruction_pointer: &mut usize,
    ) -> Option<Result<ReturnValue, ScriptError>> {
        match self.instructions[*instruction_pointer] {
            Instruction::IADD { lhs, rhs, target } => {
                self.stack_frames[self.current_frame].register_values[target.0].i64 =
                    self.get_reg_i64(lhs) + self.get_reg_i64(rhs);
                *instruction_pointer += 1;
            }
            Instruction::ISUB { lhs, rhs, target } => {
                self.stack_frames[self.current_frame].register_values[target.0].i64 =
                    self.get_reg_i64(lhs) - self.get_reg_i64(rhs);

                *instruction_pointer += 1;
            }
            Instruction::IMUL { lhs, rhs, target } => {
                self.stack_frames[self.current_frame].register_values[target.0].i64 =
                    self.get_reg_i64(lhs) * self.get_reg_i64(rhs);

                *instruction_pointer += 1;
            }
            Instruction::IDIV { lhs, rhs, target } => {
                if self.get_reg_i64(rhs) == 0 {
                    return Some(Err(
                        self.error("division by zero", self.source_map[*instruction_pointer])
                    ));
                }
                self.stack_frames[self.current_frame].register_values[target.0].i64 =
                    self.get_reg_i64(lhs) / self.get_reg_i64(rhs);

                *instruction_pointer += 1
            }
            Instruction::ILT { lhs, rhs, target } => {
                self.stack_frames[self.current_frame].register_values[target.0].bool =
                    self.get_reg_i64(lhs) < self.get_reg_i64(rhs);

                *instruction_pointer += 1;
            }
            Instruction::ILTE { lhs, rhs, target } => {
                self.stack_frames[self.current_frame].register_values[target.0].bool =
                    self.get_reg_i64(lhs) <= self.get_reg_i64(rhs);

                *instruction_pointer += 1;
            }
            Instruction::IGT { lhs, rhs, target } => {
                self.stack_frames[self.current_frame].register_values[target.0].bool =
                    self.get_reg_i64(lhs) > self.get_reg_i64(rhs);

                *instruction_pointer += 1;
            }
            Instruction::IGTE { lhs, rhs, target } => {
                self.stack_frames[self.current_frame].register_values[target.0].bool =
                    self.get_reg_i64(lhs) >= self.get_reg_i64(rhs);

                *instruction_pointer += 1;
            }
            Instruction::FADD { lhs, rhs, target } => {
                self.stack_frames[self.current_frame].register_values[target.0].f64 =
                    self.get_reg_f64(lhs) + self.get_reg_f64(rhs);

                *instruction_pointer += 1;
            }
            Instruction::FSUB { lhs, rhs, target } => {
                self.stack_frames[self.current_frame].register_values[target.0].f64 =
                    self.get_reg_f64(lhs) - self.get_reg_f64(rhs);

                *instruction_pointer += 1;
            }
            Instruction::FMUL { lhs, rhs, target } => {
                self.stack_frames[self.current_frame].register_values[target.0].f64 =
                    self.get_reg_f64(lhs) * self.get_reg_f64(rhs);

                *instruction_pointer += 1;
            }
            Instruction::FDIV { lhs, rhs, target } => {
                if self.get_reg_f64(rhs) == 0.0 {
                    return Some(Err(
                        self.error("division by zero", self.source_map[*instruction_pointer])
                    ));
                }

                self.stack_frames[self.current_frame].register_values[target.0].f64 =
                    self.get_reg_f64(lhs) / self.get_reg_f64(rhs);

                *instruction_pointer += 1;
            }
            Instruction::FLT { lhs, rhs, target } => {
                self.stack_frames[self.current_frame].register_values[target.0].bool =
                    self.get_reg_f64(lhs) < self.get_reg_f64(rhs);

                *instruction_pointer += 1;
            }
            Instruction::FLTE { lhs, rhs, target } => {
                self.stack_frames[self.current_frame].register_values[target.0].bool =
                    self.get_reg_f64(lhs) <= self.get_reg_f64(rhs);

                *instruction_pointer += 1;
            }
            Instruction::FGT { lhs, rhs, target } => {
                self.stack_frames[self.current_frame].register_values[target.0].bool =
                    self.get_reg_f64(lhs) > self.get_reg_f64(rhs);

                *instruction_pointer += 1;
            }
            Instruction::FGTE { lhs, rhs, target } => {
                self.stack_frames[self.current_frame].register_values[target.0].bool =
                    self.get_reg_f64(lhs) >= self.get_reg_f64(rhs);

                *instruction_pointer += 1;
            }
            Instruction::MOV { target, source } => {
                self.maybe_free_register(target);
                self.stack_frames[self.current_frame].register_values[target.0] =
                    self.stack_frames[self.current_frame].register_values[source.0];
                *instruction_pointer += 1;
            }
            Instruction::BRIF {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition = self.get_reg_bool(condition);

                if condition {
                    *instruction_pointer = then_branch.0;
                } else {
                    *instruction_pointer = else_branch.0;
                }
            }
            Instruction::JMP(location) => {
                *instruction_pointer = location.0;
            }
            Instruction::RET => {
                if self.stack_frames.len() > 1 {
                    self.stack_frames.pop();
                    self.current_frame -= 1;
                    *instruction_pointer =
                        self.stack_frames[self.current_frame].instruction_pointer.0;
                } else {
                    match self.stack_frames[self.current_frame].register_types[0] {
                        UNIT_TYPE => return Some(Ok(ReturnValue::Unit)),
                        BOOL_TYPE => {
                            return Some(Ok(ReturnValue::Bool(self.get_reg_bool(RegisterId(0)))))
                        }
                        I64_TYPE => {
                            return Some(Ok(ReturnValue::I64(self.get_reg_i64(RegisterId(0)))))
                        }
                        F64_TYPE => {
                            return Some(Ok(ReturnValue::F64(self.get_reg_f64(RegisterId(0)))))
                        }
                        STRING_TYPE => {
                            let string = self.get_reg_string(RegisterId(0));
                            return Some(Ok(ReturnValue::String(*string)));
                        }
                        _ => {
                            let value = self.get_user_type(RegisterId(0));
                            return Some(Ok(ReturnValue::Custom(value)));
                        }
                    }
                }
            }
            _ => {
                panic!("configuration-specific opcode found in common opcode evaluation")
            }
        }
        None
    }

    #[cfg(feature = "async")]
    pub async fn eval_async(
        &mut self,
        starting_function: FunctionId,
        external_functions: &[ExternalFnRecord],
    ) -> Result<ReturnValue, ScriptError> {
        self.current_frame = self.stack_frames.len();
        self.stack_frames
            .push(self.functions[starting_function.0].clone());
        let mut instruction_pointer = self.stack_frames[self.current_frame].instruction_pointer.0;

        loop {
            match &self.instructions[instruction_pointer] {
                Instruction::EXTERNALCALL { head, args, target } => {
                    let target = *target;

                    let output = self
                        .eval_external_call_async(*head, args, external_functions)
                        .await;

                    self.unbox_to_register(output, target);
                    instruction_pointer += 1;
                }
                _ => {
                    if let Some(ret_val) = self.eval_common_opcode(&mut instruction_pointer) {
                        return ret_val;
                    }
                }
            }
        }
    }

    pub fn eval(
        &mut self,
        starting_function: FunctionId,
        external_functions: &[ExternalFnRecord],
    ) -> Result<ReturnValue, ScriptError> {
        self.current_frame = self.stack_frames.len();
        self.stack_frames
            .push(self.functions[starting_function.0].clone());
        let mut instruction_pointer = self.stack_frames[self.current_frame].instruction_pointer.0;

        loop {
            match &self.instructions[instruction_pointer] {
                Instruction::EXTERNALCALL { head, args, target } => {
                    let target = *target;

                    let output = self.eval_external_call(
                        instruction_pointer,
                        *head,
                        args,
                        external_functions,
                    )?;

                    println!("output is: {:?}", output);

                    self.unbox_to_register(output, target);
                    instruction_pointer += 1;
                }
                _ => {
                    if let Some(ret_val) = self.eval_common_opcode(&mut instruction_pointer) {
                        return ret_val;
                    }
                }
            }
        }
    }

    fn eval_external_call(
        &self,
        instruction_pointer: usize,
        head: ExternalFunctionId,
        args: &[RegisterId],
        functions: &[ExternalFnRecord],
    ) -> Result<Box<dyn Any>, ScriptError> {
        #[allow(unreachable_patterns)]
        match &functions[head.0].fun {
            Function::ExternalFn0(fun) => match fun() {
                Ok(val) => Ok(val),
                Err(error) => Err(self.error(error, self.source_map[instruction_pointer])),
            },
            Function::ExternalFn1(fun) => {
                let mut arg0 = self.box_register(args[0]);

                let result = match fun(&mut arg0) {
                    Ok(val) => Ok(val),
                    Err(error) => Err(self.error(error, self.source_map[instruction_pointer])),
                };

                if self.is_heap_type(args[0]) {
                    // We leak the box here because we manually clean it up later
                    Box::leak(arg0);
                }

                result
            }
            Function::ExternalFn2(fun) => {
                let mut arg0 = self.box_register(args[0]);
                let mut arg1 = self.box_register(args[1]);

                let result = match fun(&mut arg0, &mut arg1) {
                    Ok(val) => Ok(val),
                    Err(error) => Err(self.error(error, self.source_map[instruction_pointer])),
                };

                if self.is_heap_type(args[0]) {
                    // We leak the box here because we manually clean it up later
                    Box::leak(arg0);
                }
                if self.is_heap_type(args[1]) {
                    // We leak the box here because we manually clean it up later
                    Box::leak(arg1);
                }

                result
            }
            Function::ExternalFn3(fun) => {
                let mut arg0 = self.box_register(args[0]);
                let mut arg1 = self.box_register(args[1]);
                let mut arg2 = self.box_register(args[2]);

                let result = match fun(&mut arg0, &mut arg1, &mut arg2) {
                    Ok(val) => Ok(val),
                    Err(error) => Err(self.error(error, self.source_map[instruction_pointer])),
                };

                if self.is_heap_type(args[0]) {
                    // We leak the box here because we manually clean it up later
                    Box::leak(arg0);
                }
                if self.is_heap_type(args[1]) {
                    // We leak the box here because we manually clean it up later
                    Box::leak(arg1);
                }
                if self.is_heap_type(args[2]) {
                    // We leak the box here because we manually clean it up later
                    Box::leak(arg2);
                }

                result
            }
            _ => unreachable!(),
        }
    }

    #[cfg(feature = "async")]
    async fn eval_external_call_async(
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

                if self.is_heap_type(args[0]) {
                    // We leak the box here because we manually clean it up later
                    Box::leak(arg0);
                }

                result
            }
            Function::ExternalFn2(fun) => {
                let mut arg0 = self.box_register(args[0]);
                let mut arg1 = self.box_register(args[1]);

                let result = fun(&mut arg0, &mut arg1).unwrap();

                if self.is_heap_type(args[0]) {
                    // We leak the box here because we manually clean it up later
                    Box::leak(arg0);
                }
                if self.is_heap_type(args[1]) {
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

                if self.is_heap_type(args[0]) {
                    // We leak the box here because we manually clean it up later
                    Box::leak(arg0);
                }
                if self.is_heap_type(args[1]) {
                    // We leak the box here because we manually clean it up later
                    Box::leak(arg1);
                }
                if self.is_heap_type(args[2]) {
                    // We leak the box here because we manually clean it up later
                    Box::leak(arg2);
                }

                result
            }
            Function::ExternalAsyncFn1(fun) => {
                let arg0 = if self.stack_frames[self.current_frame].register_types[args[0].0]
                    == I64_TYPE
                {
                    Box::new(self.get_reg_i64(args[0]))
                } else {
                    panic!("internal error: not an i64");
                };
                // let mut arg0 = self.box_register();
                fun(arg0).await.unwrap()

                // if self.is_user_type(args[0]) {
                //     // We leak the box here because we manually clean it up later
                //     Box::leak(arg0);
                // }
            }
        }
    }

    pub fn box_register(&self, register_id: RegisterId) -> Box<dyn Any> {
        if self.stack_frames[self.current_frame].register_types[register_id.0] == F64_TYPE {
            let val = self.get_reg_f64(register_id);
            Box::new(val)
        } else if self.stack_frames[self.current_frame].register_types[register_id.0] == BOOL_TYPE {
            let val = self.get_reg_bool(register_id);
            Box::new(val)
        } else if self.stack_frames[self.current_frame].register_types[register_id.0] == I64_TYPE {
            Box::new(self.get_reg_i64(register_id))
        } else if self.stack_frames[self.current_frame].register_types[register_id.0] == STRING_TYPE
        {
            self.get_reg_string(register_id)
        } else {
            self.get_user_type(register_id)
        }
    }

    pub fn unbox_to_register(&mut self, value: Box<dyn Any>, target: RegisterId) {
        println!(
            "unboxing as: {:?}",
            self.stack_frames[self.current_frame].register_types[target.0]
        );
        if self.stack_frames[self.current_frame].register_types[target.0] == F64_TYPE {
            if let Ok(value) = value.downcast::<f64>() {
                self.stack_frames[self.current_frame].register_values[target.0].f64 = *value;
            } else {
                panic!("internal error: could not properly handle conversion of register to f64")
            }
        } else if self.stack_frames[self.current_frame].register_types[target.0] == BOOL_TYPE {
            if let Ok(value) = value.downcast::<bool>() {
                self.stack_frames[self.current_frame].register_values[target.0].bool = *value;
            } else {
                panic!("internal error: could not properly handle conversion of register to bool")
            }
        } else if self.stack_frames[self.current_frame].register_types[target.0] == UNIT_TYPE {
            // Ignore this case, as void creates no changes
        } else if self.stack_frames[self.current_frame].register_types[target.0] == I64_TYPE {
            if let Ok(value) = value.downcast::<i64>() {
                self.stack_frames[self.current_frame].register_values[target.0].i64 = *value;
            } else {
                panic!("internal error: could not properly handle conversion of register to i64")
            }
        } else {
            self.maybe_free_register(target);
            self.stack_frames[self.current_frame].register_values[target.0].i64 =
                unsafe { std::mem::transmute::<Box<Box<dyn Any>>, i64>(Box::new(value)) };
            println!(
                "setting register #{} to be {}",
                target.0,
                self.get_reg_i64(target)
            );
        }
    }

    fn maybe_free_register(&mut self, target: RegisterId) {
        if self.is_string_type(target) {
            let _ = self.get_reg_string(target);
        } else if self.is_user_type(target)
            && unsafe { self.stack_frames[self.current_frame].register_values[target.0].i64 != 0 }
            && unsafe {
                self.stack_frames[self.current_frame].register_values[target.0].i64
                    != self.stack_frames[self.current_frame].register_values[0].i64
            }
        {
            let _ = self.get_user_type(target);
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
                    unsafe { value.f64 },
                    typechecker
                        .stringify_type(self.stack_frames[self.current_frame].register_types[idx])
                );
            } else {
                println!(
                    "    {}: {} ({})",
                    idx,
                    unsafe { value.i64 },
                    typechecker
                        .stringify_type(self.stack_frames[self.current_frame].register_types[idx])
                );
            }
        }
    }

    pub fn is_heap_type(&self, register_id: RegisterId) -> bool {
        self.is_string_type(register_id) || self.is_user_type(register_id)
    }

    pub fn is_string_type(&self, register_id: RegisterId) -> bool {
        self.stack_frames[self.current_frame].register_types[register_id.0].0 == STRING_TYPE.0
    }

    pub fn is_user_type(&self, register_id: RegisterId) -> bool {
        self.stack_frames[self.current_frame].register_types[register_id.0].0 > STRING_TYPE.0
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
