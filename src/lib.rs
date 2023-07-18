#![allow(clippy::type_complexity)]

#[cfg(feature = "cranelift")]
mod codegen_cranelift;

#[cfg(not(feature = "cranelift"))]
mod codegen;
mod delta;
mod errors;
mod eval;
mod lexer;
mod parser;
mod typechecker;

#[cfg(not(feature = "cranelift"))]
pub use crate::codegen::Translater;

#[cfg(feature = "cranelift")]
pub use crate::codegen_cranelift::Translater;

pub use crate::{
    codegen::FunctionCodegen,
    eval::Evaluator,
    parser::Parser,
    typechecker::{
        FnRegister, FunctionId, TypeChecker, TypeId, BOOL_TYPE, F64_TYPE, I64_TYPE, VOID_TYPE,
    },
};
