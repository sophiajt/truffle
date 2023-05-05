#[cfg(feature = "cranelift")]
mod codegen_cranelift;

#[cfg(not(feature = "cranelift"))]
mod codegen;
mod delta;
mod errors;
mod lexer;
mod parser;
mod typechecker;

#[cfg(not(feature = "cranelift"))]
pub use crate::codegen::Translater;

#[cfg(feature = "cranelift")]
pub use crate::codegen_cranelift::Translater;

pub use crate::{
    parser::Parser,
    typechecker::{FnRegister, TypeChecker, TypeId, BOOL_TYPE, F64_TYPE, I64_TYPE, VOID_TYPE},
};
