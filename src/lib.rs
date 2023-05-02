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
    typechecker::{FnRegister, TypeChecker},
};

// FIXME: move these later when we build up cranelift registration
pub fn print_int(value: i64) {
    println!("value: {value}")
}

pub fn add_int(lhs: i64, rhs: i64) -> i64 {
    lhs + rhs
}
