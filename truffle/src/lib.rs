#![allow(clippy::type_complexity)]

mod codegen;
mod errors;
mod eval;
mod lexer;
mod parser;
mod typechecker;

pub use crate::codegen::Translater;

pub use crate::{
    codegen::FunctionCodegen,
    errors::{ErrorBatch, ScriptError},
    eval::{Evaluator, ReturnValue},
    lexer::Lexer,
    parser::{ParseResults, Parser},
    typechecker::{
        FnRegister, FunctionId, TypeChecker, TypeId, BOOL_TYPE, F64_TYPE, I64_TYPE, UNIT_TYPE,
    },
};

// TODO: remove this, it's just a temporary hack while massaging APIs
pub use crate::typechecker::Function;

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[doc(inline)]
pub use truffle_attributes::{export, register_fn};
