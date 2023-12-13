#![allow(clippy::type_complexity)]

mod codegen;
mod engine;
mod errors;
mod eval;
mod lexer;
mod parser;
mod typechecker;

pub use crate::codegen::Translater;

pub use crate::{
    codegen::FunctionCodegen,
    engine::{Engine, FnRegister, SpanOrLocation},
    errors::{ErrorBatch, ScriptError},
    eval::{Evaluator, ReturnValue},
    lexer::Lexer,
    parser::{ParseResults, Parser, Span},
    typechecker::{FunctionId, TypeChecker, TypeId, BOOL_TYPE, F64_TYPE, I64_TYPE, UNIT_TYPE},
};

#[cfg(feature = "lsp")]
pub use errors::LineLookupTable;

// TODO: remove this, it's just a temporary hack while massaging APIs
pub use crate::typechecker::Function;

#[cfg(any(feature = "async", feature = "lsp"))]
#[cfg_attr(docsrs, doc(cfg(any(feature = "async", feature = "lsp"))))]
#[doc(inline)]
pub use truffle_attributes::{export, register_fn};

#[cfg(feature = "lsp")]
pub use postcard;

pub type Value = Box<dyn std::any::Any + Send + Sync>;

// Used for trait bounds, do not use directly in `Value` or it will break `downcast` calls.
pub trait Type: std::any::Any + Send + Sync {}
impl<T> Type for T where T: std::any::Any + Send + Sync {}
