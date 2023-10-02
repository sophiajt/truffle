use std::{any::Any, collections::HashMap, path::PathBuf};

use crate::{
    typechecker::ExternalFunctionId, ErrorBatch, Evaluator, Function, FunctionId, Lexer, Parser,
    ReturnValue, Translater, TypeChecker, TypeId,
};

#[cfg_attr(feature = "lsp", derive(serde::Serialize, serde::Deserialize))]
pub struct Engine {
    permanent_definitions: PermanentDefinitions,
}

#[cfg_attr(feature = "lsp", derive(serde::Serialize, serde::Deserialize))]
pub struct PermanentDefinitions {
    // Used by TypeId
    #[cfg_attr(feature = "lsp", serde(skip))]
    pub types: Vec<std::any::TypeId>,

    // Names of each types
    pub typenames: Vec<String>,

    // Type relationships via references
    pub reference_of_map: HashMap<TypeId, TypeId>,

    // Map between future and its output
    pub future_of_map: HashMap<TypeId, TypeId>,

    // List of all registered functions
    pub functions: Vec<ExternalFnRecord>,

    // Externally-registered functions
    pub external_functions: HashMap<Vec<u8>, Vec<ExternalFunctionId>>,
}

impl PermanentDefinitions {
    pub fn get_type<T>(&self) -> Option<TypeId>
    where
        T: Any,
    {
        for (idx, tid) in self.types.iter().enumerate() {
            if tid == &std::any::TypeId::of::<T>() {
                return Some(TypeId(idx));
            }
        }

        None
    }
}

impl Engine {
    pub fn new() -> Self {
        let permanent_definitions = PermanentDefinitions {
            types: vec![
                std::any::TypeId::of::<()>(),
                std::any::TypeId::of::<i64>(),
                std::any::TypeId::of::<f64>(),
                std::any::TypeId::of::<bool>(),
                std::any::TypeId::of::<String>(),
            ],
            typenames: vec![
                "void".into(), //std::any::type_name::<()>().to_string(),
                std::any::type_name::<i64>().to_string(),
                std::any::type_name::<f64>().to_string(),
                std::any::type_name::<bool>().to_string(),
                std::any::type_name::<String>().to_string(),
            ],
            reference_of_map: HashMap::new(),
            future_of_map: HashMap::new(),
            external_functions: HashMap::new(),
            functions: vec![],
        };

        Self {
            permanent_definitions,
        }
    }

    pub fn eval_source(
        &self,
        _fname: impl Into<PathBuf>,
        contents: &[u8],
        debug_output: bool,
    ) -> Result<ReturnValue, ErrorBatch> {
        let mut lexer = Lexer::new(contents.to_vec(), 0);

        let tokens = match lexer.lex() {
            Ok(tokens) => tokens,
            Err(errors) => {
                return Err(errors);
            }
        };

        let mut parser = Parser::new(tokens, contents.to_vec(), 0);

        match parser.parse() {
            Ok(()) => {}
            Err(errors) => {
                return Err(errors);
            }
        }
        if debug_output {
            parser.results.print();
        }

        let mut typechecker = TypeChecker::new(parser.results, &self.permanent_definitions);

        match typechecker.typecheck() {
            Ok(_) => {}
            Err(errors) => {
                return Err(errors);
            }
        }

        let mut translater = Translater::new(typechecker);

        #[allow(unused_mut)]
        let mut output = translater.translate();

        // output.debug_output();

        let mut evaluator = Evaluator::default();
        evaluator.add_function(output);

        evaluator
            .eval(FunctionId(0), &self.permanent_definitions.functions)
            .map_err(|err| ErrorBatch::one(err))
    }

    #[cfg(feature = "async")]
    pub async fn eval_source_async(
        &self,
        _fname: impl Into<PathBuf>,
        contents: &[u8],
        debug_output: bool,
    ) -> Result<ReturnValue, ErrorBatch> {
        // let fname = fname.as_ref();

        let mut lexer = Lexer::new(contents.to_vec(), 0);
        let tokens = match lexer.lex() {
            Ok(tokens) => tokens,
            Err(errors) => return Err(errors),
        };

        let mut parser = Parser::new(tokens, contents.to_vec(), 0);
        match parser.parse() {
            Ok(()) => {}
            Err(errors) => return Err(errors),
        }
        if debug_output {
            parser.results.print();
        }

        let mut typechecker = TypeChecker::new(parser.results, &self.permanent_definitions);

        match typechecker.typecheck() {
            Ok(_) => {}
            Err(errors) => return Err(errors),
        }
        if debug_output {
            typechecker.print_node_types();
        }

        let mut translater = Translater::new(typechecker);

        #[allow(unused_mut)]
        let mut output = translater.translate();

        let mut evaluator = Evaluator::default();
        evaluator.add_function(output);

        evaluator
            .eval_async(FunctionId(0), &self.permanent_definitions.functions)
            .await
            .map_err(|e| ErrorBatch::one(e))
    }

    pub fn register_type<T>(&mut self) -> TypeId
    where
        T: Any,
    {
        self.permanent_definitions
            .types
            .push(std::any::TypeId::of::<T>());
        self.permanent_definitions
            .typenames
            .push(std::any::type_name::<T>().to_string());

        let type_id = TypeId(self.permanent_definitions.types.len() - 1);

        // also register the reference
        let ref_type = self.permanent_definitions.get_type::<&T>();
        if let Some(ref_type_id) = ref_type {
            self.permanent_definitions
                .reference_of_map
                .insert(ref_type_id, type_id);
        } else {
            self.permanent_definitions
                .types
                .push(std::any::TypeId::of::<&T>());
            self.permanent_definitions
                .typenames
                .push(std::any::type_name::<&T>().to_string());
            let ref_type_id = TypeId(self.permanent_definitions.types.len() - 1);
            self.permanent_definitions
                .reference_of_map
                .insert(ref_type_id, type_id);
        }

        type_id
    }

    pub fn get_type<T>(&self) -> Option<TypeId>
    where
        T: Any,
    {
        self.permanent_definitions.get_type::<T>()
    }

    #[cfg(feature = "async")]
    pub fn add_async_call(&mut self, params: Vec<TypeId>, ret: TypeId, fun: Function, name: &str) {
        self.permanent_definitions.functions.push(ExternalFnRecord {
            params,
            ret,
            fun,
        });

        let id = self.permanent_definitions.functions.len() - 1;

        let ent = self
            .permanent_definitions
            .external_functions
            .entry(name.as_bytes().to_vec())
            .or_insert(Vec::new());
        (*ent).push(ExternalFunctionId(id));
    }

    pub fn with<F>(&mut self, f: F)
    where
        F: Fn(&mut Engine),
    {
        f(self)
    }
}

#[cfg_attr(feature = "lsp", derive(serde::Serialize, serde::Deserialize))]
pub struct ExternalFnRecord {
    pub params: Vec<TypeId>,
    pub ret: TypeId,
    #[cfg_attr(feature = "lsp", serde(skip))]
    pub fun: Function,
}

pub trait FnRegister<A, RetVal, Args> {
    fn register_fn(&mut self, name: &str, fun: A);
}

impl<A, U> FnRegister<A, U, ()> for Engine
where
    A: 'static + Fn() -> U,
    U: Any,
{
    fn register_fn(&mut self, name: &str, fun: A) {
        let wrapped: Box<dyn Fn() -> Result<Box<dyn Any>, String>> =
            Box::new(move || Ok(Box::new(fun()) as Box<dyn Any>));

        let ret = if let Some(id) = self.permanent_definitions.get_type::<U>() {
            id
        } else {
            self.register_type::<U>()
        };

        self.permanent_definitions.functions.push(ExternalFnRecord {
            params: vec![],
            ret,
            fun: Function::ExternalFn0(wrapped),
        });

        let id = self.permanent_definitions.functions.len() - 1;

        let ent = self
            .permanent_definitions
            .external_functions
            .entry(name.as_bytes().to_vec())
            .or_insert(Vec::new());
        (*ent).push(ExternalFunctionId(id));
    }
}

impl<A, T, U> FnRegister<A, U, (T,)> for Engine
where
    A: 'static + Fn(T) -> U,
    T: Clone + Any,
    U: Any,
{
    fn register_fn(&mut self, name: &str, fun: A) {
        let wrapped: Box<dyn Fn(&mut Box<dyn Any>) -> Result<Box<dyn Any>, String>> =
            Box::new(move |arg: &mut Box<dyn Any>| {
                let inside = (*arg).downcast_mut() as Option<&mut T>;
                match inside {
                    Some(b) => Ok(Box::new(fun(b.clone())) as Box<dyn Any>),
                    None => Err("ErrorFunctionArgMismatch1".into()),
                }
            });

        let param1 = if let Some(id) = self.permanent_definitions.get_type::<T>() {
            id
        } else {
            self.register_type::<T>()
        };

        let ret = if let Some(id) = self.permanent_definitions.get_type::<U>() {
            id
        } else {
            self.register_type::<U>()
        };

        self.permanent_definitions.functions.push(ExternalFnRecord {
            params: vec![param1],
            ret,
            fun: Function::ExternalFn1(wrapped),
        });

        let id = self.permanent_definitions.functions.len() - 1;

        let ent = self
            .permanent_definitions
            .external_functions
            .entry(name.as_bytes().to_vec())
            .or_insert(Vec::new());
        (*ent).push(ExternalFunctionId(id));
    }
}

impl<A, T, U, V> FnRegister<A, V, (T, U)> for Engine
where
    A: 'static + Fn(T, U) -> V,
    T: Clone + Any,
    U: Clone + Any,
    V: Any,
{
    fn register_fn(&mut self, name: &str, fun: A) {
        let wrapped: Box<
            dyn Fn(&mut Box<dyn Any>, &mut Box<dyn Any>) -> Result<Box<dyn Any>, String>,
        > = Box::new(move |arg1: &mut Box<dyn Any>, arg2: &mut Box<dyn Any>| {
            let inside1 = (*arg1).downcast_mut() as Option<&mut T>;
            let inside2 = (*arg2).downcast_mut() as Option<&mut U>;

            match (inside1, inside2) {
                (Some(b), Some(c)) => Ok(Box::new(fun(b.clone(), c.clone())) as Box<dyn Any>),
                _ => Err("ErrorFunctionArgMismatch2".into()),
            }
        });

        let param1 = if let Some(id) = self.permanent_definitions.get_type::<T>() {
            id
        } else {
            self.register_type::<T>()
        };

        let param2 = if let Some(id) = self.permanent_definitions.get_type::<U>() {
            id
        } else {
            self.register_type::<U>()
        };

        let ret = if let Some(id) = self.permanent_definitions.get_type::<V>() {
            id
        } else {
            self.register_type::<V>()
        };

        let fn_record = ExternalFnRecord {
            params: vec![param1, param2],
            ret,
            fun: Function::ExternalFn2(wrapped),
        };
        self.permanent_definitions.functions.push(fn_record);

        let id = self.permanent_definitions.functions.len() - 1;

        let ent = self
            .permanent_definitions
            .external_functions
            .entry(name.as_bytes().to_vec())
            .or_insert(Vec::new());
        (*ent).push(ExternalFunctionId(id));
    }
}

impl<'a, A, T, U, V, W> FnRegister<A, V, (&'a T, U, W)> for Engine
where
    A: 'static + Fn(T, U, W) -> V,
    T: Clone + Any,
    U: Clone + Any,
    W: Clone + Any,
    V: Any,
{
    fn register_fn(&mut self, name: &str, fun: A) {
        let wrapped: Box<
            dyn Fn(
                &mut Box<dyn Any>,
                &mut Box<dyn Any>,
                &mut Box<dyn Any>,
            ) -> Result<Box<dyn Any>, String>,
        > = Box::new(
            move |arg1: &mut Box<dyn Any>, arg2: &mut Box<dyn Any>, arg3: &mut Box<dyn Any>| {
                let inside1 = (*arg1).downcast_mut() as Option<&mut T>;
                let inside2 = (*arg2).downcast_mut() as Option<&mut U>;
                let inside3 = (*arg3).downcast_mut() as Option<&mut W>;

                match (inside1, inside2, inside3) {
                    (Some(b), Some(c), Some(d)) => {
                        Ok(Box::new(fun(b.clone(), c.clone(), d.clone())) as Box<dyn Any>)
                    }
                    _ => Err("ErrorFunctionArgMismatch3".into()),
                }
            },
        );

        let param1 = if let Some(id) = self.permanent_definitions.get_type::<T>() {
            id
        } else {
            self.register_type::<T>()
        };

        let param2 = if let Some(id) = self.permanent_definitions.get_type::<U>() {
            id
        } else {
            self.register_type::<U>()
        };

        let param3 = if let Some(id) = self.permanent_definitions.get_type::<W>() {
            id
        } else {
            self.register_type::<W>()
        };

        let ret = if let Some(id) = self.permanent_definitions.get_type::<V>() {
            id
        } else {
            self.register_type::<V>()
        };

        let fn_record = ExternalFnRecord {
            params: vec![param1, param2, param3],
            ret,
            fun: Function::ExternalFn3(wrapped),
        };
        self.permanent_definitions.functions.push(fn_record);

        let id = self.permanent_definitions.functions.len() - 1;

        let ent = self
            .permanent_definitions
            .external_functions
            .entry(name.as_bytes().to_vec())
            .or_insert(Vec::new());
        (*ent).push(ExternalFunctionId(id));
    }
}

impl<A, T, U, V, W> FnRegister<A, V, (&mut T, U, W)> for Engine
where
    A: 'static + Fn(&mut T, U, W) -> V,
    T: Any,
    U: Clone + Any,
    W: Clone + Any,
    V: Any,
{
    fn register_fn(&mut self, name: &str, fun: A) {
        let wrapped: Box<
            dyn Fn(
                &mut Box<dyn Any>,
                &mut Box<dyn Any>,
                &mut Box<dyn Any>,
            ) -> Result<Box<dyn Any>, String>,
        > = Box::new(
            move |arg1: &mut Box<dyn Any>, arg2: &mut Box<dyn Any>, arg3: &mut Box<dyn Any>| {
                let inside1 = (*arg1).downcast_mut() as Option<&mut T>;
                let inside2 = (*arg2).downcast_mut() as Option<&mut U>;
                let inside3 = (*arg3).downcast_mut() as Option<&mut W>;

                match (inside1, inside2, inside3) {
                    (Some(b), Some(c), Some(d)) => {
                        Ok(Box::new(fun(b, c.clone(), d.clone())) as Box<dyn Any>)
                    }
                    _ => Err("ErrorFunctionArgMismatch3Mut".into()),
                }
            },
        );

        let param1 = if let Some(id) = self.permanent_definitions.get_type::<T>() {
            id
        } else {
            self.register_type::<T>()
        };

        let param2 = if let Some(id) = self.permanent_definitions.get_type::<U>() {
            id
        } else {
            self.register_type::<U>()
        };

        let param3 = if let Some(id) = self.permanent_definitions.get_type::<W>() {
            id
        } else {
            self.register_type::<W>()
        };

        let ret = if let Some(id) = self.permanent_definitions.get_type::<V>() {
            id
        } else {
            self.register_type::<V>()
        };

        let fn_record = ExternalFnRecord {
            params: vec![param1, param2, param3],
            ret,
            fun: Function::ExternalFn3(wrapped),
        };
        self.permanent_definitions.functions.push(fn_record);

        let id = self.permanent_definitions.functions.len() - 1;

        let ent = self
            .permanent_definitions
            .external_functions
            .entry(name.as_bytes().to_vec())
            .or_insert(Vec::new());
        (*ent).push(ExternalFunctionId(id));
    }
}

#[cfg(not(feature = "async"))]
#[macro_export]
macro_rules! register_fn {
    ( $typechecker:expr, $name: expr, $fun:expr ) => {{
        $typechecker.register_fn($name, $fun)
    }};
}
