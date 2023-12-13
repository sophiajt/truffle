use std::{collections::HashMap, path::PathBuf};

#[cfg(feature = "lsp")]
use lsp_types::Url;

use crate::parser::Span;
use crate::Type;

use crate::{
    parser::NodeId, typechecker::ExternalFunctionId, ErrorBatch, Evaluator, Function, FunctionId,
    Lexer, ParseResults, Parser, ReturnValue, Translater, TypeChecker, TypeId, Value,
};

#[cfg_attr(feature = "lsp", derive(serde::Serialize, serde::Deserialize))]
pub struct Engine {
    permanent_definitions: PermanentDefinitions,
    app_name: Option<String>,
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

    // Info about all registered functions
    #[cfg(feature = "lsp")]
    pub function_infos: HashMap<Vec<u8>, ExternalFunctionLocation>,

    // Externally-registered functions
    pub external_functions: HashMap<Vec<u8>, Vec<ExternalFunctionId>>,
}

#[derive(Debug, PartialEq)]
#[non_exhaustive]
pub enum SpanOrLocation {
    Span(Span),
    #[cfg(feature = "lsp")]
    ExternalLocation(Url, u32), // filename and line number
}

impl PermanentDefinitions {
    pub fn get_type<T>(&self) -> Option<TypeId>
    where
        T: Type,
    {
        for (idx, tid) in self.types.iter().enumerate() {
            if tid == &std::any::TypeId::of::<T>() {
                return Some(TypeId(idx));
            }
        }

        None
    }
}

impl Default for Engine {
    fn default() -> Self {
        Self::new()
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
            #[cfg(feature = "lsp")]
            function_infos: HashMap::new(),
        };

        Self {
            permanent_definitions,
            app_name: None,
        }
    }

    pub fn app_name(&self) -> Option<&str> {
        self.app_name.as_deref()
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

        if debug_output {
            output.debug_output();
        }

        let mut evaluator = Evaluator::default();
        evaluator.add_function(output);

        evaluator
            .eval(FunctionId(0), &self.permanent_definitions.functions)
            .map_err(ErrorBatch::one)
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
            .map_err(ErrorBatch::one)
    }

    pub fn register_type<T>(&mut self) -> TypeId
    where
        T: Type,
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
        T: Type,
    {
        self.permanent_definitions.get_type::<T>()
    }

    #[cfg(feature = "async")]
    pub fn add_async_call(
        &mut self,
        params: Vec<TypeId>,
        ret: TypeId,
        fun: Function,
        name: &str,
        #[cfg_attr(not(feature = "lsp"), allow(unused_variables))]
        location: &'static std::panic::Location<'static>,
    ) {
        self.permanent_definitions
            .functions
            .push(ExternalFnRecord { params, ret, fun });

        let id = self.permanent_definitions.functions.len() - 1;

        #[cfg(feature = "lsp")]
        self.permanent_definitions.function_infos.insert(
            name.as_bytes().to_vec(),
            ExternalFunctionLocation {
                path: location.file().into(),
                line: location.line(),
                column: location.column(),
            },
        );

        let ent = self
            .permanent_definitions
            .external_functions
            .entry(name.as_bytes().to_vec())
            .or_default();
        (*ent).push(ExternalFunctionId(id));
    }

    pub fn with<F>(&mut self, f: F)
    where
        F: Fn(&mut Engine),
    {
        f(self)
    }

    // TODO: replace location here with span
    pub fn get_node_id_at_location(
        &self,
        location: usize,
        parse_results: &ParseResults,
    ) -> Option<NodeId> {
        for node_id in 0..parse_results.spans.len() {
            let span = parse_results.spans[node_id];
            if location >= span.start && location < span.end {
                return Some(NodeId(node_id));
            }
        }

        None
    }

    #[cfg(feature = "lsp")]
    pub fn hover(&self, location: usize, contents: &[u8]) -> String {
        use crate::parser::AstNode;

        let mut lexer = Lexer::new(contents.to_vec(), 0);
        let tokens = match lexer.lex() {
            Ok(tokens) => tokens,
            Err(_) => return String::new(),
        };
        let mut parser = Parser::new(tokens, contents.to_vec(), 0);
        let _ = parser.parse();

        let mut typechecker = TypeChecker::new(parser.results, &self.permanent_definitions);
        let _ = typechecker.typecheck();

        let node_id = self.get_node_id_at_location(location, &typechecker.parse_results);

        if let Some(node_id) = node_id {
            match typechecker.parse_results.ast_nodes[node_id.0] {
                AstNode::Name => {
                    let external_function_id = typechecker.call_resolution.get(&node_id);

                    if let Some(external_function_id) = external_function_id {
                        let name = typechecker.parse_results.contents_for_node(node_id);
                        typechecker.pretty_function_signature(name, *external_function_id)
                    } else {
                        let type_id = typechecker.node_types[node_id.0];

                        typechecker.stringify_type(type_id)
                    }
                }
                AstNode::Call { head, .. } => {
                    let external_function_id = typechecker.call_resolution.get(&node_id);

                    if let Some(external_function_id) = external_function_id {
                        let name = typechecker.parse_results.contents_for_node(head);
                        typechecker.pretty_function_signature(name, *external_function_id)
                    } else {
                        "<unknown function>".into()
                    }
                }
                _ => {
                    let type_id = typechecker.node_types[node_id.0];

                    typechecker.stringify_type(type_id)
                }
            }
        } else {
            String::new()
        }
    }

    #[cfg(feature = "lsp")]
    pub fn goto_definition(&self, location: usize, contents: &[u8]) -> Option<SpanOrLocation> {
        let mut lexer = Lexer::new(contents.to_vec(), 0);
        let tokens = lexer.lex().ok()?;
        let mut parser = Parser::new(tokens, contents.to_vec(), 0);
        let _ = parser.parse();

        let mut typechecker = TypeChecker::new(parser.results, &self.permanent_definitions);
        let _ = typechecker.typecheck();

        let node_id = self.get_node_id_at_location(location, &typechecker.parse_results)?;
        match &typechecker.parse_results.ast_nodes[node_id.0] {
            crate::parser::AstNode::Variable => Some(
                typechecker
                    .variable_def_site
                    .get(&node_id)
                    .map(|x| SpanOrLocation::Span(typechecker.parse_results.spans[x.0])),
            )?,
            crate::parser::AstNode::Name => {
                let record = typechecker.parse_results.contents_for_node(node_id);
                let location = self.permanent_definitions.function_infos.get(record)?;
                let line = location.line;
                let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                    .parent()?
                    .join(&location.path);
                let uri = Url::from_file_path(path).ok()?;
                Some(SpanOrLocation::ExternalLocation(uri, line))
            }
            _ => None,
        }
    }

    #[cfg(feature = "lsp")]
    pub fn check_script(&self, contents: &[u8]) -> Option<ErrorBatch> {
        let mut lexer = Lexer::new(contents.to_vec(), 0);

        let tokens = match lexer.lex() {
            Ok(tokens) => tokens,
            Err(errors) => {
                return Some(errors);
            }
        };

        let mut parser = Parser::new(tokens, contents.to_vec(), 0);

        match parser.parse() {
            Ok(()) => {}
            Err(errors) => {
                return Some(errors);
            }
        }

        let mut typechecker = TypeChecker::new(parser.results, &self.permanent_definitions);

        match typechecker.typecheck() {
            Ok(_) => None,
            Err(errors) => Some(errors),
        }
    }

    #[cfg(feature = "lsp")]
    pub fn find_all_references(&self, location: usize, contents: &[u8]) -> Option<Vec<Span>> {
        let mut lexer = Lexer::new(contents.to_vec(), 0);
        let tokens = match lexer.lex() {
            Ok(tokens) => tokens,
            Err(_) => return None,
        };

        eprintln!("tokens: {:?}", tokens);

        let mut parser = Parser::new(tokens, contents.to_vec(), 0);
        let _ = parser.parse();

        eprintln!("parse results: {:?}", parser.results);

        let mut typechecker = TypeChecker::new(parser.results, &self.permanent_definitions);
        let _ = typechecker.typecheck();

        let node_id = self.get_node_id_at_location(location, &typechecker.parse_results);

        if let Some(node_id) = node_id {
            if let Some(def_site_node_id) = typechecker.variable_def_site.get(&node_id) {
                let mut output = vec![];

                for (key, value) in typechecker.variable_def_site.iter() {
                    if value == def_site_node_id {
                        output.push(typechecker.parse_results.spans[key.0])
                    }
                }

                output.sort();

                Some(output)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn completion(&self, location: usize, contents: &[u8]) -> Vec<String> {
        let mut output = vec![];

        let mut lexer = Lexer::new(contents.to_vec(), 0);
        let tokens = match lexer.lex() {
            Ok(tokens) => tokens,
            Err(_) => return output,
        };

        // Logic to move into the token immediately left of us if necessary:
        // If we're on a space or eof, try moving left. Otherwise stay put.
        // Only try to move left if we have space to do so, If we try to move left
        let location = if location < contents.len()
            && contents[location].is_ascii_whitespace()
            && location > 0
        {
            location - 1
        } else {
            location
        };

        let mut prefix_start = location;

        loop {
            // eprintln!("prefix build: {:?}", contents[prefix_start] as char);
            if prefix_start == 0
                || (!contents[prefix_start].is_ascii_digit()
                    && !contents[prefix_start].is_ascii_alphabetic()
                    && contents[prefix_start] != b'_')
            {
                prefix_start += 1;
                break;
            }

            prefix_start -= 1;
        }

        let prefix = &contents[prefix_start..=location];

        // eprintln!("prefix: {:?}", prefix);

        // eprintln!("tokens: {:?}", tokens);

        let mut parser = Parser::new(tokens, contents.to_vec(), 0);
        let _ = parser.parse();

        // eprintln!("parse results: {:?}", parser.results);

        let mut typechecker = TypeChecker::new(parser.results, &self.permanent_definitions);
        let _ = typechecker.typecheck();

        let node_id = self.get_node_id_at_location(location, &typechecker.parse_results);

        if let Some(node_id) = node_id {
            let node_span = typechecker.parse_results.spans[node_id.0];

            for scope in typechecker.scope.iter() {
                let scope_span = typechecker.parse_results.spans[scope.node_id.0];

                if node_span.start >= scope_span.start && node_span.end <= scope_span.end {
                    for (var_name, var_node_id) in scope.variables.iter() {
                        let var_end = typechecker.parse_results.spans[var_node_id.0].end;

                        if var_end <= location && var_name.starts_with(prefix) {
                            // Variable is in scope and defined ahead of the completion location
                            output.push(String::from_utf8_lossy(var_name).to_string());
                        }
                    }
                }
            }
        }

        output.sort();
        output
    }

    #[cfg(feature = "lsp")]
    pub fn lsp_cache_writer(&self) -> std::io::BufWriter<std::fs::File> {
        let dirs = directories::ProjectDirs::from("", "truffle", "truffle-lsp").unwrap();
        let path = dirs.cache_dir();
        std::fs::create_dir_all(path).unwrap();
        let app_name = self
            .app_name
            .as_ref()
            .expect("this function should only be called if app_name is Some");
        let path = path.join(format!("truffle.{app_name}.lspdata"));
        let file = std::fs::File::create(path).unwrap();
        std::io::BufWriter::new(file)
    }

    pub fn set_app_name<'a, T>(&mut self, app_name: T)
    where
        T: Into<Option<&'a str>>,
    {
        self.app_name = app_name.into().map(String::from);
    }
}

#[cfg_attr(feature = "lsp", derive(serde::Serialize, serde::Deserialize))]
pub struct ExternalFnRecord {
    pub params: Vec<TypeId>,
    pub ret: TypeId,
    #[cfg_attr(feature = "lsp", serde(skip))]
    pub fun: Function,
}

#[cfg(feature = "lsp")]
#[derive(serde::Serialize, serde::Deserialize, Debug)]
pub struct ExternalFunctionLocation {
    path: PathBuf,
    line: u32,
    column: u32,
}

pub trait FnRegister<A, RetVal, Args> {
    fn register_fn(
        &mut self,
        name: &str,
        fun: A,
        #[cfg_attr(not(feature = "lsp"), allow(unused_variables))] location: Option<
            &'static std::panic::Location<'static>,
        >,
    );
}

impl<A, U> FnRegister<A, U, ()> for Engine
where
    A: 'static + Fn() -> U,
    U: Type,
{
    fn register_fn(
        &mut self,
        name: &str,
        fun: A,
        #[cfg_attr(not(feature = "lsp"), allow(unused_variables))] location: Option<
            &'static std::panic::Location<'static>,
        >,
    ) {
        let wrapped: Box<dyn Fn() -> Result<Value, String>> =
            Box::new(move || Ok(Box::new(fun()) as Value));

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

        #[cfg(feature = "lsp")]
        if let Some(location) = location {
            self.permanent_definitions.function_infos.insert(
                name.as_bytes().to_vec(),
                ExternalFunctionLocation {
                    path: location.file().into(),
                    line: location.line(),
                    column: location.column(),
                },
            );
        }

        let id = self.permanent_definitions.functions.len() - 1;

        let ent = self
            .permanent_definitions
            .external_functions
            .entry(name.as_bytes().to_vec())
            .or_default();
        (*ent).push(ExternalFunctionId(id));
    }
}

impl<'a, A, T, U> FnRegister<A, U, (&'a T,)> for Engine
where
    A: 'static + Fn(T) -> U,
    T: Clone + Type,
    U: Type,
{
    fn register_fn(
        &mut self,
        name: &str,
        fun: A,
        #[cfg_attr(not(feature = "lsp"), allow(unused_variables))] location: Option<
            &'static std::panic::Location<'static>,
        >,
    ) {
        let wrapped: Box<dyn Fn(&mut Value) -> Result<Value, String>> =
            Box::new(move |arg: &mut Value| {
                let inside = (*arg).downcast_mut() as Option<&mut T>;
                match inside {
                    Some(b) => Ok(Box::new(fun(b.clone())) as Value),
                    None => Err(format!(
                        "can't convert first argument to {}",
                        std::any::type_name::<T>()
                    )),
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

        #[cfg(feature = "lsp")]
        if let Some(location) = location {
            self.permanent_definitions.function_infos.insert(
                name.as_bytes().to_vec(),
                ExternalFunctionLocation {
                    path: location.file().into(),
                    line: location.line(),
                    column: location.column(),
                },
            );
        }

        let id = self.permanent_definitions.functions.len() - 1;

        let ent = self
            .permanent_definitions
            .external_functions
            .entry(name.as_bytes().to_vec())
            .or_default();
        (*ent).push(ExternalFunctionId(id));
    }
}

impl<A, T, U> FnRegister<A, U, (&mut T,)> for Engine
where
    A: 'static + Fn(&mut T) -> U,
    T: Type,
    U: Type,
{
    fn register_fn(
        &mut self,
        name: &str,
        fun: A,
        #[cfg_attr(not(feature = "lsp"), allow(unused_variables))] location: Option<
            &'static std::panic::Location<'static>,
        >,
    ) {
        let wrapped: Box<dyn Fn(&mut Value) -> Result<Value, String>> =
            Box::new(move |arg: &mut Value| {
                let inside = (*arg).downcast_mut() as Option<&mut T>;
                match inside {
                    Some(b) => Ok(Box::new(fun(b)) as Value),
                    None => Err(format!(
                        "can't convert first argument to {}",
                        std::any::type_name::<T>()
                    )),
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

        #[cfg(feature = "lsp")]
        if let Some(location) = location {
            self.permanent_definitions.function_infos.insert(
                name.as_bytes().to_vec(),
                ExternalFunctionLocation {
                    path: location.file().into(),
                    line: location.line(),
                    column: location.column(),
                },
            );
        }

        let id = self.permanent_definitions.functions.len() - 1;

        let ent = self
            .permanent_definitions
            .external_functions
            .entry(name.as_bytes().to_vec())
            .or_default();
        (*ent).push(ExternalFunctionId(id));
    }
}

impl<A, T, U, V> FnRegister<A, V, (&mut T, U)> for Engine
where
    A: 'static + Fn(&mut T, U) -> V,
    T: Type,
    U: Clone + Type,
    V: Type,
{
    fn register_fn(
        &mut self,
        name: &str,
        fun: A,
        #[cfg_attr(not(feature = "lsp"), allow(unused_variables))] location: Option<
            &'static std::panic::Location<'static>,
        >,
    ) {
        let wrapped: Box<dyn Fn(&mut Value, &mut Value) -> Result<Value, String>> =
            Box::new(move |arg1: &mut Value, arg2: &mut Value| {
                let inside1 = (*arg1).downcast_mut() as Option<&mut T>;
                let inside2 = (*arg2).downcast_mut() as Option<&mut U>;

                match (inside1, inside2) {
                    (Some(b), Some(c)) => Ok(Box::new(fun(b, c.clone())) as Value),
                    (Some(_), None) => Err(format!(
                        "can't convert second argument to {}",
                        std::any::type_name::<U>()
                    )),
                    (None, _) => Err(format!(
                        "can't convert first argument to {}",
                        std::any::type_name::<T>()
                    )),
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
        #[cfg(feature = "lsp")]
        if let Some(location) = location {
            self.permanent_definitions.function_infos.insert(
                name.as_bytes().to_vec(),
                ExternalFunctionLocation {
                    path: location.file().into(),
                    line: location.line(),
                    column: location.column(),
                },
            );
        }

        let id = self.permanent_definitions.functions.len() - 1;

        let ent = self
            .permanent_definitions
            .external_functions
            .entry(name.as_bytes().to_vec())
            .or_default();
        (*ent).push(ExternalFunctionId(id));
    }
}

impl<'a, A, T, U, V> FnRegister<A, V, (&'a T, U)> for Engine
where
    A: 'static + Fn(T, U) -> V,
    T: Clone + Type,
    U: Clone + Type,
    V: Type,
{
    fn register_fn(
        &mut self,
        name: &str,
        fun: A,
        #[cfg_attr(not(feature = "lsp"), allow(unused_variables))] location: Option<
            &'static std::panic::Location<'static>,
        >,
    ) {
        let wrapped: Box<dyn Fn(&mut Value, &mut Value) -> Result<Value, String>> =
            Box::new(move |arg1: &mut Value, arg2: &mut Value| {
                let inside1 = (*arg1).downcast_mut() as Option<&mut T>;
                let inside2 = (*arg2).downcast_mut() as Option<&mut U>;

                match (inside1, inside2) {
                    (Some(b), Some(c)) => Ok(Box::new(fun(b.clone(), c.clone())) as Value),
                    (Some(_), None) => Err(format!(
                        "can't convert second argument to {}",
                        std::any::type_name::<U>()
                    )),
                    (None, _) => Err(format!(
                        "can't convert first argument to {}",
                        std::any::type_name::<T>()
                    )),
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
        #[cfg(feature = "lsp")]
        if let Some(location) = location {
            self.permanent_definitions.function_infos.insert(
                name.as_bytes().to_vec(),
                ExternalFunctionLocation {
                    path: location.file().into(),
                    line: location.line(),
                    column: location.column(),
                },
            );
        }

        let id = self.permanent_definitions.functions.len() - 1;

        let ent = self
            .permanent_definitions
            .external_functions
            .entry(name.as_bytes().to_vec())
            .or_default();
        (*ent).push(ExternalFunctionId(id));
    }
}

impl<'a, A, T, U, V, W> FnRegister<A, V, (&'a T, U, W)> for Engine
where
    A: 'static + Fn(T, U, W) -> V,
    T: Clone + Type,
    U: Clone + Type,
    W: Clone + Type,
    V: Type,
{
    fn register_fn(
        &mut self,
        name: &str,
        fun: A,
        #[cfg_attr(not(feature = "lsp"), allow(unused_variables))] location: Option<
            &'static std::panic::Location<'static>,
        >,
    ) {
        let wrapped: Box<dyn Fn(&mut Value, &mut Value, &mut Value) -> Result<Value, String>> =
            Box::new(
                move |arg1: &mut Value, arg2: &mut Value, arg3: &mut Value| {
                    let inside1 = (*arg1).downcast_mut() as Option<&mut T>;
                    let inside2 = (*arg2).downcast_mut() as Option<&mut U>;
                    let inside3 = (*arg3).downcast_mut() as Option<&mut W>;

                    match (inside1, inside2, inside3) {
                        (Some(b), Some(c), Some(d)) => {
                            Ok(Box::new(fun(b.clone(), c.clone(), d.clone())) as Value)
                        }
                        (Some(_), Some(_), None) => Err(format!(
                            "can't convert third argument to {}",
                            std::any::type_name::<W>()
                        )),
                        (Some(_), None, _) => Err(format!(
                            "can't convert second argument to {}",
                            std::any::type_name::<U>()
                        )),
                        (None, _, _) => Err(format!(
                            "can't convert first argument to {}",
                            std::any::type_name::<T>()
                        )),
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
        #[cfg(feature = "lsp")]
        if let Some(location) = location {
            self.permanent_definitions.function_infos.insert(
                name.as_bytes().to_vec(),
                ExternalFunctionLocation {
                    path: location.file().into(),
                    line: location.line(),
                    column: location.column(),
                },
            );
        }

        let id = self.permanent_definitions.functions.len() - 1;

        let ent = self
            .permanent_definitions
            .external_functions
            .entry(name.as_bytes().to_vec())
            .or_default();
        (*ent).push(ExternalFunctionId(id));
    }
}

impl<A, T, U, V, W> FnRegister<A, V, (&mut T, U, W)> for Engine
where
    A: 'static + Fn(&mut T, U, W) -> V,
    T: Type,
    U: Clone + Type,
    W: Clone + Type,
    V: Type,
{
    fn register_fn(
        &mut self,
        name: &str,
        fun: A,
        #[cfg_attr(not(feature = "lsp"), allow(unused_variables))] location: Option<
            &'static std::panic::Location<'static>,
        >,
    ) {
        let wrapped: Box<dyn Fn(&mut Value, &mut Value, &mut Value) -> Result<Value, String>> =
            Box::new(
                move |arg1: &mut Value, arg2: &mut Value, arg3: &mut Value| {
                    let inside1 = (*arg1).downcast_mut() as Option<&mut T>;
                    let inside2 = (*arg2).downcast_mut() as Option<&mut U>;
                    let inside3 = (*arg3).downcast_mut() as Option<&mut W>;

                    match (inside1, inside2, inside3) {
                        (Some(b), Some(c), Some(d)) => {
                            Ok(Box::new(fun(b, c.clone(), d.clone())) as Value)
                        }
                        (Some(_), Some(_), None) => Err(format!(
                            "can't convert third argument to {}",
                            std::any::type_name::<W>()
                        )),
                        (Some(_), None, _) => Err(format!(
                            "can't convert second argument to {}",
                            std::any::type_name::<U>()
                        )),
                        (None, _, _) => Err(format!(
                            "can't convert first argument to {}",
                            std::any::type_name::<T>()
                        )),
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
        #[cfg(feature = "lsp")]
        if let Some(location) = location {
            self.permanent_definitions.function_infos.insert(
                name.as_bytes().to_vec(),
                ExternalFunctionLocation {
                    path: location.file().into(),
                    line: location.line(),
                    column: location.column(),
                },
            );
        }

        let id = self.permanent_definitions.functions.len() - 1;

        let ent = self
            .permanent_definitions
            .external_functions
            .entry(name.as_bytes().to_vec())
            .or_default();
        (*ent).push(ExternalFunctionId(id));
    }
}

#[cfg(not(any(feature = "async", feature = "lsp")))]
#[macro_export]
macro_rules! register_fn {
    ( $engine:expr, $name: expr, $fun:expr ) => {{
        $engine.register_fn($name, $fun, None);
        #[cfg(feature = "lsp")]
        if $engine.app_name().is_some() {
            let mut writer = $engine.lsp_cache_writer();
            let data = postcard::to_io(&$engine, &mut writer).unwrap();
            let _ = std::io::Write::flush(&mut writer);
        }
    }};
}
