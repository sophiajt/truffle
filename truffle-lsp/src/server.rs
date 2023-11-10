use std::{
    collections::HashMap,
    path::PathBuf,
    sync::{Arc, Mutex},
};

use color_eyre::eyre;
use lsp_server::{Connection, ExtractError, IoThreads, Message, Request, RequestId, Response};
use lsp_types::{
    request::{DocumentDiagnosticRequest, GotoDefinition, HoverRequest, References},
    DiagnosticOptions, GotoDefinitionParams, GotoDefinitionResponse, InitializeParams, OneOf,
    ServerCapabilities, WorkDoneProgressOptions,
};
use lsp_types::{
    DocumentDiagnosticParams, DocumentDiagnosticReport, FullDocumentDiagnosticReport, Hover,
    HoverParams, Location, ReferenceParams, RelatedFullDocumentDiagnosticReport,
};
use notify::{
    event::{AccessKind, AccessMode},
    Event, EventKind, INotifyWatcher, Watcher,
};
use tracing::info;
use truffle::{Engine, LineLookupTable};

pub struct Server {
    watcher: INotifyWatcher,
    events: crossbeam_channel::Receiver<notify::Result<Event>>,
    connection: Connection,
    io_threads: IoThreads,
    engines: Arc<Mutex<HashMap<PathBuf, Engine>>>,
    default_engine: Engine,
}

impl Server {
    pub fn new() -> eyre::Result<Self> {
        let (connection, io_threads) = Connection::stdio();
        let (sender, receiver) = crossbeam_channel::unbounded();
        let watcher = notify::recommended_watcher(sender)?;

        Ok(Self {
            watcher,
            events: receiver,
            connection,
            io_threads,
            engines: Default::default(),
            default_engine: Engine::new("default_engine"),
        })
    }

    pub fn run(mut self) -> eyre::Result<()> {
        let dirs = directories::ProjectDirs::from("", "truffle", "truffle-lsp").unwrap();
        let path = dirs.cache_dir();
        std::fs::create_dir_all(path)?;
        let recursive_mode = notify::RecursiveMode::NonRecursive;
        self.watcher.watch(path, recursive_mode)?;

        let capabilities = Self::capabilities();
        let capabilities = serde_json::to_value(capabilities).unwrap();
        let params = self.connection.initialize(capabilities)?;
        let params = serde_json::from_value(params).unwrap();

        self.main_loop(params)?;
        self.io_threads.join()?;

        Ok(())
    }

    fn handle_event(&self, event: Event) {
        dbg!(&event);
        for path in event.paths {
            match event.kind {
                EventKind::Access(AccessKind::Close(AccessMode::Write)) => {
                    let bytes = std::fs::read(&path)
                        .expect("only interpretters should write files to cache dir and all their files should be valid");
                    let engine = postcard::from_bytes(&bytes)
                        .expect("cache dir should only contain valid serialized engines");
                    self.engines.lock().unwrap().insert(path, engine);
                    dbg!(());
                }
                _ => {}
            }
        }
    }

    fn main_loop(&self, _params: InitializeParams) -> eyre::Result<()> {
        info!("starting example main loop");

        if !self.events.is_empty() {
            for res in &self.events {
                let event = res?;
                self.handle_event(event);
            }
        }

        for msg in &self.connection.receiver {
            info!("got msg: {msg:?}");

            match msg {
                Message::Request(req) => {
                    if self.connection.handle_shutdown(&req)? {
                        return Ok(());
                    }
                    info!("got request: {req:?}");
                    match req.method.as_str() {
                        "textDocument/definition" => {
                            match cast::<GotoDefinition>(req) {
                                Ok((id, params)) => {
                                    info!("got gotoDefinition request #{id}: {params:?}");
                                    let definition = self.lsp_goto_definition(params).unwrap();
                                    let result = serde_json::to_value(&definition).unwrap();
                                    let resp = Response {
                                        id,
                                        result: Some(result),
                                        error: None,
                                    };
                                    self.connection.sender.send(Message::Response(resp))?;
                                    continue;
                                }
                                Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                                Err(ExtractError::MethodMismatch(req)) => req,
                            };
                        }
                        "textDocument/hover" => {
                            match cast::<HoverRequest>(req) {
                                Ok((id, params)) => {
                                    let hover = self.lsp_hover(params);
                                    let result = serde_json::to_value(&hover).unwrap();
                                    let resp = Response {
                                        id,
                                        result: Some(result),
                                        error: None,
                                    };
                                    self.connection.sender.send(Message::Response(resp))?;
                                    continue;
                                }
                                Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                                Err(ExtractError::MethodMismatch(req)) => req,
                            };
                        }
                        "textDocument/references" => {
                            match cast::<References>(req) {
                                Ok((id, params)) => {
                                    let result = self.lsp_find_all_references(params);
                                    let result = serde_json::to_value(&result).unwrap();
                                    let resp = Response {
                                        id,
                                        result: Some(result),
                                        error: None,
                                    };
                                    self.connection.sender.send(Message::Response(resp))?;
                                    continue;
                                }
                                Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                                Err(ExtractError::MethodMismatch(req)) => req,
                            };
                        }
                        "textDocument/diagnostic" => {
                            match cast::<DocumentDiagnosticRequest>(req) {
                                Ok((id, params)) => {
                                    let report = self.lsp_check_script(params);
                                    let result = serde_json::to_value(&report).unwrap();
                                    let resp = Response {
                                        id,
                                        result: Some(result),
                                        error: None,
                                    };
                                    self.connection.sender.send(Message::Response(resp))?;
                                    continue;
                                }
                                Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                                Err(ExtractError::MethodMismatch(req)) => req,
                            };
                        }
                        method => panic!("unsupported request method: {method}"),
                    }
                }
                Message::Response(resp) => {
                    info!("got response: {resp:?}");
                }
                Message::Notification(not) => {
                    info!("got notification: {not:?}");
                }
            }
        }

        Ok(())
    }

    fn capabilities() -> ServerCapabilities {
        let diagnostic_options = DiagnosticOptions {
            identifier: Some("truffle-lsp".to_string()),
            inter_file_dependencies: true,
            workspace_diagnostics: false,
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: None,
            },
        };

        ServerCapabilities {
            definition_provider: Some(OneOf::Left(true)),
            hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)),
            references_provider: Some(OneOf::Left(true)),
            diagnostic_provider: Some(lsp_types::DiagnosticServerCapabilities::Options(
                diagnostic_options,
            )),
            ..Default::default()
        }
    }

    fn lsp_goto_definition(&self, params: GotoDefinitionParams) -> Option<GotoDefinitionResponse> {
        let path = params
            .text_document_position_params
            .text_document
            .uri
            .to_file_path()
            .unwrap();
        let lock = self.engines.lock().unwrap();
        let engine = path
            .ancestors()
            .find_map(|path| lock.get(path))
            .unwrap_or(&self.default_engine);
        let contents = std::fs::read_to_string(path).unwrap();
        let lookup = LineLookupTable::new(&contents);
        let location = lookup.from_position(params.text_document_position_params.position);
        let location = engine
            .goto_definition(location, contents.as_bytes())
            .unwrap();
        let uri = params.text_document_position_params.text_document.uri;
        Some(GotoDefinitionResponse::Scalar(
            lookup.to_location(uri, location),
        ))
    }

    pub fn lsp_check_script(
        &self,
        params: DocumentDiagnosticParams,
    ) -> Option<DocumentDiagnosticReport> {
        let path = params.text_document.uri.to_file_path().unwrap();
        let lock = self.engines.lock().unwrap();
        let engine = path
            .ancestors()
            .find_map(|path| lock.get(path))
            .unwrap_or(&self.default_engine);
        let contents = std::fs::read_to_string(&path).unwrap();
        let items = match engine.check_script(contents.as_bytes()) {
            Some(items) => items.as_diagnostics_with(contents.as_bytes()),
            None => vec![],
        };
        let result_id = None;
        let full_document_diagnostic_report = FullDocumentDiagnosticReport { result_id, items };
        let result = RelatedFullDocumentDiagnosticReport {
            related_documents: None,
            full_document_diagnostic_report,
        };
        Some(DocumentDiagnosticReport::Full(result))
    }

    pub fn lsp_hover(&self, params: HoverParams) -> Option<Hover> {
        let uri = params.text_document_position_params.text_document.uri;
        let path = uri.to_file_path().unwrap();
        let lock = self.engines.lock().unwrap();
        let engine = path
            .ancestors()
            .find_map(|path| lock.get(path))
            .unwrap_or(&self.default_engine);
        let contents = std::fs::read_to_string(path).unwrap();
        let lookup = LineLookupTable::new(&contents);
        let contents = contents.as_bytes();
        let location = lookup.from_position(params.text_document_position_params.position);
        let markdown = engine.hover(location, contents);
        let contents = lsp_types::MarkedString::from_markdown(markdown);
        let contents = lsp_types::HoverContents::Scalar(contents);
        let range = None;
        Some(Hover { contents, range })
    }

    pub fn lsp_find_all_references(&self, params: ReferenceParams) -> Option<Vec<Location>> {
        let path = params
            .text_document_position
            .text_document
            .uri
            .to_file_path()
            .unwrap();
        let lock = self.engines.lock().unwrap();
        let engine = path
            .ancestors()
            .find_map(|path| lock.get(path))
            .unwrap_or(&self.default_engine);
        let contents = std::fs::read_to_string(path).unwrap();
        let lookup = LineLookupTable::new(&contents);
        let location = lookup.from_position(params.text_document_position.position);
        let references = engine.find_all_references(location, contents.as_bytes());
        references.map(|references| {
            references
                .into_iter()
                .map(|location| {
                    lookup.to_location(
                        params.text_document_position.text_document.uri.clone(),
                        location,
                    )
                })
                .collect::<Vec<_>>()
        })
    }
}

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}
