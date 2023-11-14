use std::{
    collections::HashMap,
    path::PathBuf,
    sync::{Arc, Mutex},
};

use color_eyre::eyre;
use eyre::eyre;
use lsp_server::{Connection, IoThreads, Message};
use lsp_types::{
    request::{Completion, DocumentDiagnosticRequest, GotoDefinition, HoverRequest, References},
    CompletionItem, CompletionOptions, CompletionParams, CompletionResponse, DiagnosticOptions,
    DocumentDiagnosticReportResult, GotoDefinitionParams, GotoDefinitionResponse, InitializeParams,
    OneOf, ServerCapabilities, TextDocumentSyncKind, WorkDoneProgressOptions,
};
use lsp_types::{
    DocumentDiagnosticParams, DocumentDiagnosticReport, FullDocumentDiagnosticReport, Hover,
    HoverParams, Location, ReferenceParams, RelatedFullDocumentDiagnosticReport,
};
#[cfg(target_os = "linux")]
use notify::INotifyWatcher;
use notify::{
    event::{AccessKind, AccessMode},
    Event, EventKind, Watcher,
};

#[cfg(target_os = "macos")]
use notify::KqueueWatcher;

use tracing::info;
use truffle::{Engine, LineLookupTable};

mod dispatch;

#[cfg(target_os = "linux")]
pub struct Server {
    watcher: INotifyWatcher,
    events: crossbeam_channel::Receiver<notify::Result<Event>>,
    connection: Connection,
    io_threads: IoThreads,
    engines: Arc<Mutex<HashMap<PathBuf, Engine>>>,
    default_engine: Engine,
}

#[cfg(target_os = "macos")]
pub struct Server {
    watcher: KqueueWatcher,
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
            default_engine: Engine::new(),
        })
    }

    pub fn run(mut self) -> eyre::Result<()> {
        let dirs = directories::ProjectDirs::from("", "truffle", "truffle-lsp").unwrap();
        let path = dirs.cache_dir();
        std::fs::create_dir_all(path)?;
        let recursive_mode = notify::RecursiveMode::NonRecursive;
        self.watcher.watch(path, recursive_mode)?;

        let capabilities = Self::capabilities();
        let capabilities = serde_json::to_value(capabilities)?;
        let params = self.connection.initialize(capabilities)?;
        let params = serde_json::from_value(params)?;

        self.main_loop(params)?;
        self.io_threads.join()?;

        Ok(())
    }

    fn handle_event(&self, event: Event) {
        for path in event.paths {
            if let EventKind::Access(AccessKind::Close(AccessMode::Write)) = event.kind {
                let bytes = std::fs::read(&path)
                    .expect("only interpretters should write files to cache dir and all their files should be valid");
                let engine = postcard::from_bytes(&bytes)
                    .expect("cache dir should only contain valid serialized engines");
                self.engines.lock().unwrap().insert(path, engine);
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
                    let mut dispatcher = dispatch::Dispatcher {
                        request: Some(req),
                        server: self,
                    };

                    dispatcher
                        .dispatch::<GotoDefinition, _>(|param| self.lsp_goto_definition(param))?
                        .dispatch::<HoverRequest, _>(|param| self.lsp_hover(param))?
                        .dispatch::<References, _>(|param| self.lsp_find_all_references(param))?
                        .dispatch::<DocumentDiagnosticRequest, _>(|param| {
                            self.lsp_check_script(param)
                        })?
                        .dispatch::<Completion, _>(|param| self.lsp_completion(param))?;
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
            text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Kind(
                TextDocumentSyncKind::FULL,
            )),
            definition_provider: Some(OneOf::Left(true)),
            hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)),
            references_provider: Some(OneOf::Left(true)),
            diagnostic_provider: Some(lsp_types::DiagnosticServerCapabilities::Options(
                diagnostic_options,
            )),
            completion_provider: Some(CompletionOptions {
                resolve_provider: Some(true),
                trigger_characters: None,
                all_commit_characters: None,
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: None,
                },
                completion_item: None,
            }),
            ..Default::default()
        }
    }

    fn lsp_goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> eyre::Result<Option<GotoDefinitionResponse>> {
        let path = params
            .text_document_position_params
            .text_document
            .uri
            .to_file_path()
            .map_err(|()| eyre!("unsupported uri format"))?;
        let lock = self.engines.lock().unwrap();
        let engine = path
            .ancestors()
            .find_map(|path| lock.get(path))
            .unwrap_or(&self.default_engine);
        let contents = std::fs::read_to_string(path)?;
        let lookup = LineLookupTable::new(&contents);
        let location = lookup.from_position(params.text_document_position_params.position);
        let Some(location) = engine.goto_definition(location, contents.as_bytes()) else {
            return Ok(None);
        };
        let uri = params.text_document_position_params.text_document.uri;
        Ok(Some(GotoDefinitionResponse::Scalar(
            lookup.to_location(uri, location),
        )))
    }

    pub fn lsp_check_script(
        &self,
        params: DocumentDiagnosticParams,
    ) -> eyre::Result<DocumentDiagnosticReportResult> {
        let path = params
            .text_document
            .uri
            .to_file_path()
            .map_err(|()| eyre!("unsupported uri format"))?;
        let lock = self.engines.lock().unwrap();
        let engine = path
            .ancestors()
            .find_map(|path| lock.get(path))
            .unwrap_or(&self.default_engine);
        let contents = std::fs::read_to_string(&path)?;
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
        let report = DocumentDiagnosticReport::Full(result);
        Ok(DocumentDiagnosticReportResult::Report(report))
    }

    pub fn lsp_hover(&self, params: HoverParams) -> eyre::Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let path = uri
            .to_file_path()
            .map_err(|()| eyre!("unsupported uri format"))?;
        let lock = self.engines.lock().unwrap();
        let engine = path
            .ancestors()
            .find_map(|path| lock.get(path))
            .unwrap_or(&self.default_engine);
        let contents = std::fs::read_to_string(path)?;
        let lookup = LineLookupTable::new(&contents);
        let contents = contents.as_bytes();
        let location = lookup.from_position(params.text_document_position_params.position);
        let markdown = engine.hover(location, contents);
        let contents = lsp_types::MarkedString::from_markdown(markdown);
        let contents = lsp_types::HoverContents::Scalar(contents);
        let range = None;
        Ok(Some(Hover { contents, range }))
    }

    pub fn lsp_find_all_references(
        &self,
        params: ReferenceParams,
    ) -> eyre::Result<Option<Vec<Location>>> {
        let path = params
            .text_document_position
            .text_document
            .uri
            .to_file_path()
            .map_err(|()| eyre!("unsupported uri format"))?;
        let lock = self.engines.lock().unwrap();
        let engine = path
            .ancestors()
            .find_map(|path| lock.get(path))
            .unwrap_or(&self.default_engine);
        let contents = std::fs::read_to_string(path)?;
        let lookup = LineLookupTable::new(&contents);
        let location = lookup.from_position(params.text_document_position.position);
        let references = engine.find_all_references(location, contents.as_bytes());
        Ok(references.map(|references| {
            references
                .into_iter()
                .map(|location| {
                    lookup.to_location(
                        params.text_document_position.text_document.uri.clone(),
                        location,
                    )
                })
                .collect::<Vec<_>>()
        }))
    }

    pub fn lsp_completion(
        &self,
        params: CompletionParams,
    ) -> eyre::Result<Option<CompletionResponse>> {
        let path = &params
            .text_document_position
            .text_document
            .uri
            .to_file_path()
            .map_err(|()| eyre!("unsupported uri format"))?;
        let lock = self.engines.lock().unwrap();
        let engine = path
            .ancestors()
            .find_map(|path| lock.get(path))
            .unwrap_or(&self.default_engine);
        let contents = std::fs::read_to_string(path)?;
        let lookup = LineLookupTable::new(&contents);
        let location = lookup.from_position(params.text_document_position.position);
        let completions = engine.completion(location - 1, contents.as_bytes());
        let completions = completions
            .into_iter()
            .map(|completion_text| CompletionItem {
                label: completion_text,
                ..Default::default()
            })
            .collect();
        Ok(Some(CompletionResponse::Array(completions)))
    }
}
