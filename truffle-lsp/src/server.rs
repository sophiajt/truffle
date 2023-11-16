use std::{collections::HashMap, ffi::OsString};

use color_eyre::eyre;
use lsp_server::{Connection, IoThreads, Message};
use lsp_types::{
    notification::{DidChangeConfiguration, DidChangeTextDocument, DidOpenTextDocument},
    request::{Completion, DocumentDiagnosticRequest, GotoDefinition, HoverRequest, References},
    CompletionItem, CompletionOptions, CompletionParams, CompletionResponse, DiagnosticOptions,
    DidChangeTextDocumentParams, DocumentDiagnosticReportResult, GotoDefinitionParams,
    GotoDefinitionResponse, InitializeParams, OneOf, ServerCapabilities,
    TextDocumentContentChangeEvent, TextDocumentSyncKind, Url, VersionedTextDocumentIdentifier,
    WorkDoneProgressOptions,
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

use tracing::{debug, info};
use truffle::{Engine, LineLookupTable};

mod dispatch;

#[cfg(target_os = "linux")]
pub struct Server {
    watcher: INotifyWatcher,
    events: crossbeam_channel::Receiver<notify::Result<Event>>,
    connection: Connection,
    io_threads: IoThreads,
    engines: HashMap<OsString, Engine>,
    default_engine: Engine,
}

#[cfg(target_os = "macos")]
pub struct Server {
    watcher: KqueueWatcher,
    events: crossbeam_channel::Receiver<notify::Result<Event>>,
    connection: Connection,
    io_threads: IoThreads,
    engines: HashMap<String, Engine>,
    default_engine: Engine,
}

impl Server {
    pub fn new() -> eyre::Result<Self> {
        let (connection, io_threads) = Connection::stdio();
        let (sender, receiver) = crossbeam_channel::unbounded();
        let watcher = notify::recommended_watcher(sender)?;
        let engines = Default::default();
        let default_engine = Engine::new();

        Ok(Self {
            watcher,
            events: receiver,
            connection,
            io_threads,
            default_engine,
            engines,
        })
    }

    fn capabilities() -> ServerCapabilities {
        let text_document_sync = Some(lsp_types::TextDocumentSyncCapability::Kind(
            TextDocumentSyncKind::FULL,
        ));
        let definition_provider = Some(OneOf::Left(true));
        let hover_provider = Some(lsp_types::HoverProviderCapability::Simple(true));
        let references_provider = Some(OneOf::Left(true));
        let diagnostic_options = DiagnosticOptions {
            identifier: Some("truffle-lsp".to_string()),
            inter_file_dependencies: true,
            workspace_diagnostics: false,
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: None,
            },
        };
        let diagnostic_provider = Some(lsp_types::DiagnosticServerCapabilities::Options(
            diagnostic_options,
        ));
        let completion_provider = Some(CompletionOptions {
            resolve_provider: Some(true),
            trigger_characters: None,
            all_commit_characters: None,
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: None,
            },
            completion_item: None,
        });

        ServerCapabilities {
            text_document_sync,
            definition_provider,
            hover_provider,
            references_provider,
            diagnostic_provider,
            completion_provider,
            ..Default::default()
        }
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

    fn handle_event(&mut self, event: Event) {
        for path in event.paths {
            if let EventKind::Access(AccessKind::Close(AccessMode::Write)) = event.kind {
                let Some(extension) = path.extension() else {
                    return;
                };
                if extension != "lspdata" {
                    return;
                };
                let Some(name) = path.file_stem() else { return };

                let bytes = std::fs::read(&path)
                    .expect("only interpretters should write files to cache dir and all their files should be valid");
                let engine = postcard::from_bytes(&bytes)
                    .expect("cache dir should only contain valid serialized engines");
                self.engines.insert(name.to_os_string(), engine);
            }
        }
    }

    fn main_loop(&mut self, params: InitializeParams) -> eyre::Result<()> {
        info!("starting example main loop");
        debug!(?params);
        let mut documents = DocumentStore::new();

        if !self.events.is_empty() {
            while let Ok(event) = self.events.try_recv() {
                let event = event?;
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

                    let documents = &documents;
                    dispatcher
                        .dispatch::<GotoDefinition, _>(|param| {
                            self.lsp_goto_definition(param, documents)
                        })?
                        .dispatch::<HoverRequest, _>(|param| self.lsp_hover(param, documents))?
                        .dispatch::<References, _>(|param| {
                            self.lsp_find_all_references(param, documents)
                        })?
                        .dispatch::<DocumentDiagnosticRequest, _>(|param| {
                            self.lsp_check_script(param, documents)
                        })?
                        .dispatch::<Completion, _>(|param| self.lsp_completion(param, documents))?;
                }
                Message::Response(resp) => {
                    info!("got response: {resp:?}");
                }
                Message::Notification(not) => {
                    info!("got notification: {not:?}");
                    let mut dispatcher = dispatch::Dispatcher {
                        request: Some(not),
                        server: self,
                    };

                    dispatcher
                        .dispatch::<DidChangeConfiguration, _>(|params| {
                            documents.workspace_did_change_configuration(params)
                        })?
                        .dispatch::<DidOpenTextDocument, _>(|params| {
                            documents.document_did_open(params)
                        })?
                        .dispatch::<DidChangeTextDocument, _>(|params| {
                            documents.document_did_change(params)
                        })?;
                }
            }
        }

        Ok(())
    }

    fn lsp_goto_definition(
        &self,
        params: GotoDefinitionParams,
        documents: &DocumentStore,
    ) -> eyre::Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let engine = self.engine_for(&uri);
        let contents = documents.contents_for(&uri);
        let lookup = LineLookupTable::new(contents);
        let location = lookup.from_position(params.text_document_position_params.position);
        let Some(location) = engine.goto_definition(location, contents.as_bytes()) else {
            return Ok(None);
        };
        Ok(Some(GotoDefinitionResponse::Scalar(
            lookup.to_location(uri, location),
        )))
    }

    pub fn lsp_check_script(
        &self,
        params: DocumentDiagnosticParams,
        documents: &DocumentStore,
    ) -> eyre::Result<DocumentDiagnosticReportResult> {
        let uri = &params.text_document.uri;
        let engine = self.engine_for(uri);
        let contents = documents.contents_for(uri);
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

    pub fn lsp_hover(
        &self,
        params: HoverParams,
        documents: &DocumentStore,
    ) -> eyre::Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let engine = self.engine_for(uri);
        let contents = documents.contents_for(uri);
        let lookup = LineLookupTable::new(contents);
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
        documents: &DocumentStore,
    ) -> eyre::Result<Option<Vec<Location>>> {
        let uri = &params.text_document_position.text_document.uri.clone();
        let engine = self.engine_for(uri);
        let contents = documents.contents_for(uri);
        let lookup = LineLookupTable::new(contents);
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
        documents: &DocumentStore,
    ) -> eyre::Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let engine = self.engine_for(uri);
        let contents = documents.contents_for(uri);
        let lookup = LineLookupTable::new(contents);
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

    fn engine_for(&self, _uri: &Url) -> &Engine {
        // TODO: lookup proper engine
        &self.default_engine
    }
}

pub struct DocumentStore {
    open_files: HashMap<Url, String>,
}

impl DocumentStore {
    fn new() -> Self {
        Self {
            open_files: Default::default(),
        }
    }

    fn document_did_change(
        &mut self,
        params: DidChangeTextDocumentParams,
    ) -> Result<(), eyre::Error> {
        dbg!(&params);
        let DidChangeTextDocumentParams {
            text_document,
            content_changes,
        } = params;
        let VersionedTextDocumentIdentifier { uri, .. } = text_document;
        assert!(content_changes.len() == 1);
        let Some(TextDocumentContentChangeEvent {
            range: None, text, ..
        }) = content_changes.into_iter().next()
        else {
            unimplemented!("should never happen because we only register full updates as a supported capability");
        };

        self.open_files.insert(uri, text);

        Ok(())
    }

    fn document_did_open(
        &mut self,
        params: lsp_types::DidOpenTextDocumentParams,
    ) -> Result<(), eyre::Error> {
        dbg!(&params);
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        self.open_files.insert(uri, text);
        Ok(())
    }

    fn workspace_did_change_configuration(
        &self,
        params: lsp_types::DidChangeConfigurationParams,
    ) -> Result<(), eyre::Error> {
        dbg!(params);
        Ok(())
    }

    fn contents_for(&self, uri: &Url) -> &String {
        let panic_msg = "requests should only reference files we've already been notified are open, all open files should be stored in open_files";
        self.open_files.get(uri).expect(panic_msg)
    }
}
