use std::error::Error;
use std::fs::File;
use std::path::PathBuf;
use std::sync::Mutex;

use clap::Parser;
use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response};
use lsp_types::request::{HoverRequest, References, DocumentDiagnosticRequest};
use lsp_types::{OneOf, DiagnosticOptions, WorkDoneProgressOptions};
use lsp_types::{
    request::GotoDefinition, InitializeParams, ServerCapabilities,
};
use tracing::info;
use tracing_subscriber::fmt::writer::Tee;
use tracing_subscriber::prelude::*;
use tracing_subscriber::{fmt, EnvFilter};
use truffle::Engine;

/// Truffle Language Server Protocol Implementation
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Path to write debug output too
    #[arg(short, long)]
    log_file: Option<PathBuf>,
}

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    let args = Args::parse();
    let filter_layer = EnvFilter::try_from_default_env()
        .or_else(|_| EnvFilter::try_new("info"))
        .unwrap();
    match &args.log_file {
        Some(logfile) => {
            let log_file = File::create(logfile)?;
            let log_file = Mutex::new(log_file);
            let writer = Tee::new(log_file, std::io::stderr);
            let fmt_layer = fmt::layer().with_writer(writer).with_target(false);

            tracing_subscriber::registry()
                .with(filter_layer)
                .with(fmt_layer)
                .init();
        }
        None => {
            let fmt_layer = fmt::layer().with_writer(std::io::stderr).with_target(false);

            tracing_subscriber::registry()
                .with(filter_layer)
                .with(fmt_layer)
                .init();
        }
    };

    // Note that  we must have our logging only write out to stderr.
    info!("starting generic LSP server");

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = Connection::stdio();

    let diagnostic_options = DiagnosticOptions {
        identifier: Some("truffle-lsp".to_string()),
        inter_file_dependencies: true,
        workspace_diagnostics: false,
        work_done_progress_options: WorkDoneProgressOptions { work_done_progress: None },
    };

    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        definition_provider: Some(OneOf::Left(true)),
        hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)),
        references_provider: Some(OneOf::Left(true)),
        diagnostic_provider: Some(lsp_types::DiagnosticServerCapabilities::Options(diagnostic_options)),
        ..Default::default()
    })
    .unwrap();
    let initialization_params = connection.initialize(server_capabilities)?;
    main_loop(connection, initialization_params)?;
    io_threads.join()?;

    // Shut down gracefully.
    info!("shutting down server");
    Ok(())
}


fn main_loop(
    connection: Connection,
    params: serde_json::Value,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let engine = if let Ok(bytes) = std::fs::read("./truffle.lsp.data") {
        dbg!(());
        postcard::from_bytes(&bytes).unwrap()
    } else {
        Engine::new()
    };
    let _params: InitializeParams = serde_json::from_value(params).unwrap();
    info!("starting example main loop");
    for msg in &connection.receiver {
        info!("got msg: {msg:?}");
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                info!("got request: {req:?}");
                match req.method.as_str() {
                    "textDocument/definition" => {
                        match cast::<GotoDefinition>(req) {
                            Ok((id, params)) => {
                                info!("got gotoDefinition request #{id}: {params:?}");
                                let definition = engine
                                    .lsp_goto_definition(params)
                                    .unwrap();
                                let result = serde_json::to_value(&definition).unwrap();
                                let resp = Response {
                                    id,
                                    result: Some(result),
                                    error: None,
                                };
                                connection.sender.send(Message::Response(resp))?;
                                continue;
                            }
                            Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                            Err(ExtractError::MethodMismatch(req)) => req,
                        };
                    }
                    "textDocument/hover" => {
                        match cast::<HoverRequest>(req) {
                            Ok((id, params)) => {
                                let hover = engine.lsp_hover(params);
                                let result = serde_json::to_value(&hover).unwrap();
                                let resp = Response {
                                    id,
                                    result: Some(result),
                                    error: None,
                                };
                                connection.sender.send(Message::Response(resp))?;
                                continue;
                            }
                            Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                            Err(ExtractError::MethodMismatch(req)) => req,
                        };
                    }
                    "textDocument/references" => {
                        match cast::<References>(req) {
                            Ok((id, params)) => {
                                let result = engine.lsp_find_all_references(params);
                                let result = serde_json::to_value(&result).unwrap();
                                let resp = Response {
                                    id,
                                    result: Some(result),
                                    error: None,
                                };
                                connection.sender.send(Message::Response(resp))?;
                                continue;
                            }
                            Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                            Err(ExtractError::MethodMismatch(req)) => req,
                        };
                    }
                    "textDocument/diagnostic" => {
                        match cast::<DocumentDiagnosticRequest>(req) {
                            Ok((id, params)) => {
                                let report = engine.lsp_check_script(params);
                                let result = serde_json::to_value(&report).unwrap();
                                let resp = Response {
                                    id,
                                    result: Some(result),
                                    error: None,
                                };
                                connection.sender.send(Message::Response(resp))?;
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

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}
