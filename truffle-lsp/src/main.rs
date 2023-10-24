//! A minimal example LSP server that can only respond to the `gotoDefinition` request. To use
//! this example, execute it and then send an `initialize` request.
//!
//! ```no_run
//! Content-Length: 85
//!
//! {"jsonrpc": "2.0", "method": "initialize", "id": 1, "params": {"capabilities": {}}}
//! ```
//!
//! This will respond with a server response. Then send it a `initialized` notification which will
//! have no response.
//!
//! ```no_run
//! Content-Length: 59
//!
//! {"jsonrpc": "2.0", "method": "initialized", "params": {}}
//! ```
//!
//! Once these two are sent, then we enter the main loop of the server. The only request this
//! example can handle is `gotoDefinition`:
//!
//! ```no_run
//! Content-Length: 159
//!
//! {"jsonrpc": "2.0", "method": "textDocument/definition", "id": 2, "params": {"textDocument": {"uri": "file://temp"}, "position": {"line": 1, "character": 1}}}
//! ```
//!
//! To finish up without errors, send a shutdown request:
//!
//! ```no_run
//! Content-Length: 67
//!
//! {"jsonrpc": "2.0", "method": "shutdown", "id": 3, "params": null}
//! ```
//!
//! The server will exit the main loop and finally we send a `shutdown` notification to stop
//! the server.
//!
//! ```
//! Content-Length: 54
//!
//! {"jsonrpc": "2.0", "method": "exit", "params": null}
//! ```
use std::error::Error;
use std::fs::File;
use std::path::PathBuf;
use std::sync::Mutex;

use clap::Parser;
use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response};
use lsp_types::{Location, OneOf, Position, Range};
use lsp_types::{
    request::GotoDefinition, GotoDefinitionResponse, InitializeParams, ServerCapabilities,
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

    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        definition_provider: Some(OneOf::Left(true)),
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

#[derive(Debug)]
struct LineLookupTable {
    /// array of character indexes for each consecutive newline in the input file
    line_starts: Vec<usize>,
}

impl LineLookupTable {
    fn new(contents: &str) -> Self {
        let mut line_starts: Vec<_> = contents
            .encode_utf16()
            .enumerate()
            .filter(|&(_, c)| c == '\n' as u16)
            .map(|(i, _)| i + 1)
            .collect();
        line_starts.insert(0, 0);
        Self { line_starts }
    }

    fn from_position(&self, Position { line, character }: Position) -> usize {
        self.line_starts[line as usize] + character as usize
    }

    fn to_position(&self, position: usize) -> Position {
        let line = self.line_starts.partition_point(|&ind| ind < position) - 1;
        let character = position - self.line_starts[line];
        Position {
            line: line as u32,
            character: character as u32,
        }
    }
}

fn main_loop(
    connection: Connection,
    params: serde_json::Value,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let engine = Engine::new();
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
                match cast::<GotoDefinition>(req) {
                    Ok((id, params)) => {
                        info!("got gotoDefinition request #{id}: {params:?}");
                        let path = params
                            .text_document_position_params
                            .text_document
                            .uri
                            .to_file_path()
                            .unwrap();
                        let contents = std::fs::read_to_string(path).unwrap();
                        let lookup = LineLookupTable::new(&contents);
                        info!("lookup table: {lookup:?}");
                        let location =
                            lookup.from_position(params.text_document_position_params.position);
                        let (start, end) = engine
                            .lsp_goto_definition(location, contents.as_bytes())
                            .unwrap();
                        let uri = params.text_document_position_params.text_document.uri;
                        let start = lookup.to_position(start);
                        let end = lookup.to_position(end);
                        let range = Range { start, end };
                        let result = Some(GotoDefinitionResponse::Scalar(Location { uri, range }));
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
                // ...
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
