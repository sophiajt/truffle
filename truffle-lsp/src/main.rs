use std::fs::File;
use std::path::PathBuf;
use std::sync::Mutex;

use clap::Parser;
use color_eyre::eyre;
use tracing::info;
use tracing_subscriber::fmt::writer::Tee;
use tracing_subscriber::prelude::*;
use tracing_subscriber::{fmt, EnvFilter};

use crate::server::Server;

mod server;

/// Truffle Language Server Protocol Implementation
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Path to write debug output too
    #[arg(short, long)]
    log_file: Option<PathBuf>,
}

fn main() -> eyre::Result<()> {
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

    info!("starting truffle LSP server");
    let server = Server::new()?;
    server.run()?;
    info!("shutting down server");

    Ok(())
}
