//! SKALP Language Server Protocol (LSP) Server
//!
//! This binary provides IDE support for SKALP hardware description language.
//! It can be run as a standalone server or integrated into editors like VS Code.

use skalp_lsp::create_lsp_service;
use tower_lsp::Server;
use tracing::{info, Level};
use tracing_subscriber::FmtSubscriber;

#[tokio::main]
async fn main() {
    // Initialize logging
    let subscriber = FmtSubscriber::builder()
        .with_max_level(Level::INFO)
        .with_writer(std::io::stderr)
        .finish();

    tracing::subscriber::set_global_default(subscriber)
        .expect("setting default subscriber failed");

    info!("Starting SKALP Language Server");

    // Create the LSP service
    let (service, socket) = create_lsp_service();

    // Check if we're running in stdio mode (most common for LSP)
    let args: Vec<String> = std::env::args().collect();

    if args.len() > 1 && args[1] == "--tcp" {
        // TCP mode for debugging
        let addr = args.get(2)
            .map(|s| s.as_str())
            .unwrap_or("127.0.0.1:9257");

        info!("Running in TCP mode on {}", addr);

        let listener = tokio::net::TcpListener::bind(addr)
            .await
            .expect("Failed to bind TCP listener");

        info!("Listening for connections...");

        let (stream, addr) = listener.accept().await.expect("Failed to accept connection");
        info!("Client connected from: {}", addr);

        let (read, write) = tokio::io::split(stream);
        Server::new(read, write, socket).serve(service).await;
    } else {
        // Standard I/O mode (default)
        info!("Running in stdio mode");

        let stdin = tokio::io::stdin();
        let stdout = tokio::io::stdout();

        Server::new(stdin, stdout, socket).serve(service).await;
    }

    info!("SKALP Language Server shutting down");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_service_creation() {
        // Just verify that we can create the service
        let (_, _) = create_lsp_service();
    }
}