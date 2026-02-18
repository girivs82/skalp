//! Entry point for the `skalp-debug` binary.
//!
//! This binary wraps the SKALP simulator with a JSON-line debug protocol
//! for use by the VSCode DAP adapter.

use skalp_sim::debug_server::DebugServer;

#[tokio::main]
async fn main() {
    // All diagnostic output goes to stderr; stdout is reserved for the
    // JSON-line protocol with the DAP adapter.
    eprintln!("[skalp-debug] Starting debug server...");

    let mut server = DebugServer::new();
    server.run().await;

    eprintln!("[skalp-debug] Debug server exiting.");
}
