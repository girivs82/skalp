#![allow(dead_code, unused_variables, unused_imports, deprecated, clippy::non_canonical_clone_impl, clippy::uninlined_format_args)]
//! SKALP Language Server Protocol Implementation
//!
//! Provides IDE support for SKALP hardware description language including:
//! - Syntax highlighting and diagnostics
//! - Auto-completion
//! - Hover information
//! - Go-to definition
//! - Find references

use dashmap::DashMap;
use ropey::Rope;
use std::sync::Arc;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService};

pub mod completion;
pub mod diagnostics;
pub mod hover;
pub mod symbols;

/// SKALP Language Server
pub struct SkalpLanguageServer {
    client: Client,
    documents: Arc<DashMap<Url, DocumentState>>,
}

/// State of a single document
#[derive(Debug, Clone)]
pub struct DocumentState {
    /// Document content as a rope for efficient editing
    pub content: Rope,
    /// Document version
    pub version: i32,
    /// Parsed AST (if available)
    pub ast: Option<String>, // Simplified for now
    /// Diagnostics for this document
    pub diagnostics: Vec<Diagnostic>,
}

impl SkalpLanguageServer {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: Arc::new(DashMap::new()),
        }
    }

    /// Update diagnostics for a document
    async fn update_diagnostics(&self, uri: &Url) {
        if let Some(doc) = self.documents.get(uri) {
            let diagnostics = diagnostics::analyze_document(&doc.content.to_string());

            // Send diagnostics to client
            self.client
                .publish_diagnostics(uri.clone(), diagnostics.clone(), Some(doc.version))
                .await;

            // Update stored diagnostics
            drop(doc);
            if let Some(mut doc) = self.documents.get_mut(uri) {
                doc.diagnostics = diagnostics;
            }
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for SkalpLanguageServer {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
                    ..Default::default()
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                workspace_symbol_provider: Some(OneOf::Left(true)),
                diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
                    DiagnosticOptions {
                        identifier: Some("skalp".to_string()),
                        inter_file_dependencies: false,
                        workspace_diagnostics: false,
                        ..Default::default()
                    },
                )),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "SKALP Language Server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let content = Rope::from_str(&params.text_document.text);

        self.documents.insert(
            uri.clone(),
            DocumentState {
                content,
                version: params.text_document.version,
                ast: None,
                diagnostics: Vec::new(),
            },
        );

        self.update_diagnostics(&uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;

        // For simplicity, we're using full document sync
        if let Some(change) = params.content_changes.into_iter().next() {
            let content = Rope::from_str(&change.text);

            if let Some(mut doc) = self.documents.get_mut(&uri) {
                doc.content = content;
                doc.version = params.text_document.version;
                doc.ast = None; // Invalidate AST
            }

            self.update_diagnostics(&uri).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.documents.remove(&params.text_document.uri);
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        if let Some(doc) = self.documents.get(&uri) {
            let completions = completion::get_completions(&doc, position);
            Ok(Some(CompletionResponse::Array(completions)))
        } else {
            Ok(None)
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        if let Some(doc) = self.documents.get(&uri) {
            Ok(hover::get_hover(&doc, position))
        } else {
            Ok(None)
        }
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        if let Some(doc) = self.documents.get(&uri) {
            Ok(symbols::goto_definition(&doc, position, &uri))
        } else {
            Ok(None)
        }
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        if let Some(doc) = self.documents.get(&uri) {
            Ok(symbols::find_references(&doc, position, &uri))
        } else {
            Ok(None)
        }
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri;

        if let Some(doc) = self.documents.get(&uri) {
            let symbols = symbols::get_document_symbols(&doc);
            Ok(Some(DocumentSymbolResponse::Flat(symbols)))
        } else {
            Ok(None)
        }
    }
}

/// Create a new LSP service
pub fn create_lsp_service() -> (LspService<SkalpLanguageServer>, tower_lsp::ClientSocket) {
    LspService::new(SkalpLanguageServer::new)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_document_state_creation() {
        let content = Rope::from_str("entity counter { in clk: clock; }");
        let state = DocumentState {
            content: content.clone(),
            version: 1,
            ast: None,
            diagnostics: Vec::new(),
        };

        assert_eq!(state.version, 1);
        assert_eq!(
            state.content.to_string(),
            "entity counter { in clk: clock; }"
        );
        assert!(state.diagnostics.is_empty());
    }
}
