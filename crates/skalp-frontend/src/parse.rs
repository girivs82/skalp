//! Rowan-based parser for SKALP
//!
//! This module implements the actual parsing logic using Rowan's GreenNodeBuilder

use crate::lexer::{Lexer, Token, TokenWithPos};
use crate::syntax::{SyntaxKind, SyntaxNode};
use rowan::{GreenNode, GreenNodeBuilder};

/// Parser state for building Rowan trees
pub struct ParseState<'a> {
    /// Tokens from lexer
    tokens: Vec<TokenWithPos>,
    /// Current token position
    current: usize,
    /// Green node builder
    builder: GreenNodeBuilder<'static>,
    /// Source text
    source: &'a str,
}

impl<'a> ParseState<'a> {
    /// Create a new parser state
    pub fn new(source: &'a str) -> Self {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize();

        Self {
            tokens,
            current: 0,
            builder: GreenNodeBuilder::new(),
            source,
        }
    }

    /// Parse the source file
    pub fn parse_source_file(mut self) -> ParseResult {
        self.start_node(SyntaxKind::SOURCE_FILE);

        // Parse all top-level items
        while !self.is_at_end() {
            // Skip whitespace and comments at top level
            self.skip_trivia();

            if self.is_at_end() {
                break;
            }

            // Parse top-level items
            match self.current_kind() {
                Some(SyntaxKind::ENTITY_KW) => self.parse_entity_decl(),
                Some(SyntaxKind::IMPL_KW) => self.parse_impl_block(),
                Some(SyntaxKind::PROTOCOL_KW) => self.parse_protocol_decl(),
                Some(SyntaxKind::INTENT_KW) => self.parse_intent_decl(),
                Some(SyntaxKind::REQUIREMENT_KW) => self.parse_requirement_decl(),
                _ => {
                    // Unknown item - consume token as error and continue
                    self.error_and_bump("expected top-level item");
                }
            }
        }

        self.finish_node();

        ParseResult {
            green_node: self.builder.finish(),
            errors: Vec::new(), // TODO: Collect errors during parsing
        }
    }

    /// Parse entity declaration
    fn parse_entity_decl(&mut self) {
        self.start_node(SyntaxKind::ENTITY_DECL);

        // 'entity' keyword
        self.expect(SyntaxKind::ENTITY_KW);

        // Entity name
        self.expect(SyntaxKind::IDENT);

        // Optional generic parameters
        if self.at(SyntaxKind::LT) {
            self.parse_generic_params();
        }

        // Port list
        self.expect(SyntaxKind::L_BRACE);
        self.parse_port_list();
        self.expect(SyntaxKind::R_BRACE);

        self.finish_node();
    }

    /// Parse implementation block
    fn parse_impl_block(&mut self) {
        self.start_node(SyntaxKind::IMPL_BLOCK);

        // 'impl' keyword
        self.expect(SyntaxKind::IMPL_KW);

        // Entity name
        self.expect(SyntaxKind::IDENT);

        // Optional generic parameters
        if self.at(SyntaxKind::LT) {
            self.parse_generic_params();
        }

        // Implementation body
        self.expect(SyntaxKind::L_BRACE);
        self.parse_impl_body();
        self.expect(SyntaxKind::R_BRACE);

        self.finish_node();
    }

    /// Parse implementation body
    fn parse_impl_body(&mut self) {
        while !self.at(SyntaxKind::R_BRACE) && !self.is_at_end() {
            self.skip_trivia();

            match self.current_kind() {
                Some(SyntaxKind::SIGNAL_KW) => self.parse_signal_decl(),
                Some(SyntaxKind::VAR_KW) => self.parse_variable_decl(),
                Some(SyntaxKind::CONST_KW) => self.parse_constant_decl(),
                Some(SyntaxKind::ON_KW) => self.parse_event_block(),
                Some(SyntaxKind::IDENT) => {
                    // Could be an assignment or start of another construct
                    self.parse_assignment_or_statement();
                }
                Some(SyntaxKind::R_BRACE) => break,
                _ => {
                    self.error_and_bump("expected implementation item");
                }
            }
        }
    }

    /// Parse port list
    fn parse_port_list(&mut self) {
        self.start_node(SyntaxKind::PORT_LIST);

        while !self.at(SyntaxKind::R_BRACE) && !self.is_at_end() {
            self.skip_trivia();

            if self.at_port_direction() {
                self.parse_port_decl();
            } else if self.at(SyntaxKind::R_BRACE) {
                break;
            } else {
                self.error_and_bump("expected port declaration");
            }
        }

        self.finish_node();
    }

    /// Parse port declaration
    fn parse_port_decl(&mut self) {
        self.start_node(SyntaxKind::PORT_DECL);

        // Port direction
        self.start_node(SyntaxKind::PORT_DIRECTION);
        if self.at(SyntaxKind::IN_KW) {
            self.bump();
        } else if self.at(SyntaxKind::OUT_KW) {
            self.bump();
        } else if self.at(SyntaxKind::INOUT_KW) {
            self.bump();
        }
        self.finish_node();

        // Port name
        self.expect(SyntaxKind::IDENT);

        // Colon and type
        self.expect(SyntaxKind::COLON);
        self.parse_type();

        self.finish_node();
    }

    /// Parse signal declaration
    fn parse_signal_decl(&mut self) {
        self.start_node(SyntaxKind::SIGNAL_DECL);

        self.expect(SyntaxKind::SIGNAL_KW);
        self.expect(SyntaxKind::IDENT);
        self.expect(SyntaxKind::COLON);
        self.parse_type();

        // Optional initial value
        if self.at(SyntaxKind::ASSIGN) {
            self.bump();
            self.parse_expression();
        }

        self.finish_node();
    }

    /// Parse variable declaration
    fn parse_variable_decl(&mut self) {
        self.start_node(SyntaxKind::VARIABLE_DECL);

        self.expect(SyntaxKind::VAR_KW);
        self.expect(SyntaxKind::IDENT);
        self.expect(SyntaxKind::COLON);
        self.parse_type();

        // Optional initial value
        if self.at(SyntaxKind::ASSIGN) {
            self.bump();
            self.parse_expression();
        }

        self.finish_node();
    }

    /// Parse constant declaration
    fn parse_constant_decl(&mut self) {
        self.start_node(SyntaxKind::CONSTANT_DECL);

        self.expect(SyntaxKind::CONST_KW);
        self.expect(SyntaxKind::IDENT);
        self.expect(SyntaxKind::COLON);
        self.parse_type();
        self.expect(SyntaxKind::ASSIGN);
        self.parse_expression();

        self.finish_node();
    }

    /// Parse event block (on clause)
    fn parse_event_block(&mut self) {
        self.start_node(SyntaxKind::EVENT_BLOCK);

        self.expect(SyntaxKind::ON_KW);
        self.expect(SyntaxKind::L_PAREN);

        // Parse event triggers
        self.start_node(SyntaxKind::EVENT_TRIGGER_LIST);
        self.parse_event_trigger();

        while self.at(SyntaxKind::PIPE) {
            self.bump();
            self.parse_event_trigger();
        }
        self.finish_node();

        self.expect(SyntaxKind::R_PAREN);

        // Parse event body
        self.parse_block_statement();

        self.finish_node();
    }

    /// Parse event trigger (e.g., clk.rise)
    fn parse_event_trigger(&mut self) {
        self.start_node(SyntaxKind::EVENT_TRIGGER);

        // Signal name
        self.expect(SyntaxKind::IDENT);

        // Dot and edge type
        if self.at(SyntaxKind::DOT) {
            self.bump();

            self.start_node(SyntaxKind::EDGE_TYPE);
            if self.at(SyntaxKind::RISE_KW) || self.at(SyntaxKind::FALL_KW) || self.at(SyntaxKind::EDGE_KW) {
                self.bump();
            } else {
                self.error("expected 'rise', 'fall', or 'edge'");
            }
            self.finish_node();
        }

        self.finish_node();
    }

    /// Parse assignment or other statement starting with identifier
    fn parse_assignment_or_statement(&mut self) {
        // For now, just parse as assignment
        // In the future, we could look ahead to determine the statement type
        self.parse_assignment_stmt();
    }

    /// Parse assignment statement
    fn parse_assignment_stmt(&mut self) {
        self.start_node(SyntaxKind::ASSIGNMENT_STMT);

        // Left-hand side (identifier or field access)
        self.parse_expression();

        // Assignment operator
        if self.at(SyntaxKind::NON_BLOCKING_ASSIGN) ||
           self.at(SyntaxKind::BLOCKING_ASSIGN) ||
           self.at(SyntaxKind::ASSIGN) {
            self.bump();
        } else {
            self.error("expected assignment operator");
        }

        // Right-hand side
        self.parse_expression();

        self.finish_node();
    }

    /// Parse block statement
    fn parse_block_statement(&mut self) {
        self.start_node(SyntaxKind::BLOCK_STMT);

        self.expect(SyntaxKind::L_BRACE);

        while !self.at(SyntaxKind::R_BRACE) && !self.is_at_end() {
            self.skip_trivia();

            match self.current_kind() {
                Some(SyntaxKind::IF_KW) => self.parse_if_statement(),
                Some(SyntaxKind::MATCH_KW) => self.parse_match_statement(),
                Some(SyntaxKind::IDENT) => self.parse_assignment_stmt(),
                Some(SyntaxKind::L_BRACE) => self.parse_block_statement(),
                Some(SyntaxKind::R_BRACE) => break,
                _ => {
                    self.error_and_bump("expected statement");
                }
            }
        }

        self.expect(SyntaxKind::R_BRACE);

        self.finish_node();
    }

    /// Parse if statement
    fn parse_if_statement(&mut self) {
        self.start_node(SyntaxKind::IF_STMT);

        self.expect(SyntaxKind::IF_KW);
        self.expect(SyntaxKind::L_PAREN);
        self.parse_expression();
        self.expect(SyntaxKind::R_PAREN);

        self.parse_block_statement();

        if self.at(SyntaxKind::ELSE_KW) {
            self.bump();
            self.parse_block_statement();
        }

        self.finish_node();
    }

    /// Parse match statement (stub for now)
    fn parse_match_statement(&mut self) {
        self.start_node(SyntaxKind::MATCH_STMT);

        self.expect(SyntaxKind::MATCH_KW);
        self.parse_expression();
        self.expect(SyntaxKind::L_BRACE);

        // TODO: Parse match arms

        self.expect(SyntaxKind::R_BRACE);

        self.finish_node();
    }

    /// Parse protocol declaration (stub)
    fn parse_protocol_decl(&mut self) {
        self.start_node(SyntaxKind::PROTOCOL_DECL);
        self.expect(SyntaxKind::PROTOCOL_KW);
        self.expect(SyntaxKind::IDENT);
        self.expect(SyntaxKind::L_BRACE);
        // TODO: Parse protocol signals
        self.expect(SyntaxKind::R_BRACE);
        self.finish_node();
    }

    /// Parse intent declaration (stub)
    fn parse_intent_decl(&mut self) {
        self.start_node(SyntaxKind::INTENT_DECL);
        self.expect(SyntaxKind::INTENT_KW);
        self.expect(SyntaxKind::IDENT);
        self.expect(SyntaxKind::L_BRACE);
        // TODO: Parse intent constraints
        self.expect(SyntaxKind::R_BRACE);
        self.finish_node();
    }

    /// Parse requirement declaration (stub)
    fn parse_requirement_decl(&mut self) {
        self.start_node(SyntaxKind::REQUIREMENT_DECL);
        self.expect(SyntaxKind::REQUIREMENT_KW);
        self.expect(SyntaxKind::IDENT);
        self.expect(SyntaxKind::L_BRACE);
        // TODO: Parse requirement details
        self.expect(SyntaxKind::R_BRACE);
        self.finish_node();
    }

    /// Parse type annotation
    fn parse_type(&mut self) {
        self.start_node(SyntaxKind::TYPE_ANNOTATION);

        match self.current_kind() {
            Some(SyntaxKind::BIT_KW) => {
                self.start_node(SyntaxKind::BIT_TYPE);
                self.bump();
                self.parse_width_spec();
                self.finish_node();
            }
            Some(SyntaxKind::LOGIC_KW) => {
                self.start_node(SyntaxKind::LOGIC_TYPE);
                self.bump();
                self.parse_width_spec();
                self.finish_node();
            }
            Some(SyntaxKind::INT_KW) => {
                self.start_node(SyntaxKind::INT_TYPE);
                self.bump();
                self.parse_width_spec();
                self.finish_node();
            }
            Some(SyntaxKind::NAT_KW) => {
                self.start_node(SyntaxKind::NAT_TYPE);
                self.bump();
                self.parse_width_spec();
                self.finish_node();
            }
            Some(SyntaxKind::CLOCK_KW) => {
                self.start_node(SyntaxKind::CLOCK_TYPE);
                self.bump();
                self.finish_node();
            }
            Some(SyntaxKind::RESET_KW) => {
                self.start_node(SyntaxKind::RESET_TYPE);
                self.bump();
                self.finish_node();
            }
            Some(SyntaxKind::IDENT) => {
                self.start_node(SyntaxKind::CUSTOM_TYPE);
                self.bump();
                self.finish_node();
            }
            _ => {
                self.error("expected type");
            }
        }

        self.finish_node();
    }

    /// Parse width specification [N]
    fn parse_width_spec(&mut self) {
        if self.at(SyntaxKind::L_BRACKET) {
            self.start_node(SyntaxKind::WIDTH_SPEC);
            self.bump();
            self.expect(SyntaxKind::INT_LITERAL);
            self.expect(SyntaxKind::R_BRACKET);
            self.finish_node();
        }
    }

    /// Parse generic parameters (stub)
    fn parse_generic_params(&mut self) {
        self.start_node(SyntaxKind::GENERIC_PARAM_LIST);
        self.expect(SyntaxKind::LT);
        // TODO: Parse actual generic parameters
        self.expect(SyntaxKind::GT);
        self.finish_node();
    }

    /// Parse expression
    fn parse_expression(&mut self) {
        self.parse_primary_expression();

        // Check for binary operators
        while let Some(op) = self.current_binary_op() {
            let op_kind = self.current_kind().unwrap();
            self.start_node(SyntaxKind::BINARY_EXPR);

            // The left operand is already parsed
            self.bump(); // Consume operator
            self.parse_primary_expression(); // Parse right operand

            self.finish_node();
        }
    }

    /// Parse primary expression
    fn parse_primary_expression(&mut self) {
        match self.current_kind() {
            Some(SyntaxKind::INT_LITERAL | SyntaxKind::BIN_LITERAL |
                 SyntaxKind::HEX_LITERAL | SyntaxKind::STRING_LITERAL) => {
                self.start_node(SyntaxKind::LITERAL_EXPR);
                self.bump();
                self.finish_node();
            }
            Some(SyntaxKind::IDENT) => {
                self.start_node(SyntaxKind::IDENT_EXPR);
                self.bump();

                // Check for field access or method call
                while self.at(SyntaxKind::DOT) {
                    self.start_node(SyntaxKind::FIELD_EXPR);
                    self.bump();
                    self.expect(SyntaxKind::IDENT);
                    self.finish_node();
                }

                self.finish_node();
            }
            Some(SyntaxKind::L_PAREN) => {
                self.start_node(SyntaxKind::PAREN_EXPR);
                self.bump();
                self.parse_expression();
                self.expect(SyntaxKind::R_PAREN);
                self.finish_node();
            }
            Some(SyntaxKind::BANG | SyntaxKind::TILDE | SyntaxKind::MINUS) => {
                self.start_node(SyntaxKind::UNARY_EXPR);
                self.bump();
                self.parse_primary_expression();
                self.finish_node();
            }
            _ => {
                self.error("expected expression");
            }
        }
    }

    // === Helper methods ===

    /// Start a new syntax node
    fn start_node(&mut self, kind: SyntaxKind) {
        self.builder.start_node(rowan::SyntaxKind(kind as u16));
    }

    /// Finish the current syntax node
    fn finish_node(&mut self) {
        self.builder.finish_node();
    }

    /// Get current token kind
    fn current_kind(&self) -> Option<SyntaxKind> {
        self.current_token()
            .map(|t| SyntaxKind::from(t.token.clone()))
    }

    /// Get current token
    fn current_token(&self) -> Option<&TokenWithPos> {
        self.tokens.get(self.current)
    }

    /// Check if at end of input
    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    /// Check if current token is of given kind
    fn at(&self, kind: SyntaxKind) -> bool {
        self.current_kind() == Some(kind)
    }

    /// Check if at port direction keyword
    fn at_port_direction(&self) -> bool {
        matches!(
            self.current_kind(),
            Some(SyntaxKind::IN_KW | SyntaxKind::OUT_KW | SyntaxKind::INOUT_KW)
        )
    }

    /// Get current binary operator
    fn current_binary_op(&self) -> Option<SyntaxKind> {
        self.current_kind().filter(|k| k.is_operator())
    }

    /// Consume current token
    fn bump(&mut self) {
        if let Some(token) = self.current_token() {
            let kind = SyntaxKind::from(token.token.clone());
            let text = &self.source[token.span.clone()];
            self.builder.token(rowan::SyntaxKind(kind as u16), text);
            self.current += 1;
        }
    }

    /// Skip whitespace and comments
    fn skip_trivia(&mut self) {
        while let Some(kind) = self.current_kind() {
            if kind.is_trivia() {
                self.bump();
            } else {
                break;
            }
        }
    }

    /// Expect a specific token kind
    fn expect(&mut self, kind: SyntaxKind) {
        if self.at(kind) {
            self.bump();
        } else {
            self.error(&format!("expected {}", kind.description()));
        }
    }

    /// Report an error
    fn error(&mut self, message: &str) {
        // TODO: Collect errors properly
        eprintln!("Parse error at token {}: {}", self.current, message);
    }

    /// Report an error and consume token
    fn error_and_bump(&mut self, message: &str) {
        self.error(message);
        if !self.is_at_end() {
            self.bump();
        }
    }
}

/// Parse result containing the syntax tree and any errors
pub struct ParseResult {
    pub green_node: GreenNode,
    pub errors: Vec<ParseError>,
}

/// Parse error information
#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub position: usize,
}

/// Main parsing function
pub fn parse(source: &str) -> SyntaxNode {
    let parser = ParseState::new(source);
    let result = parser.parse_source_file();
    SyntaxNode::new_root(result.green_node)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::SyntaxNodeExt;

    #[test]
    fn test_parse_empty() {
        let source = "";
        let tree = parse(source);
        assert_eq!(tree.kind(), SyntaxKind::SOURCE_FILE);
    }

    #[test]
    fn test_parse_entity() {
        let source = "entity Counter { in clk: clock out count: nat[8] }";
        let tree = parse(source);
        assert_eq!(tree.kind(), SyntaxKind::SOURCE_FILE);

        let entity = tree.first_child().unwrap();
        assert_eq!(entity.kind(), SyntaxKind::ENTITY_DECL);
    }

    #[test]
    fn test_parse_impl_with_event() {
        let source = r#"
            impl Counter {
                signal counter: nat[8] = 0

                on(clk.rise) {
                    counter <= counter + 1
                }

                count = counter
            }
        "#;
        let tree = parse(source);
        assert_eq!(tree.kind(), SyntaxKind::SOURCE_FILE);

        let impl_block = tree.first_child().unwrap();
        assert_eq!(impl_block.kind(), SyntaxKind::IMPL_BLOCK);
    }
}