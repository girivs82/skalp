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
                Some(SyntaxKind::TRAIT_KW) => self.parse_trait_def(),
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
        // Start the node first
        self.start_node(SyntaxKind::IMPL_BLOCK);

        // 'impl' keyword
        self.expect(SyntaxKind::IMPL_KW);

        // Look ahead to determine if this is a trait impl
        // After consuming 'impl', position 0 is the next token
        // Check if we have: <trait> for <type>
        let is_trait_impl = {
            let mut found_for = false;
            let mut lookahead = 0;

            // Look for 'for' keyword within the next few tokens
            // Pattern: IDENT [<generics>] FOR IDENT
            if self.peek_kind(0) == Some(SyntaxKind::IDENT) {
                lookahead = 1;

                // Skip generic parameters if present
                if self.peek_kind(lookahead) == Some(SyntaxKind::LT) {
                    // Simple skip - just look for 'for' keyword
                    while lookahead < 10 && self.peek_kind(lookahead).is_some() {
                        if self.peek_kind(lookahead) == Some(SyntaxKind::FOR_KW) {
                            found_for = true;
                            break;
                        }
                        lookahead += 1;
                    }
                } else if self.peek_kind(lookahead) == Some(SyntaxKind::FOR_KW) {
                    found_for = true;
                }
            }
            found_for
        };

        // Close the IMPL_BLOCK node and start the appropriate node type
        self.finish_node();

        if is_trait_impl {
            self.parse_trait_impl_after_keyword();
        } else {
            self.parse_entity_impl_after_keyword();
        }
    }

    /// Parse regular entity implementation after 'impl' keyword
    fn parse_entity_impl_after_keyword(&mut self) {
        self.start_node(SyntaxKind::IMPL_BLOCK);

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

    /// Parse trait implementation after 'impl' keyword
    fn parse_trait_impl_after_keyword(&mut self) {
        self.start_node(SyntaxKind::TRAIT_IMPL);

        // Trait name
        self.expect(SyntaxKind::IDENT);

        // Optional generic parameters for trait
        if self.at(SyntaxKind::LT) {
            self.parse_generic_params();
        }

        // 'for' keyword
        self.expect(SyntaxKind::FOR_KW);

        // Target type
        self.expect(SyntaxKind::IDENT);

        // Optional generic parameters for target
        if self.at(SyntaxKind::LT) {
            self.parse_generic_params();
        }

        // Optional where clause
        if self.at(SyntaxKind::WHERE_KW) {
            self.parse_where_clause();
        }

        // Implementation body
        self.expect(SyntaxKind::L_BRACE);
        self.parse_trait_impl_body();
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
                Some(SyntaxKind::MATCH_KW) => self.parse_match_statement(),
                Some(SyntaxKind::FLOW_KW) => self.parse_flow_statement(),
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
                Some(SyntaxKind::FLOW_KW) => self.parse_flow_statement(),
                Some(SyntaxKind::IDENT) => self.parse_assignment_or_statement(),
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

    /// Parse match statement
    fn parse_match_statement(&mut self) {
        self.start_node(SyntaxKind::MATCH_STMT);

        self.expect(SyntaxKind::MATCH_KW);
        self.parse_expression();
        self.expect(SyntaxKind::L_BRACE);

        // Parse match arms
        self.start_node(SyntaxKind::MATCH_ARM_LIST);
        while !self.at(SyntaxKind::R_BRACE) && !self.is_at_end() {
            self.parse_match_arm();
        }
        self.finish_node();

        self.expect(SyntaxKind::R_BRACE);

        self.finish_node();
    }

    /// Parse a single match arm
    fn parse_match_arm(&mut self) {
        self.start_node(SyntaxKind::MATCH_ARM);

        // Parse pattern
        self.parse_pattern();

        // Expect arrow (=>)
        self.expect(SyntaxKind::ARROW);

        // Parse arm body (statement or block)
        if self.at(SyntaxKind::L_BRACE) {
            self.parse_block_statement();
        } else {
            self.parse_assignment_or_statement();
        }

        // Optional comma
        if self.at(SyntaxKind::COMMA) {
            self.bump();
        }

        self.finish_node();
    }

    /// Parse a pattern
    fn parse_pattern(&mut self) {
        match self.current_kind() {
            Some(SyntaxKind::IDENT) => {
                // Check if it's a wildcard pattern (_) or identifier pattern
                if let Some(text) = self.current_text() {
                    if text == "_" {
                        // Wildcard pattern
                        self.start_node(SyntaxKind::WILDCARD_PATTERN);
                        self.bump();
                        self.finish_node();
                    } else {
                        // Identifier pattern
                        self.start_node(SyntaxKind::IDENT_PATTERN);
                        self.bump();
                        self.finish_node();
                    }
                } else {
                    // Fallback to identifier pattern
                    self.start_node(SyntaxKind::IDENT_PATTERN);
                    self.bump();
                    self.finish_node();
                }
            }
            Some(SyntaxKind::INT_LITERAL) | Some(SyntaxKind::BIN_LITERAL) |
            Some(SyntaxKind::HEX_LITERAL) | Some(SyntaxKind::STRING_LITERAL) => {
                // Literal pattern
                self.start_node(SyntaxKind::LITERAL_PATTERN);
                self.bump(); // consume the literal token
                self.finish_node();
            }
            Some(SyntaxKind::L_PAREN) => {
                // Tuple pattern
                self.start_node(SyntaxKind::TUPLE_PATTERN);
                self.bump(); // (

                // Parse comma-separated patterns
                if !self.at(SyntaxKind::R_PAREN) {
                    self.parse_pattern();
                    while self.at(SyntaxKind::COMMA) {
                        self.bump();
                        if !self.at(SyntaxKind::R_PAREN) {
                            self.parse_pattern();
                        }
                    }
                }

                self.expect(SyntaxKind::R_PAREN);
                self.finish_node();
            }
            _ => {
                // Error recovery - treat as wildcard
                self.start_node(SyntaxKind::WILDCARD_PATTERN);
                self.error("Expected pattern");
                self.finish_node();
            }
        }
    }

    /// Parse flow statement
    fn parse_flow_statement(&mut self) {
        self.start_node(SyntaxKind::FLOW_STMT);

        self.expect(SyntaxKind::FLOW_KW);
        self.expect(SyntaxKind::L_BRACE);

        // Parse the pipeline
        self.parse_flow_pipeline();

        self.expect(SyntaxKind::R_BRACE);

        self.finish_node();
    }

    /// Parse flow pipeline with |> operators
    fn parse_flow_pipeline(&mut self) {
        self.start_node(SyntaxKind::FLOW_PIPELINE);

        // Parse the first stage
        self.parse_pipeline_stage();

        // Parse subsequent stages connected by |>
        while self.at(SyntaxKind::PIPELINE) {
            self.bump(); // consume |>
            self.parse_pipeline_stage();
        }

        self.finish_node();
    }

    /// Parse a single pipeline stage
    fn parse_pipeline_stage(&mut self) {
        self.start_node(SyntaxKind::PIPELINE_STAGE);

        if self.at(SyntaxKind::L_BRACE) {
            // Block stage
            self.parse_block_statement();
        } else {
            // Expression stage
            self.parse_expression();
        }

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

    /// Parse intent declaration
    fn parse_intent_decl(&mut self) {
        self.start_node(SyntaxKind::INTENT_DECL);
        self.expect(SyntaxKind::INTENT_KW);

        // Intent name
        self.expect(SyntaxKind::IDENT);

        // Optional for clause (intent MyIntent for EntityName)
        if self.at(SyntaxKind::FOR_KW) {
            self.bump();
            self.expect(SyntaxKind::IDENT);
        }

        self.expect(SyntaxKind::L_BRACE);

        // Parse intent constraints
        self.parse_intent_constraints();

        self.expect(SyntaxKind::R_BRACE);
        self.finish_node();
    }

    /// Parse intent constraints
    fn parse_intent_constraints(&mut self) {
        self.start_node(SyntaxKind::INTENT_CONSTRAINT_LIST);

        while !self.at(SyntaxKind::R_BRACE) && !self.is_at_end() {
            self.skip_trivia();

            if self.at(SyntaxKind::R_BRACE) {
                break;
            }

            // Parse constraint keyword (timing, power, area, throughput, latency)
            if self.current_kind() == Some(SyntaxKind::IDENT) {
                self.parse_intent_constraint();
            } else {
                self.error_and_bump("expected intent constraint");
            }
        }

        self.finish_node();
    }

    /// Parse single intent constraint
    fn parse_intent_constraint(&mut self) {
        self.start_node(SyntaxKind::INTENT_CONSTRAINT);

        // Constraint type (timing, power, area, etc.)
        self.expect(SyntaxKind::IDENT);

        // Colon
        self.expect(SyntaxKind::COLON);

        // Constraint expression
        self.parse_constraint_expression();

        self.finish_node();
    }

    /// Parse constraint expression
    fn parse_constraint_expression(&mut self) {
        // For now, parse as a simple expression
        // Could be: "< 100MHz", "minimize", "< 1000 LUTs", etc.
        self.parse_expression();

        // Optional units or additional info
        while self.current_kind() == Some(SyntaxKind::IDENT) {
            self.bump();
        }
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

    /// Parse trait definition
    fn parse_trait_def(&mut self) {
        self.start_node(SyntaxKind::TRAIT_DEF);

        // 'trait' keyword
        self.expect(SyntaxKind::TRAIT_KW);

        // Trait name
        self.expect(SyntaxKind::IDENT);

        // Optional generic parameters
        if self.at(SyntaxKind::LT) {
            self.parse_generic_params();
        }

        // Optional super traits
        if self.at(SyntaxKind::COLON) {
            self.bump();
            self.parse_trait_bound_list();
        }

        // Optional where clause
        if self.at(SyntaxKind::WHERE_KW) {
            self.parse_where_clause();
        }

        // Trait body
        self.expect(SyntaxKind::L_BRACE);
        self.parse_trait_body();
        self.expect(SyntaxKind::R_BRACE);

        self.finish_node();
    }

    /// Parse trait body
    fn parse_trait_body(&mut self) {
        self.start_node(SyntaxKind::TRAIT_ITEM_LIST);

        while !self.at(SyntaxKind::R_BRACE) && !self.is_at_end() {
            self.skip_trivia();

            match self.current_kind() {
                Some(SyntaxKind::TYPE_KW) => self.parse_trait_type(),
                Some(SyntaxKind::CONST_KW) => self.parse_trait_const(),
                Some(SyntaxKind::IDENT) => self.parse_trait_method(),
                Some(SyntaxKind::R_BRACE) => break,
                _ => {
                    self.error_and_bump("expected trait item");
                }
            }
        }

        self.finish_node();
    }

    /// Parse trait associated type
    fn parse_trait_type(&mut self) {
        self.start_node(SyntaxKind::TRAIT_TYPE);

        self.expect(SyntaxKind::TYPE_KW);
        self.expect(SyntaxKind::IDENT);

        // Optional bounds
        if self.at(SyntaxKind::COLON) {
            self.bump();
            self.parse_trait_bound_list();
        }

        // Optional default type
        if self.at(SyntaxKind::ASSIGN) {
            self.bump();
            self.parse_type();
        }

        self.expect(SyntaxKind::SEMICOLON);
        self.finish_node();
    }

    /// Parse trait associated constant
    fn parse_trait_const(&mut self) {
        self.start_node(SyntaxKind::TRAIT_CONST);

        self.expect(SyntaxKind::CONST_KW);
        self.expect(SyntaxKind::IDENT);
        self.expect(SyntaxKind::COLON);
        self.parse_type();

        // Optional default value
        if self.at(SyntaxKind::ASSIGN) {
            self.bump();
            self.parse_expression();
        }

        self.expect(SyntaxKind::SEMICOLON);
        self.finish_node();
    }

    /// Parse trait method
    fn parse_trait_method(&mut self) {
        self.start_node(SyntaxKind::TRAIT_METHOD);

        // Method name
        self.expect(SyntaxKind::IDENT);

        // Parameters
        self.expect(SyntaxKind::L_PAREN);
        // TODO: Parse parameter list
        self.expect(SyntaxKind::R_PAREN);

        // Optional return type
        if self.at(SyntaxKind::ARROW) {
            self.bump();
            self.parse_type();
        }

        // Default implementation or semicolon
        if self.at(SyntaxKind::L_BRACE) {
            self.parse_block_statement();
        } else {
            self.expect(SyntaxKind::SEMICOLON);
        }

        self.finish_node();
    }

    /// Parse trait bounds list
    fn parse_trait_bound_list(&mut self) {
        self.start_node(SyntaxKind::TRAIT_BOUND_LIST);

        self.parse_trait_bound();

        while self.at(SyntaxKind::PLUS) {
            self.bump();
            self.parse_trait_bound();
        }

        self.finish_node();
    }

    /// Parse single trait bound
    fn parse_trait_bound(&mut self) {
        self.start_node(SyntaxKind::TRAIT_BOUND);
        self.expect(SyntaxKind::IDENT);

        // Optional generic arguments
        if self.at(SyntaxKind::LT) {
            self.parse_generic_args();
        }

        self.finish_node();
    }

    /// Parse where clause
    fn parse_where_clause(&mut self) {
        self.start_node(SyntaxKind::WHERE_CLAUSE);

        self.expect(SyntaxKind::WHERE_KW);

        self.parse_where_predicate();

        while self.at(SyntaxKind::COMMA) {
            self.bump();
            self.parse_where_predicate();
        }

        self.finish_node();
    }

    /// Parse where predicate
    fn parse_where_predicate(&mut self) {
        self.start_node(SyntaxKind::WHERE_PREDICATE);

        // Type parameter
        self.expect(SyntaxKind::IDENT);

        // Colon and bounds
        self.expect(SyntaxKind::COLON);
        self.parse_trait_bound_list();

        self.finish_node();
    }

    /// Parse trait implementation body
    fn parse_trait_impl_body(&mut self) {
        while !self.at(SyntaxKind::R_BRACE) && !self.is_at_end() {
            self.skip_trivia();

            match self.current_kind() {
                Some(SyntaxKind::TYPE_KW) => self.parse_trait_impl_type(),
                Some(SyntaxKind::CONST_KW) => self.parse_trait_impl_const(),
                Some(SyntaxKind::IDENT) => self.parse_trait_impl_method(),
                Some(SyntaxKind::R_BRACE) => break,
                _ => {
                    self.error_and_bump("expected trait implementation item");
                }
            }
        }
    }

    /// Parse trait implementation type
    fn parse_trait_impl_type(&mut self) {
        self.start_node(SyntaxKind::TRAIT_TYPE);

        self.expect(SyntaxKind::TYPE_KW);
        self.expect(SyntaxKind::IDENT);
        self.expect(SyntaxKind::ASSIGN);
        self.parse_type();
        self.expect(SyntaxKind::SEMICOLON);

        self.finish_node();
    }

    /// Parse trait implementation const
    fn parse_trait_impl_const(&mut self) {
        self.start_node(SyntaxKind::TRAIT_CONST);

        self.expect(SyntaxKind::CONST_KW);
        self.expect(SyntaxKind::IDENT);
        self.expect(SyntaxKind::COLON);
        self.parse_type();
        self.expect(SyntaxKind::ASSIGN);
        self.parse_expression();
        self.expect(SyntaxKind::SEMICOLON);

        self.finish_node();
    }

    /// Parse trait implementation method
    fn parse_trait_impl_method(&mut self) {
        self.start_node(SyntaxKind::TRAIT_METHOD);

        // Method name
        self.expect(SyntaxKind::IDENT);

        // Parameters
        self.expect(SyntaxKind::L_PAREN);
        // TODO: Parse parameter list properly
        self.expect(SyntaxKind::R_PAREN);

        // Optional return type
        if self.at(SyntaxKind::ARROW) {
            self.bump();
            self.parse_type();
        }

        // Method body
        self.parse_block_statement();

        self.finish_node();
    }

    /// Parse generic arguments
    fn parse_generic_args(&mut self) {
        self.start_node(SyntaxKind::ARG_LIST);
        self.expect(SyntaxKind::LT);

        if !self.at(SyntaxKind::GT) {
            self.parse_type_or_const_arg();

            while self.at(SyntaxKind::COMMA) {
                self.bump(); // consume comma
                self.parse_type_or_const_arg();
            }
        }

        self.expect(SyntaxKind::GT);
        self.finish_node();
    }

    /// Parse a type or const argument
    fn parse_type_or_const_arg(&mut self) {
        self.start_node(SyntaxKind::ARG);

        // Check if it's a literal (const argument)
        if self.current_kind().map_or(false, |k| k.is_literal()) {
            self.parse_expression();
        }
        // Otherwise parse as type
        else {
            self.parse_type();
        }

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

        if !self.at(SyntaxKind::GT) {
            self.parse_generic_param();

            while self.at(SyntaxKind::COMMA) {
                self.bump(); // consume comma
                self.parse_generic_param();
            }
        }

        self.expect(SyntaxKind::GT);
        self.finish_node();
    }

    /// Parse a single generic parameter
    fn parse_generic_param(&mut self) {
        self.start_node(SyntaxKind::GENERIC_PARAM);

        // Check if it's a const parameter (const N: nat[32])
        if self.at(SyntaxKind::CONST_KW) {
            self.bump(); // consume 'const'
            self.expect(SyntaxKind::IDENT); // parameter name
            self.expect(SyntaxKind::COLON);
            self.parse_type(); // parameter type
        }
        // Check if it's a type parameter with bounds (T: Trait)
        else if self.current_kind() == Some(SyntaxKind::IDENT) {
            self.bump(); // consume identifier

            // Optional type bounds
            if self.at(SyntaxKind::COLON) {
                self.bump();
                self.parse_trait_bound_list();
            }
        } else {
            self.error("expected generic parameter");
        }

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

    /// Get current token text
    fn current_text(&self) -> Option<&str> {
        self.current_token()
            .map(|token| &self.source[token.span.clone()])
    }

    /// Check if at end of input
    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    /// Peek at a token without consuming it
    fn peek_kind(&self, offset: usize) -> Option<SyntaxKind> {
        let index = self.current + offset;
        if index < self.tokens.len() {
            Some(SyntaxKind::from(self.tokens[index].token.clone()))
        } else {
            None
        }
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