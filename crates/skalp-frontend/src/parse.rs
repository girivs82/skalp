//! Rowan-based parser for SKALP
//!
//! This module implements the actual parsing logic using Rowan's GreenNodeBuilder

use crate::lexer::{Lexer, TokenWithPos};
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
        self.start_node(SyntaxKind::SourceFile);

        // Parse all top-level items
        while !self.is_at_end() {
            // Skip whitespace and comments at top level
            self.skip_trivia();

            if self.is_at_end() {
                break;
            }

            // Parse top-level items
            match self.current_kind() {
                Some(SyntaxKind::EntityKw) => self.parse_entity_decl(),
                Some(SyntaxKind::ImplKw) => self.parse_impl_block(),
                Some(SyntaxKind::ProtocolKw) => self.parse_protocol_decl(),
                Some(SyntaxKind::IntentKw) => self.parse_intent_decl(),
                Some(SyntaxKind::RequirementKw) => self.parse_requirement_decl(),
                Some(SyntaxKind::TraitKw) => self.parse_trait_def(),
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
        self.start_node(SyntaxKind::EntityDecl);

        // 'entity' keyword
        self.expect(SyntaxKind::EntityKw);

        // Entity name
        self.expect(SyntaxKind::Ident);

        // Optional generic parameters
        if self.at(SyntaxKind::Lt) {
            self.parse_generic_params();
        }

        // Port list
        self.expect(SyntaxKind::LBrace);
        self.parse_port_list();
        self.expect(SyntaxKind::RBrace);

        self.finish_node();
    }

    /// Parse implementation block
    fn parse_impl_block(&mut self) {
        // Start the node first
        self.start_node(SyntaxKind::ImplBlock);

        // 'impl' keyword
        self.expect(SyntaxKind::ImplKw);

        // Look ahead to determine if this is a trait impl
        // After consuming 'impl', position 0 is the next token
        // Check if we have: <trait> for <type>
        let is_trait_impl = {
            let mut found_for = false;
            let mut lookahead = 1;

            // Look for 'for' keyword within the next few tokens
            // Pattern: IDENT [<generics>] FOR IDENT
            if self.peek_kind(0) == Some(SyntaxKind::Ident) {
                // Skip generic parameters if present
                if self.peek_kind(lookahead) == Some(SyntaxKind::Lt) {
                    // Simple skip - just look for 'for' keyword
                    while lookahead < 10 && self.peek_kind(lookahead).is_some() {
                        if self.peek_kind(lookahead) == Some(SyntaxKind::ForKw) {
                            found_for = true;
                            break;
                        }
                        lookahead += 1;
                    }
                } else if self.peek_kind(lookahead) == Some(SyntaxKind::ForKw) {
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
        self.start_node(SyntaxKind::ImplBlock);

        // Entity name
        self.expect(SyntaxKind::Ident);

        // Optional generic parameters
        if self.at(SyntaxKind::Lt) {
            self.parse_generic_params();
        }

        // Implementation body
        self.expect(SyntaxKind::LBrace);
        self.parse_impl_body();
        self.expect(SyntaxKind::RBrace);

        self.finish_node();
    }

    /// Parse trait implementation after 'impl' keyword
    fn parse_trait_impl_after_keyword(&mut self) {
        self.start_node(SyntaxKind::TraitImpl);

        // Trait name
        self.expect(SyntaxKind::Ident);

        // Optional generic parameters for trait
        if self.at(SyntaxKind::Lt) {
            self.parse_generic_params();
        }

        // 'for' keyword
        self.expect(SyntaxKind::ForKw);

        // Target type
        self.expect(SyntaxKind::Ident);

        // Optional generic parameters for target
        if self.at(SyntaxKind::Lt) {
            self.parse_generic_params();
        }

        // Optional where clause
        if self.at(SyntaxKind::WhereKw) {
            self.parse_where_clause();
        }

        // Implementation body
        self.expect(SyntaxKind::LBrace);
        self.parse_trait_impl_body();
        self.expect(SyntaxKind::RBrace);

        self.finish_node();
    }

    /// Parse implementation body
    fn parse_impl_body(&mut self) {
        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            match self.current_kind() {
                Some(SyntaxKind::SignalKw) => self.parse_signal_decl(),
                Some(SyntaxKind::VarKw) => self.parse_variable_decl(),
                Some(SyntaxKind::ConstKw) => self.parse_constant_decl(),
                Some(SyntaxKind::OnKw) => self.parse_event_block(),
                Some(SyntaxKind::MatchKw) => self.parse_match_statement(),
                Some(SyntaxKind::FlowKw) => self.parse_flow_statement(),
                Some(SyntaxKind::Ident) => {
                    // Could be an assignment or start of another construct
                    self.parse_assignment_or_statement();
                }
                Some(SyntaxKind::RBrace) => break,
                _ => {
                    self.error_and_bump("expected implementation item");
                }
            }
        }
    }

    /// Parse port list
    fn parse_port_list(&mut self) {
        self.start_node(SyntaxKind::PortList);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            if self.at_port_direction() {
                self.parse_port_decl();
            } else if self.at(SyntaxKind::RBrace) {
                break;
            } else {
                self.error_and_bump("expected port declaration");
            }
        }

        self.finish_node();
    }

    /// Parse port declaration
    fn parse_port_decl(&mut self) {
        self.start_node(SyntaxKind::PortDecl);

        // Port direction
        self.start_node(SyntaxKind::PortDirection);
        if self.at(SyntaxKind::InKw) {
            self.bump();
        } else if self.at(SyntaxKind::OutKw) {
            self.bump();
        } else if self.at(SyntaxKind::InoutKw) {
            self.bump();
        }
        self.finish_node();

        // Port name
        self.expect(SyntaxKind::Ident);

        // Colon and type
        self.expect(SyntaxKind::Colon);
        self.parse_type();

        self.finish_node();
    }

    /// Parse signal declaration
    fn parse_signal_decl(&mut self) {
        self.start_node(SyntaxKind::SignalDecl);

        self.expect(SyntaxKind::SignalKw);
        self.expect(SyntaxKind::Ident);
        self.expect(SyntaxKind::Colon);
        self.parse_type();

        // Optional initial value
        if self.at(SyntaxKind::Assign) {
            self.bump();
            self.parse_expression();
        }

        self.finish_node();
    }

    /// Parse variable declaration
    fn parse_variable_decl(&mut self) {
        self.start_node(SyntaxKind::VariableDecl);

        self.expect(SyntaxKind::VarKw);
        self.expect(SyntaxKind::Ident);
        self.expect(SyntaxKind::Colon);
        self.parse_type();

        // Optional initial value
        if self.at(SyntaxKind::Assign) {
            self.bump();
            self.parse_expression();
        }

        self.finish_node();
    }

    /// Parse constant declaration
    fn parse_constant_decl(&mut self) {
        self.start_node(SyntaxKind::ConstantDecl);

        self.expect(SyntaxKind::ConstKw);
        self.expect(SyntaxKind::Ident);
        self.expect(SyntaxKind::Colon);
        self.parse_type();
        self.expect(SyntaxKind::Assign);
        self.parse_expression();

        self.finish_node();
    }

    /// Parse event block (on clause)
    fn parse_event_block(&mut self) {
        self.start_node(SyntaxKind::EventBlock);

        self.expect(SyntaxKind::OnKw);
        self.expect(SyntaxKind::LParen);

        // Parse event triggers
        self.start_node(SyntaxKind::EventTriggerList);
        self.parse_event_trigger();

        while self.at(SyntaxKind::Pipe) {
            self.bump();
            self.parse_event_trigger();
        }
        self.finish_node();

        self.expect(SyntaxKind::RParen);

        // Parse event body
        self.parse_block_statement();

        self.finish_node();
    }

    /// Parse event trigger (e.g., clk.rise)
    fn parse_event_trigger(&mut self) {
        self.start_node(SyntaxKind::EventTrigger);

        // Signal name
        self.expect(SyntaxKind::Ident);

        // Dot and edge type
        if self.at(SyntaxKind::Dot) {
            self.bump();

            self.start_node(SyntaxKind::EdgeType);
            if self.at(SyntaxKind::RiseKw) || self.at(SyntaxKind::FallKw) || self.at(SyntaxKind::EdgeKw) {
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
        self.start_node(SyntaxKind::AssignmentStmt);

        // Left-hand side (identifier or field access)
        self.parse_expression();

        // Assignment operator
        if self.at(SyntaxKind::NonBlockingAssign) ||
           self.at(SyntaxKind::BlockingAssign) ||
           self.at(SyntaxKind::Assign) {
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
        self.start_node(SyntaxKind::BlockStmt);

        self.expect(SyntaxKind::LBrace);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            match self.current_kind() {
                Some(SyntaxKind::IfKw) => self.parse_if_statement(),
                Some(SyntaxKind::MatchKw) => self.parse_match_statement(),
                Some(SyntaxKind::FlowKw) => self.parse_flow_statement(),
                Some(SyntaxKind::Ident) => self.parse_assignment_or_statement(),
                Some(SyntaxKind::LBrace) => self.parse_block_statement(),
                Some(SyntaxKind::RBrace) => break,
                _ => {
                    self.error_and_bump("expected statement");
                }
            }
        }

        self.expect(SyntaxKind::RBrace);

        self.finish_node();
    }

    /// Parse if statement
    fn parse_if_statement(&mut self) {
        self.start_node(SyntaxKind::IfStmt);

        self.expect(SyntaxKind::IfKw);
        self.expect(SyntaxKind::LParen);
        self.parse_expression();
        self.expect(SyntaxKind::RParen);

        self.parse_block_statement();

        if self.at(SyntaxKind::ElseKw) {
            self.bump();
            self.parse_block_statement();
        }

        self.finish_node();
    }

    /// Parse match statement
    fn parse_match_statement(&mut self) {
        self.start_node(SyntaxKind::MatchStmt);

        self.expect(SyntaxKind::MatchKw);
        self.parse_expression();
        self.expect(SyntaxKind::LBrace);

        // Parse match arms
        self.start_node(SyntaxKind::MatchArmList);
        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.parse_match_arm();
        }
        self.finish_node();

        self.expect(SyntaxKind::RBrace);

        self.finish_node();
    }

    /// Parse a single match arm
    fn parse_match_arm(&mut self) {
        self.start_node(SyntaxKind::MatchArm);

        // Parse pattern
        self.parse_pattern();

        // Expect arrow (=>)
        self.expect(SyntaxKind::Arrow);

        // Parse arm body (statement or block)
        if self.at(SyntaxKind::LBrace) {
            self.parse_block_statement();
        } else {
            self.parse_assignment_or_statement();
        }

        // Optional comma
        if self.at(SyntaxKind::Comma) {
            self.bump();
        }

        self.finish_node();
    }

    /// Parse a pattern
    fn parse_pattern(&mut self) {
        match self.current_kind() {
            Some(SyntaxKind::Ident) => {
                // Check if it's a wildcard pattern (_) or identifier pattern
                if let Some(text) = self.current_text() {
                    if text == "_" {
                        // Wildcard pattern
                        self.start_node(SyntaxKind::WildcardPattern);
                        self.bump();
                        self.finish_node();
                    } else {
                        // Identifier pattern
                        self.start_node(SyntaxKind::IdentPattern);
                        self.bump();
                        self.finish_node();
                    }
                } else {
                    // Fallback to identifier pattern
                    self.start_node(SyntaxKind::IdentPattern);
                    self.bump();
                    self.finish_node();
                }
            }
            Some(SyntaxKind::IntLiteral) | Some(SyntaxKind::BinLiteral) |
            Some(SyntaxKind::HexLiteral) | Some(SyntaxKind::StringLiteral) => {
                // Literal pattern
                self.start_node(SyntaxKind::LiteralPattern);
                self.bump(); // consume the literal token
                self.finish_node();
            }
            Some(SyntaxKind::LParen) => {
                // Tuple pattern
                self.start_node(SyntaxKind::TuplePattern);
                self.bump(); // (

                // Parse comma-separated patterns
                if !self.at(SyntaxKind::RParen) {
                    self.parse_pattern();
                    while self.at(SyntaxKind::Comma) {
                        self.bump();
                        if !self.at(SyntaxKind::RParen) {
                            self.parse_pattern();
                        }
                    }
                }

                self.expect(SyntaxKind::RParen);
                self.finish_node();
            }
            _ => {
                // Error recovery - treat as wildcard
                self.start_node(SyntaxKind::WildcardPattern);
                self.error("Expected pattern");
                self.finish_node();
            }
        }
    }

    /// Parse flow statement
    fn parse_flow_statement(&mut self) {
        self.start_node(SyntaxKind::FlowStmt);

        self.expect(SyntaxKind::FlowKw);
        self.expect(SyntaxKind::LBrace);

        // Parse the pipeline
        self.parse_flow_pipeline();

        self.expect(SyntaxKind::RBrace);

        self.finish_node();
    }

    /// Parse flow pipeline with |> operators
    fn parse_flow_pipeline(&mut self) {
        self.start_node(SyntaxKind::FlowPipeline);

        // Parse the first stage
        self.parse_pipeline_stage();

        // Parse subsequent stages connected by |>
        while self.at(SyntaxKind::Pipeline) {
            self.bump(); // consume |>
            self.parse_pipeline_stage();
        }

        self.finish_node();
    }

    /// Parse a single pipeline stage
    fn parse_pipeline_stage(&mut self) {
        self.start_node(SyntaxKind::PipelineStage);

        if self.at(SyntaxKind::LBrace) {
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
        self.start_node(SyntaxKind::ProtocolDecl);
        self.expect(SyntaxKind::ProtocolKw);
        self.expect(SyntaxKind::Ident);
        self.expect(SyntaxKind::LBrace);
        // TODO: Parse protocol signals
        self.expect(SyntaxKind::RBrace);
        self.finish_node();
    }

    /// Parse intent declaration
    fn parse_intent_decl(&mut self) {
        self.start_node(SyntaxKind::IntentDecl);
        self.expect(SyntaxKind::IntentKw);

        // Intent name
        self.expect(SyntaxKind::Ident);

        // Optional for clause (intent MyIntent for EntityName)
        if self.at(SyntaxKind::ForKw) {
            self.bump();
            self.expect(SyntaxKind::Ident);
        }

        self.expect(SyntaxKind::LBrace);

        // Parse intent constraints
        self.parse_intent_constraints();

        self.expect(SyntaxKind::RBrace);
        self.finish_node();
    }

    /// Parse intent constraints
    fn parse_intent_constraints(&mut self) {
        self.start_node(SyntaxKind::IntentConstraintList);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            if self.at(SyntaxKind::RBrace) {
                break;
            }

            // Parse constraint keyword (timing, power, area, throughput, latency)
            if self.current_kind() == Some(SyntaxKind::Ident) {
                self.parse_intent_constraint();
            } else {
                self.error_and_bump("expected intent constraint");
            }
        }

        self.finish_node();
    }

    /// Parse single intent constraint
    fn parse_intent_constraint(&mut self) {
        self.start_node(SyntaxKind::IntentConstraint);

        // Constraint type (timing, power, area, etc.)
        self.expect(SyntaxKind::Ident);

        // Colon
        self.expect(SyntaxKind::Colon);

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
        while self.current_kind() == Some(SyntaxKind::Ident) {
            self.bump();
        }
    }

    /// Parse requirement declaration (stub)
    fn parse_requirement_decl(&mut self) {
        self.start_node(SyntaxKind::RequirementDecl);
        self.expect(SyntaxKind::RequirementKw);
        self.expect(SyntaxKind::Ident);
        self.expect(SyntaxKind::LBrace);
        // TODO: Parse requirement details
        self.expect(SyntaxKind::RBrace);
        self.finish_node();
    }

    /// Parse trait definition
    fn parse_trait_def(&mut self) {
        self.start_node(SyntaxKind::TraitDef);

        // 'trait' keyword
        self.expect(SyntaxKind::TraitKw);

        // Trait name
        self.expect(SyntaxKind::Ident);

        // Optional generic parameters
        if self.at(SyntaxKind::Lt) {
            self.parse_generic_params();
        }

        // Optional super traits
        if self.at(SyntaxKind::Colon) {
            self.bump();
            self.parse_trait_bound_list();
        }

        // Optional where clause
        if self.at(SyntaxKind::WhereKw) {
            self.parse_where_clause();
        }

        // Trait body
        self.expect(SyntaxKind::LBrace);
        self.parse_trait_body();
        self.expect(SyntaxKind::RBrace);

        self.finish_node();
    }

    /// Parse trait body
    fn parse_trait_body(&mut self) {
        self.start_node(SyntaxKind::TraitItemList);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            match self.current_kind() {
                Some(SyntaxKind::TypeKw) => self.parse_trait_type(),
                Some(SyntaxKind::ConstKw) => self.parse_trait_const(),
                Some(SyntaxKind::Ident) => self.parse_trait_method(),
                Some(SyntaxKind::RBrace) => break,
                _ => {
                    self.error_and_bump("expected trait item");
                }
            }
        }

        self.finish_node();
    }

    /// Parse trait associated type
    fn parse_trait_type(&mut self) {
        self.start_node(SyntaxKind::TraitType);

        self.expect(SyntaxKind::TypeKw);
        self.expect(SyntaxKind::Ident);

        // Optional bounds
        if self.at(SyntaxKind::Colon) {
            self.bump();
            self.parse_trait_bound_list();
        }

        // Optional default type
        if self.at(SyntaxKind::Assign) {
            self.bump();
            self.parse_type();
        }

        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    /// Parse trait associated constant
    fn parse_trait_const(&mut self) {
        self.start_node(SyntaxKind::TraitConst);

        self.expect(SyntaxKind::ConstKw);
        self.expect(SyntaxKind::Ident);
        self.expect(SyntaxKind::Colon);
        self.parse_type();

        // Optional default value
        if self.at(SyntaxKind::Assign) {
            self.bump();
            self.parse_expression();
        }

        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    /// Parse trait method
    fn parse_trait_method(&mut self) {
        self.start_node(SyntaxKind::TraitMethod);

        // Method name
        self.expect(SyntaxKind::Ident);

        // Parameters
        self.expect(SyntaxKind::LParen);
        // TODO: Parse parameter list
        self.expect(SyntaxKind::RParen);

        // Optional return type
        if self.at(SyntaxKind::Arrow) {
            self.bump();
            self.parse_type();
        }

        // Default implementation or semicolon
        if self.at(SyntaxKind::LBrace) {
            self.parse_block_statement();
        } else {
            self.expect(SyntaxKind::Semicolon);
        }

        self.finish_node();
    }

    /// Parse trait bounds list
    fn parse_trait_bound_list(&mut self) {
        self.start_node(SyntaxKind::TraitBoundList);

        self.parse_trait_bound();

        while self.at(SyntaxKind::Plus) {
            self.bump();
            self.parse_trait_bound();
        }

        self.finish_node();
    }

    /// Parse single trait bound
    fn parse_trait_bound(&mut self) {
        self.start_node(SyntaxKind::TraitBound);
        self.expect(SyntaxKind::Ident);

        // Optional generic arguments
        if self.at(SyntaxKind::Lt) {
            self.parse_generic_args();
        }

        self.finish_node();
    }

    /// Parse where clause
    fn parse_where_clause(&mut self) {
        self.start_node(SyntaxKind::WhereClause);

        self.expect(SyntaxKind::WhereKw);

        self.parse_where_predicate();

        while self.at(SyntaxKind::Comma) {
            self.bump();
            self.parse_where_predicate();
        }

        self.finish_node();
    }

    /// Parse where predicate
    fn parse_where_predicate(&mut self) {
        self.start_node(SyntaxKind::WherePredicate);

        // Type parameter
        self.expect(SyntaxKind::Ident);

        // Colon and bounds
        self.expect(SyntaxKind::Colon);
        self.parse_trait_bound_list();

        self.finish_node();
    }

    /// Parse trait implementation body
    fn parse_trait_impl_body(&mut self) {
        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            match self.current_kind() {
                Some(SyntaxKind::TypeKw) => self.parse_trait_impl_type(),
                Some(SyntaxKind::ConstKw) => self.parse_trait_impl_const(),
                Some(SyntaxKind::Ident) => self.parse_trait_impl_method(),
                Some(SyntaxKind::RBrace) => break,
                _ => {
                    self.error_and_bump("expected trait implementation item");
                }
            }
        }
    }

    /// Parse trait implementation type
    fn parse_trait_impl_type(&mut self) {
        self.start_node(SyntaxKind::TraitType);

        self.expect(SyntaxKind::TypeKw);
        self.expect(SyntaxKind::Ident);
        self.expect(SyntaxKind::Assign);
        self.parse_type();
        self.expect(SyntaxKind::Semicolon);

        self.finish_node();
    }

    /// Parse trait implementation const
    fn parse_trait_impl_const(&mut self) {
        self.start_node(SyntaxKind::TraitConst);

        self.expect(SyntaxKind::ConstKw);
        self.expect(SyntaxKind::Ident);
        self.expect(SyntaxKind::Colon);
        self.parse_type();
        self.expect(SyntaxKind::Assign);
        self.parse_expression();
        self.expect(SyntaxKind::Semicolon);

        self.finish_node();
    }

    /// Parse trait implementation method
    fn parse_trait_impl_method(&mut self) {
        self.start_node(SyntaxKind::TraitMethod);

        // Method name
        self.expect(SyntaxKind::Ident);

        // Parameters
        self.expect(SyntaxKind::LParen);
        // TODO: Parse parameter list properly
        self.expect(SyntaxKind::RParen);

        // Optional return type
        if self.at(SyntaxKind::Arrow) {
            self.bump();
            self.parse_type();
        }

        // Method body
        self.parse_block_statement();

        self.finish_node();
    }

    /// Parse generic arguments
    fn parse_generic_args(&mut self) {
        self.start_node(SyntaxKind::ArgList);
        self.expect(SyntaxKind::Lt);

        if !self.at(SyntaxKind::Gt) {
            self.parse_type_or_const_arg();

            while self.at(SyntaxKind::Comma) {
                self.bump(); // consume comma
                self.parse_type_or_const_arg();
            }
        }

        self.expect(SyntaxKind::Gt);
        self.finish_node();
    }

    /// Parse a type or const argument
    fn parse_type_or_const_arg(&mut self) {
        self.start_node(SyntaxKind::Arg);

        // Check if it's a literal (const argument)
        if self.current_kind().is_some_and(|k| k.is_literal()) {
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
        self.start_node(SyntaxKind::TypeAnnotation);

        match self.current_kind() {
            Some(SyntaxKind::BitKw) => {
                self.start_node(SyntaxKind::BitType);
                self.bump();
                self.parse_width_spec();
                self.finish_node();
            }
            Some(SyntaxKind::LogicKw) => {
                self.start_node(SyntaxKind::LogicType);
                self.bump();
                self.parse_width_spec();
                self.finish_node();
            }
            Some(SyntaxKind::IntKw) => {
                self.start_node(SyntaxKind::IntType);
                self.bump();
                self.parse_width_spec();
                self.finish_node();
            }
            Some(SyntaxKind::NatKw) => {
                self.start_node(SyntaxKind::NatType);
                self.bump();
                self.parse_width_spec();
                self.finish_node();
            }
            Some(SyntaxKind::ClockKw) => {
                self.start_node(SyntaxKind::ClockType);
                self.bump();
                self.finish_node();
            }
            Some(SyntaxKind::ResetKw) => {
                self.start_node(SyntaxKind::ResetType);
                self.bump();
                self.finish_node();
            }
            Some(SyntaxKind::Ident) => {
                self.start_node(SyntaxKind::CustomType);
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
        if self.at(SyntaxKind::LBracket) {
            self.start_node(SyntaxKind::WidthSpec);
            self.bump();
            self.expect(SyntaxKind::IntLiteral);
            self.expect(SyntaxKind::RBracket);
            self.finish_node();
        }
    }

    /// Parse generic parameters (stub)
    fn parse_generic_params(&mut self) {
        self.start_node(SyntaxKind::GenericParamList);
        self.expect(SyntaxKind::Lt);

        if !self.at(SyntaxKind::Gt) {
            self.parse_generic_param();

            while self.at(SyntaxKind::Comma) {
                self.bump(); // consume comma
                self.parse_generic_param();
            }
        }

        self.expect(SyntaxKind::Gt);
        self.finish_node();
    }

    /// Parse a single generic parameter
    fn parse_generic_param(&mut self) {
        self.start_node(SyntaxKind::GenericParam);

        // Check if it's a const parameter (const N: nat[32])
        if self.at(SyntaxKind::ConstKw) {
            self.bump(); // consume 'const'
            self.expect(SyntaxKind::Ident); // parameter name
            self.expect(SyntaxKind::Colon);
            self.parse_type(); // parameter type
        }
        // Check if it's a type parameter with bounds (T: Trait)
        else if self.current_kind() == Some(SyntaxKind::Ident) {
            self.bump(); // consume identifier

            // Optional type bounds
            if self.at(SyntaxKind::Colon) {
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
            self.start_node(SyntaxKind::BinaryExpr);

            // The left operand is already parsed
            self.bump(); // Consume operator
            self.parse_primary_expression(); // Parse right operand

            self.finish_node();
        }
    }

    /// Parse primary expression
    fn parse_primary_expression(&mut self) {
        match self.current_kind() {
            Some(SyntaxKind::IntLiteral | SyntaxKind::BinLiteral |
                 SyntaxKind::HexLiteral | SyntaxKind::StringLiteral) => {
                self.start_node(SyntaxKind::LiteralExpr);
                self.bump();
                self.finish_node();
            }
            Some(SyntaxKind::Ident) => {
                self.start_node(SyntaxKind::IdentExpr);
                self.bump();

                // Check for field access or method call
                while self.at(SyntaxKind::Dot) {
                    self.start_node(SyntaxKind::FieldExpr);
                    self.bump();
                    self.expect(SyntaxKind::Ident);
                    self.finish_node();
                }

                self.finish_node();
            }
            Some(SyntaxKind::LParen) => {
                self.start_node(SyntaxKind::ParenExpr);
                self.bump();
                self.parse_expression();
                self.expect(SyntaxKind::RParen);
                self.finish_node();
            }
            Some(SyntaxKind::Bang | SyntaxKind::Tilde | SyntaxKind::Minus) => {
                self.start_node(SyntaxKind::UnaryExpr);
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
            Some(SyntaxKind::InKw | SyntaxKind::OutKw | SyntaxKind::InoutKw)
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
        assert_eq!(tree.kind(), SyntaxKind::SourceFile);
    }

    #[test]
    fn test_parse_entity() {
        let source = "entity Counter { in clk: clock out count: nat[8] }";
        let tree = parse(source);
        assert_eq!(tree.kind(), SyntaxKind::SourceFile);

        let entity = tree.first_child().unwrap();
        assert_eq!(entity.kind(), SyntaxKind::EntityDecl);
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
        assert_eq!(tree.kind(), SyntaxKind::SourceFile);

        let impl_block = tree.first_child().unwrap();
        assert_eq!(impl_block.kind(), SyntaxKind::ImplBlock);
    }
}