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
    /// Collected parse errors
    errors: Vec<ParseError>,
    /// Track if we've partially consumed a >> token
    partial_shr: bool,
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
            errors: Vec::new(),
            partial_shr: false,
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
                Some(SyntaxKind::StructKw) => self.parse_struct_decl(),
                Some(SyntaxKind::EnumKw) => self.parse_enum_decl(),
                Some(SyntaxKind::UnionKw) => self.parse_union_decl(),
                _ => {
                    // Unknown item - consume token as error and continue
                    self.error_and_bump("expected top-level item");
                }
            }
        }

        self.finish_node();

        ParseResult {
            green_node: self.builder.finish(),
            errors: self.errors,
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
                Some(SyntaxKind::LetKw) => self.parse_instance_decl(),
                Some(SyntaxKind::OnKw) => self.parse_event_block(),
                Some(SyntaxKind::MatchKw) => self.parse_match_statement(),
                Some(SyntaxKind::FlowKw) => self.parse_flow_statement(),
                Some(SyntaxKind::AssignKw) => self.parse_continuous_assignment(),
                Some(SyntaxKind::CovergroupKw) => self.parse_covergroup_decl(),
                Some(SyntaxKind::FormalKw) => self.parse_formal_block(),
                Some(SyntaxKind::InvariantKw) => self.parse_invariant_decl(),
                Some(SyntaxKind::SafetyKw) => self.parse_safety_property(),
                Some(SyntaxKind::LivenessKw) => self.parse_liveness_property(),
                Some(SyntaxKind::ProveKw) => self.parse_prove_statement(),
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
        if self.at(SyntaxKind::InKw)
            || self.at(SyntaxKind::OutKw)
            || self.at(SyntaxKind::InoutKw)
            || self.at(SyntaxKind::PortKw)
        {
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

    /// Parse instance declaration (let instance = Entity { ... })
    fn parse_instance_decl(&mut self) {
        self.start_node(SyntaxKind::InstanceDecl);

        // 'let' keyword
        self.expect(SyntaxKind::LetKw);

        // Instance name
        self.expect(SyntaxKind::Ident);

        // Optional array index for instance arrays
        if self.at(SyntaxKind::LBracket) {
            self.bump();
            self.parse_expression();
            self.expect(SyntaxKind::RBracket);
        }

        // '=' sign
        self.expect(SyntaxKind::Assign);

        // Entity name
        self.expect(SyntaxKind::Ident);

        // Optional generic parameters
        if self.at(SyntaxKind::Lt) {
            self.parse_generic_params();
        }

        // Connection list
        self.expect(SyntaxKind::LBrace);
        self.parse_connection_list();
        self.expect(SyntaxKind::RBrace);

        self.finish_node();
    }

    /// Parse connection list for instance
    fn parse_connection_list(&mut self) {
        self.start_node(SyntaxKind::ConnectionList);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            if self.at(SyntaxKind::RBrace) {
                break;
            }

            // Parse connection: port_name: signal_name
            self.start_node(SyntaxKind::Connection);
            self.expect(SyntaxKind::Ident); // port name
            self.expect(SyntaxKind::Colon);
            self.parse_expression(); // signal expression
            self.finish_node();

            // Optional comma
            if self.at(SyntaxKind::Comma) {
                self.bump();
            } else if !self.at(SyntaxKind::RBrace) {
                break;
            }
        }

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
            if self.at(SyntaxKind::RiseKw) || self.at(SyntaxKind::FallKw) {
                self.bump();
            } else {
                self.error("expected 'rise' or 'fall'");
            }
            self.finish_node();
        }

        self.finish_node();
    }

    /// Parse assignment or other statement starting with identifier
    fn parse_assignment_or_statement(&mut self) {
        // Check if this is a return statement
        if self.at(SyntaxKind::ReturnKw) {
            self.parse_return_statement();
        } else {
            // For now, just parse as assignment
            // In the future, we could look ahead to determine the statement type
            self.parse_assignment_stmt();
        }
    }

    /// Parse continuous assignment (assign keyword)
    fn parse_continuous_assignment(&mut self) {
        self.start_node(SyntaxKind::AssignmentStmt);

        // Consume 'assign' keyword
        self.expect(SyntaxKind::AssignKw);

        // Left-hand side
        self.parse_expression();

        // Assignment operator (should be =)
        self.expect(SyntaxKind::Assign);

        // Right-hand side
        self.parse_expression();

        // Optional semicolon
        self.consume_semicolon();

        self.finish_node();
    }

    /// Parse assignment statement
    fn parse_assignment_stmt(&mut self) {
        self.start_node(SyntaxKind::AssignmentStmt);

        // Left-hand side (identifier or field access)
        self.parse_expression();

        // Assignment operator
        if self.at(SyntaxKind::NonBlockingAssign)
            || self.at(SyntaxKind::BlockingAssign)
            || self.at(SyntaxKind::Assign)
        {
            self.bump();
        } else {
            self.error("expected assignment operator");
        }

        // Right-hand side
        self.parse_expression();

        // Optional semicolon or newline
        self.consume_semicolon();

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
                Some(SyntaxKind::AssertKw) => self.parse_assert_statement(),
                Some(SyntaxKind::PropertyKw) => self.parse_property_statement(),
                Some(SyntaxKind::CoverKw) => self.parse_cover_statement(),
                Some(SyntaxKind::SequenceKw) => self.parse_sequence_statement(),
                Some(SyntaxKind::AssumeKw) => self.parse_assume_statement(),
                Some(SyntaxKind::ExpectKw) => self.parse_expect_statement(),
                Some(SyntaxKind::ReturnKw) => self.parse_return_statement(),
                Some(SyntaxKind::Ident) => self.parse_assignment_or_statement(),
                Some(SyntaxKind::LBrace) => self.parse_block_statement(),
                Some(SyntaxKind::AssignKw) => {
                    self.error("'assign' statements cannot be inside event blocks");
                    // Skip the assign statement without creating a node
                    self.error_and_bump("skipping misplaced assign statement");
                    break; // Exit the block parsing
                }
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

        // Parentheses are optional in SKALP
        let has_parens = self.at(SyntaxKind::LParen);
        if has_parens {
            self.bump();
        }

        self.parse_expression();

        if has_parens {
            self.expect(SyntaxKind::RParen);
        }

        self.parse_block_statement();

        if self.at(SyntaxKind::ElseKw) {
            self.bump();
            // Check for else-if chain
            if self.at(SyntaxKind::IfKw) {
                self.parse_if_statement();
            } else {
                self.parse_block_statement();
            }
        }

        self.finish_node();
    }

    /// Parse if expression (for use in expressions, not statements)
    fn parse_if_expression(&mut self) {
        self.start_node(SyntaxKind::IfExpr);

        self.expect(SyntaxKind::IfKw);
        // Condition expression (without parens for expression form)
        self.parse_expression();

        // Then block
        self.expect(SyntaxKind::LBrace);
        self.parse_expression();
        self.expect(SyntaxKind::RBrace);

        // Else block (required for expressions to have a value)
        self.expect(SyntaxKind::ElseKw);

        // Check if else is followed by another if (else if chain)
        if self.at(SyntaxKind::IfKw) {
            self.parse_if_expression();
        } else {
            self.expect(SyntaxKind::LBrace);
            self.parse_expression();
            self.expect(SyntaxKind::RBrace);
        }

        self.finish_node();
    }

    /// Parse match statement
    fn parse_match_statement(&mut self) {
        self.start_node(SyntaxKind::MatchStmt);

        self.expect(SyntaxKind::MatchKw);
        self.parse_expression();
        self.expect(SyntaxKind::LBrace);

        // Parse match arms (with statements)
        self.start_node(SyntaxKind::MatchArmList);
        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.parse_match_arm_statement();
        }
        self.finish_node();

        self.expect(SyntaxKind::RBrace);

        self.finish_node();
    }

    /// Parse match expression (for use in expressions, not statements)
    fn parse_match_expression(&mut self) {
        self.start_node(SyntaxKind::MatchExpr);

        self.expect(SyntaxKind::MatchKw);
        self.parse_expression();
        self.expect(SyntaxKind::LBrace);

        // Parse match arms (with expressions)
        self.start_node(SyntaxKind::MatchArmList);
        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.parse_match_arm_expression();
        }
        self.finish_node();

        self.expect(SyntaxKind::RBrace);

        self.finish_node();
    }

    /// Parse a single match arm with statement body (for match statements)
    fn parse_match_arm_statement(&mut self) {
        self.start_node(SyntaxKind::MatchArm);

        // Parse pattern
        self.parse_pattern();

        // Parse optional guard (if expression)
        if self.at(SyntaxKind::IfKw) {
            self.start_node(SyntaxKind::MatchGuard);
            self.bump(); // consume 'if'
            self.parse_expression(); // guard condition
            self.finish_node();
        }

        // Expect arrow (-> or =>)
        if !self.at(SyntaxKind::Arrow) && !self.at(SyntaxKind::FatArrow) {
            self.error("Expected '->' or '=>' after pattern");
        } else {
            self.bump(); // consume arrow
        }

        // Parse arm body (can be statement(s) or block)
        if self.at(SyntaxKind::LBrace) {
            self.parse_block_statement();
        } else {
            // Parse a single statement (assignment, if, match, etc.)
            // We need to check what kind of statement this is
            match self.current_kind() {
                Some(SyntaxKind::Ident) => {
                    // Could be assignment or expression
                    // Look ahead to see if there's an assignment operator
                    let next = self.peek_kind(1);
                    if next == Some(SyntaxKind::Assign)
                        || next == Some(SyntaxKind::NonBlockingAssign)
                        || next == Some(SyntaxKind::BlockingAssign)
                        || next == Some(SyntaxKind::LBracket)
                        || next == Some(SyntaxKind::Dot)
                    {
                        // Parse as assignment statement
                        self.parse_assignment_stmt();
                    } else {
                        // Parse as expression
                        self.parse_expression();
                    }
                }
                Some(SyntaxKind::IfKw) => {
                    self.parse_if_statement();
                }
                Some(SyntaxKind::MatchKw) => {
                    self.parse_match_statement();
                }
                _ => {
                    // Default to expression
                    self.parse_expression();
                }
            }
        }

        // Optional comma
        if self.at(SyntaxKind::Comma) {
            self.bump();
        }

        self.finish_node();
    }

    /// Parse a single match arm with expression body (for match expressions)
    fn parse_match_arm_expression(&mut self) {
        self.start_node(SyntaxKind::MatchArm);

        // Parse pattern
        self.parse_pattern();

        // Parse optional guard (if expression)
        if self.at(SyntaxKind::IfKw) {
            self.start_node(SyntaxKind::MatchGuard);
            self.bump(); // consume 'if'
            self.parse_expression(); // guard condition
            self.finish_node();
        }

        // Expect arrow (-> or =>)
        if !self.at(SyntaxKind::Arrow) && !self.at(SyntaxKind::FatArrow) {
            self.error("Expected '->' or '=>' after pattern");
        } else {
            self.bump(); // consume arrow
        }

        // Parse arm body - must be an expression
        self.parse_expression();

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
                        // Could be identifier pattern or path pattern (Enum::Variant)
                        self.start_node(SyntaxKind::IdentPattern);
                        self.bump(); // consume first identifier

                        // Check for path pattern (::)
                        while self.at(SyntaxKind::ColonColon) {
                            self.bump(); // consume ::
                            self.expect(SyntaxKind::Ident); // consume path segment
                        }

                        self.finish_node();
                    }
                } else {
                    // Fallback to identifier pattern
                    self.start_node(SyntaxKind::IdentPattern);
                    self.bump();
                    self.finish_node();
                }
            }
            Some(SyntaxKind::IntLiteral)
            | Some(SyntaxKind::BinLiteral)
            | Some(SyntaxKind::HexLiteral)
            | Some(SyntaxKind::StringLiteral) => {
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

    /// Parse assert statement
    fn parse_assert_statement(&mut self) {
        self.start_node(SyntaxKind::AssertStmt);

        // 'assert' keyword
        self.expect(SyntaxKind::AssertKw);

        // Expect opening parenthesis
        self.expect(SyntaxKind::LParen);

        // Parse condition expression
        self.parse_expression();

        // Optional comma and message
        if self.at(SyntaxKind::Comma) {
            self.bump(); // consume comma
            self.parse_expression(); // message (usually string literal)
        }

        // Expect closing parenthesis
        self.expect(SyntaxKind::RParen);

        // Optional semicolon
        self.consume_semicolon();

        self.finish_node();
    }

    /// Parse property statement
    fn parse_property_statement(&mut self) {
        self.start_node(SyntaxKind::PropertyStmt);

        // 'property' keyword
        self.expect(SyntaxKind::PropertyKw);

        // Property name (identifier)
        self.expect(SyntaxKind::Ident);

        // Property body
        self.expect(SyntaxKind::LBrace);

        // For now, parse as generic expression until we implement full property syntax
        self.parse_expression();

        self.expect(SyntaxKind::RBrace);

        self.finish_node();
    }

    /// Parse cover statement
    fn parse_cover_statement(&mut self) {
        self.start_node(SyntaxKind::CoverStmt);

        // 'cover' keyword
        self.expect(SyntaxKind::CoverKw);

        // 'property' keyword
        self.expect(SyntaxKind::PropertyKw);

        // Property expression in parentheses
        self.expect(SyntaxKind::LParen);
        self.parse_expression();
        self.expect(SyntaxKind::RParen);

        // Expect semicolon
        self.expect(SyntaxKind::Semicolon);

        self.finish_node();
    }

    /// Parse sequence statement
    fn parse_sequence_statement(&mut self) {
        self.start_node(SyntaxKind::SequenceStmt);

        // 'sequence' keyword
        self.expect(SyntaxKind::SequenceKw);

        // Sequence name (identifier)
        self.expect(SyntaxKind::Ident);

        // Sequence body
        self.expect(SyntaxKind::LBrace);

        // Parse sequence elements
        self.parse_sequence_expression();

        self.expect(SyntaxKind::RBrace);

        self.finish_node();
    }

    /// Parse assume statement
    fn parse_assume_statement(&mut self) {
        self.start_node(SyntaxKind::AssumeStmt);

        // 'assume' keyword
        self.expect(SyntaxKind::AssumeKw);

        // 'property' keyword
        self.expect(SyntaxKind::PropertyKw);

        // Property expression in parentheses
        self.expect(SyntaxKind::LParen);
        self.parse_temporal_expression();
        self.expect(SyntaxKind::RParen);

        // Expect semicolon
        self.expect(SyntaxKind::Semicolon);

        self.finish_node();
    }

    /// Parse expect statement
    fn parse_expect_statement(&mut self) {
        self.start_node(SyntaxKind::ExpectStmt);

        // 'expect' keyword
        self.expect(SyntaxKind::ExpectKw);

        // Property expression in parentheses
        self.expect(SyntaxKind::LParen);
        self.parse_temporal_expression();
        self.expect(SyntaxKind::RParen);

        // Expect semicolon
        self.expect(SyntaxKind::Semicolon);

        self.finish_node();
    }

    /// Parse return statement
    fn parse_return_statement(&mut self) {
        self.start_node(SyntaxKind::ReturnStmt);

        // 'return' keyword
        self.expect(SyntaxKind::ReturnKw);

        // Optional return value expression
        if !self.at(SyntaxKind::Semicolon) && !self.is_at_end() {
            self.parse_expression();
        }

        // Optional semicolon
        self.consume_semicolon();

        self.finish_node();
    }

    /// Parse temporal expression (properties and sequences)
    fn parse_temporal_expression(&mut self) {
        self.start_node(SyntaxKind::TemporalExpr);

        self.parse_temporal_or_expression();

        self.finish_node();
    }

    /// Parse temporal OR expression
    fn parse_temporal_or_expression(&mut self) {
        self.parse_temporal_and_expression();

        while self.at(SyntaxKind::PipePipe) {
            self.bump(); // consume ||
            self.parse_temporal_and_expression();
        }
    }

    /// Parse temporal AND expression
    fn parse_temporal_and_expression(&mut self) {
        self.parse_temporal_implication_expression();

        while self.at(SyntaxKind::AmpAmp) {
            self.bump(); // consume &&
            self.parse_temporal_implication_expression();
        }
    }

    /// Parse temporal implication expression
    fn parse_temporal_implication_expression(&mut self) {
        self.start_node(SyntaxKind::ImplicationExpr);

        self.parse_temporal_primary();

        // Check for implication operators
        if self.at(SyntaxKind::Implies) {
            self.bump(); // consume |->
            self.parse_temporal_primary();
        } else if self.at(SyntaxKind::ImpliesOverlap) {
            self.bump(); // consume |=>
            self.parse_temporal_primary();
        }

        self.finish_node();
    }

    /// Parse temporal primary expression
    fn parse_temporal_primary(&mut self) {
        match self.current_kind() {
            Some(SyntaxKind::AlwaysKw) => {
                self.bump(); // consume 'always'
                self.parse_temporal_primary();
            }
            Some(SyntaxKind::EventuallyKw) => {
                self.bump(); // consume 'eventually'
                self.parse_temporal_primary();
            }
            Some(SyntaxKind::StrongKw) | Some(SyntaxKind::WeakKw) => {
                self.bump(); // consume 'strong' or 'weak'
                self.expect(SyntaxKind::LParen);
                self.parse_temporal_expression();
                self.expect(SyntaxKind::UntilKw);
                self.parse_temporal_expression();
                self.expect(SyntaxKind::RParen);
            }
            Some(SyntaxKind::At) => {
                self.parse_clocking_event();
                self.parse_temporal_primary();
            }
            Some(SyntaxKind::LParen) => {
                self.bump(); // consume (
                self.parse_temporal_expression();
                self.expect(SyntaxKind::RParen);
            }
            _ => {
                // Parse as regular expression or sequence
                self.parse_sequence_expression();
            }
        }
    }

    /// Parse clocking event (@(posedge clk))
    fn parse_clocking_event(&mut self) {
        self.start_node(SyntaxKind::ClockingEvent);

        self.expect(SyntaxKind::At);
        self.expect(SyntaxKind::LParen);

        // Parse edge type (posedge, negedge, or just expression)
        if self.at_keyword("posedge") {
            self.bump(); // consume 'posedge'
        } else if self.at_keyword("negedge") {
            self.bump(); // consume 'negedge'
        }

        // Parse clock expression
        self.parse_expression();

        self.expect(SyntaxKind::RParen);

        self.finish_node();
    }

    /// Parse sequence expression
    fn parse_sequence_expression(&mut self) {
        self.start_node(SyntaxKind::SequenceExpr);

        self.parse_sequence_concatenation();

        self.finish_node();
    }

    /// Parse sequence concatenation
    fn parse_sequence_concatenation(&mut self) {
        self.parse_sequence_primary();

        // Handle repetition operators
        while self.at(SyntaxKind::RepeatOpen)
            || self.at(SyntaxKind::RepeatPlusOpen)
            || self.at(SyntaxKind::RepeatEqualOpen)
        {
            self.parse_repetition();
        }

        // Handle delay operators
        while self.at(SyntaxKind::HashHash) {
            self.parse_delay();
        }
    }

    /// Parse sequence primary
    fn parse_sequence_primary(&mut self) {
        if self.at(SyntaxKind::LParen) {
            self.bump(); // consume (
            self.parse_sequence_expression();
            self.expect(SyntaxKind::RParen);
        } else {
            // Parse as boolean expression
            self.parse_expression();
        }
    }

    /// Parse repetition ([*n], [+n], [=n])
    fn parse_repetition(&mut self) {
        self.start_node(SyntaxKind::RepetitionExpr);

        // Consume opening bracket
        if self.at(SyntaxKind::RepeatOpen) {
            self.bump(); // consume [*
        } else if self.at(SyntaxKind::RepeatPlusOpen) {
            self.bump(); // consume [+
        } else if self.at(SyntaxKind::RepeatEqualOpen) {
            self.bump(); // consume [=
        }

        // Parse count (number or range)
        if self.at(SyntaxKind::IntLiteral) {
            self.bump(); // consume number

            // Check for range (:)
            if self.at(SyntaxKind::Colon) {
                self.bump(); // consume :
                if self.at(SyntaxKind::IntLiteral) {
                    self.bump(); // consume upper bound
                } else if self.at(SyntaxKind::Dollar) {
                    self.bump(); // consume $ (infinity)
                }
            }
        } else if self.at(SyntaxKind::Dollar) {
            self.bump(); // consume $ (infinity)
        }

        // Consume closing bracket
        if self.at(SyntaxKind::RepeatClose) {
            self.bump(); // consume *]
        } else if self.at(SyntaxKind::RepeatPlusClose) {
            self.bump(); // consume +]
        } else if self.at(SyntaxKind::RepeatEqualClose) {
            self.bump(); // consume =]
        }

        self.finish_node();
    }

    /// Parse delay (##n, ##[m:n])
    fn parse_delay(&mut self) {
        self.start_node(SyntaxKind::DelayExpr);

        self.expect(SyntaxKind::HashHash);

        if self.at(SyntaxKind::LBracket) {
            self.bump(); // consume [
            self.expect(SyntaxKind::IntLiteral); // min delay
            if self.at(SyntaxKind::Colon) {
                self.bump(); // consume :
                if self.at(SyntaxKind::IntLiteral) {
                    self.bump(); // consume max delay
                } else if self.at(SyntaxKind::Dollar) {
                    self.bump(); // consume $ (infinity)
                }
            }
            self.expect(SyntaxKind::RBracket);
        } else if self.at(SyntaxKind::IntLiteral) {
            self.bump(); // consume fixed delay
        } else if self.at(SyntaxKind::Dollar) {
            self.bump(); // consume $ (infinity)
        }

        self.finish_node();
    }

    /// Check if current token is a specific keyword
    fn at_keyword(&self, keyword: &str) -> bool {
        if let Some(SyntaxKind::Ident) = self.current_kind() {
            if let Some(token) = self.tokens.get(self.current) {
                if let crate::lexer::Token::Identifier(id) = &token.token {
                    return id == keyword;
                }
            }
        }
        false
    }

    /// Parse covergroup declaration
    fn parse_covergroup_decl(&mut self) {
        self.start_node(SyntaxKind::CovergroupDecl);

        // 'covergroup' keyword
        self.expect(SyntaxKind::CovergroupKw);

        // Covergroup name
        self.expect(SyntaxKind::Ident);

        // Optional sampling event
        if self.at(SyntaxKind::At) {
            self.parse_clocking_event();
        }

        // Covergroup body
        self.expect(SyntaxKind::LBrace);
        self.parse_covergroup_body();
        self.expect(SyntaxKind::RBrace);

        self.finish_node();
    }

    /// Parse covergroup body
    fn parse_covergroup_body(&mut self) {
        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();
            match self.current_kind() {
                Some(SyntaxKind::CoverpointKw) => self.parse_coverpoint_decl(),
                Some(SyntaxKind::CrossKw) => self.parse_cross_decl(),
                Some(SyntaxKind::RBrace) => break,
                _ => {
                    self.error_and_bump("expected coverpoint or cross");
                }
            }
        }
    }

    /// Parse coverpoint declaration
    fn parse_coverpoint_decl(&mut self) {
        self.start_node(SyntaxKind::CoverpointDecl);

        // 'coverpoint' keyword
        self.expect(SyntaxKind::CoverpointKw);

        // Expression to cover
        self.parse_expression();

        // Optional coverpoint body with bins
        if self.at(SyntaxKind::LBrace) {
            self.bump(); // consume {
            self.parse_coverpoint_body();
            self.expect(SyntaxKind::RBrace);
        }

        // Optional semicolon
        if self.at(SyntaxKind::Semicolon) {
            self.bump();
        }

        self.finish_node();
    }

    /// Parse coverpoint body
    fn parse_coverpoint_body(&mut self) {
        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();
            match self.current_kind() {
                Some(SyntaxKind::BinsKw) => self.parse_bins_decl(),
                Some(SyntaxKind::IgnoreBinsKw) => self.parse_ignore_bins_decl(),
                Some(SyntaxKind::IllegalBinsKw) => self.parse_illegal_bins_decl(),
                Some(SyntaxKind::RBrace) => break,
                _ => {
                    self.error_and_bump("expected bins declaration");
                }
            }
        }
    }

    /// Parse bins declaration
    fn parse_bins_decl(&mut self) {
        self.start_node(SyntaxKind::BinsDecl);

        // 'bins' keyword
        self.expect(SyntaxKind::BinsKw);

        // Bins name
        self.expect(SyntaxKind::Ident);

        // Bins values
        self.expect(SyntaxKind::Assign);
        self.parse_bins_values();

        // Semicolon
        self.expect(SyntaxKind::Semicolon);

        self.finish_node();
    }

    /// Parse ignore_bins declaration
    fn parse_ignore_bins_decl(&mut self) {
        self.start_node(SyntaxKind::BinsDecl);

        // 'ignore_bins' keyword
        self.expect(SyntaxKind::IgnoreBinsKw);

        // Bins name
        self.expect(SyntaxKind::Ident);

        // Bins values
        self.expect(SyntaxKind::Assign);
        self.parse_bins_values();

        // Semicolon
        self.expect(SyntaxKind::Semicolon);

        self.finish_node();
    }

    /// Parse illegal_bins declaration
    fn parse_illegal_bins_decl(&mut self) {
        self.start_node(SyntaxKind::BinsDecl);

        // 'illegal_bins' keyword
        self.expect(SyntaxKind::IllegalBinsKw);

        // Bins name
        self.expect(SyntaxKind::Ident);

        // Bins values
        self.expect(SyntaxKind::Assign);
        self.parse_bins_values();

        // Semicolon
        self.expect(SyntaxKind::Semicolon);

        self.finish_node();
    }

    /// Parse bins values (ranges, sets, etc.)
    fn parse_bins_values(&mut self) {
        if self.at(SyntaxKind::LBrace) {
            // Set of values: { 1, 2, 3 }
            self.bump(); // consume {
            while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
                self.parse_expression();
                if self.at(SyntaxKind::Comma) {
                    self.bump();
                }
            }
            self.expect(SyntaxKind::RBrace);
        } else if self.at(SyntaxKind::LBracket) {
            // Range: [1:10]
            self.bump(); // consume [
            self.parse_expression();
            if self.at(SyntaxKind::Colon) {
                self.bump(); // consume :
                self.parse_expression();
            }
            self.expect(SyntaxKind::RBracket);
        } else {
            // Single value or expression
            self.parse_expression();
        }
    }

    /// Parse cross declaration
    fn parse_cross_decl(&mut self) {
        self.start_node(SyntaxKind::CrossDecl);

        // 'cross' keyword
        self.expect(SyntaxKind::CrossKw);

        // Cross name
        self.expect(SyntaxKind::Ident);

        // Cross variables
        self.expect(SyntaxKind::Assign);
        self.parse_expression(); // First coverpoint
        while self.at(SyntaxKind::Comma) {
            self.bump(); // consume ,
            self.parse_expression(); // Next coverpoint
        }

        // Optional cross body with bins
        if self.at(SyntaxKind::LBrace) {
            self.bump(); // consume {
            self.parse_coverpoint_body(); // Reuse coverpoint body parsing
            self.expect(SyntaxKind::RBrace);
        }

        // Semicolon
        self.expect(SyntaxKind::Semicolon);

        self.finish_node();
    }

    /// Parse formal verification block
    fn parse_formal_block(&mut self) {
        self.start_node(SyntaxKind::FormalBlock);

        // 'formal' keyword
        self.expect(SyntaxKind::FormalKw);

        // Optional block name
        if self.at(SyntaxKind::Ident) {
            self.bump(); // consume name
        }

        // Formal block body
        self.expect(SyntaxKind::LBrace);
        self.parse_formal_block_body();
        self.expect(SyntaxKind::RBrace);

        self.finish_node();
    }

    /// Parse formal block body
    fn parse_formal_block_body(&mut self) {
        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();
            match self.current_kind() {
                Some(SyntaxKind::InvariantKw) => self.parse_invariant_decl(),
                Some(SyntaxKind::SafetyKw) => self.parse_safety_property(),
                Some(SyntaxKind::LivenessKw) => self.parse_liveness_property(),
                Some(SyntaxKind::BoundedKw) => self.parse_bounded_property(),
                Some(SyntaxKind::AssumeKw) => self.parse_assume_statement(),
                Some(SyntaxKind::ProveKw) => self.parse_prove_statement(),
                Some(SyntaxKind::RBrace) => break,
                _ => {
                    self.error_and_bump("expected formal verification statement");
                }
            }
        }
    }

    /// Parse invariant declaration
    fn parse_invariant_decl(&mut self) {
        self.start_node(SyntaxKind::InvariantDecl);

        // 'invariant' keyword
        self.expect(SyntaxKind::InvariantKw);

        // Optional invariant name
        if self.at(SyntaxKind::Ident) {
            self.bump(); // consume name
        }

        // Colon separator
        if self.at(SyntaxKind::Colon) {
            self.bump();
        }

        // Property expression
        self.parse_temporal_expression();

        // Optional semicolon
        if self.at(SyntaxKind::Semicolon) {
            self.bump();
        }

        self.finish_node();
    }

    /// Parse safety property
    fn parse_safety_property(&mut self) {
        self.start_node(SyntaxKind::SafetyProperty);

        // 'safety' keyword
        self.expect(SyntaxKind::SafetyKw);

        // Optional property name
        if self.at(SyntaxKind::Ident) {
            self.bump(); // consume name
        }

        // Colon separator
        if self.at(SyntaxKind::Colon) {
            self.bump();
        }

        // Property expression
        self.parse_temporal_expression();

        // Optional bounds
        if self.at(SyntaxKind::BoundedKw) {
            self.parse_verification_bounds();
        }

        // Optional semicolon
        if self.at(SyntaxKind::Semicolon) {
            self.bump();
        }

        self.finish_node();
    }

    /// Parse liveness property
    fn parse_liveness_property(&mut self) {
        self.start_node(SyntaxKind::LivenessProperty);

        // 'liveness' keyword
        self.expect(SyntaxKind::LivenessKw);

        // Optional property name
        if self.at(SyntaxKind::Ident) {
            self.bump(); // consume name
        }

        // Colon separator
        if self.at(SyntaxKind::Colon) {
            self.bump();
        }

        // Property expression
        self.parse_temporal_expression();

        // Optional bounds
        if self.at(SyntaxKind::BoundedKw) {
            self.parse_verification_bounds();
        }

        // Optional semicolon
        if self.at(SyntaxKind::Semicolon) {
            self.bump();
        }

        self.finish_node();
    }

    /// Parse bounded property
    fn parse_bounded_property(&mut self) {
        self.start_node(SyntaxKind::BoundedProperty);

        // 'bounded' keyword
        self.expect(SyntaxKind::BoundedKw);

        // Bounds specification
        self.parse_verification_bounds();

        // Property expression
        self.parse_temporal_expression();

        // Optional semicolon
        if self.at(SyntaxKind::Semicolon) {
            self.bump();
        }

        self.finish_node();
    }

    /// Parse verification bounds
    fn parse_verification_bounds(&mut self) {
        // Bounds: bounded[min:max] or bounded[max]
        self.expect(SyntaxKind::LBracket);

        self.parse_expression(); // min or max

        if self.at(SyntaxKind::Colon) {
            self.bump(); // consume :
            self.parse_expression(); // max
        }

        self.expect(SyntaxKind::RBracket);
    }

    /// Parse prove statement
    fn parse_prove_statement(&mut self) {
        self.start_node(SyntaxKind::ProveStmt);

        // 'prove' keyword
        self.expect(SyntaxKind::ProveKw);

        // Property to prove
        self.parse_temporal_expression();

        // Optional verification bounds
        if self.at(SyntaxKind::BoundedKw) {
            self.parse_verification_bounds();
        }

        // Semicolon
        self.expect(SyntaxKind::Semicolon);

        self.finish_node();
    }

    /// Parse protocol declaration
    fn parse_protocol_decl(&mut self) {
        self.start_node(SyntaxKind::ProtocolDecl);

        // 'protocol' keyword
        self.expect(SyntaxKind::ProtocolKw);

        // Protocol name
        self.expect(SyntaxKind::Ident);

        // Optional generic parameters
        if self.at(SyntaxKind::Lt) {
            self.parse_generic_params();
        }

        // Protocol body
        self.expect(SyntaxKind::LBrace);
        self.parse_protocol_signals();
        self.expect(SyntaxKind::RBrace);

        self.finish_node();
    }

    /// Parse protocol signals
    fn parse_protocol_signals(&mut self) {
        self.start_node(SyntaxKind::ProtocolSignalList);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            if self.at(SyntaxKind::RBrace) {
                break;
            }

            self.parse_protocol_signal();
        }

        self.finish_node();
    }

    /// Parse protocol signal
    fn parse_protocol_signal(&mut self) {
        self.start_node(SyntaxKind::ProtocolSignal);

        // Protocol direction (in or out)
        self.start_node(SyntaxKind::ProtocolDirection);
        if self.at(SyntaxKind::InKw) {
            self.bump(); // consume 'in'
        } else if self.at(SyntaxKind::OutKw) {
            self.bump(); // consume 'out'
        } else {
            self.error("expected 'in' or 'out'");
        }
        self.finish_node();

        // Signal name
        self.expect(SyntaxKind::Ident);

        // Colon and type
        self.expect(SyntaxKind::Colon);
        self.parse_type();

        // Optional semicolon
        if self.at(SyntaxKind::Semicolon) {
            self.bump();
        }

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

    /// Parse requirement declaration
    fn parse_requirement_decl(&mut self) {
        self.start_node(SyntaxKind::RequirementDecl);

        // 'requirement' keyword
        self.expect(SyntaxKind::RequirementKw);

        // Requirement name
        self.expect(SyntaxKind::Ident);

        // Optional for clause (requirement MyReq for EntityName)
        if self.at(SyntaxKind::ForKw) {
            self.bump();
            self.expect(SyntaxKind::Ident);
        }

        // Requirement body
        self.expect(SyntaxKind::LBrace);
        self.parse_requirement_details();
        self.expect(SyntaxKind::RBrace);

        self.finish_node();
    }

    /// Parse requirement details
    fn parse_requirement_details(&mut self) {
        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            if self.at(SyntaxKind::RBrace) {
                break;
            }

            // Parse requirement properties (verification, description, etc.)
            if self.at(SyntaxKind::Ident) {
                self.parse_requirement_property();
            } else {
                self.error_and_bump("expected requirement property");
            }
        }
    }

    /// Parse requirement property
    fn parse_requirement_property(&mut self) {
        // Property name (verification, description, etc.)
        self.expect(SyntaxKind::Ident);

        // Colon
        self.expect(SyntaxKind::Colon);

        // Property value (expression or string)
        if self.at(SyntaxKind::StringLiteral) {
            self.bump();
        } else {
            self.parse_expression();
        }

        // Optional semicolon
        if self.at(SyntaxKind::Semicolon) {
            self.bump();
        }
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
                Some(SyntaxKind::FnKw) => self.parse_trait_method(),
                Some(SyntaxKind::SignalKw) => self.parse_trait_signal(),
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

        // 'fn' keyword
        self.expect(SyntaxKind::FnKw);

        // Method name
        self.expect(SyntaxKind::Ident);

        // Parameters
        self.expect(SyntaxKind::LParen);
        self.parse_parameter_list();
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

    /// Parse trait signal
    fn parse_trait_signal(&mut self) {
        self.start_node(SyntaxKind::TraitSignal);

        self.expect(SyntaxKind::SignalKw);
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
        self.start_node(SyntaxKind::TraitItemList);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            match self.current_kind() {
                Some(SyntaxKind::TypeKw) => self.parse_trait_impl_type(),
                Some(SyntaxKind::ConstKw) => self.parse_trait_impl_const(),
                Some(SyntaxKind::FnKw) => self.parse_trait_impl_method(),
                Some(SyntaxKind::RBrace) => break,
                _ => {
                    self.error_and_bump("expected trait implementation item");
                }
            }
        }

        self.finish_node();
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

        // 'fn' keyword
        self.expect(SyntaxKind::FnKw);

        // Method name
        self.expect(SyntaxKind::Ident);

        // Parameters
        self.expect(SyntaxKind::LParen);
        self.parse_parameter_list();
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

        if !self.at_closing_angle() {
            self.parse_type_or_const_arg();

            while self.at(SyntaxKind::Comma) {
                self.bump(); // consume comma
                if !self.at_closing_angle() {
                    self.parse_type_or_const_arg();
                }
            }
        }

        self.expect_closing_angle();
        self.finish_node();
    }

    /// Parse struct declaration
    fn parse_struct_decl(&mut self) {
        self.start_node(SyntaxKind::StructDecl);

        // 'struct' keyword
        self.expect(SyntaxKind::StructKw);

        // Struct name
        self.expect(SyntaxKind::Ident);

        // Optional generic parameters
        if self.at(SyntaxKind::Lt) {
            self.parse_generic_params();
        }

        // Struct body
        self.expect(SyntaxKind::LBrace);
        self.parse_struct_fields();
        self.expect(SyntaxKind::RBrace);

        self.finish_node();
    }

    /// Parse struct fields
    fn parse_struct_fields(&mut self) {
        self.start_node(SyntaxKind::StructFieldList);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();
            if self.at(SyntaxKind::RBrace) {
                break;
            }
            self.parse_struct_field();
            self.consume_semicolon();
        }

        self.finish_node();
    }

    /// Parse a single struct field
    fn parse_struct_field(&mut self) {
        self.start_node(SyntaxKind::StructField);

        // Field name
        self.expect(SyntaxKind::Ident);

        // Colon
        self.expect(SyntaxKind::Colon);

        // Field type
        self.parse_type();

        self.finish_node();
    }

    /// Parse enum declaration
    fn parse_enum_decl(&mut self) {
        self.start_node(SyntaxKind::EnumDecl);

        // 'enum' keyword
        self.expect(SyntaxKind::EnumKw);

        // Enum name
        self.expect(SyntaxKind::Ident);

        // Optional generic parameters
        if self.at(SyntaxKind::Lt) {
            self.parse_generic_params();
        }

        // Enum body
        self.expect(SyntaxKind::LBrace);
        self.parse_enum_variants();
        self.expect(SyntaxKind::RBrace);

        self.finish_node();
    }

    /// Parse enum variants
    fn parse_enum_variants(&mut self) {
        self.start_node(SyntaxKind::EnumVariantList);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();
            if self.at(SyntaxKind::RBrace) {
                break;
            }
            self.parse_enum_variant();

            // Variants are separated by commas
            if self.at(SyntaxKind::Comma) {
                self.bump();
            } else if !self.at(SyntaxKind::RBrace) {
                // If not at end and no comma, still try to continue
                self.error("expected ',' or '}'");
            }
        }

        self.finish_node();
    }

    /// Parse a single enum variant
    fn parse_enum_variant(&mut self) {
        self.start_node(SyntaxKind::EnumVariant);

        // Variant name
        self.expect(SyntaxKind::Ident);

        // Optional variant data
        if self.at(SyntaxKind::LParen) {
            // Tuple variant
            self.bump(); // (

            if !self.at(SyntaxKind::RParen) {
                self.parse_type();
                while self.at(SyntaxKind::Comma) {
                    self.bump();
                    if !self.at(SyntaxKind::RParen) {
                        self.parse_type();
                    }
                }
            }

            self.expect(SyntaxKind::RParen);
        } else if self.at(SyntaxKind::LBrace) {
            // Struct variant
            self.bump(); // {

            while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
                // Field name
                self.expect(SyntaxKind::Ident);
                self.expect(SyntaxKind::Colon);
                self.parse_type();

                if self.at(SyntaxKind::Comma) {
                    self.bump();
                } else if !self.at(SyntaxKind::RBrace) {
                    break;
                }
            }

            self.expect(SyntaxKind::RBrace);
        }
        // else: Unit variant (no data)

        self.finish_node();
    }

    /// Parse union declaration
    fn parse_union_decl(&mut self) {
        self.start_node(SyntaxKind::UnionDecl);

        // 'union' keyword
        self.expect(SyntaxKind::UnionKw);

        // Union name
        self.expect(SyntaxKind::Ident);

        // Optional generic parameters
        if self.at(SyntaxKind::Lt) {
            self.parse_generic_params();
        }

        // Union body
        self.expect(SyntaxKind::LBrace);
        self.parse_union_fields();
        self.expect(SyntaxKind::RBrace);

        self.finish_node();
    }

    /// Parse union fields
    fn parse_union_fields(&mut self) {
        self.start_node(SyntaxKind::UnionFieldList);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();
            if self.at(SyntaxKind::RBrace) {
                break;
            }
            self.parse_union_field();
            self.consume_semicolon();
        }

        self.finish_node();
    }

    /// Parse a single union field
    fn parse_union_field(&mut self) {
        self.start_node(SyntaxKind::UnionField);

        // Field name
        self.expect(SyntaxKind::Ident);

        // Colon
        self.expect(SyntaxKind::Colon);

        // Field type
        self.parse_type();

        self.finish_node();
    }

    /// Parse a type or const argument
    fn parse_type_or_const_arg(&mut self) {
        self.start_node(SyntaxKind::Arg);

        // Check if it's a literal (const argument)
        // Use simple expression to avoid consuming > as comparison operator
        if self.current_kind().is_some_and(|k| k.is_literal()) {
            self.start_node(SyntaxKind::LiteralExpr);
            self.bump();
            self.finish_node();
        }
        // Check if it's an identifier that looks like a const arg (not followed by type syntax)
        else if self.current_kind() == Some(SyntaxKind::Ident) {
            // Look ahead to see if this identifier is followed by type-like syntax
            // If followed by < or [, it's likely a type (e.g., Vec<T> or array[N])
            // Otherwise it's a const expression (e.g., WIDTH or DEPTH)
            let next = self.peek_kind(1);
            if matches!(next, Some(SyntaxKind::Lt) | Some(SyntaxKind::LBracket)) {
                // Looks like a generic type
                self.parse_type();
            } else {
                // Simple identifier - treat as const argument
                self.start_node(SyntaxKind::IdentExpr);
                self.bump();
                self.finish_node();
            }
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
            Some(SyntaxKind::BoolKw) => {
                self.start_node(SyntaxKind::BoolType);
                self.bump();
                // Bool type has no width specifier
                self.finish_node();
            }
            Some(SyntaxKind::NatKw) => {
                self.start_node(SyntaxKind::NatType);
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
            Some(SyntaxKind::LogicKw) => {
                self.start_node(SyntaxKind::LogicType);
                self.bump();
                self.parse_width_spec();
                self.finish_node();
            }
            Some(SyntaxKind::ClockKw) => {
                self.start_node(SyntaxKind::ClockType);
                self.bump();
                // Optional clock domain parameter - support both old and new syntax
                if self.at(SyntaxKind::LParen) {
                    // Old syntax: clock(domain_name)
                    self.bump();
                    self.expect(SyntaxKind::Ident); // clock domain name
                    self.expect(SyntaxKind::RParen);
                } else if self.at(SyntaxKind::Lt) {
                    // New syntax: clock<'lifetime>
                    self.bump(); // consume <
                    self.expect(SyntaxKind::Lifetime); // consume 'lifetime
                    self.expect_closing_angle(); // consume >
                }
                self.finish_node();
            }
            Some(SyntaxKind::ResetKw) => {
                self.start_node(SyntaxKind::ResetType);
                self.bump();
                // Optional reset polarity
                if self.at(SyntaxKind::LParen) {
                    self.bump();
                    self.expect(SyntaxKind::Ident); // active_high or active_low
                    self.expect(SyntaxKind::RParen);
                }
                self.finish_node();
            }
            Some(SyntaxKind::StreamKw) => {
                self.start_node(SyntaxKind::StreamType);
                self.bump();
                // Optional type parameter stream<T>
                if self.at(SyntaxKind::Lt) {
                    self.bump();
                    self.parse_type();
                    self.expect_closing_angle();
                }
                self.finish_node();
            }
            Some(SyntaxKind::LBracket) => {
                // Array type [element_type; size]
                self.parse_array_type();
            }
            Some(SyntaxKind::LParen) => {
                // Tuple type (T1, T2, T3)
                self.parse_tuple_type();
            }
            Some(SyntaxKind::Ident) => {
                self.start_node(SyntaxKind::CustomType);
                self.bump();

                // Optional generic arguments
                if self.at(SyntaxKind::Lt) {
                    self.parse_generic_args();
                }

                self.finish_node();
            }
            Some(SyntaxKind::SelfTypeKw) => {
                // Self or Self::Type syntax
                self.start_node(SyntaxKind::SelfType);
                self.bump(); // consume Self

                // Check for ::Type (associated type)
                if self.at(SyntaxKind::ColonColon) {
                    self.bump(); // consume ::
                    self.expect(SyntaxKind::Ident); // consume associated type name
                }

                self.finish_node();
            }
            _ => {
                self.error("expected type");
            }
        }

        self.finish_node();
    }

    /// Parse array type [T; N]
    fn parse_array_type(&mut self) {
        self.start_node(SyntaxKind::ArrayType);
        self.expect(SyntaxKind::LBracket);

        // Element type
        self.parse_type();

        // Semicolon
        self.expect(SyntaxKind::Semicolon);

        // Array size (expression)
        self.parse_expression();

        self.expect(SyntaxKind::RBracket);
        self.finish_node();
    }

    /// Parse tuple type (T1, T2, T3)
    fn parse_tuple_type(&mut self) {
        self.start_node(SyntaxKind::TupleType);
        self.expect(SyntaxKind::LParen);

        if !self.at(SyntaxKind::RParen) {
            self.parse_type();

            while self.at(SyntaxKind::Comma) {
                self.bump();
                if !self.at(SyntaxKind::RParen) {
                    self.parse_type();
                }
            }
        }

        self.expect(SyntaxKind::RParen);
        self.finish_node();
    }

    /// Parse width specification [N] or [expr] or <N> or <expr>
    fn parse_width_spec(&mut self) {
        if self.at(SyntaxKind::LBracket) {
            self.start_node(SyntaxKind::WidthSpec);
            self.bump();

            // Width can be a constant expression, not just a literal
            self.parse_expression();

            self.expect(SyntaxKind::RBracket);
            self.finish_node();
        } else if self.at(SyntaxKind::Lt) {
            // Support angle bracket syntax: bit<WIDTH>
            self.start_node(SyntaxKind::WidthSpec);
            self.bump(); // consume <

            // Parse primary expression only (literal or identifier) to avoid
            // consuming the closing > as a comparison operator
            self.parse_type_arg_expr();

            self.expect_closing_angle(); // consume >
            self.finish_node();
        }
    }

    /// Parse a simple expression for type arguments (identifier, literal, function call, or binary op)
    /// This avoids parsing comparison operators which would consume the closing >
    fn parse_type_arg_expr(&mut self) {
        let checkpoint = self.builder.checkpoint();

        // Start with primary expression
        self.parse_type_arg_primary();

        // Check for binary operators (+, -, *, /, %)
        // But stop at > which closes the type argument list
        if matches!(
            self.current_kind(),
            Some(SyntaxKind::Plus)
                | Some(SyntaxKind::Minus)
                | Some(SyntaxKind::Star)
                | Some(SyntaxKind::Slash)
                | Some(SyntaxKind::Percent)
        ) {
            // Wrap everything in a BinaryExpr node starting from the checkpoint
            self.builder
                .start_node_at(checkpoint, rowan::SyntaxKind(SyntaxKind::BinaryExpr as u16));
            self.bump(); // operator
            self.parse_type_arg_primary();
            self.finish_node();
        }
    }

    /// Parse a primary expression in type argument context
    fn parse_type_arg_primary(&mut self) {
        match self.current_kind() {
            Some(SyntaxKind::Ident) => {
                // Check if it's a function call (identifier followed by '(')
                if self.peek_kind(1) == Some(SyntaxKind::LParen) {
                    // Parse as function call (e.g., clog2(DEPTH))
                    self.start_node(SyntaxKind::CallExpr);
                    self.bump(); // function name
                    self.bump(); // '('

                    // Parse arguments (recursively handle const expressions)
                    if !self.at(SyntaxKind::RParen) {
                        self.parse_type_arg_expr();

                        while self.at(SyntaxKind::Comma) {
                            self.bump(); // ','
                            self.parse_type_arg_expr();
                        }
                    }

                    self.expect(SyntaxKind::RParen);
                    self.finish_node();
                } else {
                    // Simple identifier
                    self.start_node(SyntaxKind::IdentExpr);
                    self.bump();
                    self.finish_node();
                }
            }
            Some(kind) if kind.is_literal() => {
                self.start_node(SyntaxKind::LiteralExpr);
                self.bump(); // literal value
                self.finish_node();
            }
            Some(SyntaxKind::LParen) => {
                // Parenthesized expression
                self.start_node(SyntaxKind::ParenExpr);
                self.bump(); // '('
                self.parse_type_arg_expr();
                self.expect(SyntaxKind::RParen);
                self.finish_node();
            }
            _ => {
                self.error("expected identifier, literal, function call, or parenthesized expression in type argument");
            }
        }
    }

    /// Parse generic parameters (stub)
    fn parse_generic_params(&mut self) {
        self.start_node(SyntaxKind::GenericParamList);
        self.expect(SyntaxKind::Lt);

        if !self.at_closing_angle() {
            self.parse_generic_param();

            while self.at(SyntaxKind::Comma) {
                self.bump(); // consume comma
                self.parse_generic_param();
            }
        }

        self.expect_closing_angle();
        self.finish_node();
    }

    /// Parse a single generic parameter
    fn parse_generic_param(&mut self) {
        self.start_node(SyntaxKind::GenericParam);

        // Check if it's a lifetime parameter ('clk)
        if self.at(SyntaxKind::Lifetime) {
            self.bump(); // consume 'lifetime
        }
        // Legacy support for apostrophe + ident
        else if self.at(SyntaxKind::Apostrophe) {
            self.bump(); // consume '
            self.expect(SyntaxKind::Ident); // lifetime name
        }
        // Check if it's a const parameter (const N: nat[32])
        else if self.at(SyntaxKind::ConstKw) {
            self.bump(); // consume 'const'
            self.expect(SyntaxKind::Ident); // parameter name
            self.expect(SyntaxKind::Colon);
            self.parse_type(); // parameter type

            // Optional default value (= 8)
            if self.at(SyntaxKind::Assign) {
                self.bump(); // consume =
                self.parse_type_arg_expr(); // default value (use type_arg_expr to avoid consuming >)
            }
        }
        // Check if it's a type parameter with bounds (T: Trait) or type constraint (WIDTH: nat = 8)
        else if self.current_kind() == Some(SyntaxKind::Ident) {
            self.bump(); // consume identifier

            // Optional type annotation or bounds
            if self.at(SyntaxKind::Colon) {
                self.bump();

                // Check if next token indicates a type (nat, bit, etc.) or a trait bound
                if matches!(
                    self.current_kind(),
                    Some(SyntaxKind::NatKw)
                        | Some(SyntaxKind::BitKw)
                        | Some(SyntaxKind::IntKw)
                        | Some(SyntaxKind::LogicKw)
                        | Some(SyntaxKind::ClockKw)
                        | Some(SyntaxKind::ResetKw)
                ) {
                    // Parse type constraint (WIDTH: nat)
                    self.parse_type();

                    // Optional default value (= 8)
                    if self.at(SyntaxKind::Assign) {
                        self.bump(); // consume =
                        self.parse_type_arg_expr(); // default value (use type_arg_expr to avoid consuming >)
                    }
                } else {
                    // Parse trait bounds (T: Trait)
                    self.parse_trait_bound_list();
                }
            }
        } else {
            self.error("expected generic parameter");
        }

        self.finish_node();
    }

    /// Parse expression with operator precedence
    fn parse_expression(&mut self) {
        self.parse_logical_or_expr();
    }

    /// Parse logical OR expression (||)
    fn parse_logical_or_expr(&mut self) {
        self.parse_logical_and_expr();

        while self.at(SyntaxKind::PipePipe) {
            self.start_node(SyntaxKind::BinaryExpr);
            self.bump(); // consume ||
            self.parse_logical_and_expr();
            self.finish_node();
        }
    }

    /// Parse logical AND expression (&&)
    fn parse_logical_and_expr(&mut self) {
        self.parse_equality_expr();

        while self.at(SyntaxKind::AmpAmp) {
            self.start_node(SyntaxKind::BinaryExpr);
            self.bump(); // consume &&
            self.parse_equality_expr();
            self.finish_node();
        }
    }

    /// Parse equality expression (== !=)
    fn parse_equality_expr(&mut self) {
        self.parse_relational_expr();

        while self.at(SyntaxKind::Eq) || self.at(SyntaxKind::Neq) {
            self.start_node(SyntaxKind::BinaryExpr);
            self.bump(); // consume == or !=
            self.parse_relational_expr();
            self.finish_node();
        }
    }

    /// Parse relational expression (< > <= >=)
    fn parse_relational_expr(&mut self) {
        self.parse_bitwise_or_expr();

        while matches!(
            self.current_kind(),
            Some(SyntaxKind::Lt)
                | Some(SyntaxKind::Gt)
                | Some(SyntaxKind::Le)
                | Some(SyntaxKind::Ge)
        ) {
            self.start_node(SyntaxKind::BinaryExpr);
            self.bump(); // consume relational operator
            self.parse_bitwise_or_expr();
            self.finish_node();
        }
    }

    /// Parse bitwise OR expression (|)
    fn parse_bitwise_or_expr(&mut self) {
        self.parse_bitwise_xor_expr();

        while self.at(SyntaxKind::Pipe) {
            self.start_node(SyntaxKind::BinaryExpr);
            self.bump(); // consume |
            self.parse_bitwise_xor_expr();
            self.finish_node();
        }
    }

    /// Parse bitwise XOR expression (^)
    fn parse_bitwise_xor_expr(&mut self) {
        self.parse_bitwise_and_expr();

        while self.at(SyntaxKind::Caret) {
            self.start_node(SyntaxKind::BinaryExpr);
            self.bump(); // consume ^
            self.parse_bitwise_and_expr();
            self.finish_node();
        }
    }

    /// Parse bitwise AND expression (&)
    fn parse_bitwise_and_expr(&mut self) {
        self.parse_shift_expr();

        while self.at(SyntaxKind::Amp) {
            self.start_node(SyntaxKind::BinaryExpr);
            self.bump(); // consume &
            self.parse_shift_expr();
            self.finish_node();
        }
    }

    /// Parse shift expression (<< >>)
    fn parse_shift_expr(&mut self) {
        self.parse_additive_expr();

        while self.at(SyntaxKind::Shl) || self.at(SyntaxKind::Shr) {
            self.start_node(SyntaxKind::BinaryExpr);
            self.bump(); // consume << or >>
            self.parse_additive_expr();
            self.finish_node();
        }
    }

    /// Parse additive expression (+ -)
    fn parse_additive_expr(&mut self) {
        self.parse_multiplicative_expr();

        while self.at(SyntaxKind::Plus) || self.at(SyntaxKind::Minus) {
            self.start_node(SyntaxKind::BinaryExpr);
            self.bump(); // consume + or -
            self.parse_multiplicative_expr();
            self.finish_node();
        }
    }

    /// Parse multiplicative expression (* / %)
    fn parse_multiplicative_expr(&mut self) {
        self.parse_unary_expr();

        while matches!(
            self.current_kind(),
            Some(SyntaxKind::Star) | Some(SyntaxKind::Slash) | Some(SyntaxKind::Percent)
        ) {
            self.start_node(SyntaxKind::BinaryExpr);
            self.bump(); // consume *, /, or %
            self.parse_unary_expr();
            self.finish_node();
        }
    }

    /// Parse unary expression (! ~ -)
    fn parse_unary_expr(&mut self) {
        if matches!(
            self.current_kind(),
            Some(SyntaxKind::Bang) | Some(SyntaxKind::Tilde) | Some(SyntaxKind::Minus)
        ) {
            self.start_node(SyntaxKind::UnaryExpr);
            self.bump(); // consume unary operator
            self.parse_unary_expr(); // right-associative
            self.finish_node();
        } else {
            self.parse_primary_expression();
        }
    }

    /// Parse primary expression
    fn parse_primary_expression(&mut self) {
        match self.current_kind() {
            Some(
                SyntaxKind::IntLiteral
                | SyntaxKind::BinLiteral
                | SyntaxKind::HexLiteral
                | SyntaxKind::StringLiteral
                | SyntaxKind::TrueKw
                | SyntaxKind::FalseKw,
            ) => {
                self.start_node(SyntaxKind::LiteralExpr);
                self.bump();
                self.finish_node();
            }
            Some(SyntaxKind::Ident) => {
                self.parse_identifier_expression();
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
            Some(SyntaxKind::LBracket) => {
                self.parse_array_literal();
            }
            Some(SyntaxKind::IfKw) => {
                self.parse_if_expression();
            }
            Some(SyntaxKind::MatchKw) => {
                self.parse_match_expression();
            }
            _ => {
                self.error("expected expression");
            }
        }
    }

    /// Parse identifier expression with possible postfix operations
    fn parse_identifier_expression(&mut self) {
        // Check for :: ahead to determine if this is a path expression
        let is_path = self.current_kind() == Some(SyntaxKind::Ident)
            && self.peek_kind(1) == Some(SyntaxKind::ColonColon);

        if is_path {
            // Parse as path expression (Type::Variant)
            self.start_node(SyntaxKind::PathExpr);
            self.expect(SyntaxKind::Ident); // enum type name
            self.expect(SyntaxKind::ColonColon); // ::
            self.expect(SyntaxKind::Ident); // variant name
            self.finish_node();
            return;
        }

        // Parse as regular identifier expression
        self.start_node(SyntaxKind::IdentExpr);
        self.bump(); // consume identifier

        // Handle postfix operations
        loop {
            match self.current_kind() {
                Some(SyntaxKind::Dot) => {
                    // Field access
                    self.finish_node(); // finish IdentExpr
                    self.start_node(SyntaxKind::FieldExpr);
                    self.bump(); // consume dot
                    self.expect(SyntaxKind::Ident);
                }
                Some(SyntaxKind::LBracket) => {
                    // Array/bit indexing
                    self.finish_node(); // finish current expression
                    self.start_node(SyntaxKind::IndexExpr);
                    self.bump(); // consume '['
                    self.parse_expression();

                    // Check for range indexing [start:end]
                    if self.at(SyntaxKind::Colon) {
                        self.bump(); // consume ':'
                        self.parse_expression();
                    }

                    self.expect(SyntaxKind::RBracket);
                }
                Some(SyntaxKind::LParen) => {
                    // Function call
                    self.finish_node(); // finish current expression
                    self.start_node(SyntaxKind::CallExpr);
                    self.bump(); // consume '('
                    self.parse_argument_list();
                    self.expect(SyntaxKind::RParen);
                }
                _ => break,
            }
        }

        self.finish_node();
    }

    /// Parse array literal [1, 2, 3]
    fn parse_array_literal(&mut self) {
        self.start_node(SyntaxKind::ArrayLiteral);
        self.expect(SyntaxKind::LBracket);

        if !self.at(SyntaxKind::RBracket) {
            self.parse_expression();

            while self.at(SyntaxKind::Comma) {
                self.bump();
                if !self.at(SyntaxKind::RBracket) {
                    self.parse_expression();
                }
            }
        }

        self.expect(SyntaxKind::RBracket);
        self.finish_node();
    }

    /// Parse parameter list
    fn parse_parameter_list(&mut self) {
        if !self.at(SyntaxKind::RParen) {
            self.parse_parameter();

            while self.at(SyntaxKind::Comma) {
                self.bump();
                if !self.at(SyntaxKind::RParen) {
                    self.parse_parameter();
                }
            }
        }
    }

    /// Parse single parameter
    fn parse_parameter(&mut self) {
        self.start_node(SyntaxKind::Parameter);

        // Handle &self or self
        if self.at(SyntaxKind::Amp) {
            self.bump(); // &
            if self.at(SyntaxKind::SelfKw) {
                self.bump(); // self
                self.finish_node();
                return;
            }
        } else if self.at(SyntaxKind::SelfKw) {
            self.bump(); // self
            self.finish_node();
            return;
        }

        // Parameter name
        self.expect(SyntaxKind::Ident);

        // Colon and type
        self.expect(SyntaxKind::Colon);
        self.parse_type();

        // Optional default value
        if self.at(SyntaxKind::Assign) {
            self.bump();
            self.parse_expression();
        }

        self.finish_node();
    }

    /// Parse argument list for function calls
    fn parse_argument_list(&mut self) {
        if !self.at(SyntaxKind::RParen) {
            self.parse_expression();

            while self.at(SyntaxKind::Comma) {
                self.bump();
                if !self.at(SyntaxKind::RParen) {
                    self.parse_expression();
                }
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
            Some(SyntaxKind::InKw | SyntaxKind::OutKw | SyntaxKind::InoutKw | SyntaxKind::PortKw)
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

    /// Check if we're at a closing angle bracket, treating >> as two >
    fn at_closing_angle(&self) -> bool {
        self.partial_shr || matches!(self.current_kind(), Some(SyntaxKind::Gt | SyntaxKind::Shr))
    }

    /// Consume a closing angle bracket, handling >> as two separate >
    /// This is used when parsing generic arguments to handle cases like List<List<T>>
    fn expect_closing_angle(&mut self) {
        // Check if we have a partially consumed >> token
        if self.partial_shr {
            // We already consumed one > from >>, now consume the second
            self.builder
                .token(rowan::SyntaxKind(SyntaxKind::Gt as u16), ">");
            self.partial_shr = false;
            // NOW consume the >> token that we deferred earlier
            self.current += 1;
            return;
        }

        match self.current_kind() {
            Some(SyntaxKind::Gt) => {
                self.bump();
            }
            Some(SyntaxKind::Shr) => {
                // Handle >> as two separate > tokens
                // Emit the first > but DON'T consume the token yet
                self.builder
                    .token(rowan::SyntaxKind(SyntaxKind::Gt as u16), ">");

                // Mark that we've consumed one > from >>
                // The actual token consumption will happen on the next call
                self.partial_shr = true;

                // DON'T consume the >> token here - wait for next call
            }
            _ => {
                self.expect(SyntaxKind::Gt);
            }
        }
    }

    /// Expect a specific token kind
    fn expect(&mut self, kind: SyntaxKind) {
        if self.at(kind) {
            self.bump();
        } else {
            let position = if let Some(token) = self.current_token() {
                token.span.start
            } else {
                self.source.len()
            };

            let error = ParseError {
                message: format!("expected {}", kind.description()),
                position,
                kind: ParseErrorKind::MissingToken,
                expected: Some(kind.description().to_string()),
                found: self.current_token().map(|t| format!("{:?}", t.token)),
            };

            self.errors.push(error);
        }
    }

    /// Consume an optional semicolon
    fn consume_semicolon(&mut self) {
        if self.at(SyntaxKind::Semicolon) {
            self.bump();
        }
    }

    /// Report an error
    fn error(&mut self, message: &str) {
        self.report_error(message, ParseErrorKind::InvalidSyntax);
    }

    /// Report an error and consume token
    fn error_and_bump(&mut self, message: &str) {
        self.report_error(message, ParseErrorKind::UnexpectedToken);
        if !self.is_at_end() {
            self.bump();
        }
    }

    /// Report a specific error type
    fn report_error(&mut self, message: &str, kind: ParseErrorKind) {
        let position = if let Some(token) = self.current_token() {
            token.span.start
        } else {
            self.source.len()
        };

        let error = ParseError {
            message: message.to_string(),
            position,
            kind,
            expected: None,
            found: self.current_token().map(|t| format!("{:?}", t.token)),
        };

        self.errors.push(error);
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
    pub kind: ParseErrorKind,
    pub expected: Option<String>,
    pub found: Option<String>,
}

/// Types of parse errors
#[derive(Debug, Clone, PartialEq)]
pub enum ParseErrorKind {
    UnexpectedToken,
    MissingToken,
    InvalidSyntax,
    UnknownConstruct,
}

/// Main parsing function
pub fn parse(source: &str) -> SyntaxNode {
    let parser = ParseState::new(source);
    let result = parser.parse_source_file();
    SyntaxNode::new_root(result.green_node)
}

/// Parse with error reporting
pub fn parse_with_errors(source: &str) -> (SyntaxNode, Vec<ParseError>) {
    let parser = ParseState::new(source);
    let result = parser.parse_source_file();
    (SyntaxNode::new_root(result.green_node), result.errors)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_reporting() {
        let source = "entity { // missing name";
        let (_, errors) = parse_with_errors(source);

        assert!(!errors.is_empty());
        assert!(errors[0].message.contains("expected"));
    }

    #[test]
    fn test_missing_token_error() {
        let source = "entity Counter // missing brace";
        let (_, errors) = parse_with_errors(source);

        // Should report missing opening brace
        assert!(errors
            .iter()
            .any(|e| e.kind == ParseErrorKind::MissingToken));
    }

    #[test]
    fn test_valid_parse_no_errors() {
        let source = r#"
            entity Counter {
                in clk: clock
                out count: bit[8]
            }
        "#;
        let (_, errors) = parse_with_errors(source);
        assert!(errors.is_empty());
    }

    #[test]
    fn test_generic_parameter_parsing() {
        let source = r#"
            entity Counter {
                out count: bit[8]
            }
        "#;
        let (tree, errors) = parse_with_errors(source);

        // Should parse without errors - generic params will be added in Week 2
        assert!(errors.is_empty());

        // Should contain entity declaration
        assert!(tree.to_string().contains("Counter"));
    }

    #[test]
    fn test_intent_parsing() {
        let source = r#"
            entity TestEntity {
                out test: bit
            }
        "#;
        let (tree, errors) = parse_with_errors(source);

        // Should parse basic entity without errors - intent parsing will be added in Week 2
        assert!(errors.is_empty());
        assert!(tree.to_string().contains("TestEntity"));
    }

    #[test]
    fn test_pattern_matching_tokens() {
        // Test that new pattern matching tokens (:: and =>) are properly recognized
        // This validates that the lexer enhancements are working
        let mut lexer = crate::lexer::Lexer::new(":: =>");
        let tokens: Vec<_> = lexer.tokenize().into_iter().map(|t| t.token).collect();

        assert_eq!(
            tokens,
            vec![
                crate::lexer::Token::ColonColon,
                crate::lexer::Token::FatArrow,
            ]
        );

        // Note: Full pattern matching parsing will be integrated with event block parsing in Week 2
        // The core enhancements (ColonColon, FatArrow tokens, path pattern support) are implemented
    }

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
