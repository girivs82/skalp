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

            // Parse optional attributes before item
            // Attributes attach to the following item: #[parallel] entity Foo { ... }
            self.parse_attributes();

            // Skip trivia after attributes
            self.skip_trivia();

            // Parse top-level items
            match self.current_kind() {
                Some(SyntaxKind::UseKw) => self.parse_use_decl(),
                Some(SyntaxKind::PubKw) | Some(SyntaxKind::ModKw) => {
                    self.parse_item_with_visibility()
                }
                Some(SyntaxKind::EntityKw) => self.parse_entity_decl(),
                Some(SyntaxKind::ImplKw) => self.parse_impl_block(),
                Some(SyntaxKind::ProtocolKw) => self.parse_protocol_decl(),
                Some(SyntaxKind::IntentKw) => self.parse_intent_decl(),
                Some(SyntaxKind::RequirementKw) => self.parse_requirement_decl(),
                Some(SyntaxKind::TraitKw) => self.parse_trait_def(),
                Some(SyntaxKind::TypeKw) => self.parse_type_alias(),
                Some(SyntaxKind::StructKw) => self.parse_struct_decl(),
                Some(SyntaxKind::EnumKw) => self.parse_enum_decl(),
                Some(SyntaxKind::UnionKw) => self.parse_union_decl(),
                Some(SyntaxKind::ConstraintKw) => self.parse_global_constraint_block(),
                Some(SyntaxKind::ConstKw) => {
                    // Check if this is 'const fn' or just 'const'
                    if self.peek_kind(1) == Some(SyntaxKind::FnKw) {
                        self.parse_impl_function() // Reuse impl function parser
                    } else {
                        self.parse_constant_decl()
                    }
                }
                Some(SyntaxKind::FnKw) => {
                    // Top-level function
                    self.parse_impl_function()
                }
                // Safety Features (ISO 26262) - only parsed with --safety flag
                Some(SyntaxKind::SafetyGoalKw) => self.parse_safety_goal_decl(),
                Some(SyntaxKind::SafetyEntityKw) => self.parse_safety_entity_decl(),
                Some(SyntaxKind::SafetyTraitKw) => self.parse_safety_trait_decl(),
                Some(SyntaxKind::FmeaTraitKw) => self.parse_fmea_trait_decl(),
                Some(SyntaxKind::HsiTraitKw) => self.parse_hsi_trait_decl(),
                Some(SyntaxKind::FmedaLibraryKw) => self.parse_fmeda_library_decl(),
                Some(SyntaxKind::HashBracket) => {
                    // Stray attribute with no following item - error
                    self.error_and_bump("attribute must precede an item");
                }
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

        // Optional generic parameters for the impl itself
        // Pattern: impl<T, const N: nat> EntityName<T, N> { }
        let has_impl_generics = self.at(SyntaxKind::Lt);

        // Look ahead to determine if this is a trait impl
        // After consuming 'impl' and optional generics, check for: <trait> for <type>
        let is_trait_impl = {
            let mut found_for = false;
            let mut lookahead = 0;

            // Skip past impl generic parameters if present
            if has_impl_generics {
                lookahead = 1; // Start after '<'
                let mut depth = 1;
                while depth > 0 && lookahead < 50 && self.peek_kind(lookahead).is_some() {
                    match self.peek_kind(lookahead) {
                        Some(SyntaxKind::Lt) => depth += 1,
                        Some(SyntaxKind::Gt) => depth -= 1,
                        Some(SyntaxKind::Shr) => depth -= 2, // >> counts as two >
                        _ => {}
                    }
                    lookahead += 1;
                }
            }

            // Now check if we have: IDENT [<generics>] FOR IDENT
            if self.peek_kind(lookahead) == Some(SyntaxKind::Ident) {
                lookahead += 1;
                // Skip generic parameters after entity/trait name if present
                if self.peek_kind(lookahead) == Some(SyntaxKind::Lt) {
                    // Skip past the generic parameters with depth tracking
                    let mut depth = 1;
                    lookahead += 1; // Skip the '<'
                    while depth > 0 && lookahead < 50 && self.peek_kind(lookahead).is_some() {
                        match self.peek_kind(lookahead) {
                            Some(SyntaxKind::Lt) => depth += 1,
                            Some(SyntaxKind::Gt) => depth -= 1,
                            Some(SyntaxKind::Shr) => depth -= 2, // >> counts as two >
                            _ => {}
                        }
                        lookahead += 1;
                    }
                    // Now check if immediately followed by 'for' keyword (trait impl)
                    if self.peek_kind(lookahead) == Some(SyntaxKind::ForKw) {
                        found_for = true;
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

        // Optional generic parameters for the impl itself
        // Pattern: impl<T, const N: nat> EntityName<T, N>
        if self.at(SyntaxKind::Lt) {
            self.parse_generic_params();
        }

        // Entity name
        self.expect(SyntaxKind::Ident);

        // Optional generic arguments being applied to the entity
        // Pattern: EntityName<T, N>
        if self.at(SyntaxKind::Lt) {
            self.parse_generic_params();
        }

        // Optional 'with' clause for intents
        if self.at(SyntaxKind::WithKw) {
            self.bump(); // consume 'with'
            self.expect(SyntaxKind::Ident); // intent name
                                            // Optional generic parameters for intent
            if self.at(SyntaxKind::Lt) {
                self.parse_generic_params();
            }
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

        // Optional generic parameters for the impl itself
        // Pattern: impl<T, const N: nat> TraitName<T> for EntityName<T>
        if self.at(SyntaxKind::Lt) {
            self.parse_generic_params();
        }

        // Trait name
        self.expect(SyntaxKind::Ident);

        // Optional generic arguments for trait
        if self.at(SyntaxKind::Lt) {
            self.parse_generic_params();
        }

        // 'for' keyword
        self.expect(SyntaxKind::ForKw);

        // Target type - can be nat[32], bit[8], fp32, or CustomType<T>
        // Use parse_type() to handle all type forms
        self.parse_type();

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

            // Parse optional attributes before item
            // e.g., #[parallel] fn decode() { ... }
            self.parse_attributes();
            self.skip_trivia();

            match self.current_kind() {
                Some(SyntaxKind::SignalKw) => {
                    self.parse_signal_decl();
                    // Consume optional semicolon separator
                    if self.at(SyntaxKind::Semicolon) {
                        self.bump();
                    }
                }
                Some(SyntaxKind::VarKw) => {
                    self.parse_variable_decl();
                    // Consume optional semicolon separator
                    if self.at(SyntaxKind::Semicolon) {
                        self.bump();
                    }
                }
                Some(SyntaxKind::ConstKw) => {
                    // Check if this is 'const fn' or just 'const'
                    if self.peek_kind(1) == Some(SyntaxKind::FnKw) {
                        self.parse_impl_function()
                    } else {
                        self.parse_constant_decl();
                        // Consume optional semicolon separator
                        if self.at(SyntaxKind::Semicolon) {
                            self.bump();
                        }
                    }
                }
                Some(SyntaxKind::FnKw) => self.parse_impl_function(),
                Some(SyntaxKind::EnumKw) => self.parse_enum_decl(),
                Some(SyntaxKind::StructKw) => self.parse_struct_decl(),
                Some(SyntaxKind::UnionKw) => self.parse_union_decl(),
                Some(SyntaxKind::LetKw) => {
                    // Disambiguate between instance declaration and let binding
                    // Instance: let name = EntityName { ... } or let name = EntityName<T> { ... }
                    // Let binding: let name = expression
                    // Look ahead to see if we have: let ident = ident { or let ident = ident < ...
                    let mut is_instance = false;
                    if self.peek_kind(1) == Some(SyntaxKind::Ident)
                        && self.peek_kind(2) == Some(SyntaxKind::Assign)
                        && self.peek_kind(3) == Some(SyntaxKind::Ident)
                    {
                        // Check if followed by { or <
                        if self.peek_kind(4) == Some(SyntaxKind::LBrace) {
                            is_instance = true;
                        } else if self.peek_kind(4) == Some(SyntaxKind::Lt) {
                            // Generic instantiation - scan forward to find the closing >
                            // then check if there's a { after it
                            let mut depth = 0;
                            let mut offset = 4;
                            loop {
                                match self.peek_kind(offset) {
                                    Some(SyntaxKind::Lt) => depth += 1,
                                    Some(SyntaxKind::Gt) => {
                                        depth -= 1;
                                        if depth == 0 {
                                            // Found closing >, check next token
                                            if self.peek_kind(offset + 1)
                                                == Some(SyntaxKind::LBrace)
                                            {
                                                is_instance = true;
                                            }
                                            break;
                                        }
                                    }
                                    Some(SyntaxKind::Shr) => {
                                        // >> counts as two >
                                        depth -= 2;
                                        if depth <= 0 {
                                            // Found closing >>, check next token
                                            if self.peek_kind(offset + 1)
                                                == Some(SyntaxKind::LBrace)
                                            {
                                                is_instance = true;
                                            }
                                            break;
                                        }
                                    }
                                    None => break,
                                    _ => {}
                                }
                                offset += 1;
                                if offset > 20 {
                                    // Safety limit
                                    break;
                                }
                            }
                        }
                    }

                    if is_instance {
                        self.parse_instance_decl()
                    } else {
                        self.parse_let_statement()
                    }
                }
                Some(SyntaxKind::OnKw) => self.parse_event_block(),
                Some(SyntaxKind::MatchKw) => self.parse_match_statement(),
                Some(SyntaxKind::ForKw) => self.parse_for_stmt(),
                Some(SyntaxKind::GenerateKw) => self.parse_generate_stmt(),
                Some(SyntaxKind::FlowKw) => self.parse_flow_statement(),
                Some(SyntaxKind::AssignKw) => self.parse_continuous_assignment(),
                Some(SyntaxKind::CovergroupKw) => self.parse_covergroup_decl(),
                Some(SyntaxKind::FormalKw) => self.parse_formal_block(),
                Some(SyntaxKind::InvariantKw) => self.parse_invariant_decl(),
                Some(SyntaxKind::SafetyKw) => self.parse_safety_kv_pair(),
                Some(SyntaxKind::LivenessKw) => self.parse_liveness_property(),
                Some(SyntaxKind::ProveKw) => self.parse_prove_statement(),
                // Formal verification statements (assert, assume, cover)
                Some(SyntaxKind::AssertKw) => self.parse_assert_statement(),
                Some(SyntaxKind::AssumeKw) => self.parse_assume_statement(),
                Some(SyntaxKind::CoverKw) => self.parse_cover_statement(),
                Some(SyntaxKind::Ident) => {
                    // Could be an assignment or start of another construct
                    self.parse_assignment_or_statement();
                }
                // Port direction keywords can also be used as signal names in assignments
                Some(SyntaxKind::InKw)
                | Some(SyntaxKind::InputKw)
                | Some(SyntaxKind::OutKw)
                | Some(SyntaxKind::OutputKw)
                | Some(SyntaxKind::InoutKw) => {
                    // Treat as identifier in assignment context
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

            // Parse optional attributes before item
            // e.g., #[memory(depth = 1024)] signal mem: bit[64]
            self.parse_attributes();
            self.skip_trivia();

            if self.at_port_direction() {
                self.parse_port_decl();

                // Consume optional separator (comma or semicolon)
                if self.at(SyntaxKind::Comma) || self.at(SyntaxKind::Semicolon) {
                    self.bump();
                }
            } else if self.at(SyntaxKind::SignalKw) {
                // Allow signal declarations in entity body (for testbenches)
                self.parse_signal_decl();

                // Consume optional separator (comma or semicolon)
                if self.at(SyntaxKind::Comma) || self.at(SyntaxKind::Semicolon) {
                    self.bump();
                }
            } else if self.at(SyntaxKind::LetKw) {
                // Allow instance declarations in entity body (for DUT instantiation)
                self.parse_instance_decl();

                // Consume optional separator (comma or semicolon)
                if self.at(SyntaxKind::Comma) || self.at(SyntaxKind::Semicolon) {
                    self.bump();
                }
            } else if self.at(SyntaxKind::Ident) || self.at(SyntaxKind::LParen) {
                // Allow assignments in entity body (for direct signal assignments and tuple destructuring)
                // This enables patterns like: result = expr or (a, b, c) = func()
                self.parse_assignment_or_statement();

                // Consume optional separator (comma or semicolon)
                if self.at(SyntaxKind::Comma) || self.at(SyntaxKind::Semicolon) {
                    self.bump();
                }
            } else if self.at(SyntaxKind::RBrace) {
                break;
            } else {
                self.error_and_bump(
                    "expected port declaration, signal declaration, let binding, or assignment",
                );
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
            || self.at(SyntaxKind::InputKw)
            || self.at(SyntaxKind::OutKw)
            // Note: OutputKw should NOT be accepted as a direction keyword
            // Only OutKw is valid - OutputKw is a legacy synonym that creates conflicts
            || self.at(SyntaxKind::InoutKw)
            || self.at(SyntaxKind::PortKw)
        {
            self.bump();
        }
        self.finish_node();

        // Port name (allow keywords to be used as port names)
        if self.at(SyntaxKind::Ident) {
            self.bump();
        } else if self.current_kind().is_some_and(|k| k.is_keyword()) {
            self.bump(); // Allow keywords as port names (e.g., "in reset: reset")
        } else {
            self.error("expected identifier");
        }

        // Colon and type
        self.expect(SyntaxKind::Colon);
        self.parse_type();

        // Optional physical constraint block: @ { ... }
        if self.at(SyntaxKind::At) {
            self.parse_physical_constraint_block();
        }

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
        self.expect(SyntaxKind::Semicolon);

        self.finish_node();
    }

    /// Parse function declaration in impl block
    /// Syntax: [const] fn name[<generic_params>](params) -> return_type { body }
    fn parse_impl_function(&mut self) {
        self.start_node(SyntaxKind::FunctionDecl);

        // Optional 'const' keyword
        if self.at(SyntaxKind::ConstKw) {
            self.bump();
        }

        // 'fn' keyword
        self.expect(SyntaxKind::FnKw);

        // Function name
        self.expect(SyntaxKind::Ident);

        // Optional generic parameters
        if self.at(SyntaxKind::Lt) {
            self.parse_generic_params();
        }

        // Parameters
        self.expect(SyntaxKind::LParen);
        self.parse_parameter_list();
        self.expect(SyntaxKind::RParen);

        // Optional return type
        if self.at(SyntaxKind::Arrow) {
            self.bump();
            self.parse_type();
        }

        // Function body
        self.parse_block_statement();

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

        // Optional generic arguments (values, not parameter declarations)
        if self.at(SyntaxKind::Lt) {
            self.parse_generic_args();
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
            // Port name (allow keywords to be used as port names, like "reset")
            if self.at(SyntaxKind::Ident) {
                self.bump();
            } else if self.current_kind().is_some_and(|k| k.is_keyword()) {
                self.bump(); // Allow keywords as port names
            } else {
                self.error("expected port name");
            }
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
            // Parse expression first, then determine if it's assignment or expression statement
            self.parse_assignment_or_expr_stmt();
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

    /// Parse assignment or expression statement
    /// This disambiguates between assignments and standalone expressions
    fn parse_assignment_or_expr_stmt(&mut self) {
        // Save checkpoint before parsing
        let checkpoint = self.builder.checkpoint();

        // Parse left-hand side expression
        self.parse_expression();

        // Check if there's an assignment operator
        // Using unified `=` operator with context-based inference for all assignments
        if self.at(SyntaxKind::Assign) {
            // It's an assignment statement - wrap in AssignmentStmt
            self.builder.start_node_at(
                checkpoint,
                rowan::SyntaxKind(SyntaxKind::AssignmentStmt as u16),
            );

            // Consume assignment operator
            self.bump();

            // Parse right-hand side
            self.parse_expression();

            // Optional semicolon
            self.consume_semicolon();

            self.finish_node();
        } else {
            // It's an expression statement - wrap in ExprStmt
            self.builder
                .start_node_at(checkpoint, rowan::SyntaxKind(SyntaxKind::ExprStmt as u16));

            // Optional semicolon
            self.consume_semicolon();

            self.finish_node();
        }
    }

    /// Parse assignment statement
    fn parse_assignment_stmt(&mut self) {
        self.start_node(SyntaxKind::AssignmentStmt);

        // Left-hand side (identifier or field access)
        self.parse_expression();

        // Assignment operator
        // Using unified `=` operator with context-based inference for all assignments
        if self.at(SyntaxKind::Assign) {
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

            // Parse optional attributes before statement
            // e.g., #[parallel] match sel { ... }
            self.parse_attributes();
            self.skip_trivia();

            match self.current_kind() {
                Some(SyntaxKind::IfKw) => self.parse_if_statement(),
                Some(SyntaxKind::MatchKw) => self.parse_match_statement(),
                Some(SyntaxKind::ForKw) => self.parse_for_stmt(),
                Some(SyntaxKind::GenerateKw) => self.parse_generate_stmt(),
                Some(SyntaxKind::FlowKw) => self.parse_flow_statement(),
                Some(SyntaxKind::LetKw) => self.parse_let_statement(),
                Some(SyntaxKind::AssertKw) => self.parse_assert_statement(),
                Some(SyntaxKind::PropertyKw) => self.parse_property_statement(),
                Some(SyntaxKind::CoverKw) => self.parse_cover_statement(),
                Some(SyntaxKind::SequenceKw) => self.parse_sequence_statement(),
                Some(SyntaxKind::AssumeKw) => self.parse_assume_statement(),
                Some(SyntaxKind::ExpectKw) => self.parse_expect_statement(),
                Some(SyntaxKind::ReturnKw) => self.parse_return_statement(),
                Some(SyntaxKind::Ident) => self.parse_assignment_or_statement(),
                Some(SyntaxKind::LBrace) => self.parse_block_statement(),
                Some(SyntaxKind::LParen) => {
                    // Handle tuple expression as statement (e.g., implicit return: (y, x))
                    // Bug #85 fix: Support tuple expressions in function bodies for implicit returns
                    self.start_node(SyntaxKind::ExprStmt);
                    self.parse_expression();
                    self.consume_semicolon();
                    self.finish_node();
                }
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

        // Parse condition expression (parentheses are part of the expression itself)
        // Bug #79 fix: Don't try to handle optional parens specially - the expression
        // parser handles them naturally. This fixes parsing of complex expressions like:
        // if (temp & 0xFFFF0000) == 0 { ... }
        self.parse_expression();

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

    /// Parse let statement
    /// Syntax: let [mut] pattern [: type] = value [;]
    /// Supports: let x = expr, let mut x = expr, let (a, b) = expr
    fn parse_let_statement(&mut self) {
        self.start_node(SyntaxKind::LetStmt);

        // 'let' keyword
        self.expect(SyntaxKind::LetKw);

        // Optional 'mut' keyword (Bug #78 fix)
        if self.at(SyntaxKind::MutKw) {
            self.bump(); // consume 'mut'
        }

        // Pattern (identifier or tuple destructuring)
        self.parse_pattern();

        // Optional type annotation: : type
        if self.at(SyntaxKind::Colon) {
            self.bump(); // consume ':'
            self.parse_type();
        }

        // Assignment operator '='
        self.expect(SyntaxKind::Assign);

        // Initializer expression
        self.parse_expression();

        // Optional semicolon (SKALP allows both with and without)
        self.consume_semicolon();

        self.finish_node();
    }

    /// Parse if expression (for use in expressions, not statements)
    fn parse_if_expression(&mut self) {
        self.start_node(SyntaxKind::IfExpr);

        self.expect(SyntaxKind::IfKw);
        // Condition expression (without parens for expression form)
        self.parse_expression();

        // Then block (can be a block expression with statements)
        self.expect(SyntaxKind::LBrace);
        self.parse_block_expression();
        self.expect(SyntaxKind::RBrace);

        // Else block (required for expressions to have a value)
        self.expect(SyntaxKind::ElseKw);

        // Check if else is followed by another if (else if chain)
        if self.at(SyntaxKind::IfKw) {
            self.parse_if_expression();
        } else {
            self.expect(SyntaxKind::LBrace);
            self.parse_block_expression();
            self.expect(SyntaxKind::RBrace);
        }

        self.finish_node();
    }

    /// Parse block expression: { stmt1; stmt2; final_expr }
    /// A block expression contains zero or more statements followed by an expression
    fn parse_block_expression(&mut self) {
        self.start_node(SyntaxKind::BlockExpr);

        // Parse statements and expressions until we hit the closing brace
        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            if self.at(SyntaxKind::RBrace) {
                break;
            }

            // Parse optional attributes before statement/expression
            // e.g., #[parallel] match sel { ... }
            self.parse_attributes();
            self.skip_trivia();

            // Check if this looks like a statement (starts with a statement keyword)
            match self.current_kind() {
                Some(SyntaxKind::LetKw) => {
                    self.parse_let_statement();
                }
                Some(SyntaxKind::ReturnKw) => {
                    self.parse_return_statement();
                }
                Some(SyntaxKind::IfKw) => {
                    // Bug #80/#81 fix: Determine if this is an if-statement or if-expression
                    // - If has no else → must be statement (parse as statement)
                    // - If has else → could be expression or statement, let parse_expression() handle it
                    // We peek ahead to check if there's an else after the if block
                    if self.is_if_without_else_lookahead() {
                        // Parse as statement (optional else)
                        self.parse_if_statement();
                    } else {
                        // Fall through to expression parsing (will handle if-expression with else)
                        // Parse as expression
                        let pos_before = self.current;
                        self.parse_expression();
                        let pos_after = self.current;

                        if pos_before == pos_after {
                            self.error("failed to parse expression in block");
                            self.bump();
                            continue;
                        }

                        if self.at(SyntaxKind::Semicolon) {
                            self.bump();
                        } else {
                            break;
                        }
                        continue;
                    }
                }
                _ => {
                    // Try to parse as expression
                    // If the expression is followed by a semicolon, it's an expression statement
                    // Otherwise, it's the final expression

                    // Save current position to detect if we're making progress
                    let pos_before = self.current;
                    self.parse_expression();
                    let pos_after = self.current;

                    // Error recovery: if we didn't consume any tokens, skip one and continue
                    if pos_before == pos_after {
                        self.error("failed to parse expression in block");
                        self.bump(); // consume one token to make progress
                        continue;
                    }

                    // Check if there's a semicolon (expression statement) or not (final expression)
                    if self.at(SyntaxKind::Semicolon) {
                        self.bump(); // consume semicolon for expression statement
                    } else {
                        // This is the final expression - stop parsing
                        break;
                    }
                }
            }
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
            self.skip_trivia();
            if self.at(SyntaxKind::RBrace) {
                break;
            }
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
            self.skip_trivia();
            if self.at(SyntaxKind::RBrace) {
                break;
            }
            self.parse_match_arm_expression();
        }
        self.finish_node();

        self.expect(SyntaxKind::RBrace);

        self.finish_node();
    }

    /// Parse closure expression: |param: Type| { body }
    fn parse_closure_expression(&mut self) {
        self.start_node(SyntaxKind::ClosureExpr);

        // Expect opening pipe
        self.expect(SyntaxKind::Pipe);

        // Parse parameter list
        self.start_node(SyntaxKind::ClosureParamList);

        // Parse first parameter if present
        if !self.at(SyntaxKind::Pipe) {
            self.parse_closure_param();

            // Parse additional parameters
            while self.at(SyntaxKind::Comma) {
                self.bump(); // consume ','
                if !self.at(SyntaxKind::Pipe) {
                    self.parse_closure_param();
                }
            }
        }

        self.finish_node(); // ClosureParamList

        // Expect closing pipe
        self.expect(SyntaxKind::Pipe);

        // Parse body (block with expression)
        if self.at(SyntaxKind::LBrace) {
            self.bump(); // consume '{'
            self.parse_expression(); // Parse the body expression
            self.expect(SyntaxKind::RBrace);
        } else {
            self.error("expected block expression after closure parameters");
        }

        self.finish_node(); // ClosureExpr
    }

    /// Parse closure parameter: param: Type
    fn parse_closure_param(&mut self) {
        self.start_node(SyntaxKind::ClosureParam);

        // Parse parameter name
        self.expect(SyntaxKind::Ident);

        // Parse type annotation
        if self.at(SyntaxKind::Colon) {
            self.bump(); // consume ':'
            self.parse_type();
        }

        self.finish_node();
    }

    /// Parse for statement: for i in 0..3 { ... }
    fn parse_for_stmt(&mut self) {
        self.start_node(SyntaxKind::ForStmt);

        self.expect(SyntaxKind::ForKw);

        // Parse loop variable identifier
        self.expect(SyntaxKind::Ident);

        self.expect(SyntaxKind::InKw);

        // Parse range expression
        self.parse_range_expr();

        // Parse loop body
        self.expect(SyntaxKind::LBrace);

        // Parse statements in the loop body
        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();
            if self.at(SyntaxKind::RBrace) {
                break;
            }

            // Parse any statement that can appear in a block
            match self.current_kind() {
                Some(SyntaxKind::IfKw) => self.parse_if_statement(),
                Some(SyntaxKind::MatchKw) => self.parse_match_statement(),
                Some(SyntaxKind::ForKw) => self.parse_for_stmt(), // Nested for loops
                Some(SyntaxKind::LetKw) => {
                    // Disambiguate between instance declaration and let binding
                    // Instance: let name = EntityName { ... } or let name = EntityName<T> { ... }
                    // Let binding: let name = expression
                    let mut is_instance = false;
                    if self.peek_kind(1) == Some(SyntaxKind::Ident)
                        && self.peek_kind(2) == Some(SyntaxKind::Assign)
                        && self.peek_kind(3) == Some(SyntaxKind::Ident)
                    {
                        // Check if followed by { or <
                        if self.peek_kind(4) == Some(SyntaxKind::LBrace) {
                            is_instance = true;
                        } else if self.peek_kind(4) == Some(SyntaxKind::Lt) {
                            // Generic instantiation - scan forward to find the closing >
                            // then check if there's a { after it
                            let mut depth = 0;
                            let mut offset = 4;
                            loop {
                                match self.peek_kind(offset) {
                                    Some(SyntaxKind::Lt) => depth += 1,
                                    Some(SyntaxKind::Gt) => {
                                        depth -= 1;
                                        if depth == 0 {
                                            // Found closing >, check next token
                                            if self.peek_kind(offset + 1)
                                                == Some(SyntaxKind::LBrace)
                                            {
                                                is_instance = true;
                                            }
                                            break;
                                        }
                                    }
                                    Some(SyntaxKind::Shr) => {
                                        // >> counts as two >
                                        depth -= 2;
                                        if depth <= 0 {
                                            break;
                                        }
                                    }
                                    None => break,
                                    _ => {}
                                }
                                offset += 1;
                                if offset > 50 {
                                    break; // Safety limit
                                }
                            }
                        }
                    }

                    if is_instance {
                        self.parse_instance_decl()
                    } else {
                        self.parse_let_statement()
                    }
                }
                Some(SyntaxKind::SignalKw) => self.parse_signal_decl(),
                Some(SyntaxKind::ReturnKw) => self.parse_return_statement(),
                Some(SyntaxKind::Ident) => self.parse_assignment_or_statement(),
                _ => {
                    self.error("unexpected token in for loop body");
                    self.bump(); // Consume the unexpected token to avoid infinite loop
                }
            }
        }

        self.expect(SyntaxKind::RBrace);
        self.finish_node();
    }

    /// Parse generate statement: generate for/if/match
    /// Dispatches to specific generate variant based on next token
    fn parse_generate_stmt(&mut self) {
        // Peek at the token after 'generate' to determine the variant
        let next = self.peek_kind(1);
        match next {
            Some(SyntaxKind::ForKw) => self.parse_generate_for_stmt(),
            Some(SyntaxKind::IfKw) => self.parse_generate_if_stmt(),
            Some(SyntaxKind::MatchKw) => self.parse_generate_match_stmt(),
            _ => {
                // Error: generate must be followed by for, if, or match
                self.start_node(SyntaxKind::GenerateForStmt);
                self.expect(SyntaxKind::GenerateKw);
                self.error("expected 'for', 'if', or 'match' after 'generate'");
                self.finish_node();
            }
        }
    }

    /// Parse generate for statement: generate for i in 0..N [step S] { ... }
    fn parse_generate_for_stmt(&mut self) {
        self.start_node(SyntaxKind::GenerateForStmt);

        self.expect(SyntaxKind::GenerateKw);
        self.expect(SyntaxKind::ForKw);

        // Parse loop variable identifier
        self.expect(SyntaxKind::Ident);

        self.expect(SyntaxKind::InKw);

        // Parse range expression
        self.parse_range_expr();

        // Parse optional step expression: step N
        if self.at(SyntaxKind::StepKw) {
            self.bump(); // consume 'step'
            self.parse_expression(); // parse step value
        }

        // Parse loop body
        self.expect(SyntaxKind::LBrace);
        self.parse_generate_body();
        self.expect(SyntaxKind::RBrace);

        self.finish_node();
    }

    /// Parse generate if statement: generate if COND { ... } else { ... }
    fn parse_generate_if_stmt(&mut self) {
        self.start_node(SyntaxKind::GenerateIfStmt);

        self.expect(SyntaxKind::GenerateKw);
        self.expect(SyntaxKind::IfKw);

        // Parse condition expression
        self.parse_expression();

        // Parse then block
        self.expect(SyntaxKind::LBrace);
        self.parse_generate_body();
        self.expect(SyntaxKind::RBrace);

        // Parse optional else block
        if self.at(SyntaxKind::ElseKw) {
            self.bump(); // consume 'else'

            // Could be 'else if' or 'else { }'
            if self.at(SyntaxKind::IfKw) {
                // else if - recursively parse another if
                self.expect(SyntaxKind::IfKw);
                self.parse_expression();
                self.expect(SyntaxKind::LBrace);
                self.parse_generate_body();
                self.expect(SyntaxKind::RBrace);
            } else {
                // else { }
                self.expect(SyntaxKind::LBrace);
                self.parse_generate_body();
                self.expect(SyntaxKind::RBrace);
            }
        }

        self.finish_node();
    }

    /// Parse generate match statement: generate match VALUE { ... }
    fn parse_generate_match_stmt(&mut self) {
        self.start_node(SyntaxKind::GenerateMatchStmt);

        self.expect(SyntaxKind::GenerateKw);
        self.expect(SyntaxKind::MatchKw);

        // Parse the value to match on
        self.parse_expression();

        // Parse match body
        self.expect(SyntaxKind::LBrace);

        // Parse match arms
        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();
            if self.at(SyntaxKind::RBrace) {
                break;
            }
            self.parse_generate_match_arm();
        }

        self.expect(SyntaxKind::RBrace);
        self.finish_node();
    }

    /// Parse a generate match arm: pattern => { statements }
    fn parse_generate_match_arm(&mut self) {
        self.start_node(SyntaxKind::MatchArm);

        // Parse pattern
        self.parse_pattern();

        // Expect arrow (-> or =>)
        if !self.at(SyntaxKind::Arrow) && !self.at(SyntaxKind::FatArrow) {
            self.error("Expected '->' or '=>' after pattern");
        } else {
            self.bump(); // consume arrow
        }

        // Parse arm body (must be a block for generate match)
        self.expect(SyntaxKind::LBrace);
        self.parse_generate_body();
        self.expect(SyntaxKind::RBrace);

        // Optional comma
        if self.at(SyntaxKind::Comma) {
            self.bump();
        }

        self.finish_node();
    }

    /// Parse the body of a generate block (signals, variables, on blocks, etc.)
    fn parse_generate_body(&mut self) {
        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();
            if self.at(SyntaxKind::RBrace) {
                break;
            }

            // Parse optional attributes before item
            self.parse_attributes();
            self.skip_trivia();

            match self.current_kind() {
                Some(SyntaxKind::SignalKw) => {
                    self.parse_signal_decl();
                    if self.at(SyntaxKind::Semicolon) {
                        self.bump();
                    }
                }
                Some(SyntaxKind::VarKw) => {
                    self.parse_variable_decl();
                    if self.at(SyntaxKind::Semicolon) {
                        self.bump();
                    }
                }
                Some(SyntaxKind::ConstKw) => {
                    if self.peek_kind(1) == Some(SyntaxKind::FnKw) {
                        self.parse_impl_function()
                    } else {
                        self.parse_constant_decl();
                        if self.at(SyntaxKind::Semicolon) {
                            self.bump();
                        }
                    }
                }
                Some(SyntaxKind::FnKw) => self.parse_impl_function(),
                Some(SyntaxKind::LetKw) => {
                    // Check for instance declaration pattern
                    let mut is_instance = false;
                    if self.peek_kind(1) == Some(SyntaxKind::Ident)
                        && self.peek_kind(2) == Some(SyntaxKind::Assign)
                        && self.peek_kind(3) == Some(SyntaxKind::Ident)
                    {
                        if self.peek_kind(4) == Some(SyntaxKind::LBrace) {
                            is_instance = true;
                        } else if self.peek_kind(4) == Some(SyntaxKind::Lt) {
                            // Generic instantiation check
                            let mut depth = 0;
                            let mut offset = 4;
                            loop {
                                match self.peek_kind(offset) {
                                    Some(SyntaxKind::Lt) => depth += 1,
                                    Some(SyntaxKind::Gt) => {
                                        depth -= 1;
                                        if depth == 0 {
                                            if self.peek_kind(offset + 1)
                                                == Some(SyntaxKind::LBrace)
                                            {
                                                is_instance = true;
                                            }
                                            break;
                                        }
                                    }
                                    Some(SyntaxKind::Shr) => {
                                        depth -= 2;
                                        if depth <= 0 {
                                            break;
                                        }
                                    }
                                    None => break,
                                    _ => {}
                                }
                                offset += 1;
                                if offset > 20 {
                                    break;
                                }
                            }
                        }
                    }

                    if is_instance {
                        self.parse_instance_decl()
                    } else {
                        self.parse_let_statement()
                    }
                }
                Some(SyntaxKind::OnKw) => self.parse_event_block(),
                Some(SyntaxKind::MatchKw) => self.parse_match_statement(),
                Some(SyntaxKind::ForKw) => self.parse_for_stmt(),
                Some(SyntaxKind::GenerateKw) => self.parse_generate_stmt(), // Nested generate
                Some(SyntaxKind::FlowKw) => self.parse_flow_statement(),
                Some(SyntaxKind::AssignKw) => self.parse_continuous_assignment(),
                Some(SyntaxKind::IfKw) => self.parse_if_statement(),
                Some(SyntaxKind::Ident) => self.parse_assignment_or_statement(),
                _ => {
                    self.error("unexpected token in generate block");
                    self.bump();
                }
            }
        }
    }

    /// Parse range expression: 0..3 or 0..=10
    fn parse_range_expr(&mut self) {
        self.start_node(SyntaxKind::RangeExpr);

        // Parse start expression (could be literal or identifier)
        self.parse_expression();

        // Parse range operator (.. or ..=)
        if self.at(SyntaxKind::DotDotEq) {
            self.bump(); // consume ..=
        } else if self.at(SyntaxKind::DotDot) {
            self.bump(); // consume ..
        } else {
            self.error("expected '..' or '..='");
        }

        // Parse end expression
        self.parse_expression();

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
            // Save position for error recovery
            let pos_before = self.current;

            match self.current_kind() {
                Some(SyntaxKind::Ident)
                | Some(SyntaxKind::OutputKw)
                | Some(SyntaxKind::InputKw)
                | Some(SyntaxKind::InoutKw)
                | Some(SyntaxKind::SignalKw)
                | Some(SyntaxKind::VarKw) => {
                    // Could be assignment or expression
                    // Look ahead to see if there's an assignment operator
                    let next = self.peek_kind(1);
                    // Using unified `=` operator with context-based inference for all assignments
                    if next == Some(SyntaxKind::Assign)
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

            // Error recovery: if we didn't consume any tokens, skip one to make progress
            // But don't skip if we're at a token that might be valid syntax
            if pos_before == self.current
                && !self.at(SyntaxKind::Comma)
                && !self.at(SyntaxKind::RBrace)
                && !self.at(SyntaxKind::Arrow)
                && !self.at(SyntaxKind::FatArrow)
                && !self.is_at_end()
            {
                self.error("failed to parse match arm body");
                self.bump(); // consume one token to avoid infinite loop
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

        // Parse arm body - can be a block expression or a simple expression
        // Save position for error recovery
        let pos_before = self.current;

        // Check if this is a block expression
        if self.at(SyntaxKind::LBrace) {
            self.bump(); // consume '{'
            self.parse_block_expression();
            self.expect(SyntaxKind::RBrace);
        } else {
            self.parse_expression();

            // Error recovery: if we didn't consume any tokens, skip one to make progress
            // But don't skip if we're at a token that might be valid syntax
            if pos_before == self.current
                && !self.at(SyntaxKind::Comma)
                && !self.at(SyntaxKind::RBrace)
                && !self.at(SyntaxKind::Arrow)
                && !self.at(SyntaxKind::FatArrow)
                && !self.is_at_end()
            {
                self.error("failed to parse match arm expression");
                self.bump(); // consume one token to avoid infinite loop
            }
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
                        // Could be identifier pattern or path pattern (Enum::Variant)
                        self.start_node(SyntaxKind::IdentPattern);
                        self.bump(); // consume first identifier

                        // Check for path pattern (::)
                        while self.at(SyntaxKind::ColonColon) {
                            self.bump(); // consume ::
                            self.expect(SyntaxKind::Ident); // consume path segment
                        }

                        // Check for tuple destructuring: State::Active(n)
                        if self.at(SyntaxKind::LParen) {
                            // This is a tuple struct pattern
                            self.finish_node(); // finish the path part
                            self.start_node(SyntaxKind::TuplePattern);
                            self.bump(); // consume '('

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
                        } else {
                            self.finish_node();
                        }
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
            | Some(SyntaxKind::StringLiteral)
            | Some(SyntaxKind::TrueKw)
            | Some(SyntaxKind::FalseKw) => {
                // Literal pattern (including boolean literals)
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
    /// Supports: assert(cond), assert!(cond), assert(cond, "msg"),
    ///           assert!(cond, severity: Error), assert!(cond, severity: Warning, "msg")
    fn parse_assert_statement(&mut self) {
        self.start_node(SyntaxKind::AssertStmt);

        // 'assert' keyword
        self.expect(SyntaxKind::AssertKw);

        // Optional '!' for macro-style (assert! vs assert)
        if self.at(SyntaxKind::Bang) {
            self.bump(); // consume '!'
        }

        // Expect opening parenthesis
        self.expect(SyntaxKind::LParen);

        // Parse condition expression
        self.parse_expression();

        // Optional arguments: severity and/or message
        // Format: assert!(cond, severity: Error, "message")
        // or: assert!(cond, "message")
        // or: assert!(cond, severity: Warning)
        while self.at(SyntaxKind::Comma) {
            self.bump(); // consume comma

            // Check for severity: keyword
            if self.at(SyntaxKind::Ident) && self.current_text() == Some("severity") {
                self.start_node(SyntaxKind::SeveritySpec);
                self.bump(); // consume 'severity'
                self.expect(SyntaxKind::Colon);
                // Parse severity level (Info, Warning, Error, Fatal)
                if self.at(SyntaxKind::Ident) {
                    self.bump(); // consume severity level
                }
                self.finish_node();
            } else {
                // Message expression
                self.parse_expression();
            }
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
    /// Supports:
    /// - cover property(temporal_expr); -- SVA style
    /// - cover!(cond); -- simple macro style
    /// - cover!(cond, "message"); -- with message
    fn parse_cover_statement(&mut self) {
        // Check if this is cover! (macro style) or cover property (SVA style)
        // by peeking ahead
        let is_macro_style = self.peek_kind(1) == Some(SyntaxKind::Bang);

        if is_macro_style {
            // Simple cover!(cond) form
            self.start_node(SyntaxKind::CoverMacroStmt);

            self.expect(SyntaxKind::CoverKw);
            self.expect(SyntaxKind::Bang); // consume '!'
            self.expect(SyntaxKind::LParen);

            // Parse condition expression
            self.parse_expression();

            // Optional arguments: message
            while self.at(SyntaxKind::Comma) {
                self.bump(); // consume comma

                // Check for severity: keyword (for consistency with assert/assume)
                if self.at(SyntaxKind::Ident) && self.current_text() == Some("severity") {
                    self.start_node(SyntaxKind::SeveritySpec);
                    self.bump(); // consume 'severity'
                    self.expect(SyntaxKind::Colon);
                    if self.at(SyntaxKind::Ident) {
                        self.bump(); // consume severity level
                    }
                    self.finish_node();
                } else {
                    // Message expression
                    self.parse_expression();
                }
            }

            self.expect(SyntaxKind::RParen);
            self.consume_semicolon();
            self.finish_node();
        } else {
            // SVA-style cover property(...)
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
    /// Supports:
    /// - assume property(temporal_expr); -- SVA style
    /// - assume!(cond); -- simple macro style
    /// - assume!(cond, "message"); -- with message
    fn parse_assume_statement(&mut self) {
        // Check if this is assume! (macro style) or assume property (SVA style)
        // by peeking ahead
        let is_macro_style = self.peek_kind(1) == Some(SyntaxKind::Bang);

        if is_macro_style {
            // Simple assume!(cond) form
            self.start_node(SyntaxKind::AssumeMacroStmt);

            self.expect(SyntaxKind::AssumeKw);
            self.expect(SyntaxKind::Bang); // consume '!'
            self.expect(SyntaxKind::LParen);

            // Parse condition expression
            self.parse_expression();

            // Optional arguments: severity and/or message
            while self.at(SyntaxKind::Comma) {
                self.bump(); // consume comma

                // Check for severity: keyword
                if self.at(SyntaxKind::Ident) && self.current_text() == Some("severity") {
                    self.start_node(SyntaxKind::SeveritySpec);
                    self.bump(); // consume 'severity'
                    self.expect(SyntaxKind::Colon);
                    if self.at(SyntaxKind::Ident) {
                        self.bump(); // consume severity level
                    }
                    self.finish_node();
                } else {
                    // Message expression
                    self.parse_expression();
                }
            }

            self.expect(SyntaxKind::RParen);
            self.consume_semicolon();
            self.finish_node();
        } else {
            // SVA-style assume property(...)
            self.start_node(SyntaxKind::AssumeStmt);

            self.expect(SyntaxKind::AssumeKw);
            self.expect(SyntaxKind::PropertyKw);

            // Property expression in parentheses
            self.expect(SyntaxKind::LParen);
            self.parse_temporal_expression();
            self.expect(SyntaxKind::RParen);

            self.expect(SyntaxKind::Semicolon);
            self.finish_node();
        }
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
                Some(SyntaxKind::SafetyKw) => self.parse_safety_kv_pair(),
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
    /// Supports both block form and single-line form:
    ///   Block form: `intent low_power { mux_style: priority, ... }`
    ///   Single-line: `intent parallel = mux_style::parallel;`
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

        // Check for single-line form: `intent parallel = mux_style::parallel;`
        if self.at(SyntaxKind::Assign) {
            self.bump(); // consume '='
            self.parse_intent_value();
            self.expect(SyntaxKind::Semicolon);
        } else {
            // Block form
            self.expect(SyntaxKind::LBrace);
            self.parse_intent_constraints();
            self.expect(SyntaxKind::RBrace);
        }

        self.finish_node();
    }

    /// Parse intent value expression (for single-line intents)
    /// Supports: `mux_style::parallel`, `foo + bar`, `mux_style::parallel + timing::critical`
    fn parse_intent_value(&mut self) {
        self.start_node(SyntaxKind::IntentValue);

        // Parse first term (either path like mux_style::parallel or identifier like parallel)
        self.parse_intent_term();

        // Handle composition with + operator
        while self.at(SyntaxKind::Plus) {
            self.bump(); // consume '+'
            self.parse_intent_term();
        }

        self.finish_node();
    }

    /// Parse a single intent term: either a path (mux_style::parallel), identifier (parallel),
    /// or attribute with argument (unroll(4))
    fn parse_intent_term(&mut self) {
        // Parse identifier or keyword-as-ident (namespace or intent name)
        // Keywords like 'isolation' can be used as attribute names
        if self.at(SyntaxKind::Ident) || self.at_keyword_as_ident() {
            self.bump();

            // Check for path separator ::
            while self.at(SyntaxKind::ColonColon) {
                self.bump(); // consume '::'
                self.expect(SyntaxKind::Ident);
            }

            // Check for function-call style argument: unroll(4), pipeline(stages=3), pipeline(stages=3, target_freq=100_000_000)
            if self.at(SyntaxKind::LParen) {
                self.bump(); // consume '('

                // Parse arguments which can be:
                // - Simple value: unroll(4)
                // - Key-value pair: pipeline(stages=3)
                // - Multiple key-value pairs: pipeline(stages=3, target_freq=100_000_000)
                // - Boolean keyword: pipeline(stages=2, auto_balance=true)
                while !self.at(SyntaxKind::RParen) && !self.is_at_end() {
                    self.skip_trivia();

                    if self.at(SyntaxKind::RParen) {
                        break;
                    }

                    // Parse identifier or keyword (could be key name or standalone value)
                    // Keywords like 'group', 'from', 'type' can be used as attribute keys
                    if self.at(SyntaxKind::Ident) || self.at_keyword_as_ident() {
                        self.bump(); // consume identifier/keyword
                        self.skip_trivia();

                        // Check for '=' indicating key-value pair
                        // Note: '=' is tokenized as Assign, not Eq
                        if self.at(SyntaxKind::Assign) {
                            self.bump(); // consume '='
                            self.skip_trivia();

                            // Parse the value (IntLiteral, Ident, Lifetime, StringLiteral, keyword, etc.)
                            if self.at(SyntaxKind::IntLiteral) {
                                self.bump();
                                self.skip_trivia();
                            } else if self.at(SyntaxKind::Ident) {
                                // Handle identifiers like domain names
                                self.bump();
                                self.skip_trivia();
                            } else if self.at_keyword_as_ident() {
                                // Handle keywords used as values (e.g., cdc_type = gray)
                                self.bump();
                                self.skip_trivia();
                            } else if self.at(SyntaxKind::Lifetime) {
                                // Handle lifetime-style domain references: from = 'clk_fast
                                self.bump();
                                self.skip_trivia();
                            } else if self.at(SyntaxKind::StringLiteral) {
                                // Handle string literals: group = "control", display_name = "FSM State"
                                self.bump();
                                self.skip_trivia();
                            } else if self.at(SyntaxKind::TrueKw) || self.at(SyntaxKind::FalseKw) {
                                self.bump();
                                self.skip_trivia();
                            }
                        }
                    } else if self.at(SyntaxKind::IntLiteral) {
                        // Simple numeric value like unroll(4)
                        self.bump();
                        self.skip_trivia();
                    } else if self.at(SyntaxKind::StringLiteral) {
                        // Simple string value
                        self.bump();
                        self.skip_trivia();
                    } else if self.at(SyntaxKind::TrueKw) || self.at(SyntaxKind::FalseKw) {
                        self.bump();
                        self.skip_trivia();
                    }

                    // Handle comma separator for multiple arguments
                    if self.at(SyntaxKind::Comma) {
                        self.bump(); // consume ','
                        self.skip_trivia();
                    } else if !self.at(SyntaxKind::RParen) {
                        break; // unexpected token, stop parsing
                    }
                }

                self.expect(SyntaxKind::RParen);
            }
        } else {
            self.error("expected intent name or namespace::value");
        }
    }

    /// Parse attribute list: zero or more `#[...]` attributes
    /// Returns true if any attributes were parsed
    fn parse_attributes(&mut self) -> bool {
        let mut found_any = false;

        while self.at(SyntaxKind::HashBracket) {
            found_any = true;
            self.parse_attribute();
        }

        found_any
    }

    /// Parse a single attribute: `#[name]`, `#[name, name2]`, or `#[name + name2]`
    fn parse_attribute(&mut self) {
        self.start_node(SyntaxKind::Attribute);

        // Consume '#['
        self.expect(SyntaxKind::HashBracket);

        // Parse the attribute content (intent value with optional composition)
        self.parse_attribute_content();

        // Handle comma-separated list: #[parallel, critical]
        while self.at(SyntaxKind::Comma) {
            self.bump(); // consume ','
            self.skip_trivia();
            if !self.at(SyntaxKind::RBracket) {
                self.parse_attribute_content();
            }
        }

        // Consume ']'
        self.expect(SyntaxKind::RBracket);

        self.finish_node();
    }

    /// Parse attribute content: either an intent path or composition
    /// Supports: `parallel`, `mux_style::parallel`, `parallel + critical`
    fn parse_attribute_content(&mut self) {
        // Reuse intent value parsing which handles paths and + composition
        self.parse_intent_value();
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
            // Allow both identifiers and keywords (like 'area', 'performance') as constraint names
            if self.current_kind() == Some(SyntaxKind::Ident) || self.at_keyword_as_ident() {
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
        // Allow keywords to be used as constraint names
        if self.current_kind() == Some(SyntaxKind::Ident) {
            self.expect(SyntaxKind::Ident);
        } else if self.at_keyword_as_ident() {
            // Bump any keyword being used as a constraint name
            self.bump();
        } else {
            self.error("expected constraint name");
        }

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

        // Optional units or additional info on the same line
        // Stop at newlines to avoid consuming the next constraint's keyword
        while self.current_kind() == Some(SyntaxKind::Ident) && !self.at_newline() {
            self.bump();
        }
    }

    /// Check if we're at a newline by looking ahead in trivia
    fn at_newline(&self) -> bool {
        if self.current >= self.tokens.len() {
            return false;
        }
        // Check the trivia before current position for newlines
        let current_pos = self.current;
        if current_pos == 0 {
            return false;
        }
        // Look at the trivia between last token and current position
        let last_token_end = if current_pos > 0 {
            self.tokens[current_pos - 1].span.end
        } else {
            0
        };
        let current_token_start = if current_pos < self.tokens.len() {
            self.tokens[current_pos].span.start
        } else {
            self.source.len()
        };
        let trivia = &self.source[last_token_end..current_token_start];
        trivia.contains('\n')
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

    /// Parse type alias declaration: `pub type Name<T> = Type;`
    fn parse_type_alias(&mut self) {
        self.start_node(SyntaxKind::TypeAlias);

        // 'type' keyword
        self.expect(SyntaxKind::TypeKw);

        // Type alias name
        self.expect(SyntaxKind::Ident);

        // Optional generic parameters
        if self.at(SyntaxKind::Lt) {
            self.parse_generic_params();
        }

        // '=' sign
        self.expect(SyntaxKind::Assign);

        // Target type
        self.parse_type();

        // Semicolon
        self.expect(SyntaxKind::Semicolon);

        self.finish_node();
    }

    /// Parse function declaration: `pub fn name<T: Trait>(params) -> RetType { body }`
    fn parse_function_decl(&mut self) {
        self.start_node(SyntaxKind::FunctionDecl);

        // 'fn' keyword
        self.expect(SyntaxKind::FnKw);

        // Function name
        self.expect(SyntaxKind::Ident);

        // Optional generic parameters with bounds
        if self.at(SyntaxKind::Lt) {
            self.parse_generic_params();
        }

        // Parameter list
        self.expect(SyntaxKind::LParen);
        self.parse_parameter_list();
        self.expect(SyntaxKind::RParen);

        // Optional return type
        if self.at(SyntaxKind::Arrow) {
            self.bump();
            self.parse_type();
        }

        // Optional where clause
        if self.at(SyntaxKind::WhereKw) {
            self.parse_where_clause();
        }

        // Function body
        self.parse_block_statement();

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

            // Consume optional separator (semicolon or comma)
            if self.at(SyntaxKind::Semicolon) || self.at(SyntaxKind::Comma) {
                self.bump();
            }
        }

        self.finish_node();
    }

    /// Parse a single struct field
    fn parse_struct_field(&mut self) {
        self.start_node(SyntaxKind::StructField);

        // Optional visibility modifier
        if self.at(SyntaxKind::PubKw) {
            self.parse_visibility();
        }

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

        // Optional discriminant value: NAME = expr
        if self.at(SyntaxKind::Assign) {
            self.bump(); // consume =
            self.parse_expression(); // parse discriminant value
        }
        // Optional variant data
        else if self.at(SyntaxKind::LParen) {
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

            // Consume optional separator (semicolon or comma)
            if self.at(SyntaxKind::Semicolon) || self.at(SyntaxKind::Comma) {
                self.bump();
            }
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
    /// Supports both positional args (`32`) and named args (`WIDTH: 32`)
    fn parse_type_or_const_arg(&mut self) {
        // Check for named argument syntax: IDENT `:` VALUE
        // Look ahead: if we have Ident followed by Colon, it's a named argument
        if self.current_kind() == Some(SyntaxKind::Ident)
            && self.peek_kind(1) == Some(SyntaxKind::Colon)
        {
            // Named argument: e.g., `WIDTH: 32` or `T: fp32`
            self.start_node(SyntaxKind::NamedArg);
            self.bump(); // consume the name identifier
            self.expect(SyntaxKind::Colon); // consume the colon
            // Parse the value (can be a literal, identifier, or type)
            self.parse_named_arg_value();
            self.finish_node();
            return;
        }

        // Positional argument (existing logic)
        self.start_node(SyntaxKind::Arg);

        // Check if it's a literal or boolean keyword (const argument)
        // Use simple expression to avoid consuming > as comparison operator
        if self.current_kind().is_some_and(|k| k.is_literal())
            || matches!(
                self.current_kind(),
                Some(SyntaxKind::TrueKw) | Some(SyntaxKind::FalseKw)
            )
        {
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

    /// Parse the value part of a named argument
    /// Can be a literal, identifier, or type
    fn parse_named_arg_value(&mut self) {
        // Check if it's a literal or boolean keyword
        if self.current_kind().is_some_and(|k| k.is_literal())
            || matches!(
                self.current_kind(),
                Some(SyntaxKind::TrueKw) | Some(SyntaxKind::FalseKw)
            )
        {
            self.start_node(SyntaxKind::LiteralExpr);
            self.bump();
            self.finish_node();
        }
        // Check if it's an identifier
        else if self.current_kind() == Some(SyntaxKind::Ident) {
            let next = self.peek_kind(1);
            if matches!(next, Some(SyntaxKind::Lt) | Some(SyntaxKind::LBracket)) {
                // Looks like a generic type
                self.parse_type();
            } else {
                // Simple identifier
                self.start_node(SyntaxKind::IdentExpr);
                self.bump();
                self.finish_node();
            }
        }
        // Otherwise parse as type
        else {
            self.parse_type();
        }
    }

    /// Parse type annotation
    fn parse_type(&mut self) {
        // Save a checkpoint BEFORE starting TypeAnnotation so we can wrap it later
        let type_checkpoint = self.builder.checkpoint();
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
            Some(SyntaxKind::StringKw) => {
                self.start_node(SyntaxKind::StringType);
                self.bump();
                // String type has no width specifier
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
            Some(SyntaxKind::Fp16Kw) => {
                self.start_node(SyntaxKind::Fp16Type);
                self.bump();
                // FP16 has fixed width (16-bit), no width specifier
                self.finish_node();
            }
            Some(SyntaxKind::Fp32Kw) => {
                self.start_node(SyntaxKind::Fp32Type);
                self.bump();
                // FP32 has fixed width (32-bit), no width specifier
                self.finish_node();
            }
            Some(SyntaxKind::Fp64Kw) => {
                self.start_node(SyntaxKind::Fp64Type);
                self.bump();
                // FP64 has fixed width (64-bit), no width specifier
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
            Some(SyntaxKind::StructKw) => {
                // Inline struct type: struct { field1: Type1, field2: Type2 }
                self.start_node(SyntaxKind::InlineStructType);
                self.bump(); // consume 'struct'
                self.expect(SyntaxKind::LBrace);
                self.parse_struct_fields();
                self.expect(SyntaxKind::RBrace);
                self.finish_node();
            }
            Some(SyntaxKind::EnumKw) => {
                // Inline enum type: enum { Variant1, Variant2(Type), Variant3 { field: Type } }
                self.start_node(SyntaxKind::InlineEnumType);
                self.bump(); // consume 'enum'
                self.expect(SyntaxKind::LBrace);
                self.parse_enum_variants();
                self.expect(SyntaxKind::RBrace);
                self.finish_node();
            }
            Some(SyntaxKind::UnionKw) => {
                // Inline union type: union { field1: Type1, field2: Type2 }
                self.start_node(SyntaxKind::InlineUnionType);
                self.bump(); // consume 'union'
                self.expect(SyntaxKind::LBrace);
                self.parse_struct_fields(); // Unions use same field syntax as structs
                self.expect(SyntaxKind::RBrace);
                self.finish_node();
            }
            _ => {
                self.error("expected type");
            }
        }

        self.finish_node(); // finish TypeAnnotation

        // After parsing the base type, check for postfix array dimensions: T[N][M]
        // This allows syntax like nat[8][4] meaning "array of 4 elements of nat[8]"
        // Each `[N]` wraps the previous type in an ArrayType node
        while self.at(SyntaxKind::LBracket) {
            // Wrap everything parsed so far in an ArrayType using the saved checkpoint
            self.builder.start_node_at(
                type_checkpoint,
                rowan::SyntaxKind(SyntaxKind::ArrayType as u16),
            );

            self.bump(); // consume [
            self.parse_expression(); // array size
            self.expect(SyntaxKind::RBracket);

            self.finish_node(); // finish ArrayType

            // Note: We reuse the same checkpoint, which means each ArrayType wraps
            // all previous content. For nat[8][4], this creates:
            // ArrayType { TypeAnnotation { NatType[8] }, 4 }
            // This is correct: "array of 4 elements of nat[8]"
        }
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
            // Support angle bracket syntax: bit<WIDTH> or int<WIDTH, SIGNED>
            self.start_node(SyntaxKind::WidthSpec);
            self.bump(); // consume <

            // Parse first argument (width)
            self.parse_type_arg_expr();

            // Parse additional arguments if present (e.g., signedness for int<W, true>)
            while self.at(SyntaxKind::Comma) {
                self.bump(); // consume comma
                self.parse_type_arg_expr();
            }

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
            Some(SyntaxKind::TrueKw) | Some(SyntaxKind::FalseKw) => {
                // Boolean literals in type arguments
                self.start_node(SyntaxKind::LiteralExpr);
                self.bump();
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
        // Check if it's an intent parameter (intent I: Intent)
        else if self.at(SyntaxKind::IntentKw) {
            self.bump(); // consume 'intent'
            self.expect(SyntaxKind::Ident); // parameter name

            // Optional type annotation (: Intent) or default value
            if self.at(SyntaxKind::Colon) {
                self.bump(); // consume ':'
                self.parse_type(); // Intent type
            }

            // Optional default value (= Intent::default())
            if self.at(SyntaxKind::Assign) {
                self.bump(); // consume =
                self.parse_type_arg_expr(); // default value
            }
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
        self.parse_ternary_expr();
    }

    /// Parse ternary conditional expression: condition ? true_expr : false_expr
    fn parse_ternary_expr(&mut self) {
        let checkpoint = self.builder.checkpoint();
        self.parse_logical_or_expr();

        if self.at(SyntaxKind::Question) {
            self.builder.start_node_at(
                checkpoint,
                rowan::SyntaxKind(SyntaxKind::TernaryExpr as u16),
            );
            self.bump(); // consume ?
            self.parse_logical_or_expr(); // true expression
            self.expect(SyntaxKind::Colon);
            self.parse_logical_or_expr(); // false expression
            self.builder.finish_node();
        }
    }

    /// Parse logical OR expression (||)
    fn parse_logical_or_expr(&mut self) {
        let mut checkpoint = self.builder.checkpoint();
        self.parse_logical_and_expr();

        while self.at(SyntaxKind::PipePipe) {
            self.builder
                .start_node_at(checkpoint, rowan::SyntaxKind(SyntaxKind::BinaryExpr as u16));
            self.bump(); // consume ||
            self.parse_logical_and_expr();
            self.builder.finish_node();
            // Take a new checkpoint for the next iteration (for left-associativity)
            checkpoint = self.builder.checkpoint();
        }
    }

    /// Parse logical AND expression (&&)
    fn parse_logical_and_expr(&mut self) {
        let mut checkpoint = self.builder.checkpoint();
        self.parse_equality_expr();

        while self.at(SyntaxKind::AmpAmp) {
            self.builder
                .start_node_at(checkpoint, rowan::SyntaxKind(SyntaxKind::BinaryExpr as u16));
            self.bump(); // consume &&
            self.parse_equality_expr();
            self.builder.finish_node();
            // Take a new checkpoint for the next iteration (for left-associativity)
            checkpoint = self.builder.checkpoint();
        }
    }

    /// Parse equality expression (== !=)
    fn parse_equality_expr(&mut self) {
        let mut checkpoint = self.builder.checkpoint();
        self.parse_relational_expr();

        while self.at(SyntaxKind::Eq) || self.at(SyntaxKind::Neq) {
            self.builder
                .start_node_at(checkpoint, rowan::SyntaxKind(SyntaxKind::BinaryExpr as u16));
            self.bump(); // consume == or !=
            self.parse_relational_expr();
            self.builder.finish_node();
            checkpoint = self.builder.checkpoint();
        }
    }

    /// Parse relational expression (< > <= >=)
    fn parse_relational_expr(&mut self) {
        let mut checkpoint = self.builder.checkpoint();
        self.parse_bitwise_or_expr();

        while matches!(
            self.current_kind(),
            Some(SyntaxKind::Lt)
                | Some(SyntaxKind::Gt)
                | Some(SyntaxKind::Le)
                | Some(SyntaxKind::Ge)
        ) {
            // Before treating < as a comparison operator, check if this might be
            // the start of a generic function call: identifier < type > (
            // This can happen in expressions like: a + helper<T>(x)
            // We already parsed 'a +', and now we're about to parse helper<T>(x)
            // But we need to avoid treating the < as a comparison operator

            // However, at this point we've already parsed the left side (e.g., 'helper')
            // So we can't lookahead for 'Ident <'. Instead, we just proceed with
            // comparison parsing. The real fix is in parse_identifier_expression
            // which handles this case when the identifier is initially parsed.

            self.builder
                .start_node_at(checkpoint, rowan::SyntaxKind(SyntaxKind::BinaryExpr as u16));
            self.bump(); // consume relational operator
            self.parse_bitwise_or_expr();
            self.builder.finish_node();
            checkpoint = self.builder.checkpoint();
        }
    }

    /// Parse bitwise OR expression (|)
    fn parse_bitwise_or_expr(&mut self) {
        let mut checkpoint = self.builder.checkpoint();
        self.parse_bitwise_xor_expr();

        while self.at(SyntaxKind::Pipe) {
            self.builder
                .start_node_at(checkpoint, rowan::SyntaxKind(SyntaxKind::BinaryExpr as u16));
            self.bump(); // consume |
            self.parse_bitwise_xor_expr();
            self.builder.finish_node();
            checkpoint = self.builder.checkpoint();
        }
    }

    /// Parse bitwise XOR expression (^)
    fn parse_bitwise_xor_expr(&mut self) {
        let mut checkpoint = self.builder.checkpoint();
        self.parse_bitwise_and_expr();

        while self.at(SyntaxKind::Caret) {
            self.builder
                .start_node_at(checkpoint, rowan::SyntaxKind(SyntaxKind::BinaryExpr as u16));
            self.bump(); // consume ^
            self.parse_bitwise_and_expr();
            self.builder.finish_node();
            checkpoint = self.builder.checkpoint();
        }
    }

    /// Parse bitwise AND expression (&)
    fn parse_bitwise_and_expr(&mut self) {
        let mut checkpoint = self.builder.checkpoint();
        self.parse_shift_expr();

        while self.at(SyntaxKind::Amp) {
            self.builder
                .start_node_at(checkpoint, rowan::SyntaxKind(SyntaxKind::BinaryExpr as u16));
            self.bump(); // consume &
            self.parse_shift_expr();
            self.builder.finish_node();
            checkpoint = self.builder.checkpoint();
        }
    }

    /// Parse shift expression (<< >>)
    fn parse_shift_expr(&mut self) {
        let mut checkpoint = self.builder.checkpoint();
        self.parse_additive_expr();

        while self.at(SyntaxKind::Shl) || self.at(SyntaxKind::Shr) {
            self.builder
                .start_node_at(checkpoint, rowan::SyntaxKind(SyntaxKind::BinaryExpr as u16));
            self.bump(); // consume << or >>
            self.parse_additive_expr();
            self.builder.finish_node();
            checkpoint = self.builder.checkpoint();
        }
    }

    /// Parse additive expression (+ -)
    fn parse_additive_expr(&mut self) {
        let mut checkpoint = self.builder.checkpoint();
        self.parse_multiplicative_expr();

        while self.at(SyntaxKind::Plus) || self.at(SyntaxKind::Minus) {
            self.builder
                .start_node_at(checkpoint, rowan::SyntaxKind(SyntaxKind::BinaryExpr as u16));
            self.bump(); // consume + or -
            self.parse_multiplicative_expr();
            self.builder.finish_node();
            checkpoint = self.builder.checkpoint();
        }
    }

    /// Parse multiplicative expression (* / %)
    fn parse_multiplicative_expr(&mut self) {
        let mut checkpoint = self.builder.checkpoint();
        self.parse_unary_expr();

        while matches!(
            self.current_kind(),
            Some(SyntaxKind::Star) | Some(SyntaxKind::Slash) | Some(SyntaxKind::Percent)
        ) {
            self.builder
                .start_node_at(checkpoint, rowan::SyntaxKind(SyntaxKind::BinaryExpr as u16));
            self.bump(); // consume *, /, or %
            self.parse_unary_expr();
            self.builder.finish_node();
            checkpoint = self.builder.checkpoint();
        }
    }

    /// Parse unary expression (! ~ -)
    fn parse_unary_expr(&mut self) {
        if matches!(
            self.current_kind(),
            Some(SyntaxKind::Bang)
                | Some(SyntaxKind::Tilde)
                | Some(SyntaxKind::Minus)
                | Some(SyntaxKind::Amp)
                | Some(SyntaxKind::Caret)
        ) {
            self.start_node(SyntaxKind::UnaryExpr);
            self.bump(); // consume unary operator
            self.parse_unary_expr(); // right-associative
            self.finish_node();
        } else {
            self.parse_cast_expr();
        }
    }

    /// Parse cast expression (expr as Type)
    /// Cast has higher precedence than binary operators but lower than postfix operations
    fn parse_cast_expr(&mut self) {
        let checkpoint = self.builder.checkpoint();
        self.parse_primary_expression();

        // Check for 'as' keyword to create a cast expression
        if self.at(SyntaxKind::AsKw) {
            self.builder
                .start_node_at(checkpoint, rowan::SyntaxKind(SyntaxKind::CastExpr as u16));
            self.bump(); // consume 'as'
            self.parse_type();
            self.builder.finish_node();
        }
    }

    /// Parse primary expression
    fn parse_primary_expression(&mut self) {
        match self.current_kind() {
            Some(
                SyntaxKind::IntLiteral
                | SyntaxKind::BinLiteral
                | SyntaxKind::HexLiteral
                | SyntaxKind::FloatLiteral
                | SyntaxKind::StringLiteral
                | SyntaxKind::TrueKw
                | SyntaxKind::FalseKw,
            ) => {
                self.start_node(SyntaxKind::LiteralExpr);
                self.bump();
                self.finish_node();
            }
            Some(SyntaxKind::Ident)
            | Some(SyntaxKind::Fp16Kw)
            | Some(SyntaxKind::Fp32Kw)
            | Some(SyntaxKind::Fp64Kw) => {
                // Check if this is a struct literal: Type { field: value, ... } or Type<T> { field: value, ... } or Type::<T> { ... }
                // Look ahead for: Type { Ident : or Type { } or Type<...> { Ident : or Type<...> { } or Type::<...> { ... }
                // Type can be Ident or type keyword (vec3, fp32, etc.)
                let mut is_struct_literal = false;
                let mut brace_offset = 1;

                // Check for turbofish: Type::<...>
                if self.peek_kind(1) == Some(SyntaxKind::ColonColon)
                    && self.peek_kind(2) == Some(SyntaxKind::Lt)
                {
                    // Scan forward to find the closing >
                    let mut depth = 0;
                    let mut offset = 2; // Start at <
                    loop {
                        match self.peek_kind(offset) {
                            Some(SyntaxKind::Lt) => depth += 1,
                            Some(SyntaxKind::Gt) => {
                                depth -= 1;
                                if depth == 0 {
                                    brace_offset = offset + 1;
                                    break;
                                }
                            }
                            Some(SyntaxKind::Shr) => {
                                // >> counts as two >
                                depth -= 2;
                                if depth <= 0 {
                                    brace_offset = offset + 1;
                                    break;
                                }
                            }
                            None => break,
                            _ => {}
                        }
                        offset += 1;
                        if offset > 50 {
                            break;
                        }
                    }
                }
                // Check for regular generic arguments: Type<...>
                else if self.peek_kind(1) == Some(SyntaxKind::Lt) {
                    // Scan forward to find the closing >
                    let mut depth = 0;
                    let mut offset = 1; // Start at <
                    loop {
                        match self.peek_kind(offset) {
                            Some(SyntaxKind::Lt) => depth += 1,
                            Some(SyntaxKind::Gt) => {
                                depth -= 1;
                                if depth == 0 {
                                    brace_offset = offset + 1;
                                    break;
                                }
                            }
                            Some(SyntaxKind::Shr) => {
                                // >> counts as two >
                                depth -= 2;
                                if depth <= 0 {
                                    brace_offset = offset + 1;
                                    break;
                                }
                            }
                            None => break,
                            _ => {}
                        }
                        offset += 1;
                        if offset > 50 {
                            break;
                        }
                    }
                }

                // Now check if there's a { at brace_offset
                if self.peek_kind(brace_offset) == Some(SyntaxKind::LBrace) {
                    // Check for struct literal patterns: { Ident : or { }
                    is_struct_literal = (self.peek_kind(brace_offset + 1)
                        == Some(SyntaxKind::Ident)
                        && self.peek_kind(brace_offset + 2) == Some(SyntaxKind::Colon))
                        || self.peek_kind(brace_offset + 1) == Some(SyntaxKind::RBrace);
                }

                if is_struct_literal {
                    self.parse_struct_literal();
                } else {
                    self.parse_identifier_expression();
                }
            }
            // Allow specific keywords to be used as identifiers in expression contexts
            // This handles cases like: in reset: reset, then later: if (reset.active)
            // But NOT expression keywords like if, match, for, etc.
            Some(kind)
                if kind.is_keyword()
                    && !matches!(
                        kind,
                        SyntaxKind::IfKw
                            | SyntaxKind::MatchKw
                            | SyntaxKind::ElseKw
                            | SyntaxKind::ReturnKw
                            | SyntaxKind::ForKw
                    ) =>
            {
                self.parse_identifier_expression();
            }
            Some(SyntaxKind::LParen) => {
                // Could be parenthesized expression (a) or tuple (a, b, c)
                let checkpoint = self.builder.checkpoint();
                self.bump(); // consume '('

                // Check for empty tuple ()
                if self.at(SyntaxKind::RParen) {
                    self.builder
                        .start_node_at(checkpoint, rowan::SyntaxKind(SyntaxKind::TupleExpr as u16));
                    self.bump(); // consume ')'
                    self.finish_node();
                } else {
                    // Parse first expression
                    self.parse_expression();

                    // Check if there's a comma (tuple) or just closing paren (parenthesized expr)
                    if self.at(SyntaxKind::Comma) {
                        // It's a tuple
                        self.builder.start_node_at(
                            checkpoint,
                            rowan::SyntaxKind(SyntaxKind::TupleExpr as u16),
                        );

                        // Parse remaining tuple elements
                        while self.at(SyntaxKind::Comma) {
                            self.bump(); // consume ','
                            if !self.at(SyntaxKind::RParen) {
                                self.parse_expression();
                            }
                        }

                        self.expect(SyntaxKind::RParen);
                        self.finish_node();
                    } else {
                        // It's a parenthesized expression
                        self.builder.start_node_at(
                            checkpoint,
                            rowan::SyntaxKind(SyntaxKind::ParenExpr as u16),
                        );
                        self.expect(SyntaxKind::RParen);
                        self.finish_node();

                        // Handle postfix operations on parenthesized expressions
                        // This allows (expr).method(), (expr)[index], (expr)(args), (expr) as Type
                        let mut has_postfix = false;
                        loop {
                            match self.current_kind() {
                                Some(SyntaxKind::Dot) => {
                                    // Field access or method call on parenthesized expression
                                    if has_postfix {
                                        self.finish_node(); // finish previous postfix node
                                    }
                                    self.start_node(SyntaxKind::FieldExpr);
                                    self.bump(); // consume dot

                                    if self.at(SyntaxKind::Ident) || self.at(SyntaxKind::IntLiteral)
                                    {
                                        self.bump(); // consume field name or tuple index
                                    } else {
                                        self.error("expected field name or tuple index after '.'");
                                    }
                                    has_postfix = true;
                                }
                                Some(SyntaxKind::LBracket) => {
                                    // Array/bit indexing on parenthesized expression
                                    if has_postfix {
                                        self.finish_node(); // finish previous postfix node
                                    }
                                    self.start_node(SyntaxKind::IndexExpr);
                                    self.bump(); // consume '['
                                    self.parse_expression();

                                    // Check for range indexing [start:end]
                                    if self.at(SyntaxKind::Colon) {
                                        self.bump(); // consume ':'
                                        self.parse_expression();
                                    }

                                    self.expect(SyntaxKind::RBracket);
                                    has_postfix = true;
                                }
                                Some(SyntaxKind::LParen) => {
                                    // Function call on parenthesized expression
                                    if has_postfix {
                                        self.finish_node(); // finish previous postfix node
                                    }
                                    self.start_node(SyntaxKind::CallExpr);
                                    self.bump(); // consume '('
                                    self.parse_argument_list();
                                    self.expect(SyntaxKind::RParen);
                                    has_postfix = true;
                                }
                                Some(SyntaxKind::AsKw) => {
                                    // Type cast on parenthesized expression
                                    if has_postfix {
                                        self.finish_node(); // finish previous postfix node
                                    }
                                    self.start_node(SyntaxKind::CastExpr);
                                    self.bump(); // consume 'as'
                                    self.parse_type();
                                    has_postfix = true;
                                }
                                _ => break,
                            }
                        }
                        // Finish the final postfix node if any
                        if has_postfix {
                            self.finish_node();
                        }
                    }
                }
            }
            Some(
                SyntaxKind::Bang
                | SyntaxKind::Tilde
                | SyntaxKind::Minus
                | SyntaxKind::Amp
                | SyntaxKind::Caret,
            ) => {
                self.start_node(SyntaxKind::UnaryExpr);
                self.bump();
                self.parse_primary_expression();
                self.finish_node();
            }
            Some(SyntaxKind::LBracket) => {
                self.parse_array_literal();
            }
            Some(SyntaxKind::LBrace) => {
                self.parse_concat_expression();
            }
            Some(SyntaxKind::IfKw) => {
                self.parse_if_expression();
            }
            Some(SyntaxKind::MatchKw) => {
                self.parse_match_expression();
            }
            Some(SyntaxKind::Pipe) => {
                // Closure expression: |param: Type| { body }
                self.parse_closure_expression();
            }
            _ => {
                self.error("expected expression");
            }
        }
    }

    /// Check if we're at a generic path expression like `Type<T>::CONST`
    /// Returns true if pattern is: Ident < ... > ::
    fn is_generic_path_expr(&self) -> bool {
        // Check if current token is identifier or keyword (for cases like fp32<IEEE754_32>::ZERO)
        if self.current_kind() != Some(SyntaxKind::Ident)
            && !self.current_kind().is_some_and(|k| k.is_keyword())
        {
            return false;
        }
        if self.peek_kind(1) != Some(SyntaxKind::Lt) {
            return false;
        }

        // Look ahead to find matching > followed by ::
        // We need to handle nesting: Type<A<B>>
        let mut depth = 0;
        let mut offset = 2; // Start after "Ident <"

        loop {
            match self.peek_kind(offset) {
                Some(SyntaxKind::Lt) => depth += 1,
                Some(SyntaxKind::Gt) => {
                    if depth == 0 {
                        // Found matching >, check for :: after it
                        return self.peek_kind(offset + 1) == Some(SyntaxKind::ColonColon);
                    }
                    depth -= 1;
                }
                Some(SyntaxKind::Shr) => {
                    // >> counts as two >
                    if depth == 0 {
                        return false; // Would close too many
                    }
                    depth -= 1;
                    if depth == 0 {
                        return self.peek_kind(offset + 1) == Some(SyntaxKind::ColonColon);
                    }
                    depth -= 1;
                }
                None => return false, // End of input
                _ => {}               // Continue scanning
            }
            offset += 1;

            // Safety limit to avoid infinite loops
            if offset > 50 {
                return false;
            }
        }
    }

    /// Check if we're at a generic function call like `function<T>(args)`
    /// Returns true if pattern is: Ident < ... > (
    fn is_generic_call_expr(&self) -> bool {
        if self.current_kind() != Some(SyntaxKind::Ident) {
            return false;
        }
        if self.peek_kind(1) != Some(SyntaxKind::Lt) {
            return false;
        }

        // Look ahead to find matching > followed by (
        // We need to handle nesting: function<A<B>>(args)
        let mut depth = 0;
        let mut offset = 2; // Start after "Ident <"

        loop {
            match self.peek_kind(offset) {
                Some(SyntaxKind::Lt) => depth += 1,
                Some(SyntaxKind::Gt) => {
                    if depth == 0 {
                        // Found matching >, check for ( after it
                        return self.peek_kind(offset + 1) == Some(SyntaxKind::LParen);
                    }
                    depth -= 1;
                }
                Some(SyntaxKind::Shr) => {
                    // >> counts as two >
                    if depth == 0 {
                        return false; // Would close too many
                    }
                    depth -= 1;
                    if depth == 0 {
                        return self.peek_kind(offset + 1) == Some(SyntaxKind::LParen);
                    }
                    depth -= 1;
                }
                None => return false, // End of input
                _ => {}               // Continue scanning
            }
            offset += 1;

            // Safety limit to avoid infinite loops
            if offset > 50 {
                return false;
            }
        }
    }

    /// Check if we're at a turbofish call like `function::<T>(args)`
    /// Returns true if pattern is: Ident :: < ... > (
    fn is_turbofish_call_expr(&self) -> bool {
        if self.current_kind() != Some(SyntaxKind::Ident) {
            return false;
        }
        if self.peek_kind(1) != Some(SyntaxKind::ColonColon) {
            return false;
        }
        if self.peek_kind(2) != Some(SyntaxKind::Lt) {
            return false;
        }

        // Look ahead to find matching > followed by (
        // We need to handle nesting: function::<A<B>>(args)
        let mut depth = 0;
        let mut offset = 3; // Start after "Ident :: <"

        loop {
            match self.peek_kind(offset) {
                Some(SyntaxKind::Lt) => depth += 1,
                Some(SyntaxKind::Gt) => {
                    if depth == 0 {
                        // Found matching >, check for ( after it
                        return self.peek_kind(offset + 1) == Some(SyntaxKind::LParen);
                    }
                    depth -= 1;
                }
                Some(SyntaxKind::Shr) => {
                    // >> counts as two >
                    if depth == 0 {
                        return false; // Would close too many
                    }
                    depth -= 1;
                    if depth == 0 {
                        return self.peek_kind(offset + 1) == Some(SyntaxKind::LParen);
                    }
                    depth -= 1;
                }
                None => return false, // End of input
                _ => {}               // Continue scanning
            }
            offset += 1;

            // Safety limit to avoid infinite loops
            if offset > 50 {
                return false;
            }
        }
    }

    /// Parse identifier expression with possible postfix operations
    fn parse_identifier_expression(&mut self) {
        // Check for special patterns that need lookahead:
        // 1. Type::Variant or Type<T>::Variant (path expression)
        // 2. function<T>(args) (generic function call)
        // 3. function::<T>(args) (turbofish call)
        // Note: Path expressions can start with identifiers OR keywords (e.g., fp32::ZERO)
        let is_simple_path = (self.current_kind() == Some(SyntaxKind::Ident)
            || self.current_kind().is_some_and(|k| k.is_keyword()))
            && self.peek_kind(1) == Some(SyntaxKind::ColonColon)
            && self.peek_kind(2) != Some(SyntaxKind::Lt); // Not a turbofish
        let is_generic_path = self.is_generic_path_expr();
        let is_turbofish_call = self.is_turbofish_call_expr();
        let is_generic_call = self.is_generic_call_expr();
        let is_path = is_simple_path || is_generic_path;

        if is_path {
            // Parse as path expression (Type::Variant or Type<T>::Variant)
            // Note: Type can be an identifier or keyword (e.g., fp32::ZERO)
            self.start_node(SyntaxKind::PathExpr);
            self.bump(); // type name (identifier or keyword)

            // Parse generic arguments if present
            if self.at(SyntaxKind::Lt) {
                self.parse_generic_args();
            }

            self.expect(SyntaxKind::ColonColon); // ::
            self.expect(SyntaxKind::Ident); // variant/constant name
                                            // Don't return yet - check for postfix operations like State::Active(0)
        } else if is_turbofish_call {
            // Parse as turbofish call: function::<T>(args)
            self.start_node(SyntaxKind::CallExpr);
            self.expect(SyntaxKind::Ident); // function name
            self.expect(SyntaxKind::ColonColon); // ::

            // Parse generic arguments (parse_generic_args expects and consumes the <)
            self.parse_generic_args();

            // Parse function call arguments
            self.expect(SyntaxKind::LParen);
            self.parse_argument_list();
            self.expect(SyntaxKind::RParen);
            // CallExpr is complete, but check for chained postfix operations
        } else if is_generic_call {
            // Parse as generic function call: function<T>(args)
            self.start_node(SyntaxKind::CallExpr);
            self.expect(SyntaxKind::Ident); // function name

            // Parse generic arguments (parse_generic_args expects and consumes the <)
            self.parse_generic_args();

            // Parse function call arguments
            self.expect(SyntaxKind::LParen);
            self.parse_argument_list();
            self.expect(SyntaxKind::RParen);
            // CallExpr is complete, but check for chained postfix operations
        } else {
            // Parse as regular identifier expression
            self.start_node(SyntaxKind::IdentExpr);
            self.bump(); // consume identifier
        }

        // Handle postfix operations (works for both path and identifier expressions)
        loop {
            match self.current_kind() {
                Some(SyntaxKind::Dot) => {
                    // Field access (can be identifier for struct fields or numeric literal for tuple fields)
                    self.finish_node(); // finish IdentExpr
                    self.start_node(SyntaxKind::FieldExpr);
                    self.bump(); // consume dot

                    // Accept either identifier (struct field) or numeric literal (tuple field: .0, .1, .2)
                    if self.at(SyntaxKind::Ident) || self.at(SyntaxKind::IntLiteral) {
                        self.bump(); // consume field name or index
                    } else {
                        self.error("expected field name or tuple index after '.'");
                    }
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
                Some(SyntaxKind::LBrace) => {
                    // A { after an identifier means we've hit the start of a block
                    // This is not a postfix operation, so break from the loop
                    break;
                }
                // Note: Cast expressions (as Type) are handled in parse_unary_expr() using checkpoints,
                // not as postfix operations. Break here if we see 'as' keyword.
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

            // Check for repeat syntax: [value; count]
            if self.at(SyntaxKind::Semicolon) {
                self.bump(); // consume semicolon
                self.parse_expression(); // parse count
            } else {
                // Regular array literal: [elem1, elem2, ...]
                while self.at(SyntaxKind::Comma) {
                    self.bump();
                    if !self.at(SyntaxKind::RBracket) {
                        self.parse_expression();
                    }
                }
            }
        }

        self.expect(SyntaxKind::RBracket);
        self.finish_node();
    }

    /// Parse concatenation expression: {a, b, c}
    fn parse_concat_expression(&mut self) {
        self.start_node(SyntaxKind::ConcatExpr);
        self.expect(SyntaxKind::LBrace);

        // Parse comma-separated list of expressions
        if !self.at(SyntaxKind::RBrace) {
            self.parse_expression();

            while self.at(SyntaxKind::Comma) {
                self.bump(); // consume ','
                if !self.at(SyntaxKind::RBrace) {
                    self.parse_expression();
                }
            }
        }

        self.expect(SyntaxKind::RBrace);
        self.finish_node();
    }

    /// Parse struct literal: TypeName { field: value, ... }
    /// Supports both TypeName<T> and TypeName::<T> syntax
    fn parse_struct_literal(&mut self) {
        self.start_node(SyntaxKind::StructLiteral);

        // Type name (identifier or type keyword like vec3, fp32)
        if self.at(SyntaxKind::Ident)
            || matches!(
                self.current_kind(),
                Some(SyntaxKind::Fp16Kw) | Some(SyntaxKind::Fp32Kw) | Some(SyntaxKind::Fp64Kw)
            )
        {
            self.bump();
        } else {
            self.error("expected type name for struct literal");
        }

        // Optional turbofish (::) before generic arguments
        if self.at(SyntaxKind::ColonColon) && self.peek_kind(1) == Some(SyntaxKind::Lt) {
            self.bump(); // consume ::
        }

        // Optional generic arguments (e.g., vec3<fp32> or vec3::<fp32>)
        if self.at(SyntaxKind::Lt) {
            self.parse_generic_args();
        }

        // Opening brace
        self.expect(SyntaxKind::LBrace);

        // Parse field initializations
        if !self.at(SyntaxKind::RBrace) {
            self.parse_struct_field_init();

            while self.at(SyntaxKind::Comma) {
                self.bump(); // consume ','
                if !self.at(SyntaxKind::RBrace) {
                    self.parse_struct_field_init();
                }
            }
        }

        // Closing brace
        self.expect(SyntaxKind::RBrace);

        self.finish_node();
    }

    /// Parse struct field initialization: field_name: value
    fn parse_struct_field_init(&mut self) {
        self.start_node(SyntaxKind::StructFieldInit);
        self.expect(SyntaxKind::Ident); // field name
        self.expect(SyntaxKind::Colon); // :
        self.parse_expression(); // field value
        self.finish_node();
    }

    /// Parse parameter list
    fn parse_parameter_list(&mut self) {
        self.start_node(SyntaxKind::ParameterList);

        if !self.at(SyntaxKind::RParen) {
            self.parse_parameter();

            while self.at(SyntaxKind::Comma) {
                self.bump();
                if !self.at(SyntaxKind::RParen) {
                    self.parse_parameter();
                }
            }
        }

        self.finish_node();
    }

    /// Parse single parameter
    fn parse_parameter(&mut self) {
        self.start_node(SyntaxKind::Parameter);

        // Handle &self or self
        if self.at(SyntaxKind::Amp) {
            self.bump(); // &
            if self.at(SyntaxKind::SelfKw) {
                self.bump(); // self
                // Check for optional type annotation: &self: Type
                if self.at(SyntaxKind::Colon) {
                    self.bump();
                    self.parse_type();
                }
                self.finish_node();
                return;
            }
        } else if self.at(SyntaxKind::SelfKw) {
            self.bump(); // self
            // Check for optional type annotation: self: Self or self: Type
            if self.at(SyntaxKind::Colon) {
                self.bump();
                self.parse_type();
            }
            self.finish_node();
            return;
        }

        // Parameter name - allow keywords as identifiers
        self.expect_ident_or_keyword();

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
            Some(
                SyntaxKind::InKw
                    | SyntaxKind::InputKw
                    | SyntaxKind::OutKw
                    // Note: OutputKw is NOT a valid port direction - only OutKw is valid
                    // OutputKw is a synonym that should be treated as an identifier in port names
                    | SyntaxKind::InoutKw
                    | SyntaxKind::PortKw
            )
        )
    }

    /// Check if at a keyword that can be used as an identifier in certain contexts
    /// (e.g., 'area' as intent constraint name, 'group' as attribute key)
    fn at_keyword_as_ident(&self) -> bool {
        matches!(
            self.current_kind(),
            Some(SyntaxKind::AreaKw)
                | Some(SyntaxKind::GroupKw)
                | Some(SyntaxKind::TypeKw)
                | Some(SyntaxKind::FastKw)
                | Some(SyntaxKind::SlowKw)
                | Some(SyntaxKind::MediumKw)
                // Power intent keywords that can be used as attribute names
                | Some(SyntaxKind::IsolationKw)
        )
    }

    /// Lookahead to check if the current if statement has no else clause
    /// Assumes we're currently at an IfKw token
    /// Returns true if there's no else, false if there is an else
    fn is_if_without_else_lookahead(&self) -> bool {
        if self.current_kind() != Some(SyntaxKind::IfKw) {
            return false;
        }

        // Scan forward to find the matching closing brace of the if block
        // and check if it's followed by 'else'
        let mut pos = self.current + 1; // Skip 'if'
        let mut brace_depth = 0;
        let mut found_then_block = false;

        while pos < self.tokens.len() {
            let kind = SyntaxKind::from(self.tokens[pos].token.clone());

            // Skip trivia
            if kind.is_trivia() {
                pos += 1;
                continue;
            }

            // Track braces in the if condition and body
            match kind {
                SyntaxKind::LBrace => {
                    brace_depth += 1;
                    found_then_block = true;
                }
                SyntaxKind::RBrace => {
                    brace_depth -= 1;
                    // When we close the then-block, check next token
                    if found_then_block && brace_depth == 0 {
                        // Look at next non-trivia token
                        let mut next_pos = pos + 1;
                        while next_pos < self.tokens.len() {
                            let next_kind = SyntaxKind::from(self.tokens[next_pos].token.clone());
                            if !next_kind.is_trivia() {
                                return next_kind != SyntaxKind::ElseKw;
                            }
                            next_pos += 1;
                        }
                        // No more tokens after closing brace = no else
                        return true;
                    }
                }
                _ => {}
            }
            pos += 1;
        }

        // Couldn't find matching brace, assume no else
        true
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

    /// Consume current token as an identifier, even if it's a keyword
    /// This allows keywords to be used as identifiers in unambiguous contexts
    fn bump_as_ident(&mut self) {
        if let Some(token) = self.current_token() {
            let text = &self.source[token.span.clone()];
            // Always emit as Ident, regardless of the actual token kind
            self.builder
                .token(rowan::SyntaxKind(SyntaxKind::Ident as u16), text);
            self.current += 1;
        }
    }

    /// Check if current token can be used as an identifier
    /// Returns true for actual identifiers or keywords that can serve as identifiers
    fn at_ident_or_keyword(&self) -> bool {
        match self.current_kind() {
            Some(SyntaxKind::Ident) => true,
            // Allow these keywords to be used as identifiers
            Some(
                SyntaxKind::InKw
                | SyntaxKind::InputKw
                | SyntaxKind::OutKw
                | SyntaxKind::OutputKw
                | SyntaxKind::InoutKw
                | SyntaxKind::SignalKw,
            ) => true,
            _ => false,
        }
    }

    /// Expect an identifier, allowing certain keywords to be used as identifiers
    fn expect_ident_or_keyword(&mut self) {
        if self.at_ident_or_keyword() {
            self.bump_as_ident();
        } else {
            self.expect(SyntaxKind::Ident);
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

    // ===== Physical Constraint Parsing =====

    /// Parse global constraint block: constraint physical { ... }
    fn parse_global_constraint_block(&mut self) {
        self.start_node(SyntaxKind::GlobalConstraintBlock);

        // 'constraint' keyword
        self.expect(SyntaxKind::ConstraintKw);

        // Constraint type (physical, timing, etc.)
        if self.at(SyntaxKind::PhysicalKw) {
            self.bump();
        } else {
            self.error("expected constraint type (e.g., 'physical')");
        }

        // Constraint body
        self.expect(SyntaxKind::LBrace);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            if self.at(SyntaxKind::RBrace) {
                break;
            }

            // Parse constraint statements
            match self.current_kind() {
                Some(SyntaxKind::DeviceKw) => self.parse_device_spec(),
                Some(SyntaxKind::BankKw) => self.parse_bank_block(),
                Some(SyntaxKind::FloorplanKw) => self.parse_floorplan_block(),
                Some(SyntaxKind::IoDefaultsKw) => self.parse_io_defaults_block(),
                _ => {
                    self.error_and_bump("expected constraint statement");
                }
            }
        }

        self.expect(SyntaxKind::RBrace);
        self.finish_node();
    }

    /// Parse inline physical constraint block: @ { pin: "A1", ... }
    fn parse_physical_constraint_block(&mut self) {
        self.start_node(SyntaxKind::PhysicalConstraintBlock);

        // '@' symbol
        self.expect(SyntaxKind::At);

        // Constraint pairs
        self.expect(SyntaxKind::LBrace);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            if self.at(SyntaxKind::RBrace) {
                break;
            }

            self.parse_constraint_pair();

            // Optional comma
            if self.at(SyntaxKind::Comma) {
                self.bump();
            }
        }

        self.expect(SyntaxKind::RBrace);
        self.finish_node();
    }

    /// Parse a constraint pair: key: value
    fn parse_constraint_pair(&mut self) {
        self.start_node(SyntaxKind::ConstraintPair);

        // Key (identifier or keyword)
        match self.current_kind() {
            Some(SyntaxKind::PinKw)
            | Some(SyntaxKind::PinsKw)
            | Some(SyntaxKind::PinPKw)
            | Some(SyntaxKind::PinNKw)
            | Some(SyntaxKind::IoStandardKw)
            | Some(SyntaxKind::DriveKw)
            | Some(SyntaxKind::SlewKw)
            | Some(SyntaxKind::PullKw)
            | Some(SyntaxKind::DiffTermKw)
            | Some(SyntaxKind::SchmittKw)
            | Some(SyntaxKind::BankKw)
            | Some(SyntaxKind::VoltageKw) => {
                self.bump();
            }
            _ => {
                self.error("expected constraint key");
                self.finish_node();
                return;
            }
        }

        // Colon
        self.expect(SyntaxKind::Colon);

        // Value - depends on the key
        self.parse_constraint_value();

        self.finish_node();
    }

    /// Parse constraint value (can be string, number, identifier, or array)
    fn parse_constraint_value(&mut self) {
        match self.current_kind() {
            Some(SyntaxKind::StringLiteral) => {
                self.bump(); // String value like "A1" or "LVCMOS33"
            }
            Some(SyntaxKind::IntLiteral) => {
                self.bump(); // Integer value
            }
            Some(SyntaxKind::TrueKw) | Some(SyntaxKind::FalseKw) => {
                self.bump(); // Boolean value
            }
            Some(SyntaxKind::FastKw) | Some(SyntaxKind::SlowKw) | Some(SyntaxKind::MediumKw) => {
                self.start_node(SyntaxKind::SlewRate);
                self.bump();
                self.finish_node();
            }
            Some(SyntaxKind::UpKw)
            | Some(SyntaxKind::DownKw)
            | Some(SyntaxKind::NoneKw)
            | Some(SyntaxKind::KeeperKw) => {
                self.start_node(SyntaxKind::Termination);
                self.bump();
                self.finish_node();
            }
            Some(SyntaxKind::LBracket) => {
                // Array of pins: ["A1", "A2", "A3"]
                self.start_node(SyntaxKind::PinArray);
                self.bump(); // '['

                while !self.at(SyntaxKind::RBracket) && !self.is_at_end() {
                    self.skip_trivia();

                    if self.at(SyntaxKind::RBracket) {
                        break;
                    }

                    self.expect(SyntaxKind::StringLiteral);

                    if self.at(SyntaxKind::Comma) {
                        self.bump();
                    }
                }

                self.expect(SyntaxKind::RBracket);
                self.finish_node();
            }
            Some(SyntaxKind::Ident) => {
                self.bump(); // Identifier value
            }
            _ => {
                self.error("expected constraint value (string, number, boolean, or array)");
            }
        }
    }

    /// Parse device specification: device: "iCE40HX8K-CT256"
    fn parse_device_spec(&mut self) {
        self.start_node(SyntaxKind::DeviceSpec);

        self.expect(SyntaxKind::DeviceKw);
        self.expect(SyntaxKind::Colon);
        self.expect(SyntaxKind::StringLiteral);

        self.finish_node();
    }

    /// Parse bank block: bank 0 { voltage: 3.3, io_standard: "LVCMOS33" }
    fn parse_bank_block(&mut self) {
        self.start_node(SyntaxKind::BankBlock);

        self.expect(SyntaxKind::BankKw);
        self.expect(SyntaxKind::IntLiteral); // Bank number

        self.expect(SyntaxKind::LBrace);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            if self.at(SyntaxKind::RBrace) {
                break;
            }

            self.parse_constraint_pair();

            if self.at(SyntaxKind::Comma) {
                self.bump();
            }
        }

        self.expect(SyntaxKind::RBrace);
        self.finish_node();
    }

    /// Parse floorplan block: floorplan { region "name" { ... } }
    fn parse_floorplan_block(&mut self) {
        self.start_node(SyntaxKind::FloorplanBlock);

        self.expect(SyntaxKind::FloorplanKw);
        self.expect(SyntaxKind::LBrace);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            if self.at(SyntaxKind::RBrace) {
                break;
            }

            if self.at(SyntaxKind::RegionKw) {
                self.parse_region_block();
            } else if self.at(SyntaxKind::GroupKw) {
                self.parse_group_block();
            } else {
                self.error_and_bump("expected 'region' or 'group'");
            }
        }

        self.expect(SyntaxKind::RBrace);
        self.finish_node();
    }

    /// Parse region block: region "name" { area: (10, 10, 30, 30), ... }
    fn parse_region_block(&mut self) {
        self.start_node(SyntaxKind::RegionBlock);

        self.expect(SyntaxKind::RegionKw);
        self.expect(SyntaxKind::StringLiteral); // Region name

        self.expect(SyntaxKind::LBrace);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            if self.at(SyntaxKind::RBrace) {
                break;
            }

            self.parse_constraint_pair();

            if self.at(SyntaxKind::Comma) {
                self.bump();
            }
        }

        self.expect(SyntaxKind::RBrace);
        self.finish_node();
    }

    /// Parse group block: group "name" { instances: [...], ... }
    fn parse_group_block(&mut self) {
        self.start_node(SyntaxKind::GroupBlock);

        self.expect(SyntaxKind::GroupKw);
        self.expect(SyntaxKind::StringLiteral); // Group name

        self.expect(SyntaxKind::LBrace);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            if self.at(SyntaxKind::RBrace) {
                break;
            }

            self.parse_constraint_pair();

            if self.at(SyntaxKind::Comma) {
                self.bump();
            }
        }

        self.expect(SyntaxKind::RBrace);
        self.finish_node();
    }

    /// Parse I/O defaults block: io_defaults { io_standard: "LVCMOS33", ... }
    fn parse_io_defaults_block(&mut self) {
        self.start_node(SyntaxKind::IoDefaultsBlock);

        self.expect(SyntaxKind::IoDefaultsKw);
        self.expect(SyntaxKind::LBrace);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            if self.at(SyntaxKind::RBrace) {
                break;
            }

            self.parse_constraint_pair();

            if self.at(SyntaxKind::Comma) {
                self.bump();
            }
        }

        self.expect(SyntaxKind::RBrace);
        self.finish_node();
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

impl ParseState<'_> {
    /// Parse a use declaration
    fn parse_use_decl(&mut self) {
        self.start_node(SyntaxKind::UseDecl);

        // 'use' keyword
        self.expect(SyntaxKind::UseKw);

        // Parse the use path
        self.parse_use_path();

        // Semicolon
        self.expect(SyntaxKind::Semicolon);

        self.finish_node();
    }

    /// Parse a use path (can be simple, renamed, glob, or nested)
    fn parse_use_path(&mut self) {
        self.start_node(SyntaxKind::UsePath);

        // Parse path segments separated by ::
        // Allow keywords as identifiers in use paths (e.g., use foo::{fp32, vec3})
        if self.at(SyntaxKind::Ident) || self.current_kind().is_some_and(|k| k.is_keyword()) {
            self.bump();
        } else {
            self.error("expected identifier");
        }

        while self.at(SyntaxKind::ColonColon) {
            self.bump(); // ::

            if self.at(SyntaxKind::Star) {
                // Glob import: use foo::bar::*;
                self.bump(); // *
                break;
            } else if self.at(SyntaxKind::LBrace) {
                // Nested imports: use foo::bar::{Baz, Qux};
                self.parse_use_tree();
                break;
            } else {
                // Continue path - allow keywords as identifiers
                if self.at(SyntaxKind::Ident) || self.current_kind().is_some_and(|k| k.is_keyword())
                {
                    self.bump();
                } else {
                    self.error("expected identifier");
                }

                // Check for rename: as alias
                if self.at(SyntaxKind::AsKw) {
                    self.bump(); // as
                                 // Alias can also be a keyword used as identifier
                    if self.at(SyntaxKind::Ident)
                        || self.current_kind().is_some_and(|k| k.is_keyword())
                    {
                        self.bump();
                    } else {
                        self.error("expected identifier");
                    }
                    break;
                }
            }
        }

        self.finish_node();
    }

    /// Parse nested use tree: { Item1, Item2, ... }
    fn parse_use_tree(&mut self) {
        self.start_node(SyntaxKind::UseTree);

        self.expect(SyntaxKind::LBrace);

        // Parse first item
        if !self.at(SyntaxKind::RBrace) {
            self.parse_use_path();

            // Parse remaining items
            while self.at(SyntaxKind::Comma) {
                self.bump(); // ,
                if self.at(SyntaxKind::RBrace) {
                    break; // Trailing comma
                }
                self.parse_use_path();
            }
        }

        self.expect(SyntaxKind::RBrace);

        self.finish_node();
    }

    /// Parse item with optional visibility modifier
    fn parse_item_with_visibility(&mut self) {
        // Parse visibility if present
        if self.at(SyntaxKind::PubKw) {
            self.parse_visibility();
        }

        // Parse the actual item
        match self.current_kind() {
            Some(SyntaxKind::ModKw) => self.parse_module_decl(),
            Some(SyntaxKind::EntityKw) => self.parse_entity_decl(),
            Some(SyntaxKind::TraitKw) => self.parse_trait_def(),
            Some(SyntaxKind::TypeKw) => self.parse_type_alias(),
            Some(SyntaxKind::StructKw) => self.parse_struct_decl(),
            Some(SyntaxKind::EnumKw) => self.parse_enum_decl(),
            Some(SyntaxKind::FnKw) => self.parse_function_decl(),
            Some(SyntaxKind::ConstKw) => self.parse_constant_decl(),
            _ => {
                self.error_and_bump("expected item after visibility modifier");
            }
        }
    }

    /// Parse visibility modifier: pub, pub(crate), pub(super)
    fn parse_visibility(&mut self) {
        self.start_node(SyntaxKind::Visibility);

        self.expect(SyntaxKind::PubKw);

        // Check for restricted visibility: pub(crate) or pub(super)
        if self.at(SyntaxKind::LParen) {
            self.bump(); // (

            // Expect 'crate' or 'super' or a path
            if self.at(SyntaxKind::Ident) {
                self.bump(); // crate/super
            }

            self.expect(SyntaxKind::RParen);
        }

        self.finish_node();
    }

    /// Parse module declaration
    fn parse_module_decl(&mut self) {
        self.start_node(SyntaxKind::ModuleDecl);

        // 'mod' keyword
        self.expect(SyntaxKind::ModKw);

        // Module name
        self.expect(SyntaxKind::Ident);

        // Either inline module { ... } or external module ;
        if self.at(SyntaxKind::LBrace) {
            // Inline module
            self.bump(); // {

            // Parse module items (recursively parse as mini source file)
            while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
                self.skip_trivia();

                if self.at(SyntaxKind::RBrace) {
                    break;
                }

                // Parse items within the module
                match self.current_kind() {
                    Some(SyntaxKind::UseKw) => self.parse_use_decl(),
                    Some(SyntaxKind::PubKw) | Some(SyntaxKind::ModKw) => {
                        self.parse_item_with_visibility()
                    }
                    Some(SyntaxKind::EntityKw) => self.parse_entity_decl(),
                    Some(SyntaxKind::TraitKw) => self.parse_trait_def(),
                    _ => {
                        self.error_and_bump("expected module item");
                    }
                }
            }

            self.expect(SyntaxKind::RBrace);
        } else {
            // External module (file)
            self.expect(SyntaxKind::Semicolon);
        }

        self.finish_node();
    }

    // ========================================================================
    // Safety Features (ISO 26262) Parsing
    // ========================================================================

    /// Parse safety_goal declaration
    /// Syntax: safety_goal Name: ASIL_LEVEL { ... }
    fn parse_safety_goal_decl(&mut self) {
        self.start_node(SyntaxKind::SafetyGoalDecl);

        // 'safety_goal' keyword
        self.expect(SyntaxKind::SafetyGoalKw);

        // Goal name
        self.expect(SyntaxKind::Ident);

        // ':' ASIL level
        self.expect(SyntaxKind::Colon);
        self.parse_asil_level();

        // Body
        self.expect(SyntaxKind::LBrace);
        self.parse_safety_goal_body();
        self.expect(SyntaxKind::RBrace);

        self.finish_node();
    }

    /// Parse ASIL level (ASIL_A, ASIL_B, ASIL_C, ASIL_D, or decomposition like ASIL_B + ASIL_B)
    fn parse_asil_level(&mut self) {
        self.skip_trivia();
        // Parse ASIL level identifier (ASIL_A, ASIL_B, etc.) or QM
        if self.at(SyntaxKind::Ident) {
            self.bump();
            // Check for decomposition (ASIL_B + ASIL_B)
            self.skip_trivia();
            if self.at(SyntaxKind::Plus) {
                self.bump();
                self.skip_trivia();
                if self.at(SyntaxKind::Ident) {
                    self.bump();
                }
            }
        }
    }

    /// Parse safety goal body
    fn parse_safety_goal_body(&mut self) {
        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            match self.current_kind() {
                Some(SyntaxKind::Ident) => {
                    // id:, description:, ftti:, traces_to:
                    self.parse_safety_kv_pair();
                }
                Some(SyntaxKind::TargetKw) => self.parse_safety_target_block(),
                Some(SyntaxKind::HsrKw) => self.parse_hsr_decl(),
                Some(SyntaxKind::LsmKw) => self.parse_lsm_decl(),
                Some(SyntaxKind::UseKw) => self.parse_safety_trait_usage(),
                _ => {
                    if !self.at(SyntaxKind::RBrace) {
                        self.error_and_bump("expected safety goal member");
                    }
                }
            }
        }
    }

    /// Parse safety key-value pair (key: value,)
    fn parse_safety_kv_pair(&mut self) {
        self.skip_trivia();
        // Key - can be identifier or keyword (spfm, lfm, pmhf, dc, lc, etc.)
        if self.at(SyntaxKind::Ident) || self.current_kind().map(|k| k.is_keyword()).unwrap_or(false) {
            self.bump();
        } else {
            // Not a valid key - error and bump to avoid infinite loop
            self.error_and_bump("expected identifier or keyword as key");
            return;
        }
        self.skip_trivia();
        // Colon
        if self.at(SyntaxKind::Colon) {
            self.bump();
        } else {
            // Missing colon - error but continue
            self.error("expected ':'");
        }
        self.skip_trivia();
        // Value - can be string literal, number, or array
        self.parse_safety_kv_value();
        self.skip_trivia();
        // Optional comma
        if self.at(SyntaxKind::Comma) {
            self.bump();
        }
    }

    /// Parse safety key-value value
    fn parse_safety_kv_value(&mut self) {
        self.skip_trivia();
        match self.current_kind() {
            Some(SyntaxKind::StringLiteral) => {
                self.bump();
            }
            Some(SyntaxKind::IntLiteral) | Some(SyntaxKind::FloatLiteral) => {
                self.bump();
                // Check for unit suffix like _ms, _us, _cycle
                self.skip_trivia();
                if self.at(SyntaxKind::Ident) {
                    self.bump();
                }
            }
            Some(SyntaxKind::LBracket) => {
                // Array value like ["DOORS:HARA-H001"]
                self.parse_safety_array_value();
            }
            Some(SyntaxKind::Ge) | Some(SyntaxKind::Le) => {
                // >= 99.0 or <= 10.0 or >= DC (generic parameter)
                self.bump();
                self.skip_trivia();
                if self.at(SyntaxKind::FloatLiteral)
                    || self.at(SyntaxKind::IntLiteral)
                    || self.at(SyntaxKind::Ident)
                {
                    self.bump();
                }
            }
            Some(kind) if kind == SyntaxKind::Ident || kind.is_keyword() => {
                // Identifier or keyword value (e.g., DC for generic parameter)
                self.bump();
            }
            Some(SyntaxKind::RBrace) | Some(SyntaxKind::Comma) | None => {
                // End of value - nothing to parse (empty value is allowed)
            }
            _ => {
                // Unexpected token - error and bump to avoid infinite loop
                self.error_and_bump("unexpected token in safety value");
            }
        }
    }

    /// Parse array value like ["DOORS:HARA-H001", "item2"]
    fn parse_safety_array_value(&mut self) {
        self.expect(SyntaxKind::LBracket);
        while !self.at(SyntaxKind::RBracket) && !self.is_at_end() {
            self.skip_trivia();
            // Parse element
            self.parse_safety_kv_value();
            self.skip_trivia();
            if self.at(SyntaxKind::Comma) {
                self.bump();
            }
        }
        self.expect(SyntaxKind::RBracket);
    }

    /// Parse target { spfm: >= 99.0, lfm: >= 90.0, pmhf: <= 10.0 }
    fn parse_safety_target_block(&mut self) {
        self.start_node(SyntaxKind::SafetyTargetBlock);
        self.expect(SyntaxKind::TargetKw);
        self.expect(SyntaxKind::LBrace);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();
            // Parse metric: spfm, lfm, pmhf
            match self.current_kind() {
                Some(SyntaxKind::SpfmKw)
                | Some(SyntaxKind::LfmKw)
                | Some(SyntaxKind::PmhfKw)
                | Some(SyntaxKind::Ident) => {
                    self.parse_safety_kv_pair();
                }
                _ => {
                    if !self.at(SyntaxKind::RBrace) {
                        self.error_and_bump("expected metric (spfm, lfm, pmhf)");
                    }
                }
            }
        }

        self.expect(SyntaxKind::RBrace);
        self.finish_node();
    }

    /// Parse hsr HSR_001 { requirement: "...", verification: [...], psm Name { ... } }
    fn parse_hsr_decl(&mut self) {
        self.start_node(SyntaxKind::HsrDecl);
        self.expect(SyntaxKind::HsrKw);

        // HSR identifier
        self.expect(SyntaxKind::Ident);

        self.expect(SyntaxKind::LBrace);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            match self.current_kind() {
                Some(SyntaxKind::PsmKw) => self.parse_psm_decl(),
                Some(SyntaxKind::Ident)
                | Some(SyntaxKind::RequirementKw)
                | Some(SyntaxKind::VerificationKw) => {
                    // requirement:, verification:, etc.
                    self.parse_safety_kv_pair();
                }
                _ => {
                    if !self.at(SyntaxKind::RBrace) {
                        self.error_and_bump("expected HSR member");
                    }
                }
            }
        }

        self.expect(SyntaxKind::RBrace);
        self.finish_node();
    }

    /// Parse psm Name { dc: >= 99.0, dhsr DHSR_001 { ... } }
    fn parse_psm_decl(&mut self) {
        self.start_node(SyntaxKind::PsmDecl);
        self.expect(SyntaxKind::PsmKw);

        // PSM name
        self.expect(SyntaxKind::Ident);

        self.expect(SyntaxKind::LBrace);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            match self.current_kind() {
                Some(SyntaxKind::DhsrKw) => self.parse_dhsr_decl(),
                Some(SyntaxKind::DcKw) | Some(SyntaxKind::Ident) => {
                    // dc:, etc.
                    self.parse_safety_kv_pair();
                }
                _ => {
                    if !self.at(SyntaxKind::RBrace) {
                        self.error_and_bump("expected PSM member");
                    }
                }
            }
        }

        self.expect(SyntaxKind::RBrace);
        self.finish_node();
    }

    /// Parse dhsr DHSR_001 { requirement: "...", detection_time: <= 1_cycle }
    fn parse_dhsr_decl(&mut self) {
        self.start_node(SyntaxKind::DhsrDecl);
        self.expect(SyntaxKind::DhsrKw);

        // DHSR identifier
        self.expect(SyntaxKind::Ident);

        self.expect(SyntaxKind::LBrace);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();
            // Properties can be identifiers or keywords like requirement, detection_time
            let kind = self.current_kind();
            if kind == Some(SyntaxKind::Ident)
                || kind == Some(SyntaxKind::RequirementKw)
                || kind == Some(SyntaxKind::DetectionTimeKw)
                || kind.map(|k| k.is_keyword()).unwrap_or(false)
            {
                self.parse_safety_kv_pair();
            } else if !self.at(SyntaxKind::RBrace) {
                self.error_and_bump("expected DHSR property");
            }
        }

        self.expect(SyntaxKind::RBrace);
        self.finish_node();
    }

    /// Parse lsm Name { lc: >= 90.0, interval: <= 100_ms }
    fn parse_lsm_decl(&mut self) {
        self.start_node(SyntaxKind::LsmDecl);
        self.expect(SyntaxKind::LsmKw);

        // LSM name
        self.expect(SyntaxKind::Ident);

        self.expect(SyntaxKind::LBrace);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();
            match self.current_kind() {
                Some(SyntaxKind::LcKw) | Some(SyntaxKind::IntervalKw) | Some(SyntaxKind::Ident) => {
                    self.parse_safety_kv_pair();
                }
                _ => {
                    if !self.at(SyntaxKind::RBrace) {
                        self.error_and_bump("expected LSM property");
                    }
                }
            }
        }

        self.expect(SyntaxKind::RBrace);
        self.finish_node();
    }

    /// Parse safety_entity declaration
    /// Syntax: safety_entity Name implements GoalName { ... }
    fn parse_safety_entity_decl(&mut self) {
        self.start_node(SyntaxKind::SafetyEntityDecl);

        // 'safety_entity' keyword
        self.expect(SyntaxKind::SafetyEntityKw);

        // Entity name
        self.expect(SyntaxKind::Ident);

        // 'implements' goal_name
        self.expect(SyntaxKind::ImplementsKw);
        self.expect(SyntaxKind::Ident);

        // Body
        self.expect(SyntaxKind::LBrace);
        self.parse_safety_entity_body();
        self.expect(SyntaxKind::RBrace);

        self.finish_node();
    }

    /// Parse safety entity body
    fn parse_safety_entity_body(&mut self) {
        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            match self.current_kind() {
                Some(SyntaxKind::CoversKw) => self.parse_covers_block(),
                Some(SyntaxKind::PsmKw) => self.parse_psm_override(),
                Some(SyntaxKind::LsmKw) => self.parse_lsm_override(),
                Some(SyntaxKind::HsiKw) => self.parse_hsi_block(),
                Some(SyntaxKind::FmeaKw) => self.parse_fmea_block(),
                Some(SyntaxKind::InstancesKw) => self.parse_safety_entity_instance(),
                Some(SyntaxKind::UseKw) => self.parse_safety_trait_usage(),
                _ => {
                    if !self.at(SyntaxKind::RBrace) {
                        self.error_and_bump("expected safety entity member");
                    }
                }
            }
        }
    }

    /// Parse covers { top.brake_main, top.brake_aux }
    fn parse_covers_block(&mut self) {
        self.start_node(SyntaxKind::CoversBlock);
        self.expect(SyntaxKind::CoversKw);
        self.expect(SyntaxKind::LBrace);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();
            // Parse instance path (e.g., top.brake_main)
            self.parse_design_path();
            self.skip_trivia();
            if self.at(SyntaxKind::Comma) {
                self.bump();
            }
        }

        self.expect(SyntaxKind::RBrace);
        self.finish_node();
    }

    /// Parse design path like top.brake_main::pressure_a
    fn parse_design_path(&mut self) {
        self.skip_trivia();
        // First segment
        if self.at(SyntaxKind::Ident) || self.at(SyntaxKind::Star) {
            self.bump();
        }
        // Following segments with . or ::
        loop {
            self.skip_trivia();
            if self.at(SyntaxKind::Dot) {
                self.bump();
                self.skip_trivia();
                if self.at(SyntaxKind::Ident) || self.at(SyntaxKind::Star) {
                    self.bump();
                }
            } else if self.at(SyntaxKind::ColonColon) {
                self.bump();
                self.skip_trivia();
                if self.at(SyntaxKind::Ident) || self.at(SyntaxKind::Star) {
                    self.bump();
                }
            } else {
                break;
            }
        }
    }

    /// Parse psm override: psm SensorVoting { dc: 99.5 }
    fn parse_psm_override(&mut self) {
        self.start_node(SyntaxKind::PsmOverride);
        self.expect(SyntaxKind::PsmKw);
        self.expect(SyntaxKind::Ident);
        self.expect(SyntaxKind::LBrace);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();
            if self.at(SyntaxKind::DcKw) || self.at(SyntaxKind::Ident) {
                self.parse_safety_kv_pair();
            } else if !self.at(SyntaxKind::RBrace) {
                self.error_and_bump("expected PSM override property");
            }
        }

        self.expect(SyntaxKind::RBrace);
        self.finish_node();
    }

    /// Parse lsm override: lsm MemoryTest { lc: 90.0 }
    fn parse_lsm_override(&mut self) {
        self.start_node(SyntaxKind::LsmOverride);
        self.expect(SyntaxKind::LsmKw);
        self.expect(SyntaxKind::Ident);
        self.expect(SyntaxKind::LBrace);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();
            if self.at(SyntaxKind::LcKw) || self.at(SyntaxKind::Ident) {
                self.parse_safety_kv_pair();
            } else if !self.at(SyntaxKind::RBrace) {
                self.error_and_bump("expected LSM override property");
            }
        }

        self.expect(SyntaxKind::RBrace);
        self.finish_node();
    }

    /// Parse hsi { signals, exclude: [...], timing { ... } }
    fn parse_hsi_block(&mut self) {
        self.start_node(SyntaxKind::HsiDecl);
        self.expect(SyntaxKind::HsiKw);
        self.expect(SyntaxKind::LBrace);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            match self.current_kind() {
                Some(SyntaxKind::ExcludeKw) => {
                    self.bump();
                    self.expect(SyntaxKind::Colon);
                    self.parse_safety_array_value();
                    if self.at(SyntaxKind::Comma) {
                        self.bump();
                    }
                }
                Some(SyntaxKind::Ident) => {
                    // Signal path
                    self.parse_design_path();
                    if self.at(SyntaxKind::Comma) {
                        self.bump();
                    }
                }
                _ => {
                    if !self.at(SyntaxKind::RBrace) {
                        self.error_and_bump("expected HSI member");
                    }
                }
            }
        }

        self.expect(SyntaxKind::RBrace);
        self.finish_node();
    }

    /// Parse fmea { component path { ... } }
    fn parse_fmea_block(&mut self) {
        self.start_node(SyntaxKind::FmeaBlock);
        self.expect(SyntaxKind::FmeaKw);
        self.expect(SyntaxKind::LBrace);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            if self.at(SyntaxKind::ComponentKw) {
                self.parse_fmea_component();
            } else if !self.at(SyntaxKind::RBrace) {
                self.error_and_bump("expected FMEA component");
            }
        }

        self.expect(SyntaxKind::RBrace);
        self.finish_node();
    }

    /// Parse fmea component declaration
    fn parse_fmea_component(&mut self) {
        self.start_node(SyntaxKind::FmeaComponentDecl);
        self.expect(SyntaxKind::ComponentKw);

        // Design path
        self.parse_design_path();

        self.expect(SyntaxKind::LBrace);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            match self.current_kind() {
                Some(SyntaxKind::LibraryKw) | Some(SyntaxKind::PartKw) => {
                    self.bump();
                    self.expect(SyntaxKind::Colon);
                    self.parse_safety_kv_value();
                    if self.at(SyntaxKind::Comma) {
                        self.bump();
                    }
                }
                Some(SyntaxKind::PsmKw) | Some(SyntaxKind::LsmKw) => {
                    // psm::MechanismName { failure_modes }
                    self.parse_fmea_mechanism_group();
                }
                Some(SyntaxKind::SafetyKw) => {
                    // safe { failure_modes }
                    self.bump();
                    self.expect(SyntaxKind::LBrace);
                    while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
                        self.skip_trivia();
                        if self.at(SyntaxKind::FailureModeKw) {
                            self.parse_failure_mode();
                        } else if !self.at(SyntaxKind::RBrace) {
                            self.error_and_bump("expected failure_mode");
                        }
                    }
                    self.expect(SyntaxKind::RBrace);
                }
                _ => {
                    if !self.at(SyntaxKind::RBrace) {
                        self.error_and_bump("expected FMEA component member");
                    }
                }
            }
        }

        self.expect(SyntaxKind::RBrace);
        self.finish_node();
    }

    /// Parse psm::Name or lsm::Name { failure_modes }
    fn parse_fmea_mechanism_group(&mut self) {
        // psm or lsm keyword
        self.bump();
        self.expect(SyntaxKind::ColonColon);
        self.expect(SyntaxKind::Ident);

        self.expect(SyntaxKind::LBrace);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();
            if self.at(SyntaxKind::FailureModeKw) {
                self.parse_failure_mode();
            } else if !self.at(SyntaxKind::RBrace) {
                self.error_and_bump("expected failure_mode");
            }
        }

        self.expect(SyntaxKind::RBrace);
    }

    /// Parse failure_mode name { severity: S3, dc: 99.5, class: residual }
    fn parse_failure_mode(&mut self) {
        self.start_node(SyntaxKind::FailureModeDecl);
        self.expect(SyntaxKind::FailureModeKw);
        self.expect(SyntaxKind::Ident);

        self.expect(SyntaxKind::LBrace);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();
            match self.current_kind() {
                Some(SyntaxKind::SeverityKw)
                | Some(SyntaxKind::DcKw)
                | Some(SyntaxKind::LcKw)
                | Some(SyntaxKind::Ident) => {
                    self.parse_safety_kv_pair();
                }
                _ => {
                    if !self.at(SyntaxKind::RBrace) {
                        self.error_and_bump("expected failure mode property");
                    }
                }
            }
        }

        self.expect(SyntaxKind::RBrace);
        self.finish_node();
    }

    /// Parse safety entity instance: inst name: EntityType
    fn parse_safety_entity_instance(&mut self) {
        self.start_node(SyntaxKind::SafetyEntityInstance);
        self.expect(SyntaxKind::InstancesKw);
        self.expect(SyntaxKind::Ident);
        self.expect(SyntaxKind::Colon);
        self.expect(SyntaxKind::Ident);

        // Optional body
        if self.at(SyntaxKind::LBrace) {
            self.bump();
            while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
                self.skip_trivia();
                self.parse_safety_kv_pair();
            }
            self.expect(SyntaxKind::RBrace);
        }

        self.finish_node();
    }

    /// Parse use TraitName<params> { overrides }
    fn parse_safety_trait_usage(&mut self) {
        self.start_node(SyntaxKind::SafetyTraitUsage);
        self.expect(SyntaxKind::UseKw);
        self.expect(SyntaxKind::Ident);

        // Optional generic parameters
        if self.at(SyntaxKind::Lt) {
            self.parse_generic_params();
        }

        // Optional override block
        if self.at(SyntaxKind::LBrace) {
            self.bump();
            while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
                self.skip_trivia();
                // Parse overrides
                match self.current_kind() {
                    Some(SyntaxKind::PsmKw) => self.parse_psm_override(),
                    Some(SyntaxKind::LsmKw) => self.parse_lsm_override(),
                    Some(SyntaxKind::Plus) => {
                        // Addition: + hsr NEW { ... }
                        self.bump();
                        match self.current_kind() {
                            Some(SyntaxKind::HsrKw) => self.parse_hsr_decl(),
                            Some(SyntaxKind::LsmKw) => self.parse_lsm_decl(),
                            _ => self.error_and_bump("expected hsr or lsm"),
                        }
                    }
                    Some(SyntaxKind::Minus) => {
                        // Removal: - hsr HSR_NAME
                        self.bump();
                        self.bump(); // keyword
                        self.expect(SyntaxKind::Ident);
                    }
                    _ => {
                        if !self.at(SyntaxKind::RBrace) {
                            self.error_and_bump("expected trait override");
                        }
                    }
                }
            }
            self.expect(SyntaxKind::RBrace);
        }

        // Semicolon
        if self.at(SyntaxKind::Semicolon) {
            self.bump();
        }

        self.finish_node();
    }

    /// Parse safety_trait declaration
    fn parse_safety_trait_decl(&mut self) {
        self.start_node(SyntaxKind::SafetyTraitDecl);
        self.expect(SyntaxKind::SafetyTraitKw);
        self.expect(SyntaxKind::Ident);

        // Optional generic parameters
        if self.at(SyntaxKind::Lt) {
            self.parse_generic_params();
        }

        self.expect(SyntaxKind::LBrace);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            match self.current_kind() {
                Some(SyntaxKind::HsrKw) => self.parse_hsr_decl(),
                Some(SyntaxKind::LsmKw) => self.parse_lsm_decl(),
                _ => {
                    if !self.at(SyntaxKind::RBrace) {
                        self.error_and_bump("expected safety trait member");
                    }
                }
            }
        }

        self.expect(SyntaxKind::RBrace);
        self.finish_node();
    }

    /// Parse fmea_trait declaration
    fn parse_fmea_trait_decl(&mut self) {
        self.start_node(SyntaxKind::FmeaTraitDecl);
        self.expect(SyntaxKind::FmeaTraitKw);
        self.expect(SyntaxKind::Ident);

        self.expect(SyntaxKind::LBrace);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            // component_pattern *::signal { ... }
            if self.at(SyntaxKind::Ident) {
                self.bump(); // component_pattern
                self.parse_design_path(); // pattern
                self.expect(SyntaxKind::LBrace);

                while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
                    self.skip_trivia();
                    match self.current_kind() {
                        Some(SyntaxKind::LibraryKw) | Some(SyntaxKind::PartKw) => {
                            self.bump();
                            self.expect(SyntaxKind::Colon);
                            self.parse_safety_kv_value();
                            if self.at(SyntaxKind::Comma) {
                                self.bump();
                            }
                        }
                        Some(SyntaxKind::PsmKw) | Some(SyntaxKind::LsmKw) => {
                            self.parse_fmea_mechanism_group();
                        }
                        _ => {
                            if !self.at(SyntaxKind::RBrace) {
                                self.error_and_bump("expected FMEA trait member");
                            }
                        }
                    }
                }

                self.expect(SyntaxKind::RBrace);
            } else if !self.at(SyntaxKind::RBrace) {
                self.error_and_bump("expected component_pattern");
            }
        }

        self.expect(SyntaxKind::RBrace);
        self.finish_node();
    }

    /// Parse hsi_trait declaration
    fn parse_hsi_trait_decl(&mut self) {
        self.start_node(SyntaxKind::HsiTraitDecl);
        self.expect(SyntaxKind::HsiTraitKw);
        self.expect(SyntaxKind::Ident);

        self.expect(SyntaxKind::LBrace);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            match self.current_kind() {
                Some(SyntaxKind::Ident) => {
                    // signals { ... } or timing { ... }
                    let ident = self.current_text();
                    self.bump();

                    self.expect(SyntaxKind::LBrace);

                    while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
                        self.skip_trivia();
                        self.parse_design_path();
                        if self.at(SyntaxKind::LBrace) {
                            // timing constraints
                            self.bump();
                            while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
                                self.skip_trivia();
                                self.parse_safety_kv_pair();
                            }
                            self.expect(SyntaxKind::RBrace);
                        }
                        if self.at(SyntaxKind::Comma) {
                            self.bump();
                        }
                    }

                    self.expect(SyntaxKind::RBrace);
                }
                _ => {
                    if !self.at(SyntaxKind::RBrace) {
                        self.error_and_bump("expected HSI trait member");
                    }
                }
            }
        }

        self.expect(SyntaxKind::RBrace);
        self.finish_node();
    }

    /// Parse fmeda_library declaration
    fn parse_fmeda_library_decl(&mut self) {
        self.start_node(SyntaxKind::FmedaLibraryDecl);
        self.expect(SyntaxKind::FmedaLibraryKw);
        self.expect(SyntaxKind::Ident);

        self.expect(SyntaxKind::LBrace);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            if self.at(SyntaxKind::ComponentKw) {
                self.parse_fmeda_library_component();
            } else if !self.at(SyntaxKind::RBrace) {
                self.error_and_bump("expected component declaration");
            }
        }

        self.expect(SyntaxKind::RBrace);
        self.finish_node();
    }

    /// Parse fmeda library component
    fn parse_fmeda_library_component(&mut self) {
        self.start_node(SyntaxKind::FmedaComponentDecl);
        self.expect(SyntaxKind::ComponentKw);
        self.parse_safety_kv_value(); // Component name (string)

        self.expect(SyntaxKind::LBrace);

        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
            self.skip_trivia();

            match self.current_kind() {
                Some(SyntaxKind::Ident) => {
                    let ident = self.current_text();
                    if ident == Some("failure_modes") || ident == Some("mechanisms") {
                        self.bump();
                        self.expect(SyntaxKind::LBrace);

                        while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
                            self.skip_trivia();
                            // failure_mode_name: { fit: 30.0, class: single_point }
                            if self.at(SyntaxKind::Ident) {
                                self.bump();
                                self.expect(SyntaxKind::Colon);
                                self.expect(SyntaxKind::LBrace);

                                while !self.at(SyntaxKind::RBrace) && !self.is_at_end() {
                                    self.skip_trivia();
                                    self.parse_safety_kv_pair();
                                }

                                self.expect(SyntaxKind::RBrace);

                                if self.at(SyntaxKind::Comma) {
                                    self.bump();
                                }
                            } else if !self.at(SyntaxKind::RBrace) {
                                self.error_and_bump("expected failure mode or mechanism");
                            }
                        }

                        self.expect(SyntaxKind::RBrace);
                    } else {
                        // base_fit: 50.0
                        self.parse_safety_kv_pair();
                    }
                }
                _ => {
                    if !self.at(SyntaxKind::RBrace) {
                        self.error_and_bump("expected library component member");
                    }
                }
            }
        }

        self.expect(SyntaxKind::RBrace);
        self.finish_node();
    }
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
    fn test_const_generic_with_usize() {
        let source = "entity Test<const W: usize> { in a: bit }";
        let (tree, errors) = parse_with_errors(source);

        eprintln!("Errors: {:?}", errors);
        eprintln!("Tree: {}", tree);

        assert!(
            errors.is_empty(),
            "Should parse const generic with usize type"
        );

        // Now test HIR building
        let hir_result = crate::parse_and_build_hir(source);
        if let Err(e) = &hir_result {
            eprintln!("HIR Error: {:?}", e);
        }
        assert!(hir_result.is_ok(), "Should build HIR from const generic");
    }

    #[test]
    fn test_const_generic_with_nat() {
        let source = "entity Test<const W: nat> { in a: bit }";
        let (tree, errors) = parse_with_errors(source);

        eprintln!("Errors: {:?}", errors);
        eprintln!("Tree: {}", tree);

        for error in &errors {
            eprintln!("Parse error: {:?}", error);
        }

        assert!(
            errors.is_empty(),
            "Should parse const generic with nat type"
        );

        // Now test HIR building
        let hir_result = crate::parse_and_build_hir(source);
        if let Err(e) = &hir_result {
            eprintln!("HIR Error: {:?}", e);
        }
        assert!(
            hir_result.is_ok(),
            "Should build HIR from const generic with nat"
        );
    }

    #[test]
    fn test_two_const_generics() {
        let source = "entity Test<const W: nat, const D: nat> { in a: bit }";
        let (tree, errors) = parse_with_errors(source);

        eprintln!("Errors for two generics: {:?}", errors);
        eprintln!("Tree: {}", tree);

        for error in &errors {
            eprintln!("Parse error: {:?}", error);
        }

        assert!(errors.is_empty(), "Should parse two const generics");
    }

    #[test]
    fn test_const_generic_with_default() {
        let source = "entity Test<const W: nat = 8> { in a: bit }";
        let (tree, errors) = parse_with_errors(source);

        eprintln!("Errors for default: {:?}", errors);
        eprintln!("Tree: {}", tree);

        for error in &errors {
            eprintln!("Parse error: {:?}", error);
        }

        assert!(
            errors.is_empty(),
            "Should parse const generic with default value"
        );
    }

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

    #[test]
    fn test_parse_inline_physical_constraints() {
        let source = r#"
            entity LedBlinker {
                in clk: clock @ {
                    pin: "A1",
                    io_standard: "LVCMOS33"
                }
                out led: bit @ {
                    pin: "B2",
                    slew: fast
                }
            }
        "#;
        let tree = parse(source);
        assert_eq!(tree.kind(), SyntaxKind::SourceFile);

        let entity = tree.first_child().unwrap();
        assert_eq!(entity.kind(), SyntaxKind::EntityDecl);
    }

    #[test]
    fn test_parse_global_constraint_block() {
        let source = r#"
            constraint physical {
                device: "iCE40HX8K-CT256"

                io_defaults {
                    io_standard: "LVCMOS33",
                    slew: slow
                }
            }
        "#;
        let tree = parse(source);
        assert_eq!(tree.kind(), SyntaxKind::SourceFile);

        let constraint_block = tree.first_child().unwrap();
        assert_eq!(constraint_block.kind(), SyntaxKind::GlobalConstraintBlock);
    }

    #[test]
    fn test_parse_multi_pin_constraint() {
        let source = r#"
            entity Counter {
                out count: nat[8] @ {
                    pins: ["C1", "C2", "C3", "C4", "D1", "D2", "D3", "D4"],
                    io_standard: "LVCMOS33"
                }
            }
        "#;
        let tree = parse(source);
        assert_eq!(tree.kind(), SyntaxKind::SourceFile);

        let entity = tree.first_child().unwrap();
        assert_eq!(entity.kind(), SyntaxKind::EntityDecl);
    }

    // Tests for intent syntax refinements

    #[test]
    fn test_parse_single_line_intent() {
        let source = r#"
            intent parallel = mux_style::parallel;
            intent priority = mux_style::priority;
            intent critical = timing::critical_path;
        "#;
        let (tree, errors) = parse_with_errors(source);

        eprintln!("Errors: {:?}", errors);
        assert!(
            errors.is_empty(),
            "Single-line intent syntax should parse without errors"
        );
        assert_eq!(tree.kind(), SyntaxKind::SourceFile);
    }

    #[test]
    fn test_parse_composed_intent() {
        let source = r#"
            intent parallel = mux_style::parallel;
            intent critical = timing::critical_path;
            intent fast_decode = parallel + critical;
        "#;
        let (tree, errors) = parse_with_errors(source);

        eprintln!("Errors: {:?}", errors);
        assert!(
            errors.is_empty(),
            "Composed intent syntax should parse without errors"
        );
        assert_eq!(tree.kind(), SyntaxKind::SourceFile);
    }

    #[test]
    fn test_parse_attribute_on_entity() {
        let source = r#"
            intent parallel = mux_style::parallel;

            #[parallel]
            entity Decoder {
                in sel: bit[3]
                out result: bit[8]
            }
        "#;
        let (tree, errors) = parse_with_errors(source);

        eprintln!("Errors: {:?}", errors);
        assert!(
            errors.is_empty(),
            "Attribute on entity should parse without errors"
        );
        assert_eq!(tree.kind(), SyntaxKind::SourceFile);
    }

    #[test]
    fn test_parse_attribute_on_function() {
        let source = r#"
            intent critical = timing::critical_path;

            #[critical]
            fn decode(sel: bit[3]) -> bit[8] {
                match sel {
                    0 => 1,
                    _ => 0,
                }
            }
        "#;
        let (tree, errors) = parse_with_errors(source);

        eprintln!("Errors: {:?}", errors);
        assert!(
            errors.is_empty(),
            "Attribute on function should parse without errors"
        );
        assert_eq!(tree.kind(), SyntaxKind::SourceFile);
    }

    #[test]
    fn test_parse_multiple_attributes() {
        let source = r#"
            intent parallel = mux_style::parallel;
            intent critical = timing::critical_path;

            #[parallel, critical]
            entity Decoder {
                in sel: bit[3]
                out result: bit[8]
            }
        "#;
        let (tree, errors) = parse_with_errors(source);

        eprintln!("Errors: {:?}", errors);
        assert!(
            errors.is_empty(),
            "Multiple attributes should parse without errors"
        );
        assert_eq!(tree.kind(), SyntaxKind::SourceFile);
    }

    #[test]
    fn test_parse_composed_attribute() {
        let source = r#"
            intent parallel = mux_style::parallel;
            intent critical = timing::critical_path;

            #[parallel + critical]
            entity FastDecoder {
                in opcode: bit[4]
                out result: bit[16]
            }
        "#;
        let (tree, errors) = parse_with_errors(source);

        eprintln!("Errors: {:?}", errors);
        assert!(
            errors.is_empty(),
            "Composed attribute should parse without errors"
        );
        assert_eq!(tree.kind(), SyntaxKind::SourceFile);
    }

    #[test]
    fn test_parse_attribute_in_function_body() {
        let source = r#"
            fn decode(sel: bit[3]) -> bit[8] {
                #[parallel]
                let result = match sel {
                    0 => 1,
                    1 => 2,
                    _ => 0,
                };
                result
            }
        "#;
        let (tree, errors) = parse_with_errors(source);

        eprintln!("Errors: {:?}", errors);
        assert!(
            errors.is_empty(),
            "Attribute in function body should parse without errors"
        );
        assert_eq!(tree.kind(), SyntaxKind::SourceFile);
    }

    #[test]
    fn test_parse_namespaced_intent_value() {
        let source = r#"
            intent low_latency = mux_style::parallel + timing::critical_path + pipeline::disabled;
        "#;
        let (tree, errors) = parse_with_errors(source);

        eprintln!("Errors: {:?}", errors);
        assert!(
            errors.is_empty(),
            "Namespaced intent composition should parse without errors"
        );
        assert_eq!(tree.kind(), SyntaxKind::SourceFile);
    }

    #[test]
    fn test_parse_intent_without_leading_whitespace() {
        // This mimics a file that starts directly with intent declaration (no leading whitespace)
        let source = "intent parallel = mux_style::parallel;\n\nintent priority = mux_style::priority;\n";
        let (tree, errors) = parse_with_errors(source);

        eprintln!("Errors: {:?}", errors);
        assert!(
            errors.is_empty(),
            "Intent declarations without leading whitespace should parse without errors"
        );
        assert_eq!(tree.kind(), SyntaxKind::SourceFile);
    }

    // ========================================================================
    // Safety Features (ISO 26262) Parsing Tests
    // ========================================================================

    #[test]
    fn test_parse_safety_goal() {
        let source = r#"
            safety_goal BrakingSafety: ASIL_D {
                id: "SG-001",
                description: "Prevent unintended braking",

                target {
                    spfm: >= 99.0,
                    lfm: >= 90.0,
                }

                hsr HSR_001 {
                    requirement: "Detect sensor faults",
                    psm SensorVoting {
                        dc: >= 99.0,
                        dhsr DHSR_001 {
                            requirement: "Voting error detected",
                        }
                    }
                }

                lsm MemoryTest {
                    lc: >= 90.0,
                }
            }
        "#;
        let (tree, errors) = parse_with_errors(source);

        eprintln!("Errors: {:?}", errors);
        assert!(
            errors.is_empty(),
            "Safety goal should parse without errors"
        );
        assert_eq!(tree.kind(), SyntaxKind::SourceFile);
    }

    #[test]
    fn test_parse_safety_entity() {
        let source = r#"
            safety_entity BrakingControl implements BrakingSafety {
                covers {
                    top.brake_main,
                    top.brake_aux,
                }

                psm SensorVoting { dc: 99.5 }

                hsi {
                    top.brake_main::pressure_a,
                    top.brake_main::fault,
                }

                fmea {
                    component top.brake_main::pressure_a {
                        library: "automotive_sensors",
                        part: "PRESSURE_SENSOR",

                        psm::SensorVoting {
                            failure_mode stuck { severity: S3, dc: 99.5 }
                        }
                    }
                }
            }
        "#;
        let (tree, errors) = parse_with_errors(source);

        eprintln!("Errors: {:?}", errors);
        assert!(
            errors.is_empty(),
            "Safety entity should parse without errors"
        );
        assert_eq!(tree.kind(), SyntaxKind::SourceFile);
    }

    #[test]
    fn test_parse_safety_trait() {
        let source = r#"
            safety_trait PecProtection<DC> {
                hsr HSR_PEC {
                    requirement: "Commands protected with PEC",
                    psm CommandIntegrity { dc: >= DC }
                }
            }
        "#;
        let (tree, errors) = parse_with_errors(source);

        eprintln!("Errors: {:?}", errors);
        assert!(
            errors.is_empty(),
            "Safety trait should parse without errors"
        );
        assert_eq!(tree.kind(), SyntaxKind::SourceFile);
    }

    #[test]
    fn test_parse_fmeda_library() {
        let source = r#"
            fmeda_library automotive_grade {
                component "ARM_Cortex_M7" {
                    base_fit: 50.0,

                    failure_modes {
                        logic_upset: { fit: 30.0, class: single_point },
                        register_stuck: { fit: 15.0, class: residual },
                    }

                    mechanisms {
                        lockstep: { dc: 99.0 },
                        ecc: { dc: 99.5 },
                    }
                }
            }
        "#;
        let (tree, errors) = parse_with_errors(source);

        eprintln!("Errors: {:?}", errors);
        assert!(
            errors.is_empty(),
            "FMEDA library should parse without errors"
        );
        assert_eq!(tree.kind(), SyntaxKind::SourceFile);
    }
}
