use crate::diagnostics::VhdlError;
use crate::lexer::{self, Token, TokenWithPos};
use crate::syntax::{SyntaxKind, SyntaxNode, VhdlLanguage};
use rowan::GreenNodeBuilder;

pub struct ParseResult {
    pub root: SyntaxNode,
    pub errors: Vec<VhdlError>,
}

struct ParseState<'a> {
    tokens: Vec<TokenWithPos>,
    current: usize,
    builder: GreenNodeBuilder<'static>,
    source: &'a str,
    errors: Vec<VhdlError>,
    /// When true, `parse_expression()` will not consume trailing `when...else`
    inhibit_when_else: bool,
}

/// Map a lexer Token to its SyntaxKind for the rowan tree
fn token_to_syntax_kind(token: &Token) -> SyntaxKind {
    match token {
        Token::Entity => SyntaxKind::EntityKw,
        Token::Architecture => SyntaxKind::ArchitectureKw,
        Token::Package => SyntaxKind::PackageKw,
        Token::Body => SyntaxKind::BodyKw,
        Token::Configuration => SyntaxKind::ConfigurationKw,
        Token::Library => SyntaxKind::LibraryKw,
        Token::Use => SyntaxKind::UseKw,
        Token::Port => SyntaxKind::PortKw,
        Token::Generic => SyntaxKind::GenericKw,
        Token::Map => SyntaxKind::MapKw,
        Token::Component => SyntaxKind::ComponentKw,
        Token::In => SyntaxKind::InKw,
        Token::Out => SyntaxKind::OutKw,
        Token::Inout => SyntaxKind::InoutKw,
        Token::Buffer => SyntaxKind::BufferKw,
        Token::Signal => SyntaxKind::SignalKw,
        Token::Variable => SyntaxKind::VariableKw,
        Token::Constant => SyntaxKind::ConstantKw,
        Token::Type => SyntaxKind::TypeKw,
        Token::Subtype => SyntaxKind::SubtypeKw,
        Token::Alias => SyntaxKind::AliasKw,
        Token::Attribute => SyntaxKind::AttributeKw,
        Token::Array => SyntaxKind::ArrayKw,
        Token::Record => SyntaxKind::RecordKw,
        Token::Range => SyntaxKind::RangeKw,
        Token::Process => SyntaxKind::ProcessKw,
        Token::Begin => SyntaxKind::BeginKw,
        Token::End => SyntaxKind::EndKw,
        Token::Generate => SyntaxKind::GenerateKw,
        Token::Block => SyntaxKind::BlockKw,
        Token::If => SyntaxKind::IfKw,
        Token::Then => SyntaxKind::ThenKw,
        Token::Elsif => SyntaxKind::ElsifKw,
        Token::Else => SyntaxKind::ElseKw,
        Token::Case => SyntaxKind::CaseKw,
        Token::When => SyntaxKind::WhenKw,
        Token::For => SyntaxKind::ForKw,
        Token::Loop => SyntaxKind::LoopKw,
        Token::While => SyntaxKind::WhileKw,
        Token::Next => SyntaxKind::NextKw,
        Token::Exit => SyntaxKind::ExitKw,
        Token::Return => SyntaxKind::ReturnKw,
        Token::Null => SyntaxKind::NullKw,
        Token::Assert => SyntaxKind::AssertKw,
        Token::Report => SyntaxKind::ReportKw,
        Token::Severity => SyntaxKind::SeverityKw,
        Token::Is => SyntaxKind::IsKw,
        Token::Of => SyntaxKind::OfKw,
        Token::All => SyntaxKind::AllKw,
        Token::Others => SyntaxKind::OthersKw,
        Token::Open => SyntaxKind::OpenKw,
        Token::With => SyntaxKind::WithKw,
        Token::Select => SyntaxKind::SelectKw,
        Token::Unaffected => SyntaxKind::UnaffectedKw,
        Token::And => SyntaxKind::AndKw,
        Token::Or => SyntaxKind::OrKw,
        Token::Xor => SyntaxKind::XorKw,
        Token::Nand => SyntaxKind::NandKw,
        Token::Nor => SyntaxKind::NorKw,
        Token::Xnor => SyntaxKind::XnorKw,
        Token::Not => SyntaxKind::NotKw,
        Token::Mod => SyntaxKind::ModKw,
        Token::Rem => SyntaxKind::RemKw,
        Token::Abs => SyntaxKind::AbsKw,
        Token::Sll => SyntaxKind::SllKw,
        Token::Srl => SyntaxKind::SrlKw,
        Token::Sla => SyntaxKind::SlaKw,
        Token::Sra => SyntaxKind::SraKw,
        Token::Rol => SyntaxKind::RolKw,
        Token::Ror => SyntaxKind::RorKw,
        Token::To => SyntaxKind::ToKw,
        Token::Downto => SyntaxKind::DowntoKw,
        Token::Function => SyntaxKind::FunctionKw,
        Token::Procedure => SyntaxKind::ProcedureKw,
        Token::Impure => SyntaxKind::ImpureKw,
        Token::Pure => SyntaxKind::PureKw,
        Token::Wait => SyntaxKind::WaitKw,
        Token::After => SyntaxKind::AfterKw,
        Token::Transport => SyntaxKind::TransportKw,
        Token::Reject => SyntaxKind::RejectKw,
        Token::File => SyntaxKind::FileKw,
        Token::Access => SyntaxKind::AccessKw,
        Token::Shared => SyntaxKind::SharedKw,
        Token::New => SyntaxKind::NewKw,
        Token::Interface => SyntaxKind::InterfaceKw,
        Token::View => SyntaxKind::ViewKw,
        Token::Private => SyntaxKind::PrivateKw,
        Token::True => SyntaxKind::TrueKw,
        Token::False => SyntaxKind::FalseKw,
        Token::StdLogic => SyntaxKind::StdLogicKw,
        Token::StdUlogic => SyntaxKind::StdUlogicKw,
        Token::StdLogicVector => SyntaxKind::StdLogicVectorKw,
        Token::StdUlogicVector => SyntaxKind::StdUlogicVectorKw,
        Token::Unsigned => SyntaxKind::UnsignedKw,
        Token::Signed => SyntaxKind::SignedKw,
        Token::Boolean => SyntaxKind::BooleanKw,
        Token::Integer => SyntaxKind::IntegerKw,
        Token::Natural => SyntaxKind::NaturalKw,
        Token::Positive => SyntaxKind::PositiveKw,
        Token::Real => SyntaxKind::RealKw,
        Token::StringKw => SyntaxKind::StringKw,
        Token::Bit => SyntaxKind::BitKw,
        Token::BitVector => SyntaxKind::BitVectorKw,
        Token::RisingEdge => SyntaxKind::RisingEdgeKw,
        Token::FallingEdge => SyntaxKind::FallingEdgeKw,
        Token::ToUnsigned => SyntaxKind::ToUnsignedKw,
        Token::ToSigned => SyntaxKind::ToSignedKw,
        Token::ToInteger => SyntaxKind::ToIntegerKw,
        Token::Resize => SyntaxKind::ResizeKw,
        Token::ConvInteger => SyntaxKind::ConvIntegerKw,
        Token::ConvStdLogicVector => SyntaxKind::ConvStdLogicVectorKw,
        Token::Ident(_) => SyntaxKind::Ident,
        Token::IntLiteral => SyntaxKind::IntLiteral,
        Token::RealLiteral => SyntaxKind::RealLiteral,
        Token::StringLiteral => SyntaxKind::StringLiteral,
        Token::CharLiteral => SyntaxKind::CharLiteral,
        Token::BitStringLiteral => SyntaxKind::BitStringLiteral,
        Token::BasedLiteral => SyntaxKind::BasedLiteral,
        Token::SignalAssign => SyntaxKind::SignalAssign,
        Token::VarAssign => SyntaxKind::VarAssign,
        Token::Arrow => SyntaxKind::Arrow,
        Token::Box => SyntaxKind::BoxOp,
        Token::DoubleStar => SyntaxKind::DoubleStar,
        Token::NotEqual => SyntaxKind::NotEqual,
        Token::GreaterEqual => SyntaxKind::GreaterEqual,
        Token::DoubleLess => SyntaxKind::DoubleLess,
        Token::DoubleGreater => SyntaxKind::DoubleGreater,
        Token::LParen => SyntaxKind::LParen,
        Token::RParen => SyntaxKind::RParen,
        Token::LBracket => SyntaxKind::LBracket,
        Token::RBracket => SyntaxKind::RBracket,
        Token::Comma => SyntaxKind::Comma,
        Token::Semicolon => SyntaxKind::Semicolon,
        Token::Colon => SyntaxKind::Colon,
        Token::Dot => SyntaxKind::Dot,
        Token::Tick => SyntaxKind::Tick,
        Token::Ampersand => SyntaxKind::Ampersand,
        Token::Bar => SyntaxKind::Bar,
        Token::Plus => SyntaxKind::Plus,
        Token::Minus => SyntaxKind::Minus,
        Token::Star => SyntaxKind::Star,
        Token::Slash => SyntaxKind::Slash,
        Token::Equal => SyntaxKind::Equal,
        Token::LessThan => SyntaxKind::LessThan,
        Token::GreaterThan => SyntaxKind::GreaterThan,
        Token::At => SyntaxKind::At,
        Token::Comment => SyntaxKind::Comment,
    }
}

impl<'a> ParseState<'a> {
    fn new(source: &'a str) -> Self {
        let tokens = lexer::tokenize(source);
        Self {
            tokens,
            current: 0,
            builder: GreenNodeBuilder::new(),
            source,
            errors: Vec::new(),
            inhibit_when_else: false,
        }
    }

    // ====================================================================
    // Core parser helpers
    // ====================================================================

    fn start_node(&mut self, kind: SyntaxKind) {
        self.builder.start_node(kind.into());
    }

    fn finish_node(&mut self) {
        self.builder.finish_node();
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn current_token(&self) -> Option<&TokenWithPos> {
        self.tokens.get(self.current)
    }

    fn current_kind(&self) -> Option<SyntaxKind> {
        self.current_token().map(|t| token_to_syntax_kind(&t.token))
    }

    fn current_offset(&self) -> usize {
        self.current_token()
            .map(|t| t.offset)
            .unwrap_or(self.source.len())
    }

    fn current_end_offset(&self) -> usize {
        self.current_token()
            .map(|t| t.offset + t.text.len())
            .unwrap_or(self.source.len())
    }

    fn current_text(&self) -> &str {
        self.current_token().map(|t| t.text.as_str()).unwrap_or("")
    }

    fn peek_kind(&self, offset: usize) -> Option<SyntaxKind> {
        // Skip trivia tokens when peeking
        let mut pos = self.current;
        let mut skipped = 0;
        while pos < self.tokens.len() && skipped < offset {
            pos += 1;
            // Skip whitespace and comments while counting non-trivia
            while pos < self.tokens.len() {
                let kind = token_to_syntax_kind(&self.tokens[pos].token);
                if kind == SyntaxKind::Whitespace || kind == SyntaxKind::Comment {
                    pos += 1;
                } else {
                    break;
                }
            }
            skipped += 1;
        }
        self.tokens.get(pos).map(|t| token_to_syntax_kind(&t.token))
    }

    fn peek_raw(&self, offset: usize) -> Option<SyntaxKind> {
        self.tokens
            .get(self.current + offset)
            .map(|t| token_to_syntax_kind(&t.token))
    }

    fn bump(&mut self) {
        if let Some(tok) = self.tokens.get(self.current) {
            let kind = token_to_syntax_kind(&tok.token);
            self.builder.token(kind.into(), &tok.text);
            self.current += 1;
        }
    }

    fn skip_trivia(&mut self) {
        while let Some(kind) = self.current_kind() {
            if kind == SyntaxKind::Comment || kind == SyntaxKind::Whitespace {
                self.bump();
            } else {
                break;
            }
        }
    }

    fn at(&self, kind: SyntaxKind) -> bool {
        self.current_kind() == Some(kind)
    }

    fn at_token(&self, token: &Token) -> bool {
        self.current_token()
            .map(|t| &t.token == token)
            .unwrap_or(false)
    }

    fn expect(&mut self, kind: SyntaxKind) -> bool {
        self.skip_trivia();
        if self.at(kind) {
            self.bump();
            true
        } else {
            let pos = self.current_offset();
            let end = self.current_end_offset();
            let found = self
                .current_kind()
                .map(|k| format!("{:?}", k))
                .unwrap_or_else(|| "EOF".to_string());
            self.errors.push(VhdlError {
                kind: crate::diagnostics::VhdlErrorKind::ParseError,
                message: format!("expected {:?}, found {}", kind, found),
                position: pos,
                end_position: end,
                severity: crate::diagnostics::VhdlSeverity::Error,
            });
            false
        }
    }

    fn eat(&mut self, kind: SyntaxKind) -> bool {
        self.skip_trivia();
        if self.at(kind) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn error(&mut self, message: &str) {
        let pos = self.current_offset();
        let end = self.current_end_offset();
        self.errors.push(VhdlError {
            kind: crate::diagnostics::VhdlErrorKind::ParseError,
            message: message.to_string(),
            position: pos,
            end_position: end,
            severity: crate::diagnostics::VhdlSeverity::Error,
        });
    }

    fn error_recover(&mut self, message: &str) {
        self.error(message);
        // Skip to next semicolon or keyword
        self.start_node(SyntaxKind::Error);
        while !self.is_at_end() {
            if self.at(SyntaxKind::Semicolon) {
                self.bump();
                break;
            }
            let kind = self.current_kind();
            if matches!(
                kind,
                Some(SyntaxKind::EndKw)
                    | Some(SyntaxKind::BeginKw)
                    | Some(SyntaxKind::EntityKw)
                    | Some(SyntaxKind::ArchitectureKw)
                    | Some(SyntaxKind::ProcessKw)
                    | Some(SyntaxKind::IfKw)
                    | Some(SyntaxKind::CaseKw)
                    | Some(SyntaxKind::ForKw)
            ) {
                break;
            }
            self.bump();
        }
        self.finish_node();
    }

    /// Check for unsynthesizable constructs
    fn check_unsynthesizable(&mut self) -> bool {
        let kind = self.current_kind();
        let pos = self.current_offset();
        let end = self.current_end_offset();
        match kind {
            Some(SyntaxKind::WaitKw) => {
                self.errors.push(VhdlError {
                    kind: crate::diagnostics::VhdlErrorKind::Unsynthesizable(
                        "wait statement".to_string(),
                    ),
                    message: "wait statements are not synthesizable".to_string(),
                    position: pos,
                    end_position: end,
                    severity: crate::diagnostics::VhdlSeverity::Error,
                });
                true
            }
            Some(SyntaxKind::AfterKw) => {
                self.errors.push(VhdlError {
                    kind: crate::diagnostics::VhdlErrorKind::Unsynthesizable(
                        "after clause".to_string(),
                    ),
                    message: "after clauses are not synthesizable".to_string(),
                    position: pos,
                    end_position: end,
                    severity: crate::diagnostics::VhdlSeverity::Error,
                });
                true
            }
            Some(SyntaxKind::TransportKw) | Some(SyntaxKind::RejectKw) => {
                self.errors.push(VhdlError {
                    kind: crate::diagnostics::VhdlErrorKind::Unsynthesizable(
                        "transport/reject delay".to_string(),
                    ),
                    message: "transport/reject delay models are not synthesizable".to_string(),
                    position: pos,
                    end_position: end,
                    severity: crate::diagnostics::VhdlSeverity::Error,
                });
                true
            }
            Some(SyntaxKind::FileKw) => {
                self.errors.push(VhdlError {
                    kind: crate::diagnostics::VhdlErrorKind::Unsynthesizable(
                        "file declaration".to_string(),
                    ),
                    message: "file declarations are not synthesizable".to_string(),
                    position: pos,
                    end_position: end,
                    severity: crate::diagnostics::VhdlSeverity::Error,
                });
                true
            }
            Some(SyntaxKind::AccessKw) => {
                self.errors.push(VhdlError {
                    kind: crate::diagnostics::VhdlErrorKind::Unsynthesizable(
                        "access type".to_string(),
                    ),
                    message: "access types are not synthesizable".to_string(),
                    position: pos,
                    end_position: end,
                    severity: crate::diagnostics::VhdlSeverity::Error,
                });
                true
            }
            _ => false,
        }
    }

    /// Check if current identifier text matches (case-insensitive)
    fn current_ident_is(&self, name: &str) -> bool {
        match &self.current_token().map(|t| &t.token) {
            Some(Token::Ident(s)) => s == name,
            _ => false,
        }
    }

    /// Get the current identifier text
    fn current_ident_text(&self) -> Option<&str> {
        match self.current_token() {
            Some(TokenWithPos {
                token: Token::Ident(s),
                ..
            }) => Some(s.as_str()),
            _ => None,
        }
    }

    // ====================================================================
    // Grammar: top-level
    // ====================================================================

    fn parse_source_file(mut self) -> ParseResult {
        self.start_node(SyntaxKind::SourceFile);

        while !self.is_at_end() {
            self.skip_trivia();
            if self.is_at_end() {
                break;
            }

            let pos_before = self.current;

            match self.current_kind() {
                Some(SyntaxKind::LibraryKw) => self.parse_library_clause(),
                Some(SyntaxKind::UseKw) => self.parse_use_clause(),
                Some(SyntaxKind::EntityKw) => self.parse_entity_decl(),
                Some(SyntaxKind::ArchitectureKw) => self.parse_architecture_body(),
                Some(SyntaxKind::PackageKw) => self.parse_package_or_body(),
                Some(SyntaxKind::InterfaceKw) => self.parse_interface_decl(),
                Some(SyntaxKind::ViewKw) => self.parse_view_decl(),
                _ => {
                    self.error_recover("unexpected token at top level");
                }
            }

            // Safety: if no progress was made, force advance to avoid infinite loop
            if self.current == pos_before && !self.is_at_end() {
                self.bump();
            }
        }

        self.finish_node();

        let green = self.builder.finish();
        ParseResult {
            root: SyntaxNode::new_root(green),
            errors: self.errors,
        }
    }

    // ====================================================================
    // Grammar: library/use
    // ====================================================================

    fn parse_library_clause(&mut self) {
        self.start_node(SyntaxKind::LibraryClause);
        self.expect(SyntaxKind::LibraryKw);
        self.skip_trivia();
        // library name(s)
        self.bump(); // identifier
        while self.eat(SyntaxKind::Comma) {
            self.skip_trivia();
            self.bump(); // next library name
        }
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_use_clause(&mut self) {
        self.start_node(SyntaxKind::UseClause);
        self.expect(SyntaxKind::UseKw);
        self.skip_trivia();
        // Parse selected name: lib.pkg.item or lib.pkg.all
        self.parse_selected_name();
        while self.eat(SyntaxKind::Comma) {
            self.skip_trivia();
            self.parse_selected_name();
        }
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_selected_name(&mut self) {
        self.start_node(SyntaxKind::SelectedName);
        // First component
        self.bump(); // ident or keyword like 'all'
        while self.eat(SyntaxKind::Dot) {
            self.skip_trivia();
            self.bump(); // next component (ident or 'all')
        }
        self.finish_node();
    }

    // ====================================================================
    // Grammar: entity
    // ====================================================================

    fn parse_entity_decl(&mut self) {
        self.start_node(SyntaxKind::EntityDecl);
        self.expect(SyntaxKind::EntityKw);
        self.skip_trivia();
        // Entity name
        self.bump(); // ident
        self.skip_trivia();
        self.expect(SyntaxKind::IsKw);
        self.skip_trivia();

        // Optional generic clause
        if self.at(SyntaxKind::GenericKw) {
            self.parse_generic_clause();
            self.skip_trivia();
        }

        // Optional port clause
        if self.at(SyntaxKind::PortKw) {
            self.parse_port_clause();
            self.skip_trivia();
        }

        // Entity declarative region (between port clause and 'end')
        // VHDL allows: attribute decl/spec, signal, constant, type, subtype,
        // alias, component, function, procedure in entity declarative parts.
        self.parse_entity_declarations();

        // Optional entity statement part: begin { concurrent_statement }
        // Used for passive processes/assertions in entity declarations
        if self.at(SyntaxKind::BeginKw) {
            self.bump(); // begin
            self.skip_trivia();
            // Parse concurrent statements until 'end'
            self.parse_concurrent_statements();
        }

        // end [entity] [name] ;
        self.expect(SyntaxKind::EndKw);
        self.skip_trivia();
        // Optional 'entity' keyword
        self.eat(SyntaxKind::EntityKw);
        self.skip_trivia();
        // Optional entity name
        if !self.at(SyntaxKind::Semicolon) {
            self.bump(); // entity name
            self.skip_trivia();
        }
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_entity_declarations(&mut self) {
        loop {
            self.skip_trivia();
            if self.is_at_end() || self.at(SyntaxKind::EndKw) || self.at(SyntaxKind::BeginKw) {
                break;
            }
            let pos_before = self.current;
            match self.current_kind() {
                Some(SyntaxKind::AttributeKw) => self.parse_attribute_decl_or_spec(),
                Some(SyntaxKind::SignalKw) => self.parse_signal_decl(),
                Some(SyntaxKind::ConstantKw) => self.parse_constant_decl(),
                Some(SyntaxKind::TypeKw) => self.parse_type_decl(),
                Some(SyntaxKind::SubtypeKw) => self.parse_subtype_decl(),
                Some(SyntaxKind::ComponentKw) => self.parse_component_decl(),
                Some(SyntaxKind::AliasKw) => self.parse_alias_decl(),
                Some(SyntaxKind::UseKw) => self.parse_use_clause(),
                Some(SyntaxKind::FunctionKw)
                | Some(SyntaxKind::PureKw)
                | Some(SyntaxKind::ImpureKw) => {
                    self.parse_function_decl_or_body();
                }
                Some(SyntaxKind::ProcedureKw) => {
                    self.parse_procedure_decl_or_body();
                }
                _ => break,
            }
            if self.current == pos_before && !self.is_at_end() {
                self.bump();
            }
        }
    }

    fn parse_generic_clause(&mut self) {
        self.start_node(SyntaxKind::GenericClause);
        self.expect(SyntaxKind::GenericKw);
        self.skip_trivia();
        self.expect(SyntaxKind::LParen);
        self.skip_trivia();

        // Parse generic declarations
        if !self.at(SyntaxKind::RParen) {
            self.parse_generic_decl();
            while self.eat(SyntaxKind::Semicolon) {
                self.skip_trivia();
                if self.at(SyntaxKind::RParen) {
                    break;
                }
                self.parse_generic_decl();
            }
        }

        self.expect(SyntaxKind::RParen);
        self.skip_trivia();
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_generic_decl(&mut self) {
        self.start_node(SyntaxKind::GenericDecl);
        self.skip_trivia();

        // Check for generic type parameter: type T [is (<>) [range <>]]
        if self.at(SyntaxKind::TypeKw) {
            self.bump(); // type keyword
            self.skip_trivia();
            self.bump(); // type parameter name (ident)
            self.skip_trivia();
            // Optional constraint: is (<>) or is (<>) range <>
            if self.at(SyntaxKind::IsKw) {
                self.bump(); // is
                self.skip_trivia();
                // Parse parenthesized constraint like (<>) or (<>) range <>
                if self.at(SyntaxKind::LParen) {
                    let mut depth = 0;
                    loop {
                        if self.is_at_end() {
                            break;
                        }
                        if self.at(SyntaxKind::LParen) {
                            depth += 1;
                        }
                        if self.at(SyntaxKind::RParen) {
                            depth -= 1;
                            self.bump();
                            if depth == 0 {
                                break;
                            }
                            continue;
                        }
                        self.bump();
                        self.skip_trivia();
                    }
                    self.skip_trivia();
                    // Optional "range <>"
                    if self.at(SyntaxKind::RangeKw) {
                        self.bump(); // range
                        self.skip_trivia();
                        if self.at(SyntaxKind::BoxOp) {
                            self.bump(); // <>
                        }
                    }
                }
            }
            self.finish_node();
            return;
        }

        // Regular value generic: name [, name]* : [in] type [:= default]
        self.bump(); // first name
        while self.eat(SyntaxKind::Comma) {
            self.skip_trivia();
            self.bump(); // next name
        }
        self.skip_trivia();
        self.expect(SyntaxKind::Colon);
        self.skip_trivia();
        // Optional direction keyword (VHDL allows 'in' in generic interface constants)
        if self.at(SyntaxKind::InKw) {
            self.bump();
            self.skip_trivia();
        }
        self.parse_subtype_indication();
        self.skip_trivia();
        // Optional default value
        if self.at(SyntaxKind::VarAssign) {
            self.bump(); // :=
            self.skip_trivia();
            self.parse_expression();
        }
        self.finish_node();
    }

    fn parse_port_clause(&mut self) {
        self.start_node(SyntaxKind::PortClause);
        self.expect(SyntaxKind::PortKw);
        self.skip_trivia();
        self.expect(SyntaxKind::LParen);
        self.skip_trivia();

        // Parse port declarations
        if !self.at(SyntaxKind::RParen) {
            self.parse_port_decl();
            while self.eat(SyntaxKind::Semicolon) {
                self.skip_trivia();
                if self.at(SyntaxKind::RParen) {
                    break;
                }
                self.parse_port_decl();
            }
        }

        self.expect(SyntaxKind::RParen);
        self.skip_trivia();
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_port_decl(&mut self) {
        self.start_node(SyntaxKind::PortDecl);
        self.skip_trivia();
        // name [, name]* : direction type
        self.bump(); // first name
        while self.eat(SyntaxKind::Comma) {
            self.skip_trivia();
            self.bump(); // next name
        }
        self.skip_trivia();
        self.expect(SyntaxKind::Colon);
        self.skip_trivia();

        // View port: `name : view view_name` — no PortDirection or SubtypeIndication
        if self.at(SyntaxKind::ViewKw) {
            self.bump(); // 'view'
            self.skip_trivia();
            self.bump(); // view name
            self.finish_node();
            return;
        }

        // Direction: in, out, inout, buffer (default: in, if omitted)
        self.start_node(SyntaxKind::PortDirection);
        if matches!(
            self.current_kind(),
            Some(SyntaxKind::InKw)
                | Some(SyntaxKind::OutKw)
                | Some(SyntaxKind::InoutKw)
                | Some(SyntaxKind::BufferKw)
        ) {
            self.bump();
        }
        // else: no explicit direction — defaults to 'in' per VHDL standard
        self.finish_node();
        self.skip_trivia();

        // Type
        self.parse_subtype_indication();

        // Optional default value
        self.skip_trivia();
        if self.at(SyntaxKind::VarAssign) {
            self.bump(); // :=
            self.skip_trivia();
            self.parse_expression();
        }

        self.finish_node();
    }

    // ====================================================================
    // Grammar: architecture
    // ====================================================================

    fn parse_architecture_body(&mut self) {
        self.start_node(SyntaxKind::ArchitectureBody);
        self.expect(SyntaxKind::ArchitectureKw);
        self.skip_trivia();
        self.bump(); // architecture name
        self.skip_trivia();
        self.expect(SyntaxKind::OfKw);
        self.skip_trivia();
        self.bump(); // entity name
        self.skip_trivia();
        self.expect(SyntaxKind::IsKw);
        self.skip_trivia();

        // Declarative region
        self.parse_architecture_declarations();

        self.expect(SyntaxKind::BeginKw);
        self.skip_trivia();

        // Concurrent statement region
        self.parse_concurrent_statements();

        self.expect(SyntaxKind::EndKw);
        self.skip_trivia();
        self.eat(SyntaxKind::ArchitectureKw);
        self.skip_trivia();
        if !self.at(SyntaxKind::Semicolon) {
            self.bump(); // architecture name
            self.skip_trivia();
        }
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_architecture_declarations(&mut self) {
        loop {
            self.skip_trivia();
            if self.is_at_end() || self.at(SyntaxKind::BeginKw) {
                break;
            }
            let pos_before = self.current;
            match self.current_kind() {
                Some(SyntaxKind::SignalKw) => self.parse_signal_decl(),
                Some(SyntaxKind::ConstantKw) => self.parse_constant_decl(),
                Some(SyntaxKind::VariableKw) => self.parse_variable_decl(),
                Some(SyntaxKind::TypeKw) => self.parse_type_decl(),
                Some(SyntaxKind::SubtypeKw) => self.parse_subtype_decl(),
                Some(SyntaxKind::ComponentKw) => self.parse_component_decl(),
                Some(SyntaxKind::UseKw) => self.parse_use_clause(),
                Some(SyntaxKind::FunctionKw)
                | Some(SyntaxKind::PureKw)
                | Some(SyntaxKind::ImpureKw) => {
                    self.parse_function_decl_or_body();
                }
                Some(SyntaxKind::ProcedureKw) => {
                    self.parse_procedure_decl_or_body();
                }
                Some(SyntaxKind::AliasKw) => self.parse_alias_decl(),
                Some(SyntaxKind::AttributeKw) => self.parse_attribute_decl_or_spec(),
                Some(SyntaxKind::FileKw) | Some(SyntaxKind::SharedKw) => {
                    self.check_unsynthesizable();
                    self.error_recover("unsynthesizable declaration");
                }
                _ => break,
            }
            // Safety: if no progress was made, force advance to avoid infinite loop
            if self.current == pos_before && !self.is_at_end() {
                self.bump();
            }
        }
    }

    // ====================================================================
    // Grammar: declarations
    // ====================================================================

    fn parse_signal_decl(&mut self) {
        self.start_node(SyntaxKind::SignalDecl);
        self.expect(SyntaxKind::SignalKw);
        self.skip_trivia();
        // name [, name]* : type [:= expr] ;
        self.bump(); // first name
        while self.eat(SyntaxKind::Comma) {
            self.skip_trivia();
            self.bump();
        }
        self.skip_trivia();
        self.expect(SyntaxKind::Colon);
        self.skip_trivia();
        self.parse_subtype_indication();
        self.skip_trivia();
        if self.at(SyntaxKind::VarAssign) {
            self.bump(); // :=
            self.skip_trivia();
            self.parse_expression();
        }
        self.skip_trivia();
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_constant_decl(&mut self) {
        self.start_node(SyntaxKind::ConstantDecl);
        self.expect(SyntaxKind::ConstantKw);
        self.skip_trivia();
        self.bump(); // name
        self.skip_trivia();
        self.expect(SyntaxKind::Colon);
        self.skip_trivia();
        self.parse_subtype_indication();
        self.skip_trivia();
        if self.at(SyntaxKind::VarAssign) {
            self.bump(); // :=
            self.skip_trivia();
            self.parse_expression();
        }
        self.skip_trivia();
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_variable_decl(&mut self) {
        self.start_node(SyntaxKind::VariableDecl);
        self.expect(SyntaxKind::VariableKw);
        self.skip_trivia();
        self.bump(); // name
        while self.eat(SyntaxKind::Comma) {
            self.skip_trivia();
            self.bump();
        }
        self.skip_trivia();
        self.expect(SyntaxKind::Colon);
        self.skip_trivia();
        self.parse_subtype_indication();
        self.skip_trivia();
        if self.at(SyntaxKind::VarAssign) {
            self.bump(); // :=
            self.skip_trivia();
            self.parse_expression();
        }
        self.skip_trivia();
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_type_decl(&mut self) {
        self.start_node(SyntaxKind::TypeDecl);
        self.expect(SyntaxKind::TypeKw);
        self.skip_trivia();
        self.bump(); // type name
        self.skip_trivia();
        self.expect(SyntaxKind::IsKw);
        self.skip_trivia();

        match self.current_kind() {
            Some(SyntaxKind::LParen) => self.parse_enum_type_def(),
            Some(SyntaxKind::RecordKw) => self.parse_record_type_def(),
            Some(SyntaxKind::ArrayKw) => self.parse_array_type_def(),
            Some(SyntaxKind::RangeKw) => {
                // range <> is unconstrained
                self.bump(); // range
                self.skip_trivia();
                self.parse_expression(); // or <>
            }
            _ => {
                // subtype or other
                self.parse_subtype_indication();
            }
        }
        self.skip_trivia();
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_enum_type_def(&mut self) {
        self.start_node(SyntaxKind::EnumTypeDef);
        self.expect(SyntaxKind::LParen);
        self.skip_trivia();
        self.bump(); // first variant (ident or char literal)
        while self.eat(SyntaxKind::Comma) {
            self.skip_trivia();
            self.bump(); // next variant
        }
        self.skip_trivia();
        self.expect(SyntaxKind::RParen);
        self.finish_node();
    }

    fn parse_record_type_def(&mut self) {
        self.start_node(SyntaxKind::RecordTypeDef);
        self.expect(SyntaxKind::RecordKw);
        self.skip_trivia();

        while !self.is_at_end() && !self.at(SyntaxKind::EndKw) {
            self.start_node(SyntaxKind::RecordField);
            self.skip_trivia();
            self.bump(); // field name
            while self.eat(SyntaxKind::Comma) {
                self.skip_trivia();
                self.bump();
            }
            self.skip_trivia();
            self.expect(SyntaxKind::Colon);
            self.skip_trivia();
            self.parse_subtype_indication();
            self.skip_trivia();
            self.expect(SyntaxKind::Semicolon);
            self.finish_node();
            self.skip_trivia();
        }

        self.expect(SyntaxKind::EndKw);
        self.skip_trivia();
        self.expect(SyntaxKind::RecordKw);
        self.skip_trivia();
        // Optional name after end record
        if !self.at(SyntaxKind::Semicolon) {
            self.bump();
            self.skip_trivia();
        }
        self.finish_node();
    }

    fn parse_array_type_def(&mut self) {
        self.start_node(SyntaxKind::ArrayTypeDef);
        self.expect(SyntaxKind::ArrayKw);
        self.skip_trivia();
        self.expect(SyntaxKind::LParen);
        self.skip_trivia();
        // Index range(s) — may be multi-dimensional: (range1, range2, ...)
        self.parse_discrete_range();
        self.skip_trivia();
        while self.eat(SyntaxKind::Comma) {
            self.skip_trivia();
            self.parse_discrete_range();
            self.skip_trivia();
        }
        self.expect(SyntaxKind::RParen);
        self.skip_trivia();
        self.expect(SyntaxKind::OfKw);
        self.skip_trivia();
        self.parse_subtype_indication();
        self.finish_node();
    }

    fn parse_discrete_range(&mut self) {
        self.start_node(SyntaxKind::DiscreteRange);
        // Could be: "0 to N", "N-1 downto 0", "natural range <>", "type_name range expression"
        self.parse_expression();
        self.skip_trivia();
        match self.current_kind() {
            Some(SyntaxKind::ToKw) | Some(SyntaxKind::DowntoKw) => {
                self.bump(); // to/downto
                self.skip_trivia();
                self.parse_expression();
            }
            Some(SyntaxKind::RangeKw) => {
                self.bump(); // range
                self.skip_trivia();
                if self.at(SyntaxKind::BoxOp) {
                    self.bump(); // <>
                } else {
                    self.parse_expression();
                    self.skip_trivia();
                    if self.at(SyntaxKind::ToKw) || self.at(SyntaxKind::DowntoKw) {
                        self.bump();
                        self.skip_trivia();
                        self.parse_expression();
                    }
                }
            }
            _ => {}
        }
        self.finish_node();
    }

    fn parse_subtype_decl(&mut self) {
        self.start_node(SyntaxKind::SubtypeDecl);
        self.expect(SyntaxKind::SubtypeKw);
        self.skip_trivia();
        self.bump(); // subtype name
        self.skip_trivia();
        self.expect(SyntaxKind::IsKw);
        self.skip_trivia();
        self.parse_subtype_indication();
        self.skip_trivia();
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_component_decl(&mut self) {
        self.start_node(SyntaxKind::ComponentDecl);
        self.expect(SyntaxKind::ComponentKw);
        self.skip_trivia();
        self.bump(); // component name
        self.skip_trivia();
        if self.at(SyntaxKind::IsKw) {
            self.bump();
            self.skip_trivia();
        }
        // Optional generic clause
        if self.at(SyntaxKind::GenericKw) {
            self.parse_generic_clause();
            self.skip_trivia();
        }
        // Optional port clause
        if self.at(SyntaxKind::PortKw) {
            self.parse_port_clause();
            self.skip_trivia();
        }
        self.expect(SyntaxKind::EndKw);
        self.skip_trivia();
        self.eat(SyntaxKind::ComponentKw);
        self.skip_trivia();
        if !self.at(SyntaxKind::Semicolon) {
            self.bump(); // optional name
            self.skip_trivia();
        }
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_alias_decl(&mut self) {
        self.start_node(SyntaxKind::AliasDecl);
        self.expect(SyntaxKind::AliasKw);
        self.skip_trivia();
        self.bump(); // alias name
        self.skip_trivia();
        // Optional : type
        if self.at(SyntaxKind::Colon) {
            self.bump();
            self.skip_trivia();
            self.parse_subtype_indication();
            self.skip_trivia();
        }
        self.expect(SyntaxKind::IsKw);
        self.skip_trivia();
        self.parse_expression(); // the aliased name/expression
        self.skip_trivia();
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_attribute_decl_or_spec(&mut self) {
        // Both start with 'attribute ident'
        // Declaration: attribute name : type ;
        // Specification: attribute name of target : class is expression ;
        let is_decl = self.peek_kind(2) == Some(SyntaxKind::Colon);
        let node_kind = if is_decl {
            SyntaxKind::AttributeDecl
        } else {
            SyntaxKind::AttributeSpec
        };
        self.start_node(node_kind);
        self.expect(SyntaxKind::AttributeKw);
        self.skip_trivia();
        self.bump(); // attribute name
        self.skip_trivia();
        // Skip to semicolon — attributes are synthesis tool metadata
        while !self.is_at_end() && !self.at(SyntaxKind::Semicolon) {
            self.bump();
            self.skip_trivia();
        }
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_function_decl_or_body(&mut self) {
        self.start_node(SyntaxKind::FunctionBody);
        // [pure|impure] function name [(params)] return type is ... begin ... end;
        if self.at(SyntaxKind::PureKw) || self.at(SyntaxKind::ImpureKw) {
            self.bump();
            self.skip_trivia();
        }
        self.expect(SyntaxKind::FunctionKw);
        self.skip_trivia();
        self.bump(); // function name
        self.skip_trivia();

        // Optional parameter list
        if self.at(SyntaxKind::LParen) {
            self.parse_param_list();
            self.skip_trivia();
        }

        self.expect(SyntaxKind::ReturnKw);
        self.skip_trivia();
        self.parse_subtype_indication();
        self.skip_trivia();

        if self.at(SyntaxKind::IsKw) {
            // Function body
            self.bump(); // is
            self.skip_trivia();
            // Declarations
            self.parse_process_declarations();
            self.expect(SyntaxKind::BeginKw);
            self.skip_trivia();
            self.parse_sequential_statements();
            self.expect(SyntaxKind::EndKw);
            self.skip_trivia();
            self.eat(SyntaxKind::FunctionKw);
            self.skip_trivia();
            if !self.at(SyntaxKind::Semicolon) {
                self.bump(); // optional name
                self.skip_trivia();
            }
        }
        // else: just a declaration, end at ;
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_procedure_decl_or_body(&mut self) {
        self.start_node(SyntaxKind::ProcedureBody);
        self.expect(SyntaxKind::ProcedureKw);
        self.skip_trivia();
        self.bump(); // procedure name
        self.skip_trivia();

        if self.at(SyntaxKind::LParen) {
            self.parse_param_list();
            self.skip_trivia();
        }

        if self.at(SyntaxKind::IsKw) {
            self.bump(); // is
            self.skip_trivia();
            self.parse_process_declarations();
            self.expect(SyntaxKind::BeginKw);
            self.skip_trivia();
            self.parse_sequential_statements();
            self.expect(SyntaxKind::EndKw);
            self.skip_trivia();
            self.eat(SyntaxKind::ProcedureKw);
            self.skip_trivia();
            if !self.at(SyntaxKind::Semicolon) {
                self.bump();
                self.skip_trivia();
            }
        }
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_param_list(&mut self) {
        self.start_node(SyntaxKind::ParamList);
        self.expect(SyntaxKind::LParen);
        self.skip_trivia();
        if !self.at(SyntaxKind::RParen) {
            self.parse_param_decl();
            while self.eat(SyntaxKind::Semicolon) {
                self.skip_trivia();
                if self.at(SyntaxKind::RParen) {
                    break;
                }
                self.parse_param_decl();
            }
        }
        self.expect(SyntaxKind::RParen);
        self.finish_node();
    }

    fn parse_param_decl(&mut self) {
        self.start_node(SyntaxKind::ParamDecl);
        self.skip_trivia();
        // Optional: signal|variable|constant|file
        if matches!(
            self.current_kind(),
            Some(SyntaxKind::SignalKw)
                | Some(SyntaxKind::VariableKw)
                | Some(SyntaxKind::ConstantKw)
                | Some(SyntaxKind::FileKw)
        ) {
            self.bump();
            self.skip_trivia();
        }
        // name [, name]*
        self.bump(); // first name
        while self.eat(SyntaxKind::Comma) {
            self.skip_trivia();
            self.bump();
        }
        self.skip_trivia();
        self.expect(SyntaxKind::Colon);
        self.skip_trivia();
        // Optional direction
        if matches!(
            self.current_kind(),
            Some(SyntaxKind::InKw) | Some(SyntaxKind::OutKw) | Some(SyntaxKind::InoutKw)
        ) {
            self.bump();
            self.skip_trivia();
        }
        self.parse_subtype_indication();
        self.skip_trivia();
        if self.at(SyntaxKind::VarAssign) {
            self.bump();
            self.skip_trivia();
            self.parse_expression();
        }
        self.finish_node();
    }

    // ====================================================================
    // Grammar: concurrent statements
    // ====================================================================

    fn parse_concurrent_statements(&mut self) {
        loop {
            self.skip_trivia();
            if self.is_at_end() || self.at(SyntaxKind::EndKw) {
                break;
            }

            let pos_before = self.current;

            if self.check_unsynthesizable() {
                self.error_recover("unsynthesizable construct");
                if self.current == pos_before {
                    self.bump(); // force progress to avoid infinite loop
                }
                continue;
            }

            match self.current_kind() {
                Some(SyntaxKind::ProcessKw) => self.parse_process_stmt(None),
                Some(SyntaxKind::WithKw) => self.parse_selected_assign(),
                Some(SyntaxKind::ForKw) => self.parse_for_generate(),
                Some(SyntaxKind::IfKw) => self.parse_if_generate(),
                Some(SyntaxKind::AssertKw) => {
                    // Concurrent assert — parse properly but silently ignore
                    // (simulation-only construct, ubiquitous in real VHDL)
                    self.parse_assert_stmt();
                }
                Some(SyntaxKind::ReportKw) => {
                    // Standalone report (equivalent to assert false report ...)
                    self.parse_report_stmt();
                }
                Some(SyntaxKind::ComponentKw) => {
                    // Component declaration in architecture (should be in declarative region
                    // but some designs put them here)
                    self.parse_component_decl();
                }
                Some(SyntaxKind::Ident) => {
                    // Could be:
                    // 1. label: process/component/for generate
                    // 2. signal <= expr (concurrent assignment)
                    // 3. label: entity work.Foo port map (...)
                    // 4. label: Comp port map / generic map (...)
                    self.parse_concurrent_statement_starting_with_ident();
                }
                // Type/function keywords also treated as identifiers for assignments
                _ if self.is_name_start() => {
                    self.parse_concurrent_signal_assign_or_conditional();
                }
                _ => {
                    self.error_recover("unexpected token in concurrent region");
                }
            }

            // Safety: if no progress was made, force advance to avoid infinite loop
            if self.current == pos_before && !self.is_at_end() {
                self.bump();
            }
        }
    }

    fn is_name_start(&self) -> bool {
        matches!(
            self.current_kind(),
            Some(SyntaxKind::Ident)
                | Some(SyntaxKind::StdLogicKw)
                | Some(SyntaxKind::StdLogicVectorKw)
                | Some(SyntaxKind::StdUlogicVectorKw)
                | Some(SyntaxKind::UnsignedKw)
                | Some(SyntaxKind::SignedKw)
                | Some(SyntaxKind::StdUlogicKw)
                | Some(SyntaxKind::BooleanKw)
                | Some(SyntaxKind::IntegerKw)
                | Some(SyntaxKind::NaturalKw)
                | Some(SyntaxKind::PositiveKw)
                | Some(SyntaxKind::RealKw)
                | Some(SyntaxKind::BitKw)
                | Some(SyntaxKind::BitVectorKw)
        )
    }

    fn parse_concurrent_statement_starting_with_ident(&mut self) {
        // Peek to determine what this is
        // label: ... -> labeled statement
        // name <= ... -> concurrent signal assignment
        // name(...) -> could be a component instantiation with positional

        // Look ahead past the identifier and potential parenthesized suffix, dots
        let saved_pos = self.current;

        // Check if it's a label (ident followed by colon NOT followed by =)
        if self.at(SyntaxKind::Ident) && self.peek_kind(1) == Some(SyntaxKind::Colon) {
            // It's a label
            let label_start = self.current;
            self.bump(); // label ident
            self.skip_trivia();
            self.bump(); // colon
            self.skip_trivia();

            // What follows the label?
            match self.current_kind() {
                Some(SyntaxKind::ProcessKw) => {
                    self.parse_process_stmt(Some(label_start));
                    return;
                }
                Some(SyntaxKind::ForKw) => {
                    self.parse_for_generate();
                    return;
                }
                Some(SyntaxKind::IfKw) => {
                    self.parse_if_generate();
                    return;
                }
                Some(SyntaxKind::BlockKw) => {
                    self.parse_block_stmt();
                    return;
                }
                Some(SyntaxKind::EntityKw) => {
                    // Direct entity instantiation: label: entity work.Foo port map (...)
                    self.parse_component_inst_entity();
                    return;
                }
                Some(SyntaxKind::Ident) | Some(SyntaxKind::ComponentKw) => {
                    // Component instantiation: label: CompName [generic map (...)] port map (...)
                    self.parse_component_inst_component();
                    return;
                }
                _ => {
                    self.error_recover("unexpected token after label");
                    return;
                }
            }
        }

        // Not a label — must be a concurrent signal assignment or conditional
        self.current = saved_pos;
        self.parse_concurrent_signal_assign_or_conditional();
    }

    fn parse_concurrent_signal_assign_or_conditional(&mut self) {
        // Parse target/name
        // If followed by <= : concurrent signal assignment
        // If followed by ;  : concurrent procedure call (name already includes args)

        self.start_node(SyntaxKind::ConcurrentSignalAssign);
        self.parse_name();
        self.skip_trivia();

        // Concurrent procedure call: name(args) ;
        if self.at(SyntaxKind::Semicolon) {
            self.bump(); // ;
            self.finish_node();
            return;
        }

        if !self.at(SyntaxKind::SignalAssign) {
            self.finish_node();
            self.error_recover("expected '<=' or ':=' in assignment");
            return;
        }
        self.bump(); // <=
        self.skip_trivia();

        // Parse RHS — check for conditional (expr when cond else ...)
        self.parse_expression();
        self.skip_trivia();

        if self.at(SyntaxKind::WhenKw) {
            // Conditional assignment — parse when/else chain
            while self.at(SyntaxKind::WhenKw) {
                self.bump(); // when
                self.skip_trivia();
                self.parse_expression(); // condition
                self.skip_trivia();
                if self.at(SyntaxKind::ElseKw) {
                    self.bump(); // else
                    self.skip_trivia();
                    self.parse_expression(); // next value
                    self.skip_trivia();
                }
            }
        }

        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_selected_assign(&mut self) {
        self.start_node(SyntaxKind::SelectedAssign);
        self.expect(SyntaxKind::WithKw);
        self.skip_trivia();
        self.parse_expression(); // selector expression
        self.skip_trivia();
        self.expect(SyntaxKind::SelectKw);
        self.skip_trivia();
        self.parse_name(); // target
        self.skip_trivia();
        self.expect(SyntaxKind::SignalAssign);
        self.skip_trivia();

        // value when choices, value when choices, ... value when others
        // Inhibit when...else expression parsing so `when` is consumed at statement level
        let prev = self.inhibit_when_else;
        self.inhibit_when_else = true;
        loop {
            self.parse_expression(); // value
            self.skip_trivia();
            self.expect(SyntaxKind::WhenKw);
            self.skip_trivia();
            self.parse_choices();
            self.skip_trivia();
            if self.eat(SyntaxKind::Comma) {
                self.skip_trivia();
            } else {
                break;
            }
        }
        self.inhibit_when_else = prev;
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_process_stmt(&mut self, _label_pos: Option<usize>) {
        self.start_node(SyntaxKind::ProcessStmt);
        self.expect(SyntaxKind::ProcessKw);
        self.skip_trivia();

        // Optional sensitivity list
        if self.at(SyntaxKind::LParen) {
            self.parse_sensitivity_list();
            self.skip_trivia();
        }

        // Optional 'is'
        self.eat(SyntaxKind::IsKw);
        self.skip_trivia();

        // Process declarations (variables, types, etc.)
        self.parse_process_declarations();

        self.expect(SyntaxKind::BeginKw);
        self.skip_trivia();

        // Sequential statements
        self.parse_sequential_statements();

        self.expect(SyntaxKind::EndKw);
        self.skip_trivia();
        self.eat(SyntaxKind::ProcessKw);
        self.skip_trivia();
        // Optional label
        if !self.at(SyntaxKind::Semicolon) && self.at(SyntaxKind::Ident) {
            self.bump();
            self.skip_trivia();
        }
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_sensitivity_list(&mut self) {
        self.start_node(SyntaxKind::SensitivityList);
        self.expect(SyntaxKind::LParen);
        self.skip_trivia();

        if self.at(SyntaxKind::AllKw) {
            self.bump(); // all
        } else if !self.at(SyntaxKind::RParen) {
            self.parse_name();
            while self.eat(SyntaxKind::Comma) {
                self.skip_trivia();
                self.parse_name();
            }
        }
        self.skip_trivia();
        self.expect(SyntaxKind::RParen);
        self.finish_node();
    }

    fn parse_process_declarations(&mut self) {
        loop {
            self.skip_trivia();
            if self.is_at_end() || self.at(SyntaxKind::BeginKw) {
                break;
            }
            match self.current_kind() {
                Some(SyntaxKind::VariableKw) => self.parse_variable_decl(),
                Some(SyntaxKind::ConstantKw) => self.parse_constant_decl(),
                Some(SyntaxKind::TypeKw) => self.parse_type_decl(),
                Some(SyntaxKind::SubtypeKw) => self.parse_subtype_decl(),
                Some(SyntaxKind::UseKw) => self.parse_use_clause(),
                Some(SyntaxKind::FunctionKw)
                | Some(SyntaxKind::PureKw)
                | Some(SyntaxKind::ImpureKw) => {
                    self.parse_function_decl_or_body();
                }
                Some(SyntaxKind::ProcedureKw) => {
                    self.parse_procedure_decl_or_body();
                }
                Some(SyntaxKind::AliasKw) => self.parse_alias_decl(),
                _ => break,
            }
        }
    }

    // ====================================================================
    // Grammar: sequential statements
    // ====================================================================

    fn parse_sequential_statements(&mut self) {
        loop {
            self.skip_trivia();
            if self.is_at_end()
                || self.at(SyntaxKind::EndKw)
                || self.at(SyntaxKind::ElseKw)
                || self.at(SyntaxKind::ElsifKw)
                || self.at(SyntaxKind::WhenKw)
            {
                break;
            }

            let pos_before = self.current;
            self.parse_sequential_statement();
            // Safety: if no progress was made, force advance to avoid infinite loop
            if self.current == pos_before && !self.is_at_end() {
                self.bump();
            }
        }
    }

    fn parse_sequential_statement(&mut self) {
        self.skip_trivia();
        if self.is_at_end() {
            return;
        }

        if self.check_unsynthesizable() {
            self.error_recover("unsynthesizable statement");
            return;
        }

        // Check for optional label: ident ':'
        if self.at(SyntaxKind::Ident) && self.peek_kind(1) == Some(SyntaxKind::Colon) {
            self.bump(); // label
            self.skip_trivia();
            self.bump(); // colon
            self.skip_trivia();
        }

        match self.current_kind() {
            Some(SyntaxKind::IfKw) => self.parse_if_stmt(),
            Some(SyntaxKind::CaseKw) => self.parse_case_stmt(),
            Some(SyntaxKind::ForKw) => self.parse_for_loop_stmt(),
            Some(SyntaxKind::WhileKw) => self.parse_while_loop_stmt(),
            Some(SyntaxKind::LoopKw) => {
                // Bare loop: [label:] loop ... end loop;
                self.bump(); // loop
                self.skip_trivia();
                self.parse_sequential_statements();
                self.expect(SyntaxKind::EndKw);
                self.skip_trivia();
                self.expect(SyntaxKind::LoopKw);
                self.skip_trivia();
                if self.at(SyntaxKind::Ident) {
                    self.bump(); // optional label
                    self.skip_trivia();
                }
                self.expect(SyntaxKind::Semicolon);
            }
            Some(SyntaxKind::NullKw) => self.parse_null_stmt(),
            Some(SyntaxKind::ReturnKw) => self.parse_return_stmt(),
            Some(SyntaxKind::AssertKw) => self.parse_assert_stmt(),
            Some(SyntaxKind::ReportKw) => self.parse_report_stmt(),
            Some(SyntaxKind::NextKw) => self.parse_next_stmt(),
            Some(SyntaxKind::ExitKw) => self.parse_exit_stmt(),
            _ => {
                // Signal assignment (name <= expr) or variable assignment (name := expr)
                self.parse_signal_or_variable_assign();
            }
        }
    }

    fn parse_if_stmt(&mut self) {
        self.start_node(SyntaxKind::IfStmt);
        self.expect(SyntaxKind::IfKw);
        self.skip_trivia();
        self.parse_expression(); // condition
        self.skip_trivia();
        self.expect(SyntaxKind::ThenKw);
        self.skip_trivia();
        self.parse_sequential_statements();

        // elsif chain
        while self.eat(SyntaxKind::ElsifKw) {
            self.skip_trivia();
            self.parse_expression(); // condition
            self.skip_trivia();
            self.expect(SyntaxKind::ThenKw);
            self.skip_trivia();
            self.parse_sequential_statements();
        }

        // Optional else
        if self.eat(SyntaxKind::ElseKw) {
            self.skip_trivia();
            self.parse_sequential_statements();
        }

        self.expect(SyntaxKind::EndKw);
        self.skip_trivia();
        self.expect(SyntaxKind::IfKw);
        self.skip_trivia();
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_case_stmt(&mut self) {
        self.start_node(SyntaxKind::CaseStmt);
        self.expect(SyntaxKind::CaseKw);
        self.skip_trivia();
        self.parse_expression(); // selector
        self.skip_trivia();
        self.expect(SyntaxKind::IsKw);
        self.skip_trivia();

        // Case alternatives: when choices => statements
        while self.at(SyntaxKind::WhenKw) {
            self.start_node(SyntaxKind::CaseAlternative);
            self.bump(); // when
            self.skip_trivia();
            self.parse_choices();
            self.skip_trivia();
            self.expect(SyntaxKind::Arrow);
            self.skip_trivia();
            self.parse_sequential_statements();
            self.finish_node();
        }

        self.expect(SyntaxKind::EndKw);
        self.skip_trivia();
        self.expect(SyntaxKind::CaseKw);
        self.skip_trivia();
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_choices(&mut self) {
        self.start_node(SyntaxKind::ChoiceList);
        self.parse_choice();
        while self.eat(SyntaxKind::Bar) {
            self.skip_trivia();
            self.parse_choice();
        }
        self.finish_node();
    }

    fn parse_choice(&mut self) {
        self.start_node(SyntaxKind::Choice);
        self.skip_trivia();
        if self.at(SyntaxKind::OthersKw) {
            self.bump(); // others
        } else {
            self.parse_expression();
            self.skip_trivia();
            // range: expr to/downto expr
            if self.at(SyntaxKind::ToKw) || self.at(SyntaxKind::DowntoKw) {
                self.bump();
                self.skip_trivia();
                self.parse_expression();
            }
        }
        self.finish_node();
    }

    fn parse_for_loop_stmt(&mut self) {
        self.start_node(SyntaxKind::ForLoopStmt);
        self.expect(SyntaxKind::ForKw);
        self.skip_trivia();
        self.bump(); // iterator variable
        self.skip_trivia();
        self.expect(SyntaxKind::InKw);
        self.skip_trivia();
        self.parse_discrete_range();
        self.skip_trivia();
        self.expect(SyntaxKind::LoopKw);
        self.skip_trivia();
        self.parse_sequential_statements();
        self.expect(SyntaxKind::EndKw);
        self.skip_trivia();
        self.expect(SyntaxKind::LoopKw);
        self.skip_trivia();
        // Optional label
        if self.at(SyntaxKind::Ident) {
            self.bump();
            self.skip_trivia();
        }
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_while_loop_stmt(&mut self) {
        self.start_node(SyntaxKind::WhileLoopStmt);
        self.expect(SyntaxKind::WhileKw);
        self.skip_trivia();
        self.parse_expression();
        self.skip_trivia();
        self.expect(SyntaxKind::LoopKw);
        self.skip_trivia();
        self.parse_sequential_statements();
        self.expect(SyntaxKind::EndKw);
        self.skip_trivia();
        self.expect(SyntaxKind::LoopKw);
        self.skip_trivia();
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_null_stmt(&mut self) {
        self.start_node(SyntaxKind::NullStmt);
        self.expect(SyntaxKind::NullKw);
        self.skip_trivia();
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_return_stmt(&mut self) {
        self.start_node(SyntaxKind::ReturnStmt);
        self.expect(SyntaxKind::ReturnKw);
        self.skip_trivia();
        if !self.at(SyntaxKind::Semicolon) {
            self.parse_expression();
            self.skip_trivia();
        }
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_assert_stmt(&mut self) {
        self.start_node(SyntaxKind::AssertStmt);
        self.expect(SyntaxKind::AssertKw);
        self.skip_trivia();
        self.parse_expression();
        self.skip_trivia();
        if self.at(SyntaxKind::ReportKw) {
            self.bump();
            self.skip_trivia();
            self.parse_expression();
            self.skip_trivia();
        }
        if self.at(SyntaxKind::SeverityKw) {
            self.bump();
            self.skip_trivia();
            self.parse_expression();
            self.skip_trivia();
        }
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    /// Standalone report statement: report "msg" [severity level];
    /// Equivalent to: assert false report "msg" severity ...;
    fn parse_report_stmt(&mut self) {
        self.start_node(SyntaxKind::AssertStmt);
        self.expect(SyntaxKind::ReportKw);
        self.skip_trivia();
        self.parse_expression();
        self.skip_trivia();
        if self.at(SyntaxKind::SeverityKw) {
            self.bump();
            self.skip_trivia();
            self.parse_expression();
            self.skip_trivia();
        }
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_next_stmt(&mut self) {
        self.start_node(SyntaxKind::NextStmt);
        self.expect(SyntaxKind::NextKw);
        self.skip_trivia();
        // Optional label
        if self.at(SyntaxKind::Ident) {
            self.bump();
            self.skip_trivia();
        }
        // Optional when condition
        if self.at(SyntaxKind::WhenKw) {
            self.bump();
            self.skip_trivia();
            self.parse_expression();
            self.skip_trivia();
        }
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_exit_stmt(&mut self) {
        self.start_node(SyntaxKind::ExitStmt);
        self.expect(SyntaxKind::ExitKw);
        self.skip_trivia();
        if self.at(SyntaxKind::Ident) {
            self.bump();
            self.skip_trivia();
        }
        if self.at(SyntaxKind::WhenKw) {
            self.bump();
            self.skip_trivia();
            self.parse_expression();
            self.skip_trivia();
        }
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_signal_or_variable_assign(&mut self) {
        // Parse target name, then decide based on <= or :=
        let start = self.current;
        self.start_node(SyntaxKind::SequentialSignalAssign);
        self.parse_name();
        self.skip_trivia();

        match self.current_kind() {
            Some(SyntaxKind::SignalAssign) => {
                self.bump(); // <=
                self.skip_trivia();
                self.parse_expression();
                self.skip_trivia();
                // Check for "when" conditional assignment in sequential context
                if self.at(SyntaxKind::WhenKw) {
                    self.bump(); // when
                    self.skip_trivia();
                    self.parse_expression(); // condition
                    self.skip_trivia();
                    while self.at(SyntaxKind::ElseKw) {
                        self.bump();
                        self.skip_trivia();
                        self.parse_expression(); // value
                        self.skip_trivia();
                        if self.at(SyntaxKind::WhenKw) {
                            self.bump();
                            self.skip_trivia();
                            self.parse_expression(); // condition
                            self.skip_trivia();
                        }
                    }
                }
                self.expect(SyntaxKind::Semicolon);
            }
            Some(SyntaxKind::VarAssign) => {
                self.bump(); // :=
                self.skip_trivia();
                self.parse_expression();
                self.skip_trivia();
                self.expect(SyntaxKind::Semicolon);
            }
            _ => {
                // Could be a procedure call: name(args);
                if self.at(SyntaxKind::Semicolon) {
                    self.bump();
                } else {
                    self.finish_node();
                    self.error_recover("expected '<=' or ':=' in assignment");
                    return;
                }
            }
        }
        self.finish_node();
    }

    // ====================================================================
    // Grammar: component instantiation
    // ====================================================================

    fn parse_component_inst_entity(&mut self) {
        self.start_node(SyntaxKind::ComponentInst);
        // entity work.Name[(arch)] [generic map (...)] port map (...)
        self.expect(SyntaxKind::EntityKw);
        self.skip_trivia();
        self.parse_selected_name();
        self.skip_trivia();
        // Optional (architecture_name)
        if self.at(SyntaxKind::LParen) {
            self.bump();
            self.skip_trivia();
            self.bump(); // arch name
            self.skip_trivia();
            self.expect(SyntaxKind::RParen);
            self.skip_trivia();
        }
        self.parse_port_and_generic_maps();
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_component_inst_component(&mut self) {
        self.start_node(SyntaxKind::ComponentInst);
        // [component] name [generic map (...)] port map (...)
        // Name can be dotted: lib.pkg.component (e.g., gaisler.memctrl.ssrctrl)
        self.eat(SyntaxKind::ComponentKw);
        self.skip_trivia();
        self.parse_selected_name();
        self.skip_trivia();
        self.parse_port_and_generic_maps();
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_port_and_generic_maps(&mut self) {
        // Optional generic map
        if self.at(SyntaxKind::GenericKw) {
            self.start_node(SyntaxKind::GenericMap);
            self.bump(); // generic
            self.skip_trivia();
            self.expect(SyntaxKind::MapKw);
            self.skip_trivia();
            self.parse_association_list();
            self.finish_node();
            self.skip_trivia();
        }

        // Port map (required for instantiation)
        if self.at(SyntaxKind::PortKw) {
            self.start_node(SyntaxKind::PortMap);
            self.bump(); // port
            self.skip_trivia();
            self.expect(SyntaxKind::MapKw);
            self.skip_trivia();
            self.parse_association_list();
            self.finish_node();
            self.skip_trivia();
        }
    }

    fn parse_association_list(&mut self) {
        self.start_node(SyntaxKind::AssociationList);
        self.expect(SyntaxKind::LParen);
        self.skip_trivia();

        if !self.at(SyntaxKind::RParen) {
            self.parse_association_element();
            while self.eat(SyntaxKind::Comma) {
                self.skip_trivia();
                self.parse_association_element();
            }
        }

        self.skip_trivia();
        self.expect(SyntaxKind::RParen);
        self.finish_node();
    }

    fn parse_association_element(&mut self) {
        self.start_node(SyntaxKind::AssociationElement);
        self.skip_trivia();

        if self.at(SyntaxKind::OpenKw) {
            self.bump(); // open
        } else {
            // Detect named vs positional association by scanning ahead
            // for '=>' after the formal name (which may include parenthesized
            // index/slice, e.g., data(7 downto 0) => actual).
            let is_named = self.lookahead_is_named_association();

            if is_named {
                // Parse the formal: name with optional index/slice
                self.parse_name();
                self.skip_trivia();
                self.expect(SyntaxKind::Arrow);
                self.skip_trivia();
            }

            if self.at(SyntaxKind::OpenKw) {
                self.bump();
            } else {
                self.parse_expression();
            }
        }
        self.finish_node();
    }

    /// Lookahead (without consuming tokens) to detect named association.
    /// Scans: name [.name]* [(...)]['..]* =>
    fn lookahead_is_named_association(&self) -> bool {
        let mut pos = self.current;
        let len = self.tokens.len();

        // Must start with identifier or type keyword
        let kind = self.tokens.get(pos).map(|t| token_to_syntax_kind(&t.token));
        if !matches!(
            kind,
            Some(SyntaxKind::Ident)
                | Some(SyntaxKind::StdLogicKw)
                | Some(SyntaxKind::StdLogicVectorKw)
                | Some(SyntaxKind::StdUlogicVectorKw)
                | Some(SyntaxKind::UnsignedKw)
                | Some(SyntaxKind::SignedKw)
                | Some(SyntaxKind::StdUlogicKw)
                | Some(SyntaxKind::BooleanKw)
                | Some(SyntaxKind::IntegerKw)
                | Some(SyntaxKind::NaturalKw)
                | Some(SyntaxKind::PositiveKw)
        ) {
            return false;
        }
        pos += 1;

        // Skip name suffixes: dots, parens, ticks
        loop {
            // Skip trivia
            while pos < len {
                let k = token_to_syntax_kind(&self.tokens[pos].token);
                if k == SyntaxKind::Whitespace || k == SyntaxKind::Comment {
                    pos += 1;
                } else {
                    break;
                }
            }
            if pos >= len {
                return false;
            }

            let k = token_to_syntax_kind(&self.tokens[pos].token);
            match k {
                SyntaxKind::Dot => {
                    pos += 1; // skip dot
                              // Skip trivia
                    while pos < len {
                        let k2 = token_to_syntax_kind(&self.tokens[pos].token);
                        if k2 == SyntaxKind::Whitespace || k2 == SyntaxKind::Comment {
                            pos += 1;
                        } else {
                            break;
                        }
                    }
                    if pos < len {
                        pos += 1; // skip field name
                    }
                }
                SyntaxKind::LParen => {
                    // Skip to matching RParen
                    pos += 1;
                    let mut depth = 1;
                    while depth > 0 && pos < len {
                        let k2 = token_to_syntax_kind(&self.tokens[pos].token);
                        match k2 {
                            SyntaxKind::LParen => depth += 1,
                            SyntaxKind::RParen => depth -= 1,
                            _ => {}
                        }
                        pos += 1;
                    }
                }
                SyntaxKind::Tick => {
                    pos += 1; // skip tick
                              // Skip trivia + attribute name
                    while pos < len {
                        let k2 = token_to_syntax_kind(&self.tokens[pos].token);
                        if k2 == SyntaxKind::Whitespace || k2 == SyntaxKind::Comment {
                            pos += 1;
                        } else {
                            break;
                        }
                    }
                    if pos < len {
                        pos += 1; // skip attribute name
                    }
                }
                SyntaxKind::Arrow => return true,
                _ => return false,
            }
        }
    }

    // ====================================================================
    // Grammar: generate statements
    // ====================================================================

    fn parse_for_generate(&mut self) {
        self.start_node(SyntaxKind::ForGenerate);
        self.expect(SyntaxKind::ForKw);
        self.skip_trivia();
        self.bump(); // iterator variable
        self.skip_trivia();
        self.expect(SyntaxKind::InKw);
        self.skip_trivia();
        self.parse_discrete_range();
        self.skip_trivia();
        self.expect(SyntaxKind::GenerateKw);
        self.skip_trivia();

        // Optional declarative region + begin
        self.parse_generate_body();

        self.expect(SyntaxKind::EndKw);
        self.skip_trivia();
        self.expect(SyntaxKind::GenerateKw);
        self.skip_trivia();
        // Optional label
        if self.at(SyntaxKind::Ident) {
            self.bump();
            self.skip_trivia();
        }
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    /// Parse generate body: optional declarations, optional begin, concurrent statements.
    /// Used by both for-generate and if-generate.
    fn parse_generate_body(&mut self) {
        // VHDL allows optional declarations + begin in generate bodies:
        //   for ... generate
        //     [declarations]
        //   begin
        //     [concurrent_statements]
        //   end generate;
        // Or simply:
        //   for ... generate
        //     [concurrent_statements]
        //   end generate;
        if self.at(SyntaxKind::BeginKw) {
            self.bump(); // begin
            self.skip_trivia();
        } else {
            // Check for declarative items before begin
            let has_decls = matches!(
                self.current_kind(),
                Some(SyntaxKind::SignalKw)
                    | Some(SyntaxKind::ConstantKw)
                    | Some(SyntaxKind::VariableKw)
                    | Some(SyntaxKind::TypeKw)
                    | Some(SyntaxKind::SubtypeKw)
                    | Some(SyntaxKind::ComponentKw)
                    | Some(SyntaxKind::AttributeKw)
                    | Some(SyntaxKind::AliasKw)
                    | Some(SyntaxKind::UseKw)
                    | Some(SyntaxKind::FunctionKw)
                    | Some(SyntaxKind::PureKw)
                    | Some(SyntaxKind::ImpureKw)
                    | Some(SyntaxKind::ProcedureKw)
            );
            if has_decls {
                self.parse_architecture_declarations();
                if self.at(SyntaxKind::BeginKw) {
                    self.bump();
                    self.skip_trivia();
                }
            }
        }
        self.parse_concurrent_statements();
    }

    fn parse_if_generate(&mut self) {
        self.start_node(SyntaxKind::IfGenerate);
        self.expect(SyntaxKind::IfKw);
        self.skip_trivia();
        self.parse_expression(); // condition
        self.skip_trivia();
        self.expect(SyntaxKind::GenerateKw);
        self.skip_trivia();

        self.parse_generate_body();

        // VHDL-2008: elsif generate / else generate
        while self.at(SyntaxKind::ElsifKw) {
            self.bump();
            self.skip_trivia();
            self.parse_expression();
            self.skip_trivia();
            self.expect(SyntaxKind::GenerateKw);
            self.skip_trivia();
            self.parse_generate_body();
        }
        if self.at(SyntaxKind::ElseKw) {
            self.bump();
            self.skip_trivia();
            self.expect(SyntaxKind::GenerateKw);
            self.skip_trivia();
            self.parse_generate_body();
        }

        self.expect(SyntaxKind::EndKw);
        self.skip_trivia();
        self.expect(SyntaxKind::GenerateKw);
        self.skip_trivia();
        if self.at(SyntaxKind::Ident) {
            self.bump();
            self.skip_trivia();
        }
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_block_stmt(&mut self) {
        self.start_node(SyntaxKind::BlockStmt);
        self.expect(SyntaxKind::BlockKw);
        self.skip_trivia();

        // Optional guard expression in parens — skip past it
        if self.at(SyntaxKind::LParen) {
            let mut depth = 0;
            loop {
                if self.is_at_end() {
                    break;
                }
                if self.at(SyntaxKind::LParen) {
                    depth += 1;
                }
                if self.at(SyntaxKind::RParen) {
                    depth -= 1;
                    self.bump();
                    if depth == 0 {
                        break;
                    }
                    continue;
                }
                self.bump();
            }
            self.skip_trivia();
        }

        // Optional 'is'
        self.eat(SyntaxKind::IsKw);
        self.skip_trivia();

        // Declarations (same as architecture declarative region)
        self.parse_architecture_declarations();

        self.expect(SyntaxKind::BeginKw);
        self.skip_trivia();

        // Concurrent statements
        self.parse_concurrent_statements();

        self.expect(SyntaxKind::EndKw);
        self.skip_trivia();
        self.eat(SyntaxKind::BlockKw);
        self.skip_trivia();
        // Optional label
        if self.at(SyntaxKind::Ident) {
            self.bump();
            self.skip_trivia();
        }
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    // ====================================================================
    // Grammar: package
    // ====================================================================

    fn parse_package_or_body(&mut self) {
        // Peek after 'package' to see if next is 'body'
        if self.peek_kind(1) == Some(SyntaxKind::BodyKw) {
            self.parse_package_body();
        } else {
            self.parse_package_decl();
        }
    }

    fn parse_package_decl(&mut self) {
        // Peek ahead to detect package instantiation: package X is new Y ...
        // We need to check: package IDENT is new
        let saved = self.current;
        let mut lookahead = self.current;
        // Skip: package
        lookahead += 1;
        while lookahead < self.tokens.len() {
            let k = token_to_syntax_kind(&self.tokens[lookahead].token);
            if k == SyntaxKind::Whitespace || k == SyntaxKind::Comment {
                lookahead += 1;
            } else {
                break;
            }
        }
        // Skip: IDENT
        lookahead += 1;
        while lookahead < self.tokens.len() {
            let k = token_to_syntax_kind(&self.tokens[lookahead].token);
            if k == SyntaxKind::Whitespace || k == SyntaxKind::Comment {
                lookahead += 1;
            } else {
                break;
            }
        }
        // Skip: is
        lookahead += 1;
        while lookahead < self.tokens.len() {
            let k = token_to_syntax_kind(&self.tokens[lookahead].token);
            if k == SyntaxKind::Whitespace || k == SyntaxKind::Comment {
                lookahead += 1;
            } else {
                break;
            }
        }
        // Check: new
        let is_instantiation = lookahead < self.tokens.len()
            && token_to_syntax_kind(&self.tokens[lookahead].token) == SyntaxKind::NewKw;

        if is_instantiation {
            self.parse_package_instantiation();
            return;
        }

        self.start_node(SyntaxKind::PackageDecl);
        self.expect(SyntaxKind::PackageKw);
        self.skip_trivia();
        self.bump(); // package name
        self.skip_trivia();
        self.expect(SyntaxKind::IsKw);
        self.skip_trivia();

        // Optional generic clause
        if self.at(SyntaxKind::GenericKw) {
            self.parse_generic_clause();
            self.skip_trivia();
        }

        // Declarations until end
        self.parse_package_declarations();

        self.expect(SyntaxKind::EndKw);
        self.skip_trivia();
        self.eat(SyntaxKind::PackageKw);
        self.skip_trivia();
        if !self.at(SyntaxKind::Semicolon) {
            self.bump();
            self.skip_trivia();
        }
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_package_instantiation(&mut self) {
        // package X is new Y generic map (...);
        self.start_node(SyntaxKind::PackageInstantiation);
        self.expect(SyntaxKind::PackageKw);
        self.skip_trivia();
        self.bump(); // instance name (X)
        self.skip_trivia();
        self.expect(SyntaxKind::IsKw);
        self.skip_trivia();
        self.expect(SyntaxKind::NewKw);
        self.skip_trivia();
        // Source package name — may be selected name (work.pkg)
        self.parse_selected_name();
        self.skip_trivia();
        // Optional generic map
        if self.at(SyntaxKind::GenericKw) {
            self.start_node(SyntaxKind::GenericMap);
            self.bump(); // generic
            self.skip_trivia();
            self.expect(SyntaxKind::MapKw);
            self.skip_trivia();
            self.parse_association_list();
            self.finish_node();
            self.skip_trivia();
        }
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_package_body(&mut self) {
        self.start_node(SyntaxKind::PackageBody);
        self.expect(SyntaxKind::PackageKw);
        self.skip_trivia();
        self.expect(SyntaxKind::BodyKw);
        self.skip_trivia();
        self.bump(); // package name
        self.skip_trivia();
        self.expect(SyntaxKind::IsKw);
        self.skip_trivia();

        self.parse_package_declarations();

        self.expect(SyntaxKind::EndKw);
        self.skip_trivia();
        self.eat(SyntaxKind::PackageKw);
        self.skip_trivia();
        self.eat(SyntaxKind::BodyKw);
        self.skip_trivia();
        if !self.at(SyntaxKind::Semicolon) {
            self.bump();
            self.skip_trivia();
        }
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_package_declarations(&mut self) {
        loop {
            self.skip_trivia();
            if self.is_at_end() || self.at(SyntaxKind::EndKw) {
                break;
            }
            let pos_before = self.current;
            match self.current_kind() {
                Some(SyntaxKind::TypeKw) => self.parse_type_decl(),
                Some(SyntaxKind::SubtypeKw) => self.parse_subtype_decl(),
                Some(SyntaxKind::ConstantKw) => self.parse_constant_decl(),
                Some(SyntaxKind::SignalKw) => self.parse_signal_decl(),
                Some(SyntaxKind::ComponentKw) => self.parse_component_decl(),
                Some(SyntaxKind::FunctionKw)
                | Some(SyntaxKind::PureKw)
                | Some(SyntaxKind::ImpureKw) => {
                    self.parse_function_decl_or_body();
                }
                Some(SyntaxKind::ProcedureKw) => {
                    self.parse_procedure_decl_or_body();
                }
                Some(SyntaxKind::AliasKw) => self.parse_alias_decl(),
                Some(SyntaxKind::AttributeKw) => self.parse_attribute_decl_or_spec(),
                Some(SyntaxKind::UseKw) => self.parse_use_clause(),
                Some(SyntaxKind::GenericKw) => {
                    // generic clause inside package (already parsed at package level, skip if duplicate)
                    self.parse_generic_clause();
                }
                _ => {
                    self.error_recover("unexpected declaration in package");
                }
            }
            // Safety: if no progress was made, force advance to avoid infinite loop
            if self.current == pos_before && !self.is_at_end() {
                self.bump();
            }
        }
    }

    // ====================================================================
    // Grammar: VHDL-2019 interfaces and views
    // ====================================================================

    fn parse_interface_decl(&mut self) {
        self.start_node(SyntaxKind::InterfaceDecl);
        self.expect(SyntaxKind::InterfaceKw);
        self.skip_trivia();
        self.bump(); // interface name
        self.skip_trivia();
        self.expect(SyntaxKind::IsKw);
        self.skip_trivia();

        // Signal declarations until 'end'
        while !self.is_at_end() && !self.at(SyntaxKind::EndKw) {
            self.parse_signal_decl();
            self.skip_trivia();
        }

        self.expect(SyntaxKind::EndKw);
        self.skip_trivia();
        self.eat(SyntaxKind::InterfaceKw);
        self.skip_trivia();
        if !self.at(SyntaxKind::Semicolon) {
            self.bump();
            self.skip_trivia();
        }
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    fn parse_view_decl(&mut self) {
        self.start_node(SyntaxKind::ViewDecl);
        self.expect(SyntaxKind::ViewKw);
        self.skip_trivia();
        self.bump(); // view name
        self.skip_trivia();
        self.expect(SyntaxKind::OfKw);
        self.skip_trivia();
        self.bump(); // interface name
        self.skip_trivia();
        self.expect(SyntaxKind::IsKw);
        self.skip_trivia();

        // Field direction assignments until 'end'
        while !self.is_at_end() && !self.at(SyntaxKind::EndKw) {
            self.start_node(SyntaxKind::ViewFieldDirection);
            self.skip_trivia();
            self.bump(); // field name
            self.skip_trivia();
            self.expect(SyntaxKind::Colon);
            self.skip_trivia();
            // Direction: in, out, inout
            self.bump();
            self.skip_trivia();
            self.expect(SyntaxKind::Semicolon);
            self.finish_node();
            self.skip_trivia();
        }

        self.expect(SyntaxKind::EndKw);
        self.skip_trivia();
        self.eat(SyntaxKind::ViewKw);
        self.skip_trivia();
        if !self.at(SyntaxKind::Semicolon) {
            self.bump();
            self.skip_trivia();
        }
        self.expect(SyntaxKind::Semicolon);
        self.finish_node();
    }

    // ====================================================================
    // Grammar: expressions (Pratt parser)
    // ====================================================================

    fn parse_expression(&mut self) {
        self.parse_logical_expr();
        self.skip_trivia();
        // Conditional expression: value when condition else value (lowest precedence)
        // Inhibited in contexts where `when` has different meaning (e.g. selected assignments)
        if !self.inhibit_when_else && self.at(SyntaxKind::WhenKw) {
            self.bump(); // when
            self.skip_trivia();
            self.parse_logical_expr(); // condition
            self.skip_trivia();
            if self.at(SyntaxKind::ElseKw) {
                self.bump(); // else
                self.skip_trivia();
                self.parse_expression(); // else value (recursive for chaining)
            }
        }
    }

    // Precedence (lowest to highest):
    // logical: and, or, xor, nand, nor, xnor
    // relational: =, /=, <, <=, >, >=
    // shift: sll, srl, sla, sra, rol, ror
    // additive: +, -, &
    // multiplicative: *, /, mod, rem
    // unary: +, -, not, abs
    // primary: literal, name, parenthesized, aggregate, function call, type conversion

    fn parse_logical_expr(&mut self) {
        self.parse_relational_expr();
        self.skip_trivia();
        while matches!(
            self.current_kind(),
            Some(SyntaxKind::AndKw)
                | Some(SyntaxKind::OrKw)
                | Some(SyntaxKind::XorKw)
                | Some(SyntaxKind::NandKw)
                | Some(SyntaxKind::NorKw)
                | Some(SyntaxKind::XnorKw)
        ) {
            self.bump(); // operator
            self.skip_trivia();
            self.parse_relational_expr();
            self.skip_trivia();
        }
    }

    fn parse_relational_expr(&mut self) {
        self.parse_shift_expr();
        self.skip_trivia();

        // Relational: =, /=, <, <=, >, >=
        // Note: <= is SignalAssign token; in expression context, treat as LessEqual
        match self.current_kind() {
            Some(SyntaxKind::Equal)
            | Some(SyntaxKind::NotEqual)
            | Some(SyntaxKind::LessThan)
            | Some(SyntaxKind::GreaterThan)
            | Some(SyntaxKind::GreaterEqual) => {
                self.bump();
                self.skip_trivia();
                self.parse_shift_expr();
            }
            Some(SyntaxKind::SignalAssign) => {
                // <= in expression context is LessEqual
                self.bump();
                self.skip_trivia();
                self.parse_shift_expr();
            }
            _ => {}
        }
    }

    fn parse_shift_expr(&mut self) {
        self.parse_additive_expr();
        self.skip_trivia();
        if matches!(
            self.current_kind(),
            Some(SyntaxKind::SllKw)
                | Some(SyntaxKind::SrlKw)
                | Some(SyntaxKind::SlaKw)
                | Some(SyntaxKind::SraKw)
                | Some(SyntaxKind::RolKw)
                | Some(SyntaxKind::RorKw)
        ) {
            self.bump();
            self.skip_trivia();
            self.parse_additive_expr();
        }
    }

    fn parse_additive_expr(&mut self) {
        self.parse_multiplicative_expr();
        self.skip_trivia();
        while matches!(
            self.current_kind(),
            Some(SyntaxKind::Plus) | Some(SyntaxKind::Minus) | Some(SyntaxKind::Ampersand)
        ) {
            self.bump();
            self.skip_trivia();
            self.parse_multiplicative_expr();
            self.skip_trivia();
        }
    }

    fn parse_multiplicative_expr(&mut self) {
        self.parse_unary_expr();
        self.skip_trivia();
        while matches!(
            self.current_kind(),
            Some(SyntaxKind::Star)
                | Some(SyntaxKind::Slash)
                | Some(SyntaxKind::ModKw)
                | Some(SyntaxKind::RemKw)
        ) {
            self.bump();
            self.skip_trivia();
            self.parse_unary_expr();
            self.skip_trivia();
        }
    }

    fn parse_unary_expr(&mut self) {
        self.skip_trivia();
        match self.current_kind() {
            Some(SyntaxKind::NotKw)
            | Some(SyntaxKind::AbsKw)
            | Some(SyntaxKind::Plus)
            | Some(SyntaxKind::Minus) => {
                self.bump();
                self.skip_trivia();
                self.parse_unary_expr();
            }
            _ => {
                self.parse_primary_expr();
                self.skip_trivia();
                // Power operator ** (highest binary precedence)
                if self.at(SyntaxKind::DoubleStar) {
                    self.bump();
                    self.skip_trivia();
                    self.parse_unary_expr();
                }
            }
        }
    }

    fn parse_primary_expr(&mut self) {
        self.skip_trivia();
        match self.current_kind() {
            Some(SyntaxKind::IntLiteral)
            | Some(SyntaxKind::RealLiteral)
            | Some(SyntaxKind::BasedLiteral) => {
                self.bump();
                // VHDL physical type literals: numeric value followed by unit name
                // e.g. "10 ns", "1.5 ps", "100 ms"
                self.skip_trivia();
                if self.at(SyntaxKind::Ident) {
                    self.bump(); // unit suffix
                }
            }
            Some(SyntaxKind::StringLiteral) | Some(SyntaxKind::BitStringLiteral) => {
                self.bump();
            }
            Some(SyntaxKind::CharLiteral) => {
                self.bump();
            }
            Some(SyntaxKind::TrueKw) | Some(SyntaxKind::FalseKw) => {
                self.bump();
            }
            Some(SyntaxKind::NullKw) => {
                self.bump();
            }
            Some(SyntaxKind::NewKw) => {
                // Allocator: new subtype_indication or new qualified_expression
                self.bump(); // new
                self.skip_trivia();
                // Parse the type name and optional constraint/qualified expression
                self.parse_name_or_call();
            }
            Some(SyntaxKind::LParen) => {
                // Parenthesized expression or aggregate
                self.parse_parenthesized_or_aggregate();
            }
            Some(SyntaxKind::DoubleLess) => {
                // External name: << signal .path.to.sig : type >>
                self.parse_external_name();
            }
            // Named primary: identifier, type keyword as name, or function call
            _ if self.is_name_or_builtin_start() => {
                self.parse_name_or_call();
            }
            _ => {
                self.error("expected expression");
            }
        }
    }

    fn is_name_or_builtin_start(&self) -> bool {
        matches!(
            self.current_kind(),
            Some(SyntaxKind::Ident)
                | Some(SyntaxKind::StdLogicKw)
                | Some(SyntaxKind::StdUlogicKw)
                | Some(SyntaxKind::StdLogicVectorKw)
                | Some(SyntaxKind::StdUlogicVectorKw)
                | Some(SyntaxKind::UnsignedKw)
                | Some(SyntaxKind::SignedKw)
                | Some(SyntaxKind::BooleanKw)
                | Some(SyntaxKind::IntegerKw)
                | Some(SyntaxKind::NaturalKw)
                | Some(SyntaxKind::PositiveKw)
                | Some(SyntaxKind::RealKw)
                | Some(SyntaxKind::StringKw)
                | Some(SyntaxKind::RisingEdgeKw)
                | Some(SyntaxKind::FallingEdgeKw)
                | Some(SyntaxKind::ToUnsignedKw)
                | Some(SyntaxKind::ToSignedKw)
                | Some(SyntaxKind::ToIntegerKw)
                | Some(SyntaxKind::ResizeKw)
                | Some(SyntaxKind::ConvIntegerKw)
                | Some(SyntaxKind::ConvStdLogicVectorKw)
                | Some(SyntaxKind::BitKw)
                | Some(SyntaxKind::BitVectorKw)
        )
    }

    /// Parse external name: `<< signal|variable|constant .path.to.sig : subtype >>`
    fn parse_external_name(&mut self) {
        self.start_node(SyntaxKind::ExternalNameExpr);
        self.expect(SyntaxKind::DoubleLess);
        self.skip_trivia();
        // Object class keyword (signal, variable, constant)
        if self.at(SyntaxKind::SignalKw)
            || self.at(SyntaxKind::VariableKw)
            || self.at(SyntaxKind::ConstantKw)
        {
            self.bump();
            self.skip_trivia();
        }
        // Pathname: .path.to.signal (dots and identifiers until colon)
        while !self.is_at_end()
            && !self.at(SyntaxKind::Colon)
            && !self.at(SyntaxKind::DoubleGreater)
        {
            self.bump();
            self.skip_trivia();
        }
        // : subtype_indication
        if self.at(SyntaxKind::Colon) {
            self.bump();
            self.skip_trivia();
            self.parse_subtype_indication();
            self.skip_trivia();
        }
        self.expect(SyntaxKind::DoubleGreater);
        self.finish_node();
    }

    /// Parse a single argument in a function/procedure call or array index.
    /// Handles: expression, discrete range (expr to/downto expr),
    /// and named association (name => expr).
    fn parse_call_or_index_arg(&mut self) {
        self.skip_trivia();
        // Check for named association using lookahead
        if self.lookahead_is_named_association() {
            self.parse_name(); // formal name (may include index/slice)
            self.skip_trivia();
            self.expect(SyntaxKind::Arrow); // =>
            self.skip_trivia();
            if self.at(SyntaxKind::OpenKw) {
                self.bump();
            } else {
                self.parse_expression();
            }
        } else {
            self.parse_expression();
            self.skip_trivia();
            // Check for discrete range
            if self.at(SyntaxKind::ToKw) || self.at(SyntaxKind::DowntoKw) {
                self.bump();
                self.skip_trivia();
                self.parse_expression();
            }
        }
        self.skip_trivia();
    }

    fn parse_name_or_call(&mut self) {
        self.parse_name();
    }

    fn parse_name(&mut self) {
        self.start_node(SyntaxKind::Name);
        self.bump(); // first identifier or type keyword
        self.skip_trivia();

        // Handle suffixes: ( ), ., ', (discrete_range)
        loop {
            self.skip_trivia();
            match self.current_kind() {
                Some(SyntaxKind::LParen) => {
                    // Function call, type conversion, index, or slice
                    // Also handles named association: func(param => value, ...)
                    self.expect(SyntaxKind::LParen);
                    self.skip_trivia();
                    if !self.at(SyntaxKind::RParen) {
                        self.parse_call_or_index_arg();
                        while self.eat(SyntaxKind::Comma) {
                            self.skip_trivia();
                            self.parse_call_or_index_arg();
                        }
                    }
                    self.skip_trivia();
                    self.expect(SyntaxKind::RParen);
                }
                Some(SyntaxKind::Dot) => {
                    self.bump(); // .
                    self.skip_trivia();
                    if !self.is_at_end() {
                        self.bump(); // field name
                    }
                }
                Some(SyntaxKind::Tick) => {
                    // Attribute access: name'attribute
                    // But be careful with qualified expressions: type'(expr)
                    self.bump(); // '
                    self.skip_trivia();
                    if self.at(SyntaxKind::LParen) {
                        // Qualified expression: type'(aggregate_or_expr)
                        // The content can be a plain expression OR an aggregate
                        // with named associations: type'(field => val, ...)
                        self.bump(); // (
                        self.skip_trivia();
                        if !self.at(SyntaxKind::RParen) {
                            self.parse_aggregate_element();
                            self.skip_trivia();
                            while self.eat(SyntaxKind::Comma) {
                                self.skip_trivia();
                                self.parse_aggregate_element();
                                self.skip_trivia();
                            }
                        }
                        self.skip_trivia();
                        self.expect(SyntaxKind::RParen);
                    } else if !self.is_at_end() {
                        // Attribute name
                        self.bump();
                        // Attribute might have an argument: 'image(expr)
                        self.skip_trivia();
                        if self.at(SyntaxKind::LParen) {
                            self.bump();
                            self.skip_trivia();
                            if !self.at(SyntaxKind::RParen) {
                                self.parse_expression();
                            }
                            self.skip_trivia();
                            self.expect(SyntaxKind::RParen);
                        }
                    }
                }
                _ => break,
            }
        }
        self.finish_node();
    }

    fn parse_parenthesized_or_aggregate(&mut self) {
        // ( expr [, expr]* ) or ( [choices =>] expr [, ...] )
        self.start_node(SyntaxKind::AggregateExpr);
        self.expect(SyntaxKind::LParen);
        self.skip_trivia();

        if self.at(SyntaxKind::RParen) {
            self.bump();
            self.finish_node();
            return;
        }

        // Check if first element uses =>
        self.parse_aggregate_element();

        while self.eat(SyntaxKind::Comma) {
            self.skip_trivia();
            self.parse_aggregate_element();
        }

        self.skip_trivia();
        self.expect(SyntaxKind::RParen);
        self.finish_node();
    }

    fn parse_aggregate_element(&mut self) {
        self.start_node(SyntaxKind::AggregateElement);
        self.skip_trivia();

        // Check for 'others =>' or 'expr =>' (named association)
        if self.at(SyntaxKind::OthersKw) {
            self.bump(); // others
            self.skip_trivia();
            self.expect(SyntaxKind::Arrow);
            self.skip_trivia();
            self.parse_expression();
        } else {
            // Parse expression, then check for range and/or =>
            self.parse_expression();
            self.skip_trivia();

            // Check for range choice: expr to/downto expr => value
            if self.at(SyntaxKind::ToKw) || self.at(SyntaxKind::DowntoKw) {
                self.bump(); // to/downto
                self.skip_trivia();
                self.parse_expression();
                self.skip_trivia();
            }

            if self.at(SyntaxKind::Arrow) {
                // Named association: already parsed the choice, now parse value
                self.bump(); // =>
                self.skip_trivia();
                self.parse_expression();
            }
            // else: positional association, expression already parsed
        }
        self.finish_node();
    }

    // ====================================================================
    // Grammar: subtype indication
    // ====================================================================

    fn parse_subtype_indication(&mut self) {
        self.start_node(SyntaxKind::SubtypeIndication);
        // Type mark (name or built-in type keyword)
        // Supports dotted names: lib.pkg.type_name
        if self.is_name_or_builtin_start() {
            self.bump(); // type name (first component)
                         // Consume dotted suffixes: .pkg.type_name
            while self.at(SyntaxKind::Dot) {
                self.bump(); // .
                if self.is_name_or_builtin_start() || self.at(SyntaxKind::AllKw) {
                    self.bump(); // next name component
                } else {
                    break;
                }
            }
        } else {
            self.error("expected type name");
            self.finish_node();
            return;
        }
        self.skip_trivia();

        // VHDL resolution function: "subtype T is resolve_fn base_type;"
        // If the next token is another type name (Ident or builtin), the first
        // name we parsed was actually a resolution function, and this is the
        // real base type mark.
        if self.is_name_or_builtin_start() {
            self.bump(); // actual base type name
                         // Consume dotted suffixes on the base type
            while self.at(SyntaxKind::Dot) {
                self.bump(); // .
                if self.is_name_or_builtin_start() || self.at(SyntaxKind::AllKw) {
                    self.bump();
                } else {
                    break;
                }
            }
            self.skip_trivia();
        }

        // Optional constraint(s): (range) or (N-1 downto 0)
        // VHDL allows chained constraints for arrays of arrays:
        //   StlvArray_t(0 to N-1)(7 downto 0)
        // Also handles discrete range constraints with range keyword:
        //   ram_t(integer range 0 to size-1)
        while self.at(SyntaxKind::LParen) {
            self.bump(); // (
            self.skip_trivia();
            self.parse_expression();
            self.skip_trivia();
            if self.at(SyntaxKind::RangeKw) {
                // type range expr to/downto expr  OR  type range <>
                self.bump(); // range
                self.skip_trivia();
                if self.at(SyntaxKind::BoxOp) {
                    self.bump(); // <>
                } else {
                    self.parse_expression();
                    self.skip_trivia();
                    if self.at(SyntaxKind::ToKw) || self.at(SyntaxKind::DowntoKw) {
                        self.bump();
                        self.skip_trivia();
                        self.parse_expression();
                    }
                }
            } else if self.at(SyntaxKind::ToKw) || self.at(SyntaxKind::DowntoKw) {
                self.bump();
                self.skip_trivia();
                self.parse_expression();
            }
            self.skip_trivia();
            self.expect(SyntaxKind::RParen);
            self.skip_trivia();
        }
        if self.at(SyntaxKind::RangeKw) {
            self.bump(); // range
            self.skip_trivia();
            self.parse_expression();
            self.skip_trivia();
            if self.at(SyntaxKind::ToKw) || self.at(SyntaxKind::DowntoKw) {
                self.bump();
                self.skip_trivia();
                self.parse_expression();
            }
        }
        self.finish_node();
    }
}

// ========================================================================
// Public API
// ========================================================================

pub fn parse_vhdl(source: &str) -> ParseResult {
    let parser = ParseState::new(source);
    parser.parse_source_file()
}
