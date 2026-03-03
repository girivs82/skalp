//! VHDL source formatter using Wadler-Lindig pretty-printing
//!
//! Architecture: source -> rowan CST -> Doc IR -> layout -> formatted string
//!
//! The formatter walks the lossless concrete syntax tree produced by the parser,
//! builds an intermediate pretty-printing document (Doc), and then uses the
//! Wadler-Lindig algorithm to lay out the document within a configurable line width.

use crate::parse::parse_vhdl;
use crate::syntax::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};
use anyhow::Result;

// ============================================================================
// Configuration
// ============================================================================

/// Formatting options
pub struct FormatOptions {
    /// Maximum line width (default: 80)
    pub line_width: usize,
    /// Indentation width in spaces (default: 2)
    pub indent_width: i32,
    /// Lowercase all keywords (default: true)
    pub lowercase_keywords: bool,
}

impl Default for FormatOptions {
    fn default() -> Self {
        Self {
            line_width: 80,
            indent_width: 2,
            lowercase_keywords: true,
        }
    }
}

// ============================================================================
// Pretty-printer document IR
// ============================================================================

#[derive(Clone, Debug)]
enum Doc {
    /// Empty document
    Nil,
    /// Literal text
    Text(String),
    /// Hard line break (always breaks)
    HardLine,
    /// Soft line: becomes a space in flat mode, newline in broken mode
    Line,
    /// Soft line: becomes nothing in flat mode, newline in broken mode
    LineOrEmpty,
    /// Sequence of documents
    Concat(Vec<Doc>),
    /// Indent inner document by given amount
    Nest(i32, Box<Doc>),
    /// Group: try to fit on one line (flat mode); if not, use broken mode
    Group(Box<Doc>),
}

// Constructor helpers

fn nil() -> Doc {
    Doc::Nil
}

fn text(s: impl Into<String>) -> Doc {
    let s = s.into();
    if s.is_empty() {
        Doc::Nil
    } else {
        Doc::Text(s)
    }
}

fn hardline() -> Doc {
    Doc::HardLine
}

fn line() -> Doc {
    Doc::Line
}

fn line_or_empty() -> Doc {
    Doc::LineOrEmpty
}

fn nest(indent: i32, doc: Doc) -> Doc {
    if matches!(doc, Doc::Nil) {
        return Doc::Nil;
    }
    Doc::Nest(indent, Box::new(doc))
}

fn group(doc: Doc) -> Doc {
    Doc::Group(Box::new(doc))
}

fn concat(docs: Vec<Doc>) -> Doc {
    let mut flat = Vec::new();
    for d in docs {
        match d {
            Doc::Nil => {}
            Doc::Concat(inner) => flat.extend(inner),
            other => flat.push(other),
        }
    }
    match flat.len() {
        0 => Doc::Nil,
        1 => flat.into_iter().next().unwrap(),
        _ => Doc::Concat(flat),
    }
}

/// Join docs with a separator between each pair
fn intersperse(sep: Doc, docs: Vec<Doc>) -> Doc {
    let mut result = Vec::new();
    for (i, d) in docs.into_iter().enumerate() {
        if i > 0 {
            result.push(sep.clone());
        }
        result.push(d);
    }
    concat(result)
}

fn space() -> Doc {
    text(" ")
}

// ============================================================================
// Layout algorithm (Wadler-Lindig)
// ============================================================================

#[derive(Clone, Copy, PartialEq)]
enum Mode {
    Flat,
    Break,
}

/// Lay out a Doc within the given line width
fn layout(width: usize, doc: &Doc) -> String {
    let mut output = String::new();
    let mut column: usize = 0;
    // Work stack: (indent, mode, doc)
    let mut stack: Vec<(i32, Mode, &Doc)> = vec![(0, Mode::Break, doc)];

    while let Some((indent, mode, doc)) = stack.pop() {
        match doc {
            Doc::Nil => {}
            Doc::Text(s) => {
                output.push_str(s);
                column += s.len();
            }
            Doc::HardLine => {
                output.push('\n');
                let spaces = indent.max(0) as usize;
                for _ in 0..spaces {
                    output.push(' ');
                }
                column = spaces;
            }
            Doc::Line => {
                if mode == Mode::Flat {
                    output.push(' ');
                    column += 1;
                } else {
                    output.push('\n');
                    let spaces = indent.max(0) as usize;
                    for _ in 0..spaces {
                        output.push(' ');
                    }
                    column = spaces;
                }
            }
            Doc::LineOrEmpty => {
                if mode == Mode::Flat {
                    // nothing
                } else {
                    output.push('\n');
                    let spaces = indent.max(0) as usize;
                    for _ in 0..spaces {
                        output.push(' ');
                    }
                    column = spaces;
                }
            }
            Doc::Concat(docs) => {
                // Push in reverse order so first doc is processed first
                for d in docs.iter().rev() {
                    stack.push((indent, mode, d));
                }
            }
            Doc::Nest(n, inner) => {
                stack.push((indent + n, mode, inner));
            }
            Doc::Group(inner) => {
                if fits(width as i32 - column as i32, &[(indent, Mode::Flat, inner)]) {
                    stack.push((indent, Mode::Flat, inner));
                } else {
                    stack.push((indent, Mode::Break, inner));
                }
            }
        }
    }

    output
}

/// Check if a document fits within the remaining width when rendered flat
fn fits(mut remaining: i32, stack: &[(i32, Mode, &Doc)]) -> bool {
    let mut work: Vec<(i32, Mode, &Doc)> = stack.to_vec();

    while remaining >= 0 {
        let Some((indent, mode, doc)) = work.pop() else {
            return true;
        };
        match doc {
            Doc::Nil => {}
            Doc::Text(s) => remaining -= s.len() as i32,
            Doc::HardLine => return true,
            Doc::Line => {
                if mode == Mode::Flat {
                    remaining -= 1;
                } else {
                    return true;
                }
            }
            Doc::LineOrEmpty => {
                if mode != Mode::Flat {
                    return true;
                }
            }
            Doc::Concat(docs) => {
                for d in docs.iter().rev() {
                    work.push((indent, mode, d));
                }
            }
            Doc::Nest(n, inner) => {
                work.push((indent + n, mode, inner));
            }
            Doc::Group(inner) => {
                work.push((indent, Mode::Flat, inner));
            }
        }
    }
    false
}

// ============================================================================
// CST -> Doc conversion
// ============================================================================

struct Formatter {
    options: FormatOptions,
    depth: std::cell::Cell<usize>,
}

const MAX_FORMAT_DEPTH: usize = 128;

impl Formatter {
    fn new(options: FormatOptions) -> Self {
        Self {
            options,
            depth: std::cell::Cell::new(0),
        }
    }

    /// Main entry: format a SyntaxNode (with depth guard)
    fn format_node(&self, node: &SyntaxNode) -> Doc {
        let d = self.depth.get();
        if d >= MAX_FORMAT_DEPTH {
            // Bail out: emit tokens inline without further recursion
            return text(node.text().to_string());
        }
        self.depth.set(d + 1);
        let result = self.format_node_inner(node);
        self.depth.set(d);
        result
    }

    fn format_node_inner(&self, node: &SyntaxNode) -> Doc {
        match node.kind() {
            SyntaxKind::SourceFile => self.fmt_source_file(node),
            SyntaxKind::LibraryClause | SyntaxKind::UseClause => self.fmt_inline(node),
            SyntaxKind::EntityDecl => self.fmt_entity_decl(node),
            SyntaxKind::ArchitectureBody => self.fmt_architecture_body(node),
            SyntaxKind::PackageDecl | SyntaxKind::PackageBody => self.fmt_package(node),
            SyntaxKind::PortClause => self.fmt_port_clause(node),
            SyntaxKind::GenericClause => self.fmt_generic_clause(node),
            SyntaxKind::PortDecl | SyntaxKind::GenericDecl => self.fmt_inline(node),
            SyntaxKind::PortDirection => self.fmt_port_direction(node),
            SyntaxKind::ProcessStmt => self.fmt_process_stmt(node),
            SyntaxKind::SensitivityList => self.fmt_sensitivity_list(node),
            SyntaxKind::IfStmt => self.fmt_if_stmt(node),
            SyntaxKind::CaseStmt => self.fmt_case_stmt(node),
            SyntaxKind::CaseAlternative => self.fmt_case_alternative(node),
            SyntaxKind::ForLoopStmt | SyntaxKind::WhileLoopStmt => {
                self.fmt_loop_stmt(node)
            }
            SyntaxKind::ComponentDecl => self.fmt_component_decl(node),
            SyntaxKind::ComponentInst => self.fmt_component_inst(node),
            SyntaxKind::ForGenerate | SyntaxKind::IfGenerate => {
                self.fmt_generate_stmt(node)
            }
            SyntaxKind::BlockStmt => self.fmt_block_stmt(node),
            SyntaxKind::FunctionBody | SyntaxKind::ProcedureBody => {
                self.fmt_subprogram_body(node)
            }
            SyntaxKind::ParamList => self.fmt_param_list(node),
            SyntaxKind::PortMap | SyntaxKind::GenericMap => self.fmt_map(node),
            SyntaxKind::AssociationList => self.fmt_association_list(node),
            SyntaxKind::RecordTypeDef => self.fmt_record_type_def(node),
            SyntaxKind::InterfaceDecl | SyntaxKind::ViewDecl => {
                self.fmt_interface_or_view(node)
            }
            _ => self.fmt_inline(node),
        }
    }

    // ========================================================================
    // Helpers
    // ========================================================================

    /// Get non-whitespace children of a node
    fn children(&self, node: &SyntaxNode) -> Vec<SyntaxElement> {
        node.children_with_tokens()
            .filter(|child| !matches!(child.kind(), SyntaxKind::Whitespace))
            .collect()
    }

    /// Format a token, lowercasing keywords if configured
    fn fmt_token(&self, token: &SyntaxToken) -> Doc {
        let kind = token.kind();
        if kind == SyntaxKind::Comment {
            return text(token.text().trim_end());
        }
        let txt = token.text();
        if self.options.lowercase_keywords && is_keyword(kind) {
            text(txt.to_lowercase())
        } else {
            text(txt.to_string())
        }
    }

    /// Format a child element (token or node)
    fn fmt_element(&self, elem: &SyntaxElement) -> Doc {
        match elem {
            SyntaxElement::Token(t) => self.fmt_token(t),
            SyntaxElement::Node(n) => self.format_node(n),
        }
    }

    /// Get the last token kind from a SyntaxElement (for spacing decisions)
    fn last_token_kind(elem: &SyntaxElement) -> Option<SyntaxKind> {
        match elem {
            SyntaxElement::Token(t) => Some(t.kind()),
            SyntaxElement::Node(n) => n.last_token().map(|t| t.kind()),
        }
    }

    /// Get the first token kind from a SyntaxElement
    fn first_token_kind(elem: &SyntaxElement) -> Option<SyntaxKind> {
        match elem {
            SyntaxElement::Token(t) => Some(t.kind()),
            SyntaxElement::Node(n) => n.first_token().map(|t| t.kind()),
        }
    }

    // ========================================================================
    // Default inline formatter
    // ========================================================================

    /// Format a node inline: tokens separated by smart spacing
    fn fmt_inline(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let mut docs = Vec::new();
        let mut prev: Option<&SyntaxElement> = None;

        for child in &children {
            if let Some(p) = prev {
                if let (Some(left), Some(right)) =
                    (Self::last_token_kind(p), Self::first_token_kind(child))
                {
                    if needs_space_between(left, right) {
                        docs.push(space());
                    }
                }
            }
            docs.push(self.fmt_element(child));
            prev = Some(child);
        }

        concat(docs)
    }

    // ========================================================================
    // Source file
    // ========================================================================

    fn fmt_source_file(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let mut docs: Vec<Doc> = Vec::new();
        let mut prev_kind: Option<SyntaxKind> = None;

        for child in &children {
            let kind = child.kind();
            let is_design_unit = matches!(
                kind,
                SyntaxKind::EntityDecl
                    | SyntaxKind::ArchitectureBody
                    | SyntaxKind::PackageDecl
                    | SyntaxKind::PackageBody
                    | SyntaxKind::InterfaceDecl
                    | SyntaxKind::ViewDecl
                    | SyntaxKind::PackageInstantiation
            );
            let is_context = matches!(
                kind,
                SyntaxKind::LibraryClause | SyntaxKind::UseClause
            );
            let prev_is_design = prev_kind.map_or(false, |pk| {
                matches!(
                    pk,
                    SyntaxKind::EntityDecl
                        | SyntaxKind::ArchitectureBody
                        | SyntaxKind::PackageDecl
                        | SyntaxKind::PackageBody
                        | SyntaxKind::InterfaceDecl
                        | SyntaxKind::ViewDecl
                        | SyntaxKind::PackageInstantiation
                )
            });
            let prev_is_context = prev_kind.map_or(false, |pk| {
                matches!(pk, SyntaxKind::LibraryClause | SyntaxKind::UseClause)
            });

            if !docs.is_empty() {
                if (is_design_unit && (prev_is_design || prev_is_context))
                    || (is_context && prev_is_design)
                {
                    // Blank line between design units and between context + design unit
                    docs.push(hardline());
                    docs.push(hardline());
                } else {
                    docs.push(hardline());
                }
            }

            docs.push(self.fmt_element(child));
            if kind != SyntaxKind::Comment {
                prev_kind = Some(kind);
            }
        }

        // Ensure file ends with newline
        if !docs.is_empty() {
            docs.push(hardline());
        }

        concat(docs)
    }

    // ========================================================================
    // Entity declaration
    // ========================================================================

    fn fmt_entity_decl(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let indent = self.options.indent_width;
        let mut docs = Vec::new();
        let mut i = 0;

        // entity NAME is
        //   [generic (...);]
        //   [port (...);]
        //   [declarations]
        // [begin
        //   statements]
        // end [entity] [NAME];

        // 'entity' NAME 'is'
        while i < children.len() {
            let kind = children[i].kind();
            if kind == SyntaxKind::IsKw {
                if !docs.is_empty() {
                    docs.push(space());
                }
                docs.push(self.fmt_element(&children[i]));
                i += 1;
                break;
            }
            if !docs.is_empty() {
                docs.push(space());
            }
            docs.push(self.fmt_element(&children[i]));
            i += 1;
        }

        // Body: generic clause, port clause, declarations
        let mut body_docs = Vec::new();
        while i < children.len() {
            let kind = children[i].kind();
            if kind == SyntaxKind::EndKw || kind == SyntaxKind::BeginKw {
                break;
            }
            body_docs.push(hardline());
            body_docs.push(self.fmt_element(&children[i]));
            i += 1;
        }
        if !body_docs.is_empty() {
            docs.push(nest(indent, concat(body_docs)));
        }

        // Optional 'begin' + passive statements
        if i < children.len() && children[i].kind() == SyntaxKind::BeginKw {
            docs.push(hardline());
            docs.push(self.fmt_element(&children[i]));
            i += 1;
            let mut stmt_docs = Vec::new();
            while i < children.len() && children[i].kind() != SyntaxKind::EndKw {
                stmt_docs.push(hardline());
                stmt_docs.push(self.fmt_element(&children[i]));
                i += 1;
            }
            if !stmt_docs.is_empty() {
                docs.push(nest(indent, concat(stmt_docs)));
            }
        }

        // end [entity] [NAME] ;
        docs.push(hardline());
        docs.push(self.fmt_end_clause(&children[i..]));

        concat(docs)
    }

    /// Format an `end [keyword] [name] ;` sequence
    fn fmt_end_clause(&self, children: &[SyntaxElement]) -> Doc {
        let mut docs = Vec::new();
        let mut prev: Option<&SyntaxElement> = None;

        for child in children {
            if let Some(p) = prev {
                if let (Some(left), Some(right)) =
                    (Self::last_token_kind(p), Self::first_token_kind(child))
                {
                    if needs_space_between(left, right) {
                        docs.push(space());
                    }
                }
            }
            docs.push(self.fmt_element(child));
            prev = Some(child);
        }
        concat(docs)
    }

    // ========================================================================
    // Port / Generic clauses
    // ========================================================================

    fn fmt_port_clause(&self, node: &SyntaxNode) -> Doc {
        self.fmt_clause_with_list(node, SyntaxKind::PortKw, SyntaxKind::PortDecl)
    }

    fn fmt_generic_clause(&self, node: &SyntaxNode) -> Doc {
        self.fmt_clause_with_list(node, SyntaxKind::GenericKw, SyntaxKind::GenericDecl)
    }

    /// Shared formatting for port(...) and generic(...) clauses
    fn fmt_clause_with_list(
        &self,
        node: &SyntaxNode,
        keyword_kind: SyntaxKind,
        decl_kind: SyntaxKind,
    ) -> Doc {
        let children = self.children(node);
        let indent = self.options.indent_width;
        let mut docs = Vec::new();
        let mut decls: Vec<Doc> = Vec::new();
        let mut in_parens = false;

        for child in &children {
            let kind = child.kind();
            match kind {
                k if k == keyword_kind => {
                    docs.push(self.fmt_element(child));
                }
                SyntaxKind::LParen => {
                    docs.push(space());
                    docs.push(text("("));
                    in_parens = true;
                }
                SyntaxKind::RParen => {
                    in_parens = false;
                }
                k if k == decl_kind && in_parens => {
                    decls.push(self.fmt_element(child));
                }
                SyntaxKind::Comment if in_parens => {
                    decls.push(self.fmt_element(child));
                }
                SyntaxKind::Semicolon if in_parens => {
                    // Separator between declarations -- handled by intersperse
                }
                _ => {}
            }
        }

        if decls.is_empty() {
            docs.push(text(");"));
        } else {
            let inner = intersperse(concat(vec![text(";"), hardline()]), decls);
            docs.push(nest(indent, concat(vec![hardline(), inner])));
            docs.push(hardline());
            docs.push(text(");"));
        }

        concat(docs)
    }

    fn fmt_port_direction(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        if children.is_empty() {
            return nil();
        }
        self.fmt_inline(node)
    }

    // ========================================================================
    // Architecture
    // ========================================================================

    fn fmt_architecture_body(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let indent = self.options.indent_width;
        let mut docs = Vec::new();
        let mut i = 0;

        // architecture NAME of ENTITY is
        while i < children.len() {
            let kind = children[i].kind();
            if kind == SyntaxKind::IsKw {
                if !docs.is_empty() {
                    docs.push(space());
                }
                docs.push(self.fmt_element(&children[i]));
                i += 1;
                break;
            }
            if !docs.is_empty() {
                docs.push(space());
            }
            docs.push(self.fmt_element(&children[i]));
            i += 1;
        }

        // Declarations (before 'begin')
        let mut decl_docs = Vec::new();
        while i < children.len() && children[i].kind() != SyntaxKind::BeginKw {
            decl_docs.push(hardline());
            decl_docs.push(self.fmt_element(&children[i]));
            i += 1;
        }
        if !decl_docs.is_empty() {
            docs.push(nest(indent, concat(decl_docs)));
        }

        // 'begin'
        if i < children.len() && children[i].kind() == SyntaxKind::BeginKw {
            docs.push(hardline());
            docs.push(self.fmt_element(&children[i]));
            i += 1;
        }

        // Concurrent statements (before 'end')
        let mut stmt_docs = Vec::new();
        let mut prev_was_block = false;
        while i < children.len() && children[i].kind() != SyntaxKind::EndKw {
            let kind = children[i].kind();
            let is_block = matches!(
                kind,
                SyntaxKind::ProcessStmt
                    | SyntaxKind::ComponentInst
                    | SyntaxKind::ForGenerate
                    | SyntaxKind::IfGenerate
                    | SyntaxKind::BlockStmt
            );
            // Blank line before block-level constructs
            if !stmt_docs.is_empty() && (is_block || prev_was_block) {
                stmt_docs.push(hardline());
            }
            stmt_docs.push(hardline());
            stmt_docs.push(self.fmt_element(&children[i]));
            prev_was_block = is_block;
            i += 1;
        }
        if !stmt_docs.is_empty() {
            docs.push(nest(indent, concat(stmt_docs)));
        }

        // end [architecture] [NAME] ;
        docs.push(hardline());
        if i < children.len() {
            docs.push(self.fmt_end_clause(&children[i..]));
        }

        concat(docs)
    }

    // ========================================================================
    // Process
    // ========================================================================

    fn fmt_process_stmt(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let indent = self.options.indent_width;
        let mut docs = Vec::new();
        let mut i = 0;

        // Optional label (ident + colon before 'process')
        if i + 1 < children.len()
            && children[i].kind() == SyntaxKind::Ident
            && children[i + 1].kind() == SyntaxKind::Colon
        {
            docs.push(self.fmt_element(&children[i]));
            docs.push(space());
            docs.push(text(":"));
            docs.push(space());
            i += 2;
        }

        // 'process'
        if i < children.len() && children[i].kind() == SyntaxKind::ProcessKw {
            docs.push(self.fmt_element(&children[i]));
            i += 1;
        }

        // Optional sensitivity list
        if i < children.len() && children[i].kind() == SyntaxKind::SensitivityList {
            docs.push(self.fmt_element(&children[i]));
            i += 1;
        }

        // Optional 'is' -- preserve it
        if i < children.len() && children[i].kind() == SyntaxKind::IsKw {
            docs.push(space());
            docs.push(self.fmt_element(&children[i]));
            i += 1;
        }

        // Declarations (before 'begin')
        let mut decl_docs = Vec::new();
        while i < children.len() && children[i].kind() != SyntaxKind::BeginKw {
            decl_docs.push(hardline());
            decl_docs.push(self.fmt_element(&children[i]));
            i += 1;
        }
        if !decl_docs.is_empty() {
            docs.push(nest(indent, concat(decl_docs)));
        }

        // 'begin'
        if i < children.len() && children[i].kind() == SyntaxKind::BeginKw {
            docs.push(hardline());
            docs.push(self.fmt_element(&children[i]));
            i += 1;
        }

        // Sequential statements (before 'end')
        let mut stmt_docs = Vec::new();
        while i < children.len() && children[i].kind() != SyntaxKind::EndKw {
            stmt_docs.push(hardline());
            stmt_docs.push(self.fmt_element(&children[i]));
            i += 1;
        }
        if !stmt_docs.is_empty() {
            docs.push(nest(indent, concat(stmt_docs)));
        }

        // 'end' 'process' [label] ';'
        docs.push(hardline());
        if i < children.len() {
            docs.push(self.fmt_end_clause(&children[i..]));
        }

        concat(docs)
    }

    fn fmt_sensitivity_list(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let mut docs = Vec::new();

        for child in &children {
            match child.kind() {
                SyntaxKind::Comma => {
                    docs.push(text(","));
                    docs.push(space());
                }
                _ => {
                    docs.push(self.fmt_element(child));
                }
            }
        }
        concat(docs)
    }

    // ========================================================================
    // If statement
    // ========================================================================

    fn fmt_if_stmt(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let indent = self.options.indent_width;
        let mut docs = Vec::new();
        let mut i = 0;

        while i < children.len() {
            let kind = children[i].kind();
            match kind {
                SyntaxKind::IfKw => {
                    docs.push(self.fmt_element(&children[i]));
                    docs.push(space());
                    i += 1;
                    // Condition (everything until 'then')
                    let mut cond_docs = Vec::new();
                    while i < children.len() && children[i].kind() != SyntaxKind::ThenKw
                    {
                        if !cond_docs.is_empty() {
                            cond_docs.push(space());
                        }
                        cond_docs.push(self.fmt_element(&children[i]));
                        i += 1;
                    }
                    docs.push(concat(cond_docs));
                    // 'then'
                    if i < children.len() && children[i].kind() == SyntaxKind::ThenKw {
                        docs.push(space());
                        docs.push(self.fmt_element(&children[i]));
                        i += 1;
                    }
                    // Statements until elsif/else/end
                    let mut stmt_docs = Vec::new();
                    while i < children.len()
                        && !matches!(
                            children[i].kind(),
                            SyntaxKind::ElsifKw
                                | SyntaxKind::ElseKw
                                | SyntaxKind::EndKw
                        )
                    {
                        stmt_docs.push(hardline());
                        stmt_docs.push(self.fmt_element(&children[i]));
                        i += 1;
                    }
                    if !stmt_docs.is_empty() {
                        docs.push(nest(indent, concat(stmt_docs)));
                    }
                }
                SyntaxKind::ElsifKw => {
                    docs.push(hardline());
                    docs.push(self.fmt_element(&children[i]));
                    docs.push(space());
                    i += 1;
                    // Condition
                    let mut cond_docs = Vec::new();
                    while i < children.len() && children[i].kind() != SyntaxKind::ThenKw
                    {
                        if !cond_docs.is_empty() {
                            cond_docs.push(space());
                        }
                        cond_docs.push(self.fmt_element(&children[i]));
                        i += 1;
                    }
                    docs.push(concat(cond_docs));
                    // 'then'
                    if i < children.len() && children[i].kind() == SyntaxKind::ThenKw {
                        docs.push(space());
                        docs.push(self.fmt_element(&children[i]));
                        i += 1;
                    }
                    // Statements
                    let mut stmt_docs = Vec::new();
                    while i < children.len()
                        && !matches!(
                            children[i].kind(),
                            SyntaxKind::ElsifKw
                                | SyntaxKind::ElseKw
                                | SyntaxKind::EndKw
                        )
                    {
                        stmt_docs.push(hardline());
                        stmt_docs.push(self.fmt_element(&children[i]));
                        i += 1;
                    }
                    if !stmt_docs.is_empty() {
                        docs.push(nest(indent, concat(stmt_docs)));
                    }
                }
                SyntaxKind::ElseKw => {
                    docs.push(hardline());
                    docs.push(self.fmt_element(&children[i]));
                    i += 1;
                    // Statements
                    let mut stmt_docs = Vec::new();
                    while i < children.len() && children[i].kind() != SyntaxKind::EndKw {
                        stmt_docs.push(hardline());
                        stmt_docs.push(self.fmt_element(&children[i]));
                        i += 1;
                    }
                    if !stmt_docs.is_empty() {
                        docs.push(nest(indent, concat(stmt_docs)));
                    }
                }
                SyntaxKind::EndKw => {
                    docs.push(hardline());
                    docs.push(self.fmt_end_clause(&children[i..]));
                    break;
                }
                _ => {
                    i += 1;
                }
            }
        }

        concat(docs)
    }

    // ========================================================================
    // Case statement
    // ========================================================================

    fn fmt_case_stmt(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let indent = self.options.indent_width;
        let mut docs = Vec::new();
        let mut i = 0;

        // 'case'
        if i < children.len() && children[i].kind() == SyntaxKind::CaseKw {
            docs.push(self.fmt_element(&children[i]));
            docs.push(space());
            i += 1;
        }

        // Expression (until 'is')
        let mut expr_docs = Vec::new();
        while i < children.len() && children[i].kind() != SyntaxKind::IsKw {
            if !expr_docs.is_empty() {
                expr_docs.push(space());
            }
            expr_docs.push(self.fmt_element(&children[i]));
            i += 1;
        }
        docs.push(concat(expr_docs));

        // 'is'
        if i < children.len() && children[i].kind() == SyntaxKind::IsKw {
            docs.push(space());
            docs.push(self.fmt_element(&children[i]));
            i += 1;
        }

        // Case alternatives (until 'end')
        let mut alt_docs = Vec::new();
        while i < children.len() && children[i].kind() != SyntaxKind::EndKw {
            alt_docs.push(hardline());
            alt_docs.push(self.fmt_element(&children[i]));
            i += 1;
        }
        if !alt_docs.is_empty() {
            docs.push(nest(indent, concat(alt_docs)));
        }

        // end case ;
        docs.push(hardline());
        if i < children.len() {
            docs.push(self.fmt_end_clause(&children[i..]));
        }

        concat(docs)
    }

    fn fmt_case_alternative(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let indent = self.options.indent_width;
        let mut docs = Vec::new();
        let mut i = 0;

        // 'when'
        if i < children.len() && children[i].kind() == SyntaxKind::WhenKw {
            docs.push(self.fmt_element(&children[i]));
            docs.push(space());
            i += 1;
        }

        // Choices (until '=>')
        let mut choice_docs = Vec::new();
        while i < children.len() && children[i].kind() != SyntaxKind::Arrow {
            if children[i].kind() == SyntaxKind::Bar {
                choice_docs.push(space());
                choice_docs.push(self.fmt_element(&children[i]));
                choice_docs.push(space());
            } else {
                choice_docs.push(self.fmt_element(&children[i]));
            }
            i += 1;
        }
        docs.push(concat(choice_docs));

        // '=>'
        if i < children.len() && children[i].kind() == SyntaxKind::Arrow {
            docs.push(space());
            docs.push(self.fmt_element(&children[i]));
            i += 1;
        }

        // Statements
        let mut stmt_docs = Vec::new();
        while i < children.len() {
            stmt_docs.push(hardline());
            stmt_docs.push(self.fmt_element(&children[i]));
            i += 1;
        }
        if !stmt_docs.is_empty() {
            docs.push(nest(indent, concat(stmt_docs)));
        }

        concat(docs)
    }

    // ========================================================================
    // Loop statements
    // ========================================================================

    fn fmt_loop_stmt(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let indent = self.options.indent_width;
        let mut docs = Vec::new();
        let mut i = 0;

        // Header (until 'loop')
        let mut header_docs = Vec::new();
        while i < children.len() && children[i].kind() != SyntaxKind::LoopKw {
            if !header_docs.is_empty() {
                header_docs.push(space());
            }
            header_docs.push(self.fmt_element(&children[i]));
            i += 1;
        }
        docs.push(concat(header_docs));

        // 'loop'
        if i < children.len() && children[i].kind() == SyntaxKind::LoopKw {
            docs.push(space());
            docs.push(self.fmt_element(&children[i]));
            i += 1;
        }

        // Statements (until 'end')
        let mut stmt_docs = Vec::new();
        while i < children.len() && children[i].kind() != SyntaxKind::EndKw {
            stmt_docs.push(hardline());
            stmt_docs.push(self.fmt_element(&children[i]));
            i += 1;
        }
        if !stmt_docs.is_empty() {
            docs.push(nest(indent, concat(stmt_docs)));
        }

        // end loop [label] ;
        docs.push(hardline());
        if i < children.len() {
            docs.push(self.fmt_end_clause(&children[i..]));
        }

        concat(docs)
    }

    // ========================================================================
    // Component instantiation
    // ========================================================================

    fn fmt_component_inst(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let indent = self.options.indent_width;
        let mut docs = Vec::new();
        let mut i = 0;

        // Header tokens (before GenericMap/PortMap)
        let mut header_docs = Vec::new();
        while i < children.len()
            && !matches!(
                children[i].kind(),
                SyntaxKind::GenericMap | SyntaxKind::PortMap | SyntaxKind::Semicolon
            )
        {
            if !header_docs.is_empty() {
                header_docs.push(space());
            }
            header_docs.push(self.fmt_element(&children[i]));
            i += 1;
        }
        docs.push(concat(header_docs));

        // Generic map and port map
        let mut map_docs = Vec::new();
        while i < children.len() && children[i].kind() != SyntaxKind::Semicolon {
            map_docs.push(hardline());
            map_docs.push(self.fmt_element(&children[i]));
            i += 1;
        }
        if !map_docs.is_empty() {
            docs.push(nest(indent, concat(map_docs)));
        }

        // Semicolon
        if i < children.len() && children[i].kind() == SyntaxKind::Semicolon {
            docs.push(text(";"));
        }

        concat(docs)
    }

    fn fmt_map(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let mut docs = Vec::new();

        for child in &children {
            match child.kind() {
                SyntaxKind::PortKw | SyntaxKind::GenericKw => {
                    docs.push(self.fmt_element(child));
                }
                SyntaxKind::MapKw => {
                    docs.push(space());
                    docs.push(self.fmt_element(child));
                }
                SyntaxKind::AssociationList => {
                    docs.push(space());
                    docs.push(self.fmt_element(child));
                }
                _ => {
                    docs.push(self.fmt_element(child));
                }
            }
        }

        concat(docs)
    }

    fn fmt_association_list(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let indent = self.options.indent_width;
        let mut assoc_elements: Vec<Doc> = Vec::new();

        for child in &children {
            match child.kind() {
                SyntaxKind::LParen | SyntaxKind::RParen | SyntaxKind::Comma => {}
                SyntaxKind::AssociationElement | SyntaxKind::Comment => {
                    assoc_elements.push(self.fmt_element(child));
                }
                _ => {}
            }
        }

        if assoc_elements.is_empty() {
            return text("()");
        }

        // Try to fit all on one line, otherwise break
        group(concat(vec![
            text("("),
            nest(
                indent,
                concat(vec![
                    line_or_empty(),
                    intersperse(concat(vec![text(","), line()]), assoc_elements),
                ]),
            ),
            line_or_empty(),
            text(")"),
        ]))
    }

    // ========================================================================
    // Component declaration
    // ========================================================================

    fn fmt_component_decl(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let indent = self.options.indent_width;
        let mut docs = Vec::new();
        let mut i = 0;

        // Header: component NAME [is]
        while i < children.len() {
            let kind = children[i].kind();
            if matches!(
                kind,
                SyntaxKind::GenericClause | SyntaxKind::PortClause | SyntaxKind::EndKw
            ) {
                break;
            }
            if !docs.is_empty() {
                docs.push(space());
            }
            docs.push(self.fmt_element(&children[i]));
            i += 1;
        }

        // Body: generic/port clauses
        let mut body_docs = Vec::new();
        while i < children.len() && children[i].kind() != SyntaxKind::EndKw {
            body_docs.push(hardline());
            body_docs.push(self.fmt_element(&children[i]));
            i += 1;
        }
        if !body_docs.is_empty() {
            docs.push(nest(indent, concat(body_docs)));
        }

        // end component [NAME] ;
        docs.push(hardline());
        if i < children.len() {
            docs.push(self.fmt_end_clause(&children[i..]));
        }

        concat(docs)
    }

    // ========================================================================
    // Generate statements
    // ========================================================================

    fn fmt_generate_stmt(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let indent = self.options.indent_width;
        let mut docs = Vec::new();
        let mut i = 0;

        // Header until 'generate'
        let mut header_docs = Vec::new();
        while i < children.len() && children[i].kind() != SyntaxKind::GenerateKw {
            if !header_docs.is_empty() {
                header_docs.push(space());
            }
            header_docs.push(self.fmt_element(&children[i]));
            i += 1;
        }
        docs.push(concat(header_docs));

        // 'generate'
        if i < children.len() && children[i].kind() == SyntaxKind::GenerateKw {
            docs.push(space());
            docs.push(self.fmt_element(&children[i]));
            i += 1;
        }

        // Body until 'end'
        let mut body_docs = Vec::new();
        while i < children.len() && children[i].kind() != SyntaxKind::EndKw {
            body_docs.push(hardline());
            body_docs.push(self.fmt_element(&children[i]));
            i += 1;
        }
        if !body_docs.is_empty() {
            docs.push(nest(indent, concat(body_docs)));
        }

        // end generate [label] ;
        docs.push(hardline());
        if i < children.len() {
            docs.push(self.fmt_end_clause(&children[i..]));
        }

        concat(docs)
    }

    // ========================================================================
    // Block statement
    // ========================================================================

    fn fmt_block_stmt(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let indent = self.options.indent_width;
        let mut docs = Vec::new();
        let mut i = 0;

        // Header until 'is' or first declaration/begin
        let mut header_docs = Vec::new();
        while i < children.len()
            && !matches!(
                children[i].kind(),
                SyntaxKind::IsKw
                    | SyntaxKind::BeginKw
                    | SyntaxKind::SignalDecl
                    | SyntaxKind::EndKw
            )
        {
            if !header_docs.is_empty() {
                header_docs.push(space());
            }
            header_docs.push(self.fmt_element(&children[i]));
            i += 1;
        }
        docs.push(concat(header_docs));

        // Optional 'is'
        if i < children.len() && children[i].kind() == SyntaxKind::IsKw {
            docs.push(space());
            docs.push(self.fmt_element(&children[i]));
            i += 1;
        }

        // Declarations
        let mut decl_docs = Vec::new();
        while i < children.len()
            && children[i].kind() != SyntaxKind::BeginKw
            && children[i].kind() != SyntaxKind::EndKw
        {
            decl_docs.push(hardline());
            decl_docs.push(self.fmt_element(&children[i]));
            i += 1;
        }
        if !decl_docs.is_empty() {
            docs.push(nest(indent, concat(decl_docs)));
        }

        // 'begin'
        if i < children.len() && children[i].kind() == SyntaxKind::BeginKw {
            docs.push(hardline());
            docs.push(self.fmt_element(&children[i]));
            i += 1;
        }

        // Statements
        let mut stmt_docs = Vec::new();
        while i < children.len() && children[i].kind() != SyntaxKind::EndKw {
            stmt_docs.push(hardline());
            stmt_docs.push(self.fmt_element(&children[i]));
            i += 1;
        }
        if !stmt_docs.is_empty() {
            docs.push(nest(indent, concat(stmt_docs)));
        }

        // end block [label] ;
        docs.push(hardline());
        if i < children.len() {
            docs.push(self.fmt_end_clause(&children[i..]));
        }

        concat(docs)
    }

    // ========================================================================
    // Packages
    // ========================================================================

    fn fmt_package(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let indent = self.options.indent_width;
        let mut docs = Vec::new();
        let mut i = 0;

        // Header until 'is'
        while i < children.len() {
            let kind = children[i].kind();
            if kind == SyntaxKind::IsKw {
                if !docs.is_empty() {
                    docs.push(space());
                }
                docs.push(self.fmt_element(&children[i]));
                i += 1;
                break;
            }
            if !docs.is_empty() {
                docs.push(space());
            }
            docs.push(self.fmt_element(&children[i]));
            i += 1;
        }

        // Declarations (until 'end')
        let mut decl_docs = Vec::new();
        while i < children.len() && children[i].kind() != SyntaxKind::EndKw {
            decl_docs.push(hardline());
            decl_docs.push(self.fmt_element(&children[i]));
            i += 1;
        }
        if !decl_docs.is_empty() {
            docs.push(nest(indent, concat(decl_docs)));
        }

        // end [package] [NAME] ;
        docs.push(hardline());
        if i < children.len() {
            docs.push(self.fmt_end_clause(&children[i..]));
        }

        concat(docs)
    }

    // ========================================================================
    // Subprograms
    // ========================================================================

    fn fmt_subprogram_body(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let indent = self.options.indent_width;
        let mut docs = Vec::new();
        let mut i = 0;

        // Header until 'is'
        let mut header_docs = Vec::new();
        while i < children.len() {
            let kind = children[i].kind();
            if kind == SyntaxKind::IsKw {
                if !header_docs.is_empty() {
                    header_docs.push(space());
                }
                header_docs.push(self.fmt_element(&children[i]));
                i += 1;
                break;
            }
            if !header_docs.is_empty() && kind != SyntaxKind::ParamList {
                header_docs.push(space());
            }
            header_docs.push(self.fmt_element(&children[i]));
            i += 1;
        }
        docs.push(concat(header_docs));

        // Declarations (before 'begin')
        let mut decl_docs = Vec::new();
        while i < children.len() && children[i].kind() != SyntaxKind::BeginKw {
            decl_docs.push(hardline());
            decl_docs.push(self.fmt_element(&children[i]));
            i += 1;
        }
        if !decl_docs.is_empty() {
            docs.push(nest(indent, concat(decl_docs)));
        }

        // 'begin'
        if i < children.len() && children[i].kind() == SyntaxKind::BeginKw {
            docs.push(hardline());
            docs.push(self.fmt_element(&children[i]));
            i += 1;
        }

        // Statements (before 'end')
        let mut stmt_docs = Vec::new();
        while i < children.len() && children[i].kind() != SyntaxKind::EndKw {
            stmt_docs.push(hardline());
            stmt_docs.push(self.fmt_element(&children[i]));
            i += 1;
        }
        if !stmt_docs.is_empty() {
            docs.push(nest(indent, concat(stmt_docs)));
        }

        // end [function/procedure] [NAME] ;
        docs.push(hardline());
        if i < children.len() {
            docs.push(self.fmt_end_clause(&children[i..]));
        }

        concat(docs)
    }

    fn fmt_param_list(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let indent = self.options.indent_width;
        let mut params: Vec<Doc> = Vec::new();

        for child in &children {
            match child.kind() {
                SyntaxKind::LParen | SyntaxKind::RParen | SyntaxKind::Semicolon => {}
                SyntaxKind::ParamDecl => {
                    params.push(self.fmt_element(child));
                }
                _ => {}
            }
        }

        if params.is_empty() {
            return text("()");
        }

        group(concat(vec![
            text("("),
            nest(
                indent,
                concat(vec![
                    line_or_empty(),
                    intersperse(concat(vec![text(";"), line()]), params),
                ]),
            ),
            line_or_empty(),
            text(")"),
        ]))
    }

    // ========================================================================
    // Record type definition
    // ========================================================================

    fn fmt_record_type_def(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let indent = self.options.indent_width;
        let mut docs = Vec::new();
        let mut i = 0;

        // 'record'
        if i < children.len() && children[i].kind() == SyntaxKind::RecordKw {
            docs.push(self.fmt_element(&children[i]));
            i += 1;
        }

        // Fields
        let mut field_docs = Vec::new();
        while i < children.len() && children[i].kind() != SyntaxKind::EndKw {
            field_docs.push(hardline());
            field_docs.push(self.fmt_element(&children[i]));
            i += 1;
        }
        if !field_docs.is_empty() {
            docs.push(nest(indent, concat(field_docs)));
        }

        // end record
        docs.push(hardline());
        if i < children.len() {
            docs.push(self.fmt_end_clause(&children[i..]));
        }

        concat(docs)
    }

    // ========================================================================
    // VHDL-2019 interfaces
    // ========================================================================

    fn fmt_interface_or_view(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let indent = self.options.indent_width;
        let mut docs = Vec::new();
        let mut i = 0;

        // Header until 'is'
        while i < children.len() {
            let kind = children[i].kind();
            if kind == SyntaxKind::IsKw {
                if !docs.is_empty() {
                    docs.push(space());
                }
                docs.push(self.fmt_element(&children[i]));
                i += 1;
                break;
            }
            if !docs.is_empty() {
                docs.push(space());
            }
            docs.push(self.fmt_element(&children[i]));
            i += 1;
        }

        // Declarations
        let mut decl_docs = Vec::new();
        while i < children.len() && children[i].kind() != SyntaxKind::EndKw {
            decl_docs.push(hardline());
            decl_docs.push(self.fmt_element(&children[i]));
            i += 1;
        }
        if !decl_docs.is_empty() {
            docs.push(nest(indent, concat(decl_docs)));
        }

        // end ...
        docs.push(hardline());
        if i < children.len() {
            docs.push(self.fmt_end_clause(&children[i..]));
        }

        concat(docs)
    }
}

// ============================================================================
// Token classification helpers
// ============================================================================

fn is_keyword(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::EntityKw
            | SyntaxKind::ArchitectureKw
            | SyntaxKind::PackageKw
            | SyntaxKind::BodyKw
            | SyntaxKind::ConfigurationKw
            | SyntaxKind::LibraryKw
            | SyntaxKind::UseKw
            | SyntaxKind::PortKw
            | SyntaxKind::GenericKw
            | SyntaxKind::MapKw
            | SyntaxKind::ComponentKw
            | SyntaxKind::InKw
            | SyntaxKind::OutKw
            | SyntaxKind::InoutKw
            | SyntaxKind::BufferKw
            | SyntaxKind::SignalKw
            | SyntaxKind::VariableKw
            | SyntaxKind::ConstantKw
            | SyntaxKind::TypeKw
            | SyntaxKind::SubtypeKw
            | SyntaxKind::AliasKw
            | SyntaxKind::AttributeKw
            | SyntaxKind::ArrayKw
            | SyntaxKind::RecordKw
            | SyntaxKind::RangeKw
            | SyntaxKind::ProcessKw
            | SyntaxKind::BeginKw
            | SyntaxKind::EndKw
            | SyntaxKind::GenerateKw
            | SyntaxKind::BlockKw
            | SyntaxKind::IfKw
            | SyntaxKind::ThenKw
            | SyntaxKind::ElsifKw
            | SyntaxKind::ElseKw
            | SyntaxKind::CaseKw
            | SyntaxKind::WhenKw
            | SyntaxKind::ForKw
            | SyntaxKind::LoopKw
            | SyntaxKind::WhileKw
            | SyntaxKind::NextKw
            | SyntaxKind::ExitKw
            | SyntaxKind::ReturnKw
            | SyntaxKind::NullKw
            | SyntaxKind::AssertKw
            | SyntaxKind::ReportKw
            | SyntaxKind::SeverityKw
            | SyntaxKind::IsKw
            | SyntaxKind::OfKw
            | SyntaxKind::AllKw
            | SyntaxKind::OthersKw
            | SyntaxKind::OpenKw
            | SyntaxKind::WithKw
            | SyntaxKind::SelectKw
            | SyntaxKind::UnaffectedKw
            | SyntaxKind::AndKw
            | SyntaxKind::OrKw
            | SyntaxKind::XorKw
            | SyntaxKind::NandKw
            | SyntaxKind::NorKw
            | SyntaxKind::XnorKw
            | SyntaxKind::NotKw
            | SyntaxKind::ModKw
            | SyntaxKind::RemKw
            | SyntaxKind::AbsKw
            | SyntaxKind::SllKw
            | SyntaxKind::SrlKw
            | SyntaxKind::SlaKw
            | SyntaxKind::SraKw
            | SyntaxKind::RolKw
            | SyntaxKind::RorKw
            | SyntaxKind::ToKw
            | SyntaxKind::DowntoKw
            | SyntaxKind::FunctionKw
            | SyntaxKind::ProcedureKw
            | SyntaxKind::ImpureKw
            | SyntaxKind::PureKw
            | SyntaxKind::WaitKw
            | SyntaxKind::AfterKw
            | SyntaxKind::TransportKw
            | SyntaxKind::RejectKw
            | SyntaxKind::FileKw
            | SyntaxKind::AccessKw
            | SyntaxKind::SharedKw
            | SyntaxKind::NewKw
            | SyntaxKind::InterfaceKw
            | SyntaxKind::ViewKw
            | SyntaxKind::PrivateKw
            | SyntaxKind::TrueKw
            | SyntaxKind::FalseKw
            | SyntaxKind::StdLogicKw
            | SyntaxKind::StdUlogicKw
            | SyntaxKind::StdLogicVectorKw
            | SyntaxKind::StdUlogicVectorKw
            | SyntaxKind::UnsignedKw
            | SyntaxKind::SignedKw
            | SyntaxKind::BooleanKw
            | SyntaxKind::IntegerKw
            | SyntaxKind::NaturalKw
            | SyntaxKind::PositiveKw
            | SyntaxKind::RealKw
            | SyntaxKind::StringKw
            | SyntaxKind::BitKw
            | SyntaxKind::BitVectorKw
            | SyntaxKind::RisingEdgeKw
            | SyntaxKind::FallingEdgeKw
            | SyntaxKind::ToUnsignedKw
            | SyntaxKind::ToSignedKw
            | SyntaxKind::ToIntegerKw
            | SyntaxKind::ResizeKw
            | SyntaxKind::ConvIntegerKw
            | SyntaxKind::ConvStdLogicVectorKw
    )
}

fn is_name_like(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::Ident
            | SyntaxKind::IntLiteral
            | SyntaxKind::RealLiteral
            | SyntaxKind::BasedLiteral
            | SyntaxKind::StringLiteral
            | SyntaxKind::CharLiteral
            | SyntaxKind::BitStringLiteral
            | SyntaxKind::RParen
            | SyntaxKind::RBracket
            | SyntaxKind::TrueKw
            | SyntaxKind::FalseKw
            | SyntaxKind::StdLogicKw
            | SyntaxKind::StdUlogicKw
            | SyntaxKind::StdLogicVectorKw
            | SyntaxKind::StdUlogicVectorKw
            | SyntaxKind::UnsignedKw
            | SyntaxKind::SignedKw
            | SyntaxKind::BooleanKw
            | SyntaxKind::IntegerKw
            | SyntaxKind::NaturalKw
            | SyntaxKind::PositiveKw
            | SyntaxKind::RealKw
            | SyntaxKind::StringKw
            | SyntaxKind::BitKw
            | SyntaxKind::BitVectorKw
            | SyntaxKind::RisingEdgeKw
            | SyntaxKind::FallingEdgeKw
            | SyntaxKind::ToUnsignedKw
            | SyntaxKind::ToSignedKw
            | SyntaxKind::ToIntegerKw
            | SyntaxKind::ResizeKw
            | SyntaxKind::ConvIntegerKw
            | SyntaxKind::ConvStdLogicVectorKw
    )
}

/// Determine if a space is needed between two adjacent token kinds
fn needs_space_between(left: SyntaxKind, right: SyntaxKind) -> bool {
    // No space before certain tokens
    if matches!(right, SyntaxKind::Semicolon | SyntaxKind::Comma) {
        return false;
    }
    // No space after '(' or before ')'
    if left == SyntaxKind::LParen || right == SyntaxKind::RParen {
        return false;
    }
    // No space around '.'
    if left == SyntaxKind::Dot || right == SyntaxKind::Dot {
        return false;
    }
    // No space around tick (attribute mark)
    if left == SyntaxKind::Tick || right == SyntaxKind::Tick {
        return false;
    }
    // No space before '(' for function calls / type constructors
    if right == SyntaxKind::LParen && is_name_like(left) {
        return false;
    }
    // Space around assignment operators
    if matches!(
        left,
        SyntaxKind::SignalAssign | SyntaxKind::VarAssign | SyntaxKind::Arrow
    ) {
        return true;
    }
    if matches!(
        right,
        SyntaxKind::SignalAssign | SyntaxKind::VarAssign | SyntaxKind::Arrow
    ) {
        return true;
    }
    // Space around binary operators
    if matches!(
        left,
        SyntaxKind::Plus
            | SyntaxKind::Minus
            | SyntaxKind::Star
            | SyntaxKind::Slash
            | SyntaxKind::Equal
            | SyntaxKind::NotEqual
            | SyntaxKind::LessThan
            | SyntaxKind::GreaterThan
            | SyntaxKind::GreaterEqual
            | SyntaxKind::DoubleStar
            | SyntaxKind::Ampersand
            | SyntaxKind::Bar
    ) {
        return true;
    }
    if matches!(
        right,
        SyntaxKind::Plus
            | SyntaxKind::Minus
            | SyntaxKind::Star
            | SyntaxKind::Slash
            | SyntaxKind::Equal
            | SyntaxKind::NotEqual
            | SyntaxKind::LessThan
            | SyntaxKind::GreaterThan
            | SyntaxKind::GreaterEqual
            | SyntaxKind::DoubleStar
            | SyntaxKind::Ampersand
            | SyntaxKind::Bar
    ) {
        return true;
    }
    // Space around colon
    if left == SyntaxKind::Colon || right == SyntaxKind::Colon {
        return true;
    }
    // Space between identifiers/keywords (word-like tokens)
    if (is_keyword(left) || is_name_like(left))
        && (is_keyword(right) || is_name_like(right))
    {
        return true;
    }
    // Space after keyword before most tokens
    if is_keyword(left)
        && !matches!(
            right,
            SyntaxKind::Semicolon | SyntaxKind::Comma | SyntaxKind::RParen
        )
    {
        return true;
    }
    // Space before keyword after most tokens
    if is_keyword(right) && is_name_like(left) {
        return true;
    }
    // Default for non-trivial adjacent tokens
    true
}

// ============================================================================
// Public API
// ============================================================================

/// Format VHDL source code with default options
pub fn format_vhdl(source: &str) -> Result<String> {
    format_vhdl_with_options(source, &FormatOptions::default())
}

/// Format VHDL source code with custom options
pub fn format_vhdl_with_options(source: &str, options: &FormatOptions) -> Result<String> {
    let parse_result = parse_vhdl(source);

    // Don't format if there are parse errors
    if !parse_result.errors.is_empty() {
        let msgs: Vec<String> = parse_result
            .errors
            .iter()
            .map(|e| e.message.clone())
            .collect();
        anyhow::bail!(
            "Cannot format VHDL with parse errors: {}",
            msgs.join("; ")
        );
    }

    let formatter = Formatter::new(FormatOptions {
        line_width: options.line_width,
        indent_width: options.indent_width,
        lowercase_keywords: options.lowercase_keywords,
    });

    let doc = formatter.format_node(&parse_result.root);
    let result = layout(options.line_width, &doc);

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_simple_entity() {
        let source = "entity test is\nend entity test;\n";
        let result = format_vhdl(source).unwrap();
        assert!(result.contains("entity test is"));
        assert!(result.contains("end entity test;"));
    }

    #[test]
    fn test_format_entity_with_ports() {
        let source = r#"library ieee;
use ieee.std_logic_1164.all;

entity test_entity is
    port (
        clk   : in  std_logic;
        rst   : in  std_logic;
        data  : out std_logic_vector(7 downto 0)
    );
end entity test_entity;
"#;
        let result = format_vhdl(source).unwrap();
        assert!(result.contains("library ieee;"));
        assert!(result.contains("entity test_entity is"));
        assert!(result.contains("port ("));
        assert!(result.contains("end entity test_entity;"));
    }

    #[test]
    fn test_format_keywords_lowercased() {
        let source = "ENTITY test IS\nEND ENTITY test;\n";
        let result = format_vhdl(source).unwrap();
        assert!(result.contains("entity test is"));
        assert!(result.contains("end entity test;"));
    }

    #[test]
    fn test_format_architecture() {
        let source = r#"
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity counter is
    port (
        clk   : in  std_logic;
        rst   : in  std_logic;
        count : out unsigned(7 downto 0)
    );
end entity counter;

architecture rtl of counter is
    signal count_reg : unsigned(7 downto 0);
begin
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                count_reg <= (others => '0');
            else
                count_reg <= count_reg + 1;
            end if;
        end if;
    end process;

    count <= count_reg;
end architecture rtl;
"#;
        let result = format_vhdl(source).unwrap();
        assert!(result.contains("architecture rtl of counter is"));
        assert!(result.contains("process(clk)"));
        assert!(result.contains("end process;"));
        assert!(result.contains("end architecture rtl;"));
    }

    #[test]
    fn test_format_case_statement() {
        let source = r#"
library ieee;
use ieee.std_logic_1164.all;

entity mux is
    port (
        a   : in  std_logic_vector(7 downto 0);
        sel : in  std_logic;
        y   : out std_logic_vector(7 downto 0)
    );
end entity mux;

architecture rtl of mux is
begin
    process(all)
    begin
        case sel is
            when '0' =>
                y <= a;
            when others =>
                y <= a;
        end case;
    end process;
end architecture rtl;
"#;
        let result = format_vhdl(source).unwrap();
        assert!(result.contains("case sel is"));
        assert!(result.contains("when '0' =>"));
        assert!(result.contains("end case;"));
    }

    #[test]
    fn test_format_preserves_comments() {
        let source = r#"
library ieee;
use ieee.std_logic_1164.all;

-- This is a counter entity
entity counter is
    port (
        clk : in std_logic
    );
end entity counter;
"#;
        let result = format_vhdl(source).unwrap();
        assert!(result.contains("-- This is a counter entity"));
    }

    #[test]
    fn test_format_roundtrip_counter() {
        let source = std::fs::read_to_string(
            std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                .parent()
                .unwrap()
                .parent()
                .unwrap()
                .join("examples/vhdl/counter.vhd"),
        )
        .unwrap();
        let result = format_vhdl(&source).unwrap();
        // Formatted output should still parse
        let re_parsed = parse_vhdl(&result);
        assert!(
            re_parsed.errors.is_empty(),
            "formatted output has parse errors: {:?}",
            re_parsed.errors
        );
    }

    #[test]
    fn test_format_roundtrip_mux4() {
        let source = std::fs::read_to_string(
            std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                .parent()
                .unwrap()
                .parent()
                .unwrap()
                .join("examples/vhdl/mux4.vhd"),
        )
        .unwrap();
        let result = format_vhdl(&source).unwrap();
        let re_parsed = parse_vhdl(&result);
        assert!(
            re_parsed.errors.is_empty(),
            "formatted output has parse errors: {:?}",
            re_parsed.errors
        );
    }

    #[test]
    fn test_format_idempotent() {
        let source = r#"library ieee;
use ieee.std_logic_1164.all;

entity test is
  port (
    clk : in std_logic;
    data : out std_logic_vector(7 downto 0)
  );
end entity test;
"#;
        let first = format_vhdl(source).unwrap();
        let second = format_vhdl(&first).unwrap();
        assert_eq!(first, second, "formatter is not idempotent");
    }
}
