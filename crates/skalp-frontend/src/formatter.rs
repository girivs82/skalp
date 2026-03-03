//! Skalp source formatter using Wadler-Lindig pretty-printing
//!
//! Architecture: source -> rowan CST -> Doc IR -> layout -> formatted string
//!
//! The formatter walks the lossless concrete syntax tree produced by the parser,
//! builds an intermediate pretty-printing document (Doc), and then uses the
//! Wadler-Lindig algorithm to lay out the document within a configurable line width.

use crate::parse;
use crate::syntax::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};
use anyhow::Result;

// ============================================================================
// Configuration
// ============================================================================

/// Formatting options
pub struct FormatOptions {
    /// Maximum line width (default: 80)
    pub line_width: usize,
    /// Indentation width in spaces (default: 4)
    pub indent_width: i32,
}

impl Default for FormatOptions {
    fn default() -> Self {
        Self {
            line_width: 80,
            indent_width: 4,
        }
    }
}

// ============================================================================
// Pretty-printer document IR
// ============================================================================

#[derive(Clone, Debug)]
enum Doc {
    Nil,
    Text(String),
    HardLine,
    Line,
    LineOrEmpty,
    Concat(Vec<Doc>),
    Nest(i32, Box<Doc>),
    Group(Box<Doc>),
}

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

fn layout(width: usize, doc: &Doc) -> String {
    let mut output = String::new();
    let mut column: usize = 0;
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
                if mode != Mode::Flat {
                    output.push('\n');
                    let spaces = indent.max(0) as usize;
                    for _ in 0..spaces {
                        output.push(' ');
                    }
                    column = spaces;
                }
            }
            Doc::Concat(docs) => {
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
    /// Set of byte offsets (in the stripped CST) of comments that are standalone
    /// (on their own line in the original source, with only whitespace before them).
    standalone_comments: std::collections::HashSet<usize>,
}

const MAX_FORMAT_DEPTH: usize = 128;

impl Formatter {
    fn new(options: FormatOptions, root: &SyntaxNode, original_source: &str) -> Self {
        let standalone_comments = Self::classify_comments(root, original_source);
        Self {
            options,
            depth: std::cell::Cell::new(0),
            standalone_comments,
        }
    }

    /// Walk the CST tokens in document order and match them against the original
    /// source (skipping whitespace) to determine which comments are standalone.
    fn classify_comments(root: &SyntaxNode, source: &str) -> std::collections::HashSet<usize> {
        let mut result = std::collections::HashSet::new();
        let mut src_pos: usize = 0;

        fn visit(
            node: &SyntaxNode,
            source: &str,
            src_pos: &mut usize,
            result: &mut std::collections::HashSet<usize>,
        ) {
            for child in node.children_with_tokens() {
                match child {
                    SyntaxElement::Token(t) => {
                        let token_text = t.text();
                        // Skip whitespace in original source to find this token
                        while *src_pos < source.len() {
                            let remaining = &source[*src_pos..];
                            if remaining.starts_with(token_text) {
                                break;
                            }
                            // Advance past one character of whitespace
                            if let Some(ch) = remaining.chars().next() {
                                if ch.is_whitespace() {
                                    *src_pos += ch.len_utf8();
                                } else {
                                    // Unexpected non-whitespace; bail
                                    break;
                                }
                            } else {
                                break;
                            }
                        }

                        if t.kind() == SyntaxKind::Comment {
                            // Check if this line (up to src_pos) has only whitespace before //
                            let line_start = source[..*src_pos].rfind('\n').map_or(0, |p| p + 1);
                            let before = &source[line_start..*src_pos];
                            if before.chars().all(|c| c.is_whitespace()) {
                                let stripped_offset: usize = t.text_range().start().into();
                                result.insert(stripped_offset);
                            }
                        }

                        *src_pos += token_text.len();
                    }
                    SyntaxElement::Node(n) => visit(&n, source, src_pos, result),
                }
            }
        }

        visit(root, source, &mut src_pos, &mut result);
        result
    }

    fn format_node(&self, node: &SyntaxNode) -> Doc {
        let d = self.depth.get();
        if d >= MAX_FORMAT_DEPTH {
            return text(node.text().to_string());
        }
        self.depth.set(d + 1);
        let result = self.format_node_inner(node);
        self.depth.set(d);
        result
    }

    fn format_node_inner(&self, node: &SyntaxNode) -> Doc {
        use SyntaxKind::*;
        match node.kind() {
            SourceFile => self.fmt_source_file(node),

            EntityDecl => self.fmt_braced_block_with_body(node),
            ImplBlock => self.fmt_impl_block(node),
            StructDecl | EnumDecl | UnionDecl => self.fmt_braced_block_with_body(node),
            TraitDef | TraitImpl => self.fmt_braced_block_with_body(node),
            ModuleDecl | ProtocolDecl => self.fmt_braced_block_with_body(node),
            FunctionDecl => self.fmt_function_decl(node),
            TypeAlias | DistinctTypeDecl | UseDecl => self.fmt_inline(node),

            // Entity/struct/enum inner lists (no braces on them)
            PortList => self.fmt_item_list_newline(node),
            StructFieldList => self.fmt_item_list_newline(node),
            UnionFieldList => self.fmt_item_list_newline(node),
            EnumVariantList => self.fmt_item_list_comma(node),

            PortDecl | PortDirection => self.fmt_inline(node),
            GenericParamList | ArgList => self.fmt_angle_bracket_list(node),
            AttributeList => self.fmt_attribute_list(node),
            Attribute => self.fmt_inline(node),

            EventBlock => self.fmt_event_block(node),
            SignalDecl | VariableDecl | ConstantDecl => self.fmt_inline(node),

            IfStmt => self.fmt_if_stmt(node),
            MatchStmt => self.fmt_match_common(node),
            ForStmt => self.fmt_for_stmt(node),
            BlockStmt => self.fmt_block_stmt(node),
            LetStmt | AssignmentStmt | ExprStmt | ReturnStmt | FlowStmt | BarrierStmt => {
                self.fmt_inline(node)
            }

            GenerateForStmt | GenerateIfStmt | GenerateMatchStmt => self.fmt_generate_stmt(node),

            MatchArmList => self.fmt_match_arm_list(node),
            MatchArm => self.fmt_inline(node),

            MatchExpr => self.fmt_match_common(node),
            IfExpr => self.fmt_inline(node),
            InstanceDecl => self.fmt_instance_decl(node),

            ConnectionList => self.fmt_connection_list_items(node),
            TraitItemList => self.fmt_item_list_newline(node),

            SafetyGoalDecl
            | SafetyEntityDecl
            | SafetyTraitDecl
            | FmeaTraitDecl
            | HsiTraitDecl
            | FmedaLibraryDecl
            | FormalBlock
            | CovergroupDecl
            | GlobalConstraintBlock
            | PhysicalConstraintBlock => self.fmt_braced_block_with_body(node),

            AssertStmt | PropertyStmt | CoverStmt | SequenceStmt | AssumeStmt | ExpectStmt
            | ProveStmt | AssumeMacroStmt | CoverMacroStmt => self.fmt_inline(node),

            _ => self.fmt_inline(node),
        }
    }

    // ========================================================================
    // Helpers
    // ========================================================================

    fn children(&self, node: &SyntaxNode) -> Vec<SyntaxElement> {
        node.children_with_tokens()
            .filter(|child| !matches!(child.kind(), SyntaxKind::Whitespace))
            .collect()
    }

    fn fmt_token(&self, token: &SyntaxToken) -> Doc {
        if token.kind() == SyntaxKind::Comment {
            return text(token.text().trim_end());
        }
        text(token.text().to_string())
    }

    fn fmt_element(&self, elem: &SyntaxElement) -> Doc {
        match elem {
            SyntaxElement::Token(t) => self.fmt_token(t),
            SyntaxElement::Node(n) => self.format_node(n),
        }
    }

    fn last_token_kind(elem: &SyntaxElement) -> Option<SyntaxKind> {
        match elem {
            SyntaxElement::Token(t) => Some(t.kind()),
            SyntaxElement::Node(n) => n.last_token().map(|t| t.kind()),
        }
    }

    fn first_token_kind(elem: &SyntaxElement) -> Option<SyntaxKind> {
        match elem {
            SyntaxElement::Token(t) => Some(t.kind()),
            SyntaxElement::Node(n) => n.first_token().map(|t| t.kind()),
        }
    }

    /// Check if an ImplBlock node contains only ImplKw (parser split bug)
    fn is_impl_only_keyword(children: &[SyntaxElement]) -> bool {
        children.len() == 1 && children[0].kind() == SyntaxKind::ImplKw
    }

    /// Space decision that considers element kinds (not just token kinds).
    /// Suppresses space before GenericParamList/ArgList (angle brackets attached to name).
    fn should_space_between(left: &SyntaxElement, right: &SyntaxElement) -> bool {
        // No space before angle-bracket lists (generics: Adder<WIDTH>)
        if matches!(
            right.kind(),
            SyntaxKind::GenericParamList | SyntaxKind::ArgList
        ) {
            return false;
        }

        if let (Some(l), Some(r)) = (Self::last_token_kind(left), Self::first_token_kind(right)) {
            needs_space_between(l, r)
        } else {
            true
        }
    }

    /// Check if a comment token should be on its own line.
    /// Uses the pre-classified standalone_comments set.
    fn comment_on_own_line(&self, token: &SyntaxToken) -> bool {
        let offset: usize = token.text_range().start().into();
        self.standalone_comments.contains(&offset)
    }

    /// Emit any trailing children (typically comments) after index `i`.
    /// These are comments absorbed by the parser into a declaration node after RBrace.
    /// Adds a blank line before the comment block for visual separation.
    fn emit_trailing_comments(
        &self,
        children: &[SyntaxElement],
        start: usize,
        docs: &mut Vec<Doc>,
    ) {
        let mut first = true;
        for child in children.iter().skip(start) {
            if child.kind() == SyntaxKind::Comment {
                if first {
                    docs.push(hardline()); // blank line before comment block
                    first = false;
                }
                docs.push(hardline());
                docs.push(self.fmt_element(child));
            }
        }
    }

    // ========================================================================
    // Default inline formatter
    // ========================================================================

    fn fmt_inline(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let mut docs = Vec::new();
        let mut prev: Option<&SyntaxElement> = None;

        for child in &children {
            // Special handling for comments: check if they need a newline
            if child.kind() == SyntaxKind::Comment {
                if let SyntaxElement::Token(t) = child {
                    if self.comment_on_own_line(t) {
                        docs.push(hardline());
                        docs.push(self.fmt_token(t));
                        prev = Some(child);
                        continue;
                    }
                }
            }

            if let Some(p) = prev {
                if Self::should_space_between(p, child) {
                    docs.push(space());
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
        let mut i = 0;

        while i < children.len() {
            let child = &children[i];
            let kind = child.kind();

            // Handle split ImplBlock: parser sometimes creates two nodes,
            // first with only ImplKw, second with Ident + body
            if kind == SyntaxKind::ImplBlock {
                if let SyntaxElement::Node(impl_node) = child {
                    let impl_children = self.children(impl_node);
                    if Self::is_impl_only_keyword(&impl_children)
                        && i + 1 < children.len()
                        && children[i + 1].kind() == SyntaxKind::ImplBlock
                    {
                        // Merge: emit "impl" + space + second ImplBlock content
                        if !docs.is_empty() {
                            docs.push(hardline());
                            docs.push(hardline());
                        }
                        docs.push(text("impl"));
                        docs.push(space());
                        // Format the second ImplBlock (which has Ident, LBrace, body, RBrace)
                        if let SyntaxElement::Node(body_node) = &children[i + 1] {
                            docs.push(self.fmt_impl_body(body_node));
                        }
                        prev_kind = Some(SyntaxKind::ImplBlock);
                        i += 2;
                        continue;
                    }
                }
            }

            let is_use = kind == SyntaxKind::UseDecl;
            let prev_is_use = prev_kind == Some(SyntaxKind::UseDecl);
            let is_design_unit = is_design_unit_kind(kind);
            let prev_is_design = prev_kind.is_some_and(is_design_unit_kind);

            if !docs.is_empty() && kind != SyntaxKind::Comment {
                if is_use && prev_is_use {
                    docs.push(hardline());
                } else if is_design_unit || prev_is_design || (is_use && !prev_is_use) {
                    docs.push(hardline());
                    docs.push(hardline());
                } else {
                    docs.push(hardline());
                }
            } else if !docs.is_empty() {
                docs.push(hardline());
            }

            docs.push(self.fmt_element(child));
            if kind != SyntaxKind::Comment {
                prev_kind = Some(kind);
            }
            i += 1;
        }

        if !docs.is_empty() {
            docs.push(hardline());
        }

        concat(docs)
    }

    // ========================================================================
    // Angle bracket list: <items> (for GenericParamList, ArgList)
    // ========================================================================

    fn fmt_angle_bracket_list(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let mut docs = Vec::new();
        let mut prev_kind: Option<SyntaxKind> = None;

        for child in &children {
            let kind = child.kind();
            match kind {
                SyntaxKind::Lt => docs.push(text("<")),
                SyntaxKind::Gt => docs.push(text(">")),
                SyntaxKind::Comma => docs.push(text(",")),
                _ => {
                    // Space after comma, otherwise tight
                    if prev_kind == Some(SyntaxKind::Comma) {
                        docs.push(space());
                    }
                    docs.push(self.fmt_element(child));
                }
            }
            prev_kind = Some(kind);
        }

        concat(docs)
    }

    // ========================================================================
    // Braced block: header { body } where LBrace/RBrace are direct children
    // Used for entity, struct, enum, union, trait, module, protocol, etc.
    // ========================================================================

    fn fmt_braced_block_with_body(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let indent = self.options.indent_width;
        let mut docs = Vec::new();
        let mut i = 0;

        // Header (everything before LBrace)
        while i < children.len() && children[i].kind() != SyntaxKind::LBrace {
            if !docs.is_empty()
                && Self::should_space_between(&children[i - 1], &children[i]) {
                    docs.push(space());
                }
            docs.push(self.fmt_element(&children[i]));
            i += 1;
        }

        // { body }
        if i < children.len() && children[i].kind() == SyntaxKind::LBrace {
            docs.push(space());
            docs.push(text("{"));
            i += 1; // skip LBrace

            let mut body_docs = Vec::new();
            while i < children.len() && children[i].kind() != SyntaxKind::RBrace {
                body_docs.push(hardline());
                body_docs.push(self.fmt_element(&children[i]));
                i += 1;
            }
            if !body_docs.is_empty() {
                docs.push(nest(indent, concat(body_docs)));
            }
            docs.push(hardline());
            docs.push(text("}"));
            i += 1; // skip RBrace
        }

        // Trailing children after } (comments absorbed by parser)
        self.emit_trailing_comments(&children, i, &mut docs);

        concat(docs)
    }

    // ========================================================================
    // Impl block
    // ========================================================================

    fn fmt_impl_block(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);

        // If this is a split ImplBlock with only "impl", just emit "impl"
        // (source_file formatter handles merging, but handle standalone too)
        if Self::is_impl_only_keyword(&children) {
            return text("impl");
        }

        // Check if this ImplBlock has ImplKw (non-split case)
        let has_impl_kw = children.iter().any(|c| c.kind() == SyntaxKind::ImplKw);

        if has_impl_kw {
            // Normal case: impl Name { ... }
            self.fmt_impl_full(node)
        } else {
            // Split case: this is the body part (Ident, LBrace, body, RBrace)
            // When called standalone (not from source_file merger), prefix with "impl"
            concat(vec![text("impl"), space(), self.fmt_impl_body(node)])
        }
    }

    /// Format a complete impl block (has ImplKw)
    fn fmt_impl_full(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let mut docs = Vec::new();
        let mut i = 0;

        // Header: impl Name
        while i < children.len() && children[i].kind() != SyntaxKind::LBrace {
            if !docs.is_empty()
                && Self::should_space_between(&children[i - 1], &children[i]) {
                    docs.push(space());
                }
            docs.push(self.fmt_element(&children[i]));
            i += 1;
        }

        self.fmt_impl_body_from(&children, &mut i, &mut docs);
        concat(docs)
    }

    /// Format an impl body-only node (no ImplKw, has Ident, LBrace, body, RBrace)
    fn fmt_impl_body(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let mut docs = Vec::new();
        let mut i = 0;

        // Name (everything before LBrace)
        while i < children.len() && children[i].kind() != SyntaxKind::LBrace {
            if !docs.is_empty()
                && Self::should_space_between(&children[i - 1], &children[i]) {
                    docs.push(space());
                }
            docs.push(self.fmt_element(&children[i]));
            i += 1;
        }

        self.fmt_impl_body_from(&children, &mut i, &mut docs);
        concat(docs)
    }

    /// Shared: format { body } part of impl block with blank line handling
    fn fmt_impl_body_from(&self, children: &[SyntaxElement], i: &mut usize, docs: &mut Vec<Doc>) {
        let indent = self.options.indent_width;

        if *i < children.len() && children[*i].kind() == SyntaxKind::LBrace {
            docs.push(space());
            docs.push(text("{"));
            *i += 1;
        } else {
            return;
        }

        let mut body_docs = Vec::new();
        let mut prev_body_kind: Option<SyntaxKind> = None;

        while *i < children.len() && children[*i].kind() != SyntaxKind::RBrace {
            let kind = children[*i].kind();
            let is_block_item = is_block_level_kind(kind);
            let prev_was_block = prev_body_kind.is_some_and(is_block_level_kind);

            if !body_docs.is_empty() && (is_block_item || prev_was_block) {
                body_docs.push(hardline());
            }
            body_docs.push(hardline());
            body_docs.push(self.fmt_element(&children[*i]));
            if kind != SyntaxKind::Comment {
                prev_body_kind = Some(kind);
            }
            *i += 1;
        }

        if !body_docs.is_empty() {
            docs.push(nest(indent, concat(body_docs)));
        }
        docs.push(hardline());
        docs.push(text("}"));
        *i += 1; // skip RBrace

        // Trailing comments after }
        self.emit_trailing_comments(children, *i, docs);
    }

    // ========================================================================
    // Item lists (no braces — the braces are on the parent)
    // ========================================================================

    /// Format items one per line (for PortList, StructFieldList, UnionFieldList, TraitItemList)
    fn fmt_item_list_newline(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let mut item_docs: Vec<Doc> = Vec::new();

        for child in &children {
            match child.kind() {
                SyntaxKind::Semicolon => {} // skip separators
                _ => item_docs.push(self.fmt_element(child)),
            }
        }

        intersperse(hardline(), item_docs)
    }

    /// Format items comma-separated, one per line (for EnumVariantList)
    fn fmt_item_list_comma(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let mut item_docs: Vec<Doc> = Vec::new();

        for child in &children {
            match child.kind() {
                SyntaxKind::Comma => {} // skip — we'll add our own
                _ => item_docs.push(self.fmt_element(child)),
            }
        }

        intersperse(concat(vec![text(","), hardline()]), item_docs)
    }

    // ========================================================================
    // Event block: on(trigger) { stmts }
    // CST: OnKw, LParen, EventTriggerList, RParen, BlockStmt
    // ========================================================================

    fn fmt_event_block(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let mut docs = Vec::new();
        let mut i = 0;

        // "on"
        if i < children.len() && children[i].kind() == SyntaxKind::OnKw {
            docs.push(text("on"));
            i += 1;
        }

        // Trigger: ( EventTriggerList )
        if i < children.len() && children[i].kind() == SyntaxKind::LParen {
            docs.push(text("("));
            i += 1;
            while i < children.len()
                && children[i].kind() != SyntaxKind::RParen
                && children[i].kind() != SyntaxKind::BlockStmt
            {
                docs.push(self.fmt_element(&children[i]));
                i += 1;
            }
            if i < children.len() && children[i].kind() == SyntaxKind::RParen {
                docs.push(text(")"));
                i += 1;
            }
        }

        // Body: BlockStmt
        if i < children.len() && children[i].kind() == SyntaxKind::BlockStmt {
            docs.push(space());
            docs.push(self.fmt_block_stmt(children[i].as_node().unwrap()));
            i += 1;
        }

        // Trailing comments
        self.emit_trailing_comments(&children, i, &mut docs);

        concat(docs)
    }

    // ========================================================================
    // If statement
    // CST: IfKw, condition (ParenExpr or expr), BlockStmt, [ElseKw, BlockStmt/IfStmt]
    // ========================================================================

    fn fmt_if_stmt(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let mut docs = Vec::new();
        let mut i = 0;

        while i < children.len() {
            let kind = children[i].kind();
            match kind {
                SyntaxKind::IfKw => {
                    docs.push(text("if"));
                    i += 1;

                    // Condition (until BlockStmt)
                    let mut cond_docs = Vec::new();
                    while i < children.len()
                        && children[i].kind() != SyntaxKind::BlockStmt
                        && children[i].kind() != SyntaxKind::LBrace
                    {
                        if cond_docs.is_empty()
                            || Self::should_space_between(&children[i - 1], &children[i])
                        {
                            cond_docs.push(space());
                        }
                        cond_docs.push(self.fmt_element(&children[i]));
                        i += 1;
                    }
                    docs.push(concat(cond_docs));

                    // Body
                    if i < children.len() && children[i].kind() == SyntaxKind::BlockStmt {
                        docs.push(space());
                        docs.push(self.fmt_block_stmt(children[i].as_node().unwrap()));
                        i += 1;
                    }
                }
                SyntaxKind::ElseKw => {
                    docs.push(space());
                    docs.push(text("else"));
                    i += 1;

                    if i < children.len() && children[i].kind() == SyntaxKind::IfKw {
                        docs.push(space());
                        continue; // let IfKw branch handle it
                    }

                    if i < children.len() && children[i].kind() == SyntaxKind::BlockStmt {
                        docs.push(space());
                        docs.push(self.fmt_block_stmt(children[i].as_node().unwrap()));
                        i += 1;
                    } else if i < children.len() && children[i].kind() == SyntaxKind::IfStmt {
                        docs.push(space());
                        docs.push(self.format_node(children[i].as_node().unwrap()));
                        i += 1;
                    }
                }
                SyntaxKind::Comment => {
                    // Trailing comment on if/else block
                    if let SyntaxElement::Token(t) = &children[i] {
                        if self.comment_on_own_line(t) {
                            docs.push(hardline());
                        } else {
                            docs.push(space());
                        }
                        docs.push(self.fmt_token(t));
                    }
                    i += 1;
                }
                _ => {
                    i += 1;
                }
            }
        }

        concat(docs)
    }

    // ========================================================================
    // Match statement / expression
    // ========================================================================

    fn fmt_match_common(&self, node: &SyntaxNode) -> Doc {
        // CST: MatchKw, expr..., LBrace, MatchArmList, RBrace [, Semicolon]
        let children = self.children(node);
        let indent = self.options.indent_width;
        let mut docs = Vec::new();
        let mut i = 0;

        if i < children.len() && children[i].kind() == SyntaxKind::MatchKw {
            docs.push(text("match"));
            i += 1;
        }

        // Expression (until LBrace)
        while i < children.len() && children[i].kind() != SyntaxKind::LBrace {
            docs.push(space());
            docs.push(self.fmt_element(&children[i]));
            i += 1;
        }

        // { MatchArmList }
        if i < children.len() && children[i].kind() == SyntaxKind::LBrace {
            docs.push(space());
            docs.push(text("{"));
            i += 1; // skip LBrace

            if i < children.len() && children[i].kind() == SyntaxKind::MatchArmList {
                let arm_doc = self.fmt_match_arm_list(children[i].as_node().unwrap());
                docs.push(nest(indent, concat(vec![hardline(), arm_doc])));
                i += 1;
            }

            docs.push(hardline());
            docs.push(text("}"));
            if i < children.len() && children[i].kind() == SyntaxKind::RBrace {
                i += 1; // skip RBrace
            }
        }

        // Trailing semicolon (if present as direct child of MatchExpr/MatchStmt)
        if i < children.len() && children[i].kind() == SyntaxKind::Semicolon {
            docs.push(text(";"));
            i += 1;
        }

        // Trailing comments after } or ;
        self.emit_trailing_comments(&children, i, &mut docs);

        concat(docs)
    }

    /// MatchArmList has no braces (braces are on parent MatchExpr/MatchStmt).
    /// Commas are inside MatchArm nodes, not on MatchArmList.
    fn fmt_match_arm_list(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let mut arm_docs: Vec<Doc> = Vec::new();

        for child in &children {
            arm_docs.push(self.fmt_element(child));
        }

        intersperse(hardline(), arm_docs)
    }

    // ========================================================================
    // For statement
    // ========================================================================

    fn fmt_for_stmt(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let mut docs = Vec::new();
        let mut i = 0;

        // Header: for i in range
        while i < children.len()
            && children[i].kind() != SyntaxKind::BlockStmt
            && children[i].kind() != SyntaxKind::LBrace
        {
            if !docs.is_empty()
                && Self::should_space_between(&children[i - 1], &children[i]) {
                    docs.push(space());
                }
            docs.push(self.fmt_element(&children[i]));
            i += 1;
        }

        if i < children.len() && children[i].kind() == SyntaxKind::BlockStmt {
            docs.push(space());
            docs.push(self.fmt_block_stmt(children[i].as_node().unwrap()));
            i += 1;
        }

        self.emit_trailing_comments(&children, i, &mut docs);

        concat(docs)
    }

    // ========================================================================
    // Block statement: { stmts }
    // CST: LBrace, statements..., RBrace
    // ========================================================================

    fn fmt_block_stmt(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let indent = self.options.indent_width;
        let mut docs = Vec::new();
        docs.push(text("{"));

        let mut stmt_docs = Vec::new();
        for child in &children {
            match child.kind() {
                SyntaxKind::LBrace | SyntaxKind::RBrace => {}
                _ => {
                    stmt_docs.push(hardline());
                    stmt_docs.push(self.fmt_element(child));
                }
            }
        }
        if !stmt_docs.is_empty() {
            docs.push(nest(indent, concat(stmt_docs)));
        }
        docs.push(hardline());
        docs.push(text("}"));

        concat(docs)
    }

    // ========================================================================
    // Instance declaration: let name = Entity<G> { connections }
    // ========================================================================

    fn fmt_instance_decl(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let indent = self.options.indent_width;
        let mut docs = Vec::new();
        let mut i = 0;

        // Header: let name = Entity<Args>
        while i < children.len() && children[i].kind() != SyntaxKind::LBrace {
            if !docs.is_empty()
                && Self::should_space_between(&children[i - 1], &children[i]) {
                    docs.push(space());
                }
            docs.push(self.fmt_element(&children[i]));
            i += 1;
        }

        // { ConnectionList }
        // LBrace, ConnectionList, RBrace are separate direct children of InstanceDecl
        if i < children.len() && children[i].kind() == SyntaxKind::LBrace {
            docs.push(space());
            docs.push(text("{"));
            i += 1; // skip LBrace

            // Collect connection items from ConnectionList (if present)
            let mut conn_docs: Vec<Doc> = Vec::new();
            while i < children.len() && children[i].kind() != SyntaxKind::RBrace {
                if children[i].kind() == SyntaxKind::ConnectionList {
                    if let SyntaxElement::Node(cl) = &children[i] {
                        let cl_children = self.children(cl);
                        for cc in &cl_children {
                            match cc.kind() {
                                SyntaxKind::Comma => {} // skip — we add our own
                                _ => conn_docs.push(self.fmt_element(cc)),
                            }
                        }
                    }
                } else if children[i].kind() == SyntaxKind::Comment {
                    conn_docs.push(self.fmt_element(&children[i]));
                }
                i += 1;
            }

            if !conn_docs.is_empty() {
                let inner = intersperse(concat(vec![text(","), hardline()]), conn_docs);
                docs.push(nest(indent, concat(vec![hardline(), inner])));
            }
            docs.push(hardline());
            docs.push(text("}"));
            i += 1; // skip RBrace
        }

        // Trailing comments after }
        self.emit_trailing_comments(&children, i, &mut docs);

        concat(docs)
    }

    /// Format ConnectionList items (no braces — braces are on InstanceDecl).
    fn fmt_connection_list_items(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let mut conn_docs: Vec<Doc> = Vec::new();

        for child in &children {
            match child.kind() {
                SyntaxKind::Comma => {} // skip — handled by parent
                _ => conn_docs.push(self.fmt_element(child)),
            }
        }

        intersperse(concat(vec![text(","), space()]), conn_docs)
    }

    // ========================================================================
    // Function declaration
    // ========================================================================

    fn fmt_function_decl(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let mut docs = Vec::new();
        let mut i = 0;

        // Header: fn name(params) -> RetType
        while i < children.len()
            && children[i].kind() != SyntaxKind::LBrace
            && children[i].kind() != SyntaxKind::BlockStmt
        {
            if !docs.is_empty()
                && Self::should_space_between(&children[i - 1], &children[i]) {
                    docs.push(space());
                }
            docs.push(self.fmt_element(&children[i]));
            i += 1;
        }

        if i < children.len() && children[i].kind() == SyntaxKind::BlockStmt {
            docs.push(space());
            docs.push(self.fmt_block_stmt(children[i].as_node().unwrap()));
            i += 1;
        } else if i < children.len() && children[i].kind() == SyntaxKind::LBrace {
            // Fallback: direct braces
            docs.push(space());
            docs.push(self.fmt_braced_block_inline(&children, &mut i));
        }

        self.emit_trailing_comments(&children, i, &mut docs);

        concat(docs)
    }

    // ========================================================================
    // Generate statements
    // ========================================================================

    fn fmt_generate_stmt(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let mut docs = Vec::new();
        let mut i = 0;

        while i < children.len()
            && children[i].kind() != SyntaxKind::LBrace
            && children[i].kind() != SyntaxKind::BlockStmt
        {
            if !docs.is_empty()
                && Self::should_space_between(&children[i - 1], &children[i]) {
                    docs.push(space());
                }
            docs.push(self.fmt_element(&children[i]));
            i += 1;
        }

        if i < children.len() && children[i].kind() == SyntaxKind::BlockStmt {
            docs.push(space());
            docs.push(self.fmt_block_stmt(children[i].as_node().unwrap()));
            i += 1;
        } else if i < children.len() && children[i].kind() == SyntaxKind::LBrace {
            docs.push(space());
            docs.push(self.fmt_braced_block_inline(&children, &mut i));
        }

        self.emit_trailing_comments(&children, i, &mut docs);

        concat(docs)
    }

    // ========================================================================
    // Attribute list
    // ========================================================================

    fn fmt_attribute_list(&self, node: &SyntaxNode) -> Doc {
        let children = self.children(node);
        let mut attr_docs: Vec<Doc> = Vec::new();
        for child in &children {
            attr_docs.push(self.fmt_element(child));
        }
        intersperse(hardline(), attr_docs)
    }

    // ========================================================================
    // Helper: format { body } inline from children slice at LBrace position
    // ========================================================================

    fn fmt_braced_block_inline(&self, children: &[SyntaxElement], i: &mut usize) -> Doc {
        let indent = self.options.indent_width;
        let mut docs = Vec::new();
        docs.push(text("{"));
        *i += 1;

        let mut body_docs = Vec::new();
        while *i < children.len() && children[*i].kind() != SyntaxKind::RBrace {
            body_docs.push(hardline());
            body_docs.push(self.fmt_element(&children[*i]));
            *i += 1;
        }
        if !body_docs.is_empty() {
            docs.push(nest(indent, concat(body_docs)));
        }
        docs.push(hardline());
        docs.push(text("}"));
        if *i < children.len() {
            *i += 1;
        }
        concat(docs)
    }
}

// ============================================================================
// Token classification helpers
// ============================================================================

fn is_name_like(kind: SyntaxKind) -> bool {
    use SyntaxKind::*;
    matches!(
        kind,
        Ident
            | IntLiteral
            | BinLiteral
            | HexLiteral
            | FloatLiteral
            | StringLiteral
            | RParen
            | RBracket
            | RBrace
            | TrueKw
            | FalseKw
            | SelfKw
            | SelfTypeKw
    )
}

fn is_design_unit_kind(kind: SyntaxKind) -> bool {
    use SyntaxKind::*;
    matches!(
        kind,
        EntityDecl
            | ImplBlock
            | StructDecl
            | EnumDecl
            | UnionDecl
            | TraitDef
            | TraitImpl
            | TypeAlias
            | DistinctTypeDecl
            | ModuleDecl
            | ProtocolDecl
            | FunctionDecl
            | SafetyGoalDecl
            | SafetyEntityDecl
            | SafetyTraitDecl
            | FmeaTraitDecl
            | HsiTraitDecl
            | FmedaLibraryDecl
            | FormalBlock
            | CovergroupDecl
            | GlobalConstraintBlock
            | PhysicalConstraintBlock
    )
}

fn is_block_level_kind(kind: SyntaxKind) -> bool {
    use SyntaxKind::*;
    matches!(
        kind,
        EventBlock
            | FunctionDecl
            | IfStmt
            | MatchStmt
            | ForStmt
            | FormalBlock
            | CovergroupDecl
            | GenerateForStmt
            | GenerateIfStmt
            | GenerateMatchStmt
    )
}

fn needs_space_between(left: SyntaxKind, right: SyntaxKind) -> bool {
    use SyntaxKind::*;

    // No space before ';', ','
    if matches!(right, Semicolon | Comma) {
        return false;
    }

    // No space after '(' or '[' or before ')' or ']'
    if matches!(left, LParen | LBracket) || matches!(right, RParen | RBracket) {
        return false;
    }

    // No space around '.' (field access)
    if left == Dot || right == Dot {
        return false;
    }

    // No space around '::' (path separator)
    if left == ColonColon || right == ColonColon {
        return false;
    }

    // No space before '(' after identifier/keyword (fn calls, on(...))
    if right == LParen && (is_name_like(left) || left == OnKw) {
        return false;
    }

    // No space before '[' after identifier or type keyword (indexing, width specs)
    if right == LBracket && (is_name_like(left) || is_type_keyword(left)) {
        return false;
    }

    // No space inside #[ (attributes)
    if left == HashBracket {
        return false;
    }

    // No space after unary '!', '~' (but != needs space)
    if matches!(left, Bang | Tilde) && !matches!(right, Assign | Eq) {
        return false;
    }

    // Space around '=' (assignment)
    if left == Assign || right == Assign {
        return true;
    }

    // Space around '=>' (fat arrow)
    if left == FatArrow || right == FatArrow {
        return true;
    }

    // Space around '->' (arrow)
    if left == Arrow || right == Arrow {
        return true;
    }

    // Space around '|>' (pipeline)
    if left == Pipeline || right == Pipeline {
        return true;
    }

    // Space around binary operators
    if matches!(
        left,
        Plus | Minus
            | Star
            | Slash
            | Percent
            | Amp
            | Pipe
            | Caret
            | AmpAmp
            | PipePipe
            | Shl
            | Shr
            | Eq
            | Neq
            | Lt
            | Gt
            | Le
            | Ge
            | WidenAdd
    ) {
        return true;
    }
    if matches!(
        right,
        Plus | Minus
            | Star
            | Slash
            | Percent
            | Amp
            | Pipe
            | Caret
            | AmpAmp
            | PipePipe
            | Shl
            | Shr
            | Eq
            | Neq
            | Lt
            | Gt
            | Le
            | Ge
            | WidenAdd
    ) {
        return true;
    }

    // No space before ':'
    if right == Colon {
        return false;
    }
    // Space after ':' when followed by type name (keyword or identifier)
    if left == Colon && (right.is_keyword() || right == Ident) {
        return true;
    }
    // No space after ':' otherwise (e.g. bit slices 31:0)
    if left == Colon {
        return false;
    }

    // No space around '..' / '..='
    if matches!(left, DotDot | DotDotEq) || matches!(right, DotDot | DotDotEq) {
        return false;
    }

    // Space between word-like tokens
    if (left.is_keyword() || is_name_like(left)) && (right.is_keyword() || is_name_like(right)) {
        return true;
    }

    // Space after keyword before most tokens
    if left.is_keyword()
        && !matches!(
            right,
            Semicolon | Comma | RParen | RBracket | LParen | Dot | ColonColon | LBracket
        )
    {
        return true;
    }

    // Space before keyword after name-like tokens
    if right.is_keyword() && is_name_like(left) {
        return true;
    }

    true
}

fn is_type_keyword(kind: SyntaxKind) -> bool {
    use SyntaxKind::*;
    matches!(kind, BitKw | BoolKw | NatKw | IntKw | LogicKw | StringKw)
}

// ============================================================================
// Public API
// ============================================================================

/// Format skalp source code with default options
pub fn format_skalp(source: &str) -> Result<String> {
    format_skalp_with_options(source, &FormatOptions::default())
}

/// Format skalp source code with custom options
pub fn format_skalp_with_options(source: &str, options: &FormatOptions) -> Result<String> {
    let (root, errors) = parse::parse_with_errors(source);

    if !errors.is_empty() {
        let msgs: Vec<String> = errors.iter().map(|e| format!("{:?}", e)).collect();
        anyhow::bail!(
            "Cannot format skalp source with parse errors: {}",
            msgs.join("; ")
        );
    }

    let formatter = Formatter::new(
        FormatOptions {
            line_width: options.line_width,
            indent_width: options.indent_width,
        },
        &root,
        source,
    );

    let doc = formatter.format_node(&root);
    let result = layout(options.line_width, &doc);

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_simple_entity() {
        let source =
            "entity Counter {\n    in clk: clock\n    in rst: reset\n    out count: nat[8]\n}\n";
        let result = format_skalp(source).unwrap();
        assert!(result.contains("entity Counter {"), "result:\n{}", result);
        assert!(result.contains("in clk: clock"), "result:\n{}", result);
        assert!(result.contains("out count: nat[8]"), "result:\n{}", result);
    }

    #[test]
    fn test_format_impl_block() {
        let source = r#"entity Counter {
    in clk: clock
    in rst: reset
    out count: nat[8]
}

impl Counter {
    signal counter: nat[8] = 0

    on(clk.rise) {
        if (rst) {
            counter = 0
        } else {
            counter = counter + 1
        }
    }

    count = counter
}
"#;
        let result = format_skalp(source).unwrap();
        assert!(result.contains("impl Counter {"), "result:\n{}", result);
        assert!(
            result.contains("signal counter: nat[8]"),
            "result:\n{}",
            result
        );
        assert!(result.contains("on(clk.rise) {"), "result:\n{}", result);
    }

    #[test]
    fn test_format_match_expression() {
        let source = r#"entity ALU {
    in a: bit[32]
    in b: bit[32]
    in op: bit[3]
    in clk: clock
    out result: bit[32]
}

impl ALU {
    signal result_comb: bit[32]

    result_comb = match op {
        0b000 => a + b,
        0b001 => a - b,
        _ => 0
    };

    on(clk.rise) {
        result = result_comb
    }
}
"#;
        let result = format_skalp(source).unwrap();
        assert!(result.contains("match op"), "result:\n{}", result);
        assert!(result.contains("=> a + b"), "result:\n{}", result);
    }

    #[test]
    fn test_format_if_else() {
        let source = r#"entity Test {
    in clk: clock
    in rst: reset
    out val: bit[8]
}

impl Test {
    on(clk.rise) {
        if (rst) {
            val = 0
        } else {
            val = 1
        }
    }
}
"#;
        let result = format_skalp(source).unwrap();
        assert!(result.contains("if"), "result:\n{}", result);
        assert!(result.contains("} else {"), "result:\n{}", result);
    }

    #[test]
    fn test_format_struct_enum() {
        let source = r#"struct PacketHeader {
    src_addr: nat[32]
    dst_addr: nat[32]
    length: nat[16]
}

enum State {
    Idle,
    Processing,
    Done,
    Error
}
"#;
        let result = format_skalp(source).unwrap();
        assert!(
            result.contains("struct PacketHeader {"),
            "result:\n{}",
            result
        );
        assert!(
            result.contains("    src_addr: nat[32]"),
            "result:\n{}",
            result
        );
        assert!(result.contains("enum State {"), "result:\n{}", result);
    }

    #[test]
    fn test_format_generic_entity() {
        let source = r#"entity FIFO<const WIDTH: nat = 8, const DEPTH: nat = 16> {
    in clk: clock
    in rst: reset(active_high)
    in wr_en: bit
    in wr_data: bit[WIDTH]
    out full: bit
}
"#;
        let result = format_skalp(source).unwrap();
        assert!(result.contains("entity FIFO"), "result:\n{}", result);
        assert!(result.contains("WIDTH"), "result:\n{}", result);
    }

    #[test]
    fn test_format_idempotent() {
        let source = r#"entity Counter {
    in clk: clock
    in rst: reset
    out count: nat[8]
}

impl Counter {
    signal counter: nat[8] = 0

    on(clk.rise) {
        if (rst) {
            counter = 0
        } else {
            counter = counter + 1
        }
    }

    count = counter
}
"#;
        let first = format_skalp(source).unwrap();
        let second = format_skalp(&first).unwrap();
        assert_eq!(
            first, second,
            "formatter is not idempotent.\nFirst:\n{}\nSecond:\n{}",
            first, second
        );
    }

    #[test]
    fn test_format_comments_preserved() {
        let source = "// This is a counter entity\nentity Counter {\n    in clk: clock\n    out count: nat[8]\n}\n";
        let result = format_skalp(source).unwrap();
        assert!(
            result.contains("// This is a counter entity"),
            "result:\n{}",
            result
        );
    }

    #[test]
    fn test_format_roundtrip_counter() {
        let source = std::fs::read_to_string(
            std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                .parent()
                .unwrap()
                .parent()
                .unwrap()
                .join("examples/counter.sk"),
        )
        .unwrap();
        let result = format_skalp(&source).unwrap();
        let (_, errors) = crate::parse::parse_with_errors(&result);
        assert!(
            errors.is_empty(),
            "formatted output has parse errors: {:?}\nformatted:\n{}",
            errors,
            result
        );
    }

    #[test]
    fn test_format_roundtrip_advanced_types() {
        let source = std::fs::read_to_string(
            std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                .parent()
                .unwrap()
                .parent()
                .unwrap()
                .join("examples/advanced_types.sk"),
        )
        .unwrap();
        let result = format_skalp(&source).unwrap();
        let (_, errors) = crate::parse::parse_with_errors(&result);
        assert!(
            errors.is_empty(),
            "formatted output has parse errors: {:?}\nformatted:\n{}",
            errors,
            result
        );
    }

    #[test]
    fn test_format_comment_own_line_in_body() {
        let source =
            "impl X {\n    signal s: bit[8]\n\n    // Comment on own line\n    signal t: bit\n}\n";
        let result = format_skalp(source).unwrap();
        // Comment should be on its own line, not merged with previous signal
        assert!(
            result.contains("bit[8]\n") && result.contains("// Comment on own line\n"),
            "Comment should be on own line.\nresult:\n{}",
            result
        );
        assert!(
            !result.contains("bit[8] // Comment"),
            "Comment should NOT be on same line as signal.\nresult:\n{}",
            result
        );
    }

    #[test]
    fn test_format_trailing_comment_same_line() {
        let source = "entity A {\n    out lt: bit  // less than\n    out eq: bit  // equal\n}\n";
        let result = format_skalp(source).unwrap();
        // Trailing comment on same line should stay on same line
        assert!(
            result.contains("bit // less than"),
            "Trailing comment should stay on same line.\nresult:\n{}",
            result
        );
    }

    #[test]
    fn test_format_roundtrip_hierarchical_alu() {
        let source = std::fs::read_to_string(
            std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                .parent()
                .unwrap()
                .parent()
                .unwrap()
                .join("examples/hierarchical_alu.sk"),
        )
        .unwrap();
        let result = format_skalp(&source).unwrap();
        let (_, errors) = crate::parse::parse_with_errors(&result);
        assert!(
            errors.is_empty(),
            "formatted output has parse errors: {:?}\nformatted:\n{}",
            errors,
            result
        );
        // Verify idempotency
        let result2 = format_skalp(&result).unwrap();
        assert_eq!(
            result, result2,
            "formatter is not idempotent on hierarchical_alu.sk.\nFirst:\n{}\nSecond:\n{}",
            result, result2
        );
    }
}
