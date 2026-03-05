use anyhow::Result;
use indexmap::IndexMap;
use skalp_frontend::hir::*;
use skalp_frontend::safety_attributes::ModuleSafetyDefinitions;
use std::collections::HashSet;

use crate::builtins::BuiltinScope;
use crate::diagnostics::{VhdlError, VhdlErrorKind, VhdlSeverity};
use crate::syntax::{SyntaxElement, SyntaxKind, SyntaxNode};
use crate::vhdl_types::*;

/// Info about an async reset detected from VHDL process body pattern.
struct AsyncResetInfo {
    active_high: bool,
}

/// Snapshot of a generic package's declarations for instantiation
#[derive(Debug, Clone)]
struct PackageScope {
    generic_type_params: Vec<String>,
    types: IndexMap<String, HirType>,
    constants: Vec<HirConstant>,
    functions: Vec<HirFunction>,
}

pub struct VhdlHirBuilder {
    next_entity_id: u32,
    next_port_id: u32,
    next_signal_id: u32,
    next_variable_id: u32,
    next_constant_id: u32,
    next_block_id: u32,
    next_assignment_id: u32,
    next_instance_id: u32,
    next_for_loop_id: u32,
    next_function_id: u32,

    builtin_scope: BuiltinScope,
    function_map: IndexMap<String, FunctionId>,
    user_types: IndexMap<String, HirType>,
    /// Maps view_name -> (interface_name, field_name -> direction)
    view_defs: IndexMap<String, (String, IndexMap<String, HirPortDirection>)>,
    entity_map: IndexMap<String, EntityId>,
    /// Maps entity_id -> generics from the entity declaration
    entity_generics: IndexMap<EntityId, Vec<HirGeneric>>,
    port_map: IndexMap<String, PortId>,
    signal_map: IndexMap<String, SignalId>,
    variable_map: IndexMap<String, VariableId>,
    constant_map: IndexMap<String, ConstantId>,

    /// Maps signal/port/variable names to their types for attribute resolution ('high, 'low, etc.)
    name_type_map: IndexMap<String, HirType>,

    /// Known package names (for qualified name resolution like pkg.constant)
    package_names: HashSet<String>,
    /// Current generic type parameter names (for HirType::Custom resolution)
    generic_type_params: HashSet<String>,
    /// Saved generic package scopes for instantiation
    package_scopes: IndexMap<String, PackageScope>,

    inside_event_block: bool,

    /// Signals created from external name expressions (<< signal .path : type >>)
    external_signals: Vec<HirSignal>,

    /// Clock frequency attributes: maps signal name → frequency in Hz
    /// From: attribute clock_frequency of clk : signal is 100_000_000;
    clock_frequency_attrs: IndexMap<String, u64>,

    errors: Vec<VhdlError>,
    file_path: Option<std::path::PathBuf>,
}

// ========================================================================
// Helper: extract children and tokens from rowan nodes
// ========================================================================

fn child_nodes(node: &SyntaxNode) -> Vec<SyntaxNode> {
    node.children().collect()
}

fn child_tokens_of_kind(node: &SyntaxNode, kind: SyntaxKind) -> Vec<String> {
    node.children_with_tokens()
        .filter_map(|el| match el {
            SyntaxElement::Token(t) if t.kind() == kind => Some(t.text().to_string()),
            _ => None,
        })
        .collect()
}

fn first_token_text(node: &SyntaxNode, kind: SyntaxKind) -> Option<String> {
    node.children_with_tokens().find_map(|el| match el {
        SyntaxElement::Token(t) if t.kind() == kind => Some(t.text().to_string()),
        _ => None,
    })
}

fn first_child_of_kind(node: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxNode> {
    node.children().find(|c| c.kind() == kind)
}

fn all_children_of_kind(node: &SyntaxNode, kind: SyntaxKind) -> Vec<SyntaxNode> {
    node.children().filter(|c| c.kind() == kind).collect()
}

/// Get all ident texts from a node (direct children only)
fn ident_texts(node: &SyntaxNode) -> Vec<String> {
    child_tokens_of_kind(node, SyntaxKind::Ident)
}

/// Get the first ident text
fn first_ident(node: &SyntaxNode) -> Option<String> {
    first_token_text(node, SyntaxKind::Ident)
}

/// Get the first ident text, also looking inside child `Name` nodes.
/// Needed because the parser wraps identifiers in `Name` nodes after
/// `parse_selected_name()` / `parse_name()` calls.
fn first_ident_recursive(node: &SyntaxNode) -> Option<String> {
    // Check direct Ident tokens first
    if let Some(id) = first_token_text(node, SyntaxKind::Ident) {
        return Some(id);
    }
    // Fall back to Ident inside a child Name node
    for child in node.children() {
        if child.kind() == SyntaxKind::Name {
            if let Some(id) = first_token_text(&child, SyntaxKind::Ident) {
                return Some(id);
            }
        }
    }
    None
}

/// Check if a token of given kind exists as direct child
fn has_token(node: &SyntaxNode, kind: SyntaxKind) -> bool {
    node.children_with_tokens()
        .any(|el| matches!(el, SyntaxElement::Token(ref t) if t.kind() == kind))
}

/// Extract the instance label from preceding siblings of a ComponentInst node.
/// In VHDL: `u_sender: entity work.sender port map(...)`, the label "u_sender"
/// and colon are siblings before the ComponentInst node in the parent's children.
fn extract_instance_label(node: &SyntaxNode) -> Option<String> {
    // Walk backward through preceding siblings to find Ident:Colon pattern
    let mut prev = node.prev_sibling_or_token();
    // Skip whitespace/trivia
    while let Some(ref p) = prev {
        if p.kind() != SyntaxKind::Whitespace && p.kind() != SyntaxKind::Comment {
            break;
        }
        prev = p.prev_sibling_or_token();
    }
    // Expect Colon
    if prev.as_ref().map(|p| p.kind()) != Some(SyntaxKind::Colon) {
        return None;
    }
    prev = prev.unwrap().prev_sibling_or_token();
    // Skip whitespace again
    while let Some(ref p) = prev {
        if p.kind() != SyntaxKind::Whitespace && p.kind() != SyntaxKind::Comment {
            break;
        }
        prev = p.prev_sibling_or_token();
    }
    // Expect Ident (the label)
    prev.and_then(|p| {
        if p.kind() == SyntaxKind::Ident {
            p.into_token().map(|t| t.text().to_string())
        } else {
            None
        }
    })
}

/// Collect leading comments from preceding siblings of a CST node.
/// Returns comment text with the `--` prefix stripped.
fn collect_leading_comments(node: &SyntaxNode) -> Vec<String> {
    let mut comments = Vec::new();
    let mut prev = node.prev_sibling_or_token();
    while let Some(ref element) = prev {
        match element {
            SyntaxElement::Token(tok) if tok.kind() == SyntaxKind::Whitespace => {
                prev = element.prev_sibling_or_token();
            }
            SyntaxElement::Token(tok) if tok.kind() == SyntaxKind::Comment => {
                let text = tok.text().to_string();
                let stripped = text.strip_prefix("--").unwrap_or(&text).trim().to_string();
                comments.push(stripped);
                prev = element.prev_sibling_or_token();
            }
            _ => break,
        }
    }
    comments.reverse();
    comments
}

/// Collect trailing comment from a PortDecl node.
/// Checks both:
/// 1. Trailing children of the PortDecl itself (comments consumed by skip_trivia inside parse_port_decl)
/// 2. Next siblings of the PortDecl (comments between ports, after semicolons)
fn collect_trailing_comment(node: &SyntaxNode) -> Option<String> {
    // First check trailing tokens within the node itself (last-to-first)
    for element in node
        .children_with_tokens()
        .collect::<Vec<_>>()
        .into_iter()
        .rev()
    {
        match &element {
            SyntaxElement::Token(tok) if tok.kind() == SyntaxKind::Comment => {
                return Some(tok.text().to_string());
            }
            SyntaxElement::Token(tok) if tok.kind() == SyntaxKind::Whitespace => {
                // Skip whitespace, keep looking
                continue;
            }
            _ => break, // Stop at first non-trivia
        }
    }

    // Then check next siblings (for comments after semicolons between ports)
    let mut next = node.next_sibling_or_token();
    while let Some(ref element) = next {
        match element {
            SyntaxElement::Token(tok) if tok.kind() == SyntaxKind::Whitespace => {
                // Only continue on same-line whitespace
                if tok.text().contains('\n') {
                    return None;
                }
                next = element.next_sibling_or_token();
            }
            SyntaxElement::Token(tok) if tok.kind() == SyntaxKind::Semicolon => {
                next = element.next_sibling_or_token();
            }
            SyntaxElement::Token(tok) if tok.kind() == SyntaxKind::Comment => {
                return Some(tok.text().to_string());
            }
            _ => return None,
        }
    }
    None
}

/// Parse a `-- skalp: { key: value, ... }` comment into PhysicalConstraints.
/// Returns None if the comment doesn't start with `skalp:`.
fn extract_skalp_constraint_from_comment(comment_text: &str) -> Option<PhysicalConstraints> {
    // Strip `--` prefix and whitespace
    let stripped = comment_text.strip_prefix("--")?.trim();

    // Check for `skalp:` prefix
    let body = stripped.strip_prefix("skalp:")?.trim();

    // Extract content between `{` and `}`
    let inner = body.strip_prefix('{')?.trim();
    let inner = inner.strip_suffix('}')?.trim();

    if inner.is_empty() {
        return None;
    }

    let mut pin_location = None;
    let mut io_standard = None;
    let mut drive_strength = None;
    let mut slew_rate = None;
    let mut termination = None;
    let mut schmitt_trigger = None;
    let mut bank = None;
    let mut diff_term = None;
    let mut pad_type = None;
    let mut pad_cell = None;

    // Parse comma-separated key: value pairs
    // Handle pins: ["A1", "A2"] by tracking bracket nesting
    let pairs = split_constraint_pairs(inner);
    for pair in &pairs {
        let pair = pair.trim();
        if let Some((key, value)) = pair.split_once(':') {
            let key = key.trim();
            let value = value.trim();
            // Strip surrounding quotes from value if present
            let unquoted = value
                .strip_prefix('"')
                .and_then(|v| v.strip_suffix('"'))
                .unwrap_or(value);

            match key {
                "pin" => {
                    pin_location = Some(PinLocation::Single(unquoted.to_string()));
                }
                "pins" => {
                    // Parse ["A1", "A2", ...] array
                    let arr = value
                        .strip_prefix('[')
                        .and_then(|v| v.strip_suffix(']'))
                        .unwrap_or(value);
                    let pins: Vec<String> = arr
                        .split(',')
                        .map(|s| {
                            s.trim()
                                .strip_prefix('"')
                                .and_then(|v| v.strip_suffix('"'))
                                .unwrap_or(s.trim())
                                .to_string()
                        })
                        .collect();
                    pin_location = Some(PinLocation::Multiple(pins));
                }
                "pin_p" => {
                    // Differential positive — set or update
                    let neg = match &pin_location {
                        Some(PinLocation::Differential { negative, .. }) => negative.clone(),
                        _ => String::new(),
                    };
                    pin_location = Some(PinLocation::Differential {
                        positive: unquoted.to_string(),
                        negative: neg,
                    });
                }
                "pin_n" => {
                    // Differential negative — set or update
                    let pos = match &pin_location {
                        Some(PinLocation::Differential { positive, .. }) => positive.clone(),
                        _ => String::new(),
                    };
                    pin_location = Some(PinLocation::Differential {
                        positive: pos,
                        negative: unquoted.to_string(),
                    });
                }
                "io_standard" => {
                    io_standard = Some(unquoted.to_string());
                }
                "drive" => {
                    drive_strength = match unquoted.to_ascii_lowercase().as_str() {
                        "4ma" => Some(DriveStrength::Ma4),
                        "8ma" => Some(DriveStrength::Ma8),
                        "12ma" => Some(DriveStrength::Ma12),
                        "16ma" => Some(DriveStrength::Ma16),
                        _ => None,
                    };
                }
                "slew" => {
                    slew_rate = match unquoted.to_ascii_lowercase().as_str() {
                        "fast" => Some(SlewRate::Fast),
                        "slow" => Some(SlewRate::Slow),
                        "medium" => Some(SlewRate::Medium),
                        _ => None,
                    };
                }
                "pull" => {
                    termination = match unquoted.to_ascii_lowercase().as_str() {
                        "up" => Some(Termination::PullUp),
                        "down" => Some(Termination::PullDown),
                        "none" => Some(Termination::None),
                        "keeper" => Some(Termination::Keeper),
                        _ => None,
                    };
                }
                "schmitt" => {
                    schmitt_trigger = Some(matches!(
                        unquoted.to_ascii_lowercase().as_str(),
                        "true" | "yes" | "1"
                    ));
                }
                "diff_term" => {
                    diff_term = Some(matches!(
                        unquoted.to_ascii_lowercase().as_str(),
                        "true" | "yes" | "1"
                    ));
                }
                "bank" => {
                    bank = unquoted.parse::<u32>().ok();
                }
                "pad_type" => {
                    pad_type = match unquoted.to_ascii_lowercase().as_str() {
                        "input" => Some(PadType::Input),
                        "output" => Some(PadType::Output),
                        "bidirectional" | "bidir" => Some(PadType::Bidirectional),
                        "clock" => Some(PadType::Clock),
                        "power" => Some(PadType::Power),
                        "ground" => Some(PadType::Ground),
                        "analog" => Some(PadType::Analog),
                        _ => None,
                    };
                }
                "pad_cell" => {
                    pad_cell = Some(unquoted.to_string());
                }
                _ => {} // Unknown keys are silently ignored
            }
        }
    }

    Some(PhysicalConstraints {
        pin_location,
        io_standard,
        drive_strength,
        slew_rate,
        termination,
        schmitt_trigger,
        bank,
        diff_term,
        pad_type,
        pad_cell,
        ldo_config: None,
    })
}

/// Split constraint pairs by commas, respecting bracket nesting for array values like `["A1", "A2"]`.
fn split_constraint_pairs(s: &str) -> Vec<String> {
    let mut pairs = Vec::new();
    let mut current = String::new();
    let mut bracket_depth = 0;

    for ch in s.chars() {
        match ch {
            '[' => {
                bracket_depth += 1;
                current.push(ch);
            }
            ']' => {
                bracket_depth -= 1;
                current.push(ch);
            }
            ',' if bracket_depth == 0 => {
                pairs.push(std::mem::take(&mut current));
            }
            _ => current.push(ch),
        }
    }
    if !current.is_empty() {
        pairs.push(current);
    }
    pairs
}

/// Collect all non-trivia token texts
fn all_token_texts(node: &SyntaxNode) -> Vec<(SyntaxKind, String)> {
    node.children_with_tokens()
        .filter_map(|el| match el {
            SyntaxElement::Token(t)
                if t.kind() != SyntaxKind::Whitespace && t.kind() != SyntaxKind::Comment =>
            {
                Some((t.kind(), t.text().to_string()))
            }
            _ => None,
        })
        .collect()
}

fn to_pascal_case(name: &str) -> String {
    name.split('_')
        .map(|part| {
            let mut chars: Vec<char> = part.chars().collect();
            if !chars.is_empty() {
                chars[0] = chars[0].to_ascii_uppercase();
            }
            chars.into_iter().collect::<String>()
        })
        .collect()
}

impl VhdlHirBuilder {
    pub fn new(file_path: Option<&std::path::Path>) -> Self {
        Self {
            next_entity_id: 0,
            next_port_id: 0,
            next_signal_id: 0,
            next_variable_id: 0,
            next_constant_id: 0,
            next_block_id: 0,
            next_assignment_id: 0,
            next_instance_id: 0,
            next_for_loop_id: 0,
            next_function_id: 0,
            builtin_scope: BuiltinScope::new(),
            function_map: IndexMap::new(),
            user_types: IndexMap::new(),
            view_defs: IndexMap::new(),
            entity_map: IndexMap::new(),
            entity_generics: IndexMap::new(),
            port_map: IndexMap::new(),
            signal_map: IndexMap::new(),
            variable_map: IndexMap::new(),
            constant_map: IndexMap::new(),
            name_type_map: IndexMap::new(),
            package_names: HashSet::new(),
            generic_type_params: HashSet::new(),
            package_scopes: IndexMap::new(),
            inside_event_block: false,
            external_signals: Vec::new(),
            clock_frequency_attrs: IndexMap::new(),
            errors: Vec::new(),
            file_path: file_path.map(|p| p.to_path_buf()),
        }
    }

    pub fn diagnostics(&self) -> &[VhdlError] {
        &self.errors
    }

    fn emit_warning_at(&mut self, msg: impl Into<String>, node: Option<&SyntaxNode>) {
        let message = msg.into();
        let (position, end_position) = node
            .map(|n| {
                (
                    usize::from(n.text_range().start()),
                    usize::from(n.text_range().end()),
                )
            })
            .unwrap_or((0, 0));
        self.errors.push(VhdlError {
            kind: VhdlErrorKind::LoweringError(message.clone()),
            message,
            position,
            end_position,
            severity: VhdlSeverity::Warning,
        });
    }

    fn emit_error_at(&mut self, msg: impl Into<String>, node: Option<&SyntaxNode>) {
        let message = msg.into();
        let (position, end_position) = node
            .map(|n| {
                (
                    usize::from(n.text_range().start()),
                    usize::from(n.text_range().end()),
                )
            })
            .unwrap_or((0, 0));
        self.errors.push(VhdlError {
            kind: VhdlErrorKind::LoweringError(message.clone()),
            message,
            position,
            end_position,
            severity: VhdlSeverity::Error,
        });
    }

    fn alloc_entity_id(&mut self) -> EntityId {
        let id = EntityId(self.next_entity_id);
        self.next_entity_id += 1;
        id
    }
    fn alloc_port_id(&mut self) -> PortId {
        let id = PortId(self.next_port_id);
        self.next_port_id += 1;
        id
    }
    fn alloc_signal_id(&mut self) -> SignalId {
        let id = SignalId(self.next_signal_id);
        self.next_signal_id += 1;
        id
    }
    fn alloc_variable_id(&mut self) -> VariableId {
        let id = VariableId(self.next_variable_id);
        self.next_variable_id += 1;
        id
    }
    fn alloc_constant_id(&mut self) -> ConstantId {
        let id = ConstantId(self.next_constant_id);
        self.next_constant_id += 1;
        id
    }
    fn alloc_block_id(&mut self) -> BlockId {
        let id = BlockId(self.next_block_id);
        self.next_block_id += 1;
        id
    }
    fn alloc_assignment_id(&mut self) -> AssignmentId {
        let id = AssignmentId(self.next_assignment_id);
        self.next_assignment_id += 1;
        id
    }
    fn alloc_instance_id(&mut self) -> InstanceId {
        let id = InstanceId(self.next_instance_id);
        self.next_instance_id += 1;
        id
    }
    fn alloc_for_loop_id(&mut self) -> ForLoopId {
        let id = ForLoopId(self.next_for_loop_id);
        self.next_for_loop_id += 1;
        id
    }
    fn alloc_function_id(&mut self) -> FunctionId {
        let id = FunctionId(self.next_function_id);
        self.next_function_id += 1;
        id
    }

    // ====================================================================
    // Top-level lowering
    // ====================================================================

    pub fn lower(&mut self, root: &SyntaxNode) -> Result<Hir> {
        let file_name = self
            .file_path
            .as_ref()
            .and_then(|p| p.file_stem())
            .and_then(|s| s.to_str())
            .unwrap_or("vhdl_design")
            .to_string();

        self.lower_multiple_roots(file_name, &[root.clone()])
    }

    /// Lower multiple VHDL syntax roots (from different files) into a single HIR.
    /// Two-pass: first collect all entity names across files, then full lowering.
    pub fn lower_multiple_roots(
        &mut self,
        design_name: String,
        roots: &[SyntaxNode],
    ) -> Result<Hir> {
        let mut hir = Hir::new(design_name);

        // First pass: collect entity names from ALL roots for forward references
        for root in roots {
            for child in root.children() {
                if child.kind() == SyntaxKind::EntityDecl {
                    if let Some(name) = first_ident(&child) {
                        let id = self.alloc_entity_id();
                        let pascal = to_pascal_case(&name);
                        self.entity_map.insert(name.clone(), id);
                        self.entity_map.insert(pascal, id);
                    }
                    // Reset ID counter — we'll re-allocate in the real pass
                    self.next_entity_id -= 1;
                }
            }
        }
        self.next_entity_id = 0;
        self.entity_map.clear();

        // Second pass: full lowering across all roots
        for root in roots {
            self.lower_root_into(root, &mut hir);
        }

        Ok(hir)
    }

    /// Lower a single root node into an existing HIR (used by both lower() and lower_multiple_roots())
    fn lower_root_into(&mut self, root: &SyntaxNode, hir: &mut Hir) {
        for child in root.children() {
            match child.kind() {
                SyntaxKind::LibraryClause => self.lower_library_clause(&child),
                SyntaxKind::UseClause => self.lower_use_clause(&child),
                SyntaxKind::EntityDecl => {
                    if let Some(entity) = self.lower_entity_decl(&child) {
                        hir.entities.push(entity);
                    }
                }
                SyntaxKind::ArchitectureBody => {
                    if let Some(imp) = self.lower_architecture(&child) {
                        // Apply clock_frequency attributes to the entity's clock domains
                        if !self.clock_frequency_attrs.is_empty() {
                            self.apply_clock_frequency_attrs(imp.entity, hir);
                            self.clock_frequency_attrs.clear();
                        }
                        hir.implementations.push(imp);
                    }
                }
                SyntaxKind::InterfaceDecl => {
                    self.lower_interface_decl(&child);
                }
                SyntaxKind::ViewDecl => {
                    self.lower_view_decl(&child);
                }
                SyntaxKind::PackageDecl | SyntaxKind::PackageBody => {
                    self.lower_package(&child, hir);
                }
                SyntaxKind::PackageInstantiation => {
                    self.lower_package_instantiation(&child, hir);
                }
                _ => {}
            }
        }
    }

    // ====================================================================
    // Library/Use
    // ====================================================================

    fn lower_library_clause(&mut self, _node: &SyntaxNode) {
        // Library clause: just note it for context
    }

    fn lower_use_clause(&mut self, node: &SyntaxNode) {
        // Extract the selected name text (e.g., "ieee.std_logic_1164.all")
        if let Some(sel_name) = first_child_of_kind(node, SyntaxKind::SelectedName) {
            let parts: Vec<String> = sel_name
                .children_with_tokens()
                .filter_map(|el| match el {
                    SyntaxElement::Token(t)
                        if t.kind() != SyntaxKind::Whitespace
                            && t.kind() != SyntaxKind::Comment
                            && t.kind() != SyntaxKind::Dot =>
                    {
                        Some(t.text().to_string())
                    }
                    _ => None,
                })
                .collect();
            let full_name = parts.join(".");
            self.builtin_scope.register_use(&full_name);
        }
    }

    // ====================================================================
    // Entity
    // ====================================================================

    /// Apply collected clock_frequency attributes to an entity's clock domains.
    ///
    /// For each signal with a clock_frequency attribute, finds or creates a clock domain
    /// on the entity and sets its frequency_hz.
    fn apply_clock_frequency_attrs(&self, entity_id: EntityId, hir: &mut Hir) {
        if let Some(entity) = hir.entities.iter_mut().find(|e| e.id == entity_id) {
            for (signal_name, freq_hz) in &self.clock_frequency_attrs {
                // Check if there's already a clock domain for this signal
                let existing = entity
                    .clock_domains
                    .iter_mut()
                    .find(|cd| cd.name == *signal_name);
                if let Some(cd) = existing {
                    cd.frequency_hz = Some(*freq_hz);
                } else {
                    // Create a new clock domain for this signal
                    let domain_id = ClockDomainId(entity.clock_domains.len() as u32);
                    entity.clock_domains.push(HirClockDomain {
                        id: domain_id,
                        name: signal_name.clone(),
                        frequency_hz: Some(*freq_hz),
                    });
                }
            }
        }
    }

    /// Try to parse a `clock_frequency` attribute specification.
    ///
    /// Recognizes: `attribute clock_frequency of <signal> : signal is <value>;`
    /// Stores the frequency in `self.clock_frequency_attrs` for later use
    /// when building clock domains.
    fn try_parse_clock_frequency_attr(&mut self, node: &SyntaxNode) {
        let tokens = all_token_texts(node);
        // Expected pattern: attribute clock_frequency of <target> : signal is <value> ;
        // Token indices:     0         1                2  3        4 5      6  7       8
        if tokens.len() < 8 {
            return;
        }

        // tokens[0] = "attribute", tokens[1] = attribute name
        let attr_name = tokens[1].1.to_ascii_lowercase();
        if attr_name != "clock_frequency" {
            return;
        }

        // tokens[2] = "of", tokens[3] = target signal name
        if !tokens[2].1.eq_ignore_ascii_case("of") {
            return;
        }
        let target_signal = tokens[3].1.to_ascii_lowercase();

        // Find "is" keyword and the value after it
        let is_pos = tokens
            .iter()
            .position(|(_, text)| text.eq_ignore_ascii_case("is"));
        if let Some(is_idx) = is_pos {
            if let Some((_, value_text)) = tokens.get(is_idx + 1) {
                // Parse the frequency value (may contain underscores)
                let cleaned = value_text.replace('_', "");
                if let Ok(freq_hz) = cleaned.parse::<u64>() {
                    self.clock_frequency_attrs.insert(target_signal, freq_hz);
                }
            }
        }
    }

    fn lower_entity_decl(&mut self, node: &SyntaxNode) -> Option<HirEntity> {
        // VHDL is case-insensitive: normalize entity name to lowercase
        let name = first_ident(node)?.to_ascii_lowercase();
        let pascal_name = to_pascal_case(&name);
        let id = self.alloc_entity_id();
        self.entity_map.insert(name.clone(), id);
        self.entity_map.insert(pascal_name.clone(), id);

        // Clear port map for this entity
        self.port_map.clear();

        let mut ports = Vec::new();
        let mut generics = Vec::new();

        // Process generic clause
        if let Some(gc) = first_child_of_kind(node, SyntaxKind::GenericClause) {
            for gd in all_children_of_kind(&gc, SyntaxKind::GenericDecl) {
                if let Some(g) = self.lower_generic_decl(&gd) {
                    generics.push(g);
                }
            }
        }

        // Store generics for injection into architectures
        self.entity_generics.insert(id, generics.clone());

        // Process port clause
        if let Some(pc) = first_child_of_kind(node, SyntaxKind::PortClause) {
            for pd in all_children_of_kind(&pc, SyntaxKind::PortDecl) {
                ports.extend(self.lower_port_decl(&pd));
            }
        }

        Some(HirEntity {
            id,
            name: pascal_name,
            comments: collect_leading_comments(node),
            is_async: false,
            visibility: HirVisibility::Public,
            ports,
            generics,
            clock_domains: Vec::new(),
            assignments: Vec::new(),
            signals: Vec::new(),
            span: None,
            pipeline_config: None,
            vendor_ip_config: None,
            power_domains: Vec::new(),
            power_domain_config: None,
            safety_mechanism_config: None,
            seooc_config: None,
            compiled_ip_config: None,
        })
    }

    fn lower_generic_decl(&mut self, node: &SyntaxNode) -> Option<HirGeneric> {
        // Check for generic type parameter: GenericDecl with TypeKw as child
        if has_token(node, SyntaxKind::TypeKw) {
            let name = first_ident(node)?;
            self.generic_type_params.insert(name.clone());

            // Check for type bounds: `is (<>)` and/or `range <>`
            let has_is = has_token(node, SyntaxKind::IsKw);
            let has_box = has_token(node, SyntaxKind::BoxOp);
            let has_range = has_token(node, SyntaxKind::RangeKw);

            let param_type = if has_is && has_box {
                let mut bounds = vec!["discrete".to_string()];
                if has_range {
                    bounds.push("range".to_string());
                }
                HirGenericType::TypeWithBounds(bounds)
            } else {
                HirGenericType::Type
            };

            return Some(HirGeneric {
                name,
                param_type,
                default_value: None,
            });
        }

        let idents = ident_texts(node);
        let name = idents.first()?.clone();

        // Determine generic type from the subtype indication
        let subtype = first_child_of_kind(node, SyntaxKind::SubtypeIndication);
        let param_type = if let Some(ref st) = subtype {
            let type_name = self.subtype_indication_type_name(st);
            match type_name.as_str() {
                "integer" | "natural" | "positive" => HirGenericType::Const(HirType::Nat(32)),
                "boolean" => HirGenericType::Const(HirType::Bool),
                _ => HirGenericType::Const(HirType::Nat(32)),
            }
        } else {
            HirGenericType::Const(HirType::Nat(32))
        };

        // Check for default value
        let default_value = self.extract_default_value(node);

        Some(HirGeneric {
            name,
            param_type,
            default_value,
        })
    }

    fn lower_port_decl(&mut self, node: &SyntaxNode) -> Vec<HirPort> {
        // VHDL is case-insensitive: normalize all identifiers to lowercase
        let idents: Vec<String> = ident_texts(node)
            .into_iter()
            .map(|s| s.to_ascii_lowercase())
            .collect();
        if idents.is_empty() {
            return Vec::new();
        }

        // Check for view port: PortDecl contains ViewKw token (no PortDirection child)
        if has_token(node, SyntaxKind::ViewKw) {
            return self.lower_view_port_decl(node, &idents);
        }

        // Extract direction from PortDirection child
        let direction = if let Some(dir_node) = first_child_of_kind(node, SyntaxKind::PortDirection)
        {
            if has_token(&dir_node, SyntaxKind::InKw) {
                HirPortDirection::Input
            } else if has_token(&dir_node, SyntaxKind::OutKw) {
                HirPortDirection::Output
            } else if has_token(&dir_node, SyntaxKind::InoutKw)
                || has_token(&dir_node, SyntaxKind::BufferKw)
            {
                HirPortDirection::Bidirectional
            } else {
                HirPortDirection::Input
            }
        } else {
            HirPortDirection::Input
        };

        // Extract type
        let port_type = if let Some(st) = first_child_of_kind(node, SyntaxKind::SubtypeIndication) {
            self.lower_subtype_indication(&st)
        } else {
            let name_hint = idents.first().map(|s| s.as_str()).unwrap_or("?");
            self.emit_warning_at(
                format!(
                    "missing type for port '{}', defaulting to std_logic",
                    name_hint
                ),
                Some(node),
            );
            HirType::Logic(1)
        };

        // Extract physical constraints from trailing skalp comment
        let physical_constraints = collect_trailing_comment(node)
            .and_then(|comment| extract_skalp_constraint_from_comment(&comment));

        let mut ports = Vec::new();
        for name in &idents {
            let id = self.alloc_port_id();
            self.port_map.insert(name.clone(), id);
            self.name_type_map.insert(name.clone(), port_type.clone());
            ports.push(HirPort {
                id,
                name: name.clone(),
                comments: collect_leading_comments(node),
                direction: direction.clone(),
                port_type: port_type.clone(),
                physical_constraints: physical_constraints.clone(),
                detection_config: None,
                power_domain_config: None,
                isolation_config: None,
                retention_config: None,
            });
        }
        ports
    }

    /// Lower a view port declaration by flattening interface fields into individual ports.
    /// `idents` contains: [port_name, view_name] (both extracted as Ident tokens)
    fn lower_view_port_decl(&mut self, _node: &SyntaxNode, idents: &[String]) -> Vec<HirPort> {
        // idents[0] = port name (e.g., "bus"), idents[1] = view name (e.g., "axi_master")
        let port_name = match idents.first() {
            Some(n) => n.clone(),
            None => return Vec::new(),
        };
        let view_name = match idents.get(1) {
            Some(n) => n.clone(),
            None => return Vec::new(),
        };

        // Look up view definition
        let (interface_name, direction_map) = match self.view_defs.get(&view_name) {
            Some(v) => v.clone(),
            None => return Vec::new(),
        };

        // Look up interface struct type to get field names and types
        let fields = match self.user_types.get(&interface_name) {
            Some(HirType::Struct(st)) => st.fields.clone(),
            _ => return Vec::new(),
        };

        // Flatten: create one HirPort per interface field
        let mut ports = Vec::new();
        for field in &fields {
            let flat_name = format!("{}_{}", port_name, field.name);
            let direction = direction_map.get(&field.name).cloned().unwrap_or_else(|| {
                self.emit_warning_at(
                    format!(
                        "no direction for view field '{}', defaulting to in",
                        field.name
                    ),
                    None,
                );
                HirPortDirection::Input
            });

            let id = self.alloc_port_id();
            self.port_map.insert(flat_name.clone(), id);
            self.name_type_map
                .insert(flat_name.clone(), field.field_type.clone());
            ports.push(HirPort {
                id,
                name: flat_name,
                comments: vec![],
                direction,
                port_type: field.field_type.clone(),
                physical_constraints: None,
                detection_config: None,
                power_domain_config: None,
                isolation_config: None,
                retention_config: None,
            });
        }
        ports
    }

    // ====================================================================
    // Architecture -> HirImplementation
    // ====================================================================

    fn lower_architecture(&mut self, node: &SyntaxNode) -> Option<HirImplementation> {
        // Architecture idents: arch_name, entity_name
        let idents = ident_texts(node);
        let entity_name = idents.get(1).cloned().unwrap_or_default();
        let pascal_entity = to_pascal_case(&entity_name);

        let entity_id = match self
            .entity_map
            .get(&entity_name)
            .or_else(|| self.entity_map.get(&pascal_entity))
            .copied()
        {
            Some(id) => id,
            None => {
                self.emit_error_at(
                    format!("architecture references unknown entity: '{}'", entity_name),
                    Some(node),
                );
                EntityId(0)
            }
        };

        // Clear signal/variable maps for this architecture
        self.signal_map.clear();
        self.variable_map.clear();
        self.constant_map.clear();

        let mut signals = Vec::new();
        let mut variables = Vec::new();
        let mut constants = Vec::new();
        let mut functions = Vec::new();
        let mut event_blocks = Vec::new();
        let mut assignments = Vec::new();
        let mut instances = Vec::new();
        let mut generate_stmts = Vec::new();

        // Inject entity generics as constants so they're resolvable via resolve_name()
        if let Some(generics) = self.entity_generics.get(&entity_id).cloned() {
            for generic in &generics {
                if let HirGenericType::Const(ref const_type) = generic.param_type {
                    let id = self.alloc_constant_id();
                    self.constant_map
                        .insert(generic.name.to_ascii_lowercase(), id);
                    let default = generic
                        .default_value
                        .clone()
                        .unwrap_or(HirExpression::Literal(HirLiteral::Integer(0)));
                    constants.push(HirConstant {
                        id,
                        name: generic.name.to_ascii_lowercase(),
                        comments: vec![],
                        const_type: const_type.clone(),
                        value: default,
                    });
                }
            }
        }

        // Process declarations
        for child in node.children() {
            match child.kind() {
                SyntaxKind::SignalDecl => {
                    signals.extend(self.lower_signal_decl(&child));
                }
                SyntaxKind::ConstantDecl => {
                    if let Some(c) = self.lower_constant_decl(&child) {
                        constants.push(c);
                    }
                }
                SyntaxKind::VariableDecl => {
                    if let Some(v) = self.lower_variable_decl(&child) {
                        variables.push(v);
                    }
                }
                SyntaxKind::TypeDecl => {
                    self.lower_type_decl(&child);
                }
                SyntaxKind::SubtypeDecl => {
                    self.lower_subtype_decl(&child);
                }
                SyntaxKind::AliasDecl => {
                    self.lower_alias_decl(&child);
                }
                SyntaxKind::FunctionBody => {
                    if let Some(f) = self.lower_function(&child) {
                        functions.push(f);
                    }
                }
                SyntaxKind::ProcedureBody => {
                    if let Some(f) = self.lower_procedure(&child) {
                        functions.push(f);
                    }
                }
                SyntaxKind::FunctionDecl | SyntaxKind::ProcedureDecl => {
                    // Forward declarations — ignore
                }
                SyntaxKind::AttributeDecl => {
                    // Attribute declarations — ignore
                }
                SyntaxKind::AttributeSpec => {
                    // Check for clock_frequency attribute specification
                    self.try_parse_clock_frequency_attr(&child);
                }
                SyntaxKind::ComponentDecl => {
                    // Component declarations — handled at instantiation
                }
                _ => {}
            }
        }

        // Process concurrent statements
        for child in node.children() {
            match child.kind() {
                SyntaxKind::ProcessStmt => {
                    let (eb, proc_vars) = self.lower_process(&child);
                    if let Some(eb) = eb {
                        event_blocks.push(eb);
                    }
                    variables.extend(proc_vars);
                }
                SyntaxKind::ConcurrentSignalAssign => {
                    if let Some(a) = self.lower_concurrent_assign(&child) {
                        assignments.push(a);
                    }
                }
                SyntaxKind::ComponentInst => {
                    if let Some(inst) = self.lower_component_inst(&child) {
                        instances.push(inst);
                    }
                }
                SyntaxKind::SelectedAssign => {
                    if let Some(a) = self.lower_selected_assign(&child) {
                        assignments.push(a);
                    }
                }
                SyntaxKind::ForGenerate => {
                    if let Some(g) = self.lower_for_generate(&child) {
                        generate_stmts.push(HirStatement::GenerateFor(g));
                    }
                }
                SyntaxKind::IfGenerate => {
                    if let Some(g) = self.lower_if_generate(&child) {
                        generate_stmts.push(HirStatement::GenerateIf(g));
                    }
                }
                SyntaxKind::BlockStmt => {
                    // Flatten block statement into parent scope
                    self.lower_block_stmt_into(
                        &child,
                        &mut signals,
                        &mut variables,
                        &mut constants,
                        &mut event_blocks,
                        &mut assignments,
                        &mut instances,
                    );
                }
                _ => {}
            }
        }

        // Merge any external name signals created during expression lowering
        signals.append(&mut self.external_signals);

        let arch_name = idents
            .first()
            .cloned()
            .unwrap_or_default()
            .to_ascii_lowercase();

        Some(HirImplementation {
            name: Some(arch_name),
            entity: entity_id,
            signals,
            variables,
            constants,
            functions,
            event_blocks,
            assignments,
            instances,
            covergroups: Vec::new(),
            formal_blocks: Vec::new(),
            statements: generate_stmts,
        })
    }

    // ====================================================================
    // Signal/Variable/Constant declarations
    // ====================================================================

    fn lower_signal_decl(&mut self, node: &SyntaxNode) -> Vec<HirSignal> {
        // VHDL is case-insensitive: normalize signal names to lowercase
        let idents: Vec<String> = ident_texts(node)
            .into_iter()
            .map(|s| s.to_ascii_lowercase())
            .collect();
        let signal_type = if let Some(st) = first_child_of_kind(node, SyntaxKind::SubtypeIndication)
        {
            self.lower_subtype_indication(&st)
        } else {
            let name_hint = idents.first().map(|s| s.as_str()).unwrap_or("?");
            self.emit_warning_at(
                format!(
                    "missing type for signal '{}', defaulting to std_logic",
                    name_hint
                ),
                Some(node),
            );
            HirType::Logic(1)
        };

        let init = self.extract_default_value(node);

        let mut signals = Vec::new();
        for name in &idents {
            let id = self.alloc_signal_id();
            self.signal_map.insert(name.clone(), id);
            self.name_type_map.insert(name.clone(), signal_type.clone());
            signals.push(HirSignal {
                id,
                name: name.clone(),
                comments: collect_leading_comments(node),
                signal_type: signal_type.clone(),
                initial_value: init.clone(),
                clock_domain: None,
                span: None,
                memory_config: None,
                trace_config: None,
                cdc_config: None,
                breakpoint_config: None,
                power_config: None,
                safety_config: None,
                power_domain: None,
            });
        }
        signals
    }

    fn lower_variable_decl(&mut self, node: &SyntaxNode) -> Option<HirVariable> {
        let name = first_ident(node)?;
        let var_type = if let Some(st) = first_child_of_kind(node, SyntaxKind::SubtypeIndication) {
            self.lower_subtype_indication(&st)
        } else {
            self.emit_warning_at(
                format!(
                    "missing type for variable '{}', defaulting to std_logic",
                    name
                ),
                Some(node),
            );
            HirType::Logic(1)
        };
        let init = self.extract_default_value(node);
        let id = self.alloc_variable_id();
        self.variable_map.insert(name.clone(), id);
        self.name_type_map.insert(name.clone(), var_type.clone());
        Some(HirVariable {
            id,
            name,
            var_type,
            initial_value: init,
            span: None,
            comments: collect_leading_comments(node),
        })
    }

    fn lower_constant_decl(&mut self, node: &SyntaxNode) -> Option<HirConstant> {
        let name = first_ident(node)?;
        let const_type = if let Some(st) = first_child_of_kind(node, SyntaxKind::SubtypeIndication)
        {
            self.lower_subtype_indication(&st)
        } else {
            self.emit_warning_at(
                format!("no type for constant '{}', defaulting to natural(32)", name),
                Some(node),
            );
            HirType::Nat(32)
        };
        let value = self.extract_default_value(node).unwrap_or_else(|| {
            self.emit_warning_at(
                format!("no default value for constant '{}', using 0", name),
                Some(node),
            );
            HirExpression::Literal(HirLiteral::Integer(0))
        });
        let id = self.alloc_constant_id();
        self.constant_map.insert(name.clone(), id);
        Some(HirConstant {
            id,
            name,
            comments: collect_leading_comments(node),
            const_type,
            value,
        })
    }

    fn lower_type_decl(&mut self, node: &SyntaxNode) {
        let name = match first_ident(node) {
            Some(n) => n,
            None => return,
        };

        // Enum type
        if let Some(enum_def) = first_child_of_kind(node, SyntaxKind::EnumTypeDef) {
            let variants = ident_texts(&enum_def);
            if !variants.is_empty() {
                let ty = make_enum_type(&name, &variants);
                self.user_types.insert(name, ty);
            }
            return;
        }

        // Record type
        if let Some(rec_def) = first_child_of_kind(node, SyntaxKind::RecordTypeDef) {
            let mut fields = Vec::new();
            for field_node in all_children_of_kind(&rec_def, SyntaxKind::RecordField) {
                let field_names = ident_texts(&field_node);
                let field_type = if let Some(st) =
                    first_child_of_kind(&field_node, SyntaxKind::SubtypeIndication)
                {
                    self.lower_subtype_indication(&st)
                } else {
                    let fn_hint = field_names.first().map(|s| s.as_str()).unwrap_or("?");
                    self.emit_warning_at(
                        format!(
                            "missing type for record field '{}', defaulting to std_logic",
                            fn_hint
                        ),
                        Some(&field_node),
                    );
                    HirType::Logic(1)
                };
                for fn_name in &field_names {
                    fields.push((fn_name.clone(), field_type.clone()));
                }
            }
            let ty = make_struct_type(&name, &fields);
            self.user_types.insert(name, ty);
            return;
        }

        // Array type
        if let Some(arr_def) = first_child_of_kind(node, SyntaxKind::ArrayTypeDef) {
            let elem_type =
                if let Some(st) = first_child_of_kind(&arr_def, SyntaxKind::SubtypeIndication) {
                    self.lower_subtype_indication(&st)
                } else {
                    self.emit_warning_at(
                        format!(
                            "missing element type for array type '{}', defaulting to std_logic",
                            name
                        ),
                        Some(node),
                    );
                    HirType::Logic(1)
                };
            // Collect all discrete ranges (supports multi-dimensional arrays)
            let ranges = all_children_of_kind(&arr_def, SyntaxKind::DiscreteRange);
            let sizes: Vec<u32> = ranges
                .iter()
                .map(|dr| self.extract_range_size(dr).unwrap_or(1))
                .collect();
            // Build nested array type: innermost dimension last
            // For array(0 to 3, 0 to 7) of T → Array(Array(T, 8), 4)
            let ty = sizes
                .iter()
                .rev()
                .fold(elem_type, |inner, &sz| make_array_type(inner, sz));
            self.user_types.insert(name, ty);
        }
    }

    // ====================================================================
    // Function/Procedure lowering
    // ====================================================================

    fn lower_function(&mut self, node: &SyntaxNode) -> Option<HirFunction> {
        let name = first_ident(node)?;
        let id = self.alloc_function_id();
        self.function_map.insert(name.clone(), id);

        let is_pure = !has_token(node, SyntaxKind::ImpureKw);
        let params = self.lower_param_list(node);
        let return_type = self.extract_return_type(node);

        // Lower body statements
        let body = self.lower_sequential_statements(node);

        Some(HirFunction {
            id,
            is_const: is_pure,
            name,
            generics: Vec::new(),
            params,
            return_type: Some(return_type),
            body,
            span: None,
            pipeline_config: None,
            comments: collect_leading_comments(node),
        })
    }

    fn lower_procedure(&mut self, node: &SyntaxNode) -> Option<HirFunction> {
        let name = first_ident(node)?;
        let id = self.alloc_function_id();
        self.function_map.insert(name.clone(), id);

        let params = self.lower_param_list(node);
        let body = self.lower_sequential_statements(node);

        Some(HirFunction {
            id,
            is_const: false,
            name,
            generics: Vec::new(),
            params,
            return_type: None,
            body,
            span: None,
            pipeline_config: None,
            comments: collect_leading_comments(node),
        })
    }

    fn lower_param_list(&mut self, node: &SyntaxNode) -> Vec<HirParameter> {
        let mut params = Vec::new();
        if let Some(pl) = first_child_of_kind(node, SyntaxKind::ParamList) {
            for pd in all_children_of_kind(&pl, SyntaxKind::ParamDecl) {
                params.extend(self.lower_param_decl(&pd));
            }
        }
        params
    }

    fn lower_param_decl(&mut self, node: &SyntaxNode) -> Vec<HirParameter> {
        let idents = ident_texts(node);
        let param_type = if let Some(st) = first_child_of_kind(node, SyntaxKind::SubtypeIndication)
        {
            self.lower_subtype_indication(&st)
        } else {
            let name_hint = idents.first().map(|s| s.as_str()).unwrap_or("?");
            self.emit_warning_at(
                format!(
                    "missing type for parameter '{}', defaulting to std_logic",
                    name_hint
                ),
                Some(node),
            );
            HirType::Logic(1)
        };
        let default_value = self.extract_default_value(node);

        idents
            .into_iter()
            .map(|name| HirParameter {
                name,
                param_type: param_type.clone(),
                default_value: default_value.clone(),
            })
            .collect()
    }

    fn extract_return_type(&mut self, node: &SyntaxNode) -> HirType {
        // Find SubtypeIndication that is a direct child of the function body
        // (not inside ParamList). The return type SubtypeIndication comes after ReturnKw.
        let mut found_return = false;
        for child in node.children_with_tokens() {
            match child.kind() {
                SyntaxKind::ReturnKw => {
                    found_return = true;
                }
                SyntaxKind::IsKw | SyntaxKind::Semicolon => {
                    break;
                }
                _ if found_return => {
                    if let SyntaxElement::Node(ref n) = child {
                        if n.kind() == SyntaxKind::SubtypeIndication {
                            return self.lower_subtype_indication(n);
                        }
                    }
                }
                _ => {}
            }
        }
        self.emit_warning_at(
            "could not resolve function return type, defaulting to std_logic",
            Some(node),
        );
        HirType::Logic(1)
    }

    // ====================================================================
    // Subtype/Alias lowering
    // ====================================================================

    fn lower_subtype_decl(&mut self, node: &SyntaxNode) {
        let name = match first_ident(node) {
            Some(n) => n,
            None => return,
        };
        if let Some(st) = first_child_of_kind(node, SyntaxKind::SubtypeIndication) {
            let ty = self.lower_subtype_indication(&st);
            self.user_types.insert(name, ty);
        }
    }

    fn lower_alias_decl(&mut self, node: &SyntaxNode) {
        let alias_name = match first_ident(node) {
            Some(n) => n,
            None => return,
        };

        // The target is after IsKw — it may be an expression node (Name) or direct ident.
        // Collect all idents that appear after the IsKw token.
        let mut after_is = false;
        let mut target_name = None;
        for el in node.children_with_tokens() {
            match el.kind() {
                SyntaxKind::IsKw => {
                    after_is = true;
                }
                SyntaxKind::Ident if after_is && target_name.is_none() => {
                    if let SyntaxElement::Token(ref t) = el {
                        target_name = Some(t.text().to_ascii_lowercase());
                    }
                }
                SyntaxKind::Name if after_is && target_name.is_none() => {
                    // Target is inside a Name node
                    if let SyntaxElement::Node(ref n) = el {
                        target_name = first_ident(n).map(|s| s.to_ascii_lowercase());
                    }
                }
                _ => {}
            }
        }

        let target_name = match target_name {
            Some(n) => n,
            None => return,
        };

        // Copy mapping from target to alias
        if let Some(id) = self.port_map.get(&target_name).copied() {
            self.port_map.insert(alias_name, id);
        } else if let Some(id) = self.signal_map.get(&target_name).copied() {
            self.signal_map.insert(alias_name, id);
        } else if let Some(id) = self.variable_map.get(&target_name).copied() {
            self.variable_map.insert(alias_name, id);
        } else if let Some(id) = self.constant_map.get(&target_name).copied() {
            self.constant_map.insert(alias_name, id);
        } else if let Some(ty) = self.user_types.get(&target_name).cloned() {
            self.user_types.insert(alias_name, ty);
        }
    }

    // ====================================================================
    // Process -> HirEventBlock
    // ====================================================================

    fn lower_process(&mut self, node: &SyntaxNode) -> (Option<HirEventBlock>, Vec<HirVariable>) {
        let block_id = self.alloc_block_id();

        // Extract sensitivity list
        let sensitivity = first_child_of_kind(node, SyntaxKind::SensitivityList);
        let is_combinational = sensitivity
            .as_ref()
            .map(|sl| has_token(sl, SyntaxKind::AllKw))
            .unwrap_or(false);

        let sens_names: Vec<String> = sensitivity
            .as_ref()
            .map(|sl| {
                sl.children()
                    .filter(|c| c.kind() == SyntaxKind::Name)
                    .filter_map(|n| first_ident(&n).or_else(|| self.name_first_text(&n)))
                    .collect()
            })
            .unwrap_or_default();

        // Process declarations (variables)
        let mut proc_vars = Vec::new();
        for child in node.children() {
            if child.kind() == SyntaxKind::VariableDecl {
                if let Some(v) = self.lower_variable_decl(&child) {
                    proc_vars.push(v);
                }
            }
        }

        // Sequential statements
        self.inside_event_block = !is_combinational;
        let mut statements = self.lower_sequential_statements(node);
        self.inside_event_block = false;

        // Determine triggers
        let triggers = if is_combinational {
            Vec::new() // Empty triggers = combinational
        } else {
            self.detect_triggers(&sens_names, &statements)
        };

        // Post-process: strip rising_edge/falling_edge Call wrappers from if-conditions
        // so downstream MIR sees the signal reference directly
        Self::strip_edge_calls(&mut statements);

        // Unwrap the outer if-wrapper left behind by strip_edge_calls.
        // After stripping, a process like:
        //   process(clk) begin if rising_edge(clk) then <body> end if; end process;
        // has statements = [If { condition: Port(clk), then: <body>, else: None }]
        // The If is redundant because the trigger already captures the edge.
        if !triggers.is_empty() {
            if let [HirStatement::If(ref ifs)] = statements.as_slice() {
                let cond_matches_trigger =
                    triggers.iter().any(|t| match (&t.signal, &ifs.condition) {
                        (HirEventSignal::Port(pid), HirExpression::Port(cond_pid)) => {
                            pid == cond_pid
                        }
                        (HirEventSignal::Signal(sid), HirExpression::Signal(cond_sid)) => {
                            sid == cond_sid
                        }
                        _ => false,
                    });
                if cond_matches_trigger && ifs.else_statements.is_none() {
                    statements = ifs.then_statements.clone();
                }
            }
        }

        let event_block = HirEventBlock {
            id: block_id,
            triggers,
            statements,
            comments: collect_leading_comments(node),
        };

        (Some(event_block), proc_vars)
    }

    fn detect_triggers(
        &mut self,
        sens_names: &[String],
        statements: &[HirStatement],
    ) -> Vec<HirEventTrigger> {
        // 1. Try to detect edge from process body (rising_edge/falling_edge calls)
        if let Some(mut edge_triggers) = self.extract_edge_from_body(statements) {
            // 2. For async reset pattern (rising_edge in elsif, not outermost if),
            // add non-clock sensitivity list signals as level-sensitive reset triggers.
            // E.g. process(clk, rst) with "if rst='1' then ... elsif rising_edge(clk)"
            // should have rst as an Active trigger for async reset behavior.
            if let Some(reset_info) = self.detect_async_reset_from_body(statements) {
                // Find the reset signal in the sensitivity list
                for name in sens_names {
                    let lower = name.to_ascii_lowercase();
                    // Skip signals already in edge_triggers (i.e. the clock)
                    let is_clock = edge_triggers.iter().any(|t| match &t.signal {
                        HirEventSignal::Port(pid) => {
                            self.port_map.get(&lower).map(|p| p == pid).unwrap_or(false)
                        }
                        HirEventSignal::Signal(sid) => self
                            .signal_map
                            .get(&lower)
                            .map(|s| s == sid)
                            .unwrap_or(false),
                    });
                    if !is_clock {
                        let signal = self.resolve_signal_ref(name);
                        let edge = if reset_info.active_high {
                            HirEdgeType::Active
                        } else {
                            HirEdgeType::Inactive
                        };
                        edge_triggers.push(HirEventTrigger { signal, edge });
                    }
                }
            }
            return edge_triggers;
        }

        // 3. If no edge calls found in body, this is a combinational process.
        // Don't use name-based heuristics — names like "sys_clk_cnt_max" contain
        // "clk" but are counter signals, not clocks. Only rising_edge()/falling_edge()
        // calls in the process body reliably indicate sequential logic.
        Vec::new()
    }

    fn strip_edge_calls(statements: &mut [HirStatement]) {
        for stmt in statements.iter_mut() {
            if let HirStatement::If(ref mut ifs) = stmt {
                if let HirExpression::Call(ref call) = ifs.condition {
                    let func = call.function.to_ascii_lowercase();
                    if func == "rising_edge" || func == "falling_edge" {
                        if let Some(arg) = call.args.first() {
                            ifs.condition = arg.clone();
                        }
                    }
                }
                Self::strip_edge_calls(&mut ifs.then_statements);
                if let Some(ref mut else_stmts) = ifs.else_statements {
                    Self::strip_edge_calls(else_stmts);
                }
            }
        }
    }

    fn extract_edge_from_body(&self, statements: &[HirStatement]) -> Option<Vec<HirEventTrigger>> {
        // Look for the outermost if-statement whose condition is a rising_edge/falling_edge call
        let first_if = statements.iter().find_map(|s| {
            if let HirStatement::If(ifs) = s {
                Some(ifs)
            } else {
                None
            }
        })?;

        // Check the outermost condition first (sync reset pattern: if rising_edge(clk) then ...)
        if let Some(triggers) = self.extract_edge_from_condition(&first_if.condition) {
            return Some(triggers);
        }

        // Check elsif branches for async reset pattern:
        //   if rst = '1' then ...
        //   elsif rising_edge(clk) then ...
        if let Some(ref else_stmts) = first_if.else_statements {
            for stmt in else_stmts {
                if let HirStatement::If(elsif) = stmt {
                    if let Some(triggers) = self.extract_edge_from_condition(&elsif.condition) {
                        return Some(triggers);
                    }
                }
            }
        }

        None
    }

    fn extract_edge_from_condition(&self, expr: &HirExpression) -> Option<Vec<HirEventTrigger>> {
        match expr {
            HirExpression::Call(call) => {
                let func = call.function.to_ascii_lowercase();
                if func == "rising_edge" || func.starts_with("rising_edge_") {
                    let signal = self.expr_to_event_signal(call.args.first()?)?;
                    return Some(vec![HirEventTrigger {
                        signal,
                        edge: HirEdgeType::Rising,
                    }]);
                }
                if func == "falling_edge" || func.starts_with("falling_edge_") {
                    let signal = self.expr_to_event_signal(call.args.first()?)?;
                    return Some(vec![HirEventTrigger {
                        signal,
                        edge: HirEdgeType::Falling,
                    }]);
                }
                None
            }
            // rising_edge might have been lowered to just the port ref (pre-fix)
            // For backwards compatibility, if condition is a port, treat as rising edge
            HirExpression::Port(pid) => Some(vec![HirEventTrigger {
                signal: HirEventSignal::Port(*pid),
                edge: HirEdgeType::Rising,
            }]),
            _ => None,
        }
    }

    fn expr_to_event_signal(&self, expr: &HirExpression) -> Option<HirEventSignal> {
        match expr {
            HirExpression::Port(pid) => Some(HirEventSignal::Port(*pid)),
            HirExpression::Signal(sid) => Some(HirEventSignal::Signal(*sid)),
            _ => None,
        }
    }

    /// Detect async reset pattern: `if <reset_cond> then ... elsif rising_edge(clk) then ...`
    /// Returns reset polarity info if the pattern is found.
    fn detect_async_reset_from_body(&self, statements: &[HirStatement]) -> Option<AsyncResetInfo> {
        let first_if = statements.iter().find_map(|s| {
            if let HirStatement::If(ifs) = s {
                Some(ifs)
            } else {
                None
            }
        })?;

        // The outermost condition must NOT be a rising_edge/falling_edge call
        // (otherwise it's a sync reset pattern, not async)
        if self
            .extract_edge_from_condition(&first_if.condition)
            .is_some()
        {
            return None;
        }

        // Check if an elsif branch has rising_edge/falling_edge
        if let Some(ref else_stmts) = first_if.else_statements {
            for stmt in else_stmts {
                if let HirStatement::If(elsif) = stmt {
                    if self.extract_edge_from_condition(&elsif.condition).is_some() {
                        // Async reset pattern confirmed. Determine polarity from outer condition.
                        // `if rst = '1'` => active_high=true
                        // `if rst = '0'` => active_high=false (active-low reset)
                        let active_high = match &first_if.condition {
                            HirExpression::Binary(op) => {
                                // `rst = '0'` means active-low
                                if matches!(op.op, HirBinaryOp::Equal) {
                                    !matches!(
                                        *op.right,
                                        HirExpression::Literal(HirLiteral::Integer(0))
                                    )
                                } else {
                                    true // default to active-high
                                }
                            }
                            _ => true, // default to active-high
                        };
                        return Some(AsyncResetInfo { active_high });
                    }
                }
            }
        }

        None
    }

    fn resolve_signal_ref(&mut self, name: &str) -> HirEventSignal {
        let lower = name.to_ascii_lowercase();
        if let Some(port_id) = self.port_map.get(&lower) {
            HirEventSignal::Port(*port_id)
        } else if let Some(sig_id) = self.signal_map.get(&lower) {
            HirEventSignal::Signal(*sig_id)
        } else {
            self.emit_error_at(format!("unresolved sensitivity signal: '{}'", name), None);
            HirEventSignal::Port(PortId(0))
        }
    }

    // ====================================================================
    // Sequential statements
    // ====================================================================

    fn lower_sequential_statements(&mut self, parent: &SyntaxNode) -> Vec<HirStatement> {
        let children: Vec<SyntaxNode> = parent.children().collect();
        self.lower_sequential_children(&children)
    }

    fn lower_sequential_children(&mut self, children: &[SyntaxNode]) -> Vec<HirStatement> {
        let mut stmts = Vec::new();
        let mut i = 0;
        while i < children.len() {
            let child = &children[i];
            match child.kind() {
                SyntaxKind::IfStmt => {
                    if let Some(s) = self.lower_if_stmt(child) {
                        stmts.push(s);
                    }
                }
                SyntaxKind::CaseStmt => {
                    if let Some(s) = self.lower_case_stmt(child) {
                        stmts.push(s);
                    }
                }
                SyntaxKind::ForLoopStmt => {
                    if let Some(s) = self.lower_for_loop(child) {
                        stmts.push(s);
                    }
                }
                SyntaxKind::SequentialSignalAssign | SyntaxKind::VariableAssignStmt => {
                    if let Some(a) = self.lower_sequential_assign(child) {
                        stmts.push(HirStatement::Assignment(a));
                    }
                }
                SyntaxKind::WhileLoopStmt => {
                    if let Some(s) = self.lower_while_loop(child) {
                        stmts.push(s);
                    }
                }
                SyntaxKind::ReturnStmt => {
                    stmts.push(self.lower_return_stmt(child));
                }
                SyntaxKind::NullStmt => {
                    // null statement — no-op
                }
                SyntaxKind::AssertStmt => {
                    // assert is legitimately non-synthesizable — silent skip
                }
                SyntaxKind::ExitStmt | SyntaxKind::NextStmt => {
                    let is_exit = child.kind() == SyntaxKind::ExitStmt;
                    if let Some(guard) = self.lower_loop_control(child, is_exit, &children[i + 1..])
                    {
                        stmts.push(guard);
                    }
                    // All remaining children are already wrapped in the guard
                    break;
                }
                _ => {
                    // Try recursive
                    let sub = self.lower_sequential_statements(child);
                    stmts.extend(sub);
                }
            }
            i += 1;
        }
        stmts
    }

    /// Lower `exit [when cond]` or `next [when cond]` by wrapping remaining
    /// loop body statements in a conditional guard.
    ///
    /// `exit when cond` → `if not cond then <remaining_stmts> end if;`
    /// `next when cond` → `if not cond then <remaining_stmts> end if;`
    /// `exit` (unconditional) → remaining stmts are dead code, omit them.
    fn lower_loop_control(
        &mut self,
        node: &SyntaxNode,
        _is_exit: bool,
        remaining_siblings: &[SyntaxNode],
    ) -> Option<HirStatement> {
        // Extract condition: elements between WhenKw and Semicolon
        let has_when = has_token(node, SyntaxKind::WhenKw);

        if !has_when {
            // Unconditional exit/next — remaining statements are unreachable
            return None;
        }

        // Extract the condition expression (after WhenKw)
        let elements: Vec<SyntaxElement> = node
            .children_with_tokens()
            .filter(|el| el.kind() != SyntaxKind::Whitespace && el.kind() != SyntaxKind::Comment)
            .collect();

        let mut cond_elements = Vec::new();
        let mut after_when = false;
        for el in &elements {
            match el.kind() {
                SyntaxKind::WhenKw => {
                    after_when = true;
                }
                SyntaxKind::Semicolon => break,
                _ if after_when => {
                    cond_elements.push(el.clone());
                }
                _ => {}
            }
        }

        if cond_elements.is_empty() {
            return None;
        }

        let condition = self.lower_expr_from_elements(&cond_elements);

        // Negate the condition: wrap remaining body in `if NOT condition`
        let negated = HirExpression::Unary(HirUnaryExpr {
            op: HirUnaryOp::Not,
            operand: Box::new(condition),
        });

        // Lower remaining siblings as the guarded body
        let guarded_body = self.lower_sequential_children(remaining_siblings);

        Some(HirStatement::If(HirIfStatement {
            condition: negated,
            then_statements: guarded_body,
            else_statements: None,
            mux_style: MuxStyle::default(),
        }))
    }

    fn lower_return_stmt(&mut self, node: &SyntaxNode) -> HirStatement {
        let elements: Vec<SyntaxElement> = node
            .children_with_tokens()
            .filter(|el| {
                el.kind() != SyntaxKind::Whitespace
                    && el.kind() != SyntaxKind::Comment
                    && el.kind() != SyntaxKind::ReturnKw
                    && el.kind() != SyntaxKind::Semicolon
            })
            .collect();
        if elements.is_empty() {
            HirStatement::Return(None)
        } else {
            HirStatement::Return(Some(self.lower_expr_from_elements(&elements)))
        }
    }

    fn lower_while_loop(&mut self, node: &SyntaxNode) -> Option<HirStatement> {
        // While loops must be bounded for synthesis.
        // Lower as: for i in 0 to 255 loop if condition then body end if; end loop;
        let elements: Vec<SyntaxElement> = node.children_with_tokens().collect();

        // Extract condition: tokens between WhileKw and LoopKw
        let mut condition_elements = Vec::new();
        let mut in_condition = false;
        for el in &elements {
            match el.kind() {
                SyntaxKind::WhileKw => {
                    in_condition = true;
                }
                SyntaxKind::LoopKw if in_condition => {
                    break;
                }
                SyntaxKind::Whitespace | SyntaxKind::Comment => {}
                _ if in_condition => {
                    condition_elements.push(el.clone());
                }
                _ => {}
            }
        }

        let condition = self.lower_expr_from_elements(&condition_elements);
        let body = self.lower_sequential_statements(node);

        let loop_id = self.alloc_for_loop_id();
        let iter_var_id = self.alloc_variable_id();
        let iterator = format!("__while_iter_{}", loop_id.0);

        let guarded_body = vec![HirStatement::If(HirIfStatement {
            condition,
            then_statements: body,
            else_statements: None,
            mux_style: MuxStyle::default(),
        })];

        Some(HirStatement::For(HirForStatement {
            id: loop_id,
            iterator,
            iterator_var_id: iter_var_id,
            range: HirRange {
                start: HirExpression::Literal(HirLiteral::Integer(0)),
                end: HirExpression::Literal(HirLiteral::Integer(255)),
                inclusive: true,
                step: None,
            },
            body: guarded_body,
            unroll: None,
        }))
    }

    fn lower_if_stmt(&mut self, node: &SyntaxNode) -> Option<HirStatement> {
        let all_elements: Vec<SyntaxElement> = node.children_with_tokens().collect();
        self.lower_if_from_elements(&all_elements)
    }

    fn lower_if_from_elements(&mut self, elements: &[SyntaxElement]) -> Option<HirStatement> {
        // Find sections delimited by If/Then/Elsif/Else/End tokens
        let mut sections: Vec<IfSection> = Vec::new();
        let mut current_section = IfSection::new(IfSectionKind::If);
        let mut in_condition = true;

        for el in elements {
            let kind = el.kind();
            match kind {
                SyntaxKind::Whitespace | SyntaxKind::Comment => continue,
                SyntaxKind::IfKw => {
                    // Start of if or end if — check context
                    if !current_section.condition_elements.is_empty()
                        || !current_section.body_elements.is_empty()
                    {
                        // This is "end if" — finalize
                        sections.push(current_section);
                        break;
                    }
                    in_condition = true;
                }
                SyntaxKind::ThenKw => {
                    in_condition = false;
                }
                SyntaxKind::ElsifKw => {
                    sections.push(current_section);
                    current_section = IfSection::new(IfSectionKind::Elsif);
                    in_condition = true;
                }
                SyntaxKind::ElseKw => {
                    sections.push(current_section);
                    current_section = IfSection::new(IfSectionKind::Else);
                    in_condition = false;
                }
                SyntaxKind::EndKw => {
                    sections.push(current_section);
                    break;
                }
                SyntaxKind::Semicolon => {
                    // End of "end if;" — just skip
                }
                _ => {
                    if in_condition {
                        current_section.condition_elements.push(el.clone());
                    } else {
                        current_section.body_elements.push(el.clone());
                    }
                }
            }
        }

        if sections.is_empty() {
            return None;
        }

        // Build nested if/elsif/else chain
        self.build_if_chain(&sections, 0)
    }

    fn build_if_chain(&mut self, sections: &[IfSection], idx: usize) -> Option<HirStatement> {
        if idx >= sections.len() {
            return None;
        }

        let section = &sections[idx];

        match section.kind {
            IfSectionKind::If | IfSectionKind::Elsif => {
                let condition = self.lower_expr_from_elements(&section.condition_elements);
                let then_stmts = self.lower_stmts_from_elements(&section.body_elements);

                let else_stmts = if idx + 1 < sections.len() {
                    match sections[idx + 1].kind {
                        IfSectionKind::Elsif => {
                            let elsif_stmt = self.build_if_chain(sections, idx + 1)?;
                            Some(vec![elsif_stmt])
                        }
                        IfSectionKind::Else => {
                            Some(self.lower_stmts_from_elements(&sections[idx + 1].body_elements))
                        }
                        _ => None,
                    }
                } else {
                    None
                };

                Some(HirStatement::If(HirIfStatement {
                    condition,
                    then_statements: then_stmts,
                    else_statements: else_stmts,
                    mux_style: MuxStyle::default(),
                }))
            }
            IfSectionKind::Else => {
                // Else by itself shouldn't appear at top level of chain
                None
            }
        }
    }

    fn lower_case_stmt(&mut self, node: &SyntaxNode) -> Option<HirStatement> {
        // Find the selector expression (first Name/expression child after CaseKw)
        let elements: Vec<SyntaxElement> = node.children_with_tokens().collect();

        // Extract selector: everything between "case" and "is"
        let mut selector_elements = Vec::new();
        let mut in_selector = false;
        for el in &elements {
            match el.kind() {
                SyntaxKind::CaseKw if !in_selector => {
                    in_selector = true;
                }
                SyntaxKind::IsKw if in_selector => {
                    break;
                }
                SyntaxKind::Whitespace | SyntaxKind::Comment => {}
                _ if in_selector => {
                    selector_elements.push(el.clone());
                }
                _ => {}
            }
        }

        let expr = self.lower_expr_from_elements(&selector_elements);

        // Process case alternatives — expand multiple choices into separate arms
        let mut arms = Vec::new();
        for child in node.children() {
            if child.kind() == SyntaxKind::CaseAlternative {
                arms.extend(self.lower_case_alternative_arms(&child));
            }
        }

        Some(HirStatement::Match(HirMatchStatement {
            expr,
            arms,
            mux_style: MuxStyle::default(),
        }))
    }

    fn lower_case_alternative_arms(&mut self, node: &SyntaxNode) -> Vec<HirMatchArm> {
        // when choice1 | choice2 => statements
        // Emit one arm per choice, each with the same body
        let choice_list = first_child_of_kind(node, SyntaxKind::ChoiceList);
        let body = self.lower_sequential_statements(node);

        let choices = choice_list
            .map(|cl| all_children_of_kind(&cl, SyntaxKind::Choice))
            .unwrap_or_default();

        if choices.is_empty() {
            return vec![HirMatchArm {
                pattern: HirPattern::Wildcard,
                guard: None,
                statements: body,
            }];
        }

        choices
            .iter()
            .map(|choice| {
                let pattern = self.lower_choice_to_pattern(choice);
                HirMatchArm {
                    pattern,
                    guard: None,
                    statements: body.clone(),
                }
            })
            .collect()
    }

    fn lower_choice_to_pattern(&mut self, node: &SyntaxNode) -> HirPattern {
        if has_token(node, SyntaxKind::OthersKw) {
            return HirPattern::Wildcard;
        }

        // Try integer literal
        if let Some(text) = first_token_text(node, SyntaxKind::IntLiteral) {
            if let Ok(val) = text.replace('_', "").parse::<u64>() {
                return HirPattern::Literal(HirLiteral::Integer(val));
            }
        }

        // Try bit string literal
        if let Some(text) = first_token_text(node, SyntaxKind::BitStringLiteral) {
            return HirPattern::Literal(self.parse_bit_string_literal(&text));
        }

        // Try string literal (used as bit pattern)
        if let Some(text) = first_token_text(node, SyntaxKind::StringLiteral) {
            let inner = text.trim_matches('"');
            if inner.chars().all(|c| c == '0' || c == '1') {
                let val = u64::from_str_radix(inner, 2).unwrap_or(0);
                return HirPattern::Literal(HirLiteral::Integer(val));
            }
        }

        // Try char literal
        if let Some(text) = first_token_text(node, SyntaxKind::CharLiteral) {
            let ch = text.trim_matches('\'').chars().next().unwrap_or('0');
            match ch {
                '0' => return HirPattern::Literal(HirLiteral::Integer(0)),
                '1' => return HirPattern::Literal(HirLiteral::Integer(1)),
                _ => return HirPattern::Literal(HirLiteral::Integer(0)),
            }
        }

        // Try identifier (enum variant or variable binding)
        // Look for Ident both as direct child and inside child Name nodes
        let name = first_ident(node).or_else(|| {
            // VHDL parser wraps choice identifiers in a Name node:
            //   Choice -> Name -> Ident
            // So look inside child Name nodes for the identifier
            node.children()
                .find(|c| c.kind() == SyntaxKind::Name)
                .and_then(|name_node| first_ident(&name_node))
        });
        if let Some(name) = name {
            // Check if it's a known enum variant
            for (_type_name, hir_type) in &self.user_types {
                if let HirType::Enum(ref et) = hir_type {
                    if et.variants.iter().any(|v| v.name == name) {
                        return HirPattern::Path(et.name.clone(), name);
                    }
                }
            }
            return HirPattern::Variable(name);
        }

        // Try expression child
        let child_nodes: Vec<SyntaxNode> = node.children().collect();
        if let Some(first) = child_nodes.first() {
            let elements: Vec<SyntaxElement> = vec![SyntaxElement::Node(first.clone())];
            let expr = self.lower_expr_from_elements(&elements);
            if let HirExpression::Literal(lit) = expr {
                return HirPattern::Literal(lit);
            }
        }

        HirPattern::Wildcard
    }

    fn lower_for_loop(&mut self, node: &SyntaxNode) -> Option<HirStatement> {
        let idents = ident_texts(node);
        let iterator = idents.first()?.clone();
        let loop_id = self.alloc_for_loop_id();
        let iter_var_id = self.alloc_variable_id();
        self.variable_map.insert(iterator.clone(), iter_var_id);

        let range = first_child_of_kind(node, SyntaxKind::DiscreteRange)
            .and_then(|dr| self.lower_discrete_range(&dr))
            .unwrap_or_else(|| {
                self.emit_error_at(
                    "could not resolve for-loop range, defaulting to 0..0",
                    Some(node),
                );
                HirRange {
                    start: HirExpression::Literal(HirLiteral::Integer(0)),
                    end: HirExpression::Literal(HirLiteral::Integer(0)),
                    inclusive: true,
                    step: None,
                }
            });

        let body = self.lower_sequential_statements(node);

        Some(HirStatement::For(HirForStatement {
            id: loop_id,
            iterator,
            iterator_var_id: iter_var_id,
            range,
            body,
            unroll: None,
        }))
    }

    fn lower_discrete_range(&mut self, node: &SyntaxNode) -> Option<HirRange> {
        let elements: Vec<SyntaxElement> = node
            .children_with_tokens()
            .filter(|el| el.kind() != SyntaxKind::Whitespace && el.kind() != SyntaxKind::Comment)
            .collect();

        // Find "to" or "downto" token
        let mut before = Vec::new();
        let mut after = Vec::new();
        let mut direction = None;

        for el in &elements {
            match el.kind() {
                SyntaxKind::ToKw => {
                    direction = Some(RangeDirection::To);
                }
                SyntaxKind::DowntoKw => {
                    direction = Some(RangeDirection::Downto);
                }
                _ => {
                    if direction.is_none() {
                        before.push(el.clone());
                    } else {
                        after.push(el.clone());
                    }
                }
            }
        }

        let left = self.lower_expr_from_elements(&before);
        let right = self.lower_expr_from_elements(&after);

        match direction {
            Some(RangeDirection::To) => Some(HirRange {
                start: left,
                end: right,
                inclusive: true,
                step: None,
            }),
            Some(RangeDirection::Downto) => Some(HirRange {
                start: right,
                end: left,
                inclusive: true,
                step: None,
            }),
            None => Some(HirRange {
                start: left,
                end: right,
                inclusive: true,
                step: None,
            }),
        }
    }

    fn lower_sequential_assign(&mut self, node: &SyntaxNode) -> Option<HirAssignment> {
        let elements: Vec<SyntaxElement> = node
            .children_with_tokens()
            .filter(|el| el.kind() != SyntaxKind::Whitespace && el.kind() != SyntaxKind::Comment)
            .collect();

        // Split on <= or :=
        let mut lhs_elements = Vec::new();
        let mut rhs_elements = Vec::new();
        let mut assignment_type = if self.inside_event_block {
            HirAssignmentType::NonBlocking
        } else {
            HirAssignmentType::Combinational
        };
        let mut found_assign = false;
        let mut has_when = false;

        for el in &elements {
            match el.kind() {
                SyntaxKind::SignalAssign if !found_assign => {
                    found_assign = true;
                    if self.inside_event_block {
                        assignment_type = HirAssignmentType::NonBlocking;
                    }
                }
                SyntaxKind::VarAssign if !found_assign => {
                    found_assign = true;
                    assignment_type = HirAssignmentType::Blocking;
                }
                SyntaxKind::Semicolon => {}
                SyntaxKind::WhenKw if found_assign => {
                    has_when = true;
                    rhs_elements.push(el.clone());
                }
                _ => {
                    if found_assign {
                        rhs_elements.push(el.clone());
                    } else {
                        lhs_elements.push(el.clone());
                    }
                }
            }
        }

        if !found_assign {
            return None;
        }

        let lhs = self.lower_lvalue_from_elements(&lhs_elements);
        let rhs = if has_when {
            self.lower_conditional_expr(&rhs_elements)
        } else {
            self.lower_expr_from_elements(&rhs_elements)
        };

        Some(HirAssignment {
            id: self.alloc_assignment_id(),
            lhs,
            assignment_type,
            rhs,
            comments: collect_leading_comments(node),
        })
    }

    // ====================================================================
    // Concurrent assignments
    // ====================================================================

    fn lower_concurrent_assign(&mut self, node: &SyntaxNode) -> Option<HirAssignment> {
        let elements: Vec<SyntaxElement> = node
            .children_with_tokens()
            .filter(|el| el.kind() != SyntaxKind::Whitespace && el.kind() != SyntaxKind::Comment)
            .collect();

        let mut lhs_elements = Vec::new();
        let mut rhs_elements = Vec::new();
        let mut found_assign = false;
        let mut has_when = false;

        for el in &elements {
            match el.kind() {
                SyntaxKind::SignalAssign if !found_assign => {
                    found_assign = true;
                }
                SyntaxKind::Semicolon => {}
                SyntaxKind::WhenKw => {
                    has_when = true;
                    rhs_elements.push(el.clone());
                }
                _ => {
                    if found_assign {
                        rhs_elements.push(el.clone());
                    } else {
                        lhs_elements.push(el.clone());
                    }
                }
            }
        }

        if !found_assign {
            return None;
        }

        let lhs = self.lower_lvalue_from_elements(&lhs_elements);

        let rhs = if has_when {
            self.lower_conditional_expr(&rhs_elements)
        } else {
            self.lower_expr_from_elements(&rhs_elements)
        };

        Some(HirAssignment {
            id: self.alloc_assignment_id(),
            lhs,
            assignment_type: HirAssignmentType::Combinational,
            rhs,
            comments: collect_leading_comments(node),
        })
    }

    /// Lower `with sel select target <= val1 when choices1, val2 when choices2, ...`
    /// into an `HirAssignment` with a `Match` expression on the RHS.
    fn lower_selected_assign(&mut self, node: &SyntaxNode) -> Option<HirAssignment> {
        let elements: Vec<SyntaxElement> = node
            .children_with_tokens()
            .filter(|el| el.kind() != SyntaxKind::Whitespace && el.kind() != SyntaxKind::Comment)
            .collect();

        // Phase 1: Extract selector (between WithKw and SelectKw)
        let mut selector_elements = Vec::new();
        let mut target_elements = Vec::new();
        let mut rhs_elements = Vec::new();
        let mut phase = 0u8; // 0=before with, 1=selector, 2=target, 3=rhs

        for el in &elements {
            match (phase, el.kind()) {
                (0, SyntaxKind::WithKw) => phase = 1,
                (1, SyntaxKind::SelectKw) => phase = 2,
                (2, SyntaxKind::SignalAssign) => phase = 3,
                (1, _) => selector_elements.push(el.clone()),
                (2, _) => target_elements.push(el.clone()),
                (3, SyntaxKind::Semicolon) => {}
                (3, _) => rhs_elements.push(el.clone()),
                _ => {}
            }
        }

        if selector_elements.is_empty() || target_elements.is_empty() {
            return None;
        }

        let selector_expr = self.lower_expr_from_elements(&selector_elements);
        let lhs = self.lower_lvalue_from_elements(&target_elements);

        // Phase 2: Parse value-when-choices groups from rhs_elements.
        // Structure: value1, WhenKw, choices1, Comma, value2, WhenKw, choices2, ...
        // We split into groups separated by Comma at depth 0, then split each on WhenKw.
        let mut arms: Vec<HirMatchArmExpr> = Vec::new();
        let mut current_group: Vec<SyntaxElement> = Vec::new();
        let mut paren_depth = 0i32;

        let flush_group = |builder: &mut Self,
                           group: &mut Vec<SyntaxElement>,
                           arms: &mut Vec<HirMatchArmExpr>| {
            if group.is_empty() {
                return;
            }
            // Split on WhenKw at depth 0
            let when_pos = group.iter().enumerate().find_map(|(i, el)| {
                if el.kind() == SyntaxKind::WhenKw {
                    Some(i)
                } else {
                    None
                }
            });
            if let Some(pos) = when_pos {
                let value_elements = &group[..pos];
                let choice_elements = &group[pos + 1..];

                let value_expr = builder.lower_expr_from_elements(value_elements);

                // Check for "others" keyword
                let is_others = choice_elements
                    .iter()
                    .any(|el| el.kind() == SyntaxKind::OthersKw);
                if is_others {
                    arms.push(HirMatchArmExpr {
                        pattern: HirPattern::Wildcard,
                        guard: None,
                        expr: value_expr,
                    });
                } else {
                    // Parse choices — may be inside a ChoiceList/Choice CST node
                    let choice_nodes: Vec<&SyntaxNode> = choice_elements
                        .iter()
                        .filter_map(|el| {
                            if let SyntaxElement::Node(n) = el {
                                if n.kind() == SyntaxKind::ChoiceList
                                    || n.kind() == SyntaxKind::Choice
                                {
                                    Some(n)
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        })
                        .collect();

                    if let Some(choice_list) = choice_nodes
                        .iter()
                        .find(|n| n.kind() == SyntaxKind::ChoiceList)
                    {
                        for choice in all_children_of_kind(choice_list, SyntaxKind::Choice) {
                            let pattern = builder.lower_choice_to_pattern(&choice);
                            arms.push(HirMatchArmExpr {
                                pattern,
                                guard: None,
                                expr: value_expr.clone(),
                            });
                        }
                    } else {
                        // Fallback: lower the choice elements as a pattern expression
                        let choice_expr = builder.lower_expr_from_elements(choice_elements);
                        let pattern = match &choice_expr {
                            HirExpression::Literal(lit) => HirPattern::Literal(lit.clone()),
                            _ => HirPattern::Wildcard,
                        };
                        arms.push(HirMatchArmExpr {
                            pattern,
                            guard: None,
                            expr: value_expr,
                        });
                    }
                }
            }
            group.clear();
        };

        for el in &rhs_elements {
            match el.kind() {
                SyntaxKind::LParen => {
                    paren_depth += 1;
                    current_group.push(el.clone());
                }
                SyntaxKind::RParen => {
                    paren_depth -= 1;
                    current_group.push(el.clone());
                }
                SyntaxKind::Comma if paren_depth == 0 => {
                    flush_group(self, &mut current_group, &mut arms);
                }
                _ => {
                    current_group.push(el.clone());
                }
            }
        }
        flush_group(self, &mut current_group, &mut arms);

        // Build the match expression as RHS
        let match_expr = HirExpression::Match(HirMatchExpr {
            expr: Box::new(selector_expr),
            arms,
            mux_style: MuxStyle::default(),
        });

        Some(HirAssignment {
            id: self.alloc_assignment_id(),
            lhs,
            assignment_type: HirAssignmentType::Combinational,
            rhs: match_expr,
            comments: collect_leading_comments(node),
        })
    }

    fn lower_conditional_expr(&mut self, elements: &[SyntaxElement]) -> HirExpression {
        // value when condition else value when condition else default
        // Split on WhenKw and ElseKw tokens
        let mut segments: Vec<Vec<SyntaxElement>> = Vec::new();
        let mut current = Vec::new();

        for el in elements {
            match el.kind() {
                SyntaxKind::WhenKw | SyntaxKind::ElseKw => {
                    if !current.is_empty() {
                        segments.push(current);
                        current = Vec::new();
                    }
                }
                _ => {
                    current.push(el.clone());
                }
            }
        }
        if !current.is_empty() {
            segments.push(current);
        }

        // Build ternary chain: segments come in pairs (value, condition) + final default
        if segments.len() >= 3 {
            let value = self.lower_expr_from_elements(&segments[0]);
            let condition = self.lower_expr_from_elements(&segments[1]);
            let else_val = if segments.len() > 3 {
                self.lower_conditional_expr_from_segments(&segments[2..])
            } else {
                self.lower_expr_from_elements(&segments[2])
            };
            HirExpression::Ternary {
                condition: Box::new(condition),
                true_expr: Box::new(value),
                false_expr: Box::new(else_val),
            }
        } else if segments.len() == 1 {
            self.lower_expr_from_elements(&segments[0])
        } else {
            HirExpression::Literal(HirLiteral::Integer(0))
        }
    }

    fn lower_conditional_expr_from_segments(
        &mut self,
        segments: &[Vec<SyntaxElement>],
    ) -> HirExpression {
        if segments.len() >= 3 {
            let value = self.lower_expr_from_elements(&segments[0]);
            let condition = self.lower_expr_from_elements(&segments[1]);
            let else_val = self.lower_conditional_expr_from_segments(&segments[2..]);
            HirExpression::Ternary {
                condition: Box::new(condition),
                true_expr: Box::new(value),
                false_expr: Box::new(else_val),
            }
        } else if segments.len() == 1 {
            self.lower_expr_from_elements(&segments[0])
        } else {
            HirExpression::Literal(HirLiteral::Integer(0))
        }
    }

    // ====================================================================
    // Component instantiation
    // ====================================================================

    fn lower_component_inst(&mut self, node: &SyntaxNode) -> Option<HirInstance> {
        let comp_name = {
            // Try direct idents first (component-style instantiation)
            let idents = ident_texts(node);
            if let Some(name) = idents.first() {
                name.clone()
            } else if let Some(sel_name) = first_child_of_kind(node, SyntaxKind::SelectedName) {
                // entity work.Name style — extract last ident (entity name)
                let sel_idents = ident_texts(&sel_name);
                sel_idents.last().cloned().unwrap_or_default()
            } else if let Some(name) = first_ident_recursive(node) {
                // Component name wrapped in a Name node (from parse_selected_name)
                name
            } else {
                return None;
            }
        };
        let pascal_name = to_pascal_case(&comp_name);

        let entity_id = match self
            .entity_map
            .get(&comp_name)
            .or_else(|| self.entity_map.get(&pascal_name))
            .copied()
        {
            Some(id) => id,
            None => {
                self.emit_error_at(format!("unresolved component: '{}'", comp_name), Some(node));
                EntityId(u32::MAX)
            }
        };

        // Use the label from the parent scope if available, otherwise generate one
        let instance_name =
            extract_instance_label(node).unwrap_or_else(|| format!("u_{}", comp_name));

        // Port map connections
        let mut connections = Vec::new();
        if let Some(pm) = first_child_of_kind(node, SyntaxKind::PortMap) {
            if let Some(al) = first_child_of_kind(&pm, SyntaxKind::AssociationList) {
                for ae in all_children_of_kind(&al, SyntaxKind::AssociationElement) {
                    if let Some(conn) = self.lower_association_element(&ae) {
                        connections.push(conn);
                    }
                }
            }
        }

        // Generic map
        let mut generic_args = Vec::new();
        if let Some(gm) = first_child_of_kind(node, SyntaxKind::GenericMap) {
            if let Some(al) = first_child_of_kind(&gm, SyntaxKind::AssociationList) {
                for ae in all_children_of_kind(&al, SyntaxKind::AssociationElement) {
                    let elements: Vec<SyntaxElement> = ae
                        .children_with_tokens()
                        .filter(|el| {
                            el.kind() != SyntaxKind::Whitespace
                                && el.kind() != SyntaxKind::Comment
                                && el.kind() != SyntaxKind::Arrow
                        })
                        .collect();
                    // For positional: just the expression
                    // For named: skip formal name and =>
                    let expr = self.lower_expr_from_elements(&elements);
                    generic_args.push(expr);
                }
            }
        }

        Some(HirInstance {
            id: self.alloc_instance_id(),
            name: instance_name,
            entity: entity_id,
            generic_args,
            named_generic_args: IndexMap::new(),
            connections,
            safety_config: None,
            variable_id: None,
            comments: collect_leading_comments(node),
        })
    }

    fn lower_association_element(&mut self, node: &SyntaxNode) -> Option<HirConnection> {
        // Check for named association: formal [=> actual]
        // The formal may be a bare Ident token or wrapped in a Name node.
        let has_arrow = node
            .children_with_tokens()
            .any(|el| el.kind() == SyntaxKind::Arrow);

        if has_arrow {
            // Named: port_name => expression
            // Extract port name from the first Name child node or bare Ident token
            let port_name = self.extract_formal_name(node);

            let actual_elements: Vec<SyntaxElement> = node
                .children_with_tokens()
                .filter(|el| {
                    el.kind() != SyntaxKind::Whitespace && el.kind() != SyntaxKind::Comment
                })
                .skip_while(|el| el.kind() != SyntaxKind::Arrow)
                .skip(1) // skip the arrow
                .collect();

            let expr = self.lower_expr_from_elements(&actual_elements);
            Some(HirConnection {
                port: port_name,
                expr,
            })
        } else {
            // Positional — no port name info available
            let all: Vec<SyntaxElement> = node
                .children_with_tokens()
                .filter(|el| {
                    el.kind() != SyntaxKind::Whitespace && el.kind() != SyntaxKind::Comment
                })
                .collect();
            let expr = self.lower_expr_from_elements(&all);
            Some(HirConnection {
                port: String::new(), // positional
                expr,
            })
        }
    }

    /// Extract the formal port name from an association element.
    /// Handles both bare Ident tokens and Name nodes wrapping the formal.
    fn extract_formal_name(&self, node: &SyntaxNode) -> String {
        for el in node.children_with_tokens() {
            if el.kind() == SyntaxKind::Arrow {
                break; // stop before =>
            }
            match el {
                SyntaxElement::Token(ref t)
                    if t.kind() == SyntaxKind::Ident
                        || t.kind() == SyntaxKind::StdLogicKw
                        || t.kind() == SyntaxKind::StdLogicVectorKw
                        || t.kind() == SyntaxKind::UnsignedKw
                        || t.kind() == SyntaxKind::SignedKw =>
                {
                    return t.text().to_string();
                }
                SyntaxElement::Node(ref n) if n.kind() == SyntaxKind::Name => {
                    // Extract the first ident from the Name node
                    if let Some(name) = first_ident(n) {
                        return name;
                    }
                }
                _ => {}
            }
        }
        String::new()
    }

    // ====================================================================
    // VHDL-2019 Interface
    // ====================================================================

    fn lower_view_decl(&mut self, node: &SyntaxNode) {
        let idents = ident_texts(node);
        // First ident = view name, second ident = interface name
        let view_name = match idents.first() {
            Some(n) => n.clone(),
            None => return,
        };
        let interface_name = match idents.get(1) {
            Some(n) => n.clone(),
            None => return,
        };

        let mut direction_map = IndexMap::new();
        for field_dir in all_children_of_kind(node, SyntaxKind::ViewFieldDirection) {
            let field_name = match first_ident(&field_dir) {
                Some(n) => n,
                None => continue,
            };
            let dir = if has_token(&field_dir, SyntaxKind::InKw) {
                HirPortDirection::Input
            } else if has_token(&field_dir, SyntaxKind::OutKw) {
                HirPortDirection::Output
            } else if has_token(&field_dir, SyntaxKind::InoutKw) {
                HirPortDirection::Bidirectional
            } else {
                HirPortDirection::Input
            };
            direction_map.insert(field_name, dir);
        }

        self.view_defs
            .insert(view_name, (interface_name, direction_map));
    }

    fn lower_interface_decl(&mut self, node: &SyntaxNode) {
        let name = match first_ident(node) {
            Some(n) => n,
            None => return,
        };

        let mut fields = Vec::new();
        for sig_decl in all_children_of_kind(node, SyntaxKind::SignalDecl) {
            let sig_names = ident_texts(&sig_decl);
            let sig_type =
                if let Some(st) = first_child_of_kind(&sig_decl, SyntaxKind::SubtypeIndication) {
                    self.lower_subtype_indication(&st)
                } else {
                    let sn_hint = sig_names.first().map(|s| s.as_str()).unwrap_or("?");
                    self.emit_warning_at(
                        format!(
                            "missing type for interface signal '{}', defaulting to std_logic",
                            sn_hint
                        ),
                        Some(&sig_decl),
                    );
                    HirType::Logic(1)
                };
            for sn in &sig_names {
                fields.push((sn.clone(), sig_type.clone()));
            }
        }

        let ty = make_struct_type(&name, &fields);
        self.user_types.insert(name, ty);
    }

    // ====================================================================
    // Package lowering
    // ====================================================================

    fn lower_package(&mut self, node: &SyntaxNode, hir: &mut Hir) {
        // Extract package name and register it
        let pkg_name = first_ident(node).unwrap_or_default();
        if !pkg_name.is_empty() {
            self.package_names.insert(pkg_name.clone());
        }

        // Check for generic clause with type parameters
        let generic_clause = first_child_of_kind(node, SyntaxKind::GenericClause);
        let mut generic_type_param_names = Vec::new();

        if let Some(ref gc) = generic_clause {
            for gd in all_children_of_kind(gc, SyntaxKind::GenericDecl) {
                if has_token(&gd, SyntaxKind::TypeKw) {
                    if let Some(name) = first_ident(&gd) {
                        self.generic_type_params.insert(name.clone());
                        generic_type_param_names.push(name);
                    }
                }
            }
        }

        // Record types/constants before processing so we can snapshot
        let types_before = self.user_types.len();
        let constants_before_keys: Vec<String> = self.constant_map.keys().cloned().collect();

        let mut pkg_functions = Vec::new();
        let mut pending_pkg_constants = Vec::new();

        for child in node.children() {
            match child.kind() {
                SyntaxKind::UseClause => self.lower_use_clause(&child),
                SyntaxKind::TypeDecl => self.lower_type_decl(&child),
                SyntaxKind::SubtypeDecl => self.lower_subtype_decl(&child),
                SyntaxKind::ConstantDecl => {
                    if let Some(c) = self.lower_constant_decl(&child) {
                        pending_pkg_constants.push(c);
                    }
                }
                SyntaxKind::AliasDecl => self.lower_alias_decl(&child),
                SyntaxKind::FunctionBody => {
                    if let Some(f) = self.lower_function(&child) {
                        pkg_functions.push(f.clone());
                        hir.functions.push(f);
                    }
                }
                SyntaxKind::ProcedureBody => {
                    if let Some(f) = self.lower_procedure(&child) {
                        pkg_functions.push(f.clone());
                        hir.functions.push(f);
                    }
                }
                SyntaxKind::FunctionDecl | SyntaxKind::ProcedureDecl => {
                    // Forward declarations — ignore
                }
                SyntaxKind::AttributeDecl | SyntaxKind::AttributeSpec => {
                    // Ignore metadata
                }
                SyntaxKind::SignalDecl | SyntaxKind::VariableDecl | SyntaxKind::ComponentDecl => {
                    // Package-level declarations — register types but skip hardware
                }
                _ => {}
            }
        }

        // If this package has generic type parameters, save a PackageScope
        if !generic_type_param_names.is_empty() && !pkg_name.is_empty() {
            // Collect types added during this package
            let mut pkg_types = IndexMap::new();
            for (k, v) in self.user_types.iter().skip(types_before) {
                pkg_types.insert(k.clone(), v.clone());
            }

            self.package_scopes.insert(
                pkg_name,
                PackageScope {
                    generic_type_params: generic_type_param_names,
                    types: pkg_types,
                    constants: pending_pkg_constants,
                    functions: pkg_functions,
                },
            );
        }

        // Clear generic type params after processing the package
        self.generic_type_params.clear();
    }

    // ====================================================================
    // Package instantiation lowering
    // ====================================================================

    fn lower_package_instantiation(&mut self, node: &SyntaxNode, hir: &mut Hir) {
        let idents = ident_texts(node);
        let instance_name = match idents.first() {
            Some(n) => n.clone(),
            None => return,
        };

        // Source package name: from SelectedName child
        let source_pkg = if let Some(sel) = first_child_of_kind(node, SyntaxKind::SelectedName) {
            let parts: Vec<String> = sel
                .children_with_tokens()
                .filter_map(|el| match el {
                    SyntaxElement::Token(t)
                        if t.kind() != SyntaxKind::Whitespace
                            && t.kind() != SyntaxKind::Comment
                            && t.kind() != SyntaxKind::Dot =>
                    {
                        Some(t.text().to_ascii_lowercase())
                    }
                    _ => None,
                })
                .collect();
            // If "work.pkg", use last part; otherwise use first
            parts.last().cloned().unwrap_or_default()
        } else {
            idents.get(1).cloned().unwrap_or_default()
        };

        // Look up source PackageScope
        let scope = match self.package_scopes.get(&source_pkg) {
            Some(s) => s.clone(),
            None => return,
        };

        // Parse generic map associations to build type substitution map
        let mut type_subs: IndexMap<String, HirType> = IndexMap::new();
        if let Some(gm) = first_child_of_kind(node, SyntaxKind::GenericMap) {
            if let Some(al) = first_child_of_kind(&gm, SyntaxKind::AssociationList) {
                let assoc_elements = all_children_of_kind(&al, SyntaxKind::AssociationElement);
                for (i, ae) in assoc_elements.iter().enumerate() {
                    let tokens_vec = all_token_texts(ae);
                    let arrow_pos = tokens_vec.iter().position(|(k, _)| *k == SyntaxKind::Arrow);

                    let (param_name, type_elements) = if let Some(pos) = arrow_pos {
                        // Named: T => unsigned(7 downto 0)
                        let formal = tokens_vec
                            .first()
                            .map(|(_, t)| t.to_ascii_lowercase())
                            .unwrap_or_default();
                        let actual_elements: Vec<SyntaxElement> = ae
                            .children_with_tokens()
                            .filter(|el| {
                                el.kind() != SyntaxKind::Whitespace
                                    && el.kind() != SyntaxKind::Comment
                            })
                            .skip_while(|el| el.kind() != SyntaxKind::Arrow)
                            .skip(1)
                            .collect();
                        (formal, actual_elements)
                    } else {
                        // Positional: match by order
                        let formal = scope
                            .generic_type_params
                            .get(i)
                            .cloned()
                            .unwrap_or_default();
                        let actual_elements: Vec<SyntaxElement> = ae
                            .children_with_tokens()
                            .filter(|el| {
                                el.kind() != SyntaxKind::Whitespace
                                    && el.kind() != SyntaxKind::Comment
                            })
                            .collect();
                        (formal, actual_elements)
                    };

                    if param_name.is_empty() {
                        continue;
                    }

                    // Try to resolve the RHS as a type (SubtypeIndication child)
                    let resolved_type =
                        if let Some(st) = first_child_of_kind(ae, SyntaxKind::SubtypeIndication) {
                            self.lower_subtype_indication(&st)
                        } else {
                            // Try interpreting the token elements as a type name
                            let type_name = type_elements
                                .iter()
                                .find_map(|el| match el {
                                    SyntaxElement::Token(t) if t.kind() != SyntaxKind::Arrow => {
                                        Some(t.text().to_ascii_lowercase())
                                    }
                                    SyntaxElement::Node(n) => {
                                        if n.kind() == SyntaxKind::SubtypeIndication {
                                            return None; // handled above
                                        }
                                        first_ident(n)
                                    }
                                    _ => None,
                                })
                                .unwrap_or_default();

                            if !type_name.is_empty() {
                                resolve_vhdl_type(
                                    &type_name,
                                    None,
                                    &self.builtin_scope,
                                    &self.user_types,
                                )
                                .unwrap_or(HirType::Custom(type_name))
                            } else {
                                continue;
                            }
                        };

                    type_subs.insert(param_name, resolved_type);
                }
            }
        }

        // Clone and substitute all declarations from the source scope
        for (type_name, ty) in &scope.types {
            let subst = substitute_type(ty, &type_subs);
            self.user_types.insert(type_name.clone(), subst);
        }

        for constant in &scope.constants {
            let subst_type = substitute_type(&constant.const_type, &type_subs);
            let id = self.alloc_constant_id();
            self.constant_map.insert(constant.name.clone(), id);
        }

        for func in &scope.functions {
            let mut subst_func = func.clone();
            subst_func.id = self.alloc_function_id();
            if let Some(ref rt) = subst_func.return_type {
                subst_func.return_type = Some(substitute_type(rt, &type_subs));
            }
            for param in &mut subst_func.params {
                param.param_type = substitute_type(&param.param_type, &type_subs);
            }
            self.function_map
                .insert(subst_func.name.clone(), subst_func.id);
            hir.functions.push(subst_func);
        }

        // Register instance name as a known package
        self.package_names.insert(instance_name);
    }

    // ====================================================================
    // Generate statement lowering
    // ====================================================================

    fn lower_for_generate(&mut self, node: &SyntaxNode) -> Option<HirGenerateFor> {
        let idents = ident_texts(node);
        let iterator = idents.first()?.clone();
        let iter_var_id = self.alloc_variable_id();
        self.variable_map.insert(iterator.clone(), iter_var_id);

        let range = first_child_of_kind(node, SyntaxKind::DiscreteRange)
            .and_then(|dr| self.lower_discrete_range(&dr))
            .unwrap_or(HirRange {
                start: HirExpression::Literal(HirLiteral::Integer(0)),
                end: HirExpression::Literal(HirLiteral::Integer(0)),
                inclusive: true,
                step: None,
            });

        let body = self.lower_generate_body(node);

        Some(HirGenerateFor {
            iterator,
            iterator_var_id: iter_var_id,
            range,
            body,
            mode: GenerateMode::Elaborate,
        })
    }

    fn lower_if_generate(&mut self, node: &SyntaxNode) -> Option<HirGenerateIf> {
        let elements: Vec<SyntaxElement> = node.children_with_tokens().collect();

        // Extract sections: if condition generate body [else generate body] end generate
        let mut sections: Vec<GenerateSection> = Vec::new();
        let mut current = GenerateSection {
            kind: GenerateSectionKind::If,
            condition_elements: Vec::new(),
            body_nodes: Vec::new(),
        };
        let mut in_condition = true;

        for el in &elements {
            let kind = el.kind();
            match kind {
                SyntaxKind::Whitespace | SyntaxKind::Comment => continue,
                SyntaxKind::IfKw => {
                    if !current.condition_elements.is_empty() || !current.body_nodes.is_empty() {
                        // "end ... if" — finalize
                        sections.push(current);
                        break;
                    }
                    in_condition = true;
                }
                SyntaxKind::GenerateKw => {
                    in_condition = false;
                }
                SyntaxKind::ElseKw => {
                    sections.push(current);
                    current = GenerateSection {
                        kind: GenerateSectionKind::Else,
                        condition_elements: Vec::new(),
                        body_nodes: Vec::new(),
                    };
                    in_condition = false;
                }
                SyntaxKind::ElsifKw => {
                    sections.push(current);
                    current = GenerateSection {
                        kind: GenerateSectionKind::Elsif,
                        condition_elements: Vec::new(),
                        body_nodes: Vec::new(),
                    };
                    in_condition = true;
                }
                SyntaxKind::EndKw => {
                    sections.push(current);
                    break;
                }
                _ => {
                    if in_condition {
                        current.condition_elements.push(el.clone());
                    } else {
                        current.body_nodes.push(el.clone());
                    }
                }
            }
        }

        if sections.is_empty() {
            return None;
        }

        let condition = self.lower_expr_from_elements(&sections[0].condition_elements);
        let then_body = self.lower_generate_body_from_elements(&sections[0].body_nodes);

        let else_body = if sections.len() > 1 {
            match sections[1].kind {
                GenerateSectionKind::Else => {
                    Some(self.lower_generate_body_from_elements(&sections[1].body_nodes))
                }
                _ => None,
            }
        } else {
            None
        };

        Some(HirGenerateIf {
            condition,
            then_body,
            else_body,
            mode: GenerateMode::Elaborate,
        })
    }

    fn lower_generate_body(&mut self, node: &SyntaxNode) -> HirGenerateBody {
        let mut body = HirGenerateBody::default();

        for child in node.children() {
            match child.kind() {
                SyntaxKind::SignalDecl => {
                    body.signals.extend(self.lower_signal_decl(&child));
                }
                SyntaxKind::ConstantDecl => {
                    if let Some(c) = self.lower_constant_decl(&child) {
                        body.constants.push(c);
                    }
                }
                SyntaxKind::ProcessStmt => {
                    let (eb, _vars) = self.lower_process(&child);
                    if let Some(eb) = eb {
                        body.event_blocks.push(eb);
                    }
                }
                SyntaxKind::ConcurrentSignalAssign => {
                    if let Some(a) = self.lower_concurrent_assign(&child) {
                        body.assignments.push(a);
                    }
                }
                SyntaxKind::ComponentInst => {
                    if let Some(inst) = self.lower_component_inst(&child) {
                        body.instances.push(inst);
                    }
                }
                SyntaxKind::ForGenerate => {
                    if let Some(g) = self.lower_for_generate(&child) {
                        body.generate_stmts.push(HirStatement::GenerateFor(g));
                    }
                }
                SyntaxKind::IfGenerate => {
                    if let Some(g) = self.lower_if_generate(&child) {
                        body.generate_stmts.push(HirStatement::GenerateIf(g));
                    }
                }
                _ => {}
            }
        }

        body
    }

    fn lower_generate_body_from_elements(&mut self, elements: &[SyntaxElement]) -> HirGenerateBody {
        let mut body = HirGenerateBody::default();

        for el in elements {
            if let SyntaxElement::Node(ref n) = el {
                match n.kind() {
                    SyntaxKind::SignalDecl => {
                        body.signals.extend(self.lower_signal_decl(n));
                    }
                    SyntaxKind::ConstantDecl => {
                        if let Some(c) = self.lower_constant_decl(n) {
                            body.constants.push(c);
                        }
                    }
                    SyntaxKind::ProcessStmt => {
                        let (eb, _vars) = self.lower_process(n);
                        if let Some(eb) = eb {
                            body.event_blocks.push(eb);
                        }
                    }
                    SyntaxKind::ConcurrentSignalAssign => {
                        if let Some(a) = self.lower_concurrent_assign(n) {
                            body.assignments.push(a);
                        }
                    }
                    SyntaxKind::ComponentInst => {
                        if let Some(inst) = self.lower_component_inst(n) {
                            body.instances.push(inst);
                        }
                    }
                    _ => {}
                }
            }
        }

        body
    }

    // ====================================================================
    // Block statement lowering
    // ====================================================================

    #[allow(clippy::too_many_arguments)]
    fn lower_block_stmt_into(
        &mut self,
        node: &SyntaxNode,
        signals: &mut Vec<HirSignal>,
        variables: &mut Vec<HirVariable>,
        constants: &mut Vec<HirConstant>,
        event_blocks: &mut Vec<HirEventBlock>,
        assignments: &mut Vec<HirAssignment>,
        instances: &mut Vec<HirInstance>,
    ) {
        for child in node.children() {
            match child.kind() {
                SyntaxKind::SignalDecl => {
                    signals.extend(self.lower_signal_decl(&child));
                }
                SyntaxKind::ConstantDecl => {
                    if let Some(c) = self.lower_constant_decl(&child) {
                        constants.push(c);
                    }
                }
                SyntaxKind::VariableDecl => {
                    if let Some(v) = self.lower_variable_decl(&child) {
                        variables.push(v);
                    }
                }
                SyntaxKind::TypeDecl => {
                    self.lower_type_decl(&child);
                }
                SyntaxKind::SubtypeDecl => {
                    self.lower_subtype_decl(&child);
                }
                SyntaxKind::ProcessStmt => {
                    let (eb, proc_vars) = self.lower_process(&child);
                    if let Some(eb) = eb {
                        event_blocks.push(eb);
                    }
                    variables.extend(proc_vars);
                }
                SyntaxKind::ConcurrentSignalAssign => {
                    if let Some(a) = self.lower_concurrent_assign(&child) {
                        assignments.push(a);
                    }
                }
                SyntaxKind::ComponentInst => {
                    if let Some(inst) = self.lower_component_inst(&child) {
                        instances.push(inst);
                    }
                }
                _ => {}
            }
        }
    }

    // ====================================================================
    // Expression lowering from syntax elements
    // ====================================================================

    fn lower_expr_from_elements(&mut self, elements: &[SyntaxElement]) -> HirExpression {
        if elements.is_empty() {
            return HirExpression::Literal(HirLiteral::Integer(0));
        }

        // Single element
        if elements.len() == 1 {
            return self.lower_single_element(&elements[0]);
        }

        // Check for conditional expression (when...else) at depth 0 — lowest precedence
        {
            let mut depth = 0i32;
            let has_when = elements.iter().any(|el| {
                match el.kind() {
                    SyntaxKind::LParen => depth += 1,
                    SyntaxKind::RParen => depth -= 1,
                    SyntaxKind::WhenKw if depth == 0 => return true,
                    _ => {}
                }
                false
            });
            if has_when {
                return self.lower_conditional_expr(elements);
            }
        }

        // Look for binary operators (lowest precedence first)
        // Logical: and, or, xor, nand, nor, xnor
        if let Some(expr) = self.try_binary_split(
            elements,
            &[
                SyntaxKind::AndKw,
                SyntaxKind::OrKw,
                SyntaxKind::XorKw,
                SyntaxKind::NandKw,
                SyntaxKind::NorKw,
                SyntaxKind::XnorKw,
            ],
        ) {
            return expr;
        }

        // Relational: =, /=, <, <=, >, >=
        if let Some(expr) = self.try_binary_split(
            elements,
            &[
                SyntaxKind::Equal,
                SyntaxKind::NotEqual,
                SyntaxKind::LessThan,
                SyntaxKind::SignalAssign, // <= as comparison
                SyntaxKind::GreaterThan,
                SyntaxKind::GreaterEqual,
            ],
        ) {
            return expr;
        }

        // Additive: +, -, &
        if let Some(expr) = self.try_binary_split(
            elements,
            &[SyntaxKind::Plus, SyntaxKind::Minus, SyntaxKind::Ampersand],
        ) {
            return expr;
        }

        // Multiplicative: *, /
        if let Some(expr) = self.try_binary_split(elements, &[SyntaxKind::Star, SyntaxKind::Slash])
        {
            return expr;
        }

        // Unary prefix: not, -, +
        if elements.len() >= 2 {
            let first_kind = elements[0].kind();
            if first_kind == SyntaxKind::NotKw {
                let operand = self.lower_expr_from_elements(&elements[1..]);
                return HirExpression::Unary(HirUnaryExpr {
                    op: HirUnaryOp::Not,
                    operand: Box::new(operand),
                });
            }
            if first_kind == SyntaxKind::Minus {
                let operand = self.lower_expr_from_elements(&elements[1..]);
                return HirExpression::Unary(HirUnaryExpr {
                    op: HirUnaryOp::Negate,
                    operand: Box::new(operand),
                });
            }
        }

        // VHDL type conversion keywords followed by parenthesized arguments:
        // integer(x), real(x), natural(x), positive(x)
        // Lower these as function calls to the corresponding builtins.
        if elements.len() >= 3 {
            let first_kind = elements[0].kind();
            let fn_name = match first_kind {
                SyntaxKind::IntegerKw => Some("integer"),
                SyntaxKind::RealKw => Some("real"),
                SyntaxKind::NaturalKw => Some("natural"),
                SyntaxKind::PositiveKw => Some("natural"),
                _ => None,
            };
            if let Some(fn_name) = fn_name {
                if elements[1].kind() == SyntaxKind::LParen {
                    // Extract the argument between parentheses
                    let mut arg_elems = Vec::new();
                    let mut depth = 0i32;
                    for el in &elements[2..] {
                        match el.kind() {
                            SyntaxKind::LParen => {
                                depth += 1;
                                arg_elems.push(el.clone());
                            }
                            SyntaxKind::RParen if depth == 0 => break,
                            SyntaxKind::RParen => {
                                depth -= 1;
                                arg_elems.push(el.clone());
                            }
                            SyntaxKind::Whitespace | SyntaxKind::Comment => {}
                            _ => arg_elems.push(el.clone()),
                        }
                    }
                    let arg = self.lower_expr_from_elements(&arg_elems);
                    return HirExpression::Call(HirCallExpr {
                        function: fn_name.to_string(),
                        type_args: Vec::new(),
                        named_type_args: IndexMap::new(),
                        args: vec![arg],
                        impl_style: ImplStyle::Auto,
                    });
                }
            }
        }

        // If first element is a node, try to lower it
        if let SyntaxElement::Node(ref n) = elements[0] {
            return self.lower_syntax_node_expr(n);
        }

        // Ident followed by LParen: function call or indexing
        if elements.len() >= 3 {
            if let SyntaxElement::Token(ref t) = elements[0] {
                if t.kind() == SyntaxKind::Ident && elements[1].kind() == SyntaxKind::LParen {
                    let name = t.text().to_ascii_lowercase();
                    let mut arg_elems = Vec::new();
                    let mut depth = 0i32;
                    for el in &elements[2..] {
                        match el.kind() {
                            SyntaxKind::LParen => {
                                depth += 1;
                                arg_elems.push(el.clone());
                            }
                            SyntaxKind::RParen if depth == 0 => break,
                            SyntaxKind::RParen => {
                                depth -= 1;
                                arg_elems.push(el.clone());
                            }
                            SyntaxKind::Whitespace | SyntaxKind::Comment => {}
                            _ => arg_elems.push(el.clone()),
                        }
                    }
                    let arg = self.lower_expr_from_elements(&arg_elems);

                    // If it's a known signal/port, treat as indexing
                    if self.port_map.contains_key(&name) || self.signal_map.contains_key(&name) {
                        let base = self.resolve_name(&name);
                        return HirExpression::Index(Box::new(base), Box::new(arg));
                    }

                    // Otherwise, function call
                    return HirExpression::Call(HirCallExpr {
                        function: name,
                        type_args: Vec::new(),
                        named_type_args: IndexMap::new(),
                        args: vec![arg],
                        impl_style: ImplStyle::Auto,
                    });
                }
            }
        }

        // Fallback: try to interpret as a single token
        self.lower_single_element(&elements[0])
    }

    fn try_binary_split(
        &mut self,
        elements: &[SyntaxElement],
        ops: &[SyntaxKind],
    ) -> Option<HirExpression> {
        // Find the rightmost operator at depth 0 (not inside parens)
        let mut depth = 0i32;
        let mut best_pos = None;

        for (i, el) in elements.iter().enumerate() {
            match el.kind() {
                SyntaxKind::LParen => depth += 1,
                SyntaxKind::RParen => depth -= 1,
                k if depth == 0 && ops.contains(&k) && i > 0 => {
                    best_pos = Some(i);
                }
                _ => {}
            }
        }

        let pos = best_pos?;
        let lhs_elements = &elements[..pos];
        let rhs_elements = &elements[pos + 1..];

        if lhs_elements.is_empty() || rhs_elements.is_empty() {
            return None;
        }

        let op = match elements[pos].kind() {
            SyntaxKind::AndKw => HirBinaryOp::And,
            SyntaxKind::OrKw => HirBinaryOp::Or,
            SyntaxKind::XorKw => HirBinaryOp::Xor,
            SyntaxKind::NandKw | SyntaxKind::NorKw | SyntaxKind::XnorKw => {
                let inner_op = match elements[pos].kind() {
                    SyntaxKind::NandKw => HirBinaryOp::And,
                    SyntaxKind::NorKw => HirBinaryOp::Or,
                    _ => HirBinaryOp::Xor,
                };
                let left = self.lower_expr_from_elements(lhs_elements);
                let right = self.lower_expr_from_elements(rhs_elements);
                return Some(HirExpression::Unary(HirUnaryExpr {
                    op: HirUnaryOp::Not,
                    operand: Box::new(HirExpression::Binary(HirBinaryExpr {
                        left: Box::new(left),
                        op: inner_op,
                        right: Box::new(right),
                        is_trait_op: false,
                    })),
                }));
            }
            SyntaxKind::Equal => HirBinaryOp::Equal,
            SyntaxKind::NotEqual => HirBinaryOp::NotEqual,
            SyntaxKind::LessThan => HirBinaryOp::Less,
            SyntaxKind::SignalAssign => HirBinaryOp::LessEqual, // <= in expr context
            SyntaxKind::GreaterThan => HirBinaryOp::Greater,
            SyntaxKind::GreaterEqual => HirBinaryOp::GreaterEqual,
            SyntaxKind::Plus => HirBinaryOp::Add,
            SyntaxKind::Minus => HirBinaryOp::Sub,
            SyntaxKind::Ampersand => {
                // Concatenation
                let left = self.lower_expr_from_elements(lhs_elements);
                let right = self.lower_expr_from_elements(rhs_elements);
                return Some(HirExpression::Concat(vec![left, right]));
            }
            SyntaxKind::Star => HirBinaryOp::Mul,
            SyntaxKind::Slash => HirBinaryOp::Div,
            _ => return None,
        };

        let left = self.lower_expr_from_elements(lhs_elements);
        let right = self.lower_expr_from_elements(rhs_elements);

        Some(HirExpression::Binary(HirBinaryExpr {
            left: Box::new(left),
            op,
            right: Box::new(right),
            is_trait_op: false,
        }))
    }

    fn lower_single_element(&mut self, element: &SyntaxElement) -> HirExpression {
        match element {
            SyntaxElement::Token(t) => self.lower_token_expr(t.kind(), t.text()),
            SyntaxElement::Node(n) => self.lower_syntax_node_expr(n),
        }
    }

    fn lower_token_expr(&mut self, kind: SyntaxKind, text: &str) -> HirExpression {
        match kind {
            SyntaxKind::IntLiteral => {
                let cleaned = text.replace('_', "");
                // VHDL integer literals can use scientific notation (e.g., 50e6 = 50_000_000)
                // Rust's u64::parse doesn't support this, so fall back to f64 parsing
                let val = cleaned
                    .parse::<u64>()
                    .unwrap_or_else(|_| cleaned.parse::<f64>().map(|f| f as u64).unwrap_or(0));
                HirExpression::Literal(HirLiteral::Integer(val))
            }
            SyntaxKind::CharLiteral => {
                let ch = text.trim_matches('\'').chars().next().unwrap_or('0');
                match ch {
                    '0' => HirExpression::Literal(HirLiteral::Integer(0)),
                    '1' => HirExpression::Literal(HirLiteral::Integer(1)),
                    _ => HirExpression::Literal(HirLiteral::Integer(0)),
                }
            }
            SyntaxKind::BitStringLiteral => {
                HirExpression::Literal(self.parse_bit_string_literal(text))
            }
            SyntaxKind::StringLiteral => {
                let inner = text.trim_matches('"');
                if inner.chars().all(|c| c == '0' || c == '1') {
                    let val = u64::from_str_radix(inner, 2).unwrap_or(0);
                    HirExpression::Literal(HirLiteral::Integer(val))
                } else {
                    HirExpression::Literal(HirLiteral::String(inner.to_string()))
                }
            }
            SyntaxKind::BasedLiteral => {
                let val = self.parse_based_literal(text);
                HirExpression::Literal(HirLiteral::Integer(val))
            }
            SyntaxKind::TrueKw => HirExpression::Literal(HirLiteral::Boolean(true)),
            SyntaxKind::FalseKw => HirExpression::Literal(HirLiteral::Boolean(false)),
            SyntaxKind::Ident => self.resolve_name(text),
            SyntaxKind::OthersKw => {
                // (others => '0') should have been handled at aggregate level
                HirExpression::Literal(HirLiteral::Integer(0))
            }
            _ => {
                // Try to resolve as a name
                HirExpression::Literal(HirLiteral::Integer(0))
            }
        }
    }

    fn lower_syntax_node_expr(&mut self, node: &SyntaxNode) -> HirExpression {
        match node.kind() {
            SyntaxKind::Name => self.lower_name_expr(node),
            SyntaxKind::AggregateExpr => self.lower_aggregate_expr(node),
            SyntaxKind::SubtypeIndication => {
                // Type conversion: unsigned(x), std_logic_vector(y)
                self.lower_type_conversion(node)
            }
            SyntaxKind::ExternalNameExpr => {
                // << signal .path.to.sig : type >> → hierarchical reference
                let tokens: Vec<String> = node
                    .children_with_tokens()
                    .filter_map(|el| match el {
                        SyntaxElement::Token(t)
                            if t.kind() == SyntaxKind::Ident || t.kind() == SyntaxKind::Dot =>
                        {
                            Some(t.text().to_string())
                        }
                        _ => None,
                    })
                    .collect();
                let path = tokens.join("");

                // Extract SubtypeIndication to get the declared type
                let sig_type =
                    if let Some(sti) = first_child_of_kind(node, SyntaxKind::SubtypeIndication) {
                        self.lower_subtype_indication(&sti)
                    } else {
                        HirType::Logic(1)
                    };

                // Sanitize path: replace dots with underscores for a valid signal name
                let sanitized = path.trim_start_matches('.').replace('.', "_");
                let lower_name = sanitized.to_ascii_lowercase();

                // If already registered (e.g. same external name used twice), reuse
                if let Some(sig_id) = self.signal_map.get(&lower_name) {
                    return HirExpression::Signal(*sig_id);
                }

                let id = self.alloc_signal_id();
                self.signal_map.insert(lower_name.clone(), id);
                self.name_type_map
                    .insert(lower_name.clone(), sig_type.clone());
                self.external_signals.push(HirSignal {
                    id,
                    name: lower_name,
                    comments: vec![],
                    signal_type: sig_type,
                    initial_value: None,
                    clock_domain: None,
                    span: None,
                    memory_config: None,
                    trace_config: None,
                    cdc_config: None,
                    breakpoint_config: None,
                    power_config: None,
                    safety_config: None,
                    power_domain: None,
                });
                HirExpression::Signal(id)
            }
            _ => {
                // Try to recurse into child elements
                let elements: Vec<SyntaxElement> = node
                    .children_with_tokens()
                    .filter(|el| {
                        el.kind() != SyntaxKind::Whitespace && el.kind() != SyntaxKind::Comment
                    })
                    .collect();
                self.lower_expr_from_elements(&elements)
            }
        }
    }

    fn lower_name_expr(&mut self, node: &SyntaxNode) -> HirExpression {
        let tokens: Vec<(SyntaxKind, String)> = all_token_texts(node);
        if tokens.is_empty() {
            return HirExpression::Literal(HirLiteral::Integer(0));
        }

        let (first_kind, first_text) = &tokens[0];

        // Check for builtin function calls
        match first_kind {
            SyntaxKind::RisingEdgeKw | SyntaxKind::FallingEdgeKw => {
                let func_name = if *first_kind == SyntaxKind::RisingEdgeKw {
                    "rising_edge"
                } else {
                    "falling_edge"
                };
                let args = self.extract_call_args(node);
                return HirExpression::Call(HirCallExpr {
                    function: func_name.to_string(),
                    type_args: Vec::new(),
                    named_type_args: IndexMap::new(),
                    args,
                    impl_style: ImplStyle::Auto,
                });
            }
            SyntaxKind::ToUnsignedKw | SyntaxKind::ToSignedKw => {
                let args = self.extract_call_args(node);
                return if let Some(first_arg) = args.into_iter().next() {
                    first_arg
                } else {
                    HirExpression::Literal(HirLiteral::Integer(0))
                };
            }
            SyntaxKind::ToIntegerKw | SyntaxKind::ConvIntegerKw => {
                let args = self.extract_call_args(node);
                return if let Some(first_arg) = args.into_iter().next() {
                    first_arg
                } else {
                    HirExpression::Literal(HirLiteral::Integer(0))
                };
            }
            SyntaxKind::ConvStdLogicVectorKw => {
                // conv_std_logic_vector(value, width) — cast first arg to logic vector
                let args = self.extract_call_args(node);
                let first_arg = args
                    .into_iter()
                    .next()
                    .unwrap_or(HirExpression::Literal(HirLiteral::Integer(0)));
                return HirExpression::Cast(HirCastExpr {
                    expr: Box::new(first_arg),
                    target_type: HirType::Logic(0),
                });
            }
            SyntaxKind::ResizeKw => {
                let args = self.extract_call_args(node);
                let mut it = args.into_iter();
                let signal = it
                    .next()
                    .unwrap_or(HirExpression::Literal(HirLiteral::Integer(0)));
                let width_expr = it.next();
                let target_type = match &width_expr {
                    Some(HirExpression::Literal(HirLiteral::Integer(w))) => HirType::Nat(*w as u32),
                    _ => HirType::Nat(32),
                };
                return HirExpression::Cast(HirCastExpr {
                    expr: Box::new(signal),
                    target_type,
                });
            }
            SyntaxKind::UnsignedKw
            | SyntaxKind::SignedKw
            | SyntaxKind::StdLogicVectorKw
            | SyntaxKind::StdUlogicVectorKw => {
                let first_kw = *first_kind;
                let args = self.extract_call_args(node);
                let first_arg = args
                    .into_iter()
                    .next()
                    .unwrap_or(HirExpression::Literal(HirLiteral::Integer(0)));
                let target_type = match first_kw {
                    SyntaxKind::UnsignedKw => HirType::Nat(0),
                    SyntaxKind::SignedKw => HirType::Int(0),
                    _ => HirType::Logic(0),
                };
                return HirExpression::Cast(HirCastExpr {
                    expr: Box::new(first_arg),
                    target_type,
                });
            }
            _ => {}
        }

        // Regular name resolution
        let name = first_text.to_ascii_lowercase();

        // Check for function call pattern: name(args)
        if tokens.len() > 1 && tokens.get(1).map(|(k, _)| *k) == Some(SyntaxKind::LParen) {
            // Check for slice: name(expr downto expr) or name(expr to expr)
            let has_downto = tokens.iter().any(|(k, _)| *k == SyntaxKind::DowntoKw);
            let has_to_range =
                !has_downto && tokens.iter().skip(2).any(|(k, _)| *k == SyntaxKind::ToKw);
            if has_downto || has_to_range {
                let base = self.resolve_name(&name);
                if let Some((high_expr, low_expr)) = self.extract_paren_range(node) {
                    return HirExpression::Range(
                        Box::new(base),
                        Box::new(high_expr),
                        Box::new(low_expr),
                    );
                }
            }

            // Could be function call or indexing
            let args = self.extract_call_args(node);
            let base = self.resolve_name(&name);

            // If it's a known signal/port with args, it's indexing
            if (self.port_map.contains_key(&name) || self.signal_map.contains_key(&name))
                && args.len() == 1
            {
                return HirExpression::Index(
                    Box::new(base),
                    Box::new(args.into_iter().next().unwrap()),
                );
            }

            // Check for builtin type conversion functions: std_logic(expr), unsigned(expr), etc.
            // These are VHDL type conversion function calls that should become Cast expressions
            if args.len() == 1 {
                let cast_type = match name.as_str() {
                    "unsigned" => Some(HirType::Nat(0)),
                    "signed" => Some(HirType::Int(0)),
                    "std_logic_vector" | "std_ulogic_vector" => Some(HirType::Logic(0)),
                    "std_logic" | "std_ulogic" => Some(HirType::Logic(1)),
                    _ => None,
                };
                if let Some(target_type) = cast_type {
                    return HirExpression::Cast(HirCastExpr {
                        expr: Box::new(args.into_iter().next().unwrap()),
                        target_type,
                    });
                }
            }

            // Otherwise it's a function call
            return HirExpression::Call(HirCallExpr {
                function: name,
                type_args: Vec::new(),
                named_type_args: IndexMap::new(),
                args,
                impl_style: ImplStyle::Auto,
            });
        }

        // Check for field access or qualified name: name.field or pkg.constant
        if let Some(dot_pos) = tokens.iter().position(|(k, _)| *k == SyntaxKind::Dot) {
            if let Some((_, suffix)) = tokens.get(dot_pos + 1) {
                let suffix_lower = suffix.to_ascii_lowercase();

                // Strip "work." prefix: work.pkg.name -> resolve pkg.name
                let (prefix, resolved_suffix) = if name == "work" {
                    if let Some(dot2_pos) = tokens
                        .iter()
                        .skip(dot_pos + 2)
                        .position(|(k, _)| *k == SyntaxKind::Dot)
                    {
                        let real_prefix = suffix_lower.clone();
                        let real_suffix = tokens
                            .get(dot_pos + 2 + dot2_pos + 1)
                            .map(|(_, t)| t.to_ascii_lowercase())
                            .unwrap_or_default();
                        (real_prefix, real_suffix)
                    } else {
                        // work.name — treat as simple name
                        return self.resolve_name(&suffix_lower);
                    }
                } else {
                    (name.clone(), suffix_lower)
                };

                // Qualified name resolution: pkg.constant_name
                if self.package_names.contains(&prefix) {
                    // Look up suffix in our maps
                    if let Some(const_id) = self.constant_map.get(&resolved_suffix) {
                        return HirExpression::Constant(*const_id);
                    }
                    if self.function_map.contains_key(&resolved_suffix) {
                        return HirExpression::GenericParam(resolved_suffix);
                    }
                    if self.user_types.contains_key(&resolved_suffix) {
                        return HirExpression::GenericParam(resolved_suffix);
                    }
                }

                // Regular field access
                let base = self.resolve_name(&name);
                return HirExpression::FieldAccess {
                    base: Box::new(base),
                    field: suffix.clone(),
                };
            }
        }

        // Check for tick/attribute or qualified expression: name'attribute or type'(expr)
        if let Some(tick_pos) = tokens.iter().position(|(k, _)| *k == SyntaxKind::Tick) {
            // Qualified expression: type'(expr) — Tick followed by LParen
            if tokens
                .get(tick_pos + 1)
                .map(|(k, _)| *k == SyntaxKind::LParen)
                .unwrap_or(false)
            {
                // Extract content between the parens after tick
                let inner_expr = self.extract_call_args(node);
                let expr = inner_expr
                    .into_iter()
                    .next()
                    .unwrap_or(HirExpression::Literal(HirLiteral::Integer(0)));
                // Use the type prefix to generate a Cast
                let target_type = match name.as_str() {
                    "unsigned" => Some(HirType::Nat(0)),
                    "signed" => Some(HirType::Int(0)),
                    "std_logic_vector" | "std_ulogic_vector" => Some(HirType::Logic(0)),
                    "std_logic" | "std_ulogic" => Some(HirType::Logic(1)),
                    _ => None,
                };
                return if let Some(ty) = target_type {
                    HirExpression::Cast(HirCastExpr {
                        expr: Box::new(expr),
                        target_type: ty,
                    })
                } else {
                    // Unknown type qualifier — return the inner expression directly
                    expr
                };
            }
            // For MVP: handle common attributes
            if let Some((_, attr_name)) = tokens.get(tick_pos + 1) {
                let lower_attr = attr_name.to_ascii_lowercase();
                let base = self.resolve_name(&name);
                match lower_attr.as_str() {
                    "event" => return base,
                    "high" | "low" | "length" | "left" | "right" => {
                        if let Some(ty) =
                            self.name_type_map.get(&name.to_ascii_lowercase()).cloned()
                        {
                            // For expression-based types, derive attribute from the width expression
                            // and let the pipeline's const evaluator resolve it
                            if let Some(width_expr) = Self::type_width_expr(&ty) {
                                let attr_expr = match lower_attr.as_str() {
                                    // high/left = width - 1 (downto convention)
                                    "high" | "left" => HirExpression::Binary(HirBinaryExpr {
                                        op: HirBinaryOp::Sub,
                                        left: Box::new(width_expr),
                                        right: Box::new(HirExpression::Literal(
                                            HirLiteral::Integer(1),
                                        )),
                                        is_trait_op: false,
                                    }),
                                    // low/right = 0
                                    "low" | "right" => {
                                        HirExpression::Literal(HirLiteral::Integer(0))
                                    }
                                    // length = width
                                    "length" => width_expr,
                                    _ => HirExpression::Literal(HirLiteral::Integer(0)),
                                };
                                return attr_expr;
                            }
                            // Concrete type — safe to resolve statically
                            let width = Self::type_width(&ty);
                            if width > 0 {
                                let val = match lower_attr.as_str() {
                                    "high" | "left" => width - 1,
                                    "low" | "right" => 0,
                                    "length" => width,
                                    _ => 0,
                                };
                                return HirExpression::Literal(HirLiteral::Integer(val as u64));
                            }
                        }
                        // Type not found — produce a generic param reference as best-effort
                        return HirExpression::GenericParam(format!("{}_{}", name, lower_attr));
                    }
                    "subtype" => {
                        // 'subtype is a type assertion — the signal's type is already
                        // tracked in name_type_map. Return the base expression directly.
                        return base;
                    }
                    "range" | "ascending" | "reverse_range" | "image" | "value" => {
                        return HirExpression::Call(HirCallExpr {
                            function: format!("{}_{}", name, lower_attr),
                            type_args: Vec::new(),
                            named_type_args: IndexMap::new(),
                            args: vec![base],
                            impl_style: ImplStyle::Auto,
                        });
                    }
                    _ => {
                        self.emit_warning_at(
                            format!("unsupported attribute '{}' on '{}'", lower_attr, name),
                            Some(node),
                        );
                        return base;
                    }
                }
            }
        }

        self.resolve_name(&name)
    }

    fn extract_call_args(&mut self, node: &SyntaxNode) -> Vec<HirExpression> {
        // Find content between LParen and RParen tokens
        let elements: Vec<SyntaxElement> = node.children_with_tokens().collect();
        let mut inside_parens = false;
        let mut depth = 0;
        let mut current_arg: Vec<SyntaxElement> = Vec::new();
        let mut args = Vec::new();

        for el in &elements {
            let kind = el.kind();
            match kind {
                SyntaxKind::LParen if !inside_parens => {
                    inside_parens = true;
                    depth = 1;
                }
                SyntaxKind::LParen if inside_parens => {
                    depth += 1;
                    current_arg.push(el.clone());
                }
                SyntaxKind::RParen if inside_parens => {
                    depth -= 1;
                    if depth == 0 {
                        if !current_arg.is_empty() {
                            let filtered = Self::strip_named_assoc(&current_arg);
                            if !filtered.is_empty() {
                                args.push(self.lower_expr_from_elements(&filtered));
                            }
                        }
                        break;
                    } else {
                        current_arg.push(el.clone());
                    }
                }
                SyntaxKind::Comma if inside_parens && depth == 1 => {
                    let filtered = Self::strip_named_assoc(&current_arg);
                    if !filtered.is_empty() {
                        args.push(self.lower_expr_from_elements(&filtered));
                    }
                    current_arg.clear();
                }
                SyntaxKind::Whitespace | SyntaxKind::Comment => {
                    // skip trivia
                }
                _ if inside_parens => {
                    current_arg.push(el.clone());
                }
                _ => {}
            }
        }

        args
    }

    /// Strip named association syntax (`formal => actual`) from call argument elements.
    /// If an Arrow token is found at depth 0, return only the elements after the Arrow.
    /// Otherwise, return the filtered (no-trivia) elements as-is.
    fn strip_named_assoc(elements: &[SyntaxElement]) -> Vec<SyntaxElement> {
        let mut depth = 0i32;
        let mut arrow_pos = None;
        for (i, el) in elements.iter().enumerate() {
            match el.kind() {
                SyntaxKind::LParen => depth += 1,
                SyntaxKind::RParen => depth -= 1,
                SyntaxKind::Arrow if depth == 0 => {
                    arrow_pos = Some(i);
                    break;
                }
                _ => {}
            }
        }
        let slice = if let Some(pos) = arrow_pos {
            &elements[pos + 1..]
        } else {
            elements
        };
        slice
            .iter()
            .filter(|e| e.kind() != SyntaxKind::Whitespace && e.kind() != SyntaxKind::Comment)
            .cloned()
            .collect()
    }

    fn lower_aggregate_expr(&mut self, node: &SyntaxNode) -> HirExpression {
        let children = all_children_of_kind(node, SyntaxKind::AggregateElement);

        // Check for (others => value) pattern
        if children.len() == 1 {
            let child = &children[0];
            if has_token(child, SyntaxKind::OthersKw) {
                // (others => '0') -> Literal(0), (others => '1') -> all ones
                let val_elements: Vec<SyntaxElement> = child
                    .children_with_tokens()
                    .filter(|el| {
                        el.kind() != SyntaxKind::Whitespace
                            && el.kind() != SyntaxKind::Comment
                            && el.kind() != SyntaxKind::OthersKw
                            && el.kind() != SyntaxKind::Arrow
                    })
                    .collect();
                // BUG FIX: (others => '1') must produce all-ones, not literal 1.
                // Use u64::MAX; codegen's width mask truncates to the correct width.
                if val_elements.len() == 1 {
                    if let Some(tok) = val_elements[0].as_token() {
                        let text = tok.text().to_string();
                        if text == "'1'" {
                            return HirExpression::Literal(HirLiteral::Integer(u64::MAX));
                        }
                    }
                }
                return self.lower_expr_from_elements(&val_elements);
            }
        }

        // Regular aggregate — lower to struct literal or array literal
        let mut field_inits = Vec::new();
        let mut positional = Vec::new();
        let mut others_value: Option<HirExpression> = None;

        for child in &children {
            let has_arrow = has_token(child, SyntaxKind::Arrow);
            let has_others = has_token(child, SyntaxKind::OthersKw);
            let elements: Vec<SyntaxElement> = child
                .children_with_tokens()
                .filter(|el| {
                    el.kind() != SyntaxKind::Whitespace && el.kind() != SyntaxKind::Comment
                })
                .collect();

            if has_others && has_arrow {
                // others => value — extract the default fill value
                let val_elements: Vec<SyntaxElement> = elements
                    .into_iter()
                    .filter(|el| {
                        el.kind() != SyntaxKind::OthersKw && el.kind() != SyntaxKind::Arrow
                    })
                    .collect();
                // Handle (others => '1') specially
                if val_elements.len() == 1 {
                    if let Some(tok) = val_elements[0].as_token() {
                        if tok.text() == "'1'" {
                            others_value =
                                Some(HirExpression::Literal(HirLiteral::Integer(u64::MAX)));
                            continue;
                        }
                    }
                }
                others_value = Some(self.lower_expr_from_elements(&val_elements));
            } else if has_arrow {
                // Named: key => value
                let arrow_pos = elements
                    .iter()
                    .position(|el| el.kind() == SyntaxKind::Arrow);
                if let Some(pos) = arrow_pos {
                    let key_elements = &elements[..pos];
                    let val_elements = &elements[pos + 1..];
                    let key_text = key_elements
                        .first()
                        .and_then(|el| match el {
                            SyntaxElement::Token(t) => Some(t.text().to_string()),
                            _ => None,
                        })
                        .unwrap_or_default();
                    let value = self.lower_expr_from_elements(val_elements);
                    field_inits.push(HirStructFieldInit {
                        name: key_text,
                        value,
                    });
                }
            } else {
                let expr = self.lower_expr_from_elements(&elements);
                positional.push(expr);
            }
        }

        // If we have named integer-index fields + a simple others fill ('0' or '1'),
        // compute the concrete integer value directly. This covers (0 => '1', others => '0')
        // and similar patterns without producing a StructLiteral with __others.
        if let Some(ref default_val) = others_value {
            let others_is_zero =
                matches!(default_val, HirExpression::Literal(HirLiteral::Integer(0)));
            let others_is_ones = matches!(default_val, HirExpression::Literal(HirLiteral::Integer(v)) if *v == u64::MAX);

            if (others_is_zero || others_is_ones) && !field_inits.is_empty() {
                // Check if ALL named keys parse as integer indices and values are simple literals
                let all_simple = field_inits.iter().all(|f| {
                    f.name.parse::<u64>().is_ok()
                        && matches!(f.value, HirExpression::Literal(HirLiteral::Integer(_)))
                });

                if all_simple {
                    let mut result: u64 = if others_is_zero { 0 } else { u64::MAX };
                    for f in &field_inits {
                        let index = f.name.parse::<u64>().unwrap();
                        if let HirExpression::Literal(HirLiteral::Integer(val)) = &f.value {
                            if others_is_zero && *val != 0 {
                                // others='0', field='1' → set bit
                                result |= 1u64 << index;
                            } else if others_is_ones && *val == 0 {
                                // others='1', field='0' → clear bit
                                result &= !(1u64 << index);
                            }
                        }
                    }
                    return HirExpression::Literal(HirLiteral::Integer(result));
                }
            }
        }

        // If we have an `others` default, add it as a special "__others" field
        if let Some(default_val) = others_value {
            field_inits.push(HirStructFieldInit {
                name: "__others".to_string(),
                value: default_val,
            });
        }

        if !field_inits.is_empty() {
            HirExpression::StructLiteral(HirStructLiteral {
                type_name: String::new(), // inferred from context
                generic_args: Vec::new(),
                fields: field_inits,
            })
        } else if positional.len() == 1 {
            // Single positional element without arrows = parenthesized expression,
            // not a single-element aggregate. Unwrap it.
            positional.into_iter().next().unwrap()
        } else if !positional.is_empty() {
            HirExpression::ArrayLiteral(positional)
        } else {
            HirExpression::Literal(HirLiteral::Integer(0))
        }
    }

    fn lower_type_conversion(&mut self, node: &SyntaxNode) -> HirExpression {
        // Type conversion: unsigned(x), std_logic_vector(y)
        // Extract the argument expression between LParen/RParen.
        // If the parens contain to/downto, it's a range declaration, not a conversion.
        let mut inside = false;
        let mut depth = 0i32;
        let mut arg_elements: Vec<SyntaxElement> = Vec::new();
        let mut has_range_dir = false;

        for el in node.children_with_tokens() {
            let kind = el.kind();
            match kind {
                SyntaxKind::LParen if !inside => {
                    inside = true;
                    depth = 1;
                }
                SyntaxKind::LParen => {
                    depth += 1;
                    arg_elements.push(el);
                }
                SyntaxKind::RParen if inside => {
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                    arg_elements.push(el);
                }
                SyntaxKind::ToKw | SyntaxKind::DowntoKw if inside && depth == 1 => {
                    has_range_dir = true;
                    arg_elements.push(el);
                }
                SyntaxKind::Whitespace | SyntaxKind::Comment => {}
                _ if inside => {
                    arg_elements.push(el);
                }
                _ => {}
            }
        }

        if has_range_dir || arg_elements.is_empty() {
            // It's a type range declaration (e.g. unsigned(7 downto 0)), not a conversion
            return HirExpression::Literal(HirLiteral::Integer(0));
        }

        self.lower_expr_from_elements(&arg_elements)
    }

    // ====================================================================
    // LValue lowering
    // ====================================================================

    fn lower_lvalue_from_elements(&mut self, elements: &[SyntaxElement]) -> HirLValue {
        if elements.is_empty() {
            self.emit_error_at("empty lvalue expression", None);
            return HirLValue::Signal(SignalId(0));
        }

        // If single Name node
        if elements.len() == 1 {
            if let SyntaxElement::Node(n) = &elements[0] {
                return self.lower_name_lvalue(n);
            }
            if let SyntaxElement::Token(t) = &elements[0] {
                let name = t.text().to_ascii_lowercase();
                return self.resolve_lvalue_name(&name);
            }
        }

        // Try to find the name
        let first = &elements[0];
        if let SyntaxElement::Node(n) = first {
            return self.lower_name_lvalue(n);
        }
        if let SyntaxElement::Token(t) = first {
            let name = t.text().to_ascii_lowercase();
            return self.resolve_lvalue_name(&name);
        }

        self.emit_error_at("could not resolve lvalue expression", None);
        HirLValue::Signal(SignalId(0))
    }

    fn lower_name_lvalue(&mut self, node: &SyntaxNode) -> HirLValue {
        let tokens: Vec<(SyntaxKind, String)> = all_token_texts(node);
        if tokens.is_empty() {
            self.emit_error_at("empty name in lvalue", Some(node));
            return HirLValue::Signal(SignalId(0));
        }

        let name = tokens[0].1.to_ascii_lowercase();
        let base = self.resolve_lvalue_name(&name);

        // Check for indexing or slicing: name(expr) or name(expr downto expr)
        if tokens.len() > 1 && tokens.get(1).map(|(k, _)| *k) == Some(SyntaxKind::LParen) {
            // Check if this is a range (downto/to) or a single index
            let has_downto = tokens.iter().any(|(k, _)| *k == SyntaxKind::DowntoKw);
            let has_to = tokens.iter().any(|(k, _)| *k == SyntaxKind::ToKw);

            if has_downto || has_to {
                // Slice: name(high downto low) or name(low to high)
                let range = self.extract_paren_range(node);
                if let Some((high_expr, low_expr)) = range {
                    return HirLValue::Range(Box::new(base), high_expr, low_expr);
                }
            }

            let args = self.extract_call_args(node);
            if let Some(idx) = args.into_iter().next() {
                return HirLValue::Index(Box::new(base), idx);
            }
        }

        // Check for field access: name.field
        if let Some(dot_pos) = tokens.iter().position(|(k, _)| *k == SyntaxKind::Dot) {
            if let Some((_, field_name)) = tokens.get(dot_pos + 1) {
                return HirLValue::FieldAccess {
                    base: Box::new(base),
                    field: field_name.clone(),
                };
            }
        }

        base
    }

    /// Extract a range (high_expr downto low_expr) from parenthesized content in a Name node.
    /// Returns (high_expr, low_expr) for downto, or (low_expr, high_expr) adjusted for to.
    fn extract_paren_range(&mut self, node: &SyntaxNode) -> Option<(HirExpression, HirExpression)> {
        let elements: Vec<SyntaxElement> = node.children_with_tokens().collect();

        // Collect content between outer ( and )
        let mut inside = false;
        let mut depth = 0;
        let mut content: Vec<SyntaxElement> = Vec::new();
        for el in &elements {
            let kind = el.kind();
            match kind {
                SyntaxKind::LParen if !inside => {
                    inside = true;
                    depth = 1;
                }
                SyntaxKind::LParen if inside => {
                    depth += 1;
                    content.push(el.clone());
                }
                SyntaxKind::RParen if inside => {
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                    content.push(el.clone());
                }
                _ if inside => {
                    content.push(el.clone());
                }
                _ => {}
            }
        }

        // Split on DowntoKw or ToKw
        let split_pos = content
            .iter()
            .position(|el| el.kind() == SyntaxKind::DowntoKw || el.kind() == SyntaxKind::ToKw)?;

        let left_els: Vec<_> = content[..split_pos]
            .iter()
            .filter(|e| e.kind() != SyntaxKind::Whitespace && e.kind() != SyntaxKind::Comment)
            .cloned()
            .collect();
        let right_els: Vec<_> = content[split_pos + 1..]
            .iter()
            .filter(|e| e.kind() != SyntaxKind::Whitespace && e.kind() != SyntaxKind::Comment)
            .cloned()
            .collect();

        if left_els.is_empty() || right_els.is_empty() {
            return None;
        }

        let left_expr = self.lower_expr_from_elements(&left_els);
        let right_expr = self.lower_expr_from_elements(&right_els);

        // For both downto and to, Range(base, high, low): high is left for downto
        Some((left_expr, right_expr))
    }

    // ====================================================================
    // Name resolution
    // ====================================================================

    /// Get the bit width of an HIR type with concrete bounds (for attribute resolution).
    /// Returns 0 for expression-based types — use `type_width_expr` for those.
    fn type_width(ty: &HirType) -> usize {
        match ty {
            HirType::Logic(w) | HirType::Nat(w) | HirType::Int(w) => *w as usize,
            HirType::Bool => 1,
            HirType::Struct(st) => st
                .fields
                .iter()
                .map(|f| Self::type_width(&f.field_type))
                .sum(),
            HirType::Array(inner, size) => (*size as usize) * Self::type_width(inner),
            HirType::Enum(et) => {
                let n = et.variants.len();
                if n <= 1 {
                    1
                } else {
                    (usize::BITS - (n - 1).leading_zeros()) as usize
                }
            }
            _ => 0,
        }
    }

    /// Extract the width expression from expression-based HIR types.
    /// Returns None for concrete types (use `type_width` for those).
    fn type_width_expr(ty: &HirType) -> Option<HirExpression> {
        match ty {
            HirType::LogicExpr(expr)
            | HirType::NatExpr(expr)
            | HirType::IntExpr(expr)
            | HirType::BitExpr(expr) => Some(*expr.clone()),
            _ => None,
        }
    }

    fn resolve_name(&self, name: &str) -> HirExpression {
        let lower = name.to_ascii_lowercase();
        if let Some(port_id) = self.port_map.get(&lower) {
            HirExpression::Port(*port_id)
        } else if let Some(sig_id) = self.signal_map.get(&lower) {
            HirExpression::Signal(*sig_id)
        } else if let Some(var_id) = self.variable_map.get(&lower) {
            HirExpression::Variable(*var_id)
        } else if let Some(const_id) = self.constant_map.get(&lower) {
            HirExpression::Constant(*const_id)
        } else if let Some(variant_val) = self.resolve_enum_variant(&lower) {
            variant_val
        } else {
            // Could be a generic parameter or unresolved name
            HirExpression::GenericParam(lower)
        }
    }

    /// Check if `name` is an enum variant in any user-defined enum type.
    /// Returns the variant's integer value as a literal expression.
    fn resolve_enum_variant(&self, name: &str) -> Option<HirExpression> {
        for (_type_name, hir_type) in &self.user_types {
            if let HirType::Enum(enum_def) = hir_type {
                for (index, variant) in enum_def.variants.iter().enumerate() {
                    if variant.name.to_ascii_lowercase() == name {
                        let value = if let Some(ref value_expr) = variant.value {
                            value_expr.clone()
                        } else {
                            HirExpression::Literal(HirLiteral::Integer(index as u64))
                        };
                        return Some(value);
                    }
                }
            }
        }
        None
    }

    fn resolve_lvalue_name(&mut self, name: &str) -> HirLValue {
        let lower = name.to_ascii_lowercase();
        if let Some(port_id) = self.port_map.get(&lower) {
            HirLValue::Port(*port_id)
        } else if let Some(sig_id) = self.signal_map.get(&lower) {
            HirLValue::Signal(*sig_id)
        } else if let Some(var_id) = self.variable_map.get(&lower) {
            HirLValue::Variable(*var_id)
        } else {
            self.emit_error_at(format!("unresolved signal target: '{}'", name), None);
            HirLValue::Signal(SignalId(0))
        }
    }

    // ====================================================================
    // Subtype indication
    // ====================================================================

    fn lower_subtype_indication(&mut self, node: &SyntaxNode) -> HirType {
        let type_name = self.subtype_indication_type_name(node);

        // Strip known package prefixes from dotted names: "pkg.my_type" -> "my_type"
        let resolved_name = self.strip_package_prefix(&type_name);

        // If the type name is a generic type parameter, return Custom
        if self.generic_type_params.contains(&resolved_name) {
            return HirType::Custom(resolved_name);
        }

        let range = self.subtype_indication_range(node);

        resolve_vhdl_type(
            &resolved_name,
            range.as_ref(),
            &self.builtin_scope,
            &self.user_types,
        )
        .unwrap_or_else(|| {
            self.emit_warning_at(
                format!(
                    "could not resolve type '{}', defaulting to std_logic",
                    resolved_name
                ),
                Some(node),
            );
            HirType::Logic(1)
        })
    }

    /// Strip known package prefixes from a dotted type name.
    /// E.g., "work.my_pkg.byte_t" -> "byte_t", "my_pkg.byte_t" -> "byte_t"
    fn strip_package_prefix(&self, name: &str) -> String {
        if !name.contains('.') {
            return name.to_string();
        }
        let parts: Vec<&str> = name.split('.').collect();
        // Try the last component directly
        let last = parts.last().copied().unwrap_or(name);
        // Check if stripping "work." prefix leaves a known package prefix
        let without_work: &[&str] = if parts.first() == Some(&"work") {
            &parts[1..]
        } else {
            &parts
        };
        // If the prefix (all but last) is a known package, return the last part
        if without_work.len() >= 2 {
            let pkg = without_work[..without_work.len() - 1].join(".");
            if self.package_names.contains(&pkg)
                || without_work
                    .iter()
                    .take(without_work.len() - 1)
                    .any(|p| self.package_names.contains(*p))
            {
                return last.to_string();
            }
        }
        // Fallback: if the type with just the last component resolves, use it
        if self.user_types.contains_key(last) || self.generic_type_params.contains(last) {
            return last.to_string();
        }
        // Return as-is if no prefix matched
        name.to_string()
    }

    fn subtype_indication_type_name(&self, node: &SyntaxNode) -> String {
        // Collect type name tokens including dots (e.g., "my_pkg.byte_t")
        let mut parts = Vec::new();
        let mut iter = node.children_with_tokens().peekable();
        while let Some(el) = iter.next() {
            if let SyntaxElement::Token(t) = &el {
                let kind = t.kind();
                if kind == SyntaxKind::Whitespace || kind == SyntaxKind::Comment {
                    continue;
                }
                // Map keyword tokens to type names — these are terminal (no dotted path)
                let mapped = match kind {
                    SyntaxKind::StdLogicKw => return "std_logic".to_string(),
                    SyntaxKind::StdUlogicKw => return "std_ulogic".to_string(),
                    SyntaxKind::StdLogicVectorKw => return "std_logic_vector".to_string(),
                    SyntaxKind::StdUlogicVectorKw => return "std_ulogic_vector".to_string(),
                    SyntaxKind::UnsignedKw => return "unsigned".to_string(),
                    SyntaxKind::SignedKw => return "signed".to_string(),
                    SyntaxKind::BooleanKw => return "boolean".to_string(),
                    SyntaxKind::IntegerKw => return "integer".to_string(),
                    SyntaxKind::NaturalKw => return "natural".to_string(),
                    SyntaxKind::PositiveKw => return "positive".to_string(),
                    SyntaxKind::RealKw => return "real".to_string(),
                    SyntaxKind::BitKw => return "bit".to_string(),
                    SyntaxKind::BitVectorKw => return "bit_vector".to_string(),
                    SyntaxKind::Ident => t.text().to_ascii_lowercase(),
                    SyntaxKind::Dot => {
                        // Continue accumulating dotted path
                        parts.push(".".to_string());
                        continue;
                    }
                    SyntaxKind::LParen => break, // range starts — stop
                    _ => break,
                };
                parts.push(mapped);
                // Peek ahead for Dot to continue the dotted name
                let mut peek = iter.clone();
                let next_is_dot = loop {
                    match peek.next() {
                        Some(SyntaxElement::Token(pt))
                            if pt.kind() == SyntaxKind::Whitespace
                                || pt.kind() == SyntaxKind::Comment =>
                        {
                            continue;
                        }
                        Some(SyntaxElement::Token(pt)) if pt.kind() == SyntaxKind::Dot => {
                            break true;
                        }
                        _ => break false,
                    }
                };
                if !next_is_dot {
                    break;
                }
            }
        }
        if parts.is_empty() {
            "std_logic".to_string()
        } else {
            parts.join("")
        }
    }

    fn subtype_indication_range(&mut self, node: &SyntaxNode) -> Option<RangeInfo> {
        // Look for (expr downto/to expr) or range expr to/downto expr
        let elements: Vec<SyntaxElement> = node.children_with_tokens().collect();

        let mut in_parens = false;
        let mut paren_depth = 0;
        let mut range_elements = Vec::new();

        for el in &elements {
            match el.kind() {
                SyntaxKind::LParen => {
                    if !in_parens {
                        in_parens = true;
                        paren_depth = 1;
                    } else {
                        paren_depth += 1;
                        range_elements.push(el.clone());
                    }
                }
                SyntaxKind::RParen => {
                    paren_depth -= 1;
                    if paren_depth == 0 {
                        break;
                    }
                    range_elements.push(el.clone());
                }
                SyntaxKind::RangeKw if !in_parens => {
                    in_parens = true; // treat rest as range
                }
                _ if in_parens && paren_depth >= 1 => {
                    range_elements.push(el.clone());
                }
                _ if in_parens && paren_depth == 0 => {
                    range_elements.push(el.clone());
                }
                _ => {}
            }
        }

        if range_elements.is_empty() {
            return None;
        }

        // Find to/downto
        let mut before = Vec::new();
        let mut after = Vec::new();
        let mut direction = None;

        for el in &range_elements {
            match el.kind() {
                SyntaxKind::Whitespace | SyntaxKind::Comment => {}
                SyntaxKind::ToKw => direction = Some(RangeDirection::To),
                SyntaxKind::DowntoKw => direction = Some(RangeDirection::Downto),
                _ => {
                    if direction.is_none() {
                        before.push(el.clone());
                    } else {
                        after.push(el.clone());
                    }
                }
            }
        }

        let left = self.eval_const_expr(&before);
        let right = self.eval_const_expr(&after);

        // Also lower the bound expressions as HIR for generic-dependent widths.
        // If either bound evaluated to 0 AND the elements contain non-literal tokens,
        // the width likely depends on generics and needs expression-based resolution.
        let has_non_literal = |elems: &[SyntaxElement]| -> bool {
            elems.iter().any(|el| match el {
                SyntaxElement::Token(t) => t.kind() == SyntaxKind::Ident,
                SyntaxElement::Node(n) => {
                    // Check inside Name nodes for Ident tokens (generic references)
                    n.kind() == SyntaxKind::Name
                        || n.descendants_with_tokens()
                            .any(|d| d.kind() == SyntaxKind::Ident)
                }
            })
        };
        let (left_expr, right_expr) = if has_non_literal(&before) || has_non_literal(&after) {
            let le = if !before.is_empty() {
                Some(self.lower_expr_from_elements(&before))
            } else {
                None
            };
            let re = if !after.is_empty() {
                Some(self.lower_expr_from_elements(&after))
            } else {
                None
            };
            (le, re)
        } else {
            (None, None)
        };

        Some(RangeInfo {
            left,
            right,
            direction: direction.unwrap_or(RangeDirection::To),
            left_expr,
            right_expr,
        })
    }

    /// Best-effort extraction of integer values from syntax elements for range bounds.
    /// Only handles pure literal expressions (e.g., "7", "15 - 1"). Returns 0 for
    /// anything involving identifiers or complex expressions — those are handled by
    /// `lower_expr_from_elements()` which produces HIR expressions for the pipeline.
    fn eval_const_expr(&mut self, elements: &[SyntaxElement]) -> i64 {
        if elements.is_empty() {
            return 0;
        }
        if elements.len() == 1 {
            if let SyntaxElement::Token(t) = &elements[0] {
                if t.kind() == SyntaxKind::IntLiteral {
                    let cleaned = t.text().replace('_', "");
                    return cleaned
                        .parse::<i64>()
                        .unwrap_or_else(|_| cleaned.parse::<f64>().map(|f| f as i64).unwrap_or(0));
                }
            }
        }

        // Simple evaluation for patterns like "N-1", "WIDTH-1"
        // Find minus token
        if elements.len() == 3 {
            if elements[1].kind() == SyntaxKind::Minus {
                let left = self.eval_const_expr(&elements[..1]);
                let right = self.eval_const_expr(&elements[2..]);
                return left - right;
            }
            if elements[1].kind() == SyntaxKind::Plus {
                let left = self.eval_const_expr(&elements[..1]);
                let right = self.eval_const_expr(&elements[2..]);
                return left + right;
            }
        }

        // Single node
        if let SyntaxElement::Node(n) = &elements[0] {
            let sub: Vec<SyntaxElement> = n
                .children_with_tokens()
                .filter(|el| {
                    el.kind() != SyntaxKind::Whitespace && el.kind() != SyntaxKind::Comment
                })
                .collect();
            return self.eval_const_expr(&sub);
        }

        0
    }

    // ====================================================================
    // Literal parsing helpers
    // ====================================================================

    fn parse_bit_string_literal(&self, text: &str) -> HirLiteral {
        // X"FF", B"1010", O"77"
        if text.len() < 3 {
            return HirLiteral::Integer(0);
        }
        let prefix = text.chars().next().unwrap().to_ascii_lowercase();
        let inner = text[2..text.len() - 1].replace('_', "");

        let val = match prefix {
            'x' => u64::from_str_radix(&inner, 16).unwrap_or(0),
            'b' => u64::from_str_radix(&inner, 2).unwrap_or(0),
            'o' => u64::from_str_radix(&inner, 8).unwrap_or(0),
            'd' => inner.parse::<u64>().unwrap_or(0),
            _ => 0,
        };
        HirLiteral::Integer(val)
    }

    fn parse_based_literal(&self, text: &str) -> u64 {
        // 16#FF#, 2#1010#
        let parts: Vec<&str> = text.split('#').collect();
        if parts.len() >= 2 {
            let base = parts[0].parse::<u32>().unwrap_or(10);
            let digits = parts[1].replace('_', "");
            u64::from_str_radix(&digits, base).unwrap_or(0)
        } else {
            0
        }
    }

    // ====================================================================
    // Misc helpers
    // ====================================================================

    fn extract_default_value(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        // Look for := followed by expression
        let elements: Vec<SyntaxElement> = node.children_with_tokens().collect();
        let assign_pos = elements
            .iter()
            .position(|el| el.kind() == SyntaxKind::VarAssign)?;

        let value_elements: Vec<SyntaxElement> = elements[assign_pos + 1..]
            .iter()
            .filter(|el| {
                el.kind() != SyntaxKind::Whitespace
                    && el.kind() != SyntaxKind::Comment
                    && el.kind() != SyntaxKind::Semicolon
            })
            .cloned()
            .collect();

        if value_elements.is_empty() {
            return None;
        }

        Some(self.lower_expr_from_elements(&value_elements))
    }

    fn extract_range_size(&mut self, node: &SyntaxNode) -> Option<u32> {
        // First try the subtype_indication_range path (handles parenthesized ranges)
        if let Some(ri) = self.subtype_indication_range(node) {
            return Some(ri.width());
        }
        // Fallback: handle bare ranges like `0 to 3` in DiscreteRange nodes
        let elements: Vec<SyntaxElement> = node
            .children_with_tokens()
            .filter(|el| el.kind() != SyntaxKind::Whitespace && el.kind() != SyntaxKind::Comment)
            .collect();
        let mut before = Vec::new();
        let mut after = Vec::new();
        let mut direction = None;
        for el in &elements {
            match el.kind() {
                SyntaxKind::ToKw => direction = Some(RangeDirection::To),
                SyntaxKind::DowntoKw => direction = Some(RangeDirection::Downto),
                _ => {
                    if direction.is_none() {
                        before.push(el.clone());
                    } else {
                        after.push(el.clone());
                    }
                }
            }
        }
        if let Some(dir) = direction {
            if !before.is_empty() && !after.is_empty() {
                let left = self.eval_const_expr(&before);
                let right = self.eval_const_expr(&after);
                let ri = RangeInfo {
                    left,
                    right,
                    direction: dir,
                    left_expr: None,
                    right_expr: None,
                };
                return Some(ri.width());
            }
        }
        None
    }

    fn name_first_text(&self, node: &SyntaxNode) -> Option<String> {
        node.children_with_tokens().find_map(|el| match el {
            SyntaxElement::Token(t)
                if t.kind() != SyntaxKind::Whitespace && t.kind() != SyntaxKind::Comment =>
            {
                Some(t.text().to_ascii_lowercase())
            }
            _ => None,
        })
    }

    fn lower_stmts_from_elements(&mut self, elements: &[SyntaxElement]) -> Vec<HirStatement> {
        // Collect just the Node elements for processing with exit/next awareness
        let nodes: Vec<SyntaxNode> = elements
            .iter()
            .filter_map(|el| {
                if let SyntaxElement::Node(n) = el {
                    Some(n.clone())
                } else {
                    None
                }
            })
            .collect();
        self.lower_sequential_children(&nodes)
    }
}

/// Recursively substitute generic type parameters in an HirType
fn substitute_type(ty: &HirType, subs: &IndexMap<String, HirType>) -> HirType {
    match ty {
        HirType::Custom(name) => {
            if let Some(replacement) = subs.get(name) {
                replacement.clone()
            } else {
                ty.clone()
            }
        }
        HirType::Array(elem, size) => HirType::Array(Box::new(substitute_type(elem, subs)), *size),
        HirType::Struct(st) => {
            let fields = st
                .fields
                .iter()
                .map(|f| HirStructField {
                    name: f.name.clone(),
                    field_type: substitute_type(&f.field_type, subs),
                })
                .collect();
            HirType::Struct(HirStructType {
                name: st.name.clone(),
                fields,
                packed: st.packed,
            })
        }
        HirType::Enum(et) => {
            let base = substitute_type(&et.base_type, subs);
            let variants = et
                .variants
                .iter()
                .map(|v| HirEnumVariant {
                    name: v.name.clone(),
                    value: v.value.clone(),
                    associated_data: v
                        .associated_data
                        .as_ref()
                        .map(|d| d.iter().map(|t| substitute_type(t, subs)).collect()),
                })
                .collect();
            HirType::Enum(Box::new(HirEnumType {
                name: et.name.clone(),
                variants,
                base_type: Box::new(base),
            }))
        }
        HirType::Tuple(types) => {
            HirType::Tuple(types.iter().map(|t| substitute_type(t, subs)).collect())
        }
        // All primitive types pass through unchanged
        _ => ty.clone(),
    }
}

#[derive(Clone)]
struct IfSection {
    kind: IfSectionKind,
    condition_elements: Vec<SyntaxElement>,
    body_elements: Vec<SyntaxElement>,
}

#[derive(Clone, Copy)]
enum IfSectionKind {
    If,
    Elsif,
    Else,
}

impl IfSection {
    fn new(kind: IfSectionKind) -> Self {
        Self {
            kind,
            condition_elements: Vec::new(),
            body_elements: Vec::new(),
        }
    }
}

#[derive(Clone)]
struct GenerateSection {
    kind: GenerateSectionKind,
    condition_elements: Vec<SyntaxElement>,
    body_nodes: Vec<SyntaxElement>,
}

#[derive(Clone, Copy)]
enum GenerateSectionKind {
    If,
    Elsif,
    Else,
}
