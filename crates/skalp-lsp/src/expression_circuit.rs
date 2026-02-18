use serde::Serialize;
use skalp_frontend::parse::parse;
use skalp_frontend::syntax::{SyntaxKind, SyntaxNode, SyntaxNodeExt};

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ExpressionCircuitData {
    target_name: String,
    nodes: Vec<CircuitNode>,
    wires: Vec<CircuitWire>,
    inputs: Vec<CircuitInput>,
    source_range: [u32; 2],
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct CircuitNode {
    id: String,
    #[serde(rename = "type")]
    node_type: String,
    label: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    input_labels: Option<Vec<String>>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct CircuitWire {
    id: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    label: Option<String>,
    from_node: String,
    to_node: String,
    to_port: u32,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct CircuitInput {
    id: String,
    name: String,
    is_constant: bool,
    /// "clock", "reset", or null — derived from entity port type declarations
    #[serde(skip_serializing_if = "Option::is_none")]
    role: Option<String>,
}

struct CircuitBuilder {
    node_counter: u32,
    wire_counter: u32,
    input_counter: u32,
    nodes: Vec<CircuitNode>,
    wires: Vec<CircuitWire>,
    inputs: Vec<CircuitInput>,
    input_map: std::collections::HashMap<String, String>,
}

impl CircuitBuilder {
    fn new() -> Self {
        Self {
            node_counter: 0,
            wire_counter: 0,
            input_counter: 0,
            nodes: Vec::new(),
            wires: Vec::new(),
            inputs: Vec::new(),
            input_map: std::collections::HashMap::new(),
        }
    }

    fn add_node(
        &mut self,
        node_type: &str,
        label: &str,
        input_labels: Option<Vec<String>>,
    ) -> String {
        let id = format!("n{}", self.node_counter);
        self.node_counter += 1;
        self.nodes.push(CircuitNode {
            id: id.clone(),
            node_type: node_type.to_string(),
            label: label.to_string(),
            input_labels,
        });
        id
    }

    fn add_wire(&mut self, from_node: &str, to_node: &str, to_port: u32, label: Option<&str>) {
        let id = format!("w{}", self.wire_counter);
        self.wire_counter += 1;
        self.wires.push(CircuitWire {
            id,
            label: label.map(|s| s.to_string()),
            from_node: from_node.to_string(),
            to_node: to_node.to_string(),
            to_port,
        });
    }

    fn add_input(&mut self, name: &str) -> String {
        let trimmed = name.trim();
        if let Some(existing) = self.input_map.get(trimmed) {
            return existing.clone();
        }
        let id = format!("i{}", self.input_counter);
        self.input_counter += 1;
        let is_constant = is_constant_text(trimmed);
        self.inputs.push(CircuitInput {
            id: id.clone(),
            name: trimmed.to_string(),
            is_constant,
            role: None,
        });
        self.input_map.insert(trimmed.to_string(), id.clone());
        id
    }

    fn set_input_role(&mut self, id: &str, role: &str) {
        if let Some(input) = self.inputs.iter_mut().find(|i| i.id == id) {
            input.role = Some(role.to_string());
        }
    }
}

fn is_constant_text(text: &str) -> bool {
    if text == "true" || text == "false" {
        return true;
    }
    if text.starts_with("0x") || text.starts_with("0X") || text.starts_with("0b") || text.starts_with("0B") {
        return true;
    }
    // Numeric: digits possibly with dots, fp suffix, etc.
    if text.starts_with(|c: char| c.is_ascii_digit()) {
        return true;
    }
    false
}

fn operator_to_gate(op_text: &str) -> (&str, &str) {
    match op_text {
        "&&" => ("and", "&&"),
        "||" => ("or", "||"),
        "&" => ("and", "&"),
        "|" => ("or", "|"),
        "^" => ("xor", "^"),
        "+" => ("add", "+"),
        "-" => ("sub", "-"),
        "*" => ("mul", "\u{00d7}"),
        "/" => ("div", "\u{00f7}"),
        "%" => ("div", "%"),
        "<<" => ("shl", "<<"),
        ">>" => ("shr", ">>"),
        "==" => ("cmp", "=="),
        "!=" => ("cmp", "!="),
        "<" => ("cmp", "<"),
        "<=" => ("cmp", "<="),
        ">" => ("cmp", ">"),
        ">=" => ("cmp", ">="),
        _ => ("func", op_text),
    }
}

/// Main entry point: given source code and cursor position, return circuit data.
pub fn get_expression_circuit(
    source: &str,
    line: u32,
    _column: u32,
) -> Option<ExpressionCircuitData> {
    let tree = parse(source);

    // Build a mapping from tree TextSize offsets to source line numbers.
    // The parser strips whitespace, so tree offsets don't match source byte offsets.
    let tree_offset_to_source_line = build_offset_to_line_map(source, &tree);

    // Find the statement whose tokens fall on the cursor line
    let (stmt_node, target_name) =
        find_statement_on_line(&tree, line, &tree_offset_to_source_line)?;

    // Walk the statement RHS (handles flat BinaryExpr chains + compound IdentExpr.FieldExpr)
    let mut builder = CircuitBuilder::new();
    let root_id = walk_statement_rhs(&stmt_node, &mut builder)?;

    // If inside an on() block, wrap the expression with a DFF/ADFF node
    if let Some(clock_text) = find_enclosing_event_clock(&stmt_node) {
        // Check if this on() block has an async reset trigger
        let async_reset = find_enclosing_event_block(&stmt_node)
            .and_then(|eb| find_async_reset_in_event(&eb));

        let (dff_type, dff_label, dff_inputs) = if async_reset.is_some() {
            ("adff", "ADFF", vec!["D".into(), "CLK".into(), "RST".into()])
        } else {
            ("dff", "DFF", vec!["D".into(), "CLK".into()])
        };

        let dff_id = builder.add_node(dff_type, dff_label, Some(dff_inputs));
        builder.add_wire(&root_id, &dff_id, 0, Some("D"));

        let clk_id = builder.add_input(&clock_text);
        builder.set_input_role(&clk_id, "clock");
        builder.add_wire(&clk_id, &dff_id, 1, Some("CLK"));

        // Wire async reset input if present
        if let Some((rst_signal, rst_edge)) = async_reset {
            let rst_text = format!("{}.{}", rst_signal, rst_edge);
            let rst_id = builder.add_input(&rst_text);
            builder.set_input_role(&rst_id, "reset");
            builder.add_wire(&rst_id, &dff_id, 2, Some("RST"));
        }

        // Rename any input matching the target to show it's the registered feedback
        for input in &mut builder.inputs {
            if input.name == target_name {
                input.name = format!("{} (Q)", target_name);
            }
        }
    }

    // Tag inputs with roles from entity port type declarations (clock/reset)
    let port_roles = find_enclosing_entity_port_roles(&stmt_node);
    for input in &mut builder.inputs {
        if input.role.is_some() {
            continue; // already tagged (e.g., DFF clock)
        }
        // Match input name against port names
        // Strip " (Q)" suffix and ".rising"/".falling" edge suffix
        let base_name = input.name.strip_suffix(" (Q)").unwrap_or(&input.name);
        let base_name = base_name
            .strip_suffix(".rising")
            .or_else(|| base_name.strip_suffix(".falling"))
            .unwrap_or(base_name);
        if let Some(role) = port_roles.get(base_name) {
            input.role = Some(role.clone());
        }
    }

    // Compute source range from the statement's tokens
    let (start_line, end_line) =
        node_source_line_range(&stmt_node, &tree_offset_to_source_line);

    Some(ExpressionCircuitData {
        target_name,
        nodes: builder.nodes,
        wires: builder.wires,
        inputs: builder.inputs,
        source_range: [start_line, end_line],
    })
}

/// Build a map from tree TextSize offsets to source line numbers.
///
/// The parser strips whitespace, so we walk all tokens in order and find each
/// token's text in the source (advancing a cursor). This gives us the real
/// source position for each tree offset.
fn build_offset_to_line_map(
    source: &str,
    tree: &SyntaxNode,
) -> std::collections::HashMap<u32, u32> {
    let mut map = std::collections::HashMap::new();
    let mut source_pos = 0usize;
    let mut current_line = 0u32;

    // Walk tokens in tree order
    for event in tree.preorder_with_tokens() {
        if let rowan::WalkEvent::Enter(rowan::NodeOrToken::Token(token)) = event {
            let token_text = token.text();
            let tree_offset: u32 = token.text_range().start().into();

            // Advance source_pos past whitespace/comments to find this token
            while source_pos < source.len() {
                if source[source_pos..].starts_with(token_text) {
                    map.insert(tree_offset, current_line);
                    break;
                }
                // Skip one char (handles multi-byte UTF-8)
                let ch = source[source_pos..].chars().next().unwrap();
                if ch == '\n' {
                    current_line += 1;
                }
                source_pos += ch.len_utf8();
            }

            // Advance past the token text in source
            for ch in token_text.chars() {
                if ch == '\n' {
                    current_line += 1;
                }
                source_pos += ch.len_utf8();
            }
        }
    }

    map
}

/// Get the source line number for a tree node by looking up its first token.
fn node_source_line(
    node: &SyntaxNode,
    map: &std::collections::HashMap<u32, u32>,
) -> Option<u32> {
    // Find first token in this node
    for event in node.preorder_with_tokens() {
        if let rowan::WalkEvent::Enter(rowan::NodeOrToken::Token(token)) = event {
            let offset: u32 = token.text_range().start().into();
            return map.get(&offset).copied();
        }
    }
    None
}

/// Get source line range (start_line, end_line) for a node.
fn node_source_line_range(
    node: &SyntaxNode,
    map: &std::collections::HashMap<u32, u32>,
) -> (u32, u32) {
    let mut min_line = u32::MAX;
    let mut max_line = 0u32;
    for event in node.preorder_with_tokens() {
        if let rowan::WalkEvent::Enter(rowan::NodeOrToken::Token(token)) = event {
            let offset: u32 = token.text_range().start().into();
            if let Some(&line) = map.get(&offset) {
                min_line = min_line.min(line);
                max_line = max_line.max(line);
            }
        }
    }
    if min_line == u32::MAX {
        (0, 0)
    } else {
        (min_line, max_line)
    }
}

/// Find the best statement node on the given cursor line.
fn find_statement_on_line(
    root: &SyntaxNode,
    cursor_line: u32,
    map: &std::collections::HashMap<u32, u32>,
) -> Option<(SyntaxNode, String)> {
    let mut best: Option<(SyntaxNode, String)> = None;

    for node in root.descendants() {
        match node.kind() {
            SyntaxKind::AssignmentStmt => {
                let (start, end) = node_source_line_range(&node, map);
                if cursor_line >= start && cursor_line <= end {
                    if let Some(target) = extract_assignment_target(&node) {
                        best = Some((node.clone(), target));
                    }
                }
            }
            SyntaxKind::LetStmt => {
                let (start, end) = node_source_line_range(&node, map);
                if cursor_line >= start && cursor_line <= end {
                    if let Some(target) = extract_let_target(&node) {
                        best = Some((node.clone(), target));
                    }
                }
            }
            SyntaxKind::IfStmt | SyntaxKind::MatchStmt => {
                let (start, end) = node_source_line_range(&node, map);
                if cursor_line >= start && cursor_line <= end {
                    if is_inside_on_block(&node) {
                        let targets = find_assigned_targets_in(&node);
                        let target_name = if targets.is_empty() {
                            "?".to_string()
                        } else {
                            targets.join(", ")
                        };
                        best = Some((node.clone(), target_name));
                    }
                }
            }
            _ => {}
        }
    }

    best
}

fn extract_assignment_target(node: &SyntaxNode) -> Option<String> {
    // AssignmentStmt: first child expressions before the '=' token
    // Collect text of everything before Assign token
    let mut parts = Vec::new();
    for child in node.children_with_tokens() {
        match child {
            rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::Assign => break,
            rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::Whitespace => {}
            rowan::NodeOrToken::Token(t) => parts.push(t.text().to_string()),
            rowan::NodeOrToken::Node(n) => parts.push(n.text().to_string()),
        }
    }
    let target = parts.join("").trim().to_string();
    if target.is_empty() {
        None
    } else {
        Some(target)
    }
}

fn extract_let_target(node: &SyntaxNode) -> Option<String> {
    // LetStmt: 'let' IDENT '=' expr
    for child in node.children_with_tokens() {
        if let rowan::NodeOrToken::Token(t) = child {
            if t.kind() == SyntaxKind::Ident {
                return Some(t.text().to_string());
            }
        }
    }
    None
}

fn is_inside_on_block(node: &SyntaxNode) -> bool {
    let mut parent = node.parent();
    while let Some(p) = parent {
        // OnBlock typically starts as a statement with `on` keyword, or a block inside one
        // Check for the OnKw token in ancestors
        for child in p.children_with_tokens() {
            if let rowan::NodeOrToken::Token(t) = &child {
                if t.kind() == SyntaxKind::OnKw {
                    return true;
                }
            }
        }
        parent = p.parent();
    }
    false
}

/// Find the enclosing entity declaration and extract port roles from type annotations.
/// The statement may be inside an `impl Foo { ... }` block, so we find the impl block's
/// entity name, then search the tree root for the corresponding `entity Foo { ... }`.
/// Returns a map of port_name → role ("clock" or "reset").
fn find_enclosing_entity_port_roles(
    node: &SyntaxNode,
) -> std::collections::HashMap<String, String> {
    let mut roles = std::collections::HashMap::new();

    // Walk up to find the enclosing EntityDecl or ImplBlock
    let mut entity_node = None;
    let mut parent = node.parent();
    while let Some(p) = parent {
        if p.kind() == SyntaxKind::EntityDecl {
            entity_node = Some(p.clone());
            break;
        }
        if p.kind() == SyntaxKind::ImplBlock {
            // Get entity name from impl block (first Ident token)
            let impl_name = p.children_with_tokens().find_map(|c| {
                if let rowan::NodeOrToken::Token(t) = c {
                    if t.kind() == SyntaxKind::Ident {
                        return Some(t.text().to_string());
                    }
                }
                None
            });
            // Find the corresponding EntityDecl at the tree root
            if let Some(name) = impl_name {
                let root = {
                    let mut n = p.clone();
                    while let Some(pp) = n.parent() {
                        n = pp;
                    }
                    n
                };
                for child in root.children() {
                    if child.kind() == SyntaxKind::EntityDecl {
                        let entity_name = child.children_with_tokens().find_map(|c| {
                            if let rowan::NodeOrToken::Token(t) = c {
                                if t.kind() == SyntaxKind::Ident {
                                    return Some(t.text().to_string());
                                }
                            }
                            None
                        });
                        if entity_name.as_deref() == Some(&name) {
                            entity_node = Some(child);
                            break;
                        }
                    }
                }
            }
            break;
        }
        parent = p.parent();
    }

    if let Some(entity) = entity_node {
        extract_port_roles_from_entity(&entity, &mut roles);
    }
    roles
}

fn extract_port_roles_from_entity(
    entity: &SyntaxNode,
    roles: &mut std::collections::HashMap<String, String>,
) {
    for desc in entity.descendants() {
        if desc.kind() == SyntaxKind::PortDecl {
            let mut port_name = String::new();
            let mut port_role = None;
            for child in desc.children_with_tokens() {
                match &child {
                    rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::Ident => {
                        if port_name.is_empty() {
                            port_name = t.text().to_string();
                        }
                    }
                    rowan::NodeOrToken::Node(n) if n.kind() == SyntaxKind::TypeAnnotation => {
                        for tc in n.descendants() {
                            if tc.kind() == SyntaxKind::ClockType {
                                port_role = Some("clock".to_string());
                                break;
                            }
                            if tc.kind() == SyntaxKind::ResetType {
                                port_role = Some("reset".to_string());
                                break;
                            }
                        }
                    }
                    _ => {}
                }
            }
            if !port_name.is_empty() {
                if let Some(role) = port_role {
                    roles.insert(port_name, role);
                }
            }
        }
    }
}

/// Find the enclosing EventBlock and extract the clock expression text (e.g., "clk.rising").
fn find_enclosing_event_clock(node: &SyntaxNode) -> Option<String> {
    find_enclosing_event_block(node).and_then(|eb| extract_event_clock(&eb))
}

/// Find the enclosing EventBlock node.
fn find_enclosing_event_block(node: &SyntaxNode) -> Option<SyntaxNode> {
    let mut parent = node.parent();
    while let Some(p) = parent {
        if p.kind() == SyntaxKind::EventBlock {
            return Some(p);
        }
        parent = p.parent();
    }
    None
}

fn extract_event_clock(event_block: &SyntaxNode) -> Option<String> {
    // Find EventTrigger to get the clock signal name
    let mut clock_name = String::new();
    for desc in event_block.descendants() {
        if desc.kind() == SyntaxKind::EventTrigger {
            for c in desc.children_with_tokens() {
                if let rowan::NodeOrToken::Token(t) = c {
                    if t.kind() == SyntaxKind::Ident {
                        clock_name = t.text().to_string();
                    }
                }
            }
            break;
        }
    }

    if clock_name.is_empty() {
        return None;
    }

    // Find edge type: "rising" or "falling" appears as IdentExpr in the EventBlock
    let edge = find_edge_type(event_block);
    Some(format!("{}.{}", clock_name, edge))
}

fn find_edge_type(event_block: &SyntaxNode) -> String {
    for desc in event_block.descendants() {
        if desc.kind() == SyntaxKind::IdentExpr {
            let text = desc.text().to_string();
            if text == "rising" || text == "falling" {
                return text;
            }
        }
    }
    "rising".to_string()
}

/// Check whether the EventBlock has an async reset trigger (active/inactive keyword).
/// Returns Some((reset_signal_name, edge_keyword)) if found.
///
/// Parser structure: `on(clk.rise, rst.active)` produces:
///   EventBlock → EventTriggerList → EventTrigger[Ident("clk"), Dot, EdgeType[RiseKw]]
///                                   EventTrigger[Ident("rst"), Dot, EdgeType[ActiveKw]]
fn find_async_reset_in_event(event_block: &SyntaxNode) -> Option<(String, String)> {
    for desc in event_block.descendants() {
        if desc.kind() == SyntaxKind::EventTrigger {
            let mut signal_name = String::new();
            let mut edge_kw = None;

            for child in desc.children_with_tokens() {
                if let rowan::NodeOrToken::Token(t) = &child {
                    if t.kind() == SyntaxKind::Ident {
                        signal_name = t.text().to_string();
                    }
                }
                if let rowan::NodeOrToken::Node(n) = &child {
                    if n.kind() == SyntaxKind::EdgeType {
                        // Check the keyword token inside EdgeType
                        for et_child in n.children_with_tokens() {
                            if let rowan::NodeOrToken::Token(t) = &et_child {
                                if t.kind() == SyntaxKind::ActiveKw {
                                    edge_kw = Some("active".to_string());
                                } else if t.kind() == SyntaxKind::InactiveKw {
                                    edge_kw = Some("inactive".to_string());
                                }
                            }
                        }
                    }
                }
            }

            if let Some(kw) = edge_kw {
                if !signal_name.is_empty() {
                    return Some((signal_name, kw));
                }
            }
        }
    }

    None
}

fn find_assigned_targets_in(node: &SyntaxNode) -> Vec<String> {
    let mut targets = Vec::new();
    for desc in node.descendants() {
        if desc.kind() == SyntaxKind::AssignmentStmt {
            if let Some(target) = extract_assignment_target(&desc) {
                if !targets.contains(&target) {
                    targets.push(target);
                }
            }
        }
    }
    targets
}

/// Walk the RHS of a statement, handling flat BinaryExpr chains that the parser
/// produces for chained operators like `a | b | c`.
fn walk_statement_rhs(stmt: &SyntaxNode, builder: &mut CircuitBuilder) -> Option<String> {
    match stmt.kind() {
        SyntaxKind::AssignmentStmt | SyntaxKind::LetStmt => {
            // Collect ALL expression children after the '=' token
            let mut found_eq = false;
            let mut rhs_children: Vec<SyntaxNode> = Vec::new();
            for child in stmt.children_with_tokens() {
                if let rowan::NodeOrToken::Token(t) = &child {
                    if t.kind() == SyntaxKind::Assign {
                        found_eq = true;
                        continue;
                    }
                }
                if found_eq {
                    if let rowan::NodeOrToken::Node(n) = child {
                        rhs_children.push(n);
                    }
                }
            }
            walk_expression_chain(&rhs_children, builder)
        }
        SyntaxKind::IfStmt | SyntaxKind::MatchStmt => {
            walk_expr(stmt, builder)
        }
        _ => None,
    }
}

/// Process a chain of expression siblings. The skalp parser produces flat
/// BinaryExpr siblings for chained operators (e.g., `a | b | c` becomes
/// [BinaryExpr(a | b), BinaryExpr(| c)] as siblings rather than nested).
fn walk_expression_chain(children: &[SyntaxNode], builder: &mut CircuitBuilder) -> Option<String> {
    if children.is_empty() {
        return None;
    }
    if children.len() == 1 {
        return walk_expr(&children[0], builder);
    }

    // Multiple children: first is a full expression, subsequent BinaryExprs
    // are continuations with just operator + right operand
    let mut result_id = walk_expr(&children[0], builder)?;

    for child in &children[1..] {
        if child.kind() == SyntaxKind::BinaryExpr {
            // Continuation BinaryExpr: has operator token + right operand nodes
            let mut op_text: Option<String> = None;
            let mut operand_nodes: Vec<SyntaxNode> = Vec::new();

            for c in child.children_with_tokens() {
                match c {
                    rowan::NodeOrToken::Token(t) if is_operator_token(t.kind()) => {
                        if op_text.is_none() {
                            op_text = Some(t.text().to_string());
                        }
                    }
                    rowan::NodeOrToken::Node(n) => operand_nodes.push(n),
                    _ => {}
                }
            }

            let op = op_text.unwrap_or_else(|| "?".to_string());
            let (gate_type, gate_label) = operator_to_gate(&op);
            let node_id = builder.add_node(gate_type, gate_label, None);

            builder.add_wire(&result_id, &node_id, 0, None);

            // The right operand may be a compound IdentExpr+FieldExpr
            let operand_groups = group_into_operands(&operand_nodes);
            if let Some(right_id) = operand_groups
                .first()
                .and_then(|ops| walk_compound_operand(ops, builder))
            {
                builder.add_wire(&right_id, &node_id, 1, None);
            }

            result_id = node_id;
        } else {
            // Non-BinaryExpr sibling — walk it directly
            if let Some(id) = walk_expr(child, builder) {
                result_id = id;
            }
        }
    }

    Some(result_id)
}

/// Group a list of child nodes into operands, combining IdentExpr+FieldExpr
/// (and IdentExpr+IndexExpr) pairs that the parser produces for `a.b` or `a[i]`.
fn group_into_operands(nodes: &[SyntaxNode]) -> Vec<Vec<SyntaxNode>> {
    let mut groups: Vec<Vec<SyntaxNode>> = Vec::new();
    let mut i = 0;
    while i < nodes.len() {
        if nodes[i].kind() == SyntaxKind::IdentExpr {
            let mut group = vec![nodes[i].clone()];
            // Consume following FieldExpr / IndexExpr postfix nodes
            while i + 1 < nodes.len()
                && matches!(
                    nodes[i + 1].kind(),
                    SyntaxKind::FieldExpr | SyntaxKind::IndexExpr
                )
            {
                i += 1;
                group.push(nodes[i].clone());
            }
            groups.push(group);
        } else {
            groups.push(vec![nodes[i].clone()]);
        }
        i += 1;
    }
    groups
}

/// Walk a compound operand: IdentExpr+FieldExpr pair → single input "a.b",
/// or a single expression node.
fn walk_compound_operand(nodes: &[SyntaxNode], builder: &mut CircuitBuilder) -> Option<String> {
    if nodes.is_empty() {
        return None;
    }

    // IdentExpr followed by FieldExpr(s): combine into "a.b.c"
    if nodes.len() >= 2
        && nodes[0].kind() == SyntaxKind::IdentExpr
        && nodes[1].kind() == SyntaxKind::FieldExpr
    {
        let mut name = nodes[0].text().to_string().trim().to_string();
        for n in &nodes[1..] {
            if matches!(n.kind(), SyntaxKind::FieldExpr | SyntaxKind::IndexExpr) {
                name.push_str(&n.text().to_string().trim().to_string());
            }
        }
        return Some(builder.add_input(&name));
    }

    // Single node: walk normally
    walk_expr(&nodes[0], builder)
}

/// Recursively walk a syntax expression node and build the circuit graph.
/// Returns the id of the root element (node or input) for wiring.
fn walk_expr(node: &SyntaxNode, builder: &mut CircuitBuilder) -> Option<String> {
    match node.kind() {
        SyntaxKind::BinaryExpr => walk_binary(node, builder),
        SyntaxKind::UnaryExpr => walk_unary(node, builder),
        SyntaxKind::IfExpr | SyntaxKind::IfStmt => walk_if(node, builder),
        SyntaxKind::MatchExpr | SyntaxKind::MatchStmt => walk_match(node, builder),
        SyntaxKind::TernaryExpr => walk_ternary(node, builder),
        SyntaxKind::CastExpr => walk_cast(node, builder),
        SyntaxKind::ConcatExpr => walk_concat(node, builder),
        SyntaxKind::ReplicateExpr => walk_replicate(node, builder),
        SyntaxKind::IndexExpr => walk_index(node, builder),
        SyntaxKind::CallExpr => walk_call(node, builder),
        SyntaxKind::ParenExpr => walk_paren(node, builder),
        SyntaxKind::BlockExpr => walk_block(node, builder),
        SyntaxKind::FieldExpr => walk_field(node, builder),
        SyntaxKind::LiteralExpr => {
            let text = node.text().to_string().trim().to_string();
            Some(builder.add_input(&text))
        }
        SyntaxKind::IdentExpr => {
            let text = node.text().to_string().trim().to_string();
            Some(builder.add_input(&text))
        }
        SyntaxKind::PathExpr => {
            let text = node.text().to_string().trim().to_string();
            Some(builder.add_input(&text))
        }
        _ => {
            // For statement nodes that contain expression children, try walking the first expr child
            for child in node.children() {
                if is_expression_kind(child.kind()) {
                    return walk_expr(&child, builder);
                }
            }
            // Last resort: treat as input with the node text
            let text = node.text().to_string().trim().to_string();
            if text.is_empty() {
                None
            } else {
                Some(builder.add_input(&text))
            }
        }
    }
}

fn is_expression_kind(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::BinaryExpr
            | SyntaxKind::UnaryExpr
            | SyntaxKind::IfExpr
            | SyntaxKind::MatchExpr
            | SyntaxKind::TernaryExpr
            | SyntaxKind::CastExpr
            | SyntaxKind::ConcatExpr
            | SyntaxKind::ReplicateExpr
            | SyntaxKind::IndexExpr
            | SyntaxKind::CallExpr
            | SyntaxKind::ParenExpr
            | SyntaxKind::BlockExpr
            | SyntaxKind::FieldExpr
            | SyntaxKind::LiteralExpr
            | SyntaxKind::IdentExpr
            | SyntaxKind::PathExpr
            | SyntaxKind::ArrayLiteral
            | SyntaxKind::TupleExpr
    )
}

fn walk_binary(node: &SyntaxNode, builder: &mut CircuitBuilder) -> Option<String> {
    // BinaryExpr children may include compound IdentExpr+FieldExpr pairs
    // (e.g., `prot_faults.ov | prot_faults.uv` has 4 child nodes, not 2)
    let mut child_nodes: Vec<SyntaxNode> = Vec::new();
    let mut op_text: Option<String> = None;

    for child in node.children_with_tokens() {
        match child {
            rowan::NodeOrToken::Node(n) => child_nodes.push(n),
            rowan::NodeOrToken::Token(t) => {
                if op_text.is_none() && is_operator_token(t.kind()) {
                    op_text = Some(t.text().to_string());
                }
            }
        }
    }

    // Group children into operands (combining IdentExpr+FieldExpr pairs)
    let operands = group_into_operands(&child_nodes);

    if operands.len() < 2 {
        // Single operand: this is a continuation BinaryExpr (just right operand)
        // or a degenerate case — walk the compound operand
        return operands
            .first()
            .and_then(|ops| walk_compound_operand(ops, builder));
    }

    let op = op_text.unwrap_or_else(|| "?".to_string());
    let (gate_type, gate_label) = operator_to_gate(&op);
    let node_id = builder.add_node(gate_type, gate_label, None);

    if let Some(left_id) = walk_compound_operand(&operands[0], builder) {
        builder.add_wire(&left_id, &node_id, 0, None);
    }
    if let Some(right_id) = walk_compound_operand(&operands[1], builder) {
        builder.add_wire(&right_id, &node_id, 1, None);
    }

    Some(node_id)
}

fn is_operator_token(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::Plus
            | SyntaxKind::Minus
            | SyntaxKind::Star
            | SyntaxKind::Slash
            | SyntaxKind::Percent
            | SyntaxKind::Amp
            | SyntaxKind::Pipe
            | SyntaxKind::Caret
            | SyntaxKind::AmpAmp
            | SyntaxKind::PipePipe
            | SyntaxKind::Shl
            | SyntaxKind::Shr
            | SyntaxKind::Eq
            | SyntaxKind::Neq
            | SyntaxKind::Lt
            | SyntaxKind::Le
            | SyntaxKind::Gt
            | SyntaxKind::Ge
            | SyntaxKind::Bang
            | SyntaxKind::Tilde
    )
}

fn walk_unary(node: &SyntaxNode, builder: &mut CircuitBuilder) -> Option<String> {
    let mut op_text: Option<String> = None;
    let mut operand: Option<SyntaxNode> = None;

    for child in node.children_with_tokens() {
        match child {
            rowan::NodeOrToken::Token(t) => {
                if op_text.is_none() && is_operator_token(t.kind()) {
                    op_text = Some(t.text().to_string());
                }
            }
            rowan::NodeOrToken::Node(n) => {
                if operand.is_none() {
                    operand = Some(n);
                }
            }
        }
    }

    let op = op_text.unwrap_or_else(|| "?".to_string());
    let (gate_type, gate_label) = match op.as_str() {
        "!" => ("not", "!"),
        "~" => ("bitnot", "~"),
        "-" => ("neg", "NEG"),
        _ => ("func", op.as_str()),
    };
    let node_id = builder.add_node(gate_type, gate_label, None);

    if let Some(child) = operand {
        if let Some(child_id) = walk_expr(&child, builder) {
            builder.add_wire(&child_id, &node_id, 0, None);
        }
    }

    Some(node_id)
}

fn walk_if(node: &SyntaxNode, builder: &mut CircuitBuilder) -> Option<String> {
    // IfExpr/IfStmt: if COND { THEN } else { ELSE }
    // Children structure varies, but typically:
    //   IfKw, condition_expr, BlockExpr(then), [ElseKw, BlockExpr(else)|IfExpr(else-if)]

    let mut condition: Option<SyntaxNode> = None;
    let mut then_block: Option<SyntaxNode> = None;
    let mut else_block: Option<SyntaxNode> = None;
    let mut found_else = false;
    let mut found_if = false;

    for child in node.children_with_tokens() {
        match child {
            rowan::NodeOrToken::Token(t) => {
                if t.kind() == SyntaxKind::IfKw {
                    found_if = true;
                } else if t.kind() == SyntaxKind::ElseKw {
                    found_else = true;
                }
            }
            rowan::NodeOrToken::Node(n) => {
                if !found_if {
                    continue;
                }
                if found_else {
                    else_block = Some(n);
                    found_else = false;
                } else if condition.is_none() {
                    condition = Some(n);
                } else if then_block.is_none() {
                    then_block = Some(n);
                } else {
                    // Could be else block without explicit else keyword in some parse structures
                    else_block = Some(n);
                }
            }
        }
    }

    let has_else = else_block.is_some();
    let labels = if has_else {
        Some(vec!["sel".into(), "d1".into(), "d0".into()])
    } else {
        Some(vec!["sel".into(), "d1".into()])
    };

    let node_id = builder.add_node("mux", "MUX", labels);

    if let Some(cond) = condition {
        if let Some(sel_id) = walk_expr(&cond, builder) {
            builder.add_wire(&sel_id, &node_id, 0, Some("sel"));
        }
    }
    if let Some(then_b) = then_block {
        if let Some(d1_id) = walk_expr(&then_b, builder) {
            builder.add_wire(&d1_id, &node_id, 1, Some("d1"));
        }
    }
    if let Some(else_b) = else_block {
        if let Some(d0_id) = walk_expr(&else_b, builder) {
            builder.add_wire(&d0_id, &node_id, 2, Some("d0"));
        }
    }

    Some(node_id)
}

fn walk_match(node: &SyntaxNode, builder: &mut CircuitBuilder) -> Option<String> {
    // MatchExpr/MatchStmt: match EXPR { arm1, arm2, ... }
    // Find the match expression and the arm list
    let mut match_expr: Option<SyntaxNode> = None;
    let mut arm_list: Option<SyntaxNode> = None;

    let mut found_match = false;
    for child in node.children_with_tokens() {
        match child {
            rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::MatchKw => {
                found_match = true;
            }
            rowan::NodeOrToken::Node(n) if found_match => {
                if n.kind() == SyntaxKind::MatchArmList {
                    arm_list = Some(n);
                } else if match_expr.is_none() {
                    match_expr = Some(n);
                }
            }
            _ => {}
        }
    }

    let match_input_id = match_expr.as_ref().and_then(|e| walk_expr(e, builder));

    // Collect arms
    let arms: Vec<SyntaxNode> = arm_list
        .map(|al| al.children_of_kind(SyntaxKind::MatchArm))
        .unwrap_or_default();

    if arms.is_empty() {
        return match_input_id;
    }

    // Build chain of CMP+MUX (reverse order for cascading)
    let mut result_id: Option<String> = None;

    for (i, arm) in arms.iter().enumerate().rev() {
        let (pattern, value) = extract_match_arm_parts(arm);

        let val_id = value.and_then(|v| walk_expr(&v, builder));
        let Some(val_id) = val_id else { continue };

        let is_last = i == arms.len() - 1;
        let is_wildcard = pattern
            .as_ref()
            .map(|p| p.text().to_string().trim() == "_")
            .unwrap_or(false);

        if is_last && is_wildcard {
            // Default arm
            result_id = Some(val_id);
        } else {
            // CMP node
            let cmp_id = builder.add_node("cmp", "==", None);
            if let Some(ref mi) = match_input_id {
                builder.add_wire(mi, &cmp_id, 0, None);
            }
            if let Some(pat) = &pattern {
                let pat_text = pat.text().to_string().trim().to_string();
                let pat_id = builder.add_input(&pat_text);
                builder.add_wire(&pat_id, &cmp_id, 1, None);
            }

            // MUX node
            let mux_id = builder.add_node(
                "mux",
                "MUX",
                Some(vec!["sel".into(), "d1".into(), "d0".into()]),
            );
            builder.add_wire(&cmp_id, &mux_id, 0, Some("sel"));
            builder.add_wire(&val_id, &mux_id, 1, Some("d1"));
            if let Some(ref prev) = result_id {
                builder.add_wire(prev, &mux_id, 2, Some("d0"));
            }

            result_id = Some(mux_id);
        }
    }

    result_id
}

fn extract_match_arm_parts(arm: &SyntaxNode) -> (Option<SyntaxNode>, Option<SyntaxNode>) {
    // MatchArm: pattern => expr
    let mut pattern: Option<SyntaxNode> = None;
    let mut value: Option<SyntaxNode> = None;
    let mut found_arrow = false;

    for child in arm.children_with_tokens() {
        match child {
            rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::FatArrow => {
                found_arrow = true;
            }
            rowan::NodeOrToken::Node(n) => {
                if found_arrow {
                    value = Some(n);
                } else {
                    pattern = Some(n);
                }
            }
            _ => {}
        }
    }

    (pattern, value)
}

fn walk_ternary(node: &SyntaxNode, builder: &mut CircuitBuilder) -> Option<String> {
    // TernaryExpr: cond ? then : else
    let exprs: Vec<SyntaxNode> = node.children().collect();
    let node_id = builder.add_node(
        "mux",
        "MUX",
        Some(vec!["sel".into(), "d1".into(), "d0".into()]),
    );

    if let Some(cond) = exprs.first() {
        if let Some(sel_id) = walk_expr(cond, builder) {
            builder.add_wire(&sel_id, &node_id, 0, Some("sel"));
        }
    }
    if let Some(then_e) = exprs.get(1) {
        if let Some(d1_id) = walk_expr(then_e, builder) {
            builder.add_wire(&d1_id, &node_id, 1, Some("d1"));
        }
    }
    if let Some(else_e) = exprs.get(2) {
        if let Some(d0_id) = walk_expr(else_e, builder) {
            builder.add_wire(&d0_id, &node_id, 2, Some("d0"));
        }
    }

    Some(node_id)
}

fn walk_cast(node: &SyntaxNode, builder: &mut CircuitBuilder) -> Option<String> {
    // CastExpr: expr as Type
    let mut expr_node: Option<SyntaxNode> = None;
    let mut type_text = String::new();
    let mut found_as = false;

    for child in node.children_with_tokens() {
        match child {
            rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::AsKw => {
                found_as = true;
            }
            rowan::NodeOrToken::Node(n) => {
                if found_as {
                    type_text = n.text().to_string().trim().to_string();
                } else if expr_node.is_none() {
                    expr_node = Some(n);
                }
            }
            _ => {}
        }
    }

    let label = format!("as {}", type_text);
    let node_id = builder.add_node("cast", &label, None);

    if let Some(expr) = expr_node {
        if let Some(child_id) = walk_expr(&expr, builder) {
            builder.add_wire(&child_id, &node_id, 0, None);
        }
    }

    Some(node_id)
}

fn walk_concat(node: &SyntaxNode, builder: &mut CircuitBuilder) -> Option<String> {
    // ConcatExpr: {a, b, c}
    let children: Vec<SyntaxNode> = node
        .children()
        .filter(|c| is_expression_kind(c.kind()))
        .collect();
    let labels: Vec<String> = (0..children.len()).map(|i| format!("{}", i)).collect();
    let node_id = builder.add_node("concat", "CONCAT", Some(labels));

    for (i, child) in children.iter().enumerate() {
        if let Some(child_id) = walk_expr(child, builder) {
            builder.add_wire(&child_id, &node_id, i as u32, None);
        }
    }

    Some(node_id)
}

fn walk_replicate(node: &SyntaxNode, builder: &mut CircuitBuilder) -> Option<String> {
    // ReplicateExpr: {expr; N}
    let mut expr_node: Option<SyntaxNode> = None;
    let mut count = String::new();

    for child in node.children_with_tokens() {
        match child {
            rowan::NodeOrToken::Node(n) => {
                if expr_node.is_none() {
                    expr_node = Some(n);
                } else {
                    // Second child might be the count
                    count = n.text().to_string().trim().to_string();
                }
            }
            rowan::NodeOrToken::Token(t) => {
                if t.kind() == SyntaxKind::IntLiteral {
                    count = t.text().to_string();
                }
            }
        }
    }

    let label = format!("REP x{}", count);
    let node_id = builder.add_node("replicate", &label, None);

    if let Some(expr) = expr_node {
        if let Some(child_id) = walk_expr(&expr, builder) {
            builder.add_wire(&child_id, &node_id, 0, None);
        }
    }

    Some(node_id)
}

fn walk_index(node: &SyntaxNode, builder: &mut CircuitBuilder) -> Option<String> {
    // IndexExpr: expr[index]
    // Children: base_expr, LBracket, index_expr, RBracket
    let mut base: Option<SyntaxNode> = None;
    let mut index_text = String::new();

    for child in node.children_with_tokens() {
        match child {
            rowan::NodeOrToken::Node(n) => {
                if base.is_none() {
                    base = Some(n);
                } else {
                    // This is the index expression
                    index_text = n.text().to_string().trim().to_string();
                }
            }
            _ => {}
        }
    }

    // Build the index text with brackets
    let label = format!("[{}]", index_text);
    let node_id = builder.add_node("bitselect", &label, None);

    if let Some(b) = base {
        if let Some(base_id) = walk_expr(&b, builder) {
            builder.add_wire(&base_id, &node_id, 0, None);
        }
    }

    Some(node_id)
}

fn walk_call(node: &SyntaxNode, builder: &mut CircuitBuilder) -> Option<String> {
    // CallExpr: name(arg1, arg2, ...)
    // The function name is typically an IdentExpr or PathExpr child, followed by arg expressions
    let mut func_name = String::new();
    let mut args: Vec<SyntaxNode> = Vec::new();
    let mut found_name = false;

    for child in node.children() {
        if !found_name {
            // First child is the function name/path
            func_name = child.text().to_string().trim().to_string();
            found_name = true;
        } else {
            // Remaining children are arguments
            if is_expression_kind(child.kind()) {
                args.push(child);
            }
        }
    }

    let labels: Vec<String> = (0..args.len()).map(|i| format!("in{}", i)).collect();
    let node_id = builder.add_node("func", &func_name, Some(labels));

    for (i, arg) in args.iter().enumerate() {
        if let Some(arg_id) = walk_expr(arg, builder) {
            builder.add_wire(&arg_id, &node_id, i as u32, None);
        }
    }

    Some(node_id)
}

fn walk_paren(node: &SyntaxNode, builder: &mut CircuitBuilder) -> Option<String> {
    // ParenExpr: (expr) — unwrap
    for child in node.children() {
        if is_expression_kind(child.kind()) {
            return walk_expr(&child, builder);
        }
    }
    None
}

fn walk_block(node: &SyntaxNode, builder: &mut CircuitBuilder) -> Option<String> {
    // BlockExpr: { stmts; result_expr }
    // Collect trailing expression children (may be a flat BinaryExpr chain)
    let mut trailing_exprs: Vec<SyntaxNode> = Vec::new();
    for child in node.children() {
        let kind = child.kind();
        if is_expression_kind(kind) {
            trailing_exprs.push(child);
        } else if kind == SyntaxKind::ExprStmt {
            // Unwrap ExprStmt
            trailing_exprs.clear(); // Reset: statements before aren't the result
            for inner in child.children() {
                if is_expression_kind(inner.kind()) {
                    trailing_exprs.push(inner);
                }
            }
        } else {
            // Non-expression child (e.g., LetStmt) — reset trailing list
            trailing_exprs.clear();
        }
    }
    walk_expression_chain(&trailing_exprs, builder)
}

fn walk_field(node: &SyntaxNode, builder: &mut CircuitBuilder) -> Option<String> {
    // FieldExpr: expr.field — treat as a single input name
    let text = node.text().to_string().trim().to_string();
    Some(builder.add_input(&text))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn dump_tree(node: &SyntaxNode, depth: usize) {
        let indent = "  ".repeat(depth);
        let text_preview = if node.children().next().is_none() {
            format!(" {:?}", node.text().to_string())
        } else {
            String::new()
        };
        eprintln!("{}{:?} [{:?}]{}", indent, node.kind(), node.text_range(), text_preview);
        for child in node.children_with_tokens() {
            match child {
                rowan::NodeOrToken::Token(t) => {
                    let tindent = "  ".repeat(depth + 1);
                    eprintln!("{}tok {:?} {:?}", tindent, t.kind(), t.text());
                }
                rowan::NodeOrToken::Node(n) => dump_tree(&n, depth + 1),
            }
        }
    }

    #[test]
    fn test_on_block_dff_wrapping() {
        let source = r#"entity Foo {
    in clk: clock
    out counter: nat[8]
}
impl Foo {
    on (clk.rising) {
        counter = counter + 1
    }
}"#;
        let result = get_expression_circuit(source, 6, 10);
        assert!(result.is_some(), "Expected circuit for assignment inside on-block");
        let data = result.unwrap();
        assert_eq!(data.target_name, "counter");
        // 2 nodes: ADD gate + DFF
        assert_eq!(data.nodes.len(), 2);
        assert!(data.nodes.iter().any(|n| n.node_type == "add"), "Expected ADD gate");
        let dff = data.nodes.iter().find(|n| n.node_type == "dff").expect("Expected DFF node");
        assert_eq!(dff.label, "DFF");
        assert_eq!(dff.input_labels.as_ref().unwrap(), &["D", "CLK"]);
        // 3 inputs: counter (Q), 1, clk.rising
        assert_eq!(data.inputs.len(), 3);
        assert!(data.inputs.iter().any(|i| i.name == "counter (Q)"), "Expected feedback input renamed");
        assert!(data.inputs.iter().any(|i| i.name == "1" && i.is_constant));
        let clk_input = data.inputs.iter().find(|i| i.name == "clk.rising").expect("Expected clk input");
        assert_eq!(clk_input.role.as_deref(), Some("clock"), "Clock input should have role=clock");
        // Non-clock inputs should not have a role
        assert!(data.inputs.iter().filter(|i| i.name != "clk.rising").all(|i| i.role.is_none()));
        // Wires: 2 for ADD (counter+1), 1 from ADD→DFF D, 1 from clk→DFF CLK = 4
        assert_eq!(data.wires.len(), 4);
    }

    #[test]
    fn test_on_block_adff_wrapping_async_reset() {
        let source = r#"entity Foo {
    in clk: clock
    in rst: reset(active_high)
    in data: nat[8]
    out result: nat[8]
}
impl Foo {
    signal state: nat[8]
    on (clk.rise, rst.active) {
        if rst {
            state = 0
        } else {
            state = data
        }
    }
    result = state
}"#;
        // Cursor on line 12 (state = data inside else)
        let result = get_expression_circuit(source, 12, 10);
        assert!(result.is_some(), "Expected circuit for assignment inside async reset on-block");
        let data = result.unwrap();
        // Should have an ADFF node (not DFF)
        let adff = data.nodes.iter().find(|n| n.node_type == "adff");
        assert!(adff.is_some(), "Expected ADFF node for async reset on-block, got: {:?}",
            data.nodes.iter().map(|n| &n.node_type).collect::<Vec<_>>());
        let adff = adff.unwrap();
        assert_eq!(adff.label, "ADFF");
        assert_eq!(adff.input_labels.as_ref().unwrap(), &["D", "CLK", "RST"]);
        // Should have a reset input with role=reset
        let rst_input = data.inputs.iter().find(|i| i.name.contains("active"));
        assert!(rst_input.is_some(), "Expected rst.active input, got: {:?}",
            data.inputs.iter().map(|i| &i.name).collect::<Vec<_>>());
        assert_eq!(rst_input.unwrap().role.as_deref(), Some("reset"));
        // Clock input should still be present
        assert!(data.inputs.iter().any(|i| i.role.as_deref() == Some("clock")));
    }

    #[test]
    fn test_port_roles_clock_and_reset() {
        let source = r#"entity Foo {
    in clk: clock
    in rst: reset(active_high)
    in enable: bit
    out q: bit
}
impl Foo {
    q = enable & !rst
}"#;
        let result = get_expression_circuit(source, 7, 8);
        assert!(result.is_some());
        let data = result.unwrap();
        // rst input should have role=reset
        let rst_input = data.inputs.iter().find(|i| i.name == "rst").expect("Expected rst input");
        assert_eq!(rst_input.role.as_deref(), Some("reset"));
        // enable input should have no role
        let en_input = data.inputs.iter().find(|i| i.name == "enable").expect("Expected enable input");
        assert!(en_input.role.is_none());
    }

    #[test]
    fn test_chained_or_with_fields() {
        let source = r#"entity Foo {
    in prot_faults: ProtFaults
    in bms_fault_flag: bit
    in bms_timeout: bit
    in lockstep_fault: bit
    out any_fault_flag: bit
}
impl Foo {
    any_fault_flag = prot_faults.ov | prot_faults.uv | prot_faults.oc | prot_faults.ot |
                     prot_faults.desat | bms_fault_flag | bms_timeout | lockstep_fault
}"#;
        let result = get_expression_circuit(source, 8, 20);
        assert!(result.is_some(), "Expected circuit data");
        let data = result.unwrap();
        assert_eq!(data.target_name, "any_fault_flag");
        // 7 OR gates for 8-input chain
        assert_eq!(data.nodes.len(), 7, "Expected 7 OR gates for 8-input OR chain");
        assert!(data.nodes.iter().all(|n| n.node_type == "or"));
        // 8 distinct inputs with proper field names
        assert_eq!(data.inputs.len(), 8, "Expected 8 distinct inputs");
        assert!(data.inputs.iter().any(|i| i.name == "prot_faults.ov"));
        assert!(data.inputs.iter().any(|i| i.name == "prot_faults.desat"));
        assert!(data.inputs.iter().any(|i| i.name == "lockstep_fault"));
        // 14 wires (7 gates × 2 inputs each)
        assert_eq!(data.wires.len(), 14);
    }

    #[test]
    fn test_simple_binary() {
        let source = "entity Foo { out x: bit[8]; }\nimpl Foo { x = a + b; }";
        let result = get_expression_circuit(source, 1, 15);
        assert!(result.is_some(), "Expected circuit data for 'x = a + b'");
        let data = result.unwrap();
        assert_eq!(data.target_name, "x");
        assert_eq!(data.nodes.len(), 1); // one add gate
        assert_eq!(data.nodes[0].node_type, "add");
        assert_eq!(data.inputs.len(), 2); // a and b
    }

    #[test]
    fn test_no_expression_on_blank() {
        let source = "entity Foo { out x: bit[8]; }\n\nimpl Foo { x = a + b; }";
        let result = get_expression_circuit(source, 1, 0);
        assert!(result.is_none());
    }

    #[test]
    fn test_constant_detection() {
        assert!(is_constant_text("42"));
        assert!(is_constant_text("0xFF"));
        assert!(is_constant_text("0b1010"));
        assert!(is_constant_text("3.14fp32"));
        assert!(is_constant_text("true"));
        assert!(is_constant_text("false"));
        assert!(!is_constant_text("my_signal"));
    }

    #[test]
    fn test_chained_or() {
        let source =
            "entity Foo { out x: bit; }\nimpl Foo { x = a | b | c | d; }";
        let result = get_expression_circuit(source, 1, 15);
        assert!(result.is_some());
        let data = result.unwrap();
        assert_eq!(data.target_name, "x");
        // At least one OR gate and multiple inputs
        assert!(!data.nodes.is_empty());
        assert!(data.nodes.iter().any(|n| n.node_type == "or"));
        assert!(data.inputs.len() >= 2);
    }

    #[test]
    fn test_cast_expression() {
        let source =
            "entity Foo { out x: nat[32]; }\nimpl Foo { x = counter as nat[32]; }";
        let result = get_expression_circuit(source, 1, 15);
        assert!(result.is_some());
        let data = result.unwrap();
        assert_eq!(data.target_name, "x");
        assert_eq!(data.nodes.len(), 1);
        assert_eq!(data.nodes[0].node_type, "cast");
        assert!(data.nodes[0].label.contains("nat"));
    }

    #[test]
    fn test_source_range() {
        let source = "entity Foo { out x: bit[8]; }\nimpl Foo {\n    x = a + b;\n}";
        let result = get_expression_circuit(source, 2, 8);
        assert!(result.is_some());
        let data = result.unwrap();
        assert_eq!(data.source_range[0], 2); // assignment is on line 2
        assert_eq!(data.source_range[1], 2); // single line
    }

    // --- Integration tests with real SKALP patterns ---

    #[test]
    fn test_if_else_expression() {
        let source = r#"entity Foo {
    in temp_pri: int[16]
    in temp_sec: int[16]
    out temp_max: int[16]
}
impl Foo {
    temp_max = if temp_pri > temp_sec { temp_pri } else { temp_sec }
}"#;
        // Cursor on the assignment line (line 6)
        let result = get_expression_circuit(source, 6, 10);
        assert!(result.is_some(), "Expected circuit for if/else expression");
        let data = result.unwrap();
        assert_eq!(data.target_name, "temp_max");
        // Should have a MUX node and a CMP node
        assert!(data.nodes.iter().any(|n| n.node_type == "mux"), "Expected MUX node");
        assert!(data.nodes.iter().any(|n| n.node_type == "cmp"), "Expected CMP node");
        // temp_pri and temp_sec as inputs
        assert!(data.inputs.iter().any(|i| i.name == "temp_pri"));
        assert!(data.inputs.iter().any(|i| i.name == "temp_sec"));
    }

    #[test]
    fn test_multiline_if_else() {
        let source = r#"entity Foo {
    in progress: nat[32]
    out limit: int[10]
}
impl Foo {
    limit = if progress > 1023 {
        1023
    } else {
        progress as int[10]
    }
}"#;
        // Cursor on line 7 (inside the else block)
        let result = get_expression_circuit(source, 7, 8);
        assert!(result.is_some(), "Expected circuit for multiline if/else, cursor in else block");
        let data = result.unwrap();
        assert_eq!(data.target_name, "limit");
        assert!(data.nodes.iter().any(|n| n.node_type == "mux"), "Expected MUX");
        // Source range should span multiple lines
        assert!(
            data.source_range[1] > data.source_range[0],
            "Expected multi-line source range, got {:?}",
            data.source_range
        );
    }

    #[test]
    fn test_complex_arithmetic_with_casts() {
        let source = r#"entity Foo {
    in v_bat_mv: int[16]
    in i_bat_ma: int[16]
    out power_mw: int[32]
}
impl Foo {
    power_mw = ((v_bat_mv as int[32]) * (i_bat_ma as int[32])) / 1000;
}"#;
        let result = get_expression_circuit(source, 6, 15);
        assert!(result.is_some(), "Expected circuit for complex arithmetic");
        let data = result.unwrap();
        assert_eq!(data.target_name, "power_mw");
        // Should have: 2 CAST nodes, 1 MUL node, 1 DIV node
        let casts = data.nodes.iter().filter(|n| n.node_type == "cast").count();
        let muls = data.nodes.iter().filter(|n| n.node_type == "mul").count();
        let divs = data.nodes.iter().filter(|n| n.node_type == "div").count();
        assert!(casts >= 1, "Expected CAST nodes, got {}", casts);
        assert_eq!(muls, 1, "Expected 1 MUL node");
        assert_eq!(divs, 1, "Expected 1 DIV node");
        // 1000 should be a constant input
        assert!(data.inputs.iter().any(|i| i.name == "1000" && i.is_constant));
    }

    #[test]
    fn test_real_file_battery_dcdc() {
        // Test against an actual file from the sangam codebase
        let path = "/Users/girivs/src/design/sangam/src/battery_dcdc/main.sk";
        let source = match std::fs::read_to_string(path) {
            Ok(s) => s,
            Err(_) => return, // Skip if file not available
        };
        // Find a line with an assignment
        for (line_num, line) in source.lines().enumerate() {
            let trimmed = line.trim();
            if trimmed.contains(" = ") && !trimmed.starts_with("//") && !trimmed.starts_with("const") {
                let result = get_expression_circuit(&source, line_num as u32, 10);
                if let Some(data) = result {
                    assert!(!data.target_name.is_empty(), "Target name should not be empty on line {}", line_num);
                    // At minimum we should get either nodes or inputs
                    assert!(
                        !data.nodes.is_empty() || !data.inputs.is_empty(),
                        "Expected some circuit elements on line {}: {}",
                        line_num, trimmed
                    );
                    return; // One success is enough
                }
            }
        }
        // If we get here, we didn't find any parseable assignment — that's OK for this test
    }
}
