//! Shared compilation context cache for SKALP LSP
//!
//! Maintains a per-file parsed state using the SKALP frontend parser.
//! Provides symbol resolution, type information, and position mapping.

use std::collections::HashMap;

/// Cached analysis state for a source file
#[derive(Debug, Clone)]
pub struct AnalysisContext {
    /// Entity definitions: name → (line, col)
    pub entities: Vec<SymbolInfo>,
    /// Signal definitions: name → (line, col, entity_scope)
    pub signals: Vec<SymbolInfo>,
    /// Port definitions: name → (line, col, entity_scope, direction)
    pub ports: Vec<PortInfo>,
    /// Constant definitions: name → (line, col, value)
    pub constants: Vec<SymbolInfo>,
    /// Trait definitions: name → (line, col)
    pub traits: Vec<SymbolInfo>,
    /// Instance declarations: name → (line, col, entity_type)
    pub instances: Vec<InstanceInfo>,
    /// Import references: module_path → line
    pub imports: Vec<ImportInfo>,
    /// Scope map: line → containing entity name
    pub scope_map: HashMap<u32, String>,
}

#[derive(Debug, Clone)]
pub struct SymbolInfo {
    pub name: String,
    pub line: u32,
    pub column: u32,
    pub scope: Option<String>,
    pub type_str: Option<String>,
}

#[derive(Debug, Clone)]
pub struct PortInfo {
    pub name: String,
    pub line: u32,
    pub column: u32,
    pub direction: String,
    pub type_str: String,
    pub width: Option<u32>,
    pub entity: String,
}

#[derive(Debug, Clone)]
pub struct InstanceInfo {
    pub name: String,
    pub line: u32,
    pub column: u32,
    pub entity_type: String,
    pub scope: String,
}

#[derive(Debug, Clone)]
pub struct ImportInfo {
    pub module_path: String,
    pub line: u32,
}

impl AnalysisContext {
    /// Build analysis context from source text (lightweight text-based analysis)
    pub fn from_source(content: &str) -> Self {
        let mut ctx = AnalysisContext {
            entities: Vec::new(),
            signals: Vec::new(),
            ports: Vec::new(),
            constants: Vec::new(),
            traits: Vec::new(),
            instances: Vec::new(),
            imports: Vec::new(),
            scope_map: HashMap::new(),
        };

        let mut current_entity: Option<String> = None;
        let mut brace_depth = 0u32;
        let mut entity_depth = 0u32;
        let mut in_port_section = false;

        for (line_num, line) in content.lines().enumerate() {
            let line_u32 = line_num as u32;
            let trimmed = line.trim();

            // Track braces
            for ch in line.chars() {
                if ch == '{' { brace_depth += 1; }
                if ch == '}' {
                    if brace_depth > 0 { brace_depth -= 1; }
                    if brace_depth <= entity_depth && current_entity.is_some() {
                        current_entity = None;
                        in_port_section = false;
                    }
                }
            }

            // Entity declarations
            if let Some(rest) = trimmed.strip_prefix("entity ") {
                let name = rest.split(|c: char| !c.is_alphanumeric() && c != '_')
                    .next()
                    .unwrap_or("")
                    .to_string();
                if !name.is_empty() {
                    let col = line.find("entity").unwrap_or(0) as u32;
                    ctx.entities.push(SymbolInfo {
                        name: name.clone(),
                        line: line_u32,
                        column: col,
                        scope: None,
                        type_str: None,
                    });
                    current_entity = Some(name);
                    entity_depth = brace_depth;
                    in_port_section = true;
                }
            }

            // Impl blocks (enter entity scope)
            if let Some(rest) = trimmed.strip_prefix("impl ") {
                let name = rest.split(|c: char| !c.is_alphanumeric() && c != '_')
                    .next()
                    .unwrap_or("")
                    .to_string();
                if !name.is_empty() {
                    current_entity = Some(name);
                    entity_depth = brace_depth;
                    in_port_section = false;
                }
            }

            // Update scope map
            if let Some(ref ent) = current_entity {
                ctx.scope_map.insert(line_u32, ent.clone());
            }

            // Port declarations
            if (trimmed.starts_with("in ") || trimmed.starts_with("out ") || trimmed.starts_with("inout "))
                && current_entity.is_some()
            {
                let direction;
                let rest;
                if let Some(r) = trimmed.strip_prefix("inout ") {
                    direction = "inout";
                    rest = r;
                } else if let Some(r) = trimmed.strip_prefix("in ") {
                    direction = "in";
                    rest = r;
                } else if let Some(r) = trimmed.strip_prefix("out ") {
                    direction = "out";
                    rest = r;
                } else {
                    continue;
                }

                let parts: Vec<&str> = rest.splitn(2, ':').collect();
                if parts.len() >= 1 {
                    let name = parts[0].trim().trim_end_matches(|c: char| !c.is_alphanumeric() && c != '_');
                    let type_str = if parts.len() > 1 {
                        parts[1].trim().trim_end_matches(|c: char| c == ';' || c == ',' || c == '{').trim().to_string()
                    } else {
                        String::new()
                    };

                    let col = line.find(name).unwrap_or(0) as u32;
                    ctx.ports.push(PortInfo {
                        name: name.to_string(),
                        line: line_u32,
                        column: col,
                        direction: direction.to_string(),
                        type_str: type_str.clone(),
                        width: infer_width(&type_str),
                        entity: current_entity.clone().unwrap_or_default(),
                    });
                }
            }

            // Signal declarations
            if let Some(rest) = trimmed.strip_prefix("signal ") {
                let parts: Vec<&str> = rest.splitn(2, ':').collect();
                if !parts.is_empty() {
                    let name = parts[0].trim();
                    let type_str = if parts.len() > 1 {
                        Some(parts[1].trim().trim_end_matches(';').trim().to_string())
                    } else {
                        None
                    };
                    let col = line.find("signal").unwrap_or(0) as u32 + 7;
                    ctx.signals.push(SymbolInfo {
                        name: name.to_string(),
                        line: line_u32,
                        column: col,
                        scope: current_entity.clone(),
                        type_str,
                    });
                }
            }

            // Constants
            if let Some(rest) = trimmed.strip_prefix("const ") {
                let parts: Vec<&str> = rest.splitn(2, ':').collect();
                if !parts.is_empty() {
                    let name = parts[0].trim().split('=').next().unwrap_or("").trim();
                    let col = line.find("const").unwrap_or(0) as u32 + 6;
                    ctx.constants.push(SymbolInfo {
                        name: name.to_string(),
                        line: line_u32,
                        column: col,
                        scope: current_entity.clone(),
                        type_str: None,
                    });
                }
            }

            // Trait declarations
            if let Some(rest) = trimmed.strip_prefix("trait ") {
                let name = rest.split(|c: char| !c.is_alphanumeric() && c != '_')
                    .next()
                    .unwrap_or("")
                    .to_string();
                if !name.is_empty() {
                    let col = line.find("trait").unwrap_or(0) as u32;
                    ctx.traits.push(SymbolInfo {
                        name,
                        line: line_u32,
                        column: col,
                        scope: None,
                        type_str: None,
                    });
                }
            }

            // Instance declarations: let name = Entity(...) or inst name: Entity
            if trimmed.starts_with("let ") && trimmed.contains('=') && current_entity.is_some() {
                let after_let = trimmed.strip_prefix("let ").unwrap();
                let parts: Vec<&str> = after_let.splitn(2, '=').collect();
                if parts.len() == 2 {
                    let name = parts[0].trim();
                    let rhs = parts[1].trim();
                    // Check for Entity(...) pattern
                    if let Some(paren_pos) = rhs.find('(') {
                        let entity_type = rhs[..paren_pos].trim();
                        if !entity_type.is_empty() && entity_type.chars().next().unwrap().is_uppercase() {
                            let col = line.find(name).unwrap_or(0) as u32;
                            ctx.instances.push(InstanceInfo {
                                name: name.to_string(),
                                line: line_u32,
                                column: col,
                                entity_type: entity_type.to_string(),
                                scope: current_entity.clone().unwrap_or_default(),
                            });
                        }
                    }
                }
            }

            // Import/use declarations
            if let Some(rest) = trimmed.strip_prefix("use ") {
                let path = rest.trim_end_matches(';').trim();
                ctx.imports.push(ImportInfo {
                    module_path: path.to_string(),
                    line: line_u32,
                });
            }
        }

        ctx
    }

    /// Find definition of a symbol by name
    pub fn find_definition(&self, name: &str) -> Option<(u32, u32)> {
        // Search entities
        if let Some(s) = self.entities.iter().find(|s| s.name == name) {
            return Some((s.line, s.column));
        }
        // Search signals
        if let Some(s) = self.signals.iter().find(|s| s.name == name) {
            return Some((s.line, s.column));
        }
        // Search ports
        if let Some(p) = self.ports.iter().find(|p| p.name == name) {
            return Some((p.line, p.column));
        }
        // Search constants
        if let Some(s) = self.constants.iter().find(|s| s.name == name) {
            return Some((s.line, s.column));
        }
        // Search traits
        if let Some(s) = self.traits.iter().find(|s| s.name == name) {
            return Some((s.line, s.column));
        }
        // Search instances
        if let Some(i) = self.instances.iter().find(|i| i.name == name) {
            return Some((i.line, i.column));
        }
        None
    }

    /// Find all references to a symbol
    pub fn find_references_in(&self, name: &str, content: &str) -> Vec<(u32, u32, u32)> {
        let mut refs = Vec::new();
        for (line_num, line) in content.lines().enumerate() {
            let mut search_from = 0;
            while let Some(pos) = line[search_from..].find(name) {
                let abs_pos = search_from + pos;
                // Verify it's a word boundary
                let before_ok = abs_pos == 0
                    || !line.as_bytes()[abs_pos - 1].is_ascii_alphanumeric()
                    && line.as_bytes()[abs_pos - 1] != b'_';
                let after_pos = abs_pos + name.len();
                let after_ok = after_pos >= line.len()
                    || !line.as_bytes()[after_pos].is_ascii_alphanumeric()
                    && line.as_bytes()[after_pos] != b'_';

                if before_ok && after_ok {
                    refs.push((line_num as u32, abs_pos as u32, name.len() as u32));
                }
                search_from = abs_pos + 1;
            }
        }
        refs
    }

    /// Get hover information for a symbol
    pub fn hover_info(&self, name: &str) -> Option<String> {
        // Entity
        if let Some(e) = self.entities.iter().find(|s| s.name == name) {
            let ports: Vec<&PortInfo> = self.ports.iter().filter(|p| p.entity == e.name).collect();
            let mut info = format!("**entity** `{}`\n\n", e.name);
            if !ports.is_empty() {
                info.push_str("Ports:\n");
                for p in &ports {
                    info.push_str(&format!("- `{} {}: {}`\n", p.direction, p.name, p.type_str));
                }
            }
            return Some(info);
        }

        // Port
        if let Some(p) = self.ports.iter().find(|p| p.name == name) {
            let width_str = p.width.map(|w| format!(" ({} bits)", w)).unwrap_or_default();
            return Some(format!(
                "**{} port** `{}`: `{}`{}\n\nIn entity `{}`",
                p.direction, p.name, p.type_str, width_str, p.entity
            ));
        }

        // Signal
        if let Some(s) = self.signals.iter().find(|s| s.name == name) {
            let type_str = s.type_str.as_deref().unwrap_or("unknown");
            let scope = s.scope.as_deref().unwrap_or("global");
            return Some(format!("**signal** `{}`: `{}`\n\nIn `{}`", s.name, type_str, scope));
        }

        // Constant
        if let Some(c) = self.constants.iter().find(|s| s.name == name) {
            return Some(format!("**const** `{}`", c.name));
        }

        // Trait
        if let Some(t) = self.traits.iter().find(|s| s.name == name) {
            return Some(format!("**trait** `{}`", t.name));
        }

        // Instance
        if let Some(i) = self.instances.iter().find(|i| i.name == name) {
            return Some(format!("**instance** `{}`: `{}`\n\nIn `{}`", i.name, i.entity_type, i.scope));
        }

        None
    }
}

fn infer_width(type_str: &str) -> Option<u32> {
    let t = type_str.trim();
    if t == "bit" || t == "bool" || t == "clock" || t == "reset" { return Some(1); }
    if t == "fp32" { return Some(32); }
    if t == "fp16" { return Some(16); }
    if t == "fp64" { return Some(64); }

    // bit<N>, nat<N>, int<N>
    for prefix in &["bit<", "nat<", "int<"] {
        if let Some(rest) = t.strip_prefix(prefix) {
            if let Some(num_str) = rest.strip_suffix('>') {
                return num_str.parse().ok();
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_analysis_context() {
        let source = r#"
entity Counter {
    in clk: clock;
    in reset: bit;
    out count: nat<8>;
}

impl Counter {
    signal internal: nat<8>;
    on(clk.rise) {
        count <= count + 1;
    }
}
"#;
        let ctx = AnalysisContext::from_source(source);
        assert_eq!(ctx.entities.len(), 1);
        assert_eq!(ctx.entities[0].name, "Counter");
        assert_eq!(ctx.ports.len(), 3);
        assert_eq!(ctx.signals.len(), 1);
        assert_eq!(ctx.signals[0].name, "internal");
    }

    #[test]
    fn test_find_definition() {
        let source = "entity Foo {\n  in x: bit;\n  out y: bit;\n}\nimpl Foo {\n  signal z: nat<4>;\n}";
        let ctx = AnalysisContext::from_source(source);
        assert!(ctx.find_definition("Foo").is_some());
        assert!(ctx.find_definition("x").is_some());
        assert!(ctx.find_definition("z").is_some());
        assert!(ctx.find_definition("nonexistent").is_none());
    }
}
