//! Shared compilation context cache for SKALP LSP
//!
//! Maintains a per-file parsed state using the SKALP frontend parser.
//! Provides symbol resolution, type information, and position mapping.

use std::collections::HashMap;
use std::path::Path;

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
    /// Generic parameters: name → (line, col, type, default_value, entity)
    pub generics: Vec<GenericInfo>,
    /// Type aliases: name → (line, col, underlying_type)
    pub type_aliases: Vec<SymbolInfo>,
    /// Import references: module_path → line
    pub imports: Vec<ImportInfo>,
    /// Scope map: line → containing entity name
    pub scope_map: HashMap<u32, String>,
}

#[derive(Debug, Clone, Default)]
pub struct SymbolInfo {
    pub name: String,
    pub line: u32,
    pub column: u32,
    pub scope: Option<String>,
    pub type_str: Option<String>,
    /// Source file path (set for symbols imported from other modules)
    pub source_file: Option<String>,
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
pub struct GenericInfo {
    pub name: String,
    pub line: u32,
    pub column: u32,
    pub type_str: String,
    pub default_value: Option<String>,
    pub entity: String,
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
            generics: Vec::new(),
            type_aliases: Vec::new(),
            imports: Vec::new(),
            scope_map: HashMap::new(),
        };

        let mut current_entity: Option<String> = None;
        let mut brace_depth = 0u32;
        let mut entity_depth = 0u32;
        let mut in_generic_params = false;
        let mut generic_entity_name = String::new();
        let mut in_struct: Option<String> = None;
        let mut struct_fields: Vec<String> = Vec::new();

        for (line_num, line) in content.lines().enumerate() {
            let line_u32 = line_num as u32;
            let trimmed = line.trim();

            // Track braces
            for ch in line.chars() {
                if ch == '{' {
                    brace_depth += 1;
                }
                if ch == '}' {
                    brace_depth = brace_depth.saturating_sub(1);
                    if brace_depth <= entity_depth && current_entity.is_some() {
                        current_entity = None;
                    }
                }
            }

            // Entity declarations: entity Name or entity Name< or pub entity Name
            let entity_rest = trimmed
                .strip_prefix("pub entity ")
                .or_else(|| trimmed.strip_prefix("entity "));
            if let Some(rest) = entity_rest {
                let name = rest
                    .split(|c: char| !c.is_alphanumeric() && c != '_')
                    .next()
                    .unwrap_or("")
                    .to_string();
                if !name.is_empty() {
                    let col = line.find(&name).unwrap_or(0) as u32;
                    ctx.entities.push(SymbolInfo {
                        name: name.clone(),
                        line: line_u32,
                        column: col,
                        ..Default::default()
                    });
                    // Check if this entity has generic parameters (line contains '<' after name)
                    if rest.contains('<') && !rest.contains('>') {
                        in_generic_params = true;
                        generic_entity_name = name.clone();
                    } else if rest.contains('<') && rest.contains('>') {
                        // Single-line generics: entity Name<PARAM: type = val>
                        if let Some(gen_start) = rest.find('<') {
                            if let Some(gen_end) = rest.find('>') {
                                let gen_str = &rest[gen_start + 1..gen_end];
                                parse_generic_params(gen_str, line_u32, &name, &mut ctx.generics);
                            }
                        }
                    }
                    current_entity = Some(name);
                    entity_depth = brace_depth;
                }
            }

            // Multi-line generic parameters (inside entity<...> block)
            if in_generic_params {
                // Check if this line closes the generic block: a '>' not balanced by '<'
                let angle_depth: i32 = trimmed.chars().fold(0i32, |d, c| match c {
                    '<' => d + 1,
                    '>' => d - 1,
                    _ => d,
                });
                let closes_generics = angle_depth < 0;

                if closes_generics {
                    // Last line of generics — parse up to the unbalanced '>'
                    // Find the closing '>' that's not part of a type (track nesting)
                    let mut depth = 0i32;
                    let mut close_pos = trimmed.len();
                    for (i, c) in trimmed.char_indices() {
                        match c {
                            '<' => depth += 1,
                            '>' => {
                                if depth == 0 {
                                    close_pos = i;
                                    break;
                                }
                                depth -= 1;
                            }
                            _ => {}
                        }
                    }
                    let before_close = &trimmed[..close_pos];
                    if !before_close.is_empty() && before_close.contains(':') {
                        parse_generic_params(
                            before_close,
                            line_u32,
                            &generic_entity_name,
                            &mut ctx.generics,
                        );
                    }
                    in_generic_params = false;
                } else if trimmed.contains(':')
                    && !trimmed.starts_with("//")
                    && !trimmed.starts_with("in ")
                    && !trimmed.starts_with("out ")
                {
                    // Generic param line like: PARAM_NAME: type = default,
                    parse_generic_params(
                        trimmed,
                        line_u32,
                        &generic_entity_name,
                        &mut ctx.generics,
                    );
                }
            }

            // Impl blocks (enter entity scope)
            if let Some(rest) = trimmed.strip_prefix("impl ") {
                let name = rest
                    .split(|c: char| !c.is_alphanumeric() && c != '_')
                    .next()
                    .unwrap_or("")
                    .to_string();
                if !name.is_empty() {
                    current_entity = Some(name);
                    entity_depth = brace_depth;
                }
            }

            // Update scope map
            if let Some(ref ent) = current_entity {
                ctx.scope_map.insert(line_u32, ent.clone());
            }

            // Port declarations
            if (trimmed.starts_with("in ")
                || trimmed.starts_with("out ")
                || trimmed.starts_with("inout "))
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
                if !parts.is_empty() {
                    let name = parts[0]
                        .trim()
                        .trim_end_matches(|c: char| !c.is_alphanumeric() && c != '_');
                    let type_str = if parts.len() > 1 {
                        parts[1]
                            .trim()
                            .trim_end_matches([';', ',', '{'])
                            .trim()
                            .to_string()
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
                        ..Default::default()
                    });
                }
            }

            // Constants: const NAME: TYPE = VALUE; or pub const NAME: TYPE = VALUE;
            let const_rest = trimmed
                .strip_prefix("pub const ")
                .or_else(|| trimmed.strip_prefix("const "));
            if let Some(rest) = const_rest {
                // Split on '=' first to get name:type and value
                let eq_parts: Vec<&str> = rest.splitn(2, '=').collect();
                let name_type = eq_parts[0].trim();
                let value = if eq_parts.len() > 1 {
                    Some(eq_parts[1].trim().trim_end_matches(';').trim().to_string())
                } else {
                    None
                };

                let colon_parts: Vec<&str> = name_type.splitn(2, ':').collect();
                let name = colon_parts[0].trim();
                let type_str = if colon_parts.len() > 1 {
                    Some(colon_parts[1].trim().to_string())
                } else {
                    None
                };

                let col = line.find(name).unwrap_or(0) as u32;
                let mut display = String::new();
                if let Some(ref t) = type_str {
                    display.push_str(t);
                }
                if let Some(ref v) = value {
                    if !display.is_empty() {
                        display.push_str(" = ");
                    }
                    display.push_str(v);
                }

                ctx.constants.push(SymbolInfo {
                    name: name.to_string(),
                    line: line_u32,
                    column: col,
                    scope: current_entity.clone(),
                    type_str: if display.is_empty() {
                        None
                    } else {
                        Some(display)
                    },
                    ..Default::default()
                });
            }

            // Type aliases: type Name = Type; or pub type Name = Type;
            // Also: distinct type Name = Type;
            let type_rest = trimmed
                .strip_prefix("pub type ")
                .or_else(|| trimmed.strip_prefix("pub distinct type "))
                .or_else(|| trimmed.strip_prefix("distinct type "))
                .or_else(|| trimmed.strip_prefix("type "));
            if let Some(rest) = type_rest {
                let eq_parts: Vec<&str> = rest.splitn(2, '=').collect();
                if eq_parts.len() == 2 {
                    let name = eq_parts[0].trim();
                    let underlying = eq_parts[1].trim().trim_end_matches(';').trim();
                    if !name.is_empty() {
                        let col = line.find(name).unwrap_or(0) as u32;
                        ctx.type_aliases.push(SymbolInfo {
                            name: name.to_string(),
                            line: line_u32,
                            column: col,
                            scope: current_entity.clone(),
                            type_str: Some(underlying.to_string()),
                            ..Default::default()
                        });
                    }
                }
            }

            // Collect struct/enum members when inside a definition
            if let Some(ref struct_name) = in_struct {
                if trimmed == "}" || trimmed.starts_with('}') {
                    // Close struct/enum — update the type_aliases entry with collected fields
                    let fields_str = struct_fields.join(", ");
                    if let Some(ta) = ctx
                        .type_aliases
                        .iter_mut()
                        .rev()
                        .find(|t| t.name == *struct_name)
                    {
                        let kind = if ta.type_str.as_deref() == Some("enum") {
                            "enum"
                        } else {
                            "struct"
                        };
                        ta.type_str = Some(format!("{} {{ {} }}", kind, fields_str));
                    }
                    in_struct = None;
                    struct_fields.clear();
                } else if !trimmed.starts_with("//") && !trimmed.is_empty() {
                    if trimmed.contains(':') {
                        // Struct field: field_name: type,
                        let parts: Vec<&str> = trimmed.splitn(2, ':').collect();
                        if parts.len() == 2 {
                            let fname = parts[0].trim();
                            let ftype = parts[1]
                                .trim()
                                .trim_end_matches(',')
                                .trim()
                                .trim_end_matches(';')
                                .trim();
                            // Strip trailing comment
                            let ftype = ftype.split("//").next().unwrap_or(ftype).trim();
                            struct_fields.push(format!("{}: {}", fname, ftype));
                        }
                    } else if trimmed.contains('=') {
                        // Enum variant: Name = value,
                        let parts: Vec<&str> = trimmed.splitn(2, '=').collect();
                        if parts.len() == 2 {
                            let vname = parts[0].trim();
                            let vval = parts[1]
                                .trim()
                                .trim_end_matches(',')
                                .trim()
                                .trim_end_matches(';')
                                .trim();
                            let vval = vval.split("//").next().unwrap_or(vval).trim();
                            struct_fields.push(format!("{} = {}", vname, vval));
                        }
                    }
                }
            }

            // Struct declarations: struct Name { or pub struct Name {
            let struct_rest = trimmed
                .strip_prefix("pub struct ")
                .or_else(|| trimmed.strip_prefix("struct "));
            if let Some(rest) = struct_rest {
                let name = rest
                    .split(|c: char| !c.is_alphanumeric() && c != '_')
                    .next()
                    .unwrap_or("")
                    .to_string();
                if !name.is_empty() {
                    let col = line.find(&name).unwrap_or(0) as u32;
                    ctx.type_aliases.push(SymbolInfo {
                        name: name.clone(),
                        line: line_u32,
                        column: col,
                        scope: current_entity.clone(),
                        type_str: Some("struct".to_string()),
                        ..Default::default()
                    });
                    in_struct = Some(name);
                    struct_fields.clear();
                }
            }

            // Enum declarations: enum Name { or pub enum Name {
            let enum_rest = trimmed
                .strip_prefix("pub enum ")
                .or_else(|| trimmed.strip_prefix("enum "));
            if let Some(rest) = enum_rest {
                let name = rest
                    .split(|c: char| !c.is_alphanumeric() && c != '_')
                    .next()
                    .unwrap_or("")
                    .to_string();
                if !name.is_empty() {
                    let col = line.find(&name).unwrap_or(0) as u32;
                    ctx.type_aliases.push(SymbolInfo {
                        name: name.clone(),
                        line: line_u32,
                        column: col,
                        scope: current_entity.clone(),
                        type_str: Some("enum".to_string()),
                        ..Default::default()
                    });
                    in_struct = Some(name);
                    struct_fields.clear();
                }
            }

            // Trait declarations
            if let Some(rest) = trimmed.strip_prefix("trait ") {
                let name = rest
                    .split(|c: char| !c.is_alphanumeric() && c != '_')
                    .next()
                    .unwrap_or("")
                    .to_string();
                if !name.is_empty() {
                    let col = line.find("trait").unwrap_or(0) as u32;
                    ctx.traits.push(SymbolInfo {
                        name,
                        line: line_u32,
                        column: col,
                        ..Default::default()
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
                        if !entity_type.is_empty()
                            && entity_type.chars().next().unwrap().is_uppercase()
                        {
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

    /// Build analysis context with cross-module resolution.
    /// Reads `mod X;` declarations and parses sibling `.sk` files for constants/entities.
    /// Also searches project `src_dirs` from `skalp.toml` for module resolution.
    pub fn from_source_with_path(content: &str, file_path: &Path) -> Self {
        let mut ctx = Self::from_source(content);

        // Collect mod declarations from the source
        let mut mod_names = Vec::new();
        for line in content.lines() {
            let trimmed = line.trim();
            if let Some(rest) = trimmed.strip_prefix("mod ") {
                let name = rest.trim_end_matches(';').trim();
                if !name.is_empty() {
                    mod_names.push(name.to_string());
                }
            }
        }

        if mod_names.is_empty() {
            return ctx;
        }

        // Build search directories: parent dir + project src_dirs from skalp.toml
        let parent = match file_path.parent() {
            Some(p) => p,
            None => return ctx,
        };
        let mut search_dirs = vec![parent.to_path_buf()];

        // Walk up to find skalp.toml and extract src_dirs
        let mut dir = parent;
        loop {
            let toml_path = dir.join("skalp.toml");
            if toml_path.exists() {
                if let Ok(toml_content) = std::fs::read_to_string(&toml_path) {
                    for line in toml_content.lines() {
                        let trimmed = line.trim();
                        if trimmed.starts_with("src_dirs") {
                            // Parse src_dirs = ["lib", "battery_dcdc", ...]
                            if let Some(arr_start) = trimmed.find('[') {
                                if let Some(arr_end) = trimmed.find(']') {
                                    let arr = &trimmed[arr_start + 1..arr_end];
                                    for entry in arr.split(',') {
                                        let name = entry.trim().trim_matches('"').trim();
                                        if !name.is_empty() {
                                            search_dirs.push(dir.join(name));
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                break;
            }
            match dir.parent() {
                Some(p) => dir = p,
                None => break,
            }
        }

        // Resolve modules across all search directories
        let mut resolved = std::collections::HashSet::new();
        for mod_name in &mod_names {
            for search_dir in &search_dirs {
                if resolved.contains(mod_name.as_str()) {
                    break;
                }
                let candidates = [
                    search_dir.join(format!("{}.sk", mod_name)),
                    search_dir.join(mod_name).join("mod.sk"),
                ];
                for candidate in &candidates {
                    if candidate.exists() {
                        if let Ok(mod_content) = std::fs::read_to_string(candidate) {
                            let mod_ctx = Self::from_source(&mod_content);
                            let src = candidate.to_string_lossy().to_string();
                            for mut c in mod_ctx.constants {
                                c.source_file = Some(src.clone());
                                ctx.constants.push(c);
                            }
                            for mut e in mod_ctx.entities {
                                e.source_file = Some(src.clone());
                                ctx.entities.push(e);
                            }
                            for mut t in mod_ctx.traits {
                                t.source_file = Some(src.clone());
                                ctx.traits.push(t);
                            }
                            for mut ta in mod_ctx.type_aliases {
                                ta.source_file = Some(src.clone());
                                ctx.type_aliases.push(ta);
                            }
                            resolved.insert(mod_name.as_str());
                            break;
                        }
                    }
                }
            }
        }

        ctx
    }

    /// Find definition of a symbol by name.
    /// Returns (line, column, optional_source_file_path).
    pub fn find_definition(&self, name: &str) -> Option<(u32, u32, Option<&str>)> {
        // Search entities
        if let Some(s) = self.entities.iter().find(|s| s.name == name) {
            return Some((s.line, s.column, s.source_file.as_deref()));
        }
        // Search signals
        if let Some(s) = self.signals.iter().find(|s| s.name == name) {
            return Some((s.line, s.column, s.source_file.as_deref()));
        }
        // Search ports
        if let Some(p) = self.ports.iter().find(|p| p.name == name) {
            return Some((p.line, p.column, None));
        }
        // Search constants
        if let Some(s) = self.constants.iter().find(|s| s.name == name) {
            return Some((s.line, s.column, s.source_file.as_deref()));
        }
        // Search traits
        if let Some(s) = self.traits.iter().find(|s| s.name == name) {
            return Some((s.line, s.column, s.source_file.as_deref()));
        }
        // Search instances
        if let Some(i) = self.instances.iter().find(|i| i.name == name) {
            return Some((i.line, i.column, None));
        }
        // Search generics
        if let Some(g) = self.generics.iter().find(|g| g.name == name) {
            return Some((g.line, g.column, None));
        }
        // Search type aliases
        if let Some(t) = self.type_aliases.iter().find(|t| t.name == name) {
            return Some((t.line, t.column, t.source_file.as_deref()));
        }
        // Search enum variants — navigate to the enum definition
        for ta in &self.type_aliases {
            let underlying = ta.type_str.as_deref().unwrap_or("");
            if underlying.starts_with("enum {") {
                if let Some(inner) = underlying
                    .strip_prefix("enum { ")
                    .and_then(|s| s.strip_suffix(" }"))
                {
                    for variant in inner.split(", ") {
                        let vname = variant.split(['=', ':']).next().unwrap_or(variant).trim();
                        if vname == name {
                            return Some((ta.line, ta.column, ta.source_file.as_deref()));
                        }
                    }
                }
            }
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
            let width_str = p
                .width
                .map(|w| format!(" ({} bits)", w))
                .unwrap_or_default();
            return Some(format!(
                "**{} port** `{}`: `{}`{}\n\nIn entity `{}`",
                p.direction, p.name, p.type_str, width_str, p.entity
            ));
        }

        // Signal
        if let Some(s) = self.signals.iter().find(|s| s.name == name) {
            let type_str = s.type_str.as_deref().unwrap_or("unknown");
            let scope = s.scope.as_deref().unwrap_or("global");
            return Some(format!(
                "**signal** `{}`: `{}`\n\nIn `{}`",
                s.name, type_str, scope
            ));
        }

        // Constant
        if let Some(c) = self.constants.iter().find(|s| s.name == name) {
            let detail = c.type_str.as_deref().unwrap_or("");
            if detail.is_empty() {
                return Some(format!("**const** `{}`", c.name));
            } else {
                return Some(format!("**const** `{}`: `{}`", c.name, detail));
            }
        }

        // Trait
        if let Some(t) = self.traits.iter().find(|s| s.name == name) {
            return Some(format!("**trait** `{}`", t.name));
        }

        // Instance
        if let Some(i) = self.instances.iter().find(|i| i.name == name) {
            return Some(format!(
                "**instance** `{}`: `{}`\n\nIn `{}`",
                i.name, i.entity_type, i.scope
            ));
        }

        // Generic parameter
        if let Some(g) = self.generics.iter().find(|g| g.name == name) {
            let default = g
                .default_value
                .as_deref()
                .map(|v| format!(" = {}", v))
                .unwrap_or_default();
            let mut info = format!(
                "**generic** `{}`: `{}{}`\n\nIn entity `{}`",
                g.name, g.type_str, default, g.entity
            );
            // Resolve default value if it references a known constant
            if let Some(ref default_name) = g.default_value {
                if let Some(c) = self.constants.iter().find(|c| c.name == *default_name) {
                    if let Some(ref detail) = c.type_str {
                        info.push_str(&format!("\n\n`{}: {}`", default_name, detail));
                    }
                }
            }
            return Some(info);
        }

        // Type alias / struct / enum
        if let Some(t) = self.type_aliases.iter().find(|t| t.name == name) {
            let underlying = t.type_str.as_deref().unwrap_or("unknown");
            if underlying.starts_with("struct {") || underlying.starts_with("enum {") {
                let is_enum = underlying.starts_with("enum");
                let kind = if is_enum { "enum" } else { "struct" };
                let prefix = format!("{} {{ ", kind);
                let inner = underlying
                    .strip_prefix(&prefix)
                    .and_then(|s| s.strip_suffix(" }"))
                    .unwrap_or("");
                let mut info = format!("**{}** `{}`\n\n", kind, t.name);
                if !inner.is_empty() {
                    info.push_str(if is_enum { "Variants:\n" } else { "Fields:\n" });
                    for field in inner.split(", ") {
                        info.push_str(&format!("- `{}`\n", field));
                    }
                }
                return Some(info);
            } else if underlying == "struct" {
                return Some(format!("**struct** `{}`", t.name));
            } else if underlying == "enum" {
                return Some(format!("**enum** `{}`", t.name));
            } else {
                return Some(format!("**type** `{}` = `{}`", t.name, underlying));
            }
        }

        // Enum variant: search all enums for a matching variant name
        for ta in &self.type_aliases {
            let underlying = ta.type_str.as_deref().unwrap_or("");
            if underlying.starts_with("enum {") {
                if let Some(inner) = underlying
                    .strip_prefix("enum { ")
                    .and_then(|s| s.strip_suffix(" }"))
                {
                    for variant in inner.split(", ") {
                        let vname = variant.split(['=', ':']).next().unwrap_or(variant).trim();
                        if vname == name {
                            return Some(format!(
                                "**variant** `{}::{}` \n\n`{}`\n\nIn enum `{}`",
                                ta.name,
                                vname,
                                variant.trim(),
                                ta.name
                            ));
                        }
                    }
                }
            }
        }

        None
    }
}

/// Parse generic parameter declarations like "N: nat<32> = 8" from a comma-separated string
fn parse_generic_params(
    params_str: &str,
    line: u32,
    entity: &str,
    generics: &mut Vec<GenericInfo>,
) {
    for param in params_str.split(',') {
        let param = param.trim().trim_end_matches(',');
        if param.is_empty() || param.starts_with("//") {
            continue;
        }
        // Format: NAME: TYPE or NAME: TYPE = DEFAULT
        let colon_parts: Vec<&str> = param.splitn(2, ':').collect();
        if colon_parts.len() < 2 {
            continue;
        }
        let name = colon_parts[0].trim();
        if name.is_empty() {
            continue;
        }
        let rest = colon_parts[1].trim();
        // Split on '=' for default value
        let eq_parts: Vec<&str> = rest.splitn(2, '=').collect();
        let type_str = eq_parts[0].trim().to_string();
        let default_value = if eq_parts.len() > 1 {
            Some(eq_parts[1].trim().trim_end_matches(';').trim().to_string())
        } else {
            None
        };

        generics.push(GenericInfo {
            name: name.to_string(),
            line,
            column: 0,
            type_str,
            default_value,
            entity: entity.to_string(),
        });
    }
}

fn infer_width(type_str: &str) -> Option<u32> {
    let t = type_str.trim();
    if t == "bit" || t == "bool" || t == "clock" || t == "reset" {
        return Some(1);
    }
    if t == "fp32" {
        return Some(32);
    }
    if t == "fp16" {
        return Some(16);
    }
    if t == "fp64" {
        return Some(64);
    }

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
    fn test_generics_and_constants() {
        let source = r#"
entity PidController<
    N: nat<32> = 8,
    KP: nat<16> = 100,
> {
    in clk: clock;
    out result: int<N>;
}

impl PidController {
    const MAX_VALUE: nat<32> = 1000;
}
"#;
        let ctx = AnalysisContext::from_source(source);
        assert_eq!(ctx.generics.len(), 2);
        assert_eq!(ctx.generics[0].name, "N");
        assert_eq!(ctx.generics[0].type_str, "nat<32>");
        assert_eq!(ctx.generics[0].default_value, Some("8".to_string()));
        assert_eq!(ctx.generics[0].entity, "PidController");
        assert_eq!(ctx.generics[1].name, "KP");

        // Test constant with value
        assert_eq!(ctx.constants.len(), 1);
        assert_eq!(ctx.constants[0].name, "MAX_VALUE");
        assert!(ctx.constants[0].type_str.as_ref().unwrap().contains("1000"));

        // Test hover
        let hover = ctx.hover_info("N").unwrap();
        assert!(hover.contains("generic"));
        assert!(hover.contains("nat<32>"));
        assert!(hover.contains("= 8"));

        let hover = ctx.hover_info("MAX_VALUE").unwrap();
        assert!(hover.contains("const"));
        assert!(hover.contains("1000"));
    }

    #[test]
    fn test_generics_exact_file_format() {
        // Match exact format from battery_dcdc/main.sk
        let source = r#"pub const BMS_TIMEOUT: nat[32] = 100_000_000;
pub const SOFT_START_DURATION: nat[32] = 1_000;

#[trace(group = "battery", display_name = "DAB Controller")]
entity DabBatteryController<
    SOFT_START_CYCLES: nat[32] = SOFT_START_DURATION,
    PRECHARGE_CYCLES: nat[32] = 1000,
    BMS_TIMEOUT_CYCLES: nat[32] = BMS_TIMEOUT,
    CC_CV_FLOAT_ENTRY_CYCLES: nat[32] = FLOAT_ENTRY_DELAY
> {
    in clk: clock
    in rst: reset(active_high)
    in enable: bit
}
"#;
        let ctx = AnalysisContext::from_source(source);

        // All 4 generics should be parsed
        assert_eq!(
            ctx.generics.len(),
            4,
            "Expected 4 generics, got: {:?}",
            ctx.generics.iter().map(|g| &g.name).collect::<Vec<_>>()
        );
        assert_eq!(ctx.generics[0].name, "SOFT_START_CYCLES");
        assert_eq!(ctx.generics[1].name, "PRECHARGE_CYCLES");
        assert_eq!(ctx.generics[2].name, "BMS_TIMEOUT_CYCLES");
        assert_eq!(ctx.generics[3].name, "CC_CV_FLOAT_ENTRY_CYCLES");
        assert_eq!(ctx.generics[3].type_str, "nat[32]");
        assert_eq!(
            ctx.generics[3].default_value,
            Some("FLOAT_ENTRY_DELAY".to_string())
        );

        // Hover should work on all generics
        assert!(ctx.hover_info("SOFT_START_CYCLES").is_some());
        assert!(ctx.hover_info("CC_CV_FLOAT_ENTRY_CYCLES").is_some());

        // Constants from same file should resolve
        assert!(ctx.hover_info("BMS_TIMEOUT").is_some());
        assert!(ctx.hover_info("SOFT_START_DURATION").is_some());
        // FLOAT_ENTRY_DELAY is from imported module — won't resolve
        assert!(ctx.hover_info("FLOAT_ENTRY_DELAY").is_none());
    }

    #[test]
    fn test_find_definition() {
        let source =
            "entity Foo {\n  in x: bit;\n  out y: bit;\n}\nimpl Foo {\n  signal z: nat<4>;\n}";
        let ctx = AnalysisContext::from_source(source);
        assert!(ctx.find_definition("Foo").is_some());
        assert!(ctx.find_definition("x").is_some());
        assert!(ctx.find_definition("z").is_some());
        assert!(ctx.find_definition("nonexistent").is_none());
    }
}
