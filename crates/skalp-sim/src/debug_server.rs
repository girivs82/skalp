//! Debug server for interactive simulation debugging via JSON-line protocol.
//!
//! Communicates over stdin/stdout with JSON objects (one per line).
//! Designed to be driven by the VSCode DAP adapter (TypeScript side).
//!
//! # Protocol
//!
//! Commands arrive as JSON objects on stdin, one per line.
//! Events are emitted as JSON objects on stdout, one per line.
//! Stderr is used for diagnostic logging only.

use std::collections::BTreeMap;
use std::io::{self, BufRead, Write};
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tokio::sync::mpsc;

use crate::breakpoint::{BreakpointCondition, BreakpointManager};
use crate::simulator::SimulationState;
use crate::unified_runtime::{SimLevel, UnifiedSimConfig, UnifiedSimulator};
use crate::waveform::Waveform;

// ---------------------------------------------------------------------------
// Command types (stdin)
// ---------------------------------------------------------------------------

#[derive(Debug, Deserialize)]
#[serde(tag = "cmd")]
pub enum Command {
    #[serde(rename = "launch")]
    Launch {
        file: String,
        level: Option<String>,
        top_module: Option<String>,
        max_cycles: Option<u64>,
    },
    #[serde(rename = "step")]
    Step {
        granularity: Option<String>,
    },
    #[serde(rename = "continue")]
    Continue {
        max_cycles: Option<u64>,
    },
    #[serde(rename = "pause")]
    Pause,
    #[serde(rename = "set_breakpoint")]
    SetBreakpoint {
        signal: String,
        condition: Option<String>,
        value: Option<u64>,
    },
    #[serde(rename = "remove_breakpoint")]
    RemoveBreakpoint { id: u32 },
    #[serde(rename = "set_input")]
    SetInput { name: String, value: u64 },
    #[serde(rename = "get_variables")]
    GetVariables { scope: String },
    #[serde(rename = "get_hierarchy")]
    GetHierarchy,
    #[serde(rename = "get_source_map")]
    GetSourceMap { file: String },
    #[serde(rename = "evaluate")]
    Evaluate { expr: String },
    #[serde(rename = "get_inline_values")]
    GetInlineValues { file: String },
    #[serde(rename = "get_waveform_data")]
    GetWaveformData {
        from_cycle: Option<u64>,
        to_cycle: Option<u64>,
    },
    #[serde(rename = "step_back")]
    StepBack,
    #[serde(rename = "disconnect")]
    Disconnect,
}

// ---------------------------------------------------------------------------
// Response / variable types
// ---------------------------------------------------------------------------

#[derive(Debug, Serialize)]
pub struct VariableInfo {
    pub name: String,
    pub value: String,
    pub width: usize,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hex_value: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct SourceMapping {
    pub line: usize,
    pub signal: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct InlineValueInfo {
    pub line: usize,
    pub signal: String,
    pub value: String,
    pub changed: bool,
}

// ---------------------------------------------------------------------------
// Debug Server
// ---------------------------------------------------------------------------

pub struct DebugServer {
    simulator: Option<UnifiedSimulator>,
    breakpoint_mgr: BreakpointManager,
    waveform: Waveform,
    current_cycle: u64,
    max_cycles: u64,
    last_state: Option<SimulationState>,
    source_map: BTreeMap<String, Vec<SourceMapping>>,
    /// Display names for input ports (user-facing)
    input_names: Vec<String>,
    /// Display names for output ports (user-facing)
    output_names: Vec<String>,
    /// Display names for registers (user-facing)
    register_names: Vec<String>,
    /// Display name → bit width
    signal_widths: IndexMap<String, usize>,
    /// Sanitized (internal) name → display name
    sanitized_to_display: BTreeMap<String, String>,
    /// Display name → sanitized (internal) name
    display_to_sanitized: BTreeMap<String, String>,
    /// Clock signal name (display name, e.g. "clk") for auto-toggling
    clock_name: Option<String>,
    /// Current input values (display_name → value) tracked for get_variables
    current_inputs: BTreeMap<String, u64>,
    source_file: Option<PathBuf>,
    waveform_path: Option<PathBuf>,
    /// Half-cycle counter for waveform time axis (allows clock toggling)
    waveform_time: u64,
    /// Last waveform time sent to the viewer (for incremental delta)
    last_emitted_waveform_time: u64,
    /// Line of the top-level entity declaration (for stack frame highlight)
    entity_line: usize,
}

impl DebugServer {
    pub fn new() -> Self {
        Self {
            simulator: None,
            breakpoint_mgr: BreakpointManager::new(),
            waveform: Waveform::new(),
            current_cycle: 0,
            max_cycles: 100_000,
            last_state: None,
            source_map: BTreeMap::new(),
            input_names: Vec::new(),
            output_names: Vec::new(),
            register_names: Vec::new(),
            signal_widths: IndexMap::new(),
            sanitized_to_display: BTreeMap::new(),
            display_to_sanitized: BTreeMap::new(),
            clock_name: None,
            current_inputs: BTreeMap::new(),
            source_file: None,
            waveform_path: None,
            waveform_time: 0,
            last_emitted_waveform_time: 0,
            entity_line: 1,
        }
    }

    // ------------------------------------------------------------------
    // Name translation helpers
    // ------------------------------------------------------------------

    /// Translate a SimulationState from sanitized (internal) names to display names.
    /// Filters out signals that don't have a display name mapping (internal temporaries).
    fn translate_state(&self, state: &SimulationState) -> SimulationState {
        let mut signals = IndexMap::new();
        for (name, value) in &state.signals {
            if let Some(display) = self.sanitized_to_display.get(name) {
                signals.insert(display.clone(), value.clone());
            }
        }
        let mut registers = IndexMap::new();
        for (name, value) in &state.registers {
            if let Some(display) = self.sanitized_to_display.get(name) {
                registers.insert(display.clone(), value.clone());
            }
        }
        SimulationState {
            cycle: state.cycle,
            signals,
            registers,
        }
    }

    /// Resolve a user-facing display name to the sanitized (internal) name.
    fn resolve_display_name(&self, name: &str) -> String {
        self.display_to_sanitized
            .get(name)
            .cloned()
            .unwrap_or_else(|| name.to_string())
    }

    // ------------------------------------------------------------------
    // Output helpers
    // ------------------------------------------------------------------

    fn emit(&self, event: &Value) {
        let line = serde_json::to_string(event).expect("JSON serialization failed");
        let stdout = io::stdout();
        let mut handle = stdout.lock();
        let _ = writeln!(handle, "{}", line);
        let _ = handle.flush();
    }

    fn emit_error(&self, message: &str) {
        self.emit(&serde_json::json!({
            "event": "error",
            "message": message,
        }));
    }

    fn emit_stopped(&mut self, reason: &str, hits: &[crate::breakpoint::BreakpointHit]) {
        let mut event = serde_json::json!({
            "event": "stopped",
            "reason": reason,
            "cycle": self.current_cycle,
            "waveform_time": self.waveform_time,
        });

        if let Some(state) = &self.last_state {
            let translated = self.translate_state(state);
            let (signals, registers) = Self::state_to_json(&translated);
            event["signals"] = Value::Object(signals);
            event["registers"] = Value::Object(registers);

            // Include waveform changes for live viewer sync.
            // Pull actual data from the waveform object for all time points
            // since last_emitted_waveform_time — this captures intermediate
            // half-cycle values (essential for clock toggling).
            let from_time = self.last_emitted_waveform_time;
            let mut waveform_changes = serde_json::Map::new();
            for (name, signal) in &self.waveform.signals {
                let mut changes = Vec::new();
                for (time, value) in &signal.values {
                    if *time > from_time && *time <= self.waveform_time {
                        changes.push(serde_json::json!([
                            *time,
                            Self::format_hex_raw(value)
                        ]));
                    }
                }
                if !changes.is_empty() {
                    waveform_changes.insert(name.clone(), Value::Array(changes));
                }
            }
            event["waveform_changes"] = Value::Object(waveform_changes);

            // Include signal widths so waveform viewer can render bit vs bus correctly
            let widths_json: serde_json::Map<String, Value> = self
                .signal_widths
                .iter()
                .map(|(name, &w)| (name.clone(), Value::Number(serde_json::Number::from(w))))
                .collect();
            event["signal_widths"] = Value::Object(widths_json);
        }

        if !hits.is_empty() {
            // Primary hit (first) for backwards compatibility
            let hit = &hits[0];
            let display_signal = self.sanitized_to_display
                .get(&hit.signal_name)
                .cloned()
                .unwrap_or_else(|| hit.signal_name.clone());
            event["hit"] = serde_json::json!({
                "bp_id": hit.id,
                "name": hit.name,
                "signal": display_signal,
                "value": Self::format_value(&hit.signal_value),
            });

            // All hits (for multi-breakpoint support)
            let all_hits: Vec<serde_json::Value> = hits
                .iter()
                .map(|h| {
                    let sig = self.sanitized_to_display
                        .get(&h.signal_name)
                        .cloned()
                        .unwrap_or_else(|| h.signal_name.clone());
                    serde_json::json!({
                        "bp_id": h.id,
                        "signal": sig,
                        "value": Self::format_value(&h.signal_value),
                    })
                })
                .collect();
            event["hits"] = serde_json::json!(all_hits);
        }

        self.last_emitted_waveform_time = self.waveform_time;
        self.emit(&event);
    }

    // ------------------------------------------------------------------
    // Value formatting
    // ------------------------------------------------------------------

    fn format_value(bytes: &[u8]) -> String {
        if bytes.is_empty() {
            return "0".to_string();
        }
        let mut hex = String::new();
        for byte in bytes.iter().rev() {
            hex.push_str(&format!("{:02x}", byte));
        }
        let trimmed = hex.trim_start_matches('0');
        if trimmed.is_empty() {
            "0".to_string()
        } else {
            format!("0x{}", trimmed)
        }
    }

    fn format_hex_raw(bytes: &[u8]) -> String {
        let mut hex = String::new();
        for byte in bytes.iter().rev() {
            hex.push_str(&format!("{:02x}", byte));
        }
        let trimmed = hex.trim_start_matches('0');
        if trimmed.is_empty() {
            "0".to_string()
        } else {
            trimmed.to_string()
        }
    }

    fn state_to_json(
        state: &SimulationState,
    ) -> (serde_json::Map<String, Value>, serde_json::Map<String, Value>) {
        let mut signals = serde_json::Map::new();
        for (name, value) in &state.signals {
            signals.insert(name.clone(), Value::String(Self::format_value(value)));
        }
        let mut registers = serde_json::Map::new();
        for (name, value) in &state.registers {
            registers.insert(name.clone(), Value::String(Self::format_value(value)));
        }
        (signals, registers)
    }

    // ------------------------------------------------------------------
    // Waveform recording
    // ------------------------------------------------------------------

    /// Record waveform values at the given time point (half-cycle granularity).
    fn record_waveform_at(&mut self, state: &SimulationState, time: u64) {
        // Record with display names (the waveform signals were initialized with display names)
        for (name, value) in &state.signals {
            let display = self
                .sanitized_to_display
                .get(name)
                .cloned()
                .unwrap_or_else(|| name.clone());
            self.waveform.add_value(&display, time, value.clone());
        }
        for (name, value) in &state.registers {
            let display = self
                .sanitized_to_display
                .get(name)
                .cloned()
                .unwrap_or_else(|| name.clone());
            self.waveform.add_value(&display, time, value.clone());
        }
        // Record input values (not in SimulationState on GPU backend)
        for (display_name, &value) in &self.current_inputs {
            let width = self.signal_widths.get(display_name).copied().unwrap_or(1);
            let num_bytes = (width + 7) / 8;
            let bytes = value.to_le_bytes();
            self.waveform
                .add_value(display_name, time, bytes[..num_bytes].to_vec());
        }
    }

    // ------------------------------------------------------------------
    // Command handlers
    // ------------------------------------------------------------------

    async fn handle_launch(
        &mut self,
        file: &str,
        level: Option<&str>,
        top_module: Option<&str>,
        max_cycles: Option<u64>,
    ) {
        use skalp_frontend::parse_and_build_hir_from_file;
        use skalp_mir::MirCompiler;
        use skalp_sir::convert_mir_to_sir_with_hierarchy;

        let path = PathBuf::from(file);
        self.source_file = Some(path.clone());

        if let Some(mc) = max_cycles {
            self.max_cycles = mc;
        }

        // Waveform output: alongside the source file
        let waveform_path = path.with_extension("debug.skw");
        self.waveform_path = Some(waveform_path);

        // Parse → HIR
        eprintln!("[skalp-debug] Parsing {}...", file);
        let hir = match parse_and_build_hir_from_file(&path) {
            Ok(h) => h,
            Err(e) => {
                self.emit_error(&format!("Failed to parse: {}", e));
                return;
            }
        };

        // HIR → MIR
        eprintln!("[skalp-debug] Compiling to MIR...");
        let compiler = MirCompiler::new();
        let mir = match compiler.compile_to_mir(&hir) {
            Ok(m) => m,
            Err(e) => {
                self.emit_error(&format!("Failed to compile to MIR: {}", e));
                return;
            }
        };

        if mir.modules.is_empty() {
            self.emit_error("No modules found in design");
            return;
        }

        // Find top-level module
        let top = if let Some(name) = top_module {
            match mir.modules.iter().find(|m| m.name == name) {
                Some(m) => m,
                None => {
                    self.emit_error(&format!("Module '{}' not found", name));
                    return;
                }
            }
        } else {
            // Default: last module (typically the top in dependency order)
            mir.modules.last().unwrap()
        };

        eprintln!(
            "[skalp-debug] Top module: {} ({} modules total)",
            top.name,
            mir.modules.len()
        );

        // MIR → SIR
        eprintln!("[skalp-debug] Converting to SIR...");
        let sir = convert_mir_to_sir_with_hierarchy(&mir, top);

        // Create and initialize simulator
        let sim_level = match level {
            Some("gate") | Some("gate_level") => SimLevel::GateLevel,
            _ => SimLevel::Behavioral,
        };

        let config = UnifiedSimConfig {
            level: sim_level,
            capture_waveforms: true,
            ..Default::default()
        };

        let mut simulator = match UnifiedSimulator::new(config) {
            Ok(s) => s,
            Err(e) => {
                self.emit_error(&format!("Failed to create simulator: {}", e));
                return;
            }
        };

        if let Err(e) = simulator.load_behavioral(&sir).await {
            self.emit_error(&format!("Failed to load design: {}", e));
            return;
        }

        // Build sanitized→display name mapping from simulator
        let name_map = simulator.get_signal_name_map();
        self.sanitized_to_display.clear();
        self.display_to_sanitized.clear();
        for (sanitized, display) in &name_map {
            self.sanitized_to_display
                .insert(sanitized.clone(), display.clone());
            self.display_to_sanitized
                .insert(display.clone(), sanitized.clone());
        }

        self.signal_widths = simulator.get_signal_widths();

        // Classify signals into inputs/outputs/registers using the SIR structure.
        // get_input_names()/get_output_names() return internal names, so translate.
        let raw_inputs = simulator.get_input_names();
        let raw_outputs = simulator.get_output_names();

        self.input_names = raw_inputs
            .iter()
            .filter_map(|s| self.sanitized_to_display.get(s).cloned())
            .collect();
        self.output_names = raw_outputs
            .iter()
            .filter_map(|s| self.sanitized_to_display.get(s).cloned())
            .collect();

        // Registers: in signal_widths but not in inputs or outputs
        let input_set: std::collections::HashSet<_> = self.input_names.iter().collect();
        let output_set: std::collections::HashSet<_> = self.output_names.iter().collect();
        self.register_names = self
            .signal_widths
            .keys()
            .filter(|n| !input_set.contains(n) && !output_set.contains(n))
            .cloned()
            .collect();

        // Initialize waveform capture signals (with display names)
        for (name, &width) in &self.signal_widths {
            self.waveform.add_signal(name.clone(), width);
        }

        self.simulator = Some(simulator);

        // Detect clock signal (first 1-bit input matching "clk" or "clock")
        self.clock_name = self
            .input_names
            .iter()
            .find(|n| {
                let lower = n.to_lowercase();
                (lower == "clk" || lower == "clock" || lower.contains("clk"))
                    && self.signal_widths.get(*n).copied() == Some(1)
            })
            .cloned();
        if let Some(ref clk) = self.clock_name {
            eprintln!("[skalp-debug] Auto-detected clock: {}", clk);
        }

        // Initialize all inputs to 0 and record initial waveform state at time 0
        for name in &self.input_names {
            self.current_inputs.insert(name.clone(), 0);
        }
        // Record initial values (all zeros) at waveform time 0
        for (name, &width) in &self.signal_widths {
            let num_bytes = (width + 7) / 8;
            self.waveform
                .add_value(name, 0, vec![0u8; num_bytes]);
        }

        // Build source map from MIR: line → signal name
        self.build_source_map(top);

        eprintln!(
            "[skalp-debug] Initialized: {} inputs, {} outputs, {} registers, {} signals total",
            self.input_names.len(),
            self.output_names.len(),
            self.register_names.len(),
            self.signal_widths.len()
        );

        // Emit initialized event with display names, widths + source map
        let all_signals: Vec<String> = self.signal_widths.keys().cloned().collect();
        let signal_widths_json: serde_json::Map<String, Value> = self
            .signal_widths
            .iter()
            .map(|(name, &width)| (name.clone(), Value::Number(serde_json::Number::from(width))))
            .collect();
        let source_map_entries: Vec<&SourceMapping> = self
            .source_map
            .values()
            .flat_map(|v| v.iter())
            .collect();
        self.emit(&serde_json::json!({
            "event": "initialized",
            "signals": all_signals,
            "signal_widths": signal_widths_json,
            "inputs": self.input_names,
            "outputs": self.output_names,
            "registers": self.register_names,
            "source_map": source_map_entries,
            "entity_line": self.entity_line,
        }));
    }

    // ------------------------------------------------------------------
    // Source map construction
    // ------------------------------------------------------------------

    /// Build source map from MIR module + source text scanning.
    ///
    /// Two-phase approach:
    /// 1. Collect all known signal/port names from the MIR module
    /// 2. Scan the source file for declaration and assignment patterns
    ///    referencing those names
    ///
    /// This is more robust than relying on MIR spans (which are mostly None).
    fn build_source_map(&mut self, module: &skalp_mir::Module) {
        self.source_map.clear();

        let source_path = match self.source_file.as_ref() {
            Some(p) => p.clone(),
            None => return,
        };
        let file_key = source_path.to_string_lossy().to_string();

        // Phase 1: collect known signal/port names
        let mut known_names: std::collections::BTreeSet<String> =
            std::collections::BTreeSet::new();
        for sig in &module.signals {
            known_names.insert(sig.name.clone());
        }
        for port in &module.ports {
            known_names.insert(port.name.clone());
        }

        // Phase 2: scan source file for patterns
        let source = match std::fs::read_to_string(&source_path) {
            Ok(s) => s,
            Err(_) => return,
        };

        let mut mappings: Vec<SourceMapping> = Vec::new();
        let entity_prefix = format!("entity {}", module.name);

        for (idx, line) in source.lines().enumerate() {
            let line_num = idx + 1; // 1-indexed
            let trimmed = line.trim();

            // Skip comments and empty lines
            if trimmed.is_empty() || trimmed.starts_with("//") {
                continue;
            }

            // Entity declaration: `entity <ModuleName> {`
            if trimmed.starts_with(&entity_prefix) {
                self.entity_line = line_num;
            }

            // Port declarations: `in <name>:` or `out <name>:`
            if let Some(name) = Self::extract_port_decl(trimmed) {
                if known_names.contains(&name) {
                    mappings.push(SourceMapping {
                        line: line_num,
                        signal: name,
                    });
                    continue;
                }
            }

            // Signal declarations: `signal <name>:` or `signal <name> =`
            if let Some(name) = Self::extract_signal_decl(trimmed) {
                if known_names.contains(&name) {
                    mappings.push(SourceMapping {
                        line: line_num,
                        signal: name,
                    });
                    continue;
                }
            }

            // Assignment target gets a mapping (for breakpoints)
            if let Some(name) = Self::extract_assignment_target(trimmed) {
                if known_names.contains(&name) {
                    mappings.push(SourceMapping {
                        line: line_num,
                        signal: name,
                    });
                    // Don't continue — also scan for other signal references on this line
                }
            }

            // Scan for all signal name references on this line (for inline values).
            // Match whole words only: signal name must be bounded by non-alphanumeric chars.
            let mut found_on_line = std::collections::BTreeSet::new();
            // Skip assignment target — already added above
            if let Some(ref assign_name) = Self::extract_assignment_target(trimmed) {
                found_on_line.insert(assign_name.clone());
            }
            for name in &known_names {
                if found_on_line.contains(name) {
                    continue;
                }
                // Word-boundary match: check all occurrences of `name` in the line
                let name_bytes = name.as_bytes();
                let line_bytes = trimmed.as_bytes();
                let mut pos = 0;
                while pos + name_bytes.len() <= line_bytes.len() {
                    if let Some(offset) = trimmed[pos..].find(name.as_str()) {
                        let start = pos + offset;
                        let end = start + name_bytes.len();
                        let before_ok = start == 0
                            || !line_bytes[start - 1].is_ascii_alphanumeric()
                                && line_bytes[start - 1] != b'_';
                        let after_ok = end >= line_bytes.len()
                            || !line_bytes[end].is_ascii_alphanumeric()
                                && line_bytes[end] != b'_';
                        if before_ok && after_ok {
                            found_on_line.insert(name.clone());
                            break;
                        }
                        pos = start + 1;
                    } else {
                        break;
                    }
                }
            }
            for name in found_on_line {
                // Avoid duplicate (line, signal) pairs
                if !mappings.iter().any(|m| m.line == line_num && m.signal == name) {
                    mappings.push(SourceMapping {
                        line: line_num,
                        signal: name,
                    });
                }
            }
        }

        // Sort by line number (stable: declarations before references on same line)
        mappings.sort_by_key(|m| m.line);

        if !mappings.is_empty() {
            eprintln!(
                "[skalp-debug] Source map: {} line→signal entries",
                mappings.len()
            );
        }

        self.source_map.insert(file_key, mappings);
    }

    /// Extract port name from `in <name>:` or `out <name>:` patterns.
    fn extract_port_decl(line: &str) -> Option<String> {
        let rest = if line.starts_with("in ") {
            &line[3..]
        } else if line.starts_with("out ") {
            &line[4..]
        } else {
            return None;
        };
        let rest = rest.trim_start();
        // Take identifier chars until `:` or whitespace
        let name: String = rest
            .chars()
            .take_while(|c| c.is_alphanumeric() || *c == '_')
            .collect();
        if name.is_empty() {
            None
        } else {
            Some(name)
        }
    }

    /// Extract signal name from `signal <name>:` or `signal <name> =` patterns.
    fn extract_signal_decl(line: &str) -> Option<String> {
        if !line.starts_with("signal ") {
            return None;
        }
        let rest = line[7..].trim_start();
        let name: String = rest
            .chars()
            .take_while(|c| c.is_alphanumeric() || *c == '_')
            .collect();
        if name.is_empty() {
            None
        } else {
            Some(name)
        }
    }

    /// Extract assignment target from `<name> =` or `<name>[...] =` patterns.
    /// Returns None for comparisons (`==`, `!=`, `<=`, `>=`).
    fn extract_assignment_target(line: &str) -> Option<String> {
        // Must start with an identifier
        let name: String = line
            .chars()
            .take_while(|c| c.is_alphanumeric() || *c == '_')
            .collect();
        if name.is_empty() {
            return None;
        }
        // Skip keywords that aren't signals
        if matches!(
            name.as_str(),
            "if" | "else"
                | "match"
                | "for"
                | "while"
                | "return"
                | "let"
                | "signal"
                | "in"
                | "out"
                | "entity"
                | "impl"
                | "fn"
                | "on"
                | "import"
                | "use"
                | "const"
                | "type"
                | "enum"
                | "struct"
        ) {
            return None;
        }

        let rest = line[name.len()..].trim_start();

        // Handle indexed: name[...] =
        let rest = if rest.starts_with('[') {
            // Skip to matching ]
            let mut depth = 0;
            let mut skip = 0;
            for ch in rest.chars() {
                skip += ch.len_utf8();
                match ch {
                    '[' => depth += 1,
                    ']' => {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                    }
                    _ => {}
                }
            }
            rest[skip..].trim_start()
        } else {
            rest
        };

        // Must have `=` but not `==`
        if rest.starts_with('=') && !rest.starts_with("==") {
            Some(name)
        } else {
            None
        }
    }

    /// Execute one full clock cycle: clk=0→step→clk=1→step, returning the
    /// final SimulationState. Records waveform at both half-cycles for proper
    /// clock toggling. For half-cycle granularity, just one phase.
    async fn step_one_cycle(&mut self, half_cycle: bool) -> Option<SimulationState> {
        let clk = self.clock_name.clone();

        if let Some(clk) = clk {
            if half_cycle {
                let current = self.current_inputs.get(&clk).copied().unwrap_or(0);
                let next = if current == 0 { 1 } else { 0 };
                // Sim operations (borrows self.simulator)
                let state = {
                    let sim = self.simulator.as_mut()?;
                    sim.set_input(&clk, next).await;
                    sim.step_with_snapshot().await
                };
                // Now sim borrow is dropped — safe to record
                self.current_inputs.insert(clk, next);
                if let Some(ref s) = state {
                    self.waveform_time += 1;
                    self.record_waveform_at(s, self.waveform_time);
                }
                state
            } else {
                // Low phase (clk=0)
                let low_state = {
                    let sim = self.simulator.as_mut()?;
                    sim.set_input(&clk, 0).await;
                    sim.step_with_snapshot().await
                };
                self.current_inputs.insert(clk.clone(), 0);
                if let Some(ref s) = low_state {
                    self.waveform_time += 1;
                    self.record_waveform_at(s, self.waveform_time);
                }
                // High phase (clk=1)
                let high_state = {
                    let sim = self.simulator.as_mut()?;
                    sim.set_input(&clk, 1).await;
                    sim.step_with_snapshot().await
                };
                self.current_inputs.insert(clk, 1);
                if let Some(ref s) = high_state {
                    self.waveform_time += 1;
                    self.record_waveform_at(s, self.waveform_time);
                }
                high_state
            }
        } else {
            let state = {
                let sim = self.simulator.as_mut()?;
                sim.step_with_snapshot().await
            };
            if let Some(ref s) = state {
                self.waveform_time += 1;
                self.record_waveform_at(s, self.waveform_time);
            }
            state
        }
    }

    async fn handle_step(&mut self, granularity: Option<&str>) {
        if self.simulator.is_none() {
            self.emit_error("Simulator not initialized");
            return;
        }

        let half_cycle = matches!(granularity, Some("half_cycle"));

        let state = self.step_one_cycle(half_cycle).await;

        if let Some(state) = state {
            self.current_cycle += 1;

            // Run check_cycle to keep previous_values updated for edge detection,
            // but don't stop on breakpoints — the user asked to step.
            // Hits are included as informational data for the console.
            let mut all_values = state.signals.clone();
            for (k, v) in &state.registers {
                all_values.insert(k.clone(), v.clone());
            }
            let hits = self
                .breakpoint_mgr
                .check_cycle(self.current_cycle, &all_values);

            self.last_state = Some(state);

            // Always "step" reason — stepping is explicit, breakpoints don't hijack it.
            // Hits are still reported in the event for console output.
            self.emit_stopped("step", &hits);
        } else {
            self.emit(&serde_json::json!({
                "event": "terminated",
                "cycle": self.current_cycle,
            }));
        }
    }

    async fn handle_continue(&mut self, max_cycles: Option<u64>, pause_flag: &Arc<AtomicBool>) {
        if self.simulator.is_none() {
            self.emit_error("Simulator not initialized");
            return;
        }

        self.emit(&serde_json::json!({"event": "continued"}));

        let limit = max_cycles.unwrap_or(self.max_cycles);
        let start = self.current_cycle;

        loop {
            // Check for pause (set by stdin reader thread)
            if pause_flag.load(Ordering::Relaxed) {
                pause_flag.store(false, Ordering::Relaxed);
                self.emit_stopped("pause", &[]);
                return;
            }

            // Use clock-toggling step (same as handle_step)
            let state = self.step_one_cycle(false).await;

            if let Some(state) = state {
                self.current_cycle += 1;

                // Check breakpoints against both signals and registers
                let mut all_values = state.signals.clone();
                for (k, v) in &state.registers {
                    all_values.insert(k.clone(), v.clone());
                }
                let hits = self
                    .breakpoint_mgr
                    .check_cycle(self.current_cycle, &all_values);

                self.last_state = Some(state);

                if !hits.is_empty() {
                    self.emit_stopped("breakpoint", &hits);
                    return;
                }

                if self.current_cycle - start >= limit {
                    self.emit_stopped("max_cycles", &[]);
                    return;
                }
            } else {
                self.emit(&serde_json::json!({
                    "event": "terminated",
                    "cycle": self.current_cycle,
                }));
                return;
            }
        }
    }

    fn handle_set_breakpoint(
        &mut self,
        signal: &str,
        condition: Option<&str>,
        value: Option<u64>,
    ) -> u32 {
        let bp_condition = match condition {
            Some("equals") => {
                if let Some(v) = value {
                    BreakpointCondition::Equals(v.to_le_bytes().to_vec())
                } else {
                    BreakpointCondition::NonZero
                }
            }
            Some("not_equals") => {
                if let Some(v) = value {
                    BreakpointCondition::NotEquals(v.to_le_bytes().to_vec())
                } else {
                    BreakpointCondition::NonZero
                }
            }
            Some("greater_than") => BreakpointCondition::GreaterThan(value.unwrap_or(0)),
            Some("less_than") => BreakpointCondition::LessThan(value.unwrap_or(0)),
            Some("rising") | Some("rising_edge") => BreakpointCondition::RisingEdge,
            Some("falling") | Some("falling_edge") => BreakpointCondition::FallingEdge,
            Some("any_change") | Some("change") | None => BreakpointCondition::AnyChange,
            Some(other) => {
                // Try parsing as a comparison expression
                BreakpointCondition::parse(other)
            }
        };

        // User provides display name; register with sanitized name for matching
        let sanitized = self.resolve_display_name(signal);
        let id = self
            .breakpoint_mgr
            .register_with_condition(&sanitized, signal, bp_condition);

        // Seed the breakpoint manager's previous values with current state
        // so AnyChange/RisingEdge/FallingEdge can detect the first change.
        if let Some(state) = &self.last_state {
            let mut all_values = state.signals.clone();
            for (k, v) in &state.registers {
                all_values.insert(k.clone(), v.clone());
            }
            // Only seed the signal we're watching
            if let Some(val) = all_values.get(&sanitized) {
                self.breakpoint_mgr.seed_previous_value(&sanitized, val.clone());
            }
        }

        id
    }

    fn handle_get_variables(&self, scope: &str) -> Vec<VariableInfo> {
        // For inputs, use tracked input values (not from SimulationState)
        if scope == "inputs" {
            return self
                .input_names
                .iter()
                .map(|name| {
                    let value = self.current_inputs.get(name).copied().unwrap_or(0);
                    let width = self.signal_widths.get(name).copied().unwrap_or(1);
                    let bytes = value.to_le_bytes();
                    let num_bytes = (width + 7) / 8;
                    VariableInfo {
                        name: name.clone(),
                        value: Self::format_value(&bytes[..num_bytes]),
                        width,
                        hex_value: Some(Self::format_hex_raw(&bytes[..num_bytes])),
                    }
                })
                .collect();
        }

        let state = match &self.last_state {
            Some(s) => s,
            None => return Vec::new(),
        };

        // Translate state to display names first
        let translated = self.translate_state(state);

        let entries: Vec<(&String, &Vec<u8>)> = match scope {
            "outputs" => translated
                .signals
                .iter()
                .chain(translated.registers.iter())
                .filter(|(name, _)| self.output_names.contains(name))
                .collect(),
            "registers" => translated
                .registers
                .iter()
                .filter(|(name, _)| self.register_names.contains(name))
                .collect(),
            _ => {
                // "signals" scope: return everything except inputs
                translated
                    .signals
                    .iter()
                    .chain(translated.registers.iter())
                    .filter(|(name, _)| !self.input_names.contains(name))
                    .collect()
            }
        };

        entries
            .into_iter()
            .map(|(name, value)| {
                let width = self
                    .signal_widths
                    .get(name)
                    .copied()
                    .unwrap_or(value.len() * 8);
                VariableInfo {
                    name: name.clone(),
                    value: Self::format_value(value),
                    width,
                    hex_value: Some(Self::format_hex_raw(value)),
                }
            })
            .collect()
    }

    fn handle_evaluate(&self, expr: &str) -> Value {
        if let Some(state) = &self.last_state {
            let translated = self.translate_state(state);

            // Look up by display name in signals, registers, or inputs
            let (value_owned, sig_type);
            if let Some(v) = translated.signals.get(expr) {
                value_owned = v.clone();
                sig_type = "signal";
            } else if let Some(v) = translated.registers.get(expr) {
                value_owned = v.clone();
                sig_type = "register";
            } else if let Some(&input_val) = self.current_inputs.get(expr) {
                let width = self.signal_widths.get(expr).copied().unwrap_or(1);
                let num_bytes = (width + 7) / 8;
                value_owned = input_val.to_le_bytes()[..num_bytes].to_vec();
                sig_type = "input";
            } else {
                return serde_json::json!({
                    "result": "undefined",
                    "type": "error",
                });
            };
            let value = &value_owned;

            let width = self.signal_widths.get(expr).copied().unwrap_or(value.len() * 8);
            let hex = Self::format_value(value);
            let decimal = crate::breakpoint::bytes_to_u64(value);
            let binary = format!("{:0>width$b}", decimal, width = width);
            let prev = self.breakpoint_mgr.get_previous_value(expr)
                .or_else(|| {
                    // Also check sanitized name
                    let sanitized = self.display_to_sanitized.get(expr)?;
                    self.breakpoint_mgr.get_previous_value(sanitized)
                });
            let changed = prev.map(|p| p != value).unwrap_or(false);

            let mut result = serde_json::json!({
                "result": hex,
                "type": sig_type,
                "width": width,
                "decimal": decimal,
                "binary": binary,
                "changed": changed,
            });
            if let Some(prev_bytes) = prev {
                result["previous"] = Value::String(Self::format_value(prev_bytes));
            }
            return result;
        }
        serde_json::json!({
            "result": "undefined",
            "type": "error",
        })
    }

    fn handle_get_inline_values(&self, file: &str) -> Vec<InlineValueInfo> {
        let mappings = match self.source_map.get(file) {
            Some(m) => m,
            None => return Vec::new(),
        };

        let state = match &self.last_state {
            Some(s) => s,
            None => return Vec::new(),
        };

        let translated = self.translate_state(state);

        // Build a merged signal→value map from translated state
        let mut all_values: IndexMap<String, Vec<u8>> = IndexMap::new();
        for (name, value) in &translated.signals {
            all_values.insert(name.clone(), value.clone());
        }
        for (name, value) in &translated.registers {
            all_values.insert(name.clone(), value.clone());
        }
        // Include inputs
        for (name, &value) in &self.current_inputs {
            let width = self.signal_widths.get(name).copied().unwrap_or(1);
            let num_bytes = (width + 7) / 8;
            let bytes = value.to_le_bytes();
            all_values.insert(name.clone(), bytes[..num_bytes].to_vec());
        }

        // Previous cycle values from breakpoint manager (for change detection)
        let previous = &self.breakpoint_mgr;

        mappings
            .iter()
            .filter_map(|m| {
                let value_bytes = all_values.get(&m.signal)
                    .or_else(|| {
                        // Try sanitized name as key
                        let sanitized = self.resolve_display_name(&m.signal);
                        all_values.get(&sanitized)
                    })?;
                let formatted = Self::format_value(value_bytes);

                // Check if changed from previous cycle
                let sanitized = self.resolve_display_name(&m.signal);
                let changed = previous
                    .get_previous_value(&sanitized)
                    .map(|prev| prev != value_bytes.as_slice())
                    .unwrap_or(false);

                Some(InlineValueInfo {
                    line: m.line,
                    signal: m.signal.clone(),
                    value: formatted,
                    changed,
                })
            })
            .collect()
    }

    fn handle_get_waveform_data(&self, from_cycle: Option<u64>, to_cycle: Option<u64>) -> Value {
        let from = from_cycle.unwrap_or(0);
        let to = to_cycle.unwrap_or(self.waveform_time);

        let mut changes: BTreeMap<String, Vec<(u64, String)>> = BTreeMap::new();

        for (name, signal) in &self.waveform.signals {
            let mut sig_changes = Vec::new();
            for (time, value) in &signal.values {
                if *time >= from && *time <= to {
                    sig_changes.push((*time, Self::format_value(value)));
                }
            }
            if !sig_changes.is_empty() {
                changes.insert(name.clone(), sig_changes);
            }
        }

        serde_json::json!({
            "from_cycle": from,
            "to_cycle": to,
            "changes": changes,
        })
    }

    async fn handle_step_back(&mut self) {
        if self.current_cycle == 0 {
            self.emit_stopped("step", &[]);
            return;
        }

        // Decrement current cycle
        self.current_cycle -= 1;

        // Reconstruct state from waveform data at the target waveform time
        // waveform_time tracks half-cycles; for step_back, we go back by 2 (full cycle)
        // unless waveform_time is already low
        if self.waveform_time >= 2 {
            self.waveform_time -= 2;
        } else {
            self.waveform_time = 0;
        }

        let target_time = self.waveform_time;

        // Reconstruct state from waveform
        let values = self.waveform.get_values_at_time(target_time);

        // Build a SimulationState from the waveform values
        let mut signals = IndexMap::new();
        let mut registers = IndexMap::new();
        let input_set: std::collections::BTreeSet<_> = self
            .input_names
            .iter()
            .flat_map(|n| self.display_to_sanitized.get(n))
            .collect();
        let register_set: std::collections::BTreeSet<_> = self
            .register_names
            .iter()
            .flat_map(|n| self.display_to_sanitized.get(n))
            .collect();

        for (display_name, value) in &values {
            // Get sanitized name
            let sanitized = self
                .display_to_sanitized
                .get(display_name)
                .cloned()
                .unwrap_or_else(|| display_name.clone());

            if input_set.contains(&sanitized) {
                // Update current_inputs
                let val = crate::breakpoint::bytes_to_u64(value);
                self.current_inputs.insert(display_name.clone(), val);
            } else if register_set.contains(&sanitized) {
                registers.insert(sanitized, value.clone());
            } else {
                signals.insert(sanitized, value.clone());
            }
        }

        let state = SimulationState {
            cycle: self.current_cycle,
            signals,
            registers,
        };

        // Update breakpoint manager previous values for edge detection
        let mut all_values = state.signals.clone();
        for (k, v) in &state.registers {
            all_values.insert(k.clone(), v.clone());
        }
        let hits = self
            .breakpoint_mgr
            .check_cycle(self.current_cycle, &all_values);

        self.last_state = Some(state);
        self.emit_stopped("step", &hits);
    }

    async fn handle_disconnect(&mut self) {
        // Export waveform to disk
        if let Some(ref path) = self.waveform_path {
            let design_name = self
                .source_file
                .as_ref()
                .and_then(|p| p.file_stem())
                .map(|s| s.to_string_lossy().to_string())
                .unwrap_or_else(|| "debug".to_string());

            if self.current_cycle > 0 {
                eprintln!(
                    "[skalp-debug] Exporting waveform ({} cycles) to {:?}",
                    self.current_cycle, path
                );
                let _ = self.waveform.export_skw(path, &design_name);
            }
        }

        self.emit(&serde_json::json!({
            "event": "terminated",
            "cycle": self.current_cycle,
        }));
    }

    // ------------------------------------------------------------------
    // Main command loop
    // ------------------------------------------------------------------

    /// Run the debug server, reading commands from stdin and emitting events
    /// to stdout. This is the main entry point for the `skalp-debug` binary.
    pub async fn run(&mut self) {
        let pause_flag = Arc::new(AtomicBool::new(false));

        // Spawn a thread to read stdin lines and send them through a channel.
        // This allows the continue loop to run asynchronously while still
        // receiving pause commands.
        let (tx, mut rx) = mpsc::unbounded_channel::<String>();
        let pause_flag_reader = pause_flag.clone();

        std::thread::spawn(move || {
            let stdin = io::stdin();
            let reader = stdin.lock();
            for line in reader.lines() {
                match line {
                    Ok(l) if !l.trim().is_empty() => {
                        // Fast-path: set pause flag immediately so the continue
                        // loop sees it without waiting for channel delivery.
                        if l.contains("\"pause\"") {
                            pause_flag_reader.store(true, Ordering::Relaxed);
                        }
                        if tx.send(l).is_err() {
                            break;
                        }
                    }
                    Ok(_) => {} // skip blank lines
                    Err(_) => break,
                }
            }
        });

        // Process commands from the channel
        loop {
            let line = match rx.recv().await {
                Some(l) => l,
                None => break, // stdin closed
            };

            let cmd: Command = match serde_json::from_str(&line) {
                Ok(c) => c,
                Err(e) => {
                    self.emit_error(&format!("Invalid command: {}", e));
                    continue;
                }
            };

            match cmd {
                Command::Launch {
                    file,
                    level,
                    top_module,
                    max_cycles,
                } => {
                    self.handle_launch(
                        &file,
                        level.as_deref(),
                        top_module.as_deref(),
                        max_cycles,
                    )
                    .await;
                }

                Command::Step { granularity } => {
                    self.handle_step(granularity.as_deref()).await;
                }

                Command::Continue { max_cycles } => {
                    self.handle_continue(max_cycles, &pause_flag).await;
                }

                Command::Pause => {
                    // If not in a continue loop, just acknowledge.
                    // (During continue, the flag is handled by handle_continue.)
                    self.emit_stopped("pause", &[]);
                }

                Command::SetBreakpoint {
                    signal,
                    condition,
                    value,
                } => {
                    let id =
                        self.handle_set_breakpoint(&signal, condition.as_deref(), value);
                    self.emit(&serde_json::json!({
                        "event": "breakpoint_set",
                        "id": id,
                        "signal": signal,
                    }));
                }

                Command::RemoveBreakpoint { id } => {
                    let success = self.breakpoint_mgr.remove(id);
                    self.emit(&serde_json::json!({
                        "event": "breakpoint_removed",
                        "id": id,
                        "success": success,
                    }));
                }

                Command::SetInput { name, value } => {
                    if let Some(sim) = self.simulator.as_mut() {
                        sim.set_input(&name, value).await;
                        self.current_inputs.insert(name.clone(), value);
                        self.emit(&serde_json::json!({
                            "event": "input_set",
                            "name": name,
                            "value": value,
                        }));
                    } else {
                        self.emit_error("Simulator not initialized");
                    }
                }

                Command::GetVariables { scope } => {
                    let vars = self.handle_get_variables(&scope);
                    self.emit(&serde_json::json!({
                        "event": "variables",
                        "scope": scope,
                        "vars": vars,
                    }));
                }

                Command::GetHierarchy => {
                    let modules = vec![serde_json::json!({
                        "name": "top",
                        "signals": self.signal_widths.keys().collect::<Vec<_>>(),
                        "children": [],
                    })];
                    self.emit(&serde_json::json!({
                        "event": "hierarchy",
                        "modules": modules,
                    }));
                }

                Command::GetSourceMap { file } => {
                    let mappings = self.source_map.get(&file).cloned().unwrap_or_default();
                    self.emit(&serde_json::json!({
                        "event": "source_map",
                        "file": file,
                        "mappings": mappings,
                    }));
                }

                Command::Evaluate { expr } => {
                    let result = self.handle_evaluate(&expr);
                    self.emit(&serde_json::json!({
                        "event": "evaluate",
                        "expression": expr,
                        "result": result,
                    }));
                }

                Command::GetInlineValues { file } => {
                    let values = self.handle_get_inline_values(&file);
                    self.emit(&serde_json::json!({
                        "event": "inline_values",
                        "file": file,
                        "values": values,
                    }));
                }

                Command::GetWaveformData {
                    from_cycle,
                    to_cycle,
                } => {
                    let data = self.handle_get_waveform_data(from_cycle, to_cycle);
                    self.emit(&serde_json::json!({
                        "event": "waveform_data",
                        "data": data,
                    }));
                }

                Command::StepBack => {
                    self.handle_step_back().await;
                }

                Command::Disconnect => {
                    self.handle_disconnect().await;
                    break;
                }
            }
        }
    }
}
