//! Breakpoint system for simulation debugging
//!
//! Provides breakpoint management for CPU and GPU simulation runtimes.
//! Breakpoints can trigger on:
//! - Signal value conditions (e.g., signal > 100)
//! - Signal transitions (rising/falling edge)
//! - Specific cycle numbers
//! - Named breakpoints with custom messages

use indexmap::IndexMap;
use skalp_frontend::hir::BreakpointConfig;

/// Action to take when a breakpoint triggers
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BreakpointAction {
    /// Pause simulation and wait for user interaction
    Pause,
    /// Log message and continue simulation
    Log,
    /// Stop simulation immediately (for error breakpoints)
    Stop,
}

/// Result of checking a breakpoint condition
#[derive(Debug, Clone)]
pub struct BreakpointHit {
    /// Name of the breakpoint (signal name or custom name)
    pub name: String,
    /// Signal that triggered the breakpoint
    pub signal_name: String,
    /// Current value of the signal (as bytes)
    pub signal_value: Vec<u8>,
    /// Cycle when the breakpoint was hit
    pub cycle: u64,
    /// Optional message to display
    pub message: Option<String>,
    /// Whether this is an error breakpoint
    pub is_error: bool,
    /// Action to take
    pub action: BreakpointAction,
}

/// A registered breakpoint in the simulator
#[derive(Debug, Clone)]
pub struct SimBreakpoint {
    /// Unique identifier for this breakpoint
    pub id: u32,
    /// Signal name to monitor
    pub signal_name: String,
    /// Display name for the breakpoint
    pub name: String,
    /// Condition expression (parsed from BreakpointConfig)
    pub condition: Option<BreakpointCondition>,
    /// Message to display when triggered
    pub message: Option<String>,
    /// Whether this is an error breakpoint (stops simulation)
    pub is_error: bool,
    /// Whether the breakpoint is enabled
    pub enabled: bool,
    /// Number of times this breakpoint has been hit
    pub hit_count: u64,
}

/// Parsed condition for efficient runtime evaluation
#[derive(Debug, Clone)]
pub enum BreakpointCondition {
    /// Trigger when signal is non-zero
    NonZero,
    /// Trigger when signal equals a specific value
    Equals(Vec<u8>),
    /// Trigger when signal does not equal a value
    NotEquals(Vec<u8>),
    /// Trigger when signal is greater than value (unsigned comparison)
    GreaterThan(u64),
    /// Trigger when signal is less than value (unsigned comparison)
    LessThan(u64),
    /// Trigger on rising edge (0 -> non-zero)
    RisingEdge,
    /// Trigger on falling edge (non-zero -> 0)
    FallingEdge,
    /// Trigger on any change
    AnyChange,
    /// Raw expression string (for complex conditions - evaluated by expression parser)
    Expression(String),
}

impl BreakpointCondition {
    /// Parse a condition string into a BreakpointCondition
    pub fn parse(condition_str: &str) -> Self {
        let condition = condition_str.trim();

        // Simple pattern matching for common conditions
        if condition.is_empty() {
            return BreakpointCondition::NonZero;
        }

        // Check for comparison operators
        if let Some(pos) = condition.find(">=") {
            if let Ok(val) = condition[pos + 2..].trim().parse::<u64>() {
                // >= is "not less than"
                return BreakpointCondition::Expression(condition.to_string());
            }
        }
        if let Some(pos) = condition.find("<=") {
            if let Ok(val) = condition[pos + 2..].trim().parse::<u64>() {
                return BreakpointCondition::Expression(condition.to_string());
            }
        }
        if let Some(pos) = condition.find("!=") {
            let value_str = condition[pos + 2..].trim();
            if let Some(bytes) = parse_value_to_bytes(value_str) {
                return BreakpointCondition::NotEquals(bytes);
            }
        }
        if let Some(pos) = condition.find("==") {
            let value_str = condition[pos + 2..].trim();
            if let Some(bytes) = parse_value_to_bytes(value_str) {
                return BreakpointCondition::Equals(bytes);
            }
        }
        if let Some(pos) = condition.find('>') {
            if let Ok(val) = condition[pos + 1..].trim().parse::<u64>() {
                return BreakpointCondition::GreaterThan(val);
            }
        }
        if let Some(pos) = condition.find('<') {
            if let Ok(val) = condition[pos + 1..].trim().parse::<u64>() {
                return BreakpointCondition::LessThan(val);
            }
        }

        // Fall back to expression for complex conditions
        BreakpointCondition::Expression(condition.to_string())
    }

    /// Check if the condition is satisfied
    pub fn check(&self, current_value: &[u8], previous_value: Option<&[u8]>) -> bool {
        match self {
            BreakpointCondition::NonZero => current_value.iter().any(|&b| b != 0),
            BreakpointCondition::Equals(expected) => {
                // Compare with zero-padding for different lengths
                values_equal(current_value, expected)
            }
            BreakpointCondition::NotEquals(expected) => !values_equal(current_value, expected),
            BreakpointCondition::GreaterThan(threshold) => {
                let current = bytes_to_u64(current_value);
                current > *threshold
            }
            BreakpointCondition::LessThan(threshold) => {
                let current = bytes_to_u64(current_value);
                current < *threshold
            }
            BreakpointCondition::RisingEdge => {
                if let Some(prev) = previous_value {
                    let prev_zero = prev.iter().all(|&b| b == 0);
                    let curr_nonzero = current_value.iter().any(|&b| b != 0);
                    prev_zero && curr_nonzero
                } else {
                    false
                }
            }
            BreakpointCondition::FallingEdge => {
                if let Some(prev) = previous_value {
                    let prev_nonzero = prev.iter().any(|&b| b != 0);
                    let curr_zero = current_value.iter().all(|&b| b == 0);
                    prev_nonzero && curr_zero
                } else {
                    false
                }
            }
            BreakpointCondition::AnyChange => {
                if let Some(prev) = previous_value {
                    current_value != prev
                } else {
                    false
                }
            }
            BreakpointCondition::Expression(_expr) => {
                // Complex expressions require full evaluation
                // For now, treat as NonZero check on the signal
                current_value.iter().any(|&b| b != 0)
            }
        }
    }
}

/// Manages breakpoints during simulation
#[derive(Debug, Default)]
pub struct BreakpointManager {
    /// Registered breakpoints, keyed by signal name
    breakpoints: IndexMap<String, Vec<SimBreakpoint>>,
    /// Previous values for edge detection
    previous_values: IndexMap<String, Vec<u8>>,
    /// Next breakpoint ID
    next_id: u32,
    /// Whether breakpoint checking is enabled
    enabled: bool,
    /// Breakpoint hits accumulated this cycle
    pending_hits: Vec<BreakpointHit>,
}

impl BreakpointManager {
    /// Create a new breakpoint manager
    pub fn new() -> Self {
        Self {
            breakpoints: IndexMap::new(),
            previous_values: IndexMap::new(),
            next_id: 0,
            enabled: true,
            pending_hits: Vec::new(),
        }
    }

    /// Register a breakpoint from HIR BreakpointConfig
    pub fn register_from_config(&mut self, signal_name: &str, config: &BreakpointConfig) -> u32 {
        let id = self.next_id;
        self.next_id += 1;

        let condition = config
            .condition
            .as_ref()
            .map(|c| BreakpointCondition::parse(c))
            .unwrap_or(BreakpointCondition::NonZero);

        let breakpoint = SimBreakpoint {
            id,
            signal_name: signal_name.to_string(),
            name: config
                .name
                .clone()
                .unwrap_or_else(|| signal_name.to_string()),
            condition: Some(condition),
            message: config.message.clone(),
            is_error: config.is_error,
            enabled: true,
            hit_count: 0,
        };

        self.breakpoints
            .entry(signal_name.to_string())
            .or_default()
            .push(breakpoint);

        id
    }

    /// Register a breakpoint with a specific condition
    pub fn register_with_condition(
        &mut self,
        signal_name: &str,
        name: &str,
        condition: BreakpointCondition,
    ) -> u32 {
        let id = self.next_id;
        self.next_id += 1;

        let breakpoint = SimBreakpoint {
            id,
            signal_name: signal_name.to_string(),
            name: name.to_string(),
            condition: Some(condition),
            message: None,
            is_error: false,
            enabled: true,
            hit_count: 0,
        };

        self.breakpoints
            .entry(signal_name.to_string())
            .or_default()
            .push(breakpoint);

        id
    }

    /// Register a simple breakpoint on a signal
    pub fn register_simple(&mut self, signal_name: &str) -> u32 {
        let id = self.next_id;
        self.next_id += 1;

        let breakpoint = SimBreakpoint {
            id,
            signal_name: signal_name.to_string(),
            name: signal_name.to_string(),
            condition: Some(BreakpointCondition::NonZero),
            message: None,
            is_error: false,
            enabled: true,
            hit_count: 0,
        };

        self.breakpoints
            .entry(signal_name.to_string())
            .or_default()
            .push(breakpoint);

        id
    }

    /// Enable or disable all breakpoints
    pub fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
    }

    /// Enable or disable a specific breakpoint by ID
    pub fn set_breakpoint_enabled(&mut self, id: u32, enabled: bool) -> bool {
        for bps in self.breakpoints.values_mut() {
            for bp in bps.iter_mut() {
                if bp.id == id {
                    bp.enabled = enabled;
                    return true;
                }
            }
        }
        false
    }

    /// Remove a breakpoint by ID
    pub fn remove(&mut self, id: u32) -> bool {
        for bps in self.breakpoints.values_mut() {
            if let Some(pos) = bps.iter().position(|bp| bp.id == id) {
                bps.remove(pos);
                return true;
            }
        }
        false
    }

    /// Check all breakpoints against current signal values
    /// Returns list of triggered breakpoints
    pub fn check_cycle(
        &mut self,
        cycle: u64,
        signals: &IndexMap<String, Vec<u8>>,
    ) -> Vec<BreakpointHit> {
        if !self.enabled {
            return Vec::new();
        }

        let mut hits = Vec::new();

        for (signal_name, bps) in &mut self.breakpoints {
            if let Some(current_value) = signals.get(signal_name) {
                let previous_value = self.previous_values.get(signal_name);

                for bp in bps.iter_mut() {
                    if !bp.enabled {
                        continue;
                    }

                    let triggered = if let Some(ref condition) = bp.condition {
                        condition.check(current_value, previous_value.map(|v| v.as_slice()))
                    } else {
                        // No condition means always trigger (shouldn't happen normally)
                        true
                    };

                    if triggered {
                        bp.hit_count += 1;

                        let action = if bp.is_error {
                            BreakpointAction::Stop
                        } else {
                            BreakpointAction::Pause
                        };

                        hits.push(BreakpointHit {
                            name: bp.name.clone(),
                            signal_name: signal_name.clone(),
                            signal_value: current_value.clone(),
                            cycle,
                            message: bp.message.clone(),
                            is_error: bp.is_error,
                            action,
                        });
                    }
                }
            }
        }

        // Update previous values for next cycle's edge detection
        for (name, value) in signals {
            self.previous_values.insert(name.clone(), value.clone());
        }

        hits
    }

    /// Get all registered breakpoints
    pub fn list_breakpoints(&self) -> Vec<&SimBreakpoint> {
        self.breakpoints.values().flatten().collect()
    }

    /// Get breakpoint by ID
    pub fn get_breakpoint(&self, id: u32) -> Option<&SimBreakpoint> {
        self.breakpoints.values().flatten().find(|bp| bp.id == id)
    }

    /// Seed a previous value for edge/change detection.
    /// Call this when setting a breakpoint mid-simulation so that
    /// AnyChange/RisingEdge/FallingEdge can detect the first transition.
    pub fn seed_previous_value(&mut self, signal_name: &str, value: Vec<u8>) {
        self.previous_values.insert(signal_name.to_string(), value);
    }

    /// Clear all breakpoints
    pub fn clear(&mut self) {
        self.breakpoints.clear();
        self.previous_values.clear();
        self.next_id = 0;
    }

    /// Get number of registered breakpoints
    pub fn count(&self) -> usize {
        self.breakpoints.values().map(|v| v.len()).sum()
    }
}

// Helper functions

fn parse_value_to_bytes(value_str: &str) -> Option<Vec<u8>> {
    let value_str = value_str.trim();

    // Handle hex format (0x...)
    if value_str.starts_with("0x") || value_str.starts_with("0X") {
        if let Ok(val) = u64::from_str_radix(&value_str[2..], 16) {
            return Some(val.to_le_bytes().to_vec());
        }
    }

    // Handle binary format (0b...)
    if value_str.starts_with("0b") || value_str.starts_with("0B") {
        if let Ok(val) = u64::from_str_radix(&value_str[2..], 2) {
            return Some(val.to_le_bytes().to_vec());
        }
    }

    // Handle decimal
    if let Ok(val) = value_str.parse::<u64>() {
        return Some(val.to_le_bytes().to_vec());
    }

    None
}

fn bytes_to_u64(bytes: &[u8]) -> u64 {
    let mut result = 0u64;
    for (i, &byte) in bytes.iter().take(8).enumerate() {
        result |= (byte as u64) << (i * 8);
    }
    result
}

fn values_equal(a: &[u8], b: &[u8]) -> bool {
    // Compare values, treating shorter arrays as zero-padded
    let max_len = a.len().max(b.len());
    for i in 0..max_len {
        let a_byte = a.get(i).copied().unwrap_or(0);
        let b_byte = b.get(i).copied().unwrap_or(0);
        if a_byte != b_byte {
            return false;
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_breakpoint_condition_parse() {
        // Test simple conditions
        assert!(matches!(
            BreakpointCondition::parse(""),
            BreakpointCondition::NonZero
        ));

        assert!(matches!(
            BreakpointCondition::parse("> 100"),
            BreakpointCondition::GreaterThan(100)
        ));

        assert!(matches!(
            BreakpointCondition::parse("< 50"),
            BreakpointCondition::LessThan(50)
        ));
    }

    #[test]
    fn test_breakpoint_condition_check() {
        let cond = BreakpointCondition::NonZero;
        assert!(cond.check(&[1], None));
        assert!(!cond.check(&[0], None));

        let cond = BreakpointCondition::GreaterThan(100);
        assert!(cond.check(&[101, 0, 0, 0, 0, 0, 0, 0], None));
        assert!(!cond.check(&[50, 0, 0, 0, 0, 0, 0, 0], None));
    }

    #[test]
    fn test_breakpoint_manager() {
        let mut manager = BreakpointManager::new();

        // Register a simple breakpoint
        let id = manager.register_simple("error_flag");
        assert_eq!(manager.count(), 1);

        // Check with signal = 0 (should not trigger)
        let mut signals = IndexMap::new();
        signals.insert("error_flag".to_string(), vec![0]);
        let hits = manager.check_cycle(0, &signals);
        assert!(hits.is_empty());

        // Check with signal = 1 (should trigger)
        signals.insert("error_flag".to_string(), vec![1]);
        let hits = manager.check_cycle(1, &signals);
        assert_eq!(hits.len(), 1);
        assert_eq!(hits[0].signal_name, "error_flag");

        // Remove the breakpoint
        assert!(manager.remove(id));
        assert_eq!(manager.count(), 0);
    }

    #[test]
    fn test_edge_detection() {
        let cond = BreakpointCondition::RisingEdge;

        // No previous value - no trigger
        assert!(!cond.check(&[1], None));

        // Previous 0, current 1 - rising edge
        assert!(cond.check(&[1], Some(&[0])));

        // Previous 1, current 1 - no edge
        assert!(!cond.check(&[1], Some(&[1])));

        let cond = BreakpointCondition::FallingEdge;

        // Previous 1, current 0 - falling edge
        assert!(cond.check(&[0], Some(&[1])));
    }
}
