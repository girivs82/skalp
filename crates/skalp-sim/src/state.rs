//! Simulation state management
//!
//! Manages signal values and state during GPU simulation

use bitvec::prelude::*;
use std::collections::HashMap;

/// Simulation state
#[derive(Debug, Clone)]
pub struct SimState {
    /// Signal values (signal_id -> value)
    signal_values: HashMap<u32, BitVec>,
    /// Current simulation time
    current_time: u64,
}

impl SimState {
    /// Create a new simulation state
    pub fn new() -> Self {
        Self {
            signal_values: HashMap::new(),
            current_time: 0,
        }
    }

    /// Set signal value
    pub fn set_signal(&mut self, signal_id: u32, value: BitVec) {
        self.signal_values.insert(signal_id, value);
    }

    /// Get signal value
    pub fn get_signal(&self, signal_id: u32) -> Option<&BitVec> {
        self.signal_values.get(&signal_id)
    }

    /// Get current time
    pub fn current_time(&self) -> u64 {
        self.current_time
    }

    /// Advance simulation time
    pub fn advance_time(&mut self, delta: u64) {
        self.current_time += delta;
    }
}

impl Default for SimState {
    fn default() -> Self {
        Self::new()
    }
}