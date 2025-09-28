//! Simulation state management
//!
//! Manages signal values and state during GPU simulation

use bitvec::prelude::*;
use std::collections::{HashMap, HashSet};
use crate::sir::SirSignalId;
use crate::event::SimulationEvent;

/// Simulation state with change tracking
#[derive(Debug, Clone)]
pub struct SimulationState {
    /// Current signal values
    signal_values: HashMap<SirSignalId, BitVec>,
    /// Previous signal values (for edge detection)
    prev_signal_values: HashMap<SirSignalId, BitVec>,
    /// Signals that changed in the last cycle
    changed_signals: HashSet<SirSignalId>,
    /// Current simulation time in picoseconds
    current_time: u64,
    /// Current simulation cycle
    current_cycle: u64,
    /// Signals marked as clocks
    clock_signals: HashSet<SirSignalId>,
}

impl SimulationState {
    /// Create a new simulation state
    pub fn new(num_signals: usize) -> Self {
        Self {
            signal_values: HashMap::with_capacity(num_signals),
            prev_signal_values: HashMap::with_capacity(num_signals),
            changed_signals: HashSet::new(),
            current_time: 0,
            current_cycle: 0,
            clock_signals: HashSet::new(),
        }
    }

    /// Set signal value
    pub fn set_signal_value(&mut self, signal_id: SirSignalId, value: BitVec) {
        // Store previous value for edge detection
        if let Some(current) = self.signal_values.get(&signal_id) {
            if current != &value {
                self.prev_signal_values.insert(signal_id, current.clone());
                self.changed_signals.insert(signal_id);
            }
        } else {
            self.changed_signals.insert(signal_id);
        }

        self.signal_values.insert(signal_id, value);
    }

    /// Get signal value
    pub fn get_signal_value(&self, signal_id: SirSignalId) -> Option<BitVec> {
        self.signal_values.get(&signal_id).cloned()
    }

    /// Check if signal has changed in this cycle
    pub fn has_signal_changed(&self, signal_id: SirSignalId) -> bool {
        self.changed_signals.contains(&signal_id)
    }

    /// Check for clock edge
    pub fn has_clock_edge(&self, clock_id: SirSignalId) -> bool {
        // Check for positive edge (0 -> 1)
        if let (Some(prev), Some(curr)) = (
            self.prev_signal_values.get(&clock_id),
            self.signal_values.get(&clock_id)
        ) {
            // Check if the first bit transitioned from 0 to 1
            let prev_bit = prev.first().map(|b| *b).unwrap_or(false);
            let curr_bit = curr.first().map(|b| *b).unwrap_or(false);
            !prev_bit && curr_bit
        } else {
            false
        }
    }

    /// Check for negative clock edge
    pub fn has_negative_clock_edge(&self, clock_id: SirSignalId) -> bool {
        // Check for negative edge (1 -> 0)
        if let (Some(prev), Some(curr)) = (
            self.prev_signal_values.get(&clock_id),
            self.signal_values.get(&clock_id)
        ) {
            let prev_bit = prev.first().map(|b| *b).unwrap_or(false);
            let curr_bit = curr.first().map(|b| *b).unwrap_or(false);
            prev_bit && !curr_bit
        } else {
            false
        }
    }

    /// Mark a signal as a clock
    pub fn mark_as_clock(&mut self, signal_id: SirSignalId) {
        self.clock_signals.insert(signal_id);
    }

    /// Clear changed signals for next cycle
    pub fn clear_changed_signals(&mut self) {
        self.changed_signals.clear();
    }

    /// Advance simulation time
    pub fn advance_time(&mut self, delta_ps: u64) {
        self.current_time += delta_ps;
        self.current_cycle += 1;
    }

    /// Get current simulation time
    pub fn current_time(&self) -> u64 {
        self.current_time
    }

    /// Get current simulation cycle
    pub fn current_cycle(&self) -> u64 {
        self.current_cycle
    }

    /// Apply an event to the state
    pub fn apply_event(&mut self, event: &SimulationEvent) -> Result<(), Box<dyn std::error::Error>> {
        use crate::event::EventType;
        match &event.event_type {
            EventType::SignalChange { signal_id, new_value } => {
                self.set_signal_value(*signal_id, new_value.clone());
                Ok(())
            }
            EventType::TimeAdvance { delta } => {
                self.advance_time(*delta);
                Ok(())
            }
            _ => Ok(()), // Handle other event types as needed
        }
    }

    /// Create a state snapshot
    pub fn snapshot(&self) -> StateSnapshot {
        StateSnapshot {
            signal_values: self.signal_values.clone(),
            time: self.current_time,
            cycle: self.current_cycle,
        }
    }

    /// Restore from snapshot
    pub fn restore(&mut self, snapshot: &StateSnapshot) {
        self.signal_values = snapshot.signal_values.clone();
        self.current_time = snapshot.time;
        self.current_cycle = snapshot.cycle;
        self.changed_signals.clear();
        self.prev_signal_values.clear();
    }

    /// Get statistics about the current state
    pub fn get_statistics(&self) -> StateStatistics {
        let total_signals = self.signal_values.len();
        let total_bits = self.signal_values.values()
            .map(|v| v.len())
            .sum();
        let changed_count = self.changed_signals.len();

        StateStatistics {
            total_signals,
            total_bits,
            changed_signals: changed_count,
            clock_signals: self.clock_signals.len(),
        }
    }
}

impl Default for SimulationState {
    fn default() -> Self {
        Self::new(0)
    }
}

/// State snapshot for checkpointing
#[derive(Debug, Clone)]
pub struct StateSnapshot {
    /// Signal values at snapshot time
    pub signal_values: HashMap<SirSignalId, BitVec>,
    /// Simulation time at snapshot
    pub time: u64,
    /// Simulation cycle at snapshot
    pub cycle: u64,
}

/// State statistics
#[derive(Debug, Clone)]
pub struct StateStatistics {
    /// Total number of signals
    pub total_signals: usize,
    /// Total number of bits across all signals
    pub total_bits: usize,
    /// Number of signals that changed in last cycle
    pub changed_signals: usize,
    /// Number of clock signals
    pub clock_signals: usize,
}

/// Alias for backward compatibility
pub type SimState = SimulationState;