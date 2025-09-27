//! Event system for simulation
//!
//! Handles timing and event scheduling in GPU simulation

/// Simulation event
#[derive(Debug, Clone)]
pub struct SimEvent {
    /// Event time
    pub time: u64,
    /// Event data
    pub data: EventData,
}

/// Event data types
#[derive(Debug, Clone)]
pub enum EventData {
    /// Clock edge
    ClockEdge { signal_id: u32, rising: bool },
    /// Signal change
    SignalChange { signal_id: u32, value: u64 },
}

/// Event queue for simulation
#[derive(Debug)]
pub struct EventQueue {
    events: Vec<SimEvent>,
}

impl EventQueue {
    /// Create a new event queue
    pub fn new() -> Self {
        Self { events: Vec::new() }
    }

    /// Add an event
    pub fn add_event(&mut self, event: SimEvent) {
        self.events.push(event);
        // Keep sorted by time
        self.events.sort_by_key(|e| e.time);
    }

    /// Get next event
    pub fn pop_event(&mut self) -> Option<SimEvent> {
        if self.events.is_empty() {
            None
        } else {
            Some(self.events.remove(0))
        }
    }
}

impl Default for EventQueue {
    fn default() -> Self {
        Self::new()
    }
}