//! Event system for simulation
//!
//! Handles timing and event scheduling in GPU simulation

use bitvec::prelude::*;
use std::collections::BinaryHeap;
use std::cmp::{Ordering, Reverse};
use crate::sir::SirSignalId;

/// Simulation event with timing information
#[derive(Debug, Clone)]
pub struct SimulationEvent {
    /// Event time in picoseconds
    pub time: u64,
    /// Delta cycle within this time (for same-time events)
    pub delta: u32,
    /// Event type
    pub event_type: EventType,
}

impl SimulationEvent {
    /// Create a new simulation event
    pub fn new(time: u64, delta: u32, event_type: EventType) -> Self {
        Self { time, delta, event_type }
    }

    /// Create a signal change event
    pub fn signal_change(time: u64, signal_id: SirSignalId, new_value: BitVec) -> Self {
        Self {
            time,
            delta: 0,
            event_type: EventType::SignalChange { signal_id, new_value },
        }
    }

    /// Create a time advance event
    pub fn time_advance(delta: u64) -> Self {
        Self {
            time: 0, // Will be updated by queue
            delta: 0,
            event_type: EventType::TimeAdvance { delta },
        }
    }
}

/// Types of simulation events
#[derive(Debug, Clone)]
pub enum EventType {
    /// Signal value change
    SignalChange {
        signal_id: SirSignalId,
        new_value: BitVec,
    },
    /// Clock edge event
    ClockEdge {
        signal_id: SirSignalId,
        rising: bool,
    },
    /// Time advance
    TimeAdvance {
        delta: u64,
    },
    /// Process activation
    ProcessActivation {
        process_id: u32,
    },
    /// Memory write
    MemoryWrite {
        address: BitVec,
        data: BitVec,
    },
}

/// Wrapper for event ordering in priority queue
#[derive(Debug, Clone)]
struct EventWrapper {
    event: SimulationEvent,
}

impl PartialEq for EventWrapper {
    fn eq(&self, other: &Self) -> bool {
        self.event.time == other.event.time && self.event.delta == other.event.delta
    }
}

impl Eq for EventWrapper {}

impl PartialOrd for EventWrapper {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for EventWrapper {
    fn cmp(&self, other: &Self) -> Ordering {
        // First compare time (reverse for min-heap behavior)
        match self.event.time.cmp(&other.event.time).reverse() {
            Ordering::Equal => {
                // Then compare delta cycles (reverse for min-heap)
                self.event.delta.cmp(&other.event.delta).reverse()
            }
            other => other,
        }
    }
}

/// Priority-based event queue for simulation
#[derive(Debug)]
pub struct EventQueue {
    /// Priority queue of events (min-heap by time)
    events: BinaryHeap<Reverse<EventWrapper>>,
    /// Current simulation time
    current_time: u64,
    /// Current delta cycle
    current_delta: u32,
    /// Statistics
    total_events_processed: u64,
}

impl EventQueue {
    /// Create a new event queue
    pub fn new() -> Self {
        Self {
            events: BinaryHeap::new(),
            current_time: 0,
            current_delta: 0,
            total_events_processed: 0,
        }
    }

    /// Schedule an event
    pub fn schedule_event(&mut self, mut event: SimulationEvent) {
        // Adjust time if it's a relative event
        if matches!(event.event_type, EventType::TimeAdvance { .. }) {
            event.time = self.current_time + event.time;
        }

        // Ensure event is not in the past
        if event.time < self.current_time ||
           (event.time == self.current_time && event.delta < self.current_delta) {
            // Schedule for next delta cycle
            event.time = self.current_time;
            event.delta = self.current_delta + 1;
        }

        self.events.push(Reverse(EventWrapper { event }));
    }

    /// Get the next event
    pub fn pop_next_event(&mut self) -> Option<SimulationEvent> {
        if let Some(Reverse(wrapper)) = self.events.pop() {
            self.current_time = wrapper.event.time;
            self.current_delta = wrapper.event.delta;
            self.total_events_processed += 1;
            Some(wrapper.event)
        } else {
            None
        }
    }

    /// Peek at the next event without removing it
    pub fn peek_next_event(&self) -> Option<&SimulationEvent> {
        self.events.peek().map(|Reverse(w)| &w.event)
    }

    /// Check if queue is empty
    pub fn is_empty(&self) -> bool {
        self.events.is_empty()
    }

    /// Get the current simulation time
    pub fn current_time(&self) -> u64 {
        self.current_time
    }

    /// Get the current delta cycle
    pub fn current_delta(&self) -> u32 {
        self.current_delta
    }

    /// Advance to next delta cycle
    pub fn next_delta_cycle(&mut self) {
        self.current_delta += 1;
    }

    /// Advance simulation time
    pub fn advance_time(&mut self, delta: u64) {
        self.current_time += delta;
        self.current_delta = 0;
    }

    /// Get statistics
    pub fn get_statistics(&self) -> EventQueueStatistics {
        EventQueueStatistics {
            pending_events: self.events.len(),
            total_processed: self.total_events_processed,
            current_time: self.current_time,
            current_delta: self.current_delta,
        }
    }

    /// Clear all events
    pub fn clear(&mut self) {
        self.events.clear();
    }
}

impl Default for EventQueue {
    fn default() -> Self {
        Self::new()
    }
}

/// Event queue statistics
#[derive(Debug, Clone)]
pub struct EventQueueStatistics {
    /// Number of pending events
    pub pending_events: usize,
    /// Total events processed
    pub total_processed: u64,
    /// Current simulation time
    pub current_time: u64,
    /// Current delta cycle
    pub current_delta: u32,
}

/// Aliases for compatibility
pub use SimulationEvent as SimEvent;
pub use EventType as EventData;