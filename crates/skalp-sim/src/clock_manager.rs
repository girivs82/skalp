use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ClockEdge {
    None = 0,
    Rising = 1,
    Falling = 2,
    Both = 3,
}

#[derive(Debug, Clone)]
pub struct ClockInfo {
    pub name: String,
    pub current_value: bool,
    pub previous_value: bool,
    pub period_ps: u64,  // Period in picoseconds
    pub duty_cycle: f32, // 0.0 to 1.0
}

impl ClockInfo {
    pub fn new(name: String, period_ps: u64) -> Self {
        ClockInfo {
            name,
            current_value: false,
            previous_value: false,
            period_ps,
            duty_cycle: 0.5,
        }
    }

    pub fn detect_edge(&self) -> ClockEdge {
        match (self.previous_value, self.current_value) {
            (false, true) => ClockEdge::Rising,
            (true, false) => ClockEdge::Falling,
            _ => ClockEdge::None,
        }
    }

    pub fn update(&mut self, new_value: bool) {
        self.previous_value = self.current_value;
        self.current_value = new_value;
    }

    pub fn toggle(&mut self) {
        self.update(!self.current_value);
    }
}

pub struct ClockManager {
    pub clocks: HashMap<String, ClockInfo>,
    current_time_ps: u64,
    auto_toggle: bool,
}

impl Default for ClockManager {
    fn default() -> Self {
        Self::new()
    }
}

impl ClockManager {
    pub fn new() -> Self {
        ClockManager {
            clocks: HashMap::new(),
            current_time_ps: 0,
            auto_toggle: false,
        }
    }

    pub fn with_auto_toggle(mut self) -> Self {
        self.auto_toggle = true;
        self
    }

    pub fn add_clock(&mut self, name: String, period_ps: u64) {
        self.clocks
            .insert(name.clone(), ClockInfo::new(name, period_ps));
    }

    pub fn set_clock(&mut self, name: &str, value: bool) -> Option<ClockEdge> {
        if let Some(clock) = self.clocks.get_mut(name) {
            clock.update(value);
            Some(clock.detect_edge())
        } else {
            None
        }
    }

    pub fn toggle_clock(&mut self, name: &str) -> Option<ClockEdge> {
        if let Some(clock) = self.clocks.get_mut(name) {
            clock.toggle();
            Some(clock.detect_edge())
        } else {
            None
        }
    }

    pub fn get_clock_value(&self, name: &str) -> Option<bool> {
        self.clocks.get(name).map(|c| c.current_value)
    }

    pub fn get_clock_edge(&self, name: &str) -> Option<ClockEdge> {
        self.clocks.get(name).map(|c| c.detect_edge())
    }

    pub fn advance_time(&mut self, delta_ps: u64) {
        self.current_time_ps += delta_ps;

        if self.auto_toggle {
            // Auto-toggle clocks based on their periods
            for clock in self.clocks.values_mut() {
                let half_period = clock.period_ps / 2;
                let phase = (self.current_time_ps / half_period) % 2;
                let should_be_high = phase == 1;

                if clock.current_value != should_be_high {
                    clock.update(should_be_high);
                }
            }
        }
    }

    pub fn get_all_edges(&self) -> HashMap<String, ClockEdge> {
        self.clocks
            .iter()
            .map(|(name, clock)| (name.clone(), clock.detect_edge()))
            .collect()
    }

    pub fn reset(&mut self) {
        self.current_time_ps = 0;
        for clock in self.clocks.values_mut() {
            clock.current_value = false;
            clock.previous_value = false;
        }
    }
}
