//! Test generation strategies

pub struct RandomStrategy {
    pub seed: Option<u64>,
}

impl Default for RandomStrategy {
    fn default() -> Self {
        Self::new()
    }
}

impl RandomStrategy {
    pub fn new() -> Self {
        Self { seed: None }
    }
}

pub struct DirectedStrategy {
    pub target_states: Vec<String>,
}

impl Default for DirectedStrategy {
    fn default() -> Self {
        Self::new()
    }
}

impl DirectedStrategy {
    pub fn new() -> Self {
        Self {
            target_states: Vec::new(),
        }
    }
}
