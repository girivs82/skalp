//! Test generators for hardware signals

use crate::{TestGenerator, TestingResult, Stimulus};
use rand::Rng;
use rand::rngs::StdRng;
use rand_distr::Distribution as _;
use std::collections::HashMap;

/// Clock signal generator
pub struct ClockGenerator {
    pub period_ns: u64,
    pub duty_cycle: f64,
    pub phase_shift_ns: u64,
    pub jitter_percent: f64,
}

impl ClockGenerator {
    pub fn new(period_ns: u64) -> Self {
        Self {
            period_ns,
            duty_cycle: 0.5,
            phase_shift_ns: 0,
            jitter_percent: 0.0,
        }
    }

    pub fn with_duty_cycle(mut self, duty_cycle: f64) -> Self {
        self.duty_cycle = duty_cycle.clamp(0.1, 0.9);
        self
    }

    pub fn with_phase_shift(mut self, phase_shift_ns: u64) -> Self {
        self.phase_shift_ns = phase_shift_ns;
        self
    }

    pub fn with_jitter(mut self, jitter_percent: f64) -> Self {
        self.jitter_percent = jitter_percent.clamp(0.0, 10.0);
        self
    }
}

impl TestGenerator for ClockGenerator {
    fn generate(&self, rng: &mut StdRng) -> TestingResult<Stimulus> {
        let mut period = self.period_ns as f64;

        // Add jitter if specified
        if self.jitter_percent > 0.0 {
            let jitter_range = period * (self.jitter_percent / 100.0);
            let jitter = rng.gen_range(-jitter_range..=jitter_range);
            period += jitter;
        }

        Ok(Stimulus::Clock {
            period_ns: period as u64,
            duty_cycle: self.duty_cycle,
        })
    }
}

/// Random value generator
pub struct RandomGenerator {
    pub width: usize,
    pub min: u64,
    pub max: u64,
    pub distribution: Distribution,
}

impl RandomGenerator {
    pub fn new(width: usize) -> Self {
        let max = (1u64 << width) - 1;
        Self {
            width,
            min: 0,
            max,
            distribution: Distribution::Uniform,
        }
    }

    pub fn with_range(mut self, min: u64, max: u64) -> Self {
        self.min = min;
        self.max = max;
        self
    }

    pub fn with_distribution(mut self, distribution: Distribution) -> Self {
        self.distribution = distribution;
        self
    }
}

impl TestGenerator for RandomGenerator {
    fn generate(&self, rng: &mut StdRng) -> TestingResult<Stimulus> {
        let value = match self.distribution {
            Distribution::Uniform => rng.gen_range(self.min..=self.max),
            Distribution::Normal { mean, std_dev } => {
                let normal = rand_distr::Normal::new(mean, std_dev).unwrap();
                let val: f64 = rng.sample(normal);
                val.clamp(self.min as f64, self.max as f64) as u64
            }
            Distribution::Weighted(ref weights) => {
                // Select value based on weights
                self.weighted_selection(rng, weights)
            }
        };

        Ok(Stimulus::Value { value, width: self.width })
    }
}

impl RandomGenerator {
    fn weighted_selection(&self, rng: &mut StdRng, weights: &[(u64, f64)]) -> u64 {
        let total_weight: f64 = weights.iter().map(|(_, w)| w).sum();
        let mut selection = rng.gen_range(0.0..total_weight);

        for (value, weight) in weights {
            selection -= weight;
            if selection <= 0.0 {
                return *value;
            }
        }

        weights.last().map(|(v, _)| *v).unwrap_or(0)
    }
}

/// Pattern-based generator
pub struct PatternGenerator {
    pub pattern: Vec<u64>,
    pub width: usize,
    pub repeat_count: Option<usize>,
}

impl PatternGenerator {
    pub fn new(pattern: Vec<u64>, width: usize) -> Self {
        Self {
            pattern,
            width,
            repeat_count: None,
        }
    }

    pub fn with_repeat(mut self, count: usize) -> Self {
        self.repeat_count = Some(count);
        self
    }

    /// Create a walking-ones pattern
    pub fn walking_ones(width: usize) -> Self {
        let mut pattern = Vec::new();
        for i in 0..width {
            pattern.push(1u64 << i);
        }
        Self::new(pattern, width)
    }

    /// Create a walking-zeros pattern
    pub fn walking_zeros(width: usize) -> Self {
        let mut pattern = Vec::new();
        let all_ones = (1u64 << width) - 1;
        for i in 0..width {
            pattern.push(all_ones ^ (1u64 << i));
        }
        Self::new(pattern, width)
    }

    /// Create a checkerboard pattern
    pub fn checkerboard(width: usize) -> Self {
        let pattern1 = 0xAAAAAAAAAAAAAAAAu64 & ((1u64 << width) - 1);
        let pattern2 = 0x5555555555555555u64 & ((1u64 << width) - 1);
        Self::new(vec![pattern1, pattern2], width)
    }
}

impl TestGenerator for PatternGenerator {
    fn generate(&self, _rng: &mut StdRng) -> TestingResult<Stimulus> {
        Ok(Stimulus::Pattern {
            values: self.pattern.clone(),
            width: self.width,
            repeat_count: self.repeat_count,
        })
    }
}

/// Constrained random generator
pub struct ConstrainedRandomGenerator {
    pub width: usize,
    pub constraints: Vec<Constraint>,
}

impl ConstrainedRandomGenerator {
    pub fn new(width: usize) -> Self {
        Self {
            width,
            constraints: Vec::new(),
        }
    }

    pub fn add_constraint(mut self, constraint: Constraint) -> Self {
        self.constraints.push(constraint);
        self
    }

    fn satisfies_constraints(&self, value: u64) -> bool {
        self.constraints.iter().all(|c| c.check(value))
    }
}

impl TestGenerator for ConstrainedRandomGenerator {
    fn generate(&self, rng: &mut StdRng) -> TestingResult<Stimulus> {
        let max_attempts = 1000;
        let max_value = (1u64 << self.width) - 1;

        for _ in 0..max_attempts {
            let value = rng.gen_range(0..=max_value);
            if self.satisfies_constraints(value) {
                return Ok(Stimulus::Value { value, width: self.width });
            }
        }

        // If we can't find a valid value, use constraint solving
        let value = self.solve_constraints();
        Ok(Stimulus::Value { value, width: self.width })
    }
}

impl ConstrainedRandomGenerator {
    fn solve_constraints(&self) -> u64 {
        // Simplified constraint solving - would use an SMT solver in production
        0
    }
}

/// Distribution types for random generation
#[derive(Clone, Debug)]
pub enum Distribution {
    Uniform,
    Normal { mean: f64, std_dev: f64 },
    Weighted(Vec<(u64, f64)>),
}

/// Constraint for constrained random generation
#[derive(Clone)]
pub enum Constraint {
    Range { min: u64, max: u64 },
    NotEqual(u64),
    OneOf(Vec<u64>),
    #[allow(dead_code)]
    Custom(std::sync::Arc<dyn Fn(u64) -> bool + Send + Sync>),
}

impl std::fmt::Debug for Constraint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constraint::Range { min, max } => write!(f, "Range {{ min: {}, max: {} }}", min, max),
            Constraint::NotEqual(v) => write!(f, "NotEqual({})", v),
            Constraint::OneOf(values) => write!(f, "OneOf({:?})", values),
            Constraint::Custom(_) => write!(f, "Custom(<function>)"),
        }
    }
}

impl Constraint {
    fn check(&self, value: u64) -> bool {
        match self {
            Constraint::Range { min, max } => value >= *min && value <= *max,
            Constraint::NotEqual(v) => value != *v,
            Constraint::OneOf(values) => values.contains(&value),
            Constraint::Custom(f) => f(value),
        }
    }
}

/// Bus transaction generator
pub struct TransactionGenerator {
    pub address_width: usize,
    pub data_width: usize,
    pub transaction_types: Vec<TransactionType>,
    pub burst_length: Option<usize>,
}

impl TransactionGenerator {
    pub fn new(address_width: usize, data_width: usize) -> Self {
        Self {
            address_width,
            data_width,
            transaction_types: vec![TransactionType::Read, TransactionType::Write],
            burst_length: None,
        }
    }

    pub fn with_burst(mut self, length: usize) -> Self {
        self.burst_length = Some(length);
        self
    }
}

impl TestGenerator for TransactionGenerator {
    fn generate(&self, rng: &mut StdRng) -> TestingResult<Stimulus> {
        let max_addr = (1u64 << self.address_width) - 1;
        let max_data = (1u64 << self.data_width) - 1;

        let trans_type = &self.transaction_types[rng.gen_range(0..self.transaction_types.len())];
        let address = rng.gen_range(0..=max_addr);

        let data = match trans_type {
            TransactionType::Write => Some(rng.gen_range(0..=max_data)),
            _ => None,
        };

        Ok(Stimulus::Transaction {
            transaction_type: trans_type.clone(),
            address,
            data,
            burst_length: self.burst_length,
        })
    }
}

#[derive(Clone, Debug)]
pub enum TransactionType {
    Read,
    Write,
    ReadModifyWrite,
    Idle,
}