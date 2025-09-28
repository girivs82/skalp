//! Test generators for hardware signals

use crate::{TestGenerator, TestingResult, Stimulus};

pub struct ClockGenerator {
    pub period_ns: u64,
    pub duty_cycle: f64,
}

impl TestGenerator for ClockGenerator {
    fn generate(&self, _rng: &mut dyn rand::Rng) -> TestingResult<Stimulus> {
        Ok(Stimulus::Clock {
            period_ns: self.period_ns,
            duty_cycle: self.duty_cycle,
        })
    }
}

pub struct RandomGenerator {
    pub min: u64,
    pub max: u64,
    pub num_cycles: usize,
}

impl TestGenerator for RandomGenerator {
    fn generate(&self, rng: &mut dyn rand::Rng) -> TestingResult<Stimulus> {
        Ok(Stimulus::Random {
            min: self.min,
            max: self.max,
            num_cycles: self.num_cycles,
        })
    }
}