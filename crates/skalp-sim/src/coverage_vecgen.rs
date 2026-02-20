//! Coverage-directed vector generation
//!
//! Three-phase test vector generation:
//! - **Phase 1 (Systematic)**: Deterministic patterns - all-zeros, all-ones, walking-one,
//!   walking-zero, boundary values. Ensures each bit toggles at least once.
//! - **Phase 2 (LFSR sweep)**: Cheap pseudo-random sweep for broad coverage.
//! - **Phase 3 (Coverage-biased)**: Targeted generation based on coverage gaps.

use crate::sim_coverage::SimCoverageDb;
use skalp_sir::SirPort;

/// Input signal info
#[derive(Debug, Clone)]
struct InputInfo {
    name: String,
    width: usize,
}

/// A test vector: set of (input_name, value) pairs
#[derive(Debug, Clone)]
pub struct InputVector {
    pub values: Vec<(String, u64)>,
}

/// State for systematic generation
#[derive(Debug, Clone)]
enum SysState {
    AllZeros,
    AllOnes,
    WalkingOne { input_idx: usize, bit: usize },
    WalkingZero { input_idx: usize, bit: usize },
    Boundary { input_idx: usize, case: usize },
    Done,
}

/// Generation phase
#[derive(Debug, Clone)]
enum Phase {
    Systematic(SysState),
    LfsrSweep {
        seed: u64,
        budget: usize,
        spent: usize,
    },
    CoverageBiased {
        budget: usize,
        spent: usize,
        seed: u64,
    },
    Done,
}

/// Three-phase coverage-directed vector generator
pub struct CoverageVectorGen {
    phase: Phase,
    input_info: Vec<InputInfo>,
    /// Default budget for LFSR phase
    lfsr_budget: usize,
    /// Default budget for coverage-biased phase
    bias_budget: usize,
    /// Coverage goal percentage (stop early if reached)
    coverage_goal: f64,
}

impl CoverageVectorGen {
    /// Create a new vector generator from SIR input ports
    pub fn new(inputs: &[SirPort], seed: u64) -> Self {
        let input_info: Vec<InputInfo> = inputs
            .iter()
            .filter(|p| {
                let name = p.name.to_lowercase();
                // Skip clock and reset
                !name.contains("clk")
                    && !name.contains("clock")
                    && !name.contains("rst")
                    && !name.contains("reset")
            })
            .map(|p| InputInfo {
                name: p.name.clone(),
                width: p.width,
            })
            .collect();

        Self {
            phase: Phase::Systematic(SysState::AllZeros),
            input_info,
            lfsr_budget: 1000,
            bias_budget: 500,
            coverage_goal: 90.0,
        }
    }

    /// Create from (name, width) pairs (for use with matched input info in EC)
    pub fn from_input_info(inputs: &[(String, usize)], seed: u64) -> Self {
        let input_info: Vec<InputInfo> = inputs
            .iter()
            .filter(|(name, _)| {
                let name = name.to_lowercase();
                !name.contains("clk")
                    && !name.contains("clock")
                    && !name.contains("rst")
                    && !name.contains("reset")
            })
            .map(|(name, width)| InputInfo {
                name: name.clone(),
                width: *width,
            })
            .collect();

        Self {
            phase: Phase::Systematic(SysState::AllZeros),
            input_info,
            lfsr_budget: 1000,
            bias_budget: 500,
            coverage_goal: 90.0,
        }
    }

    /// Set LFSR phase budget
    pub fn with_lfsr_budget(mut self, budget: usize) -> Self {
        self.lfsr_budget = budget;
        self
    }

    /// Set coverage-biased phase budget
    pub fn with_bias_budget(mut self, budget: usize) -> Self {
        self.bias_budget = budget;
        self
    }

    /// Set coverage goal
    pub fn with_coverage_goal(mut self, goal: f64) -> Self {
        self.coverage_goal = goal;
        self
    }

    /// Get the total number of data inputs (excluding clock/reset)
    pub fn input_count(&self) -> usize {
        self.input_info.len()
    }

    /// Get current phase name
    pub fn phase_name(&self) -> &'static str {
        match &self.phase {
            Phase::Systematic(_) => "systematic",
            Phase::LfsrSweep { .. } => "lfsr",
            Phase::CoverageBiased { .. } => "biased",
            Phase::Done => "done",
        }
    }

    /// Generate next vector. Accepts coverage state for Phase 3 bias.
    /// Returns None when all phases exhausted.
    pub fn next(&mut self, coverage: Option<&SimCoverageDb>) -> Option<InputVector> {
        loop {
            match &self.phase {
                Phase::Done => return None,
                Phase::Systematic(_) => {
                    if let Some(vec) = self.next_systematic() {
                        return Some(vec);
                    }
                    // Systematic exhausted, move to LFSR
                    self.phase = Phase::LfsrSweep {
                        seed: 0x12345678u64,
                        budget: self.lfsr_budget,
                        spent: 0,
                    };
                }
                Phase::LfsrSweep { .. } => {
                    if let Some(vec) = self.next_lfsr() {
                        return Some(vec);
                    }
                    // Check if coverage goal already met
                    if let Some(cov) = coverage {
                        let m = cov.metrics();
                        if m.overall_pct >= self.coverage_goal {
                            self.phase = Phase::Done;
                            return None;
                        }
                    }
                    // LFSR exhausted, move to coverage-biased
                    self.phase = Phase::CoverageBiased {
                        budget: self.bias_budget,
                        spent: 0,
                        seed: 0xDEADBEEF,
                    };
                }
                Phase::CoverageBiased { .. } => {
                    // Check if coverage goal met
                    if let Some(cov) = coverage {
                        let m = cov.metrics();
                        if m.overall_pct >= self.coverage_goal {
                            self.phase = Phase::Done;
                            return None;
                        }
                    }
                    if let Some(vec) = self.next_biased(coverage) {
                        return Some(vec);
                    }
                    self.phase = Phase::Done;
                    return None;
                }
            }
        }
    }

    /// Generate the next systematic vector
    fn next_systematic(&mut self) -> Option<InputVector> {
        let state = match &self.phase {
            Phase::Systematic(s) => s.clone(),
            _ => return None,
        };

        if self.input_info.is_empty() {
            self.phase = Phase::Systematic(SysState::Done);
            return None;
        }

        match state {
            SysState::AllZeros => {
                // All inputs = 0
                let values = self
                    .input_info
                    .iter()
                    .map(|i| (i.name.clone(), 0u64))
                    .collect();
                self.phase = Phase::Systematic(SysState::AllOnes);
                Some(InputVector { values })
            }
            SysState::AllOnes => {
                // All inputs = max value
                let values = self
                    .input_info
                    .iter()
                    .map(|i| {
                        let max = if i.width >= 64 {
                            u64::MAX
                        } else {
                            (1u64 << i.width) - 1
                        };
                        (i.name.clone(), max)
                    })
                    .collect();
                self.phase = Phase::Systematic(SysState::WalkingOne {
                    input_idx: 0,
                    bit: 0,
                });
                Some(InputVector { values })
            }
            SysState::WalkingOne { input_idx, bit } => {
                if input_idx >= self.input_info.len() {
                    self.phase = Phase::Systematic(SysState::WalkingZero {
                        input_idx: 0,
                        bit: 0,
                    });
                    return self.next_systematic();
                }

                let info = &self.input_info[input_idx];
                if bit >= info.width {
                    // Move to next input
                    self.phase = Phase::Systematic(SysState::WalkingOne {
                        input_idx: input_idx + 1,
                        bit: 0,
                    });
                    return self.next_systematic();
                }

                // Set all inputs to 0 except input_idx bit `bit`
                let values = self
                    .input_info
                    .iter()
                    .enumerate()
                    .map(|(i, info)| {
                        if i == input_idx {
                            (info.name.clone(), 1u64 << bit)
                        } else {
                            (info.name.clone(), 0u64)
                        }
                    })
                    .collect();

                self.phase = Phase::Systematic(SysState::WalkingOne {
                    input_idx,
                    bit: bit + 1,
                });
                Some(InputVector { values })
            }
            SysState::WalkingZero { input_idx, bit } => {
                if input_idx >= self.input_info.len() {
                    self.phase = Phase::Systematic(SysState::Boundary {
                        input_idx: 0,
                        case: 0,
                    });
                    return self.next_systematic();
                }

                let info = &self.input_info[input_idx];
                if bit >= info.width {
                    self.phase = Phase::Systematic(SysState::WalkingZero {
                        input_idx: input_idx + 1,
                        bit: 0,
                    });
                    return self.next_systematic();
                }

                // Set all inputs to max except input_idx bit `bit` cleared
                let values = self
                    .input_info
                    .iter()
                    .enumerate()
                    .map(|(i, ii)| {
                        let max = if ii.width >= 64 {
                            u64::MAX
                        } else {
                            (1u64 << ii.width) - 1
                        };
                        if i == input_idx {
                            (ii.name.clone(), max & !(1u64 << bit))
                        } else {
                            (ii.name.clone(), max)
                        }
                    })
                    .collect();

                self.phase = Phase::Systematic(SysState::WalkingZero {
                    input_idx,
                    bit: bit + 1,
                });
                Some(InputVector { values })
            }
            SysState::Boundary { input_idx, case } => {
                if input_idx >= self.input_info.len() {
                    self.phase = Phase::Systematic(SysState::Done);
                    return None;
                }

                // Boundary cases per input: 0, 1, max-1, max, 0x5555..., 0xAAAA...
                const NUM_BOUNDARY_CASES: usize = 6;

                if case >= NUM_BOUNDARY_CASES {
                    self.phase = Phase::Systematic(SysState::Boundary {
                        input_idx: input_idx + 1,
                        case: 0,
                    });
                    return self.next_systematic();
                }

                let info = &self.input_info[input_idx];
                let max = if info.width >= 64 {
                    u64::MAX
                } else {
                    (1u64 << info.width) - 1
                };

                let boundary_val = match case {
                    0 => 0u64,
                    1 => 1u64,
                    2 => max.saturating_sub(1),
                    3 => max,
                    4 => 0x5555_5555_5555_5555u64 & max,
                    5 => 0xAAAA_AAAA_AAAA_AAAAu64 & max,
                    _ => unreachable!(),
                };

                let values = self
                    .input_info
                    .iter()
                    .enumerate()
                    .map(|(i, ii)| {
                        if i == input_idx {
                            (ii.name.clone(), boundary_val)
                        } else {
                            (ii.name.clone(), 0u64)
                        }
                    })
                    .collect();

                self.phase = Phase::Systematic(SysState::Boundary {
                    input_idx,
                    case: case + 1,
                });
                Some(InputVector { values })
            }
            SysState::Done => None,
        }
    }

    /// Generate the next LFSR vector
    fn next_lfsr(&mut self) -> Option<InputVector> {
        let (seed, budget, spent) = match &mut self.phase {
            Phase::LfsrSweep {
                seed,
                budget,
                spent,
            } => (seed, *budget, spent),
            _ => return None,
        };

        if *spent >= budget {
            return None;
        }

        let values = self
            .input_info
            .iter()
            .map(|info| {
                // LFSR step (same algorithm as existing EC code)
                *seed = seed.wrapping_mul(6364136223846793005).wrapping_add(1);
                let mask = if info.width >= 64 {
                    u64::MAX
                } else {
                    (1u64 << info.width) - 1
                };
                (info.name.clone(), *seed & mask)
            })
            .collect();

        *spent += 1;
        Some(InputVector { values })
    }

    /// Generate a coverage-biased vector
    fn next_biased(&mut self, coverage: Option<&SimCoverageDb>) -> Option<InputVector> {
        let (budget, spent, current_seed) = match &mut self.phase {
            Phase::CoverageBiased {
                budget,
                spent,
                seed,
            } => (*budget, *spent, *seed),
            _ => return None,
        };

        if spent >= budget {
            return None;
        }

        // Simple PRNG for biased generation
        let mut seed = current_seed
            .wrapping_mul(6364136223846793005)
            .wrapping_add(0xCAFEBABE);

        let values = if let Some(cov) = coverage {
            let v = self.generate_biased_vector(cov, seed);
            seed = seed.wrapping_mul(6364136223846793005).wrapping_add(1);
            v
        } else {
            // No coverage info, fall back to random
            self.input_info
                .iter()
                .map(|info| {
                    seed = seed.wrapping_mul(6364136223846793005).wrapping_add(1);
                    let mask = if info.width >= 64 {
                        u64::MAX
                    } else {
                        (1u64 << info.width) - 1
                    };
                    (info.name.clone(), seed & mask)
                })
                .collect()
        };

        // Write back the mutated state
        if let Phase::CoverageBiased {
            spent: s, seed: sd, ..
        } = &mut self.phase
        {
            *s = spent + 1;
            *sd = seed;
        }

        Some(InputVector { values })
    }

    /// Generate a biased vector targeting coverage gaps
    fn generate_biased_vector(&self, cov: &SimCoverageDb, mut seed: u64) -> Vec<(String, u64)> {
        // Start with random base
        let mut values: Vec<(String, u64)> = self
            .input_info
            .iter()
            .map(|info| {
                seed = seed.wrapping_mul(6364136223846793005).wrapping_add(1);
                let mask = if info.width >= 64 {
                    u64::MAX
                } else {
                    (1u64 << info.width) - 1
                };
                (info.name.clone(), seed & mask)
            })
            .collect();

        // Try to target uncovered toggle bits:
        // If an input bit has never toggled, force it to a specific value
        let uncovered_toggles = cov.uncovered_toggles();
        for (sig_name, bit, direction) in &uncovered_toggles {
            // Find this signal in our inputs
            if let Some(pos) = values.iter().position(|(name, _)| name == sig_name) {
                match *direction {
                    "0->1" => {
                        // Need to see this bit go to 1
                        values[pos].1 |= 1u64 << bit;
                    }
                    "1->0" => {
                        // Need to see this bit go to 0
                        values[pos].1 &= !(1u64 << bit);
                    }
                    _ => {
                        // "neither" - alternate between 0 and 1
                        seed = seed.wrapping_mul(6364136223846793005).wrapping_add(1);
                        if seed & 1 == 0 {
                            values[pos].1 |= 1u64 << bit;
                        } else {
                            values[pos].1 &= !(1u64 << bit);
                        }
                    }
                }
            }
        }

        // Mask all values to their actual width
        for (i, info) in self.input_info.iter().enumerate() {
            if info.width < 64 {
                values[i].1 &= (1u64 << info.width) - 1;
            }
        }

        values
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use skalp_sir::{sir::PortDirection, SirType};

    fn make_port(name: &str, width: usize) -> SirPort {
        SirPort {
            name: name.to_string(),
            width,
            sir_type: SirType::Bits(width),
            direction: PortDirection::Input,
            clock_domain: None,
            span: None,
        }
    }

    #[test]
    fn test_systematic_basic() {
        let ports = vec![make_port("a", 2), make_port("b", 1)];
        let mut gen = CoverageVectorGen::new(&ports, 0);

        // Phase 1: systematic
        assert_eq!(gen.phase_name(), "systematic");

        // All zeros
        let v = gen.next(None).unwrap();
        assert_eq!(v.values.len(), 2);
        assert_eq!(v.values[0], ("a".to_string(), 0));
        assert_eq!(v.values[1], ("b".to_string(), 0));

        // All ones
        let v = gen.next(None).unwrap();
        assert_eq!(v.values[0], ("a".to_string(), 3)); // 2-bit max
        assert_eq!(v.values[1], ("b".to_string(), 1)); // 1-bit max
    }

    #[test]
    fn test_phases_transition() {
        let ports = vec![make_port("x", 1)];
        let mut gen = CoverageVectorGen::new(&ports, 0)
            .with_lfsr_budget(2)
            .with_bias_budget(1);

        // Exhaust systematic
        assert_eq!(gen.phase_name(), "systematic");
        let mut sys_count = 0;
        while gen.phase_name() == "systematic" {
            if gen.next(None).is_none() {
                break;
            }
            sys_count += 1;
        }
        // For 1-bit input: all-zeros, all-ones, walking-one(1), walking-zero(1), boundary(6) = 10
        // But some boundary values duplicate for 1-bit: 0, 1, 0, 1, 1, 0 => all emitted
        assert!(sys_count > 0);

        // Now in LFSR phase
        assert_eq!(gen.phase_name(), "lfsr");
        let _ = gen.next(None);
        let _ = gen.next(None);

        // After LFSR budget exhausted, should move to biased
        let _ = gen.next(None);
        // Either biased or done
        assert!(gen.phase_name() == "biased" || gen.phase_name() == "done");
    }

    #[test]
    fn test_skips_clock_reset() {
        let ports = vec![
            make_port("clk", 1),
            make_port("rst", 1),
            make_port("data", 8),
        ];
        let gen = CoverageVectorGen::new(&ports, 0);
        // Should only have 1 input (data)
        assert_eq!(gen.input_count(), 1);
    }
}
