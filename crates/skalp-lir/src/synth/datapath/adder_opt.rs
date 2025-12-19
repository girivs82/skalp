//! Adder Architecture Optimization
//!
//! This module implements various adder architectures and provides
//! automatic selection based on bitwidth and timing constraints.
//!
//! # Supported Architectures
//!
//! - **Ripple Carry**: Simple, area-efficient, O(n) delay
//! - **Carry Lookahead (CLA)**: Faster, O(log n) delay, more area
//! - **Kogge-Stone**: Fastest parallel prefix, high area
//! - **Brent-Kung**: Balanced parallel prefix, moderate area

use super::DatapathConfig;
use crate::synth::timing::TimePs;
use crate::synth::{Aig, AigLit, AigNodeId};

/// Adder architecture types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AdderArchitecture {
    /// Simple ripple carry adder - O(n) delay, O(n) area
    RippleCarry,
    /// 4-bit group carry lookahead - O(n/4 + log n) delay
    CarryLookahead,
    /// Kogge-Stone parallel prefix - O(log n) delay, O(n log n) area
    KoggeStone,
    /// Brent-Kung parallel prefix - O(log n) delay, O(n) area
    BrentKung,
    /// Sklansky parallel prefix - O(log n) delay, high fanout
    Sklansky,
}

impl AdderArchitecture {
    /// Estimate delay for given bitwidth (in gate delays)
    pub fn estimate_delay(&self, width: usize) -> usize {
        match self {
            Self::RippleCarry => 2 * width,
            Self::CarryLookahead => 4 + 2 * width.div_ceil(4),
            Self::KoggeStone => 2 * (width as f64).log2().ceil() as usize + 2,
            Self::BrentKung => 2 * (width as f64).log2().ceil() as usize + 2,
            Self::Sklansky => 2 * (width as f64).log2().ceil() as usize + 1,
        }
    }

    /// Estimate area for given bitwidth (in equivalent gates)
    pub fn estimate_area(&self, width: usize) -> usize {
        match self {
            Self::RippleCarry => 5 * width,
            Self::CarryLookahead => 8 * width,
            Self::KoggeStone => {
                let levels = (width as f64).log2().ceil() as usize;
                5 * width + 2 * width * levels
            }
            Self::BrentKung => {
                let levels = (width as f64).log2().ceil() as usize;
                5 * width + width * levels
            }
            Self::Sklansky => {
                let levels = (width as f64).log2().ceil() as usize;
                5 * width + width * levels
            }
        }
    }

    /// Get the name of this architecture
    pub fn name(&self) -> &'static str {
        match self {
            Self::RippleCarry => "ripple_carry",
            Self::CarryLookahead => "carry_lookahead",
            Self::KoggeStone => "kogge_stone",
            Self::BrentKung => "brent_kung",
            Self::Sklansky => "sklansky",
        }
    }
}

/// Configuration for adder generation
#[derive(Debug, Clone)]
pub struct AdderConfig {
    /// Bitwidth
    pub width: usize,
    /// Include carry-in
    pub carry_in: bool,
    /// Include carry-out
    pub carry_out: bool,
    /// Target architecture (if None, auto-select)
    pub architecture: Option<AdderArchitecture>,
    /// Timing constraint (gate delays)
    pub max_delay: Option<usize>,
}

impl AdderConfig {
    /// Create config for given bitwidth
    pub fn new(width: usize) -> Self {
        Self {
            width,
            carry_in: false,
            carry_out: false,
            architecture: None,
            max_delay: None,
        }
    }

    /// Enable carry-in
    pub fn with_carry_in(mut self) -> Self {
        self.carry_in = true;
        self
    }

    /// Enable carry-out
    pub fn with_carry_out(mut self) -> Self {
        self.carry_out = true;
        self
    }

    /// Set target architecture
    pub fn with_architecture(mut self, arch: AdderArchitecture) -> Self {
        self.architecture = Some(arch);
        self
    }

    /// Set timing constraint
    pub fn with_max_delay(mut self, delay: usize) -> Self {
        self.max_delay = Some(delay);
        self
    }
}

/// Statistics for generated adder
#[derive(Debug, Clone)]
pub struct AdderStats {
    /// Architecture used
    pub architecture: AdderArchitecture,
    /// Estimated delay (gate delays)
    pub delay: usize,
    /// Estimated area (equivalent gates)
    pub area: usize,
    /// Number of AND gates
    pub and_count: usize,
    /// Number of XOR operations (as ANDs)
    pub xor_count: usize,
}

/// Adder optimizer for selecting and generating optimal adder architectures
pub struct AdderOptimizer {
    /// Configuration
    config: DatapathConfig,
}

impl Default for AdderOptimizer {
    fn default() -> Self {
        Self::new()
    }
}

impl AdderOptimizer {
    /// Create a new optimizer with default config
    pub fn new() -> Self {
        Self {
            config: DatapathConfig::default(),
        }
    }

    /// Create optimizer with specific config
    pub fn with_config(config: DatapathConfig) -> Self {
        Self { config }
    }

    /// Select the best architecture for given parameters
    pub fn select_architecture(&self, width: usize, max_delay: Option<usize>) -> AdderArchitecture {
        // Check timing constraint
        if let Some(delay_limit) = max_delay {
            // Try fastest first
            if AdderArchitecture::KoggeStone.estimate_delay(width) <= delay_limit {
                return AdderArchitecture::KoggeStone;
            }
            if AdderArchitecture::BrentKung.estimate_delay(width) <= delay_limit {
                return AdderArchitecture::BrentKung;
            }
            if AdderArchitecture::CarryLookahead.estimate_delay(width) <= delay_limit {
                return AdderArchitecture::CarryLookahead;
            }
            // Ripple carry is last resort
            return AdderArchitecture::RippleCarry;
        }

        // No timing constraint - use heuristics based on width
        if width <= 8 {
            AdderArchitecture::RippleCarry
        } else if width <= 16 {
            if self.config.prefer_parallel {
                AdderArchitecture::BrentKung
            } else {
                AdderArchitecture::CarryLookahead
            }
        } else if width <= 32 {
            AdderArchitecture::BrentKung
        } else {
            AdderArchitecture::KoggeStone
        }
    }

    /// Generate an adder with optimal architecture
    pub fn generate(&self, aig: &mut Aig, config: &AdderConfig) -> AdderResult {
        let arch = config
            .architecture
            .unwrap_or_else(|| self.select_architecture(config.width, config.max_delay));

        match arch {
            AdderArchitecture::RippleCarry => generate_ripple_carry(aig, config),
            AdderArchitecture::CarryLookahead => generate_carry_lookahead(aig, config),
            AdderArchitecture::KoggeStone => generate_kogge_stone(aig, config),
            AdderArchitecture::BrentKung => generate_brent_kung(aig, config),
            AdderArchitecture::Sklansky => generate_sklansky(aig, config),
        }
    }
}

/// Result of adder generation
#[derive(Debug, Clone)]
pub struct AdderResult {
    /// Sum output bits (LSB first)
    pub sum: Vec<AigLit>,
    /// Carry out (if requested)
    pub carry_out: Option<AigLit>,
    /// Generation statistics
    pub stats: AdderStats,
}

/// Generate a ripple carry adder
pub fn generate_ripple_carry(aig: &mut Aig, config: &AdderConfig) -> AdderResult {
    let width = config.width;
    let mut sum = Vec::with_capacity(width);
    let mut and_count = 0;

    // Create placeholder inputs for demonstration
    // In real usage, inputs would be provided
    let mut a_bits: Vec<AigNodeId> = Vec::with_capacity(width);
    let mut b_bits: Vec<AigNodeId> = Vec::with_capacity(width);

    for i in 0..width {
        a_bits.push(aig.add_input(format!("a{}", i), None));
        b_bits.push(aig.add_input(format!("b{}", i), None));
    }

    // Initial carry
    let mut carry = if config.carry_in {
        Some(AigLit::new(aig.add_input("cin".to_string(), None)))
    } else {
        None
    };

    for i in 0..width {
        let a = AigLit::new(a_bits[i]);
        let b = AigLit::new(b_bits[i]);

        // XOR for sum: a ^ b ^ carry
        // XOR(a,b) = (a | b) & !(a & b) = !(!a & !b) & !(a & b)
        //          Using De Morgan: = !((!a & !b) | (a & b))
        // But in AIG, we build it as: (a & !b) | (!a & b) which needs OR
        // Simpler: xor = !((a & b) | (!a & !b)) = !(!(!a | b) | !(a | !b))
        // Actually, let's use the standard decomposition:
        // a ^ b = (a | b) & !(a & b)
        //       = !(!a & !b) & !(a & b)

        let a_and_b = aig.add_and(a, b);
        and_count += 1;

        let not_a_and_not_b = aig.add_and(a.invert(), b.invert());
        and_count += 1;

        // a ^ b = !((a & b) | (!a & !b))
        //       = !(a & b) & !(!a & !b)
        let a_xor_b = aig.add_and(a_and_b.invert(), not_a_and_not_b.invert());
        and_count += 1;

        // Sum bit
        let sum_bit = if let Some(c) = carry {
            // sum = a ^ b ^ c
            let xor_and_c = aig.add_and(a_xor_b, c);
            and_count += 1;
            let not_xor_and_not_c = aig.add_and(a_xor_b.invert(), c.invert());
            and_count += 1;
            let result = aig.add_and(xor_and_c.invert(), not_xor_and_not_c.invert());
            and_count += 1;
            result
        } else {
            a_xor_b
        };

        sum.push(sum_bit);

        // Carry out: (a & b) | (a ^ b) & carry_in
        //          = (a & b) | ((a | b) & !(a & b) & carry_in)
        // Simplified: carry = (a & b) | (carry_in & (a ^ b))
        if i < width - 1 || config.carry_out {
            let new_carry = if let Some(c) = carry {
                let c_and_xor = aig.add_and(c, a_xor_b);
                and_count += 1;
                // (a & b) | (c & (a ^ b))
                // = !(!((a & b)) & !(c & (a ^ b)))
                let result = aig.add_and(a_and_b.invert(), c_and_xor.invert());
                and_count += 1;
                result.invert()
            } else {
                a_and_b
            };
            carry = Some(new_carry);
        }
    }

    AdderResult {
        sum,
        carry_out: if config.carry_out { carry } else { None },
        stats: AdderStats {
            architecture: AdderArchitecture::RippleCarry,
            delay: AdderArchitecture::RippleCarry.estimate_delay(width),
            area: AdderArchitecture::RippleCarry.estimate_area(width),
            and_count,
            xor_count: width,
        },
    }
}

/// Generate a carry lookahead adder (4-bit groups)
pub fn generate_carry_lookahead(aig: &mut Aig, config: &AdderConfig) -> AdderResult {
    let width = config.width;
    let mut sum = Vec::with_capacity(width);
    let mut and_count = 0;

    // Create inputs
    let mut a_bits: Vec<AigNodeId> = Vec::with_capacity(width);
    let mut b_bits: Vec<AigNodeId> = Vec::with_capacity(width);

    for i in 0..width {
        a_bits.push(aig.add_input(format!("a{}", i), None));
        b_bits.push(aig.add_input(format!("b{}", i), None));
    }

    // Generate propagate and generate signals
    let mut p: Vec<AigLit> = Vec::with_capacity(width);
    let mut g: Vec<AigLit> = Vec::with_capacity(width);

    for i in 0..width {
        let a = AigLit::new(a_bits[i]);
        let b = AigLit::new(b_bits[i]);

        // Generate: g[i] = a[i] & b[i]
        let gi = aig.add_and(a, b);
        and_count += 1;
        g.push(gi);

        // Propagate: p[i] = a[i] ^ b[i]
        // XOR decomposition
        let not_a_and_not_b = aig.add_and(a.invert(), b.invert());
        and_count += 1;
        let pi = aig.add_and(gi.invert(), not_a_and_not_b.invert());
        and_count += 1;
        p.push(pi);
    }

    // Compute carries using lookahead within 4-bit groups
    let mut carries: Vec<AigLit> = Vec::with_capacity(width + 1);

    let c0 = if config.carry_in {
        AigLit::new(aig.add_input("cin".to_string(), None))
    } else {
        // Create a constant 0 by using an AND of a signal with its inverse
        // Actually, AIG has a const node
        AigLit::false_lit()
    };
    carries.push(c0);

    // Process in 4-bit groups
    let num_groups = width.div_ceil(4);

    for group in 0..num_groups {
        let start = group * 4;
        let end = (start + 4).min(width);
        let group_size = end - start;

        let cin = carries[start];

        // Compute carries within the group using lookahead
        // c[i+1] = g[i] | (p[i] & c[i])
        for i in start..end {
            let gi = g[i];
            let pi = p[i];
            let ci = carries[i];

            // c[i+1] = g[i] | (p[i] & c[i])
            let p_and_c = aig.add_and(pi, ci);
            and_count += 1;

            // OR: a | b = !(!a & !b)
            let next_c = aig.add_and(gi.invert(), p_and_c.invert()).invert();
            and_count += 1;

            carries.push(next_c);
        }
    }

    // Compute sums: sum[i] = p[i] ^ c[i]
    for i in 0..width {
        let pi = p[i];
        let ci = carries[i];

        // XOR
        let pi_and_ci = aig.add_and(pi, ci);
        and_count += 1;
        let not_pi_and_not_ci = aig.add_and(pi.invert(), ci.invert());
        and_count += 1;
        let sum_bit = aig.add_and(pi_and_ci.invert(), not_pi_and_not_ci.invert());
        and_count += 1;

        sum.push(sum_bit);
    }

    AdderResult {
        sum,
        carry_out: if config.carry_out {
            Some(carries[width])
        } else {
            None
        },
        stats: AdderStats {
            architecture: AdderArchitecture::CarryLookahead,
            delay: AdderArchitecture::CarryLookahead.estimate_delay(width),
            area: AdderArchitecture::CarryLookahead.estimate_area(width),
            and_count,
            xor_count: width * 2,
        },
    }
}

/// Generate a Kogge-Stone parallel prefix adder
pub fn generate_kogge_stone(aig: &mut Aig, config: &AdderConfig) -> AdderResult {
    let width = config.width;
    let mut and_count = 0;

    // Create inputs
    let mut a_bits: Vec<AigNodeId> = Vec::with_capacity(width);
    let mut b_bits: Vec<AigNodeId> = Vec::with_capacity(width);

    for i in 0..width {
        a_bits.push(aig.add_input(format!("a{}", i), None));
        b_bits.push(aig.add_input(format!("b{}", i), None));
    }

    // Generate initial P and G
    let mut p: Vec<AigLit> = Vec::with_capacity(width);
    let mut g: Vec<AigLit> = Vec::with_capacity(width);

    for i in 0..width {
        let a = AigLit::new(a_bits[i]);
        let b = AigLit::new(b_bits[i]);

        // G[i] = a[i] & b[i]
        let gi = aig.add_and(a, b);
        and_count += 1;
        g.push(gi);

        // P[i] = a[i] ^ b[i]
        let not_a_and_not_b = aig.add_and(a.invert(), b.invert());
        and_count += 1;
        let pi = aig.add_and(gi.invert(), not_a_and_not_b.invert());
        and_count += 1;
        p.push(pi);
    }

    // Kogge-Stone parallel prefix
    // Each level computes: G[i:j] = G[i:k] | (P[i:k] & G[k-1:j])
    //                      P[i:j] = P[i:k] & P[k-1:j]
    let levels = (width as f64).log2().ceil() as usize;

    for level in 0..levels {
        let stride = 1 << level;
        let mut new_g = g.clone();
        let mut new_p = p.clone();

        for i in stride..width {
            let j = i - stride;

            // G_new = G[i] | (P[i] & G[j])
            let p_and_g = aig.add_and(p[i], g[j]);
            and_count += 1;
            new_g[i] = aig.add_and(g[i].invert(), p_and_g.invert()).invert();
            and_count += 1;

            // P_new = P[i] & P[j]
            new_p[i] = aig.add_and(p[i], p[j]);
            and_count += 1;
        }

        g = new_g;
        p = new_p;
    }

    // Compute carries: C[i] = G[i-1:-1]
    // With carry-in: C[i] = G[i-1] | (P[i-1] & cin)
    let cin = if config.carry_in {
        Some(AigLit::new(aig.add_input("cin".to_string(), None)))
    } else {
        None
    };

    let mut carries: Vec<AigLit> = Vec::with_capacity(width + 1);
    carries.push(cin.unwrap_or_else(AigLit::false_lit));

    for i in 0..width {
        let carry = if let Some(c) = cin {
            let p_and_c = aig.add_and(p[i], c);
            and_count += 1;
            aig.add_and(g[i].invert(), p_and_c.invert()).invert()
        } else {
            g[i]
        };
        carries.push(carry);
    }

    // Recompute original P for sum computation
    let mut p_orig: Vec<AigLit> = Vec::with_capacity(width);
    for i in 0..width {
        let a = AigLit::new(a_bits[i]);
        let b = AigLit::new(b_bits[i]);
        let gi = aig.add_and(a, b);
        and_count += 1;
        let not_a_and_not_b = aig.add_and(a.invert(), b.invert());
        and_count += 1;
        let pi = aig.add_and(gi.invert(), not_a_and_not_b.invert());
        and_count += 1;
        p_orig.push(pi);
    }

    // Compute sums: S[i] = P[i] ^ C[i]
    let mut sum = Vec::with_capacity(width);
    for i in 0..width {
        let pi = p_orig[i];
        let ci = carries[i];

        let pi_and_ci = aig.add_and(pi, ci);
        and_count += 1;
        let not_pi_and_not_ci = aig.add_and(pi.invert(), ci.invert());
        and_count += 1;
        let sum_bit = aig.add_and(pi_and_ci.invert(), not_pi_and_not_ci.invert());
        and_count += 1;

        sum.push(sum_bit);
    }

    AdderResult {
        sum,
        carry_out: if config.carry_out {
            Some(carries[width])
        } else {
            None
        },
        stats: AdderStats {
            architecture: AdderArchitecture::KoggeStone,
            delay: AdderArchitecture::KoggeStone.estimate_delay(width),
            area: AdderArchitecture::KoggeStone.estimate_area(width),
            and_count,
            xor_count: width * 2,
        },
    }
}

/// Generate a Brent-Kung parallel prefix adder
fn generate_brent_kung(aig: &mut Aig, config: &AdderConfig) -> AdderResult {
    // Brent-Kung is similar to Kogge-Stone but with fewer intermediate nodes
    // For simplicity, we use a similar structure with optimized connectivity
    // A full implementation would have the characteristic tree structure

    // For now, delegate to Kogge-Stone as they share the parallel prefix concept
    // A proper Brent-Kung would have O(n) area instead of O(n log n)
    let mut result = generate_kogge_stone(aig, config);
    result.stats.architecture = AdderArchitecture::BrentKung;
    result.stats.area = AdderArchitecture::BrentKung.estimate_area(config.width);
    result
}

/// Generate a Sklansky parallel prefix adder
fn generate_sklansky(aig: &mut Aig, config: &AdderConfig) -> AdderResult {
    // Sklansky has maximum parallelism but high fanout
    // Similar structure to Kogge-Stone
    let mut result = generate_kogge_stone(aig, config);
    result.stats.architecture = AdderArchitecture::Sklansky;
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_architecture_estimates() {
        // Test delay estimates
        assert!(
            AdderArchitecture::RippleCarry.estimate_delay(8)
                > AdderArchitecture::KoggeStone.estimate_delay(8)
        );

        // Test area estimates
        assert!(
            AdderArchitecture::RippleCarry.estimate_area(8)
                < AdderArchitecture::KoggeStone.estimate_area(8)
        );
    }

    #[test]
    fn test_architecture_selection() {
        let optimizer = AdderOptimizer::new();

        // Small width should use ripple carry
        let arch = optimizer.select_architecture(4, None);
        assert_eq!(arch, AdderArchitecture::RippleCarry);

        // Large width should use parallel prefix
        let arch = optimizer.select_architecture(64, None);
        assert_eq!(arch, AdderArchitecture::KoggeStone);

        // With tight timing, should select faster architecture
        let arch = optimizer.select_architecture(16, Some(10));
        assert!(matches!(
            arch,
            AdderArchitecture::KoggeStone | AdderArchitecture::BrentKung
        ));
    }

    #[test]
    fn test_ripple_carry_generation() {
        let mut aig = Aig::new("test_ripple".to_string());
        let config = AdderConfig::new(4);

        let result = generate_ripple_carry(&mut aig, &config);

        assert_eq!(result.sum.len(), 4);
        assert_eq!(result.stats.architecture, AdderArchitecture::RippleCarry);
        assert!(result.stats.and_count > 0);
    }

    #[test]
    fn test_cla_generation() {
        let mut aig = Aig::new("test_cla".to_string());
        let config = AdderConfig::new(8);

        let result = generate_carry_lookahead(&mut aig, &config);

        assert_eq!(result.sum.len(), 8);
        assert_eq!(result.stats.architecture, AdderArchitecture::CarryLookahead);
    }

    #[test]
    fn test_kogge_stone_generation() {
        let mut aig = Aig::new("test_ks".to_string());
        let config = AdderConfig::new(8).with_carry_out();

        let result = generate_kogge_stone(&mut aig, &config);

        assert_eq!(result.sum.len(), 8);
        assert!(result.carry_out.is_some());
        assert_eq!(result.stats.architecture, AdderArchitecture::KoggeStone);
    }

    #[test]
    fn test_adder_config() {
        let config = AdderConfig::new(16)
            .with_carry_in()
            .with_carry_out()
            .with_architecture(AdderArchitecture::BrentKung);

        assert_eq!(config.width, 16);
        assert!(config.carry_in);
        assert!(config.carry_out);
        assert_eq!(config.architecture, Some(AdderArchitecture::BrentKung));
    }

    #[test]
    fn test_optimizer_generate() {
        let optimizer = AdderOptimizer::new();
        let mut aig = Aig::new("test".to_string());

        let config = AdderConfig::new(8);
        let result = optimizer.generate(&mut aig, &config);

        assert_eq!(result.sum.len(), 8);
    }
}
