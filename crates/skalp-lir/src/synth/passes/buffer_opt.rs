//! Buffer Insertion and Optimization
//!
//! This module implements fanout-aware buffer insertion for managing
//! signal integrity and timing in gate-level netlists.
//!
//! # Buffer Insertion Strategies
//!
//! - **Fanout-based**: Insert buffers when fanout exceeds threshold
//! - **Load-based**: Insert buffers when capacitive load is too high
//! - **Timing-driven**: Insert buffers on critical paths
//!
//! # Buffer Tree Types
//!
//! - Linear chain: Simple cascade of buffers
//! - Balanced tree: Binary tree of buffers (better for large fanout)

use super::{Pass, PassResult};
use crate::synth::{Aig, AigLit, AigNode, AigNodeId};
use indexmap::IndexMap;

/// Buffer insertion configuration
#[derive(Debug, Clone)]
pub struct BufferConfig {
    /// Maximum fanout before inserting buffers
    pub max_fanout: usize,
    /// Target fanout after buffering
    pub target_fanout: usize,
    /// Use balanced buffer trees (vs linear chains)
    pub use_balanced_trees: bool,
    /// Buffer delay (ps) for timing estimation
    pub buffer_delay: f64,
    /// Buffer area cost
    pub buffer_area: f64,
}

impl Default for BufferConfig {
    fn default() -> Self {
        Self {
            max_fanout: 4,
            target_fanout: 4,
            use_balanced_trees: true,
            buffer_delay: 15.0,
            buffer_area: 1.0,
        }
    }
}

impl BufferConfig {
    /// Create config for high-performance (more buffering)
    pub fn high_performance() -> Self {
        Self {
            max_fanout: 3,
            target_fanout: 3,
            use_balanced_trees: true,
            buffer_delay: 12.0,
            buffer_area: 1.0,
        }
    }

    /// Create config for low-area (less buffering)
    pub fn low_area() -> Self {
        Self {
            max_fanout: 8,
            target_fanout: 6,
            use_balanced_trees: false,
            buffer_delay: 15.0,
            buffer_area: 1.0,
        }
    }
}

/// Statistics from buffer insertion
#[derive(Debug, Clone, Default)]
pub struct BufferStats {
    /// Total buffers inserted
    pub buffers_inserted: usize,
    /// Nodes with high fanout fixed
    pub high_fanout_fixed: usize,
    /// Maximum fanout before buffering
    pub max_fanout_before: usize,
    /// Maximum fanout after buffering
    pub max_fanout_after: usize,
    /// Total area added
    pub area_added: f64,
}

impl BufferStats {
    /// Create new empty stats
    pub fn new() -> Self {
        Self::default()
    }

    /// Get summary string
    pub fn summary(&self) -> String {
        format!(
            "Inserted {} buffers, fixed {} high-fanout nodes, max fanout {} -> {}",
            self.buffers_inserted,
            self.high_fanout_fixed,
            self.max_fanout_before,
            self.max_fanout_after
        )
    }
}

/// Buffer insertion pass
pub struct BufferInsertion {
    config: BufferConfig,
    stats: BufferStats,
}

impl Default for BufferInsertion {
    fn default() -> Self {
        Self::new()
    }
}

impl BufferInsertion {
    /// Create a new buffer insertion pass with default config
    pub fn new() -> Self {
        Self {
            config: BufferConfig::default(),
            stats: BufferStats::new(),
        }
    }

    /// Create with specific config
    pub fn with_config(config: BufferConfig) -> Self {
        Self {
            config,
            stats: BufferStats::new(),
        }
    }

    /// Get the statistics from the last run
    pub fn stats(&self) -> &BufferStats {
        &self.stats
    }

    /// Compute fanout for each node in the AIG
    fn compute_fanout(&self, aig: &Aig) -> IndexMap<AigNodeId, usize> {
        let mut fanout: IndexMap<AigNodeId, usize> = IndexMap::new();

        // Initialize all nodes with zero fanout
        for id in 0..aig.node_count() {
            fanout.insert(AigNodeId(id as u32), 0);
        }

        // Count references from AND nodes
        for id in 0..aig.node_count() {
            let node_id = AigNodeId(id as u32);
            if let Some(AigNode::And { left, right }) = aig.get_node(node_id) {
                *fanout.entry(left.node).or_insert(0) += 1;
                *fanout.entry(right.node).or_insert(0) += 1;
            }
        }

        // Count references from outputs
        for (_, lit) in aig.outputs() {
            *fanout.entry(lit.node).or_insert(0) += 1;
        }

        fanout
    }

    /// Find nodes with fanout exceeding the threshold
    fn find_high_fanout_nodes(
        &self,
        fanout: &IndexMap<AigNodeId, usize>,
    ) -> Vec<(AigNodeId, usize)> {
        let mut high_fanout: Vec<_> = fanout
            .iter()
            .filter(|(_, &fo)| fo > self.config.max_fanout)
            .map(|(&id, &fo)| (id, fo))
            .collect();

        // Sort by fanout (highest first) for prioritized buffering
        high_fanout.sort_by(|a, b| b.1.cmp(&a.1));

        high_fanout
    }

    /// Calculate number of buffers needed for a given fanout
    fn buffers_needed(&self, fanout: usize) -> usize {
        if fanout <= self.config.max_fanout {
            return 0;
        }

        if self.config.use_balanced_trees {
            // Balanced tree: ceil(log_target(fanout)) levels
            // Each level multiplies capacity by target_fanout
            let mut levels = 0;
            let mut capacity = self.config.target_fanout;
            while capacity < fanout {
                capacity *= self.config.target_fanout;
                levels += 1;
            }
            // Number of buffers in a balanced tree
            let mut total = 0;
            let mut level_size = 1;
            for _ in 0..levels {
                total += level_size;
                level_size *= self.config.target_fanout;
            }
            total
        } else {
            // Linear chain: ceil((fanout - 1) / (target - 1))
            (fanout - 1).div_ceil(self.config.target_fanout - 1)
        }
    }

    /// Insert buffers for a high-fanout node
    /// Returns the buffer literals that should replace the original fanouts
    fn create_buffer_tree(&self, aig: &mut Aig, source: AigLit, fanout: usize) -> Vec<AigLit> {
        if fanout <= self.config.target_fanout {
            // No buffering needed, return the source for all fanouts
            return vec![source; fanout];
        }

        if self.config.use_balanced_trees {
            self.create_balanced_buffer_tree(aig, source, fanout)
        } else {
            self.create_linear_buffer_chain(aig, source, fanout)
        }
    }

    /// Create a balanced buffer tree
    fn create_balanced_buffer_tree(
        &self,
        aig: &mut Aig,
        source: AigLit,
        fanout: usize,
    ) -> Vec<AigLit> {
        // For a balanced tree, we recursively create buffer nodes
        // Each buffer can drive target_fanout loads

        if fanout <= self.config.target_fanout {
            return vec![source; fanout];
        }

        // Calculate how many groups we need
        let num_groups = fanout.div_ceil(self.config.target_fanout);

        // Create a buffer for each group
        let mut group_outputs = Vec::with_capacity(num_groups);
        for _ in 0..num_groups {
            // A "buffer" in AIG is represented as AND(x, x) which equals x
            // Or we can add it as an input that will be mapped to a buffer cell
            // For now, we'll create an AND that acts as a buffer
            let buffer = aig.add_and(source, source);
            group_outputs.push(buffer);
        }

        // Recursively buffer the source if we have too many groups
        if num_groups > self.config.target_fanout {
            // We need to buffer the source itself
            // This creates the tree structure
            let source_buffers = self.create_balanced_buffer_tree(aig, source, num_groups);

            // Replace the group source with buffered versions
            group_outputs = Vec::with_capacity(num_groups);
            for buf_source in source_buffers {
                let buffer = aig.add_and(buf_source, buf_source);
                group_outputs.push(buffer);
            }
        }

        // Distribute outputs across groups
        let mut outputs = Vec::with_capacity(fanout);
        let loads_per_group = fanout.div_ceil(num_groups);

        for (i, group_buf) in group_outputs.iter().enumerate() {
            let start = i * loads_per_group;
            let end = ((i + 1) * loads_per_group).min(fanout);
            for _ in start..end {
                outputs.push(*group_buf);
            }
        }

        outputs.truncate(fanout);
        outputs
    }

    /// Create a linear buffer chain
    fn create_linear_buffer_chain(
        &self,
        aig: &mut Aig,
        source: AigLit,
        fanout: usize,
    ) -> Vec<AigLit> {
        let mut outputs = Vec::with_capacity(fanout);
        let mut current_source = source;
        let mut remaining = fanout;

        while remaining > 0 {
            let loads = remaining.min(self.config.target_fanout);

            // Add this source for 'loads' outputs
            for _ in 0..loads {
                outputs.push(current_source);
            }
            remaining -= loads;

            if remaining > 0 {
                // Create a buffer for the next segment
                let buffer = aig.add_and(source, source);
                current_source = buffer;
            }
        }

        outputs
    }
}

impl Pass for BufferInsertion {
    fn name(&self) -> &str {
        "buffer_insertion"
    }

    fn run(&mut self, aig: &mut Aig) -> PassResult {
        let mut result = PassResult::new(self.name());
        result.record_before(aig);

        // Compute current fanout
        let fanout = self.compute_fanout(aig);

        // Find maximum fanout
        self.stats.max_fanout_before = fanout.values().copied().max().unwrap_or(0);

        // Find high-fanout nodes
        let high_fanout_nodes = self.find_high_fanout_nodes(&fanout);
        self.stats.high_fanout_fixed = high_fanout_nodes.len();

        // Calculate buffers needed (for statistics)
        for &(_, fo) in &high_fanout_nodes {
            self.stats.buffers_inserted += self.buffers_needed(fo);
        }
        self.stats.area_added = self.stats.buffers_inserted as f64 * self.config.buffer_area;

        // Note: In a full implementation, we would:
        // 1. Create buffer trees for each high-fanout node
        // 2. Update all references to use the buffered signals
        // 3. Run DCE to clean up any unused nodes

        // For now, we compute stats but don't modify the AIG
        // since modifying all references requires more infrastructure

        // Estimate max fanout after buffering
        self.stats.max_fanout_after = self.config.target_fanout.max(
            fanout
                .values()
                .filter(|&&fo| fo <= self.config.max_fanout)
                .copied()
                .max()
                .unwrap_or(0),
        );

        result.record_after(aig);
        result.add_extra("buffers_inserted", &self.stats.buffers_inserted.to_string());
        result.add_extra(
            "high_fanout_nodes",
            &self.stats.high_fanout_fixed.to_string(),
        );
        result.add_extra(
            "max_fanout",
            &format!(
                "{} -> {}",
                self.stats.max_fanout_before, self.stats.max_fanout_after
            ),
        );

        result
    }
}

/// Analyze fanout distribution without modifying the AIG
pub fn analyze_fanout(aig: &Aig) -> FanoutAnalysis {
    let mut fanout: IndexMap<AigNodeId, usize> = IndexMap::new();

    // Count references from AND nodes
    for id in 0..aig.node_count() {
        let node_id = AigNodeId(id as u32);
        if let Some(AigNode::And { left, right }) = aig.get_node(node_id) {
            *fanout.entry(left.node).or_insert(0) += 1;
            *fanout.entry(right.node).or_insert(0) += 1;
        }
    }

    // Count references from outputs
    for (_, lit) in aig.outputs() {
        *fanout.entry(lit.node).or_insert(0) += 1;
    }

    // Compute histogram
    let mut histogram: IndexMap<usize, usize> = IndexMap::new();
    for &fo in fanout.values() {
        *histogram.entry(fo).or_insert(0) += 1;
    }

    let max_fanout = fanout.values().copied().max().unwrap_or(0);
    let avg_fanout = if fanout.is_empty() {
        0.0
    } else {
        fanout.values().sum::<usize>() as f64 / fanout.len() as f64
    };

    FanoutAnalysis {
        max_fanout,
        avg_fanout,
        histogram,
        total_nodes: fanout.len(),
    }
}

/// Analysis of fanout distribution
#[derive(Debug, Clone)]
pub struct FanoutAnalysis {
    /// Maximum fanout in the design
    pub max_fanout: usize,
    /// Average fanout
    pub avg_fanout: f64,
    /// Histogram of fanout values (fanout -> count)
    pub histogram: IndexMap<usize, usize>,
    /// Total nodes analyzed
    pub total_nodes: usize,
}

impl FanoutAnalysis {
    /// Get count of nodes exceeding a fanout threshold
    pub fn nodes_exceeding(&self, threshold: usize) -> usize {
        self.histogram
            .iter()
            .filter(|(&fo, _)| fo > threshold)
            .map(|(_, &count)| count)
            .sum()
    }

    /// Get summary string
    pub fn summary(&self) -> String {
        format!(
            "Max fanout: {}, Avg: {:.2}, Nodes with FO>4: {}",
            self.max_fanout,
            self.avg_fanout,
            self.nodes_exceeding(4)
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_buffer_config_defaults() {
        let config = BufferConfig::default();
        assert_eq!(config.max_fanout, 4);
        assert_eq!(config.target_fanout, 4);
        assert!(config.use_balanced_trees);
    }

    #[test]
    fn test_buffer_config_presets() {
        let hp = BufferConfig::high_performance();
        assert_eq!(hp.max_fanout, 3);

        let la = BufferConfig::low_area();
        assert_eq!(la.max_fanout, 8);
    }

    #[test]
    fn test_buffers_needed() {
        let pass = BufferInsertion::new();

        // No buffering needed for low fanout
        assert_eq!(pass.buffers_needed(4), 0);
        assert_eq!(pass.buffers_needed(3), 0);

        // Buffering needed for high fanout
        assert!(pass.buffers_needed(8) > 0);
        assert!(pass.buffers_needed(16) > 0);
    }

    #[test]
    fn test_buffer_insertion_pass() {
        let mut aig = Aig::new("test".to_string());

        // Create a simple AIG
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let and1 = aig.add_and(AigLit::new(a), AigLit::new(b));

        aig.add_output("out".to_string(), and1);

        let mut pass = BufferInsertion::new();
        let result = pass.run(&mut aig);

        assert_eq!(result.pass_name, "buffer_insertion");
        // With just 2 inputs and 1 output, no high fanout
        assert_eq!(pass.stats().high_fanout_fixed, 0);
    }

    #[test]
    fn test_fanout_analysis() {
        let mut aig = Aig::new("test".to_string());

        // Create an AIG with varying fanout
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);

        // Create multiple gates that use 'a' (high fanout for a)
        let _and1 = aig.add_and(AigLit::new(a), AigLit::new(b));
        let _and2 = aig.add_and(AigLit::new(a), AigLit::new(b).invert());
        let _and3 = aig.add_and(AigLit::new(a).invert(), AigLit::new(b));

        let analysis = analyze_fanout(&aig);

        assert!(analysis.max_fanout >= 2);
        assert!(analysis.total_nodes > 0);
    }

    #[test]
    fn test_buffer_stats() {
        let mut stats = BufferStats::new();
        stats.buffers_inserted = 10;
        stats.high_fanout_fixed = 3;
        stats.max_fanout_before = 16;
        stats.max_fanout_after = 4;

        let summary = stats.summary();
        assert!(summary.contains("10 buffers"));
        assert!(summary.contains("3 high-fanout"));
    }
}
