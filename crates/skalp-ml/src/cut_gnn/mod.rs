//! Graph Neural Network for Cut Selection
//!
//! This module provides GNN-based cut selection during technology mapping.
//! The model learns to score cuts based on their graph structure and
//! downstream effects on area and delay.
//!
//! # Architecture
//!
//! ```text
//! AIG Graph → GNN Encoder → Cut Scorer → Best Cut
//! ```
//!
//! # Features
//!
//! - Message-passing GNN (3-4 layers)
//! - Node features: type, fanout, level
//! - Edge features: inverted or not
//! - Cut scoring based on local and global context

mod graph;
mod model;

pub use graph::{AigGraph, NodeFeatures};
pub use model::{CutScorer, CutScorerConfig, GnnCutSelector};
