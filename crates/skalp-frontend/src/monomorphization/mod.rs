//! Monomorphization - Transform generic entities into concrete implementations
//!
//! This module handles:
//! - Collecting generic instantiations
//! - Evaluating const parameters
//! - Generating specialized entity definitions
//! - Intent-driven code selection

pub mod collector;
pub mod engine;

pub use collector::{Instantiation, InstantiationCollector};
pub use engine::MonomorphizationEngine;
