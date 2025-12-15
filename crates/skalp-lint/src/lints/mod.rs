//! Individual lint implementations
//!
//! This module contains all the built-in lints organized by category:
//!
//! - `unused`: Unused variables, parameters, and functions
//! - `types`: Type-related lints (width mismatches, sign confusion)
//! - `hardware`: Hardware design best practices

pub mod hardware;
pub mod types;
pub mod unused;
