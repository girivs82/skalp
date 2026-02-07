//! Code generation module for shared CPU/GPU simulation
//!
//! This module provides a unified codegen architecture that generates nearly identical
//! C++ code for both Metal (GPU) and native C++ (CPU) backends. The shared core
//! generates backend-agnostic expressions and struct layouts, while thin backend
//! wrappers add target-specific headers and function signatures.
//!
//! # Architecture
//!
//! ```text
//!                       SirModule
//!                           │
//!                           ▼
//!               ┌───────────────────────┐
//!               │   SharedCodegen       │
//!               │   (C++ expressions)   │
//!               └───────────────────────┘
//!                           │
//!             ┌─────────────┴─────────────┐
//!             │                           │
//!             ▼                           ▼
//!     ┌───────────────┐          ┌───────────────┐
//!     │ MetalBackend  │          │  CppBackend   │
//!     │ (thin wrapper)│          │ (thin wrapper)│
//!     └───────────────┘          └───────────────┘
//! ```
//!
//! # Usage
//!
//! ```ignore
//! use skalp_sir::codegen::{MetalBackend, CppBackend};
//!
//! // Generate Metal shader
//! let metal_source = MetalBackend::generate(&sir_module);
//!
//! // Generate C++ source (for compiled CPU simulation)
//! let cpp_source = CppBackend::generate(&sir_module);
//! ```

mod cpp;
mod metal;
mod shared;
mod types;

pub use cpp::CppBackend;
pub use metal::MetalBackend;
pub use shared::SharedCodegen;
pub use types::{BackendTarget, TypeInfo, TypeMapper};
