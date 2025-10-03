#![allow(dead_code, unused_variables, unused_imports)]
//! SKALP Place & Route for Open FPGAs
//!
//! This crate handles:
//! - Placement algorithms
//! - Routing algorithms
//! - Bitstream generation
//! - Device programming

pub mod bitstream;
pub mod device;
pub mod placer;
pub mod router;
pub mod timing;

pub use bitstream::BitstreamGenerator;
pub use placer::Placer;
pub use router::Router;
