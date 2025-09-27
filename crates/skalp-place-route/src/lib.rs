//! SKALP Place & Route for Open FPGAs
//!
//! This crate handles:
//! - Placement algorithms
//! - Routing algorithms
//! - Bitstream generation
//! - Device programming

pub mod placer;
pub mod router;
pub mod bitstream;
pub mod device;

pub use placer::Placer;
pub use router::Router;
pub use bitstream::BitstreamGenerator;