//! Error types for Place and Route operations

use thiserror::Error;

/// Errors that can occur during place and route
#[derive(Debug, Error)]
pub enum PlaceRouteError {
    /// Error parsing chip database
    #[error("ChipDB parse error: {0}")]
    ChipDbParse(String),

    /// Error loading chip database
    #[error("ChipDB load error: {0}")]
    ChipDbLoad(String),

    /// Device not supported
    #[error("Unsupported device: {0}")]
    UnsupportedDevice(String),

    /// Placement failed
    #[error("Placement failed: {0}")]
    PlacementFailed(String),

    /// No valid BEL found for cell
    #[error("No valid BEL for cell {cell_type} at path {path}")]
    NoBelForCell { cell_type: String, path: String },

    /// Design exceeds device capacity
    #[error("Design exceeds device capacity: {required} {resource} needed, {available} available")]
    CapacityExceeded {
        resource: String,
        required: usize,
        available: usize,
    },

    /// Routing failed
    #[error("Routing failed: {0}")]
    RoutingFailed(String),

    /// Clock nets whose clock pins take their clock from the dedicated
    /// network only, every entry to it that leads there being held by the
    /// clocks routed before them
    #[error(
        "{} clock net(s) cannot reach their clock pins: {}. Those pins take their clock \
         from the dedicated clock network only, and every entry to it that leads there \
         already carries one of the {routed} clocks routed before them. Use fewer clocks \
         (clock enables on a shared clock, say), or give the clock a source on the \
         network (a clock pin or a PLL output)",
        nets.len(),
        nets.join(", ")
    )]
    ClockEntriesExhausted {
        /// The stranded clock nets, by name
        nets: Vec<String>,
        /// How many clocks were routed onto the network
        routed: usize,
    },

    /// No route found for net
    #[error("No route found for net {net_name}")]
    NoRouteFound { net_name: String },

    /// Congestion too high after routing
    #[error("Routing congestion exceeded: {congestion:.2} > {threshold:.2}")]
    CongestionExceeded { congestion: f64, threshold: f64 },

    /// Bitstream generation failed
    #[error("Bitstream generation failed: {0}")]
    BitstreamFailed(String),

    /// Device programming failed; the message is complete as it stands
    #[error("{0}")]
    ProgrammingFailed(String),

    /// Timing constraint not met
    #[error("Timing constraint not met: {0}")]
    TimingFailed(String),

    /// Invalid constraint
    #[error("Invalid constraint: {0}")]
    InvalidConstraint(String),

    /// I/O pin constraint error
    #[error("I/O constraint error for {signal}: {message}")]
    IoConstraintError { signal: String, message: String },

    /// Internal error
    #[error("Internal error: {0}")]
    Internal(String),

    /// IO error
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

/// Result type for place and route operations
pub type Result<T> = std::result::Result<T, PlaceRouteError>;
