#[cfg(target_os = "macos")]
pub mod metal_codegen;
pub mod mir_to_sir;
pub mod sir;

#[cfg(target_os = "macos")]
pub use metal_codegen::generate_metal_shader;
pub use mir_to_sir::{convert_mir_to_sir, convert_mir_to_sir_with_hierarchy};
pub use sir::{
    BinaryOperation, ClockDomain, ClockEdge, CombinationalCone, SignalRef, SirModule, SirNode,
    SirNodeKind, SirPort, SirSignal, SirType, StateElement, UnaryOperation,
};
