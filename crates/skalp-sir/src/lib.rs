pub mod sir;
pub mod mir_to_sir;
pub mod metal_codegen;

pub use sir::{SirModule, SirNode, SirNodeKind, SirPort, SirSignal, CombinationalCone, StateElement, ClockDomain, ClockEdge, SignalRef};
pub use mir_to_sir::convert_mir_to_sir;
pub use metal_codegen::generate_metal_shader;