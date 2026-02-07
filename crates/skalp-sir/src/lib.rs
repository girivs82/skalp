// GLOBAL: Disable all debug output for performance
#[cfg(target_os = "macos")]
macro_rules! eprintln {
    ($($arg:tt)*) => {{}};
}

pub mod codegen;
pub mod metal_codegen;
pub mod mir_to_sir;
pub mod pipeline;
pub mod sir;

#[cfg(target_os = "macos")]
pub use metal_codegen::generate_metal_shader;
pub use mir_to_sir::{convert_mir_to_sir, convert_mir_to_sir_with_hierarchy};
pub use pipeline::{insert_pipeline_registers, PipelineResult};
pub use sir::{
    BinaryOperation, ClockDomain, ClockEdge, CombinationalCone, SignalRef, SirModule, SirNode,
    SirNodeKind, SirPort, SirSignal, SirType, StateElement, UnaryOperation,
};

// Export new codegen module
pub use codegen::{BackendTarget, CppBackend, MetalBackend, SharedCodegen, TypeInfo, TypeMapper};
