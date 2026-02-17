pub mod codegen;
pub mod mir_to_sir;
pub mod pipeline;
pub mod sir;

pub use mir_to_sir::{convert_mir_to_sir, convert_mir_to_sir_with_hierarchy};
pub use pipeline::{insert_pipeline_registers, PipelineResult};
pub use sir::{
    BinaryOperation, ClockDomain, ClockEdge, CombinationalCone, SignalRef, SirModule, SirNode,
    SirNodeKind, SirPort, SirSignal, SirType, StateElement, UnaryOperation,
};

// Export codegen module
pub use codegen::{BackendTarget, CppBackend, MetalBackend, SharedCodegen, TypeInfo, TypeMapper};
