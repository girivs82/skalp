//! SKALP Simulation Engine
//!
//! GPU-accelerated simulation using Metal on macOS.
//! Single unified simulation path for simplicity.

pub mod sir;
pub mod mir_to_sir;
pub mod simulator;
pub mod event;
pub mod state;
pub mod cone;
pub mod shader;
pub mod runtime;
pub mod testbench;

#[cfg(target_os = "macos")]
pub mod metal;

pub use sir::{Sir, SirModule};
pub use mir_to_sir::MirToSir;
pub use simulator::Simulator;

#[cfg(test)]
mod tests {
    use super::*;
    use skalp_mir::*;

    #[test]
    fn test_sir_generation_simple() {
        // Create a simple MIR with a counter module
        let mut mir = Mir::new("test_counter".to_string());

        // Create a simple module with basic structure
        let module = Module {
            id: ModuleId(0),
            name: "counter_module".to_string(),
            parameters: Vec::new(),
            ports: Vec::new(),
            signals: vec![
                Signal {
                    id: SignalId(0),
                    name: "clk".to_string(),
                    signal_type: DataType::Bit(1),
                    initial: None,
                },
                Signal {
                    id: SignalId(1),
                    name: "rst".to_string(),
                    signal_type: DataType::Bit(1),
                    initial: None,
                },
                Signal {
                    id: SignalId(2),
                    name: "counter".to_string(),
                    signal_type: DataType::Bit(8),
                    initial: Some(Value::Integer(0)),
                },
            ],
            variables: Vec::new(),
            processes: vec![
                Process {
                    id: ProcessId(0),
                    kind: ProcessKind::Sequential,
                    sensitivity: SensitivityList::Edge(vec![EdgeSensitivity {
                        signal: LValue::Signal(SignalId(0)), // clk
                        edge: EdgeType::Rising,
                    }]),
                    body: Block {
                        statements: vec![
                            Statement::Assignment(Assignment {
                                lhs: LValue::Signal(SignalId(2)), // counter
                                rhs: Expression::Binary {
                                    op: BinaryOp::Add,
                                    left: Box::new(Expression::Ref(LValue::Signal(SignalId(2)))),
                                    right: Box::new(Expression::Literal(Value::Integer(1))),
                                },
                                kind: AssignmentKind::NonBlocking,
                            })
                        ]
                    }
                }
            ],
            assignments: Vec::new(),
            instances: Vec::new(),
            clock_domains: Vec::new(),
        };

        mir.add_module(module);

        // Transform to SIR
        let mut transformer = MirToSir::new();
        let sir = transformer.transform(&mir);

        // Validate SIR structure
        assert_eq!(sir.name, "test_counter");
        assert!(sir.top_module.signals.len() >= 3); // clk, rst, counter

        println!("SIR generation test passed - created {} signals, {} sequential blocks, {} combinational blocks",
                sir.top_module.signals.len(),
                sir.top_module.seq_blocks.len(),
                sir.top_module.comb_blocks.len());
    }

    #[test]
    fn test_sir_basic_structure() {
        // Simple test just to verify SIR creation works
        let mir = Mir::new("test_design".to_string());
        let mut transformer = MirToSir::new();
        let sir = transformer.transform(&mir);

        assert_eq!(sir.name, "test_design");
        assert_eq!(sir.top_module.name, "test_design");

        println!("Basic SIR structure test passed");
    }
}