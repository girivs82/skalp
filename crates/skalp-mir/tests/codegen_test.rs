//! Tests for SystemVerilog code generation

use skalp_mir::codegen::SystemVerilogGenerator;
use skalp_mir::mir::*;

/// Helper to create a simple counter module
fn create_counter_mir() -> Mir {
    let mut mir = Mir::new("test_design".to_string());

    let mut module = Module::new(ModuleId(0), "counter".to_string());

    // Add ports
    module.ports.push(Port {
        id: PortId(0),
        name: "clk".to_string(),
        direction: PortDirection::Input,
        port_type: DataType::Clock { domain: None },
        physical_constraints: None,
    });
    module.ports.push(Port {
        id: PortId(1),
        name: "rst".to_string(),
        direction: PortDirection::Input,
        port_type: DataType::Reset {
            active_high: true,
            domain: None,
        },
        physical_constraints: None,
    });
    module.ports.push(Port {
        id: PortId(2),
        name: "count".to_string(),
        direction: PortDirection::Output,
        port_type: DataType::Logic(8),
        physical_constraints: None,
    });

    // Add signal for count register
    module.signals.push(Signal {
        id: SignalId(0),
        name: "count_reg".to_string(),
        signal_type: DataType::Logic(8),
        initial: Some(Value::Integer(0)),
        clock_domain: None,
    });

    // Create synchronous process
    let mut process_body = Block { statements: vec![] };

    // Reset condition
    let reset_check = IfStatement {
        condition: Expression::Ref(LValue::Port(PortId(1))),
        then_block: Block {
            statements: vec![Statement::Assignment(Assignment {
                lhs: LValue::Signal(SignalId(0)),
                rhs: Expression::Literal(Value::Integer(0)),
                kind: AssignmentKind::NonBlocking,
            })],
        },
        else_block: Some(Block {
            statements: vec![Statement::Assignment(Assignment {
                lhs: LValue::Signal(SignalId(0)),
                rhs: Expression::Binary {
                    op: BinaryOp::Add,
                    left: Box::new(Expression::Ref(LValue::Signal(SignalId(0)))),
                    right: Box::new(Expression::Literal(Value::Integer(1))),
                },
                kind: AssignmentKind::NonBlocking,
            })],
        }),
    };

    process_body.statements.push(Statement::If(reset_check));

    let process = Process {
        id: ProcessId(0),
        kind: ProcessKind::Sequential,
        sensitivity: SensitivityList::Edge(vec![EdgeSensitivity {
            signal: LValue::Port(PortId(0)),
            edge: EdgeType::Rising,
        }]),
        body: process_body,
    };

    module.processes.push(process);

    // Add continuous assignment for output
    module.assignments.push(ContinuousAssign {
        lhs: LValue::Port(PortId(2)),
        rhs: Expression::Ref(LValue::Signal(SignalId(0))),
    });

    mir.add_module(module);
    mir
}

#[test]
fn test_simple_module_generation() {
    let mir = create_counter_mir();
    let mut generator = SystemVerilogGenerator::new();
    let code = generator.generate(&mir);

    // Check for module declaration
    assert!(code.contains("module counter"));
    assert!(code.contains("endmodule"));

    // Check for ports
    assert!(code.contains("input wire clk"));
    assert!(code.contains("input wire rst"));
    assert!(code.contains("output logic [7:0] count"));

    // Check for signal declaration
    assert!(code.contains("logic [7:0] count_reg"));

    // Check for always block
    assert!(code.contains("always_ff @(posedge"));

    // Check for continuous assignment
    assert!(code.contains("assign"));
}

#[test]
fn test_combinational_logic() {
    let mut mir = Mir::new("comb_test".to_string());
    let mut module = Module::new(ModuleId(0), "adder".to_string());

    // Add ports
    module.ports.push(Port {
        id: PortId(0),
        name: "a".to_string(),
        direction: PortDirection::Input,
        port_type: DataType::Logic(8),
        physical_constraints: None,
    });
    module.ports.push(Port {
        id: PortId(1),
        name: "b".to_string(),
        direction: PortDirection::Input,
        port_type: DataType::Logic(8),
        physical_constraints: None,
    });
    module.ports.push(Port {
        id: PortId(2),
        name: "sum".to_string(),
        direction: PortDirection::Output,
        port_type: DataType::Logic(9),
        physical_constraints: None,
    });

    // Continuous assignment for sum
    module.assignments.push(ContinuousAssign {
        lhs: LValue::Port(PortId(2)),
        rhs: Expression::Binary {
            op: BinaryOp::Add,
            left: Box::new(Expression::Ref(LValue::Port(PortId(0)))),
            right: Box::new(Expression::Ref(LValue::Port(PortId(1)))),
        },
    });

    mir.add_module(module);

    let mut generator = SystemVerilogGenerator::new();
    let code = generator.generate(&mir);

    // Check module structure
    assert!(code.contains("module adder"));
    assert!(code.contains("input logic [7:0] a"));
    assert!(code.contains("input logic [7:0] b"));
    assert!(code.contains("output logic [8:0] sum"));

    // Check continuous assignment
    assert!(code.contains("assign"));
}

#[test]
fn test_case_statement() {
    let mut mir = Mir::new("case_test".to_string());
    let mut module = Module::new(ModuleId(0), "decoder".to_string());

    // Add ports
    module.ports.push(Port {
        id: PortId(0),
        name: "sel".to_string(),
        direction: PortDirection::Input,
        port_type: DataType::Logic(2),
        physical_constraints: None,
    });
    module.ports.push(Port {
        id: PortId(1),
        name: "out".to_string(),
        direction: PortDirection::Output,
        port_type: DataType::Logic(4),
        physical_constraints: None,
    });

    // Create combinational process with case
    let case_stmt = CaseStatement {
        expr: Expression::Ref(LValue::Port(PortId(0))),
        items: vec![
            CaseItem {
                values: vec![Expression::Literal(Value::Integer(0))],
                block: Block {
                    statements: vec![Statement::Assignment(Assignment {
                        lhs: LValue::Port(PortId(1)),
                        rhs: Expression::Literal(Value::BitVector {
                            width: 4,
                            value: 0b0001,
                        }),
                        kind: AssignmentKind::Blocking,
                    })],
                },
            },
            CaseItem {
                values: vec![Expression::Literal(Value::Integer(1))],
                block: Block {
                    statements: vec![Statement::Assignment(Assignment {
                        lhs: LValue::Port(PortId(1)),
                        rhs: Expression::Literal(Value::BitVector {
                            width: 4,
                            value: 0b0010,
                        }),
                        kind: AssignmentKind::Blocking,
                    })],
                },
            },
        ],
        default: Some(Block {
            statements: vec![Statement::Assignment(Assignment {
                lhs: LValue::Port(PortId(1)),
                rhs: Expression::Literal(Value::BitVector {
                    width: 4,
                    value: 0b0000,
                }),
                kind: AssignmentKind::Blocking,
            })],
        }),
    };

    let process = Process {
        id: ProcessId(0),
        kind: ProcessKind::Combinational,
        sensitivity: SensitivityList::Always,
        body: Block {
            statements: vec![Statement::Case(case_stmt)],
        },
    };

    module.processes.push(process);
    mir.add_module(module);

    let mut generator = SystemVerilogGenerator::new();
    let code = generator.generate(&mir);

    // Check for case statement
    assert!(code.contains("case"));
    assert!(code.contains("endcase"));
    assert!(code.contains("default:"));
    assert!(code.contains("always_comb"));
}
