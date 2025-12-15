//! Tests for MIR optimization passes

use skalp_mir::mir::*;
use skalp_mir::optimize::*;

/// Helper to create a module with unused signals and variables
fn create_module_with_unused() -> Mir {
    let mut mir = Mir::new("test".to_string());
    let mut module = Module::new(ModuleId(0), "test_module".to_string());

    // Add ports
    module.ports.push(Port {
        id: PortId(0),
        name: "in".to_string(),
        direction: PortDirection::Input,
        port_type: DataType::Logic(8),
        physical_constraints: None,
        span: None,
    });
    module.ports.push(Port {
        id: PortId(1),
        name: "out".to_string(),
        direction: PortDirection::Output,
        port_type: DataType::Logic(8),
        physical_constraints: None,
        span: None,
    });

    // Add signals - some used, some unused
    module.signals.push(Signal {
        id: SignalId(0),
        name: "used_signal".to_string(),
        signal_type: DataType::Logic(8),
        clock_domain: None,
        initial: None,
        span: None,
        memory_config: None,
        trace_config: None,
        cdc_config: None,
        breakpoint_config: None,
        power_config: None,
    });
    module.signals.push(Signal {
        id: SignalId(1),
        name: "unused_signal".to_string(),
        signal_type: DataType::Logic(8),
        clock_domain: None,
        initial: None,
        span: None,
        memory_config: None,
        trace_config: None,
        cdc_config: None,
        breakpoint_config: None,
        power_config: None,
    });

    // Add variables - some used, some unused
    module.variables.push(Variable {
        id: VariableId(0),
        name: "used_var".to_string(),
        var_type: DataType::Logic(8),
        initial: None,
        span: None,
    });
    module.variables.push(Variable {
        id: VariableId(1),
        name: "unused_var".to_string(),
        var_type: DataType::Logic(8),
        initial: None,
        span: None,
    });

    // Create process that only uses some signals/variables
    let process = Process {
        id: ProcessId(0),
        kind: ProcessKind::Combinational,
        sensitivity: SensitivityList::Always,
        body: Block {
            statements: vec![
                Statement::Assignment(Assignment {
                    lhs: LValue::Signal(SignalId(0)),
                    rhs: Expression::with_unknown_type(ExpressionKind::Ref(LValue::Port(PortId(
                        0,
                    )))),
                    kind: AssignmentKind::Blocking,
                    span: None,
                }),
                Statement::Assignment(Assignment {
                    lhs: LValue::Variable(VariableId(0)),
                    rhs: Expression::with_unknown_type(ExpressionKind::Ref(LValue::Signal(
                        SignalId(0),
                    ))),
                    kind: AssignmentKind::Blocking,
                    span: None,
                }),
            ],
        },
        span: None,
    };
    module.processes.push(process);

    // Continuous assignment using the used variable
    module.assignments.push(ContinuousAssign {
        lhs: LValue::Port(PortId(1)),
        rhs: Expression::with_unknown_type(ExpressionKind::Ref(LValue::Variable(VariableId(0)))),
        span: None,
    });

    mir.add_module(module);
    mir
}

/// Helper to create a module with constant expressions
fn create_module_with_constants() -> Mir {
    let mut mir = Mir::new("test".to_string());
    let mut module = Module::new(ModuleId(0), "const_test".to_string());

    // Add port
    module.ports.push(Port {
        id: PortId(0),
        name: "out".to_string(),
        direction: PortDirection::Output,
        port_type: DataType::Logic(8),
        physical_constraints: None,
        span: None,
    });

    // Add signal
    module.signals.push(Signal {
        id: SignalId(0),
        name: "result".to_string(),
        signal_type: DataType::Logic(8),
        clock_domain: None,
        initial: None,
        span: None,
        memory_config: None,
        trace_config: None,
        cdc_config: None,
        breakpoint_config: None,
        power_config: None,
    });

    // Create process with constant expressions
    let process = Process {
        id: ProcessId(0),
        kind: ProcessKind::Combinational,
        sensitivity: SensitivityList::Always,
        body: Block {
            statements: vec![
                // 2 + 3 = 5
                Statement::Assignment(Assignment {
                    lhs: LValue::Signal(SignalId(0)),
                    rhs: Expression::with_unknown_type(ExpressionKind::Binary {
                        op: BinaryOp::Add,
                        left: Box::new(Expression::with_unknown_type(ExpressionKind::Literal(
                            Value::Integer(2),
                        ))),
                        right: Box::new(Expression::with_unknown_type(ExpressionKind::Literal(
                            Value::Integer(3),
                        ))),
                    }),
                    kind: AssignmentKind::Blocking,
                    span: None,
                }),
                // Conditional with constant condition
                Statement::If(IfStatement {
                    condition: Expression::with_unknown_type(ExpressionKind::Literal(
                        Value::Integer(1),
                    )), // Always true
                    then_block: Block {
                        statements: vec![Statement::Assignment(Assignment {
                            lhs: LValue::Port(PortId(0)),
                            rhs: Expression::with_unknown_type(ExpressionKind::Literal(
                                Value::Integer(42),
                            )),
                            kind: AssignmentKind::Blocking,
                            span: None,
                        })],
                    },
                    else_block: Some(Block {
                        statements: vec![Statement::Assignment(Assignment {
                            lhs: LValue::Port(PortId(0)),
                            rhs: Expression::with_unknown_type(ExpressionKind::Literal(
                                Value::Integer(0),
                            )),
                            kind: AssignmentKind::Blocking,
                            span: None,
                        })],
                    }),
                    span: None,
                }),
            ],
        },
        span: None,
    };
    module.processes.push(process);

    mir.add_module(module);
    mir
}

#[test]
fn test_dead_code_elimination() {
    let mut mir = create_module_with_unused();

    // Before optimization, should have 2 signals and 2 variables
    assert_eq!(mir.modules[0].signals.len(), 2);
    assert_eq!(mir.modules[0].variables.len(), 2);

    // Apply dead code elimination
    let mut dce = DeadCodeElimination::new();
    dce.apply(&mut mir);

    // After optimization, should only have used signals and variables
    assert_eq!(mir.modules[0].signals.len(), 1);
    assert_eq!(mir.modules[0].variables.len(), 1);

    // Check that the remaining ones are the used ones
    assert_eq!(mir.modules[0].signals[0].id, SignalId(0));
    assert_eq!(mir.modules[0].variables[0].id, VariableId(0));
}

#[test]
fn test_constant_folding() {
    let mut mir = create_module_with_constants();

    // Apply constant folding
    let mut cf = ConstantFolding::new();
    cf.apply(&mut mir);

    // Check that the binary expression was folded
    if let Statement::Assignment(assign) = &mir.modules[0].processes[0].body.statements[0] {
        match &assign.rhs.kind {
            ExpressionKind::Literal(Value::Integer(n)) => {
                assert_eq!(*n, 5); // 2 + 3 = 5
            }
            _ => panic!("Expected folded constant"),
        }
    } else {
        panic!("Expected assignment statement");
    }
}

#[test]
fn test_constant_folding_arithmetic() {
    let mut mir = Mir::new("test".to_string());
    let mut module = Module::new(ModuleId(0), "arith_test".to_string());

    module.signals.push(Signal {
        id: SignalId(0),
        name: "s".to_string(),
        signal_type: DataType::Logic(8),
        clock_domain: None,
        initial: None,
        span: None,
        memory_config: None,
        trace_config: None,
        cdc_config: None,
        breakpoint_config: None,
        power_config: None,
    });

    // Test various arithmetic operations
    let statements = vec![
        // Test multiplication: 3 * 4 = 12
        Statement::Assignment(Assignment {
            lhs: LValue::Signal(SignalId(0)),
            rhs: Expression::with_unknown_type(ExpressionKind::Binary {
                op: BinaryOp::Mul,
                left: Box::new(Expression::with_unknown_type(ExpressionKind::Literal(
                    Value::Integer(3),
                ))),
                right: Box::new(Expression::with_unknown_type(ExpressionKind::Literal(
                    Value::Integer(4),
                ))),
            }),
            kind: AssignmentKind::Blocking,
            span: None,
        }),
        // Test division: 20 / 4 = 5
        Statement::Assignment(Assignment {
            lhs: LValue::Signal(SignalId(0)),
            rhs: Expression::with_unknown_type(ExpressionKind::Binary {
                op: BinaryOp::Div,
                left: Box::new(Expression::with_unknown_type(ExpressionKind::Literal(
                    Value::Integer(20),
                ))),
                right: Box::new(Expression::with_unknown_type(ExpressionKind::Literal(
                    Value::Integer(4),
                ))),
            }),
            kind: AssignmentKind::Blocking,
            span: None,
        }),
        // Test subtraction: 10 - 3 = 7
        Statement::Assignment(Assignment {
            lhs: LValue::Signal(SignalId(0)),
            rhs: Expression::with_unknown_type(ExpressionKind::Binary {
                op: BinaryOp::Sub,
                left: Box::new(Expression::with_unknown_type(ExpressionKind::Literal(
                    Value::Integer(10),
                ))),
                right: Box::new(Expression::with_unknown_type(ExpressionKind::Literal(
                    Value::Integer(3),
                ))),
            }),
            kind: AssignmentKind::Blocking,
            span: None,
        }),
    ];

    let process = Process {
        id: ProcessId(0),
        kind: ProcessKind::Combinational,
        sensitivity: SensitivityList::Always,
        body: Block { statements },
        span: None,
    };
    module.processes.push(process);
    mir.add_module(module);

    // Apply constant folding
    let mut cf = ConstantFolding::new();
    cf.apply(&mut mir);

    // Check folded values
    let statements = &mir.modules[0].processes[0].body.statements;

    // Check multiplication
    if let Statement::Assignment(assign) = &statements[0] {
        if let ExpressionKind::Literal(Value::Integer(n)) = &assign.rhs.kind {
            assert_eq!(*n, 12);
        } else {
            panic!("Expected folded multiplication");
        }
    }

    // Check division
    if let Statement::Assignment(assign) = &statements[1] {
        if let ExpressionKind::Literal(Value::Integer(n)) = &assign.rhs.kind {
            assert_eq!(*n, 5);
        } else {
            panic!("Expected folded division");
        }
    }

    // Check subtraction
    if let Statement::Assignment(assign) = &statements[2] {
        if let ExpressionKind::Literal(Value::Integer(n)) = &assign.rhs.kind {
            assert_eq!(*n, 7);
        } else {
            panic!("Expected folded subtraction");
        }
    }
}

#[test]
fn test_constant_folding_conditional() {
    let mut mir = Mir::new("test".to_string());
    let mut module = Module::new(ModuleId(0), "cond_test".to_string());

    module.signals.push(Signal {
        id: SignalId(0),
        name: "s".to_string(),
        signal_type: DataType::Logic(8),
        clock_domain: None,
        initial: None,
        span: None,
        memory_config: None,
        trace_config: None,
        cdc_config: None,
        breakpoint_config: None,
        power_config: None,
    });

    // Test conditional expression with constant condition
    let cond_expr = Expression::with_unknown_type(ExpressionKind::Conditional {
        cond: Box::new(Expression::with_unknown_type(ExpressionKind::Literal(
            Value::Integer(1),
        ))), // true
        then_expr: Box::new(Expression::with_unknown_type(ExpressionKind::Literal(
            Value::Integer(42),
        ))),
        else_expr: Box::new(Expression::with_unknown_type(ExpressionKind::Literal(
            Value::Integer(0),
        ))),
    });

    let process = Process {
        id: ProcessId(0),
        kind: ProcessKind::Combinational,
        sensitivity: SensitivityList::Always,
        body: Block {
            statements: vec![Statement::Assignment(Assignment {
                lhs: LValue::Signal(SignalId(0)),
                rhs: cond_expr,
                kind: AssignmentKind::Blocking,
                span: None,
            })],
        },
        span: None,
    };
    module.processes.push(process);
    mir.add_module(module);

    // Apply constant folding
    let mut cf = ConstantFolding::new();
    cf.apply(&mut mir);

    // Check that conditional was folded to the then branch
    if let Statement::Assignment(assign) = &mir.modules[0].processes[0].body.statements[0] {
        if let ExpressionKind::Literal(Value::Integer(n)) = &assign.rhs.kind {
            assert_eq!(*n, 42); // Should select the then branch
        } else {
            panic!("Expected folded conditional");
        }
    }
}
