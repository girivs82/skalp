//! Tests for #[breakpoint] attribute feature
//!
//! Tests the #[breakpoint] attribute for debug assertions and their propagation
//! through HIR -> MIR -> SystemVerilog codegen.

use skalp_codegen::generate_systemverilog_from_mir;
use skalp_frontend::parse_and_build_hir;
use skalp_lir::lower_to_lir;
use skalp_mir::{MirCompiler, OptimizationLevel};

#[test]
fn test_breakpoint_attribute_basic() {
    println!("=== Testing Basic #[breakpoint] Attribute ===");

    let source = r#"
entity BreakpointTest {
    in clk: bit,
    in data_in: bit[8],
    out data_out: bit[8],

    // Signal with breakpoint attribute
    #[breakpoint]
    signal error_flag: bit,
}

impl BreakpointTest {
    data_out = data_in;
    error_flag = data_in[7];
}
"#;

    // Parse and build HIR
    let hir = parse_and_build_hir(source).expect("HIR building should succeed");

    // Find the entity
    let entity = hir.entities.iter().find(|e| e.name == "BreakpointTest");
    assert!(entity.is_some(), "Should have BreakpointTest entity");
    let entity = entity.unwrap();

    // Find the error_flag signal
    let signal = entity.signals.iter().find(|s| s.name == "error_flag");
    assert!(signal.is_some(), "Should have 'error_flag' signal");
    let signal = signal.unwrap();

    // Check that breakpoint_config is present
    assert!(
        signal.breakpoint_config.is_some(),
        "error_flag signal should have breakpoint_config from #[breakpoint] attribute"
    );

    let bp_config = signal.breakpoint_config.as_ref().unwrap();
    // Basic breakpoint should have no condition, name, or message
    assert!(
        bp_config.condition.is_none(),
        "Basic breakpoint should have no condition"
    );
    assert!(
        bp_config.name.is_none(),
        "Basic breakpoint should have no name"
    );
    assert!(
        !bp_config.is_error,
        "Basic breakpoint should not be an error type"
    );

    println!("✓ Basic breakpoint attribute parsed correctly");
}

#[test]
fn test_breakpoint_with_condition() {
    println!("=== Testing #[breakpoint] with Condition ===");

    let source = r#"
entity ConditionTest {
    in clk: bit,
    in counter: bit[8],
    out overflow: bit,

    #[breakpoint(condition = "counter > 100")]
    signal watch_counter: bit[8],
}

impl ConditionTest {
    watch_counter = counter;
    overflow = counter[7];
}
"#;

    let hir = parse_and_build_hir(source).expect("HIR building should succeed");
    let entity = hir
        .entities
        .iter()
        .find(|e| e.name == "ConditionTest")
        .unwrap();
    let signal = entity
        .signals
        .iter()
        .find(|s| s.name == "watch_counter")
        .unwrap();

    assert!(
        signal.breakpoint_config.is_some(),
        "Should have breakpoint_config"
    );
    let bp_config = signal.breakpoint_config.as_ref().unwrap();

    assert_eq!(
        bp_config.condition.as_deref(),
        Some("counter > 100"),
        "Breakpoint should have condition"
    );

    println!("✓ Conditional breakpoint parsed correctly");
}

#[test]
fn test_breakpoint_with_name_and_message() {
    println!("=== Testing #[breakpoint] with Name and Message ===");

    let source = r#"
entity NamedBreakpoint {
    in clk: bit,
    in state: bit[4],
    out valid: bit,

    #[breakpoint(name = "FSM_ERROR", message = "Invalid FSM state")]
    signal fsm_state: bit[4],
}

impl NamedBreakpoint {
    fsm_state = state;
    valid = 1;
}
"#;

    let hir = parse_and_build_hir(source).expect("HIR building should succeed");
    let entity = hir
        .entities
        .iter()
        .find(|e| e.name == "NamedBreakpoint")
        .unwrap();
    let signal = entity
        .signals
        .iter()
        .find(|s| s.name == "fsm_state")
        .unwrap();

    assert!(
        signal.breakpoint_config.is_some(),
        "Should have breakpoint_config"
    );
    let bp_config = signal.breakpoint_config.as_ref().unwrap();

    assert_eq!(
        bp_config.name.as_deref(),
        Some("FSM_ERROR"),
        "Breakpoint should have name"
    );
    assert_eq!(
        bp_config.message.as_deref(),
        Some("Invalid FSM state"),
        "Breakpoint should have message"
    );

    println!("✓ Named breakpoint with message parsed correctly");
}

#[test]
fn test_breakpoint_error_type() {
    println!("=== Testing #[breakpoint] with Error Type ===");

    let source = r#"
entity ErrorBreakpoint {
    in clk: bit,
    in async_reset: bit,
    out valid: bit,

    #[breakpoint(error = true, message = "Unexpected reset")]
    signal critical_error: bit,
}

impl ErrorBreakpoint {
    critical_error = async_reset;
    valid = !async_reset;
}
"#;

    let hir = parse_and_build_hir(source).expect("HIR building should succeed");
    let entity = hir
        .entities
        .iter()
        .find(|e| e.name == "ErrorBreakpoint")
        .unwrap();
    let signal = entity
        .signals
        .iter()
        .find(|s| s.name == "critical_error")
        .unwrap();

    assert!(
        signal.breakpoint_config.is_some(),
        "Should have breakpoint_config"
    );
    let bp_config = signal.breakpoint_config.as_ref().unwrap();

    assert!(
        bp_config.is_error,
        "Breakpoint should be marked as error type"
    );
    assert_eq!(
        bp_config.message.as_deref(),
        Some("Unexpected reset"),
        "Breakpoint should have error message"
    );

    println!("✓ Error breakpoint parsed correctly");
}

#[test]
fn test_breakpoint_mir_propagation() {
    println!("=== Testing Breakpoint MIR Propagation ===");

    let source = r#"
entity MirTest {
    in clk: bit,
    in data: bit[8],
    out out_data: bit[8],

    #[breakpoint(condition = "data == 0xFF")]
    signal max_value: bit,
}

impl MirTest {
    out_data = data;
    max_value = data == 0xFF;
}
"#;

    let hir = parse_and_build_hir(source).expect("HIR building should succeed");
    let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
    let mir = compiler
        .compile_to_mir(&hir)
        .expect("MIR compilation should succeed");

    // Find the module and signal
    let module = mir.modules.iter().find(|m| m.name == "MirTest").unwrap();
    let signal = module.signals.iter().find(|s| s.name == "max_value");
    assert!(signal.is_some(), "Should have 'max_value' signal in MIR");
    let signal = signal.unwrap();

    assert!(
        signal.breakpoint_config.is_some(),
        "Breakpoint config should propagate to MIR"
    );
    let bp_config = signal.breakpoint_config.as_ref().unwrap();
    assert_eq!(
        bp_config.condition.as_deref(),
        Some("data == 0xFF"),
        "Condition should propagate to MIR"
    );

    println!("✓ Breakpoint config propagated to MIR correctly");
}

#[test]
fn test_breakpoint_codegen() {
    println!("=== Testing Breakpoint SystemVerilog Codegen ===");

    let source = r#"
entity CodegenTest {
    in clk: bit,
    in data: bit[8],
    out valid: bit,

    #[breakpoint(name = "DATA_CHECK", condition = "data > 128")]
    signal data_high: bit,
}

impl CodegenTest {
    valid = 1;
    data_high = data > 128;
}
"#;

    let hir = parse_and_build_hir(source).expect("HIR building should succeed");
    let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
    let mir = compiler
        .compile_to_mir(&hir)
        .expect("MIR compilation should succeed");
    let lir = lower_to_lir(&mir).expect("LIR lowering should succeed");
    let sv = generate_systemverilog_from_mir(&mir, &lir).expect("SV codegen should succeed");

    println!("Generated SystemVerilog:\n{}", sv);

    // Check for breakpoint assertion in output
    assert!(
        sv.contains("// Debug Breakpoint Assertions") || sv.contains("// Breakpoint:"),
        "Should have breakpoint comment in output"
    );
    assert!(
        sv.contains("BREAKPOINT") || sv.contains("$stop"),
        "Should have breakpoint action in output"
    );
    assert!(
        sv.contains("DATA_CHECK") || sv.contains("data_high"),
        "Should reference breakpoint name or signal"
    );

    println!("✓ Breakpoint SystemVerilog codegen works correctly");
}

#[test]
fn test_breakpoint_simple_codegen() {
    println!("=== Testing Simple Breakpoint SystemVerilog Codegen ===");

    let source = r#"
entity SimpleBreakpoint {
    in clk: bit,
    in error: bit,
    out ok: bit,

    #[breakpoint]
    signal error_signal: bit,
}

impl SimpleBreakpoint {
    ok = !error;
    error_signal = error;
}
"#;

    let hir = parse_and_build_hir(source).expect("HIR building should succeed");
    let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
    let mir = compiler
        .compile_to_mir(&hir)
        .expect("MIR compilation should succeed");
    let lir = lower_to_lir(&mir).expect("LIR lowering should succeed");
    let sv = generate_systemverilog_from_mir(&mir, &lir).expect("SV codegen should succeed");

    println!("Generated SystemVerilog:\n{}", sv);

    // Simple breakpoint should use signal name as condition
    assert!(
        sv.contains("error_signal") && sv.contains("$stop"),
        "Should have error_signal breakpoint with $stop"
    );

    println!("✓ Simple breakpoint codegen works correctly");
}
