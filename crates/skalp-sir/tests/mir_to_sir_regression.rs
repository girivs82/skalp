//! Comprehensive MIR to SIR Regression Tests
//!
//! Tests the MIR → SIR transformation pipeline to ensure:
//! - All MIR constructs are correctly translated to SIR (Simulation IR)
//! - Combinational and sequential logic properly represented
//! - State elements correctly identified
//! - Clock domains extracted
//! - Simulation-ready structure generated
//!
//! Coverage:
//! - Port conversion (input, output, inout)
//! - Signal conversion (combinational, sequential/state)
//! - Logic conversion (combinational, sequential)
//! - Clock domain extraction
//! - State element identification
//! - Complex patterns (ALU, FSM, counter)

use skalp_frontend::hir_builder::build_hir;
use skalp_frontend::parse::parse;
use skalp_mir::hir_to_mir::HirToMir;
use skalp_sir::mir_to_sir::convert_mir_to_sir;
use skalp_sir::sir::*;

// ============================================================================
// Helper Functions
// ============================================================================

/// Compile SKALP source to SIR
fn compile_to_sir(source: &str) -> SirModule {
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");
    let mut transformer = HirToMir::new();
    let mir = transformer.transform(&hir);

    assert!(
        !mir.modules.is_empty(),
        "MIR should have at least one module"
    );
    convert_mir_to_sir(&mir.modules[0])
}

// ============================================================================
// Port Conversion Tests
// ============================================================================

#[test]
fn test_port_conversion_simple() {
    let source = r#"
entity Wire {
    in a: bit[8]
    out b: bit[8]
}

impl Wire {
    b = a
}
"#;
    let sir = compile_to_sir(source);

    assert_eq!(sir.name, "Wire");
    assert_eq!(sir.inputs.len(), 1);
    assert_eq!(sir.outputs.len(), 1);
    assert_eq!(sir.inputs[0].name, "a");
    assert_eq!(sir.outputs[0].name, "b");
}

#[test]
fn test_port_directions() {
    let source = r#"
entity Test {
    in a: bit[8]
    out b: bit[8]
    inout c: bit[8]
}

impl Test {
    b = a
}
"#;
    let sir = compile_to_sir(source);

    assert_eq!(sir.inputs.len(), 1);
    assert!(!sir.outputs.is_empty()); // b + possibly c
    assert!(matches!(sir.inputs[0].direction, PortDirection::Input));
}

#[test]
fn test_port_widths() {
    let source = r#"
entity Test {
    in a: bit[1]
    in b: bit[8]
    in c: bit[32]
    out result: bit[32]
}

impl Test {
    result = c
}
"#;
    let sir = compile_to_sir(source);

    assert_eq!(sir.inputs[0].width, 1);
    assert_eq!(sir.inputs[1].width, 8);
    assert_eq!(sir.inputs[2].width, 32);
}

// ============================================================================
// Signal Conversion Tests
// ============================================================================

#[test]
fn test_combinational_signal() {
    let source = r#"
entity Test {
    in a: bit[8]
    out b: bit[8]
}

impl Test {
    signal temp: bit[8]

    temp = a + 1
    b = temp
}
"#;
    let sir = compile_to_sir(source);

    // Should have signals for ports and temp
    assert!(sir.signals.len() >= 3);

    // temp should not be a state element (combinational)
    let temp_signal = sir.signals.iter().find(|s| s.name == "temp");
    assert!(temp_signal.is_some());
}

#[test]
fn test_sequential_signal() {
    let source = r#"
entity Register {
    in clk: clock
    in d: bit[8]
    out q: bit[8]
}

impl Register {
    signal q_reg: bit[8] = 0

    on(clk.rise) {
        q_reg <= d
    }

    q = q_reg
}
"#;
    let sir = compile_to_sir(source);

    // q_reg should be a state element
    assert!(sir.state_elements.contains_key("q_reg"));

    let q_reg_signal = sir.signals.iter().find(|s| s.name == "q_reg");
    assert!(q_reg_signal.is_some());
    assert!(q_reg_signal.unwrap().is_state);
}

#[test]
fn test_multiple_signals() {
    let source = r#"
entity Test {
    in a: bit[8]
    out result: bit[8]
}

impl Test {
    signal s1: bit[8]
    signal s2: bit[8]
    signal s3: bit[8]

    s1 = a
    s2 = s1 + 1
    s3 = s2 + 1
    result = s3
}
"#;
    let sir = compile_to_sir(source);

    // Should have signals for all declared signals plus ports
    assert!(sir.signals.len() >= 5);
}

// ============================================================================
// State Element Identification Tests
// ============================================================================

#[test]
fn test_state_element_in_register() {
    let source = r#"
entity DFF {
    in clk: clock
    in d: bit
    out q: bit
}

impl DFF {
    signal q_reg: bit = 0

    on(clk.rise) {
        q_reg <= d
    }

    q = q_reg
}
"#;
    let sir = compile_to_sir(source);

    assert!(!sir.state_elements.is_empty());
    assert!(sir.state_elements.contains_key("q_reg"));

    let state = &sir.state_elements["q_reg"];
    assert_eq!(state.name, "q_reg");
    assert_eq!(state.width, 1);
}

#[test]
fn test_state_element_in_counter() {
    let source = r#"
entity Counter {
    in clk: clock
    in rst: reset
    out count: bit[8]
}

impl Counter {
    signal count_reg: bit[8] = 0

    on(clk.rise) {
        count_reg <= if rst { 0 } else { count_reg + 1 }
    }

    count = count_reg
}
"#;
    let sir = compile_to_sir(source);

    assert!(sir.state_elements.contains_key("count_reg"));

    let state = &sir.state_elements["count_reg"];
    assert_eq!(state.width, 8);
}

#[test]
fn test_state_element_in_fsm() {
    let source = r#"
entity FSM {
    in clk: clock
    in rst: reset
    out done: bit
}

impl FSM {
    signal state: bit[2] = 0

    on(clk.rise) {
        state <= if rst {
            0
        } else {
            match state {
                0 => 1,
                1 => 2,
                2 => 3,
                3 => 0,
                _ => 0
            }
        }
    }

    done = if state == 3 { 1 } else { 0 }
}
"#;
    let sir = compile_to_sir(source);

    assert!(sir.state_elements.contains_key("state"));
}

// ============================================================================
// Node Generation Tests
// ============================================================================

#[test]
fn test_nodes_for_combinational_logic() {
    let source = r#"
entity Test {
    in a: bit[8]
    in b: bit[8]
    out result: bit[8]
}

impl Test {
    result = a + b
}
"#;
    let sir = compile_to_sir(source);

    // Should have nodes representing the add operation
    assert!(!sir.combinational_nodes.is_empty() || !sir.sequential_nodes.is_empty());
}

#[test]
fn test_nodes_for_sequential_logic() {
    let source = r#"
entity Register {
    in clk: clock
    in d: bit[8]
    out q: bit[8]
}

impl Register {
    signal q_reg: bit[8] = 0

    on(clk.rise) {
        q_reg <= d
    }

    q = q_reg
}
"#;
    let sir = compile_to_sir(source);

    // Should have nodes for the register
    assert!(!sir.combinational_nodes.is_empty() || !sir.sequential_nodes.is_empty());

    // Should have at least one flipflop node in sequential nodes
    let has_ff = sir
        .sequential_nodes
        .iter()
        .any(|node| matches!(node.kind, SirNodeKind::FlipFlop { .. }));
    assert!(has_ff);
}

// ============================================================================
// Clock Domain Extraction Tests
// ============================================================================

#[test]
fn test_single_clock_domain() {
    let source = r#"
entity Test {
    in clk: clock
    in d: bit[8]
    out q: bit[8]
}

impl Test {
    signal q_reg: bit[8] = 0

    on(clk.rise) {
        q_reg <= d
    }

    q = q_reg
}
"#;
    let sir = compile_to_sir(source);

    // Should extract clock domain
    assert!(!sir.clock_domains.is_empty());
}

#[test]
fn test_clock_and_reset() {
    let source = r#"
entity Test {
    in clk: clock
    in rst: reset
    in d: bit[8]
    out q: bit[8]
}

impl Test {
    signal q_reg: bit[8] = 0

    on(clk.rise) {
        q_reg <= if rst { 0 } else { d }
    }

    q = q_reg
}
"#;
    let sir = compile_to_sir(source);

    assert!(!sir.clock_domains.is_empty());
}

// ============================================================================
// Complex Pattern Tests
// ============================================================================

#[test]
fn test_alu_conversion() {
    let source = r#"
entity ALU {
    in a: bit[32]
    in b: bit[32]
    in op: bit[3]
    out result: bit[32]
}

impl ALU {
    result = match op {
        0b000 => a + b,
        0b001 => a - b,
        0b010 => a & b,
        0b011 => a | b,
        0b100 => a ^ b,
        0b101 => a << b[4:0],
        0b110 => a >> b[4:0],
        _ => 0
    }
}
"#;
    let sir = compile_to_sir(source);

    assert_eq!(sir.name, "ALU");
    assert!(sir.inputs.len() >= 3);
    assert!(!sir.outputs.is_empty());
    assert!(!sir.combinational_nodes.is_empty() || !sir.sequential_nodes.is_empty());
}

#[test]
fn test_pipelined_alu() {
    let source = r#"
entity PipelinedALU {
    in clk: clock
    in a: bit[32]
    in b: bit[32]
    in op: bit[3]
    out result: bit[32]
}

impl PipelinedALU {
    signal stage1: bit[32] = 0
    signal stage2: bit[32] = 0

    signal temp: bit[32]

    temp = match op {
        0 => a + b,
        1 => a - b,
        _ => 0
    }

    on(clk.rise) {
        stage1 <= temp
        stage2 <= stage1
    }

    result = stage2
}
"#;
    let sir = compile_to_sir(source);

    // Should have state elements for pipeline stages
    assert!(sir.state_elements.len() >= 2);
}

#[test]
fn test_state_machine_conversion() {
    let source = r#"
entity StateMachine {
    in clk: clock
    in rst: reset
    in start: bit
    out done: bit
}

impl StateMachine {
    signal state: bit[2] = 0

    on(clk.rise) {
        state <= if rst {
            0
        } else {
            match state {
                0 => if start { 1 } else { 0 },
                1 => 2,
                2 => 3,
                3 => 0,
                _ => 0
            }
        }
    }

    done = if state == 3 { 1 } else { 0 }
}
"#;
    let sir = compile_to_sir(source);

    assert!(sir.state_elements.contains_key("state"));
    assert!(!sir.combinational_nodes.is_empty() || !sir.sequential_nodes.is_empty());
}

#[test]
fn test_counter_conversion() {
    let source = r#"
entity Counter {
    in clk: clock
    in rst: reset
    in enable: bit
    out count: bit[8]
}

impl Counter {
    signal count_reg: bit[8] = 0

    on(clk.rise) {
        count_reg <= if rst {
            0
        } else if enable {
            count_reg + 1
        } else {
            count_reg
        }
    }

    count = count_reg
}
"#;
    let sir = compile_to_sir(source);

    assert_eq!(sir.name, "Counter");
    assert!(sir.state_elements.contains_key("count_reg"));
}

// ============================================================================
// Real-World Example Tests
// ============================================================================

#[test]
fn test_real_world_counter() {
    let source = include_str!("../../../examples/counter.sk");
    let sir = compile_to_sir(source);

    assert_eq!(sir.name, "Counter");
    assert!(!sir.state_elements.is_empty());
}

#[test]
fn test_real_world_alu() {
    let source = include_str!("../../../examples/alu.sk");
    let sir = compile_to_sir(source);

    assert_eq!(sir.name, "ALU");
    assert!(!sir.combinational_nodes.is_empty() || !sir.sequential_nodes.is_empty());
}

// ============================================================================
// Edge Case Tests
// ============================================================================

#[test]
fn test_multiple_state_elements() {
    let source = r#"
entity MultiState {
    in clk: clock
    in d1: bit[8]
    in d2: bit[8]
    out q1: bit[8]
    out q2: bit[8]
}

impl MultiState {
    signal reg1: bit[8] = 0
    signal reg2: bit[8] = 0

    on(clk.rise) {
        reg1 <= d1
        reg2 <= d2
    }

    q1 = reg1
    q2 = reg2
}
"#;
    let sir = compile_to_sir(source);

    assert_eq!(sir.state_elements.len(), 2);
    assert!(sir.state_elements.contains_key("reg1"));
    assert!(sir.state_elements.contains_key("reg2"));
}

#[test]
fn test_mixed_combinational_sequential() {
    let source = r#"
entity Mixed {
    in clk: clock
    in a: bit[8]
    in b: bit[8]
    out result: bit[8]
}

impl Mixed {
    signal temp: bit[8]
    signal reg: bit[8] = 0

    temp = a + b

    on(clk.rise) {
        reg <= temp
    }

    result = reg
}
"#;
    let sir = compile_to_sir(source);

    // temp is combinational, reg is sequential
    assert_eq!(sir.state_elements.len(), 1);
    assert!(sir.state_elements.contains_key("reg"));

    let temp_signal = sir.signals.iter().find(|s| s.name == "temp");
    assert!(temp_signal.is_some());
    assert!(!temp_signal.unwrap().is_state);
}

#[test]
fn test_deeply_nested_logic() {
    let source = r#"
entity Deep {
    in a: bit[8]
    in b: bit[8]
    in c: bit[8]
    in d: bit[8]
    out result: bit[8]
}

impl Deep {
    result = ((a + b) * (c + d))
}
"#;
    let sir = compile_to_sir(source);

    assert_eq!(sir.name, "Deep");
    assert!(!sir.combinational_nodes.is_empty() || !sir.sequential_nodes.is_empty());
}

#[test]
fn test_complex_bit_operations() {
    let source = r#"
entity BitOps {
    in a: bit[32]
    in b: bit[32]
    in c: bit[32]
    out overflow: bit
}

impl BitOps {
    overflow = (~a[31] & ~b[31] & c[31]) | (a[31] & b[31] & ~c[31])
}
"#;
    let sir = compile_to_sir(source);

    assert_eq!(sir.name, "BitOps");
    assert!(!sir.combinational_nodes.is_empty() || !sir.sequential_nodes.is_empty());
}

// ============================================================================
// Pipeline Attribute Tests
// ============================================================================

#[test]
fn test_pipeline_attribute_on_entity() {
    let source = r#"
#[pipeline(stages=3)]
entity PipelinedAdder {
    in a: bit[32]
    in b: bit[32]
    out result: bit[32]
}

impl PipelinedAdder {
    result = a + b
}
"#;
    let sir = compile_to_sir(source);

    assert_eq!(sir.name, "PipelinedAdder");
    // Verify pipeline_config was propagated to SIR
    assert!(
        sir.pipeline_config.is_some(),
        "Pipeline config should be present in SIR"
    );

    let config = sir.pipeline_config.as_ref().unwrap();
    assert_eq!(config.stages, 3, "Pipeline should have 3 stages");
}

#[test]
fn test_pipeline_attribute_with_target_freq() {
    let source = r#"
#[pipeline(stages=4, target_freq=100_000_000)]
entity PipelinedMul {
    in a: bit[32]
    in b: bit[32]
    out result: bit[64]
}

impl PipelinedMul {
    result = (a as bit[64]) * (b as bit[64])
}
"#;
    let sir = compile_to_sir(source);

    assert!(sir.pipeline_config.is_some());
    let config = sir.pipeline_config.as_ref().unwrap();
    assert_eq!(config.stages, 4);
    assert_eq!(config.target_freq, Some(100_000_000));
}

#[test]
fn test_pipeline_attribute_with_auto_balance() {
    let source = r#"
#[pipeline(stages=2, auto_balance=true)]
entity BalancedOp {
    in x: bit[16]
    in y: bit[16]
    out result: bit[16]
}

impl BalancedOp {
    result = x ^ y
}
"#;
    let sir = compile_to_sir(source);

    assert!(sir.pipeline_config.is_some());
    let config = sir.pipeline_config.as_ref().unwrap();
    assert_eq!(config.stages, 2);
    assert!(config.auto_balance);
}

#[test]
fn test_no_pipeline_attribute_on_entity() {
    let source = r#"
entity SimpleAdder {
    in a: bit[32]
    in b: bit[32]
    out result: bit[32]
}

impl SimpleAdder {
    result = a + b
}
"#;
    let sir = compile_to_sir(source);

    // No pipeline attribute means no pipeline_config
    assert!(
        sir.pipeline_config.is_none(),
        "Pipeline config should be None when no attribute is used"
    );
}

#[test]
fn test_single_stage_pipeline_skipped() {
    let source = r#"
#[pipeline(stages=1)]
entity SingleStage {
    in a: bit[8]
    out b: bit[8]
}

impl SingleStage {
    b = a
}
"#;
    let sir = compile_to_sir(source);

    // Pipeline config exists but stages=1 means no registers inserted
    assert!(sir.pipeline_config.is_some());
    let config = sir.pipeline_config.as_ref().unwrap();
    assert_eq!(config.stages, 1);
    // No pipeline registers should be inserted for single stage
}
