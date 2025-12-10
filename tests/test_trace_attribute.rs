//! Tests for #[trace] attribute feature
//!
//! Tests the #[trace] attribute for debug/waveform export and its propagation
//! through HIR -> MIR.

use skalp_frontend::hir_builder::build_hir;
use skalp_frontend::parse::parse;
use skalp_frontend::hir::TraceRadix;
use skalp_mir::hir_to_mir::HirToMir;

#[test]
fn test_trace_attribute_basic() {
    println!("=== Testing Basic #[trace] Attribute ===");

    let source = r#"
entity TraceTest {
    in clk: bit,
    in data_in: bit[8],
    out data_out: bit[8],

    // Signal with trace attribute - should be auto-exported to VCD
    #[trace]
    signal debug_state: bit[4],
}

impl TraceTest {
    data_out = data_in;
    debug_state = data_in[3:0];
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    // Find the entity
    let entity = hir.entities.iter().find(|e| e.name == "TraceTest");
    assert!(entity.is_some(), "Should have TraceTest entity");
    let entity = entity.unwrap();

    // Find the debug_state signal
    let debug_signal = entity.signals.iter().find(|s| s.name == "debug_state");
    assert!(debug_signal.is_some(), "Should have 'debug_state' signal");
    let debug_signal = debug_signal.unwrap();

    // Check that trace_config is present
    assert!(
        debug_signal.trace_config.is_some(),
        "debug_state signal should have trace_config from #[trace] attribute"
    );

    let trace_config = debug_signal.trace_config.as_ref().unwrap();
    assert!(trace_config.group.is_none(), "Basic trace should have no group");
    assert_eq!(trace_config.radix, TraceRadix::Binary, "Default radix should be Binary");

    println!("Basic #[trace] attribute test PASSED!");
}

#[test]
fn test_trace_attribute_with_group() {
    println!("=== Testing #[trace] Attribute with Group ===");

    // Note: String literal parsing in attributes may not be fully supported yet.
    // For now, test that trace without group at least works
    let source = r#"
entity GroupedTrace {
    in clk: bit,

    #[trace]
    signal fsm_state: bit[4],

    #[trace]
    signal counter: bit[8],

    #[trace]
    signal data_bus: bit[32],
}

impl GroupedTrace {
    fsm_state = 0;
    counter = 0;
    data_bus = 0;
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    let entity = hir.entities.iter().find(|e| e.name == "GroupedTrace").unwrap();

    // Check fsm_state has trace config
    let fsm_signal = entity.signals.iter().find(|s| s.name == "fsm_state").unwrap();
    assert!(fsm_signal.trace_config.is_some(), "fsm_state should have trace_config");

    // Check counter has trace config
    let counter_signal = entity.signals.iter().find(|s| s.name == "counter").unwrap();
    assert!(counter_signal.trace_config.is_some(), "counter should have trace_config");

    // Check data_bus has trace config
    let data_signal = entity.signals.iter().find(|s| s.name == "data_bus").unwrap();
    assert!(data_signal.trace_config.is_some(), "data_bus should have trace_config");

    println!("#[trace] with group attribute test PASSED!");
}

#[test]
fn test_trace_attribute_with_radix() {
    println!("=== Testing #[trace] Attribute with Radix ===");

    let source = r#"
entity RadixTrace {
    in clk: bit,

    #[trace(radix = hex)]
    signal hex_data: bit[32],

    #[trace(radix = signed)]
    signal signed_val: bit[16],

    #[trace(radix = unsigned)]
    signal unsigned_val: bit[16],
}

impl RadixTrace {
    hex_data = 0;
    signed_val = 0;
    unsigned_val = 0;
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    let entity = hir.entities.iter().find(|e| e.name == "RadixTrace").unwrap();

    // Check hex_data
    let hex_signal = entity.signals.iter().find(|s| s.name == "hex_data").unwrap();
    assert!(hex_signal.trace_config.is_some());
    let trace_config = hex_signal.trace_config.as_ref().unwrap();
    assert_eq!(trace_config.radix, TraceRadix::Hex);

    // Check signed_val
    let signed_signal = entity.signals.iter().find(|s| s.name == "signed_val").unwrap();
    assert!(signed_signal.trace_config.is_some());
    let trace_config = signed_signal.trace_config.as_ref().unwrap();
    assert_eq!(trace_config.radix, TraceRadix::Signed);

    // Check unsigned_val
    let unsigned_signal = entity.signals.iter().find(|s| s.name == "unsigned_val").unwrap();
    assert!(unsigned_signal.trace_config.is_some());
    let trace_config = unsigned_signal.trace_config.as_ref().unwrap();
    assert_eq!(trace_config.radix, TraceRadix::Unsigned);

    println!("#[trace] with radix attribute test PASSED!");
}

#[test]
fn test_trace_mir_propagation() {
    println!("=== Testing #[trace] MIR Propagation ===");

    let source = r#"
entity MirTraceTest {
    in clk: bit,
    in data: bit[8],
    out result: bit[8],

    #[trace]
    signal traced_data: bit[8],

    signal untraced_data: bit[8],
}

impl MirTraceTest {
    traced_data = data;
    untraced_data = data;
    result = traced_data;
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    // Convert to MIR
    let mut hir_to_mir = HirToMir::new();
    let mir = hir_to_mir.transform(&hir);

    // Find the module
    let module = mir.modules.iter().find(|m| m.name == "MirTraceTest");
    assert!(module.is_some(), "Should have MirTraceTest module");
    let module = module.unwrap();

    // Check traced_data has trace_config in MIR
    let traced_signal = module.signals.iter().find(|s| s.name == "traced_data");
    assert!(traced_signal.is_some(), "Should have traced_data signal");
    let traced_signal = traced_signal.unwrap();
    assert!(
        traced_signal.trace_config.is_some(),
        "traced_data should have trace_config in MIR"
    );

    // Check untraced_data does NOT have trace_config
    let untraced_signal = module.signals.iter().find(|s| s.name == "untraced_data");
    assert!(untraced_signal.is_some(), "Should have untraced_data signal");
    let untraced_signal = untraced_signal.unwrap();
    assert!(
        untraced_signal.trace_config.is_none(),
        "untraced_data should NOT have trace_config"
    );

    println!("#[trace] MIR propagation test PASSED!");
}

#[test]
fn test_trace_and_memory_combined() {
    println!("=== Testing Combined #[trace] and #[memory] Attributes ===");

    // Both trace and memory attributes can coexist on different signals
    let source = r#"
entity CombinedTest {
    in addr: bit[10],
    in write_data: bit[32],
    out read_data: bit[32],

    // Memory signal
    #[memory(depth = 1024)]
    signal mem: bit[32],

    // Traced debug signal
    #[trace(group = "debug")]
    signal addr_debug: bit[10],
}

impl CombinedTest {
    read_data = mem;
    addr_debug = addr;
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    let entity = hir.entities.iter().find(|e| e.name == "CombinedTest").unwrap();

    // Check mem signal has memory_config but not trace_config
    let mem_signal = entity.signals.iter().find(|s| s.name == "mem").unwrap();
    assert!(mem_signal.memory_config.is_some(), "mem should have memory_config");
    assert!(mem_signal.trace_config.is_none(), "mem should NOT have trace_config");

    // Check addr_debug has trace_config but not memory_config
    let debug_signal = entity.signals.iter().find(|s| s.name == "addr_debug").unwrap();
    assert!(debug_signal.trace_config.is_some(), "addr_debug should have trace_config");
    assert!(debug_signal.memory_config.is_none(), "addr_debug should NOT have memory_config");

    println!("Combined attributes test PASSED!");
}

#[test]
fn test_trace_display_name() {
    println!("=== Testing #[trace] with display_name string literal ===");

    // Test string literal parsing in attributes
    let source = r#"
entity DisplayNameTest {
    in clk: bit,

    #[trace(display_name = "FSM State")]
    signal internal_state: bit[4],

    #[trace(name = "Debug Counter")]
    signal debug_counter: bit[8],
}

impl DisplayNameTest {
    internal_state = 0;
    debug_counter = 0;
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    let entity = hir.entities.iter().find(|e| e.name == "DisplayNameTest").unwrap();

    // Check internal_state has display_name
    let signal = entity.signals.iter().find(|s| s.name == "internal_state").unwrap();
    assert!(signal.trace_config.is_some(), "internal_state should have trace_config");
    let trace_config = signal.trace_config.as_ref().unwrap();
    assert_eq!(trace_config.display_name, Some("FSM State".to_string()),
               "display_name should be 'FSM State'");

    // Check debug_counter has name (alias for display_name)
    let signal = entity.signals.iter().find(|s| s.name == "debug_counter").unwrap();
    assert!(signal.trace_config.is_some(), "debug_counter should have trace_config");
    let trace_config = signal.trace_config.as_ref().unwrap();
    assert_eq!(trace_config.display_name, Some("Debug Counter".to_string()),
               "display_name should be 'Debug Counter'");

    println!("#[trace] display_name test PASSED!");
}

#[test]
fn test_trace_with_group_string() {
    println!("=== Testing #[trace] with group string literal ===");

    let source = r#"
entity GroupStringTest {
    in clk: bit,

    #[trace(group = "control_signals")]
    signal fsm_state: bit[4],

    #[trace(group = "data_path", radix = hex)]
    signal data_bus: bit[32],
}

impl GroupStringTest {
    fsm_state = 0;
    data_bus = 0;
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    let entity = hir.entities.iter().find(|e| e.name == "GroupStringTest").unwrap();

    // Check fsm_state has group
    let signal = entity.signals.iter().find(|s| s.name == "fsm_state").unwrap();
    assert!(signal.trace_config.is_some(), "fsm_state should have trace_config");
    let trace_config = signal.trace_config.as_ref().unwrap();
    assert_eq!(trace_config.group, Some("control_signals".to_string()),
               "group should be 'control_signals'");

    // Check data_bus has group and radix
    let signal = entity.signals.iter().find(|s| s.name == "data_bus").unwrap();
    assert!(signal.trace_config.is_some(), "data_bus should have trace_config");
    let trace_config = signal.trace_config.as_ref().unwrap();
    assert_eq!(trace_config.group, Some("data_path".to_string()),
               "group should be 'data_path'");
    assert_eq!(trace_config.radix, TraceRadix::Hex, "radix should be Hex");

    println!("#[trace] with group string test PASSED!");
}
