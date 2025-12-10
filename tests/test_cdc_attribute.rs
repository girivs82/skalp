//! Tests for #[cdc] attribute feature
//!
//! Tests the #[cdc] attribute for clock domain crossing synchronization
//! and its propagation through HIR -> MIR.

use skalp_frontend::hir_builder::build_hir;
use skalp_frontend::parse::parse;
use skalp_frontend::hir::CdcType;
use skalp_mir::hir_to_mir::HirToMir;

#[test]
fn test_cdc_attribute_basic() {
    println!("=== Testing Basic #[cdc] Attribute ===");

    let source = r#"
entity CdcTest {
    in clk: bit,
    in async_in: bit,
    out sync_out: bit,

    // Signal with cdc attribute - should be auto-synchronized
    #[cdc]
    signal cross_domain: bit,
}

impl CdcTest {
    cross_domain = async_in;
    sync_out = cross_domain;
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    // Find the entity
    let entity = hir.entities.iter().find(|e| e.name == "CdcTest");
    assert!(entity.is_some(), "Should have CdcTest entity");
    let entity = entity.unwrap();

    // Find the cross_domain signal
    let cdc_signal = entity.signals.iter().find(|s| s.name == "cross_domain");
    assert!(cdc_signal.is_some(), "Should have 'cross_domain' signal");
    let cdc_signal = cdc_signal.unwrap();

    // Check that cdc_config is present
    assert!(
        cdc_signal.cdc_config.is_some(),
        "cross_domain signal should have cdc_config from #[cdc] attribute"
    );

    let cdc_config = cdc_signal.cdc_config.as_ref().unwrap();
    assert_eq!(cdc_config.sync_stages, 2, "Default sync_stages should be 2");
    assert_eq!(cdc_config.cdc_type, CdcType::TwoFF, "Default cdc_type should be TwoFF");

    println!("Basic #[cdc] attribute test PASSED!");
}

#[test]
fn test_cdc_attribute_with_stages() {
    println!("=== Testing #[cdc] Attribute with Custom Stages ===");

    let source = r#"
entity CdcStagesTest {
    in clk: bit,
    in async_in: bit,
    out sync_out: bit,

    // Signal with 3-stage synchronizer
    #[cdc(sync_stages = 3)]
    signal metastable_input: bit,

    // Signal with just a number (shorthand)
    #[cdc(4)]
    signal four_stage: bit,
}

impl CdcStagesTest {
    metastable_input = async_in;
    four_stage = async_in;
    sync_out = metastable_input;
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    let entity = hir.entities.iter().find(|e| e.name == "CdcStagesTest").unwrap();

    // Check metastable_input has 3 stages
    let signal = entity.signals.iter().find(|s| s.name == "metastable_input").unwrap();
    assert!(signal.cdc_config.is_some(), "metastable_input should have cdc_config");
    let cdc_config = signal.cdc_config.as_ref().unwrap();
    assert_eq!(cdc_config.sync_stages, 3, "metastable_input should have 3 sync_stages");

    // Check four_stage has 4 stages
    let signal = entity.signals.iter().find(|s| s.name == "four_stage").unwrap();
    assert!(signal.cdc_config.is_some(), "four_stage should have cdc_config");
    let cdc_config = signal.cdc_config.as_ref().unwrap();
    assert_eq!(cdc_config.sync_stages, 4, "four_stage should have 4 sync_stages");

    println!("#[cdc] with custom stages test PASSED!");
}

#[test]
fn test_cdc_attribute_with_type() {
    println!("=== Testing #[cdc] Attribute with CDC Type ===");

    let source = r#"
entity CdcTypeTest {
    in clk: bit,
    in async_bus: bit[8],
    out sync_bus: bit[8],

    // Gray code synchronizer for bus
    #[cdc(cdc_type = gray)]
    signal gray_sync: bit[8],

    // Pulse synchronizer
    #[cdc(cdc_type = pulse)]
    signal pulse_sync: bit,

    // Handshake synchronizer
    #[cdc(cdc_type = handshake)]
    signal handshake_sync: bit,
}

impl CdcTypeTest {
    gray_sync = async_bus;
    pulse_sync = async_bus[0];
    handshake_sync = async_bus[1];
    sync_bus = gray_sync;
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    let entity = hir.entities.iter().find(|e| e.name == "CdcTypeTest").unwrap();

    // Check gray_sync has Gray type
    let signal = entity.signals.iter().find(|s| s.name == "gray_sync").unwrap();
    assert!(signal.cdc_config.is_some());
    let cdc_config = signal.cdc_config.as_ref().unwrap();
    assert_eq!(cdc_config.cdc_type, CdcType::Gray, "gray_sync should have Gray cdc_type");

    // Check pulse_sync has Pulse type
    let signal = entity.signals.iter().find(|s| s.name == "pulse_sync").unwrap();
    assert!(signal.cdc_config.is_some());
    let cdc_config = signal.cdc_config.as_ref().unwrap();
    assert_eq!(cdc_config.cdc_type, CdcType::Pulse, "pulse_sync should have Pulse cdc_type");

    // Check handshake_sync has Handshake type
    let signal = entity.signals.iter().find(|s| s.name == "handshake_sync").unwrap();
    assert!(signal.cdc_config.is_some());
    let cdc_config = signal.cdc_config.as_ref().unwrap();
    assert_eq!(cdc_config.cdc_type, CdcType::Handshake, "handshake_sync should have Handshake cdc_type");

    println!("#[cdc] with CDC type test PASSED!");
}

#[test]
fn test_cdc_mir_propagation() {
    println!("=== Testing #[cdc] MIR Propagation ===");

    let source = r#"
entity MirCdcTest {
    in clk: bit,
    in data: bit[8],
    out result: bit[8],

    #[cdc(sync_stages = 3)]
    signal cdc_data: bit[8],

    signal normal_data: bit[8],
}

impl MirCdcTest {
    cdc_data = data;
    normal_data = data;
    result = cdc_data;
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    // Convert to MIR
    let mut hir_to_mir = HirToMir::new();
    let mir = hir_to_mir.transform(&hir);

    // Find the module
    let module = mir.modules.iter().find(|m| m.name == "MirCdcTest");
    assert!(module.is_some(), "Should have MirCdcTest module");
    let module = module.unwrap();

    // Check cdc_data has cdc_config in MIR
    let cdc_signal = module.signals.iter().find(|s| s.name == "cdc_data");
    assert!(cdc_signal.is_some(), "Should have cdc_data signal");
    let cdc_signal = cdc_signal.unwrap();
    assert!(
        cdc_signal.cdc_config.is_some(),
        "cdc_data should have cdc_config in MIR"
    );
    assert_eq!(
        cdc_signal.cdc_config.as_ref().unwrap().sync_stages,
        3,
        "cdc_data should have 3 sync_stages"
    );

    // Check normal_data does NOT have cdc_config
    let normal_signal = module.signals.iter().find(|s| s.name == "normal_data");
    assert!(normal_signal.is_some(), "Should have normal_data signal");
    let normal_signal = normal_signal.unwrap();
    assert!(
        normal_signal.cdc_config.is_none(),
        "normal_data should NOT have cdc_config"
    );

    println!("#[cdc] MIR propagation test PASSED!");
}

#[test]
fn test_cdc_and_trace_combined() {
    println!("=== Testing Combined #[cdc] and #[trace] Attributes ===");

    // Both cdc and trace attributes should work on different signals
    let source = r#"
entity CombinedCdcTest {
    in clk: bit,
    in async_data: bit[8],
    out result: bit[8],

    // CDC signal
    #[cdc(sync_stages = 2)]
    signal sync_data: bit[8],

    // Traced debug signal
    #[trace]
    signal debug_state: bit[4],
}

impl CombinedCdcTest {
    sync_data = async_data;
    debug_state = async_data[3:0];
    result = sync_data;
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    let entity = hir.entities.iter().find(|e| e.name == "CombinedCdcTest").unwrap();

    // Check sync_data has cdc_config but not trace_config
    let sync_signal = entity.signals.iter().find(|s| s.name == "sync_data").unwrap();
    assert!(sync_signal.cdc_config.is_some(), "sync_data should have cdc_config");
    assert!(sync_signal.trace_config.is_none(), "sync_data should NOT have trace_config");

    // Check debug_state has trace_config but not cdc_config
    let debug_signal = entity.signals.iter().find(|s| s.name == "debug_state").unwrap();
    assert!(debug_signal.trace_config.is_some(), "debug_state should have trace_config");
    assert!(debug_signal.cdc_config.is_none(), "debug_state should NOT have cdc_config");

    println!("Combined #[cdc] and #[trace] test PASSED!");
}

#[test]
fn test_cdc_combined_attributes() {
    println!("=== Testing #[cdc] with Multiple Parameters ===");

    let source = r#"
entity CdcCombinedTest {
    in clk: bit,
    in data: bit[8],
    out result: bit[8],

    // CDC with both stages and type
    #[cdc(sync_stages = 3, cdc_type = gray)]
    signal gray_bus: bit[8],

    // CDC with stages shorthand
    #[cdc(stages = 2)]
    signal two_stage: bit,
}

impl CdcCombinedTest {
    gray_bus = data;
    two_stage = data[0];
    result = gray_bus;
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    let entity = hir.entities.iter().find(|e| e.name == "CdcCombinedTest").unwrap();

    // Check gray_bus has both stages and type
    let signal = entity.signals.iter().find(|s| s.name == "gray_bus").unwrap();
    assert!(signal.cdc_config.is_some());
    let cdc_config = signal.cdc_config.as_ref().unwrap();
    assert_eq!(cdc_config.sync_stages, 3, "gray_bus should have 3 stages");
    assert_eq!(cdc_config.cdc_type, CdcType::Gray, "gray_bus should have Gray type");

    // Check two_stage uses "stages" shorthand
    let signal = entity.signals.iter().find(|s| s.name == "two_stage").unwrap();
    assert!(signal.cdc_config.is_some());
    let cdc_config = signal.cdc_config.as_ref().unwrap();
    assert_eq!(cdc_config.sync_stages, 2, "two_stage should have 2 stages");

    println!("#[cdc] combined attributes test PASSED!");
}

#[test]
fn test_cdc_with_domain_identifiers() {
    println!("=== Testing #[cdc] with Domain Identifiers ===");

    // Test domain references using identifiers
    // Note: Use source/destination to ensure all tokens are captured
    let source = r#"
entity CdcDomainTest {
    in clk_fast: bit,
    in clk_slow: bit,
    in data: bit[8],
    out result: bit[8],

    // CDC with explicit domain names
    #[cdc(source = fast_clk, destination = slow_clk, sync_stages = 2)]
    signal cross_domain: bit[8],

    // Using source/destination aliases
    #[cdc(source = producer, destination = consumer)]
    signal async_data: bit[8],
}

impl CdcDomainTest {
    cross_domain = data;
    async_data = data;
    result = cross_domain;
}
"#;

    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    let entity = hir.entities.iter().find(|e| e.name == "CdcDomainTest").unwrap();

    // Check cross_domain has from/to domains
    let signal = entity.signals.iter().find(|s| s.name == "cross_domain").unwrap();
    assert!(signal.cdc_config.is_some(), "cross_domain should have cdc_config");
    let cdc_config = signal.cdc_config.as_ref().unwrap();
    assert_eq!(cdc_config.from_domain, Some("fast_clk".to_string()), "from_domain should be 'fast_clk'");
    assert_eq!(cdc_config.to_domain, Some("slow_clk".to_string()), "to_domain should be 'slow_clk'");
    assert_eq!(cdc_config.sync_stages, 2, "sync_stages should be 2");

    // Check async_data with source/destination aliases
    let signal = entity.signals.iter().find(|s| s.name == "async_data").unwrap();
    assert!(signal.cdc_config.is_some(), "async_data should have cdc_config");
    let cdc_config = signal.cdc_config.as_ref().unwrap();
    assert_eq!(cdc_config.from_domain, Some("producer".to_string()), "from_domain should be 'producer'");
    assert_eq!(cdc_config.to_domain, Some("consumer".to_string()), "to_domain should be 'consumer'");

    println!("#[cdc] with domain identifiers test PASSED!");
}

// NOTE: Lifetime-style domains (#[cdc(source = 'fast_clk, ...)]) require
// parser enhancement to capture lifetime tokens in attribute values.
// The attribute system is designed to support this; the parser needs updating.
// For now, use identifier-style domain names: #[cdc(source = fast_clk, ...)]
