//! Tests for #[xilinx_ip], #[intel_ip], and #[vendor_ip] attributes
//!
//! Tests the vendor IP attribute for wrapping vendor-specific IP cores
//! and its propagation through HIR -> MIR -> SystemVerilog codegen.

use skalp_frontend::hir_builder::build_hir;
use skalp_frontend::parse::parse;
use skalp_frontend::hir::{VendorType};

#[test]
fn test_xilinx_ip_basic() {
    println!("=== Testing Basic #[xilinx_ip] Attribute ===");

    let source = r#"
#[xilinx_ip("xpm_fifo_sync")]
entity SyncFifo {
    in wr_clk: clock,
    in wr_en: bit,
    in din: bit[32],
    out full: bit,
    in rd_clk: clock,
    in rd_en: bit,
    out dout: bit[32],
    out empty: bit,
}

impl SyncFifo {
    // Vendor IP - no implementation needed
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    // Find the entity
    let entity = hir.entities.iter().find(|e| e.name == "SyncFifo");
    assert!(entity.is_some(), "Should have SyncFifo entity");
    let entity = entity.unwrap();

    // Check that vendor_ip_config is present
    assert!(
        entity.vendor_ip_config.is_some(),
        "SyncFifo should have vendor_ip_config from #[xilinx_ip] attribute"
    );

    let config = entity.vendor_ip_config.as_ref().unwrap();
    assert_eq!(config.ip_name, "xpm_fifo_sync");
    assert_eq!(config.vendor, VendorType::Xilinx);

    println!("#[xilinx_ip] basic test PASSED!");
}

#[test]
fn test_xilinx_ip_with_library() {
    println!("=== Testing #[xilinx_ip] with Library ===");

    let source = r#"
#[xilinx_ip(name = "xpm_memory_spram", library = "xpm")]
entity SinglePortRam {
    in clk: clock,
    in addr: bit[10],
    in din: bit[32],
    in we: bit,
    out dout: bit[32],
}

impl SinglePortRam {
    // Vendor IP - no implementation needed
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    let entity = hir.entities.iter().find(|e| e.name == "SinglePortRam").unwrap();

    assert!(entity.vendor_ip_config.is_some());
    let config = entity.vendor_ip_config.as_ref().unwrap();

    assert_eq!(config.ip_name, "xpm_memory_spram");
    assert_eq!(config.vendor, VendorType::Xilinx);
    assert_eq!(config.library, Some("xpm".to_string()));

    println!("#[xilinx_ip] with library test PASSED!");
}

#[test]
fn test_intel_ip_basic() {
    println!("=== Testing #[intel_ip] Attribute ===");

    let source = r#"
#[intel_ip("altera_fifo")]
entity IntelFifo {
    in clk: clock,
    in data_in: bit[16],
    in wrreq: bit,
    out q: bit[16],
    in rdreq: bit,
    out empty: bit,
    out full: bit,
}

impl IntelFifo {
    // Vendor IP - no implementation needed
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    let entity = hir.entities.iter().find(|e| e.name == "IntelFifo").unwrap();

    assert!(entity.vendor_ip_config.is_some());
    let config = entity.vendor_ip_config.as_ref().unwrap();

    assert_eq!(config.ip_name, "altera_fifo");
    assert_eq!(config.vendor, VendorType::Intel);

    println!("#[intel_ip] test PASSED!");
}

#[test]
fn test_generic_vendor_ip() {
    println!("=== Testing #[vendor_ip] Attribute ===");

    let source = r#"
#[vendor_ip(name = "custom_ip", vendor = xilinx)]
entity CustomIp {
    in clk: clock,
    in data: bit[64],
    out result: bit[64],
}

impl CustomIp {
    // Vendor IP - no implementation needed
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    let entity = hir.entities.iter().find(|e| e.name == "CustomIp").unwrap();

    assert!(entity.vendor_ip_config.is_some());
    let config = entity.vendor_ip_config.as_ref().unwrap();

    assert_eq!(config.ip_name, "custom_ip");
    // Note: vendor = xilinx should override the default Generic vendor
    assert_eq!(config.vendor, VendorType::Xilinx);

    println!("#[vendor_ip] test PASSED!");
}

#[test]
fn test_vendor_ip_mir_propagation() {
    println!("=== Testing Vendor IP MIR Propagation ===");

    let source = r#"
#[xilinx_ip("xpm_fifo_async")]
entity AsyncFifo {
    in wr_clk: clock,
    in wr_en: bit,
    in din: bit[32],
    out full: bit,
    in rd_clk: clock,
    in rd_en: bit,
    out dout: bit[32],
    out empty: bit,
}

impl AsyncFifo {
    // Vendor IP - no implementation needed
}
"#;

    use skalp_mir::{MirCompiler, OptimizationLevel};

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    // Compile to MIR
    let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
    let mir = compiler.compile_to_mir(&hir).expect("Failed to compile to MIR");

    // Find the module
    let module = mir.modules.iter().find(|m| m.name == "AsyncFifo");
    assert!(module.is_some(), "Should have AsyncFifo module in MIR");
    let module = module.unwrap();

    // Check vendor_ip_config propagated
    assert!(
        module.vendor_ip_config.is_some(),
        "AsyncFifo should have vendor_ip_config in MIR"
    );

    let config = module.vendor_ip_config.as_ref().unwrap();
    assert_eq!(config.ip_name, "xpm_fifo_async");
    assert_eq!(config.vendor, VendorType::Xilinx);

    println!("Vendor IP MIR propagation test PASSED!");
}

#[test]
fn test_vendor_ip_codegen() {
    println!("=== Testing Vendor IP SystemVerilog Codegen ===");

    let source = r#"
#[xilinx_ip("xpm_fifo_sync")]
entity XpmFifoWrapper {
    in clk: clock,
    in rst: bit,
    in din: bit[32],
    in wr_en: bit,
    out dout: bit[32],
    in rd_en: bit,
    out full: bit,
    out empty: bit,
}

impl XpmFifoWrapper {
    // Vendor IP - no implementation needed
}
"#;

    use skalp_codegen::generate_systemverilog_from_mir;
    use skalp_frontend::parse_and_build_hir;
    use skalp_lir::lower_to_lir;
    use skalp_mir::{MirCompiler, OptimizationLevel};

    // Full pipeline
    let hir = parse_and_build_hir(source).expect("Failed to parse");
    let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
    let mir = compiler.compile_to_mir(&hir).expect("Failed to compile to MIR");
    let lir = lower_to_lir(&mir).expect("Failed to lower to LIR");
    let sv = generate_systemverilog_from_mir(&mir, &lir).expect("SV codegen should succeed");

    println!("Generated SystemVerilog:\n{}", sv);

    // Check output contains expected elements
    assert!(sv.contains("Vendor IP Wrapper"), "Should have vendor IP comment");
    assert!(sv.contains("xpm_fifo_sync"), "Should reference IP name");
    assert!(sv.contains("module XpmFifoWrapper"), "Should have module declaration");
    assert!(sv.contains("XpmFifoWrapper_inst"), "Should have IP instance");

    println!("Vendor IP codegen test PASSED!");
}

#[test]
fn test_blackbox_vendor_ip() {
    println!("=== Testing Black-box Vendor IP ===");

    // Use vendor_ip with black_box = true
    let source = r#"
#[vendor_ip(name = "external_ip", black_box = true)]
entity ExternalIp {
    in clk: clock,
    in data_in: bit[8],
    out data_out: bit[8],
}

impl ExternalIp {
    // Black-box - implemented externally
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    let entity = hir.entities.iter().find(|e| e.name == "ExternalIp").unwrap();

    assert!(entity.vendor_ip_config.is_some());
    let config = entity.vendor_ip_config.as_ref().unwrap();

    assert_eq!(config.ip_name, "external_ip");
    assert_eq!(config.vendor, VendorType::Generic);
    assert!(config.black_box, "Should be marked as black-box");

    println!("Black-box vendor IP test PASSED!");
}
