//! Tests for memory inference feature
//!
//! Tests the #[memory(depth=N)] attribute and its propagation through
//! HIR -> MIR.

use skalp_frontend::hir::MemoryStyle;
use skalp_frontend::hir_builder::build_hir;
use skalp_frontend::parse::parse;
use skalp_mir::hir_to_mir::HirToMir;

/// Debug test to understand syntax tree structure
#[test]
fn test_debug_syntax_tree_structure() {
    use skalp_frontend::syntax::{SyntaxKind, SyntaxNode};

    fn print_tree(node: &SyntaxNode, indent: usize) {
        let prefix = "  ".repeat(indent);
        println!("{}{:?}", prefix, node.kind());
        for child in node.children() {
            print_tree(&child, indent + 1);
        }
    }

    let source = r#"
entity Test {
    in x: bit[8],

    #[memory(depth = 1024)]
    signal mem: bit[64],
}
"#;

    let tree = parse(source);
    println!("\n=== SYNTAX TREE STRUCTURE ===");
    print_tree(&tree, 0);
    println!("=== END TREE ===\n");

    // Find the PortList and examine its children
    fn find_port_list(node: &SyntaxNode) -> Option<SyntaxNode> {
        if node.kind() == SyntaxKind::PortList {
            return Some(node.clone());
        }
        for child in node.children() {
            if let Some(found) = find_port_list(&child) {
                return Some(found);
            }
        }
        None
    }

    if let Some(port_list) = find_port_list(&tree) {
        println!("=== PORTLIST DIRECT CHILDREN ===");
        for (i, child) in port_list.children().enumerate() {
            println!("  [{}] {:?}", i, child.kind());
        }
        println!("=== END PORTLIST ===\n");
    }
}

#[test]
fn test_memory_attribute_parsing() {
    println!("=== Testing Memory Attribute Parsing ===");

    let source = r#"
entity MemoryTest {
    in addr: bit[10],
    in write_data: bit[64],
    in write_en: bit,
    out read_data: bit[64],

    // Memory signal with depth attribute
    #[memory(depth = 1024)]
    signal mem: bit[64],
}

impl MemoryTest {
    // Simple passthrough for now
    read_data = mem
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    // Find the memory signal in the entity
    let entity = hir.entities.iter().find(|e| e.name == "MemoryTest");
    assert!(entity.is_some(), "Should have MemoryTest entity");
    let entity = entity.unwrap();

    // Find the mem signal
    let mem_signal = entity.signals.iter().find(|s| s.name == "mem");
    assert!(mem_signal.is_some(), "Should have 'mem' signal");
    let mem_signal = mem_signal.unwrap();

    // Check that memory_config is present
    assert!(
        mem_signal.memory_config.is_some(),
        "mem signal should have memory_config from #[memory] attribute"
    );

    let mem_config = mem_signal.memory_config.as_ref().unwrap();
    assert_eq!(mem_config.depth, 1024, "Memory depth should be 1024");
    assert_eq!(
        mem_config.style,
        MemoryStyle::Auto,
        "Default style should be Auto"
    );

    println!("Memory attribute parsing test PASSED!");
}

#[test]
fn test_memory_style_attribute() {
    println!("=== Testing Memory Style Attribute ===");

    let source = r#"
entity BramTest {
    in addr: bit[8],
    out data: bit[32],

    // Memory signal with explicit BRAM style
    #[memory(depth = 256, style = block)]
    signal bram: bit[32],
}

impl BramTest {
    data = bram
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    let entity = hir.entities.iter().find(|e| e.name == "BramTest").unwrap();
    let bram_signal = entity.signals.iter().find(|s| s.name == "bram").unwrap();

    assert!(bram_signal.memory_config.is_some());
    let mem_config = bram_signal.memory_config.as_ref().unwrap();
    assert_eq!(mem_config.depth, 256);
    assert_eq!(mem_config.style, MemoryStyle::Block);

    println!("Memory style attribute test PASSED!");
}

#[test]
fn test_memory_mir_propagation() {
    println!("=== Testing Memory MIR Propagation ===");

    let source = r#"
entity MirMemTest {
    in addr: bit[10],
    out data: bit[64],

    #[memory(depth = 1024)]
    signal mem: bit[64],
}

impl MirMemTest {
    data = mem
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    // Compile to MIR
    let mut transformer = HirToMir::new();
    let mir = transformer.transform(&hir);

    // Find the MirMemTest module
    let module = mir.modules.iter().find(|m| m.name == "MirMemTest");
    assert!(module.is_some(), "Should have MirMemTest module");
    let module = module.unwrap();

    // Find the mem signal in MIR
    let mem_signal = module.signals.iter().find(|s| s.name == "mem");
    assert!(mem_signal.is_some(), "Should have 'mem' signal in MIR");
    let mem_signal = mem_signal.unwrap();

    // Check that memory_config propagated to MIR
    assert!(
        mem_signal.memory_config.is_some(),
        "mem signal in MIR should have memory_config"
    );

    let mem_config = mem_signal.memory_config.as_ref().unwrap();
    assert_eq!(
        mem_config.depth, 1024,
        "Memory depth should propagate to MIR"
    );

    println!("Memory MIR propagation test PASSED!");
}

#[test]
fn test_memory_distributed_style() {
    println!("=== Testing Distributed Memory Style ===");

    let source = r#"
entity DistMemTest {
    in addr: bit[4],
    out data: bit[16],

    #[memory(depth = 16, style = distributed)]
    signal lutram: bit[16],
}

impl DistMemTest {
    data = lutram
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    let entity = hir
        .entities
        .iter()
        .find(|e| e.name == "DistMemTest")
        .unwrap();
    let mem_signal = entity.signals.iter().find(|s| s.name == "lutram").unwrap();

    assert!(mem_signal.memory_config.is_some());
    let mem_config = mem_signal.memory_config.as_ref().unwrap();
    assert_eq!(mem_config.depth, 16);
    assert_eq!(mem_config.style, MemoryStyle::Distributed);

    // Compile to MIR to verify propagation
    let mut transformer = HirToMir::new();
    let mir = transformer.transform(&hir);

    let module = mir
        .modules
        .iter()
        .find(|m| m.name == "DistMemTest")
        .unwrap();
    let mir_signal = module.signals.iter().find(|s| s.name == "lutram").unwrap();

    assert!(mir_signal.memory_config.is_some());
    assert_eq!(
        mir_signal.memory_config.as_ref().unwrap().style,
        MemoryStyle::Distributed
    );

    println!("Distributed memory style test PASSED!");
}

#[test]
fn test_memory_with_width_override() {
    println!("=== Testing Memory with Width Override ===");

    let source = r#"
entity WidthOverrideTest {
    in addr: bit[8],
    out data: bit[32],

    // Memory with explicit width different from signal type
    #[memory(depth = 256, width = 32)]
    signal custom_mem: bit[32],
}

impl WidthOverrideTest {
    data = custom_mem
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    let entity = hir
        .entities
        .iter()
        .find(|e| e.name == "WidthOverrideTest")
        .unwrap();
    let mem_signal = entity
        .signals
        .iter()
        .find(|s| s.name == "custom_mem")
        .unwrap();

    assert!(mem_signal.memory_config.is_some());
    let mem_config = mem_signal.memory_config.as_ref().unwrap();
    assert_eq!(mem_config.depth, 256);
    assert_eq!(mem_config.width, Some(32));

    println!("Memory width override test PASSED!");
}

#[test]
fn test_memory_ultra_style() {
    println!("=== Testing Ultra Memory Style ===");

    let source = r#"
entity UltraMemTest {
    in addr: bit[12],
    out data: bit[72],

    #[memory(depth = 4096, style = ultra)]
    signal ultraram: bit[72],
}

impl UltraMemTest {
    data = ultraram
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    let entity = hir
        .entities
        .iter()
        .find(|e| e.name == "UltraMemTest")
        .unwrap();
    let mem_signal = entity
        .signals
        .iter()
        .find(|s| s.name == "ultraram")
        .unwrap();

    assert!(mem_signal.memory_config.is_some());
    let mem_config = mem_signal.memory_config.as_ref().unwrap();
    assert_eq!(mem_config.depth, 4096);
    assert_eq!(mem_config.style, MemoryStyle::Ultra);

    println!("Ultra memory style test PASSED!");
}

#[test]
fn test_memory_register_style() {
    println!("=== Testing Register Memory Style ===");

    let source = r#"
entity RegMemTest {
    in addr: bit[3],
    out data: bit[16],

    #[memory(depth = 8, style = register)]
    signal regfile: bit[16],
}

impl RegMemTest {
    data = regfile
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    let entity = hir
        .entities
        .iter()
        .find(|e| e.name == "RegMemTest")
        .unwrap();
    let mem_signal = entity.signals.iter().find(|s| s.name == "regfile").unwrap();

    assert!(mem_signal.memory_config.is_some());
    let mem_config = mem_signal.memory_config.as_ref().unwrap();
    assert_eq!(mem_config.depth, 8);
    assert_eq!(mem_config.style, MemoryStyle::Register);

    println!("Register memory style test PASSED!");
}

// =============================================================================
// LIR-Level Memory Inference Tests
// =============================================================================

use skalp_lir::{lower_mir_module_to_lir, lower_mir_module_to_lir_with_bram, LirOp};
use skalp_mir::MirCompiler;

fn compile_to_mir_module(
    source: &str,
) -> skalp_mir::mir::Module {
    use skalp_frontend::parse_and_build_hir;

    let hir = parse_and_build_hir(source).expect("HIR parsing failed");
    let compiler = MirCompiler::new();
    let mir = compiler
        .compile_to_mir(&hir)
        .expect("MIR compilation failed");
    mir.modules.into_iter().next().expect("No modules")
}

/// Verify that memory signals with style=block produce MemBlock in LIR
/// when target_has_bram is enabled
#[test]
fn test_memory_block_style_produces_memblock_lir() {
    let source = r#"
entity BramLirTest {
    in clk: clock
    in addr: bit[8]
    in wdata: bit[8]
    in we: bit
    out rdata: bit[8]

    #[memory(depth = 256, style = block)]
    signal mem: bit[8]
}

impl BramLirTest {
    on(clk.rise) {
        if we {
            mem[addr] = wdata
        }
    }
    rdata = mem[addr]
}
"#;

    let module = compile_to_mir_module(source);

    // With BRAM enabled, should produce MemBlock
    let lir_result = lower_mir_module_to_lir_with_bram(&module);
    let lir = &lir_result.lir;

    let memblock_count = lir
        .nodes
        .iter()
        .filter(|n| matches!(n.op, LirOp::MemBlock { .. }))
        .count();
    assert!(
        memblock_count > 0,
        "LIR should contain MemBlock node for style=block memory. Nodes: {:?}",
        lir.nodes.iter().map(|n| format!("{:?}", n.op)).collect::<Vec<_>>()
    );

    println!("MemBlock LIR node count: {}", memblock_count);
}

/// Verify that memory signals without BRAM target do NOT produce MemBlock
#[test]
fn test_memory_block_style_without_bram_target() {
    let source = r#"
entity NoBramTest {
    in clk: clock
    in addr: bit[4]
    in wdata: bit[8]
    in we: bit
    out rdata: bit[8]

    #[memory(depth = 16, style = block)]
    signal mem: bit[8]
}

impl NoBramTest {
    on(clk.rise) {
        if we {
            mem[addr] = wdata
        }
    }
    rdata = mem[addr]
}
"#;

    let module = compile_to_mir_module(source);

    // Without BRAM target, style=block still forces BRAM inference
    // (it's a user override — they explicitly requested block RAM)
    let lir_result = lower_mir_module_to_lir(&module);
    let lir = &lir_result.lir;

    let memblock_count = lir
        .nodes
        .iter()
        .filter(|n| matches!(n.op, LirOp::MemBlock { .. }))
        .count();
    // style=block always uses BRAM regardless of target
    assert!(
        memblock_count > 0,
        "style=block should produce MemBlock even without target BRAM"
    );
}

/// Verify that style=distributed does NOT produce MemBlock
#[test]
fn test_memory_distributed_stays_luts() {
    let source = r#"
entity DistLirTest {
    in clk: clock
    in addr: bit[4]
    in wdata: bit[8]
    in we: bit
    out rdata: bit[8]

    #[memory(depth = 16, style = distributed)]
    signal mem: bit[8]
}

impl DistLirTest {
    on(clk.rise) {
        if we {
            mem[addr] = wdata
        }
    }
    rdata = mem[addr]
}
"#;

    let module = compile_to_mir_module(source);
    let lir_result = lower_mir_module_to_lir_with_bram(&module);
    let lir = &lir_result.lir;

    let memblock_count = lir
        .nodes
        .iter()
        .filter(|n| matches!(n.op, LirOp::MemBlock { .. }))
        .count();
    assert_eq!(
        memblock_count, 0,
        "style=distributed should NOT produce MemBlock, should stay as LUT mux tree"
    );
}

/// Verify auto-inference heuristic: large memory → BRAM
#[test]
fn test_auto_inference_large_memory() {
    let source = r#"
entity AutoLargeTest {
    in clk: clock
    in addr: bit[10]
    in wdata: bit[8]
    in we: bit
    out rdata: bit[8]

    #[memory(depth = 1024)]
    signal mem: bit[8]
}

impl AutoLargeTest {
    on(clk.rise) {
        if we {
            mem[addr] = wdata
        }
    }
    rdata = mem[addr]
}
"#;

    let module = compile_to_mir_module(source);

    // With BRAM target: 1024x8 = 8192 bits > 256, depth 1024 > 16 → BRAM
    let lir_result = lower_mir_module_to_lir_with_bram(&module);
    let lir = &lir_result.lir;

    let memblock_count = lir
        .nodes
        .iter()
        .filter(|n| matches!(n.op, LirOp::MemBlock { .. }))
        .count();
    assert!(
        memblock_count > 0,
        "Auto style with 1024x8 should infer BRAM when target has BRAM"
    );
}

/// Verify auto-inference heuristic: small memory → LUTs
#[test]
fn test_auto_inference_small_memory() {
    let source = r#"
entity AutoSmallTest {
    in clk: clock
    in addr: bit[3]
    in wdata: bit[4]
    in we: bit
    out rdata: bit[4]

    #[memory(depth = 8)]
    signal mem: bit[4]
}

impl AutoSmallTest {
    on(clk.rise) {
        if we {
            mem[addr] = wdata
        }
    }
    rdata = mem[addr]
}
"#;

    let module = compile_to_mir_module(source);

    // With BRAM target: 8x4 = 32 bits < 256 → stays as LUTs
    let lir_result = lower_mir_module_to_lir_with_bram(&module);
    let lir = &lir_result.lir;

    let memblock_count = lir
        .nodes
        .iter()
        .filter(|n| matches!(n.op, LirOp::MemBlock { .. }))
        .count();
    assert_eq!(
        memblock_count, 0,
        "Auto style with 8x4 (32 bits) should NOT infer BRAM"
    );
}

/// Verify that auto-inference without BRAM target never uses BRAM
#[test]
fn test_auto_inference_no_bram_target() {
    let source = r#"
entity AutoNoBramTest {
    in clk: clock
    in addr: bit[10]
    in wdata: bit[8]
    in we: bit
    out rdata: bit[8]

    #[memory(depth = 1024)]
    signal mem: bit[8]
}

impl AutoNoBramTest {
    on(clk.rise) {
        if we {
            mem[addr] = wdata
        }
    }
    rdata = mem[addr]
}
"#;

    let module = compile_to_mir_module(source);

    // Without BRAM target, even large memories stay as LUTs for auto
    let lir_result = lower_mir_module_to_lir(&module);
    let lir = &lir_result.lir;

    let memblock_count = lir
        .nodes
        .iter()
        .filter(|n| matches!(n.op, LirOp::MemBlock { .. }))
        .count();
    assert_eq!(
        memblock_count, 0,
        "Auto style without BRAM target should NOT produce MemBlock"
    );
}
