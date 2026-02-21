//! Sanity check test for the MCU safety flow
//!
//! This test verifies the full compilation pipeline works:
//! HIR → MIR → Lir → TechMapper → GateNetlist → SIR

use indexmap::IndexMap;
use skalp_frontend::parse_and_build_hir;
use skalp_lir::{get_stdlib_library, lower_mir_module_to_lir, tech_mapper::TechMapper};
use skalp_mir::MirCompiler;
use skalp_sim::convert_gate_netlist_to_sir;
use std::fs;

#[test]
fn test_mcu_v11_full_flow() {
    let source_path = &format!(
        "{}/examples/safety/mcu/mcu_v11.sk",
        env!("CARGO_MANIFEST_DIR")
    );
    let source = fs::read_to_string(source_path).expect("Failed to read MCU source");

    println!("\n=== MCU v11 Safety Flow Sanity Check ===\n");

    // Parse to HIR
    println!("📖 Parsing to HIR...");
    let hir = parse_and_build_hir(&source).expect("Failed to parse HIR");
    println!("   Entities: {}", hir.entities.len());
    assert!(!hir.entities.is_empty(), "Should have entities");

    // Compile to MIR
    println!("🔧 Compiling to MIR...");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
        .expect("Failed to compile to MIR");
    println!("   Modules: {}", mir.modules.len());
    assert!(!mir.modules.is_empty(), "Should have modules");

    // Find the main MCU module
    let mcu_module = mir
        .modules
        .iter()
        .find(|m| m.name.contains("MCU") || m.name.contains("Mcu"))
        .or_else(|| mir.modules.last())
        .expect("Should have at least one module");

    println!("\n📦 Processing module: {}", mcu_module.name);

    // Lower to Lir
    println!("🔩 Lowering to Lir...");
    let lir_result = lower_mir_module_to_lir(mcu_module);
    println!("   Lir nodes: {}", lir_result.lir.nodes.len());
    println!("   Lir signals: {}", lir_result.lir.signals.len());
    assert!(!lir_result.lir.nodes.is_empty(), "Should have Lir nodes");

    // Tech map to GateNetlist
    println!("⚙️  Technology mapping to GateNetlist...");
    let library = get_stdlib_library("generic_asic").expect("Failed to load library");
    let mut mapper = TechMapper::new(&library);
    let tech_result = mapper.map(&lir_result.lir);
    let netlist = &tech_result.netlist;

    println!("   Gate cells: {}", netlist.cells.len());
    println!("   Gate nets: {}", netlist.nets.len());
    println!("   Total FIT: {:.4}", netlist.total_fit());
    assert!(!netlist.cells.is_empty(), "Should have gate cells");
    assert!(netlist.total_fit() > 0.0, "Should have positive FIT");

    // Show cell type distribution
    let mut cell_types: IndexMap<&str, usize> = IndexMap::new();
    for cell in &netlist.cells {
        *cell_types.entry(&cell.cell_type).or_insert(0) += 1;
    }
    println!("   Cell type distribution:");
    let mut sorted_types: Vec<_> = cell_types.iter().collect();
    sorted_types.sort_by(|a, b| b.1.cmp(a.1));
    for (cell_type, count) in sorted_types.iter().take(10) {
        println!("     {}: {}", cell_type, count);
    }

    // Convert to SIR
    println!("🔄 Converting to SIR...");
    let sir_result = convert_gate_netlist_to_sir(netlist);
    println!("   SIR signals: {}", sir_result.stats.signals_created);
    println!("   SIR primitives: {}", sir_result.stats.primitives_created);
    println!("   SIR total FIT: {:.4}", sir_result.stats.total_fit);
    assert!(
        sir_result.stats.primitives_created > 0,
        "Should have SIR primitives"
    );

    // Verify SIR structure
    let sir = &sir_result.sir;
    println!("\n📊 SIR Structure:");
    println!("   Module name: {}", sir.name);
    println!("   Signals: {}", sir.top_module.signals.len());
    println!(
        "   Combinational blocks: {}",
        sir.top_module.comb_blocks.len()
    );
    println!("   Sequential blocks: {}", sir.top_module.seq_blocks.len());

    println!("\n✅ MCU v11 flow completed successfully!");
}
