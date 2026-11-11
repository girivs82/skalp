//! The gate netlist the synthesizer writes has one cell per mapped AIG node.
//!
//! A node a consumer needs before the node's turn in the writer's
//! topological order is written on demand; it must not be written a second
//! time when its turn comes. The second copy was an identical LUT on a second
//! net of the same name, and flattening a hierarchy merges nets by name, so
//! the flattened netlist had nets with two drivers.

use skalp_lir::GateNetlist;

fn synthesize(fixture: &str, flat: bool) -> GateNetlist {
    let path = std::path::PathBuf::from(format!(
        "{}/tests/fixtures/{fixture}",
        env!("CARGO_MANIFEST_DIR")
    ));
    let ctx = skalp_frontend::parse_and_build_compilation_context(&path).unwrap();
    let mir = skalp_mir::MirCompiler::new()
        .with_optimization_level(skalp_mir::OptimizationLevel::None)
        .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
        .unwrap();
    let (hier, _) = skalp_lir::lower_mir_hierarchical_for_optimize_first(&mir);
    let library = skalp_lir::get_stdlib_library("nexus").unwrap();
    let preset = skalp_lir::synth::SynthPreset::Full;
    if flat {
        skalp_lir::synthesize(&hier.flatten(), &library, preset).netlist
    } else {
        skalp_lir::synthesize_hierarchical(&hier, &library, preset).flatten_for_library(&library)
    }
}

/// LUT cells with the same type, inputs and truth table
fn duplicate_luts(netlist: &GateNetlist) -> usize {
    let mut seen = std::collections::HashMap::new();
    for cell in &netlist.cells {
        if cell.lut_init.is_some() && !cell.inputs.is_empty() {
            *seen
                .entry((cell.cell_type.clone(), cell.inputs.clone(), cell.lut_init))
                .or_insert(0usize) += 1;
        }
    }
    seen.values().map(|&n| n - 1).sum()
}

/// A design whose writer order needs some nodes early (clock-enable
/// extraction): each node is written once, flat or hierarchical, and no net
/// has two drivers.
#[test]
fn each_node_is_written_once() {
    for flat in [false, true] {
        let netlist = synthesize("clock_enable_check.sk", flat);
        assert_eq!(duplicate_luts(&netlist), 0, "flat {flat}");
        assert!(
            netlist.multiply_driven_nets().is_empty(),
            "flat {flat}: nets with two drivers {:?}",
            netlist
                .multiply_driven_nets()
                .iter()
                .map(|n| &netlist.nets[n.0 as usize].name)
                .collect::<Vec<_>>()
        );
    }
}
