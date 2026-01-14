// Debug test for boundary-only NCL expansion
// This test isolates the boundary-only NCL logic to find the issue

use indexmap::IndexMap;
use skalp_frontend::parse_and_build_hir;
use skalp_lir::{
    expand_to_ncl, get_stdlib_library, lower_mir_module_to_lir_skip_ncl, map_lir_to_gates,
    NclConfig,
};
use skalp_mir::MirCompiler;
use skalp_sim::{CircuitMode, HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};

const SIMPLE_PASSTHROUGH: &str = r#"
async entity SimplePass {
    in a: bit[8]
    out result: bit[8]
}

impl SimplePass {
    result = a
}
"#;

#[test]
fn test_boundary_ncl_lir_expansion() {
    println!("\n=== Test Boundary-Only NCL LIR Expansion ===\n");

    let hir = parse_and_build_hir(SIMPLE_PASSTHROUGH).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
        .expect("Failed to compile to MIR");

    // Get the single-rail LIR first (without NCL expansion)
    // Find the first async module
    let module = mir.modules.first().expect("No modules in MIR");
    let lir_result = lower_mir_module_to_lir_skip_ncl(module);
    let lir = &lir_result.lir;

    println!("Original LIR:");
    println!("  Name: {}", lir.name);
    println!("  Inputs: {:?}", lir.inputs);
    println!("  Outputs: {:?}", lir.outputs);
    println!("  Signals: {}", lir.signals.len());
    println!("  Nodes: {}", lir.nodes.len());

    for (i, sig) in lir.signals.iter().enumerate() {
        println!(
            "    Signal[{}]: {} (width={}, input={}, output={})",
            i, sig.name, sig.width, sig.is_input, sig.is_output
        );
    }

    for (i, node) in lir.nodes.iter().enumerate() {
        println!(
            "    Node[{}]: {:?} inputs={:?} output={:?} path={}",
            i, node.op, node.inputs, node.output, node.path
        );
    }

    // Test with boundary_only = true
    let config = NclConfig {
        boundary_only: true,
        use_weak_completion: true,
        completion_tree_depth: None,
        generate_null_wavefront: true,
        use_opaque_arithmetic: true,
    };

    println!("\nExpanding to NCL with boundary_only=true...");
    let ncl_result = expand_to_ncl(lir, &config);

    println!("\nBoundary NCL LIR:");
    println!("  Name: {}", ncl_result.lir.name);
    println!("  Inputs: {:?}", ncl_result.lir.inputs);
    println!("  Outputs: {:?}", ncl_result.lir.outputs);
    println!("  Signals: {}", ncl_result.lir.signals.len());
    println!("  Nodes: {}", ncl_result.lir.nodes.len());

    for (i, sig) in ncl_result.lir.signals.iter().enumerate() {
        println!(
            "    Signal[{}]: {} (width={}, input={}, output={})",
            i, sig.name, sig.width, sig.is_input, sig.is_output
        );
    }

    for (i, node) in ncl_result.lir.nodes.iter().enumerate() {
        println!(
            "    Node[{}]: {:?} inputs={:?} output={:?} path={}",
            i, node.op, node.inputs, node.output, node.path
        );
    }

    // Verify signal IDs are valid
    println!("\nValidating signal references...");
    let max_signal_id = ncl_result.lir.signals.len() as u32;

    for (i, node) in ncl_result.lir.nodes.iter().enumerate() {
        for &input_id in &node.inputs {
            if input_id.0 >= max_signal_id {
                panic!(
                    "Node[{}] has invalid input signal ID {}, max is {}",
                    i,
                    input_id.0,
                    max_signal_id - 1
                );
            }
        }
        if node.output.0 >= max_signal_id {
            panic!(
                "Node[{}] has invalid output signal ID {}, max is {}",
                i,
                node.output.0,
                max_signal_id - 1
            );
        }
    }

    println!("All signal references are valid!");
    println!("\n=== Test Passed ===");
}

#[test]
fn test_boundary_ncl_gate_level() {
    println!("\n=== Test Boundary-Only NCL Gate-Level Simulation ===\n");

    let hir = parse_and_build_hir(SIMPLE_PASSTHROUGH).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
        .expect("Failed to compile to MIR");

    // Get the single-rail LIR first (without NCL expansion)
    let module = mir.modules.first().expect("No modules in MIR");
    let lir_result = lower_mir_module_to_lir_skip_ncl(module);
    let lir = &lir_result.lir;

    // Apply boundary-only NCL expansion
    let config = NclConfig {
        boundary_only: true, // Now test boundary-only
        use_weak_completion: true,
        completion_tree_depth: None,
        generate_null_wavefront: true,
        use_opaque_arithmetic: true,
    };

    println!("Step 1: Expanding to boundary-only NCL...");
    let ncl_result = expand_to_ncl(lir, &config);
    println!(
        "  NCL LIR: {} signals, {} nodes",
        ncl_result.lir.signals.len(),
        ncl_result.lir.nodes.len()
    );

    // Map to gates
    println!("\nStep 2: Tech mapping to gates...");
    let library = get_stdlib_library("generic_asic").expect("Failed to get library");
    let result = map_lir_to_gates(&ncl_result.lir, &library);
    let netlist = result.netlist;
    println!(
        "  Gate netlist: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    // Print cell types
    let mut cell_types: IndexMap<String, usize> = IndexMap::new();
    for cell in &netlist.cells {
        *cell_types.entry(cell.cell_type.clone()).or_insert(0) += 1;
    }
    println!("  Cell types:");
    for (cell_type, count) in &cell_types {
        println!("    {}: {}", cell_type, count);
    }

    // Print first few cells for debugging
    println!("\n  First 10 cells:");
    for (i, cell) in netlist.cells.iter().take(10).enumerate() {
        println!(
            "    Cell[{}]: {} '{}' inputs={:?} outputs={:?}",
            i, cell.cell_type, cell.path, cell.inputs, cell.outputs
        );
    }

    // Print net names for debugging
    println!("\n  Input nets:");
    for net in &netlist.nets {
        if net.is_input {
            println!("    {} (id={})", net.name, net.id.0);
        }
    }
    println!("  Output nets:");
    for net in &netlist.nets {
        if net.is_output {
            println!("    {} (id={})", net.name, net.id.0);
        }
    }

    // Simulate
    println!("\nStep 3: Simulating...");
    let sim_config = UnifiedSimConfig {
        level: SimLevel::GateLevel,
        circuit_mode: CircuitMode::Ncl,
        hw_accel: HwAccel::Cpu, // Use CPU for debugging
        max_iterations: 100,    // Low limit for debugging
        ncl_debug: true,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(sim_config).expect("Failed to create simulator");
    eprintln!("  [TEST] is_ncl_mode before load: {}", sim.is_ncl_mode());

    sim.load_ncl_gate_level(netlist)
        .expect("Failed to load NCL netlist");

    eprintln!("  [TEST] is_ncl_mode after load: {}", sim.is_ncl_mode());

    // Debug: check what input net names are available
    eprintln!("\n  [TEST] About to set input...");

    // Set input: a = 0x42 (0b01000010)
    // For dual-rail: bit0=0 -> t0=0,f0=1; bit1=1 -> t1=1,f1=0; etc.
    eprintln!("  [TEST] Setting input a = 0x42");
    sim.set_ncl_input("a", 0x42, 8);
    eprintln!("  [TEST] Done setting input");

    // Debug: check a single iteration to see what happens
    eprintln!("  [TEST] Running single step...");
    sim.step();
    eprintln!("  [TEST] Step complete");

    println!("  Running simulation...");
    let result = sim.run_until_stable();
    println!(
        "  Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );

    // Get output
    match sim.get_ncl_output("result", 8) {
        Some(value) => {
            println!("  Result: 0x{:02X} (expected 0x42)", value);
            assert_eq!(value, 0x42, "Passthrough should preserve value");
        }
        None => {
            println!("  Result: NULL");
            panic!("Result should not be NULL");
        }
    }

    println!("\n=== Test Passed ===");
}
