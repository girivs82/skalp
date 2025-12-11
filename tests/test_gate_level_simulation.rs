//! Integration test for gate-level simulation pipeline
//!
//! This test demonstrates the complete flow:
//! 1. Parse Skalp source code
//! 2. Build HIR
//! 3. Lower to MIR
//! 4. Transform to GateNetlist (primitive-based)
//! 5. Convert to SIR (structural mode)
//! 6. Evaluate with fault injection

use skalp_frontend::parse_and_build_hir;
use skalp_lir::{
    lower_to_gate_netlist, GateOptimizationPipeline, MirToGateNetlistResult,
    PrimitiveId, PrimitiveType,
};
use skalp_mir::MirCompiler;
use skalp_sim::{
    convert_gate_netlist_to_sir,
    gate_eval::{evaluate_primitive, evaluate_primitive_with_fault},
    sir::FaultInjectionConfig,
    GateLevelSimulator,
};

/// Helper to compile Skalp source to GateNetlist
fn compile_to_gate_netlist(source: &str) -> Vec<MirToGateNetlistResult> {
    let hir = parse_and_build_hir(source).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler.compile(&hir).expect("Failed to compile to MIR");
    lower_to_gate_netlist(&mir).expect("Failed to lower to GateNetlist")
}

#[test]
fn test_simple_and_gate_pipeline() {
    let source = r#"
        entity AndGate<'clk> {
            in a: bit,
            in b: bit,
            out y: bit,
        }

        impl AndGate<'clk> {
            y = a & b;
        }
    "#;

    let results = compile_to_gate_netlist(source);
    assert!(!results.is_empty(), "Should produce at least one netlist");

    let result = &results[0];
    println!("=== AND Gate Pipeline ===");
    println!("Module: {}", result.netlist.name);
    println!("Ports: {}", result.stats.ports);
    println!("Primitives: {}", result.netlist.primitives.len());
    println!("Nets: {}", result.netlist.nets.len());

    // Check for AND primitive
    let and_prims: Vec<_> = result
        .netlist
        .primitives
        .iter()
        .filter(|p| matches!(p.ptype, PrimitiveType::And { .. }))
        .collect();
    println!("AND primitives: {}", and_prims.len());

    // Print all primitives
    for prim in &result.netlist.primitives {
        println!(
            "  {:?} {} inputs={:?} outputs={:?}",
            prim.ptype, prim.path, prim.inputs, prim.outputs
        );
    }

    assert!(and_prims.len() >= 1, "Should have at least one AND gate");
}

#[test]
fn test_xor_gate_pipeline() {
    let source = r#"
        entity XorGate<'clk> {
            in a: bit,
            in b: bit,
            out y: bit,
        }

        impl XorGate<'clk> {
            y = a ^ b;
        }
    "#;

    let results = compile_to_gate_netlist(source);
    let result = &results[0];

    println!("=== XOR Gate Pipeline ===");
    println!("Primitives: {}", result.netlist.primitives.len());

    let xor_prims: Vec<_> = result
        .netlist
        .primitives
        .iter()
        .filter(|p| matches!(p.ptype, PrimitiveType::Xor))
        .collect();

    for prim in &result.netlist.primitives {
        println!("  {:?} {}", prim.ptype, prim.path);
    }

    assert!(xor_prims.len() >= 1, "Should have at least one XOR gate");
}

#[test]
fn test_mux_pipeline() {
    let source = r#"
        entity Mux2<'clk> {
            in sel: bit,
            in a: bit,
            in b: bit,
            out y: bit,
        }

        impl Mux2<'clk> {
            y = if sel { a } else { b };
        }
    "#;

    let results = compile_to_gate_netlist(source);
    let result = &results[0];

    println!("=== MUX Pipeline ===");
    println!("Primitives: {}", result.netlist.primitives.len());

    for prim in &result.netlist.primitives {
        println!("  {:?} {}", prim.ptype, prim.path);
    }

    let mux_prims: Vec<_> = result
        .netlist
        .primitives
        .iter()
        .filter(|p| matches!(p.ptype, PrimitiveType::Mux2))
        .collect();

    assert!(mux_prims.len() >= 1, "Should have at least one MUX2");
}

#[test]
fn test_multi_bit_adder_pipeline() {
    let source = r#"
        entity Adder4<'clk> {
            in a: bit[4],
            in b: bit[4],
            out sum: bit[4],
        }

        impl Adder4<'clk> {
            sum = a + b;
        }
    "#;

    let results = compile_to_gate_netlist(source);
    let result = &results[0];

    println!("=== 4-bit Adder Pipeline ===");
    println!("Total bits: {}", result.stats.total_bits);
    println!("Primitives: {}", result.netlist.primitives.len());

    // Count adder primitives
    let half_adders: Vec<_> = result
        .netlist
        .primitives
        .iter()
        .filter(|p| matches!(p.ptype, PrimitiveType::HalfAdder))
        .collect();

    let full_adders: Vec<_> = result
        .netlist
        .primitives
        .iter()
        .filter(|p| matches!(p.ptype, PrimitiveType::FullAdder))
        .collect();

    println!("HalfAdders: {}", half_adders.len());
    println!("FullAdders: {}", full_adders.len());

    for prim in &result.netlist.primitives {
        if matches!(
            prim.ptype,
            PrimitiveType::HalfAdder | PrimitiveType::FullAdder
        ) {
            println!("  {:?} {}", prim.ptype, prim.path);
        }
    }

    // 4-bit adder should have 1 HalfAdder + 3 FullAdders
    assert!(
        half_adders.len() + full_adders.len() >= 4,
        "4-bit adder should have at least 4 adder primitives"
    );
}

#[test]
fn test_comparator_pipeline() {
    let source = r#"
        entity Comparator<'clk> {
            in a: bit[4],
            in b: bit[4],
            out eq: bit,
        }

        impl Comparator<'clk> {
            eq = if a == b { 1 } else { 0 };
        }
    "#;

    let results = compile_to_gate_netlist(source);
    let result = &results[0];

    println!("=== 4-bit Comparator Pipeline ===");
    println!("Primitives: {}", result.netlist.primitives.len());

    // Equality comparison uses XNOR for each bit pair, then AND to combine
    let xnor_prims: Vec<_> = result
        .netlist
        .primitives
        .iter()
        .filter(|p| matches!(p.ptype, PrimitiveType::Xnor))
        .collect();

    println!("XNOR primitives: {}", xnor_prims.len());

    for prim in &result.netlist.primitives {
        println!("  {:?} {}", prim.ptype, prim.path);
    }
}

#[test]
fn test_fit_calculation() {
    let source = r#"
        entity FitTest<'clk> {
            in a: bit[8],
            in b: bit[8],
            out y: bit[8],
        }

        impl FitTest<'clk> {
            y = a & b;
        }
    "#;

    let results = compile_to_gate_netlist(source);
    let result = &results[0];

    println!("=== FIT Calculation ===");
    println!("Primitives: {}", result.netlist.primitives.len());

    // Calculate total FIT
    let total_fit: f64 = result
        .netlist
        .primitives
        .iter()
        .map(|p| p.ptype.base_fit())
        .sum();

    println!("Total FIT: {:.2}", total_fit);

    // Print FIT breakdown by primitive type
    let mut fit_by_type: std::collections::HashMap<String, (usize, f64)> =
        std::collections::HashMap::new();
    for prim in &result.netlist.primitives {
        let type_name = format!("{:?}", prim.ptype);
        let entry = fit_by_type.entry(type_name).or_insert((0, 0.0));
        entry.0 += 1;
        entry.1 += prim.ptype.base_fit();
    }

    for (ptype, (count, fit)) in &fit_by_type {
        println!("  {}: {} primitives, {:.2} FIT", ptype, count, fit);
    }

    assert!(total_fit > 0.0, "Should have positive total FIT");
}

#[test]
fn test_gate_optimization_pipeline() {
    let source = r#"
        entity OptTest<'clk> {
            in a: bit,
            in b: bit,
            out y: bit,
        }

        impl OptTest<'clk> {
            y = a & b;
        }
    "#;

    let results = compile_to_gate_netlist(source);
    let mut netlist = results[0].netlist.clone();

    println!("=== Gate Optimization ===");
    println!("Before optimization:");
    println!("  Primitives: {}", netlist.primitives.len());
    println!("  Nets: {}", netlist.nets.len());

    // Run optimization pipeline
    let mut pipeline = GateOptimizationPipeline::for_safety_analysis();
    let opt_results = pipeline.optimize(&mut netlist);

    println!("\nOptimization passes:");
    for result in &opt_results {
        println!(
            "  {}: {} -> {} primitives",
            result.pass_name, result.primitives_before, result.primitives_after
        );
    }

    println!("\nAfter optimization:");
    println!("  Primitives: {}", netlist.primitives.len());
    println!("  Nets: {}", netlist.nets.len());
}

#[test]
fn test_sir_conversion() {
    let source = r#"
        entity SirTest<'clk> {
            in a: bit,
            in b: bit,
            out y: bit,
        }

        impl SirTest<'clk> {
            y = a | b;
        }
    "#;

    let results = compile_to_gate_netlist(source);
    let netlist = &results[0].netlist;

    println!("=== SIR Conversion ===");
    println!("GateNetlist primitives: {}", netlist.primitives.len());

    // Convert to SIR
    let sir_result = convert_gate_netlist_to_sir(netlist);

    println!("SIR signals: {}", sir_result.sir.top_module.signals.len());
    println!(
        "SIR comb_blocks: {}",
        sir_result.sir.top_module.comb_blocks.len()
    );
    println!(
        "SIR seq_blocks: {}",
        sir_result.sir.top_module.seq_blocks.len()
    );
    println!("Total FIT: {:.2}", sir_result.total_fit);

    // Check structural info
    for (i, block) in sir_result.sir.top_module.comb_blocks.iter().enumerate() {
        if let Some(info) = &block.structural_info {
            println!(
                "CombBlock {}: structural={}, ops={}, fit={:.2}",
                i,
                info.is_structural,
                block.operations.len(),
                info.total_fit
            );
        }
    }
}

#[test]
fn test_fault_injection_simulation() {
    println!("=== Fault Injection Simulation ===");

    // Test AND gate behavior with bool inputs
    let inputs_and = vec![true, true]; // a=1, b=1
    let result_normal = evaluate_primitive(&PrimitiveType::And { inputs: 2 }, &inputs_and);
    println!("AND(1,1) normal: {:?}", result_normal);
    assert_eq!(result_normal, vec![true], "AND(1,1) should be 1");

    // Test with stuck-at-0 fault
    let fault_sa0 = FaultInjectionConfig::stuck_at_0(
        PrimitiveId(0),
        0, // output bit 0
    );
    let result_sa0 = evaluate_primitive_with_fault(
        &PrimitiveType::And { inputs: 2 },
        &inputs_and,
        Some(&fault_sa0),
        0, // current cycle
    );
    println!("AND(1,1) with SA0 fault: {:?}", result_sa0);
    assert_eq!(result_sa0, vec![false], "SA0 fault should force output to 0");

    // Test with stuck-at-1 fault
    let inputs_and_00 = vec![false, false]; // a=0, b=0
    let fault_sa1 = FaultInjectionConfig::stuck_at_1(
        PrimitiveId(0),
        0, // output bit 0
    );
    let result_sa1 = evaluate_primitive_with_fault(
        &PrimitiveType::And { inputs: 2 },
        &inputs_and_00,
        Some(&fault_sa1),
        0, // current cycle
    );
    println!("AND(0,0) with SA1 fault: {:?}", result_sa1);
    assert_eq!(result_sa1, vec![true], "SA1 fault should force output to 1");

    // Test XOR gate
    let inputs_xor = vec![true, false];
    let result_xor = evaluate_primitive(&PrimitiveType::Xor, &inputs_xor);
    println!("XOR(1,0) normal: {:?}", result_xor);
    assert_eq!(result_xor, vec![true], "XOR(1,0) should be 1");

    // Test MUX2
    let inputs_mux = vec![true, false, true]; // sel=1, d0=0, d1=1
    let result_mux = evaluate_primitive(&PrimitiveType::Mux2, &inputs_mux);
    println!("MUX2(sel=1, d0=0, d1=1): {:?}", result_mux);
    assert_eq!(result_mux, vec![true], "MUX2 with sel=1 should select d1");

    // Test HalfAdder
    let inputs_ha = vec![true, true]; // a=1, b=1
    let result_ha = evaluate_primitive(&PrimitiveType::HalfAdder, &inputs_ha);
    println!(
        "HalfAdder(1,1): sum={}, cout={}",
        result_ha[0], result_ha[1]
    );
    assert_eq!(result_ha, vec![false, true], "1+1 = 0 with carry 1");

    // Test FullAdder
    let inputs_fa = vec![true, true, true]; // a=1, b=1, cin=1
    let result_fa = evaluate_primitive(&PrimitiveType::FullAdder, &inputs_fa);
    println!(
        "FullAdder(1,1,1): sum={}, cout={}",
        result_fa[0], result_fa[1]
    );
    assert_eq!(result_fa, vec![true, true], "1+1+1 = 1 with carry 1");

    println!("\nAll fault injection tests passed!");
}

#[test]
fn test_complete_pipeline_with_optimization_and_sir() {
    let source = r#"
        entity FullPipeline<'clk> {
            in a: bit[4],
            in b: bit[4],
            in sel: bit,
            out sum: bit[4],
            out muxed: bit[4],
        }

        impl FullPipeline<'clk> {
            sum = a + b;
            muxed = if sel { a } else { b };
        }
    "#;

    println!("=== Complete Pipeline Test ===");

    // Step 1: Compile to GateNetlist
    let results = compile_to_gate_netlist(source);
    let mut netlist = results[0].netlist.clone();

    println!("\n1. Initial GateNetlist:");
    println!("   Primitives: {}", netlist.primitives.len());
    println!("   Nets: {}", netlist.nets.len());

    let initial_fit: f64 = netlist.primitives.iter().map(|p| p.ptype.base_fit()).sum();
    println!("   Total FIT: {:.2}", initial_fit);

    // Step 2: Optimize
    let mut pipeline = GateOptimizationPipeline::for_safety_analysis();
    let _opt_results = pipeline.optimize(&mut netlist);

    println!("\n2. After Optimization:");
    println!("   Primitives: {}", netlist.primitives.len());
    println!("   Nets: {}", netlist.nets.len());

    let optimized_fit: f64 = netlist.primitives.iter().map(|p| p.ptype.base_fit()).sum();
    println!("   Total FIT: {:.2}", optimized_fit);

    // Step 3: Convert to SIR
    let sir_result = convert_gate_netlist_to_sir(&netlist);

    println!("\n3. SIR Representation:");
    println!("   Signals: {}", sir_result.sir.top_module.signals.len());
    println!(
        "   Combinational blocks: {}",
        sir_result.sir.top_module.comb_blocks.len()
    );
    println!(
        "   Sequential blocks: {}",
        sir_result.sir.top_module.seq_blocks.len()
    );

    // Step 4: Analyze primitives by type
    println!("\n4. Primitive Breakdown:");
    let mut counts: std::collections::HashMap<String, usize> = std::collections::HashMap::new();
    for prim in &netlist.primitives {
        *counts
            .entry(prim.ptype.short_name().to_string())
            .or_insert(0) += 1;
    }
    for (ptype, count) in counts.iter() {
        println!("   {}: {}", ptype, count);
    }

    println!("\nPipeline test complete!");
}

#[test]
fn test_hierarchy_preservation() {
    let source = r#"
        entity HierTest<'clk> {
            in a: bit[8],
            in b: bit[8],
            out y: bit[8],
        }

        impl HierTest<'clk> {
            y = (a & b) | (a ^ b);
        }
    "#;

    let results = compile_to_gate_netlist(source);
    let netlist = &results[0].netlist;

    println!("=== Hierarchy Preservation ===");
    println!("Module: {}", netlist.name);

    // Check hierarchy nodes
    for node in &netlist.hierarchy {
        println!(
            "Hierarchy: {} (module: {}, primitives: {:?})",
            node.path, node.module, node.primitive_range
        );
    }

    // Check primitive paths for traceability
    println!("\nPrimitive paths (sample):");
    for prim in netlist.primitives.iter().take(5) {
        println!("  {} -> {:?}", prim.path, prim.ptype);
    }

    assert!(!netlist.hierarchy.is_empty(), "Should have hierarchy info");
}

#[test]
fn test_gate_level_simulator() {
    let source = r#"
        entity XorGate<'clk> {
            in a: bit,
            in b: bit,
            out y: bit,
        }

        impl XorGate<'clk> {
            y = a ^ b;
        }
    "#;

    let results = compile_to_gate_netlist(source);
    let netlist = &results[0].netlist;

    // Convert to SIR
    let sir_result = convert_gate_netlist_to_sir(netlist);

    // Create simulator
    let mut sim = GateLevelSimulator::new(&sir_result.sir);

    println!("=== Gate-Level Simulator Test ===");
    println!("Primitive count: {}", sim.primitive_count());
    println!("Total FIT: {:.2}", sim.total_fit());

    // Test XOR truth table
    let test_cases = [
        (false, false, false), // 0 ^ 0 = 0
        (false, true, true),   // 0 ^ 1 = 1
        (true, false, true),   // 1 ^ 0 = 1
        (true, true, false),   // 1 ^ 1 = 0
    ];

    for (a, b, expected) in test_cases {
        sim.set_input("a", &[a]);
        sim.set_input("b", &[b]);
        sim.step();

        if let Some(y) = sim.get_output("y") {
            let result = y.first().copied().unwrap_or(false);
            println!("XOR({}, {}) = {} (expected {})", a, b, result, expected);
            assert_eq!(result, expected, "XOR gate mismatch");
        }
    }

    println!("\nGate-level simulator test passed!");
}

#[test]
fn test_gate_level_simulator_with_fault() {
    let source = r#"
        entity AndGate<'clk> {
            in a: bit,
            in b: bit,
            out y: bit,
        }

        impl AndGate<'clk> {
            y = a & b;
        }
    "#;

    let results = compile_to_gate_netlist(source);
    let netlist = &results[0].netlist;
    let sir_result = convert_gate_netlist_to_sir(netlist);
    let mut sim = GateLevelSimulator::new(&sir_result.sir);

    println!("=== Fault Injection Test ===");

    // Normal operation: AND(1,1) = 1
    sim.set_input("a", &[true]);
    sim.set_input("b", &[true]);
    sim.step();
    let normal_result = sim.get_output("y").and_then(|v| v.first().copied());
    println!("Normal: AND(1,1) = {:?}", normal_result);
    assert_eq!(normal_result, Some(true));

    // Inject SA0 fault on first primitive
    sim.inject_fault(FaultInjectionConfig::stuck_at_0(PrimitiveId(0), 0));
    sim.step();
    let faulty_result = sim.get_output("y").and_then(|v| v.first().copied());
    println!("Faulty (SA0): AND(1,1) = {:?}", faulty_result);
    assert_eq!(faulty_result, Some(false), "SA0 should force output to 0");

    // Clear fault and verify normal operation
    sim.clear_fault();
    sim.step();
    let recovered = sim.get_output("y").and_then(|v| v.first().copied());
    println!("Recovered: AND(1,1) = {:?}", recovered);
    assert_eq!(recovered, Some(true));

    println!("\nFault injection test passed!");
}

#[test]
fn test_gate_level_analysis_alu() {
    // A more substantial design: simple ALU with add/sub/and/or/xor
    let source = r#"
        entity SimpleAlu<'clk> {
            in clk: clock,
            in op: bit[3],
            in a: bit[8],
            in b: bit[8],
            out result: bit[8],
            out zero: bit,
        }

        impl SimpleAlu<'clk> {
            signal alu_result: bit[8];

            alu_result = match op {
                0b000 => a + b,
                0b001 => a - b,
                0b010 => a & b,
                0b011 => a | b,
                0b100 => a ^ b,
                0b101 => !a,
                _ => 0
            };

            result = alu_result;
            zero = alu_result == 0;
        }
    "#;

    let results = compile_to_gate_netlist(source);
    assert!(!results.is_empty(), "Should produce at least one netlist");

    let netlist = &results[0].netlist;
    let stats = &results[0].stats;

    println!("=== Gate-Level ALU Analysis ===");
    println!("Module: {}", netlist.name);
    println!("\n--- Statistics ---");
    println!("Ports: {}", stats.ports);
    println!("Total primitives: {}", netlist.primitives.len());
    println!("Total nets: {}", netlist.nets.len());

    // Primitive breakdown
    let mut counts: std::collections::HashMap<String, usize> = std::collections::HashMap::new();
    let mut total_fit = 0.0f64;
    for prim in &netlist.primitives {
        *counts.entry(prim.ptype.short_name().to_string()).or_insert(0) += 1;
        total_fit += prim.ptype.base_fit();
    }

    println!("\n--- Primitive Breakdown ---");
    let mut sorted: Vec<_> = counts.into_iter().collect();
    sorted.sort_by(|a, b| b.1.cmp(&a.1));
    for (ptype, count) in &sorted {
        println!("  {:20} : {:5}", ptype, count);
    }

    println!("\n--- Reliability ---");
    println!("Total FIT: {:.2}", total_fit);

    // Convert to SIR and create simulator
    let sir_result = convert_gate_netlist_to_sir(netlist);
    let sim = GateLevelSimulator::new(&sir_result.sir);

    println!("\n--- Simulator Ready ---");
    println!("Primitives in SIR: {}", sim.primitive_count());
    println!("SIR Total FIT: {:.2}", sim.total_fit());

    assert!(netlist.primitives.len() > 10, "ALU should have many primitives");
}

#[test]
fn test_gate_level_analysis_pipeline() {
    // A more complex design with pipeline registers
    let source = r#"
        entity Pipeline<'clk> {
            in clk: clock,
            in valid_in: bit,
            in data_in: bit[16],
            out valid_out: bit,
            out data_out: bit[16],
        }

        impl Pipeline<'clk> {
            // Stage 1: Input register
            signal stage1_valid: bit;
            signal stage1_data: bit[16];

            on(clk.rise) {
                stage1_valid <= valid_in;
                stage1_data <= data_in;
            }

            // Stage 2: Processing (add 1, multiply by 2)
            signal stage2_valid: bit;
            signal stage2_data: bit[16];
            signal processed: bit[16];

            processed = (stage1_data + 1) << 1;

            on(clk.rise) {
                stage2_valid <= stage1_valid;
                stage2_data <= processed;
            }

            // Stage 3: Output register
            signal stage3_valid: bit;
            signal stage3_data: bit[16];

            on(clk.rise) {
                stage3_valid <= stage2_valid;
                stage3_data <= stage2_data;
            }

            valid_out = stage3_valid;
            data_out = stage3_data;
        }
    "#;

    let results = compile_to_gate_netlist(source);
    assert!(!results.is_empty(), "Should produce at least one netlist");

    let netlist = &results[0].netlist;

    println!("=== Gate-Level Pipeline Analysis ===");
    println!("Module: {}", netlist.name);
    println!("Primitives: {}", netlist.primitives.len());
    println!("Nets: {}", netlist.nets.len());

    // Count sequential vs combinational
    let seq_count = netlist.primitives.iter()
        .filter(|p| p.ptype.is_sequential())
        .count();
    let comb_count = netlist.primitives.len() - seq_count;

    println!("\nSequential primitives: {} (flip-flops)", seq_count);
    println!("Combinational primitives: {}", comb_count);

    // FIT breakdown
    let seq_fit: f64 = netlist.primitives.iter()
        .filter(|p| p.ptype.is_sequential())
        .map(|p| p.ptype.base_fit())
        .sum();
    let comb_fit: f64 = netlist.primitives.iter()
        .filter(|p| !p.ptype.is_sequential())
        .map(|p| p.ptype.base_fit())
        .sum();

    println!("\nSequential FIT: {:.2}", seq_fit);
    println!("Combinational FIT: {:.2}", comb_fit);
    println!("Total FIT: {:.2}", seq_fit + comb_fit);

    // Note: Sequential logic (DFFs) generation in mir_to_gate_netlist is still WIP
    // For now, just verify we get some primitives from the combinational logic
    println!("\nNote: DFF generation is still WIP in mir_to_gate_netlist");
    assert!(netlist.primitives.len() > 0, "Should have some primitives");
}

#[cfg(target_os = "macos")]
#[test]
fn test_gpu_fault_campaign_on_alu() {
    use skalp_sim::{GpuFaultSimulator, GpuFaultCampaignConfig};

    let source = r#"
        entity FaultTestAlu<'clk> {
            in a: bit[4],
            in b: bit[4],
            out sum: bit[4],
            out carry: bit,
        }

        impl FaultTestAlu<'clk> {
            // Simple 4-bit adder without complex casts
            sum = a + b;
            carry = (a[3] & b[3]) | ((a[3] | b[3]) & !sum[3]);
        }
    "#;

    let results = compile_to_gate_netlist(source);
    let netlist = &results[0].netlist;
    let sir_result = convert_gate_netlist_to_sir(netlist);

    println!("=== GPU Fault Campaign on ALU ===");
    println!("Primitives: {}", netlist.primitives.len());

    let gpu_sim = GpuFaultSimulator::new(&sir_result.sir).expect("Failed to create GPU simulator");
    println!("GPU Device: {}", gpu_sim.device_info());

    let config = GpuFaultCampaignConfig {
        cycles_per_fault: 10,
        fault_types: vec![
            skalp_sim::sir::FaultType::StuckAt0,
            skalp_sim::sir::FaultType::StuckAt1,
        ],
        ..Default::default()
    };

    let results = gpu_sim.run_fault_campaign(&config);

    println!("\n--- Fault Campaign Results ---");
    println!("Total faults tested: {}", results.total_faults);
    println!("Detected faults: {}", results.detected_faults);
    println!("Corruption faults: {}", results.corruption_faults);
    println!("Diagnostic Coverage: {:.2}%", results.diagnostic_coverage);
}
