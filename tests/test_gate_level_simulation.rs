//! Integration test for gate-level simulation pipeline
//!
//! This test demonstrates the complete flow:
//! 1. Parse Skalp source code
//! 2. Build HIR
//! 3. Lower to MIR
//! 4. Transform to Lir (primitive-based)
//! 5. Convert to SIR (structural mode)
//! 6. Evaluate with fault injection

use skalp_frontend::parse_and_build_hir;
use skalp_lir::{
    lower_to_lir, GateOptimizationPipeline, MirToLirResult, PrimitiveId, PrimitiveType,
};
use skalp_mir::MirCompiler;
use skalp_sim::{
    convert_lir_to_sir,
    gate_eval::{evaluate_primitive, evaluate_primitive_with_fault},
    sir::FaultInjectionConfig,
    GateLevelSimulator,
};

/// Helper to compile Skalp source to Lir
fn compile_to_gate_netlist(source: &str) -> Vec<MirToLirResult> {
    let hir = parse_and_build_hir(source).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
        .expect("Failed to compile to MIR");
    lower_to_lir(&mir).expect("Failed to lower to Lir")
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
    println!("Module: {}", result.lir.name);
    println!("Ports: {}", result.stats.ports);
    println!("Primitives: {}", result.lir.primitives.len());
    println!("Nets: {}", result.lir.nets.len());

    // Check for AND primitive
    let and_prims: Vec<_> = result
        .lir
        .primitives
        .iter()
        .filter(|p| matches!(p.ptype, PrimitiveType::And { .. }))
        .collect();
    println!("AND primitives: {}", and_prims.len());

    // Print all primitives
    for prim in &result.lir.primitives {
        println!(
            "  {:?} {} inputs={:?} outputs={:?}",
            prim.ptype, prim.path, prim.inputs, prim.outputs
        );
    }

    assert!(!and_prims.is_empty(), "Should have at least one AND gate");
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
    println!("Primitives: {}", result.lir.primitives.len());

    let xor_prims: Vec<_> = result
        .lir
        .primitives
        .iter()
        .filter(|p| matches!(p.ptype, PrimitiveType::Xor))
        .collect();

    for prim in &result.lir.primitives {
        println!("  {:?} {}", prim.ptype, prim.path);
    }

    assert!(!xor_prims.is_empty(), "Should have at least one XOR gate");
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
    println!("Primitives: {}", result.lir.primitives.len());

    for prim in &result.lir.primitives {
        println!("  {:?} {}", prim.ptype, prim.path);
    }

    let mux_prims: Vec<_> = result
        .lir
        .primitives
        .iter()
        .filter(|p| matches!(p.ptype, PrimitiveType::Mux2))
        .collect();

    assert!(!mux_prims.is_empty(), "Should have at least one MUX2");
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
    println!("Primitives: {}", result.lir.primitives.len());

    // Count adder primitives
    let half_adders: Vec<_> = result
        .lir
        .primitives
        .iter()
        .filter(|p| matches!(p.ptype, PrimitiveType::HalfAdder))
        .collect();

    let full_adders: Vec<_> = result
        .lir
        .primitives
        .iter()
        .filter(|p| matches!(p.ptype, PrimitiveType::FullAdder))
        .collect();

    println!("HalfAdders: {}", half_adders.len());
    println!("FullAdders: {}", full_adders.len());

    for prim in &result.lir.primitives {
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
    println!("Primitives: {}", result.lir.primitives.len());

    // Equality comparison uses XNOR for each bit pair, then AND to combine
    let xnor_prims: Vec<_> = result
        .lir
        .primitives
        .iter()
        .filter(|p| matches!(p.ptype, PrimitiveType::Xnor))
        .collect();

    println!("XNOR primitives: {}", xnor_prims.len());

    for prim in &result.lir.primitives {
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
    println!("Primitives: {}", result.lir.primitives.len());

    // Calculate total FIT
    let total_fit: f64 = result
        .lir
        .primitives
        .iter()
        .map(|p| p.ptype.base_fit())
        .sum();

    println!("Total FIT: {:.2}", total_fit);

    // Print FIT breakdown by primitive type
    let mut fit_by_type: std::collections::HashMap<String, (usize, f64)> =
        std::collections::HashMap::new();
    for prim in &result.lir.primitives {
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
    let mut netlist = results[0].lir.clone();

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
    let netlist = &results[0].lir;

    println!("=== SIR Conversion ===");
    println!("Lir primitives: {}", netlist.primitives.len());

    // Convert to SIR
    let sir_result = convert_lir_to_sir(netlist);

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
    assert_eq!(
        result_sa0,
        vec![false],
        "SA0 fault should force output to 0"
    );

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

    // Step 1: Compile to Lir
    let results = compile_to_gate_netlist(source);
    let mut netlist = results[0].lir.clone();

    println!("\n1. Initial Lir:");
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
    let sir_result = convert_lir_to_sir(&netlist);

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
    let netlist = &results[0].lir;

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
    let netlist = &results[0].lir;

    // Convert to SIR
    let sir_result = convert_lir_to_sir(netlist);

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
    let netlist = &results[0].lir;
    let sir_result = convert_lir_to_sir(netlist);
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

    let netlist = &results[0].lir;
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
        *counts
            .entry(prim.ptype.short_name().to_string())
            .or_insert(0) += 1;
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
    let sir_result = convert_lir_to_sir(netlist);
    let sim = GateLevelSimulator::new(&sir_result.sir);

    println!("\n--- Simulator Ready ---");
    println!("Primitives in SIR: {}", sim.primitive_count());
    println!("SIR Total FIT: {:.2}", sim.total_fit());

    assert!(
        netlist.primitives.len() > 10,
        "ALU should have many primitives"
    );
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
                stage1_valid = valid_in;
                stage1_data = data_in;
            }

            // Stage 2: Processing (add 1, multiply by 2)
            signal stage2_valid: bit;
            signal stage2_data: bit[16];
            signal processed: bit[16];

            processed = (stage1_data + 1) << 1;

            on(clk.rise) {
                stage2_valid = stage1_valid;
                stage2_data = processed;
            }

            // Stage 3: Output register
            signal stage3_valid: bit;
            signal stage3_data: bit[16];

            on(clk.rise) {
                stage3_valid = stage2_valid;
                stage3_data = stage2_data;
            }

            valid_out = stage3_valid;
            data_out = stage3_data;
        }
    "#;

    let results = compile_to_gate_netlist(source);
    assert!(!results.is_empty(), "Should produce at least one netlist");

    let netlist = &results[0].lir;

    println!("=== Gate-Level Pipeline Analysis ===");
    println!("Module: {}", netlist.name);
    println!("Primitives: {}", netlist.primitives.len());
    println!("Nets: {}", netlist.nets.len());

    // Count sequential vs combinational
    let seq_count = netlist
        .primitives
        .iter()
        .filter(|p| p.ptype.is_sequential())
        .count();
    let comb_count = netlist.primitives.len() - seq_count;

    println!("\nSequential primitives: {} (flip-flops)", seq_count);
    println!("Combinational primitives: {}", comb_count);

    // FIT breakdown
    let seq_fit: f64 = netlist
        .primitives
        .iter()
        .filter(|p| p.ptype.is_sequential())
        .map(|p| p.ptype.base_fit())
        .sum();
    let comb_fit: f64 = netlist
        .primitives
        .iter()
        .filter(|p| !p.ptype.is_sequential())
        .map(|p| p.ptype.base_fit())
        .sum();

    println!("\nSequential FIT: {:.2}", seq_fit);
    println!("Combinational FIT: {:.2}", comb_fit);
    println!("Total FIT: {:.2}", seq_fit + comb_fit);

    // Note: Sequential logic (DFFs) generation in mir_to_gate_netlist is still WIP
    // For now, just verify we get some primitives from the combinational logic
    println!("\nNote: DFF generation is still WIP in mir_to_gate_netlist");
    assert!(
        !netlist.primitives.is_empty(),
        "Should have some primitives"
    );
}

#[cfg(target_os = "macos")]
#[test]
fn test_gpu_fault_campaign_on_alu() {
    use skalp_sim::{GpuFaultCampaignConfig, GpuFaultSimulator};

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
    let netlist = &results[0].lir;
    let sir_result = convert_lir_to_sir(netlist);

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

/// Debug test to trace gate-level evaluation of the 4-bit adder with slice
#[test]
fn test_debug_gate_adder_4bit() {
    let source = r#"
        entity Adder4<'clk> {
            in a: bit[4],
            in b: bit[4],
            out sum: bit[4],
            out carry: bit,
        }
        impl Adder4<'clk> {
            signal temp: bit[5] = a + b;
            sum = temp[3:0];
            carry = temp[4];
        }
    "#;

    let results = compile_to_gate_netlist(source);
    let netlist = &results[0].lir;
    let sir_result = convert_lir_to_sir(netlist);
    let mut sim = GateLevelSimulator::new(&sir_result.sir);

    println!("=== Debug 4-bit Adder with Slice ===\n");

    // Print all signals
    println!("--- SIR Signals ---");
    for sig in &sir_result.sir.top_module.signals {
        println!(
            "  {} (id={}, width={}): {:?}",
            sig.name, sig.id.0, sig.width, sig.signal_type
        );
    }

    // Print all primitives with eval order
    println!(
        "\n--- Primitives ({}) ---",
        sir_result
            .sir
            .top_module
            .comb_blocks
            .iter()
            .map(|b| b.operations.len())
            .sum::<usize>()
    );
    for block in &sir_result.sir.top_module.comb_blocks {
        if let Some(ref info) = block.structural_info {
            if let Some(ref order) = info.eval_order {
                println!("Eval order: {:?}", order);
            }
        }
        for (i, op) in block.operations.iter().enumerate() {
            if let skalp_sim::sir::SirOperation::Primitive {
                ptype,
                inputs,
                outputs,
                path,
                ..
            } = op
            {
                println!(
                    "  [{}] {:?} @ {}: in={:?} out={:?}",
                    i, ptype, path, inputs, outputs
                );
            }
        }
    }

    // Test: 1 + 3 = 4
    println!("\n--- Test: a=1, b=3 (expected sum=4, carry=0) ---");
    sim.set_input_u64("a", 1);
    sim.set_input_u64("b", 3);

    // Check inputs were set
    println!("After set_input:");
    for sig in &sir_result.sir.top_module.signals {
        if sig.name.starts_with("a[") || sig.name.starts_with("b[") {
            let val = sim.get_output(&sig.name).unwrap_or_default();
            println!("  {} = {:?}", sig.name, val);
        }
    }

    sim.step();

    println!("\nAfter step:");
    // Show all signals with non-zero values or output signals
    for sig in &sir_result.sir.top_module.signals {
        let val = sim.get_output(&sig.name).unwrap_or_default();
        if val.iter().any(|&b| b)
            || sig.name.contains("sum")
            || sig.name.contains("carry")
            || sig.name.contains("temp")
        {
            println!("  {} = {:?}", sig.name, val);
        }
    }

    let sum_result = sim.get_output_u64("sum").unwrap_or(0);
    let carry_result = sim.get_output_u64("carry").unwrap_or(0);
    println!(
        "\nResult: sum={}, carry={} (expected sum=4, carry=0)",
        sum_result, carry_result
    );

    // Test a few more values
    println!("\n--- More tests ---");
    for (a, b, exp_sum, exp_carry) in [(1, 3, 4, 0), (7, 8, 15, 0), (8, 8, 0, 1), (15, 1, 0, 1)] {
        sim.reset();
        sim.set_input_u64("a", a);
        sim.set_input_u64("b", b);
        sim.step();
        let sum = sim.get_output_u64("sum").unwrap_or(0);
        let carry = sim.get_output_u64("carry").unwrap_or(0);
        let ok = sum == exp_sum && carry == exp_carry;
        println!(
            "{} + {} = sum={}, carry={} (expected {},{}) {}",
            a,
            b,
            sum,
            carry,
            exp_sum,
            exp_carry,
            if ok { "✓" } else { "✗" }
        );
    }
}

/// Debug test to trace gate-level evaluation of a simple 2-bit adder
#[test]
fn test_debug_gate_adder() {
    let source = r#"
        entity Add2<'clk> {
            in a: bit[2],
            in b: bit[2],
            out sum: bit[2],
        }
        impl Add2<'clk> {
            sum = a + b;
        }
    "#;

    let results = compile_to_gate_netlist(source);
    let netlist = &results[0].lir;
    let sir_result = convert_lir_to_sir(netlist);
    let mut sim = GateLevelSimulator::new(&sir_result.sir);

    println!("=== Debug 2-bit Adder ===\n");

    // Print all signals
    println!("--- SIR Signals ---");
    for sig in &sir_result.sir.top_module.signals {
        println!(
            "  {} (id={}, width={}): {:?}",
            sig.name, sig.id.0, sig.width, sig.signal_type
        );
    }

    // Print all primitives
    println!("\n--- Primitives ---");
    for block in &sir_result.sir.top_module.comb_blocks {
        for op in &block.operations {
            if let skalp_sim::sir::SirOperation::Primitive {
                ptype,
                inputs,
                outputs,
                path,
                ..
            } = op
            {
                println!(
                    "  {:?} @ {}: in={:?} out={:?}",
                    ptype, path, inputs, outputs
                );
            }
        }
    }

    // Test: 1 + 1 = 2
    println!("\n--- Test: a=1, b=1 ---");
    sim.set_input_u64("a", 1);
    sim.set_input_u64("b", 1);

    // Check inputs were set
    println!("After set_input:");
    for sig in &sir_result.sir.top_module.signals {
        if sig.name.starts_with("a[")
            || sig.name.starts_with("b[")
            || sig.name == "a"
            || sig.name == "b"
        {
            let val = sim.get_output(&sig.name).unwrap_or_default();
            println!("  {} = {:?}", sig.name, val);
        }
    }

    sim.step();

    println!("\nAfter step:");
    for sig in &sir_result.sir.top_module.signals {
        let val = sim.get_output(&sig.name).unwrap_or_default();
        let u64val = sim.get_output_u64(&sig.name).unwrap_or(0);
        if val.iter().any(|&b| b) || sig.name.contains("sum") || sig.name.contains("add_") {
            println!("  {} = {:?} (u64={})", sig.name, val, u64val);
        }
    }

    let result = sim.get_output_u64("sum").unwrap_or(0);
    println!("\nResult: sum = {} (expected 2)", result);

    // Test a few more values
    for (a, b) in [(0, 0), (1, 0), (0, 1), (1, 1), (2, 1), (3, 3)] {
        sim.reset();
        sim.set_input_u64("a", a);
        sim.set_input_u64("b", b);
        sim.step();
        let result = sim.get_output_u64("sum").unwrap_or(0);
        let expected = (a + b) & 3; // Mask to 2 bits
        println!(
            "{} + {} = {} (expected {}) {}",
            a,
            b,
            result,
            expected,
            if result == expected { "✓" } else { "✗" }
        );
    }
}

/// Test equivalence between functional (behavioral) and gate-level simulators
/// This test verifies that both simulators produce identical outputs for the same inputs
#[tokio::test]
async fn test_functional_vs_gate_level_equivalence() {
    use skalp_mir::MirCompiler;
    use skalp_sim::{SimulationConfig, Simulator};

    println!("=== Functional vs Gate-Level Equivalence Test ===\n");

    // Simple 4-bit adder - purely combinational
    let source = r#"
        entity Adder4<'clk> {
            in a: bit[4],
            in b: bit[4],
            out sum: bit[4],
            out carry: bit,
        }

        impl Adder4<'clk> {
            signal temp: bit[5] = a + b;
            sum = temp[3:0];
            carry = temp[4];
        }
    "#;

    // ============ Setup Gate-Level Simulator ============
    let results = compile_to_gate_netlist(source);
    let netlist = &results[0].lir;
    let gate_sir = convert_lir_to_sir(netlist);
    let mut gate_sim = GateLevelSimulator::new(&gate_sir.sir);

    println!(
        "Gate-level netlist: {} primitives, {:.2} FIT",
        netlist.primitives.len(),
        gate_sim.total_fit()
    );

    // ============ Setup Functional Simulator ============
    let hir = parse_and_build_hir(source).expect("Failed to parse");
    let compiler = MirCompiler::new();
    let mir = compiler
        .compile_to_mir(&hir)
        .expect("Failed to compile to MIR");
    let func_sir = skalp_sir::convert_mir_to_sir(&mir.modules[0]);

    let config = SimulationConfig {
        use_gpu: false, // Use CPU runtime for comparison
        max_cycles: 100,
        timeout_ms: 5000,
        capture_waveforms: false,
        parallel_threads: 1,
    };

    let mut func_sim = Simulator::new(config)
        .await
        .expect("Failed to create simulator");
    func_sim
        .load_module(&func_sir)
        .await
        .expect("Failed to load module");

    println!(
        "Functional SIR: {} inputs, {} outputs, {} comb nodes",
        func_sir.inputs.len(),
        func_sir.outputs.len(),
        func_sir.combinational_nodes.len()
    );

    // ============ Test All Input Combinations ============
    let mut mismatches = 0;
    let mut total_tests = 0;

    println!("\n--- Testing all input combinations (0-15 × 0-15) ---");

    for a in 0u8..16 {
        for b in 0u8..16 {
            total_tests += 1;

            // Expected result
            let expected_sum = (a + b) & 0x0F;
            let expected_carry = (a + b) > 15;

            // ---- Gate-Level Simulation ----
            gate_sim.reset();
            gate_sim.set_input_u64("a", a as u64);
            gate_sim.set_input_u64("b", b as u64);
            gate_sim.step();

            let gate_sum = gate_sim.get_output_u64("sum").unwrap_or(0) as u8;
            let gate_carry = gate_sim.get_output_u64("carry").unwrap_or(0) != 0;

            // ---- Functional Simulation ----
            func_sim.reset().await.expect("Reset failed");
            func_sim
                .set_input("a", vec![a])
                .await
                .expect("Set a failed");
            func_sim
                .set_input("b", vec![b])
                .await
                .expect("Set b failed");
            func_sim.step_simulation().await.expect("Step failed");

            let func_sum_bytes = func_sim.get_output("sum").await.expect("Get sum failed");
            let func_carry_bytes = func_sim
                .get_output("carry")
                .await
                .expect("Get carry failed");
            let func_sum = func_sum_bytes.first().copied().unwrap_or(0) & 0x0F;
            let func_carry = func_carry_bytes.first().copied().unwrap_or(0) != 0;

            // ---- Compare Results ----
            let gate_correct = gate_sum == expected_sum && gate_carry == expected_carry;
            let func_correct = func_sum == expected_sum && func_carry == expected_carry;
            let match_each_other = gate_sum == func_sum && gate_carry == func_carry;

            if !gate_correct || !func_correct || !match_each_other {
                mismatches += 1;
                if mismatches <= 5 {
                    println!(
                        "MISMATCH a={}, b={}: expected={},{} gate={},{} func={},{}",
                        a,
                        b,
                        expected_sum,
                        expected_carry as u8,
                        gate_sum,
                        gate_carry as u8,
                        func_sum,
                        func_carry as u8
                    );
                }
            }
        }
    }

    println!("\n--- Results ---");
    println!("Total tests: {}", total_tests);
    println!("Mismatches: {}", mismatches);
    println!(
        "Match rate: {:.2}%",
        (total_tests - mismatches) as f64 / total_tests as f64 * 100.0
    );

    if mismatches == 0 {
        println!("✓ Gate-level and functional simulators produce IDENTICAL results");
    } else {
        println!("✗ EQUIVALENCE FAILURE: {} mismatches detected", mismatches);
    }

    // The test should pass if results match
    assert_eq!(
        mismatches, 0,
        "Functional and gate-level simulators should produce identical results"
    );
}

/// Test equivalence for bitwise operations
#[tokio::test]
async fn test_bitwise_ops_equivalence() {
    use skalp_mir::MirCompiler;
    use skalp_sim::{SimulationConfig, Simulator};

    println!("=== Bitwise Operations Equivalence Test ===\n");

    let source = r#"
        entity BitwiseOps<'clk> {
            in a: bit[8],
            in b: bit[8],
            out and_out: bit[8],
            out or_out: bit[8],
            out xor_out: bit[8],
            out not_out: bit[8],
        }

        impl BitwiseOps<'clk> {
            and_out = a & b;
            or_out = a | b;
            xor_out = a ^ b;
            not_out = !a;
        }
    "#;

    // Setup gate-level
    let results = compile_to_gate_netlist(source);
    let netlist = &results[0].lir;
    let gate_sir = convert_lir_to_sir(netlist);
    let mut gate_sim = GateLevelSimulator::new(&gate_sir.sir);

    // Setup functional
    let hir = parse_and_build_hir(source).expect("Failed to parse");
    let compiler = MirCompiler::new();
    let mir = compiler
        .compile_to_mir(&hir)
        .expect("Failed to compile to MIR");
    let func_sir = skalp_sir::convert_mir_to_sir(&mir.modules[0]);

    let config = SimulationConfig {
        use_gpu: false,
        max_cycles: 100,
        timeout_ms: 5000,
        capture_waveforms: false,
        parallel_threads: 1,
    };

    let mut func_sim = Simulator::new(config)
        .await
        .expect("Failed to create simulator");
    func_sim
        .load_module(&func_sir)
        .await
        .expect("Failed to load module");

    println!("Gate-level: {} primitives", netlist.primitives.len());

    let test_values: Vec<(u8, u8)> = vec![
        (0x00, 0x00),
        (0xFF, 0xFF),
        (0xAA, 0x55),
        (0x0F, 0xF0),
        (0x12, 0x34),
        (0x80, 0x01),
        (0x7F, 0x80),
        (0x55, 0xAA),
    ];

    let mut mismatches = 0;

    for (a, b) in &test_values {
        // Gate-level
        gate_sim.reset();
        gate_sim.set_input_u64("a", *a as u64);
        gate_sim.set_input_u64("b", *b as u64);
        gate_sim.step();

        let gate_and = gate_sim.get_output_u64("and_out").unwrap_or(0) as u8;
        let gate_or = gate_sim.get_output_u64("or_out").unwrap_or(0) as u8;
        let gate_xor = gate_sim.get_output_u64("xor_out").unwrap_or(0) as u8;
        let gate_not = gate_sim.get_output_u64("not_out").unwrap_or(0) as u8;

        // Functional
        func_sim.reset().await.expect("Reset failed");
        func_sim
            .set_input("a", vec![*a])
            .await
            .expect("Set a failed");
        func_sim
            .set_input("b", vec![*b])
            .await
            .expect("Set b failed");
        func_sim.step_simulation().await.expect("Step failed");

        let func_and = func_sim
            .get_output("and_out")
            .await
            .unwrap()
            .first()
            .copied()
            .unwrap_or(0);
        let func_or = func_sim
            .get_output("or_out")
            .await
            .unwrap()
            .first()
            .copied()
            .unwrap_or(0);
        let func_xor = func_sim
            .get_output("xor_out")
            .await
            .unwrap()
            .first()
            .copied()
            .unwrap_or(0);
        let func_not = func_sim
            .get_output("not_out")
            .await
            .unwrap()
            .first()
            .copied()
            .unwrap_or(0);

        // Expected
        let exp_and = a & b;
        let exp_or = a | b;
        let exp_xor = a ^ b;
        let exp_not = !a;

        if gate_and != exp_and
            || gate_or != exp_or
            || gate_xor != exp_xor
            || gate_not != exp_not
            || func_and != exp_and
            || func_or != exp_or
            || func_xor != exp_xor
            || func_not != exp_not
            || gate_and != func_and
            || gate_or != func_or
            || gate_xor != func_xor
            || gate_not != func_not
        {
            mismatches += 1;
            println!("MISMATCH a=0x{:02X}, b=0x{:02X}:", a, b);
            println!(
                "  AND: exp=0x{:02X} gate=0x{:02X} func=0x{:02X}",
                exp_and, gate_and, func_and
            );
            println!(
                "  OR:  exp=0x{:02X} gate=0x{:02X} func=0x{:02X}",
                exp_or, gate_or, func_or
            );
            println!(
                "  XOR: exp=0x{:02X} gate=0x{:02X} func=0x{:02X}",
                exp_xor, gate_xor, func_xor
            );
            println!(
                "  NOT: exp=0x{:02X} gate=0x{:02X} func=0x{:02X}",
                exp_not, gate_not, func_not
            );
        }
    }

    println!("\nTotal tests: {}", test_values.len());
    println!("Mismatches: {}", mismatches);

    assert_eq!(
        mismatches, 0,
        "Bitwise operations should produce identical results"
    );
}

/// Test that DFFs are generated for sequential logic
#[test]
fn test_dff_generation() {
    // Simple counter - should generate DFFs
    let source = r#"
        entity Counter<'clk> {
            in clk: clock,
            in rst: bit,
            out count: bit[4],
        }

        impl Counter<'clk> {
            signal counter_reg: bit[4];

            on(clk.rise) {
                if rst {
                    counter_reg = 0;
                } else {
                    counter_reg = counter_reg + 1;
                }
            }

            count = counter_reg;
        }
    "#;

    let hir = parse_and_build_hir(source).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
        .expect("Failed to compile to MIR");

    // Verify process is sequential
    let module = &mir.modules[0];
    assert_eq!(module.processes.len(), 1);
    assert!(matches!(
        module.processes[0].kind,
        skalp_mir::mir::ProcessKind::Sequential
    ));

    // Lower to gate netlist
    let results = lower_to_lir(&mir).expect("Failed to lower to Lir");
    let netlist = &results[0].lir;

    // Count DFF primitives
    let dff_count = netlist
        .primitives
        .iter()
        .filter(|p| {
            matches!(
                p.ptype,
                PrimitiveType::DffP
                    | PrimitiveType::DffN
                    | PrimitiveType::DffAR
                    | PrimitiveType::DffAS
            )
        })
        .count();

    // Should have DFFs for the 4-bit counter register
    assert!(
        dff_count >= 4,
        "Counter should generate at least 4 DFF primitives for the 4-bit register, got {}",
        dff_count
    );

    // Verify DFFs have clock connected
    for prim in &netlist.primitives {
        if matches!(
            prim.ptype,
            PrimitiveType::DffP | PrimitiveType::DffN | PrimitiveType::DffAR | PrimitiveType::DffAS
        ) {
            assert!(prim.clock.is_some(), "DFF should have clock connected");
        }
    }
}

/// Test sequential simulation equivalence between gate-level and functional simulators
/// This test runs a counter through multiple clock cycles and compares outputs
#[tokio::test]
async fn test_sequential_simulation_equivalence() {
    use skalp_mir::MirCompiler;
    use skalp_sim::{SimulationConfig, Simulator};

    println!("=== Sequential Simulation Equivalence Test ===\n");

    // Simple 4-bit counter with synchronous reset
    let source = r#"
        entity Counter4<'clk> {
            in clk: clock,
            in rst: bit,
            out count: bit[4],
        }

        impl Counter4<'clk> {
            signal counter_reg: bit[4];

            on(clk.rise) {
                if rst {
                    counter_reg = 0;
                } else {
                    counter_reg = counter_reg + 1;
                }
            }

            count = counter_reg;
        }
    "#;

    // ============ Setup Gate-Level Simulator ============
    let results = compile_to_gate_netlist(source);
    let netlist = &results[0].lir;

    // Count DFFs to verify sequential logic was generated
    let dff_count = netlist
        .primitives
        .iter()
        .filter(|p| {
            matches!(
                p.ptype,
                PrimitiveType::DffP
                    | PrimitiveType::DffN
                    | PrimitiveType::DffAR
                    | PrimitiveType::DffAS
            )
        })
        .count();
    println!(
        "Gate-level netlist: {} primitives, {} DFFs",
        netlist.primitives.len(),
        dff_count
    );

    // Debug: show all primitives
    println!("\n--- Gate Primitives ---");
    for prim in &netlist.primitives {
        println!(
            "  {:?} @ {} inputs={:?} outputs={:?} clk={:?}",
            prim.ptype, prim.path, prim.inputs, prim.outputs, prim.clock
        );
    }

    assert!(
        dff_count >= 4,
        "Should have at least 4 DFFs for 4-bit counter, got {}",
        dff_count
    );

    let gate_sir = convert_lir_to_sir(netlist);

    // Debug: check seq_blocks
    println!("\n--- SIR Sequential Blocks ---");
    println!(
        "Number of seq_blocks: {}",
        gate_sir.sir.top_module.seq_blocks.len()
    );
    for (i, block) in gate_sir.sir.top_module.seq_blocks.iter().enumerate() {
        println!(
            "Block {}: clock={:?}, edge={:?}, {} operations",
            i,
            block.clock,
            block.clock_edge,
            block.operations.len()
        );
        for (j, op) in block.operations.iter().enumerate() {
            if let skalp_sim::sir::SirOperation::Primitive {
                id: _,
                ptype,
                inputs,
                outputs,
                path,
            } = op
            {
                println!(
                    "  Op {}: {:?} @ {} inputs={:?} outputs={:?}",
                    j, ptype, path, inputs, outputs
                );
            }
        }
    }

    // Debug: print SIR signal mapping
    println!("\n--- SIR Signals (ID -> Name) ---");
    for sig in &gate_sir.sir.top_module.signals {
        println!(
            "  SirSignalId({}) = '{}' width={}",
            sig.id.0, sig.name, sig.width
        );
    }

    let mut gate_sim = GateLevelSimulator::new(&gate_sir.sir);

    // ============ Setup Functional Simulator ============
    let hir = parse_and_build_hir(source).expect("Failed to parse");
    let compiler = MirCompiler::new();
    let mir = compiler
        .compile_to_mir(&hir)
        .expect("Failed to compile to MIR");
    let func_sir = skalp_sir::convert_mir_to_sir(&mir.modules[0]);

    let config = SimulationConfig {
        use_gpu: false,
        max_cycles: 100,
        timeout_ms: 5000,
        capture_waveforms: false,
        parallel_threads: 1,
    };

    let mut func_sim = Simulator::new(config)
        .await
        .expect("Failed to create simulator");
    func_sim
        .load_module(&func_sir)
        .await
        .expect("Failed to load module");

    println!(
        "Functional SIR: {} sequential nodes",
        func_sir.sequential_nodes.len()
    );

    // ============ Test Reset Behavior ============
    println!("\n--- Testing reset behavior ---");

    // Apply reset
    gate_sim.reset();
    gate_sim.set_input_u64("rst", 1);
    gate_sim.set_input_u64("clk", 0);
    gate_sim.step(); // Setup
    gate_sim.set_input_u64("clk", 1); // Rising edge
    gate_sim.step();

    func_sim.reset().await.expect("Reset failed");
    func_sim
        .set_input("rst", vec![1])
        .await
        .expect("Set rst failed");
    func_sim
        .set_input("clk", vec![0])
        .await
        .expect("Set clk failed");
    func_sim.step_simulation().await.expect("Step failed");
    func_sim
        .set_input("clk", vec![1])
        .await
        .expect("Set clk failed");
    func_sim.step_simulation().await.expect("Step failed");

    let _gate_count_after_reset = gate_sim.get_output_u64("count").unwrap_or(999);
    let func_count_bytes = func_sim
        .get_output("count")
        .await
        .expect("Get count failed");
    let _func_count_after_reset = func_count_bytes.first().copied().unwrap_or(99) as u64;

    // ============ Test Counting Behavior ============

    // Release reset and count for several cycles
    gate_sim.set_input_u64("rst", 0);
    func_sim
        .set_input("rst", vec![0])
        .await
        .expect("Set rst failed");

    let mut mismatches = 0;
    let num_cycles = 20;

    for cycle in 0..num_cycles {
        let expected_count = (cycle + 1) % 16; // 4-bit counter wraps at 16

        // Clock low
        gate_sim.set_input_u64("clk", 0);
        gate_sim.step();
        func_sim
            .set_input("clk", vec![0])
            .await
            .expect("Set clk failed");
        func_sim.step_simulation().await.expect("Step failed");

        // Clock high (rising edge - count increments)
        gate_sim.set_input_u64("clk", 1);
        gate_sim.step();
        func_sim
            .set_input("clk", vec![1])
            .await
            .expect("Set clk failed");
        func_sim.step_simulation().await.expect("Step failed");

        // Read outputs
        let gate_count = gate_sim.get_output_u64("count").unwrap_or(999);
        let func_count_bytes = func_sim
            .get_output("count")
            .await
            .expect("Get count failed");
        let func_count = func_count_bytes.first().copied().unwrap_or(99) as u64;

        let gate_correct = gate_count == expected_count;
        let func_correct = func_count == expected_count;
        let match_each_other = gate_count == func_count;

        if !gate_correct || !func_correct || !match_each_other {
            mismatches += 1;
            // On mismatch, dump signals for debugging
            let signals = gate_sim.dump_signals();
            println!(
                "Cycle {}: MISMATCH expected={} gate={} func={}",
                cycle, expected_count, gate_count, func_count
            );
            for (name, val) in &signals {
                let v: u64 = val.iter().enumerate().map(|(i, &b)| (b as u64) << i).sum();
                if name.contains("counter") || name.contains("count") || name.contains("add_") {
                    println!("    {} = {} ({:?})", name, v, val);
                }
            }
        }
    }

    // Verify all cycles matched
    assert_eq!(mismatches, 0, "Sequential simulation: gate-level and functional should produce identical results for all {} cycles", num_cycles);
}
