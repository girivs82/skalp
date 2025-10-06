//! Example of complete synthesis flow from MIR to optimized LIR

use skalp_lir::{
    lir::{Gate, GateType, Lir, Net},
    optimization::*,
    tech_mapping::TechMapper,
    technology::{Technology, TechnologyKind},
    timing::TimingAnalyzer,
};

fn main() {
    println!("=== SKALP Synthesis Example ===\n");

    // Create a simple LIR design (normally would come from MIR)
    let mut lir = create_example_design();

    println!("Original design:");
    print_stats(&lir);

    // Step 1: Run optimization pipeline
    println!("\n--- Running Optimization Pipeline ---");
    let mut pipeline = OptimizationPipeline::standard();
    let opt_results = pipeline.optimize(&mut lir);

    for result in &opt_results {
        println!(
            "  {}: {} -> {} gates",
            result.pass_name, result.gates_before, result.gates_after
        );
    }

    println!("\nOptimized design:");
    print_stats(&lir);

    // Step 2: Technology mapping for FPGA
    println!("\n--- FPGA Technology Mapping ---");
    let fpga_tech = Technology {
        name: "Xilinx 7-Series".to_string(),
        kind: TechnologyKind::FPGA,
        process_nm: 28,
    };

    let fpga_mapper = TechMapper::new(fpga_tech);
    let fpga_report = fpga_mapper.map(&mut lir.clone());
    fpga_report.print();

    // Step 3: Technology mapping for ASIC
    println!("\n--- ASIC Technology Mapping ---");
    let asic_tech = Technology {
        name: "TSMC 7nm".to_string(),
        kind: TechnologyKind::ASIC,
        process_nm: 7,
    };

    let asic_mapper = TechMapper::new(asic_tech);
    let asic_report = asic_mapper.map(&mut lir.clone());
    asic_report.print();

    // Step 4: Timing analysis
    println!("\n--- Timing Analysis ---");
    let mut analyzer = TimingAnalyzer::new(1000.0); // 1ns clock
    analyzer.build_graph(&lir);
    let timing_report = analyzer.analyze();
    timing_report.print();

    // Compare area between FPGA and ASIC
    println!("\n--- Synthesis Comparison ---");
    println!("FPGA area: {:.2} LUTs", fpga_report.total_area);
    println!("ASIC area: {:.2} gate equivalents", asic_report.total_area);
    println!(
        "Area ratio (ASIC/FPGA): {:.2}x",
        asic_report.total_area / fpga_report.total_area
    );

    // Check if we met our optimization goal (20% area reduction)
    let original_gates = create_example_design().gates.len();
    let optimized_gates = lir.gates.len();
    let reduction = 100.0 * (1.0 - optimized_gates as f64 / original_gates as f64);

    println!("\n--- Optimization Success ---");
    println!("Original gates: {}", original_gates);
    println!("Optimized gates: {}", optimized_gates);
    println!("Area reduction: {:.1}%", reduction);

    if reduction >= 20.0 {
        println!("✅ SUCCESS: Achieved >20% area reduction!");
    } else {
        println!("⚠️  Area reduction less than 20% target");
    }
}

/// Create an example design with optimization opportunities
fn create_example_design() -> Lir {
    let mut lir = Lir::new("example_alu".to_string());

    // Create nets
    for name in &[
        "a", "b", "c", "d", "sel", "out1", "out2", "temp1", "temp2", "temp3",
    ] {
        lir.nets.push(Net {
            id: name.to_string(),
            width: 1,
            driver: None,
            loads: Vec::new(),
            is_output: false,
            is_input: false,
        });
    }

    // Add some gates with optimization opportunities

    // Constant folding opportunity
    lir.gates.push(Gate {
        id: "and_const".to_string(),
        gate_type: GateType::And,
        inputs: vec!["tie_low".to_string(), "a".to_string()],
        outputs: vec!["temp1".to_string()],
    });

    // Double negation for Boolean simplification
    lir.gates.push(Gate {
        id: "not1".to_string(),
        gate_type: GateType::Not,
        inputs: vec!["b".to_string()],
        outputs: vec!["temp2".to_string()],
    });

    lir.gates.push(Gate {
        id: "not2".to_string(),
        gate_type: GateType::Not,
        inputs: vec!["temp2".to_string()],
        outputs: vec!["temp3".to_string()],
    });

    // Duplicate logic for CSE
    lir.gates.push(Gate {
        id: "and1".to_string(),
        gate_type: GateType::And,
        inputs: vec!["c".to_string(), "d".to_string()],
        outputs: vec!["out1".to_string()],
    });

    lir.gates.push(Gate {
        id: "and2".to_string(),
        gate_type: GateType::And,
        inputs: vec!["c".to_string(), "d".to_string()],
        outputs: vec!["out2".to_string()],
    });

    // Dead code (output not used)
    lir.gates.push(Gate {
        id: "dead_or".to_string(),
        gate_type: GateType::Or,
        inputs: vec!["a".to_string(), "b".to_string()],
        outputs: vec!["unused".to_string()],
    });

    // Some useful logic
    lir.gates.push(Gate {
        id: "xor1".to_string(),
        gate_type: GateType::Xor,
        inputs: vec!["a".to_string(), "b".to_string()],
        outputs: vec!["out_xor".to_string()],
    });

    // Mark output nets
    lir.nets.push(Net {
        id: "out_xor".to_string(),
        width: 1,
        driver: Some("xor1".to_string()),
        loads: Vec::new(),
        is_output: true,
        is_input: false,
    });

    lir
}

/// Print design statistics
fn print_stats(lir: &Lir) {
    println!("  Gates: {}", lir.gates.len());
    println!("  Nets: {}", lir.nets.len());

    // Count gate types
    let mut gate_counts = std::collections::HashMap::new();
    for gate in &lir.gates {
        *gate_counts.entry(&gate.gate_type).or_insert(0) += 1;
    }

    print!("  Types: ");
    for (gate_type, count) in gate_counts {
        print!("{:?}:{} ", gate_type, count);
    }
    println!();
}
