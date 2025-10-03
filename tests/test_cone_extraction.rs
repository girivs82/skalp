#[cfg(test)]
mod cone_extraction_tests {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::{MirCompiler, OptimizationLevel};
    use skalp_sir::sir::BinaryOperation;
    use skalp_sir::{convert_mir_to_sir, SirNodeKind};

    #[test]
    fn test_counter_cone_extraction() {
        let source = r#"
        entity Counter {
            in clk: clock
            in rst: reset
            out count: nat[8]
        }

        impl Counter {
            signal counter: nat[8] = 0

            on(clk.rise) {
                if (rst) {
                    counter <= 0
                } else {
                    counter <= counter + 1
                }
            }

            count = counter
        }
        "#;

        let hir = parse_and_build_hir(source).unwrap();
        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::Basic);
        let mir = compiler.compile_to_mir(&hir).unwrap();

        println!("=== MIR Analysis ===");
        println!("Module: {:?}", mir.modules[0].id);
        println!("Processes: {}", mir.modules[0].processes.len());

        for (i, process) in mir.modules[0].processes.iter().enumerate() {
            println!("\nProcess {}: {:?}", i, process.id);
            println!("  Type: {:?}", process.kind);
            println!("  Body statements: {}", process.body.statements.len());
        }

        println!("\nAssignments: {}", mir.modules[0].assignments.len());

        let sir = convert_mir_to_sir(&mir.modules[0]);

        println!("\n=== SIR Analysis ===");
        println!("Module: {}", sir.name);
        println!(
            "Inputs: {:?}",
            sir.inputs.iter().map(|p| &p.name).collect::<Vec<_>>()
        );
        println!(
            "Outputs: {:?}",
            sir.outputs.iter().map(|p| &p.name).collect::<Vec<_>>()
        );
        println!(
            "State elements: {:?}",
            sir.state_elements.keys().collect::<Vec<_>>()
        );

        println!("\nCombinational nodes: {}", sir.combinational_nodes.len());
        for (i, node) in sir.combinational_nodes.iter().enumerate() {
            println!("  Comb Node {}: {:?}", i, node.kind);
            println!("    ID: {}", node.id);
            println!("    Inputs: {:?}", node.inputs);
            println!("    Outputs: {:?}", node.outputs);
        }

        println!("\nSequential nodes: {}", sir.sequential_nodes.len());
        for (i, node) in sir.sequential_nodes.iter().enumerate() {
            println!("  Seq Node {}: {:?}", i, node.kind);
            println!("    ID: {}", node.id);
            println!("    Inputs: {:?}", node.inputs);
            println!("    Outputs: {:?}", node.outputs);
        }

        // Extract and print combinational cones
        let cones = sir.extract_combinational_cones();
        println!("\n=== Combinational Cones ===");
        println!("Number of cones: {}", cones.len());

        for (i, cone) in cones.iter().enumerate() {
            println!("\nCone {}:", i);
            println!("  Node IDs in cone: {:?}", cone.nodes);
            println!("  Cone inputs: {:?}", cone.inputs);
            println!("  Cone outputs: {:?}", cone.outputs);

            // Print what each node in the cone does
            println!("  Node details:");
            for &node_id in &cone.nodes {
                if let Some(node) = sir.combinational_nodes.iter().find(|n| n.id == node_id) {
                    println!("    Node {}: {:?}", node_id, node.kind);
                    println!("      Inputs: {:?}", node.inputs);
                    println!("      Outputs: {:?}", node.outputs);
                }
            }
        }

        // Verify we have the increment logic
        assert!(
            sir.combinational_nodes.len() > 0 || sir.sequential_nodes.len() > 0,
            "Should have some nodes"
        );

        // Check if any combinational node is an add operation
        let has_add = sir
            .combinational_nodes
            .iter()
            .any(|node| matches!(node.kind, SirNodeKind::BinaryOp(BinaryOperation::Add)));

        println!("\nHas ADD operation in combinational nodes: {}", has_add);

        // The counter should have either:
        // 1. A combinational ADD node for counter + 1
        // 2. Or the increment should be in sequential logic
        assert!(
            has_add || sir.sequential_nodes.len() > 0,
            "Counter should have increment logic"
        );
    }
}
