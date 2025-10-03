#[cfg(test)]
mod debug_lir_tests {
    use skalp_frontend::parse_and_build_hir;
    use skalp_lir::transform_mir_to_lir;
    use skalp_mir::lower_to_mir;

    #[test]
    fn debug_lir_generation_detailed() {
        let source = r#"
        entity SimpleTest {
            in a: bool
            in b: bool
            out y: bool
        }

        impl SimpleTest {
            assign y = a && b;
        }
        "#;

        println!("🔍 Debugging LIR generation in detail...");

        let hir = parse_and_build_hir(source).expect("Should parse");
        let mir = lower_to_mir(&hir).expect("Should build MIR");

        println!("\n📊 MIR Analysis:");
        if let Some(module) = mir.modules.first() {
            println!("Module: {}", module.name);
            println!("Ports ({}):", module.ports.len());
            for port in &module.ports {
                println!(
                    "  - {} ({:?}, {:?})",
                    port.name, port.direction, port.port_type
                );
            }

            println!("Assignments ({}):", module.assignments.len());
            for (i, assign) in module.assignments.iter().enumerate() {
                println!("  Assignment {}: <lhs> = <rhs>", i);
                // Note: ContinuousAssign has 'lhs' and 'rhs' fields, not 'target'
            }

            println!("Processes ({}):", module.processes.len());
            for (i, process) in module.processes.iter().enumerate() {
                println!("  Process {}: {:?}", i, process.kind);
            }

            // Transform to LIR and examine details
            let lir = transform_mir_to_lir(module);

            println!("\n🔧 LIR Analysis:");
            println!("Gates ({}):", lir.gates.len());
            for (i, gate) in lir.gates.iter().enumerate() {
                println!("  Gate {}: {} ({:?})", i, gate.id, gate.gate_type);
                println!("    Inputs: {:?}", gate.inputs);
                println!("    Outputs: {:?}", gate.outputs);
            }

            println!("Nets ({}):", lir.nets.len());
            for (i, net) in lir.nets.iter().enumerate() {
                println!(
                    "  Net {}: {} (width: {}, driver: {:?})",
                    i, net.id, net.width, net.driver
                );
                println!("    Loads: {:?}", net.loads);
            }

            // The issue: LIR generation creates very simple placeholder gates
            // The MIR->LIR conversion is simplified and doesn't properly
            // decompose complex expressions into proper gate networks

            println!("\n❗ ISSUE IDENTIFIED:");
            println!("The MIR to LIR transformation is creating simplified placeholder gates");
            println!("that don't represent the actual logic. When dead code elimination runs,");
            println!("it removes these placeholder gates because they're not properly connected");
            println!("to the output ports.");

            println!("\n🎯 What we actually need:");
            println!("1. Proper expression decomposition (a && b) -> AND gate with a,b inputs");
            println!("2. Correct net connectivity (gate outputs connect to port outputs)");
            println!("3. Output port marking so DCE knows what to preserve");
        }
    }

    #[test]
    fn test_realistic_area_measurement() {
        println!("🎯 Testing realistic area improvement measurement");

        // Create a design that should have measurable but reasonable optimization
        let source = r#"
        entity RealisticTest {
            in a: bool
            in b: bool
            in c: bool
            out result: bool
        }

        impl RealisticTest {
            assign result = a || b || c;  // Should create OR gates
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Should parse");
        let mir = lower_to_mir(&hir).expect("Should build MIR");

        if let Some(module) = mir.modules.first() {
            let lir = transform_mir_to_lir(module);

            println!("Generated LIR structure:");
            println!("  Gates: {}", lir.gates.len());
            println!("  Nets: {}", lir.nets.len());

            // Real area would be based on actual gate count and complexity
            // Current implementation is too simplified to give meaningful area metrics

            println!("\n📝 Conclusion:");
            println!("The current LIR generation creates placeholder structures.");
            println!("For realistic area optimization, we need:");
            println!("1. Proper gate decomposition of complex expressions");
            println!("2. Technology-specific gate libraries");
            println!("3. Area models based on actual gate sizes");

            // For now, let's measure what we can
            let has_structure = lir.gates.len() > 0 || lir.nets.len() > 0;
            println!("Infrastructure working: {}", has_structure);

            assert!(has_structure, "Should generate some LIR structure");
        }
    }
}
