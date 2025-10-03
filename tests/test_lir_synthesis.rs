#[cfg(test)]
mod lir_synthesis_tests {
    use skalp_frontend::parse_and_build_hir;
    use skalp_lir::{lower_to_lir, transform_mir_to_lir};
    use skalp_mir::lower_to_mir;

    #[test]
    fn test_basic_mir_to_lir_conversion() {
        let source = r#"
        entity SimpleLogic {
            in a: bool
            in b: bool
            out y: bool
        }

        impl SimpleLogic {
            assign y = a && b;
        }
        "#;

        // Parse to HIR
        let hir = parse_and_build_hir(source).expect("Should parse");

        // Build MIR
        let mir = lower_to_mir(&hir).expect("Should build MIR");

        // Convert to LIR using the high-level API
        let lir_design = lower_to_lir(&mir).expect("Should convert to LIR");

        println!("✅ Basic MIR to LIR conversion successful!");
        println!("Design: {}", lir_design.name);
        println!("Modules: {}", lir_design.modules.len());

        // Verify we have expected structure
        assert!(!lir_design.modules.is_empty(), "Should have modules");

        let module = &lir_design.modules[0];
        println!("Module: {}", module.name);
        println!("Gates: {}", module.gates.len());
        println!("Nets: {}", module.nets.len());
        println!("Signals: {}", module.signals.len());

        // Should have signals for the ports
        assert!(
            module.signals.len() >= 3,
            "Should have at least 3 signals (a, b, y)"
        );

        // Check that we have input and output signals
        let input_signals: Vec<_> = module.signals.iter().filter(|s| s.is_input).collect();
        let output_signals: Vec<_> = module.signals.iter().filter(|s| s.is_output).collect();

        assert_eq!(input_signals.len(), 2, "Should have 2 input signals");
        assert_eq!(output_signals.len(), 1, "Should have 1 output signal");
    }

    #[test]
    fn test_module_level_transformation() {
        let source = r#"
        entity Counter {
            in clk: clock
            in reset: bool
            in enable: bool
            out count: nat[4]
        }

        impl Counter {
            signal count_reg: nat[4] = 0;

            on(clk.rise) {
                if reset {
                    count_reg <= 0;
                } else if enable {
                    count_reg <= count_reg + 1;
                }
            }

            assign count = count_reg;
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Should parse");
        let mir = lower_to_mir(&hir).expect("Should build MIR");

        // Test the module-level transformation directly
        if let Some(module) = mir.modules.first() {
            let lir = transform_mir_to_lir(module);

            println!("✅ Module-level transformation successful!");
            println!("Module: {}", lir.name);
            println!("Gates: {}", lir.gates.len());
            println!("Nets: {}", lir.nets.len());

            // Should have created some gates for the logic
            // Even with simplified implementation, should have at least buffer gates
            assert!(!lir.nets.is_empty(), "Should have created nets");
        }
    }

    #[test]
    fn test_lir_design_structure() {
        let source = r#"
        entity TestDesign {
            in data_in: nat[8]
            in valid: bool
            out data_out: nat[8]
            out ready: bool
        }

        impl TestDesign {
            signal buffer: nat[8] = 0;

            assign data_out = buffer;
            assign ready = valid;
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Should parse");
        let mir = lower_to_mir(&hir).expect("Should build MIR");
        let lir_design = lower_to_lir(&mir).expect("Should convert to LIR");

        println!("✅ LIR design structure test successful!");
        println!("Design: {}", lir_design.name);
        println!("Modules: {}", lir_design.modules.len());

        // Test design structure
        assert_eq!(lir_design.name, "TestDesign");
        assert!(!lir_design.modules.is_empty(), "Should have modules");

        // Test module structure
        let module = &lir_design.modules[0];
        println!("Module signals:");
        for signal in &module.signals {
            println!(
                "  - {} (input: {}, output: {}, register: {})",
                signal.name, signal.is_input, signal.is_output, signal.is_register
            );
        }

        // Should have proper signal classification
        let input_count = module.signals.iter().filter(|s| s.is_input).count();
        let output_count = module.signals.iter().filter(|s| s.is_output).count();
        let register_count = module.signals.iter().filter(|s| s.is_register).count();

        assert!(input_count >= 2, "Should have input signals");
        assert!(output_count >= 2, "Should have output signals");
        assert!(register_count >= 1, "Should have register signals");
    }

    #[test]
    fn test_multiple_modules() {
        // Test with a design that might have multiple modules
        let source = r#"
        entity SimpleEntity {
            in x: bool
            out y: bool
        }

        impl SimpleEntity {
            assign y = x;
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Should parse");
        let mir = lower_to_mir(&hir).expect("Should build MIR");
        let lir_design = lower_to_lir(&mir).expect("Should convert to LIR");

        println!("✅ Multiple modules test successful!");
        println!("Design: {}", lir_design.name);
        println!("Modules: {}", lir_design.modules.len());

        // Even simple designs should create at least one module
        assert!(
            !lir_design.modules.is_empty(),
            "Should have at least one module"
        );

        for (i, module) in lir_design.modules.iter().enumerate() {
            println!(
                "Module {}: {} (signals: {}, gates: {}, nets: {})",
                i,
                module.name,
                module.signals.len(),
                module.gates.len(),
                module.nets.len()
            );
        }
    }

    #[test]
    fn test_gate_creation() {
        let source = r#"
        entity GateTest {
            in a: bool
            in b: bool
            in c: bool
            out result: bool
        }

        impl GateTest {
            assign result = a || (b && c);
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Should parse");
        let mir = lower_to_mir(&hir).expect("Should build MIR");

        // Test the module transformation to see gate creation
        if let Some(module) = mir.modules.first() {
            let lir = transform_mir_to_lir(module);

            println!("✅ Gate creation test successful!");
            println!("Gates created: {}", lir.gates.len());
            println!("Nets created: {}", lir.nets.len());

            // Print out the gates that were created
            for (i, gate) in lir.gates.iter().enumerate() {
                println!(
                    "Gate {}: {} ({:?}) - inputs: {}, outputs: {}",
                    i,
                    gate.id,
                    gate.gate_type,
                    gate.inputs.len(),
                    gate.outputs.len()
                );
            }

            // Print out the nets
            for (i, net) in lir.nets.iter().enumerate() {
                println!("Net {}: {} (width: {})", i, net.id, net.width);
            }

            // Should have created some structure
            assert!(
                !lir.gates.is_empty() || !lir.nets.is_empty(),
                "Should have created gates or nets"
            );
        }
    }
}
