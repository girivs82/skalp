#[cfg(test)]
mod sequential_debug_tests {
    #[cfg(target_os = "macos")]
    use skalp_frontend::parse_and_build_hir;
    #[cfg(target_os = "macos")]
    use skalp_mir::{MirCompiler, OptimizationLevel};
    #[cfg(target_os = "macos")]
    use skalp_sir::convert_mir_to_sir;

    #[test]
    #[cfg(target_os = "macos")]
    fn test_assignment_after_if_statement() {
        let simple_source = r#"
        entity TestSequential {
            in clk: clock
            in rst: bit
            out result: nat[8]
        }

        impl TestSequential {
            signal counter: nat[8] = 0
            signal pipeline_valid: nat[4] = 0

            on(clk.rise) {
                if (rst) {
                    counter <= 0
                } else {
                    counter <= counter + 1
                }

                // This assignment should be processed AFTER the if-statement
                pipeline_valid <= pipeline_valid + 1
            }

            result = counter
        }
        "#;

        println!("=== Testing assignment after if-statement ===");
        let hir = parse_and_build_hir(simple_source).expect("Failed to parse");

        // Compile to MIR
        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR");

        // Convert to SIR
        let sir = convert_mir_to_sir(&mir.modules[0]);

        println!("\n=== SIR Analysis ===");
        println!("Module: {}", sir.name);
        println!(
            "State elements: {:?}",
            sir.state_elements.keys().collect::<Vec<_>>()
        );
        println!("Combinational nodes: {}", sir.combinational_nodes.len());
        println!("Sequential nodes: {}", sir.sequential_nodes.len());

        // Generate and print Metal shader for debugging
        let shader_code = skalp_sir::generate_metal_shader(&sir);
        println!("\n=== Generated Metal Shader ===");
        println!("{}", &shader_code);

        // Check if pipeline_valid assignment is present and correct
        let has_pipeline_addition = shader_code.contains("pipeline_valid")
            && (shader_code.contains("+ uint(1)") || shader_code.contains("+ signals->"));

        if has_pipeline_addition {
            println!("\n✅ pipeline_valid assignment found and appears to have addition operation");
        } else {
            println!("\n❌ pipeline_valid assignment missing or incorrect");
            println!("   Looking for addition operation in Metal shader...");

            // Print lines containing pipeline_valid for debugging
            for line in shader_code.lines() {
                if line.contains("pipeline_valid") {
                    println!("   pipeline_valid line: {}", line.trim());
                }
            }
        }
    }
}
