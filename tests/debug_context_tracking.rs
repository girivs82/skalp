#[cfg(test)]
mod context_debug_tests {
    #[cfg(target_os = "macos")]
    use skalp_frontend::parse_and_build_hir;
    #[cfg(target_os = "macos")]
    use skalp_mir::{MirCompiler, OptimizationLevel};
    #[cfg(target_os = "macos")]
    use skalp_sir::convert_mir_to_sir;

    #[test]
    #[cfg(target_os = "macos")]
    fn test_inter_signal_dependency() {
        let simple_source = r#"
        entity TestContext {
            in clk: clock
            in rst: bit
            in data_in: nat[8]
            out result: nat[8]
        }

        impl TestContext {
            signal temp_value: nat[8] = 0
            signal final_result: nat[8] = 0

            on(clk.rise) {
                if (rst) {
                    temp_value = 0
                    final_result = 0
                } else {
                    temp_value = data_in + 5
                    final_result = temp_value + 10
                }
            }

            result = final_result
        }
        "#;

        println!("=== Testing inter-signal dependency ===");
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

        // Generate and print Metal shader for debugging
        let shader_code = skalp_sir::generate_metal_shader(&sir);
        println!("\n=== Generated Metal Shader ===");
        println!("{}", &shader_code);

        // Check if final_result references temp_value correctly
        // It should NOT duplicate the data_in + 5 computation
        let has_duplicate_computation =
            shader_code.contains("data_in") && shader_code.matches("data_in").count() > 2; // Allow some references

        if has_duplicate_computation {
            println!("\n❌ Found duplicate computation - context tracking not working");
            // Print lines containing data_in for debugging
            for (i, line) in shader_code.lines().enumerate() {
                if line.contains("data_in") {
                    println!("   Line {}: {}", i + 1, line.trim());
                }
            }
        } else {
            println!("\n✅ Context tracking appears to be working correctly");
        }
    }
}
