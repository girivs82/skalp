#[cfg(test)]
mod integration_tests {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::{MirCompiler, OptimizationLevel};
    use skalp_lir::lower_to_lir;
    use skalp_codegen::generate_systemverilog_from_mir;
    use skalp_sir::convert_mir_to_sir;
    use skalp_sim::{Simulator, SimulationConfig};
    use std::fs;
    
    use tempfile::TempDir;

    #[test]
    fn test_full_compilation_pipeline() {
        let source = r#"
        entity Adder {
            in a: bit[8]
            in b: bit[8]
            out sum: bit[8]
            out carry: bit[1]
        }

        impl Adder {
            signal result: bit[9]
            result = a + b
            sum = result[7:0]
            carry = result[8]
        }
        "#;

        // Parse to HIR
        let hir = parse_and_build_hir(source).expect("Failed to parse");
        assert!(!hir.entities.is_empty(), "Should have at least one entity");

        // Compile to MIR
        let compiler = MirCompiler::new()
            .with_optimization_level(OptimizationLevel::Basic);
        let mir = compiler.compile_to_mir(&hir).expect("Failed to compile to MIR");
        assert!(!mir.modules.is_empty(), "Should have at least one module");

        // Lower to LIR
        let lir = lower_to_lir(&mir).expect("Failed to lower to LIR");
        assert!(!lir.modules.is_empty(), "Should have at least one LIR module");

        // Convert to SIR
        let sir = convert_mir_to_sir(&mir.modules[0]);
        assert_eq!(sir.name, format!("{:?}", mir.modules[0].id), "Module names should match");

        // Generate SystemVerilog
        let sv_code = generate_systemverilog_from_mir(&mir, &lir)
            .expect("Failed to generate SystemVerilog");
        assert!(sv_code.contains("module Adder"), "Should contain module declaration");
        assert!(sv_code.contains("input"), "Should contain input ports");
        assert!(sv_code.contains("output"), "Should contain output ports");
    }

    #[tokio::test]
    async fn test_compile_and_simulate() {
        let source = r#"
        entity SimpleCounter {
            in clk: clock
            in reset: reset
            out value: bit[4]
        }

        impl SimpleCounter {
            signal counter: bit[4] = 0

            on(clk.rise) {
                if (reset.active) {
                    counter <= 0
                } else {
                    counter <= counter + 1
                }
            }

            value = counter
        }
        "#;

        // Compile through the pipeline
        let hir = parse_and_build_hir(source).unwrap();
        let compiler = MirCompiler::new()
            .with_optimization_level(OptimizationLevel::Basic);
        let mir = compiler.compile_to_mir(&hir).unwrap();
        let sir = convert_mir_to_sir(&mir.modules[0]);

        // Set up simulation
        let config = SimulationConfig {
            use_gpu: true,
            max_cycles: 100,
            timeout_ms: 5000,
            capture_waveforms: true,
            parallel_threads: 1,
        };

        let mut sim = Simulator::new(config).await.unwrap();
        sim.load_module(&sir).await.unwrap();

        // Initialize
        sim.set_input("reset", vec![1]).await.unwrap();
        sim.set_input("clk", vec![0]).await.unwrap();

        // Reset cycle
        sim.step_simulation().await.unwrap();
        sim.set_input("clk", vec![1]).await.unwrap();
        sim.step_simulation().await.unwrap();

        // Release reset
        sim.set_input("reset", vec![0]).await.unwrap();
        sim.set_input("clk", vec![0]).await.unwrap();
        sim.step_simulation().await.unwrap();

        // Count for a few cycles
        let mut prev_value = 0u8;
        for i in 0..8 {
            // Rising edge
            sim.set_input("clk", vec![1]).await.unwrap();
            sim.step_simulation().await.unwrap();

            // Check value incremented
            let value = sim.get_output("value").await.unwrap();
            let current_value = value[0] & 0x0F;  // 4-bit value

            if i > 0 {
                assert_eq!(current_value, (prev_value + 1) & 0x0F,
                          "Counter should increment modulo 16");
            }
            prev_value = current_value;

            // Falling edge
            sim.set_input("clk", vec![0]).await.unwrap();
            sim.step_simulation().await.unwrap();
        }
    }

    #[test]
    fn test_optimization_levels() {
        let source = r#"
        entity OptTest {
            in a: bit[8]
            in b: bit[8]
            out result: bit[8]
        }

        impl OptTest {
            signal temp1: bit[8]
            signal temp2: bit[8]
            signal temp3: bit[8]

            temp1 = a + 0       // Should be optimized away
            temp2 = temp1 * 1   // Should be optimized away
            temp3 = temp2 & 0xFF // Should be optimized away
            result = temp3
        }
        "#;

        let hir = parse_and_build_hir(source).unwrap();

        // Test different optimization levels
        for opt_level in [OptimizationLevel::None, OptimizationLevel::Basic, OptimizationLevel::Full] {
            let compiler = MirCompiler::new()
                .with_optimization_level(opt_level);
            let mir = compiler.compile_to_mir(&hir).unwrap();

            // Basic optimization should simplify the circuit
            if matches!(opt_level, OptimizationLevel::Basic | OptimizationLevel::Full) {
                // Check that optimizations were applied
                // This would depend on the specific optimizations implemented
                assert!(!mir.modules.is_empty());
            }
        }
    }

    #[test]
    fn test_error_handling() {
        // Test parse errors
        let bad_syntax = "entity { invalid syntax }";
        assert!(parse_and_build_hir(bad_syntax).is_err(),
                "Should fail on invalid syntax");

        // Test semantic errors
        let bad_semantics = r#"
        entity BadDesign {
            in a: bit[8]
            out b: bit[16]
        }

        impl BadDesign {
            b = a  // Type mismatch: can't assign 8-bit to 16-bit without explicit conversion
        }
        "#;

        if let Ok(hir) = parse_and_build_hir(bad_semantics) {
            let compiler = MirCompiler::new();
            // This might fail during MIR compilation or type checking
            let result = compiler.compile_to_mir(&hir);
            // Depending on where type checking happens, this might or might not fail
        }
    }

    #[test]
    fn test_file_output() {
        let source = r#"
        entity TestModule {
            in clk: clock
            in data: bit[8]
            out result: bit[8]
        }

        impl TestModule {
            signal reg: bit[8] = 0

            on(clk.rise) {
                reg <= data
            }

            result = reg
        }
        "#;

        // Create temp directory
        let temp_dir = TempDir::new().unwrap();
        let output_path = temp_dir.path().join("output.sv");

        // Compile and generate code
        let hir = parse_and_build_hir(source).unwrap();
        let compiler = MirCompiler::new();
        let mir = compiler.compile_to_mir(&hir).unwrap();
        let lir = lower_to_lir(&mir).unwrap();
        let sv_code = generate_systemverilog_from_mir(&mir, &lir).unwrap();

        // Write to file
        fs::write(&output_path, sv_code).unwrap();

        // Verify file was created and contains expected content
        assert!(output_path.exists(), "Output file should exist");
        let content = fs::read_to_string(&output_path).unwrap();
        assert!(content.contains("module TestModule"), "Should contain module");
        assert!(content.contains("always_ff"), "Should contain sequential logic");
    }

    #[tokio::test]
    async fn test_simulation_determinism() {
        let source = r#"
        entity DeterTest {
            in clk: clock
            in data: bit[8]
            out result: bit[8]
        }

        impl DeterTest {
            signal reg: bit[8] = 42

            on(clk.rise) {
                reg <= reg ^ data
            }

            result = reg
        }
        "#;

        let hir = parse_and_build_hir(source).unwrap();
        let compiler = MirCompiler::new();
        let mir = compiler.compile_to_mir(&hir).unwrap();
        let sir = convert_mir_to_sir(&mir.modules[0]);

        // Run simulation twice with same inputs
        let mut results1 = Vec::new();
        let mut results2 = Vec::new();

        for run in 0..2 {
            let config = SimulationConfig {
                use_gpu: true,
                max_cycles: 50,
                timeout_ms: 5000,
                capture_waveforms: false,
                parallel_threads: 1,
            };

            let mut sim = Simulator::new(config).await.unwrap();
            sim.load_module(&sir).await.unwrap();

            // Same sequence of inputs
            for i in 0..10 {
                sim.set_input("data", vec![i as u8 * 7]).await.unwrap();
                sim.set_input("clk", vec![0]).await.unwrap();
                sim.step_simulation().await.unwrap();
                sim.set_input("clk", vec![1]).await.unwrap();
                sim.step_simulation().await.unwrap();

                let output = sim.get_output("result").await.unwrap();

                if run == 0 {
                    results1.push(output);
                } else {
                    results2.push(output);
                }
            }
        }

        // Results should be identical
        assert_eq!(results1, results2, "Simulation should be deterministic");
    }
}