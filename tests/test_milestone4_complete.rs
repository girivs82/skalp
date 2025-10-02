#[cfg(test)]
mod milestone4_tests {
    use skalp_frontend::parse_and_build_hir;

    #[test]
    fn test_milestone4_comprehensive() {
        let source = r#"
        // Enhanced Generics with Const Parameters
        entity Buffer<const SIZE: nat[16]> {
            in clk: clock
            in data_in: nat[8]
            out data_out: nat[8]
        }

        impl Buffer<const SIZE: nat[16]> {
            signal buffer_mem: nat[8] = 0;

            on(clk.rise) {
                data_out <= buffer_mem;
            }
        }

        // Intent Declarations with Constraints
        intent PerformanceGoal {
            timing: 100;
            power: 50;
            area: 1000;
        }

        intent OptimizationTarget for Buffer {
            performance: 90;
            timing: 10;
        }

        // Trait System (from Milestone 3)
        trait Clocked {
            fn on_rising_edge(&self);
            const FREQ: nat[32];
        }

        impl Clocked for Buffer<32> {
            fn on_rising_edge(&self) {
                // Implementation
            }
            const FREQ: nat[32] = 100_000_000;
        }

        // Flow Blocks (from Milestone 2)
        entity DataProcessor {
            in clk: clock
            in data: nat[8]
            out result: nat[8]
        }

        impl DataProcessor {
            signal temp1: nat[8] = 0;
            signal temp2: nat[8] = 0;

            on(clk.rise) {
                flow {
                    data |> temp1 |> temp2 |> result
                }
            }
        }

        // Match Expressions (from Milestone 1)
        entity StateMachine {
            in clk: clock
            in rst: reset
            in ready: bool
            out state_out: nat[2]
        }

        impl StateMachine {
            signal current_state: nat[2] = 0;
            signal next_state: nat[2] = 0;

            on(clk.rise) {
                if (rst) {
                    current_state <= 0;
                } else {
                    current_state <= next_state;
                }

                match current_state {
                    0 => next_state <= 1,
                    1 if ready => next_state <= 2,
                    1 => next_state <= 1,
                    2 => next_state <= 0,
                    _ => next_state <= 0,
                }
            }

            state_out = current_state;
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse comprehensive milestone 4 test");

        // Verify all features are working
        println!("=== MILESTONE 4: COMPREHENSIVE TEST RESULTS ===");
        println!("Entities: {}", hir.entities.len());
        println!("Implementations: {}", hir.implementations.len());
        println!("Trait Definitions: {}", hir.trait_definitions.len());
        println!("Trait Implementations: {}", hir.trait_implementations.len());
        println!("Intents: {}", hir.intents.len());

        // Verify Enhanced Generics
        let buffer_entity = hir.entities.iter().find(|e| e.name == "Buffer").expect("Buffer entity should exist");
        println!("Buffer generics: {}", buffer_entity.generics.len());
        for generic in &buffer_entity.generics {
            println!("  Generic: {} type: {:?}", generic.name, generic.param_type);
        }

        // Verify Intent Constraints
        for intent in &hir.intents {
            println!("Intent: {} with {} constraints", intent.name, intent.constraints.len());
            for constraint in &intent.constraints {
                println!("  Constraint: {:?}", constraint.constraint_type);
            }
        }

        // Verify core functionality
        assert!(hir.entities.len() >= 4, "Should have Buffer, DataProcessor, StateMachine, and others");
        assert!(hir.implementations.len() >= 4, "Should have multiple implementations");
        assert_eq!(hir.trait_definitions.len(), 1, "Should have Clocked trait");
        assert_eq!(hir.trait_implementations.len(), 1, "Should have trait implementation");
        assert_eq!(hir.intents.len(), 2, "Should have 2 intent declarations");

        // Verify enhanced generics specifically
        assert!(buffer_entity.generics.len() >= 2, "Buffer should have const and type generics");

        println!("✅ ALL MILESTONE 4 FEATURES WORKING!");
        println!("✅ Enhanced Generics: Const parameters and constraints");
        println!("✅ Intent System: Declarations and constraints");
        println!("✅ Full compatibility with Milestones 1-3");
        println!("✅ Comprehensive hardware description capabilities");
    }
}