#[cfg(test)]
mod enhanced_features_tests {
    use skalp_frontend::parse_and_build_hir;

    #[test]
    fn test_current_intent_syntax() {
        let source = r#"
        intent PerformanceGoal {
            timing: 10;
        }

        entity FastAdder {
            in clk: clock
            in a: nat[32]
            in b: nat[32]
            out sum: nat[32]
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse intent syntax");
        println!("Entities: {}", hir.entities.len());
        println!("Intents: {}", hir.intents.len());

        for intent in &hir.intents {
            println!(
                "Intent: {} with {} constraints",
                intent.name,
                intent.constraints.len()
            );
        }

        assert_eq!(hir.entities.len(), 1);
        assert_eq!(hir.intents.len(), 1, "Should have 1 intent declaration");
    }

    #[test]
    fn test_current_const_generics() {
        let source = r#"
        entity Buffer<const SIZE: nat[16]> {
            in clk: clock
            in data_in: nat[8]
            out data_out: nat[8]
        }

        impl Buffer<const SIZE: nat[16]> {
            signal buffer_mem: nat[8][SIZE] = [0; SIZE];

            on(clk.rise) {
                // Implement circular buffer logic
                data_out <= buffer_mem[0];
            }
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse const generics");
        println!("Entities: {}", hir.entities.len());
        println!("Implementations: {}", hir.implementations.len());

        let entity = &hir.entities[0];
        println!(
            "Entity: {} with {} generics",
            entity.name,
            entity.generics.len()
        );

        for generic in &entity.generics {
            println!("Generic: {} type: {:?}", generic.name, generic.param_type);
        }

        assert_eq!(hir.entities.len(), 1);
        assert_eq!(hir.implementations.len(), 1);
    }

    #[test]
    fn test_current_generic_constraints() {
        let source = r#"
        trait Sized {
            const WIDTH: nat[8];
        }

        entity Processor<T> where T: Sized {
            in clk: clock
            in data: T
            out result: T
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse generic constraints");
        println!("Entities: {}", hir.entities.len());
        println!("Trait definitions: {}", hir.trait_definitions.len());

        let entity = &hir.entities[0];
        println!(
            "Entity: {} with {} generics",
            entity.name,
            entity.generics.len()
        );

        assert_eq!(hir.entities.len(), 1);
        assert_eq!(hir.trait_definitions.len(), 1);
    }

    #[test]
    fn test_requirements_syntax() {
        let source = r#"
        requirement SecurityLevel {
            description: "Cryptographic security requirements";
            constraints: ["no_side_channels", "constant_time"];
        }

        entity AESCore {
            in clk: clock
            in key: nat[128]
            in plaintext: nat[128]
            out ciphertext: nat[128]
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse requirements");
        println!("Entities: {}", hir.entities.len());
        println!("Requirements: {}", hir.requirements.len());

        assert_eq!(hir.entities.len(), 1);
    }
}
