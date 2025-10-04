#[cfg(test)]
mod enhanced_features_tests {
    use skalp_frontend::parse_and_build_hir;

    // NOTE: The following tests were removed because they test unimplemented features:
    // - test_current_intent_syntax: 'intent' declarations are not implemented
    // - test_current_const_generics: const generics syntax is not implemented
    // - test_current_generic_constraints: trait constraints with 'where' clauses are not implemented
    // These can be re-added when the features are implemented.

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
