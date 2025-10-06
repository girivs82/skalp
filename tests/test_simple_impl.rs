#[cfg(test)]
mod simple_impl_tests {
    use skalp_frontend::parse_and_build_hir;

    #[test]
    #[ignore = "Parser fails on implementation syntax - implementation parsing incomplete"]
    fn test_simple_entity_impl() {
        let source = r#"
        entity Counter {
            in clk: clock
            out count: nat[8]
        }

        impl Counter {
            signal internal: nat[8] = 0;
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse entity impl");
        println!("Entity implementations: {}", hir.implementations.len());
        assert_eq!(hir.implementations.len(), 1);
    }

    #[test]
    #[ignore = "Trait implementation syntax not fully implemented"]
    fn test_trait_impl_syntax() {
        let source = r#"
        trait Test {
            fn test_method(&self);
        }

        entity TestEntity {
            in clk: clock
        }

        impl Test for TestEntity {
            fn test_method(&self) {}
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse trait impl");
        println!("Entities: {}", hir.entities.len());
        for entity in &hir.entities {
            println!("  Entity: {}", entity.name);
        }
        println!("Trait definitions: {}", hir.trait_definitions.len());
        for trait_def in &hir.trait_definitions {
            println!("  Trait: {}", trait_def.name);
        }
        println!("Trait implementations: {}", hir.trait_implementations.len());

        assert_eq!(hir.entities.len(), 1);
        assert_eq!(hir.trait_definitions.len(), 1);
        assert_eq!(hir.trait_implementations.len(), 1);
    }
}
