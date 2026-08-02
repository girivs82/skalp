//! Proof of Concept: Method Call Support
//!
//! Tests the current state of method call infrastructure

#[cfg(test)]
#[allow(clippy::module_inception)]
mod test_method_call_poc {
    use skalp_frontend::parse_and_build_hir;

    #[test]
    fn test_method_call_parsing() {
        let skalp_code = r#"
    trait Numeric {
        fn add(self, other: Self) -> Self;
    }

    impl Numeric for nat[32] {
        fn add(self, other: Self) -> Self {
            return self + other
        }
    }

    fn test_method() -> nat[32] {
        let x = 5;
        let y = 3;
        return x.add(y)
    }
        "#;

        println!("\n=== Testing Method Call POC ===");
        println!("Code:\n{}", skalp_code);

        match parse_and_build_hir(skalp_code) {
            Ok(hir) => {
                println!("✅ Parsed successfully!");
                println!("Trait definitions: {}", hir.trait_definitions.len());
                println!("Trait implementations: {}", hir.trait_implementations.len());
                println!("Functions: {}", hir.functions.len());

                // Check trait definition
                if !hir.trait_definitions.is_empty() {
                    let trait_def = &hir.trait_definitions[0];
                    println!("\n📋 Trait: {}", trait_def.name);
                    println!("   Methods: {}", trait_def.methods.len());
                    for method in &trait_def.methods {
                        println!(
                            "     - {}({} params) -> {:?}",
                            method.name,
                            method.parameters.len(),
                            method.return_type
                        );
                    }
                }

                // Check trait implementation
                if !hir.trait_implementations.is_empty() {
                    println!("\n📦 Trait implementation found");
                    let trait_impl = &hir.trait_implementations[0];
                    println!("   Trait: {}", trait_impl.trait_name);
                    println!(
                        "   Method impls: {}",
                        trait_impl.method_implementations.len()
                    );
                }

                // Check function
                if !hir.functions.is_empty() {
                    let func = hir.functions.iter().find(|f| f.name == "test_method");

                    if let Some(func) = func {
                        println!("\n🔧 Function: {}", func.name);
                        println!("   Body statements: {}", func.body.len());

                        // Look for method call in function body
                        for stmt in &func.body {
                            println!("   Statement: {:?}", stmt);
                        }
                    }
                }

                println!("\n✅ Test complete - Check output for current capabilities");
            }
            Err(e) => {
                println!("❌ Failed to parse: {:?}", e);
                panic!("Parse error");
            }
        }
    }

    #[test]
    fn test_self_parameter() {
        let skalp_code = r#"
    trait Test {
        fn method(self) -> bit[32];
    }
        "#;

        println!("\n=== Testing Self Parameter ===");

        match parse_and_build_hir(skalp_code) {
            Ok(hir) => {
                println!("✅ Parsed trait with self parameter");
                if !hir.trait_definitions.is_empty() {
                    let method = &hir.trait_definitions[0].methods[0];
                    println!("Method: {}", method.name);
                    println!("Parameters: {}", method.parameters.len());
                    for param in &method.parameters {
                        println!("  - {}: {:?}", param.name, param.param_type);
                    }
                }
            }
            Err(e) => {
                println!("❌ Error: {:?}", e);
            }
        }
    }

    #[test]
    fn test_self_type() {
        let skalp_code = r#"
    trait Test {
        fn zero() -> Self;
        fn add(self, other: Self) -> Self;
    }
        "#;

        println!("\n=== Testing Self Type ===");

        match parse_and_build_hir(skalp_code) {
            Ok(hir) => {
                println!("✅ Parsed trait with Self type");
                if !hir.trait_definitions.is_empty() {
                    for method in &hir.trait_definitions[0].methods {
                        println!("Method: {}", method.name);
                        println!("  Return: {:?}", method.return_type);
                        println!("  Params: {}", method.parameters.len());
                    }
                }
            }
            Err(e) => {
                println!("❌ Error: {:?}", e);
            }
        }
    }
}
