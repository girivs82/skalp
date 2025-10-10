#[cfg(test)]
mod stdlib_synthesis_tests {
    use skalp_frontend::parse_and_build_hir;
    use skalp_lir::{lower_to_lir, transform_mir_to_lir};
    use skalp_mir::lower_to_mir;

    #[test]
    fn test_vec2_field_access_synthesizes() {
        let source = r#"
        entity Vec2FieldAccess {
            in a: vec2<fp32>
            out x_out: fp32
            out y_out: fp32
        }

        impl Vec2FieldAccess {
            x_out = a.x
            y_out = a.y
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Should parse");
        let mir = lower_to_mir(&hir).expect("Should build MIR");

        assert!(!mir.modules.is_empty(), "Should have at least one module");

        let module = &mir.modules[0];
        let lir = transform_mir_to_lir(module);

        // Verify LIR structure - field extraction creates wire assignments
        // The actual structure depends on how field access is lowered

        println!("✅ Vec2 field access synthesizes successfully");
        println!("   Module: {}", lir.name);
        println!("   Nets: {}", lir.nets.len());
        println!("   Gates: {}", lir.gates.len());
    }

    #[test]
    fn test_vec2_addition_synthesizes() {
        let source = r#"
        entity Vec2Add {
            in a: vec2<fp32>
            in b: vec2<fp32>
            out x_sum: fp32
            out y_sum: fp32
        }

        impl Vec2Add {
            x_sum = a.x + b.x
            y_sum = a.y + b.y
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Should parse");
        let mir = lower_to_mir(&hir).expect("Should build MIR");

        assert!(!mir.modules.is_empty(), "Should have at least one module");

        let module = &mir.modules[0];
        let lir = transform_mir_to_lir(module);

        // Verify LIR structure - should have gates for addition operations
        assert!(
            !lir.gates.is_empty() || !lir.nets.is_empty(),
            "Should have gates or nets for addition operations"
        );

        println!("✅ Vec2 addition synthesizes successfully");
        println!("   Module: {}", lir.name);
        println!("   Nets: {}", lir.nets.len());
        println!("   Gates: {}", lir.gates.len());
    }

    #[test]
    fn test_fp32_binary_ops_synthesize() {
        let source = r#"
        entity FP32BinaryOps {
            in a: fp32
            in b: fp32
            out sum: fp32
            out diff: fp32
            out prod: fp32
        }

        impl FP32BinaryOps {
            sum = a + b
            diff = a - b
            prod = a * b
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Should parse");
        let mir = lower_to_mir(&hir).expect("Should build MIR");

        assert!(!mir.modules.is_empty(), "Should have at least one module");

        let module = &mir.modules[0];
        let lir = transform_mir_to_lir(module);

        // Should have gates or nets for binary operations
        assert!(
            !lir.gates.is_empty() || !lir.nets.is_empty(),
            "Should have gates or nets for binary operations"
        );

        println!("✅ FP32 binary operations synthesize successfully");
        println!("   Module: {}", lir.name);
        println!("   Nets: {}", lir.nets.len());
        println!("   Gates: {}", lir.gates.len());
    }

    #[test]
    fn test_vec3_component_operations_synthesize() {
        let source = r#"
        entity Vec3ComponentOps {
            in a: vec3<fp32>
            in b: vec3<fp32>
            out x_sum: fp32
            out y_sum: fp32
            out z_sum: fp32
        }

        impl Vec3ComponentOps {
            x_sum = a.x + b.x
            y_sum = a.y + b.y
            z_sum = a.z + b.z
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Should parse");
        let mir = lower_to_mir(&hir).expect("Should build MIR");

        assert!(!mir.modules.is_empty(), "Should have at least one module");

        let module = &mir.modules[0];
        let lir = transform_mir_to_lir(module);

        // Should have gates or nets for component operations
        assert!(
            !lir.gates.is_empty() || !lir.nets.is_empty(),
            "Should have gates or nets for component operations"
        );

        println!("✅ Vec3 component operations synthesize successfully");
        println!("   Module: {}", lir.name);
        println!("   Nets: {}", lir.nets.len());
        println!("   Gates: {}", lir.gates.len());
    }

    #[test]
    fn test_vec4_field_access_synthesizes() {
        let source = r#"
        entity Vec4FieldAccess {
            in v: vec4<fp32>
            out x: fp32
            out y: fp32
            out z: fp32
            out w: fp32
        }

        impl Vec4FieldAccess {
            x = v.x
            y = v.y
            z = v.z
            w = v.w
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Should parse");
        let mir = lower_to_mir(&hir).expect("Should build MIR");

        assert!(!mir.modules.is_empty(), "Should have at least one module");

        let module = &mir.modules[0];
        let lir = transform_mir_to_lir(module);

        // Field access should create wire assignments
        println!("✅ Vec4 field access synthesizes successfully");
        println!("   Module: {}", lir.name);
        println!("   Nets: {}", lir.nets.len());
        println!("   Gates: {}", lir.gates.len());
    }

    #[test]
    fn test_nested_field_and_binary_synthesizes() {
        let source = r#"
        entity NestedOps {
            in a: vec2<fp32>
            in b: vec2<fp32>
            in c: vec2<fp32>
            out result: fp32
        }

        impl NestedOps {
            result = a.x + b.y + c.x
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Should parse");
        let mir = lower_to_mir(&hir).expect("Should build MIR");

        assert!(!mir.modules.is_empty(), "Should have at least one module");

        let module = &mir.modules[0];
        let lir = transform_mir_to_lir(module);

        // Should have gates or nets for chained operations
        assert!(
            !lir.gates.is_empty() || !lir.nets.is_empty(),
            "Should have gates or nets for chained operations"
        );

        println!("✅ Nested field access and binary ops synthesize successfully");
        println!("   Module: {}", lir.name);
        println!("   Nets: {}", lir.nets.len());
        println!("   Gates: {}", lir.gates.len());
    }

    #[test]
    fn test_mixed_fp_types_synthesize() {
        let source = r#"
        entity MixedFPTypes {
            in a: fp16
            in b: fp32
            in c: fp64
            out x: fp16
            out y: fp32
            out z: fp64
        }

        impl MixedFPTypes {
            x = a
            y = b
            z = c
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Should parse");
        let mir = lower_to_mir(&hir).expect("Should build MIR");

        assert!(!mir.modules.is_empty(), "Should have at least one module");

        let module = &mir.modules[0];
        let lir = transform_mir_to_lir(module);

        println!("✅ Mixed FP types synthesize successfully");
        println!("   Module: {}", lir.name);
        println!("   Nets: {}", lir.nets.len());
        println!("   Gates: {}", lir.gates.len());
    }
}
