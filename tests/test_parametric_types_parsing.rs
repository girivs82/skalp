// Test parsing of parametric types and intent parameters

use skalp_frontend::parse_and_build_hir;

#[test]
fn test_parse_intent_parameter() {
    let source = r#"
    entity Sqrt<const F: FloatFormat, intent I: Intent> {
        in x: fp<F>
        out result: fp<F>
    }

    impl<const F: FloatFormat, intent I> Sqrt<F, I> {
        result = x
    }
    "#;

    let hir = parse_and_build_hir(source).expect("Should parse intent parameter");
    assert!(!hir.entities.is_empty());

    let entity = &hir.entities[0];
    assert_eq!(entity.name, "Sqrt");
    assert_eq!(entity.generics.len(), 2);

    // Check const parameter
    assert_eq!(entity.generics[0].name, "F");
    assert!(matches!(
        entity.generics[0].param_type,
        skalp_frontend::hir::HirGenericType::Const(_)
    ));

    // Check intent parameter
    assert_eq!(entity.generics[1].name, "I");
    assert!(matches!(
        entity.generics[1].param_type,
        skalp_frontend::hir::HirGenericType::Intent
    ));
}

#[test]
fn test_parse_intent_with_default() {
    let source = r#"
    entity FMA<T, intent I: Intent> {
        in a: T
        in b: T
        in c: T
        out result: T
    }

    impl<T, intent I> FMA<T, I> {
        result = a * b + c
    }
    "#;

    let hir = parse_and_build_hir(source).expect("Should parse intent parameter");
    assert!(!hir.entities.is_empty());

    let entity = &hir.entities[0];
    assert_eq!(entity.generics.len(), 2);

    // Check intent parameter
    assert_eq!(entity.generics[1].name, "I");
    assert!(matches!(
        entity.generics[1].param_type,
        skalp_frontend::hir::HirGenericType::Intent
    ));
}

#[test]
fn test_parse_multiple_const_parameters() {
    let source = r#"
    entity Memory<const DEPTH: nat = 256, const WIDTH: nat = 32> {
        in addr: bit<clog2(DEPTH)>
        in data: bit<WIDTH>
        in we: bit
        in clk: clock
        out q: bit<WIDTH>
    }
    "#;

    let hir = parse_and_build_hir(source).expect("Should parse multiple const parameters");
    assert!(!hir.entities.is_empty());

    let entity = &hir.entities[0];
    assert_eq!(entity.name, "Memory");
    assert_eq!(entity.generics.len(), 2);

    // Check DEPTH parameter
    assert_eq!(entity.generics[0].name, "DEPTH");
    assert!(entity.generics[0].default_value.is_some());

    // Check WIDTH parameter
    assert_eq!(entity.generics[1].name, "WIDTH");
    assert!(entity.generics[1].default_value.is_some());
}

#[test]
fn test_parse_floatformat_const_parameter() {
    let source = r#"
    entity FpAdd<const F: FloatFormat> {
        in a: fp<F>
        in b: fp<F>
        out result: fp<F>
    }

    impl<const F: FloatFormat> FpAdd<F> {
        result = a + b
    }
    "#;

    let hir = parse_and_build_hir(source).expect("Should parse FloatFormat const parameter");
    assert!(!hir.entities.is_empty());

    let entity = &hir.entities[0];
    assert_eq!(entity.name, "FpAdd");
    assert_eq!(entity.generics.len(), 1);
    assert_eq!(entity.generics[0].name, "F");
}

#[test]
fn test_parse_mixed_generic_parameters() {
    let source = r#"
    entity Pipeline<const N: nat, intent DataFlow: Intent, intent Timing: Intent> {
        in data: bit<N>
        out result: bit<N>
    }

    impl<const N: nat, intent DataFlow, intent Timing> Pipeline<N, DataFlow, Timing> {
        result = data
    }
    "#;

    let hir = parse_and_build_hir(source).expect("Should parse mixed generic parameters");
    assert!(!hir.entities.is_empty());

    let entity = &hir.entities[0];
    assert_eq!(entity.name, "Pipeline");
    assert_eq!(entity.generics.len(), 3);

    // Check const parameter
    assert_eq!(entity.generics[0].name, "N");
    assert!(matches!(
        entity.generics[0].param_type,
        skalp_frontend::hir::HirGenericType::Const(_)
    ));

    // Check first intent parameter
    assert_eq!(entity.generics[1].name, "DataFlow");
    assert!(matches!(
        entity.generics[1].param_type,
        skalp_frontend::hir::HirGenericType::Intent
    ));

    // Check second intent parameter
    assert_eq!(entity.generics[2].name, "Timing");
    assert!(matches!(
        entity.generics[2].param_type,
        skalp_frontend::hir::HirGenericType::Intent
    ));
}
