//! Tests for constant evaluation in SKALP
//!
//! Tests cover both generic and non-generic const evaluation paths

use skalp_frontend::const_eval::{ConstEvaluator, ConstValue};
use skalp_frontend::hir::{
    HirBinaryExpr, HirBinaryOp, HirEnumType, HirEnumVariant, HirExpression, HirLiteral, HirType,
    ImplStyle,
};

#[test]
fn test_const_literal_evaluation() {
    let mut eval = ConstEvaluator::new();

    // Test integer literal
    let expr = HirExpression::Literal(HirLiteral::Integer(42));
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(42));

    // Test boolean literal
    let expr = HirExpression::Literal(HirLiteral::Boolean(true));
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Bool(true));
}

#[test]
fn test_const_arithmetic() {
    let mut eval = ConstEvaluator::new();

    // Test 2 + 3 = 5
    let expr = HirExpression::Binary(HirBinaryExpr {
        op: HirBinaryOp::Add,
        left: Box::new(HirExpression::Literal(HirLiteral::Integer(2))),
        right: Box::new(HirExpression::Literal(HirLiteral::Integer(3))),
    });
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(5));

    // Test 10 * 5 = 50
    let expr = HirExpression::Binary(HirBinaryExpr {
        op: HirBinaryOp::Mul,
        left: Box::new(HirExpression::Literal(HirLiteral::Integer(10))),
        right: Box::new(HirExpression::Literal(HirLiteral::Integer(5))),
    });
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(50));

    // Test 20 - 5 = 15
    let expr = HirExpression::Binary(HirBinaryExpr {
        op: HirBinaryOp::Sub,
        left: Box::new(HirExpression::Literal(HirLiteral::Integer(20))),
        right: Box::new(HirExpression::Literal(HirLiteral::Integer(5))),
    });
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(15));
}

#[test]
fn test_const_generic_param() {
    let mut eval = ConstEvaluator::new();
    eval.bind("WIDTH".to_string(), ConstValue::Nat(8));

    // Test WIDTH + 1 = 9
    let expr = HirExpression::Binary(HirBinaryExpr {
        op: HirBinaryOp::Add,
        left: Box::new(HirExpression::GenericParam("WIDTH".to_string())),
        right: Box::new(HirExpression::Literal(HirLiteral::Integer(1))),
    });
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(9));
}

#[test]
fn test_builtin_clog2() {
    let mut eval = ConstEvaluator::new();

    // Test clog2(1024) = 10
    let expr = HirExpression::Call(skalp_frontend::hir::HirCallExpr {
        function: "clog2".to_string(),
        type_args: vec![],
        args: vec![HirExpression::Literal(HirLiteral::Integer(1024))],
        named_type_args: std::collections::HashMap::new(),
        impl_style: ImplStyle::default(),
    });
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(10));

    // Test clog2(1) = 0
    let expr = HirExpression::Call(skalp_frontend::hir::HirCallExpr {
        function: "clog2".to_string(),
        type_args: vec![],
        args: vec![HirExpression::Literal(HirLiteral::Integer(1))],
        named_type_args: std::collections::HashMap::new(),
        impl_style: ImplStyle::default(),
    });
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(0));

    // Test clog2(7) = 3
    let expr = HirExpression::Call(skalp_frontend::hir::HirCallExpr {
        function: "clog2".to_string(),
        type_args: vec![],
        args: vec![HirExpression::Literal(HirLiteral::Integer(7))],
        named_type_args: std::collections::HashMap::new(),
        impl_style: ImplStyle::default(),
    });
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(3));
}

#[test]
fn test_builtin_pow2() {
    let mut eval = ConstEvaluator::new();

    // Test pow2(3) = 8
    let expr = HirExpression::Call(skalp_frontend::hir::HirCallExpr {
        function: "pow2".to_string(),
        type_args: vec![],
        args: vec![HirExpression::Literal(HirLiteral::Integer(3))],
        named_type_args: std::collections::HashMap::new(),
        impl_style: ImplStyle::default(),
    });
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(8));

    // Test pow2(0) = 1
    let expr = HirExpression::Call(skalp_frontend::hir::HirCallExpr {
        function: "pow2".to_string(),
        type_args: vec![],
        args: vec![HirExpression::Literal(HirLiteral::Integer(0))],
        named_type_args: std::collections::HashMap::new(),
        impl_style: ImplStyle::default(),
    });
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(1));
}

#[test]
fn test_builtin_max_min() {
    let mut eval = ConstEvaluator::new();

    // Test max(5, 10) = 10
    let expr = HirExpression::Call(skalp_frontend::hir::HirCallExpr {
        function: "max".to_string(),
        type_args: vec![],
        args: vec![
            HirExpression::Literal(HirLiteral::Integer(5)),
            HirExpression::Literal(HirLiteral::Integer(10)),
        ],
        named_type_args: std::collections::HashMap::new(),
        impl_style: ImplStyle::default(),
    });
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(10));

    // Test min(5, 10) = 5
    let expr = HirExpression::Call(skalp_frontend::hir::HirCallExpr {
        function: "min".to_string(),
        type_args: vec![],
        args: vec![
            HirExpression::Literal(HirLiteral::Integer(5)),
            HirExpression::Literal(HirLiteral::Integer(10)),
        ],
        named_type_args: std::collections::HashMap::new(),
        impl_style: ImplStyle::default(),
    });
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(5));
}

#[test]
fn test_builtin_popcount() {
    let mut eval = ConstEvaluator::new();

    // Test popcount(7) = 3 (binary: 111)
    let expr = HirExpression::Call(skalp_frontend::hir::HirCallExpr {
        function: "popcount".to_string(),
        type_args: vec![],
        args: vec![HirExpression::Literal(HirLiteral::Integer(7))],
        named_type_args: std::collections::HashMap::new(),
        impl_style: ImplStyle::default(),
    });
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(3));

    // Test popcount(15) = 4 (binary: 1111)
    let expr = HirExpression::Call(skalp_frontend::hir::HirCallExpr {
        function: "popcount".to_string(),
        type_args: vec![],
        args: vec![HirExpression::Literal(HirLiteral::Integer(15))],
        named_type_args: std::collections::HashMap::new(),
        impl_style: ImplStyle::default(),
    });
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(4));
}

#[test]
fn test_builtin_is_power_of_2() {
    let mut eval = ConstEvaluator::new();

    // Test is_power_of_2(8) = true
    let expr = HirExpression::Call(skalp_frontend::hir::HirCallExpr {
        function: "is_power_of_2".to_string(),
        type_args: vec![],
        args: vec![HirExpression::Literal(HirLiteral::Integer(8))],
        named_type_args: std::collections::HashMap::new(),
        impl_style: ImplStyle::default(),
    });
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Bool(true));

    // Test is_power_of_2(7) = false
    let expr = HirExpression::Call(skalp_frontend::hir::HirCallExpr {
        function: "is_power_of_2".to_string(),
        type_args: vec![],
        args: vec![HirExpression::Literal(HirLiteral::Integer(7))],
        named_type_args: std::collections::HashMap::new(),
        impl_style: ImplStyle::default(),
    });
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Bool(false));
}

#[test]
fn test_builtin_gray_code() {
    let mut eval = ConstEvaluator::new();

    // Test gray_encode(5) = 7 (binary: 101 -> 111)
    let expr = HirExpression::Call(skalp_frontend::hir::HirCallExpr {
        function: "gray_encode".to_string(),
        type_args: vec![],
        args: vec![HirExpression::Literal(HirLiteral::Integer(5))],
        named_type_args: std::collections::HashMap::new(),
        impl_style: ImplStyle::default(),
    });
    let encoded = eval.eval(&expr).unwrap();
    assert_eq!(encoded, ConstValue::Nat(7));

    // Test gray_decode(7) = 5 (reverse of above)
    let expr = HirExpression::Call(skalp_frontend::hir::HirCallExpr {
        function: "gray_decode".to_string(),
        type_args: vec![],
        args: vec![HirExpression::Literal(HirLiteral::Integer(7))],
        named_type_args: std::collections::HashMap::new(),
        impl_style: ImplStyle::default(),
    });
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(5));
}

/// Test enum variant const evaluation
/// Tests that enum variants can be evaluated as const expressions
/// when used as generic const parameters
#[test]
fn test_enum_variant_evaluation() {
    let mut eval = ConstEvaluator::new();

    // Create an enum type with explicit values
    // pub enum Mode : bit[2] { Fast = 0, Normal = 1, Slow = 2 }
    let mode_enum = HirEnumType {
        name: "Mode".to_string(),
        variants: vec![
            HirEnumVariant {
                name: "Fast".to_string(),
                value: Some(HirExpression::Literal(HirLiteral::Integer(0))),
                associated_data: None,
            },
            HirEnumVariant {
                name: "Normal".to_string(),
                value: Some(HirExpression::Literal(HirLiteral::Integer(1))),
                associated_data: None,
            },
            HirEnumVariant {
                name: "Slow".to_string(),
                value: Some(HirExpression::Literal(HirLiteral::Integer(2))),
                associated_data: None,
            },
        ],
        base_type: Box::new(HirType::Bit(2)),
    };

    // Register the enum
    eval.register_enum(mode_enum);

    // Test Mode::Fast = 0
    let expr = HirExpression::EnumVariant {
        enum_type: "Mode".to_string(),
        variant: "Fast".to_string(),
    };
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(0));

    // Test Mode::Normal = 1
    let expr = HirExpression::EnumVariant {
        enum_type: "Mode".to_string(),
        variant: "Normal".to_string(),
    };
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(1));

    // Test Mode::Slow = 2
    let expr = HirExpression::EnumVariant {
        enum_type: "Mode".to_string(),
        variant: "Slow".to_string(),
    };
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(2));
}

/// Test enum variant with implicit values (index-based)
#[test]
fn test_enum_variant_implicit_values() {
    let mut eval = ConstEvaluator::new();

    // Create an enum type without explicit values
    // pub enum State { Idle, Running, Done }
    let state_enum = HirEnumType {
        name: "State".to_string(),
        variants: vec![
            HirEnumVariant {
                name: "Idle".to_string(),
                value: None, // Implicit value 0
                associated_data: None,
            },
            HirEnumVariant {
                name: "Running".to_string(),
                value: None, // Implicit value 1
                associated_data: None,
            },
            HirEnumVariant {
                name: "Done".to_string(),
                value: None, // Implicit value 2
                associated_data: None,
            },
        ],
        base_type: Box::new(HirType::Nat(8)),
    };

    // Register the enum
    eval.register_enum(state_enum);

    // Test State::Idle = 0 (implicit)
    let expr = HirExpression::EnumVariant {
        enum_type: "State".to_string(),
        variant: "Idle".to_string(),
    };
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(0));

    // Test State::Running = 1 (implicit)
    let expr = HirExpression::EnumVariant {
        enum_type: "State".to_string(),
        variant: "Running".to_string(),
    };
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(1));

    // Test State::Done = 2 (implicit)
    let expr = HirExpression::EnumVariant {
        enum_type: "State".to_string(),
        variant: "Done".to_string(),
    };
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(2));
}

/// Test error cases for enum variant evaluation
#[test]
fn test_enum_variant_errors() {
    let mut eval = ConstEvaluator::new();

    // Test undefined enum type
    let expr = HirExpression::EnumVariant {
        enum_type: "UnknownEnum".to_string(),
        variant: "Value".to_string(),
    };
    assert!(eval.eval(&expr).is_err());

    // Create an enum and test undefined variant
    let mode_enum = HirEnumType {
        name: "Mode".to_string(),
        variants: vec![HirEnumVariant {
            name: "Fast".to_string(),
            value: Some(HirExpression::Literal(HirLiteral::Integer(0))),
            associated_data: None,
        }],
        base_type: Box::new(HirType::Bit(2)),
    };
    eval.register_enum(mode_enum);

    // Test undefined variant in existing enum
    let expr = HirExpression::EnumVariant {
        enum_type: "Mode".to_string(),
        variant: "UndefinedVariant".to_string(),
    };
    assert!(eval.eval(&expr).is_err());
}

/// Test enum variant used in arithmetic expressions
#[test]
fn test_enum_variant_in_expressions() {
    let mut eval = ConstEvaluator::new();

    // Create enum with values suitable for arithmetic
    let level_enum = HirEnumType {
        name: "Level".to_string(),
        variants: vec![
            HirEnumVariant {
                name: "Low".to_string(),
                value: Some(HirExpression::Literal(HirLiteral::Integer(10))),
                associated_data: None,
            },
            HirEnumVariant {
                name: "High".to_string(),
                value: Some(HirExpression::Literal(HirLiteral::Integer(20))),
                associated_data: None,
            },
        ],
        base_type: Box::new(HirType::Nat(8)),
    };
    eval.register_enum(level_enum);

    // Test Level::Low + Level::High = 30
    let expr = HirExpression::Binary(HirBinaryExpr {
        op: HirBinaryOp::Add,
        left: Box::new(HirExpression::EnumVariant {
            enum_type: "Level".to_string(),
            variant: "Low".to_string(),
        }),
        right: Box::new(HirExpression::EnumVariant {
            enum_type: "Level".to_string(),
            variant: "High".to_string(),
        }),
    });
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(30));

    // Test Level::High - Level::Low = 10
    let expr = HirExpression::Binary(HirBinaryExpr {
        op: HirBinaryOp::Sub,
        left: Box::new(HirExpression::EnumVariant {
            enum_type: "Level".to_string(),
            variant: "High".to_string(),
        }),
        right: Box::new(HirExpression::EnumVariant {
            enum_type: "Level".to_string(),
            variant: "Low".to_string(),
        }),
    });
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(10));
}
