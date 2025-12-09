//! Tests for constant evaluation in SKALP
//!
//! Tests cover both generic and non-generic const evaluation paths

use skalp_frontend::const_eval::{ConstEvaluator, ConstValue};
use skalp_frontend::hir::{HirBinaryExpr, HirBinaryOp, HirExpression, HirLiteral, ImplStyle};

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
        impl_style: ImplStyle::default(),
    });
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(10));

    // Test clog2(1) = 0
    let expr = HirExpression::Call(skalp_frontend::hir::HirCallExpr {
        function: "clog2".to_string(),
        type_args: vec![],
        args: vec![HirExpression::Literal(HirLiteral::Integer(1))],
        impl_style: ImplStyle::default(),
    });
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(0));

    // Test clog2(7) = 3
    let expr = HirExpression::Call(skalp_frontend::hir::HirCallExpr {
        function: "clog2".to_string(),
        type_args: vec![],
        args: vec![HirExpression::Literal(HirLiteral::Integer(7))],
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
        impl_style: ImplStyle::default(),
    });
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(8));

    // Test pow2(0) = 1
    let expr = HirExpression::Call(skalp_frontend::hir::HirCallExpr {
        function: "pow2".to_string(),
        type_args: vec![],
        args: vec![HirExpression::Literal(HirLiteral::Integer(0))],
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
        impl_style: ImplStyle::default(),
    });
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(3));

    // Test popcount(15) = 4 (binary: 1111)
    let expr = HirExpression::Call(skalp_frontend::hir::HirCallExpr {
        function: "popcount".to_string(),
        type_args: vec![],
        args: vec![HirExpression::Literal(HirLiteral::Integer(15))],
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
        impl_style: ImplStyle::default(),
    });
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Bool(true));

    // Test is_power_of_2(7) = false
    let expr = HirExpression::Call(skalp_frontend::hir::HirCallExpr {
        function: "is_power_of_2".to_string(),
        type_args: vec![],
        args: vec![HirExpression::Literal(HirLiteral::Integer(7))],
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
        impl_style: ImplStyle::default(),
    });
    let encoded = eval.eval(&expr).unwrap();
    assert_eq!(encoded, ConstValue::Nat(7));

    // Test gray_decode(7) = 5 (reverse of above)
    let expr = HirExpression::Call(skalp_frontend::hir::HirCallExpr {
        function: "gray_decode".to_string(),
        type_args: vec![],
        args: vec![HirExpression::Literal(HirLiteral::Integer(7))],
        impl_style: ImplStyle::default(),
    });
    assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(5));
}
