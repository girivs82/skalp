//! Tests for const expression evaluation in monomorphization

use skalp_frontend::const_eval::{ConstEvaluator, ConstValue};
use skalp_frontend::hir::{HirBinaryExpr, HirBinaryOp, HirExpression, HirLiteral};

#[test]
fn test_basic_arithmetic() {
    let mut eval = ConstEvaluator::new();

    // 5 + 3
    let expr = HirExpression::Binary(HirBinaryExpr {
        op: HirBinaryOp::Add,
        left: Box::new(HirExpression::Literal(HirLiteral::Integer(5))),
        right: Box::new(HirExpression::Literal(HirLiteral::Integer(3))),
        is_trait_op: false,
    });

    let result = eval.eval(&expr).unwrap();
    assert_eq!(result, ConstValue::Nat(8));
}

#[test]
fn test_multiplication() {
    let mut eval = ConstEvaluator::new();

    // 4 * 7
    let expr = HirExpression::Binary(HirBinaryExpr {
        op: HirBinaryOp::Mul,
        left: Box::new(HirExpression::Literal(HirLiteral::Integer(4))),
        right: Box::new(HirExpression::Literal(HirLiteral::Integer(7))),
        is_trait_op: false,
    });

    let result = eval.eval(&expr).unwrap();
    assert_eq!(result, ConstValue::Nat(28));
}

#[test]
fn test_subtraction() {
    let mut eval = ConstEvaluator::new();

    // 10 - 3
    let expr = HirExpression::Binary(HirBinaryExpr {
        op: HirBinaryOp::Sub,
        left: Box::new(HirExpression::Literal(HirLiteral::Integer(10))),
        right: Box::new(HirExpression::Literal(HirLiteral::Integer(3))),
        is_trait_op: false,
    });

    let result = eval.eval(&expr).unwrap();
    assert_eq!(result, ConstValue::Nat(7));
}

#[test]
fn test_division() {
    let mut eval = ConstEvaluator::new();

    // 20 / 4
    let expr = HirExpression::Binary(HirBinaryExpr {
        op: HirBinaryOp::Div,
        left: Box::new(HirExpression::Literal(HirLiteral::Integer(20))),
        right: Box::new(HirExpression::Literal(HirLiteral::Integer(4))),
        is_trait_op: false,
    });

    let result = eval.eval(&expr).unwrap();
    assert_eq!(result, ConstValue::Nat(5));
}

#[test]
fn test_nested_arithmetic() {
    let mut eval = ConstEvaluator::new();

    // (5 + 3) * 2
    let add_expr = HirExpression::Binary(HirBinaryExpr {
        op: HirBinaryOp::Add,
        left: Box::new(HirExpression::Literal(HirLiteral::Integer(5))),
        right: Box::new(HirExpression::Literal(HirLiteral::Integer(3))),
        is_trait_op: false,
    });

    let mul_expr = HirExpression::Binary(HirBinaryExpr {
        op: HirBinaryOp::Mul,
        left: Box::new(add_expr),
        right: Box::new(HirExpression::Literal(HirLiteral::Integer(2))),
        is_trait_op: false,
    });

    let result = eval.eval(&mul_expr).unwrap();
    assert_eq!(result, ConstValue::Nat(16));
}

#[test]
fn test_comparison_operators() {
    let mut eval = ConstEvaluator::new();

    // 5 < 10
    let expr = HirExpression::Binary(HirBinaryExpr {
        op: HirBinaryOp::Less,
        left: Box::new(HirExpression::Literal(HirLiteral::Integer(5))),
        right: Box::new(HirExpression::Literal(HirLiteral::Integer(10))),
        is_trait_op: false,
    });

    let result = eval.eval(&expr).unwrap();
    assert_eq!(result, ConstValue::Bool(true));

    // 5 > 10
    let expr2 = HirExpression::Binary(HirBinaryExpr {
        op: HirBinaryOp::Greater,
        left: Box::new(HirExpression::Literal(HirLiteral::Integer(5))),
        right: Box::new(HirExpression::Literal(HirLiteral::Integer(10))),
        is_trait_op: false,
    });

    let result2 = eval.eval(&expr2).unwrap();
    assert_eq!(result2, ConstValue::Bool(false));
}

#[test]
fn test_equality() {
    let mut eval = ConstEvaluator::new();

    // 5 == 5
    let expr = HirExpression::Binary(HirBinaryExpr {
        op: HirBinaryOp::Equal,
        left: Box::new(HirExpression::Literal(HirLiteral::Integer(5))),
        right: Box::new(HirExpression::Literal(HirLiteral::Integer(5))),
        is_trait_op: false,
    });

    let result = eval.eval(&expr).unwrap();
    assert_eq!(result, ConstValue::Bool(true));
}

#[test]
fn test_logical_and() {
    let mut eval = ConstEvaluator::new();

    // true && false
    let expr = HirExpression::Binary(HirBinaryExpr {
        op: HirBinaryOp::LogicalAnd,
        left: Box::new(HirExpression::Literal(HirLiteral::Boolean(true))),
        right: Box::new(HirExpression::Literal(HirLiteral::Boolean(false))),
        is_trait_op: false,
    });

    let result = eval.eval(&expr).unwrap();
    assert_eq!(result, ConstValue::Bool(false));
}

#[test]
fn test_logical_or() {
    let mut eval = ConstEvaluator::new();

    // true || false
    let expr = HirExpression::Binary(HirBinaryExpr {
        op: HirBinaryOp::LogicalOr,
        left: Box::new(HirExpression::Literal(HirLiteral::Boolean(true))),
        right: Box::new(HirExpression::Literal(HirLiteral::Boolean(false))),
        is_trait_op: false,
    });

    let result = eval.eval(&expr).unwrap();
    assert_eq!(result, ConstValue::Bool(true));
}

#[test]
fn test_bitwise_operations() {
    let mut eval = ConstEvaluator::new();

    // 5 | 3 (0b101 | 0b011 = 0b111 = 7)
    let expr = HirExpression::Binary(HirBinaryExpr {
        op: HirBinaryOp::Or,
        left: Box::new(HirExpression::Literal(HirLiteral::Integer(5))),
        right: Box::new(HirExpression::Literal(HirLiteral::Integer(3))),
        is_trait_op: false,
    });

    let result = eval.eval(&expr).unwrap();
    assert_eq!(result, ConstValue::Nat(7));
}

#[test]
fn test_shift_left() {
    let mut eval = ConstEvaluator::new();

    // 1 << 3
    let expr = HirExpression::Binary(HirBinaryExpr {
        op: HirBinaryOp::LeftShift,
        left: Box::new(HirExpression::Literal(HirLiteral::Integer(1))),
        right: Box::new(HirExpression::Literal(HirLiteral::Integer(3))),
        is_trait_op: false,
    });

    let result = eval.eval(&expr).unwrap();
    assert_eq!(result, ConstValue::Nat(8));
}

#[test]
fn test_shift_right() {
    let mut eval = ConstEvaluator::new();

    // 16 >> 2
    let expr = HirExpression::Binary(HirBinaryExpr {
        op: HirBinaryOp::RightShift,
        left: Box::new(HirExpression::Literal(HirLiteral::Integer(16))),
        right: Box::new(HirExpression::Literal(HirLiteral::Integer(2))),
        is_trait_op: false,
    });

    let result = eval.eval(&expr).unwrap();
    assert_eq!(result, ConstValue::Nat(4));
}

#[test]
fn test_generic_parameter_binding() {
    let mut eval = ConstEvaluator::new();

    // Bind N = 8
    eval.bind("N".to_string(), ConstValue::Nat(8));

    // N + 1
    let expr = HirExpression::Binary(HirBinaryExpr {
        op: HirBinaryOp::Add,
        left: Box::new(HirExpression::GenericParam("N".to_string())),
        right: Box::new(HirExpression::Literal(HirLiteral::Integer(1))),
        is_trait_op: false,
    });

    let result = eval.eval(&expr).unwrap();
    assert_eq!(result, ConstValue::Nat(9));
}

#[test]
fn test_complex_expression_with_params() {
    let mut eval = ConstEvaluator::new();

    // Bind WIDTH = 16
    eval.bind("WIDTH".to_string(), ConstValue::Nat(16));

    // WIDTH * 2 + 8
    let mul_expr = HirExpression::Binary(HirBinaryExpr {
        op: HirBinaryOp::Mul,
        left: Box::new(HirExpression::GenericParam("WIDTH".to_string())),
        right: Box::new(HirExpression::Literal(HirLiteral::Integer(2))),
        is_trait_op: false,
    });

    let add_expr = HirExpression::Binary(HirBinaryExpr {
        op: HirBinaryOp::Add,
        left: Box::new(mul_expr),
        right: Box::new(HirExpression::Literal(HirLiteral::Integer(8))),
        is_trait_op: false,
    });

    let result = eval.eval(&add_expr).unwrap();
    assert_eq!(result, ConstValue::Nat(40));
}

#[test]
fn test_boolean_literals() {
    let mut eval = ConstEvaluator::new();

    let expr_true = HirExpression::Literal(HirLiteral::Boolean(true));
    let result = eval.eval(&expr_true).unwrap();
    assert_eq!(result, ConstValue::Bool(true));

    let expr_false = HirExpression::Literal(HirLiteral::Boolean(false));
    let result = eval.eval(&expr_false).unwrap();
    assert_eq!(result, ConstValue::Bool(false));
}

#[test]
fn test_modulo_operation() {
    let mut eval = ConstEvaluator::new();

    // 17 % 5
    let expr = HirExpression::Binary(HirBinaryExpr {
        op: HirBinaryOp::Mod,
        left: Box::new(HirExpression::Literal(HirLiteral::Integer(17))),
        right: Box::new(HirExpression::Literal(HirLiteral::Integer(5))),
        is_trait_op: false,
    });

    let result = eval.eval(&expr).unwrap();
    assert_eq!(result, ConstValue::Nat(2));
}
