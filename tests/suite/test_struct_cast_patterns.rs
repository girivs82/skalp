/// Comprehensive tests for type casts in struct field initializers
/// This verifies that SKALP supports various cast patterns in struct fields
use skalp_frontend::parse_and_build_hir;

#[test]
fn test_simple_cast_in_struct() {
    let source = r#"
struct Point { x: fp32, y: fp32 }

pub fn test() -> Point {
    let a = 1 as bit[32];
    return Point {
        x: a as fp32,
        y: a as fp32
    }
}
"#;
    assert!(parse_and_build_hir(source).is_ok());
}

#[test]
fn test_nested_cast_in_struct() {
    let source = r#"
struct Point { x: fp32, y: fp32 }

pub fn test() -> Point {
    return Point {
        x: (1 as bit[32]) as fp32,
        y: (2 as bit[32]) as fp32
    }
}
"#;
    assert!(parse_and_build_hir(source).is_ok());
}

#[test]
fn test_expression_with_cast_in_struct() {
    let source = r#"
struct Point { x: fp32, y: fp32 }

pub fn test(a: bit[32], b: bit[32]) -> Point {
    return Point {
        x: (a + b) as fp32,
        y: (a - b) as fp32
    }
}
"#;
    assert!(parse_and_build_hir(source).is_ok());
}

#[test]
fn test_function_call_with_cast_in_struct() {
    let source = r#"
fn helper(x: bit[32]) -> bit[32] { return x }

struct Point { x: fp32, y: fp32 }

pub fn test(a: bit[32]) -> Point {
    return Point {
        x: helper(a) as fp32,
        y: helper(a + 1) as fp32
    }
}
"#;
    assert!(parse_and_build_hir(source).is_ok());
}

#[test]
fn test_vec3_with_inline_casts() {
    let source = r#"
struct vec3<T> { x: T, y: T, z: T }

pub fn test() -> vec3<fp32> {
    let a = 1 as bit[32];
    let b = 2 as bit[32];
    let c = 3 as bit[32];

    return vec3 {
        x: a as fp32,
        y: b as fp32,
        z: c as fp32
    }
}
"#;
    assert!(parse_and_build_hir(source).is_ok());
}

#[test]
fn test_vec4_with_tuple_casts() {
    let source = r#"
struct vec4<T> { x: T, y: T, z: T, w: T }

pub fn test(data: (bit[32], bit[32], bit[32], bit[32])) -> vec4<fp32> {
    return vec4 {
        x: data.0 as fp32,
        y: data.1 as fp32,
        z: data.2 as fp32,
        w: data.3 as fp32
    }
}
"#;
    assert!(parse_and_build_hir(source).is_ok());
}

#[test]
fn test_mixed_cast_and_no_cast() {
    let source = r#"
struct Mixed { a: fp32, b: fp32, c: bit[32] }

pub fn test(x: bit[32], y: fp32) -> Mixed {
    return Mixed {
        a: x as fp32,
        b: y,
        c: x
    }
}
"#;
    assert!(parse_and_build_hir(source).is_ok());
}

#[test]
fn test_cast_with_method_call() {
    let source = r#"
struct Point { x: fp32, y: fp32 }

trait FloatOps {
    fn add(self, other: Self) -> Self;
}

impl FloatOps for fp32 {
    fn add(self, other: fp32) -> fp32 { return self }
}

pub fn test(a: fp32, b: fp32) -> Point {
    let x_bits = a.add(b) as bit[32];
    return Point {
        x: x_bits as fp32,
        y: x_bits as fp32
    }
}
"#;
    assert!(parse_and_build_hir(source).is_ok());
}
