/// Test that type casts work in struct field initializers
use skalp_frontend::parse_and_build_hir;

#[test]
fn test_cast_in_struct_field_inline() {
    let source = r#"
struct Vec3 {
    x: fp32,
    y: fp32,
    z: fp32
}

pub fn test() -> Vec3 {
    let a = 1.0 as bit[32];
    return Vec3 {
        x: a as fp32,
        y: a as fp32,
        z: a as fp32
    }
}
"#;

    let result = parse_and_build_hir(source);
    match &result {
        Ok(_) => println!("✓ Parser successfully handles casts in struct field initializers"),
        Err(e) => println!("✗ Parser failed: {}", e),
    }

    assert!(
        result.is_ok(),
        "Parser should support type casts in struct field initializers"
    );
}

#[test]
fn test_cast_in_struct_field_workaround() {
    let source = r#"
struct Vec3 {
    x: fp32,
    y: fp32,
    z: fp32
}

pub fn test() -> Vec3 {
    let a = 1.0 as bit[32];
    let x_val = a as fp32;
    let y_val = a as fp32;
    let z_val = a as fp32;
    return Vec3 {
        x: x_val,
        y: y_val,
        z: z_val
    }
}
"#;

    let result = parse_and_build_hir(source);
    assert!(result.is_ok(), "Workaround approach should definitely work");
}
