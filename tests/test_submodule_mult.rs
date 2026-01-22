use skalp_testing::Testbench;

fn setup_stdlib_path() {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let stdlib_path = format!("{}/crates/skalp-stdlib", manifest_dir);
    std::env::set_var("SKALP_STDLIB_PATH", &stdlib_path);
}

fn fixture_path(name: &str) -> String {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    format!("{}/tests/fixtures/{}", manifest_dir, name)
}

#[tokio::test]
async fn test_submodule_multiplication() {
    setup_stdlib_path();

    // Create a test with a simple sub-module instantiation
    let source = r#"
pub entity Mult6x6 {
    in a: bit[6]
    in b: bit[6]
    out product: bit[12]
}

impl Mult6x6 {
    product = a * b
}

pub entity WrapperMult {
    in x: bit[6]
    in y: bit[6]
    out z: bit[12]
}

impl WrapperMult {
    let m = Mult6x6 { a: x, b: y, product: z }
}
"#;

    std::fs::write("/tmp/submodule_mult_test.sk", source).unwrap();

    let mut tb = Testbench::with_top_module("/tmp/submodule_mult_test.sk", "WrapperMult")
        .await
        .expect("Failed to create testbench");

    // Test: 3 × 5 = 15
    tb.set("x", 3u64).set("y", 5u64);
    tb.step().await;
    let product: u64 = tb.get_as("z").await;
    println!("Via submodule: 3 × 5 = {} (expected 15)", product);
    assert_eq!(product, 15, "3 × 5 should equal 15 via submodule");

    // Test: 7 × 8 = 56
    tb.set("x", 7u64).set("y", 8u64);
    tb.step().await;
    let product: u64 = tb.get_as("z").await;
    println!("Via submodule: 7 × 8 = {} (expected 56)", product);
    assert_eq!(product, 56, "7 × 8 should equal 56 via submodule");
}

#[tokio::test]
async fn test_multiple_multiplier_instances() {
    setup_stdlib_path();

    // Create a test with multiple multiplier instances
    let source = r#"
pub entity Mult6x6 {
    in a: bit[6]
    in b: bit[6]
    out product: bit[12]
}

impl Mult6x6 {
    product = a * b
}

pub entity DualMult {
    in a0: bit[6]
    in b0: bit[6]
    in a1: bit[6]
    in b1: bit[6]
    out p0: bit[12]
    out p1: bit[12]
}

impl DualMult {
    let m0 = Mult6x6 { a: a0, b: b0, product: p0 }
    let m1 = Mult6x6 { a: a1, b: b1, product: p1 }
}
"#;

    std::fs::write("/tmp/dual_mult_test.sk", source).unwrap();

    let mut tb = Testbench::with_top_module("/tmp/dual_mult_test.sk", "DualMult")
        .await
        .expect("Failed to create testbench");

    // Test: m0 = 3 × 5 = 15, m1 = 7 × 8 = 56
    tb.set("a0", 3u64).set("b0", 5u64);
    tb.set("a1", 7u64).set("b1", 8u64);
    tb.step().await;
    let p0: u64 = tb.get_as("p0").await;
    let p1: u64 = tb.get_as("p1").await;
    println!(
        "Dual mult: p0 = {} (expected 15), p1 = {} (expected 56)",
        p0, p1
    );
    assert_eq!(p0, 15, "p0 should be 3×5=15");
    assert_eq!(p1, 56, "p1 should be 7×8=56");
}

#[tokio::test]
async fn test_nested_hierarchy() {
    setup_stdlib_path();

    let fixture = fixture_path("submodule/nested_mult.sk");
    let mut tb = Testbench::with_top_module(&fixture, "QuadMult")
        .await
        .expect("Failed to create testbench");

    // Set all 4 multiplier inputs
    tb.set("x0", 3u64).set("y0", 5u64); // 15
    tb.set("x1", 7u64).set("y1", 8u64); // 56
    tb.set("x2", 4u64).set("y2", 6u64); // 24
    tb.set("x3", 9u64).set("y3", 10u64); // 90
    tb.step().await;

    let r0: u64 = tb.get_as("r0").await;
    let r1: u64 = tb.get_as("r1").await;
    let r2: u64 = tb.get_as("r2").await;
    let r3: u64 = tb.get_as("r3").await;

    println!(
        "Nested quad mult: r0={} (15), r1={} (56), r2={} (24), r3={} (90)",
        r0, r1, r2, r3
    );
    assert_eq!(r0, 15, "r0 should be 3×5=15");
    assert_eq!(r1, 56, "r1 should be 7×8=56");
    assert_eq!(r2, 24, "r2 should be 4×6=24");
    assert_eq!(r3, 90, "r3 should be 9×10=90");
}

#[tokio::test]
async fn test_partial_product_generation() {
    setup_stdlib_path();

    let fixture = fixture_path("submodule/pp_gen4.sk");
    let mut tb = Testbench::with_top_module(&fixture, "PPGen4")
        .await
        .expect("Failed to create testbench");

    // a = 0x123 (291), b = 0x456 (1110)
    // a_lo = 0x23 = 35, a_hi = 0x04 = 4
    // b_lo = 0x16 = 22, b_hi = 0x11 = 17
    // p00 = 35 * 22 = 770
    // p01 = 35 * 17 = 595
    // p10 = 4 * 22 = 88
    // p11 = 4 * 17 = 68
    let a: u64 = 0x123;
    let b: u64 = 0x456;
    let a_lo = a & 0x3F; // 35
    let a_hi = (a >> 6) & 0x3F; // 4
    let b_lo = b & 0x3F; // 22
    let b_hi = (b >> 6) & 0x3F; // 17

    tb.set("a", a).set("b", b);
    tb.step().await;

    let p00: u64 = tb.get_as("p00").await;
    let p01: u64 = tb.get_as("p01").await;
    let p10: u64 = tb.get_as("p10").await;
    let p11: u64 = tb.get_as("p11").await;

    let exp_p00 = a_lo * b_lo;
    let exp_p01 = a_lo * b_hi;
    let exp_p10 = a_hi * b_lo;
    let exp_p11 = a_hi * b_hi;

    println!(
        "a=0x{:03X} (lo={}, hi={}), b=0x{:03X} (lo={}, hi={})",
        a, a_lo, a_hi, b, b_lo, b_hi
    );
    println!("p00={} (expected {})", p00, exp_p00);
    println!("p01={} (expected {})", p01, exp_p01);
    println!("p10={} (expected {})", p10, exp_p10);
    println!("p11={} (expected {})", p11, exp_p11);

    assert_eq!(p00, exp_p00, "p00 should be a_lo * b_lo");
    assert_eq!(p01, exp_p01, "p01 should be a_lo * b_hi");
    assert_eq!(p10, exp_p10, "p10 should be a_hi * b_lo");
    assert_eq!(p11, exp_p11, "p11 should be a_hi * b_hi");
}

#[tokio::test]
async fn test_stdlib_import() {
    setup_stdlib_path();

    let fixture = fixture_path("test_stdlib_mult.sk");
    let mut tb = Testbench::with_top_module(&fixture, "TestMult")
        .await
        .expect("Failed to create testbench");

    // Test quad mode: 3×5=15 in section 0
    // a = 0x000003 (only a0=3)
    // b = 0x000005 (only b0=5)
    // mode = 2 (Quad)
    tb.set("a", 3u64).set("b", 5u64).set("mode", 2u64);
    tb.step().await;

    let prod0: u64 = tb.get_as("prod0").await;
    println!(
        "Stdlib import test - Quad mode: prod0 = {} (expected 15)",
        prod0
    );
    assert_eq!(prod0, 15, "prod0 should be 3×5=15 in quad mode");
}

#[tokio::test]
async fn test_accum_stdlib_import() {
    setup_stdlib_path();

    let fixture = fixture_path("test_accum_stdlib_import.sk");
    let mut tb = Testbench::with_top_module(&fixture, "TestAccum")
        .await
        .expect("Failed to create testbench");

    // Test quad mode with p00=15, all others zero
    // In quad mode, prod0 should equal p00 (passthrough)
    tb.set("p00", 15u64).set("mode", 2u64); // mode=2 is Quad
    tb.step().await;
    let prod0: u64 = tb.get_as("prod0").await;
    println!("Accum stdlib: prod0 = {} (expected 15 in quad mode)", prod0);
    assert_eq!(prod0, 15, "prod0 should passthrough p00 in quad mode");
}

#[tokio::test]
async fn test_pmult_direct_import() {
    setup_stdlib_path();

    let fixture = fixture_path("test_pmult_direct.sk");
    let mut tb = Testbench::with_top_module(&fixture, "TestPMultDirect")
        .await
        .expect("Failed to create testbench");

    // Test quad mode: 3×5=15 in section 0
    tb.set("a", 3u64).set("b", 5u64).set("mode", 2u64);
    tb.step().await;

    let prod0: u64 = tb.get_as("prod0").await;
    println!("PMult direct: prod0 = {} (expected 15)", prod0);
    assert_eq!(prod0, 15, "prod0 should be 3×5=15 in quad mode");
}
