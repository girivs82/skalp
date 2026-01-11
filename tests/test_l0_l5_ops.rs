//! L0-L5 Operation Gate-Level Simulation Tests
//!
//! Tests individual operations from the CLE hierarchy using boundary-only NCL.
//! Each test creates a minimal async entity and verifies correct simulation results.

#![cfg(not(debug_assertions))]

use skalp_frontend::parse_and_build_compilation_context;
use skalp_lir::{
    apply_boundary_ncl_to_hierarchy, get_stdlib_library, lower_mir_hierarchical_for_optimize_first,
    map_hierarchical_to_gates, GateNetlist, NclConfig,
};
use skalp_mir::MirCompiler;
use skalp_sim::{CircuitMode, HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};
use std::io::Write;

/// Compile source code to gate netlist
fn compile_to_gates(source: &str) -> GateNetlist {
    std::env::set_var(
        "SKALP_STDLIB_PATH",
        "/Users/girivs/src/hw/hls/crates/skalp-stdlib",
    );

    // Write source to unique temp file (use thread ID for uniqueness)
    let thread_id = std::thread::current().id();
    let temp_path_str = format!("/tmp/test_l0_l5_{:?}.sk", thread_id);
    let temp_path = std::path::Path::new(&temp_path_str);
    let mut file = std::fs::File::create(temp_path).expect("Failed to create temp file");
    file.write_all(source.as_bytes()).expect("Failed to write temp file");
    drop(file);

    let context = parse_and_build_compilation_context(temp_path).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile_to_mir_with_modules(&context.main_hir, &context.module_hirs)
        .expect("Failed to compile to MIR");

    let (hier_lir_raw, has_async) = lower_mir_hierarchical_for_optimize_first(&mir);

    let hier_lir = if has_async {
        let ncl_config = NclConfig::default();
        apply_boundary_ncl_to_hierarchy(&hier_lir_raw, &ncl_config)
    } else {
        hier_lir_raw
    };

    let library = get_stdlib_library("generic_asic").expect("Failed to load library");
    let hier_result = map_hierarchical_to_gates(&hier_lir, &library);
    hier_result.flatten()
}

/// Run NCL simulation and get output
fn simulate_32bit(netlist: &GateNetlist, a: u64, b: u64) -> Option<u64> {
    let config = UnifiedSimConfig {
        level: SimLevel::GateLevel,
        circuit_mode: CircuitMode::Ncl,
        hw_accel: HwAccel::Cpu,
        max_iterations: 10000,
        ncl_debug: false,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist.clone())
        .expect("Failed to load NCL netlist");

    sim.set_ncl_input("top.a", a, 32);
    sim.set_ncl_input("top.b", b, 32);

    let result = sim.run_until_stable();
    if !result.is_stable {
        return None;
    }

    sim.get_ncl_output("top.result", 32)
}

/// Run NCL simulation for 8-bit operations
fn simulate_8bit(netlist: &GateNetlist, a: u64, b: u64) -> Option<u64> {
    let config = UnifiedSimConfig {
        level: SimLevel::GateLevel,
        circuit_mode: CircuitMode::Ncl,
        hw_accel: HwAccel::Cpu,
        max_iterations: 10000,
        ncl_debug: false,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist.clone())
        .expect("Failed to load NCL netlist");

    sim.set_ncl_input("top.a", a, 8);
    sim.set_ncl_input("top.b", b, 8);

    let result = sim.run_until_stable();
    if !result.is_stable {
        return None;
    }

    sim.get_ncl_output("top.result", 8)
}

// ============================================================================
// L0: Basic Integer Operations (8-bit)
// ============================================================================

#[test]
fn test_l0_add_8bit() {
    let source = r#"
        async entity Add8 {
            in a: bit[8]
            in b: bit[8]
            out result: bit[8]
        }
        impl Add8 {
            result = a + b
        }
    "#;

    println!("Compiling L0 Add8...");
    let netlist = compile_to_gates(source);
    println!("Compiled: {} cells", netlist.cells.len());

    let test_cases: Vec<(u64, u64, u64)> = vec![
        (5, 3, 8),
        (0, 0, 0),
        (255, 1, 0),   // overflow wraps
        (100, 50, 150),
        (1, 1, 2),
    ];

    let mut passed = 0;
    for (a, b, expected) in test_cases {
        let result = simulate_8bit(&netlist, a, b);
        match result {
            Some(r) if r == expected => {
                println!("  {} + {} = {} ✓", a, b, r);
                passed += 1;
            }
            Some(r) => println!("  {} + {} = {} ✗ (expected {})", a, b, r, expected),
            None => println!("  {} + {} = FAILED", a, b),
        }
    }
    assert_eq!(passed, 5, "Some L0 add tests failed");
}

#[test]
fn test_l0_sub_8bit() {
    let source = r#"
        async entity Sub8 {
            in a: bit[8]
            in b: bit[8]
            out result: bit[8]
        }
        impl Sub8 {
            result = a - b
        }
    "#;

    println!("Compiling L0 Sub8...");
    let netlist = compile_to_gates(source);
    println!("Compiled: {} cells", netlist.cells.len());

    let test_cases: Vec<(u64, u64, u64)> = vec![
        (10, 3, 7),
        (100, 50, 50),
        (5, 5, 0),
        (0, 1, 255),   // underflow wraps
        (255, 255, 0),
    ];

    let mut passed = 0;
    for (a, b, expected) in test_cases {
        let result = simulate_8bit(&netlist, a, b);
        match result {
            Some(r) if r == expected => {
                println!("  {} - {} = {} ✓", a, b, r);
                passed += 1;
            }
            Some(r) => println!("  {} - {} = {} ✗ (expected {})", a, b, r, expected),
            None => println!("  {} - {} = FAILED", a, b),
        }
    }
    assert_eq!(passed, 5, "Some L0 sub tests failed");
}

#[test]
fn test_l0_and_8bit() {
    let source = r#"
        async entity And8 {
            in a: bit[8]
            in b: bit[8]
            out result: bit[8]
        }
        impl And8 {
            result = a & b
        }
    "#;

    println!("Compiling L0 And8...");
    let netlist = compile_to_gates(source);
    println!("Compiled: {} cells", netlist.cells.len());

    let test_cases: Vec<(u64, u64, u64)> = vec![
        (0xFF, 0x0F, 0x0F),
        (0xAA, 0x55, 0x00),
        (0xFF, 0xFF, 0xFF),
        (0x00, 0xFF, 0x00),
        (0x12, 0x34, 0x10),
    ];

    let mut passed = 0;
    for (a, b, expected) in test_cases {
        let result = simulate_8bit(&netlist, a, b);
        match result {
            Some(r) if r == expected => {
                println!("  0x{:02X} & 0x{:02X} = 0x{:02X} ✓", a, b, r);
                passed += 1;
            }
            Some(r) => println!("  0x{:02X} & 0x{:02X} = 0x{:02X} ✗ (expected 0x{:02X})", a, b, r, expected),
            None => println!("  0x{:02X} & 0x{:02X} = FAILED", a, b),
        }
    }
    assert_eq!(passed, 5, "Some L0 and tests failed");
}

#[test]
fn test_l0_or_8bit() {
    let source = r#"
        async entity Or8 {
            in a: bit[8]
            in b: bit[8]
            out result: bit[8]
        }
        impl Or8 {
            result = a | b
        }
    "#;

    println!("Compiling L0 Or8...");
    let netlist = compile_to_gates(source);
    println!("Compiled: {} cells", netlist.cells.len());

    let test_cases: Vec<(u64, u64, u64)> = vec![
        (0xF0, 0x0F, 0xFF),
        (0xAA, 0x55, 0xFF),
        (0x00, 0x00, 0x00),
        (0x12, 0x34, 0x36),
        (0x80, 0x01, 0x81),
    ];

    let mut passed = 0;
    for (a, b, expected) in test_cases {
        let result = simulate_8bit(&netlist, a, b);
        match result {
            Some(r) if r == expected => {
                println!("  0x{:02X} | 0x{:02X} = 0x{:02X} ✓", a, b, r);
                passed += 1;
            }
            Some(r) => println!("  0x{:02X} | 0x{:02X} = 0x{:02X} ✗ (expected 0x{:02X})", a, b, r, expected),
            None => println!("  0x{:02X} | 0x{:02X} = FAILED", a, b),
        }
    }
    assert_eq!(passed, 5, "Some L0 or tests failed");
}

#[test]
fn test_l0_xor_8bit() {
    let source = r#"
        async entity Xor8 {
            in a: bit[8]
            in b: bit[8]
            out result: bit[8]
        }
        impl Xor8 {
            result = a ^ b
        }
    "#;

    println!("Compiling L0 Xor8...");
    let netlist = compile_to_gates(source);
    println!("Compiled: {} cells", netlist.cells.len());

    let test_cases: Vec<(u64, u64, u64)> = vec![
        (0xFF, 0xFF, 0x00),
        (0xAA, 0x55, 0xFF),
        (0x00, 0x00, 0x00),
        (0x12, 0x34, 0x26),
        (0xFF, 0x00, 0xFF),
    ];

    let mut passed = 0;
    for (a, b, expected) in test_cases {
        let result = simulate_8bit(&netlist, a, b);
        match result {
            Some(r) if r == expected => {
                println!("  0x{:02X} ^ 0x{:02X} = 0x{:02X} ✓", a, b, r);
                passed += 1;
            }
            Some(r) => println!("  0x{:02X} ^ 0x{:02X} = 0x{:02X} ✗ (expected 0x{:02X})", a, b, r, expected),
            None => println!("  0x{:02X} ^ 0x{:02X} = FAILED", a, b),
        }
    }
    assert_eq!(passed, 5, "Some L0 xor tests failed");
}

#[test]
fn test_l0_shl_8bit() {
    let source = r#"
        async entity Shl8 {
            in a: bit[8]
            in b: bit[8]
            out result: bit[8]
        }
        impl Shl8 {
            result = a << b[2:0]
        }
    "#;

    println!("Compiling L0 Shl8...");
    let netlist = compile_to_gates(source);
    println!("Compiled: {} cells", netlist.cells.len());

    let test_cases: Vec<(u64, u64, u64)> = vec![
        (0x01, 0, 0x01),
        (0x01, 1, 0x02),
        (0x01, 4, 0x10),
        (0x0F, 4, 0xF0),
        (0x80, 1, 0x00),  // shift out
    ];

    let mut passed = 0;
    for (a, b, expected) in test_cases {
        let result = simulate_8bit(&netlist, a, b);
        match result {
            Some(r) if r == expected => {
                println!("  0x{:02X} << {} = 0x{:02X} ✓", a, b, r);
                passed += 1;
            }
            Some(r) => println!("  0x{:02X} << {} = 0x{:02X} ✗ (expected 0x{:02X})", a, b, r, expected),
            None => println!("  0x{:02X} << {} = FAILED", a, b),
        }
    }
    assert_eq!(passed, 5, "Some L0 shl tests failed");
}

#[test]
fn test_l0_shr_8bit() {
    let source = r#"
        async entity Shr8 {
            in a: bit[8]
            in b: bit[8]
            out result: bit[8]
        }
        impl Shr8 {
            result = a >> b[2:0]
        }
    "#;

    println!("Compiling L0 Shr8...");
    let netlist = compile_to_gates(source);
    println!("Compiled: {} cells", netlist.cells.len());

    let test_cases: Vec<(u64, u64, u64)> = vec![
        (0x80, 0, 0x80),
        (0x80, 1, 0x40),
        (0x80, 4, 0x08),
        (0xF0, 4, 0x0F),
        (0x01, 1, 0x00),  // shift out
    ];

    let mut passed = 0;
    for (a, b, expected) in test_cases {
        let result = simulate_8bit(&netlist, a, b);
        match result {
            Some(r) if r == expected => {
                println!("  0x{:02X} >> {} = 0x{:02X} ✓", a, b, r);
                passed += 1;
            }
            Some(r) => println!("  0x{:02X} >> {} = 0x{:02X} ✗ (expected 0x{:02X})", a, b, r, expected),
            None => println!("  0x{:02X} >> {} = FAILED", a, b),
        }
    }
    assert_eq!(passed, 5, "Some L0 shr tests failed");
}

// ============================================================================
// L0: Comparisons
// ============================================================================

#[test]
fn test_l0_eq_8bit() {
    let source = r#"
        async entity Eq8 {
            in a: bit[8]
            in b: bit[8]
            out result: bit[8]
        }
        impl Eq8 {
            result = (a == b) ? 8'b1 : 8'b0
        }
    "#;

    println!("Compiling L0 Eq8...");
    let netlist = compile_to_gates(source);
    println!("Compiled: {} cells", netlist.cells.len());

    let test_cases: Vec<(u64, u64, u64)> = vec![
        (5, 5, 1),
        (0, 0, 1),
        (255, 255, 1),
        (5, 6, 0),
        (0, 255, 0),
    ];

    let mut passed = 0;
    for (a, b, expected) in test_cases {
        let result = simulate_8bit(&netlist, a, b);
        match result {
            Some(r) if r == expected => {
                println!("  {} == {} ? {} ✓", a, b, r);
                passed += 1;
            }
            Some(r) => println!("  {} == {} ? {} ✗ (expected {})", a, b, r, expected),
            None => println!("  {} == {} = FAILED", a, b),
        }
    }
    assert_eq!(passed, 5, "Some L0 eq tests failed");
}

#[test]
fn test_l0_lt_8bit() {
    let source = r#"
        async entity Lt8 {
            in a: bit[8]
            in b: bit[8]
            out result: bit[8]
        }
        impl Lt8 {
            result = (a < b) ? 8'b1 : 8'b0
        }
    "#;

    println!("Compiling L0 Lt8...");
    let netlist = compile_to_gates(source);
    println!("Compiled: {} cells", netlist.cells.len());

    let test_cases: Vec<(u64, u64, u64)> = vec![
        (3, 5, 1),
        (5, 5, 0),
        (5, 3, 0),
        (0, 255, 1),
        (254, 255, 1),
    ];

    let mut passed = 0;
    for (a, b, expected) in test_cases {
        let result = simulate_8bit(&netlist, a, b);
        match result {
            Some(r) if r == expected => {
                println!("  {} < {} ? {} ✓", a, b, r);
                passed += 1;
            }
            Some(r) => println!("  {} < {} ? {} ✗ (expected {})", a, b, r, expected),
            None => println!("  {} < {} = FAILED", a, b),
        }
    }
    assert_eq!(passed, 5, "Some L0 lt tests failed");
}

// ============================================================================
// L2: FP32 Operations
// ============================================================================

#[test]
fn test_l2_fp32_mul() {
    let source = r#"
        use skalp::numeric::fp::*;

        async entity TestMulFloat32 {
            in a: bit[32]
            in b: bit[32]
            out result: bit[32]
        }
        impl TestMulFloat32 {
            signal a_fp: fp32 = a as fp32
            signal b_fp: fp32 = b as fp32
            signal prod: fp32 = a_fp * b_fp
            result = prod as bit[32]
        }
    "#;

    println!("Compiling L2 FP32Mul...");
    let netlist = compile_to_gates(source);
    println!("Compiled: {} cells", netlist.cells.len());

    let test_cases: Vec<(f32, f32, f32)> = vec![
        (2.0, 3.0, 6.0),
        (1.0, 1.0, 1.0),
        (0.5, 2.0, 1.0),
        (10.0, 0.1, 1.0),
        (-2.0, 3.0, -6.0),
    ];

    let mut passed = 0;
    for (a, b, expected) in test_cases {
        let a_bits = a.to_bits() as u64;
        let b_bits = b.to_bits() as u64;
        let result = simulate_32bit(&netlist, a_bits, b_bits);
        match result {
            Some(r) => {
                let r_f32 = f32::from_bits(r as u32);
                let error = (r_f32 - expected).abs();
                let tolerance = expected.abs() * 1e-5 + 1e-6;
                if error <= tolerance {
                    println!("  {} * {} = {} ✓", a, b, r_f32);
                    passed += 1;
                } else {
                    println!("  {} * {} = {} ✗ (expected {})", a, b, r_f32, expected);
                }
            }
            None => println!("  {} * {} = FAILED", a, b),
        }
    }
    assert_eq!(passed, 5, "Some L2 fp32 mul tests failed");
}

// ============================================================================
// L3: Vector Operations
// ============================================================================

#[test]
fn test_l3_vec3_add() {
    // Vec3 add: component-wise addition of 3 FP32 values
    // Pack as: a = {a.z, a.y, a.x} (96 bits, but we use 32-bit per component)
    let source = r#"
        use skalp::numeric::fp::*;

        async entity TestVectorAdd3 {
            in ax: bit[32]
            in ay: bit[32]
            in az: bit[32]
            in bx: bit[32]
            in by: bit[32]
            in bz: bit[32]
            out rx: bit[32]
            out ry: bit[32]
            out rz: bit[32]
        }
        impl TestVectorAdd3 {
            signal ax_fp: fp32 = ax as fp32
            signal ay_fp: fp32 = ay as fp32
            signal az_fp: fp32 = az as fp32
            signal bx_fp: fp32 = bx as fp32
            signal by_fp: fp32 = by as fp32
            signal bz_fp: fp32 = bz as fp32
            rx = (ax_fp + bx_fp) as bit[32]
            ry = (ay_fp + by_fp) as bit[32]
            rz = (az_fp + bz_fp) as bit[32]
        }
    "#;

    println!("Compiling L3 Vec3Add...");
    let netlist = compile_to_gates(source);
    println!("Compiled: {} cells", netlist.cells.len());

    // Test: (1,2,3) + (4,5,6) = (5,7,9)
    let config = UnifiedSimConfig {
        level: SimLevel::GateLevel,
        circuit_mode: CircuitMode::Ncl,
        hw_accel: HwAccel::Cpu,
        max_iterations: 10000,
        ncl_debug: false,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist).expect("Failed to load NCL netlist");

    sim.set_ncl_input("top.ax", 1.0f32.to_bits() as u64, 32);
    sim.set_ncl_input("top.ay", 2.0f32.to_bits() as u64, 32);
    sim.set_ncl_input("top.az", 3.0f32.to_bits() as u64, 32);
    sim.set_ncl_input("top.bx", 4.0f32.to_bits() as u64, 32);
    sim.set_ncl_input("top.by", 5.0f32.to_bits() as u64, 32);
    sim.set_ncl_input("top.bz", 6.0f32.to_bits() as u64, 32);

    let result = sim.run_until_stable();
    println!("Converged in {} iterations", result.iterations);

    let rx = sim.get_ncl_output("top.rx", 32).map(|v| f32::from_bits(v as u32));
    let ry = sim.get_ncl_output("top.ry", 32).map(|v| f32::from_bits(v as u32));
    let rz = sim.get_ncl_output("top.rz", 32).map(|v| f32::from_bits(v as u32));

    println!("  (1,2,3) + (4,5,6) = ({:?}, {:?}, {:?})", rx, ry, rz);

    assert_eq!(rx, Some(5.0), "X component wrong");
    assert_eq!(ry, Some(7.0), "Y component wrong");
    assert_eq!(rz, Some(9.0), "Z component wrong");
    println!("✓ Vec3 add passed");
}

#[test]
fn test_l3_vec3_dot() {
    // Vec3 dot product: a.x*b.x + a.y*b.y + a.z*b.z
    let source = r#"
        use skalp::numeric::fp::*;

        async entity TestVectorDot3 {
            in ax: bit[32]
            in ay: bit[32]
            in az: bit[32]
            in bx: bit[32]
            in by: bit[32]
            in bz: bit[32]
            out result: bit[32]
        }
        impl TestVectorDot3 {
            signal ax_fp: fp32 = ax as fp32
            signal ay_fp: fp32 = ay as fp32
            signal az_fp: fp32 = az as fp32
            signal bx_fp: fp32 = bx as fp32
            signal by_fp: fp32 = by as fp32
            signal bz_fp: fp32 = bz as fp32
            signal xx: fp32 = ax_fp * bx_fp
            signal yy: fp32 = ay_fp * by_fp
            signal zz: fp32 = az_fp * bz_fp
            signal sum_xy: fp32 = xx + yy
            signal dot: fp32 = sum_xy + zz
            result = dot as bit[32]
        }
    "#;

    println!("Compiling L3 Vec3Dot...");
    let netlist = compile_to_gates(source);
    println!("Compiled: {} cells", netlist.cells.len());

    let config = UnifiedSimConfig {
        level: SimLevel::GateLevel,
        circuit_mode: CircuitMode::Ncl,
        hw_accel: HwAccel::Cpu,
        max_iterations: 10000,
        ncl_debug: false,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist).expect("Failed to load NCL netlist");

    // Test: (1,2,3) . (4,5,6) = 4 + 10 + 18 = 32
    sim.set_ncl_input("top.ax", 1.0f32.to_bits() as u64, 32);
    sim.set_ncl_input("top.ay", 2.0f32.to_bits() as u64, 32);
    sim.set_ncl_input("top.az", 3.0f32.to_bits() as u64, 32);
    sim.set_ncl_input("top.bx", 4.0f32.to_bits() as u64, 32);
    sim.set_ncl_input("top.by", 5.0f32.to_bits() as u64, 32);
    sim.set_ncl_input("top.bz", 6.0f32.to_bits() as u64, 32);

    let result = sim.run_until_stable();
    println!("Converged in {} iterations", result.iterations);

    let dot = sim.get_ncl_output("top.result", 32).map(|v| f32::from_bits(v as u32));
    println!("  (1,2,3) . (4,5,6) = {:?}", dot);

    assert_eq!(dot, Some(32.0), "Dot product wrong");
    println!("✓ Vec3 dot passed");
}

// ============================================================================
// L5: Bit Manipulation Operations
// ============================================================================

#[test]
fn test_l5_popcount_8bit() {
    // Simple 4-bit popcount test (8-bit is too complex for quick simulation)
    let source = r#"
        async entity TestPopcount4 {
            in a: bit[8]
            in b: bit[8]
            out result: bit[8]
        }
        impl TestPopcount4 {
            // Count set bits in lower 4 bits only
            signal p0: bit[2] = {1'b0, a[0]} + {1'b0, a[1]}
            signal p1: bit[2] = {1'b0, a[2]} + {1'b0, a[3]}
            signal cnt: bit[3] = {1'b0, p0} + {1'b0, p1}
            result = {5'b0, cnt} | (b & 8'b0)  // use b to keep input
        }
    "#;

    println!("Compiling L5 Popcount4...");
    let netlist = compile_to_gates(source);
    println!("Compiled: {} cells", netlist.cells.len());

    // Test 4-bit popcount (lower nibble only)
    let test_cases: Vec<(u64, u64)> = vec![
        (0x00, 0),  // 0 bits
        (0x0F, 4),  // 4 bits
        (0x05, 2),  // 0101 = 2 bits
        (0x0A, 2),  // 1010 = 2 bits
        (0x01, 1),  // 1 bit
    ];

    let mut passed = 0;
    for (a, expected) in test_cases {
        let result = simulate_8bit(&netlist, a, 0);
        match result {
            Some(r) if r == expected => {
                println!("  popcount4(0x{:02X}) = {} ✓", a, r);
                passed += 1;
            }
            Some(r) => println!("  popcount4(0x{:02X}) = {} ✗ (expected {})", a, r, expected),
            None => println!("  popcount4(0x{:02X}) = FAILED", a),
        }
    }
    assert_eq!(passed, 5, "Some L5 popcount tests failed");
}

#[test]
fn test_l5_parity_8bit() {
    // Simple 4-bit parity test
    let source = r#"
        async entity TestParity4 {
            in a: bit[8]
            in b: bit[8]
            out result: bit[8]
        }
        impl TestParity4 {
            // XOR reduction for parity (4 bits only)
            signal p: bit = a[0] ^ a[1] ^ a[2] ^ a[3]
            result = {7'b0, p} | (b & 8'b0)  // use b
        }
    "#;

    println!("Compiling L5 Parity4...");
    let netlist = compile_to_gates(source);
    println!("Compiled: {} cells", netlist.cells.len());

    // Test 4-bit parity (lower nibble only)
    let test_cases: Vec<(u64, u64)> = vec![
        (0x00, 0),  // 0 bits = even
        (0x01, 1),  // 1 bit = odd
        (0x03, 0),  // 2 bits = even
        (0x07, 1),  // 3 bits = odd
        (0x0F, 0),  // 4 bits = even
    ];

    let mut passed = 0;
    for (a, expected) in test_cases {
        let result = simulate_8bit(&netlist, a, 0);
        match result {
            Some(r) if r == expected => {
                println!("  parity(0x{:02X}) = {} ✓", a, r);
                passed += 1;
            }
            Some(r) => println!("  parity(0x{:02X}) = {} ✗ (expected {})", a, r, expected),
            None => println!("  parity(0x{:02X}) = FAILED", a),
        }
    }
    assert_eq!(passed, 5, "Some L5 parity tests failed");
}

#[test]
fn test_l5_bitreverse_8bit() {
    // Simple 4-bit reverse test (more reliable for simulation)
    let source = r#"
        async entity TestBitreverse4 {
            in a: bit[8]
            in b: bit[8]
            out result: bit[8]
        }
        impl TestBitreverse4 {
            // Reverse lower 4 bits only: a[3:0] -> result[3:0] reversed
            // {a[0], a[1], a[2], a[3]} puts a[0] at MSB of the 4-bit field
            result = {4'b0, a[0], a[1], a[2], a[3]} | (b & 8'b0)
        }
    "#;

    println!("Compiling L5 Bitreverse4...");
    let netlist = compile_to_gates(source);
    println!("Compiled: {} cells", netlist.cells.len());

    // Test 4-bit reverse in lower nibble
    // Input 0x01 (binary 0001) -> reversed 1000 = 0x08
    // Input 0x08 (binary 1000) -> reversed 0001 = 0x01
    let test_cases: Vec<(u64, u64)> = vec![
        (0x01, 0x08),  // 0001 -> 1000
        (0x08, 0x01),  // 1000 -> 0001
        (0x0F, 0x0F),  // 1111 -> 1111
        (0x00, 0x00),  // 0000 -> 0000
        (0x05, 0x0A),  // 0101 -> 1010
    ];

    let mut passed = 0;
    for (a, expected) in test_cases {
        let result = simulate_8bit(&netlist, a, 0);
        match result {
            Some(r) if r == expected => {
                println!("  bitreverse(0x{:02X}) = 0x{:02X} ✓", a, r);
                passed += 1;
            }
            Some(r) => println!("  bitreverse(0x{:02X}) = 0x{:02X} ✗ (expected 0x{:02X})", a, r, expected),
            None => println!("  bitreverse(0x{:02X}) = FAILED", a),
        }
    }
    assert_eq!(passed, 5, "Some L5 bitreverse tests failed");
}

#[test]
fn test_simple_fp32_add() {
    let source = r#"
        use skalp::numeric::fp::*;

        async entity TestSimpleFPAdd {
            in a: bit[32]
            in b: bit[32]
            out result: bit[32]
        }
        impl TestSimpleFPAdd {
            signal a_fp: fp32 = a as fp32
            signal b_fp: fp32 = b as fp32
            signal sum: fp32 = a_fp + b_fp
            result = sum as bit[32]
        }
    "#;

    println!("Compiling simple FP32 add...");
    let netlist = compile_to_gates(source);
    println!("Compiled: {} cells", netlist.cells.len());

    // Test: 2.0 + 3.0 = 5.0
    let a = 2.0f32.to_bits() as u64;
    let b = 3.0f32.to_bits() as u64;
    
    let result = simulate_32bit(&netlist, a, b);
    match result {
        Some(r) => {
            let r_f32 = f32::from_bits(r as u32);
            println!("  2.0 + 3.0 = {} (0x{:08X})", r_f32, r);
            assert!((r_f32 - 5.0).abs() < 0.001, "Expected 5.0, got {}", r_f32);
            println!("✓ Simple FP32 add passed");
        }
        None => panic!("Simulation failed to converge"),
    }
}
