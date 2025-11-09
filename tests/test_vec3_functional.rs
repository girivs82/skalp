// Functional test for Bug #67 fix - verifies actual computation results

use skalp_testing::testbench::*;

#[tokio::test]
async fn test_vec3_functional() {
    println!("\n🧪 Testing vec3_add functional correctness after Bug #67 fix");

    let mut tb = Testbench::new("/tmp/test_vec3_functional.sk")
        .await
        .expect("Failed to create testbench");

    // Reset
    tb.set("rst", 1u8);
    tb.clock_signal("clk", 2).await;
    tb.set("rst", 0u8);

    // Test: (1,2,3) + (4,5,6) = (5,7,9)
    let ax = 1.0f32.to_bits();
    let ay = 2.0f32.to_bits();
    let az = 3.0f32.to_bits();
    let bx = 4.0f32.to_bits();
    let by = 5.0f32.to_bits();
    let bz = 6.0f32.to_bits();

    // Pack into 256-bit data1 and data2
    let mut data1 = vec![0u8; 32];
    let mut data2 = vec![0u8; 32];

    data1[0..4].copy_from_slice(&ax.to_le_bytes());
    data1[4..8].copy_from_slice(&ay.to_le_bytes());
    data1[8..12].copy_from_slice(&az.to_le_bytes());

    data2[0..4].copy_from_slice(&bx.to_le_bytes());
    data2[4..8].copy_from_slice(&by.to_le_bytes());
    data2[8..12].copy_from_slice(&bz.to_le_bytes());

    tb.set("opcode", 32u8)
        .set("data1", data1.as_slice())
        .set("data2", data2.as_slice());

    // Clock to compute and register result
    tb.clock_signal("clk", 2).await;

    // Read result
    let result_bytes: Vec<u8> = tb.get_as("result").await;

    // Extract x, y, z (first 96 bits)
    let rx_bits = u32::from_le_bytes([
        result_bytes[0],
        result_bytes[1],
        result_bytes[2],
        result_bytes[3],
    ]);
    let ry_bits = u32::from_le_bytes([
        result_bytes[4],
        result_bytes[5],
        result_bytes[6],
        result_bytes[7],
    ]);
    let rz_bits = u32::from_le_bytes([
        result_bytes[8],
        result_bytes[9],
        result_bytes[10],
        result_bytes[11],
    ]);

    let rx = f32::from_bits(rx_bits);
    let ry = f32::from_bits(ry_bits);
    let rz = f32::from_bits(rz_bits);

    println!(
        "Result: ({}, {}, {}) - Expected: (5.0, 7.0, 9.0)",
        rx, ry, rz
    );

    // Verify results
    let tolerance = 0.001;
    if (rx - 5.0).abs() < tolerance && (ry - 7.0).abs() < tolerance && (rz - 9.0).abs() < tolerance
    {
        println!("✅ Vec3 add functional test PASSES - computation is correct");
    } else {
        println!("❌ Vec3 add functional test FAILS - got zeros or wrong values");
        println!("   This indicates a SKALP compiler bug (not Karythra-specific)");
        panic!(
            "Functional test failed: got ({}, {}, {}), expected (5.0, 7.0, 9.0)",
            rx, ry, rz
        );
    }
}
