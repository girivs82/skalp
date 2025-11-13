// Test for 288-bit Metal backend limitation MWE
use skalp_testing::testbench::*;

#[tokio::test]
async fn test_288bit_metal_limitation() {
    println!("\n🔍 Testing 288-bit Metal Backend Limitation");

    let mut tb = Testbench::new("/tmp/skalp_bug_288bit_mwe.sk")
        .await
        .unwrap();

    // Reset
    tb.set("rst", 1u8);
    tb.clock_signal("clk", 2).await;
    tb.set("rst", 0u8);

    // Set up a simple test case
    // Triangle vertices: v0=(0,0,0), v1=(1,0,0), v2=(0,1,0)
    // Ray direction: (0,0,1)

    tb.set("v0x", 0.0f32.to_bits())
        .set("v0y", 0.0f32.to_bits())
        .set("v0z", 0.0f32.to_bits())
        .set("v1x", 1.0f32.to_bits())
        .set("v1y", 0.0f32.to_bits())
        .set("v1z", 0.0f32.to_bits())
        .set("v2x", 0.0f32.to_bits())
        .set("v2y", 1.0f32.to_bits())
        .set("v2z", 0.0f32.to_bits())
        .set("ray_dx", 0.0f32.to_bits())
        .set("ray_dy", 0.0f32.to_bits())
        .set("ray_dz", 1.0f32.to_bits());

    tb.step().await;
    tb.clock_signal("clk", 1).await;

    let result: Vec<u8> = tb.get_as("result").await;
    let result_val = f32::from_bits(u32::from_le_bytes([
        result[0], result[1], result[2], result[3]
    ]));

    println!("\n  INPUTS:");
    println!("    Triangle: v0=(0,0,0), v1=(1,0,0), v2=(0,1,0)");
    println!("    Ray direction: (0,0,1)");

    println!("\n  RESULT:");
    println!("    Computed value: {}", result_val);

    println!("\n  NOTE:");
    println!("    If you see this message, the test compiled and ran successfully.");
    println!("    If Metal backend fails with '288-bit' error, that demonstrates the bug.");

    println!("\n✅ Test completed!");
}
