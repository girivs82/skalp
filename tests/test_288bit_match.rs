// Test for 288-bit Metal backend issue with match expressions
// NOTE: This test requires manual setup of /tmp/skalp_bug_288bit_match.sk
use skalp_testing::testbench::*;

#[tokio::test]
#[ignore = "requires manual setup of /tmp/skalp_bug_288bit_match.sk"]
async fn test_288bit_match_metal() {
    println!("\n🔍 Testing 288-bit Match Expression Metal Backend Issue");

    let mut tb = Testbench::new("/tmp/skalp_bug_288bit_match.sk")
        .await
        .unwrap();

    // Reset
    tb.set("rst", 1u8);
    tb.clock_signal("clk", 2).await;
    tb.set("rst", 0u8);

    // Test opcode 0 (vec3 operations)
    let data1 = vec![0xAAu8; 32];
    let data2 = vec![0xBBu8; 32];

    tb.set("opcode", 0u8)
        .set("data1", data1.as_slice())
        .set("data2", data2.as_slice());

    tb.step().await;
    tb.clock_signal("clk", 1).await;

    let result: Vec<u8> = tb.get_as("result").await;

    println!("\n  INPUTS:");
    println!("    Opcode: 0 (vec3-based computation)");
    println!("    data1: {:?}", &data1[0..4]);
    println!("    data2: {:?}", &data2[0..4]);

    println!("\n  RESULT:");
    println!("    {:?}", &result[0..16]);

    // Test opcode 1 (simple computation)
    tb.set("opcode", 1u8);
    tb.step().await;
    tb.clock_signal("clk", 1).await;

    let result2: Vec<u8> = tb.get_as("result").await;
    println!("\n  Opcode 1 result: {:?}", &result2[0..16]);

    // Test opcode 2 (passthrough)
    tb.set("opcode", 2u8);
    tb.step().await;
    tb.clock_signal("clk", 1).await;

    let result3: Vec<u8> = tb.get_as("result").await;
    println!("\n  Opcode 2 result: {:?}", &result3[0..16]);

    println!("\n  NOTE:");
    println!("    If you see this message, the test compiled and ran successfully.");
    println!("    If Metal backend fails with '288-bit' error, that demonstrates the bug.");

    println!("\n✅ Test completed!");
}
