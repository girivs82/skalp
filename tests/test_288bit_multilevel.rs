// Test for 288-bit Metal backend issue with multi-level dispatch
// NOTE: This test requires manual setup of /tmp/skalp_bug_288bit_multilevel.sk
use skalp_testing::testbench::*;

#[tokio::test]
#[ignore = "requires manual setup of /tmp/skalp_bug_288bit_multilevel.sk"]
async fn test_288bit_multilevel_metal() {
    println!("\n🔍 Testing 288-bit Multi-Level Dispatch Metal Backend Issue");

    let mut tb = Testbench::new("/tmp/skalp_bug_288bit_multilevel.sk")
        .await
        .unwrap();

    // Reset
    tb.set("rst", 1u8);
    tb.clock_signal("clk", 2).await;
    tb.set("rst", 0u8);

    // Test level 0-1 (opcode < 18)
    let data1 = vec![0xAAu8; 32];
    let data2 = vec![0x55u8; 32];

    tb.set("opcode", 0u8)
        .set("data1", data1.as_slice())
        .set("data2", data2.as_slice());

    tb.step().await;
    tb.clock_signal("clk", 1).await;

    let result1: Vec<u8> = tb.get_as("result").await;
    println!("\n  Level 0-1 (opcode=0): {:?}", &result1[0..4]);

    // Test level 4-5 with vec3 operations (opcode >= 38)
    tb.set("opcode", 48u8);
    tb.step().await;
    tb.clock_signal("clk", 1).await;

    let result2: Vec<u8> = tb.get_as("result").await;
    println!("  Level 4-5 (opcode=48, vec3): {:?}", &result2[0..4]);

    println!("\n  NOTE:");
    println!("    If you see this message, the test compiled and ran successfully.");
    println!("    If Metal backend fails with '288-bit' error, that demonstrates the bug.");

    println!("\n✅ Test completed!");
}
