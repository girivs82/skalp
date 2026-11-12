//! Board self-checks for block RAM and multipliers.
//!
//! Each design checks a hard block against logic that doesn't use it and
//! latches a mismatch on `led` bit 0; bit 1 rises once the check has run
//! through. The fault twins have a wrong reference on purpose, so their
//! bit 0 must rise: that shows the checker can see an error.

use skalp_testing::Testbench;

fn fixture_path(name: &str) -> String {
    format!("{}/tests/fixtures/{}", env!("CARGO_MANIFEST_DIR"), name)
}

async fn flags_after(fixture: &str, top: &str, gate_level: bool, cycles: usize) -> u64 {
    let path = fixture_path(fixture);
    let mut tb = if gate_level {
        Testbench::gate_level_with_top_and_library(&path, top, "nexus")
            .await
            .unwrap()
    } else {
        Testbench::with_top_module(&path, top).await.unwrap()
    };
    tb.clock(cycles).await;
    // Bits 0 and 1: the error flag and the ran-through flag
    tb.get_u64("led").await & 0b11
}

#[tokio::test]
async fn test_block_ram_self_check_passes_and_sees_a_fault() {
    for gate_level in [false, true] {
        let flags = flags_after("nexus_ebr_check.sk", "EbrCheck", gate_level, 17_000).await;
        assert_eq!(flags, 0b10, "gate level {gate_level}: flags {flags:#b}");
        let flags = flags_after("nexus_ebr_fault.sk", "EbrFault", gate_level, 17_000).await;
        assert_eq!(flags, 0b11, "gate level {gate_level}: flags {flags:#b}");
    }
}

#[tokio::test]
async fn test_multiplier_self_check_passes_and_sees_a_fault() {
    for gate_level in [false, true] {
        let flags = flags_after("nexus_dsp_check.sk", "DspCheck", gate_level, 8_300).await;
        assert_eq!(flags, 0b10, "gate level {gate_level}: flags {flags:#b}");
        let flags = flags_after("nexus_dsp_fault.sk", "DspFault", gate_level, 8_300).await;
        assert_eq!(flags, 0b11, "gate level {gate_level}: flags {flags:#b}");
    }
}
