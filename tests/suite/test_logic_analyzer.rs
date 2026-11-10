//! The on-chip logic analyzer records around its trigger and sends the
//! capture over its UART line.

use skalp_testing::testbench::Testbench;

const DIVIDER: usize = 4;

/// A counter watched by the logic analyzer, which triggers when it reaches 100.
const TOP: &str = r#"
mod logic_analyzer;
use logic_analyzer::logic_analyzer;

entity LaTest {
    in clk: clock
    out tx: bit
}
impl LaTest {
    signal count: bit[16] = 0
    signal hit: bit
    hit = count == 100
    on(clk.rise) {
        count = count + 1
    }
    inst la = logic_analyzer<2, 4, 16, 4> {
        clk: clk,
        probes: count,
        trigger: hit
    }
    tx = la.tx
}
"#;

/// The test design next to the stdlib analyzer it instantiates, in a fresh
/// directory per test.
fn fixture(test: &str) -> String {
    let dir = std::env::temp_dir().join(format!("skalp_la_{test}_{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(
        dir.join("logic_analyzer.sk"),
        include_str!("../../crates/skalp-stdlib/components/logic_analyzer.sk"),
    )
    .unwrap();
    let top = dir.join("top.sk");
    std::fs::write(&top, TOP).unwrap();
    top.to_string_lossy().into_owned()
}

/// Decode 8N1 bytes from a line sampled once per clock.
fn decode_uart(line: &[u8], divider: usize) -> Vec<u8> {
    let mut bytes = Vec::new();
    let mut at = 0;
    while at + 10 * divider < line.len() {
        if line[at] == 0 {
            let value = (0..8).fold(0u8, |value, bit| {
                value | (line[at + divider * (bit + 1) + divider / 2] << bit)
            });
            bytes.push(value);
            at += 10 * divider - divider / 2;
        } else {
            at += 1;
        }
    }
    bytes
}

async fn capture(mut tb: Testbench, cycles: usize) -> Vec<u8> {
    let mut line = Vec::with_capacity(cycles);
    for _ in 0..cycles {
        tb.clock(1).await;
        line.push(tb.get_u64("tx").await as u8);
    }
    decode_uart(&line, DIVIDER)
}

fn check(bytes: &[u8]) {
    assert!(bytes.len() >= 4, "no capture was sent: {bytes:?}");
    assert_eq!(&bytes[..4], &[0xA5, 0x5A, 2, 4], "header");
    let samples: Vec<u16> = bytes[4..]
        .chunks(2)
        .take(16)
        .map(|pair| u16::from_le_bytes([pair[0], pair[1]]))
        .collect();
    assert_eq!(samples.len(), 16);
    // The trigger sample is number 8; samples are consecutive counter values.
    assert_eq!(samples[8], 100, "trigger sample: {samples:?}");
    for pair in samples.windows(2) {
        assert_eq!(pair[1], pair[0] + 1, "consecutive samples: {samples:?}");
    }
}

#[tokio::test]
async fn test_logic_analyzer_behavioral() {
    let tb = Testbench::with_top_module(&fixture("behavioral"), "LaTest")
        .await
        .unwrap();
    check(&capture(tb, 2000).await);
}

#[tokio::test]
async fn test_logic_analyzer_gate_level_nexus() {
    let tb = Testbench::gate_level_with_top_and_library(&fixture("gate"), "LaTest", "nexus")
        .await
        .unwrap();
    check(&capture(tb, 2000).await);
}
