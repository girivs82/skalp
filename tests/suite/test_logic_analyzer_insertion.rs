//! `synth --debug` adds the logic analyzer to a design: the result must still
//! compile, record the traced signals around the trigger, and send them.

use skalp_mir::logic_analyzer::{add_analyzer, parse_capture, probe_value, DebugLink};
use skalp_testing::Testbench;

const DESIGN: &str = r#"
entity Watched {
    in clk: clock
    out low: bit
}
impl Watched {
    #[trace(radix = unsigned)]
    signal count: bit[12] = 0
    #[trace]
    signal phase: bit[3] = 0
    #[breakpoint(condition = "count == 200")]
    signal hit: bit
    hit = count == 200
    on(clk.rise) {
        count = count + 1
        phase = phase + 3
    }
    low = count[0]
}
"#;

#[tokio::test]
async fn test_debug_build_captures_traced_signals_around_the_trigger() {
    let dir = std::env::temp_dir().join(format!("skalp_la_{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let original = dir.join("watched.sk");
    std::fs::write(&original, DESIGN).unwrap();

    let context = skalp_frontend::parse_and_build_compilation_context(&original).unwrap();
    let mir = skalp_mir::MirCompiler::new()
        .with_optimization_level(skalp_mir::OptimizationLevel::None)
        .compile_to_mir_with_modules(&context.main_hir, &context.module_hirs)
        .unwrap();
    let top = mir.modules.iter().find(|m| m.name == "Watched").unwrap();
    let link = DebugLink {
        tx_pin: "M9",
        io_standard: "LVCMOS33",
        clock_hz: 4_000_000.0,
        depth_bits: 4,
        baud: 1_000_000,
    };
    let (edited, map) = add_analyzer(DESIGN, top, &link).unwrap();
    assert_eq!(map.probe_bytes, 2, "15 probe bits fit in two bytes");
    assert_eq!(map.probes[0].name, "count");
    assert_eq!(map.probes[1].offset, 12);

    let debug_source = dir.join("watched_debug.sk");
    std::fs::write(&debug_source, edited).unwrap();
    let mut tb = Testbench::gate_level_with_top_and_library(
        debug_source.to_str().unwrap(),
        "Watched",
        "nexus",
    )
    .await
    .unwrap();

    // Sample the UART line once per clock and decode 8N1 at four clocks a bit.
    let mut line = Vec::new();
    for _ in 0..2600 {
        tb.clock(1).await;
        line.push(tb.get_u64("skalp_debug_tx").await as u8);
    }
    let mut stream = Vec::new();
    let mut at = 0;
    while at + 40 < line.len() {
        if line[at] == 0 {
            stream.push((0..8).fold(0u8, |b, bit| b | (line[at + 4 * (bit + 1) + 2] << bit)));
            at += 38;
        } else {
            at += 1;
        }
    }

    let samples = parse_capture(&stream, &map).unwrap();
    let count =
        |i: usize| u16::from_le_bytes(probe_value(&samples[i], &map.probes[0]).try_into().unwrap());
    assert_eq!(count(map.trigger_index), 200, "the trigger sample");
    for (i, sample) in samples.iter().enumerate().skip(1) {
        assert_eq!(count(i), count(i - 1) + 1, "consecutive samples");
        let phase = probe_value(sample, &map.probes[1])[0];
        assert_eq!(u16::from(phase), (count(i) * 3) % 8, "phase follows count");
    }
    std::fs::remove_dir_all(&dir).ok();
}
