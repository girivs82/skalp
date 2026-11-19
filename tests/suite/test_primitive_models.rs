//! Vendor primitives with simulation models.
//!
//! A design built for a device instantiates the vendor primitives of the
//! device's family by name, without declaring them; their declarations, with
//! a simulation model as the body, come from the family's primitive library
//! built into SKALP. A model that needs finer time than a design clock names
//! a sim clock port (`#[lattice_ip(sim_clock = "tick")]`): simulation drives
//! it from a top-level `sim_tick` input, and synthesis and the netlist leave
//! it out. Synthesis keeps the primitive as a black box whose parameters are
//! the instance's generics.

use std::path::Path;

use skalp_frontend::parse_and_build_compilation_context_from_source_for_device;
use skalp_mir::MirCompiler;
use skalp_testing::testbench::Testbench;

const DEVICE: &str = "lfcpnx-100";
const SIM_CLOCK: &str = "tests/fixtures/primitive_models/sim_clock.sk";
const NEXUS_TX: &str = "tests/fixtures/primitive_models/nexus_tx.sk";
const NEXUS_LOOP: &str = "tests/fixtures/primitive_models/nexus_loop.sk";
const MIXED: &str = "tests/fixtures/gate_level/mixed_dut.sk";

/// Ticks of `sim_tick` per edge clock period in the Nexus fixtures
const ECLK_TICKS: usize = 16;
/// Ticks per SCLK period (ECLK / 4)
const SCLK_TICKS: usize = 4 * ECLK_TICKS;

fn mir_for_device(
    name: &str,
    source: &str,
    device: Option<&str>,
) -> anyhow::Result<skalp_mir::Mir> {
    let ctx = parse_and_build_compilation_context_from_source_for_device(
        Path::new(name),
        source.to_string(),
        device,
    )?;
    MirCompiler::new()
        .with_optimization_level(skalp_mir::OptimizationLevel::None)
        .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
        .map_err(|e| anyhow::anyhow!("{e}"))
}

/// The cells of a design's netlist, synthesized for the Nexus library
fn nexus_cells(mir: &skalp_mir::Mir) -> Vec<skalp_lir::gate_netlist::Cell> {
    let (hier, _) = skalp_lir::lower_mir_hierarchical_for_optimize_first(mir);
    let library = skalp_lir::get_stdlib_library("nexus").expect("nexus library");
    skalp_lir::synthesize_hierarchical(&hier, &library, skalp_lir::synth::SynthPreset::Quick)
        .flatten()
        .cells
}

/// A model's sim clock is driven from the top-level `sim_tick`, through a
/// parent that never mentions it, and the design's own clock is left alone
#[tokio::test]
async fn sim_clock_is_driven_from_sim_tick() {
    let mut tb = Testbench::behavioral_with_top(SIM_CLOCK, "SimClockTop")
        .await
        .expect("build");
    tb.set("a", 1u8);
    tb.clock(3).await;
    assert_eq!(
        tb.get_u32("z").await,
        0,
        "the design clock does not tick the model"
    );
    assert_eq!(tb.get_u32("zz").await, 0);
    tb.clock_signal("sim_tick", 1).await;
    assert_eq!(tb.get_u32("z").await, 1, "one sim_tick moves the model");
    assert_eq!(tb.get_u32("zz").await, 1, "the model inside a child too");
    tb.set("a", 0u8);
    tb.clock_signal("sim_tick", 1).await;
    assert_eq!(tb.get_u32("z").await, 0);
    assert_eq!(tb.get_u32("zz").await, 0);
}

/// The sim clock port is a simulation input only: the netlist's black box
/// has no pin for it, and the top keeps its own ports
#[test]
fn sim_clock_port_is_not_in_the_netlist() {
    let source = std::fs::read_to_string(SIM_CLOCK).expect("fixture");
    let mir = mir_for_device(SIM_CLOCK, &source, None).expect("compile");
    let top_of = |mir: &skalp_mir::Mir| {
        mir.modules
            .iter()
            .find(|m| m.name == "SimClockTop")
            .expect("top")
            .clone()
    };
    let has_sim_tick = |m: &skalp_mir::Module| m.ports.iter().any(|p| p.name == "sim_tick");
    assert!(!has_sim_tick(&top_of(&mir)), "a build has no sim_tick port");
    let mut sim_mir = mir.clone();
    assert!(skalp_mir::sim_clocks::wire_sim_clocks(
        &mut sim_mir,
        top_of(&mir).id
    ));
    assert!(
        has_sim_tick(&top_of(&sim_mir)),
        "simulation adds a sim_tick input to the top"
    );
    let cells = nexus_cells(&mir);
    let boxes: Vec<_> = cells
        .iter()
        .filter(|c| c.cell_type == "BLACKBOX_DLYTEST")
        .collect();
    assert_eq!(boxes.len(), 2, "both instances stay black boxes");
    for cell in boxes {
        assert_eq!(cell.inputs.len(), 1, "{}: only A, no tick pin", cell.path);
    }
}

/// A Nexus design built for a Nexus device takes ECLKSYNC, ECLKDIV and
/// ODDRX4 from the library. The word registered on an SCLK rise leaves the
/// pin two SCLK cycles later, D0 first, one bit per ECLK edge.
#[tokio::test]
async fn nexus_output_gearing_from_the_library() {
    let mut tb = Testbench::behavioral_for_device(NEXUS_TX, "NexusTx", DEVICE)
        .await
        .expect("build");
    tb.set("rst", 1u8);
    tb.clock_signal("sim_tick", 2 * SCLK_TICKS).await;
    tb.set("rst", 0u8);

    let total = 40 * SCLK_TICKS;
    let mut q = Vec::with_capacity(total);
    let mut eclk = Vec::with_capacity(total);
    let mut sclk = Vec::with_capacity(total);
    let mut n = Vec::with_capacity(total);
    for _ in 0..total {
        tb.clock_signal("sim_tick", 1).await;
        q.push(tb.get_u32("q").await);
        eclk.push(tb.get_u32("eclk_o").await);
        sclk.push(tb.get_u32("sclk_o").await);
        n.push(tb.get_u32("n_o").await);
    }
    let rises = |v: &[u32]| -> Vec<usize> {
        (1..v.len())
            .filter(|&t| v[t - 1] == 0 && v[t] == 1)
            .collect()
    };
    let sclk_rises = rises(&sclk);
    let eclk_rises = rises(&eclk);
    assert!(sclk_rises.len() > 30, "SCLK runs");
    for w in sclk_rises.windows(2) {
        assert_eq!(w[1] - w[0], SCLK_TICKS, "SCLK = ECLK / 4");
    }
    for w in eclk_rises.windows(2) {
        assert_eq!(w[1] - w[0], ECLK_TICKS);
    }
    // Each SCLK rise registers the word n held before it; its 8 bits leave
    // on the 8 ECLK edges starting with the ECLK rise that SCLK rises with,
    // two SCLK cycles later
    let mut checked = 0;
    for k in 2..sclk_rises.len() - 3 {
        let word = n[sclk_rises[k] - 1];
        let start = sclk_rises[k + 2];
        let first_edge = *eclk_rises.iter().find(|&&t| t >= start).expect("ECLK edge");
        assert!(
            first_edge - start < ECLK_TICKS / 2,
            "ECLK and SCLK rise together"
        );
        for bit in 0..8 {
            // sample in the middle of each half edge clock period
            let t = first_edge + bit * ECLK_TICKS / 2 + ECLK_TICKS / 4;
            assert_eq!(
                q[t],
                (word >> bit) & 1,
                "word {word:#04x} (SCLK rise {k}), bit {bit}"
            );
        }
        checked += 1;
    }
    assert!(checked > 30);
}

/// A vendor primitive's body is its simulation model: the netlist has the
/// primitive's black box and nothing from the instances inside the model
#[test]
fn model_instances_stay_out_of_the_netlist() {
    let source = std::fs::read_to_string(NEXUS_TX).expect("fixture");
    let mir = mir_for_device(NEXUS_TX, &source, Some(DEVICE)).expect("compile");
    let cells = nexus_cells(&mir);
    let boxes: Vec<_> = cells
        .iter()
        .filter(|c| c.cell_type == "BLACKBOX_ODDRX4")
        .collect();
    assert_eq!(boxes.len(), 1, "ODDRX4 is a black box");
    let inside: Vec<_> = cells
        .iter()
        .filter(|c| c.path.starts_with("top.o.") && !c.cell_type.starts_with("BLACKBOX_"))
        .map(|c| format!("{} ({})", c.path, c.cell_type))
        .collect();
    assert!(
        inside.is_empty(),
        "cells from the ODDRX4 model's gearbox: {inside:?}"
    );
}

/// A design simulated at gate level (its synthesized netlist, the ODDRX4 in
/// it running its model) inside a behavioural testbench gives, tick for tick,
/// what its behavioural simulation gives: counters on the carry chain,
/// flip-flops with enable and reset, a distributed RAM and the gearbox output
#[tokio::test]
async fn gate_level_dut_matches_its_behaviour() {
    let mut runs = Vec::new();
    for gate_level in [false, true] {
        let mut tb = if gate_level {
            Testbench::gate_level_dut_for_device(MIXED, "MixedBench", DEVICE, &["MixedDut"]).await
        } else {
            Testbench::behavioral_for_device(MIXED, "MixedBench", DEVICE).await
        }
        .expect("build");
        tb.set("rst", 1u8).set("en", 0u8).set("din", 0u8);
        tb.clock_signal("sim_tick", 4 * SCLK_TICKS).await;
        tb.set("rst", 0u8);
        let mut trace = Vec::new();
        for i in 0..60 * SCLK_TICKS {
            // inputs change away from the SCLK edges, as a testbench drives them
            if i % SCLK_TICKS == SCLK_TICKS / 2 {
                let k = i / SCLK_TICKS;
                tb.set("en", (k % 7 != 3) as u8);
                tb.set("din", ((k * 5 + 3) % 16) as u8);
            }
            tb.clock_signal("sim_tick", 1).await;
            let mut row = Vec::new();
            for name in ["q", "count", "rd", "acc"] {
                row.push(tb.get_u32(name).await);
            }
            trace.push(row);
        }
        runs.push(trace);
    }
    let (rtl, gates) = (&runs[0], &runs[1]);
    assert!(
        rtl.iter()
            .map(|r| r[1])
            .collect::<std::collections::HashSet<_>>()
            .len()
            > 30,
        "the counter runs"
    );
    assert!(
        rtl.iter().any(|r| r[0] == 1) && rtl.iter().any(|r| r[0] == 0),
        "q toggles"
    );
    for (t, (a, b)) in rtl.iter().zip(gates).enumerate() {
        assert_eq!(
            a, b,
            "tick {t}: behavioural [q, count, rd, acc] vs gate level"
        );
    }
}

/// Gate-level substitution names an entity the design does not instantiate:
/// an error, not a silently behavioural run
#[tokio::test]
async fn gate_level_dut_must_be_in_the_design() {
    let result =
        Testbench::gate_level_dut_for_device(MIXED, "MixedBench", DEVICE, &["NoSuchEntity"]).await;
    let err = result
        .err()
        .expect("an unknown entity is an error")
        .to_string();
    assert!(err.contains("NoSuchEntity"), "{err}");
}

/// A Nexus primitive used without a device to take it from is an unknown
/// entity: the build fails rather than leave a part of the design unmodelled
#[test]
fn library_primitive_without_a_device_is_an_error() {
    let source = std::fs::read_to_string(NEXUS_TX).expect("fixture");
    assert!(mir_for_device(NEXUS_TX, &source, None).is_err());
    assert!(mir_for_device(NEXUS_TX, &source, Some(DEVICE)).is_ok());
    // a family without a primitive library has no ODDRX4 either
    assert!(mir_for_device(NEXUS_TX, &source, Some("xc7a35t")).is_err());
}

/// A design that declares a primitive itself keeps its own declaration
#[tokio::test]
async fn design_declaration_overrides_the_library() {
    let dir = std::env::temp_dir().join(format!("skalp_prim_override_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("temp dir");
    let path = dir.join("own_oddr.sk");
    std::fs::write(
        &path,
        r#"
#[lattice_ip(name = "ODDRX4")]
entity ODDRX4 {
    in D0: bit
    in D1: bit
    in D2: bit
    in D3: bit
    in D4: bit
    in D5: bit
    in D6: bit
    in D7: bit
    in ECLK: clock
    in SCLK: clock
    in RST: bit
    out Q: bit
}

impl ODDRX4 {
    Q = D3
}

entity OwnTop {
    in clk: clock
    in d: bit[8]
    out q: bit
}

impl OwnTop {
    inst o = ODDRX4 {
        D0: d[0], D1: d[1], D2: d[2], D3: d[3], D4: d[4], D5: d[5], D6: d[6], D7: d[7],
        ECLK: clk, SCLK: clk, RST: 0
    }
    q = o.Q
}
"#,
    )
    .expect("write design");
    let mut tb = Testbench::behavioral_for_device(path.to_str().unwrap(), "OwnTop", DEVICE)
        .await
        .expect("build");
    tb.set("d", 0x08u8);
    tb.clock(1).await;
    assert_eq!(tb.get_u32("q").await, 1);
    tb.set("d", 0xF7u8);
    tb.clock(1).await;
    assert_eq!(tb.get_u32("q").await, 0);
    let _ = std::fs::remove_dir_all(&dir);
}

/// A vendor entity's generics are the black box's parameters, next to the
/// fixed ones of its attribute, and in simulation they set the model
#[tokio::test]
async fn generics_become_cell_parameters() {
    let source = r#"
entity DelayTop {
    in sim_tick: clock
    in a: bit
    out z: bit
}

impl DelayTop {
    inst d = DELAYB<80> { A: a }
    z = d.Z
}
"#;
    let mir = mir_for_device("delay_top.sk", source, Some(DEVICE)).expect("compile");
    let cells = nexus_cells(&mir);
    let delay = cells
        .iter()
        .find(|c| c.cell_type == "BLACKBOX_DELAYB")
        .expect("DELAYB black box");
    let param = |name: &str| {
        delay
            .parameters
            .get(name)
            .map(|v| v.trim_matches('"').to_string())
    };
    assert_eq!(param("DEL_VALUE").as_deref(), Some("80"));
    assert_eq!(param("DEL_MODE").as_deref(), Some("USER_DEFINED"));

    // 80 steps of 12.5 ps = 1 ns = 8 ticks of 117.1875 ps (rounded down)
    let dir = std::env::temp_dir().join(format!("skalp_prim_delay_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("temp dir");
    let path = dir.join("delay_top.sk");
    std::fs::write(&path, source).expect("write design");
    let mut tb = Testbench::behavioral_for_device(path.to_str().unwrap(), "DelayTop", DEVICE)
        .await
        .expect("build");
    tb.set("a", 0u8);
    tb.clock_signal("sim_tick", 40).await;
    tb.set("a", 1u8);
    let mut ticks = 0;
    loop {
        tb.clock_signal("sim_tick", 1).await;
        ticks += 1;
        if tb.get_u32("z").await == 1 {
            break;
        }
        assert!(ticks < 32, "the edge never arrived");
    }
    assert_eq!(
        ticks,
        8 + 1,
        "8 ticks of delay after the tick that samples A"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// One DDR byte lane looped back on itself, every block from the library:
/// the DLL locks, the DQS buffer finds each burst, and the read gearbox
/// returns the two words the write gearboxes sent
#[tokio::test]
async fn nexus_byte_lane_loopback() {
    let mut tb = Testbench::behavioral_for_device(NEXUS_LOOP, "NexusLoop", DEVICE)
        .await
        .expect("build");
    tb.set("pause", 0u8);
    tb.set("rst", 1u8);
    tb.clock_signal("sim_tick", 20).await;
    tb.set("rst", 0u8);
    // 4 bursts, one every 16 SCLK cycles after the first 16
    tb.clock_signal("sim_tick", 84 * SCLK_TICKS).await;

    assert_eq!(tb.get_u32("locked").await, 1, "DLL locked");
    // a quarter of the 16-tick edge clock period
    assert_eq!(tb.get_u32("code").await, 4, "DLL code");
    let bursts = tb.get_u32("bursts").await;
    assert!(bursts >= 4, "{bursts} bursts detected");
    assert_eq!(tb.get_u32("bts").await, bursts, "one BTDETECT per burst");
    assert_eq!(
        tb.get_u32("valids").await,
        2 * bursts,
        "two words (eight beats each) per burst"
    );
    assert_eq!(tb.get_u32("w0").await, 0xA6, "first word of a burst");
    assert_eq!(tb.get_u32("w1").await, 0x3C, "second word of a burst");
}

/// DATAVALID marks a word of eight beats counted from the first strobe pulse
/// the gate passed, as on the device: a gate that passes only the first four
/// pulses of a burst brings one word, its first, and the burst's second half
/// never comes back (the old model returned a word per SCLK cycle of READ)
#[tokio::test]
async fn nexus_dqs_buffer_returns_a_word_per_eight_captured_beats() {
    // the lane with the read gate a whole SCLK cycle early: it opens in the preamble's SCLK
    // cycle and closes after the first four strobe pulses (a wrapper in a file of its own: the
    // fixture's NexusLoop stays the top of the other tests with its default generics)
    let wrapper = r#"
entity NexusLoopEarlyGate {
    in sim_tick: clock
    in rst: bit
    in pause: bit
    out valids: bit[8]
    out w0: bit[8]
    out w1: bit[8]
}

impl NexusLoopEarlyGate {
    inst l = NexusLoop<0, 0, 2> { sim_tick: sim_tick, rst: rst, pause: pause }
    valids = l.valids
    w0 = l.w0
    w1 = l.w1
}
"#;
    let dir = std::env::temp_dir().join(format!("skalp_prim_early_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("temp dir");
    let path = dir.join("nexus_loop_early.sk");
    let loop_src = std::fs::read_to_string(NEXUS_LOOP).expect("read fixture");
    std::fs::write(&path, format!("{loop_src}\n{wrapper}")).expect("write design");
    let mut tb =
        Testbench::behavioral_for_device(path.to_str().unwrap(), "NexusLoopEarlyGate", DEVICE)
            .await
            .expect("build");
    tb.set("pause", 0u8);
    tb.set("rst", 1u8);
    tb.clock_signal("sim_tick", 20).await;
    tb.set("rst", 0u8);
    tb.clock_signal("sim_tick", 84 * SCLK_TICKS).await;

    assert_eq!(tb.get_u32("valids").await, 4, "one word per burst");
    assert_eq!(tb.get_u32("w0").await, 0xA6, "the first half of each burst");
    assert_eq!(tb.get_u32("w1").await, 0xA6, "the first half of each burst");
    let _ = std::fs::remove_dir_all(&dir);
}

/// While the DQS buffer's PAUSE is high its read side ignores READ: the same
/// looped-back lane detects no burst and returns no word. (A PHY that held
/// PAUSE high while it moved a delay missed every read gate behind the move.)
#[tokio::test]
async fn nexus_dqs_buffer_ignores_read_while_paused() {
    let mut tb = Testbench::behavioral_for_device(NEXUS_LOOP, "NexusLoop", DEVICE)
        .await
        .expect("build");
    tb.set("pause", 1u8);
    tb.set("rst", 1u8);
    tb.clock_signal("sim_tick", 20).await;
    tb.set("rst", 0u8);
    tb.clock_signal("sim_tick", 84 * SCLK_TICKS).await;

    assert_eq!(tb.get_u32("locked").await, 1, "DLL locked");
    assert_eq!(tb.get_u32("bursts").await, 0, "no burst detected");
    assert_eq!(tb.get_u32("bts").await, 0, "no first pulse detected");
    assert_eq!(tb.get_u32("valids").await, 0, "no word returned");

    // released, the next bursts are found again
    tb.set("pause", 0u8);
    tb.clock_signal("sim_tick", 48 * SCLK_TICKS).await;
    assert!(tb.get_u32("bursts").await >= 2, "bursts after PAUSE falls");
}

/// A bare integer (or a choice between bare integers) inside a concatenation
/// takes the fewest bits that hold its values. In a clocked block it used to
/// be 32 bits, so `{r[6:0], if c { 1 } else { 0 }}` was 39 bits wide and the
/// compiled simulator failed to build.
#[tokio::test]
async fn unsized_concatenation_parts_take_their_fewest_bits() {
    let dir = std::env::temp_dir().join(format!("skalp_prim_cat_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("temp dir");
    let path = dir.join("cat.sk");
    std::fs::write(
        &path,
        r#"
entity Cat {
    in clk: clock
    in c: bit
    in a: bit[5]
    out h: bit[8]
    out k: bit[8]
    out m: bit[8]
}

impl Cat {
    signal r: bit[8] = 0
    signal s: bit[8] = 0
    on(clk.rise) {
        r = {r[6:0], if c == 1 { 1 } else { 0 }}
        s = {a, 5}
    }
    h = r
    k = s
    m = {a[3:0], 0, 0, if c == 1 { 2 } else { 1 }}
}
"#,
    )
    .expect("write design");
    let mut tb = Testbench::behavioral(path.to_str().unwrap())
        .await
        .expect("build");
    let bits = [1u8, 0, 1, 1, 0, 0, 1, 0, 1];
    let mut expect: u32 = 0;
    for (i, &b) in bits.iter().enumerate() {
        let a = (i as u32 * 7 + 3) & 0x1F;
        tb.set("c", b);
        tb.set("a", a as u8);
        tb.clock(1).await;
        expect = ((expect << 1) | b as u32) & 0xFF;
        assert_eq!(
            tb.get_u32("h").await,
            expect,
            "shift register after bit {i}"
        );
        // `5` is 3 bits: {a, 3'b101}
        assert_eq!(tb.get_u32("k").await, ((a << 3) | 5) & 0xFF, "{{a, 5}}");
        // two 1-bit zeros and a 2-bit choice: {a[3:0], 1'b0, 1'b0, 2'bxx}
        let low = if b == 1 { 2 } else { 1 };
        assert_eq!(
            tb.get_u32("m").await,
            ((a & 0xF) << 4) | low,
            "{{a[3:0], 0, 0, ..}}"
        );
    }
    let _ = std::fs::remove_dir_all(&dir);
}

/// A long run on a clock not named `clk` goes through the batched loop on
/// that clock
#[tokio::test]
async fn long_run_on_a_clock_not_named_clk() {
    let dir = std::env::temp_dir().join(format!("skalp_prim_tick_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("temp dir");
    let path = dir.join("ticker.sk");
    std::fs::write(
        &path,
        r#"
entity Ticker {
    in tick: clock
    out n: bit[16]
}

impl Ticker {
    signal c: bit[16] = 0
    on(tick.rise) {
        c = c + 1
    }
    n = c
}
"#,
    )
    .expect("write design");
    let mut tb = Testbench::behavioral(path.to_str().unwrap())
        .await
        .expect("build");
    tb.clock_signal("tick", 1000).await;
    assert_eq!(tb.get_u32("n").await, 1000);
    tb.clock_signal("tick", 5).await;
    assert_eq!(tb.get_u32("n").await, 1005);
    let _ = std::fs::remove_dir_all(&dir);
}

/// `skalp synth` for a device takes the declarations of the family's
/// primitives from the library, as simulation for the device does, so a
/// design that instantiates them by name builds from the command line
#[test]
fn synth_for_a_device_takes_the_family_primitives() {
    let dir = tempfile::TempDir::new().unwrap();
    let source = Path::new(env!("CARGO_MANIFEST_DIR")).join(NEXUS_TX);
    let output = std::process::Command::new(env!("CARGO_BIN_EXE_skalp"))
        .args([
            "synth",
            source.to_str().unwrap(),
            "-d",
            "nexus-lfcpnx100",
            "-o",
            "out",
        ])
        .current_dir(dir.path())
        .output()
        .expect("skalp runs");
    assert!(
        output.status.success(),
        "skalp synth failed:\n{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let netlist =
        std::fs::read_to_string(dir.path().join("out/design_gates.json")).expect("gate netlist");
    for prim in ["ECLKSYNC", "ECLKDIV", "ODDRX4"] {
        assert_eq!(
            netlist.matches(&format!("\"BLACKBOX_{prim}\"")).count(),
            1,
            "one {prim} black box"
        );
    }
}
