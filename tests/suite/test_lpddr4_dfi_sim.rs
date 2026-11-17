//! LPDDR4 DFI controller (`skalp::memory::lpddr4_dfi_controller`) with the
//! behavioural DFI PHY and the LPDDR4 channel model, run end to end:
//! init, mode registers, ZQ calibration, write leveling, gate and read-eye
//! training and write DQ training through DFI, refresh, PHY and controller updates, and two passes of 64 bursts
//! (writes, reads, write-to-read and read-to-write turnarounds) compared
//! against their data. The channel model checks JEDEC command timing and
//! reports any cycle where both ends drive DQ or DQS.
//!
//! The simulations build the library sources and the fixture as one file:
//! with the library path set, every operator lowers to a library entity and
//! the C++ model takes several times longer to compile. A separate test
//! checks that the fixture's `use skalp::memory::...` imports resolve.

use skalp_testing::testbench::Testbench;

const LIB_FILES: [&str; 3] = [
    "crates/skalp-stdlib/skalp/memory/lpddr4_model.sk",
    "crates/skalp-stdlib/skalp/memory/dfi_lpddr4_behav_phy.sk",
    "crates/skalp-stdlib/skalp/memory/lpddr4_dfi_controller.sk",
];
const FIXTURE: &str = "tests/fixtures/lpddr4_dfi_sim.sk";
const TOP: &str = "Lpddr4DfiSim";

/// Clocks of the fast simulation clock per tCK (the PHY's CLK_PER_CK)
const CLK_PER_CK: u64 = 8;

fn path(rel: &str) -> String {
    format!("{}/{}", env!("CARGO_MANIFEST_DIR"), rel)
}

/// The library files and the fixture as one source, with the fixture's top
/// generics set from `generics`
fn combined_source(generics: &[(&str, u64)]) -> String {
    let mut src = String::new();
    for f in LIB_FILES.iter().chain([FIXTURE].iter()) {
        let text = std::fs::read_to_string(path(f)).expect("read source");
        for line in text.lines() {
            if !line.trim_start().starts_with("use skalp::") {
                src.push_str(line);
                src.push('\n');
            }
        }
    }
    let top = src
        .find(&format!("entity {TOP}<"))
        .expect("fixture top entity");
    for (name, value) in generics {
        let key = format!("const {name}: nat = ");
        let at = top + src[top..].find(&key).expect("generic") + key.len();
        let end = at
            + src[at..]
                .find(|c: char| !c.is_ascii_digit())
                .expect("generic value");
        src.replace_range(at..end, &value.to_string());
    }
    src
}

struct Variant {
    name: &'static str,
    dqsck_clks: u64,
    dq_skew_clks: u64,
    phyupd_every: u64,
    /// board delay of each byte lane's DQS and DQ behind CK, in clocks
    wr_skew: [u64; 2],
    /// further board delay of each byte lane's DQ behind its DQS
    wr_dq_skew: [u64; 2],
}

async fn run(v: Variant) {
    let src = combined_source(&[
        ("DQSCK_CLKS", v.dqsck_clks),
        ("DQ_SKEW_CLKS", v.dq_skew_clks),
        ("PHYUPD_EVERY", v.phyupd_every),
        ("WR_SKEW0_CLKS", v.wr_skew[0]),
        ("WR_SKEW1_CLKS", v.wr_skew[1]),
        ("WR_DQ_SKEW0_CLKS", v.wr_dq_skew[0]),
        ("WR_DQ_SKEW1_CLKS", v.wr_dq_skew[1]),
    ]);
    let dir = std::env::temp_dir().join(format!("skalp_lpddr4_dfi_{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let file = dir.join(format!("{}.sk", v.name));
    std::fs::write(&file, src).unwrap();

    let mut tb = Testbench::behavioral_with_top(file.to_str().unwrap(), TOP)
        .await
        .expect("build testbench");
    tb.set_capture_waveforms(false);

    // init + training take about 20k clocks, the traffic about 130k
    let mut cycles = 0;
    while tb.get_u64("done").await == 0 {
        if cycles >= 400_000 {
            let mut state = String::new();
            for s in [
                "ctrl_state",
                "gen_state",
                "init_done",
                "train_fail",
                "lvl_fail",
                "wrlvl_fail",
                "wdq_fail",
                "wl0",
                "wl1",
                "wdq0",
                "wdq1",
                "m_errors",
                "m_err_code",
                "m_err_cycle",
                "m_n_wr",
                "m_n_rd",
                "passes",
                "reads_ok",
            ] {
                state += &format!(" {s}={}", tb.get_u64(s).await);
            }
            panic!("{}: not done after {cycles} clocks:{state}", v.name);
        }
        tb.clock(10_000).await;
        cycles += 10_000;
    }
    let n = v.name;
    macro_rules! get {
        ($s:expr) => {
            tb.get_u64($s).await
        };
    }

    // the channel model saw no timing or protocol error and no contention
    let errors = get!("m_errors");
    assert_eq!(
        errors,
        0,
        "{n}: model error code {} at clock {}",
        get!("m_err_code"),
        get!("m_err_cycle")
    );
    assert_eq!(get!("m_contention"), 0, "{n}: bus contention");
    assert_eq!(get!("m_t_ck"), CLK_PER_CK, "{n}: tCK");

    // init: 8 mode-register writes and 2 more for write leveling, ZQ start +
    // latch, MR5/MR8 read back
    assert_eq!(get!("init_done"), 1, "{n}");
    assert_eq!(get!("m_n_mrw"), 10, "{n}: MRW count");
    assert_eq!(get!("m_n_zq"), 1, "{n}: ZQ calibrations");
    assert_eq!(get!("m_n_mrr"), 2, "{n}: MRR count");
    assert_eq!(get!("mr5"), 0xFF, "{n}: MR5 (manufacturer)");
    assert_eq!(get!("mr8"), 0x10, "{n}: MR8 (density/IO)");

    // training: the gate opens half a tCK before the first DQS rise, which
    // comes tDQSCK + half a tCK into the read slot; the read eye passes for
    // sample delays from the DQ skew up to half a tCK - 1, its centre is used
    assert_eq!(get!("train_fail"), 0, "{n}: training");
    assert_eq!(get!("lvl_fail"), 0, "{n}: read eye");
    let half = CLK_PER_CK / 2;
    let eye = v.dq_skew_clks + ((half - v.dq_skew_clks - 1) >> 1);
    for lane in 0..2 {
        let gate = get!(["gate0", "gate1"][lane]);
        let rdly = get!(["rdly0", "rdly1"][lane]);
        assert_eq!(gate, v.dqsck_clks, "{n}: lane {lane} gate delay");
        assert_eq!(rdly, eye, "{n}: lane {lane} read sample delay");
    }

    // write leveling: the lane's DQS is launched half a tCK early and delayed
    // until its rise meets CK's at the DRAM, which takes out the board delay
    assert_eq!(get!("wrlvl_fail"), 0, "{n}: write leveling");
    assert_eq!(get!("wdq_fail"), 0, "{n}: write DQ training");
    for lane in 0..2 {
        let wl = get!(["wl0", "wl1"][lane]);
        assert_eq!(
            wl,
            half - v.wr_skew[lane],
            "{n}: lane {lane} write leveling delay"
        );
        // write DQ: the DRAM takes a beat when DQ changed neither in the
        // clock of the DQS edge nor in the one after, so DQ may change 2 to
        // half a tCK - 1 clocks after an edge; the board DQ delay counts too
        let q = v.wr_dq_skew[lane];
        let lo = 2u64.saturating_sub(q);
        let hi = half - 1 - q;
        let wdq = get!(["wdq0", "wdq1"][lane]);
        assert_eq!(
            wdq,
            lo + ((hi - lo) >> 1),
            "{n}: lane {lane} write DQ delay"
        );
    }
    // the sweep went through failing delays, and nothing failed after it
    let viol_init = get!("m_dq_viol_init");
    assert!(viol_init > 0, "{n}: write DQ sweep saw no failing delay");
    assert_eq!(
        get!("m_dq_viol"),
        viol_init,
        "{n}: DQ timing after training"
    );

    // traffic: every read returned the data written, nothing timed out
    let bursts = 64;
    assert_eq!(get!("passes"), 2, "{n}");
    assert_eq!(get!("mismatches"), 0, "{n}: data mismatches");
    assert_eq!(get!("reads_ok"), bursts * 2 * 2, "{n}: reads");
    assert_eq!(get!("gate_timeouts"), 0, "{n}: gate timeouts");
    let train_writes = get!("m_n_wr_init");
    assert!(train_writes >= half, "{n}: write DQ training writes");
    assert_eq!(get!("m_n_wr") - train_writes, bursts * 2 * 2, "{n}: writes");
    assert_eq!(get!("m_aliased"), 0, "{n}: aliased lines");
    assert_eq!(get!("m_unwritten"), 0, "{n}: unwritten reads");

    // refresh ran at tREFI through the traffic
    assert!(get!("m_n_ref") >= 5, "{n}: refreshes");

    // updates: no command inside a PHY update window
    assert_eq!(get!("upd_violations"), 0, "{n}: update windows");
    let phyupd = get!("phyupd_count");
    if v.phyupd_every == 0 {
        assert_eq!(phyupd, 0, "{n}: PHY updates");
    } else {
        assert!(phyupd >= 10, "{n}: PHY updates, got {phyupd}");
    }

    let _ = std::fs::remove_file(&file);
}

#[tokio::test]
async fn lpddr4_dfi_nominal() {
    run(Variant {
        name: "nominal",
        dqsck_clks: 0,
        dq_skew_clks: 0,
        phyupd_every: 0,
        wr_skew: [0, 0],
        wr_dq_skew: [0, 0],
    })
    .await;
}

#[tokio::test]
async fn lpddr4_dfi_dqsck_board_skew_and_phy_updates() {
    run(Variant {
        name: "skewed",
        dqsck_clks: 13,
        dq_skew_clks: 2,
        phyupd_every: 200,
        wr_skew: [3, 1],
        wr_dq_skew: [1, 0],
    })
    .await;
}

#[test]
fn lpddr4_dfi_library_imports_resolve() {
    let stdlib = path("crates/skalp-stdlib");
    std::env::set_var("SKALP_STDLIB_PATH", &stdlib);
    let ctx =
        skalp_frontend::parse_and_build_compilation_context(std::path::Path::new(&path(FIXTURE)))
            .expect("parse fixture with library imports");
    for entity in [
        "Lpddr4DfiController",
        "Lpddr4BankState",
        "DfiLpddr4BehavPhy",
        "Lpddr4ChannelModel",
        TOP,
    ] {
        assert!(
            ctx.main_hir.entities.iter().any(|e| e.name == entity),
            "entity {entity} available to the fixture"
        );
    }
}
