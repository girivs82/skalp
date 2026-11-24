//! LPDDR4 DFI controller (`skalp::memory::lpddr4_dfi_controller`) with the
//! Nexus DFI PHY (`skalp::memory::dfi_lpddr4_nexus_phy`), built for a Nexus
//! device so the PHY's DDR primitives (ECLKSYNC, ECLKDIV, DDRDLL, DQSBUF,
//! x4 gearing, delays) come from the family's simulation models, and the
//! LPDDR4 channel model, run end to end at 1066 Mbps: init, training
//! through DFI (write leveling, read gate, read eye, write DQ) and two passes
//! of traffic compared against their data. The channel model checks JEDEC
//! command timing and reports any tick where both ends drive DQ or DQS.

use skalp_testing::testbench::Testbench;

const LIB_FILES: [&str; 3] = [
    "crates/skalp-stdlib/skalp/memory/lpddr4_model.sk",
    "crates/skalp-stdlib/skalp/memory/dfi_lpddr4_nexus_phy.sk",
    "crates/skalp-stdlib/skalp/memory/lpddr4_dfi_controller.sk",
];
const FIXTURE: &str = "tests/fixtures/lpddr4_dfi_nexus_sim.sk";
const TOP: &str = "Lpddr4DfiNexusSim";
const DEVICE: &str = "lfcpnx-100";

/// Simulation ticks (117.1875 ps) per tCK at 1066 Mbps
const TICKS_PER_CK: u64 = 16;

/// Trained settings with no skew, in 12.5 ps delay steps (a tick is 9.375
/// steps) and, for the gate, eighths of a tCK from the controller's `rddata_en`.
struct Nominal {
    /// write leveling: the DQS delay starts half a tCK before CK, so the
    /// edge comes half a tCK less this many steps of the models' fixed DQS
    /// path delay ahead of CK
    wl_before_half_ck: f64,
    gate: f64,
    /// read and write DQ delays (128 = the DQS buffer's middle)
    rd: f64,
    wdq: f64,
    /// the read and write DQ sweeps (128 +- 64 steps, 1.6 ns) reach a failing
    /// delay: they do while a bit is shorter than the sweep. Where it is longer
    /// (400 Mbps) the whole sweep passes and training takes its middle
    sweep_finds_edges: bool,
}

/// [`Nominal`] at `ck_ticks` per tCK. The DQ delays come from the models'
/// eye, clipped by the sweep: measured once per rate, they pin training's
/// result against regressions.
fn nominal(ck_ticks: u64) -> Nominal {
    let (rd, wdq, sweep_finds_edges) = match ck_ticks {
        TICKS_PER_CK => (110.0, 132.0, true),
        22 => (114.0, 118.0, true),
        42 => (128.0, 128.0, false),
        _ => panic!("no nominal settings for {ck_ticks} ticks per tCK"),
    };
    Nominal {
        wl_before_half_ck: 15.0,
        gate: 31.0,
        rd,
        wdq,
        sweep_finds_edges,
    }
}

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
    /// DRAM read DQS delay (tDQSCK), in ticks
    dqsck: u64,
    /// read DQ behind DQS at the DRAM, in ticks
    dq_skew: u64,
    /// board delay of each byte lane's DQS and DQ behind CK, in ticks
    wr_skew: [u64; 2],
    /// further board delay of each byte lane's DQ behind its DQS, in ticks
    wr_dq_skew: [u64; 2],
    n_bursts: u64,
    /// simulation ticks per tCK (even); TICKS_PER_CK is 1066 Mbps
    ck_ticks: u64,
    /// entities simulated at gate level (synthesized for the device, their
    /// vendor primitives running the models); none: all behavioural
    gate_level: &'static [&'static str],
    /// board delay of CK and the command pins behind the PHY's pins, in ticks
    ca_late: u64,
    /// the controller's TPHY_WRLAT_ADJ
    wr_adj: u64,
    /// ticks the DQ drivers take to turn on at the pins after the PHY's
    /// output enable (undriven DQ reads 0 at the DRAM, as its ODT pulls it low)
    dq_oe_late: u64,
}

const STATUS: [&str; 22] = [
    "ctrl_state",
    "gen_state",
    "dll_lock",
    "init_done",
    "train_fail",
    "wrlvl_fail",
    "gate_fail",
    "lvl_fail",
    "wdq_fail",
    "wl0",
    "wl1",
    "gate0",
    "gate1",
    "rdly0",
    "rdly1",
    "wdq0",
    "wdq1",
    "m_errors",
    "m_err_code",
    "m_err_cycle",
    "passes",
    "reads_ok",
];

async fn status(tb: &mut Testbench) -> String {
    let mut s = String::new();
    for name in STATUS {
        s += &format!(" {name}={}", tb.get_u64(name).await);
    }
    s
}

async fn run(v: Variant) {
    let src = combined_source(&[
        ("DQSCK_CLKS", v.dqsck),
        ("DQ_SKEW_CLKS", v.dq_skew),
        ("N_BURSTS", v.n_bursts),
        ("WR_SKEW0_CLKS", v.wr_skew[0]),
        ("WR_SKEW1_CLKS", v.wr_skew[1]),
        ("WR_DQ_SKEW0_CLKS", v.wr_dq_skew[0]),
        ("WR_DQ_SKEW1_CLKS", v.wr_dq_skew[1]),
        ("CK_TICKS", v.ck_ticks),
        ("CA_LATE_TICKS", v.ca_late),
        ("WR_ADJ", v.wr_adj),
        ("DQ_OE_LATE_TICKS", v.dq_oe_late),
    ]);
    let dir = std::env::temp_dir().join(format!("skalp_lpddr4_nexus_{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let file = dir.join(format!("{}.sk", v.name));
    std::fs::write(&file, src).unwrap();

    let mut tb = if v.gate_level.is_empty() {
        Testbench::behavioral_for_device(file.to_str().unwrap(), TOP, DEVICE).await
    } else {
        Testbench::gate_level_dut_for_device(file.to_str().unwrap(), TOP, DEVICE, v.gate_level)
            .await
    }
    .expect("build testbench");
    tb.set_capture_waveforms(false);

    let verbose = std::env::var("SKALP_NEXUS_SIM_TRACE").is_ok();
    let mut ticks = 0u64;
    while tb.get_u64("done").await == 0 {
        // init and training take a fixed number of clocks: the bound scales with tCK
        if ticks >= 20_000_000 * v.ck_ticks / TICKS_PER_CK {
            panic!(
                "{}: not done after {ticks} ticks:{}",
                v.name,
                status(&mut tb).await
            );
        }
        tb.clock_signal("sim_tick", 50_000).await;
        ticks += 50_000;
        if verbose {
            eprintln!("{} @{ticks}:{}", v.name, status(&mut tb).await);
        }
    }
    let n = v.name;
    macro_rules! get {
        ($s:expr) => {
            tb.get_u64($s).await
        };
    }

    let errors = get!("m_errors");
    assert_eq!(
        errors,
        0,
        "{n}: model error code {} at tick {}",
        get!("m_err_code"),
        get!("m_err_cycle")
    );
    assert_eq!(get!("m_contention"), 0, "{n}: bus contention");
    assert_eq!(get!("m_t_ck"), v.ck_ticks, "{n}: tCK");

    assert_eq!(get!("dll_lock"), 1, "{n}: DLL lock");
    assert_eq!(get!("init_done"), 1, "{n}");
    assert_eq!(get!("m_n_mrw"), 14, "{n}: MRW count");
    assert_eq!(get!("m_n_zq"), 1, "{n}: ZQ calibrations");
    assert_eq!(get!("m_n_mrr"), 2, "{n}: MRR count");
    assert_eq!(get!("mr5"), 0xFF, "{n}: MR5 (manufacturer)");
    assert_eq!(get!("mr8"), 0x10, "{n}: MR8 (density/IO)");

    assert_eq!(get!("train_fail"), 0, "{n}: training");
    assert_eq!(get!("wrlvl_fail"), 0, "{n}: write leveling");
    assert_eq!(get!("gate_fail"), 0, "{n}: read gate");
    assert_eq!(get!("lvl_fail"), 0, "{n}: read eye");
    assert_eq!(get!("wdq_fail"), 0, "{n}: write DQ training");

    // Trained settings against the skews. The nominal settings are those of the
    // primitive models with no skew (see [`Nominal`]); a tick is 117.1875 ps, a delay step
    // 12.5 ps and a gate setting an eighth of a tCK. Write leveling and write DQ move
    // WL_STEP / WDQ_STEP = 4 steps per event.
    let nom = nominal(v.ck_ticks);
    let steps = |ticks: u64| ticks as f64 * 117.1875 / 12.5;
    let near = |got: u64, want: f64, tol: f64, what: &str| {
        assert!(
            (got as f64 - want).abs() <= tol,
            "{n}: {what} {got}, expected {want:.1} +- {tol}"
        );
    };
    for lane in 0..2 {
        // write leveling takes the board delay of the lane's DQS out
        let wl = get!(["wl0", "wl1"][lane]);
        near(
            wl,
            steps(v.ck_ticks) / 2.0 - nom.wl_before_half_ck - steps(v.wr_skew[lane]),
            4.0,
            &format!("lane {lane} write leveling"),
        );
        // the gate follows tDQSCK and the command delay
        let gate = get!(["gate0", "gate1"][lane]);
        near(
            gate,
            nom.gate + ((v.dqsck + v.ca_late) * 8) as f64 / v.ck_ticks as f64,
            1.0,
            &format!("lane {lane} gate"),
        );
        // the read sample point follows the read DQ lag behind DQS
        let rd = get!(["rdly0", "rdly1"][lane]);
        near(
            rd,
            nom.rd + steps(v.dq_skew),
            8.0,
            &format!("lane {lane} read delay"),
        );
        // write DQ takes the lane's board DQ delay behind its DQS out
        let wdq = get!(["wdq0", "wdq1"][lane]);
        near(
            wdq,
            nom.wdq - steps(v.wr_dq_skew[lane]),
            4.0,
            &format!("lane {lane} write DQ delay"),
        );
    }

    // the write DQ sweep went through failing delays where a bit is shorter than it, and
    // nothing failed after it
    let viol_init = get!("m_dq_viol_init");
    assert_eq!(
        viol_init > 0,
        nom.sweep_finds_edges,
        "{n}: write DQ sweep saw a failing delay ({viol_init} DQ timing errors)"
    );
    assert_eq!(
        get!("m_dq_viol"),
        viol_init,
        "{n}: DQ timing after training"
    );

    let bursts = v.n_bursts;
    assert_eq!(get!("passes"), 2, "{n}");
    assert_eq!(get!("mismatches"), 0, "{n}: data mismatches");
    assert_eq!(get!("reads_ok"), bursts * 2 * 2, "{n}: reads");
    let train_writes = get!("m_n_wr_init");
    assert_eq!(get!("m_n_wr") - train_writes, bursts * 2 * 2, "{n}: writes");
    assert_eq!(get!("m_aliased"), 0, "{n}: aliased lines");
    assert_eq!(get!("m_unwritten"), 0, "{n}: unwritten reads");
    // every read came back through both lanes' DQS buffers: the traffic and the two mode
    // register reads (MR5, MR8), two half bursts each
    let halves = (bursts * 2 * 2 + 2) * 2;
    assert_eq!(get!("bursts0"), halves, "{n}: lane 0 half bursts");
    assert_eq!(get!("bursts1"), halves, "{n}: lane 1 half bursts");

    let _ = std::fs::remove_file(&file);
}

#[tokio::test]
async fn lpddr4_dfi_nexus_nominal() {
    run(Variant {
        name: "nominal",
        dqsck: 0,
        dq_skew: 0,
        wr_skew: [0, 0],
        wr_dq_skew: [0, 0],
        n_bursts: 16,
        ck_ticks: TICKS_PER_CK,
        gate_level: &[],
        ca_late: 0,
        wr_adj: 3,
        dq_oe_late: 0,
    })
    .await;
}

/// Read DQS 2.3 ns late (tDQSCK) with DQ a tick behind it, and each lane's
/// write path delayed on the board, lane 0 more than lane 1, with lane 0's DQ
/// later again than its DQS
#[tokio::test]
async fn lpddr4_dfi_nexus_dqsck_and_board_skew() {
    run(Variant {
        name: "skewed",
        dqsck: 20,
        dq_skew: 1,
        wr_skew: [5, 2],
        wr_dq_skew: [2, 0],
        n_bursts: 16,
        ck_ticks: TICKS_PER_CK,
        gate_level: &[],
        ca_late: 0,
        wr_adj: 3,
        dq_oe_late: 0,
    })
    .await;
}

/// The PHY and the controller as they are built for the device: their
/// synthesized netlists, with the PHY's DDR primitives running their models
const GATE_LEVEL: &[&str] = &["DfiLpddr4NexusPhy", "Lpddr4DfiController"];

/// [`lpddr4_dfi_nexus_nominal`] with the PHY and controller at gate level
#[tokio::test]
async fn lpddr4_dfi_nexus_gate_level_nominal() {
    run(Variant {
        name: "gate_level_nominal",
        dqsck: 0,
        dq_skew: 0,
        wr_skew: [0, 0],
        wr_dq_skew: [0, 0],
        n_bursts: 16,
        ck_ticks: TICKS_PER_CK,
        gate_level: GATE_LEVEL,
        ca_late: 0,
        wr_adj: 3,
        dq_oe_late: 0,
    })
    .await;
}

/// [`lpddr4_dfi_nexus_dqsck_and_board_skew`] with the PHY and controller at
/// gate level
#[tokio::test]
async fn lpddr4_dfi_nexus_gate_level_dqsck_and_board_skew() {
    run(Variant {
        name: "gate_level_skewed",
        dqsck: 20,
        dq_skew: 1,
        wr_skew: [5, 2],
        wr_dq_skew: [2, 0],
        n_bursts: 16,
        ck_ticks: TICKS_PER_CK,
        gate_level: GATE_LEVEL,
        ca_late: 0,
        wr_adj: 3,
        dq_oe_late: 0,
    })
    .await;
}

/// [`lpddr4_dfi_nexus_nominal`] at about 800 Mbps (tCK 22 ticks, 2.58 ns:
/// 776 Mbps), the second step of the board ladder
#[tokio::test]
async fn lpddr4_dfi_nexus_nominal_800() {
    run(Variant {
        name: "nominal_800",
        dqsck: 0,
        dq_skew: 0,
        wr_skew: [0, 0],
        wr_dq_skew: [0, 0],
        n_bursts: 16,
        ck_ticks: 22,
        gate_level: &[],
        ca_late: 0,
        wr_adj: 3,
        dq_oe_late: 0,
    })
    .await;
}

/// [`lpddr4_dfi_nexus_nominal`] at about 400 Mbps (tCK 42 ticks, 4.92 ns:
/// 406 Mbps), the first step of the board ladder
#[tokio::test]
async fn lpddr4_dfi_nexus_nominal_400() {
    run(Variant {
        name: "nominal_400",
        dqsck: 0,
        dq_skew: 0,
        wr_skew: [0, 0],
        wr_dq_skew: [0, 0],
        n_bursts: 16,
        ck_ticks: 42,
        gate_level: &[],
        ca_late: 0,
        wr_adj: 3,
        dq_oe_late: 0,
    })
    .await;
}

/// CK and the command pins a whole tCK late on the board (as the FPGA's
/// output paths make them on the device, relative to the models), so the
/// controller sends write data a slot later (TPHY_WRLAT_ADJ = 4) and the
/// read data comes a tCK later: writes on their own (the rewrite phase) and
/// in runs must land the same as with no delay
#[tokio::test]
async fn lpddr4_dfi_nexus_command_tck_late() {
    run(Variant {
        name: "command_late",
        dqsck: 0,
        dq_skew: 0,
        wr_skew: [0, 0],
        wr_dq_skew: [0, 0],
        n_bursts: 16,
        ck_ticks: TICKS_PER_CK,
        gate_level: &[],
        ca_late: TICKS_PER_CK,
        wr_adj: 4,
        dq_oe_late: 0,
    })
    .await;
}

/// The DQ drivers turning on most of a tCK after the PHY's output enable (as
/// on a CertusPro-NX, where a lone write's first beat reached the DRAM
/// undriven): the PHY turns them on a slot ahead of the first beat, so writes
/// on their own (the rewrite phase) store every beat
#[tokio::test]
async fn lpddr4_dfi_nexus_dq_enable_late() {
    run(Variant {
        name: "dq_enable_late",
        dqsck: 0,
        dq_skew: 0,
        wr_skew: [0, 0],
        wr_dq_skew: [0, 0],
        n_bursts: 16,
        ck_ticks: TICKS_PER_CK,
        gate_level: &[],
        ca_late: 0,
        wr_adj: 3,
        dq_oe_late: TICKS_PER_CK * 3 / 4,
    })
    .await;
}
