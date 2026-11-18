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

/// Trained settings with no skew: write leveling and the read and write DQ
/// delays in 12.5 ps steps (read and write: 128 = the DQS buffer's middle), the
/// gate in eighths of a tCK from the controller's `rddata_en`
const WL_NOMINAL: f64 = 60.0;
const GATE_NOMINAL: f64 = 31.0;
const RD_NOMINAL: f64 = 110.0;
const WDQ_NOMINAL: f64 = 132.0;

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
    /// entities simulated at gate level (synthesized for the device, their
    /// vendor primitives running the models); none: all behavioural
    gate_level: &'static [&'static str],
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
        if ticks >= 20_000_000 {
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
    assert_eq!(get!("m_t_ck"), TICKS_PER_CK, "{n}: tCK");

    assert_eq!(get!("dll_lock"), 1, "{n}: DLL lock");
    assert_eq!(get!("init_done"), 1, "{n}");
    assert_eq!(get!("m_n_mrw"), 10, "{n}: MRW count");
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
    // primitive models with no skew; a tick is 117.1875 ps, a delay step 12.5 ps
    // (9.375 steps per tick) and a gate setting an eighth of a tCK (half a tick).
    // Write leveling and write DQ move WL_STEP / WDQ_STEP = 4 steps per event.
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
            WL_NOMINAL - steps(v.wr_skew[lane]),
            4.0,
            &format!("lane {lane} write leveling"),
        );
        // the gate follows tDQSCK
        let gate = get!(["gate0", "gate1"][lane]);
        near(
            gate,
            GATE_NOMINAL + v.dqsck as f64 / 2.0,
            1.0,
            &format!("lane {lane} gate"),
        );
        // the read sample point follows the read DQ lag behind DQS
        let rd = get!(["rdly0", "rdly1"][lane]);
        near(
            rd,
            RD_NOMINAL + steps(v.dq_skew),
            8.0,
            &format!("lane {lane} read delay"),
        );
        // write DQ takes the lane's board DQ delay behind its DQS out
        let wdq = get!(["wdq0", "wdq1"][lane]);
        near(
            wdq,
            WDQ_NOMINAL - steps(v.wr_dq_skew[lane]),
            4.0,
            &format!("lane {lane} write DQ delay"),
        );
    }

    // the write DQ sweep went through failing delays, and nothing failed after it
    let viol_init = get!("m_dq_viol_init");
    assert!(viol_init > 0, "{n}: write DQ sweep saw no failing delay");
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
        gate_level: &[],
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
        gate_level: &[],
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
        gate_level: GATE_LEVEL,
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
        gate_level: GATE_LEVEL,
    })
    .await;
}
