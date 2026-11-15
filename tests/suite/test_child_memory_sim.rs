//! Behavioural simulation of a child instance's memories and bit vectors.
//!
//! An array MIR keeps whole as a memory (one write port) stays one SIR array
//! in a child instance too, read with ArrayRead and written with ArrayWrite,
//! instead of one register per entry and a mux over every entry for each
//! read (an 8K-entry memory in a child used to give ~700K SIR nodes).
//! A bit of a child's bit vector written at a run-time index (`v[i] = b`)
//! used to be dropped, and one read at a run-time index (`v[i]`) read bit 0.
//! The compiled CPU kernel's batched loop now runs long clock counts.

use skalp_frontend::parse_and_build_hir;
use skalp_mir::lower_to_mir;
use skalp_sim::{HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};
use skalp_sir::{SirModule, SirType};

const DESIGN: &str = r#"
entity Store {
    in clk: clock
    in we: bit
    in wa: bit[6]
    in wd: bit[16]
    in ra: bit[6]
    in set: bit
    in clr: bit
    in all: bit
    in fi: bit[3]
    in ri: bit[3]
    out rd: bit[16]
    out flag: bit
    out flags_o: bit[8]
}

impl Store {
    signal mem: [bit[16]; 64]
    signal flags: bit[8] = 0
    let fr: bit[3] = ri
    on(clk.rise) {
        if all == 1 {
            flags = 0
        }
        if set == 1 {
            flags[fi] = 1
        } else {
            if clr == 1 {
                flags[fi] = 0
            }
        }
        if we == 1 {
            if wa[0] == 0 {
                mem[wa] = wd
            } else {
                mem[wa] = wd ^ 16'hFFFF
            }
        }
    }
    rd = mem[ra]
    flag = flags[fr]
    flags_o = flags
}

entity ChildMemTop {
    in clk: clock
    in we: bit
    in wa: bit[6]
    in wd: bit[16]
    in ra: bit[6]
    in set: bit
    in clr: bit
    in all: bit
    in fi: bit[3]
    in ri: bit[3]
    out rd: bit[16]
    out flag: bit
    out flags_o: bit[8]
}

impl ChildMemTop {
    inst s = Store {
        clk: clk, we: we, wa: wa, wd: wd, ra: ra,
        set: set, clr: clr, all: all, fi: fi, ri: ri
    }
    rd = s.rd
    flag = s.flag
    flags_o = s.flags_o
}
"#;

fn sir_of(source: &str) -> SirModule {
    let hir = parse_and_build_hir(source).expect("parse design");
    let mir = lower_to_mir(&hir).expect("lower to MIR");
    let top = mir
        .modules
        .iter()
        .find(|m| m.name == "ChildMemTop")
        .expect("top module");
    skalp_sir::convert_mir_to_sir_with_hierarchy(&mir, top)
}

async fn simulator(sir: &SirModule) -> UnifiedSimulator {
    let config = UnifiedSimConfig {
        level: SimLevel::Behavioral,
        hw_accel: HwAccel::Cpu,
        max_cycles: 1_000_000,
        capture_waveforms: false,
        ..Default::default()
    };
    let mut sim = UnifiedSimulator::new(config).expect("create simulator");
    sim.load_behavioral(sir).await.expect("load design");
    sim
}

/// One clock: inputs set while the clock is low, flip-flops update on the rise
async fn clock(sim: &mut UnifiedSimulator) {
    sim.set_input("clk", 0).await;
    sim.step().await;
    sim.set_input("clk", 1).await;
    sim.step().await;
}

#[test]
fn child_memory_stays_one_array() {
    let sir = sir_of(DESIGN);
    let arrays: Vec<_> = sir
        .state_elements
        .values()
        .filter(|s| matches!(s.sir_type, Some(SirType::Array(_, 64))))
        .collect();
    assert_eq!(arrays.len(), 1, "the child's memory is one array state element");
    // flags + mem; flattened, the memory alone would be 64 registers
    assert!(
        sir.state_elements.len() < 10,
        "{} state elements",
        sir.state_elements.len()
    );
}

#[tokio::test]
async fn child_memory_and_bit_writes_match_a_reference() {
    let sir = sir_of(DESIGN);
    let mut sim = simulator(&sir).await;

    let mut mem = [0u64; 64];
    let mut flags: u64 = 0;
    let mut seed: u64 = 0x1234_5678;
    let mut next = |bits: u32| {
        seed = seed
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        (seed >> 33) & ((1u64 << bits) - 1)
    };
    let mut checked = 0;
    for cycle in 0..600 {
        let we = next(1);
        let wa = next(6);
        let wd = next(16);
        let ra = next(6);
        let set = next(2) == 0;
        let clr = next(2) == 0;
        let all = next(5) == 0;
        let fi = next(3);
        let ri = next(3);
        for (name, value) in [
            ("we", we),
            ("wa", wa),
            ("wd", wd),
            ("ra", ra),
            ("set", set as u64),
            ("clr", clr as u64),
            ("all", all as u64),
            ("fi", fi),
            ("ri", ri),
        ] {
            sim.set_input(name, value).await;
        }
        clock(&mut sim).await;

        // reference: the same clocked block, statement by statement
        if all {
            flags = 0;
        }
        if set {
            flags |= 1 << fi;
        } else if clr {
            flags &= !(1 << fi);
        }
        if we == 1 {
            mem[wa as usize] = if wa & 1 == 0 { wd } else { wd ^ 0xFFFF };
        }

        let rd = sim.get_output("rd").await.expect("rd");
        let flag = sim.get_output("flag").await.expect("flag");
        let flags_o = sim.get_output("flags_o").await.expect("flags_o");
        assert_eq!(flags_o, flags, "flags after cycle {cycle}");
        assert_eq!(flag, (flags >> ri) & 1, "flags[{ri}] after cycle {cycle}");
        assert_eq!(rd, mem[ra as usize], "mem[{ra}] after cycle {cycle}");
        checked += 1;
    }
    assert_eq!(checked, 600);
}

const COUNTER: &str = r#"
entity Logger {
    in clk: clock
    in ra: bit[7]
    out rd: bit[16]
    out count: bit[16]
}

impl Logger {
    signal log: [bit[16]; 128]
    signal n: bit[16] = 0
    signal acc: bit[16] = 1
    on(clk.rise) {
        n = n + 1
        acc = acc * 3 + n
        log[n[6:0]] = acc
    }
    rd = log[ra]
    count = n
}

entity ChildMemTop {
    in clk: clock
    in ra: bit[7]
    out rd: bit[16]
    out count: bit[16]
}

impl ChildMemTop {
    inst l = Logger { clk: clk, ra: ra }
    rd = l.rd
    count = l.count
}
"#;

/// A long run goes through the compiled kernel's batched loop; it must end
/// in the same state as stepping each half clock period
#[tokio::test]
async fn batched_run_matches_stepping() {
    let sir = sir_of(COUNTER);
    let cycles = 1000u64;

    let mut batched = simulator(&sir).await;
    batched.set_input("ra", 0).await;
    batched.set_input("clk", 1).await;
    batched.run_batched(cycles).await;

    let mut stepped = simulator(&sir).await;
    stepped.set_input("ra", 0).await;
    for _ in 0..cycles {
        clock(&mut stepped).await;
    }

    // reference
    let mut log = [0u64; 128];
    let mut n: u64 = 0;
    let mut acc: u64 = 1;
    for _ in 0..cycles {
        // every right-hand side reads the values from before the edge
        log[(n & 0x7F) as usize] = acc;
        acc = (acc * 3 + n) & 0xFFFF;
        n = (n + 1) & 0xFFFF;
    }

    assert_eq!(batched.get_output("count").await, Some(cycles));
    assert_eq!(stepped.get_output("count").await, Some(cycles));
    for ra in 0..128u64 {
        batched.set_input("ra", ra).await;
        stepped.set_input("ra", ra).await;
        // evaluate the read without a clock edge
        batched.set_input("clk", 0).await;
        batched.step().await;
        stepped.set_input("clk", 0).await;
        stepped.step().await;
        let b = batched.get_output("rd").await;
        let s = stepped.get_output("rd").await;
        assert_eq!(b, s, "log[{ra}]: batched vs stepped");
        assert_eq!(s, Some(log[ra as usize]), "log[{ra}]");
    }
}
