//! Cross-check of the timed gate simulator against the cycle-based gate
//! simulator: on a synchronous design clocked slowly enough to meet its
//! timing, both must give the same outputs after every clock edge.

use skalp_frontend::parse_and_build_hir;
use skalp_lir::gate_netlist::GateNetlist;
use skalp_lir::ncl_bundle::LibraryDelays;
use skalp_lir::timed_sim::{Logic, PowerUp, TimedSim};
use skalp_lir::{get_stdlib_library, lower_mir_module_to_lir, synthesize, SynthPreset};
use skalp_mir::MirCompiler;
use skalp_sim::{convert_gate_netlist_to_sir, GateLevelSimulator};

/// Clock period for the timed simulation: far longer than any path of
/// these small designs with the library's estimates
const PERIOD_PS: i64 = 200_000;

/// A small deterministic generator for input stimulus
struct Stimulus(u64);

impl Stimulus {
    fn next(&mut self) -> u64 {
        self.0 ^= self.0 << 13;
        self.0 ^= self.0 >> 7;
        self.0 ^= self.0 << 17;
        self.0
    }
}

/// The netlist's name for bit `bit` of a port `width` bits wide
fn bit_name(name: &str, width: usize, bit: usize) -> String {
    if width == 1 {
        name.to_string()
    } else {
        format!("{name}[{bit}]")
    }
}

/// The gate netlist of the last (top) module of `source`
fn netlist_of(source: &str, top: &str) -> GateNetlist {
    let hir = parse_and_build_hir(source).expect("parse");
    let mir = MirCompiler::new().compile(&hir).expect("MIR");
    let library = get_stdlib_library("generic_asic").expect("library");
    let module = mir
        .modules
        .iter()
        .find(|m| m.name == top)
        .expect("top module");
    let lir = lower_mir_module_to_lir(module);
    synthesize(&lir.lir, &library, SynthPreset::Quick).netlist
}

/// Run `cycles` clock cycles of `source` in both simulators, with the
/// inputs (`name`, width) driven from a seeded generator (except `rst`,
/// high for the first two cycles), and compare every output after every
/// rising edge
fn cross_check(source: &str, top: &str, inputs: &[(&str, usize)], outputs: &[&str], cycles: u64) {
    let netlist = netlist_of(source, top);
    let library = get_stdlib_library("generic_asic").expect("library");
    let delays = LibraryDelays::new(&library);

    let sir = convert_gate_netlist_to_sir(&netlist);
    let mut cycle_sim = GateLevelSimulator::new(&sir.sir);
    let mut timed = TimedSim::with_power_up(&netlist, &delays, PowerUp::Zero).expect("timed sim");

    let clk = timed.net("clk").expect("clk");
    let input_bits: Vec<Vec<_>> = inputs
        .iter()
        .map(|(name, width)| {
            if *width == 1 {
                vec![timed.net(name).expect("input")]
            } else {
                timed.bus(name)
            }
        })
        .collect();
    let output_bits: Vec<Vec<_>> = outputs
        .iter()
        .map(|name| {
            let bits = timed.bus(name);
            if bits.is_empty() {
                vec![timed.net(name).expect("output")]
            } else {
                bits
            }
        })
        .collect();

    let mut stimulus = Stimulus(0x9E37_79B9_7F4A_7C15);
    let mut seen = std::collections::HashSet::new();
    timed.set(clk, Logic::Zero);
    timed.run_until(PERIOD_PS / 2).unwrap();
    for cycle in 0..cycles {
        // Inputs change while the clock is low
        let mut values = Vec::new();
        for ((name, width), bits) in inputs.iter().zip(&input_bits) {
            let value = if *name == "rst" {
                u64::from(cycle < 2)
            } else {
                stimulus.next()
            };
            let value_bits: Vec<bool> = (0..*width).map(|b| (value >> b) & 1 == 1).collect();
            for (&net, &bit) in bits.iter().zip(&value_bits) {
                timed.set(net, Logic::from_bool(bit));
            }
            values.push((*name, value_bits));
        }
        for ((name, value_bits), (_, width)) in values.iter().zip(inputs) {
            for (bit, &value) in value_bits.iter().enumerate() {
                cycle_sim.set_input(&bit_name(name, *width, bit), &[value]);
            }
        }
        cycle_sim.set_input("clk", &[false]);
        cycle_sim.step();
        cycle_sim.set_input("clk", &[true]);
        cycle_sim.step();

        // Rising edge, then let everything settle before the next one
        let rise = timed.now() + PERIOD_PS / 2;
        timed.run_until(rise).unwrap();
        timed.set(clk, Logic::One);
        timed.run_until(rise + PERIOD_PS / 2 - 1).unwrap();
        timed.set(clk, Logic::Zero);
        timed.run_until(rise + PERIOD_PS / 2).unwrap();

        for (name, bits) in outputs.iter().zip(&output_bits) {
            let got: Vec<Option<bool>> = bits.iter().map(|&n| timed.value(n).to_bool()).collect();
            let expected: Vec<Option<bool>> = (0..bits.len())
                .map(|bit| {
                    let bit_name = bit_name(name, bits.len(), bit);
                    let value = cycle_sim.get_output(&bit_name).expect("cycle output");
                    Some(value[0])
                })
                .collect();
            assert_eq!(got, expected, "{top}: output {name} after cycle {cycle}");
            seen.insert((name.to_string(), got));
        }
    }
    // The outputs moved, so the comparison saw the design work
    assert!(
        seen.len() > 2 * outputs.len(),
        "{top}: outputs barely changed"
    );
    assert_eq!(
        timed.violation_count(),
        0,
        "{top}: timing violations at a slow clock: {:?}",
        timed.violations()
    );
}

#[test]
fn timed_sim_matches_cycle_sim_on_a_counter() {
    let source = r#"
        entity Counter {
            in clk: clock
            in rst: reset
            in en: bit
            out count: bit[8]
        }
        impl Counter {
            signal c: bit[8] = 0
            on(clk.rise) {
                if rst {
                    c = 0
                } else if en {
                    c = c + 1
                }
            }
            count = c
        }
    "#;
    cross_check(source, "Counter", &[("rst", 1), ("en", 1)], &["count"], 60);
}

#[test]
fn timed_sim_matches_cycle_sim_on_an_accumulator() {
    let source = r#"
        entity Accumulator {
            in clk: clock
            in rst: reset
            in op: bit[2]
            in x: bit[8]
            out acc: bit[8]
            out zero: bit
        }
        impl Accumulator {
            signal a: bit[8] = 0
            on(clk.rise) {
                if rst {
                    a = 0
                } else if op == 0 {
                    a = a + x
                } else if op == 1 {
                    a = a ^ x
                } else if op == 2 {
                    a = a - x
                } else {
                    a = (a << 1) | (a >> 7)
                }
            }
            acc = a
            zero = if a == 0 { 1 } else { 0 }
        }
    "#;
    cross_check(
        source,
        "Accumulator",
        &[("rst", 1), ("op", 2), ("x", 8)],
        &["acc", "zero"],
        80,
    );
}

#[test]
fn timed_sim_matches_cycle_sim_on_a_state_machine_with_memory() {
    let source = r#"
        entity Fsm {
            in clk: clock
            in rst: reset
            in go: bit
            in d: bit[4]
            out q: bit[4]
            out state_out: bit[2]
        }
        impl Fsm {
            signal state: bit[2] = 0
            signal mem: [bit[4]; 4]
            signal ptr: bit[2] = 0
            signal out_r: bit[4] = 0
            on(clk.rise) {
                if rst {
                    state = 0
                    ptr = 0
                    out_r = 0
                } else if state == 0 {
                    if go {
                        state = 1
                    }
                } else if state == 1 {
                    mem[ptr] = d
                    ptr = ptr + 1
                    if ptr == 3 {
                        state = 2
                    }
                } else if state == 2 {
                    out_r = mem[ptr] ^ out_r
                    ptr = ptr + 1
                    if ptr == 3 {
                        state = 3
                    }
                } else {
                    state = 0
                }
            }
            q = out_r
            state_out = state
        }
    "#;
    cross_check(
        source,
        "Fsm",
        &[("rst", 1), ("go", 1), ("d", 4)],
        &["q", "state_out"],
        120,
    );
}

/// A counter through each FPGA family's place and route: the routed netlist and
/// its delays come back, and the timed simulation of them counts
#[test]
fn every_family_gives_routed_delays_for_timed_simulation() {
    let source = r#"
        entity Counter {
            in clk: clock
            in rst: reset
            in en: bit
            out count: bit[8]
        }
        impl Counter {
            signal c: bit[8] = 0
            on(clk.rise) {
                if rst {
                    c = 0
                } else if en {
                    c = c + 1
                }
            }
            count = c
        }
    "#;
    let hir = parse_and_build_hir(source).expect("parse");
    let mir = MirCompiler::new().compile(&hir).expect("MIR");
    let lir = lower_mir_module_to_lir(&mir.modules[0]);
    type Pnr = fn(&GateNetlist) -> skalp_place_route::PnrResult;
    // (Xilinx 7-series has place and route but no technology library yet)
    let families: [(&str, &str, Pnr); 2] = [
        ("ice40", "ice40", |n| {
            skalp_place_route::place_and_route(
                n,
                skalp_place_route::Ice40Variant::Hx8k,
                Default::default(),
            )
            .unwrap()
        }),
        ("ecp5", "ecp5", |n| {
            skalp_place_route::place_and_route_ecp5(
                n,
                skalp_place_route::Ecp5Variant::Lfe5u25f,
                Default::default(),
            )
            .unwrap()
        }),
    ];
    for (family, library, pnr) in families {
        let library = get_stdlib_library(library).expect("library");
        let netlist = synthesize(&lir.lir, &library, SynthPreset::Quick).netlist;
        let result = pnr(&netlist);
        let routed = result.routed.expect("routed netlist and delays");
        assert_eq!(routed.delays.len(), 1, "{family}");
        let delays = &routed.delays[0];
        assert_eq!(delays.cells.len(), routed.netlist.cells.len(), "{family}");
        assert!(
            delays
                .cells
                .iter()
                .any(|c| c.arcs.iter().flatten().any(|a| a.is_some_and(|a| a > 0))),
            "{family}: no cell delays"
        );
        assert!(
            delays
                .cells
                .iter()
                .any(|c| c.wire_ps.iter().any(|&w| w > 0)),
            "{family}: no routing delays"
        );

        let mut sim =
            TimedSim::with_power_up(&routed.netlist, delays, PowerUp::Zero).expect("timed sim");
        let net = |sim: &TimedSim, name: &str| {
            sim.net(name)
                .or_else(|| sim.net(&format!("top.{name}")))
                .unwrap_or_else(|| panic!("{family}: no net {name}"))
        };
        let (clk, rst, en) = (net(&sim, "clk"), net(&sim, "rst"), net(&sim, "en"));
        let mut count = sim.bus("count");
        if count.is_empty() {
            count = sim.bus("top.count");
        }
        sim.set(rst, Logic::Zero);
        sim.set(en, Logic::One);
        sim.add_clock(clk, 50_000);
        // 20 rising edges, the first at 25 ns
        sim.run_until(25_000 + 19 * 50_000 + 20_000).unwrap();
        assert_eq!(sim.bus_value(&count), Some(20), "{family}");
        assert_eq!(sim.violation_count(), 0, "{family}");
    }
}
