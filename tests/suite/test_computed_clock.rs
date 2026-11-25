//! Registers clocked by logic of their own outputs (a ring oscillator made
//! of flip-flops): each must keep the clock its process names.

use skalp_frontend::hir_builder::build_hir;
use skalp_frontend::monomorphization::MonomorphizationEngine;
use skalp_frontend::parse::parse;
use skalp_lir::{get_stdlib_library, lower_mir_module_to_lir, synthesize};

const CLOCK_RING_SRC: &str = r#"
entity Cr {
    in clk: clock
    out q: bit
    out p: bit
    out n: bit[8]
}

impl Cr {
    signal go: bit = 0
    on(clk.rise) {
        go = 1
    }
    signal ca: bit = 0
    signal cb: bit = 0
    signal cc: bit
    signal ccn: bit
    cc = ~(ca ^ cb) & go
    ccn = (ca ^ cb) & go
    signal cn: bit[8] = 0
    on(cc.rise) {
        ca = ~ca
        cn = cn + 8'h01
    }
    on(ccn.rise) {
        cb = ~cb
    }
    q = ca
    p = cb
    n = cn
}
"#;

fn compile_to_mir(source: &str) -> skalp_mir::Mir {
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building failed");
    let mut engine = MonomorphizationEngine::new();
    let hir = engine.monomorphize(&hir);
    skalp_mir::lower_to_mir(&hir).expect("MIR lowering failed")
}

/// Each flip-flop of a ring clocked by logic of its own outputs keeps its
/// own clock: the writer used to write a flip-flop before the logic of its
/// clock when that logic read the flip-flop, and the flip-flop then fell
/// back to the first clock (here `clk`), silently.
#[test]
fn registers_clocked_by_their_own_logic_keep_their_clocks() {
    let mir = compile_to_mir(CLOCK_RING_SRC);
    let lir = lower_mir_module_to_lir(&mir.modules[0]).lir;
    let library = get_stdlib_library("generic_asic").expect("library");
    for preset in [
        skalp_lir::synth::SynthPreset::Quick,
        skalp_lir::synth::SynthPreset::Balanced,
    ] {
        let nl = synthesize(&lir, &library, preset).netlist;
        let clock_of = |out: &str| {
            let q = nl
                .nets
                .iter()
                .position(|n| n.name == out)
                .unwrap_or_else(|| panic!("no net {out}"));
            let ff = nl
                .cells
                .iter()
                .find(|c| c.clock.is_some() && c.outputs.iter().any(|o| o.0 as usize == q))
                .unwrap_or_else(|| panic!("no flip-flop drives {out}"));
            ff.clock.unwrap()
        };
        let clk = nl.nets.iter().position(|n| n.name == "clk").unwrap();
        let (a, b, n0) = (clock_of("q"), clock_of("p"), clock_of("n[0]"));
        assert_ne!(a.0 as usize, clk, "{preset:?}: ca is clocked by clk");
        assert_ne!(b.0 as usize, clk, "{preset:?}: cb is clocked by clk");
        assert_ne!(a, b, "{preset:?}: ca and cb share a clock");
        assert_eq!(a, n0, "{preset:?}: ca and cn take different clocks");
    }
}
