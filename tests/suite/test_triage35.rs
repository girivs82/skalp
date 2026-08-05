//! TRIAGE 2026-08-02 #35: memory writes under multi-branch control were
//! broken THREE ways, each hidden by the others:
//!
//! (a) MIR→LIR miscompile: the memory write-enable used only the OUTERMOST
//!     if-condition (plus one special-cased reset-else level), so
//!     `if !rst { if a { mem[p] = d } }` synthesized we = !rst — writing
//!     every non-reset cycle. Real hardware bug in the LIR/synthesis path
//!     (the emitted SV was fine); EC could not see it because the MIR-side
//!     AIG shared the truncation. Fixed by a guard-aware collector: every
//!     write site's FULL condition path is conjoined; multiple sites OR
//!     their guards into we with last-write-wins muxes on waddr/wdata.
//!
//! (b) Behavioral EC model: the ResolvedConditional array-write path wrote
//!     the priority-mux value UNCONDITIONALLY every cycle. Now routed
//!     through the if-statement machinery (guards + old-value keep path).
//!
//! (c) When the memory is a design's only sequential element, the clock
//!     input net didn't exist in the AIG-derived netlist; the DFF fallback
//!     silently clocked every memory row with TIE_LOW, and the netlist→SIR
//!     conversion fell back to SirSignalId(0) for the sequential block's
//!     clock. The clock net is now get-or-created AND registered in
//!     netlist.clocks.
//!
//! KNOWN REMAINING: memories inside hierarchically-inlined CHILD instances
//! are flattened to per-element scalars with no dynamic-index write path in
//! the behavioral model (writes lost, reads 0) — see the triage doc.

use skalp_formal::{check_sequential_equivalence_sat, GateNetlistToAig, MirToAig};
use skalp_frontend::hir_builder::build_hir;
use skalp_frontend::monomorphization::MonomorphizationEngine;
use skalp_frontend::parse::parse;
use skalp_lir::{get_stdlib_library, lower_mir_hierarchical_with_top};

fn ec_sat(source: &str, top: &str) {
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building failed");
    let mut engine = MonomorphizationEngine::new();
    let hir = engine.monomorphize(&hir);
    let mir = skalp_mir::MirCompiler::new()
        .compile_to_mir(&hir)
        .expect("MIR compile failed");
    let target = mir.modules.iter().find(|m| m.name == top).expect("top");
    let hier_lir = lower_mir_hierarchical_with_top(&mir, Some(top));
    let flat_lir = hier_lir.flatten();
    let library = get_stdlib_library("generic_asic").expect("library");
    let synth = skalp_lir::synthesize(&flat_lir, &library, skalp_lir::synth::SynthPreset::Quick);

    let mir_aig = MirToAig::new_with_mir(&mir, target).convert_sequential_hierarchical();
    let gate_aig = GateNetlistToAig::new().convert_sequential(&synth.netlist);
    let result = check_sequential_equivalence_sat(&mir_aig, &gate_aig, false)
        .expect("SAT equivalence errored");
    assert!(
        result.equivalent,
        "Triage #35: {} must be SAT-equivalent MIR vs gates; counterexample: {:?}",
        top, result.counterexample
    );
}

/// (a): nested guard must reach the write enable — with the truncation the
/// gates wrote every non-reset cycle and this SAT check fails.
#[test]
fn test_triage35_nested_guard_memory_write() {
    ec_sat(
        r#"
entity Mem1Br {
    in clk: clock
    in rst: reset(active_high)
    in a: bit
    in d: bit[8]
    in p: bit[4]
    out q: bit[8]
}

impl Mem1Br {
    signal mem: [bit[8]; 16]

    on(clk.rise) {
        if !rst {
            if a {
                mem[p] = d
            }
        }
    }

    q = mem[p]
}
"#,
        "Mem1Br",
    );
}

/// (a)+(b): two sibling write sites (else-if chain) with different data.
#[test]
fn test_triage35_two_site_memory_write() {
    ec_sat(
        r#"
entity MemDiff {
    in clk: clock
    in rst: reset(active_high)
    in a: bit
    in b: bit
    in d: bit[8]
    in p: bit[4]
    out q: bit[8]
}

impl MemDiff {
    signal mem: [bit[8]; 16]

    on(clk.rise) {
        if !rst {
            if a {
                mem[p] = d
            } else if b {
                mem[p] = d + 1
            }
        }
    }

    q = mem[p]
}
"#,
        "MemDiff",
    );
}

/// (c): the memory as the ONLY sequential element — the clock net must be
/// created and registered so the memory rows actually clock.
#[test]
fn test_triage35_memory_only_clock_registered() {
    let source = r#"
entity MemOnly {
    in clk: clock
    in rst: reset(active_high)
    in a: bit
    in d: bit[8]
    in p: bit[4]
    out q: bit[8]
}

impl MemOnly {
    signal mem: [bit[8]; 16]

    on(clk.rise) {
        if !rst {
            if a {
                mem[p] = d
            }
        }
    }

    q = mem[p]
}
"#;
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building failed");
    let mut engine = MonomorphizationEngine::new();
    let hir = engine.monomorphize(&hir);
    let mir = skalp_mir::MirCompiler::new()
        .compile_to_mir(&hir)
        .expect("MIR compile failed");
    let hier_lir = lower_mir_hierarchical_with_top(&mir, Some("MemOnly"));
    let flat_lir = hier_lir.flatten();
    let library = get_stdlib_library("generic_asic").expect("library");
    let synth = skalp_lir::synthesize(&flat_lir, &library, skalp_lir::synth::SynthPreset::Quick);
    let netlist = synth.netlist;
    assert!(
        !netlist.clocks.is_empty(),
        "Triage #35(c): the clock net must be registered in netlist.clocks"
    );
    let clk_id = netlist.clocks[0];
    let dffs_on_clk = netlist
        .cells
        .iter()
        .filter(|c| c.is_sequential() && c.clock == Some(clk_id))
        .count();
    assert!(
        dffs_on_clk >= 128,
        "Triage #35(c): all memory row DFFs must be clocked by the real clock, got {}",
        dffs_on_clk
    );
}
