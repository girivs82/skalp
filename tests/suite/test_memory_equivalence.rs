//! Regression tests for triage #27: `skalp ec` on designs with memory arrays.
//!
//! Two gaps used to make ANY memory design fail equivalence checking:
//! 1. The generic ASIC library has no RAM cell, and the tech mapper dropped
//!    the MemBlock entirely (rdata stayed undriven/0 forever). Now it
//!    decomposes into DFFs + write muxes + a read mux chain.
//! 2. The AIG conversions could not model the memory: lir_to_aig swept the
//!    logic cones driving the MemBlock's ports (dead from the AIG's view),
//!    and MirToAig dropped dynamic-index element reads/writes (mem[ptr]).

use skalp_formal::{check_sequential_equivalence_sat, GateNetlistToAig, MirToAig};
use skalp_frontend::hir_builder::build_hir;
use skalp_frontend::monomorphization::MonomorphizationEngine;
use skalp_frontend::parse::parse;
use skalp_lir::{get_stdlib_library, lower_mir_module_to_lir, synthesize};

/// 16-deep memory with a sliced write/read pointer — the triage #4 + #27
/// combined repro (tutorial ch08 AsyncFIFO's pattern).
const MEM_SLICE_SRC: &str = r#"
entity MemSlice {
    in clk: clock
    in rst: reset
    in wr_en: bit
    in wr_data: bit[8]
    out rd_data: bit[8]
}

impl MemSlice {
    signal mem: [bit[8]; 16]
    signal ptr: bit[5] = 0

    on(clk.rise) {
        if (rst) {
            ptr = 0
        } else {
            if (wr_en) {
                mem[ptr[3:0]] = wr_data
                ptr = ptr + 1
            }
        }
    }

    rd_data = mem[ptr[3:0]]
}
"#;

fn compile_to_mir(source: &str) -> skalp_mir::Mir {
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building failed");
    let mut engine = MonomorphizationEngine::new();
    let hir = engine.monomorphize(&hir);
    skalp_mir::lower_to_mir(&hir).expect("MIR lowering failed")
}

/// The memory's port nets must be DRIVEN in the synthesized netlist.
/// lir_to_aig used to sweep the cones feeding a physical op (nothing else
/// consumed them), leaving mem_we/mem_waddr/mem_wdata/mem_raddr undriven —
/// so the memory never saw a write.
#[test]
fn test_triage27_memory_port_nets_driven() {
    let mir = compile_to_mir(MEM_SLICE_SRC);
    let lir_result = lower_mir_module_to_lir(&mir.modules[0]);
    let library = get_stdlib_library("generic_asic").expect("library");
    let synth = synthesize(
        &lir_result.lir,
        &library,
        skalp_lir::synth::SynthPreset::Quick,
    );
    let netlist = &synth.netlist;

    let mut driven: std::collections::HashSet<u32> = std::collections::HashSet::new();
    for cell in &netlist.cells {
        for out in &cell.outputs {
            driven.insert(out.0);
        }
    }

    let mut checked = 0;
    for net in &netlist.nets {
        let is_mem_port = net.name.starts_with("mem_we")
            || net.name.starts_with("mem_waddr")
            || net.name.starts_with("mem_wdata")
            || net.name.starts_with("mem_raddr");
        if is_mem_port {
            checked += 1;
            assert!(
                net.is_input || driven.contains(&netlist.resolve_alias(net.id).0),
                "Triage #27: memory port net `{}` has no driver — the logic cone \
                 feeding the memory was swept during synthesis",
                net.name
            );
        }
    }
    assert!(checked >= 17, "expected mem port nets, found {}", checked);

    // The generic library has no RAM cell — the memory must decompose to DFFs
    let fallback_dffs = netlist
        .cells
        .iter()
        .filter(|c| c.source_op.as_deref() == Some("MemBlock_DffFallback") && c.clock.is_some())
        .count();
    assert_eq!(
        fallback_dffs,
        16 * 8,
        "expected 128 storage DFFs from the MemBlock DFF fallback"
    );
}

/// Full SAT-level equivalence: the MIR-side AIG (with dynamic-index element
/// read/write modeling) must be provably equivalent to the synthesized gate
/// netlist (with the DFF-decomposed memory).
#[test]
fn test_triage27_memory_design_sat_equivalent() {
    let mir = compile_to_mir(MEM_SLICE_SRC);
    let target = &mir.modules[0];
    let lir_result = lower_mir_module_to_lir(target);
    let library = get_stdlib_library("generic_asic").expect("library");
    let synth = synthesize(
        &lir_result.lir,
        &library,
        skalp_lir::synth::SynthPreset::Quick,
    );

    let mir_aig = MirToAig::new_with_mir(&mir, target).convert_sequential_hierarchical();
    let gate_aig = GateNetlistToAig::new().convert_sequential(&synth.netlist);

    assert!(
        !mir_aig.latches.is_empty(),
        "MIR AIG must model the memory as latches"
    );

    let result = check_sequential_equivalence_sat(&mir_aig, &gate_aig, false)
        .expect("SAT equivalence check errored");
    assert!(
        result.equivalent,
        "Triage #27: memory design must be SAT-equivalent MIR vs gates; \
         counterexample: {:?}",
        result.counterexample
    );
}

// =============================================================================
// TRIAGE 2026-08-02 #30: generate-for in GENERIC impls was dropped at parse
// time (elaborate_generate_for_in_impl bailed on symbolic bounds like
// 0..WIDTH), so the loop body never existed in any specialization. The loop
// is now preserved symbolically and elaborated during specialization with
// SSA-chained accumulator semantics: iteration i reads the previous
// iteration's value (acc__ginit for i=0) and only the last iteration writes
// the original signal — a naive unroll would multi-drive `acc` with a
// combinational self-loop.
// =============================================================================

const GENERIC_ACC_SRC: &str = r#"
entity GAcc<const W: nat> {
    in a: bit[4]
    in b: bit[4]
    out product: bit[8]
}

impl GAcc {
    signal acc: bit[8] = 0

    generate for i in 0..W {
        signal partial: bit[8]
        partial = {4'b0, a} << i

        signal addend: bit[8]
        addend = if b[i] { partial } else { 0 }

        acc = acc + addend
    }

    product = acc
}

entity GTop {
    in a: bit[4]
    in b: bit[4]
    out p: bit[8]
}

impl GTop {
    inst m = GAcc<4> { a: a, b: b }
    p = m.product
}
"#;

/// The specialized impl must contain the fully elaborated, SSA-chained loop.
#[test]
fn test_triage30_generic_generate_for_elaborated() {
    let tree = parse(GENERIC_ACC_SRC);
    let hir = build_hir(&tree).expect("HIR building failed");
    let mut engine = MonomorphizationEngine::new();
    let hir = engine.monomorphize(&hir);
    let mir = skalp_mir::lower_to_mir(&hir).expect("MIR lowering failed");
    let sv = skalp_codegen::generate_systemverilog_from_mir(&mir).expect("codegen");

    // Per-iteration signals exist for every iteration
    for i in 0..4 {
        assert!(
            sv.contains(&format!("partial__g{}", i)),
            "Triage #30: missing per-iteration signal partial__g{} in:\n{}",
            i,
            sv
        );
    }
    // The accumulator is an SSA chain seeded by the initial value…
    assert!(
        sv.contains("acc__ginit"),
        "Triage #30: missing accumulator chain seed acc__ginit:\n{}",
        sv
    );
    assert!(
        sv.contains("(acc__ginit + addend__g0)"),
        "Triage #30: iteration 0 must read the chain seed:\n{}",
        sv
    );
    // …and the ORIGINAL signal is written exactly once (by the last iteration)
    let acc_drivers = sv.matches("assign acc = ").count();
    assert_eq!(
        acc_drivers, 1,
        "Triage #30: `acc` must have exactly one driver (last chain link), got {}:\n{}",
        acc_drivers, sv
    );
}

/// SAT-level equivalence of the elaborated design, MIR vs synthesized gates.
#[test]
fn test_triage30_generic_generate_for_sat_equivalent() {
    let mir = compile_to_mir(GENERIC_ACC_SRC);
    let target = mir
        .modules
        .iter()
        .find(|m| m.name == "GTop")
        .expect("top module");
    let hier = skalp_lir::lower_mir_hierarchical_with_top(&mir, Some("GTop"));
    let flat = hier.flatten();
    let library = get_stdlib_library("generic_asic").expect("library");
    let synth = synthesize(&flat, &library, skalp_lir::synth::SynthPreset::Quick);

    let mir_aig = MirToAig::new_with_mir(&mir, target).convert_sequential_hierarchical();
    let gate_aig = GateNetlistToAig::new().convert_sequential(&synth.netlist);
    let result = check_sequential_equivalence_sat(&mir_aig, &gate_aig, false)
        .expect("SAT equivalence check errored");
    assert!(
        result.equivalent,
        "Triage #30: elaborated generate-for design must be SAT-equivalent; \
         counterexample: {:?}",
        result.counterexample
    );
}
