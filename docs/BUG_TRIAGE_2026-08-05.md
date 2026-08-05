# Compiler bug triage — 2026-08-05 audit (second pass)

Source: clustering + root-causing the 59 suite failures remaining after the
2026-08-02 campaign closed all 36 items of the first audit
(`BUG_TRIAGE_2026-08-02.md`; suite went 118 → 59 failures over that campaign).
Every failure below is PRE-EXISTING — the set has been byte-stable across the
entire campaign except where campaign fixes flipped tests to passing.

Method: `cargo test --release --test suite` failure list → cluster by module →
run one representative per cluster → root-cause to a subsystem and, where
cheap, to a mechanism. Signatures quoted are from 2026-08-05 runs at 5c04fa8.

Priorities: **P0** = a whole subsystem/flow is broken; **P1** = wrong results
in a specific feature; **P2** = missing inference/classification; **P3** =
single-test or harness-level issues.

## Failure map (59 tests, 9 root-cause clusters)

| # | Cluster | Tests | Subsystem |
|---|---------|-------|-----------|
| 1 | NCL simulation produces NULL/never converges | 36 | NCL synth + CPU sim |
| 2 | NCL GPU runtime | 4 | Metal NCL sim |
| 3 | ECP5 DSP inference + ice40 gate sim | 5 | FPGA tech mapping |
| 4 | Safety classification / FMEA DC | 5 | skalp-safety |
| 5 | Async-reset synthesis produces no registers | 3 | LIR reg mapping |
| 6 | Tuple-returning fn results (`.0` access) | 2+1 | HIR→MIR |
| 7 | Memory BRAM-inference threshold | 2 | tech mapping |
| 8 | generate-if body dropped | 1 | HIR elaboration |
| 9 | STA-fix buffers lack CellFunction | 1 | async_sta_fix |

Clusters 1, 2, and 9 (41 tests — 69% of all failures) are one subsystem:
the NCL/async flow, almost certainly broken since the f9345f2 NCL timing
rework (2026-03-18) which also left `decompose_latches` stubbed
(see 2026-08-02 audit #21).

---

## P0 — the NCL/async flow is broken end-to-end

1. **NCL gate-level simulation returns NULL outputs (36 tests).** Every
   `async entity` compiled through the NCL path fails CPU gate simulation in
   one of two ways:
   - **Small designs stabilize with NULL outputs:** dual-rail wavefront
     "converges" (`iterations: 2, stable: true`) but every output stays
     NULL/invalid — `test_ncl_cle_patterns` (13: completion detect, mux
     chains, comparator, ALU pipeline, FSM step, handshake ack, …),
     `test_ncl_std_cell_mode` (4), `test_l0_l5_ops` (14: add/sub/and/or/
     xor/shl/shr/eq/lt 8-bit, popcount, parity, bitreverse, fp32 ops),
     `test_hierarchical_ncl`, `test_wide_ncl_256bit_add`.
   - **Large designs never converge:** `test_fp32_gate_sim` (2) runs to the
     10000-iteration cap with `stable=false`.
   Both signatures say the completion/acknowledge topology produced by
   synthesis no longer propagates a wavefront the simulator can complete.
   **Prime suspect:** the f9345f2 rework ("Rework NCL timing closure to
   ready-signal-delay") — it changed the completion structure from per-fork
   data-path balancing to delayed ready signals, and the same commit stubbed
   `decompose_latches` "temporarily" (still stubbed today; its 4 unit tests
   are `#[ignore]`d). The whole NCL test population has failed since at
   least the start of the 2026-08-02 campaign. Triage this FIRST: fixing the
   wavefront-completion regression likely flips ~36 tests at once.
   Representative repro: `test_l0_l5_ops::test_l0_add_8bit` (8-bit adder,
   5/5 vectors return None).

2. **NCL GPU (Metal) runtime fails independently (4 tests).**
   `test_ncl_async_simulation::test_ncl_gpu_{inverter,add_8bit,and_8bit,
   vs_cpu_consistency}`. Cannot be meaningfully triaged until the CPU NCL
   path (item 1) is green — the vs-CPU consistency test needs a working
   reference. Re-run after item 1.

3. **`async_sta_fix` timing buffers have no `CellFunction` (1 test, real
   bug regardless of item 1).** `insert_timing_buffers` creates buffer
   cells with `function: None` (async_sta_fix.rs:646 and three sibling
   construction sites at 462/885/904); `gpu_ncl_runtime` panics:
   `cell 'top.a_dec[0].timing_fix_buf0' (type 'BUF_X1') has no CellFunction
   — all cells must have function set`. Mechanical fix: set
   `Some(CellFunction::Buf)` (and audit the other three sites).

## P1 — wrong results in specific features

4. **Async-reset processes synthesize ZERO sequential cells (3 tests).**
   `on(clk.rise, rst.active)` with the standard reset-if pattern produces a
   netlist with `0 DffR and 0 Dff` (`test_async_reset_synthesis_uses_dffr`,
   `..._nonzero_value_synthesis`, and the sync-reset variant expecting
   Dff+mux gets no ResetMux either). The register is dropped entirely in
   the `synthesize()` AIG path for async-reset sensitivity lists. Likely
   related to the stubbed `decompose_latches` and/or the AIG path not
   modeling async-reset DFFs. Note: EC never covers async-reset designs, so
   this is invisible to the equivalence flow — silent wrong hardware for
   `rst.active` users.

5. **Tuple-returning function results break MIR conversion (3 tests).**
   `test_tuple_fp32_quadratic_{solver,no_real_roots}` die in the Bug #85
   panic: `Assignment: variable_13 = FieldAccess(Variable(12).0)` fails to
   convert — element access on a variable holding a tuple-typed function
   result. `test_bug71_metal_288bit_tuple_generation` is the same family
   surfaced differently: `undefined identifier v1/v2/v3` (the tuple
   destructure of a 288-bit-tuple function result loses its bindings).
   The scalar/tuple-literal paths work (test_tuple_destructuring's other
   tests pass); the FUNCTION-RESULT path is what's broken.

## P2 — inference and classification gaps

6. **ECP5 DSP inference never fires + ice40 gate-sim mux reads zero
   (5 tests).** `MULT18X18D` count is 0 where 1 is expected for 8x8/18x18/
   signed/wide multiplies (`test_ecp5_dsp_multiply_*`, 4 tests) — the DSP
   inference pass doesn't map `*` to the ECP5 DSP cell in the current synth
   path. Separately `test_ice40_gate_level_mux` simulates an ice40-mapped
   (SB_LUT4) mux netlist and reads 0x00 where 0x55 is expected — either
   LUT4 INIT generation or the gate-sim LUT evaluation is wrong for the
   ice40 library.

7. **Memory BRAM-inference threshold inverted (2 tests).** An 8x4 (32-bit)
   memory with `auto` style INFERS BRAM though the test asserts it must
   stay in registers (`test_auto_inference_small_memory`,
   `test_auto_inference_no_bram_target`). Size/target gating in the
   inference heuristic doesn't match the documented policy.

8. **Safety classification and FMEA DC (5 tests).**
   `test_safety_annotation_pipeline` (3): entities declared as safety
   mechanisms get `SafetyMechanismOfSm: 0` classified cells — the
   `#[implements]`/mechanism classification doesn't reach gate-netlist
   cells. `test_safety_tech_mapping_flow` (2): FMEA with measured
   diagnostic coverage does not reduce residual FIT (`Residual FIT should
   be less than raw FIT when DC is applied`) — DC application in the FMEA
   rollup is a no-op.

## P3 — single-test issues

9. **generate-if body dropped (1 test).**
   `test_generate_blocks::test_generate_if_basic`: HIR builds, but the
   selected generate-if branch's assignments never reach MIR — caught by
   the (campaign-added) undriven-output check: `output port data_out of
   ConditionalPipeline is never driven`. Same family as the fixed #30
   (generate-FOR in generic impls); the generate-IF elaboration path needs
   the same treatment. Before the undriven check this would have been
   silent wrong hardware, so arguably P1 by nature — kept here because it
   is one known construct.

---

## Suggested attack order

1. Item 1 (NCL wavefront) — biggest lever, ~36 tests, likely one rework
   regression. Start by diffing an NCL netlist's completion tree pre/post
   f9345f2 for the 8-bit adder, or reading the ready-signal-delay insertion
   against what `ncl_sim`'s completion detection expects.
2. Item 3 (CellFunction on timing buffers) — mechanical, unblocks STA-fix.
3. Item 9 (generate-if) — small, mirrors the already-fixed #30.
4. Item 5 (tuple function results) — frontend, well-localized signatures.
5. Item 4 (async-reset registers) — decide together with un-stubbing
   `decompose_latches` (2026-08-02 #21 note).
6. Items 6/7/8 — backend/analysis features, independent of each other.
7. Item 2 (NCL GPU) — only after item 1.
