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

1. **FIXED (2026-08-06). NCL gate-level simulation returns NULL outputs
    (36 tests → 0).** THREE stacked bugs, none of them the suspected
    f9345f2 rework logic itself:
    (a) **Completion AND-tree name collision (the oscillator).** The
    NclComplete mapping named tree-reduction nets `and_tree_{i}` PER LEVEL;
    the hierarchical flatten merges nets BY NAME, so level-2's and_tree_0
    collapsed onto level-1's — two AND2 cells drove one net with different
    inputs and the completion signal toggled forever: every large NCL sim
    reported `stable=false` with CORRECT data (the 8-bit adder computed 8
    while "oscillating"). Fix: level-indexed names (`and_tree_l{L}_{i}`).
    This alone flipped the 14 l0_l5 basic-op tests.
    (b) **Physical-net A/B split in the non-flattened path.** When a
    physical (NCL encode/complete) net is also a module output, aig_writer
    creates a SECOND net with the same name plus an `aig.phys_buf_*`
    between them. `merge_physical_nodes_into_netlist` resolved drivers via
    net_map (LAST-registered) — physical cells drove the copy while the
    AIG cone read the original, undriven net. Fix: resolve to the FIRST
    net with the name, so drivers land on the net the cone reads and the
    buf carries the value to the output copy.
    (c) **NCL runtime name grouping.** The GpuNclRuntime groups dual-rail
    nets by base name: duplicate full names (the A/B pairs) corrupted
    groups (an 8-bit rail group collected 24 nets → every read NULL), and
    flattened netlists carry `top.` prefixes while tests address bare
    names (`set_dual_rail_value("t", …)` silently set NOTHING). Fix:
    duplicate-name groups keep only the port-flagged net, and lookups
    tolerate hierarchy prefixes (exact → `top.{name}` → `.{name}` suffix).
    NOTE: an earlier attempt renamed the internal net (`__phys__` prefix)
    instead — it fixed the CLI path but broke the flatten path's name
    merging (9 fp/vec tests regressed); the runtime-side dedupe handles
    both, netlists untouched. **Verified:** the ENTIRE NCL family is green
    — 102 test_ncl_* + 22 l0_l5 + 4 fp32_gate_sim + wide + hierarchical —
    including the GPU/Metal tests (item 2) which had the same
    name-resolution failure.

2. **FIXED (2026-08-06) — same root cause as item 1(c).** The GPU tests
    failed on the identical name-grouping/prefix-resolution bugs, not on
    Metal execution; all 4 pass (including CPU-vs-GPU consistency).

3. **FIXED (2026-08-06). `async_sta_fix` timing buffers have no
    `CellFunction`.** Both real construction sites now set
    `Some(CellFunction::Buf)`; test_async_sta_fix 6/6.

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
