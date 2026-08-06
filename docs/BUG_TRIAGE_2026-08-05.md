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

4. **FIXED (2026-08-06). Async-reset synthesis (3 tests) — the audit's
   "zero registers" was wrong, but two REAL wrong-hardware bugs hid
   behind it:**
   - The DFFR hardware was actually being synthesized all along — the
     latch cells just had `function: None`, so every CellFunction-keyed
     count (and analysis) saw zero registers. Both aig_writer latch
     construction paths now set the function (same metadata bug class as
     the STA-fix buffers, item 3).
   - **SYNC reset emitted as async DFFR (real semantic bug):** a plain
     `on(clk.rise)` reset-if produced DFFR cells — hardware that resets
     ASYNCHRONOUSLY where the source demands synchronous. LirOp::Reg
     carried `async_reset: false` correctly; the AIG path ignored it.
     Fixed end-to-end: `sync_reset` is now a field ON `AigNode::Latch`
     (it must survive the optimizer passes, which rebuild the AIG — a
     side-table keyed by node id was tried first and silently emptied),
     every rebuild pass propagates it, and the writer folds sync resets
     into the D input (plain DFF + MUX2 tagged `source_op="ResetMux"`).
   - **Nonzero async reset values silently reset to 0 (real bug):**
     `reset(5)`-style registers used plain DFFR (resets to 0) for every
     bit. Bits that reset to 1 now store the INVERTED value: INV on D
     (`AsyncResetInvD`) and INV on Q (`AsyncResetInvQ`) around a DFFR,
     so the async reset drives the internal 0 that reads back as 1.
   All 13 test_async_reset tests pass; EC sweep (register-heavy designs)
   still SAT-proves; corpus unchanged.

5. **FIXED (2026-08-06, 2 of 3 + reclassification). Tuple-returning
   function results break MIR conversion.** Root causes found:
   (a) **Instance maps collide across impl blocks** — `entity_instance_
   outputs` is keyed by BARE VariableId, which restarts per impl block;
   stale entries from earlier impls (the stdlib fp impls) collided with
   later variables, so `.0` on a tuple-fn result hit a stdlib instance's
   ports (x_out/y_out), missed, and the Bug #85 panic fired. The maps are
   now cleared per impl block.
   (b) **`.N` on module-synthesized results had no port mapping** — the
   synthesized function module exposes tuple elements as `result_{i}`
   output ports; the entity-instance field lookup now maps numeric fields
   onto them (plus `result` for `.0` of single-return fns). A cached
   fallback also converts a call-holding variable's initializer once
   (module-synthesized calls only) and extracts elements from the Concat.
   (c) **test_bug71_metal was a broken TEST** — it used `vec3<fp32>`
   with `parse_and_build_hir` (no stdlib context), so the struct
   literals never resolved and the bindings vanished; rewritten with
   plain 96-bit lanes preserving its 288-bit-tuple intent. PASSES.
   `test_tuple_fp32_quadratic_no_real_roots` PASSES.
   **RECLASSIFIED, still open:** `test_tuple_fp32_quadratic_solver` now
   compiles and simulates but computes denormal garbage instead of the
   roots — a NUMERICAL bug in the synthesized-function simulation of the
   fp sqrt/div path (early-return mux logic or fp op wiring), no longer a
   tuple/conversion issue.

## P2 — inference and classification gaps

6. **ECP5 DSP inference never fires + ice40 gate-sim mux reads zero
   (5 tests).** `MULT18X18D` count is 0 where 1 is expected for 8x8/18x18/
   signed/wide multiplies (`test_ecp5_dsp_multiply_*`, 4 tests) — the DSP
   inference pass doesn't map `*` to the ECP5 DSP cell in the current synth
   path. Separately `test_ice40_gate_level_mux` simulates an ice40-mapped
   (SB_LUT4) mux netlist and reads 0x00 where 0x55 is expected — either
   LUT4 INIT generation or the gate-sim LUT evaluation is wrong for the
   ice40 library.

7. **FIXED (2026-08-06). Memory BRAM-inference threshold (2 tests).**
   The tests asserted "no MemBlock in LIR" — but since the 2026-08-02 #27
   memory work, the LIR MemBlock is the CANONICAL memory form (required so
   dynamic writes survive lowering); BRAM-vs-DFF is the TECH MAPPER's
   decision. Two changes: (a) the mapper now applies the documented
   256-bit floor — memories under 256 bits decompose to DFFs even when
   the target has block RAM (don't burn a 4096-bit EBR on 32 bits);
   (b) the tests re-target the NETLIST (no RAM/EBR cells; small memory
   register-decomposed) instead of the obsolete LIR-level assertion.

8. **Safety classification and FMEA DC (5 tests).**
   `test_safety_annotation_pipeline` (3): entities declared as safety
   mechanisms get `SafetyMechanismOfSm: 0` classified cells — the
   `#[implements]`/mechanism classification doesn't reach gate-netlist
   cells. `test_safety_tech_mapping_flow` (2): FMEA with measured
   diagnostic coverage does not reduce residual FIT (`Residual FIT should
   be less than raw FIT when DC is applied`) — DC application in the FMEA
   rollup is a no-op.

## P3 — single-test issues

9. **FIXED (2026-08-06). generate-if body dropped (1 test) — two stacked
   frontend gaps:**
   (a) `build_statements` (the on()-block statement walker) had NO arm for
   GenerateIfStmt/GenerateMatchStmt/GenerateForStmt — a generate construct
   inside an event block was silently SKIPPED (never even built). Routed
   through build_statement now.
   (b) `try_eval_const_bool` could not resolve a bare constant reference
   (`generate if ENABLE_PIPELINE` where `const ENABLE_PIPELINE: bool =
   true`) — the "must be evaluable" error was pushed non-fatally and the
   construct vanished. The builder now keeps a ConstantId → value table
   (populated in build_constant) and both const evaluators resolve
   references through it (numeric operators re-implemented on resolved
   operands). All 14 test_generate_blocks tests pass.

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
