# Compiler bug triage — 2026-08-02 audit

Source: full-project design review (spec/frontend read-through + compile-testing all
published tutorial examples against a fresh `cargo build --release --bin skalp`).
Priorities: **P0** = tool reports success while producing wrong hardware or hiding
errors; **P1** = missing checks the language explicitly promises; **P2** = semantic /
codegen gaps; **P3** = hygiene.

## P0 — false success / silent wrong hardware

1. **FIXED (partially reclassified). Semantic errors exited 0 with wrong hardware.**
   Original report said parse errors exit 0 — that did NOT reproduce (parse errors
   correctly exit 1; the audit's `$?` capture was a shell artifact). The real bug was
   worse: *semantic* errors were silent successes:
   - `y = undefined_name + a` compiled to `assign y = (0 + a)` — unresolved
     identifiers lowered to literal 0 (`hir_to_mir.rs` GenericParam fallback).
   - `let u = NoSuchEntity { x: a }` vanished entirely — unknown entity in
     `build_instance` returned None and the instance was dropped.
   **Fix applied 2026-08-02:** `HirToMir` now collects `conversion_errors` (undefined
   identifier, unknown struct-literal type, missing struct field) and
   `compile_to_mir_with_modules` fails the build listing them; `hir_builder` records
   unresolved instances into `Hir.unresolved_instances` (tolerated in pass 1 for
   pending imports) and `parse_and_build_compilation_context` fails after the final
   rebuild pass if any survive. Verified: both cases exit 1 with named errors;
   14-file tutorial corpus + 10 repo examples regression-clean.

2. **Tuple-match statements are silently dropped.** `count = match (a==b, b==c) {
   (true,true) => a, ... }` lowers to nothing; `count` is undriven in the emitted SV,
   exit 0. Repro: tutorial ch09 TmrCounter (the TMR *voter* is the dropped statement).

3. **FIXED (2026-08-02). Dot-notation instance outputs are never wired.**
   `let tx = UartTx { clk: clk, .. }` then `busy = tx.busy` emitted instances with
   output ports unconnected, duplicate wire declarations, and self-assignments.
   **Fix:** the `inst` keyword is implemented per the settled design below —
   `inst add = Adder { a: x, b: y }` + `result = add.sum` generates fully wired SV.
   The root cause (auto-created instance-output wires never added to the
   ModuleInstance connection map in `convert_instance`) is fixed for BOTH `inst`
   and legacy `let` instantiation, so existing dot-access code now works too.
   Name collisions between user bindings and auto-wires (`let tx_fifo_empty =
   tx_fifo.empty`) are avoided by renaming the auto-wire (`tx_fifo__empty`);
   duplicate signal declarations and self-assigns are additionally filtered at
   codegen. `inst` enforces the contract: binding an output in the port map is a
   compile error with a dot-access fix-it, and unconnected inputs are compile
   errors. Verified: 16-file corpus + 10 repo examples + golden tests + FP/counter
   simulation suites all green. STILL TODO from the settled design: deprecation
   warning on legacy `let`-instantiation (deferred to avoid stdlib warning spam),
   `skalp fmt` rewrite of portmap-output style, LSP completion, examples/tutorial
   migration to `inst`.

4. **Index-slice on memory subscript silently dropped.** `mem[wr_ptr[clog2(DEPTH)-1:0]]`
   emits `mem[wr_ptr]` — 5-bit pointer into 16-deep memory, out of range after wrap.
   Repro: tutorial ch08 AsyncFIFO.

5. **`#[cdc(...)]` auto-synchronizer conflicts with user logic.** The generated Gray
   sync chain drives the same nets as hand-written sync flops (multi-driven
   `wr_ptr_gray_sync_rd`), and the auto-generated sync registers are never clocked.
   Repro: tutorial ch08 AsyncFIFO with `#[cdc]` + manual two-flop chain.

6. **`open` port binding lowers to `.z(0)`** — instance output tied to a constant
   (illegal SV, and wrong intent). `_` binding works correctly (connection omitted);
   `open` should do the same or be removed.

7. **`let x = 0` special-cased as "placeholder signal", assignment skipped.**
   `hir_to_mir.rs` (`is_placeholder_signal`) treats any let-binding of literal 0 as a
   placeholder and suppresses the assignment. A user writing `let zero = 0` gets an
   undriven node. Replace the literal-0 heuristic with an explicit placeholder marker.

## P1 — missing checks the language promises

8. **No undriven-output diagnostic.** An `out` port never assigned builds silently
   (this is what let #2 and #3 escalate from warning-material to disaster). Cheap MIR
   pass; would also have caught most P0s in the tutorial.

9. **Match exhaustiveness checking does not exist.** `is_match_exhaustive()` in
   `skalp-lint/src/lints/hardware.rs:103` is `#[allow(dead_code)]` and returns
   `false`; the lint site is a TODO. A match missing an enum arm builds clean and the
   missing value falls into the last arm. Published docs (design-choices blog, tutorial
   ch07, projects page) all claim this is a compile error.

10. **CDC diagnostics stripped.** `report_cdc_violations` in
    `skalp-mir/src/compiler.rs:149` computes severity strings and counts, then prints
    nothing (`// Violation details removed`, `// Summary removed`). Critical violations
    fail the build with only a count — no signal names, no locations. Restore the
    rendering; this is the headline safety feature.

11. **No missing-port-connection check.** Unconnected instance inputs are silent
    (`unconnected`/`nc`/`open` attribute exists but absence of a connection is not an
    error). Docs claim "forgetting a port is a compile error."

12. **`stream<T>` generates no handshaking.** `hir_to_mir.rs:18270` lowers stream
    ports to the bare inner type (`TODO: Add proper stream protocol support`). Either
    implement valid/ready lowering or error on `stream` ports until it exists — docs
    claim the compiler "enforces backpressure."

## P2 — semantic / codegen gaps

13. **Clock-domain lifetimes lowered as power domains.** `entity Sync<'src, 'dst>`
    emits `(* power_domain = "src" *)` attributes on signals — wrong semantic category
    (and read-domain signals get tagged with the write domain). Clock lifetimes should
    feed the CDC domain tracker, not the UPF path.

14. **`bit[8]<'domain>` (width + lifetime) does not parse**, though the spec, the
    tutorial (ch08), and the projects page all use it. Decide the surface syntax for
    domain-annotated vectors and implement or excise it everywhere.

15. **Declared type of `let` bindings ignored.** `let full_sum: bit[WIDTH+1] = ...`
    infers its own width (observed 10-bit where 9 was declared). Type annotation should
    constrain or error.

16. **`signal` declared inside `on()` gets wrong width inference** (observed 32-bit
    for a 1-bit condition value). Cosmetic in the observed case but the inference is
    unsound.

17. **`++` concatenation does not parse** though tutorial ch07 uses it
    (`command_data[7:0] ++ 8'h00`). Implement or remove from docs; `{a, b}` SV-style
    concat appears in examples too — pick one.

18. **Struct-flattening separator is `__` (double underscore); docs say `_`.**
    `color_a.r` → `color_a__r`. The double underscore is arguably better (unambiguous);
    fix the docs and the tutorial's testbench signal names, and expose the mangling
    rule in one place.

19. **Intent query builtins are hardcoded constants.**
    `hir_to_mir.rs:8192`: `is_latency_optimized` → always true, `is_area_optimized` /
    `is_throughput_optimized` → always false; stdlib `FpSqrt`'s three intent profiles
    generate identical hardware. Wire these to the actual intent parameter (the
    monomorphization engine can already fold intent conditionals) or remove them.

20. **Intent generic params detected by naming convention.**
    `monomorphization/engine.rs:1607`: a generic is "intent" iff named `I` or
    `*_INTENT`. Should key on the declared `intent` parameter kind from the parser.

## P3 — hygiene / build health

21. **`cargo check --workspace --all-targets` fails at HEAD.** Unit-test targets in
    skalp-mir, skalp-frontend, skalp-lir, skalp-formal broken by un-propagated struct
    fields (`is_from_main_source`, `impl_style`); root `test_const_eval` too. Gate CI
    on `--all-targets`.

22. **~150 globally reserved keywords.** The lexer reserves the entire safety and
    physical-constraint vocabularies (`area`, `bank`, `region`, `device`, `group`,
    `fast`, `slow`, `up`, `down`, `part`, `pin`, `drive`, ...) — everyday RTL signal
    names. Convert domain keywords to contextual keywords. (Spec claims 43 keywords.)

23. **Shipped stdlib contains a dead-dialect, buggy FIFO.**
    `crates/skalp-stdlib/components/fifo.sk`: `almost_empty = (count = 1)`
    (assignment in expression), `assert property (count = DEPTH)`, mixed `=`/`<=`,
    angle-bracket widths. Migrate or delete.

24. **Debug residue in hot paths.** Emoji trace lines (`🟠🟠🟠`), a hardcoded trace for
    `_tuple_tmp_66`, bug-number archaeology comments throughout
    `hir_to_mir.rs` / `hir_builder.rs`.

25. **CLI/manifest mismatches with docs.** `skalp build <file>` positional arg is
    rejected (needs `-s`) though README/tutorial use it; `skalp new` emits `Cargo.toml`
    (not `skalp.toml`); manifest has no `[build] top` field; `skalp fmeda` /
    `skalp fault-inject` don't exist (real: `skalp safety`, `skalp build --safety`).
    Align CLI with docs or docs with CLI.

26. **Package manager: git dependencies stubbed** (`resolver.rs:172` returns
    "Git dependencies not yet implemented"). Documented as a limitation — keep it
    honest in the manifest docs until done.

## Settled design: `inst` keyword for instantiation (decided 2026-08-02)

Instantiation gets its own statement keyword; `let` stops being overloaded.

```
inst tx = UartTx<8> {
    clk: clk,
    rst: rst,
    data: tx_data,      // inputs ONLY in the port map
}

busy = tx.busy          // outputs read via dot-access, anywhere a signal is legal
serial_out = tx.tx
```

Rules:
- `inst name = Entity<generics> { input_port: expr, ... }` — the port map binds
  **inputs only**. Binding an output port in the map is a compile error (message
  should suggest dot-access).
- Instance **outputs** are read as `name.port` — combinational nodes usable in any
  expression, continuous assignment, or `on()` block. This makes the currently
  miscompiled tutorial ch03/ch04/ch06 style the *correct* style.
- Unread outputs are simply unused — no `_` or `open` binding needed (issue 6's
  `open` → `.z(0)` path gets deleted, not fixed).
- Unconnected **inputs** are a compile error (issue 11) unless explicitly marked
  (keep one marker; suggest `port: open` in the map or an `#[unconnected]` attr —
  pick one, delete the others).
- `let` reverts to exactly one meaning: immutable combinational binding (impl level,
  `on()` bodies, `fn` bodies). `let x = Entity { ... }` where the type resolves to an
  entity is a hard error with a fix-it to `inst`. This also removes the RHS-sniffing
  in `hir_to_mir.rs` (StructLiteral → entity detection) and the `let x = 0`
  placeholder hazard (issue 7) loses its reason to exist.
- Migration: one release accepting the old outputs-in-portmap style under a
  deprecation warning (it's the only style that currently works — stdlib and
  examples use it everywhere), then remove. `skalp fmt` should auto-rewrite
  portmap-output bindings to `inst` + dot-access.

Touch points: lexer (`inst` keyword), parser (statement form), HIR (`InstanceDecl`
already exists — route `inst` there, sever the LetStmt path), `hir_to_mir.rs`
instance lowering (wire dot-access reads to instance output nets — this is the actual
bug-3 fix), formatter, LSP completion, then examples/stdlib/tutorial migration.

## TRIAGED: MIR vs gate-level NON-equivalence (SpiMaster et al.)

Full triage completed 2026-08-02. The single "SpiMaster NOT EQUIVALENT" report
decomposed into FOUR distinct defects; three are FIXED, one remains open with a
minimal reproducer.

**FIXED 1 — netlist outputs dual-flagged as inputs (EC unusable).**
`tech_mapper.rs` Step 5.5 marked every preserved port net `is_input = true`,
outputs included; the gate→SIR converter classifies `is_input` first, so all
outputs surfaced as inputs and `skalp ec` died with "No matching outputs
between MIR and Gate" on every design (regression introduced after Mar 17,
when netlists still had disjoint input/output sets). Fix: preserve output-port
nets as outputs.

**FIXED 2 — enum-variant signal initializers lowered as continuous assigns.**
`signal state: State = State::Idle` failed the `is_literal` check in
`hir_to_mir` (EnumVariant is not `HirExpression::Literal`), generating a bogus
continuous assignment `state = 0` that multi-drove the state register alongside
its process driver. MIR simulation survived via last-write-wins; gate synthesis
const-folded FSM outputs to TIE cells (`ready ≡ 0`, `cs ≡ 1`). Fix:
`convert_literal_expr` now resolves enum variants to their encoded constants
(stored as the FF initial value), and both continuous-assign fallbacks gate on
const-evaluability instead of literal-ness.

**FIXED 3 — AIG writer dropped output-inversions on on-demand emission.**
The deep one, preset-dependent. When `get_or_create_lit_net` resolved a literal
whose producer had not been written yet (emission order is mapping-insertion
order = reverse-topological from backward covering), it emitted the producer on
demand and then blindly took `node_to_net` as the non-inverted net — but the
on-demand cell may be OUTPUT-INVERTED (e.g. NAND3 implementing an AND node).
Every early consumer got the complemented net with no compensating INV.
Concretely: `if (count == 7)` FSM exits inverted — the counter wrapped at the
wrong time and `SynthPreset::Quick` (used by `skalp ec`) tripped it while
`Balanced` happened not to. Reproducer: `tests/fixtures/ec_fsm_wrap.sk` — now
passes with a full SAT proof. Fix: re-resolve through the polarity-aware paths
after on-demand emission.

**FIXED 4 — Concat packed operands in reverse (LSB-first) order.**
LIR `Concat` follows Verilog `{a, b, ...}` semantics — first operand is the
MSB side (documented in ncl_expand.rs; the SIR simulation codegen packs in
reverse order accordingly). Both `lir_to_aig.rs` and the formal AIG builder in
`skalp-formal/equivalence.rs` packed the FIRST operand at the LSB, silently
"byte-swapping" every synthesized concat: `sr = {sr[6:0], din}` became
`sr | (din << 7)`. The formal copy meant the SAT reference model was wrong in
exactly the same way as the netlist, masking the bug from symbolic checks.
Fix: pack from the last operand upward in both. `ec_shift_reg.sk` now passes
with a full SAT proof; SpiMaster passes the 100-cycle smoke test (its SAT
phase still trips OPEN-5's unreachable-state strictness); the whole
pre-existing `test_bitreverse_mwe` suite (17 tests) went green.

**OPEN 5 — EC SAT phase explores unreachable states.**
For enum-typed FSMs the symbolic check now fails AFTER the 100-cycle smoke test
passes: SAT compares transition functions over ALL register states, including
encodings unreachable from reset (unused enum patterns), where MIR and gates
legitimately differ. The SAT phase needs reachability constraints or an
init-state-anchored k-induction.

Also noted during triage: `decompose_latches` in `synth/dff_decompose.rs` is
"TEMPORARILY DISABLED" (returns empty) — dead code path at HEAD; and `skalp ec`
synthesizes with `SynthPreset::Quick` while `skalp build` uses the default
preset, so EC never verifies the netlist users actually ship — it should take
the build preset (Quick's weaker pipeline is also what exposed FIXED-3).

## Pre-existing test-suite failures (measured 2026-08-02, NOT regressions)

Running the FULL integration suite (first time it has been compilable at HEAD —
see issue 21) shows **60 pre-existing test failures**, verified identical on a
clean-HEAD worktree with the original per-file test binaries: test_function_inlining
15/16, test_intent_and_numeric 5/24, test_ice40_synthesis 5, test_bitreverse_mwe 5/6 (ALL 17 fixed by the concat
fix, f1609a8), test_counter_example 4/4, test_l0_l5_ops 21/21 (the earlier
"4" undercounted — the SIGABRT-truncated run never reached the rest; verified
21/21 at the pre-fix baseline too), test_graphics_pipeline_functional 4,
test_fpmul_entity 3/3, test_async_reset 3/11 (the *_synthesis_uses_dff* trio),
test_fp32_gate_sim 4/4 (same undercount correction), test_ergonomic_testbench 2,
test_cdc_verification 2/6,
plus singles (fpmul_debug/nogeneric, equivalence_mwe, bug71_metal, async_sta_fix,
gpu_simulation). Additionally `test_ice40_aig_synthesis_timing` hits a **stack
overflow in debug builds** (runaway recursion in AIG timing analysis; also at
clean HEAD; not cured by RUST_MIN_STACK=64MB) — in the consolidated suite binary
this aborts the whole run, so it should be fixed or `#[ignore]`d with priority.
`scripts/ci_check.sh`'s test stage cannot pass until these are triaged.

## Suggested order of attack

Fix 1 (exit codes) and 8 (undriven-output check) first — together they convert most
other silent failures into visible ones. Then 2/3/4/5 (miscompilations the tutorial
itself triggers), then 10 (CDC diagnostics). Everything below that is scheduling.
