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

2. **FIXED (2026-08-03). Tuple-match statements were silently dropped.** Three
   stacked frontend/MIR gaps: (a) `build_match_expr`'s scrutinee filter lacked
   `TupleExpr`, so `match (a==b, b==c)` built to None and the entire enclosing
   assignment vanished (`build_match_statement`'s filter was even narrower —
   both broadened); (b) boolean literal patterns `(true, false)` built to
   `Tuple([])` because the LiteralPattern token filter lacked TrueKw/FalseKw;
   (c) MIR had no lowering for `HirPattern::Tuple` — expression matches now
   lower via `build_tuple_pattern_condition` (conjunction of per-element
   comparisons, wildcards unconstrained), and match STATEMENTS with tuple
   patterns lower to an if-else chain (`convert_tuple_match_statement`) since
   CaseStatement can't express them — the old path collapsed every tuple arm
   into the case default. The ch09 TmrCounter now builds with a real 4-arm
   voter, sequential `match (wr_en, rd_en)` produces a proper if-chain, and
   BOTH forms pass `skalp ec` with full SAT proofs. 41-design corpus green.

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

4. **FIXED (2026-08-03). Index-slice on memory subscript silently dropped.**
   `mem[wr_ptr[clog2(DEPTH)-1:0]]` emitted `mem[wr_ptr]` — 5-bit pointer into
   16-deep memory, out of range after wrap. Repro: tutorial ch08 AsyncFIFO.
   **Root cause:** the parser splits a nested index into SIBLING nodes — the
   outer index's children are `[IdentExpr(ptr), IndexExpr(3:0)]`, not a nested
   tree — and six separate index-building paths in `hir_builder.rs` each picked
   only the first/BinaryExpr child, silently discarding the slice. **Fix:** a
   shared `build_index_operand` helper reassembles the sibling-split shape
   (base then nested IndexExpr → build the inner index on the built base); all
   six call sites route through it: `build_index_expr`,
   `build_index_expr_with_base`, the event-block LHS lvalue builder,
   `build_lvalue`, `build_index_with_base`, and — the path serving impl-level
   continuous-assign RHS like `rd_data = mem[ptr[3:0]]` —
   `build_index_access_from_parts` (whose `indices` filter dropped IndexExpr
   children outright). Verified: repro emits `mem[ptr[3:0]]` on BOTH read and
   write sides; AsyncFIFO now emits `assign rd_addr = rd_ptr[3:0]` /
   `wr_addr = wr_ptr[3:0]`; 90-design build corpus byte-compared against
   baseline (identical pass/fail sets, zero regressions); full test suite green.
   **Follow-up discovered (see #27):** `skalp ec` cannot verify ANY design with
   a memory array — the gate-level simulation never models memory writes
   (`mem_rdata` stays 0 forever), so memory designs fail smoke EC even when
   the emitted SV is correct. This is independent of the slice fix (a plain
   `mem[ptr]` design fails identically). The gate netlist DOES get the sliced
   4-bit address (`mem_raddr[3:0]`), confirming lowering is right.

5. **FIXED (2026-08-03). `#[cdc(...)]` auto-synchronizer conflicts with user
   logic.** The generated Gray sync chain drove the same nets as hand-written
   sync flops (multi-driven `wr_ptr_gray_sync_rd`), the auto-generated sync
   registers were never clocked, and the scaffold's binary input was undriven
   — the whole blob was non-functional AND it re-declared the annotated
   signal as a wire while the user's `always_ff` non-blocking-assigned it
   (illegal SV on top of the driver conflict).
   **Fix:** `#[cdc]` is now what the published tutorial says it is — a
   VERIFICATION annotation over the user's hand-written synchronizer. All
   THREE emitters (skalp-codegen SystemVerilog, skalp-hir-codegen SV, and
   VHDL) declare the signal normally, emit a documenting `// CDC:` comment
   plus the `(* ASYNC_REG = "TRUE" *)` synthesis attribute, and generate NO
   hardware; the annotation still feeds MIR CDC analysis. The five scaffold
   generators (~270 lines: TwoFF/Gray/Pulse/Handshake/AsyncFifo) are
   deleted. **Verified:** minimal repro emits a single-driver reg with
   ASYNC_REG; the FULL tutorial ch08 AsyncFIFO (dual-clock, Gray pointers,
   `#[cdc]` + manual two-flop chains) now builds cleanly AND passes complete
   EC with SAT proof; 8-design EC sweep green; corpus unchanged; full suite
   zero regressions. Suite test:
   `test_triage5_cdc_annotation_does_not_generate_hardware`.
   NOTE: the tutorial's promised consistency check ("annotate sync_stages=2
   but only one flop in the chain → error") remains unimplemented — that is
   #10's CDC-analysis work, not codegen.

6. **FIXED (2026-08-03). `open` port binding lowers to `.z(0)`** — instance
   output tied to a constant (illegal SV, and wrong intent). `_` binding
   worked correctly (connection omitted); `open` built as an identifier
   expression, which originally lowered to constant 0 and — after the #1
   undefined-identifier check landed — became a hard error
   (``undefined identifier `open` ``). **Fix:** `open` is now recognized at
   all three wildcard-binding sites (`build_connection`,
   `build_instance_as_statements` — expression side only, so a PORT named
   `open` still binds — and `build_struct_field_init`) with exactly `_`'s
   semantics: the connection is skipped and the output gets an auto-wire
   nothing reads (DCE-able, legal SV). **Verified:** repro builds cleanly
   with `.carry(adder_carry)` instead of a constant tie and passes full EC
   with SAT proof; corpus unchanged; full suite zero regressions. Suite
   test: `test_triage6_open_binding_skips_connection`.

7. **FIXED (2026-08-04). `let x = 0` special-cased as "placeholder signal",
   assignment skipped.** `hir_to_mir.rs` treated ANY let-binding of literal 0
   as an entity-output placeholder and suppressed the initializing
   assignment — a user's `let zero = 0` got an undriven node (which happened
   to read 0, masking the drop until the value was used non-trivially).
   **Fix:** exactly what the triage prescribed — an explicit
   `is_placeholder: bool` field on `HirLetStatement` (serde-default false),
   set ONLY by `build_signal_as_let` for BARE `signal x: T;` declarations in
   trait-method bodies (whose value comes from an entity output). All 24
   construction sites audited: substitution/clone paths copy the flag,
   synthesis sites set false. The MIR heuristic now reads the flag.
   **Verified:** `let zero = 0` / `let base: bit[8] = 0` keep their
   assignments in both event-block and impl-level (combinational) contexts;
   the impl-level case passes full EC with SAT proof; stdlib trait bodies
   (whose placeholders motivated the heuristic) still work — corpus
   unchanged, full suite zero regressions. Suite tests:
   `test_triage7_let_zero_keeps_assignment` and
   `test_triage7_impl_level_let_zero`.

## P1 — missing checks the language promises

8. **FIXED (2026-08-03). No undriven-output diagnostic.** An `out` port never
   assigned built silently. Now `crates/skalp-mir/src/undriven.rs` walks every
   module (continuous assigns, process bodies incl. if/match/loops, generate
   blocks, child-instance output connections) and the compiler fails the build
   for undriven outputs in modules reachable from the main design.
   Unspecialized generic templates (empty-shell modules whose monomorphized
   specializations carry the real bodies) are skipped, and reachability roots
   now include specializations of main entities ("TmrCounter_8" for generic
   "TmrCounter"). Immediately caught two real bugs: the tutorial ch09
   TmrCounter (tuple-match drop, issue #2 — `count` and `tmr_error` both
   reported instead of silently emitting a voter-less TMR) and
   examples/advanced_types.sk, whose union-typed `data_out` was never assigned
   in the shipped example (now driven via a whole-union passthrough; note that
   union FIELD assignment to ports panics — pre-existing "Bug #85" guard).
   Verified: 40-design corpus green, EC proofs intact, golden/sim/equivalence
   suites green; test_simulation_suite (hierarchical FIFO ordering) and
   test_tuple_destructuring (2 FP32-tuple panics at the assignment-conversion
   guard) fail identically WITHOUT this change — pre-existing, need their own
   triage.

9. **FIXED (2026-08-04). Match exhaustiveness checking does not exist.**
   A match missing an enum arm built clean and the missing value fell into
   the last arm; published docs promise a compile error. **Implemented** as
   a conversion error in hir_to_mir (`check_match_exhaustiveness`, hooked at
   both the match-statement dispatcher and the match-expression converter;
   reachability-scoped like the other build-failing checks — latent stdlib
   issues can't block unrelated designs):
   - unguarded `_` or variable-binding arm ⇒ exhaustive;
   - ENUM scrutinee (identified via Path/TupleVariant patterns): every
     variant covered or error naming the missing variant(s);
   - integer scrutinee (bit/logic/nat/int/bool): all 2^N distinct literal
     values (Integer/Boolean/BitVector literals normalized) or error with
     the coverage count; N > 20 always requires a catch-all;
   - guarded arms never count toward coverage (the guard can be false);
   - tuple/struct/unresolvable scrutinees are SKIPPED (no false positives)
     — tuple-product checking is a follow-up.
   **Verified:** enum repro errors naming `Done`; 3-of-4 bit repro errors
   with the count; complete enumeration and wildcard forms build; 90-design
   corpus unchanged and full suite has ZERO new failures — no existing
   design relied on non-exhaustive matches. EC sweep green. Suite tests:
   `test_triage9_nonexhaustive_enum_match_fails`,
   `test_triage9_bit_match_coverage`.

10. **FIXED (2026-08-04). CDC diagnostics stripped — and the analysis was
    vacuous.** `report_cdc_violations` computed severity strings then printed
    nothing, and critical failures reported only a count. Restoring the
    rendering exposed the deeper problem: NO design ever produced a
    violation, because no signal ever had a clock domain — hir_to_mir never
    populated `signal.clock_domain`, and the stamps hir_builder DID write
    used pre-monomorphization port IDs (stale after specialization remaps
    them; consulting them produced false CRITICALs on the tutorial
    AsyncFIFO). **Fixes (self-contained in the CDC analyzer):** rendering
    restored with per-violation details; descriptions name the offending
    signal; clock ports without MIR domains (triage #13's lifetime gap) get
    implicit port-ID-keyed domains matching hir_builder's scheme; signal
    domains inferred from the assigning process (stale HIR stamps ignored);
    domains propagate through combinational assignments to fixpoint (Gray
    encodes of registered pointers are analyzed); severity policy —
    crossings through LOGIC = CRITICAL (fails the build with details), bare
    registered samples (synchronizer first stages) = WARNING, samples into
    #[cdc]-annotated targets = INFO. **Verified:** tutorial ch08 AsyncFIFO
    reports exactly its two true crossings as warnings and builds + passes
    full EC; arithmetic crossing fails the build naming the signal;
    single-clock designs silent; EC sweep green; corpus unchanged; suite
    zero regressions. Tests: test_triage10_cdc_critical_fails_build_with_
    details / _bare_sample_and_annotation_build /
    _comb_derived_domain_propagates. REMAINING follow-ups: sync_stages-vs-
    chain-depth consistency check; #13 lifetime plumbing so 'wr/'rd names
    appear instead of numeric domain ids.

11. **FIXED (2026-08-04) — and legacy `let`-instantiation REMOVED.** Per the
    user's decision, impl-level `let x = Entity {...}` is now a hard error
    (reachability-scoped in compiler.rs, with an `inst` fix-it); every input
    port of an instantiated entity must be connected (checked cross-file and
    post-monomorphization). ~530 in-repo sites across 64 files migrated to
    `inst` + dot-access. Exposed and fixed along the way: build_connection
    silently dropped PathExpr/ConcatExpr/CallExpr/ParenExpr/IfExpr/MatchExpr
    connection values; the stdlib PartitionableMantissaMultiplier sites were
    genuinely missing `mode`; keyword-named ports (`output`) now valid after
    `.`; signals with BOTH a decl initializer and a continuous assign kept
    the init in behavioral SIR (stuck outputs) — MIR clears dead
    wire-initializers. fn-BODY `let` remains supported (trait plumbing;
    stdlib fp bodies need output-binding across behavioral/NCL). KNOWN
    REMAINING (2 suite tests, next up): graphics multi_clock struct output
    on a keyword port reads zeros; test_mwe_lir_gate_equivalence
    (LirToAig-vs-gate BMC) mismatches though full `skalp ec` proves the
    migrated MWE.

12. **`stream<T>` generates no handshaking.** `hir_to_mir.rs:18270` lowers stream
    ports to the bare inner type (`TODO: Add proper stream protocol support`). Either
    implement valid/ready lowering or error on `stream` ports until it exists — docs
    claim the compiler "enforces backpressure."

27. **FIXED (2026-08-03). `skalp ec` cannot verify designs with memory arrays.**
    Any design containing `signal mem: [T; N]` failed smoke EC with
    `mir=<written value> gate=0` even when the emitted SV was correct.
    **Root causes (three, stacked):**
    (a) the generic ASIC library EC synthesizes with has no RAM cell, and
    `map_memblock_standalone` just warned on stderr and DROPPED the memory —
    the "falls back to DFF decomposition" doc comment was aspirational;
    (b) `lir_to_aig` swept the logic cones driving the MemBlock's ports
    (raddr/waddr/wdata/we) because nothing else in the AIG consumed them, so
    even a mapped memory would have seen undriven address/enable nets;
    (c) `MirToAig` (SAT phase) only handled constant-index BitSelect —
    dynamic element reads (`mem[ptr]`) returned a single false bit and dynamic
    element writes were silently dropped; constant-index selects on arrays
    were also wrong (treated as a 1-bit pick instead of an element).
    **Fixes:** `decompose_memblock_to_dffs` in `tech_mapper.rs` (per-word DFFs
    + write-select muxes + shared read-decode + per-bit read mux chain —
    plain cells every downstream consumer already understands);
    `lir_to_aig` Phase 6.5 exports physical-node input signals as AIG outputs
    so their cones survive optimization and land as driven nets;
    `MirToAig` gained `array_element_info`/`convert_select_read`/
    `assign_select` — element-width selects on array bases with full dynamic
    index support (priority mux for reads, read-modify-write mux per element
    for writes, composing with the If-statement snapshot/mux machinery).
    **Verified:** memplain + memslice repros pass ALL EC phases (smoke 100
    cycles, SAT proof over all states, 10/10 bug-injection detection —
    including inverted memory-DFF next-states, proving the memory is really
    modeled on both sides); `examples/async_fifo.sk` (dual-clock, memory,
    sliced pointers) passes full EC; counter/alu/fifo/adder/cdc examples all
    still pass; 90-design corpus and full suite byte-identical to baseline.
    Suite regression tests: `test_memory_equivalence` (port-nets-driven +
    128-DFF decomposition + SAT equivalence). CAVEAT: libraries WITH a real
    RAM cell (iCE40/ECP5 BRAM) still have no SAT-phase model — smoke works
    (gate sim models RamBlock), but `GateNetlistToAig` has no Ram arm. EC
    uses generic_asic, so this only matters for --library ec runs.

28. **FIXED (2026-08-03). `skalp ec` smoke-fails hierarchical designs.**
    `examples/equivalence_mwe.sk` (4 child entities via `let` instantiation)
    failed smoke EC: `counter_out`/`pwm_out`/`pwm_counter` read
    `mir=<live value> gate=0`. **Root cause:** the connection-info extractor
    in `mir_to_lir.rs` classifies a connection as a virtual instance-output
    reference (`InstancePort`) whenever the connected name starts with
    `{instance_name}_` — and instance names can be prefixes of unrelated
    parent declarations. Instance `counter` + parent port `counter_enable`
    matched, so `enable: counter_enable` became
    `InstancePort("counter","enable")` — which `flatten()` did not handle at
    all (`_ => continue`), silently dropping the connection and leaving the
    child's input ports undriven. FaultLatch/StateMachine worked only because
    their port names (`fault_in`, `sm_start`) don't start with the instance
    names. Diagnosed by dumping the flattened LIR: Counter8's mux read
    `top.counter.load_value` with no driver, while FaultLatch's nodes
    referenced parent nets directly. **Fix (two-part):** (1) never classify a
    name as InstancePort when it names a real parent port/signal/variable;
    (2) `flatten()` now resolves InstancePort connections to the sibling
    instance's flattened port signal instead of dropping them. **Verified:**
    MWE passes ALL EC phases (smoke + full SAT proof + self-test); the entire
    EC sweep (both memory repros, ec fixtures, async_fifo, counter/alu/fifo/
    adder/cdc) still passes; corpus + suite byte-identical to baseline. Suite
    regression tests: `test_triage28_child_input_ports_aliased` (no node
    consumes an unaliased child input port) and
    `test_triage28_mwe_sat_equivalent` (full MIR-vs-gates SAT proof).

29. **FIXED (2026-08-03). `examples/hierarchical_alu.sk` fails EC — sliced
    port connections dropped.** Smoke mismatch at cycle 6: `result`
    mir=0x85f00000 vs gate=0x5ccb885f (both live). The generic-children
    suspicion was WRONG — monomorphized `Adder_32`/`Comparator_32` lowered
    fine. **Root cause:** the Shifter's connections `shift_amt: b[4:0]`
    (Range) and `shift_left: op[0]` (BitSelect) hit the third dropped-
    connection kind in `flatten()`'s alias pass: `PortConnectionInfo::Range`
    and `::BitSelect` fell into `_ => continue`, so `top.shifter.shift_amt`
    and `top.shifter.shift_left` were consumed by the shift logic with no
    driver (probe showed the Shl/Shr/Mux2 nodes reading undriven nets).
    Same family as #28 — flatten dropped THREE of six connection kinds
    (InstancePort fixed in #28; Range/BitSelect here). **Fix:** the alias
    pass now synthesizes a `RangeSelect` extraction node (bit-select =
    single-bit range) from the parent signal into the child's flattened port
    signal, mirroring the existing Constant-connection handling. Output-side
    Range/BitSelect connections (child output driving a slice of a parent
    signal) remain unhandled — no known repro; would need read-modify-write
    insertion. **Verified:** hierarchical_alu passes ALL EC phases (smoke +
    full SAT proof + self-test); 12-design EC sweep green; corpus unchanged;
    full suite zero regressions. Suite regression test:
    `test_triage29_hierarchical_alu_sat_equivalent` (asserts the slice
    nodes exist + full MIR-vs-gates SAT proof).

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

30. **FIXED (2026-08-03). generate-for in GENERIC impls is dropped at parse
    time.** `elaborate_generate_for_in_impl` bailed silently on symbolic
    bounds (`0..WIDTH`), LOSING the loop body from the generic impl's HIR —
    `std_multiplier`'s shift-add chain never existed in any specialization.
    **Fix, two parts:** (a) hir_builder now preserves un-evaluable
    generate-fors as symbolic `HirStatement::GenerateFor` templates (body
    signals, assignments, lets built ONCE; barriers skipped); (b)
    `specialize_implementation` elaborates them once bounds are concrete —
    per-iteration signal clones (`{name}__g{i}`, fresh IDs), iterator
    substitution, and SSA-CHAINED accumulator semantics: whole-signal writes
    to outer signals (`acc = acc + addend`) read the previous iteration's
    value (a synthesized `acc__ginit` carries the initial value for i=0) and
    only the last iteration writes the original signal. A naive unroll
    multi-drives the accumulator with a combinational self-loop — which is
    exactly what the CONST-bound elaboration path still emits for this shape
    (pre-existing; only iteration 0 survives — see the /tmp/genmul probe).
    The bit[N] Mul trait route is re-enabled and verified: 8-bit `*` emits
    specialized `std_multiplier_8` + `std_adder_16` with a correct chain.
    **Verified:** generic-accumulator repro emits the exact SSA chain and
    passes full EC (SAT proof); function_inlining 16/16 with and without
    stdlib; intent_and_numeric 24/24; full suite net +2 passing vs the #31
    baseline with zero regressions. Suite tests:
    `test_triage30_generic_generate_for_elaborated` (chain structure,
    single driver on the accumulator) and
    `test_triage30_generic_generate_for_sat_equivalent`.
    REMAINING (#32 gate): Mul routes through the stdlib only for widths
    where the internal 2N+1-bit adds fit the behavioral backend's 64-bit
    arithmetic (N ≤ 31); wider and symbolic-width Muls stay primitive.

32. **FIXED (2026-08-03, 65-128 bits). Behavioral simulator C++ backend
    cannot do arithmetic on values wider than 64 bits.** Signals over 64
    bits lowered to `uint32_t[N]` arrays in the generated C++, and shift/add
    on them emitted invalid C++, failing compilation of the simulation
    library. **Fix:** 65-128-bit values now lower to native
    `unsigned __int128` (clang/GCC on all 64-bit hosts) — ordinary
    expression emission stays valid; the array-storage threshold moved to
    >128 bits to match Metal. Follow-on codegen fixes: scalar shift/or
    concat packing for 65-128-bit outputs (was word-wise array writes);
    slice extraction masks were CLAMPED at u64::MAX, silently zeroing bits
    64+ of a 65-128-bit slice (std_adder_80's `sum = extended_sum[79:0]`
    lost its top 16 bits); wide (65-127-bit) binary results and Not/Neg now
    mask to signal width via an __int128 mask expression (Not/Neg masking
    for 33-63 bits was also missing — pre-existing garbage upper bits).
    The Mul trait-route gate lifted from N≤31 to N≤63 (2N+1 ≤ 128); 32-bit
    `*` through std_multiplier_32's 65-bit internals now compiles AND
    computes correctly. **Verified:** new suite tests
    `test_triage32_wide_concat_shift_add` (80-bit concat/shl/add against
    u128 reference math, carry into high bits, with and without stdlib
    visible) and `test_triage32_mul32_through_stdlib_chain`; 7-design EC
    sweep green; corpus unchanged; full suite zero regressions.
    REMAINING: >128-bit values still use uint32_t arrays with no arithmetic
    support (concat/slice work element-wise; +,<<,~ do not) — no known
    designs hit this; symbolic-width (BitParam) Mul stays primitive.

33. **FIXED (2026-08-04). Event-block `let` bindings "mis-lower" on the EC
    gate side — actually: TIE cells simulated as 0.** The initial suspicion
    (MIR→LIR event-block variable lowering) was WRONG: probing showed the
    LIR and the synthesized netlist both correct (z's DFF D-input =
    `const_1`). **Root cause:** the gate simulator's cell-FUNCTION dispatch
    (`cell_function_to_primitive`) had no TieHigh/TieLow arms — the
    `_ => Buf` fallthrough turned tie cells into zero-input buffers that
    evaluate false. TIE_HIGH nets read 0, so any register fed (directly or
    through constant-folded logic) by const_1 held the wrong value in smoke
    EC. const_0 was accidentally correct, which is why most tie-using
    designs (memory decomposition WE seeds etc.) never tripped it. The SAT
    phase was unaffected (its converter matches tie cells by NAME in the
    fallback). **Fix:** explicit
    `CellFunction::TieHigh/TieLow → PrimitiveType::Constant` arms.
    **Verified:** the letzero repro passes ALL EC phases (smoke + full SAT
    proof); 14-design EC sweep green; corpus unchanged; full suite zero
    regressions. Suite test:
    `test_triage33_tie_cells_simulate_as_constants` (smoke-level, the layer
    that broke).

31. **FIXED (2026-08-03). Stdlib visibility broke most Testbench-based suites
    — the test_function_inlining cluster (15/16), counter_example,
    ergonomic_testbench, cdc_verification, graphics_pipeline_functional,
    intent_and_numeric, fpmul_entity/nogeneric/debug, and more: 51 suite
    tests flipped to passing, zero regressions (791/67 vs 740/118).**
    FIVE stacked defects, all triggered whenever SKALP_STDLIB_PATH was set
    (which any sibling test in the consolidated suite binary does
    process-globally):
    (a) trait-operator resolution (`+` on bit[8] → stdlib `impl Add for
    bit<N>` → `std_adder<8>`) runs AFTER monomorphization, so the
    specialized entity never existed — the emitted design referenced an
    undefined generic module with unresolved WIDTH. Fixed with on-demand
    specialization: the transform records missing specializations,
    compiler.rs specializes them at HIR level (MonomorphizationEngine::
    specialize_entity/implementation) and re-runs the transform to fixpoint.
    This also fixed FpMul/FpAdd entity instantiation (the fpmul clusters).
    (b) engine `remap_expr_ports` rebuilt binary exprs with
    `impl_style: default()`, discarding `#[impl_style::primitive]` — the
    stdlib adder's own `+` re-trait-resolved, recursing std_adder_8 → _9 → ….
    (c) assertion conditions (`assert property (… a + b …)`) went through
    trait resolution and instantiated hardware; now converted with trait
    resolution disabled (in_assertion flag).
    (d) `build_instance_as_statements`' Connection filter was missing
    UnaryExpr (et al.) — the stdlib Sub impl's `b: ~b` connection was
    silently dropped, leaving the subtractor's b input unconnected (10-3=11).
    (e) Testbench implicit top-module selection picked "the uninstantiated
    module with most instances" — with stdlib modules in the MIR it silently
    simulated a Cordic block instead of the fixture entity; now prefers
    main_entity_names. The SIR compilation cache also ignored
    SKALP_STDLIB_PATH, reusing stale artifacts across env changes — the env
    var value is now part of the cache key.
    Plus #30's Mul-primitive fallback for the `*` cases.

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

**FIXED 5 — "SAT unreachable-state failures" were a corrupt reference model.**
The diagnosis shifted completely: the transition functions ARE equivalent for
all states. `GateNetlistToAig::convert_cell` had no arms for
AndNot/OrNot/Aoi21/Oai21/Aoi22/Oai22 cell functions, and its name-based
fallback matched `ANDNOT_X1` against `starts_with("AND")` — converting it as a
plain AND — while `AOI21_X1` matched nothing, leaving its output unregistered
(consumers read constant false). The SAT miter therefore compared the netlist
against a corrupted model of itself: fsm_min's gate-side `busy` evaluated as
constant 1 in the formal AIG while the real netlist was correct. Fix: proper
CellFunction arms (semantics matched to gate_eval.rs) plus ordered name-based
fallbacks. All reproducers AND SpiMaster now pass with full SAT proofs
("Transition functions equivalent for ALL states"). The harness's
init-constraint machinery remains for genuine don't-care divergences.
The suspected "LIR-side converter gap" (`test_mwe_lir_gate_equivalence`)
turned out to be the opposite — **FIXED 6: a real synthesis bug the BMC was
correctly reporting.** `AigWriter::create_outputs` renamed the SOURCE net to
the output name for passthrough outputs ("rename in place"); when the source
was a primary INPUT net (e.g. `sm_state = sm_st` with `sm_st` a promoted input
driven by a child instance), the input's name was destroyed. Hierarchical
stitching then attached the child's driver to a freshly-created net under the
old input name, leaving the real output net floating — the flattened MWE
netlist drove `sm_state`/`fault_latched`/`counter_out` from nothing. Fix:
never rename primary-input nets in create_outputs; emit a dedicated output net
plus BUF (same treatment as physical pseudo-inputs). test_equivalence_mwe is
now 8/8 green including the BMC check.

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
