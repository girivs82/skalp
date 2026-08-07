# Power Intent Guide

SKALP models power intent **in the language**: the supply tree is declared
at top level, entities bind to power domains by attribute, the compiler
checks the result on every build, and IEEE 1801 (UPF) power intent is
generated from the checked model. There is no side file to keep in sync —
UPF is an *export backend*, and the native flow consumes the in-compiler
model directly.

This guide covers the implemented subset — supply-tree declarations,
checked `#[power_domain]` binding, the dependent-failure (CCF) check, the
domain-crossing warning, and UPF emission — and clearly marks what is
still future work. Every quoted diagnostic below is real compiler output.

## Table of Contents

- [Overview](#overview)
- [Declaring the Supply Tree](#declaring-the-supply-tree)
- [Binding Entities to Domains](#binding-entities-to-domains)
- [The Dependent-Failure (CCF) Check](#the-dependent-failure-ccf-check)
- [Domain-Crossing Warnings](#domain-crossing-warnings)
- [Generated UPF](#generated-upf)
- [Retention Registers](#retention-registers)
- [Isolation and Level Shifting](#isolation-and-level-shifting)
- [Still Future](#still-future)
- [Best Practices](#best-practices)
- [See Also](#see-also)

---

## Overview

Why model power in-language? ISO 26262 dependent-failure analysis requires
a safety mechanism to be *supply-independent* of the element it monitors —
a watchdog sharing a rail with the CPU it watches dies with it
(common-cause failure). In traditional flows power intent lives in a
separate UPF file, so this check degenerates to manual review. In SKALP
the supply tree is part of the design, and the check runs on `skalp
build`, the same way `#[cdc]` and the safety attributes work.

| Aspect | Traditional (UPF) | SKALP |
|--------|-------------------|-------|
| Source of truth | Separate files | Single source (UPF is generated) |
| Supply-independence check | Manual review | Compile time, fails the build |
| Maintenance | Manual sync needed | Always consistent |

Implemented today:

- Top-level `power_domain` declarations: supply tree with derivation
  kinds (`external` / `regulated` / `switched`) and voltage states
- `#[power_domain(name)]` as a **checked reference**, binding by
  containment (an entity's domain covers its whole instance subtree)
- Supply-tree validation: duplicate names, unknown parents, cycles,
  duplicate state names — all build errors
- The dependent-failure (CCF) check on `#[safety_mechanism]` entities,
  with the `allow_shared_supply` escape hatch
- A coarse cross-domain crossing warning keyed on `#[isolation]` presence
- UPF emission (`design.upf`) whenever declarations exist

A complete working reference lives at `examples/power_domains.sk`.

## Declaring the Supply Tree

A `power_domain` declaration names a rail and records **how it is
derived**. The derivation matters: independence for the CCF check is a
property of the supply tree, not of names.

```text
power_domain NAME : external [, states = { ... }] ;
power_domain NAME = regulated( PARENT [, macro = IDENT]
                               [, states = { ... }] ) ;
power_domain NAME = switched( PARENT [, on_when = [!] PATH]
                              [, ack_on = [!] PATH]
                              [, states = { ... }] ) ;
```

- **`external`** — a chip supply port; regulation is the board/PMIC's
  concern.
- **`regulated(parent, macro = ...)`** — a new voltage level produced by
  an on-die analog block (LDO, buck), referenced as a black-box hard
  macro. The language never models the regulator's analog behavior.
- **`switched(parent, on_when = ..., ack_on = ...)`** — the same voltage,
  gated by power-switch cells. `on_when` (enable) and `ack_on`
  (acknowledge) take an **optionally-negated hierarchical path**;
  active-low is expressed with `!`, never with a polarity flag.

States are named, with optional voltages. `0.9V` and `900mV` are both
accepted (values are stored in millivolts); a voltage-less state such as
`off` maps to `OFF` in the generated power state entries:

```text
power_domain vreg_main: external;
power_domain vdd_core = regulated(vreg_main, macro = u_ldo_core,
                                  states = { on: 0.9V, ret: 0.6V, off });
power_domain vdd_gpu  = switched(vdd_core, on_when = !pmu_gpu_sleep,
                                 ack_on = pmu_gpu_ack,
                                 states = { on: 0.9V, off });
power_domain vdd_mon: external;
```

`external`, `regulated`, `switched`, `states`, `macro`, `on_when`, and
`ack_on` are **contextual identifiers, not keywords** — `in external: bit`
or `signal states: bit[4]` elsewhere in the design remain legal.

### Validation

The supply tree is validated at build time. Duplicate domain names,
unknown parents, supply-tree cycles, and duplicate state names are all
hard errors:

```text
error: duplicate power_domain declaration `vdd_a`
error: power_domain `vdd_a`: unknown parent domain `vreg_missing`
error: power_domain `vdd_a`: supply tree contains a cycle through `vdd_a`
error: power_domain `vdd_a`: duplicate state `on`
```

## Binding Entities to Domains

`#[power_domain(name)]` on an entity binds the entity **and its whole
instance subtree** to the named domain. A child entity carrying its own
`#[power_domain]` rebinds its subtree; everything else inherits from its
instantiating context. (Rebinding an individual `inst` is not yet
implemented.)

```skalp
// power_intent_guide.sk
power_domain vreg_main: external;
power_domain vdd_core = regulated(vreg_main, macro = u_ldo_core,
                                  states = { on: 0.9V, ret: 0.6V, off });
power_domain vdd_mon: external;

// Bound to an independent supply path: the CCF check passes.
#[power_domain(vdd_mon)]
#[safety_mechanism(type = watchdog)]
entity Watchdog {
    in clk: clock
    in kick: bit
    out timeout: bit
}

impl Watchdog {
    signal cnt: bit[8] = 0
    on(clk.rise) {
        if kick {
            cnt = 0
        } else {
            cnt = cnt + 1
        }
    }
    timeout = cnt == 255
}

#[power_domain(vdd_core)]
entity Controller {
    in clk: clock
    in kick: bit
    out wd_timeout: bit
}

impl Controller {
    inst wd = Watchdog { clk: clk, kick: kick }
    wd_timeout = wd.timeout
}
```

Once any `power_domain` declarations exist, the attribute argument is a
**checked reference** — a typo is a build error with a fix-it:

```text
error: entity `Foo`: #[power_domain(vdd_cor)] references an undeclared power domain — declare it with `power_domain vdd_cor: ...;`
```

The legacy string form `#[power_domain("name")]` still parses and is
checked against the declarations the same way. In a design with **no**
`power_domain` declarations, both forms are legacy annotation-only: no
checking, no UPF.

## The Dependent-Failure (CCF) Check

**Independence is ancestry.** Two domains are supply-independent iff
their ancestor chains in the supply tree are disjoint. Two rails
regulated off the same `vreg_main` are **not** independent of each other —
an LDO filters its parent, it does not survive it — and the derivation
kind does not affect independence.

The check: a `#[safety_mechanism]` entity whose effective domain shares a
supply ancestor with its instantiating context's domain **fails the
build**. Binding the watchdog above to `vdd_periph` (a second regulator
output of `vreg_main`) while `Controller` sits in `vdd_core` produces:

```text
Error: Failed to compile HIR to MIR with CDC analysis: power-domain dependent-failure check failed with 1 error(s):
  safety mechanism `Watchdog` (instance `wd` in `Controller`) is in power domain `vdd_periph`, which shares a supply ancestor with its context's domain `vdd_core` — not supply-independent from the logic it monitors (common-cause failure) — bind it to an independent supply, or justify with #[power_domain(vdd_periph, allow_shared_supply)]
```

Where the standard permits a justified shared supply, add the escape
hatch — `#[power_domain(vdd_periph, allow_shared_supply)]` — and the
error becomes a documented warning:

```text
PDC warning: safety mechanism `Watchdog` (instance `wd` in `Controller`) is in power domain `vdd_periph`, which shares a supply ancestor with its context's domain `vdd_core` — not supply-independent from the logic it monitors (common-cause failure) [downgraded: allow_shared_supply]
```

Note for FPGA prototyping: `switched` and `regulated` domains are
unimplementable in FPGA fabric, and logic in the same fabric is never
supply-independent. `skalp synth` on an FPGA device therefore refuses
such designs unless you pass `--power-stub`, which prototypes the ASIC
power intent as always-on and prints a report of every stubbed element
(switches always-on, regulators assumed externally supplied) plus the
shared-VCCINT independence caveat. Separately, when a
`constraint physical` block declares bank voltages, every port's
`io_standard` is checked against its bank's rail at compile time — an
LVCMOS33 pin on a 1.8 V bank is a build error (`VCCIO mismatch`). See
the spec's Section 18.7.

## Domain-Crossing Warnings

A net crossing an instantiation edge between two different domains needs
isolation at the boundary. The implemented check is **coarse**: if
neither side of the edge declares any `#[isolation]` signal, the build
warns (it never fails):

```text
PDC warning: nets cross power domains `vdd_core` -> `vdd_mon` (instance `wd` of `Watchdog` in `Controller`) with no #[isolation] strategy declared on either side
```

Declaring an `#[isolation(...)]` signal on either side records that the
crossing is handled and suppresses the warning:

```skalp
// iso_guide.sk
power_domain vdd_core: external;
power_domain vdd_mon: external;

#[power_domain(vdd_mon)]
entity Monitor {
    in kick: bit
    out timeout: bit
}

impl Monitor {
    // The declared isolation signal marks this crossing as handled.
    #[isolation(clamp = low)]
    signal t: bit
    t = kick
    timeout = t
}

#[power_domain(vdd_core)]
entity System {
    in kick: bit
    out wd_timeout: bit
}

impl System {
    inst mon = Monitor { kick: kick }
    wd_timeout = mon.timeout
}
```

Port-granular isolation strategies (which ports, which clamp values,
inference from the power state table) are future work — today the check
tests only for the *presence* of `#[isolation]` signals.

## Generated UPF

Whenever `power_domain` declarations exist, `skalp build` writes
`design.upf` next to `design.sv`:

```text
PDC warning: nets cross power domains `vdd_core` -> `vdd_mon` (instance `wd` of `Watchdog` in `Controller`) with no #[isolation] strategy declared on either side
📄 Power intent: "build/design.upf"
✅ Build complete!
📄 Output: "build/design.sv"
```

For `examples/power_domains.sk` the generated file is:

```tcl
# UPF generated by the SKALP compiler from power_domain declarations.
# The checked model in the source is authoritative; do not hand-edit.
upf_version 2.1

create_supply_port VREG_MAIN
create_supply_net VREG_MAIN
connect_supply_net VREG_MAIN -ports VREG_MAIN
create_supply_net VDD_CORE
# VDD_CORE is driven by regulator macro instance `u_ldo_core` (analog IP)
create_supply_net VDD_GPU
create_supply_port VDD_MON
create_supply_net VDD_MON
connect_supply_net VDD_MON -ports VDD_MON

create_power_domain PD_vreg_main
create_supply_set SS_vreg_main -function {power VREG_MAIN} -function {ground VSS}
create_power_domain PD_vdd_core -elements {.}
create_supply_set SS_vdd_core -function {power VDD_CORE} -function {ground VSS}
create_power_domain PD_vdd_gpu
create_supply_set SS_vdd_gpu -function {power VDD_GPU} -function {ground VSS}
create_power_domain PD_vdd_mon -elements {wd}
create_supply_set SS_vdd_mon -function {power VDD_MON} -function {ground VSS}

create_power_switch SW_vdd_gpu -domain PD_vdd_gpu\
    -input_supply_port {sw_in VDD_CORE}\
    -output_supply_port {sw_out VDD_GPU}\
    -control_port {sw_ctrl pmu_gpu_sleep}\
    -on_state {on_s sw_in {!pmu_gpu_sleep}}\
    -off_state {off_s {pmu_gpu_sleep}}\
    -ack_port {sw_ack pmu_gpu_ack {pmu_gpu_ack}}

add_power_state SS_vdd_core -state on {-supply_expr {power == `{FULL_ON, 0.90}`}}
add_power_state SS_vdd_core -state ret {-supply_expr {power == `{FULL_ON, 0.60}`}}
add_power_state SS_vdd_core -state off {-supply_expr {power == `{OFF}`}}
add_power_state SS_vdd_gpu -state on {-supply_expr {power == `{FULL_ON, 0.90}`}}
add_power_state SS_vdd_gpu -state off {-supply_expr {power == `{OFF}`}}
```

Points worth noting:

- `external` rails get `create_supply_port` + `create_supply_net`;
  `regulated` rails get a net annotated with the driving macro instance.
- `-elements` lists only the **bound instance roots** (`wd` for
  `vdd_mon`; `.` when the bound entity is the top): containment makes
  listing the subtree redundant.
- The switch's `on_state`/`off_state` Booleans and `-ack_port` come
  straight from the declared `on_when`/`ack_on` expressions.
- Voltages in `add_power_state` come from the declared states.

The UPF is generated output. The checked in-language model is
authoritative — do not hand-edit the file.

## Retention Registers

> **Status: attribute + synthesis markers only.** `#[retention]` emits
> synthesis attributes on the register; retention *semantics* (strategy
> selection, save/restore sequencing, PST-driven inference) are future
> work. The `strategy` / `save_signal` / `restore_signal` parameters
> parse and are recorded, but do not change the generated hardware.

```skalp
// retention_guide.sk
entity RetentionExample {
    in clk: clock
    in data: bit[8]
    out result: bit[8]
}

impl RetentionExample {
    #[retention]
    signal saved_state: bit[8]
    on(clk.rise) { saved_state = data }
    result = saved_state
}
```

Generated SystemVerilog:

```systemverilog
    (* RETAIN = "TRUE" *)
    (* preserve = "true" *)
    (* DONT_TOUCH = "TRUE" *)
```

Keep retention minimal when you use it — retain configuration and
context, not large buffers.

## Isolation and Level Shifting

`#[isolation(clamp = low | high | latch, enable = "sig")]` on a signal
records an isolation strategy. Two things are real today:

1. Its **presence** marks the entity as handling cross-domain isolation,
   which satisfies the [domain-crossing check](#domain-crossing-warnings).
2. The generated SystemVerilog carries a marker comment (e.g.
   `// Isolation: clamp=low (0)`) on the signal.

Isolation-**cell insertion**, clamp-value verification, and
`#[level_shift]` semantics (voltage compatibility between domains,
shifter inference from the declared state voltages) are future work. The
old `#[pdc(from = 'a, to = 'b)]` tick syntax is retired: power domains
bind structurally by containment, not by lifetime tags — see the
language specification's "Domain Lifetimes Are Clock-Only" design note.

## Still Future

These parts of the recorded power-domain design are **not implemented**;
do not rely on them:

- **Full PST-liveness for switch controls.** No-self-power (a domain
  must not gate its own supply — build error) and a simplified
  controller-liveness warning (the controller should be always-on) ARE
  implemented; per-state liveness analysis needs the PST legality layer.
- **Pin-level related-supply compatibility** (Liberty
  `related_power_pin`; needs per-pin data in `.sklib`).
- **Port-granular isolation / level-shifter strategies and inference.**
- **`#[retention]` semantics** beyond synthesis-attribute emission.
- **Power-fault injection classes**: whole-domain loss, switch
  stuck-off/stuck-on, regulator collapse, overvoltage.
- **The PST legality table** (legal cross-domain state combinations).
- **Instance-level rebinding** (`inst`-site `#[power_domain]`).
- **FPGA bank/domain linkage**: naming a declared power domain as a
  bank's rail (bank voltages are literals today), and modeling device
  supply trees (VCCINT as an implicit external domain).

## Best Practices

1. **Declare the real supply tree**, not just names. The CCF check is
   only as good as the recorded parentage — if two rails come off one
   regulator, say so with `regulated(...)`.
2. **Bind safety mechanisms to genuinely independent rails.** Reach for
   `allow_shared_supply` only where the safety standard permits a
   justified shared supply, and document the justification in the
   design.
3. **Declare `#[isolation]` at real boundaries** so the crossing warning
   stays meaningful — an empty warning list should mean "no unhandled
   crossings", not "warnings ignored".
4. **Treat `design.upf` as build output.** Regenerate it with every
   build; never edit it by hand.

## See Also

- [Attributes Reference](../reference/attributes.md)
- [CLI Reference](../reference/cli.md) — `skalp build` outputs
- Language specification: Power Domains (Section 18) and the remaining
  future work (Section 21.1)
- `examples/power_domains.sk` — complete working reference
- [CDC Patterns Guide](clock-domain-crossing.md)
