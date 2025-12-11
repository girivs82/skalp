# Safety Goal Syntax Reference

## Overview

Skalp's safety goal system provides a clean separation between safety requirements (defined by safety engineers) and design implementation (done by design engineers), connected through a familiar instantiation-based binding.

### Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│ SAFETY GOAL (safety_goal)                                       │
│ Owner: Safety Engineer                                          │
│ Contains: Abstract signals, failure effects, temporal operators │
│ No design hierarchy references - pure safety specification      │
└─────────────────────────────┬───────────────────────────────────┘
                              │
                              │ Designer instantiates goal
                              │ and maps signals
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ DESIGN (entity + impl)                                          │
│ Owner: Design Engineer                                          │
│ Contains: RTL + inst safety: Goal { signal mappings }           │
│ Familiar instantiation syntax for binding                       │
└─────────────────────────────┬───────────────────────────────────┘
                              │
                              │ Compiler auto-simulates
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ FAULT SIMULATION & AUTO-FMEDA                                   │
│ Owner: Compiler (automatic)                                     │
│ Produces: Measured DC, gap analysis, work products              │
└─────────────────────────────────────────────────────────────────┘
```

### Key Design Principles

1. **Safety goal is abstract** - No design hierarchy references
2. **Binding via instantiation** - Familiar `inst` syntax
3. **All signals declared upfront** - Safety engineer specifies what must be observed
4. **Explicit temporal operators** - Well-defined semantics, no ambiguity
5. **Measured DC** - From simulation, not estimated from tables

---

## Safety Goal Syntax

### Basic Structure

```skalp
safety_goal <Name>: <ASIL_Level> {
    // Signal declarations (abstract interface)
    in  <name>: <type>,              // Input signals
    out <name>: <type>,              // Output signals
    signal <name>: <type>,           // Internal observation points

    // Constants for thresholds
    const <name>: <type> = <value>,

    // Failure effect definitions
    failure_effects {
        <effect_name>: <condition>,
    }

    // Severity classification
    severity {
        <effect_name>: <S1|S2|S3>,
    }

    // Target metrics
    target {
        spfm: >= <percentage>,
        lfm: >= <percentage>,
        pmhf: <= <fit_value>,
    }
}
```

### Signal Annotations

```skalp
safety_goal Example: ASIL_D {
    // Regular signals
    in  command: logic,
    out response: logic[16],
    signal internal_state: logic[4],

    // Detection signal (fault indicator)
    #[detection]
    out fault: logic,

    // Safe state indicator
    #[safe_state]
    out safe: logic,

    // Paired signals for comparison (actual vs expected)
    signal data_actual: logic[32],
    signal data_expected: logic[32],
}
```

### Complete Example

```skalp
safety_goal BrakingSafety: ASIL_D {
    // === INTERFACE SIGNALS ===
    in  brake_command: logic,
    in  brake_pressure: logic[12],
    out valve_output: logic[16],

    // Detection and safe state
    #[detection]
    out fault_indicator: logic,

    #[safe_state]
    out safe_state: logic,

    // === INTERNAL OBSERVATION POINTS ===
    // Safety engineer specifies what must be observed
    signal datapath_actual: logic[16],
    signal datapath_expected: logic[16],
    signal state_machine: logic[4],
    signal sensor_a: logic[12],
    signal sensor_b: logic[12],
    signal sensor_c: logic[12],

    // === CONSTANTS ===
    const max_sensor_deviation: nat = 50,
    const stuck_threshold_cycles: nat = 100,
    const max_valve: nat = 0xFFFF,

    // === VALID STATES ===
    const STATE_IDLE: nat = 0,
    const STATE_ACTIVE: nat = 1,
    const STATE_FAULT: nat = 15,

    // === FAILURE EFFECTS ===
    failure_effects {
        // Simple comparison
        unintended_braking: valve_output > 0 && !brake_command,
        loss_of_braking: valve_output == 0 && brake_command for > 10_cycles,

        // Paired signal comparison
        silent_corruption: datapath_actual != datapath_expected && !fault_indicator,

        // Temporal operators
        fsm_stuck: @stable(state_machine, stuck_threshold_cycles),
        valve_stuck_high: valve_output == max_valve for > 10_cycles,

        // Redundancy checking
        sensor_disagreement: @max_deviation(sensor_a, sensor_b, sensor_c) > max_sensor_deviation,

        // State machine validation
        invalid_state: @not_oneof(state_machine, [STATE_IDLE, STATE_ACTIVE, STATE_FAULT]),

        // Edge detection
        spurious_transition: @changed(state_machine) && @stable(brake_command, 5_cycles),
    }

    // === SEVERITY ===
    severity {
        unintended_braking: S3,
        loss_of_braking: S3,
        silent_corruption: S3,
        fsm_stuck: S2,
        valve_stuck_high: S3,
        sensor_disagreement: S2,
        invalid_state: S3,
        spurious_transition: S2,
    }

    // === TARGETS ===
    target {
        spfm: >= 99.0,
        lfm: >= 90.0,
        pmhf: <= 10.0,
    }
}
```

---

## Design Binding via Instantiation

The designer binds the safety goal using familiar `inst` syntax:

```skalp
entity BrakeController<'clk> {
    in  clk: clock,
    in  brake_req: bit,
    in  pressure_a: bit[12],
    in  pressure_b: bit[12],
    in  pressure_c: bit[12],
    out valve_cmd: bit[16],
    out fault: bit,
    out safe: bit,
}

impl BrakeController<'clk> {
    // Internal signals
    signal voted_pressure: bit[12];
    signal alu_out: bit[16];
    signal golden_alu: bit[16];
    signal fsm_state: bit[4];

    // === SAFETY GOAL INSTANTIATION ===
    // Binds abstract signals to concrete implementation
    inst safety: BrakingSafety {
        // Interface ports
        brake_command: brake_req,
        brake_pressure: voted_pressure,
        valve_output: valve_cmd,
        fault_indicator: fault,
        safe_state: safe,

        // Internal observation points
        datapath_actual: alu_out,
        datapath_expected: golden_alu,
        state_machine: fsm_state,
        sensor_a: pressure_a,
        sensor_b: pressure_b,
        sensor_c: pressure_c,
    }

    // === SAFETY MECHANISMS ===
    #[safety_mechanism(type: tmr)]
    signal voted_pressure: bit[12] = tmr_vote(pressure_a, pressure_b, pressure_c);

    // Reference model for comparison
    signal golden_alu: bit[16] = compute_expected(voted_pressure);

    // Regular design logic
    on(clk.rise) {
        alu_out <= compute_valve(voted_pressure);
        valve_cmd <= alu_out;
        fault <= vote_error | watchdog_timeout;
    }
}
```

### Compiler Validation

The compiler validates that all safety goal signals are bound:

```
error[SAFETY002]: incomplete safety goal binding
  --> src/brake_controller.sk:25:5
   |
25 |     inst safety: BrakingSafety {
   |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^
   |
   = note: missing binding for signal `datapath_expected`
   = help: add `datapath_expected: <your_golden_signal>` to the binding
```

### Instance-Level Coverage

Only entities with a safety goal instantiation are analyzed:

```skalp
entity Top {
    inst brake_main: BrakeController { ... }  // Has inst safety: -> analyzed
    inst brake_aux: BrakeController { ... }   // Has inst safety: -> analyzed
    inst brake_test: BrakeController { ... }  // No inst safety: -> QM (not analyzed)
}
```

---

## Temporal Operators Reference

### Edge Detection

| Operator | Meaning | Example |
|----------|---------|---------|
| `@rose(s)` | Rising edge (0→1 this cycle) | `@rose(trigger)` |
| `@fell(s)` | Falling edge (1→0 this cycle) | `@fell(enable)` |
| `@changed(s)` | Value changed this cycle | `@changed(state)` |
| `@stable(s, n)` | Unchanged for n consecutive cycles | `@stable(fsm, 100_cycles)` |

### History/Past Values

| Operator | Meaning | Example |
|----------|---------|---------|
| `@prev(s)` | Value in previous cycle | `@prev(counter)` |
| `@prev(s, n)` | Value from n cycles ago | `@prev(data, 5)` |
| `@was_high(s, n)` | Was 1 at some point in last n cycles | `@was_high(request, 10_cycles)` |
| `@was_low(s, n)` | Was 0 at some point in last n cycles | `@was_low(busy, 100_cycles)` |
| `@cycles_since(event)` | Cycles since event was true | `@cycles_since(@rose(trigger))` |

### Range/Set Membership

| Operator | Meaning | Example |
|----------|---------|---------|
| `@in_range(s, lo, hi)` | lo ≤ s ≤ hi | `@in_range(temp, 20, 80)` |
| `@outside_range(s, lo, hi)` | s < lo OR s > hi | `@outside_range(voltage, 1080, 1320)` |
| `@oneof(s, [vals])` | s in set | `@oneof(state, [IDLE, RUN, STOP])` |
| `@not_oneof(s, [vals])` | s not in set | `@not_oneof(cmd, [0, 1, 2, 3])` |

### Arithmetic

| Operator | Meaning | Example |
|----------|---------|---------|
| `@abs_diff(a, b)` | \|a - b\| | `@abs_diff(sensor1, sensor2) > 10` |
| `@max_deviation(a, b, ...)` | Maximum pairwise \|diff\| | `@max_deviation(s1, s2, s3) > tol` |
| `@popcount(s)` | Count of 1 bits | `@popcount(errors) > 3` |
| `@hamming_distance(a, b)` | Differing bit count | `@hamming_distance(rx, tx) > 1` |

### Counting/Frequency

| Operator | Meaning | Example |
|----------|---------|---------|
| `@pulse_count(s, window)` | Rising edges in window | `@pulse_count(heartbeat, 1000_cycles) < 5` |
| `@glitch_count(s, window)` | Value changes in window | `@glitch_count(data, 10_cycles) > 5` |
| `@period(s)` | Cycles between rising edges | `@period(clock) < 8` |
| `@frequency(s, window)` | Edges per window | `@frequency(pwm, 1000_cycles)` |

### Data Integrity

| Operator | Meaning | Example |
|----------|---------|---------|
| `@crc8(data)` | Calculate CRC-8 | `@crc8(payload) != received_crc` |
| `@crc16(data)` | Calculate CRC-16 | `@crc16(frame) != fcs` |
| `@crc32(data)` | Calculate CRC-32 | `@crc32(packet) != checksum` |
| `@parity(data)` | Calculate parity | `@parity(byte) != parity_bit` |

### Timing

| Operator | Meaning | Example |
|----------|---------|---------|
| `@latency(trigger, response)` | Cycles between events | `@latency(req, ack) > 100` |

---

## Duration Qualifiers

Duration qualifiers modify conditions to specify temporal requirements:

| Syntax | Meaning | Example |
|--------|---------|---------|
| `cond for N_cycles` | True for exactly N cycles | `busy for 10_cycles` |
| `cond for > N_cycles` | True for more than N cycles | `stuck for > 100_cycles` |
| `cond for >= N_cycles` | True for at least N cycles | `valid for >= 5_cycles` |
| `cond for < N_cycles` | True for fewer than N cycles | `pulse for < 3_cycles` |
| `cond within N_cycles` | Becomes true within N cycles | `ack within 50_cycles` |
| `cond after trigger` | Evaluated after trigger | `response after @rose(request)` |

### Combined Examples

```skalp
failure_effects {
    // Signal stuck high for too long
    valve_stuck: valve == 0xFFFF for > 100_cycles,

    // No acknowledgment within timeout
    ack_timeout: @rose(request) && !@rose(ack) within 1000_cycles,

    // Late response after trigger
    late_response: @rose(trigger) && @latency(trigger, response) > 50,

    // Pulse too short (glitch)
    glitch_detected: @rose(signal) && @fell(signal) for < 3_cycles,
}
```

---

## Advanced Patterns

### Redundant Channel Monitoring

```skalp
safety_goal TripleRedundancy: ASIL_D {
    signal channel_a: logic[16],
    signal channel_b: logic[16],
    signal channel_c: logic[16],
    signal voted_result: logic[16],

    const max_deviation: nat = 10,

    failure_effects {
        // No clear majority
        vote_ambiguous: @no_majority(channel_a, channel_b, channel_c, max_deviation),

        // Single channel diverged
        channel_a_fault: @abs_diff(channel_a, voted_result) > max_deviation &&
                        @abs_diff(channel_b, channel_c) <= max_deviation,

        // All channels disagree
        total_disagreement: @abs_diff(channel_a, channel_b) > max_deviation &&
                           @abs_diff(channel_b, channel_c) > max_deviation &&
                           @abs_diff(channel_a, channel_c) > max_deviation,
    }
}
```

### Protocol Compliance

```skalp
safety_goal AXIProtocol: ASIL_B {
    signal awvalid: logic,
    signal awready: logic,
    signal wvalid: logic,
    signal wready: logic,
    signal bvalid: logic,
    signal bready: logic,
    signal awaddr: logic[32],

    failure_effects {
        // Address changed during handshake
        addr_unstable: @changed(awaddr) && awvalid && !awready,

        // Write data without address phase
        write_before_addr: @rose(wvalid) && !@was_high(awvalid, 10_cycles),

        // Response without write
        spurious_response: @rose(bvalid) && !@was_high(wvalid, 100_cycles),

        // Handshake timeout
        write_timeout: wvalid && !wready for > 1000_cycles,

        // Valid stuck high (protocol violation)
        valid_stuck: awvalid && !awready for > 10000_cycles,
    }
}
```

### Watchdog Monitoring

```skalp
safety_goal WatchdogSafety: ASIL_D {
    signal kick: logic,
    signal timeout: logic,
    signal cpu_alive: logic,

    const kick_window_min: nat = 100,
    const kick_window_max: nat = 1000,

    failure_effects {
        // Kick too fast (potential runaway)
        kick_too_fast: @period(kick) < kick_window_min,

        // Kick too slow (about to timeout)
        kick_too_slow: @period(kick) > kick_window_max,

        // Missing kicks
        kick_missing: @pulse_count(kick, 5000_cycles) < 3,

        // Timeout without effect
        timeout_ignored: @rose(timeout) && cpu_alive for > 100_cycles after @rose(timeout),

        // Spurious timeout
        spurious_timeout: @rose(timeout) && @cycles_since(@rose(kick)) < kick_window_max,
    }
}
```

### State Machine Safety

```skalp
safety_goal FSMSafety: ASIL_C {
    signal state: logic[4],
    signal next_state: logic[4],
    signal trigger: logic,

    const IDLE: nat = 0,
    const INIT: nat = 1,
    const RUN: nat = 2,
    const STOP: nat = 3,
    const ERROR: nat = 15,

    // Valid state transitions
    valid_transitions {
        IDLE -> INIT: trigger,
        INIT -> RUN: true,
        RUN -> STOP: !trigger,
        STOP -> IDLE: true,
        * -> ERROR: true,        // Any state can go to ERROR
        ERROR -> IDLE: trigger,  // Reset from ERROR
    }

    failure_effects {
        // Invalid state encoding
        invalid_encoding: @not_oneof(state, [IDLE, INIT, RUN, STOP, ERROR]),

        // Illegal transition (not in valid_transitions)
        illegal_transition: @invalid_transition(state),

        // State stuck (not ERROR/IDLE)
        state_stuck: @stable(state, 10000_cycles) &&
                    @not_oneof(state, [IDLE, ERROR]),

        // Skipped state
        skipped_init: @prev(state) == IDLE && state == RUN,

        // Backward transition (except to ERROR/IDLE)
        backward_transition: @prev(state) > state &&
                            @not_oneof(state, [IDLE, ERROR]),
    }
}
```

---

## Safety Mechanism Annotations

In the design, mark safety mechanisms for DC measurement:

```skalp
impl Controller<'clk> {
    // TMR voter
    #[safety_mechanism(type: tmr)]
    signal voted: bit[12] = tmr_vote(a, b, c);

    // CRC checker
    #[safety_mechanism(type: crc)]
    inst crc_check: Crc8Checker { ... }

    // ECC decoder
    #[safety_mechanism(type: ecc)]
    inst ecc: EccDecoder { ... }

    // Watchdog
    #[safety_mechanism(type: watchdog)]
    inst wdog: Watchdog { ... }

    // Lockstep comparator
    #[safety_mechanism(type: lockstep)]
    signal lockstep_error: bit = core_a.out != core_b.out;

    // Generic safety mechanism
    #[safety_mechanism(type: custom, name: "range_checker")]
    signal range_ok: bit = value >= min && value <= max;
}
```

### Supported Mechanism Types

| Type | Description | Typical DC |
|------|-------------|------------|
| `tmr` | Triple Modular Redundancy | 99%+ |
| `dmr` | Dual Modular Redundancy | 90%+ |
| `lockstep` | Lockstep comparison | 99%+ |
| `crc` | CRC checking | 99%+ |
| `ecc` | Error Correcting Code | 99%+ |
| `parity` | Parity checking | 90%+ |
| `watchdog` | Watchdog timer | 90%+ |
| `range` | Range/limit checking | 60-90% |
| `timing` | Timing monitor | 60-90% |
| `protocol` | Protocol checker | 90%+ |
| `custom` | User-defined | Measured |

---

## Simulation Output

When you run `skalp build --safety`, the compiler:

1. Identifies all `inst safety: <Goal>` bindings
2. Enumerates fault sites in covered design
3. Injects faults and simulates
4. Evaluates failure effect conditions
5. Measures DC per effect
6. Reports gaps with specific primitive locations

### Example Output

```
$ skalp build --safety

Safety Analysis: BrakingSafety (ASIL-D)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Design: top.brake_main (via inst safety: BrakingSafety)
Primitives: 124,500
Fault sites: 373,500
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Failure Effect         Severity  Target DC  Measured DC  Status
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
unintended_braking     S3        99.0%      99.7%        ✓ PASS
loss_of_braking        S3        99.0%      99.4%        ✓ PASS
silent_corruption      S3        99.0%      98.2%        ✗ FAIL
fsm_stuck              S2        97.0%      99.1%        ✓ PASS
sensor_disagreement    S2        97.0%      99.8%        ✓ PASS
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Gap Analysis for 'silent_corruption' (DC: 98.2%, need 99.0%):
┌─────────────────────────────────────┬────────┬─────────────┐
│ Undetected Fault Location           │ Count  │ Suggestion  │
├─────────────────────────────────────┼────────┼─────────────┤
│ datapath.alu.carry_chain[7:4]       │ 12     │ Add TMR     │
│ datapath.reg_file.shadow[3:0]       │ 8      │ Add ECC     │
└─────────────────────────────────────┴────────┴─────────────┘

Summary:
  SPFM: 99.1% (target: >= 99.0%) ✓
  LFM:  92.3% (target: >= 90.0%) ✓

error[SAFETY001]: ASIL-D requirements not met for 'silent_corruption'
  = help: Add safety mechanisms to cover 20 undetected primitives
```

---

## Migration from Previous Syntax

If you have existing `safety_entity` definitions, migrate as follows:

### Before (Three-Layer)

```skalp
// Old: safety_entity with manual FMEA
safety_entity BrakingControl implements BrakingSafety {
    covers { top.brake_main, top.brake_aux }

    hsi { ... }
    fmea { ... }

    psm SensorVoting { dc: 99.0 }
}
```

### After (Instantiation Binding)

```skalp
// New: inst safety in design
impl BrakeController<'clk> {
    inst safety: BrakingSafety {
        // Signal mappings
        ...
    }
}
```

**What's removed:**
- `safety_entity` - no longer needed
- Manual `fmea` blocks - auto-generated
- Manual `covers` - determined by which entities have `inst safety:`
- DC overrides - DC is measured, not specified

**What's kept:**
- `safety_goal` - defines abstract requirements
- `#[safety_mechanism]` - marks detection logic
- All temporal operators and failure effects

---

## Summary

| Concept | Syntax |
|---------|--------|
| Define safety goal | `safety_goal Name: ASIL_X { ... }` |
| Abstract signals | `in/out/signal name: type` |
| Detection signal | `#[detection] out fault: logic` |
| Safe state signal | `#[safe_state] out safe: logic` |
| Failure effect | `failure_effects { name: condition }` |
| Severity | `severity { name: S1/S2/S3 }` |
| Targets | `target { spfm: >= 99.0 }` |
| Bind goal to design | `inst safety: Goal { mappings }` |
| Mark safety mechanism | `#[safety_mechanism(type: tmr)]` |
| Temporal operator | `@operator(args)` |
| Duration qualifier | `cond for > N_cycles` |

This design provides:
- Clean separation of concerns
- Familiar syntax for designers
- Explicit semantics for all constructs
- Automatic FMEDA generation
- Measured (not estimated) DC
