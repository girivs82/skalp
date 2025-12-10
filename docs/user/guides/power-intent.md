# Power Intent Guide

This guide explains how to use Skalp's power intent features to specify power management requirements directly in your HDL code. Unlike traditional flows that require separate UPF (Unified Power Format) files, Skalp integrates power intent as first-class language features.

## Table of Contents

- [Overview](#overview)
- [Why Power Intent in HDL?](#why-power-intent-in-hdl)
- [Power Domain Concepts](#power-domain-concepts)
- [Retention Registers](#retention-registers)
- [Isolation Cells](#isolation-cells)
- [Level Shifters](#level-shifters)
- [Power Domain Crossings](#power-domain-crossings)
- [Real-World Examples](#real-world-examples)
- [Generated Output](#generated-output)
- [Best Practices](#best-practices)

---

## Overview

Modern SoCs use aggressive power management to reduce energy consumption. Key techniques include:

- **Power gating**: Completely shutting off unused blocks
- **Voltage scaling**: Running critical paths at higher voltages
- **Retention**: Preserving state during power-down
- **Isolation**: Clamping outputs of powered-down blocks

Traditionally, these are specified in separate UPF files that can drift from the RTL. Skalp solves this by embedding power intent directly in the design.

## Why Power Intent in HDL?

| Aspect | Traditional (UPF) | Skalp |
|--------|-------------------|-------|
| Source of truth | Separate files | Single source |
| Error detection | Simulation/synthesis | Compile time |
| Maintenance | Manual sync needed | Always consistent |
| Learning curve | TCL + UPF semantics | Familiar attributes |
| IP reuse | Complex UPF merging | Inherited from entity |

## Power Domain Concepts

### Always-On Domain
Signals that must remain powered at all times (clocks, resets, wake-up logic).

### Switchable Domain
Signals that can be completely powered off when not in use.

### Retention Domain
Signals that can be powered off but must retain their state.

```skalp
entity SoCSubsystem {
    in clk: clock,
    in reset: bit,

    // Always-on domain - global control signals
    signal wake_interrupt: bit,
    signal power_control: bit[4],

    // Switchable domain - can be fully powered off
    signal compute_engine: bit[256],

    // Retention domain - state preserved during power-down
    #[retention]
    signal processor_state: bit[64],

    #[retention]
    signal register_file: bit[32][16],
}
```

---

## Retention Registers

Retention registers preserve their value when their power domain enters a low-power state. This is essential for:

- Processor context (registers, PC, status)
- Configuration registers
- State machines
- Counters that should resume

### Basic Retention

```skalp
entity ProcessorCore {
    in clk: clock,
    in reset: bit,
    in instruction: bit[32],
    out pc: bit[32],

    // Program counter must survive power-down
    #[retention]
    signal program_counter: bit[32],

    // Status flags
    #[retention]
    signal status_reg: bit[8],

    // General purpose registers
    #[retention]
    signal gpr: bit[32][32],
}

impl ProcessorCore {
    // Normal operation - retention handled automatically
    program_counter = program_counter + 4;
    pc = program_counter;
}
```

### Retention Strategies

Different retention implementations trade off area, leakage, and reliability:

```skalp
entity RetentionStrategies {
    in clk: clock,

    // Auto - compiler chooses best strategy
    #[retention]
    signal auto_retained: bit[32],

    // Balloon latch - smaller area, higher leakage
    // Good for: small number of critical registers
    #[retention(strategy = balloon_latch)]
    signal balloon_retained: bit[16],

    // Shadow register - larger area, lower leakage
    // Good for: large register files, long retention periods
    #[retention(strategy = shadow_register)]
    signal shadow_retained: bit[64],
}
```

### Save/Restore Signals

For explicit control over when state is saved and restored:

```skalp
entity ExplicitRetention {
    in clk: clock,
    in save_state: bit,     // Trigger state save before power-down
    in restore_state: bit,  // Trigger state restore after power-up
    in data: bit[32],
    out result: bit[32],

    #[retention(save_signal = "save_state", restore_signal = "restore_state")]
    signal critical_context: bit[128],
}

impl ExplicitRetention {
    // Power management controller asserts save_state before power-down
    // and restore_state after power-up
    critical_context = data ++ result ++ critical_context[63:0];
    result = critical_context[31:0];
}
```

---

## Isolation Cells

When a power domain is shut off, its outputs become undefined (floating). Isolation cells clamp these signals to known safe values to prevent:

- Spurious toggles in active logic
- Increased current (crowbar current from mid-rail voltages)
- Logical errors in downstream logic

### Clamp to Zero (Most Common)

```skalp
entity CoreToIO {
    in clk: clock,
    in iso_enable: bit,     // Asserted when core domain is off
    in core_data: bit[32],
    out io_data: bit[32],

    // Clamp to zero when core is powered down
    #[isolation(clamp = low, enable = "iso_enable")]
    signal data_to_io: bit[32],
}

impl CoreToIO {
    data_to_io = core_data;
    io_data = data_to_io;  // Becomes 0 when iso_enable is high
}
```

### Clamp to One

For active-low signals like resets:

```skalp
entity ResetIsolation {
    in clk: clock,
    in iso_enable: bit,
    in core_reset_n: bit,
    out io_reset_n: bit,

    // Clamp to 1 (inactive) when core is off
    #[isolation(clamp = high, enable = "iso_enable")]
    signal reset_to_io: bit,
}

impl ResetIsolation {
    reset_to_io = core_reset_n;
    io_reset_n = reset_to_io;  // Stays high (inactive) when isolated
}
```

### Latch Mode

For data buses where glitches are problematic:

```skalp
entity DataBusIsolation {
    in clk: clock,
    in iso_enable: bit,
    in data: bit[64],
    out external_bus: bit[64],

    // Hold last valid value during isolation
    #[isolation(clamp = latch, enable = "iso_enable")]
    signal bus_data: bit[64],
}

impl DataBusIsolation {
    bus_data = data;
    external_bus = bus_data;  // Holds last value when isolated
}
```

### Active-Low Enable

Some power controllers use active-low isolation signals:

```skalp
entity ActiveLowIsolation {
    in clk: clock,
    in iso_en_n: bit,       // Active-low isolation enable
    in data: bit[16],
    out result: bit[16],

    #[isolation(clamp = low, enable = "iso_en_n", active_high = false)]
    signal isolated_data: bit[16],
}
```

---

## Level Shifters

When signals cross between voltage domains (e.g., 0.9V core to 1.8V I/O), level shifters are required. Skalp supports:

### Basic Level Shifting

```skalp
entity VoltageInterface {
    in clk: clock,
    in core_data: bit[32],      // 0.9V domain
    out io_data: bit[32],        // 1.8V domain

    // Mark signal as crossing voltage domains
    #[level_shift]
    signal voltage_crossing: bit[32],
}

impl VoltageInterface {
    voltage_crossing = core_data;
    io_data = voltage_crossing;
}
```

### Explicit Domain Specification

```skalp
entity MultiVoltage {
    in clk_core: clock,
    in clk_io: clock,
    in data: bit[16],
    out result: bit[16],

    // Specify source and destination voltage domains
    #[level_shift(from = "VDD_CORE", to = "VDD_IO")]
    signal core_to_io: bit[16],

    #[level_shift(from = "VDD_IO", to = "VDD_CORE")]
    signal io_to_core: bit[16],
}
```

### Shifter Type

For explicit control over shifter direction:

```skalp
entity DirectedLevelShift {
    in clk: clock,
    in low_v_data: bit[8],
    in high_v_data: bit[8],
    out result_high: bit[8],
    out result_low: bit[8],

    // Low voltage (0.9V) to high voltage (1.8V)
    #[level_shift(from = "core", to = "io", shifter_type = low_to_high)]
    signal up_shift: bit[8],

    // High voltage (1.8V) to low voltage (0.9V)
    #[level_shift(from = "io", to = "core", shifter_type = high_to_low)]
    signal down_shift: bit[8],
}
```

---

## Power Domain Crossings

The `#[pdc]` attribute combines isolation and level shifting for complete power domain crossing:

```skalp
entity PowerDomainCrossing {
    in clk: clock,
    in core_active: bit,
    in core_data: bit[32],
    out io_output: bit[32],

    // Complete power domain crossing: isolation + level shift
    #[pdc(from = 'core, to = 'io, isolation = clamp_low)]
    signal pdc_data: bit[32],
}

impl PowerDomainCrossing {
    // When core domain is off:
    // - Signal is isolated (clamped to 0)
    // - Level shifting is inactive
    // When core domain is on:
    // - Signal passes through level shifter
    pdc_data = core_data;
    io_output = pdc_data;
}
```

---

## Real-World Examples

### Mobile SoC Power Management

```skalp
entity MobileSoC {
    in clk_always_on: clock,
    in clk_cpu: clock,
    in clk_gpu: clock,

    // Power control signals (always-on domain)
    in cpu_power_en: bit,
    in gpu_power_en: bit,
    in cpu_iso_en: bit,
    in gpu_iso_en: bit,
    in save_state: bit,
    in restore_state: bit,

    // CPU interface
    in cpu_cmd: bit[8],
    out cpu_result: bit[32],

    // GPU interface
    in gpu_cmd: bit[16],
    out gpu_result: bit[64],

    // Always-on interrupt controller
    signal wake_sources: bit[8],
    signal pending_irq: bit,

    // CPU domain (can enter retention)
    #[retention(strategy = shadow_register, save_signal = "save_state", restore_signal = "restore_state")]
    signal cpu_registers: bit[32][32],

    #[retention]
    signal cpu_pc: bit[64],

    #[retention]
    signal cpu_status: bit[8],

    #[isolation(clamp = low, enable = "cpu_iso_en")]
    signal cpu_to_bus: bit[64],

    // GPU domain (switchable, no retention)
    #[isolation(clamp = low, enable = "gpu_iso_en")]
    signal gpu_to_bus: bit[128],

    // Cross-domain communication
    #[pdc(from = 'cpu, to = 'always_on, isolation = clamp_low)]
    signal cpu_to_irq: bit[8],
}

impl MobileSoC {
    // CPU operation
    cpu_registers[0] = cpu_cmd as bit[32];
    cpu_pc = cpu_pc + 4;
    cpu_to_bus = cpu_registers[1] ++ cpu_registers[2];
    cpu_result = cpu_registers[0];

    // GPU operation
    gpu_to_bus = gpu_cmd as bit[128];
    gpu_result = gpu_to_bus[63:0];

    // Interrupt handling
    cpu_to_irq = cpu_status;
    pending_irq = |wake_sources | |cpu_to_irq;
}
```

### IoT Sensor Node

```skalp
entity SensorNode {
    in clk: clock,
    in deep_sleep: bit,
    in iso_en: bit,
    in sensor_data: bit[16],
    out tx_data: bit[8],

    // Configuration survives deep sleep
    #[retention]
    signal sample_interval: bit[16],

    #[retention]
    signal sensor_calibration: bit[32],

    #[retention]
    signal packet_sequence: bit[8],

    // Sensor acquisition (switchable)
    #[isolation(clamp = low, enable = "iso_en")]
    signal adc_result: bit[16],

    // Radio interface (switchable)
    #[isolation(clamp = low, enable = "iso_en")]
    signal radio_tx: bit[8],

    // Wake-up comparator threshold (always-on)
    signal wake_threshold: bit[16],
}

impl SensorNode {
    // During active mode
    adc_result = sensor_data;
    packet_sequence = packet_sequence + 1;
    radio_tx = adc_result[15:8];
    tx_data = radio_tx;

    // sample_interval and calibration survive deep sleep
    // packet_sequence increments across sleep cycles
}
```

---

## Generated Output

### SystemVerilog with Synthesis Attributes

```skalp
entity RetentionExample {
    in clk: clock,
    in data: bit[8],
    out result: bit[8],

    #[retention]
    signal saved_state: bit[8],
}
```

Generated:
```systemverilog
module RetentionExample (
    input clk,
    input [7:0] data,
    output [7:0] result
);

    // Power Intent: saved_state
    (* RETAIN = "TRUE" *)
    (* preserve = "true" *)
    (* DONT_TOUCH = "TRUE" *)
    // Retention strategy: auto
    reg [7:0] saved_state;

    assign result = saved_state;

    always @(posedge clk) begin
        saved_state <= data;
    end

endmodule
```

### UPF Generation (Optional)

With `--emit-upf` flag, Skalp can generate standard UPF for EDA tools:

```tcl
# Generated by Skalp compiler
create_power_domain core -elements {u_core/*}
create_power_domain mem -elements {saved_state}

create_supply_net VDD_CORE -domain core
create_supply_net VDD_MEM -domain mem

set_retention ret_mem -domain mem \
    -retention_power_net VDD_MEM \
    -elements {saved_state}

set_isolation iso_core -domain core \
    -isolation_power_net VDD_IO \
    -clamp_value 0
```

---

## Best Practices

### 1. Minimize Retention State

Only retain what's truly necessary:

```skalp
// Good - minimal retention
#[retention]
signal critical_config: bit[32],  // Only config

// Avoid - over-retention wastes power
#[retention]
signal entire_buffer: bit[32][1024],  // Large, likely unnecessary
```

### 2. Group Isolation Signals

Keep isolation enable signals minimal:

```skalp
// Good - single isolation enable for domain
signal core_iso_en: bit,

#[isolation(clamp = low, enable = "core_iso_en")]
signal out1: bit[32],

#[isolation(clamp = low, enable = "core_iso_en")]
signal out2: bit[32],
```

### 3. Consider Clamp Values Carefully

```skalp
// Data buses: low is usually safe
#[isolation(clamp = low)]
signal data_bus: bit[32],

// Active-low resets: high keeps them inactive
#[isolation(clamp = high)]
signal reset_n: bit,

// Status signals: latch prevents spurious changes
#[isolation(clamp = latch)]
signal status: bit[8],
```

### 4. Annotate All Domain Crossings

Don't leave power domain crossings implicit:

```skalp
// Good - explicit crossing annotation
#[pdc(from = 'core, to = 'io, isolation = clamp_low)]
signal cross_signal: bit[16],

// Avoid - implicit crossing (compiler warning)
signal unannoted_cross: bit[16],  // Crosses domains but not marked
```

---

## See Also

- [Attributes Reference](../reference/attributes.md)
- [CDC Patterns Guide](clock-domain-crossing.md)
- [UPF Generation](../reference/cli.md#upf-generation)
