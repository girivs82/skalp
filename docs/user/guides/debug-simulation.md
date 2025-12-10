# Debug and Simulation Guide

This guide covers Skalp's built-in debug features including breakpoints, signal tracing, and simulation integration.

## Table of Contents

- [Overview](#overview)
- [Breakpoints](#breakpoints)
- [Signal Tracing](#signal-tracing)
- [Simulation Workflow](#simulation-workflow)
- [Waveform Integration](#waveform-integration)
- [Debug Strategies](#debug-strategies)

---

## Overview

Skalp provides first-class debug support through attributes that become part of your design. Unlike traditional approaches that require separate testbench code, Skalp's debug attributes:

- Compile with your design
- Generate synthesizable code with optional debug logic
- Integrate with standard simulation tools
- Can be stripped for production builds

---

## Breakpoints

Breakpoints pause simulation when specific conditions occur, allowing you to inspect the design state.

### Simple Breakpoints

Trigger on any signal change:

```skalp
entity ErrorDetector {
    in clk: clock,
    in data_valid: bit,
    in data: bit[32],
    out error: bit,

    // Break when error_flag transitions
    #[breakpoint]
    signal error_flag: bit,

    // Break on any overflow
    #[breakpoint]
    signal overflow: bit,
}

impl ErrorDetector {
    error_flag = data_valid & (data == 0xDEADBEEF);
    overflow = data[31] & data_valid;
    error = error_flag | overflow;
}
```

### Conditional Breakpoints

Trigger only when an expression is true:

```skalp
entity CounterDebug {
    in clk: clock,
    in enable: bit,
    out count: bit[16],

    // Break when counter exceeds threshold
    #[breakpoint(condition = "counter > 1000")]
    signal counter: bit[16],

    // Break on specific value
    #[breakpoint(condition = "state == 4'hF")]
    signal state: bit[4],

    // Complex condition
    #[breakpoint(condition = "fifo_level > 90 && write_en")]
    signal fifo_level: bit[8],
}
```

### Named Breakpoints

Give breakpoints identifiable names for easier debugging:

```skalp
entity StateMachine {
    in clk: clock,
    in start: bit,
    in abort: bit,
    out done: bit,

    #[breakpoint(name = "FSM_IDLE_UNEXPECTED")]
    signal unexpected_idle: bit,

    #[breakpoint(name = "FSM_INVALID_TRANSITION", condition = "next_state == 4'hF")]
    signal next_state: bit[4],

    #[breakpoint(name = "TIMEOUT_EXPIRED", condition = "timeout_count == 0")]
    signal timeout_count: bit[16],
}
```

### Breakpoints with Messages

Add descriptive messages for context:

```skalp
entity ProtocolChecker {
    in clk: clock,
    in valid: bit,
    in ready: bit,
    in data: bit[64],
    out error: bit,

    #[breakpoint(
        name = "PROTOCOL_VIOLATION",
        condition = "valid && !ready && data_changed",
        message = "Data changed while valid high but ready low - protocol violation"
    )]
    signal data_changed: bit,

    #[breakpoint(
        name = "UNDERFLOW",
        message = "FIFO read when empty - check consumer logic"
    )]
    signal underflow: bit,

    #[breakpoint(
        name = "OVERFLOW",
        message = "FIFO write when full - check producer backpressure"
    )]
    signal overflow: bit,
}
```

### Error Breakpoints

Stop simulation immediately (assertion failure):

```skalp
entity SafetyChecker {
    in clk: clock,
    in watchdog_kick: bit,
    in critical_error: bit,
    out system_reset: bit,

    // Fatal error - stops simulation
    #[breakpoint(is_error = true, name = "WATCHDOG_TIMEOUT")]
    signal watchdog_expired: bit,

    #[breakpoint(
        is_error = true,
        name = "SAFETY_VIOLATION",
        message = "Critical safety check failed - immediate stop"
    )]
    signal safety_fault: bit,

    // Warning only - pauses but can continue
    #[breakpoint(name = "WATCHDOG_WARNING", condition = "watchdog_count < 10")]
    signal watchdog_count: bit[16],
}

impl SafetyChecker {
    // Watchdog implementation
    if watchdog_kick {
        watchdog_count = 1000;
    } else if watchdog_count > 0 {
        watchdog_count = watchdog_count - 1;
    }

    watchdog_expired = watchdog_count == 0;
    safety_fault = critical_error;
    system_reset = watchdog_expired | safety_fault;
}
```

---

## Signal Tracing

Trace attributes mark signals for automatic waveform capture and organization.

### Basic Tracing

```skalp
entity DataPath {
    in clk: clock,
    in data_in: bit[32],
    out data_out: bit[32],

    // Automatically included in waveform
    #[trace]
    signal pipeline_reg1: bit[32],

    #[trace]
    signal pipeline_reg2: bit[32],

    #[trace]
    signal valid_pipe: bit,
}
```

### Grouping Traces

Organize signals into logical groups:

```skalp
entity CPUCore {
    in clk: clock,
    in instruction: bit[32],
    out pc: bit[32],

    // Fetch stage signals
    #[trace(group = "fetch")]
    signal fetch_pc: bit[32],

    #[trace(group = "fetch")]
    signal fetch_valid: bit,

    // Decode stage signals
    #[trace(group = "decode")]
    signal decode_opcode: bit[6],

    #[trace(group = "decode")]
    signal decode_rs1: bit[5],

    #[trace(group = "decode")]
    signal decode_rs2: bit[5],

    // Execute stage signals
    #[trace(group = "execute")]
    signal alu_result: bit[32],

    #[trace(group = "execute")]
    signal branch_taken: bit,

    // Memory stage signals
    #[trace(group = "memory")]
    signal mem_addr: bit[32],

    #[trace(group = "memory")]
    signal mem_data: bit[32],
}
```

### Display Formats

Control how values appear in waveform viewers:

```skalp
entity DisplayFormats {
    in clk: clock,

    // Hexadecimal (default for multi-bit)
    #[trace(radix = hex)]
    signal address: bit[32],

    // Binary for bit patterns
    #[trace(radix = binary)]
    signal control_bits: bit[8],

    // Unsigned decimal
    #[trace(radix = unsigned)]
    signal counter: bit[16],

    // Signed decimal (two's complement)
    #[trace(radix = signed)]
    signal temperature: bit[12],

    // ASCII for character data
    #[trace(radix = ascii)]
    signal uart_char: bit[8],
}
```

### Custom Display Names

Give signals human-readable names:

```skalp
entity UserFriendlyDebug {
    in clk: clock,

    #[trace(display_name = "Write Pointer")]
    signal wr_ptr: bit[10],

    #[trace(display_name = "Read Pointer")]
    signal rd_ptr: bit[10],

    #[trace(display_name = "FIFO Fill Level", radix = unsigned)]
    signal level: bit[10],

    #[trace(group = "AXI", display_name = "Write Address Valid")]
    signal awvalid: bit,

    #[trace(group = "AXI", display_name = "Write Address Ready")]
    signal awready: bit,
}
```

### Combined Options

Use multiple options together:

```skalp
entity CompleteDebug {
    in clk: clock,
    in data: bit[64],
    out result: bit[64],

    #[trace(group = "arithmetic", radix = signed, display_name = "ALU Input A")]
    signal alu_a: bit[32],

    #[trace(group = "arithmetic", radix = signed, display_name = "ALU Input B")]
    signal alu_b: bit[32],

    #[trace(group = "arithmetic", radix = signed, display_name = "ALU Result")]
    signal alu_out: bit[32],

    #[trace(group = "control", display_name = "State Machine")]
    signal state: bit[4],

    #[trace(group = "control", display_name = "Operation Code", radix = hex)]
    signal opcode: bit[8],
}
```

---

## Simulation Workflow

### Running Simulation

```bash
# Compile with debug enabled (default)
skalp build my_design.sk --output sim_design.sv

# Compile for synthesis (strips debug)
skalp build my_design.sk --output synth_design.sv --release

# Run simulation
skalp sim my_design.sk --testbench tb_my_design.sk
```

### Testbench Integration

```skalp
// Design under test
entity Counter {
    in clk: clock,
    in reset: bit,
    in enable: bit,
    out count: bit[8],

    #[breakpoint(condition = "counter == 255")]
    #[trace(group = "counter", display_name = "Counter Value")]
    signal counter: bit[8],
}

// Testbench
entity tb_Counter {
    // Test signals
    signal clk: clock,
    signal reset: bit,
    signal enable: bit,
    signal count: bit[8],

    // DUT instance
    dut: Counter,
}

impl tb_Counter {
    // Clock generation
    clk = !clk after 5ns;

    // Stimulus
    reset = 1 for 20ns then 0;
    enable = 0 for 30ns then 1;

    // Connect DUT
    dut.clk = clk;
    dut.reset = reset;
    dut.enable = enable;
    count = dut.count;
}
```

### Breakpoint Behavior in Simulation

When a breakpoint triggers:

1. **Simple breakpoint**: Simulation pauses, shows signal name and value
2. **Conditional**: Pauses only when condition is true
3. **Named**: Shows breakpoint name in console
4. **With message**: Displays message
5. **Error**: Stops simulation with error status

Example console output:
```
Time: 1250ns - BREAKPOINT [FSM_INVALID_TRANSITION]:
  Signal: next_state = 4'hF
  Location: cpu_core.sk:45

Time: 2500ns - BREAKPOINT [PROTOCOL_VIOLATION]:
  Message: Data changed while valid high but ready low - protocol violation
  Signal: data_changed = 1
  Context: valid=1, ready=0, data=0xDEADBEEF

Time: 3000ns - ERROR [WATCHDOG_TIMEOUT]:
  Simulation stopped - fatal error
  Signal: watchdog_expired = 1
```

---

## Waveform Integration

### VCD Generation

```bash
# Generate VCD for GTKWave
skalp sim design.sk --vcd output.vcd

# Generate FST for faster viewing
skalp sim design.sk --fst output.fst
```

### Waveform Organization

Traced signals appear organized by group:

```
design_hierarchy/
├── fetch/
│   ├── fetch_pc [31:0]
│   └── fetch_valid
├── decode/
│   ├── decode_opcode [5:0]
│   ├── decode_rs1 [4:0]
│   └── decode_rs2 [4:0]
├── execute/
│   ├── alu_result [31:0]
│   └── branch_taken
└── memory/
    ├── mem_addr [31:0]
    └── mem_data [31:0]
```

### Filtering Traces

In large designs, filter which traces are captured:

```bash
# Only specific groups
skalp sim design.sk --trace-groups "fetch,decode"

# Exclude groups
skalp sim design.sk --exclude-groups "memory"

# By hierarchy
skalp sim design.sk --trace-hierarchy "cpu.core.*"
```

---

## Debug Strategies

### 1. Progressive Debug Depth

Start with minimal tracing, add more as needed:

```skalp
entity ProgressiveDebug {
    // Level 1: Always traced - key interfaces
    #[trace(group = "L1_interface")]
    signal input_valid: bit,

    #[trace(group = "L1_interface")]
    signal output_valid: bit,

    // Level 2: Internal state machines
    #[trace(group = "L2_fsm")]
    signal state: bit[4],

    // Level 3: Detailed internal signals
    #[trace(group = "L3_internal")]
    signal pipeline_stage: bit[8],

    // Level 4: Deep debug - normally disabled
    #[trace(group = "L4_debug")]
    signal micro_state: bit[16],
}
```

```bash
# Quick check - only interfaces
skalp sim --trace-groups "L1_interface"

# FSM debug
skalp sim --trace-groups "L1_interface,L2_fsm"

# Full debug
skalp sim --trace-groups "L1_interface,L2_fsm,L3_internal,L4_debug"
```

### 2. Assertion-Style Breakpoints

Use breakpoints as assertions:

```skalp
entity AssertionStyle {
    in clk: clock,

    // Protocol assertion
    #[breakpoint(
        is_error = true,
        name = "ASSERT_AXI_HANDSHAKE",
        condition = "awvalid && !awready_timeout",
        message = "AXI write address handshake timeout"
    )]
    signal awready_timeout: bit,

    // Data integrity assertion
    #[breakpoint(
        is_error = true,
        name = "ASSERT_CRC_MATCH",
        condition = "packet_valid && crc_error",
        message = "CRC mismatch on received packet"
    )]
    signal crc_error: bit,

    // Invariant check
    #[breakpoint(
        is_error = true,
        name = "ASSERT_FIFO_INVARIANT",
        condition = "fifo_level > FIFO_DEPTH",
        message = "FIFO level exceeds maximum depth"
    )]
    signal fifo_level: bit[10],
}
```

### 3. Debug Probes for IP Integration

Add debug probes when integrating third-party IP:

```skalp
entity IPDebugWrapper {
    // External IP interface
    in clk: clock,
    in ip_data_in: bit[64],
    out ip_data_out: bit[64],

    // Debug probes on IP boundary
    #[trace(group = "ip_input")]
    #[breakpoint(condition = "ip_data_in == 64'hFFFF_FFFF_FFFF_FFFF")]
    signal probe_in: bit[64],

    #[trace(group = "ip_output")]
    signal probe_out: bit[64],

    #[trace(group = "ip_timing")]
    signal probe_latency_counter: bit[16],
}

impl IPDebugWrapper {
    probe_in = ip_data_in;
    probe_out = ip_data_out;

    // Measure IP latency
    if ip_data_in != 0 && probe_latency_counter == 0 {
        probe_latency_counter = 1;
    } else if probe_latency_counter > 0 && ip_data_out == 0 {
        probe_latency_counter = probe_latency_counter + 1;
    } else {
        probe_latency_counter = 0;
    }
}
```

### 4. Conditional Compilation

Strip debug for production:

```skalp
// debug.sk - included only in debug builds
entity DebugModule {
    #[trace]
    signal internal_state: bit[32],

    #[breakpoint]
    signal error: bit,
}

// Release build excludes debug signals
// skalp build --release  # Strips #[trace] and #[breakpoint]
```

---

## See Also

- [Attributes Reference](../reference/attributes.md)
- [CLI Reference](../reference/cli.md)
- [Testbench Guide](testbench.md)
