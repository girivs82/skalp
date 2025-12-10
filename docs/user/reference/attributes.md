# Skalp Attribute Reference

This document provides a comprehensive reference for all attributes available in the Skalp HDL. Attributes provide compile-time hints and directives for synthesis, simulation, debug, and design intent.

## Table of Contents

- [Debug Attributes](#debug-attributes)
  - [#[breakpoint]](#breakpoint)
  - [#[trace]](#trace)
- [Clock Domain Crossing](#clock-domain-crossing)
  - [#[cdc]](#cdc)
- [Power Intent](#power-intent)
  - [#[retention]](#retention)
  - [#[isolation]](#isolation)
  - [#[level_shift]](#level_shift)
  - [#[pdc]](#pdc)
- [Memory Configuration](#memory-configuration)
  - [#[memory]](#memory)
- [Vendor IP Integration](#vendor-ip-integration)
  - [#[xilinx_ip]](#xilinx_ip)
  - [#[intel_ip]](#intel_ip)
  - [#[vendor_ip]](#vendor_ip)
- [Synthesis Hints](#synthesis-hints)
  - [#[pipeline]](#pipeline)
  - [#[unroll]](#unroll)
  - [#[parallel]](#parallel)

---

## Debug Attributes

### #[breakpoint]

Marks a signal for breakpoint debugging during simulation. When the signal transitions or meets a condition, the simulator pauses for inspection.

**Syntax:**
```skalp
#[breakpoint]
#[breakpoint(condition = "expr")]
#[breakpoint(name = "identifier", message = "text")]
#[breakpoint(is_error = true)]
```

**Parameters:**
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `condition` | string | Signal change | Expression that triggers breakpoint |
| `name` | string | Signal name | Identifier shown in debugger |
| `message` | string | None | Message displayed when triggered |
| `is_error` | bool | false | If true, stops simulation (assertion failure) |

**Examples:**

```skalp
entity DebugExample {
    in clk: clock,
    in reset: bit,
    in data_valid: bit,
    in data: bit[32],
    out result: bit[32],

    // Simple breakpoint - triggers on any change
    #[breakpoint]
    signal error_flag: bit,

    // Conditional breakpoint - triggers when counter exceeds threshold
    #[breakpoint(condition = "counter > 100")]
    signal counter: bit[8],

    // Named breakpoint with descriptive message
    #[breakpoint(name = "FSM_INVALID_STATE", message = "State machine entered invalid state")]
    signal fsm_state: bit[4],

    // Error breakpoint - stops simulation immediately
    #[breakpoint(is_error = true, name = "FATAL_ERROR", message = "Unexpected reset during transfer")]
    signal fatal_condition: bit,
}
```

**Generated SystemVerilog:**
```systemverilog
// Debug Breakpoint: FSM_INVALID_STATE
// Condition: state == 4'hF
// Message: State machine entered invalid state
always @(posedge clk) begin
    if (fsm_state == 4'hF) begin
        $display("BREAKPOINT [FSM_INVALID_STATE]: State machine entered invalid state");
        $stop;
    end
end
```

---

### #[trace]

Marks a signal for automatic waveform export. Traced signals are automatically included in simulation waveforms with optional grouping and formatting.

**Syntax:**
```skalp
#[trace]
#[trace(group = "name")]
#[trace(radix = hex | binary | unsigned | signed | ascii)]
#[trace(display_name = "custom name")]
```

**Parameters:**
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `group` | string | "default" | Group name for waveform organization |
| `radix` | enum | hex | Display format: hex, binary, unsigned, signed, ascii |
| `display_name` | string | Signal name | Custom name in waveform viewer |

**Examples:**

```skalp
entity DataPathDebug {
    in clk: clock,
    in data_in: bit[32],
    out data_out: bit[32],

    // Simple trace - appears in default group
    #[trace]
    signal pipeline_stage1: bit[32],

    // Grouped traces for organized viewing
    #[trace(group = "control")]
    signal state_machine: bit[4],

    #[trace(group = "control")]
    signal valid: bit,

    // Hexadecimal display for addresses
    #[trace(group = "memory", radix = hex)]
    signal address_bus: bit[32],

    // Signed display for arithmetic values
    #[trace(group = "arithmetic", radix = signed, display_name = "ALU Result")]
    signal alu_result: bit[32],

    // ASCII display for character data
    #[trace(radix = ascii)]
    signal char_data: bit[8],
}
```

**Waveform Organization:**
```
default/
  pipeline_stage1
control/
  state_machine
  valid
memory/
  address_bus
arithmetic/
  ALU Result
```

---

## Clock Domain Crossing

### #[cdc]

Configures clock domain crossing synchronization for signals that cross between clock domains. Skalp automatically generates appropriate synchronizer circuits.

**Syntax:**
```skalp
#[cdc]
#[cdc(sync_stages = N)]
#[cdc(from = 'source_domain, to = 'dest_domain)]
#[cdc(cdc_type = two_ff | gray | pulse | handshake | async_fifo)]
```

**Parameters:**
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `sync_stages` | u32 | 2 | Number of synchronizer flip-flops |
| `from` | lifetime | None | Source clock domain |
| `to` | lifetime | None | Destination clock domain |
| `cdc_type` | enum | two_ff | Synchronization strategy |

**CDC Types:**
| Type | Use Case | Width |
|------|----------|-------|
| `two_ff` | Single-bit asynchronous signals | 1 bit |
| `gray` | Multi-bit counters/pointers | Multi-bit |
| `pulse` | Single-cycle pulses | 1 bit |
| `handshake` | Request/acknowledge protocols | 1 bit |
| `async_fifo` | Data buses with flow control | Multi-bit |

**Examples:**

```skalp
entity ClockCrossingExample {
    in clk_fast: clock,
    in clk_slow: clock,
    in async_input: bit,
    in data_bus: bit[8],
    out sync_output: bit,
    out data_out: bit[8],

    // Basic 2-stage synchronizer (default)
    #[cdc]
    signal button_sync: bit,

    // 3-stage synchronizer for high metastability immunity
    #[cdc(sync_stages = 3)]
    signal critical_sync: bit,

    // Explicit domain crossing with lifetime syntax
    #[cdc(from = 'clk_fast, to = 'clk_slow, sync_stages = 2)]
    signal cross_domain: logic<'clk_slow>[1],

    // Gray code synchronizer for multi-bit counter
    #[cdc(cdc_type = gray, sync_stages = 2)]
    signal fifo_write_ptr: bit[4],

    // Pulse synchronizer for single-cycle events
    #[cdc(cdc_type = pulse)]
    signal trigger_pulse: bit,

    // Handshake synchronizer for control signals
    #[cdc(cdc_type = handshake)]
    signal request_sync: bit,

    // Async FIFO for data transfer between domains
    #[cdc(cdc_type = async_fifo)]
    signal data_fifo: bit[32],
}
```

**Generated SystemVerilog (two_ff):**
```systemverilog
// CDC Synchronizer: button_sync (2-stage)
(* ASYNC_REG = "TRUE" *)
reg button_sync_meta;
(* ASYNC_REG = "TRUE" *)
reg button_sync_sync;

always @(posedge clk_slow or posedge reset) begin
    if (reset) begin
        button_sync_meta <= 1'b0;
        button_sync_sync <= 1'b0;
    end else begin
        button_sync_meta <= async_input;
        button_sync_sync <= button_sync_meta;
    end
end
assign button_sync = button_sync_sync;
```

---

## Power Intent

Power intent attributes allow you to specify power management requirements directly in your HDL code, eliminating the need for separate UPF files. These attributes generate both synthesis constraints and optional UPF output.

### #[retention]

Marks a signal as requiring state retention during power-down. The signal's value is preserved when its power domain enters retention mode.

**Syntax:**
```skalp
#[retention]
#[retention(strategy = auto | balloon_latch | shadow_register)]
#[retention(save_signal = "signal_name", restore_signal = "signal_name")]
```

**Parameters:**
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `strategy` | enum | auto | Retention implementation strategy |
| `save_signal` | string | None | Signal to trigger state save |
| `restore_signal` | string | None | Signal to trigger state restore |

**Retention Strategies:**
| Strategy | Description | Area | Power |
|----------|-------------|------|-------|
| `auto` | Compiler chooses optimal strategy | Varies | Varies |
| `balloon_latch` | Uses balloon latch technique | Lower | Higher leakage |
| `shadow_register` | Uses shadow register technique | Higher | Lower leakage |

**Examples:**

```skalp
entity RetentionExample {
    in clk: clock,
    in save: bit,
    in restore: bit,
    in data: bit[32],
    out result: bit[32],

    // Basic retention - compiler chooses implementation
    #[retention]
    signal fsm_state: bit[4],

    // Explicit balloon latch strategy
    #[retention(strategy = balloon_latch)]
    signal counter: bit[16],

    // Shadow register with explicit save/restore control
    #[retention(strategy = shadow_register, save_signal = "save", restore_signal = "restore")]
    signal critical_config: bit[32],

    // Multiple retained signals for processor context
    #[retention]
    signal register_file: bit[32][16],  // 16 x 32-bit registers

    #[retention]
    signal program_counter: bit[32],

    #[retention]
    signal status_flags: bit[8],
}
```

**Generated SystemVerilog:**
```systemverilog
// Power Intent: fsm_state
(* RETAIN = "TRUE" *)
(* preserve = "true" *)
(* DONT_TOUCH = "TRUE" *)
// Retention strategy: auto
reg [3:0] fsm_state;
```

---

### #[isolation]

Specifies isolation cell requirements for signals crossing power domain boundaries. When the source domain is powered down, the signal is clamped to a safe value.

**Syntax:**
```skalp
#[isolation]
#[isolation(clamp = low | high | latch)]
#[isolation(enable = "signal_name")]
#[isolation(active_high = true | false)]
```

**Parameters:**
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `clamp` | enum | low | Value when isolated: low (0), high (1), latch (hold) |
| `enable` | string | None | Signal controlling isolation |
| `active_high` | bool | true | Polarity of enable signal |

**Clamp Values:**
| Clamp | Description | Use Case |
|-------|-------------|----------|
| `low` | Clamp to all zeros | Default, safe for most logic |
| `high` | Clamp to all ones | Active-low signals, resets |
| `latch` | Hold last value | Avoid glitches on data buses |

**Examples:**

```skalp
entity IsolationExample {
    in clk: clock,
    in iso_en: bit,
    in core_data: bit[32],
    out io_data: bit[32],
    out status: bit[8],

    // Basic isolation - clamps to zero
    #[isolation]
    signal internal_bus: bit[32],

    // Isolation with explicit clamp value
    #[isolation(clamp = low)]
    signal data_valid: bit,

    // Clamp high for active-low signal
    #[isolation(clamp = high)]
    signal reset_n: bit,

    // Latch mode to avoid glitches
    #[isolation(clamp = latch)]
    signal config_data: bit[32],

    // Isolation with enable signal
    #[isolation(clamp = low, enable = "iso_en")]
    signal controlled_output: bit[32],

    // Active-low isolation enable
    #[isolation(clamp = low, enable = "iso_en_n", active_high = false)]
    signal alt_output: bit[16],
}
```

**Generated SystemVerilog:**
```systemverilog
// Power Intent: controlled_output
// Isolation: clamp=low (0)
// Isolation enable: iso_en (active-high)
wire [31:0] controlled_output;
(* DONT_TOUCH = "TRUE" *)
wire [31:0] controlled_output_isolated;
assign controlled_output_isolated = iso_en ? 32'b0 : controlled_output;
```

---

### #[level_shift]

Specifies level shifter requirements for signals crossing voltage domain boundaries.

**Syntax:**
```skalp
#[level_shift]
#[level_shift(from = "domain", to = "domain")]
#[level_shift(shifter_type = auto | low_to_high | high_to_low)]
```

**Parameters:**
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `from` | string | None | Source voltage domain |
| `to` | string | None | Destination voltage domain |
| `shifter_type` | enum | auto | Direction of voltage shift |

**Examples:**

```skalp
entity LevelShiftExample {
    in clk: clock,
    in core_data: bit[16],
    out io_data: bit[16],

    // Basic level shifter - compiler infers direction
    #[level_shift]
    signal voltage_crossing: bit[16],

    // Explicit domain specification
    #[level_shift(from = "VDD_CORE", to = "VDD_IO")]
    signal core_to_io: bit[32],

    // Low-to-high level shifter (0.9V to 1.8V)
    #[level_shift(from = "low_voltage", to = "high_voltage", shifter_type = low_to_high)]
    signal up_shifted: bit[8],

    // High-to-low level shifter (1.8V to 0.9V)
    #[level_shift(from = "high_voltage", to = "low_voltage", shifter_type = high_to_low)]
    signal down_shifted: bit[8],
}
```

**Generated SystemVerilog:**
```systemverilog
// Power Intent: core_to_io
// Level shifter: type=low_to_high
// Level shift from domain: VDD_CORE
// Level shift to domain: VDD_IO
(* LEVEL_SHIFTER = "TRUE" *)
wire [31:0] core_to_io;
```

---

### #[pdc]

Power Domain Crossing attribute combines isolation and level shifting for signals crossing power domain boundaries.

**Syntax:**
```skalp
#[pdc(from = 'domain1, to = 'domain2)]
#[pdc(from = 'domain1, to = 'domain2, isolation = clamp_low | clamp_high)]
```

**Parameters:**
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `from` | lifetime | Required | Source power domain |
| `to` | lifetime | Required | Destination power domain |
| `isolation` | enum | clamp_low | Isolation strategy |

**Examples:**

```skalp
entity PowerDomainCrossing {
    in clk: clock,
    in data: bit[32],
    out result: bit[32],

    // Cross from core domain to IO domain with low clamp
    #[pdc(from = 'core, to = 'io, isolation = clamp_low)]
    signal core_to_io_data: bit[32],

    // Cross with high clamp for active-low signals
    #[pdc(from = 'core, to = 'io, isolation = clamp_high)]
    signal core_to_io_reset_n: bit,
}
```

---

## Memory Configuration

### #[memory]

Configures memory inference and implementation hints for array signals.

**Syntax:**
```skalp
#[memory(depth = N)]
#[memory(depth = N, width = M)]
#[memory(depth = N, style = auto | block | distributed | ultra | register)]
#[memory(depth = N, ports = P)]
#[memory(depth = N, read_latency = L)]
#[memory(depth = N, read_only = true)]
```

**Parameters:**
| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `depth` | u32 | Required | Number of entries |
| `width` | u32 | Element width | Data width in bits |
| `style` | enum | auto | Memory implementation style |
| `ports` | u32 | 1 | Number of ports |
| `read_latency` | u32 | 1 | Read latency in cycles |
| `read_only` | bool | false | Whether memory is ROM |

**Memory Styles:**
| Style | Description | Use Case |
|-------|-------------|----------|
| `auto` | Compiler chooses | General purpose |
| `block` | Block RAM (BRAM) | Large memories |
| `distributed` | LUT-based RAM | Small, fast memories |
| `ultra` | UltraRAM (Xilinx) | Very large memories |
| `register` | Register file | Tiny, multi-read |

**Examples:**

```skalp
entity MemoryExamples {
    in clk: clock,
    in addr: bit[10],
    in wr_data: bit[32],
    in wr_en: bit,
    out rd_data: bit[32],

    // Basic BRAM - 1024 x 32 bits
    #[memory(depth = 1024, width = 32)]
    signal main_memory: bit[32][1024],

    // Distributed RAM for small lookup table
    #[memory(depth = 64, width = 16, style = distributed)]
    signal lookup_table: bit[16][64],

    // UltraRAM for large buffers (Xilinx UltraScale+)
    #[memory(depth = 65536, width = 72, style = ultra)]
    signal frame_buffer: bit[72][65536],

    // Dual-port BRAM
    #[memory(depth = 512, width = 64, ports = 2)]
    signal dual_port_mem: bit[64][512],

    // ROM with 2-cycle read latency
    #[memory(depth = 256, width = 8, read_only = true, read_latency = 2)]
    signal coefficient_rom: bit[8][256],

    // Register file for CPU
    #[memory(depth = 32, width = 64, style = register)]
    signal register_file: bit[64][32],
}
```

**Generated SystemVerilog:**
```systemverilog
// Memory: main_memory
// Style: block, Depth: 1024, Width: 32
(* ram_style = "block" *)
reg [31:0] main_memory [0:1023];

// Memory: lookup_table
// Style: distributed, Depth: 64, Width: 16
(* ram_style = "distributed" *)
reg [15:0] lookup_table [0:63];
```

---

## Vendor IP Integration

### #[xilinx_ip]

Wraps Xilinx IP cores for seamless integration.

**Syntax:**
```skalp
#[xilinx_ip("ip_name")]
#[xilinx_ip(name = "ip_name", library = "lib")]
#[xilinx_ip(name = "ip_name", version = "x.y")]
```

**Examples:**

```skalp
// XPM FIFO with parameters
#[xilinx_ip(name = "xpm_fifo_sync", library = "xpm")]
entity SyncFifo<DEPTH: 512, WIDTH: 32> {
    in clk: clock,
    in rst: bit,
    in wr_en: bit,
    in din: bit[WIDTH],
    in rd_en: bit,
    out dout: bit[WIDTH],
    out full: bit,
    out empty: bit,
}

// XPM Memory
#[xilinx_ip(name = "xpm_memory_spram", library = "xpm", version = "3.0")]
entity SinglePortRam<DEPTH: 1024, WIDTH: 32> {
    in clka: clock,
    in ena: bit,
    in wea: bit,
    in addra: bit[$clog2(DEPTH)],
    in dina: bit[WIDTH],
    out douta: bit[WIDTH],
}
```

### #[intel_ip]

Wraps Intel/Altera IP cores.

```skalp
#[intel_ip("altsyncram")]
entity DualPortRam<DEPTH: 512, WIDTH: 32> {
    in clock: clock,
    in address_a: bit[$clog2(DEPTH)],
    in address_b: bit[$clog2(DEPTH)],
    in data_a: bit[WIDTH],
    in wren_a: bit,
    out q_a: bit[WIDTH],
    out q_b: bit[WIDTH],
}
```

### #[vendor_ip]

Generic vendor IP wrapper.

```skalp
#[vendor_ip(name = "custom_pll", vendor = generic, black_box = true)]
entity CustomPLL {
    in ref_clk: clock,
    out clk_100mhz: clock,
    out clk_200mhz: clock,
    out locked: bit,
}
```

---

## Synthesis Hints

### #[pipeline]

Adds pipeline stages for timing optimization.

```skalp
#[pipeline(stages = 3)]
entity PipelinedMultiplier<WIDTH: 32> {
    in clk: clock,
    in a: bit[WIDTH],
    in b: bit[WIDTH],
    out result: bit[WIDTH * 2],
}

#[pipeline(stages = 4, target_freq = 200_000_000)]
entity HighSpeedAdder {
    in clk: clock,
    in a: bit[64],
    in b: bit[64],
    out sum: bit[64],
}
```

### #[unroll]

Controls loop unrolling.

```skalp
impl BitwiseOps {
    // Fully unroll - creates parallel hardware
    #[unroll]
    for i in 0..8 {
        result[i] = a[i] & b[i];
    }

    // Partial unroll - 4 parallel iterations
    #[unroll(4)]
    for i in 0..16 {
        data[i] = input[i] + offset;
    }
}
```

### #[parallel]

Hints for parallel implementation.

```skalp
#[parallel]
impl ParallelProcessor {
    // All operations execute in parallel
    out1 = process_a(in1);
    out2 = process_b(in2);
    out3 = process_c(in3);
}
```

---

## Quick Reference Table

| Attribute | Target | Purpose |
|-----------|--------|---------|
| `#[breakpoint]` | signal | Debug breakpoint |
| `#[trace]` | signal | Waveform tracing |
| `#[cdc]` | signal | Clock domain crossing |
| `#[retention]` | signal | Power retention |
| `#[isolation]` | signal | Power isolation |
| `#[level_shift]` | signal | Voltage level shifting |
| `#[pdc]` | signal | Power domain crossing |
| `#[memory]` | signal | Memory configuration |
| `#[xilinx_ip]` | entity | Xilinx IP wrapper |
| `#[intel_ip]` | entity | Intel IP wrapper |
| `#[vendor_ip]` | entity | Generic vendor IP |
| `#[pipeline]` | entity/impl | Pipeline stages |
| `#[unroll]` | for loop | Loop unrolling |
| `#[parallel]` | impl | Parallel execution |

---

## See Also

- [CDC Patterns Guide](../guides/clock-domain-crossing.md)
- [Power Management Guide](../guides/power-intent.md)
- [Memory Synthesis Guide](../guides/memory-synthesis.md)
- [Debug and Simulation Guide](../guides/debug-simulation.md)
