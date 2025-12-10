# Memory Synthesis Guide

This guide covers memory inference in Skalp, including configuration options, memory styles, and best practices for efficient memory synthesis.

## Table of Contents

- [Overview](#overview)
- [Memory Styles](#memory-styles)
- [Configuration Options](#configuration-options)
- [Inference Patterns](#inference-patterns)
- [Dual-Port Memories](#dual-port-memories)
- [ROM and Lookup Tables](#rom-and-lookup-tables)
- [Vendor-Specific Features](#vendor-specific-features)
- [Best Practices](#best-practices)

---

## Overview

Skalp automatically infers memory from array signals, but you can guide synthesis with the `#[memory]` attribute to:

- Specify memory style (Block RAM, Distributed RAM, etc.)
- Configure depth and width
- Set port configurations
- Control read latency
- Mark memories as read-only (ROM)

---

## Memory Styles

### Auto (Default)

Let the synthesizer choose based on size and access patterns:

```skalp
entity AutoMemory {
    in clk: clock,
    in addr: bit[8],
    in din: bit[32],
    in we: bit,
    out dout: bit[32],

    // Synthesizer decides: BRAM, distributed, or registers
    #[memory(depth = 256)]
    signal mem: bit[32][256],
}
```

### Block RAM (BRAM)

For large memories with synchronous read:

```skalp
entity BlockRamExample {
    in clk: clock,
    in addr: bit[10],
    in din: bit[64],
    in we: bit,
    out dout: bit[64],

    // Force Block RAM inference
    #[memory(depth = 1024, width = 64, style = block)]
    signal bram: bit[64][1024],
}

impl BlockRamExample {
    // Synchronous read (1 cycle latency)
    if we {
        bram[addr] = din;
    }
    dout = bram[addr];
}
```

**Characteristics:**
- Dedicated memory blocks on FPGA
- 1-2 cycle read latency
- High density (18Kb or 36Kb blocks)
- Best for > 64 entries

**Generated SystemVerilog:**
```systemverilog
// Memory: bram
// Style: block, Depth: 1024, Width: 64
(* ram_style = "block" *)
reg [63:0] bram [0:1023];

always @(posedge clk) begin
    if (we)
        bram[addr] <= din;
    dout <= bram[addr];
end
```

### Distributed RAM

For small, fast memories using LUTs:

```skalp
entity DistributedRamExample {
    in clk: clock,
    in addr: bit[6],
    in din: bit[16],
    in we: bit,
    out dout: bit[16],

    // Force Distributed RAM (LUT-based)
    #[memory(depth = 64, width = 16, style = distributed)]
    signal lutram: bit[16][64],
}

impl DistributedRamExample {
    // Asynchronous read (combinational)
    if we {
        lutram[addr] = din;
    }
    dout = lutram[addr];  // No latency
}
```

**Characteristics:**
- Uses FPGA LUTs
- Asynchronous (combinational) read
- Synchronous write
- Best for < 64 entries

### UltraRAM (Xilinx UltraScale+)

For very large memories:

```skalp
entity UltraRamExample {
    in clk: clock,
    in addr: bit[16],
    in din: bit[72],
    in we: bit,
    out dout: bit[72],

    // UltraRAM: 288Kb blocks
    #[memory(depth = 65536, width = 72, style = ultra)]
    signal uram: bit[72][65536],
}

impl UltraRamExample {
    // 2+ cycle read latency
    if we {
        uram[addr] = din;
    }
    dout = uram[addr];
}
```

**Characteristics:**
- 288Kb dedicated blocks (UltraScale+)
- 72-bit native width
- Higher latency than BRAM
- Best for > 128Kb memories

### Register File

For small, multi-read memories:

```skalp
entity RegisterFileExample {
    in clk: clock,
    in rs1: bit[5],
    in rs2: bit[5],
    in rd: bit[5],
    in wdata: bit[64],
    in we: bit,
    out rdata1: bit[64],
    out rdata2: bit[64],

    // Register file: 32 x 64-bit
    #[memory(depth = 32, width = 64, style = register)]
    signal regfile: bit[64][32],
}

impl RegisterFileExample {
    // Multiple simultaneous reads
    rdata1 = regfile[rs1];
    rdata2 = regfile[rs2];

    // Single write
    if we {
        regfile[rd] = wdata;
    }
}
```

**Characteristics:**
- Flip-flop based
- Multiple read ports
- Zero read latency
- Best for < 32 entries with many reads

---

## Configuration Options

### Depth and Width

```skalp
// Explicit dimensions
#[memory(depth = 1024, width = 32)]
signal mem: bit[32][1024],

// Width inferred from signal type
#[memory(depth = 512)]
signal mem2: bit[64][512],  // Width = 64

// Both inferred from signal
#[memory]
signal mem3: bit[128][256],  // Depth = 256, Width = 128
```

### Read Latency

Control pipeline stages for timing closure:

```skalp
entity LatencyControl {
    in clk: clock,
    in addr: bit[12],
    out dout: bit[32],

    // 1-cycle latency (default for BRAM)
    #[memory(depth = 4096, read_latency = 1)]
    signal mem_fast: bit[32][4096],

    // 2-cycle latency (better timing)
    #[memory(depth = 4096, read_latency = 2)]
    signal mem_piped: bit[32][4096],

    // 3-cycle latency (aggressive timing)
    #[memory(depth = 4096, read_latency = 3)]
    signal mem_slow: bit[32][4096],
}
```

### Port Configuration

```skalp
entity MultiPort {
    in clk: clock,

    // Single port (default)
    #[memory(depth = 256, ports = 1)]
    signal sp_mem: bit[32][256],

    // True dual-port
    #[memory(depth = 256, ports = 2)]
    signal tdp_mem: bit[32][256],

    // Simple dual-port (1 read, 1 write)
    #[memory(depth = 256, ports = 2, style = block)]
    signal sdp_mem: bit[32][256],
}
```

---

## Inference Patterns

### Read-First Mode

```skalp
impl ReadFirst {
    // Read happens before write
    dout = mem[addr];
    if we {
        mem[addr] = din;
    }
}
```

### Write-First Mode

```skalp
impl WriteFirst {
    // Write happens before read
    if we {
        mem[addr] = din;
        dout = din;  // Forward written data
    } else {
        dout = mem[addr];
    }
}
```

### No-Change Mode

```skalp
impl NoChange {
    // Output unchanged during write
    if we {
        mem[addr] = din;
        // dout retains previous value
    } else {
        dout = mem[addr];
    }
}
```

---

## Dual-Port Memories

### Simple Dual-Port (SDP)

One read port, one write port:

```skalp
entity SimpleDualPort {
    in clk: clock,

    // Write port
    in wr_addr: bit[10],
    in wr_data: bit[32],
    in wr_en: bit,

    // Read port
    in rd_addr: bit[10],
    out rd_data: bit[32],

    #[memory(depth = 1024, ports = 2, style = block)]
    signal sdp_mem: bit[32][1024],
}

impl SimpleDualPort {
    // Independent read and write
    if wr_en {
        sdp_mem[wr_addr] = wr_data;
    }
    rd_data = sdp_mem[rd_addr];
}
```

### True Dual-Port (TDP)

Two independent read/write ports:

```skalp
entity TrueDualPort {
    in clk: clock,

    // Port A
    in addr_a: bit[10],
    in din_a: bit[32],
    in we_a: bit,
    out dout_a: bit[32],

    // Port B
    in addr_b: bit[10],
    in din_b: bit[32],
    in we_b: bit,
    out dout_b: bit[32],

    #[memory(depth = 1024, ports = 2, style = block)]
    signal tdp_mem: bit[32][1024],
}

impl TrueDualPort {
    // Port A: read/write
    if we_a {
        tdp_mem[addr_a] = din_a;
    }
    dout_a = tdp_mem[addr_a];

    // Port B: read/write
    if we_b {
        tdp_mem[addr_b] = din_b;
    }
    dout_b = tdp_mem[addr_b];
}
```

### Different Port Widths

```skalp
entity AsymmetricPorts {
    in clk: clock,

    // Wide write port (64-bit)
    in wr_addr: bit[9],
    in wr_data: bit[64],
    in wr_en: bit,

    // Narrow read port (16-bit)
    in rd_addr: bit[11],
    out rd_data: bit[16],

    // Asymmetric: 512 x 64-bit = 2048 x 16-bit
    #[memory(depth = 512, width = 64, ports = 2)]
    signal asym_mem: bit[64][512],
}

impl AsymmetricPorts {
    // Write 64 bits at once
    if wr_en {
        asym_mem[wr_addr] = wr_data;
    }

    // Read 16 bits (address selects which quarter)
    let word_addr = rd_addr[10:2];
    let byte_sel = rd_addr[1:0];
    rd_data = asym_mem[word_addr][byte_sel * 16 +: 16];
}
```

---

## ROM and Lookup Tables

### Read-Only Memory

```skalp
entity SineRom {
    in clk: clock,
    in angle: bit[8],
    out sine_value: bit[16],

    // ROM: Initialized, never written
    #[memory(depth = 256, read_only = true)]
    signal sine_table: bit[16][256] = [
        16'h0000, 16'h0324, 16'h0647, 16'h096A,
        // ... 252 more entries
    ],
}

impl SineRom {
    sine_value = sine_table[angle];
}
```

### Coefficient Storage

```skalp
entity FirFilter {
    in clk: clock,
    in sample: bit[16],
    out filtered: bit[32],

    // Filter coefficients (ROM)
    #[memory(depth = 32, read_only = true)]
    signal coefficients: bit[16][32] = [
        16'h0001, 16'h0003, 16'h0007, 16'h000F,
        // ... symmetric FIR coefficients
    ],

    // Delay line (RAM)
    #[memory(depth = 32, style = distributed)]
    signal delay_line: bit[16][32],

    signal tap_index: bit[5],
}

impl FirFilter {
    // Shift delay line
    delay_line[tap_index] = sample;
    tap_index = tap_index + 1;

    // MAC with ROM coefficients
    let acc: bit[32] = 0;
    for i in 0..32 {
        acc = acc + (delay_line[i] as bit[32]) * (coefficients[i] as bit[32]);
    }
    filtered = acc;
}
```

### Lookup Table with Initialization

```skalp
entity CrcLut {
    in clk: clock,
    in byte_in: bit[8],
    out crc_partial: bit[32],

    // CRC-32 lookup table
    #[memory(depth = 256, read_only = true, style = distributed)]
    signal crc_table: bit[32][256] = generate_crc32_table(),
}

impl CrcLut {
    crc_partial = crc_table[byte_in];
}
```

---

## Vendor-Specific Features

### Xilinx BRAM

```skalp
// Xilinx 36Kb BRAM (512 x 72 or 1024 x 36)
#[memory(depth = 1024, width = 36, style = block)]
#[xilinx_bram(cascade = "none", write_mode = "read_first")]
signal xilinx_bram: bit[36][1024],

// Xilinx ECC-protected BRAM
#[memory(depth = 512, width = 72, style = block)]
#[xilinx_bram(ecc = true)]
signal ecc_bram: bit[72][512],
```

### Intel M20K

```skalp
// Intel M20K block (512 x 40)
#[memory(depth = 512, width = 40, style = block)]
#[intel_m20k(mixed_width = true)]
signal intel_mem: bit[40][512],
```

### Lattice EBR

```skalp
// Lattice EBR (512 x 36)
#[memory(depth = 512, width = 36, style = block)]
signal lattice_ebr: bit[36][512],
```

---

## Best Practices

### 1. Match Memory to Access Pattern

| Access Pattern | Recommended Style |
|----------------|-------------------|
| Sequential read/write | Block RAM |
| Random access, low latency | Distributed |
| Large storage | UltraRAM |
| Multi-read register file | Register |
| Lookup table | Distributed ROM |

### 2. Consider Timing

```skalp
// BAD: Large distributed RAM with long combinational path
#[memory(depth = 512, style = distributed)]
signal bad_choice: bit[128][512],  // Slow!

// GOOD: Block RAM with pipelining
#[memory(depth = 512, style = block, read_latency = 2)]
signal good_choice: bit[128][512],
```

### 3. Align to Physical Resources

```skalp
// BAD: Wastes half a BRAM (18Kb block)
#[memory(depth = 500, width = 18)]  // 9Kb used of 18Kb

// GOOD: Use full BRAM capacity
#[memory(depth = 512, width = 36)]  // 18Kb fully used
#[memory(depth = 1024, width = 18)] // 18Kb fully used
```

### 4. Initialize Memories When Possible

```skalp
// ROM with initialization (synthesizes to init values)
#[memory(read_only = true)]
signal lut: bit[16][64] = [...],

// RAM with reset values (uses BRAM init)
#[memory(style = block)]
signal ram: bit[32][256] = { default: 0 },
```

### 5. Use Appropriate Port Configurations

```skalp
// Single-port when only one access needed
#[memory(depth = 256, ports = 1)]
signal cache: bit[64][256],

// Dual-port only when truly needed
#[memory(depth = 256, ports = 2)]
signal shared_buffer: bit[64][256],
```

### 6. Document Memory Assumptions

```skalp
entity DocumentedMemory {
    // MEMORY ASSUMPTIONS:
    // - Read latency: 2 cycles
    // - No simultaneous read/write to same address
    // - Initialized to zero on power-up

    #[memory(depth = 4096, style = block, read_latency = 2)]
    signal data_mem: bit[128][4096],
}
```

---

## Memory Size Guidelines

| Style | Min Depth | Max Depth | Typical Width | Use Case |
|-------|-----------|-----------|---------------|----------|
| Register | 2 | 32 | Any | Register files |
| Distributed | 16 | 256 | 1-64 | Small tables |
| Block | 64 | 64K | 1-72 | General storage |
| Ultra | 4K | 256K | 72 | Large buffers |

---

## See Also

- [Attributes Reference](../reference/attributes.md)
- [CDC Guide](clock-domain-crossing.md) - For async FIFOs
- [Vendor IP Integration](../reference/attributes.md#vendor-ip-integration)
