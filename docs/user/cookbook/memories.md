# Memory Patterns

**RAM, ROM, FIFO, and memory controller patterns in SKALP.**

---

## Single-Port RAM

### Basic Single-Port RAM

**Problem:** Read/write memory with one port.

**Solution:**
```skalp
entity SinglePortRAM<const WIDTH: nat = 8, const DEPTH: nat = 256> {
    in clk: clock
    in we: bit                      // Write enable
    in addr: nat[clog2(DEPTH)]      // Address
    in din: bit[WIDTH]              // Data in
    out dout: bit[WIDTH]            // Data out
}

impl SinglePortRAM {
    signal memory: [bit[WIDTH]; DEPTH]
    signal dout_reg: bit[WIDTH] = 0

    on(clk.rise) {
        if (we) {
            memory[addr] <= din
        }
        dout_reg <= memory[addr]
    }

    dout = dout_reg
}
```

**Behavior:**
- Write on same cycle as address
- Read data available next cycle (registered output)

---

### Single-Port RAM with Byte Enable

**Problem:** Write individual bytes in a word.

**Solution:**
```skalp
entity ByteEnableRAM<const DEPTH: nat = 256> {
    in clk: clock
    in we: bit[4]                   // Byte write enables
    in addr: nat[clog2(DEPTH)]
    in din: bit[32]                 // 4 bytes
    out dout: bit[32]
}

impl ByteEnableRAM {
    signal memory: [bit[32]; DEPTH]
    signal dout_reg: bit[32] = 0

    on(clk.rise) {
        signal word: bit[32] = memory[addr]

        if (we[0]) {
            word[7:0] = din[7:0]
        }
        if (we[1]) {
            word[15:8] = din[15:8]
        }
        if (we[2]) {
            word[23:16] = din[23:16]
        }
        if (we[3]) {
            word[31:24] = din[31:24]
        }

        if (we != 0) {
            memory[addr] <= word
        }

        dout_reg <= memory[addr]
    }

    dout = dout_reg
}
```

---

## Dual-Port RAM

### True Dual-Port RAM

**Problem:** Two independent read/write ports.

**Solution:**
```skalp
entity DualPortRAM<const WIDTH: nat = 8, const DEPTH: nat = 256> {
    in clk: clock

    // Port A
    in we_a: bit
    in addr_a: nat[clog2(DEPTH)]
    in din_a: bit[WIDTH]
    out dout_a: bit[WIDTH]

    // Port B
    in we_b: bit
    in addr_b: nat[clog2(DEPTH)]
    in din_b: bit[WIDTH]
    out dout_b: bit[WIDTH]
}

impl DualPortRAM {
    signal memory: [bit[WIDTH]; DEPTH]
    signal dout_a_reg: bit[WIDTH] = 0
    signal dout_b_reg: bit[WIDTH] = 0

    on(clk.rise) {
        // Port A
        if (we_a) {
            memory[addr_a] <= din_a
        }
        dout_a_reg <= memory[addr_a]

        // Port B
        if (we_b) {
            memory[addr_b] <= din_b
        }
        dout_b_reg <= memory[addr_b]
    }

    dout_a = dout_a_reg
    dout_b = dout_b_reg
}
```

**Note:** Collision behavior (both ports writing same address) is synthesis-tool dependent.

---

### Simple Dual-Port RAM (1 Read, 1 Write)

**Problem:** One port for write, one for read.

**Solution:**
```skalp
entity SimpleDualPortRAM<const WIDTH: nat = 8, const DEPTH: nat = 256> {
    in clk: clock
    in we: bit
    in waddr: nat[clog2(DEPTH)]
    in din: bit[WIDTH]
    in raddr: nat[clog2(DEPTH)]
    out dout: bit[WIDTH]
}

impl SimpleDualPortRAM {
    signal memory: [bit[WIDTH]; DEPTH]
    signal dout_reg: bit[WIDTH] = 0

    on(clk.rise) {
        if (we) {
            memory[waddr] <= din
        }
        dout_reg <= memory[raddr]
    }

    dout = dout_reg
}
```

**Advantage:** Simpler than true dual-port, often faster/smaller.

---

## ROM

### ROM from Array

**Problem:** Read-only memory initialized with constants.

**Solution:**
```skalp
entity ROM<const WIDTH: nat = 8, const DEPTH: nat = 16> {
    in clk: clock
    in addr: nat[clog2(DEPTH)]
    out dout: bit[WIDTH]
}

impl ROM {
    // Initialize with constants
    signal memory: [bit[WIDTH]; DEPTH] = [
        0x00, 0x11, 0x22, 0x33,
        0x44, 0x55, 0x66, 0x77,
        0x88, 0x99, 0xAA, 0xBB,
        0xCC, 0xDD, 0xEE, 0xFF
    ]

    signal dout_reg: bit[WIDTH] = 0

    on(clk.rise) {
        dout_reg <= memory[addr]
    }

    dout = dout_reg
}
```

---

### Combinational ROM (Async Read)

**Problem:** ROM with immediate output (no clock).

**Solution:**
```skalp
entity AsyncROM<const WIDTH: nat = 8, const DEPTH: nat = 16> {
    in addr: nat[clog2(DEPTH)]
    out dout: bit[WIDTH]
}

impl AsyncROM {
    signal memory: [bit[WIDTH]; DEPTH] = [
        0x00, 0x11, 0x22, 0x33,
        0x44, 0x55, 0x66, 0x77,
        0x88, 0x99, 0xAA, 0xBB,
        0xCC, 0xDD, 0xEE, 0xFF
    ]

    dout = memory[addr]
}
```

**Use case:** Look-up tables, constants.

---

## FIFOs

### Synchronous FIFO

**Problem:** Buffer data between producer and consumer (same clock).

**Solution:**
```skalp
entity SyncFIFO<const WIDTH: nat = 8, const DEPTH: nat = 16> {
    in clk: clock
    in rst: reset
    in wr_en: bit
    in wr_data: bit[WIDTH]
    out full: bit
    in rd_en: bit
    out rd_data: bit[WIDTH]
    out empty: bit
}

impl SyncFIFO {
    signal memory: [bit[WIDTH]; DEPTH]
    signal wr_ptr: nat[clog2(DEPTH)] = 0
    signal rd_ptr: nat[clog2(DEPTH)] = 0
    signal count: nat[clog2(DEPTH+1)] = 0

    empty = (count == 0)
    full = (count == DEPTH)
    rd_data = memory[rd_ptr]

    on(clk.rise) {
        if (rst) {
            wr_ptr <= 0
            rd_ptr <= 0
            count <= 0
        } else {
            signal wr_ok: bit = wr_en && !full
            signal rd_ok: bit = rd_en && !empty

            if (wr_ok) {
                memory[wr_ptr] <= wr_data
                wr_ptr <= (wr_ptr + 1) % DEPTH
            }

            if (rd_ok) {
                rd_ptr <= (rd_ptr + 1) % DEPTH
            }

            if (wr_ok && !rd_ok) {
                count <= count + 1
            } else if (!wr_ok && rd_ok) {
                count <= count - 1
            }
        }
    }
}
```

**Features:**
- Circular buffer
- Full/empty flags
- Read/write in same cycle

---

### FIFO with Almost Full/Empty

**Problem:** Early warning before FIFO fills up.

**Solution:**
```skalp
entity FIFOWithThreshold<const WIDTH: nat = 8, const DEPTH: nat = 16, const THRESHOLD: nat = 12> {
    in clk: clock
    in rst: reset
    in wr_en: bit
    in wr_data: bit[WIDTH]
    out full: bit
    out almost_full: bit
    in rd_en: bit
    out rd_data: bit[WIDTH]
    out empty: bit
    out almost_empty: bit
}

impl FIFOWithThreshold {
    signal memory: [bit[WIDTH]; DEPTH]
    signal wr_ptr: nat[clog2(DEPTH)] = 0
    signal rd_ptr: nat[clog2(DEPTH)] = 0
    signal count: nat[clog2(DEPTH+1)] = 0

    empty = (count == 0)
    full = (count == DEPTH)
    almost_full = (count >= THRESHOLD)
    almost_empty = (count <= (DEPTH - THRESHOLD))
    rd_data = memory[rd_ptr]

    on(clk.rise) {
        if (rst) {
            wr_ptr <= 0
            rd_ptr <= 0
            count <= 0
        } else {
            signal wr_ok: bit = wr_en && !full
            signal rd_ok: bit = rd_en && !empty

            if (wr_ok) {
                memory[wr_ptr] <= wr_data
                wr_ptr <= (wr_ptr + 1) % DEPTH
            }

            if (rd_ok) {
                rd_ptr <= (rd_ptr + 1) % DEPTH
            }

            if (wr_ok && !rd_ok) {
                count <= count + 1
            } else if (!wr_ok && rd_ok) {
                count <= count - 1
            }
        }
    }
}
```

---

### Asynchronous FIFO (CDC-Safe)

**Problem:** Buffer data between different clock domains.

**Solution:**
```skalp
entity AsyncFIFO<const WIDTH: nat = 8, const DEPTH: nat = 16> {
    in wr_clk: clock
    in wr_rst: reset
    in wr_en: bit
    in wr_data: bit[WIDTH]
    out full: bit

    in rd_clk: clock
    in rd_rst: reset
    in rd_en: bit
    out rd_data: bit[WIDTH]
    out empty: bit
}

impl AsyncFIFO {
    signal memory: [bit[WIDTH]; DEPTH]

    // Write domain
    signal wr_ptr: nat[clog2(DEPTH)] = 0
    signal wr_ptr_gray: nat[clog2(DEPTH)] = 0

    // Read domain
    signal rd_ptr: nat[clog2(DEPTH)] = 0
    signal rd_ptr_gray: nat[clog2(DEPTH)] = 0

    // Synchronized pointers
    signal wr_ptr_gray_sync1: nat[clog2(DEPTH)] = 0
    signal wr_ptr_gray_sync2: nat[clog2(DEPTH)] = 0
    signal rd_ptr_gray_sync1: nat[clog2(DEPTH)] = 0
    signal rd_ptr_gray_sync2: nat[clog2(DEPTH)] = 0

    // Binary to Gray conversion
    signal wr_ptr_gray_next: nat[clog2(DEPTH)] = wr_ptr ^ (wr_ptr >> 1)
    signal rd_ptr_gray_next: nat[clog2(DEPTH)] = rd_ptr ^ (rd_ptr >> 1)

    // Write clock domain
    on(wr_clk.rise) {
        if (wr_rst) {
            wr_ptr <= 0
            wr_ptr_gray <= 0
            rd_ptr_gray_sync1 <= 0
            rd_ptr_gray_sync2 <= 0
        } else {
            // Synchronize read pointer
            rd_ptr_gray_sync1 <= rd_ptr_gray
            rd_ptr_gray_sync2 <= rd_ptr_gray_sync1

            if (wr_en && !full) {
                memory[wr_ptr] <= wr_data
                wr_ptr <= (wr_ptr + 1) % DEPTH
                wr_ptr_gray <= wr_ptr_gray_next
            }
        }
    }

    // Read clock domain
    on(rd_clk.rise) {
        if (rd_rst) {
            rd_ptr <= 0
            rd_ptr_gray <= 0
            wr_ptr_gray_sync1 <= 0
            wr_ptr_gray_sync2 <= 0
        } else {
            // Synchronize write pointer
            wr_ptr_gray_sync1 <= wr_ptr_gray
            wr_ptr_gray_sync2 <= wr_ptr_gray_sync1

            if (rd_en && !empty) {
                rd_ptr <= (rd_ptr + 1) % DEPTH
                rd_ptr_gray <= rd_ptr_gray_next
            }
        }
    }

    // Full/empty generation
    full = (wr_ptr_gray == rd_ptr_gray_sync2)
    empty = (rd_ptr_gray == wr_ptr_gray_sync2)
    rd_data = memory[rd_ptr]
}
```

**Key features:**
- Gray code for safe CDC
- 2-FF synchronizers
- Separate clock domains

---

## Register File

### Register File (for CPU)

**Problem:** Array of registers with read/write ports.

**Solution:**
```skalp
entity RegisterFile<const WIDTH: nat = 32, const NUM_REGS: nat = 32> {
    in clk: clock
    in rst: reset

    // Write port
    in we: bit
    in waddr: nat[clog2(NUM_REGS)]
    in wdata: bit[WIDTH]

    // Read port 1
    in raddr1: nat[clog2(NUM_REGS)]
    out rdata1: bit[WIDTH]

    // Read port 2
    in raddr2: nat[clog2(NUM_REGS)]
    out rdata2: bit[WIDTH]
}

impl RegisterFile {
    signal regs: [bit[WIDTH]; NUM_REGS]

    on(clk.rise) {
        if (rst) {
            for i in 0..NUM_REGS {
                regs[i] <= 0
            }
        } else if (we) {
            regs[waddr] <= wdata
        }
    }

    // Combinational reads
    rdata1 = regs[raddr1]
    rdata2 = regs[raddr2]
}
```

**Note:** Register 0 is often hardwired to 0 in RISC architectures.

---

### Register File with R0 = 0

**Problem:** RISC-V style where register 0 always reads 0.

**Solution:**
```skalp
entity RegisterFileRISC<const WIDTH: nat = 32, const NUM_REGS: nat = 32> {
    in clk: clock
    in rst: reset
    in we: bit
    in waddr: nat[clog2(NUM_REGS)]
    in wdata: bit[WIDTH]
    in raddr1: nat[clog2(NUM_REGS)]
    out rdata1: bit[WIDTH]
    in raddr2: nat[clog2(NUM_REGS)]
    out rdata2: bit[WIDTH]
}

impl RegisterFileRISC {
    signal regs: [bit[WIDTH]; NUM_REGS]

    on(clk.rise) {
        if (rst) {
            for i in 0..NUM_REGS {
                regs[i] <= 0
            }
        } else if (we && waddr != 0) {  // Don't write to R0
            regs[waddr] <= wdata
        }
    }

    // R0 always reads 0
    rdata1 = if raddr1 == 0 { 0 } else { regs[raddr1] }
    rdata2 = if raddr2 == 0 { 0 } else { regs[raddr2] }
}
```

---

## Content-Addressable Memory (CAM)

### Simple CAM

**Problem:** Search for data, return address.

**Solution (Small 4-entry example):**
```skalp
entity CAM4<const WIDTH: nat = 8> {
    in clk: clock
    in rst: reset

    // Write port
    in we: bit
    in waddr: bit[2]
    in wdata: bit[WIDTH]

    // Search port
    in search_data: bit[WIDTH]
    out match_found: bit
    out match_addr: bit[2]
}

impl CAM4 {
    signal entries: [bit[WIDTH]; 4]

    on(clk.rise) {
        if (rst) {
            entries[0] <= 0
            entries[1] <= 0
            entries[2] <= 0
            entries[3] <= 0
        } else if (we) {
            entries[waddr] <= wdata
        }
    }

    // Parallel search
    signal match0: bit = if entries[0] == search_data { 1 } else { 0 }
    signal match1: bit = if entries[1] == search_data { 1 } else { 0 }
    signal match2: bit = if entries[2] == search_data { 1 } else { 0 }
    signal match3: bit = if entries[3] == search_data { 1 } else { 0 }

    match_found = match0 | match1 | match2 | match3

    // Priority encoder (first match)
    match_addr = if match0 { 0b00 } else
                 if match1 { 0b01 } else
                 if match2 { 0b10 } else
                 if match3 { 0b11 } else { 0b00 }
}
```

---

## Memory Patterns Summary

| Pattern | Description | Use Case |
|---------|-------------|----------|
| **Single-Port RAM** | 1 read/write port | General purpose memory |
| **Dual-Port RAM** | 2 independent ports | Simultaneous access |
| **Simple Dual-Port** | 1 read + 1 write | Common producer/consumer |
| **ROM** | Read-only | Constants, lookup tables |
| **Sync FIFO** | Single clock buffer | Data buffering |
| **Async FIFO** | Multi-clock buffer | Clock domain crossing |
| **Register File** | Multiple registers | CPU registers |
| **CAM** | Search by content | Lookup tables, caches |

---

## Best Practices

### 1. RAM Inference
Most synthesis tools infer Block RAM when:
- Signal is an array
- Accessed with index
- Written in clocked process

### 2. FIFO Sizing
- Choose DEPTH as power of 2 for efficient modulo
- Add margin: actual usage + latency
- Use almost_full for flow control

### 3. CDC Safety
For async FIFOs:
- Always use Gray code for pointers
- Always use 2-FF synchronizers (minimum)
- Never synchronize multi-bit signals directly

### 4. Memory Initialization
- ROM: Initialize in signal declaration
- RAM: Initialize in reset (if needed)
- FIFO: Always reset pointers to 0

---

## See Also

- [Sequential Patterns](sequential.md) - Registers, counters
- [CDC Patterns](cdc.md) - Clock domain crossing (future)
- [Syntax Reference](../reference/syntax.md) - Language syntax
- [Examples](../examples/) - Complete designs

---

**Key Takeaways:**
- Use `clog2()` for address width calculation
- FIFOs need careful empty/full logic
- Async FIFOs require Gray code + synchronizers
- Register files have combinational reads, clocked writes
- ROM can be synchronous or asynchronous
