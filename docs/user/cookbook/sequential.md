# Sequential Logic Patterns

**Common sequential circuits with copy-paste SKALP code.**

Sequential logic uses registers (flip-flops) and updates on clock edges.

---

## Basic Registers

### Simple Register

**Problem:** Store a value on clock edge.

**Solution:**
```skalp
entity Register<const WIDTH: nat = 8> {
    in clk: clock
    in rst: reset
    in data_in: bit[WIDTH]
    out data_out: bit[WIDTH]
}

impl Register {
    signal reg: bit[WIDTH] = 0

    on(clk.rise) {
        if (rst) {
            reg <= 0
        } else {
            reg <= data_in
        }
    }

    data_out = reg
}
```

---

### Register with Enable

**Problem:** Only update register when enabled.

**Solution:**
```skalp
entity RegisterEn<const WIDTH: nat = 8> {
    in clk: clock
    in rst: reset
    in enable: bit
    in data_in: bit[WIDTH]
    out data_out: bit[WIDTH]
}

impl RegisterEn {
    signal reg: bit[WIDTH] = 0

    on(clk.rise) {
        if (rst) {
            reg <= 0
        } else if (enable) {
            reg <= data_in
        }
    }

    data_out = reg
}
```

---

### Register with Load

**Problem:** Register with separate load control and load value.

**Solution:**
```skalp
entity RegisterLoad<const WIDTH: nat = 8> {
    in clk: clock
    in rst: reset
    in load: bit
    in load_value: bit[WIDTH]
    in data_in: bit[WIDTH]
    out data_out: bit[WIDTH]
}

impl RegisterLoad {
    signal reg: bit[WIDTH] = 0

    on(clk.rise) {
        if (rst) {
            reg <= 0
        } else if (load) {
            reg <= load_value
        } else {
            reg <= data_in
        }
    }

    data_out = reg
}
```

---

## Counters

### Simple Up Counter

**Problem:** Count up from 0.

**Solution:**
```skalp
entity Counter<const WIDTH: nat = 8> {
    in clk: clock
    in rst: reset
    out count: bit[WIDTH]
}

impl Counter {
    signal count_reg: bit[WIDTH] = 0

    on(clk.rise) {
        if (rst) {
            count_reg <= 0
        } else {
            count_reg <= count_reg + 1
        }
    }

    count = count_reg
}
```

---

### Counter with Enable

**Problem:** Count only when enabled.

**Solution:**
```skalp
entity CounterEn<const WIDTH: nat = 8> {
    in clk: clock
    in rst: reset
    in enable: bit
    out count: bit[WIDTH]
}

impl CounterEn {
    signal count_reg: bit[WIDTH] = 0

    on(clk.rise) {
        if (rst) {
            count_reg <= 0
        } else if (enable) {
            count_reg <= count_reg + 1
        }
    }

    count = count_reg
}
```

---

### Up/Down Counter

**Problem:** Count up or down based on control signal.

**Solution:**
```skalp
entity UpDownCounter<const WIDTH: nat = 8> {
    in clk: clock
    in rst: reset
    in enable: bit
    in up: bit          // 1 = count up, 0 = count down
    out count: bit[WIDTH]
}

impl UpDownCounter {
    signal count_reg: bit[WIDTH] = 0

    on(clk.rise) {
        if (rst) {
            count_reg <= 0
        } else if (enable) {
            if (up) {
                count_reg <= count_reg + 1
            } else {
                count_reg <= count_reg - 1
            }
        }
    }

    count = count_reg
}
```

---

### Counter with Load

**Problem:** Counter that can be loaded with a specific value.

**Solution:**
```skalp
entity CounterLoad<const WIDTH: nat = 8> {
    in clk: clock
    in rst: reset
    in load: bit
    in load_value: bit[WIDTH]
    in enable: bit
    out count: bit[WIDTH]
}

impl CounterLoad {
    signal count_reg: bit[WIDTH] = 0

    on(clk.rise) {
        if (rst) {
            count_reg <= 0
        } else if (load) {
            count_reg <= load_value
        } else if (enable) {
            count_reg <= count_reg + 1
        }
    }

    count = count_reg
}
```

---

### Modulo Counter

**Problem:** Count from 0 to N-1, then wrap.

**Solution:**
```skalp
entity ModuloCounter<const WIDTH: nat = 8, const MODULO: nat = 10> {
    in clk: clock
    in rst: reset
    in enable: bit
    out count: bit[WIDTH]
    out rollover: bit
}

impl ModuloCounter {
    signal count_reg: bit[WIDTH] = 0

    on(clk.rise) {
        if (rst) {
            count_reg <= 0
        } else if (enable) {
            if (count_reg == MODULO - 1) {
                count_reg <= 0
            } else {
                count_reg <= count_reg + 1
            }
        }
    }

    count = count_reg
    rollover = if (count_reg == MODULO - 1 && enable) { 1 } else { 0 }
}
```

---

## Shift Registers

### Shift Register (Left)

**Problem:** Shift data left (towards MSB).

**Solution:**
```skalp
entity ShiftRegisterLeft<const WIDTH: nat = 8> {
    in clk: clock
    in rst: reset
    in shift_in: bit
    in enable: bit
    out shift_out: bit
    out data: bit[WIDTH]
}

impl ShiftRegisterLeft {
    signal reg: bit[WIDTH] = 0

    on(clk.rise) {
        if (rst) {
            reg <= 0
        } else if (enable) {
            reg <= (reg << 1) | shift_in
        }
    }

    shift_out = reg[WIDTH-1]
    data = reg
}
```

---

### Shift Register (Right)

**Problem:** Shift data right (towards LSB).

**Solution:**
```skalp
entity ShiftRegisterRight<const WIDTH: nat = 8> {
    in clk: clock
    in rst: reset
    in shift_in: bit
    in enable: bit
    out shift_out: bit
    out data: bit[WIDTH]
}

impl ShiftRegisterRight {
    signal reg: bit[WIDTH] = 0

    on(clk.rise) {
        if (rst) {
            reg <= 0
        } else if (enable) {
            reg <= (shift_in << (WIDTH - 1)) | (reg >> 1)
        }
    }

    shift_out = reg[0]
    data = reg
}
```

---

### Parallel Load Shift Register

**Problem:** Shift register that can be loaded in parallel.

**Solution:**
```skalp
entity ParallelShiftRegister<const WIDTH: nat = 8> {
    in clk: clock
    in rst: reset
    in load: bit
    in load_data: bit[WIDTH]
    in shift: bit
    in shift_in: bit
    out data: bit[WIDTH]
}

impl ParallelShiftRegister {
    signal reg: bit[WIDTH] = 0

    on(clk.rise) {
        if (rst) {
            reg <= 0
        } else if (load) {
            reg <= load_data
        } else if (shift) {
            reg <= (reg << 1) | shift_in
        }
    }

    data = reg
}
```

---

### LFSR (Linear Feedback Shift Register)

**Problem:** Generate pseudo-random sequence.

**Solution (8-bit, polynomial x^8 + x^6 + x^5 + x^4 + 1):**
```skalp
entity LFSR8 {
    in clk: clock
    in rst: reset
    in enable: bit
    out value: bit[8]
}

impl LFSR8 {
    signal lfsr: bit[8] = 1  // Non-zero seed

    on(clk.rise) {
        if (rst) {
            lfsr <= 1
        } else if (enable) {
            signal feedback: bit = lfsr[7] ^ lfsr[5] ^ lfsr[4] ^ lfsr[3]
            lfsr <= (lfsr << 1) | feedback
        }
    }

    value = lfsr
}
```

---

## Edge Detectors

### Rising Edge Detector

**Problem:** Detect 0→1 transition.

**Solution:**
```skalp
entity RisingEdgeDetector {
    in clk: clock
    in rst: reset
    in signal_in: bit
    out edge_detected: bit
}

impl RisingEdgeDetector {
    signal prev: bit = 0

    on(clk.rise) {
        if (rst) {
            prev <= 0
        } else {
            prev <= signal_in
        }
    }

    edge_detected = if (!prev && signal_in) { 1 } else { 0 }
}
```

---

### Falling Edge Detector

**Problem:** Detect 1→0 transition.

**Solution:**
```skalp
entity FallingEdgeDetector {
    in clk: clock
    in rst: reset
    in signal_in: bit
    out edge_detected: bit
}

impl FallingEdgeDetector {
    signal prev: bit = 0

    on(clk.rise) {
        if (rst) {
            prev <= 0
        } else {
            prev <= signal_in
        }
    }

    edge_detected = if (prev && !signal_in) { 1 } else { 0 }
}
```

---

### Any Edge Detector

**Problem:** Detect any transition (0→1 or 1→0).

**Solution:**
```skalp
entity EdgeDetector {
    in clk: clock
    in rst: reset
    in signal_in: bit
    out edge_detected: bit
}

impl EdgeDetector {
    signal prev: bit = 0

    on(clk.rise) {
        if (rst) {
            prev <= 0
        } else {
            prev <= signal_in
        }
    }

    edge_detected = if (prev != signal_in) { 1 } else { 0 }
}
```

---

## Debouncer

### Button Debouncer

**Problem:** Filter mechanical button bounce.

**Solution:**
```skalp
entity Debouncer<const COUNTER_WIDTH: nat = 16> {
    in clk: clock
    in rst: reset
    in button_in: bit
    out button_out: bit
}

impl Debouncer {
    signal counter: nat[COUNTER_WIDTH] = 0
    signal button_state: bit = 0

    on(clk.rise) {
        if (rst) {
            counter <= 0
            button_state <= 0
        } else {
            if (button_in != button_state) {
                counter <= counter + 1
                if (counter == (1 << COUNTER_WIDTH) - 1) {
                    button_state <= button_in
                    counter <= 0
                }
            } else {
                counter <= 0
            }
        }
    }

    button_out = button_state
}
```

---

## Pulse Generators

### One-Shot Pulse Generator

**Problem:** Generate single pulse on trigger.

**Solution:**
```skalp
entity OneShotPulse {
    in clk: clock
    in rst: reset
    in trigger: bit
    out pulse: bit
}

impl OneShotPulse {
    signal triggered: bit = 0

    on(clk.rise) {
        if (rst) {
            triggered <= 0
        } else if (trigger && !triggered) {
            triggered <= 1
        } else if (!trigger) {
            triggered <= 0
        }
    }

    pulse = if (trigger && !triggered) { 1 } else { 0 }
}
```

---

### Pulse Width Generator

**Problem:** Generate pulse of specific width.

**Solution:**
```skalp
entity PulseWidthGenerator<const WIDTH: nat = 10> {
    in clk: clock
    in rst: reset
    in trigger: bit
    out pulse: bit
}

impl PulseWidthGenerator {
    signal counter: nat[clog2(WIDTH+1)] = 0
    signal active: bit = 0

    on(clk.rise) {
        if (rst) {
            counter <= 0
            active <= 0
        } else if (trigger && !active) {
            active <= 1
            counter <= 1
        } else if (active) {
            if (counter == WIDTH) {
                active <= 0
                counter <= 0
            } else {
                counter <= counter + 1
            }
        }
    }

    pulse = active
}
```

---

## Delay Lines

### Fixed Delay

**Problem:** Delay signal by N clock cycles.

**Solution:**
```skalp
entity Delay<const WIDTH: nat = 8, const STAGES: nat = 3> {
    in clk: clock
    in rst: reset
    in data_in: bit[WIDTH]
    out data_out: bit[WIDTH]
}

impl Delay {
    signal regs: [bit[WIDTH]; STAGES]

    on(clk.rise) {
        if (rst) {
            // Reset all stages
            for i in 0..STAGES {
                regs[i] <= 0
            }
        } else {
            regs[0] <= data_in
            for i in 1..STAGES {
                regs[i] <= regs[i-1]
            }
        }
    }

    data_out = regs[STAGES-1]
}
```

**Note:** For loop syntax is conceptual; actual implementation uses explicit assignments.

**Practical 3-stage delay:**
```skalp
entity Delay3<const WIDTH: nat = 8> {
    in clk: clock
    in rst: reset
    in data_in: bit[WIDTH]
    out data_out: bit[WIDTH]
}

impl Delay3 {
    signal stage1: bit[WIDTH] = 0
    signal stage2: bit[WIDTH] = 0
    signal stage3: bit[WIDTH] = 0

    on(clk.rise) {
        if (rst) {
            stage1 <= 0
            stage2 <= 0
            stage3 <= 0
        } else {
            stage1 <= data_in
            stage2 <= stage1
            stage3 <= stage2
        }
    }

    data_out = stage3
}
```

---

## Synchronizers

### 2-FF Synchronizer

**Problem:** Safely cross clock domains (single bit).

**Solution:**
```skalp
entity Synchronizer2FF {
    in clk: clock
    in rst: reset
    in async_in: bit
    out sync_out: bit
}

impl Synchronizer2FF {
    signal ff1: bit = 0
    signal ff2: bit = 0

    on(clk.rise) {
        if (rst) {
            ff1 <= 0
            ff2 <= 0
        } else {
            ff1 <= async_in
            ff2 <= ff1
        }
    }

    sync_out = ff2
}
```

**Important:** Only for single-bit signals! Multi-bit requires different approach.

---

### 3-FF Synchronizer (More Reliable)

**Problem:** Better metastability protection.

**Solution:**
```skalp
entity Synchronizer3FF {
    in clk: clock
    in rst: reset
    in async_in: bit
    out sync_out: bit
}

impl Synchronizer3FF {
    signal ff1: bit = 0
    signal ff2: bit = 0
    signal ff3: bit = 0

    on(clk.rise) {
        if (rst) {
            ff1 <= 0
            ff2 <= 0
            ff3 <= 0
        } else {
            ff1 <= async_in
            ff2 <= ff1
            ff3 <= ff2
        }
    }

    sync_out = ff3
}
```

---

## Pipeline Registers

### 2-Stage Pipeline

**Problem:** Pipeline combinational logic for higher frequency.

**Solution:**
```skalp
entity Pipeline2Stage<const WIDTH: nat = 32> {
    in clk: clock
    in rst: reset
    in data_in: bit[WIDTH]
    out data_out: bit[WIDTH]
}

impl Pipeline2Stage {
    signal stage1: bit[WIDTH] = 0
    signal stage2: bit[WIDTH] = 0

    // Combinational logic (example: complex operation)
    signal comb1: bit[WIDTH] = data_in * 3
    signal comb2: bit[WIDTH] = stage1 + 7

    on(clk.rise) {
        if (rst) {
            stage1 <= 0
            stage2 <= 0
        } else {
            stage1 <= comb1
            stage2 <= comb2
        }
    }

    data_out = stage2
}
```

---

## Accumulators

### Simple Accumulator

**Problem:** Sum inputs over time.

**Solution:**
```skalp
entity Accumulator<const WIDTH: nat = 16> {
    in clk: clock
    in rst: reset
    in data_in: bit[WIDTH]
    in enable: bit
    out sum: bit[WIDTH]
}

impl Accumulator {
    signal accumulator: bit[WIDTH] = 0

    on(clk.rise) {
        if (rst) {
            accumulator <= 0
        } else if (enable) {
            accumulator <= accumulator + data_in
        }
    }

    sum = accumulator
}
```

---

### Accumulator with Saturation

**Problem:** Accumulate but saturate at maximum.

**Solution:**
```skalp
entity AccumulatorSat<const WIDTH: nat = 16> {
    in clk: clock
    in rst: reset
    in data_in: bit[WIDTH]
    in enable: bit
    out sum: bit[WIDTH]
    out saturated: bit
}

impl AccumulatorSat {
    signal accumulator: bit[WIDTH] = 0
    signal max_val: bit[WIDTH] = (1 << WIDTH) - 1

    on(clk.rise) {
        if (rst) {
            accumulator <= 0
        } else if (enable) {
            signal new_sum: bit[WIDTH+1] = accumulator + data_in
            if (new_sum > max_val) {
                accumulator <= max_val  // Saturate
            } else {
                accumulator <= new_sum[WIDTH-1:0]
            }
        }
    }

    sum = accumulator
    saturated = if (accumulator == max_val) { 1 } else { 0 }
}
```

---

## Common Patterns Summary

| Pattern | Use Case | Key Feature |
|---------|----------|-------------|
| **Register** | Store value | Basic flip-flop |
| **Register + Enable** | Conditional update | Only update when enabled |
| **Counter** | Counting events | Auto-increment |
| **Shift Register** | Serial data | Bit-by-bit shifting |
| **LFSR** | Random numbers | Pseudo-random sequence |
| **Edge Detector** | Pulse on transition | Detect 0→1 or 1→0 |
| **Debouncer** | Button input | Filter mechanical noise |
| **One-Shot** | Single pulse | Trigger → pulse |
| **Delay Line** | Pipeline timing | N-cycle delay |
| **Synchronizer** | CDC safety | Cross clock domains |
| **Accumulator** | Running sum | Add over time |

---

## See Also

- [Combinational Patterns](combinational.md) - Muxes, adders, comparators
- [State Machine Patterns](state-machines.md) - FSM patterns
- [Memory Patterns](memories.md) - RAMs, ROMs, FIFOs
- [Syntax Reference](../reference/syntax.md) - Language syntax

---

**Key Takeaways:**
- Always use `<=` for sequential assignments
- Reset handling is mandatory for registers
- Edge detectors need previous state
- Synchronizers are critical for CDC
- Pipeline stages reduce combinational depth
