# Combinational Logic Patterns

**Common combinational circuits with copy-paste SKALP code.**

---

## Multiplexers

### 2-to-1 Multiplexer

**Problem:** Select between two inputs.

**Solution:**
```skalp
entity Mux2<const WIDTH: nat = 8> {
    in sel: bit
    in in0: bit[WIDTH]
    in in1: bit[WIDTH]
    out out: bit[WIDTH]
}

impl Mux2 {
    out = if sel { in1 } else { in0 }
}
```

**Equivalent to:**
```
out = sel ? in1 : in0
```

---

### 4-to-1 Multiplexer

**Problem:** Select between four inputs.

**Solution:**
```skalp
entity Mux4<const WIDTH: nat = 8> {
    in sel: bit[2]
    in in0: bit[WIDTH]
    in in1: bit[WIDTH]
    in in2: bit[WIDTH]
    in in3: bit[WIDTH]
    out out: bit[WIDTH]
}

impl Mux4 {
    out = match sel {
        0b00 => in0,
        0b01 => in1,
        0b10 => in2,
        0b11 => in3,
        _ => 0
    }
}
```

---

### Parameterized N-to-1 Multiplexer

**Problem:** Select between N inputs (compile-time known).

**Solution:**
```skalp
entity MuxN<const WIDTH: nat = 8, const N: nat = 4> {
    in sel: nat[clog2(N)]
    in inputs: [bit[WIDTH]; N]
    out out: bit[WIDTH]
}

impl MuxN {
    out = inputs[sel]
}
```

---

## Encoders and Decoders

### Binary Encoder (4-to-2)

**Problem:** Convert one-hot encoding to binary.

**Solution:**
```skalp
entity Encoder4to2 {
    in in: bit[4]        // One-hot input
    out out: bit[2]      // Binary output
    out valid: bit       // Valid flag
}

impl Encoder4to2 {
    out = match in {
        0b0001 => 0b00,
        0b0010 => 0b01,
        0b0100 => 0b10,
        0b1000 => 0b11,
        _ => 0b00
    }

    valid = match in {
        0b0001 | 0b0010 | 0b0100 | 0b1000 => 1,
        _ => 0
    }
}
```

---

### Binary Decoder (2-to-4)

**Problem:** Convert binary to one-hot encoding.

**Solution:**
```skalp
entity Decoder2to4 {
    in in: bit[2]
    in enable: bit
    out out: bit[4]
}

impl Decoder2to4 {
    signal decoded: bit[4]

    decoded = match in {
        0b00 => 0b0001,
        0b01 => 0b0010,
        0b10 => 0b0100,
        0b11 => 0b1000,
        _ => 0b0000
    }

    out = if enable { decoded } else { 0 }
}
```

---

### Priority Encoder

**Problem:** Find the position of the highest set bit.

**Solution:**
```skalp
entity PriorityEncoder8 {
    in in: bit[8]
    out out: bit[3]      // Position of highest bit
    out valid: bit       // Any bit set?
}

impl PriorityEncoder8 {
    signal has_bit: bit = (in != 0)

    out = if in[7] { 7 } else
          if in[6] { 6 } else
          if in[5] { 5 } else
          if in[4] { 4 } else
          if in[3] { 3 } else
          if in[2] { 2 } else
          if in[1] { 1 } else
          if in[0] { 0 } else { 0 }

    valid = has_bit
}
```

---

## Adders and Arithmetic

### Half Adder

**Problem:** Add two single bits.

**Solution:**
```skalp
entity HalfAdder {
    in a: bit
    in b: bit
    out sum: bit
    out carry: bit
}

impl HalfAdder {
    sum = a ^ b
    carry = a & b
}
```

---

### Full Adder

**Problem:** Add two bits plus carry-in.

**Solution:**
```skalp
entity FullAdder {
    in a: bit
    in b: bit
    in cin: bit
    out sum: bit
    out cout: bit
}

impl FullAdder {
    sum = a ^ b ^ cin
    cout = (a & b) | (cin & (a ^ b))
}
```

---

### Ripple Carry Adder

**Problem:** Add two N-bit numbers.

**Solution:**
```skalp
entity RippleCarryAdder<const WIDTH: nat = 8> {
    in a: bit[WIDTH]
    in b: bit[WIDTH]
    in cin: bit
    out sum: bit[WIDTH]
    out cout: bit
}

impl RippleCarryAdder {
    signal result: bit[WIDTH+1]
    result = a + b + cin
    sum = result[WIDTH-1:0]
    cout = result[WIDTH]
}
```

**Note:** SKALP optimizes this automatically. Use the built-in `+` operator!

---

### Incrementer

**Problem:** Add 1 to a value.

**Solution:**
```skalp
entity Incrementer<const WIDTH: nat = 8> {
    in in: bit[WIDTH]
    out out: bit[WIDTH]
    out overflow: bit
}

impl Incrementer {
    signal result: bit[WIDTH+1]
    result = in + 1
    out = result[WIDTH-1:0]
    overflow = result[WIDTH]
}
```

---

### Subtractor

**Problem:** Subtract two numbers.

**Solution:**
```skalp
entity Subtractor<const WIDTH: nat = 8> {
    in a: bit[WIDTH]
    in b: bit[WIDTH]
    out diff: bit[WIDTH]
    out borrow: bit
}

impl Subtractor {
    signal result: bit[WIDTH+1]
    result = a - b
    diff = result[WIDTH-1:0]
    borrow = result[WIDTH]  // Borrow = 1 if a < b
}
```

---

## Comparators

### Equality Comparator

**Problem:** Check if two values are equal.

**Solution:**
```skalp
entity Comparator<const WIDTH: nat = 8> {
    in a: bit[WIDTH]
    in b: bit[WIDTH]
    out eq: bit       // a == b
    out ne: bit       // a != b
}

impl Comparator {
    eq = if a == b { 1 } else { 0 }
    ne = if a != b { 1 } else { 0 }
}
```

---

### Magnitude Comparator

**Problem:** Compare magnitude of two values.

**Solution:**
```skalp
entity MagnitudeComparator<const WIDTH: nat = 8> {
    in a: bit[WIDTH]
    in b: bit[WIDTH]
    out lt: bit       // a < b
    out eq: bit       // a == b
    out gt: bit       // a > b
}

impl MagnitudeComparator {
    lt = if a < b { 1 } else { 0 }
    eq = if a == b { 1 } else { 0 }
    gt = if a > b { 1 } else { 0 }
}
```

---

## Bitwise Operations

### Bit Counter (Population Count)

**Problem:** Count the number of set bits.

**Solution (8-bit):**
```skalp
entity PopCount8 {
    in in: bit[8]
    out count: bit[4]    // Max count is 8, needs 4 bits
}

impl PopCount8 {
    signal c0: bit = in[0]
    signal c1: bit = in[1]
    signal c2: bit = in[2]
    signal c3: bit = in[3]
    signal c4: bit = in[4]
    signal c5: bit = in[5]
    signal c6: bit = in[6]
    signal c7: bit = in[7]

    count = c0 + c1 + c2 + c3 + c4 + c5 + c6 + c7
}
```

---

### Leading Zero Counter

**Problem:** Count leading zeros.

**Solution (8-bit):**
```skalp
entity LeadingZeros8 {
    in in: bit[8]
    out count: bit[4]    // Max 8 leading zeros
}

impl LeadingZeros8 {
    count = if in[7] { 0 } else
            if in[6] { 1 } else
            if in[5] { 2 } else
            if in[4] { 3 } else
            if in[3] { 4 } else
            if in[2] { 5 } else
            if in[1] { 6 } else
            if in[0] { 7 } else { 8 }
}
```

---

### Bit Reversal

**Problem:** Reverse bit order.

**Solution:**
```skalp
entity BitReverse<const WIDTH: nat = 8> {
    in in: bit[WIDTH]
    out out: bit[WIDTH]
}

impl BitReverse {
    // For 8-bit: out[0] = in[7], out[1] = in[6], ...
    signal rev0: bit = in[7]
    signal rev1: bit = in[6]
    signal rev2: bit = in[5]
    signal rev3: bit = in[4]
    signal rev4: bit = in[3]
    signal rev5: bit = in[2]
    signal rev6: bit = in[1]
    signal rev7: bit = in[0]

    out = (rev7 << 7) | (rev6 << 6) | (rev5 << 5) | (rev4 << 4) |
          (rev3 << 3) | (rev2 << 2) | (rev1 << 1) | rev0
}
```

---

### Parity Generator

**Problem:** Generate even/odd parity bit.

**Solution:**
```skalp
entity ParityGenerator<const WIDTH: nat = 8> {
    in data: bit[WIDTH]
    out even_parity: bit
    out odd_parity: bit
}

impl ParityGenerator {
    signal xor_all: bit = data[0] ^ data[1] ^ data[2] ^ data[3] ^
                          data[4] ^ data[5] ^ data[6] ^ data[7]

    even_parity = xor_all      // XOR of all bits
    odd_parity = ~xor_all      // Inverse
}
```

---

## Barrel Shifter

**Problem:** Shift by variable amount in one cycle.

**Solution (8-bit, shift left):**
```skalp
entity BarrelShifter<const WIDTH: nat = 8> {
    in data: bit[WIDTH]
    in shift_amt: nat[clog2(WIDTH)]
    in shift_left: bit       // 1 = left, 0 = right
    out result: bit[WIDTH]
}

impl BarrelShifter {
    signal left_result: bit[WIDTH]
    signal right_result: bit[WIDTH]

    left_result = data << shift_amt
    right_result = data >> shift_amt

    result = if shift_left { left_result } else { right_result }
}
```

---

## Constant Multiplier

**Problem:** Multiply by a compile-time constant.

**Solution:**
```skalp
entity MultByConstant<const WIDTH: nat = 8, const MULT: nat = 3> {
    in in: bit[WIDTH]
    out out: bit[WIDTH*2]    // Result may need more bits
}

impl MultByConstant {
    out = in * MULT
}
```

**Optimizations:** SKALP will optimize constant multiplication to shifts and adds.

---

## Min/Max

### Minimum of Two Values

**Problem:** Find minimum of two inputs.

**Solution:**
```skalp
entity Min<const WIDTH: nat = 8> {
    in a: bit[WIDTH]
    in b: bit[WIDTH]
    out min: bit[WIDTH]
}

impl Min {
    min = if a < b { a } else { b }
}
```

---

### Maximum of Two Values

**Problem:** Find maximum of two inputs.

**Solution:**
```skalp
entity Max<const WIDTH: nat = 8> {
    in a: bit[WIDTH]
    in b: bit[WIDTH]
    out max: bit[WIDTH]
}

impl Max {
    max = if a > b { a } else { b }
}
```

---

## Gray Code Converter

### Binary to Gray Code

**Problem:** Convert binary to Gray code.

**Solution:**
```skalp
entity BinaryToGray<const WIDTH: nat = 8> {
    in binary: bit[WIDTH]
    out gray: bit[WIDTH]
}

impl BinaryToGray {
    gray = binary ^ (binary >> 1)
}
```

---

### Gray Code to Binary

**Problem:** Convert Gray code to binary.

**Solution (8-bit):**
```skalp
entity GrayToBinary8 {
    in gray: bit[8]
    out binary: bit[8]
}

impl GrayToBinary8 {
    signal b7: bit = gray[7]
    signal b6: bit = b7 ^ gray[6]
    signal b5: bit = b6 ^ gray[5]
    signal b4: bit = b5 ^ gray[4]
    signal b3: bit = b4 ^ gray[3]
    signal b2: bit = b3 ^ gray[2]
    signal b1: bit = b2 ^ gray[1]
    signal b0: bit = b1 ^ gray[0]

    binary = (b7 << 7) | (b6 << 6) | (b5 << 5) | (b4 << 4) |
             (b3 << 3) | (b2 << 2) | (b1 << 1) | b0
}
```

---

## See Also

- [Sequential Patterns](sequential.md) - Registers, counters, shift registers
- [State Machine Patterns](state-machines.md) - FSM patterns
- [Syntax Reference](../reference/syntax.md) - Language syntax
- [Examples](../examples/) - Complete designs

---

**Pattern Summary:**
- Use `if` expressions for 2-way muxes
- Use `match` for multi-way muxes
- Built-in `+`, `-`, `*` optimize automatically
- Slicing: `signal[high:low]`
- Concatenation: `(a << 8) | b`
