# Example 2: UART Transmitter

## Description

A UART (Universal Asynchronous Receiver/Transmitter) transmitter module that serializes 8-bit parallel data into serial format with configurable baud rate (115200 baud @ 50MHz clock).

## Complexity Level
⭐⭐ **Intermediate** - FSM with timing control

## Features Used

### Language Features
- ✅ FSM pattern with state encoding
- ✅ Multi-way `if/else if/else` conditionals
- ✅ Counter-based timing (baud rate generation)
- ✅ Bit manipulation (shift register with `/2`, `%2`)
- ✅ State transition logic
- ✅ Sequential and combinational assignments
- ✅ Comments for documentation

### Design Patterns
- **Finite State Machine (FSM)** - 4 states: IDLE → START → DATA → STOP
- **Baud rate timing** - Clock divider for serial bit timing
- **Shift register** - LSB-first serial transmission
- **Handshake protocol** - `tx_start` pulse, `tx_busy` flag

## Real-World Use Cases

1. **Debug console** - Serial logging and printf debugging
2. **Configuration interface** - Simple command/response protocols
3. **Sensor communication** - Many sensors use UART
4. **Bootloader** - Firmware upload over serial

## Compilation

```bash
skalp build -s uart_tx.sk -o build/
```

## Generated Output

- ✅ Compiles successfully
- ✅ Generates SystemVerilog FSM
- SystemVerilog: `/tmp/uart_tx_test/design.sv`

## Protocol Timing

```
Bit time: 50MHz / 115200 = 434 clock cycles
Frame format: [START | D0 D1 D2 D3 D4 D5 D6 D7 | STOP]
    START: 0 (1 bit time)
    DATA:  LSB first, 8 bits
    STOP:  1 (1 bit time)
Total: 10 bit times per byte
```

## Known Issues

1. **Comparison codegen bug**: Same issue as FIFO - `if (state == 0)` generates as `if (state)`
2. **No parity bit**: Simple 8N1 format only
3. **Fixed baud rate**: Hardcoded timing constant

These are **compiler bugs** in comparison generation.

## Next Steps

- Add testbench with serial data verification
- Simulate complete transmission
- Add parameterized baud rate
- Add UART receiver (RX)
