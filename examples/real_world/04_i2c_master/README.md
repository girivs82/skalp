# I2C Master Controller

A simple I2C master controller implementing standard mode (100kHz) communication.

## Features

- **Multi-state FSM**: 8 states (IDLE, START, ADDR, ACK_ADDR, DATA, ACK_DATA, STOP, WAIT)
- **7-bit addressing**: Standard I2C address format
- **Read/Write operations**: Single byte transactions
- **ACK/NACK handling**: Detects slave acknowledgment
- **START/STOP conditions**: Proper I2C bus signaling
- **Clock generation**: Divides 50MHz down to 100kHz I2C clock

## Interface

### System
- `clk`: 50MHz system clock
- `rst`: Active-high reset

### Control
- `start`: Initiate transaction (pulse high)
- `rw`: Direction (0=write, 1=read)
- `addr`: 7-bit I2C slave address
- `write_data`: Data byte to write
- `read_data`: Data byte read from slave
- `busy`: Transaction in progress
- `ack_received`: Set if slave acknowledged

### I2C Bus (Simplified)
- `scl`: I2C clock output
- `sda_out`: SDA data output
- `sda_in`: SDA data input
- `sda_oe`: SDA output enable (0=input, 1=output)

**Note**: This is a simplified interface without true bidirectional support. In real hardware, you would use a tristate buffer controlled by `sda_oe` to implement bidirectional SDA.

## Timing

- System clock: 50MHz
- I2C clock: 100kHz (standard mode)
- Clock divider: 50MHz / 125 = 400kHz tick (4 phases per I2C clock cycle)
- Each SCL period: 10μs (100kHz)

## Usage Example

```skalp
// Write 0x42 to device at address 0x50
start <= 1
rw <= 0
addr <= 0x50
write_data <= 0x42

// Wait for busy to go high
// ... transaction happens ...
// Check ack_received when busy goes low

// Read from device at address 0x51
start <= 1
rw <= 1
addr <= 0x51
// Wait for busy to go low, then check read_data
```

## Limitations

- Single byte transactions only (no multi-byte support)
- No repeated START condition
- No clock stretching in write mode
- Simplified SDA interface (not true bidirectional)
- Master always sends NACK after read (ends transaction)

## SKALP Features Demonstrated

- ✅ Complex FSM with 8 states
- ✅ Clock divider and phase tracking
- ✅ Shift register operations
- ✅ Modulo arithmetic for circular buffers
- ✅ Multiple conditional branches
- ✅ Bit manipulation (division by powers of 2)
- ✅ Sequential signal assignments

## Real-World Use Cases

- Reading temperature sensors (LM75, TMP102)
- Controlling DACs and ADCs
- Accessing EEPROMs (24Cxx series)
- RTC communication (DS1307, DS3231)
- Display interfaces (SSD1306 OLED)
