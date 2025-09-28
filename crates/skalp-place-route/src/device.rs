//! FPGA device definitions and programming

/// FPGA device information
pub struct Device {
    /// Device name
    pub name: String,
    /// Number of logic blocks
    pub logic_blocks: usize,
    /// Number of I/O pins
    pub io_pins: usize,
    /// Clock resources
    pub clock_resources: usize,
}

impl Device {
    /// Create ICE40 HX8K device
    pub fn ice40_hx8k() -> Self {
        Self {
            name: "iCE40HX8K".to_string(),
            logic_blocks: 7680,
            io_pins: 206,
            clock_resources: 8,
        }
    }

    /// Create ICE40 HX1K device
    pub fn ice40_hx1k() -> Self {
        Self {
            name: "iCE40HX1K".to_string(),
            logic_blocks: 1280,
            io_pins: 95,
            clock_resources: 8,
        }
    }
}

/// Device programmer
pub struct DeviceProgrammer {
    /// Programming interface
    interface: ProgrammingInterface,
}

/// Programming interface types
pub enum ProgrammingInterface {
    /// SPI programming
    Spi,
    /// JTAG programming
    Jtag,
    /// USB programming
    Usb,
}

impl DeviceProgrammer {
    /// Create a new device programmer
    pub fn new(interface: ProgrammingInterface) -> Self {
        Self { interface }
    }

    /// Program device with bitstream
    pub fn program(&self, _device: &Device, _bitstream: &super::bitstream::Bitstream) -> Result<(), ProgrammingError> {
        // Simplified programming - would call actual programmer tools
        Ok(())
    }
}

/// Programming errors
#[derive(Debug, thiserror::Error)]
pub enum ProgrammingError {
    #[error("Programming failed: {0}")]
    Failed(String),
    #[error("Device not found")]
    DeviceNotFound,
}