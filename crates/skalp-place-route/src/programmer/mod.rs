//! FPGA Programmer for iCE40 devices
//!
//! Native Rust implementation of iCE40 SPI programming, compatible with:
//! - iCEBreaker (FT2232H)
//! - Lattice iCE40-HX8K Breakout (FT2232H)
//! - Other FTDI-based iCE40 programmers
//!
//! # Example
//!
//! ```ignore
//! use skalp_place_route::programmer::{Ice40Programmer, BoardConfig};
//!
//! let programmer = Ice40Programmer::open(BoardConfig::IceBreaker)?;
//! programmer.program(&bitstream_data)?;
//! ```

#[cfg(feature = "programmer")]
mod ftdi;
#[cfg(feature = "programmer")]
mod ice40_spi;

#[cfg(feature = "programmer")]
pub use ftdi::FtdiDevice;
#[cfg(feature = "programmer")]
pub use ice40_spi::{detect_boards, program_ice40, Ice40Programmer};

#[cfg(not(feature = "programmer"))]
use crate::error::{PlaceRouteError, Result};

/// Board configuration for different iCE40 development boards
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BoardConfig {
    /// iCEBreaker board (1BitSquared)
    /// FT2232H, Interface B for SPI
    IceBreaker,
    /// iCEBreaker Bitsy
    IceBreakerBitsy,
    /// Lattice iCE40-HX8K Breakout Board
    /// FT2232H, Interface B for SPI
    LatticeHx8kBreakout,
    /// TinyFPGA BX (uses custom bootloader, not FTDI)
    TinyFpgaBx,
    /// UPduino v3.x (FT232H)
    Upduino3,
    /// Generic FTDI FT2232H board
    GenericFt2232h,
    /// Generic FTDI FT232H board
    GenericFt232h,
    /// Custom configuration
    Custom(FtdiPinConfig),
}

/// Pin configuration for FTDI MPSSE SPI
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FtdiPinConfig {
    /// FTDI interface to use (0 = A, 1 = B)
    pub interface: u8,
    /// GPIO pin for CRESET_B (active low reset)
    pub creset_pin: u8,
    /// GPIO pin for CDONE (configuration done)
    pub cdone_pin: u8,
    /// GPIO pin for SS (SPI chip select)
    pub ss_pin: u8,
    /// SPI clock frequency in Hz
    pub spi_freq_hz: u32,
}

impl Default for FtdiPinConfig {
    fn default() -> Self {
        // Default configuration for iCEBreaker
        Self {
            interface: 1,           // Interface B
            creset_pin: 7,          // GPIOH7 (ADBUS7)
            cdone_pin: 6,           // GPIOH6 (ADBUS6)
            ss_pin: 4,              // GPIOH4 (ADBUS4)
            spi_freq_hz: 6_000_000, // 6 MHz
        }
    }
}

impl BoardConfig {
    /// Get the FTDI pin configuration for this board
    pub fn pin_config(&self) -> FtdiPinConfig {
        match self {
            BoardConfig::IceBreaker => FtdiPinConfig {
                interface: 1,           // Interface B
                creset_pin: 7,          // ADBUS7
                cdone_pin: 6,           // ADBUS6
                ss_pin: 4,              // ADBUS4
                spi_freq_hz: 6_000_000, // 6 MHz
            },
            BoardConfig::IceBreakerBitsy => FtdiPinConfig {
                interface: 1,
                creset_pin: 7,
                cdone_pin: 6,
                ss_pin: 4,
                spi_freq_hz: 6_000_000,
            },
            BoardConfig::LatticeHx8kBreakout => FtdiPinConfig {
                interface: 1,           // Interface B
                creset_pin: 7,          // ADBUS7
                cdone_pin: 6,           // ADBUS6
                ss_pin: 4,              // ADBUS4
                spi_freq_hz: 6_000_000, // 6 MHz
            },
            BoardConfig::TinyFpgaBx => {
                // TinyFPGA BX uses a custom bootloader, not direct FTDI
                // This configuration is a placeholder
                FtdiPinConfig::default()
            }
            BoardConfig::Upduino3 => FtdiPinConfig {
                interface: 0,           // Single interface FT232H
                creset_pin: 7,          // ADBUS7
                cdone_pin: 6,           // ADBUS6
                ss_pin: 4,              // ADBUS4
                spi_freq_hz: 6_000_000, // 6 MHz
            },
            BoardConfig::GenericFt2232h => FtdiPinConfig {
                interface: 1,
                creset_pin: 7,
                cdone_pin: 6,
                ss_pin: 4,
                spi_freq_hz: 6_000_000,
            },
            BoardConfig::GenericFt232h => FtdiPinConfig {
                interface: 0,
                creset_pin: 7,
                cdone_pin: 6,
                ss_pin: 4,
                spi_freq_hz: 6_000_000,
            },
            BoardConfig::Custom(config) => *config,
        }
    }

    /// Get the FTDI USB VID:PID for this board
    pub fn usb_id(&self) -> (u16, u16) {
        match self {
            BoardConfig::IceBreaker | BoardConfig::IceBreakerBitsy => (0x0403, 0x6010), // FT2232H
            BoardConfig::LatticeHx8kBreakout => (0x0403, 0x6010),                       // FT2232H
            BoardConfig::TinyFpgaBx => (0x1d50, 0x6130), // TinyFPGA bootloader
            BoardConfig::Upduino3 => (0x0403, 0x6014),   // FT232H
            BoardConfig::GenericFt2232h => (0x0403, 0x6010),
            BoardConfig::GenericFt232h => (0x0403, 0x6014),
            BoardConfig::Custom(_) => (0x0403, 0x6010), // Default to FT2232H
        }
    }

    /// Get the board name for display
    pub fn name(&self) -> &'static str {
        match self {
            BoardConfig::IceBreaker => "iCEBreaker",
            BoardConfig::IceBreakerBitsy => "iCEBreaker Bitsy",
            BoardConfig::LatticeHx8kBreakout => "Lattice iCE40-HX8K Breakout",
            BoardConfig::TinyFpgaBx => "TinyFPGA BX",
            BoardConfig::Upduino3 => "UPduino v3",
            BoardConfig::GenericFt2232h => "Generic FT2232H",
            BoardConfig::GenericFt232h => "Generic FT232H",
            BoardConfig::Custom(_) => "Custom",
        }
    }
}

/// Programming result with statistics
#[derive(Debug, Clone)]
pub struct ProgramResult {
    /// Total bytes programmed
    pub bytes_programmed: usize,
    /// Programming time in milliseconds
    pub time_ms: u64,
    /// Transfer rate in KB/s
    pub rate_kbps: f64,
    /// Whether CDONE went high (success)
    pub cdone_high: bool,
}

/// Stub programmer for when FTDI feature is disabled
#[cfg(not(feature = "programmer"))]
pub struct Ice40Programmer;

#[cfg(not(feature = "programmer"))]
impl Ice40Programmer {
    /// Open a programmer (stub - returns error when feature disabled)
    pub fn open(_config: BoardConfig) -> Result<Self> {
        Err(PlaceRouteError::BitstreamFailed(
            "Programmer support not enabled. Rebuild with --features programmer".to_string(),
        ))
    }

    /// Program the FPGA (stub)
    pub fn program(&self, _bitstream: &[u8]) -> Result<ProgramResult> {
        Err(PlaceRouteError::BitstreamFailed(
            "Programmer support not enabled".to_string(),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_board_config() {
        let config = BoardConfig::IceBreaker;
        assert_eq!(config.name(), "iCEBreaker");
        assert_eq!(config.usb_id(), (0x0403, 0x6010));

        let pin_config = config.pin_config();
        assert_eq!(pin_config.interface, 1);
        assert_eq!(pin_config.spi_freq_hz, 6_000_000);
    }

    #[test]
    fn test_custom_config() {
        let custom = FtdiPinConfig {
            interface: 0,
            creset_pin: 5,
            cdone_pin: 4,
            ss_pin: 3,
            spi_freq_hz: 1_000_000,
        };
        let config = BoardConfig::Custom(custom);
        let pin_config = config.pin_config();
        assert_eq!(pin_config.creset_pin, 5);
    }
}
