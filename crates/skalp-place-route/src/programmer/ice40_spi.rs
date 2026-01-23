//! iCE40 SPI Programming Protocol
//!
//! Implements the iCE40 SPI slave configuration protocol as described in
//! Lattice TN1248 "iCE40 Programming and Configuration".

use crate::error::Result;
use std::time::{Duration, Instant};

use super::{BoardConfig, FtdiDevice, FtdiPinConfig, ProgramResult};

/// iCE40 programmer using FTDI MPSSE SPI
pub struct Ice40Programmer {
    /// FTDI device handle
    device: FtdiDevice,
    /// Board configuration
    board_config: BoardConfig,
    /// Pin configuration
    pin_config: FtdiPinConfig,
}

impl Ice40Programmer {
    /// Open an iCE40 programmer for the specified board
    pub fn open(board_config: BoardConfig) -> Result<Self> {
        let pin_config = board_config.pin_config();
        let (vid, pid) = board_config.usb_id();

        // Open FTDI device
        let mut device = FtdiDevice::open(vid, pid, pin_config.interface)?;

        // Configure MPSSE mode
        device.configure(pin_config)?;

        log::info!(
            "Opened {} programmer (VID:PID {:04x}:{:04x})",
            board_config.name(),
            vid,
            pid
        );

        Ok(Self {
            device,
            board_config,
            pin_config,
        })
    }

    /// Program the FPGA with a bitstream
    ///
    /// The bitstream should be in raw binary format (.bin)
    pub fn program(&mut self, bitstream: &[u8]) -> Result<ProgramResult> {
        let start = Instant::now();

        log::info!(
            "Programming {} with {} bytes",
            self.board_config.name(),
            bitstream.len()
        );

        // Step 1: Assert CRESET_B low to reset the FPGA
        self.device.set_gpio(self.pin_config.creset_pin, false)?;

        // Step 2: Assert SPI_SS low to select the FPGA
        self.device.set_gpio(self.pin_config.ss_pin, false)?;

        // Step 3: Wait at least 200ns (we use 1us for safety)
        self.device.sleep(Duration::from_micros(1));

        // Step 4: Release CRESET_B
        self.device.set_gpio(self.pin_config.creset_pin, true)?;

        // Step 5: Wait at least 1200us for iCE40 to clear internal config memory
        // Use 2ms for safety margin
        self.device.sleep(Duration::from_millis(2));

        // Step 6: Send 8 dummy clocks to initialize SPI
        self.device.send_clocks(8)?;

        // Step 7: Send the bitstream
        self.device.spi_write(bitstream)?;

        // Step 8: Send at least 100 dummy clocks for startup sequence
        // Lattice recommends 49 clocks minimum, use 200 for safety
        self.device.send_clocks(200)?;

        // Step 9: Release SS
        self.device.set_gpio(self.pin_config.ss_pin, true)?;

        // Step 10: Check CDONE
        self.device.sleep(Duration::from_millis(1));
        let cdone_high = self.device.get_gpio(self.pin_config.cdone_pin)?;

        let elapsed = start.elapsed();
        let time_ms = elapsed.as_millis() as u64;
        let rate_kbps = if time_ms > 0 {
            (bitstream.len() as f64 / 1024.0) / (time_ms as f64 / 1000.0)
        } else {
            0.0
        };

        if cdone_high {
            log::info!(
                "Programming complete: {} bytes in {}ms ({:.1} KB/s)",
                bitstream.len(),
                time_ms,
                rate_kbps
            );
        } else {
            log::warn!("Programming complete but CDONE not high - configuration may have failed");
        }

        Ok(ProgramResult {
            bytes_programmed: bitstream.len(),
            time_ms,
            rate_kbps,
            cdone_high,
        })
    }

    /// Reset the FPGA without programming
    pub fn reset(&mut self) -> Result<()> {
        log::info!("Resetting FPGA");

        // Assert CRESET_B low
        self.device.set_gpio(self.pin_config.creset_pin, false)?;
        self.device.sleep(Duration::from_millis(1));

        // Release CRESET_B
        self.device.set_gpio(self.pin_config.creset_pin, true)?;
        self.device.sleep(Duration::from_millis(10));

        Ok(())
    }

    /// Check if CDONE is high (FPGA is configured)
    pub fn is_configured(&mut self) -> Result<bool> {
        self.device.get_gpio(self.pin_config.cdone_pin)
    }

    /// Get the board configuration
    pub fn board_config(&self) -> BoardConfig {
        self.board_config
    }
}

/// Program an iCE40 FPGA from a bitstream file
///
/// Convenience function that opens the programmer, programs the device,
/// and returns the result.
pub fn program_ice40(board: BoardConfig, bitstream: &[u8]) -> Result<ProgramResult> {
    let mut programmer = Ice40Programmer::open(board)?;
    programmer.program(bitstream)
}

/// Detect connected iCE40 boards
///
/// Returns a list of detected board configurations
pub fn detect_boards() -> Result<Vec<BoardConfig>> {
    // Try to detect common board types
    let boards_to_try = [
        BoardConfig::IceBreaker,
        BoardConfig::LatticeHx8kBreakout,
        BoardConfig::Upduino3,
        BoardConfig::GenericFt2232h,
        BoardConfig::GenericFt232h,
    ];

    let mut detected = Vec::new();

    for board in boards_to_try {
        let (vid, pid) = board.usb_id();
        let pin_config = board.pin_config();

        if FtdiDevice::open(vid, pid, pin_config.interface).is_ok() {
            detected.push(board);
            break; // Found a device, stop searching
        }
    }

    if detected.is_empty() {
        log::warn!("No iCE40 development boards detected");
    }

    Ok(detected)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_board_config() {
        let config = BoardConfig::IceBreaker;
        assert_eq!(config.name(), "iCEBreaker");

        let pin_config = config.pin_config();
        assert_eq!(pin_config.interface, 1);
        assert_eq!(pin_config.creset_pin, 7);
        assert_eq!(pin_config.cdone_pin, 6);
        assert_eq!(pin_config.ss_pin, 4);
    }

    // Hardware tests are gated behind a feature flag
    // Run with: cargo test --features hardware-test
    //
    // These tests require:
    // 1. An iCEBreaker or compatible board connected via USB
    // 2. A test bitstream file at test_data/blinky_hx8k.bin
    //
    // To run hardware tests manually:
    //   cargo test --features hardware-test -- --nocapture --ignored
    #[cfg(feature = "hardware-test")]
    mod hardware_tests {
        use super::*;

        #[test]
        #[ignore = "requires hardware: iCEBreaker board connected via USB"]
        fn test_detect_boards() {
            let boards = detect_boards().unwrap();
            println!("Detected boards: {:?}", boards);
            assert!(!boards.is_empty(), "Expected at least one board connected");
        }

        #[test]
        #[ignore = "requires hardware and test bitstream file"]
        fn test_program_blinky() {
            // To run this test, create a blinky bitstream using:
            //   icestorm: yosys -p "synth_ice40 -top top -blif blinky.blif" blinky.v
            //             arachne-pnr -d 8k -P ct256 blinky.blif -o blinky.asc
            //             icepack blinky.asc blinky.bin
            // Then copy to test_data/blinky_hx8k.bin
            let bitstream = std::fs::read("test_data/blinky_hx8k.bin")
                .expect("Test bitstream not found. Create test_data/blinky_hx8k.bin");

            let result = program_ice40(BoardConfig::IceBreaker, &bitstream).unwrap();
            assert!(result.cdone_high, "CDONE should be high after programming");
        }
    }
}
