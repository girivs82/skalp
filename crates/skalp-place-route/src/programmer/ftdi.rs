//! FTDI MPSSE Interface for SPI Programming
//!
//! Low-level interface to FTDI chips using MPSSE mode for SPI communication.

use crate::error::{PlaceRouteError, Result};
use libftd2xx::{list_devices, BitMode, Ftdi, FtdiCommon, MpsseCmdBuilder};
use std::time::Duration;

use super::FtdiPinConfig;

/// FTDI device wrapper with MPSSE SPI support
pub struct FtdiDevice {
    /// FTDI device handle
    ftdi: Ftdi,
    /// Pin configuration
    config: FtdiPinConfig,
    /// Current GPIO output state
    gpio_state: u8,
    /// GPIO direction mask (1 = output)
    gpio_direction: u8,
}

impl FtdiDevice {
    /// Open an FTDI device by USB VID:PID
    pub fn open(vid: u16, pid: u16, interface: u8) -> Result<Self> {
        // List available devices
        let devices = list_devices().map_err(|e| {
            PlaceRouteError::BitstreamFailed(format!("Failed to list FTDI devices: {}", e))
        })?;

        if devices.is_empty() {
            return Err(PlaceRouteError::BitstreamFailed(
                "No FTDI devices found. Is the board connected?".to_string(),
            ));
        }

        // Find device with matching VID:PID
        let device_idx = devices
            .iter()
            .position(|d| {
                // Check if this device matches our VID:PID
                // The interface index is encoded in the serial number suffix (A, B, etc.)
                let interface_char = (b'A' + interface) as char;
                d.serial_number.ends_with(interface_char)
                    || (interface == 0 && !d.serial_number.ends_with('B'))
            })
            .ok_or_else(|| {
                PlaceRouteError::BitstreamFailed(format!(
                    "No FTDI device found with VID:PID {:04x}:{:04x}, interface {}. Available devices: {:?}",
                    vid, pid, interface,
                    devices.iter().map(|d| &d.serial_number).collect::<Vec<_>>()
                ))
            })?;

        // Open the device
        let ftdi = Ftdi::with_index(device_idx as i32).map_err(|e| {
            PlaceRouteError::BitstreamFailed(format!("Failed to open FTDI device: {}", e))
        })?;

        Ok(Self {
            ftdi,
            config: FtdiPinConfig::default(),
            gpio_state: 0,
            gpio_direction: 0,
        })
    }

    /// Open the first available FTDI device
    pub fn open_first() -> Result<Self> {
        let devices = list_devices().map_err(|e| {
            PlaceRouteError::BitstreamFailed(format!("Failed to list FTDI devices: {}", e))
        })?;

        if devices.is_empty() {
            return Err(PlaceRouteError::BitstreamFailed(
                "No FTDI devices found".to_string(),
            ));
        }

        let ftdi = Ftdi::new().map_err(|e| {
            PlaceRouteError::BitstreamFailed(format!("Failed to open FTDI device: {}", e))
        })?;

        Ok(Self {
            ftdi,
            config: FtdiPinConfig::default(),
            gpio_state: 0,
            gpio_direction: 0,
        })
    }

    /// Configure the device for MPSSE SPI mode
    pub fn configure(&mut self, config: FtdiPinConfig) -> Result<()> {
        self.config = config;

        // Reset the device
        self.ftdi.reset().map_err(|e| {
            PlaceRouteError::BitstreamFailed(format!("Failed to reset FTDI: {}", e))
        })?;

        // Set timeouts
        self.ftdi
            .set_timeouts(Duration::from_secs(5), Duration::from_secs(5))
            .map_err(|e| {
                PlaceRouteError::BitstreamFailed(format!("Failed to set timeouts: {}", e))
            })?;

        // Purge buffers
        self.ftdi.purge_all().map_err(|e| {
            PlaceRouteError::BitstreamFailed(format!("Failed to purge buffers: {}", e))
        })?;

        // Initialize MPSSE mode
        self.init_mpsse()?;

        Ok(())
    }

    /// Initialize MPSSE mode
    fn init_mpsse(&mut self) -> Result<()> {
        // Reset bit mode first
        self.ftdi.set_bit_mode(0x00, BitMode::Reset).map_err(|e| {
            PlaceRouteError::BitstreamFailed(format!("Failed to reset bit mode: {}", e))
        })?;

        // Enable MPSSE mode
        self.ftdi.set_bit_mode(0x00, BitMode::Mpsse).map_err(|e| {
            PlaceRouteError::BitstreamFailed(format!("Failed to set MPSSE mode: {}", e))
        })?;

        // Small delay for mode switch
        std::thread::sleep(Duration::from_millis(50));

        // Calculate clock divisor for desired SPI frequency
        // MPSSE clock = 60MHz / ((1 + divisor) * 2)
        // For 6 MHz: divisor = (60 / (6 * 2)) - 1 = 4
        let divisor = (60_000_000 / (self.config.spi_freq_hz * 2)) - 1;

        // Build MPSSE initialization commands
        let mut cmd = MpsseCmdBuilder::new();

        // Set clock divisor with divide-by-5 disabled (Some(false) = 60MHz base)
        // MPSSE clock = 60MHz / ((1 + divisor) * 2)
        cmd = cmd.set_clock(divisor, Some(false));

        // Disable adaptive clocking
        cmd = cmd.disable_adaptive_data_clocking();

        // Disable 3-phase clocking
        cmd = cmd.disable_3phase_data_clocking();

        // Configure GPIO pins
        // Low byte: ADBUS0-7
        // - ADBUS0 = TCK/SK (SPI clock) - output
        // - ADBUS1 = TDI/DO (MOSI) - output
        // - ADBUS2 = TDO/DI (MISO) - input
        // - ADBUS3 = TMS/CS (unused) - output
        // - ADBUS4 = GPIOL0 (SS) - output
        // - ADBUS5 = GPIOL1 - input
        // - ADBUS6 = GPIOL2 (CDONE) - input
        // - ADBUS7 = GPIOL3 (CRESET) - output

        self.gpio_direction = 0x9B; // 1001 1011: outputs on 0,1,3,4,7
        self.gpio_state = 0x90; // Initial: SS high, CRESET high

        cmd = cmd.set_gpio_lower(self.gpio_state, self.gpio_direction);

        // Send initialization commands
        let cmd_bytes = cmd.as_slice();
        self.write_raw(cmd_bytes)?;

        Ok(())
    }

    /// Set a GPIO pin state
    pub fn set_gpio(&mut self, pin: u8, high: bool) -> Result<()> {
        if high {
            self.gpio_state |= 1 << pin;
        } else {
            self.gpio_state &= !(1 << pin);
        }

        let mut cmd = MpsseCmdBuilder::new();
        cmd = cmd.set_gpio_lower(self.gpio_state, self.gpio_direction);
        self.write_raw(cmd.as_slice())
    }

    /// Read a GPIO pin state
    pub fn get_gpio(&mut self, pin: u8) -> Result<bool> {
        // Send command to read GPIO
        let cmd = [0x81]; // Read low byte
        self.write_raw(&cmd)?;

        let mut buf = [0u8; 1];
        self.read_raw(&mut buf)?;

        Ok((buf[0] & (1 << pin)) != 0)
    }

    /// Write data via SPI (MSB first, mode 0)
    pub fn spi_write(&mut self, data: &[u8]) -> Result<()> {
        if data.is_empty() {
            return Ok(());
        }

        // Write in chunks to avoid buffer overflow
        const CHUNK_SIZE: usize = 65536;

        for chunk in data.chunks(CHUNK_SIZE) {
            let len = chunk.len() - 1;

            // MPSSE command for SPI write: clock bytes out on -ve edge
            let mut cmd = vec![
                0x11, // Clock data bytes out on -ve clock edge MSB first
                (len & 0xFF) as u8,
                ((len >> 8) & 0xFF) as u8,
            ];
            cmd.extend_from_slice(chunk);

            self.write_raw(&cmd)?;
        }

        Ok(())
    }

    /// Write raw bytes to FTDI
    fn write_raw(&mut self, data: &[u8]) -> Result<()> {
        self.ftdi
            .write_all(data)
            .map_err(|e| PlaceRouteError::BitstreamFailed(format!("FTDI write failed: {}", e)))?;

        Ok(())
    }

    /// Read raw bytes from FTDI
    fn read_raw(&mut self, buf: &mut [u8]) -> Result<()> {
        self.ftdi
            .read_all(buf)
            .map_err(|e| PlaceRouteError::BitstreamFailed(format!("FTDI read failed: {}", e)))?;

        Ok(())
    }

    /// Send dummy clock cycles (for iCE40 startup)
    pub fn send_clocks(&mut self, num_bits: usize) -> Result<()> {
        let num_bytes = num_bits.div_ceil(8);
        let dummy = vec![0xFF; num_bytes];
        self.spi_write(&dummy)
    }

    /// Sleep for a specified duration
    pub fn sleep(&self, duration: Duration) {
        std::thread::sleep(duration);
    }
}

impl Drop for FtdiDevice {
    fn drop(&mut self) {
        // Reset GPIO to safe state
        let _ = self.set_gpio(self.config.creset_pin, true);
        let _ = self.set_gpio(self.config.ss_pin, true);
    }
}
