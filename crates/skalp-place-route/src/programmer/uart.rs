//! A board's UART link to the host, through its FTDI chip.
//!
//! Development boards with an FTDI FT2232H use its first channel for JTAG and
//! often wire the second to FPGA pins as a UART. This reads that UART, so a
//! design can send data back to the host on the same USB cable.

use crate::error::{PlaceRouteError, Result};
use libftd2xx::{list_devices, BitMode, BitsPerWord, Ftdi, FtdiCommon, Parity, StopBits};
use std::time::{Duration, Instant};

fn err(message: String) -> PlaceRouteError {
    PlaceRouteError::BitstreamFailed(message)
}

/// The UART side of a board's FTDI chip.
pub struct BoardUart {
    ftdi: Ftdi,
}

impl BoardUart {
    /// Open channel `channel` (0 for A, 1 for B) of the FTDI chip `vid:pid` as
    /// an 8N1 UART at `baud`.
    pub fn open(vid: u16, pid: u16, channel: u8, baud: u32) -> Result<Self> {
        let devices = list_devices().map_err(|e| err(format!("Cannot list FTDI devices: {e}")))?;
        let suffix = (b'A' + channel) as char;
        // A multi-channel chip enumerates once per channel, in channel order;
        // boards with a blank EEPROM give no serial number to tell them apart.
        let index = devices
            .iter()
            .enumerate()
            .filter(|(_, d)| d.vendor_id == vid && d.product_id == pid)
            .nth(channel as usize)
            .map(|(index, _)| index)
            .ok_or_else(|| {
                err(format!(
                    "No FTDI channel {suffix} for {vid:04x}:{pid:04x}. Is the board connected?"
                ))
            })?;
        let mut ftdi = Ftdi::with_index(index as i32)
            .map_err(|e| err(format!("Cannot open FTDI channel {suffix}: {e}")))?;
        let setup = |ftdi: &mut Ftdi| -> std::result::Result<(), libftd2xx::FtStatus> {
            ftdi.set_bit_mode(0, BitMode::Reset)?;
            ftdi.set_baud_rate(baud)?;
            ftdi.set_data_characteristics(BitsPerWord::Bits8, StopBits::Bits1, Parity::No)?;
            ftdi.set_flow_control_none()?;
            ftdi.set_timeouts(Duration::from_millis(50), Duration::from_millis(50))?;
            ftdi.set_latency_timer(Duration::from_millis(2))?;
            ftdi.purge_all()
        };
        setup(&mut ftdi).map_err(|e| err(format!("Cannot set up the UART: {e}")))?;
        Ok(Self { ftdi })
    }

    /// Everything received during `duration`.
    pub fn read_for(&mut self, duration: Duration) -> Result<Vec<u8>> {
        let mut received = Vec::new();
        let deadline = Instant::now() + duration;
        let mut buffer = vec![0u8; 65536];
        while Instant::now() < deadline {
            let waiting = self
                .ftdi
                .queue_status()
                .map_err(|e| err(format!("UART status failed: {e}")))?;
            if waiting == 0 {
                std::thread::sleep(Duration::from_millis(1));
                continue;
            }
            let want = waiting.min(buffer.len());
            let count = self
                .ftdi
                .read(&mut buffer[..want])
                .map_err(|e| err(format!("UART read failed: {e}")))?;
            received.extend_from_slice(&buffer[..count]);
        }
        Ok(received)
    }

    /// Read until `bytes` have arrived or `timeout` passes.
    pub fn read_exact_within(&mut self, bytes: usize, timeout: Duration) -> Result<Vec<u8>> {
        let mut received = Vec::with_capacity(bytes);
        let deadline = Instant::now() + timeout;
        let mut buffer = vec![0u8; 65536];
        while received.len() < bytes && Instant::now() < deadline {
            let waiting = self
                .ftdi
                .queue_status()
                .map_err(|e| err(format!("UART status failed: {e}")))?;
            if waiting == 0 {
                std::thread::sleep(Duration::from_millis(1));
                continue;
            }
            let want = waiting.min(bytes - received.len()).min(buffer.len());
            let count = self
                .ftdi
                .read(&mut buffer[..want])
                .map_err(|e| err(format!("UART read failed: {e}")))?;
            received.extend_from_slice(&buffer[..count]);
        }
        Ok(received)
    }
}
