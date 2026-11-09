//! Lattice Nexus (CertusPro-NX, CrossLink-NX) SRAM configuration over JTAG
//!
//! Drives an FTDI FT2232H in MPSSE mode as a JTAG master and loads a `.bit`
//! file into configuration SRAM. Nothing here touches the SPI flash: the load
//! is volatile, so a power cycle (or PROGRAMN) returns the board to whatever
//! the flash holds.
//!
//! The command sequence follows Lattice FPGA-TN-02099 (sysCONFIG Usage Guide
//! for Nexus Platform).

use crate::error::{PlaceRouteError, Result};
use libftd2xx::{list_devices, BitMode, Ftdi, FtdiCommon};
use std::time::{Duration, Instant};

/// IDCODE of the LFCPNX-100, as read back from silicon.
pub const LFCPNX_100_IDCODE: u32 = 0x010F_4043;

const FTDI_VID: u16 = 0x0403;
const FT2232H_PID: u16 = 0x6010;

/// JTAG clock used for configuration. 6 MHz loads the LFCPNX-100 in a few
/// seconds and has margin on the evaluation board's level shifters.
const JTAG_HZ: u32 = 6_000_000;

/// How long to keep retrying the erase, leaving time to hold PROGRAMN.
const ERASE_WINDOW: Duration = Duration::from_secs(30);
/// Pause after each erase; LFCPNX-100 needs 4.87 ms (FPGA-TN-02099 table 6.7).
const ERASE_SETTLE: Duration = Duration::from_millis(250);

// Nexus configuration opcodes.
const IDCODE_PUB: u8 = 0xE0;
const ISC_ENABLE: u8 = 0xC6;
const ISC_DISABLE: u8 = 0x26;
const ISC_ERASE: u8 = 0x0E;
const LSC_READ_STATUS: u8 = 0x3C;
const LSC_CHECK_BUSY: u8 = 0xF0;
const LSC_INIT_ADDRESS: u8 = 0x46;
const LSC_BITSTREAM_BURST: u8 = 0x7A;
const LSC_DEVICE_CONTROL: u8 = 0x7D;
const LSC_REFRESH: u8 = 0x79;
const BYPASS: u8 = 0xFF;

// MPSSE opcodes (LSB first, TDI on the falling edge, TDO on the rising edge).
const MPSSE_BYTES_OUT: u8 = 0x19;
const MPSSE_BITS_OUT: u8 = 0x1B;
const MPSSE_BYTES_INOUT: u8 = 0x39;
const MPSSE_BITS_INOUT: u8 = 0x3B;
const MPSSE_TMS_OUT: u8 = 0x4B;
const MPSSE_TMS_INOUT: u8 = 0x6B;
const MPSSE_SET_LOW: u8 = 0x80;
const MPSSE_LOOPBACK_OFF: u8 = 0x85;
const MPSSE_SET_DIVISOR: u8 = 0x86;
const MPSSE_DIV5_OFF: u8 = 0x8A;
const MPSSE_3PHASE_OFF: u8 = 0x8D;
const MPSSE_CLOCK_BYTES: u8 = 0x8F;
const MPSSE_ADAPTIVE_OFF: u8 = 0x97;
const MPSSE_BAD_COMMAND_ECHO: u8 = 0xFA;

/// A decoded 64-bit Nexus status register.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NexusStatus(pub u64);

impl NexusStatus {
    /// Configuration completed and the device entered user mode.
    pub fn done(self) -> bool {
        self.0 & (1 << 8) != 0
    }
    /// Device is in in-system configuration mode.
    pub fn isc_enabled(self) -> bool {
        self.0 & (1 << 9) != 0
    }
    /// A configuration operation is in progress.
    pub fn busy(self) -> bool {
        self.0 & (1 << 12) != 0
    }
    /// The last configuration operation failed.
    pub fn fail(self) -> bool {
        self.0 & (1 << 13) != 0
    }
    /// Bitstream engine error code (0 means no error).
    pub fn bse_error(self) -> u8 {
        ((self.0 >> 24) & 0xF) as u8
    }
}

impl std::fmt::Display for NexusStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "0x{:016x} (done={} isc={} busy={} fail={} bse_err={})",
            self.0,
            self.done() as u8,
            self.isc_enabled() as u8,
            self.busy() as u8,
            self.fail() as u8,
            self.bse_error()
        )
    }
}

/// Outcome of a successful SRAM load.
#[derive(Debug, Clone)]
pub struct NexusLoadResult {
    /// Bytes shifted into the device.
    pub bytes: usize,
    /// Wall-clock load time in milliseconds.
    pub time_ms: u64,
    /// Status register read back after configuration.
    pub status: NexusStatus,
}

/// The configuration payload of a Lattice `.bit` file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NexusBitstream<'a> {
    /// IDCODE the bitstream was built for.
    pub idcode: u32,
    /// Bytes to shift, starting at the dummy byte ahead of the preamble.
    pub payload: &'a [u8],
}

/// Split a Lattice `.bit` file into its target IDCODE and configuration payload.
pub fn parse_bitstream(file: &[u8]) -> Result<NexusBitstream<'_>> {
    const PREAMBLE: [u8; 4] = [0xFF, 0xFF, 0xBD, 0xB3];
    let bad = |why: &str| PlaceRouteError::BitstreamFailed(format!("Not a Nexus .bit file: {why}"));

    let preamble = file
        .windows(PREAMBLE.len())
        .position(|w| w == PREAMBLE)
        .ok_or_else(|| bad("no FFFFBDB3 preamble"))?;
    if preamble == 0 {
        return Err(bad("no dummy byte ahead of the preamble"));
    }

    // The first command after the preamble and any CRC resets is VERIFY_ID:
    // opcode E2, three operand bytes, then the 32-bit IDCODE, big-endian.
    let body = &file[preamble + PREAMBLE.len()..];
    let verify = body
        .windows(4)
        .take(256)
        .position(|w| w == [0xE2, 0x00, 0x00, 0x00])
        .ok_or_else(|| bad("no VERIFY_ID command"))?;
    let id = body
        .get(verify + 4..verify + 8)
        .ok_or_else(|| bad("truncated VERIFY_ID command"))?;

    Ok(NexusBitstream {
        idcode: u32::from_be_bytes([id[0], id[1], id[2], id[3]]),
        payload: &file[preamble - 1..],
    })
}

/// Which JTAG TAP state a shift should finish by way of.
#[derive(Clone, Copy, PartialEq, Eq)]
enum AfterIr {
    /// Instruction takes no operand: return to Run-Test/Idle.
    Idle,
    /// Go straight from Update-IR into Shift-DR.
    ShiftDr,
}

/// A Nexus device reached through an FT2232H JTAG bridge.
pub struct NexusJtagProgrammer {
    ftdi: Ftdi,
}

fn usb_err(what: &str, e: impl std::fmt::Display) -> PlaceRouteError {
    PlaceRouteError::BitstreamFailed(format!("{what}: {e}"))
}

impl NexusJtagProgrammer {
    /// Open interface A of the first FT2232H and take it into MPSSE mode.
    pub fn open() -> Result<Self> {
        let devices = list_devices().map_err(|e| usb_err("Failed to list FTDI devices", e))?;
        // A dual-channel part enumerates once per interface; interface A,
        // the JTAG channel on Lattice boards, comes first.
        let index = devices
            .iter()
            .position(|d| d.vendor_id == FTDI_VID && d.product_id == FT2232H_PID)
            .ok_or_else(|| {
                PlaceRouteError::BitstreamFailed(
                    "No FT2232H found. Is the board connected?".to_string(),
                )
            })?;
        let ftdi =
            Ftdi::with_index(index as i32).map_err(|e| usb_err("Failed to open FT2232H", e))?;

        let mut programmer = Self { ftdi };
        programmer.init_mpsse()?;
        programmer.reset_tap()?;
        Ok(programmer)
    }

    fn init_mpsse(&mut self) -> Result<()> {
        let f = &mut self.ftdi;
        f.reset().map_err(|e| usb_err("FTDI reset failed", e))?;
        f.set_usb_parameters(65536)
            .map_err(|e| usb_err("FTDI set_usb_parameters failed", e))?;
        f.set_chars(0, false, 0, false)
            .map_err(|e| usb_err("FTDI set_chars failed", e))?;
        f.set_timeouts(Duration::from_secs(5), Duration::from_secs(5))
            .map_err(|e| usb_err("FTDI set_timeouts failed", e))?;
        f.set_latency_timer(Duration::from_millis(2))
            .map_err(|e| usb_err("FTDI set_latency_timer failed", e))?;
        f.set_bit_mode(0x00, BitMode::Reset)
            .map_err(|e| usb_err("FTDI bit mode reset failed", e))?;
        f.set_bit_mode(0x00, BitMode::Mpsse)
            .map_err(|e| usb_err("FTDI MPSSE enable failed", e))?;
        std::thread::sleep(Duration::from_millis(50));
        f.purge_all().map_err(|e| usb_err("FTDI purge failed", e))?;

        // A deliberately invalid opcode proves the command stream is in sync:
        // the MPSSE answers with 0xFA followed by the offending byte.
        self.write(&[0xAA])?;
        let mut echo = [0u8; 2];
        self.read(&mut echo)?;
        if echo != [MPSSE_BAD_COMMAND_ECHO, 0xAA] {
            return Err(PlaceRouteError::BitstreamFailed(format!(
                "MPSSE did not synchronize (echo {echo:02x?})"
            )));
        }

        // Clock = 60 MHz / (2 * (1 + divisor)).
        let divisor = (60_000_000 / (2 * JTAG_HZ) - 1) as u16;
        self.write(&[
            MPSSE_DIV5_OFF,
            MPSSE_ADAPTIVE_OFF,
            MPSSE_3PHASE_OFF,
            MPSSE_LOOPBACK_OFF,
            MPSSE_SET_DIVISOR,
            (divisor & 0xFF) as u8,
            (divisor >> 8) as u8,
            // TCK, TDI, TMS outputs; TMS idles high, TCK low.
            MPSSE_SET_LOW,
            0x08,
            0x0B,
        ])
    }

    fn write(&mut self, data: &[u8]) -> Result<()> {
        self.ftdi
            .write_all(data)
            .map_err(|e| usb_err("FTDI write failed", e))
    }

    fn read(&mut self, buf: &mut [u8]) -> Result<()> {
        self.ftdi
            .read_all(buf)
            .map_err(|e| usb_err("FTDI read failed", e))
    }

    /// Clock `count` (1..=7) TMS bits, LSB first, with TDI held at `tdi`.
    fn tms(&mut self, bits: u8, count: u8, tdi: bool) -> Result<()> {
        debug_assert!((1..=7).contains(&count));
        self.write(&[MPSSE_TMS_OUT, count - 1, (bits & 0x7F) | ((tdi as u8) << 7)])
    }

    /// Five TMS-high clocks reach Test-Logic-Reset from any state.
    fn reset_tap(&mut self) -> Result<()> {
        self.tms(0b0_11111, 6, true)
    }

    /// Clock Run-Test/Idle for at least `cycles` TCKs.
    fn idle(&mut self, cycles: u32) -> Result<()> {
        // The MPSSE clocks whole bytes here; round up.
        let bytes = cycles.div_ceil(8).max(1);
        let mut left = bytes;
        while left > 0 {
            let chunk = left.min(0x1_0000);
            let n = (chunk - 1) as u16;
            self.write(&[MPSSE_CLOCK_BYTES, (n & 0xFF) as u8, (n >> 8) as u8])?;
            left -= chunk;
        }
        Ok(())
    }

    /// Shift `tx` through the current shift state, LSB first, raising TMS on
    /// the final bit so the TAP lands in Exit1. Captures TDO into `rx` if given.
    fn shift(&mut self, tx: &[u8], rx: Option<&mut [u8]>) -> Result<()> {
        let capture = rx.is_some();
        let (last, body) = tx.split_last().expect("shift of zero bytes");
        let mut cmd = Vec::with_capacity(tx.len() + 16);
        for chunk in body.chunks(0x1_0000) {
            let n = (chunk.len() - 1) as u16;
            cmd.push(if capture {
                MPSSE_BYTES_INOUT
            } else {
                MPSSE_BYTES_OUT
            });
            cmd.push((n & 0xFF) as u8);
            cmd.push((n >> 8) as u8);
            cmd.extend_from_slice(chunk);
        }

        // Seven data bits, then the eighth as a TMS clock carrying TDI on bit 7.
        cmd.push(if capture {
            MPSSE_BITS_INOUT
        } else {
            MPSSE_BITS_OUT
        });
        cmd.push(6);
        cmd.push(*last);
        cmd.push(if capture {
            MPSSE_TMS_INOUT
        } else {
            MPSSE_TMS_OUT
        });
        cmd.push(0);
        cmd.push(0x01 | (last & 0x80));
        self.write(&cmd)?;

        if let Some(rx) = rx {
            let mut raw = vec![0u8; body.len() + 2];
            self.read(&mut raw)?;
            rx[..body.len()].copy_from_slice(&raw[..body.len()]);
            // Bit-mode reads arrive left-justified: seven bits at [7:1], the
            // TMS-clocked final bit at [7].
            let seven = raw[body.len()] >> 1;
            let eighth = raw[body.len() + 1] & 0x80;
            rx[body.len()] = seven | eighth;
        }
        Ok(())
    }

    /// Load an instruction from Run-Test/Idle.
    fn shift_ir(&mut self, opcode: u8, after: AfterIr) -> Result<()> {
        // Idle -> Select-DR -> Select-IR -> Capture-IR -> Shift-IR.
        self.tms(0b0011, 4, true)?;
        self.shift(&[opcode], None)?;
        match after {
            // Exit1-IR -> Update-IR -> Idle.
            AfterIr::Idle => self.tms(0b01, 2, true),
            // Exit1-IR -> Update-IR -> Select-DR -> Capture-DR -> Shift-DR.
            AfterIr::ShiftDr => self.tms(0b0011, 4, true),
        }
    }

    /// Exit1-DR -> Update-DR -> Run-Test/Idle.
    fn finish_dr(&mut self) -> Result<()> {
        self.tms(0b01, 2, true)
    }

    fn command(&mut self, opcode: u8) -> Result<()> {
        self.shift_ir(opcode, AfterIr::Idle)
    }

    fn command_with(&mut self, opcode: u8, operand: &[u8]) -> Result<()> {
        self.shift_ir(opcode, AfterIr::ShiftDr)?;
        self.shift(operand, None)?;
        self.finish_dr()
    }

    fn query(&mut self, opcode: u8, rx: &mut [u8]) -> Result<()> {
        let tx = vec![0u8; rx.len()];
        self.shift_ir(opcode, AfterIr::ShiftDr)?;
        self.shift(&tx, Some(rx))?;
        self.finish_dr()
    }

    /// Read the device IDCODE.
    pub fn read_idcode(&mut self) -> Result<u32> {
        let mut rx = [0u8; 4];
        self.query(IDCODE_PUB, &mut rx)?;
        Ok(u32::from_le_bytes(rx))
    }

    /// Read the configuration status register.
    pub fn read_status(&mut self) -> Result<NexusStatus> {
        let mut rx = [0u8; 8];
        self.query(LSC_READ_STATUS, &mut rx)?;
        self.idle(1000)?;
        Ok(NexusStatus(u64::from_le_bytes(rx)))
    }

    fn wait_not_busy(&mut self, what: &str) -> Result<()> {
        let deadline = Instant::now() + Duration::from_secs(30);
        loop {
            let mut rx = [0u8; 1];
            self.query(LSC_CHECK_BUSY, &mut rx)?;
            self.idle(1000)?;
            if rx[0] == 0 {
                return Ok(());
            }
            if Instant::now() > deadline {
                return Err(PlaceRouteError::BitstreamFailed(format!(
                    "Timed out waiting for {what}"
                )));
            }
        }
    }

    /// Bulk-erase configuration SRAM and confirm it took effect.
    ///
    /// DONE and FAIL must both read 0 after the erase (FPGA-TN-02099, table
    /// 6.16 note 2). This is the only point in the load where the device
    /// proves it is acting on configuration commands: a device that ignores
    /// them still reports DONE=1 after the burst, which is indistinguishable
    /// from success.
    fn erase_sram(&mut self) -> Result<()> {
        let deadline = Instant::now() + ERASE_WINDOW;
        loop {
            self.command_with(ISC_ERASE, &[0x00])?;
            self.idle(100)?;
            std::thread::sleep(ERASE_SETTLE);
            self.wait_not_busy("SRAM erase")?;
            let status = self.read_status()?;
            log::debug!("after ISC_ERASE: {status}");
            if status.fail() {
                return Err(self.fail("SRAM erase"));
            }
            if !status.done() {
                return Ok(());
            }
            if Instant::now() > deadline {
                return Err(PlaceRouteError::BitstreamFailed(format!(
                    "SRAM erase had no effect: DONE is still set (status {status}). \
                     The device is not accepting configuration over JTAG, which \
                     happens when the running design leaves the JTAG port disabled. \
                     Hold the PROGRAMN button (SW2 on the Versa board) until the \
                     erase completes."
                )));
            }
        }
    }

    fn fail(&mut self, step: &str) -> PlaceRouteError {
        let status = self
            .read_status()
            .map(|s| s.to_string())
            .unwrap_or_else(|e| format!("unreadable ({e})"));
        PlaceRouteError::BitstreamFailed(format!("{step} failed, status {status}"))
    }

    /// Load `file` (a Lattice `.bit`) into configuration SRAM.
    ///
    /// Refuses a bitstream built for a different device before changing
    /// anything on the chip.
    ///
    /// `after_erase` runs once the SRAM is confirmed erased. PROGRAMN has to
    /// be high before ISC_DISABLE or the device stays in initialization, so
    /// this is the moment to tell whoever is holding the button to let go.
    pub fn program(&mut self, file: &[u8], after_erase: impl FnOnce()) -> Result<NexusLoadResult> {
        let result = self.load(file, after_erase);
        if result.is_err() {
            // A failed load must not strand the device in configuration mode,
            // where user logic is halted and I/O is tri-stated.
            if let Err(e) = self.leave_isc() {
                log::warn!("Could not leave configuration mode: {e}");
            }
        }
        result
    }

    fn leave_isc(&mut self) -> Result<()> {
        self.command(BYPASS)?;
        self.command(ISC_DISABLE)?;
        self.idle(2)?;
        self.command(BYPASS)?;
        self.idle(100)?;
        self.wait_not_busy("ISC_DISABLE")?;
        self.reset_tap()
    }

    fn load(&mut self, file: &[u8], after_erase: impl FnOnce()) -> Result<NexusLoadResult> {
        let bitstream = parse_bitstream(file)?;
        let idcode = self.read_idcode()?;
        if bitstream.idcode != idcode {
            return Err(PlaceRouteError::BitstreamFailed(format!(
                "Bitstream targets IDCODE 0x{:08x} but the device reports 0x{idcode:08x}",
                bitstream.idcode
            )));
        }

        let start = Instant::now();

        self.command_with(ISC_ENABLE, &[0x00])?;
        self.idle(1000)?;
        self.wait_not_busy("ISC_ENABLE")?;
        let status = self.read_status()?;
        log::debug!("after ISC_ENABLE: {status}");
        if !status.isc_enabled() {
            return Err(self.fail("ISC_ENABLE"));
        }

        // Drop the running configuration and pulse the configuration logic.
        self.command(LSC_REFRESH)?;
        self.idle(2)?;
        self.command_with(LSC_DEVICE_CONTROL, &[0x08])?;
        self.idle(1000)?;
        self.wait_not_busy("configuration reset")?;
        self.command_with(LSC_DEVICE_CONTROL, &[0x00])?;
        self.idle(1000)?;
        self.wait_not_busy("configuration reset release")?;
        log::debug!("after configuration reset: {}", self.read_status()?);

        self.erase_sram()?;
        after_erase();

        self.command(LSC_INIT_ADDRESS)?;
        self.idle(1000)?;

        self.command(LSC_BITSTREAM_BURST)?;
        self.idle(2)?;
        // Lattice shifts configuration data MSB first; JTAG is LSB first.
        let reversed: Vec<u8> = bitstream.payload.iter().map(|b| b.reverse_bits()).collect();
        // Idle -> Select-DR -> Capture-DR -> Shift-DR.
        self.tms(0b001, 3, true)?;
        self.shift(&reversed, None)?;
        self.finish_dr()?;
        self.idle(1000)?;

        let status = self.read_status()?;
        log::debug!("after bitstream: {status}");
        if !status.done() || status.busy() || status.fail() || status.bse_error() != 0 {
            return Err(self.fail("Configuration"));
        }

        self.leave_isc()?;
        let status = self.read_status()?;
        log::debug!("after ISC_DISABLE: {status}");
        if status.isc_enabled() || !status.done() {
            return Err(self.fail("ISC_DISABLE"));
        }

        Ok(NexusLoadResult {
            bytes: bitstream.payload.len(),
            time_ms: start.elapsed().as_millis() as u64,
            status,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_header_payload_and_idcode() {
        let mut file = b"LSCC\xff\x00comment\x00".to_vec();
        let dummy_at = file.len();
        file.extend_from_slice(&[0xFF, 0xFF, 0xFF, 0xBD, 0xB3]);
        file.extend_from_slice(&[0x3B, 0, 0, 0, 0xFF, 0xFF, 0xFF, 0xFF]);
        file.extend_from_slice(&[0xE2, 0, 0, 0, 0x01, 0x0F, 0x40, 0x43]);

        let parsed = parse_bitstream(&file).unwrap();
        assert_eq!(parsed.idcode, LFCPNX_100_IDCODE);
        assert_eq!(parsed.payload, &file[dummy_at..]);
    }

    #[test]
    fn rejects_files_without_a_preamble() {
        assert!(parse_bitstream(b"LSCC\xff\x00no preamble here").is_err());
    }

    #[test]
    fn rejects_files_without_verify_id() {
        let file = [0xFF, 0xFF, 0xFF, 0xBD, 0xB3, 0x3B, 0, 0, 0];
        assert!(parse_bitstream(&file).is_err());
    }

    #[test]
    fn decodes_status_flags() {
        // Value read from an LFCPNX-100 in user mode.
        let s = NexusStatus(0x1501_0000_0100);
        assert!(s.done());
        assert!(!s.isc_enabled());
        assert!(!s.busy());
        assert!(!s.fail());
        assert_eq!(s.bse_error(), 0);

        let s = NexusStatus((1 << 13) | (3 << 24));
        assert!(s.fail());
        assert_eq!(s.bse_error(), 3);
    }
}
