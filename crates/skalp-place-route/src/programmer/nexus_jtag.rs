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
use std::collections::BTreeMap;
use std::time::{Duration, Instant};

/// IDCODE of the LFCPNX-100, as read back from silicon.
pub const LFCPNX_100_IDCODE: u32 = 0x010F_4043;

/// Nexus parts this loader recognizes, from the prjoxide device database.
const KNOWN_CHIPS: [(u32, &str); 5] = [
    (LFCPNX_100_IDCODE, "LFCPNX-100"),
    (0x110F_1043, "LIFCL-40"),
    (0x010F_1043, "LIFCL-40 (engineering sample)"),
    (0x010F_0043, "LIFCL-17"),
    (0x310F_1043, "LFD2NX-40"),
];

/// Name of the Nexus part with this IDCODE, if it is one this loader knows.
pub fn chip_name(idcode: u32) -> Option<&'static str> {
    KNOWN_CHIPS
        .iter()
        .find(|(id, _)| *id == idcode)
        .map(|(_, name)| *name)
}

/// A board this tool knows the layout of.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NexusBoard {
    /// Names accepted by `--board`.
    pub cli_names: &'static [&'static str],
    /// Display name.
    pub name: &'static str,
    /// IDCODE of the FPGA fitted to this board.
    pub idcode: u32,
    /// Where to find the button wired to PROGRAMN.
    pub programn_button: &'static str,
    /// Package of the FPGA fitted to this board.
    pub package: &'static str,
    /// FTDI `(vid, pid, channel)` wired to the FPGA as a UART.
    pub uart: (u16, u16, u8),
    /// Boundary-scan bits confirmed on this board, with what they connect to.
    pub known_pads: &'static [(usize, &'static str)],
}

/// Boards with a confirmed PROGRAMN button. Add an entry for each new board
/// rather than assuming the chip implies the board: different boards fit the
/// same FPGA and wire the button differently.
pub const NEXUS_BOARDS: [NexusBoard; 1] = [NexusBoard {
    cli_names: &["certuspro-versa", "lfcpnx-versa"],
    name: "CertusPro-NX Versa",
    idcode: LFCPNX_100_IDCODE,
    programn_button: "SW2, labelled PROGRAMN, just below the Raspberry Pi connector",
    package: "LFG672",
    uart: (0x0403, 0x6010, 1),
    known_pads: &[(533, "LED2 (pad PR31A)")],
}];

impl NexusBoard {
    /// What a boundary-scan bit connects to on this board, if confirmed.
    pub fn pad_name(&self, bit: usize) -> Option<&'static str> {
        self.known_pads
            .iter()
            .find(|(b, _)| *b == bit)
            .map(|(_, name)| *name)
    }

    /// What a boundary-scan bit reads: the board's label if it has one, and
    /// the device's pad and ball name otherwise.
    pub fn pad_label(&self, bit: usize) -> Option<String> {
        self.pad_name(bit)
            .map(str::to_string)
            .or_else(|| crate::device::nexus::sites::boundary_pad_names(self.package).remove(&bit))
    }

    /// Look up a board by the name given to `--board`.
    pub fn by_name(name: &str) -> Option<&'static NexusBoard> {
        let name = name.to_lowercase();
        NEXUS_BOARDS
            .iter()
            .find(|b| b.cli_names.contains(&name.as_str()))
    }
}

fn describe_idcode(idcode: u32) -> String {
    match chip_name(idcode) {
        Some(name) => format!("{name} (IDCODE 0x{idcode:08x})"),
        None => format!("unknown device (IDCODE 0x{idcode:08x})"),
    }
}

const FTDI_VID: u16 = 0x0403;
const FT2232H_PID: u16 = 0x6010;

/// JTAG clock used for configuration. 6 MHz loads the LFCPNX-100 in a few
/// seconds and has margin on the evaluation board's level shifters.
const JTAG_HZ: u32 = 6_000_000;

/// Pause after the erase; LFCPNX-100 needs 4.87 ms (FPGA-TN-02099 table 6.7).
const ERASE_SETTLE: Duration = Duration::from_millis(50);

/// How long to wait for someone to tap PROGRAMN on a locked device.
const PROGRAMN_WAIT: Duration = Duration::from_secs(60);
/// Consecutive clear status reads that show PROGRAMN has been released.
const PROGRAMN_CLEAR_READS: u32 = 5;
const PROGRAMN_POLL: Duration = Duration::from_millis(100);

/// Bytes in each half of the boundary-scan length probe. The LFCPNX-100
/// register is 590 bits, and the register must be shorter than one half.
const BOUNDARY_PROBE_BYTES: usize = 256;
/// Pause between boundary-scan samples while watching pads.
const PAD_WATCH_POLL: Duration = Duration::from_millis(5);
/// Interval spread below which a pad counts as toggling regularly.
const REGULAR_INTERVAL_SPREAD: f64 = 0.2;

/// Bytes sent per progress report during the bitstream burst.
const BURST_CHUNK: usize = 0x1_0000;

// Nexus configuration opcodes.
const IDCODE_PUB: u8 = 0xE0;
const SAMPLE_PRELOAD: u8 = 0x1C;
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

    /// Plain-language description of any error the status register reports.
    pub fn error_summary(self) -> Option<String> {
        let mut errors: Vec<&str> = Vec::new();
        match self.bse_error() {
            0 => {}
            1 => errors.push("bitstream is for a different device (ID error)"),
            2 => errors.push("bitstream contains an illegal command"),
            3 => errors.push("bitstream failed its CRC check, so it is corrupted"),
            4 => errors.push("no valid bitstream preamble found"),
            5 => errors.push("configuration was aborted"),
            6 => errors.push("bitstream overflowed the data buffer"),
            7 => errors.push("bitstream is larger than this device's SRAM"),
            8 | 9 => errors.push("bitstream authentication failed"),
            10 => errors.push("bitstream engine timed out"),
            _ => errors.push("unrecognized bitstream engine error"),
        }
        if self.0 & (1 << 29) != 0 && self.bse_error() != 1 {
            errors.push("device ID check failed");
        }
        if self.0 & (1 << 30) != 0 {
            errors.push("device rejected a command as invalid");
        }
        if self.fail() && errors.is_empty() {
            errors.push("device reported a failed operation");
        }
        (!errors.is_empty()).then(|| errors.join("; "))
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

/// Progress of an SRAM load, reported as it happens.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LoadEvent {
    /// Erasing configuration SRAM.
    Erasing,
    /// The running design is blocking JTAG configuration; someone needs to tap
    /// PROGRAMN within `timeout`.
    NeedsProgramn { timeout: Duration },
    /// PROGRAMN was seen and the design has cleared.
    ProgramnSeen,
    /// `sent` of `total` bitstream bytes have been shifted in.
    Loading { sent: usize, total: usize },
    /// Bitstream fully shifted; checking the device's verdict.
    Verifying,
}

/// Outcome of a successful SRAM load.
#[derive(Debug, Clone)]
pub struct NexusLoadResult {
    /// IDCODE of the configured device.
    pub idcode: u32,
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
    let bad = |why: &str| {
        PlaceRouteError::ProgrammingFailed(format!("Not a Lattice Nexus .bit file: {why}"))
    };

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

/// Confirm a bitstream was built for the connected device.
pub fn check_target(bitstream_idcode: u32, device_idcode: u32) -> Result<()> {
    if bitstream_idcode == device_idcode {
        return Ok(());
    }
    Err(PlaceRouteError::ProgrammingFailed(format!(
        "This bitstream was built for {} but the connected device is {}. \
         Nothing was changed on the device.",
        describe_idcode(bitstream_idcode),
        describe_idcode(device_idcode)
    )))
}

/// What a bulk erase achieved.
#[derive(Clone, Copy, PartialEq, Eq)]
enum EraseOutcome {
    Erased,
    /// DONE stayed set: the device ignored the erase.
    Blocked,
}

/// One pad seen changing while watching boundary scan.
#[derive(Debug, Clone, PartialEq)]
pub struct PadActivity {
    /// Boundary-scan bit index.
    pub bit: usize,
    /// Changes seen.
    pub changes: usize,
    /// Mean seconds between changes, once at least two were seen.
    pub mean_interval: Option<f64>,
    /// The changes were evenly spaced, like a clocked output.
    pub regular: bool,
}

impl PadActivity {
    fn from_change_times(bit: usize, times: &[f64]) -> Self {
        let gaps: Vec<f64> = times.windows(2).map(|w| w[1] - w[0]).collect();
        if gaps.is_empty() {
            return Self {
                bit,
                changes: times.len(),
                mean_interval: None,
                regular: false,
            };
        }
        let mean = gaps.iter().sum::<f64>() / gaps.len() as f64;
        let variance = gaps.iter().map(|g| (g - mean).powi(2)).sum::<f64>() / gaps.len() as f64;
        Self {
            bit,
            changes: times.len(),
            mean_interval: Some(mean),
            regular: gaps.len() >= 2 && variance.sqrt() < mean * REGULAR_INTERVAL_SPREAD,
        }
    }
}

/// What a boundary-scan watch saw.
#[derive(Debug, Clone, PartialEq)]
pub struct PadWatch {
    /// Boundary-scan register length.
    pub bits: usize,
    /// Samples taken.
    pub samples: usize,
    /// Seconds watched.
    pub seconds: f64,
    /// Pads that changed, by bit index.
    pub active: Vec<PadActivity>,
}

/// Work out a shift register's length from a probe that shifted equal runs of
/// 0s then 1s through it: the 1s reach TDO delayed by exactly its length.
fn boundary_length(rx: &[u8]) -> Option<usize> {
    let total = rx.len() * 8;
    let half = total / 2;
    let bit = |i: usize| (rx[i / 8] >> (i % 8)) & 1 == 1;
    let first_one = (half..total).rev().take_while(|&i| bit(i)).last()?;
    match first_one - half {
        0 => None,
        length => Some(length),
    }
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
    PlaceRouteError::ProgrammingFailed(format!("{what}: {e}"))
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
                PlaceRouteError::ProgrammingFailed(
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
            return Err(PlaceRouteError::ProgrammingFailed(format!(
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

    /// Measure the boundary-scan register, whose length varies by device.
    fn boundary_scan_bits(&mut self) -> Result<usize> {
        let mut tx = vec![0u8; BOUNDARY_PROBE_BYTES];
        tx.resize(2 * BOUNDARY_PROBE_BYTES, 0xFF);
        let mut rx = vec![0u8; tx.len()];
        self.shift_ir(SAMPLE_PRELOAD, AfterIr::ShiftDr)?;
        self.shift(&tx, Some(&mut rx))?;
        self.finish_dr()?;
        boundary_length(&rx).ok_or_else(|| {
            PlaceRouteError::ProgrammingFailed(
                "Could not measure the boundary-scan register".to_string(),
            )
        })
    }

    /// Watch every pad through boundary scan for `duration`.
    ///
    /// SAMPLE only captures pad values, so this does not disturb the design.
    pub fn watch_pads(&mut self, duration: Duration) -> Result<PadWatch> {
        let bits = self.boundary_scan_bits()?;
        let bytes = bits.div_ceil(8);
        let mut sample = vec![0u8; bytes];
        let start = Instant::now();
        self.query(SAMPLE_PRELOAD, &mut sample)?;
        let mut changes: BTreeMap<usize, Vec<f64>> = BTreeMap::new();
        let mut samples = 1;
        while start.elapsed() < duration {
            std::thread::sleep(PAD_WATCH_POLL);
            let mut next = vec![0u8; bytes];
            self.query(SAMPLE_PRELOAD, &mut next)?;
            samples += 1;
            let now = start.elapsed().as_secs_f64();
            for bit in 0..bits {
                if ((sample[bit / 8] ^ next[bit / 8]) >> (bit % 8)) & 1 == 1 {
                    changes.entry(bit).or_default().push(now);
                }
            }
            sample = next;
        }
        Ok(PadWatch {
            bits,
            samples,
            seconds: start.elapsed().as_secs_f64(),
            active: changes
                .into_iter()
                .map(|(bit, times)| PadActivity::from_change_times(bit, &times))
                .collect(),
        })
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
                return Err(PlaceRouteError::ProgrammingFailed(format!(
                    "Timed out waiting for {what}"
                )));
            }
        }
    }

    /// Read the IDCODE and confirm a device is actually answering.
    pub fn identify(&mut self) -> Result<u32> {
        let idcode = self.read_idcode()?;
        if idcode == 0 || idcode == u32::MAX {
            return Err(PlaceRouteError::ProgrammingFailed(format!(
                "No device answered on the JTAG chain (read 0x{idcode:08x}). \
                 Check that the board is powered and the USB cable is connected."
            )));
        }
        Ok(idcode)
    }

    /// Reinitialize the device, as pulsing PROGRAMN does: it clears the running
    /// design and boots from SPI flash if the flash holds a valid image.
    pub fn refresh(&mut self) -> Result<NexusStatus> {
        self.command(LSC_REFRESH)?;
        self.idle(1000)?;
        // Initialization can take seconds with a large image in flash.
        std::thread::sleep(Duration::from_secs(2));
        self.reset_tap()?;
        self.wait_not_busy("device refresh")?;
        self.read_status()
    }

    /// Enter in-system configuration mode and reset the configuration logic.
    fn enter_config(&mut self) -> Result<()> {
        self.command_with(ISC_ENABLE, &[0x00])?;
        self.idle(1000)?;
        self.wait_not_busy("ISC_ENABLE")?;
        let status = self.read_status()?;
        log::debug!("after ISC_ENABLE: {status}");
        if !status.isc_enabled() {
            return Err(self.fail("Entering configuration mode"));
        }

        self.command(LSC_REFRESH)?;
        self.idle(2)?;
        self.command_with(LSC_DEVICE_CONTROL, &[0x08])?;
        self.idle(1000)?;
        self.wait_not_busy("configuration reset")?;
        self.command_with(LSC_DEVICE_CONTROL, &[0x00])?;
        self.idle(1000)?;
        self.wait_not_busy("configuration reset release")?;
        log::debug!("after configuration reset: {}", self.read_status()?);
        Ok(())
    }

    /// Bulk-erase configuration SRAM and report whether it took effect.
    ///
    /// DONE and FAIL must both read 0 after the erase (FPGA-TN-02099, table
    /// 6.16 note 2). This is the only point in the load where the device
    /// proves it is acting on configuration commands: a device that ignores
    /// them still reports DONE=1 after the burst, which is indistinguishable
    /// from success.
    fn erase_sram(&mut self) -> Result<EraseOutcome> {
        self.command_with(ISC_ERASE, &[0x00])?;
        self.idle(100)?;
        std::thread::sleep(ERASE_SETTLE);
        self.wait_not_busy("SRAM erase")?;
        let status = self.read_status()?;
        log::debug!("after ISC_ERASE: {status}");
        if status.fail() {
            return Err(self.fail("SRAM erase"));
        }
        Ok(if status.done() {
            EraseOutcome::Blocked
        } else {
            EraseOutcome::Erased
        })
    }

    /// Wait for PROGRAMN to clear the running design and be released.
    ///
    /// DONE falls as soon as the button goes down, so the status has to stay
    /// clear for several reads before configuration can safely restart.
    fn wait_for_programn(&mut self) -> Result<()> {
        let deadline = Instant::now() + PROGRAMN_WAIT;
        let mut clear_reads = 0;
        while clear_reads < PROGRAMN_CLEAR_READS {
            if Instant::now() > deadline {
                return Err(PlaceRouteError::ProgrammingFailed(format!(
                    "Timed out after {} s waiting for PROGRAMN. The running design \
                     is still blocking JTAG configuration and was not changed.",
                    PROGRAMN_WAIT.as_secs()
                )));
            }
            let status = self.read_status()?;
            clear_reads = if status.done() || status.busy() {
                0
            } else {
                clear_reads + 1
            };
            std::thread::sleep(PROGRAMN_POLL);
        }
        Ok(())
    }

    fn fail(&mut self, step: &str) -> PlaceRouteError {
        let detail = match self.read_status() {
            Ok(status) => match status.error_summary() {
                Some(summary) => format!("{summary} (status {status})"),
                None => format!("status {status}"),
            },
            Err(e) => format!("status unreadable: {e}"),
        };
        PlaceRouteError::ProgrammingFailed(format!("{step} failed: {detail}"))
    }

    /// Load `file` (a Lattice `.bit`) into configuration SRAM.
    ///
    /// Refuses a bitstream built for a different device before changing
    /// anything on the chip. `on_event` hears about each stage, including the
    /// point at which someone needs to tap PROGRAMN.
    pub fn program(
        &mut self,
        file: &[u8],
        mut on_event: impl FnMut(LoadEvent),
    ) -> Result<NexusLoadResult> {
        let bitstream = parse_bitstream(file)?;
        let idcode = self.identify()?;
        check_target(bitstream.idcode, idcode)?;

        let result = self.load(&bitstream, idcode, &mut on_event);
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
        self.wait_not_busy("leaving configuration mode")?;
        self.reset_tap()
    }

    /// Shift the bitstream into the current shift state, reporting progress.
    fn burst(&mut self, data: &[u8], on_event: &mut impl FnMut(LoadEvent)) -> Result<()> {
        let (last, head) = data.split_last().expect("empty bitstream payload");
        let mut sent = 0;
        for chunk in head.chunks(BURST_CHUNK) {
            let n = (chunk.len() - 1) as u16;
            let mut cmd = Vec::with_capacity(chunk.len() + 3);
            cmd.extend_from_slice(&[MPSSE_BYTES_OUT, (n & 0xFF) as u8, (n >> 8) as u8]);
            cmd.extend_from_slice(chunk);
            self.write(&cmd)?;
            sent += chunk.len();
            on_event(LoadEvent::Loading {
                sent,
                total: data.len(),
            });
        }
        self.shift(std::slice::from_ref(last), None)?;
        on_event(LoadEvent::Loading {
            sent: data.len(),
            total: data.len(),
        });
        Ok(())
    }

    fn load(
        &mut self,
        bitstream: &NexusBitstream<'_>,
        idcode: u32,
        on_event: &mut impl FnMut(LoadEvent),
    ) -> Result<NexusLoadResult> {
        let start = Instant::now();

        self.enter_config()?;
        on_event(LoadEvent::Erasing);
        if self.erase_sram()? == EraseOutcome::Blocked {
            self.leave_isc()?;
            on_event(LoadEvent::NeedsProgramn {
                timeout: PROGRAMN_WAIT,
            });
            self.wait_for_programn()?;
            on_event(LoadEvent::ProgramnSeen);
            self.enter_config()?;
            on_event(LoadEvent::Erasing);
            if self.erase_sram()? == EraseOutcome::Blocked {
                return Err(self.fail("SRAM erase"));
            }
        }

        self.command(LSC_INIT_ADDRESS)?;
        self.idle(1000)?;

        self.command(LSC_BITSTREAM_BURST)?;
        self.idle(2)?;
        // Lattice shifts configuration data MSB first; JTAG is LSB first.
        let reversed: Vec<u8> = bitstream.payload.iter().map(|b| b.reverse_bits()).collect();
        // Idle -> Select-DR -> Capture-DR -> Shift-DR.
        self.tms(0b001, 3, true)?;
        self.burst(&reversed, on_event)?;
        self.finish_dr()?;
        self.idle(1000)?;

        on_event(LoadEvent::Verifying);
        let status = self.read_status()?;
        log::debug!("after bitstream: {status}");
        if !status.done() || status.busy() || status.fail() || status.bse_error() != 0 {
            return Err(self.fail("Configuration"));
        }

        self.leave_isc()?;
        let status = self.read_status()?;
        log::debug!("after ISC_DISABLE: {status}");
        if status.isc_enabled() || !status.done() {
            return Err(self.fail("Leaving configuration mode"));
        }

        Ok(NexusLoadResult {
            idcode,
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
    fn measures_boundary_register_length() {
        // A 590-bit register: captured pad bits, then the 0s, then the 1s.
        let (bytes, length) = (256, 590);
        let mut rx = vec![0u8; 2 * bytes];
        let high = |i: usize| i < length && i.is_multiple_of(3) || i >= length + bytes * 8;
        for i in 0..rx.len() * 8 {
            if high(i) {
                rx[i / 8] |= 1 << (i % 8);
            }
        }
        assert_eq!(boundary_length(&rx), Some(length));
    }

    #[test]
    fn rejects_a_chain_with_no_delay() {
        assert_eq!(boundary_length(&[0x00, 0xFF]), None);
    }

    #[test]
    fn evenly_spaced_changes_are_regular() {
        let pad = PadActivity::from_change_times(7, &[0.0, 0.9, 1.8, 2.75]);
        assert!(pad.regular);
        assert!((pad.mean_interval.unwrap() - 0.9167).abs() < 0.001);
    }

    #[test]
    fn scattered_changes_are_not_regular() {
        let pad = PadActivity::from_change_times(7, &[0.0, 0.01, 0.5, 0.52, 1.5]);
        assert!(!pad.regular);
    }

    #[test]
    fn a_single_change_has_no_interval() {
        let pad = PadActivity::from_change_times(7, &[1.0]);
        assert_eq!(pad.mean_interval, None);
        assert!(!pad.regular);
    }

    #[test]
    fn finds_boards_by_cli_name() {
        let board = NexusBoard::by_name("CertusPro-Versa").unwrap();
        assert_eq!(board.idcode, LFCPNX_100_IDCODE);
        assert!(board.programn_button.contains("SW2"));
        assert_eq!(NexusBoard::by_name("icebreaker"), None);
    }

    #[test]
    fn names_known_chips() {
        assert_eq!(chip_name(LFCPNX_100_IDCODE), Some("LFCPNX-100"));
        assert_eq!(chip_name(0xDEAD_BEEF), None);
    }

    #[test]
    fn accepts_a_matching_target() {
        assert!(check_target(LFCPNX_100_IDCODE, LFCPNX_100_IDCODE).is_ok());
    }

    #[test]
    fn explains_a_mismatched_target() {
        let err = check_target(0x110F_1043, LFCPNX_100_IDCODE)
            .unwrap_err()
            .to_string();
        assert!(err.contains("LIFCL-40"), "{err}");
        assert!(err.contains("LFCPNX-100"), "{err}");
    }

    #[test]
    fn summarizes_status_errors() {
        assert_eq!(NexusStatus(0x1501_0000_0100).error_summary(), None);
        let crc = NexusStatus(3 << 24).error_summary().unwrap();
        assert!(crc.contains("CRC"), "{crc}");
        let id = NexusStatus(1 << 29).error_summary().unwrap();
        assert!(id.contains("ID"), "{id}");
        let fail = NexusStatus(1 << 13).error_summary().unwrap();
        assert!(fail.contains("failed"), "{fail}");
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
