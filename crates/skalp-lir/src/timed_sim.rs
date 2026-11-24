//! Event-driven gate-level simulation with per-pin delays
//!
//! The cycle-based gate simulator evaluates combinational logic with zero
//! delay between clock edges, so it cannot see a race: a self-timed block
//! whose control releases a request too early, a register that captures a
//! value before it has settled, or a hold violation inside a block clocked
//! by its own matched delay. This simulator runs a gate netlist in time,
//! with the delays a [`BundleDelays`] model gives it: library estimates
//! before placement, routed delays after.
//!
//! # Model
//!
//! * Values are `0`, `1` or `X` (unknown). Everything starts unknown except
//!   registers, which start at their initial value, and constants.
//! * A connection from a net to a cell pin delays the net by its wire
//!   delay, and each arc of a cell, from an input to an output, by the arc's
//!   delay: an output at time `t` is the cell's function of every input as
//!   it was one wire and one arc delay earlier (a pure, or transport,
//!   delay). Glitches propagate, as they can in silicon; pulses of zero
//!   width (changes that cancel at one instant) do not.
//! * A register samples its data pins at its clock pin's active edge. A
//!   data pin that changed within its setup time before the edge, or that
//!   changes within its hold time after it, makes the register capture `X`,
//!   and the violation is recorded. The output follows one clock-to-output
//!   delay after the edge.
//! * Threshold gates (`THmn`) and latches keep state; RAM blocks and
//!   distributed RAMs keep their memory.
//!
//! An unknown value is pessimistic: an output that depends on an unknown
//! input is unknown, unless every value the unknown inputs could take gives
//! the same output.
//!
//! # Performance
//!
//! The simulation is laid out for large netlists and long runs:
//!
//! * Each combinational output is compiled to a truth table when the
//!   simulation is built. Outputs of one cell whose arcs have the same
//!   delays share one *node*, whose inputs are a pair of bit masks (value
//!   and unknown); a change is one bit update and one table lookup. Only
//!   cells too wide for a table (hard multipliers) are evaluated through
//!   their primitive.
//! * Wire and arc delays are added together when the simulation is built,
//!   so a net change is one event per node input it reaches, straight into
//!   that input. Each fanout entry is a target and a delay, stored in one
//!   flat array per net (compressed rows).
//! * Events are 8 bytes and sit in a timing wheel of picosecond slots with
//!   a bitmap of the occupied ones; events beyond the wheel's horizon wait
//!   in a heap. Slot buffers are reused, so a steady run allocates nothing.
//! * Each instant runs in delta cycles: apply every event of the instant,
//!   evaluate each touched node once, then commit the nets that changed.

use crate::gate_netlist::{
    Cell, CellId, GateNetId, GateNetlist, BLACKBOX_OUTPUTS_PARAM, EXCLUSIVE_OP, FF_INIT_PARAM,
};
use crate::lir::PrimitiveType;
use crate::ncl_bundle::BundleDelays;
use crate::primitive_eval::{evaluate_primitive, primitive_for_cell};
use crate::tech_library::CellFunction;
use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap};
use std::fmt;

/// A logic value
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Logic {
    Zero = 0,
    One = 1,
    X = 2,
}

impl Logic {
    /// The value of a known boolean
    pub fn from_bool(value: bool) -> Self {
        if value {
            Logic::One
        } else {
            Logic::Zero
        }
    }

    /// The boolean value, `None` if unknown
    pub fn to_bool(self) -> Option<bool> {
        match self {
            Logic::Zero => Some(false),
            Logic::One => Some(true),
            Logic::X => None,
        }
    }

    /// What both `self` and `other` could be: themselves if equal, else
    /// unknown
    fn merge(self, other: Logic) -> Logic {
        if self == other {
            self
        } else {
            Logic::X
        }
    }

    fn from_u8(v: u8) -> Self {
        match v {
            0 => Logic::Zero,
            1 => Logic::One,
            _ => Logic::X,
        }
    }
}

impl fmt::Display for Logic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Logic::Zero => "0",
            Logic::One => "1",
            Logic::X => "x",
        })
    }
}

/// What went wrong at a clocked cell's inputs
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ViolationKind {
    /// A data pin changed less than its setup time before the active edge
    Setup,
    /// A data pin changed less than its hold time after the active edge
    Hold,
    /// The clock pin went to or came from an unknown value
    UnknownClock,
}

/// A vendor block the simulator has no model of (such as a PLL), kept as
/// a blackbox: nothing in the simulation drives its outputs, which stay
/// unknown unless the caller drives them ([`TimedSim::add_clock`],
/// [`TimedSim::set`])
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Blackbox {
    /// The cell
    pub cell: CellId,
    /// Its outputs
    pub outputs: Vec<GateNetId>,
}

/// Whether `cell` is a vendor block with no model here: a blackbox cell
/// that no function, truth table or hard-block port map describes
fn is_blackbox(cell: &Cell) -> bool {
    (cell.cell_type.starts_with("BLACKBOX_")
        || cell.parameters.contains_key(BLACKBOX_OUTPUTS_PARAM))
        && cell.function.is_none()
        && cell.lut_init.is_none()
        && cell.dsp_ports().is_none()
}

/// Two inputs of a latch built in one cell (its output wired back to its
/// own inputs, such as an arbiter's mutex) changing what it holds less
/// than one trip round its loop apart: a race the latch decides, as a
/// mutex is there to. In hardware it may go metastable for a while; an
/// arbiter waits for that to settle before it uses the decision.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LatchRace {
    /// When the second change reached the latch, in picoseconds
    pub time_ps: i64,
    /// The latch
    pub cell: CellId,
    /// How long before the first change would have come round the loop
    /// the second came, in picoseconds
    pub margin_ps: i64,
}

/// Two grants of one exclusive merge high at once
/// (`skalp_frontend::ncl_streams`, `match exclusive(..)`): the design
/// promised that at most one of the merged channels holds a value at a
/// time, and broke the promise. In hardware the block would take both
/// values in one step, or lose one.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExclusiveClash {
    /// When the second grant rose, in picoseconds
    pub time_ps: i64,
    /// The merge: its grant buffers' path, without the `.excl<arm>`
    pub merge: String,
    /// The arms whose grants were high, lowest first
    pub arms: Vec<usize>,
}

/// A timing violation seen at a clocked cell's pins
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Violation {
    /// When it was seen, in picoseconds
    pub time_ps: i64,
    pub kind: ViolationKind,
    /// The clocked cell
    pub cell: CellId,
    /// The data pin (input index) that changed, if any
    pub input: Option<usize>,
    /// How far inside the window the change was, in picoseconds
    pub margin_ps: i64,
}

/// Why a simulation could not be built or run
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TimedSimError {
    /// Cells of a kind this simulator does not model (type and path)
    UnsupportedCells(Vec<(String, String)>),
    /// Values kept changing without time advancing: a loop of zero delay
    ZeroDelayLoop { time_ps: i64 },
}

impl fmt::Display for TimedSimError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TimedSimError::UnsupportedCells(cells) => {
                write!(
                    f,
                    "timed simulation does not model {} cell(s):",
                    cells.len()
                )?;
                for (ty, path) in cells.iter().take(10) {
                    write!(f, " {path} ({ty})")?;
                }
                Ok(())
            }
            TimedSimError::ZeroDelayLoop { time_ps } => write!(
                f,
                "values keep changing at {time_ps} ps without time advancing (a loop with no delay)"
            ),
        }
    }
}

impl std::error::Error for TimedSimError {}

/// Distributed RAM layout: inputs `[DI0..3, WAD0..3, WRE, RAD0..3]`
const DIST_RAM_WIDTH: usize = 4;
const DIST_RAM_WRITE_ENABLE: usize = 2 * DIST_RAM_WIDTH;
const DIST_RAM_READ_ADDRESS: usize = DIST_RAM_WRITE_ENABLE + 1;

/// Most inputs a node compiles to a truth table
const MAX_TABLE_INPUTS: usize = 12;

/// Most inputs a node keeps as bit masks
const MAX_MASK_INPUTS: usize = 64;

/// The most unknown inputs a wide cell is evaluated for every value of;
/// with more, its outputs are unknown
const MAX_ENUMERATED_UNKNOWNS: usize = 8;

/// Most delta cycles at one instant before the run stops as a loop
const MAX_DELTAS: u32 = 10_000;

/// Violations kept in full (all are counted)
const MAX_KEPT_VIOLATIONS: usize = 10_000;
/// A net that is no exclusive merge's grant
const NO_MERGE: u32 = u32::MAX;

/// Wheel size in picosecond slots (a power of two)
const WHEEL_BITS: u32 = 16;
const WHEEL_SLOTS: usize = 1 << WHEEL_BITS;
const WHEEL_MASK: i64 = (WHEEL_SLOTS as i64) - 1;

/// No net
const NO_NET: u32 = u32::MAX;

// ---------------------------------------------------------------------------
// Events and the timing wheel
// ---------------------------------------------------------------------------

/// Kinds of event target, in the top bits of [`Event::target`]
const TARGET_SHIFT: u32 = 28;
const TARGET_INDEX: u32 = (1 << TARGET_SHIFT) - 1;
/// A node input slot
const T_NODE_INPUT: u32 = 0;
/// A clocked cell's data pin slot
const T_SEQ_PIN: u32 = 1;
/// A clocked cell's clock pin
const T_SEQ_CLOCK: u32 = 2;
/// A net driven by a register, memory or the testbench
const T_NET_DRIVE: u32 = 3;
/// A delayed write into a distributed RAM
const T_MEMORY_WRITE: u32 = 4;
/// A free-running clock's next toggle
const T_CLOCK_TICK: u32 = 5;
/// A node evaluates its outputs (at power-up)
const T_NODE_EVAL: u32 = 6;
/// A node output's change reaches its net, unless a later evaluation of
/// the node has replaced it (inertial delay)
const T_OUTPUT_COMMIT: u32 = 7;

/// One event: a target and a value (8 bytes)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Event {
    target: u32,
    value: u8,
    /// Which evaluation of a node output a commit is for
    generation: u16,
}

impl Event {
    fn new(kind: u32, index: u32, value: Logic) -> Self {
        debug_assert!(index <= TARGET_INDEX);
        Self {
            target: (kind << TARGET_SHIFT) | index,
            value: value as u8,
            generation: 0,
        }
    }
    fn kind(self) -> u32 {
        self.target >> TARGET_SHIFT
    }
    fn index(self) -> usize {
        (self.target & TARGET_INDEX) as usize
    }
    fn logic(self) -> Logic {
        Logic::from_u8(self.value)
    }
}

/// Picosecond timing wheel with an overflow heap
struct Wheel {
    slots: Vec<Vec<Event>>,
    /// Which slots hold events
    occupied: Vec<u64>,
    /// Events beyond the horizon: time, order, event
    far: BinaryHeap<Reverse<(i64, u64, Event)>>,
    far_seq: u64,
    /// Every event in the wheel is in `[now, now + WHEEL_SLOTS)`
    now: i64,
    pending: usize,
}

impl Wheel {
    fn new() -> Self {
        Self {
            slots: (0..WHEEL_SLOTS).map(|_| Vec::new()).collect(),
            occupied: vec![0; WHEEL_SLOTS / 64],
            far: BinaryHeap::new(),
            far_seq: 0,
            now: 0,
            pending: 0,
        }
    }

    fn push(&mut self, time: i64, event: Event) {
        debug_assert!(time >= self.now);
        if time - self.now < WHEEL_SLOTS as i64 {
            let slot = (time & WHEEL_MASK) as usize;
            self.slots[slot].push(event);
            self.occupied[slot / 64] |= 1 << (slot % 64);
            self.pending += 1;
        } else {
            self.far_seq += 1;
            self.far.push(Reverse((time, self.far_seq, event)));
        }
    }

    /// Time of the earliest event, if any
    fn next_time(&self) -> Option<i64> {
        if self.pending > 0 {
            let start = (self.now & WHEEL_MASK) as usize;
            let words = WHEEL_SLOTS / 64;
            let first_word = start / 64;
            for step in 0..=words {
                let w = (first_word + step) % words;
                let mut bits = self.occupied[w];
                if step == 0 {
                    bits &= !0u64 << (start % 64);
                } else if step == words {
                    bits &= (1u64 << (start % 64)).wrapping_sub(1);
                }
                if bits != 0 {
                    let slot = w * 64 + bits.trailing_zeros() as usize;
                    let offset = (slot as i64 - (self.now & WHEEL_MASK)) & WHEEL_MASK;
                    return Some(self.now + offset);
                }
            }
        }
        self.far.peek().map(|Reverse((t, _, _))| *t)
    }

    /// Move to `time` (no earlier than now) and bring far events inside the
    /// new horizon into the wheel
    fn advance(&mut self, time: i64) {
        debug_assert!(time >= self.now);
        self.now = time;
        while let Some(&Reverse((t, _, event))) = self.far.peek() {
            if t - self.now >= WHEEL_SLOTS as i64 {
                break;
            }
            self.far.pop();
            let slot = (t & WHEEL_MASK) as usize;
            self.slots[slot].push(event);
            self.occupied[slot / 64] |= 1 << (slot % 64);
            self.pending += 1;
        }
    }

    /// Take the events at the current time into `into` (cleared first),
    /// leaving that slot's buffer for reuse
    fn take_now(&mut self, into: &mut Vec<Event>) {
        into.clear();
        let slot = (self.now & WHEEL_MASK) as usize;
        std::mem::swap(&mut self.slots[slot], into);
        self.occupied[slot / 64] &= !(1 << (slot % 64));
        self.pending -= into.len();
    }
}

// ---------------------------------------------------------------------------
// Netlist compilation
// ---------------------------------------------------------------------------

/// How a node computes its outputs
#[derive(Debug, Clone)]
enum NodeKind {
    /// Each output is a truth table over the node's inputs
    Table,
    /// Threshold gate: 1 once at least `m` inputs are 1, 0 once all are 0,
    /// else unchanged
    Threshold { m: u8 },
    /// Level-sensitive latch, inputs `[en, d]`
    Latch,
    /// Set-reset latch, inputs `[s, r]`, reset first
    SrLatch,
    /// Read port of a distributed RAM (inputs are the read address)
    DistRamRead { seq: u32 },
    /// Evaluated through its primitive (too wide for a table); `pins` maps
    /// node inputs to the cell's inputs
    Wide {
        ptype: PrimitiveType,
        cell_inputs: u16,
        pins: Vec<u16>,
        outputs: Vec<u16>,
    },
}

/// A group of a cell's outputs that see the cell's inputs after the same
/// delays
#[derive(Debug, Clone)]
struct Node {
    kind: NodeKind,
    inputs: u16,
    /// Input values and unknown flags, for nodes of up to 64 inputs
    val: u64,
    x: u64,
    /// Offset of the node's input values in `wide_inputs`, for wider nodes
    wide: u32,
    /// The node's outputs, in `node_outputs`
    out_start: u32,
    out_len: u16,
    /// Shortest arc delay. The node's outputs take this long to follow an
    /// evaluation (the rest of each arc is on the fanout into the node), so
    /// a pulse at an output narrower than it never reaches the net: the
    /// gate's inertia, as a real gate swallows pulses shorter than its delay
    inertia_ps: u32,
    dirty: bool,
}

/// One output of a node
#[derive(Debug, Clone, Copy)]
struct NodeOutput {
    /// Canonical net, `NO_NET` if it drives nothing
    net: u32,
    /// Offset of its truth table in `tables` (table nodes)
    table: u32,
    /// From the latest evaluation (its state, for stateful nodes)
    value: Logic,
    /// On the net, or on its way there
    committed: Logic,
    /// Counts evaluations that changed `value`, so a commit that a later
    /// evaluation replaced is dropped
    generation: u16,
}

/// A table node whose output feeds back to its own inputs and holds a
/// value through that loop: a latch made of one cell (a mutex, a set/reset
/// or C-element latch built in a LUT)
///
/// Its feedback inputs are read from the node's own latest value, not from
/// their nets, so the latch holds what it decided at once. Read from the
/// nets, a change of another input that comes before the loop has carried
/// the last change round leaves a pulse circling the loop for ever: every
/// delay is the same each trip, where in hardware the pulse narrows and
/// dies, or the latch goes metastable and settles. Such a change is a race
/// ([`LatchRace`]). It always settles to a definite value, as hardware
/// does (an unknown latch would hold unknown for ever, where an arbiter
/// waits out the settling): with [`Metastability::Unknown`] the first
/// change wins, with [`Metastability::Resolve`] either, at random.
#[derive(Debug, Clone, Copy)]
struct FeedbackLoop {
    /// The latch's cell
    cell: u32,
    /// The node's input bits that read its own output
    mask: u64,
    /// From the node deciding a new value to every feedback input seeing it
    loop_ps: i64,
    /// When the node last decided a new value, and the value before it
    changed_ps: i64,
    before: Logic,
}

/// The node is not a [`FeedbackLoop`]
const NO_LOOP: u32 = u32::MAX;

/// Whether a table over `inputs` inputs, with the inputs in `mask` tied
/// together as one feedback value, can hold a value through them and never
/// oscillates: with every other input fixed, it is either constant or
/// follows the feedback, never its inverse
fn holds_through_feedback(table: &[u64], inputs: usize, mask: u64) -> bool {
    let bit = |index: u64| (table[(index / 64) as usize] >> (index % 64)) & 1 == 1;
    let mut holds = false;
    for index in 0..1u64 << inputs {
        if index & mask != 0 {
            continue;
        }
        match (bit(index), bit(index | mask)) {
            (true, false) => return false,
            (false, true) => holds = true,
            _ => {}
        }
    }
    holds
}

/// Which edge of its clock a register samples on
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Edge {
    Rising,
    Falling,
}

#[derive(Debug, Clone, Copy)]
struct Register {
    edge: Edge,
    enable: Option<u16>,
    reset: Option<u16>,
    async_reset: bool,
    sets: bool,
}

#[derive(Debug, Clone, Copy)]
enum SeqKind {
    Register(Register),
    RamBlock {
        addr: u16,
        data: u16,
    },
    /// Write port of a distributed RAM; the read port is `node`
    DistRamWrite {
        node: u32,
    },
}

/// A clocked cell
#[derive(Debug, Clone)]
struct Seq {
    kind: SeqKind,
    cell: u32,
    /// Its pins, in the `pin_*` arrays
    pin_start: u32,
    pins: u16,
    /// Output nets (canonical), `NO_NET` where nothing is driven
    out_start: u32,
    out_len: u16,
    clock: Logic,
    clock_known: bool,
    /// Clock value at the start of the current delta, when touched in it
    clock_before: Logic,
    clock_touched: bool,
    clock_to_out_ps: i64,
    last_edge_ps: i64,
    state: Logic,
    /// State before the last edge (what a redo of it starts from)
    state_before: Logic,
    /// Memory words, `width` bits each, flat
    memory: Vec<Logic>,
    width: u16,
    /// Words the last edge's write changed, and what they held before, so
    /// a redo of the edge can take the write back
    undo_addresses: Vec<u32>,
    undo_words: Vec<Logic>,
    /// The last edge's write while it waits to show (distributed RAM)
    pending_write: Option<u32>,
}

impl Seq {
    /// Take back the writes of the last edge
    fn undo(&mut self) {
        let width = self.width as usize;
        for (i, &address) in self.undo_addresses.iter().enumerate().rev() {
            let at = address as usize * width;
            self.memory[at..at + width]
                .copy_from_slice(&self.undo_words[i * width..(i + 1) * width]);
        }
        self.undo_addresses.clear();
        self.undo_words.clear();
    }

    /// Write `data` at `address` if `enable`, keeping what it changes so
    /// the write can be taken back
    fn write(&mut self, address: &[Logic], data: &[Logic], enable: Logic) {
        let width = self.width as usize;
        if enable == Logic::Zero || width == 0 {
            return;
        }
        let depth = self.memory.len() / width;
        let (memory, undo_addresses, undo_words) =
            (&self.memory, &mut self.undo_addresses, &mut self.undo_words);
        for_addresses(address, depth, |index| {
            undo_addresses.push(index as u32);
            undo_words.extend_from_slice(&memory[index * width..(index + 1) * width]);
        });
        write_memory(&mut self.memory, width, address, data, enable);
    }
}

/// A write into a distributed RAM, captured at its clock edge
#[derive(Debug, Clone)]
struct PendingWrite {
    seq: u32,
    address: [Logic; DIST_RAM_WIDTH],
    data: [Logic; DIST_RAM_WIDTH],
    enable: Logic,
}

/// A free-running clock on a net
#[derive(Debug, Clone, Copy)]
struct ClockGen {
    net: u32,
    half_period_ps: i64,
    value: Logic,
}

/// One fanout of a net: an event target and the delay to it
#[derive(Debug, Clone, Copy)]
struct Fanout {
    target: u32,
    delay_ps: u32,
}

/// What the nets of a design hold when it starts, before any cell has
/// driven them. Registers always start at their initial value, and
/// memories at their initial contents.
///
/// A loop of combinational cells (a latch made of gates, the step control
/// of a self-timed block) keeps whatever value it powers up with until its
/// inputs force it. Silicon picks some value; [`PowerUp::Unknown`] keeps
/// such a loop unknown, which no input may be able to clear, while the
/// others pick definite values. A design that works should work from any
/// of them: [`PowerUp::Random`] tries a different pick per net.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PowerUp {
    /// Every net starts unknown
    Unknown,
    /// Every net starts at 0
    Zero,
    /// Every net starts at 1
    One,
    /// Each net starts at 0 or 1, chosen from the seed
    Random(u64),
}

impl PowerUp {
    /// The value net `net` starts with
    fn value(self, net: u32) -> Logic {
        match self {
            PowerUp::Unknown => Logic::X,
            PowerUp::Zero => Logic::Zero,
            PowerUp::One => Logic::One,
            PowerUp::Random(seed) => {
                // splitmix64 of the seed and the net
                let mut z = seed ^ (u64::from(net).wrapping_mul(0x9E37_79B9_7F4A_7C15));
                z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
                z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
                Logic::from_bool((z ^ (z >> 31)) & 1 == 1)
            }
        }
    }
}

/// What a clocked element takes when an input changes inside its setup or
/// hold window, or its clock edge is unknown
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Metastability {
    /// Unknown (`X`): pessimistic, and spreads through what follows
    #[default]
    Unknown,
    /// The old or the new value, picked at random from the seed, as a
    /// metastable flip-flop settles to one of them in hardware
    Resolve(u64),
}

/// The delays of one netlist at one corner, captured from a delay model
/// (routed delays from place and route) so a simulation can run later,
/// apart from the model. It is a [`BundleDelays`] for that netlist.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct DelayAnnotation {
    /// The corner the delays are for (`slow`, `fast`)
    pub corner: String,
    /// Delays of each cell, by cell index
    pub cells: Vec<CellDelays>,
}

/// The delays of one cell, in picoseconds
#[derive(Debug, Clone, Default, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct CellDelays {
    /// Wire delay to each pin: the inputs, then a clocked cell's own reset
    /// when it is not one of them
    pub wire_ps: Vec<i64>,
    /// Wire delay to the clock pin
    pub clock_wire_ps: i64,
    /// Arc delays, by input then output; `None` where an input does not
    /// reach an output
    pub arcs: Vec<Vec<Option<i64>>>,
    pub clock_to_out_ps: i64,
    /// Which outputs a clock edge launches
    pub launches: Vec<bool>,
    /// Setup and hold time of each pin
    pub setup_ps: Vec<i64>,
    pub hold_ps: Vec<i64>,
}

impl DelayAnnotation {
    /// The delays a fraction `k` of the way from `from` to `to`, each delay
    /// on its own: `from + k * (to - from)`. Silicon sits somewhere between
    /// the vendor's fast and slow corners, depending on its process, supply
    /// and temperature; a ring oscillator on the chip measured against the
    /// same ring at both corners gives `k` (in delay, not frequency).
    /// The two must be of the same netlist; a delay either one lacks is
    /// left out.
    pub fn between(from: &Self, to: &Self, k: f64) -> Result<Self, String> {
        if from.cells.len() != to.cells.len() {
            return Err(format!(
                "the {} and {} corners have {} and {} cells",
                from.corner,
                to.corner,
                from.cells.len(),
                to.cells.len()
            ));
        }
        let mix = |a: i64, b: i64| a + ((b - a) as f64 * k).round() as i64;
        let mix_all = |a: &[i64], b: &[i64]| -> Vec<i64> {
            a.iter().zip(b).map(|(&a, &b)| mix(a, b)).collect()
        };
        let cells = from
            .cells
            .iter()
            .zip(&to.cells)
            .enumerate()
            .map(|(i, (a, b))| {
                if a.wire_ps.len() != b.wire_ps.len()
                    || a.arcs.len() != b.arcs.len()
                    || a.setup_ps.len() != b.setup_ps.len()
                    || a.hold_ps.len() != b.hold_ps.len()
                {
                    return Err(format!(
                        "cell {i} has different pins at the {} and {} corners",
                        from.corner, to.corner
                    ));
                }
                Ok(CellDelays {
                    wire_ps: mix_all(&a.wire_ps, &b.wire_ps),
                    clock_wire_ps: mix(a.clock_wire_ps, b.clock_wire_ps),
                    arcs: a
                        .arcs
                        .iter()
                        .zip(&b.arcs)
                        .map(|(x, y)| {
                            x.iter()
                                .zip(y)
                                .map(|(&x, &y)| match (x, y) {
                                    (Some(x), Some(y)) => Some(mix(x, y)),
                                    _ => None,
                                })
                                .collect()
                        })
                        .collect(),
                    clock_to_out_ps: mix(a.clock_to_out_ps, b.clock_to_out_ps),
                    launches: a.launches.clone(),
                    setup_ps: mix_all(&a.setup_ps, &b.setup_ps),
                    hold_ps: mix_all(&a.hold_ps, &b.hold_ps),
                })
            })
            .collect::<Result<Vec<_>, String>>()?;
        Ok(Self {
            corner: format!("{} to {} at {k}", from.corner, to.corner),
            cells,
        })
    }

    /// Capture every delay of `netlist` that a timed simulation uses
    pub fn capture(netlist: &GateNetlist, delays: &dyn BundleDelays, corner: &str) -> Self {
        let cells = netlist
            .cells
            .iter()
            .map(|cell| {
                let clocked = cell.clock.is_some();
                let pins = cell.inputs.len() + usize::from(clocked && cell.reset.is_some());
                let pin_net = |pin: usize| cell.inputs.get(pin).copied().or(cell.reset);
                CellDelays {
                    wire_ps: (0..pins)
                        .map(|pin| {
                            pin_net(pin).map_or(0, |net| delays.wire_ps(netlist, net, cell, pin))
                        })
                        .collect(),
                    clock_wire_ps: cell
                        .clock
                        .map_or(0, |clock| delays.clock_wire_ps(netlist, clock, cell)),
                    arcs: (0..cell.inputs.len())
                        .map(|input| {
                            (0..cell.outputs.len())
                                .map(|output| delays.arc_ps(cell, input, output))
                                .collect()
                        })
                        .collect(),
                    clock_to_out_ps: if clocked {
                        delays.clock_to_out_ps(cell)
                    } else {
                        0
                    },
                    launches: (0..cell.outputs.len())
                        .map(|output| delays.launches(cell, output))
                        .collect(),
                    setup_ps: if clocked {
                        (0..pins).map(|pin| delays.setup_ps(cell, pin)).collect()
                    } else {
                        Vec::new()
                    },
                    hold_ps: if clocked {
                        (0..pins).map(|pin| delays.hold_ps(cell, pin)).collect()
                    } else {
                        Vec::new()
                    },
                }
            })
            .collect();
        Self {
            corner: corner.to_string(),
            cells,
        }
    }

    fn cell(&self, cell: &Cell) -> Option<&CellDelays> {
        self.cells.get(cell.id.0 as usize)
    }
}

impl BundleDelays for DelayAnnotation {
    fn wire_ps(&self, _: &GateNetlist, _: GateNetId, cell: &Cell, input: usize) -> i64 {
        self.cell(cell)
            .and_then(|c| c.wire_ps.get(input))
            .copied()
            .unwrap_or(0)
    }
    fn clock_wire_ps(&self, _: &GateNetlist, _: GateNetId, cell: &Cell) -> i64 {
        self.cell(cell).map_or(0, |c| c.clock_wire_ps)
    }
    fn arc_ps(&self, cell: &Cell, input: usize, output: usize) -> Option<i64> {
        *self.cell(cell)?.arcs.get(input)?.get(output)?
    }
    fn clock_to_out_ps(&self, cell: &Cell) -> i64 {
        self.cell(cell).map_or(0, |c| c.clock_to_out_ps)
    }
    fn launches(&self, cell: &Cell, output: usize) -> bool {
        self.cell(cell)
            .and_then(|c| c.launches.get(output))
            .copied()
            .unwrap_or(cell.clock.is_some())
    }
    fn setup_ps(&self, cell: &Cell, input: usize) -> i64 {
        self.cell(cell)
            .and_then(|c| c.setup_ps.get(input))
            .copied()
            .unwrap_or(0)
    }
    fn hold_ps(&self, cell: &Cell, input: usize) -> i64 {
        self.cell(cell)
            .and_then(|c| c.hold_ps.get(input))
            .copied()
            .unwrap_or(0)
    }
}

/// Event-driven, timed simulation of a gate netlist
pub struct TimedSim<'a> {
    netlist: &'a GateNetlist,
    names: HashMap<&'a str, GateNetId>,

    // Nets
    net_val: Vec<Logic>,
    net_next: Vec<Logic>,
    net_dirty: Vec<bool>,
    dirty_nets: Vec<u32>,
    fan_start: Vec<u32>,
    fan: Vec<Fanout>,
    watched: Vec<bool>,
    watched_changed: bool,
    /// Changes of watched nets are recorded from this time on
    record_from_ps: i64,
    changes: Vec<(i64, GateNetId, Logic)>,
    /// Value of each watched net just before its first recorded change
    recorded_before: HashMap<u32, Logic>,
    /// Changes of each net from `activity_from_ps` on (switching activity)
    toggles: Vec<u64>,
    activity_from_ps: i64,

    // Combinational nodes
    nodes: Vec<Node>,
    node_outputs: Vec<NodeOutput>,
    tables: Vec<u64>,
    wide_inputs: Vec<Logic>,
    /// Node input slots: node and input bit
    slot_node: Vec<u32>,
    slot_bit: Vec<u16>,
    dirty_nodes: Vec<u32>,
    /// Each node's entry in `loops`, or `NO_LOOP`
    node_loop: Vec<u32>,
    loops: Vec<FeedbackLoop>,

    // Clocked cells
    seqs: Vec<Seq>,
    seq_outputs: Vec<u32>,
    pin_val: Vec<Logic>,
    /// Each pin's value before its last change
    pin_prev: Vec<Logic>,
    /// What each pin's cell took at its last edge
    pin_cap: Vec<Logic>,
    pin_changed_ps: Vec<i64>,
    pin_setup_ps: Vec<i64>,
    pin_hold_ps: Vec<i64>,
    pin_is_clock: Vec<bool>,
    /// Pin slots: clocked cell and pin index
    pin_seq: Vec<u32>,
    clock_touched: Vec<u32>,

    writes: Vec<Option<PendingWrite>>,
    free_writes: Vec<u32>,
    clocks: Vec<ClockGen>,

    wheel: Wheel,
    batch: Vec<Event>,
    now: i64,
    events: u64,
    violations: Vec<Violation>,
    violation_count: u64,
    latch_races: Vec<LatchRace>,
    latch_race_count: u64,
    /// Each net's exclusive merge (index into `exclusive_merges`), or
    /// `NO_MERGE`
    exclusive_of: Vec<u32>,
    /// Exclusive merges: path and grant nets, in arm order
    exclusive_merges: Vec<(String, Vec<u32>)>,
    exclusive_clashes: Vec<ExclusiveClash>,
    exclusive_clash_count: u64,
    metastability: Metastability,
    rng: u64,
    /// Comb-cell arcs the delay model has no figure for (timed with the
    /// slowest arc of that output instead)
    pub missing_arcs: usize,
    /// Vendor blocks with no model, whose outputs nothing drives
    pub blackboxes: Vec<Blackbox>,
}

/// Group a cell's outputs by their arc delays. Returns, per group, the
/// delay of each input (`None` where the input does not reach those
/// outputs) and the outputs.
///
/// Where the delay model has no arc from an input to an output, the input
/// still feeds the output if the output's function depends on it: with no
/// delay if the input is a constant net (it never changes after power-up),
/// else with the slowest arc the output has, counted in `missing`.
fn arc_groups(
    cell: &Cell,
    ptype: &PrimitiveType,
    delays: &dyn BundleDelays,
    is_constant: &dyn Fn(GateNetId) -> bool,
    missing: &mut usize,
) -> Vec<(Vec<Option<i64>>, Vec<usize>)> {
    let inputs = cell.inputs.len();
    let dist_ram = cell.function == Some(CellFunction::DistRam16x4);
    let mut groups: Vec<(Vec<Option<i64>>, Vec<usize>)> = Vec::new();
    for output in 0..cell.outputs.len() {
        let mut arcs: Vec<Option<i64>> = (0..inputs)
            .map(|i| delays.arc_ps(cell, i, output).map(|d| d.max(0)))
            .collect();
        if dist_ram {
            // Only the read address reaches the read port combinationally
            for (i, arc) in arcs.iter_mut().enumerate() {
                if i < DIST_RAM_READ_ADDRESS {
                    *arc = None;
                } else if arc.is_none() {
                    *missing += 1;
                    *arc = Some(0);
                }
            }
        } else if arcs.iter().any(Option::is_none) {
            let depends =
                (inputs <= MAX_TABLE_INPUTS).then(|| input_dependence(ptype, inputs, output));
            let slowest = arcs.iter().flatten().copied().max().unwrap_or(0);
            for (i, arc) in arcs.iter_mut().enumerate() {
                if arc.is_some() {
                    continue;
                }
                if is_constant(cell.inputs[i]) {
                    *arc = Some(0);
                } else if depends.as_ref().is_some_and(|d| !d[i]) {
                    // The output does not depend on it: no arc is right
                } else {
                    *missing += 1;
                    *arc = Some(slowest);
                }
            }
        }
        match groups.iter_mut().find(|(a, _)| *a == arcs) {
            Some((_, outs)) => outs.push(output),
            None => groups.push((arcs, vec![output])),
        }
    }
    groups
}

/// Which inputs output `output` of `ptype` (with `inputs` inputs) depends
/// on, from its full truth table
fn input_dependence(ptype: &PrimitiveType, inputs: usize, output: usize) -> Vec<bool> {
    let entries = 1usize << inputs;
    let mut bits = vec![false; inputs];
    let table: Vec<bool> = (0..entries)
        .map(|index| {
            for (k, bit) in bits.iter_mut().enumerate() {
                *bit = (index >> k) & 1 == 1;
            }
            evaluate_primitive(ptype, &bits)
                .get(output)
                .copied()
                .unwrap_or(false)
        })
        .collect();
    (0..inputs)
        .map(|i| {
            (0..entries)
                .any(|index| index & (1 << i) == 0 && table[index] != table[index | (1 << i)])
        })
        .collect()
}

/// How a combinational cell's node computes, `None` for clocked cells and
/// cells not modelled
fn comb_kind(cell: &Cell, ptype: &PrimitiveType) -> Option<NodeKind> {
    Some(match ptype {
        PrimitiveType::Th12 | PrimitiveType::Th13 | PrimitiveType::Th14 => {
            NodeKind::Threshold { m: 1 }
        }
        PrimitiveType::Th22 | PrimitiveType::Th23 | PrimitiveType::Th24 => {
            NodeKind::Threshold { m: 2 }
        }
        PrimitiveType::Th33 | PrimitiveType::Th34 => NodeKind::Threshold { m: 3 },
        PrimitiveType::Th44 => NodeKind::Threshold { m: 4 },
        PrimitiveType::Dlatch => NodeKind::Latch,
        PrimitiveType::SRlatch => NodeKind::SrLatch,
        other if other.is_sequential() || cell.clock.is_some() => return None,
        _ => NodeKind::Table,
    })
}

/// How a clocked cell behaves, `None` if not modelled
fn seq_kind(cell: &Cell, ptype: &PrimitiveType) -> Option<SeqKind> {
    let reg = |edge, enable: Option<u16>, reset: Option<u16>, async_reset, sets| {
        // A reset pin is one of the inputs, or the cell's own reset after them
        let reset = reset.filter(|&r| {
            (r as usize) < cell.inputs.len()
                || (r as usize == cell.inputs.len() && cell.reset.is_some())
        });
        SeqKind::Register(Register {
            edge,
            enable,
            reset,
            async_reset,
            sets,
        })
    };
    Some(match ptype {
        PrimitiveType::DffP | PrimitiveType::DffN | PrimitiveType::RetentionDff { .. } => {
            reg(Edge::Rising, None, None, false, false)
        }
        PrimitiveType::DffNeg => reg(Edge::Falling, None, None, false, false),
        // A flip-flop's reset pin is asynchronous: synthesis folds a
        // synchronous reset into the data input (`synth::aig_writer`), and
        // the device resets the register as soon as the pin is active
        PrimitiveType::DffSR => reg(Edge::Rising, None, Some(1), true, false),
        PrimitiveType::DffE => reg(Edge::Rising, Some(1), Some(2), true, false),
        PrimitiveType::DffAR => reg(Edge::Rising, None, Some(1), true, false),
        PrimitiveType::DffAS => reg(Edge::Rising, None, Some(1), true, true),
        PrimitiveType::RamBlock {
            addr_width,
            data_width,
        } => {
            let (addr, data) = match cell.ram_ports() {
                Some(ports) => (ports.read_addr.len(), ports.data_width()),
                None => (*addr_width as usize, *data_width as usize),
            };
            SeqKind::RamBlock {
                addr: addr as u16,
                data: data as u16,
            }
        }
        _ => return None,
    })
}

/// The truth table of `output` of `ptype` over node inputs `pins` (cell
/// inputs; the others held at 0), as 64-bit words
fn build_table(
    ptype: &PrimitiveType,
    cell_inputs: usize,
    pins: &[usize],
    output: usize,
) -> Vec<u64> {
    let n = pins.len();
    let entries = 1usize << n;
    let mut words = vec![0u64; entries.div_ceil(64)];
    let mut bits = vec![false; cell_inputs];
    for index in 0..entries {
        for (k, &pin) in pins.iter().enumerate() {
            bits[pin] = (index >> k) & 1 == 1;
        }
        if evaluate_primitive(ptype, &bits)
            .get(output)
            .copied()
            .unwrap_or(false)
        {
            words[index / 64] |= 1 << (index % 64);
        }
    }
    words
}

/// Look up a truth table with known inputs `val` and unknown inputs `x`
#[inline]
fn table_lookup(table: &[u64], val: u64, x: u64) -> Logic {
    let bit = |index: u64| (table[(index / 64) as usize] >> (index % 64)) & 1 == 1;
    if x == 0 {
        return Logic::from_bool(bit(val));
    }
    // Every value of the unknown inputs (submasks of x)
    let base = val & !x;
    let first = bit(base | x);
    let mut sub = x;
    loop {
        sub = (sub.wrapping_sub(1)) & x;
        if bit(base | sub) != first {
            return Logic::X;
        }
        if sub == 0 {
            return Logic::from_bool(first);
        }
    }
}

impl<'a> TimedSim<'a> {
    /// Build a simulation of `netlist` with the delays of `delays`, every
    /// net starting unknown ([`PowerUp::Unknown`]). Fails if the netlist has
    /// cells this simulator cannot model.
    pub fn new(netlist: &'a GateNetlist, delays: &dyn BundleDelays) -> Result<Self, TimedSimError> {
        Self::with_power_up(netlist, delays, PowerUp::Unknown)
    }

    /// Build a simulation whose nets start as `power_up` says
    pub fn with_power_up(
        netlist: &'a GateNetlist,
        delays: &dyn BundleDelays,
        power_up: PowerUp,
    ) -> Result<Self, TimedSimError> {
        let canonical = |net: GateNetId| netlist.resolve_alias(net).0;
        let n_nets = netlist.nets.len();
        // Nets driven by constant cells
        let mut constant = vec![false; n_nets];
        for cell in &netlist.cells {
            if matches!(primitive_for_cell(cell), PrimitiveType::Constant { .. }) {
                for &out in &cell.outputs {
                    constant[canonical(out) as usize] = true;
                }
            }
        }
        let is_constant = |net: GateNetId| constant[canonical(net) as usize];

        let mut nodes: Vec<Node> = Vec::new();
        let mut node_outputs: Vec<NodeOutput> = Vec::new();
        let mut tables: Vec<u64> = Vec::new();
        let mut table_cache: HashMap<Vec<u64>, u32> = HashMap::new();
        let mut wide_inputs: Vec<Logic> = Vec::new();
        let mut slot_node: Vec<u32> = Vec::new();
        let mut slot_bit: Vec<u16> = Vec::new();
        let mut loops: Vec<(u32, FeedbackLoop)> = Vec::new();

        let mut seqs: Vec<Seq> = Vec::new();
        let mut seq_outputs: Vec<u32> = Vec::new();
        let mut pin_val: Vec<Logic> = Vec::new();
        let mut pin_changed_ps: Vec<i64> = Vec::new();
        let mut pin_setup_ps: Vec<i64> = Vec::new();
        let mut pin_hold_ps: Vec<i64> = Vec::new();
        let mut pin_is_clock: Vec<bool> = Vec::new();
        let mut pin_seq: Vec<u32> = Vec::new();

        // Fanout, per net, before compression: (target, delay)
        let mut fanout: Vec<Vec<Fanout>> = vec![Vec::new(); n_nets];
        let mut unsupported = Vec::new();
        let mut missing_arcs = 0usize;
        let mut blackboxes: Vec<Blackbox> = Vec::new();

        for (index, cell) in netlist.cells.iter().enumerate() {
            if is_blackbox(cell) {
                blackboxes.push(Blackbox {
                    cell: CellId(index as u32),
                    outputs: cell
                        .outputs
                        .iter()
                        .map(|&n| netlist.resolve_alias(n))
                        .collect(),
                });
                continue;
            }
            let ptype = primitive_for_cell(cell);
            let out_nets: Vec<u32> = cell.outputs.iter().map(|&n| canonical(n)).collect();
            let wire =
                |input: usize, net: GateNetId| delays.wire_ps(netlist, net, cell, input).max(0);

            if cell.function == Some(CellFunction::DistRam16x4) {
                // Read port: a node over the read address
                if cell.inputs.len() != DIST_RAM_READ_ADDRESS + DIST_RAM_WIDTH
                    || cell.outputs.len() != DIST_RAM_WIDTH
                    || cell.clock.is_none()
                {
                    unsupported.push((cell.cell_type.clone(), cell.path.clone()));
                    continue;
                }
                let seq_index = seqs.len() as u32;
                let groups = arc_groups(cell, &ptype, delays, &is_constant, &mut missing_arcs);
                let mut read_node = None;
                for (arcs, outs) in groups {
                    let node_index = nodes.len() as u32;
                    read_node.get_or_insert(node_index);
                    let pins: Vec<usize> = (DIST_RAM_READ_ADDRESS..cell.inputs.len()).collect();
                    let out_start = node_outputs.len() as u32;
                    for &o in &outs {
                        node_outputs.push(NodeOutput {
                            net: out_nets[o],
                            table: o as u32,
                            value: Logic::X,
                            committed: Logic::X,
                            generation: 0,
                        });
                    }
                    // The node's inertia is its shortest arc; each input's
                    // fanout carries the rest of its arc
                    let inertia = arcs.iter().flatten().copied().min().unwrap_or(0).max(0);
                    for (bit, &pin) in pins.iter().enumerate() {
                        let slot = slot_node.len() as u32;
                        slot_node.push(node_index);
                        slot_bit.push(bit as u16);
                        let net = cell.inputs[pin];
                        let arc = arcs[pin].map_or(0, |arc| (arc - inertia).max(0));
                        fanout[canonical(net) as usize].push(Fanout {
                            target: (T_NODE_INPUT << TARGET_SHIFT) | slot,
                            delay_ps: (wire(pin, net) + arc) as u32,
                        });
                    }
                    nodes.push(Node {
                        kind: NodeKind::DistRamRead { seq: seq_index },
                        inputs: pins.len() as u16,
                        val: 0,
                        x: !0,
                        wide: 0,
                        out_start,
                        out_len: outs.len() as u16,
                        inertia_ps: inertia as u32,
                        dirty: false,
                    });
                }
                // Write port: a clocked cell over the write pins
                let pins = DIST_RAM_READ_ADDRESS;
                let pin_start = pin_val.len() as u32;
                let clock_net = cell.clock.map(canonical);
                for (pin, &net) in cell.inputs[..pins].iter().enumerate() {
                    let slot = pin_seq.len() as u32;
                    pin_seq.push(seq_index);
                    pin_val.push(Logic::X);
                    pin_changed_ps.push(i64::MIN / 4);
                    pin_setup_ps.push(delays.setup_ps(cell, pin));
                    pin_hold_ps.push(delays.hold_ps(cell, pin));
                    pin_is_clock.push(Some(canonical(net)) == clock_net);
                    fanout[canonical(net) as usize].push(Fanout {
                        target: (T_SEQ_PIN << TARGET_SHIFT) | slot,
                        delay_ps: wire(pin, net) as u32,
                    });
                }
                let clock = cell.clock.expect("checked above");
                fanout[canonical(clock) as usize].push(Fanout {
                    target: (T_SEQ_CLOCK << TARGET_SHIFT) | seq_index,
                    delay_ps: delays.clock_wire_ps(netlist, clock, cell).max(0) as u32,
                });
                // A write shows on the read port after the clock-to-output
                // delay where the model has one, else after the read arc
                let launch = delays.clock_to_out_ps(cell);
                let write_through = if launch > 0 {
                    launch
                } else {
                    (DIST_RAM_READ_ADDRESS..cell.inputs.len())
                        .flat_map(|pin| {
                            (0..cell.outputs.len()).filter_map(move |o| delays.arc_ps(cell, pin, o))
                        })
                        .max()
                        .unwrap_or(0)
                };
                let memory =
                    initial_memory(dist_ram_init(cell), 1 << DIST_RAM_WIDTH, DIST_RAM_WIDTH);
                seqs.push(Seq {
                    kind: SeqKind::DistRamWrite {
                        node: read_node.unwrap_or(0),
                    },
                    cell: index as u32,
                    pin_start,
                    pins: pins as u16,
                    out_start: seq_outputs.len() as u32,
                    out_len: 0,
                    clock: Logic::X,
                    clock_known: false,
                    clock_before: Logic::X,
                    clock_touched: false,
                    clock_to_out_ps: write_through.max(0),
                    last_edge_ps: i64::MIN / 4,
                    state: Logic::X,
                    state_before: Logic::X,
                    memory,
                    width: DIST_RAM_WIDTH as u16,
                    undo_addresses: Vec::new(),
                    undo_words: Vec::new(),
                    pending_write: None,
                });
                continue;
            }

            if let Some(kind) = comb_kind(cell, &ptype) {
                let groups = arc_groups(cell, &ptype, delays, &is_constant, &mut missing_arcs);
                for (arcs, outs) in groups {
                    let node_index = nodes.len() as u32;
                    let pins: Vec<usize> = (0..cell.inputs.len())
                        .filter(|&i| arcs[i].is_some())
                        .collect();
                    let n = pins.len();
                    let kind = match (&kind, n) {
                        (NodeKind::Table, n) if n <= MAX_TABLE_INPUTS => NodeKind::Table,
                        (NodeKind::Table, _) => NodeKind::Wide {
                            ptype: ptype.clone(),
                            cell_inputs: cell.inputs.len() as u16,
                            pins: pins.iter().map(|&p| p as u16).collect(),
                            outputs: outs.iter().map(|&o| o as u16).collect(),
                        },
                        (other, _) => other.clone(),
                    };
                    let out_start = node_outputs.len() as u32;
                    for &o in &outs {
                        // `x = BUF(x)` left by buffer removal drives nothing
                        let self_buffer = matches!(ptype, PrimitiveType::Buf)
                            && cell.inputs.iter().any(|&i| canonical(i) == out_nets[o]);
                        let table = if matches!(kind, NodeKind::Table) {
                            let words = build_table(&ptype, cell.inputs.len(), &pins, o);
                            *table_cache.entry(words.clone()).or_insert_with(|| {
                                let at = tables.len() as u32;
                                tables.extend_from_slice(&words);
                                at
                            })
                        } else {
                            0
                        };
                        node_outputs.push(NodeOutput {
                            net: if self_buffer { NO_NET } else { out_nets[o] },
                            table,
                            value: Logic::X,
                            committed: Logic::X,
                            generation: 0,
                        });
                    }
                    let wide = if n > MAX_MASK_INPUTS {
                        let at = wide_inputs.len() as u32;
                        wide_inputs.extend(std::iter::repeat_n(Logic::X, n));
                        at
                    } else {
                        0
                    };
                    // The node's inertia is its shortest arc; each input's
                    // fanout carries the rest of its arc
                    let inertia = arcs.iter().flatten().copied().min().unwrap_or(0).max(0);
                    for (bit, &pin) in pins.iter().enumerate() {
                        let slot = slot_node.len() as u32;
                        slot_node.push(node_index);
                        slot_bit.push(bit as u16);
                        let net = cell.inputs[pin];
                        let arc = arcs[pin].map_or(0, |arc| (arc - inertia).max(0));
                        fanout[canonical(net) as usize].push(Fanout {
                            target: (T_NODE_INPUT << TARGET_SHIFT) | slot,
                            delay_ps: (wire(pin, net) + arc) as u32,
                        });
                    }
                    // A latch in one cell: its only output read back by
                    // its own inputs
                    if matches!(kind, NodeKind::Table) && outs.len() == 1 {
                        let out_net = out_nets[outs[0]];
                        let mut mask = 0u64;
                        let mut loop_ps = 0i64;
                        for (bit, &pin) in pins.iter().enumerate() {
                            let net = cell.inputs[pin];
                            if canonical(net) == out_net {
                                mask |= 1 << bit;
                                loop_ps = loop_ps.max(wire(pin, net) + arcs[pin].unwrap_or(0));
                            }
                        }
                        let table = node_outputs[out_start as usize].table as usize;
                        if mask != 0 && holds_through_feedback(&tables[table..], n, mask) {
                            loops.push((
                                node_index,
                                FeedbackLoop {
                                    cell: index as u32,
                                    mask,
                                    loop_ps,
                                    changed_ps: i64::MIN / 4,
                                    before: Logic::X,
                                },
                            ));
                        }
                    }
                    nodes.push(Node {
                        kind,
                        inputs: n as u16,
                        val: 0,
                        x: if n >= 64 { !0 } else { (1u64 << n) - 1 },
                        wide,
                        out_start,
                        out_len: outs.len() as u16,
                        inertia_ps: inertia as u32,
                        dirty: false,
                    });
                }
                continue;
            }

            let Some(kind) = seq_kind(cell, &ptype) else {
                unsupported.push((cell.cell_type.clone(), cell.path.clone()));
                continue;
            };
            let Some(clock) = cell.clock else {
                unsupported.push((cell.cell_type.clone(), cell.path.clone()));
                continue;
            };
            let seq_index = seqs.len() as u32;
            let mut pin_nets: Vec<GateNetId> = cell.inputs.clone();
            if let SeqKind::Register(reg) = &kind {
                if reg.reset.map(usize::from) == Some(cell.inputs.len()) {
                    pin_nets.extend(cell.reset);
                }
            }
            let pin_start = pin_val.len() as u32;
            let clock_net = canonical(clock);
            for (pin, &net) in pin_nets.iter().enumerate() {
                let slot = pin_seq.len() as u32;
                pin_seq.push(seq_index);
                pin_val.push(Logic::X);
                pin_changed_ps.push(i64::MIN / 4);
                pin_setup_ps.push(delays.setup_ps(cell, pin));
                pin_hold_ps.push(delays.hold_ps(cell, pin));
                pin_is_clock.push(canonical(net) == clock_net);
                fanout[canonical(net) as usize].push(Fanout {
                    target: (T_SEQ_PIN << TARGET_SHIFT) | slot,
                    delay_ps: wire(pin, net) as u32,
                });
            }
            fanout[clock_net as usize].push(Fanout {
                target: (T_SEQ_CLOCK << TARGET_SHIFT) | seq_index,
                delay_ps: delays.clock_wire_ps(netlist, clock, cell).max(0) as u32,
            });
            let out_start = seq_outputs.len() as u32;
            seq_outputs.extend(out_nets.iter().copied());
            let (memory, width) = match kind {
                SeqKind::RamBlock { addr, data } => (
                    initial_memory(cell.memory_init(), 1 << addr, data as usize),
                    data,
                ),
                _ => (Vec::new(), 0),
            };
            let state = match kind {
                SeqKind::Register(_) => Logic::from_bool(
                    cell.parameters.get(FF_INIT_PARAM).map(String::as_str) == Some("1"),
                ),
                _ => Logic::X,
            };
            seqs.push(Seq {
                kind,
                cell: index as u32,
                pin_start,
                pins: pin_nets.len() as u16,
                out_start,
                out_len: out_nets.len() as u16,
                clock: Logic::X,
                clock_known: false,
                clock_before: Logic::X,
                clock_touched: false,
                clock_to_out_ps: delays.clock_to_out_ps(cell).max(0),
                last_edge_ps: i64::MIN / 4,
                state,
                state_before: state,
                memory,
                width,
                undo_addresses: Vec::new(),
                undo_words: Vec::new(),
                pending_write: None,
            });
        }
        if !unsupported.is_empty() {
            return Err(TimedSimError::UnsupportedCells(unsupported));
        }

        // Compress the fanout into one array
        let mut fan_start = Vec::with_capacity(n_nets + 1);
        let mut fan = Vec::with_capacity(fanout.iter().map(Vec::len).sum());
        for list in &fanout {
            fan_start.push(fan.len() as u32);
            fan.extend_from_slice(list);
        }
        fan_start.push(fan.len() as u32);
        drop(fanout);

        let names = netlist
            .nets
            .iter()
            .map(|net| (net.name.as_str(), net.id))
            .collect();
        let n_seqs = seqs.len();
        let mut node_loop = vec![NO_LOOP; nodes.len()];
        for (i, &(node, _)) in loops.iter().enumerate() {
            node_loop[node as usize] = i as u32;
        }
        let loops = loops.into_iter().map(|(_, l)| l).collect();
        // Exclusive merges: the grant buffers, by merge (see
        // `ExclusiveClash`)
        let mut exclusive_of = vec![NO_MERGE; n_nets];
        let mut exclusive_merges: Vec<(String, Vec<(usize, u32)>)> = Vec::new();
        for cell in &netlist.cells {
            if cell.source_op.as_deref() != Some(EXCLUSIVE_OP) {
                continue;
            }
            let (Some((merge, arm)), Some(&out)) =
                (cell.path.rsplit_once(".excl"), cell.outputs.first())
            else {
                continue;
            };
            let Ok(arm) = arm.parse::<usize>() else {
                continue;
            };
            let k = match exclusive_merges.iter().position(|(m, _)| m == merge) {
                Some(k) => k,
                None => {
                    exclusive_merges.push((merge.to_string(), Vec::new()));
                    exclusive_merges.len() - 1
                }
            };
            let net = canonical(out);
            exclusive_of[net as usize] = k as u32;
            exclusive_merges[k].1.push((arm, net));
        }
        let exclusive_merges = exclusive_merges
            .into_iter()
            .map(|(merge, mut arms)| {
                arms.sort_unstable();
                (merge, arms.into_iter().map(|(_, net)| net).collect())
            })
            .collect();
        let mut sim = Self {
            netlist,
            names,
            net_val: vec![Logic::X; n_nets],
            net_next: vec![Logic::X; n_nets],
            net_dirty: vec![false; n_nets],
            dirty_nets: Vec::new(),
            fan_start,
            fan,
            watched: vec![false; n_nets],
            watched_changed: false,
            record_from_ps: i64::MIN,
            changes: Vec::new(),
            recorded_before: HashMap::new(),
            toggles: vec![0; n_nets],
            activity_from_ps: 0,
            dirty_nodes: Vec::new(),
            node_loop,
            loops,
            nodes,
            node_outputs,
            tables,
            wide_inputs,
            slot_node,
            slot_bit,
            seqs,
            seq_outputs,
            pin_prev: pin_val.clone(),
            pin_cap: pin_val.clone(),
            pin_val,
            pin_changed_ps,
            pin_setup_ps,
            pin_hold_ps,
            pin_is_clock,
            pin_seq,
            clock_touched: Vec::with_capacity(n_seqs),
            writes: Vec::new(),
            free_writes: Vec::new(),
            clocks: Vec::new(),
            wheel: Wheel::new(),
            batch: Vec::new(),
            now: 0,
            events: 0,
            violations: Vec::new(),
            violation_count: 0,
            latch_races: Vec::new(),
            latch_race_count: 0,
            exclusive_of,
            exclusive_merges,
            exclusive_clashes: Vec::new(),
            exclusive_clash_count: 0,
            metastability: Metastability::Unknown,
            rng: 1,
            missing_arcs,
            blackboxes,
        };
        sim.power_up(power_up);
        Ok(sim)
    }

    /// What clocked elements take on a timing violation, and how a
    /// one-cell latch's race settles
    pub fn metastability(&self) -> Metastability {
        self.metastability
    }

    /// Choose what clocked elements take on a timing violation (unknown
    /// by default), and how a one-cell latch's race settles
    /// ([`LatchRace`])
    pub fn set_metastability(&mut self, metastability: Metastability) {
        self.metastability = metastability;
        if let Metastability::Resolve(seed) = metastability {
            // xorshift needs a nonzero state
            self.rng = seed.wrapping_mul(0x9E37_79B9_7F4A_7C15) | 1;
        }
    }

    /// Start every net as `power_up` says (registers at their initial
    /// value), with every pin seeing its net, then evaluate every node once
    fn power_up(&mut self, power_up: PowerUp) {
        for net in 0..self.net_val.len() {
            let value = power_up.value(net as u32);
            self.net_val[net] = value;
            self.net_next[net] = value;
        }
        for s in 0..self.seqs.len() {
            if let SeqKind::Register(_) = self.seqs[s].kind {
                let (start, len) = (
                    self.seqs[s].out_start as usize,
                    self.seqs[s].out_len as usize,
                );
                let state = self.seqs[s].state;
                for i in start..start + len {
                    let net = self.seq_outputs[i];
                    if net != NO_NET {
                        self.net_val[net as usize] = state;
                        self.net_next[net as usize] = state;
                    }
                }
            }
        }
        // Node outputs hold their net's value until evaluated
        for out in &mut self.node_outputs {
            if out.net != NO_NET {
                out.value = self.net_val[out.net as usize];
                out.committed = out.value;
            }
        }
        // Every pin sees its net
        for net in 0..self.net_val.len() {
            let value = self.net_val[net];
            let (start, end) = (
                self.fan_start[net] as usize,
                self.fan_start[net + 1] as usize,
            );
            for f in start..end {
                let target = self.fan[f].target;
                let index = (target & TARGET_INDEX) as usize;
                match target >> TARGET_SHIFT {
                    T_NODE_INPUT => {
                        let node = &mut self.nodes[self.slot_node[index] as usize];
                        let bit = self.slot_bit[index] as usize;
                        if node.inputs as usize > MAX_MASK_INPUTS {
                            self.wide_inputs[node.wide as usize + bit] = value;
                        } else {
                            let mask = 1u64 << bit;
                            node.val &= !mask;
                            node.x &= !mask;
                            match value {
                                Logic::One => node.val |= mask,
                                Logic::X => node.x |= mask,
                                Logic::Zero => {}
                            }
                        }
                    }
                    T_SEQ_PIN => {
                        self.pin_val[index] = value;
                        self.pin_prev[index] = value;
                        self.pin_cap[index] = value;
                    }
                    T_SEQ_CLOCK => {
                        let seq = &mut self.seqs[index];
                        seq.clock = value;
                        seq.clock_known = value != Logic::X;
                    }
                    _ => {}
                }
            }
        }
        // Registers drive their initial value (a no-op where the net
        // already holds it). Each node has seen its inputs' power-up values
        // since time 0, so it evaluates them at once and its outputs
        // follow after its inertia.
        for s in 0..self.seqs.len() {
            if let SeqKind::Register(_) = self.seqs[s].kind {
                let state = self.seqs[s].state;
                self.drive_outputs(s, 0, state);
            }
        }
        for node in 0..self.nodes.len() {
            self.wheel
                .push(0, Event::new(T_NODE_EVAL, node as u32, Logic::One));
        }
    }

    /// The net named `name`
    pub fn net(&self, name: &str) -> Option<GateNetId> {
        self.names.get(name).copied()
    }

    /// The nets named `prefix[i]`, by bit
    pub fn bus(&self, prefix: &str) -> Vec<GateNetId> {
        let mut bits: Vec<(usize, GateNetId)> = self
            .names
            .iter()
            .filter_map(|(name, &id)| {
                let rest = name.strip_prefix(prefix)?.strip_prefix('[')?;
                let bit = rest.strip_suffix(']')?.parse().ok()?;
                Some((bit, id))
            })
            .collect();
        bits.sort_unstable_by_key(|(bit, _)| *bit);
        bits.into_iter().map(|(_, id)| id).collect()
    }

    /// Current time, in picoseconds
    pub fn now(&self) -> i64 {
        self.now
    }

    /// Current value of a net
    pub fn value(&self, net: GateNetId) -> Logic {
        self.net_val[self.netlist.resolve_alias(net).0 as usize]
    }

    /// Value of a bus (least significant bit first), `None` if any bit is
    /// unknown
    pub fn bus_value(&self, bits: &[GateNetId]) -> Option<u64> {
        bits.iter().enumerate().try_fold(0u64, |acc, (i, &net)| {
            Some(acc | (u64::from(self.value(net).to_bool()?) << i))
        })
    }

    /// Drive a net (a primary input) to `value` now
    pub fn set(&mut self, net: GateNetId, value: Logic) {
        let net = self.netlist.resolve_alias(net).0;
        self.wheel
            .push(self.now, Event::new(T_NET_DRIVE, net, value));
    }

    /// Drive a net with a clock of period `period_ps`: low now, rising half
    /// a period from now
    pub fn add_clock(&mut self, net: GateNetId, period_ps: i64) {
        let net = self.netlist.resolve_alias(net).0;
        let half_period_ps = (period_ps / 2).max(1);
        let clock = self.clocks.len() as u32;
        self.clocks.push(ClockGen {
            net,
            half_period_ps,
            value: Logic::Zero,
        });
        self.wheel
            .push(self.now, Event::new(T_NET_DRIVE, net, Logic::Zero));
        self.wheel.push(
            self.now + half_period_ps,
            Event::new(T_CLOCK_TICK, clock, Logic::One),
        );
    }

    /// Record changes of watched nets, and keep violations, only from
    /// `time_ps` on, so a long run can be looked at around one moment
    /// (violations before it are still counted)
    pub fn record_from(&mut self, time_ps: i64) {
        self.record_from_ps = time_ps;
    }

    /// Record every change of `net` (see [`Self::changes`]); a change of a
    /// watched net also ends [`Self::run_while`]'s wait for its condition
    pub fn watch(&mut self, net: GateNetId) {
        let net = self.netlist.resolve_alias(net);
        self.watched[net.0 as usize] = true;
    }

    /// Count each net's changes (its switching activity, [`Self::toggles`])
    /// from `time_ps` on, forgetting those counted so far
    pub fn count_toggles_from(&mut self, time_ps: i64) {
        self.activity_from_ps = time_ps;
        self.toggles.iter_mut().for_each(|t| *t = 0);
    }

    /// How many times each net has changed value (by net index; aliases
    /// count on the net they alias), from the time
    /// [`Self::count_toggles_from`] set (power-up by default)
    pub fn toggles(&self) -> &[u64] {
        &self.toggles
    }

    /// Changes of the watched nets so far: time, net, new value
    pub fn changes(&self) -> &[(i64, GateNetId, Logic)] {
        &self.changes
    }

    /// Changes of a watched bus (least significant bit first): time and
    /// new value, `None` while any bit is unknown. Bits changing at the same
    /// time give one change. The first value is the bus as recording began
    pub fn bus_changes(&self, bits: &[GateNetId]) -> Vec<(i64, Option<u64>)> {
        let bits: Vec<GateNetId> = bits
            .iter()
            .map(|&net| self.netlist.resolve_alias(net))
            .collect();
        let mut value: Vec<Logic> = bits
            .iter()
            .map(|&net| {
                self.recorded_before
                    .get(&net.0)
                    .copied()
                    .unwrap_or_else(|| self.value(net))
            })
            .collect();
        let read = |value: &[Logic]| -> Option<u64> {
            value.iter().enumerate().try_fold(0u64, |acc, (i, v)| {
                Some(acc | (u64::from(v.to_bool()?) << i))
            })
        };
        let mut last = read(&value);
        let mut out = Vec::new();
        let mut i = 0;
        while i < self.changes.len() {
            let time = self.changes[i].0;
            let mut touched = false;
            while i < self.changes.len() && self.changes[i].0 == time {
                let (_, net, v) = self.changes[i];
                if let Some(bit) = bits.iter().position(|&b| b == net) {
                    value[bit] = v;
                    touched = true;
                }
                i += 1;
            }
            if touched {
                let now = read(&value);
                if now != last {
                    out.push((time, now));
                    last = now;
                }
            }
        }
        out
    }

    /// Timing violations seen so far (the first ones, if there were many;
    /// see [`Self::violation_count`])
    pub fn violations(&self) -> &[Violation] {
        &self.violations
    }

    /// Number of timing violations seen so far
    pub fn violation_count(&self) -> u64 {
        self.violation_count
    }

    /// The first races of one-cell latches (up to 10 000; all of them are
    /// counted, see [`Self::latch_race_count`])
    pub fn latch_races(&self) -> &[LatchRace] {
        &self.latch_races
    }

    /// How many races one-cell latches have decided
    pub fn latch_race_count(&self) -> u64 {
        self.latch_race_count
    }

    /// The first clashes of exclusive merges (up to 10 000; all of them are
    /// counted, see [`Self::exclusive_clash_count`])
    pub fn exclusive_clashes(&self) -> &[ExclusiveClash] {
        &self.exclusive_clashes
    }

    /// How many times two grants of an exclusive merge were high at once
    pub fn exclusive_clash_count(&self) -> u64 {
        self.exclusive_clash_count
    }

    /// How many exclusive merges the netlist has
    pub fn exclusive_merge_count(&self) -> usize {
        self.exclusive_merges.len()
    }

    /// Events processed so far
    pub fn events(&self) -> u64 {
        self.events
    }

    /// Path of a cell, for reports
    pub fn cell_path(&self, cell: CellId) -> &str {
        &self.netlist.cells[cell.0 as usize].path
    }

    /// Run until time `end_ps` (events at `end_ps` included)
    pub fn run_until(&mut self, end_ps: i64) -> Result<(), TimedSimError> {
        while let Some(time) = self.next_instant() {
            if time > end_ps {
                break;
            }
            self.run_instant(time)?;
        }
        self.advance_to(end_ps);
        Ok(())
    }

    /// Run until `until` holds or time `end_ps`, whichever comes first.
    /// `until` is checked at the start and after every instant in which a
    /// watched net changed. Returns whether it held.
    pub fn run_while<F: FnMut(&Self) -> bool>(
        &mut self,
        end_ps: i64,
        mut until: F,
    ) -> Result<bool, TimedSimError> {
        if until(self) {
            return Ok(true);
        }
        while let Some(time) = self.next_instant() {
            if time > end_ps {
                break;
            }
            self.watched_changed = false;
            self.run_instant(time)?;
            if self.watched_changed && until(self) {
                return Ok(true);
            }
        }
        self.advance_to(end_ps);
        Ok(until(self))
    }

    fn advance_to(&mut self, time: i64) {
        if time > self.now {
            self.now = time;
            self.wheel.advance(time);
        }
    }

    /// The next instant with work: queued events, or nodes still to
    /// evaluate (at power-up)
    fn next_instant(&self) -> Option<i64> {
        if !self.dirty_nodes.is_empty() || !self.dirty_nets.is_empty() {
            return Some(self.now);
        }
        self.wheel.next_time()
    }

    /// Run every delta cycle of one instant
    fn run_instant(&mut self, time: i64) -> Result<(), TimedSimError> {
        self.advance_to(time);
        let mut batch = std::mem::take(&mut self.batch);
        let mut deltas = 0u32;
        loop {
            self.wheel.take_now(&mut batch);
            if batch.is_empty() && self.dirty_nodes.is_empty() && self.dirty_nets.is_empty() {
                break;
            }
            deltas += 1;
            if deltas > MAX_DELTAS {
                self.batch = batch;
                return Err(TimedSimError::ZeroDelayLoop { time_ps: time });
            }
            self.events += batch.len() as u64;
            for &event in &batch {
                self.apply(event);
            }
            self.clock_edges();
            self.evaluate_nodes();
            self.commit_nets();
        }
        self.batch = batch;
        Ok(())
    }

    /// Apply one event of the current delta
    fn apply(&mut self, event: Event) {
        let index = event.index();
        let value = event.logic();
        match event.kind() {
            T_NODE_INPUT => {
                let node = self.slot_node[index] as usize;
                let bit = self.slot_bit[index] as usize;
                let n = &mut self.nodes[node];
                if n.inputs as usize > MAX_MASK_INPUTS {
                    let at = n.wide as usize + bit;
                    if self.wide_inputs[at] == value {
                        return;
                    }
                    self.wide_inputs[at] = value;
                } else {
                    let mask = 1u64 << bit;
                    let (val, x) = match value {
                        Logic::Zero => (n.val & !mask, n.x & !mask),
                        Logic::One => (n.val | mask, n.x & !mask),
                        Logic::X => (n.val & !mask, n.x | mask),
                    };
                    if val == n.val && x == n.x {
                        return;
                    }
                    n.val = val;
                    n.x = x;
                }
                if !n.dirty {
                    n.dirty = true;
                    self.dirty_nodes.push(node as u32);
                }
            }
            T_SEQ_PIN => self.seq_pin(index, value),
            T_SEQ_CLOCK => {
                let s = &mut self.seqs[index];
                if !s.clock_touched {
                    s.clock_touched = true;
                    s.clock_before = s.clock;
                    self.clock_touched.push(index as u32);
                }
                s.clock = value;
            }
            T_NET_DRIVE => self.stage_net(index, value),
            T_MEMORY_WRITE => {
                if let Some(write) = self.writes[index].take() {
                    self.free_writes.push(index as u32);
                    let s = &mut self.seqs[write.seq as usize];
                    if s.pending_write == Some(index as u32) {
                        s.pending_write = None;
                    }
                    s.write(&write.address, &write.data, write.enable);
                    if let SeqKind::DistRamWrite { node } = s.kind {
                        self.mark_node(node as usize);
                    }
                }
            }
            T_NODE_EVAL => self.mark_node(index),
            T_OUTPUT_COMMIT => {
                let out = &mut self.node_outputs[index];
                if out.generation == event.generation {
                    out.committed = value;
                    let net = out.net as usize;
                    self.stage_net(net, value);
                }
            }
            T_CLOCK_TICK => {
                let gen = &mut self.clocks[index];
                gen.value = if gen.value == Logic::One {
                    Logic::Zero
                } else {
                    Logic::One
                };
                let (net, value, half) = (gen.net, gen.value, gen.half_period_ps);
                self.stage_net(net as usize, value);
                self.wheel.push(
                    self.now + half,
                    Event::new(T_CLOCK_TICK, index as u32, value),
                );
            }
            _ => unreachable!("unknown event kind"),
        }
    }

    fn mark_node(&mut self, node: usize) {
        if !self.nodes[node].dirty {
            self.nodes[node].dirty = true;
            self.dirty_nodes.push(node as u32);
        }
    }

    /// A net takes `value` at the end of this delta
    fn stage_net(&mut self, net: usize, value: Logic) {
        self.net_next[net] = value;
        if !self.net_dirty[net] {
            self.net_dirty[net] = true;
            self.dirty_nets.push(net as u32);
        }
    }

    /// A clocked cell's data pin changes
    fn seq_pin(&mut self, slot: usize, value: Logic) {
        if self.pin_val[slot] == value {
            return;
        }
        let now = self.now;
        self.pin_prev[slot] = self.pin_val[slot];
        self.pin_val[slot] = value;
        self.pin_changed_ps[slot] = now;
        let s = self.pin_seq[slot] as usize;
        let pin = slot - self.seqs[s].pin_start as usize;
        // A change inside the hold window: the last edge may have taken the
        // new value instead
        if !self.pin_is_clock[slot] {
            let last_edge = self.seqs[s].last_edge_ps;
            let window_end = last_edge + self.pin_hold_ps[slot];
            if now >= last_edge && now < window_end {
                self.violation(Violation {
                    time_ps: now,
                    kind: ViolationKind::Hold,
                    cell: CellId(self.seqs[s].cell),
                    input: Some(pin),
                    margin_ps: window_end - now,
                });
                let taken = self.pin_cap[slot];
                let resolved = self.resolve(taken, value);
                if resolved != taken {
                    self.pin_cap[slot] = resolved;
                    let at = (last_edge + self.seqs[s].clock_to_out_ps).max(now);
                    self.capture(s, at, true);
                }
            }
        }
        // An asynchronous reset acts at once
        if let SeqKind::Register(reg) = self.seqs[s].kind {
            if reg.async_reset && reg.reset == Some(pin as u16) && value != Logic::Zero {
                let target = Logic::from_bool(reg.sets);
                let next = if value == Logic::One {
                    target
                } else {
                    self.seqs[s].state.merge(target)
                };
                self.seqs[s].state = next;
                let at = now + self.seqs[s].clock_to_out_ps;
                self.drive_outputs(s, at, next);
            }
        }
    }

    /// What a clocked element takes from an input that changed from
    /// `before` to `after` inside its timing window: either, if they
    /// agree; else unknown, or one of them picked at random
    /// ([`Metastability`])
    fn resolve(&mut self, before: Logic, after: Logic) -> Logic {
        if before == after {
            return before;
        }
        match self.metastability {
            Metastability::Unknown => Logic::X,
            Metastability::Resolve(_) => {
                if before == Logic::X || after == Logic::X {
                    Logic::X
                } else if self.coin() {
                    after
                } else {
                    before
                }
            }
        }
    }

    /// A fair coin from the simulation's own generator (xorshift64*), so a
    /// run with the same seed repeats
    fn coin(&mut self) -> bool {
        let mut x = self.rng;
        x ^= x >> 12;
        x ^= x << 25;
        x ^= x >> 27;
        self.rng = x;
        (x.wrapping_mul(0x2545_F491_4F6C_DD1D) >> 63) == 1
    }

    fn violation(&mut self, violation: Violation) {
        self.violation_count += 1;
        if self.violations.len() < MAX_KEPT_VIOLATIONS && violation.time_ps >= self.record_from_ps {
            self.violations.push(violation);
        }
    }

    /// Drive every output net of clocked cell `s` to `value` at `at`
    fn drive_outputs(&mut self, s: usize, at: i64, value: Logic) {
        let seq = &self.seqs[s];
        let (start, len) = (seq.out_start as usize, seq.out_len as usize);
        for i in start..start + len {
            let net = self.seq_outputs[i];
            if net != NO_NET {
                self.wheel.push(at, Event::new(T_NET_DRIVE, net, value));
            }
        }
    }

    /// Clock pins touched in this delta: act on the edges
    fn clock_edges(&mut self) {
        let touched = std::mem::take(&mut self.clock_touched);
        for &s in &touched {
            let s = s as usize;
            self.seqs[s].clock_touched = false;
            let (before, after) = (self.seqs[s].clock_before, self.seqs[s].clock);
            if before == after {
                continue;
            }
            if !self.seqs[s].clock_known {
                // Power-up: the first known value is not an edge
                self.seqs[s].clock_known = after != Logic::X;
                continue;
            }
            let edge = match self.seqs[s].kind {
                SeqKind::Register(reg) => reg.edge,
                _ => Edge::Rising,
            };
            let (from, to) = match edge {
                Edge::Rising => (Logic::Zero, Logic::One),
                Edge::Falling => (Logic::One, Logic::Zero),
            };
            let certain = before == from && after == to;
            if !certain && before != Logic::X && after != Logic::X {
                continue;
            }
            self.edge(s, certain);
        }
        let mut touched = touched;
        touched.clear();
        self.clock_touched = touched;
    }

    /// An active edge (or a possible one, if not `certain`) of clocked cell
    /// `s`: check setup, take the pins, and act on them
    fn edge(&mut self, s: usize, certain: bool) {
        let now = self.now;
        let cell = CellId(self.seqs[s].cell);
        let (start, pins) = (self.seqs[s].pin_start as usize, self.seqs[s].pins as usize);
        let mut unknown_edge = false;
        if !certain {
            self.violation(Violation {
                time_ps: now,
                kind: ViolationKind::UnknownClock,
                cell,
                input: None,
                margin_ps: 0,
            });
            match self.metastability {
                // Whether there was an edge at all is unknown
                Metastability::Unknown => unknown_edge = true,
                // There was one, or there was not
                Metastability::Resolve(_) => {
                    if !self.coin() {
                        return;
                    }
                }
            }
        }
        // Setup: a pin that changed within its setup time before the edge
        // may be taken at its old or its new value
        for slot in start..start + pins {
            let current = self.pin_val[slot];
            if self.pin_is_clock[slot] {
                self.pin_cap[slot] = current;
                continue;
            }
            let window_start = now - self.pin_setup_ps[slot];
            let changed = self.pin_changed_ps[slot];
            let taken = if unknown_edge {
                Logic::X
            } else if changed > window_start && changed <= now {
                self.violation(Violation {
                    time_ps: now,
                    kind: ViolationKind::Setup,
                    cell,
                    input: Some(slot - start),
                    margin_ps: changed - window_start,
                });
                let before = self.pin_prev[slot];
                self.resolve(before, current)
            } else {
                current
            };
            self.pin_cap[slot] = taken;
        }
        let seq = &mut self.seqs[s];
        seq.last_edge_ps = now;
        seq.state_before = seq.state;
        seq.undo_addresses.clear();
        seq.undo_words.clear();
        seq.pending_write = None;
        let at = now + seq.clock_to_out_ps;
        self.capture(s, at, false);
        if unknown_edge {
            if let SeqKind::Register(_) = self.seqs[s].kind {
                // No edge would have kept the old state
                let seq = &mut self.seqs[s];
                seq.state = seq.state.merge(seq.state_before);
                let state = seq.state;
                self.drive_outputs(s, at, state);
            }
        }
    }

    /// Act on what clocked cell `s` took at its last edge (`pin_cap`),
    /// with its outputs changing at `at`. A `redo` replaces what the edge
    /// did before, after a hold violation changed what it took.
    fn capture(&mut self, s: usize, at: i64, redo: bool) {
        let start = self.seqs[s].pin_start as usize;
        let pins = self.seqs[s].pins as usize;
        match self.seqs[s].kind {
            SeqKind::Register(reg) => {
                let next = register_next(
                    &reg,
                    &self.pin_cap[start..start + pins],
                    self.seqs[s].state_before,
                );
                self.seqs[s].state = next;
                self.drive_outputs(s, at, next);
            }
            SeqKind::RamBlock { addr, data } => {
                if redo {
                    self.seqs[s].undo();
                }
                let (addr, data) = (addr as usize, data as usize);
                let cap = |p: usize, default: Logic| {
                    self.pin_cap
                        .get(start + p)
                        .copied()
                        .filter(|_| p < pins)
                        .unwrap_or(default)
                };
                let read_address: Vec<Logic> = (0..addr).map(|p| cap(p, Logic::X)).collect();
                let read_clock_enable = cap(addr + 1, Logic::One);
                let read_enable = cap(addr + 2, Logic::One);
                let wd = addr + 3;
                let write_data: Vec<Logic> = (wd..wd + data).map(|p| cap(p, Logic::X)).collect();
                let wa = wd + data;
                let write_address: Vec<Logic> = (wa..wa + addr).map(|p| cap(p, Logic::X)).collect();
                let write_enable = cap(wa + addr + 2, Logic::Zero);
                let (out_start, out_len) = (
                    self.seqs[s].out_start as usize,
                    self.seqs[s].out_len as usize,
                );
                for bit in 0..out_len {
                    let net = self.seq_outputs[out_start + bit];
                    if net == NO_NET || read_clock_enable == Logic::Zero {
                        continue;
                    }
                    let word = if bit < data {
                        read_memory(&self.seqs[s].memory, data, &read_address, bit)
                    } else {
                        Logic::Zero
                    };
                    let read = match read_enable {
                        Logic::One => word,
                        Logic::Zero => Logic::Zero,
                        Logic::X => word.merge(Logic::Zero),
                    };
                    let value = match read_clock_enable {
                        Logic::X => read.merge(self.net_val[net as usize]),
                        _ => read,
                    };
                    self.wheel.push(at, Event::new(T_NET_DRIVE, net, value));
                }
                self.seqs[s].write(&write_address, &write_data, write_enable);
            }
            SeqKind::DistRamWrite { node } => {
                let cap = |p: usize| self.pin_cap[start + p];
                let mut write = PendingWrite {
                    seq: s as u32,
                    address: [Logic::X; DIST_RAM_WIDTH],
                    data: [Logic::X; DIST_RAM_WIDTH],
                    enable: cap(DIST_RAM_WRITE_ENABLE),
                };
                for b in 0..DIST_RAM_WIDTH {
                    write.data[b] = cap(b);
                    write.address[b] = cap(DIST_RAM_WIDTH + b);
                }
                // Still waiting to show: replace it
                if let Some(id) = self.seqs[s].pending_write {
                    if self.writes[id as usize].is_some() {
                        self.writes[id as usize] = Some(write);
                        return;
                    }
                }
                if redo {
                    // Already written: undo it and write what was taken now
                    self.seqs[s].undo();
                    self.seqs[s].write(&write.address, &write.data, write.enable);
                    self.mark_node(node as usize);
                    return;
                }
                if write.enable == Logic::Zero {
                    return;
                }
                let id = match self.free_writes.pop() {
                    Some(id) => {
                        self.writes[id as usize] = Some(write);
                        id
                    }
                    None => {
                        self.writes.push(Some(write));
                        (self.writes.len() - 1) as u32
                    }
                };
                self.seqs[s].pending_write = Some(id);
                // The read port's outputs follow the write after its inertia
                let inertia = i64::from(self.nodes[node as usize].inertia_ps);
                let write_at = (at - inertia).max(self.now);
                self.wheel
                    .push(write_at, Event::new(T_MEMORY_WRITE, id, Logic::One));
            }
        }
    }

    /// Evaluate every node touched in this delta
    fn evaluate_nodes(&mut self) {
        let mut dirty = std::mem::take(&mut self.dirty_nodes);
        for &node in &dirty {
            let node = node as usize;
            self.nodes[node].dirty = false;
            let (start, len) = (
                self.nodes[node].out_start as usize,
                self.nodes[node].out_len as usize,
            );
            let inertia = i64::from(self.nodes[node].inertia_ps);
            let feedback = self.node_loop[node];
            for k in 0..len {
                let value = if feedback == NO_LOOP {
                    self.node_output(node, k)
                } else {
                    self.loop_output(node, feedback as usize)
                };
                let index = start + k;
                let out = &mut self.node_outputs[index];
                if out.value == value {
                    continue;
                }
                out.value = value;
                // Any change still on its way is replaced
                out.generation = out.generation.wrapping_add(1);
                if out.net == NO_NET {
                    out.committed = value;
                } else if inertia == 0 {
                    if out.committed != value {
                        out.committed = value;
                        let net = out.net as usize;
                        self.stage_net(net, value);
                    }
                } else if out.committed != value {
                    // Back to the committed value before the change got
                    // there: the pulse is swallowed
                    let mut event = Event::new(T_OUTPUT_COMMIT, index as u32, value);
                    event.generation = out.generation;
                    self.wheel.push(self.now + inertia, event);
                }
            }
        }
        dirty.clear();
        // Keep whatever was marked while evaluating (nothing is, today)
        dirty.append(&mut self.dirty_nodes);
        self.dirty_nodes = dirty;
    }

    /// The output of latch node `node` ([`FeedbackLoop`] `l`), from its
    /// other inputs and the value it holds; a race, if this decides other
    /// than it would have before its last change came round the loop
    fn loop_output(&mut self, node: usize, l: usize) -> Logic {
        let n = &self.nodes[node];
        let out = self.node_outputs[n.out_start as usize];
        let lp = self.loops[l];
        let table = &self.tables[out.table as usize..];
        let with = |feedback: Logic| {
            let (val, x) = match feedback {
                Logic::One => (n.val | lp.mask, n.x & !lp.mask),
                Logic::Zero => (n.val & !lp.mask, n.x & !lp.mask),
                Logic::X => (n.val & !lp.mask, n.x | lp.mask),
            };
            table_lookup(table, val, x)
        };
        let held = out.value;
        let decided = with(held);
        let racing = self.now < lp.changed_ps + lp.loop_ps && lp.before != Logic::X;
        let earlier = if racing { with(lp.before) } else { decided };
        if earlier != decided {
            // Would have decided otherwise before the last change came
            // round: settles to one of the two, which then holds
            let value = match self.metastability {
                Metastability::Unknown => decided,
                Metastability::Resolve(_) => {
                    if self.coin() {
                        decided
                    } else {
                        earlier
                    }
                }
            };
            self.latch_race_count += 1;
            if self.latch_races.len() < MAX_KEPT_VIOLATIONS && self.now >= self.record_from_ps {
                self.latch_races.push(LatchRace {
                    time_ps: self.now,
                    cell: CellId(lp.cell),
                    margin_ps: lp.changed_ps + lp.loop_ps - self.now,
                });
            }
            let lp = &mut self.loops[l];
            lp.before = value;
            lp.changed_ps = self.now;
            return value;
        }
        if decided != held {
            let lp = &mut self.loops[l];
            lp.before = held;
            lp.changed_ps = self.now;
        }
        decided
    }

    /// Output `k` of node `node`, from its current inputs
    fn node_output(&self, node: usize, k: usize) -> Logic {
        let n = &self.nodes[node];
        let out = self.node_outputs[n.out_start as usize + k];
        let held = out.value;
        let input = |bit: usize| -> Logic {
            if n.inputs as usize > MAX_MASK_INPUTS {
                self.wide_inputs[n.wide as usize + bit]
            } else if (n.x >> bit) & 1 == 1 {
                Logic::X
            } else {
                Logic::from_bool((n.val >> bit) & 1 == 1)
            }
        };
        match &n.kind {
            NodeKind::Table => table_lookup(&self.tables[out.table as usize..], n.val, n.x),
            NodeKind::Threshold { m } => {
                let width = n.inputs as u32;
                let mask = if width >= 64 { !0 } else { (1u64 << width) - 1 };
                let ones = (n.val & !n.x & mask).count_ones();
                let unknown = (n.x & mask).count_ones();
                let mut result: Option<Logic> = None;
                for extra in 0..=unknown {
                    let high = ones + extra;
                    let next = if high >= u32::from(*m) {
                        Logic::One
                    } else if high == 0 {
                        Logic::Zero
                    } else {
                        held
                    };
                    result = Some(result.map_or(next, |r| r.merge(next)));
                }
                result.unwrap_or(Logic::X)
            }
            NodeKind::Latch => {
                let (en, d) = (input(0), input(1));
                match en {
                    Logic::One => d,
                    Logic::Zero => held,
                    Logic::X => held.merge(d),
                }
            }
            NodeKind::SrLatch => {
                let (s, r) = (input(0), input(1));
                match (s, r) {
                    (_, Logic::One) => Logic::Zero,
                    (Logic::One, Logic::Zero) => Logic::One,
                    (Logic::Zero, Logic::Zero) => held,
                    (Logic::X, Logic::Zero) => held.merge(Logic::One),
                    (Logic::Zero, Logic::X) => held.merge(Logic::Zero),
                    (_, Logic::X) => Logic::X,
                }
            }
            NodeKind::DistRamRead { seq } => {
                let address: Vec<Logic> = (0..DIST_RAM_WIDTH).map(input).collect();
                let seq = &self.seqs[*seq as usize];
                read_memory(
                    &seq.memory,
                    seq.width as usize,
                    &address,
                    out.table as usize,
                )
            }
            NodeKind::Wide {
                ptype,
                cell_inputs,
                pins,
                outputs,
            } => {
                let values: Vec<Logic> = (0..pins.len()).map(input).collect();
                let unknown: Vec<usize> = (0..values.len())
                    .filter(|&i| values[i] == Logic::X)
                    .collect();
                if unknown.len() > MAX_ENUMERATED_UNKNOWNS {
                    return Logic::X;
                }
                let mut bits = vec![false; *cell_inputs as usize];
                for (i, &p) in pins.iter().enumerate() {
                    bits[p as usize] = values[i] == Logic::One;
                }
                let output = outputs[k] as usize;
                let mut result: Option<bool> = None;
                for assignment in 0u32..(1 << unknown.len()) {
                    for (j, &i) in unknown.iter().enumerate() {
                        bits[pins[i] as usize] = (assignment >> j) & 1 == 1;
                    }
                    let value = evaluate_primitive(ptype, &bits)
                        .get(output)
                        .copied()
                        .unwrap_or(false);
                    match result {
                        None => result = Some(value),
                        Some(r) if r != value => return Logic::X,
                        Some(_) => {}
                    }
                }
                result.map_or(Logic::X, Logic::from_bool)
            }
        }
    }

    /// A grant of exclusive merge `k` rose: a clash if another is high too
    fn check_exclusive(&mut self, k: usize) {
        let (merge, nets) = &self.exclusive_merges[k];
        let arms: Vec<usize> = nets
            .iter()
            .enumerate()
            .filter(|&(_, &n)| self.net_val[n as usize] == Logic::One)
            .map(|(arm, _)| arm)
            .collect();
        if arms.len() < 2 {
            return;
        }
        self.exclusive_clash_count += 1;
        if self.exclusive_clashes.len() < MAX_KEPT_VIOLATIONS && self.now >= self.record_from_ps {
            self.exclusive_clashes.push(ExclusiveClash {
                time_ps: self.now,
                merge: merge.clone(),
                arms,
            });
        }
    }

    /// Nets that changed in this delta pass the change on to their fanout
    fn commit_nets(&mut self) {
        let mut dirty = std::mem::take(&mut self.dirty_nets);
        let now = self.now;
        for &net in &dirty {
            let net = net as usize;
            self.net_dirty[net] = false;
            let value = self.net_next[net];
            if self.net_val[net] == value {
                continue;
            }
            let before = std::mem::replace(&mut self.net_val[net], value);
            if now >= self.activity_from_ps {
                self.toggles[net] += 1;
            }
            if value == Logic::One && self.exclusive_of[net] != NO_MERGE {
                self.check_exclusive(self.exclusive_of[net] as usize);
            }
            if self.watched[net] {
                self.watched_changed = true;
                if now >= self.record_from_ps {
                    self.recorded_before.entry(net as u32).or_insert(before);
                    self.changes.push((now, GateNetId(net as u32), value));
                }
            }
            let (start, end) = (
                self.fan_start[net] as usize,
                self.fan_start[net + 1] as usize,
            );
            for f in &self.fan[start..end] {
                self.wheel.push(
                    now + i64::from(f.delay_ps),
                    Event {
                        target: f.target,
                        value: value as u8,
                        generation: 0,
                    },
                );
            }
        }
        dirty.clear();
        self.dirty_nets = dirty;
    }
}

/// The next state of a register at its active edge, from its pins
fn register_next(reg: &Register, pins: &[Logic], state: Logic) -> Logic {
    let d = pins.first().copied().unwrap_or(Logic::X);
    let get = |p: Option<u16>, default: Logic| {
        p.map(|p| pins.get(p as usize).copied().unwrap_or(default))
    };
    let enable = get(reg.enable, Logic::One);
    let reset = get(reg.reset, Logic::Zero);
    let options = |v: Option<Logic>| -> &'static [Option<bool>] {
        match v {
            None => &[None],
            Some(Logic::Zero) => &[Some(false)],
            Some(Logic::One) => &[Some(true)],
            Some(Logic::X) => &[Some(false), Some(true)],
        }
    };
    let mut result: Option<Logic> = None;
    for &en in options(enable) {
        for &rst in options(reset) {
            let next = if rst == Some(true) {
                Logic::from_bool(reg.sets)
            } else if en == Some(false) {
                state
            } else {
                d
            };
            result = Some(result.map_or(next, |r| r.merge(next)));
        }
    }
    result.unwrap_or(Logic::X)
}

/// Bit `bit` of the word at `address` (least significant bit first) of a
/// flat memory of `width`-bit words, over every address an unknown address
/// could be
fn read_memory(memory: &[Logic], width: usize, address: &[Logic], bit: usize) -> Logic {
    let depth = memory.len() / width.max(1);
    let mut result: Option<Logic> = None;
    let mut ok = true;
    for_addresses(address, depth, |index| {
        if !ok {
            return;
        }
        let value = memory[index * width + bit];
        let merged = result.map_or(value, |r| r.merge(value));
        result = Some(merged);
        if merged == Logic::X {
            ok = false;
        }
    });
    result.unwrap_or(Logic::X)
}

/// Write `data` at `address` if `enable`; an unknown address or enable
/// makes every word it could reach unknown where it would change
fn write_memory(
    memory: &mut [Logic],
    width: usize,
    address: &[Logic],
    data: &[Logic],
    enable: Logic,
) {
    if enable == Logic::Zero || width == 0 {
        return;
    }
    let depth = memory.len() / width;
    let certain = enable == Logic::One && !address.contains(&Logic::X);
    for_addresses(address, depth, |index| {
        let word = &mut memory[index * width..(index + 1) * width];
        for (bit, &value) in word.iter_mut().zip(data) {
            *bit = if certain { value } else { bit.merge(value) };
        }
    });
}

/// Call `f` with every address `address` could be, below `depth`
fn for_addresses(address: &[Logic], depth: usize, mut f: impl FnMut(usize)) {
    let mut base = 0usize;
    let mut free = 0usize;
    for (i, v) in address.iter().enumerate() {
        match v {
            Logic::One => base |= 1 << i,
            Logic::X => free |= 1 << i,
            Logic::Zero => {}
        }
    }
    if free == 0 {
        if base < depth {
            f(base);
        }
        return;
    }
    // Submasks of the unknown bits
    let mut sub = free;
    loop {
        let a = base | sub;
        if a < depth {
            f(a);
        }
        if sub == 0 {
            break;
        }
        sub = (sub - 1) & free;
    }
}

/// A flat memory of `depth` words of `width` bits, from initial words
fn initial_memory(words: Option<Vec<u64>>, depth: usize, width: usize) -> Vec<Logic> {
    let mut memory = Vec::with_capacity(depth * width);
    for address in 0..depth {
        let word = words
            .as_ref()
            .and_then(|w| w.get(address))
            .copied()
            .unwrap_or(0);
        memory.extend((0..width).map(|b| Logic::from_bool(b < 64 && (word >> b) & 1 == 1)));
    }
    memory
}

/// A distributed RAM's initial words, from its `INITVAL` (bit
/// `4 * word + i` is bit `i` of `word`)
fn dist_ram_init(cell: &Cell) -> Option<Vec<u64>> {
    let initval = cell
        .parameters
        .get("INITVAL")
        .and_then(|v| u64::from_str_radix(v.trim_start_matches("0x"), 16).ok())?;
    Some(
        (0..1usize << DIST_RAM_WIDTH)
            .map(|word| (initval >> (DIST_RAM_WIDTH * word)) & ((1 << DIST_RAM_WIDTH) - 1))
            .collect(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Delays between two corners are each mixed on their own, and a
    /// delay one corner lacks is left out
    #[test]
    fn delays_between_corners_mix_each_delay() {
        let cell = |wire: i64, arc: Option<i64>| CellDelays {
            wire_ps: vec![wire],
            clock_wire_ps: 2 * wire,
            arcs: vec![vec![arc]],
            clock_to_out_ps: wire,
            launches: vec![true],
            setup_ps: vec![wire],
            hold_ps: vec![wire],
        };
        let fast = DelayAnnotation {
            corner: "fast".into(),
            cells: vec![cell(100, Some(200)), cell(10, None)],
        };
        let slow = DelayAnnotation {
            corner: "slow".into(),
            cells: vec![cell(300, Some(600)), cell(30, Some(5))],
        };
        let mid = DelayAnnotation::between(&fast, &slow, 0.25).unwrap();
        assert_eq!(mid.cells[0], cell(150, Some(300)));
        assert_eq!(mid.cells[1].wire_ps, vec![15]);
        assert_eq!(mid.cells[1].arcs, vec![vec![None]]);
        assert_eq!(
            DelayAnnotation::between(&fast, &slow, 0.0).unwrap().cells,
            fast.cells
        );
        let short = DelayAnnotation {
            corner: "slow".into(),
            cells: vec![cell(1, None)],
        };
        assert!(DelayAnnotation::between(&fast, &short, 0.5).is_err());
    }
    use crate::gate_netlist::GateNet;

    /// Delays for tests: one arc and wire delay for every cell, with
    /// per-cell overrides of the arc delay by path
    struct TestDelays {
        arc_ps: i64,
        wire_ps: i64,
        clock_to_out_ps: i64,
        setup_ps: i64,
        hold_ps: i64,
        arcs: HashMap<&'static str, i64>,
    }

    impl Default for TestDelays {
        fn default() -> Self {
            Self {
                arc_ps: 100,
                wire_ps: 0,
                clock_to_out_ps: 100,
                setup_ps: 0,
                hold_ps: 0,
                arcs: HashMap::new(),
            }
        }
    }

    impl BundleDelays for TestDelays {
        fn wire_ps(&self, _: &GateNetlist, _: GateNetId, _: &Cell, _: usize) -> i64 {
            self.wire_ps
        }
        fn clock_wire_ps(&self, _: &GateNetlist, _: GateNetId, _: &Cell) -> i64 {
            0
        }
        fn arc_ps(&self, cell: &Cell, _: usize, _: usize) -> Option<i64> {
            Some(
                self.arcs
                    .get(cell.path.as_str())
                    .copied()
                    .unwrap_or(self.arc_ps),
            )
        }
        fn clock_to_out_ps(&self, _: &Cell) -> i64 {
            self.clock_to_out_ps
        }
        fn setup_ps(&self, _: &Cell, _: usize) -> i64 {
            self.setup_ps
        }
        fn hold_ps(&self, _: &Cell, _: usize) -> i64 {
            self.hold_ps
        }
    }

    fn net(nl: &mut GateNetlist, name: &str) -> GateNetId {
        nl.add_net(GateNet::new(GateNetId(0), name.to_string()))
    }

    fn gate(
        nl: &mut GateNetlist,
        path: &str,
        f: CellFunction,
        inputs: Vec<GateNetId>,
        out: GateNetId,
    ) {
        let mut cell = Cell::new_comb(
            CellId(0),
            format!("{f:?}"),
            "test".into(),
            0.0,
            path.into(),
            inputs,
            vec![out],
        );
        cell.function = Some(f);
        nl.add_cell(cell);
    }

    fn register(nl: &mut GateNetlist, path: &str, d: GateNetId, q: GateNetId, clk: GateNetId) {
        let mut cell = Cell::new_seq(
            CellId(0),
            "DFF".into(),
            "test".into(),
            0.0,
            path.into(),
            vec![d],
            vec![q],
            clk,
            None,
        );
        cell.function = Some(CellFunction::Dff);
        nl.add_cell(cell);
    }

    fn changes_of(sim: &TimedSim, net: GateNetId) -> Vec<(i64, Logic)> {
        sim.changes()
            .iter()
            .filter(|(_, n, _)| *n == net)
            .map(|(t, _, v)| (*t, *v))
            .collect()
    }

    /// Two grants of one exclusive merge high at once are reported, with
    /// the merge and its arms; one at a time, or grants of two different
    /// merges, are not
    #[test]
    fn exclusive_merge_clashes_are_reported() {
        let mut nl = GateNetlist::new("t".into(), "test".into());
        let r: Vec<GateNetId> = (0..3).map(|k| nl.add_input(format!("r{k}"))).collect();
        let other = nl.add_input("other".into());
        let grant = |nl: &mut GateNetlist, path: &str, request: GateNetId| {
            let out = nl.add_output(format!("{path}_g"));
            gate(nl, path, CellFunction::Buf, vec![request], out);
            let cell = nl.cells.last_mut().unwrap();
            cell.source_op = Some(EXCLUSIVE_OP.to_string());
        };
        for (k, &request) in r.iter().enumerate() {
            grant(
                &mut nl,
                &format!("blk.ncl_bundle.exclusive0.excl{k}"),
                request,
            );
        }
        // a second merge, of one arm, whose grant may be high with the first's
        grant(&mut nl, "blk.ncl_bundle.exclusive1.excl0", other);
        let delays = TestDelays::default();
        let mut sim = TimedSim::with_power_up(&nl, &delays, PowerUp::Zero).unwrap();
        assert_eq!(sim.exclusive_merge_count(), 2);
        for &net in r.iter().chain([&other]) {
            sim.set(net, Logic::Zero);
        }
        sim.run_until(1_000).unwrap();
        // one at a time, and with the other merge's grant up
        sim.set(other, Logic::One);
        for &net in &r {
            sim.set(net, Logic::One);
            sim.run_until(sim.now() + 1_000).unwrap();
            sim.set(net, Logic::Zero);
            sim.run_until(sim.now() + 1_000).unwrap();
        }
        assert_eq!(sim.exclusive_clash_count(), 0);
        // arms 0 and 2 at once
        sim.set(r[2], Logic::One);
        sim.run_until(sim.now() + 1_000).unwrap();
        sim.set(r[0], Logic::One);
        sim.run_until(sim.now() + 1_000).unwrap();
        let rose = sim.now() - 1_000 + 100;
        assert_eq!(sim.exclusive_clash_count(), 1);
        assert_eq!(
            sim.exclusive_clashes(),
            &[ExclusiveClash {
                time_ps: rose,
                merge: "blk.ncl_bundle.exclusive0".to_string(),
                arms: vec![0, 2],
            }]
        );
    }

    /// A change takes the wire and arc delays of each cell it goes through
    #[test]
    fn delays_add_along_a_path() {
        let mut nl = GateNetlist::new("t".into(), "test".into());
        let a = nl.add_input("a".into());
        let b = net(&mut nl, "b");
        let y = nl.add_output("y".into());
        gate(&mut nl, "buf", CellFunction::Buf, vec![a], b);
        gate(&mut nl, "inv", CellFunction::Inv, vec![b], y);
        let delays = TestDelays {
            wire_ps: 30,
            ..Default::default()
        };
        let mut sim = TimedSim::new(&nl, &delays).unwrap();
        sim.watch(y);
        sim.set(a, Logic::Zero);
        sim.run_until(1_000).unwrap();
        sim.set(a, Logic::One);
        sim.run_until(2_000).unwrap();
        // two wires and two arcs: 260 ps after each change of `a`
        assert_eq!(
            changes_of(&sim, y),
            vec![(260, Logic::One), (1_260, Logic::Zero)]
        );
    }

    /// Unequal paths to one gate make a glitch as long as their difference;
    /// equal paths make none
    #[test]
    fn glitches_follow_path_differences() {
        let mut nl = GateNetlist::new("t".into(), "test".into());
        let a = nl.add_input("a".into());
        let slow = net(&mut nl, "slow");
        let fast = net(&mut nl, "fast");
        let hazard = nl.add_output("hazard".into());
        let none = nl.add_output("none".into());
        gate(&mut nl, "slow", CellFunction::Buf, vec![a], slow);
        gate(&mut nl, "fast", CellFunction::Buf, vec![a], fast);
        gate(&mut nl, "x1", CellFunction::Xor2, vec![a, slow], hazard);
        gate(&mut nl, "x2", CellFunction::Xor2, vec![fast, slow], none);
        let mut delays = TestDelays::default();
        delays.arcs.insert("slow", 150);
        delays.arcs.insert("fast", 150);
        delays.arcs.insert("x1", 50);
        delays.arcs.insert("x2", 50);
        let mut sim = TimedSim::new(&nl, &delays).unwrap();
        sim.set(a, Logic::Zero);
        sim.run_until(1_000).unwrap();
        sim.watch(hazard);
        sim.watch(none);
        sim.set(a, Logic::One);
        sim.run_until(2_000).unwrap();
        assert_eq!(
            changes_of(&sim, hazard),
            vec![(1_050, Logic::One), (1_200, Logic::Zero)]
        );
        assert_eq!(changes_of(&sim, none), vec![]);
    }

    /// An unknown input only makes an output unknown when it matters
    #[test]
    fn unknowns_are_resolved_where_the_function_allows() {
        let mut nl = GateNetlist::new("t".into(), "test".into());
        let a = nl.add_input("a".into());
        let u = nl.add_input("u".into());
        let y = nl.add_output("y".into());
        gate(&mut nl, "and", CellFunction::And2, vec![a, u], y);
        let mut sim = TimedSim::new(&nl, &TestDelays::default()).unwrap();
        sim.set(a, Logic::Zero);
        sim.run_until(1_000).unwrap();
        assert_eq!(sim.value(y), Logic::Zero);
        sim.set(a, Logic::One);
        sim.run_until(2_000).unwrap();
        assert_eq!(sim.value(y), Logic::X);
        sim.set(u, Logic::One);
        sim.run_until(3_000).unwrap();
        assert_eq!(sim.value(y), Logic::One);
    }

    /// A threshold gate holds its output until all inputs agree
    #[test]
    fn threshold_gate_holds_between_thresholds() {
        let mut nl = GateNetlist::new("t".into(), "test".into());
        let a = nl.add_input("a".into());
        let b = nl.add_input("b".into());
        let y = nl.add_output("y".into());
        gate(&mut nl, "c", CellFunction::Th22, vec![a, b], y);
        let mut sim = TimedSim::new(&nl, &TestDelays::default()).unwrap();
        let step = |sim: &mut TimedSim, av, bv, t| {
            sim.set(a, av);
            sim.set(b, bv);
            sim.run_until(t).unwrap();
            sim.value(y)
        };
        assert_eq!(step(&mut sim, Logic::Zero, Logic::Zero, 1_000), Logic::Zero);
        assert_eq!(step(&mut sim, Logic::One, Logic::Zero, 2_000), Logic::Zero);
        assert_eq!(step(&mut sim, Logic::One, Logic::One, 3_000), Logic::One);
        assert_eq!(step(&mut sim, Logic::Zero, Logic::One, 4_000), Logic::One);
        assert_eq!(step(&mut sim, Logic::Zero, Logic::Zero, 5_000), Logic::Zero);
    }

    /// A register toggling through an inverter counts clock edges, its
    /// output one clock-to-output delay after each
    #[test]
    fn register_follows_its_clock() {
        let mut nl = GateNetlist::new("t".into(), "test".into());
        let clk = nl.add_clock("clk".into());
        let q = nl.add_output("q".into());
        let d = net(&mut nl, "d");
        gate(&mut nl, "inv", CellFunction::Inv, vec![q], d);
        register(&mut nl, "r", d, q, clk);
        let mut sim = TimedSim::new(&nl, &TestDelays::default()).unwrap();
        sim.watch(q);
        sim.add_clock(clk, 1_000);
        sim.run_until(3_900).unwrap();
        // the initial value from power-up, then a toggle 100 ps after each
        // rise
        assert_eq!(
            changes_of(&sim, q),
            vec![
                (600, Logic::One),
                (1_600, Logic::Zero),
                (2_600, Logic::One),
                (3_600, Logic::Zero)
            ]
        );
        assert!(sim.violations().is_empty());
    }

    /// Data that changes inside the setup or hold window is captured as
    /// unknown, and the violation is reported with its margin
    #[test]
    fn setup_and_hold_violations_capture_unknown() {
        for (offset, kind, margin) in [
            (-30, Some(ViolationKind::Setup), 50),
            (20, Some(ViolationKind::Hold), 20),
            (-100, None, 0),
            (60, None, 0),
        ] {
            let mut nl = GateNetlist::new("t".into(), "test".into());
            let clk = nl.add_clock("clk".into());
            let d = nl.add_input("d".into());
            let q = nl.add_output("q".into());
            register(&mut nl, "r", d, q, clk);
            let delays = TestDelays {
                setup_ps: 80,
                hold_ps: 40,
                ..Default::default()
            };
            let mut sim = TimedSim::new(&nl, &delays).unwrap();
            sim.set(d, Logic::Zero);
            sim.set(clk, Logic::Zero);
            // the clock rises at 1000 ps, the data at 1000 + offset
            let mut steps = [(1_000, clk), (1_000 + offset, d)];
            steps.sort_by_key(|(t, _)| *t);
            for (t, net) in steps {
                sim.run_until(t).unwrap();
                sim.set(net, Logic::One);
            }
            sim.run_until(3_000).unwrap();
            let got: Vec<_> = sim
                .violations()
                .iter()
                .map(|v| (v.kind, v.margin_ps))
                .collect();
            match kind {
                Some(kind) => {
                    assert_eq!(got, vec![(kind, margin)], "offset {offset}");
                    assert_eq!(sim.value(q), Logic::X, "offset {offset}");
                }
                None => {
                    assert!(got.is_empty(), "offset {offset}: {got:?}");
                    let expected = if offset < 0 { Logic::One } else { Logic::Zero };
                    assert_eq!(sim.value(q), expected, "offset {offset}");
                }
            }
        }
    }

    /// With metastability resolved, a violated register settles to the
    /// old or the new value (each seen over some seeds), never unknown,
    /// and the same seed repeats its choice
    #[test]
    fn resolved_violations_take_the_old_or_the_new_value() {
        for offset in [-30i64, 20] {
            let run = |seed: u64| {
                let mut nl = GateNetlist::new("t".into(), "test".into());
                let clk = nl.add_clock("clk".into());
                let d = nl.add_input("d".into());
                let q = nl.add_output("q".into());
                register(&mut nl, "r", d, q, clk);
                let delays = TestDelays {
                    setup_ps: 80,
                    hold_ps: 40,
                    ..Default::default()
                };
                let mut sim = TimedSim::new(&nl, &delays).unwrap();
                sim.set_metastability(Metastability::Resolve(seed));
                sim.set(d, Logic::Zero);
                sim.set(clk, Logic::Zero);
                let mut steps = [(1_000, clk), (1_000 + offset, d)];
                steps.sort_by_key(|(t, _)| *t);
                for (t, net) in steps {
                    sim.run_until(t).unwrap();
                    sim.set(net, Logic::One);
                }
                sim.run_until(3_000).unwrap();
                assert_eq!(sim.violation_count(), 1, "offset {offset}");
                sim.value(q)
            };
            let results: Vec<Logic> = (1..=32).map(run).collect();
            assert!(!results.contains(&Logic::X), "offset {offset}");
            assert!(results.contains(&Logic::Zero), "offset {offset}");
            assert!(results.contains(&Logic::One), "offset {offset}");
            assert_eq!(run(7), run(7), "offset {offset}");
        }
    }

    /// A hold violation that resolves to the new value redoes the edge:
    /// the output moves to what the edge took, without passing through
    /// unknown
    #[test]
    fn a_hold_violation_redoes_the_edge() {
        let mut nl = GateNetlist::new("t".into(), "test".into());
        let clk = nl.add_clock("clk".into());
        let d = nl.add_input("d".into());
        let q = nl.add_output("q".into());
        register(&mut nl, "r", d, q, clk);
        let delays = TestDelays {
            setup_ps: 80,
            hold_ps: 40,
            ..Default::default()
        };
        // Find a seed whose coin takes the new value
        let mut seen_one = false;
        for seed in 1..=32 {
            let mut sim = TimedSim::new(&nl, &delays).unwrap();
            sim.set_metastability(Metastability::Resolve(seed));
            sim.watch(q);
            sim.set(d, Logic::Zero);
            sim.set(clk, Logic::Zero);
            sim.run_until(1_000).unwrap();
            sim.set(clk, Logic::One);
            sim.run_until(1_020).unwrap();
            sim.set(d, Logic::One);
            sim.run_until(3_000).unwrap();
            if sim.value(q) == Logic::One {
                seen_one = true;
                // q never showed anything but its final value after power-up
                let last = sim.changes().last().map(|c| c.2);
                assert_eq!(last, Some(Logic::One));
                assert!(sim.changes().iter().all(|c| c.2 != Logic::X));
            }
        }
        assert!(seen_one);
    }

    /// A ring of inverters with delay oscillates; with no delay it is
    /// reported as a loop
    #[test]
    fn rings_oscillate_or_report_a_zero_delay_loop() {
        let ring = |arc_ps: i64| {
            let mut nl = GateNetlist::new("t".into(), "test".into());
            let en = nl.add_input("en".into());
            let n: Vec<GateNetId> = (0..3).map(|i| net(&mut nl, &format!("n{i}"))).collect();
            gate(&mut nl, "nand", CellFunction::Nand2, vec![en, n[2]], n[0]);
            gate(&mut nl, "i1", CellFunction::Inv, vec![n[0]], n[1]);
            gate(&mut nl, "i2", CellFunction::Inv, vec![n[1]], n[2]);
            let delays = TestDelays {
                arc_ps,
                ..Default::default()
            };
            (nl, delays, en, n[0])
        };
        let (nl, delays, en, out) = ring(100);
        let mut sim = TimedSim::new(&nl, &delays).unwrap();
        sim.set(en, Logic::Zero);
        sim.run_until(1_000).unwrap();
        sim.watch(out);
        sim.set(en, Logic::One);
        sim.run_until(2_500).unwrap();
        let times: Vec<i64> = changes_of(&sim, out).iter().map(|(t, _)| *t).collect();
        assert_eq!(times, vec![1_100, 1_400, 1_700, 2_000, 2_300]);

        let (nl, delays, en, _) = ring(0);
        let mut sim = TimedSim::new(&nl, &delays).unwrap();
        sim.set(en, Logic::Zero);
        sim.run_until(1_000).unwrap();
        sim.set(en, Logic::One);
        assert!(matches!(
            sim.run_until(2_000),
            Err(TimedSimError::ZeroDelayLoop { time_ps: 1_000 })
        ));
    }

    /// A distributed RAM writes on its clock edge and reads its address
    /// combinationally
    #[test]
    fn distributed_ram_writes_on_the_clock_and_reads_at_once() {
        let mut nl = GateNetlist::new("t".into(), "test".into());
        let clk = nl.add_clock("clk".into());
        let ins: Vec<GateNetId> = (0..DIST_RAM_READ_ADDRESS + DIST_RAM_WIDTH)
            .map(|i| nl.add_input(format!("i{i}")))
            .collect();
        let outs: Vec<GateNetId> = (0..DIST_RAM_WIDTH)
            .map(|i| nl.add_output(format!("o{i}")))
            .collect();
        let mut cell = Cell::new_seq(
            CellId(0),
            "DPR16X4".into(),
            "test".into(),
            0.0,
            "ram".into(),
            ins.clone(),
            outs.clone(),
            clk,
            None,
        );
        cell.function = Some(CellFunction::DistRam16x4);
        cell.parameters
            .insert("INITVAL".into(), "0x0000000000000050".into());
        nl.add_cell(cell);
        let mut sim = TimedSim::new(&nl, &TestDelays::default()).unwrap();
        let set_bits = |sim: &mut TimedSim, start: usize, value: u64| {
            for b in 0..DIST_RAM_WIDTH {
                sim.set(ins[start + b], Logic::from_bool((value >> b) & 1 == 1));
            }
        };
        sim.add_clock(clk, 1_000);
        set_bits(&mut sim, 0, 0xA); // data
        set_bits(&mut sim, DIST_RAM_WIDTH, 3); // write address
        sim.set(ins[DIST_RAM_WRITE_ENABLE], Logic::Zero);
        set_bits(&mut sim, DIST_RAM_READ_ADDRESS, 1); // read address
        sim.run_until(400).unwrap();
        assert_eq!(sim.bus_value(&outs), Some(5), "initial word 1");
        set_bits(&mut sim, DIST_RAM_READ_ADDRESS, 3);
        sim.run_until(1_400).unwrap();
        assert_eq!(sim.bus_value(&outs), Some(0), "word 3 before the write");
        sim.set(ins[DIST_RAM_WRITE_ENABLE], Logic::One);
        sim.run_until(1_550).unwrap();
        // written at the rise at 1500, shown 100 ps later
        assert_eq!(sim.bus_value(&outs), Some(0));
        sim.run_until(1_600).unwrap();
        assert_eq!(sim.bus_value(&outs), Some(0xA));
    }

    /// A captured annotation gives the same delays as the model it came
    /// from, and survives a round trip through JSON
    #[test]
    fn annotation_reproduces_its_model() {
        let mut nl = GateNetlist::new("t".into(), "test".into());
        let clk = nl.add_clock("clk".into());
        let a = nl.add_input("a".into());
        let b = net(&mut nl, "b");
        let q = nl.add_output("q".into());
        gate(&mut nl, "buf", CellFunction::Buf, vec![a], b);
        register(&mut nl, "r", b, q, clk);
        let mut delays = TestDelays {
            wire_ps: 7,
            setup_ps: 11,
            hold_ps: 13,
            clock_to_out_ps: 17,
            ..Default::default()
        };
        delays.arcs.insert("buf", 19);
        let annotation = DelayAnnotation::capture(&nl, &delays, "slow");
        let json = serde_json::to_string(&annotation).unwrap();
        let back: DelayAnnotation = serde_json::from_str(&json).unwrap();
        for (model, name) in [
            (&delays as &dyn BundleDelays, "model"),
            (&back, "annotation"),
        ] {
            let buf = &nl.cells[0];
            let reg = &nl.cells[1];
            assert_eq!(model.arc_ps(buf, 0, 0), Some(19), "{name}");
            assert_eq!(model.wire_ps(&nl, a, buf, 0), 7, "{name}");
            assert_eq!(model.clock_to_out_ps(reg), 17, "{name}");
            assert_eq!(model.setup_ps(reg, 0), 11, "{name}");
            assert_eq!(model.hold_ps(reg, 0), 13, "{name}");
        }
        assert_eq!(back.corner, "slow");
    }

    /// A latch made of two gates keeps an unknown power-up value until an
    /// input forces it; from a definite power-up it starts definite
    #[test]
    fn gate_latch_starts_from_its_power_up_value() {
        let mut nl = GateNetlist::new("t".into(), "test".into());
        let s_in = nl.add_input("s".into());
        let r_in = nl.add_input("r".into());
        let q = nl.add_output("q".into());
        let qn = net(&mut nl, "qn");
        gate(&mut nl, "n1", CellFunction::Nor2, vec![r_in, qn], q);
        gate(&mut nl, "n2", CellFunction::Nor2, vec![s_in, q], qn);
        let delays = TestDelays::default();
        let hold = |sim: &mut TimedSim| {
            sim.set(s_in, Logic::Zero);
            sim.set(r_in, Logic::Zero);
            sim.run_until(1_000).unwrap();
            (sim.value(q), sim.value(qn))
        };
        let mut sim = TimedSim::new(&nl, &delays).unwrap();
        assert_eq!(hold(&mut sim), (Logic::X, Logic::X));
        sim.set(r_in, Logic::One);
        sim.run_until(2_000).unwrap();
        assert_eq!((sim.value(q), sim.value(qn)), (Logic::Zero, Logic::One));
        // From a power-up of (1, 0) or (0, 1), with both inputs low, it
        // holds. (From (0, 0) or (1, 1) the two gates race: a real
        // oscillation in a perfectly matched latch.)
        let mut tried = 0;
        for seed in 0..64 {
            let power_up = PowerUp::Random(seed);
            let (pq, pqn) = (power_up.value(q.0), power_up.value(qn.0));
            let inputs_low =
                power_up.value(s_in.0) == Logic::Zero && power_up.value(r_in.0) == Logic::Zero;
            if pq == pqn || !inputs_low {
                continue;
            }
            tried += 1;
            let mut sim = TimedSim::with_power_up(&nl, &delays, power_up).unwrap();
            assert_eq!(hold(&mut sim), (pq, pqn), "seed {seed}");
        }
        assert!(tried >= 2, "too few seeds tried");
    }

    /// A latch in one LUT (a mutex: `b` sets it, `a` clears it, both
    /// high hold it) with its output wired back to two of its inputs:
    /// `(nl, a, b, y)`
    fn lut_mutex() -> (GateNetlist, GateNetId, GateNetId, GateNetId) {
        let mut nl = GateNetlist::new("t".into(), "test".into());
        let a = nl.add_input("a".into());
        let b = nl.add_input("b".into());
        let y = nl.add_output("y".into());
        let mut cell = Cell::new_comb(
            CellId(0),
            "LUT4".into(),
            "test".into(),
            0.0,
            "mutex".into(),
            vec![a, b, y, y],
            vec![y],
        );
        cell.lut_init = Some(0xd4d4);
        nl.add_cell(cell);
        (nl, a, b, y)
    }

    /// Run the mutex, cleared and then with both inputs low: `first` rises
    /// at 1 ns and `second` `apart` ps later. Returns the sim at 10 ns.
    fn race_mutex<'a>(
        nl: &'a GateNetlist,
        delays: &TestDelays,
        metastability: Metastability,
        (first, second): (GateNetId, GateNetId),
        apart: i64,
    ) -> TimedSim<'a> {
        let mut sim = TimedSim::new(nl, delays).unwrap();
        sim.set_metastability(metastability);
        let clear = nl.inputs[0];
        sim.set(first, Logic::Zero);
        sim.set(second, Logic::Zero);
        sim.set(clear, Logic::One);
        sim.run_until(500).unwrap();
        sim.set(clear, Logic::Zero);
        sim.run_until(1_000).unwrap();
        sim.set(first, Logic::One);
        sim.run_until(1_000 + apart).unwrap();
        sim.set(second, Logic::One);
        sim.run_until(10_000).unwrap();
        sim
    }

    /// A one-cell latch decides by which input came first; inputs closer
    /// than one trip round its loop race, and the race settles to a
    /// definite value instead of leaving a pulse circling the loop
    #[test]
    fn one_cell_latches_settle_their_races() {
        let (nl, a, b, y) = lut_mutex();
        // Arc 100 ps, wire 200 ps: the loop takes 300 ps
        let delays = TestDelays {
            wire_ps: 200,
            ..Default::default()
        };
        // Far apart: the first input decides, and nothing is reported
        for (order, held) in [((b, a), Logic::One), ((a, b), Logic::Zero)] {
            let mut sim = race_mutex(&nl, &delays, Metastability::Unknown, order, 400);
            assert_eq!(sim.value(y), held);
            assert_eq!(sim.latch_race_count(), 0);
            // Still held once the loser lets go
            sim.set(order.1, Logic::Zero);
            sim.run_until(12_000).unwrap();
            assert_eq!(sim.value(y), held);
        }
        // 150 ps apart, after the output changed (100 ps) but before the
        // change came round (300 ps)
        let settled_by = |sim: &mut TimedSim| {
            sim.watch(y);
            sim.run_until(20_000).unwrap();
            assert!(changes_of(sim, y).is_empty(), "still changing");
            sim.value(y)
        };
        let mut outcomes = Vec::new();
        for seed in 1..=32 {
            let mut sim = race_mutex(&nl, &delays, Metastability::Resolve(seed), (b, a), 150);
            assert_eq!(sim.latch_race_count(), 1, "seed {seed}");
            assert_eq!(sim.violation_count(), 0, "seed {seed}");
            let race = &sim.latch_races()[0];
            assert_eq!(race.cell, CellId(0));
            // Each input takes its 200 ps wire too: b at 1.2 ns, a at 1.35 ns
            assert_eq!((race.time_ps, race.margin_ps), (1_350, 150));
            outcomes.push(settled_by(&mut sim));
        }
        assert!(outcomes.contains(&Logic::Zero));
        assert!(outcomes.contains(&Logic::One));
        assert!(!outcomes.contains(&Logic::X));
        // Without a seed the first input wins: never unknown, which a latch
        // would hold for ever
        let mut sim = race_mutex(&nl, &delays, Metastability::Unknown, (b, a), 150);
        assert_eq!(sim.latch_race_count(), 1);
        assert_eq!(settled_by(&mut sim), Logic::One);
        // Clearing a latch that is already clear changes nothing, so the
        // set that follows has nothing to race
        let mut sim = race_mutex(&nl, &delays, Metastability::Unknown, (a, b), 150);
        assert_eq!(sim.latch_race_count(), 0);
        assert_eq!(settled_by(&mut sim), Logic::Zero);
    }

    /// Far events (beyond the wheel's horizon) still run in order
    #[test]
    fn events_beyond_the_wheel_run_in_order() {
        let mut nl = GateNetlist::new("t".into(), "test".into());
        let a = nl.add_input("a".into());
        let near = nl.add_output("near".into());
        let far = nl.add_output("far".into());
        gate(&mut nl, "near", CellFunction::Buf, vec![a], near);
        gate(&mut nl, "far", CellFunction::Buf, vec![a], far);
        let mut delays = TestDelays::default();
        let (d_near, d_far) = (3 * WHEEL_SLOTS as i64 + 7, 5 * WHEEL_SLOTS as i64 + 3);
        delays.arcs.insert("near", d_near);
        delays.arcs.insert("far", d_far);
        let mut sim = TimedSim::new(&nl, &delays).unwrap();
        sim.set(a, Logic::Zero);
        sim.run_until(10 * WHEEL_SLOTS as i64).unwrap();
        sim.watch(near);
        sim.watch(far);
        let t0 = sim.now();
        sim.set(a, Logic::One);
        sim.run_until(t0 + 10 * WHEEL_SLOTS as i64).unwrap();
        let got: Vec<_> = sim.changes().iter().map(|&(t, _, v)| (t - t0, v)).collect();
        assert_eq!(got, vec![(d_near, Logic::One), (d_far, Logic::One)]);
    }

    /// A gate swallows a pulse narrower than its delay (inertial delay)
    /// and passes a wider one
    #[test]
    fn gates_swallow_pulses_narrower_than_their_delay() {
        for (width, passes) in [(40, false), (99, false), (100, true), (250, true)] {
            let mut nl = GateNetlist::new("t".into(), "test".into());
            let a = nl.add_input("a".into());
            let y = nl.add_output("y".into());
            gate(&mut nl, "buf", CellFunction::Buf, vec![a], y);
            let delays = TestDelays {
                arc_ps: 100,
                ..Default::default()
            };
            let mut sim = TimedSim::new(&nl, &delays).unwrap();
            sim.set(a, Logic::Zero);
            sim.run_until(1_000).unwrap();
            sim.watch(y);
            sim.set(a, Logic::One);
            sim.run_until(1_000 + width).unwrap();
            sim.set(a, Logic::Zero);
            sim.run_until(3_000).unwrap();
            let expected = if passes {
                vec![(1_100, Logic::One), (1_100 + width, Logic::Zero)]
            } else {
                vec![]
            };
            assert_eq!(changes_of(&sim, y), expected, "width {width}");
        }
    }

    /// Each net's changes are counted from the time asked for
    #[test]
    fn toggles_count_each_nets_changes_from_a_time() {
        let mut nl = GateNetlist::new("t".into(), "test".into());
        let clk = nl.add_clock("clk".into());
        let a = nl.add_input("a".into());
        let y = nl.add_output("y".into());
        gate(&mut nl, "buf", CellFunction::Buf, vec![a], y);
        let mut sim = TimedSim::new(&nl, &TestDelays::default()).unwrap();
        sim.set(a, Logic::Zero);
        sim.add_clock(clk, 1_000);
        // Changes before 5.25 ns are not counted
        sim.set(a, Logic::One);
        sim.run_until(5_000).unwrap();
        sim.count_toggles_from(5_250);
        sim.run_until(5_300).unwrap();
        for k in 0..3 {
            sim.set(a, if k % 2 == 1 { Logic::One } else { Logic::Zero });
            sim.run_until(6_300 + k * 1_000).unwrap();
        }
        sim.run_until(10_250).unwrap();
        let toggles = sim.toggles();
        assert_eq!(toggles[a.0 as usize], 3);
        assert_eq!(toggles[y.0 as usize], 3);
        // Edges every 0.5 ns, 5.5 to 10 ns
        assert_eq!(toggles[clk.0 as usize], 10);
    }

    /// A vendor block with no model (a PLL) drives nothing: its output
    /// follows only what the caller drives on it, not the block's inputs
    #[test]
    fn blackboxes_leave_their_outputs_to_the_caller() {
        let mut nl = GateNetlist::new("t".into(), "test".into());
        let refclk = nl.add_clock("refclk".into());
        let out = nl.add_net(crate::gate_netlist::GateNet::new(
            GateNetId(0),
            "pll_out".into(),
        ));
        let y = nl.add_output("y".into());
        let mut pll = Cell::new_comb(
            CellId(0),
            "PLL_X".into(),
            "test".into(),
            0.0,
            "pll".into(),
            vec![refclk],
            vec![out],
        );
        pll.parameters.insert(
            crate::gate_netlist::BLACKBOX_INPUTS_PARAM.into(),
            "REF".into(),
        );
        pll.parameters
            .insert(BLACKBOX_OUTPUTS_PARAM.into(), "OUT".into());
        nl.add_cell(pll);
        gate(&mut nl, "buf", CellFunction::Buf, vec![out], y);
        let mut sim = TimedSim::new(&nl, &TestDelays::default()).unwrap();
        assert_eq!(sim.blackboxes.len(), 1);
        assert_eq!(sim.blackboxes[0].outputs, vec![nl.resolve_alias(out)]);
        // The reference clock alone leaves the output unknown
        sim.add_clock(refclk, 800);
        sim.run_until(5_000).unwrap();
        assert_eq!(sim.value(out), Logic::X);
        // Driven as a clock of its own, it toggles at that clock only
        sim.watch(y);
        sim.add_clock(out, 3_000);
        sim.run_until(20_000).unwrap();
        let edges: Vec<i64> = sim.changes().iter().map(|&(t, _, _)| t).collect();
        assert!(edges.len() >= 9, "{edges:?}");
        for pair in edges[1..].windows(2) {
            assert_eq!(pair[1] - pair[0], 1_500, "{edges:?}");
        }
    }

    /// A watched bus reports one value per moment its bits settle, starting
    /// from the value it had when recording began
    #[test]
    fn bus_changes_combine_bits_that_change_together() {
        let mut nl = GateNetlist::new("t".into(), "test".into());
        let a0 = nl.add_input("a0".into());
        let a1 = nl.add_input("a1".into());
        let y0 = nl.add_output("y[0]".into());
        let y1 = nl.add_output("y[1]".into());
        gate(&mut nl, "b0", CellFunction::Buf, vec![a0], y0);
        gate(&mut nl, "b1", CellFunction::Buf, vec![a1], y1);
        let mut delays = TestDelays::default();
        delays.arcs.insert("b1", 200);
        let mut sim = TimedSim::new(&nl, &delays).unwrap();
        sim.set(a0, Logic::Zero);
        sim.set(a1, Logic::One);
        sim.run_until(1_000).unwrap();
        sim.watch(y0);
        sim.watch(y1);
        // 0b10: y0 rises at 1100 (0b11), y1 falls at 1200 (0b01), then y0
        // falls at 2100 (0b00)
        sim.set(a0, Logic::One);
        sim.set(a1, Logic::Zero);
        sim.run_until(2_000).unwrap();
        sim.set(a0, Logic::Zero);
        sim.run_until(3_000).unwrap();
        assert_eq!(
            sim.bus_changes(&[y0, y1]),
            vec![(1_100, Some(3)), (1_200, Some(1)), (2_100, Some(0))]
        );
        // A bit that never changed holds its value throughout
        assert_eq!(sim.bus_changes(&[y1]), vec![(1_200, Some(0))]);
    }
}
