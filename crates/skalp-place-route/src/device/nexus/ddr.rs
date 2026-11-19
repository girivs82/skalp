//! DDR I/O blocks of a Nexus device: the I/O logic's gearing and delays
//! (`IOLOGIC`), the DQS buffers, the DDR delay-locked loops, and the edge
//! clock synchronizers and dividers, placed, routed and configured from the
//! device data.
//!
//! The device data says what each of the vendor's DDR primitives is
//! (`<family>/primitives.json`): the block it occupies, which of its ports
//! connect inside the I/O (to the pad, or to another primitive at the same
//! pad) rather than through the fabric, the block pin behind each other
//! port, the settings each connection implies, and its parameters. SKALP
//! knows only how Nexus devices name these blocks' wires and settings
//! ([`SiteKind`]); which settings a primitive makes is the device's.
//!
//! A design uses them as blackbox cells named after the primitive. The
//! primitives at one pad (an output gearbox, a tristate gearbox, an input
//! delay and an input gearbox, say) all go to that pad's I/O logic; a DQS
//! buffer goes to the DQS group of its strobe pad; the edge clock blocks go
//! to the bank of the I/O they clock.

use super::prjoxide_load::PrjoxideTile;
use crate::collections::{HashMap, HashMapExt, HashSet};
use serde::Deserialize;
use skalp_lir::gate_netlist::{Cell, CellId, GateNetId, GateNetlist};
use skalp_lir::tech_library::CellFunction;
use std::collections::BTreeMap;
use std::path::Path;

/// The device data file, in the family's directory.
const FILE: &str = "primitives.json";

/// First router pin number of a DDR block's fabric pins: the block's pins
/// are numbered from here in [`Primitives::pins`] order, clear of the
/// numbers logic cells use.
pub const ROUTER_PIN_BASE: usize = 1000;

/// The kinds of DDR block, as the device data names them, and how a Nexus
/// device names their wires and settings.
#[derive(Deserialize, Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[serde(rename_all = "UPPERCASE")]
pub enum SiteKind {
    /// A pad's I/O logic: gearboxes and delays (`IOLOGIC_CORE`), one per pad
    Iologic,
    /// A DQS group's strobe buffer (`DQSBUFA_CORE`)
    Dqsbuf,
    /// A DDR delay-locked loop (`DDRDLL_CORE`)
    Ddrdll,
    /// An edge clock synchronizer (`ECLKSYNC_CORE`), several per bank
    Eclksync,
    /// An edge clock divider (`ECLKDIV_CORE`), several per bank
    Eclkdiv,
}

impl SiteKind {
    /// The part of a pin wire's name after `J<pin>_`, up to the instance
    /// suffix
    fn core(self) -> &'static str {
        match self {
            SiteKind::Iologic => "IOLOGIC_CORE_I_GEARING_PIC_TOP_",
            SiteKind::Dqsbuf => "DQSBUFA_CORE_I_DQS_TOP",
            SiteKind::Ddrdll => "DDRDLL_CORE_I1",
            SiteKind::Eclksync => "ECLKSYNC_CORE_ECLKSYNC",
            SiteKind::Eclkdiv => "ECLKDIV_CORE_ECLKDIVB",
        }
    }

    const ALL: [SiteKind; 5] = [
        SiteKind::Iologic,
        SiteKind::Dqsbuf,
        SiteKind::Ddrdll,
        SiteKind::Eclksync,
        SiteKind::Eclkdiv,
    ];
}

/// One primitive, as the device data describes it.
#[derive(Deserialize, Clone, Debug)]
pub struct Primitive {
    /// The kind of block it occupies
    pub site: SiteKind,
    /// The ports that connect inside the I/O, if any
    #[serde(default)]
    pub pad: Option<PadChain>,
    /// Port -> the block's pin, by port name (bus bits as `NAME<bit>`)
    pub pins: BTreeMap<String, Pin>,
    /// Settings made whenever the primitive is used, `NAME.OPTION`
    #[serde(default)]
    pub settings: Vec<String>,
    /// The primitive's parameters
    #[serde(default)]
    pub parameters: BTreeMap<String, Parameter>,
    /// Settings made when a pin bus (`DLLCODE`) is driven by the named block
    #[serde(default)]
    pub pins_from: BTreeMap<String, BTreeMap<String, Vec<String>>>,
}

/// The ports of a primitive that connect inside the I/O.
#[derive(Deserialize, Clone, Debug)]
pub struct PadChain {
    /// What the primitive does in the chain (`output`, `tristate`, `input`,
    /// `input_delay`, `output_delay`, `strobe`)
    pub role: String,
    /// Its ports in the chain
    pub ports: Vec<String>,
}

/// The block pin behind a port.
#[derive(Deserialize, Clone, Debug)]
pub struct Pin {
    /// The pin's name in its wire (`J<pin>_<core>`)
    pub pin: String,
    /// Settings the connection implies
    #[serde(default)]
    pub settings: Vec<String>,
    /// Whether the pin is on the dedicated clock network
    #[serde(default)]
    pub clock: bool,
    /// A fixed connection between blocks that is not routed (the block and
    /// pin it comes from)
    #[serde(default)]
    pub dedicated: Option<String>,
    /// The value the pin reads when nothing is routed to it: a port tied
    /// to that value is left unrouted
    #[serde(default)]
    pub unrouted: Option<u8>,
    /// How the pin is timed against the fabric, if it is
    #[serde(default)]
    pub timing: Option<PinTiming>,
    /// For a clock output: the clock it follows
    #[serde(default)]
    pub clock_from: Option<ClockFrom>,
}

/// How a block pin is timed against the fabric.
#[derive(Deserialize, Clone, Debug, Default)]
pub struct PinTiming {
    /// The primitive's port whose clock samples (an input) or launches (an
    /// output) the pin; `None` for a pin with only combinational arcs
    #[serde(default)]
    pub clock: Option<String>,
    /// An input's setup and hold, from the timing tables
    #[serde(default)]
    pub setup: Option<SetupRef>,
    /// An output's clock-to-output delay, from the timing tables
    #[serde(default)]
    pub launch: Option<ArcRef>,
    /// An output's combinational arcs: input port -> the arc
    #[serde(default)]
    pub from: BTreeMap<String, ArcRef>,
}

/// A setup and hold entry of the timing tables.
#[derive(Deserialize, Clone, Debug)]
pub struct SetupRef {
    /// The table's cell type (`IOLOGIC:ODDRXN`)
    pub cell: String,
    /// The pin that is checked
    pub pin: String,
    /// The clock it is checked against
    pub clock: String,
}

/// A delay arc of the timing tables.
#[derive(Deserialize, Clone, Debug)]
pub struct ArcRef {
    /// The table's cell type
    pub cell: String,
    /// Where the arc starts
    pub from: String,
    /// Where it ends
    pub to: String,
}

/// Where a block's clock output comes from.
#[derive(Deserialize, Clone, Debug)]
pub struct ClockFrom {
    /// The input port it follows
    pub port: String,
    /// The parameter whose value is the division ratio; `None`: not divided
    #[serde(default)]
    pub divide_by: Option<String>,
}

/// A primitive parameter.
#[derive(Deserialize, Clone, Debug)]
pub struct Parameter {
    /// Its value when the design does not give one
    #[serde(default)]
    pub default: Option<String>,
    /// The settings of each value
    #[serde(default)]
    pub values: Option<BTreeMap<String, Vec<String>>>,
    /// A word the value is written to, as a number
    #[serde(default)]
    pub word: Option<String>,
    /// The word's width
    #[serde(default)]
    pub bits: Option<u32>,
}

#[derive(Deserialize)]
struct PrimitivesFile {
    primitives: BTreeMap<String, Primitive>,
}

/// A family's DDR primitives, from the device data.
#[derive(Debug, Default, Clone)]
pub struct Primitives {
    by_name: BTreeMap<String, Primitive>,
    /// Every block pin a primitive routes, in router pin order
    pins: Vec<String>,
    /// The pins on the clock network
    clock_pins: HashSet<String>,
}

/// A blackbox port's name as the device data writes it: a bus bit
/// `NAME[3]` as `NAME3`.
pub fn port_key(port: &str) -> String {
    match port.split_once('[') {
        Some((name, rest)) => format!("{name}{}", rest.trim_end_matches(']')),
        None => port.to_string(),
    }
}

/// A cell's primitive name: its type without the blackbox prefix.
fn primitive_name(cell: &Cell) -> &str {
    cell.cell_type
        .strip_prefix("BLACKBOX_")
        .unwrap_or(&cell.cell_type)
}

impl Primitives {
    /// The family's primitives; none if the device data has no file.
    pub fn load(db_root: &Path, family: &str) -> Result<Self, String> {
        let path = db_root.join(family).join(FILE);
        let text = match std::fs::read_to_string(&path) {
            Ok(text) => text,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(Self::default()),
            Err(e) => return Err(format!("{}: {e}", path.display())),
        };
        Self::from_json(&text).map_err(|e| format!("{}: {e}", path.display()))
    }

    fn from_json(text: &str) -> Result<Self, String> {
        let file: PrimitivesFile = serde_json::from_str(text).map_err(|e| e.to_string())?;
        let mut pins = std::collections::BTreeSet::new();
        let mut clock_pins = HashSet::default();
        for prim in file.primitives.values() {
            for pin in prim.pins.values().filter(|p| p.dedicated.is_none()) {
                pins.insert(pin.pin.clone());
                if pin.clock {
                    clock_pins.insert(pin.pin.clone());
                }
            }
        }
        Ok(Self {
            by_name: file.primitives,
            pins: pins.into_iter().collect(),
            clock_pins,
        })
    }

    /// The primitive a cell is, if it is one.
    pub fn of(&self, cell: &Cell) -> Option<&Primitive> {
        self.by_name.get(primitive_name(cell))
    }

    /// The primitive named `name`.
    pub fn named(&self, name: &str) -> Option<&Primitive> {
        self.by_name.get(name)
    }

    /// Every routed block pin, in router pin order.
    pub fn pins(&self) -> &[String] {
        &self.pins
    }

    /// Router pin number of a block pin: [`ROUTER_PIN_BASE`] onward, or
    /// [`crate::device::HARD_CLOCK_PIN_BASE`] onward for a clock pin.
    pub fn router_pin(&self, pin: &str) -> Option<usize> {
        let index = self.pins.iter().position(|p| p == pin)?;
        Some(if self.clock_pins.contains(pin) {
            crate::device::HARD_CLOCK_PIN_BASE + index
        } else {
            ROUTER_PIN_BASE + index
        })
    }

    /// The block pin of a router pin number.
    pub fn pin_of_router_pin(&self, router_pin: usize) -> Option<&str> {
        let index = router_pin
            .checked_sub(crate::device::HARD_CLOCK_PIN_BASE)
            .filter(|&i| i < self.pins.len())
            .or_else(|| {
                router_pin
                    .checked_sub(ROUTER_PIN_BASE)
                    .filter(|&i| i < self.pins.len())
            })?;
        self.pins.get(index).map(String::as_str)
    }

    /// The device data's description of a cell's `index`-th input or
    /// output port: its name and its pin, `None` for a port in the pad chain.
    fn port<'a>(
        &'a self,
        cell: &'a Cell,
        output: bool,
        index: usize,
    ) -> Option<(String, Option<&'a Pin>)> {
        let prim = self.of(cell)?;
        let port = port_key(cell.blackbox_pin(output, index)?);
        let pin = prim.pins.get(&port);
        Some((port, pin))
    }

    /// The device data's pin behind a cell's `index`-th input or output
    pub fn pin_at<'a>(&'a self, cell: &'a Cell, output: bool, index: usize) -> Option<&'a Pin> {
        self.port(cell, output, index)?.1
    }

    /// The index of a primitive cell's input port named `port`
    pub fn input_index(cell: &Cell, port: &str) -> Option<usize> {
        let mut index = 0;
        while let Some(name) = cell.blackbox_pin(false, index) {
            if port_key(name) == port {
                return Some(index);
            }
            index += 1;
        }
        None
    }

    /// The input whose clock samples (an input) or launches (an output) a
    /// primitive cell's pin, for a pin timed against a clock
    pub fn timing_clock(&self, cell: &Cell, output: bool, index: usize) -> Option<usize> {
        let clock = self
            .pin_at(cell, output, index)?
            .timing
            .as_ref()?
            .clock
            .as_ref()?;
        Self::input_index(cell, clock)
    }

    /// The frequencies, in MHz, a primitive cell's clock outputs run at,
    /// given those known on its inputs' nets: a clock output follows the
    /// input the device data names, divided by the ratio a parameter gives.
    pub fn driven_frequencies(
        &self,
        cell: &Cell,
        known: &dyn Fn(GateNetId) -> Option<f64>,
    ) -> Vec<(usize, f64)> {
        let Some(prim) = self.of(cell) else {
            return Vec::new();
        };
        let mut driven = Vec::new();
        for output in 0..cell.outputs.len() {
            let Some(from) = self
                .pin_at(cell, true, output)
                .and_then(|p| p.clock_from.as_ref())
            else {
                continue;
            };
            let Some(mhz) = Self::input_index(cell, &from.port)
                .and_then(|input| cell.inputs.get(input))
                .and_then(|&net| known(net))
            else {
                continue;
            };
            let ratio = match &from.divide_by {
                None => Some(1.0),
                Some(param) => cell
                    .parameters
                    .get(param)
                    .or_else(|| prim.parameters.get(param)?.default.as_ref())
                    .and_then(|value| value.parse::<f64>().ok())
                    .filter(|&ratio| ratio > 0.0),
            };
            if let Some(ratio) = ratio {
                driven.push((output, mhz / ratio));
            }
        }
        driven
    }

    /// Router pin of a primitive cell's `index`-th input, if it is routed
    /// (not in the pad chain, not a dedicated connection).
    pub fn input_router_pin(&self, cell: &Cell, index: usize) -> Option<usize> {
        let (_, pin) = self.port(cell, false, index)?;
        let pin = pin.filter(|p| p.dedicated.is_none())?;
        self.router_pin(&pin.pin)
    }

    /// The value a primitive cell's `index`-th input reads when nothing is
    /// routed to it, if the device data gives one.
    pub fn input_unrouted_value(&self, cell: &Cell, index: usize) -> Option<bool> {
        let (_, pin) = self.port(cell, false, index)?;
        pin?.unrouted.map(|v| v != 0)
    }

    /// Router pin of a primitive cell's `index`-th output, if it is routed.
    pub fn output_router_pin(&self, cell: &Cell, index: usize) -> Option<usize> {
        let (_, pin) = self.port(cell, true, index)?;
        let pin = pin.filter(|p| p.dedicated.is_none())?;
        self.router_pin(&pin.pin)
    }

    /// What is wrong with a primitive cell's ports, for an error: ports the
    /// device data does not describe.
    pub fn port_problems(&self, cell: &Cell) -> Option<String> {
        let prim = self.of(cell)?;
        let chain: Vec<&str> = prim
            .pad
            .iter()
            .flat_map(|p| p.ports.iter().map(String::as_str))
            .collect();
        let mut unknown = Vec::new();
        for output in [false, true] {
            let mut index = 0;
            while let Some(port) = cell.blackbox_pin(output, index) {
                let key = port_key(port);
                if !prim.pins.contains_key(&key) && !chain.contains(&key.as_str()) {
                    unknown.push(port.to_string());
                }
                index += 1;
            }
        }
        (!unknown.is_empty()).then(|| {
            format!(
                "'{}' ({}) has ports the device data does not describe: {}",
                cell.path,
                primitive_name(cell),
                unknown.join(", ")
            )
        })
    }

    /// Whether a cell's `index`-th input or output is in its pad chain.
    fn in_chain(&self, cell: &Cell, output: bool, index: usize) -> bool {
        let Some(prim) = self.of(cell) else {
            return false;
        };
        let Some(port) = cell.blackbox_pin(output, index) else {
            return false;
        };
        prim.pad
            .as_ref()
            .is_some_and(|p| p.ports.iter().any(|q| *q == port_key(port)))
    }
}

/// One DDR block of the device.
#[derive(Debug, Clone)]
pub struct Site {
    /// Its kind
    pub kind: SiteKind,
    /// Its name, unique within its tile: `IOLOGIC_A`, `DQSBUF`,
    /// `DDRDLL_CORE_R72C158`, `ECLKSYNC3_0`, `ECLKDIVB3_0`
    pub name: String,
    /// The tile its BEL is in
    pub x: u32,
    pub y: u32,
    /// Its bank, for the edge clock blocks
    pub bank: Option<u32>,
    /// Its pins: the tile that names each pin's wire, and the name there
    pub pins: SitePins,
    /// The tiles that hold its settings (prjoxide tile names)
    pub settings_tiles: Vec<String>,
    /// The prefix of its settings' names: `IOLOGICA`, `DQSBUF`, `DDRDLL`,
    /// `ECLKSYNC3A`, `ECLKDIV3A`
    pub prefix: String,
}

/// The quoted strings of a tile type's text.
fn quoted(text: &str) -> impl Iterator<Item = &str> {
    text.split('"').skip(1).step_by(2)
}

/// A pin wire of `kind` that a tile type names: (the name as written, the
/// pin, the instance suffix after the core).
fn pin_wire(kind: SiteKind, name: &str) -> Option<(&str, &str)> {
    let base = name.rsplit_once(':').map_or(name, |(_, b)| b);
    let rest = base.strip_prefix('J')?;
    let marker = format!("_{}", kind.core());
    let at = rest.find(&marker)?;
    Some((&rest[..at], &rest[at + marker.len()..]))
}

/// The settings prefix letter of an edge clock block's instance number.
fn letter(index: u32) -> Option<char> {
    char::from_u32(u32::from(b'A') + index).filter(|c| c.is_ascii_uppercase())
}

/// Every DDR block of a device whose tiles are `tiles`.
pub fn sites(db_root: &Path, family: &str, tiles: &[PrjoxideTile]) -> Vec<Site> {
    let mut texts: HashMap<&str, Option<String>> = HashMap::new();
    for tile in tiles {
        texts.entry(&tile.tiletype).or_insert_with(|| {
            let path = db_root
                .join(family)
                .join("tiletypes")
                .join(format!("{}.ron", tile.tiletype));
            std::fs::read_to_string(path).ok()
        });
    }
    let text_of =
        |tile: &PrjoxideTile| texts.get(tile.tiletype.as_str()).and_then(|t| t.as_deref());
    let has_settings = |tile: &PrjoxideTile, prefix: &str| {
        let needle = format!("\"{prefix}.");
        text_of(tile).is_some_and(|t| t.contains(&needle))
    };
    let at = |x: i64, y: u32| -> Vec<&PrjoxideTile> {
        tiles
            .iter()
            .filter(|t| i64::from(t.x) == x && t.y == y)
            .collect()
    };

    let mut sites = Vec::new();
    // Edge clock blocks, by (kind, bank, index): their pins, wherever named
    // DQS buffer outputs every pad of the group reaches (`DQSG:` wires,
    // one per group): the I/O tiles name them, not the buffer's tile
    let mut group_pins: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    let mut edge: BTreeMap<(SiteKind, u32, u32), SitePins> = BTreeMap::new();
    for tile in tiles {
        let Some(text) = text_of(tile) else {
            continue;
        };
        let mut iologic: [BTreeMap<String, (u32, u32, String)>; 2] = Default::default();
        let mut dqsbuf: BTreeMap<String, (u32, u32, String)> = BTreeMap::new();
        let mut ddrdll: BTreeMap<String, (u32, u32, String)> = BTreeMap::new();
        for name in quoted(text) {
            for kind in SiteKind::ALL {
                let Some((pin, suffix)) = pin_wire(kind, name) else {
                    continue;
                };
                let wire = (tile.x, tile.y, name.to_string());
                match kind {
                    SiteKind::Iologic if !name.contains(':') => {
                        if let Some(side) = ["A", "B"].iter().position(|s| *s == suffix) {
                            iologic[side].insert(pin.to_string(), wire);
                        }
                    }
                    SiteKind::Dqsbuf if suffix.is_empty() && name.starts_with("DQSG:") => {
                        group_pins.insert(pin.to_string());
                    }
                    SiteKind::Dqsbuf if suffix.is_empty() => {
                        dqsbuf.entry(pin.to_string()).or_insert(wire);
                    }
                    SiteKind::Ddrdll if suffix.is_empty() => {
                        ddrdll.entry(pin.to_string()).or_insert(wire);
                    }
                    SiteKind::Eclksync | SiteKind::Eclkdiv => {
                        let Some((bank, index)) = suffix
                            .split_once('_')
                            .and_then(|(b, i)| Some((b.parse().ok()?, i.parse().ok()?)))
                        else {
                            continue;
                        };
                        edge.entry((kind, bank, index))
                            .or_default()
                            .entry(pin.to_string())
                            .or_insert(wire);
                    }
                    _ => {}
                }
            }
        }
        for (side, pins) in iologic.into_iter().enumerate() {
            if pins.is_empty() {
                continue;
            }
            let prefix = format!("IOLOGIC{}", ["A", "B"][side]);
            // The B side's settings sit in the next tile along the edge.
            let settings_tiles = [i64::from(tile.x), i64::from(tile.x) + 1]
                .iter()
                .flat_map(|&x| at(x, tile.y))
                .filter(|t| has_settings(t, &prefix))
                .map(|t| t.name.clone())
                .collect();
            sites.push(Site {
                kind: SiteKind::Iologic,
                name: format!("IOLOGIC_{}", ["A", "B"][side]),
                x: tile.x,
                y: tile.y,
                bank: super::sites::bank_of_io_tile(&tile.tiletype),
                pins,
                settings_tiles,
                prefix,
            });
        }
        // A DQS buffer is named by the tile below its group's strobe; its
        // settings spread over that tile and the next four (DQS0 to DQS4).
        if dqsbuf.keys().any(|p| p == "DQSI") {
            let settings_tiles = (i64::from(tile.x)..=i64::from(tile.x) + 4)
                .flat_map(|x| at(x, tile.y))
                .filter(|t| has_settings(t, "DQSBUF"))
                .map(|t| t.name.clone())
                .collect();
            sites.push(Site {
                kind: SiteKind::Dqsbuf,
                name: "DQSBUF".to_string(),
                x: tile.x,
                y: tile.y,
                bank: super::sites::bank_of_io_tile(&tile.tiletype),
                pins: dqsbuf,
                settings_tiles,
                prefix: "DQSBUF".to_string(),
            });
        }
        if ddrdll.keys().any(|p| p == "CLKIN") && has_settings(tile, "DDRDLL") {
            let home =
                ddrdll
                    .values()
                    .find_map(|(x, y, name)| {
                        match super::prjoxide_graph::resolve_wire(*x, *y, name)? {
                            super::NodeKey::Local { x, y, .. } => Some((x, y)),
                            _ => None,
                        }
                    });
            let (hx, hy) = home.unwrap_or((tile.x, tile.y));
            sites.push(Site {
                kind: SiteKind::Ddrdll,
                name: format!("DDRDLL_CORE_R{hy}C{hx}"),
                x: tile.x,
                y: tile.y,
                bank: None,
                pins: ddrdll,
                settings_tiles: vec![tile.name.clone()],
                prefix: "DDRDLL".to_string(),
            });
        }
    }
    // Each DQS buffer drives its group's wires, which resolve per group from
    // any tile of it
    for site in sites.iter_mut().filter(|s| s.kind == SiteKind::Dqsbuf) {
        for pin in &group_pins {
            site.pins.entry(pin.clone()).or_insert_with(|| {
                (
                    site.x,
                    site.y,
                    format!("DQSG:J{pin}_{}", SiteKind::Dqsbuf.core()),
                )
            });
        }
    }
    // Edge clock blocks sit in the tile with their settings.
    for ((kind, bank, index), pins) in edge {
        let Some(letter) = letter(index) else {
            continue;
        };
        let (prefix, name) = match kind {
            SiteKind::Eclksync => (
                format!("ECLKSYNC{bank}{letter}"),
                format!("ECLKSYNC{bank}_{index}"),
            ),
            _ => (
                format!("ECLKDIV{bank}{letter}"),
                format!("ECLKDIVB{bank}_{index}"),
            ),
        };
        let settings: Vec<&PrjoxideTile> =
            tiles.iter().filter(|t| has_settings(t, &prefix)).collect();
        // A block with no settings goes where its pins are named.
        let anchor = settings
            .first()
            .map(|t| (t.x, t.y))
            .or_else(|| pins.values().next().map(|&(x, y, _)| (x, y)));
        let settings_tiles = settings.iter().map(|t| t.name.clone()).collect();
        let Some((x, y)) = anchor else {
            continue;
        };
        sites.push(Site {
            kind,
            name,
            x,
            y,
            bank: Some(bank),
            pins,
            settings_tiles,
            prefix,
        });
    }
    sites.sort_by(|a, b| (a.kind, a.y, a.x, &a.name).cmp(&(b.kind, b.y, b.x, &b.name)));
    sites
}

/// The DDR blocks of a design: which cells share a pad, and the nets that
/// connect inside the I/O.
#[derive(Debug, Default)]
pub struct DesignBlocks {
    /// The pad each I/O logic or DQS buffer cell is at
    pub pad_of: HashMap<CellId, CellId>,
    /// Nets that connect inside an I/O, never through the fabric
    pub internal_nets: HashSet<GateNetId>,
    /// Every primitive cell
    pub cells: Vec<CellId>,
}

/// A buffer cell: one input copied to one output.
fn is_buffer(cell: &Cell) -> bool {
    matches!(
        cell.function,
        Some(CellFunction::Buf) | Some(CellFunction::ClkBuf)
    ) && cell.inputs.len() == 1
        && cell.outputs.len() == 1
        && cell.clock.is_none()
}

/// Leave out the buffers between a primitive's pad-side output and its pad:
/// they would be logic in the fabric, which the connection never enters.
pub fn bypass_pad_buffers(netlist: &GateNetlist, prims: &Primitives) -> Option<GateNetlist> {
    let mut out = netlist.clone();
    let mut changed = false;
    loop {
        // Inputs reading each net
        let mut readers: HashMap<GateNetId, usize> = HashMap::new();
        for cell in &out.cells {
            for &net in &cell.inputs {
                *readers.entry(net).or_default() += 1;
            }
        }
        // A pad-chain output whose only reader is a buffer drives the
        // buffer's output instead; the buffer goes
        let mut moved: Option<(usize, usize, usize)> = None;
        'search: for (index, cell) in out.cells.iter().enumerate() {
            for (output, &net) in cell.outputs.iter().enumerate() {
                if !prims.in_chain(cell, true, output) || readers.get(&net) != Some(&1) {
                    continue;
                }
                if let Some(buffer) = out
                    .cells
                    .iter()
                    .position(|c| is_buffer(c) && c.inputs[0] == net)
                {
                    moved = Some((index, output, buffer));
                    break 'search;
                }
            }
        }
        let Some((index, output, buffer)) = moved else {
            break;
        };
        out.cells[index].outputs[output] = out.cells[buffer].outputs[0];
        out.cells.remove(buffer);
        out.rebuild_net_connectivity();
        changed = true;
    }
    changed.then_some(out)
}

/// Find the DDR blocks of a design and check how they connect: each chain
/// of primitives at one pad reaches exactly that pad, inside the I/O.
pub fn analyze(
    netlist: &GateNetlist,
    prims: &Primitives,
    is_pad: &dyn Fn(&Cell) -> bool,
) -> Result<DesignBlocks, String> {
    let mut blocks = DesignBlocks::default();
    if prims.by_name.is_empty() {
        return Ok(blocks);
    }
    // Inputs of every net, as (cell, input index)
    let mut sinks: HashMap<GateNetId, Vec<(CellId, usize)>> = HashMap::new();
    for cell in &netlist.cells {
        for (index, &net) in cell.inputs.iter().enumerate() {
            sinks.entry(net).or_default().push((cell.id, index));
        }
    }
    let mut parent: HashMap<CellId, CellId> = HashMap::new();
    fn find(parent: &mut HashMap<CellId, CellId>, c: CellId) -> CellId {
        let p = *parent.get(&c).unwrap_or(&c);
        if p == c {
            return c;
        }
        let root = find(parent, p);
        parent.insert(c, root);
        root
    }
    let union = |parent: &mut HashMap<CellId, CellId>, a: CellId, b: CellId| {
        let (ra, rb) = (find(parent, a), find(parent, b));
        if ra != rb {
            parent.insert(ra, rb);
        }
    };
    let cell = |id: CellId| netlist.get_cell(id);
    let name = |id: CellId| cell(id).map_or("?", |c| c.path.as_str()).to_string();
    for c in netlist.cells.iter().filter(|c| prims.of(c).is_some()) {
        blocks.cells.push(c.id);
        if let Some(problem) = prims.port_problems(c) {
            return Err(problem);
        }
        for (index, &net) in c.inputs.iter().enumerate() {
            if !prims.in_chain(c, false, index) {
                continue;
            }
            blocks.internal_nets.insert(net);
            let driver = netlist.nets.get(net.0 as usize).and_then(|n| n.driver);
            let Some(driver) = driver else {
                return Err(format!(
                    "'{}' input {} must come from a pad or from another DDR primitive at the \
                     pad, but nothing drives it",
                    c.path,
                    c.blackbox_pin(false, index).unwrap_or("?")
                ));
            };
            let d = cell(driver).ok_or("driver out of range")?;
            let from_chain = d
                .outputs
                .iter()
                .position(|&n| n == net)
                .is_some_and(|i| prims.in_chain(d, true, i));
            if !is_pad(d) && !from_chain {
                return Err(format!(
                    "'{}' input {} connects inside the I/O, so it must come from the pad or \
                     from another DDR primitive at the pad, not from '{}' in the fabric",
                    c.path,
                    c.blackbox_pin(false, index).unwrap_or("?"),
                    d.path
                ));
            }
            union(&mut parent, c.id, driver);
        }
        for (index, &net) in c.outputs.iter().enumerate() {
            if !prims.in_chain(c, true, index) {
                continue;
            }
            blocks.internal_nets.insert(net);
            for &(sink, input) in sinks.get(&net).map(Vec::as_slice).unwrap_or(&[]) {
                let s = cell(sink).ok_or("sink out of range")?;
                if !is_pad(s) && !prims.in_chain(s, false, input) {
                    return Err(format!(
                        "'{}' output {} connects inside the I/O, so it can only go to the pad \
                         or to another DDR primitive at the pad, not to '{}' in the fabric",
                        c.path,
                        c.blackbox_pin(true, index).unwrap_or("?"),
                        s.path
                    ));
                }
                union(&mut parent, c.id, sink);
            }
        }
    }
    // A pad that a primitive reads cannot also feed the fabric directly.
    for &net in &blocks.internal_nets {
        let driver = netlist.nets.get(net.0 as usize).and_then(|n| n.driver);
        if !driver.and_then(cell).is_some_and(is_pad) {
            continue;
        }
        for &(sink, input) in sinks.get(&net).map(Vec::as_slice).unwrap_or(&[]) {
            let s = cell(sink).ok_or("sink out of range")?;
            if !prims.in_chain(s, false, input) {
                return Err(format!(
                    "The pad '{}' is read by a DDR primitive, so it cannot also go to '{}' in \
                     the fabric; take that from the primitive's output",
                    name(driver.unwrap_or(CellId(0))),
                    s.path
                ));
            }
        }
    }
    // Each chain reaches one pad.
    let mut pads: HashMap<CellId, Vec<CellId>> = HashMap::new();
    for pad in netlist.cells.iter().filter(|c| is_pad(c)) {
        let root = find(&mut parent, pad.id);
        pads.entry(root).or_default().push(pad.id);
    }
    for &id in &blocks.cells {
        let c = cell(id).ok_or("cell out of range")?;
        let prim = prims.of(c).ok_or("not a primitive")?;
        if prim.pad.is_none() {
            continue;
        }
        let root = find(&mut parent, id);
        match pads.get(&root).map(Vec::as_slice) {
            Some([pad]) => {
                blocks.pad_of.insert(id, *pad);
            }
            Some(many) => {
                let names: Vec<String> = many.iter().map(|&p| name(p)).collect();
                return Err(format!(
                    "'{}' connects inside the I/O to more than one pad: {}",
                    c.path,
                    names.join(", ")
                ));
            }
            None => {
                return Err(format!(
                    "'{}' connects inside the I/O, but its chain reaches no pad",
                    c.path
                ));
            }
        }
    }
    blocks.cells.sort_unstable_by_key(|c| c.0);
    Ok(blocks)
}

/// Where each DDR primitive goes: an index into the device's [`Site`]s.
///
/// - The primitives at a pad go to its I/O logic.
/// - A DQS buffer goes to the DQS group of its strobe pad.
/// - An edge clock synchronizer goes to the bank of the I/O it clocks (the
///   first free one), a divider to the synchronizer that drives it.
/// - A DLL goes to the one nearest the DQS buffers it feeds.
///
/// `pad_site` gives a pad cell's site, (tile x, y, A = 0 / B = 1);
/// `strobe_of` the strobe pad tile of a pad tile's DQS group; `accepts`
/// whether a synchronizer or DLL may take a site (one its clock reaches,
/// say): those that have a choice take the first it accepts.
pub fn choose_sites(
    netlist: &GateNetlist,
    prims: &Primitives,
    blocks: &DesignBlocks,
    sites: &[Site],
    pad_site: PadSiteOf<'_>,
    strobe_of: StrobeOf<'_>,
    accepts: Accepts<'_>,
) -> Result<HashMap<CellId, usize>, String> {
    let mut chosen: HashMap<CellId, usize> = HashMap::new();
    let cell = |id: CellId| {
        netlist
            .get_cell(id)
            .ok_or_else(|| "cell out of range".to_string())
    };
    let find = |kind: SiteKind, pred: &dyn Fn(&Site) -> bool| {
        sites.iter().position(|s| s.kind == kind && pred(s))
    };
    // Pads and DQS groups first
    for &id in &blocks.cells {
        let c = cell(id)?;
        let prim = prims.of(c).ok_or("not a primitive")?;
        let Some(&pad) = blocks.pad_of.get(&id) else {
            continue;
        };
        let pad_cell = cell(pad)?;
        let (x, y, side) = pad_site(pad_cell).ok_or_else(|| {
            format!(
                "'{}' is at pad '{}', which has no pin assignment",
                c.path, pad_cell.path
            )
        })?;
        let site = match prim.site {
            SiteKind::Iologic => {
                let name = format!("IOLOGIC_{}", ["A", "B"][side as usize % 2]);
                find(SiteKind::Iologic, &|s| {
                    s.x == x && s.y == y && s.name == name
                })
                .ok_or_else(|| {
                    format!(
                        "'{}' needs I/O logic at pad '{}', which has none in the device \
                             data",
                        c.path, pad_cell.path
                    )
                })?
            }
            SiteKind::Dqsbuf => {
                let strobe = strobe_of(x, y).filter(|&(sx, sy)| (sx, sy) == (x, y) && side == 0);
                let (sx, sy) = strobe.ok_or_else(|| {
                    format!(
                        "'{}' reads its strobe from pad '{}', which is not the strobe (DQS) \
                         pad of a DQS group",
                        c.path, pad_cell.path
                    )
                })?;
                sites
                    .iter()
                    .enumerate()
                    .filter(|(_, s)| s.kind == SiteKind::Dqsbuf && s.y == sy)
                    .min_by_key(|(_, s)| s.x.abs_diff(sx))
                    .map(|(i, _)| i)
                    .filter(|&i| sites[i].x.abs_diff(sx) <= 2)
                    .ok_or_else(|| {
                        format!(
                            "'{}': the DQS group of pad '{}' has no DQS buffer in the device \
                             data",
                            c.path, pad_cell.path
                        )
                    })?
            }
            other => {
                return Err(format!(
                    "'{}' connects to a pad, but a {other:?} block has no pad",
                    c.path
                ))
            }
        };
        chosen.insert(id, site);
    }
    // Two primitives of one kind cannot share a block.
    let mut seen: HashMap<(usize, &str), CellId> = HashMap::new();
    for &id in &blocks.cells {
        let Some(&site) = chosen.get(&id) else {
            continue;
        };
        let c = cell(id)?;
        let role = prims
            .of(c)
            .and_then(|p| p.pad.as_ref())
            .map_or("block", |p| p.role.as_str());
        if let Some(&other) = seen.get(&(site, role)) {
            return Err(format!(
                "'{}' and '{}' both need the {} of pad site {} ({}, {})",
                cell(other)?.path,
                c.path,
                role,
                sites[site].name,
                sites[site].x,
                sites[site].y
            ));
        }
        seen.insert((site, role), id);
    }

    // The cells each net reaches
    let mut sinks: HashMap<GateNetId, Vec<CellId>> = HashMap::new();
    for c in &netlist.cells {
        for &net in &c.inputs {
            sinks.entry(net).or_default().push(c.id);
        }
    }
    let bank_of =
        |id: CellId, chosen: &HashMap<CellId, usize>| chosen.get(&id).and_then(|&s| sites[s].bank);
    let mut used: HashSet<usize> = chosen.values().copied().collect();
    // Edge clock synchronizers: the bank of what they clock
    for &id in &blocks.cells {
        let c = cell(id)?;
        if prims.of(c).map(|p| p.site) != Some(SiteKind::Eclksync) {
            continue;
        }
        let mut banks: Vec<u32> = c
            .outputs
            .iter()
            .flat_map(|net| sinks.get(net).into_iter().flatten())
            .filter_map(|&s| bank_of(s, &chosen))
            .collect();
        banks.sort_unstable();
        banks.dedup();
        let bank = match banks.as_slice() {
            [bank] => *bank,
            [] => {
                return Err(format!(
                    "'{}' clocks no I/O logic or DQS buffer, so its bank is unknown",
                    c.path
                ))
            }
            many => {
                return Err(format!(
                    "'{}' clocks I/O in banks {many:?}; an edge clock reaches one bank, so use \
                     one synchronizer per bank",
                    c.path
                ))
            }
        };
        let free: Vec<usize> = (0..sites.len())
            .filter(|&i| {
                sites[i].kind == SiteKind::Eclksync
                    && sites[i].bank == Some(bank)
                    && !used.contains(&i)
                    && sites[i].pins.contains_key("ECLKIN")
            })
            .collect();
        if free.is_empty() {
            return Err(format!(
                "'{}': bank {bank} has no free edge clock synchronizer with its input in the \
                 device data",
                c.path
            ));
        }
        let site = free
            .iter()
            .copied()
            .find(|&i| accepts(id, i))
            .ok_or_else(|| {
                let names: Vec<&str> = free.iter().map(|&i| sites[i].name.as_str()).collect();
                format!(
                    "'{}': none of bank {bank}'s free edge clock synchronizers ({}) can take \
                     its input clock from where that clock comes",
                    c.path,
                    names.join(", ")
                )
            })?;
        used.insert(site);
        chosen.insert(id, site);
    }
    // Dividers: beside the synchronizer that drives them
    for &id in &blocks.cells {
        let c = cell(id)?;
        if prims.of(c).map(|p| p.site) != Some(SiteKind::Eclkdiv) {
            continue;
        }
        let driver = c
            .inputs
            .iter()
            .enumerate()
            .find(|&(i, _)| c.blackbox_pin(false, i) == Some("ECLKIN"))
            .and_then(|(_, net)| netlist.nets.get(net.0 as usize)?.driver);
        let sync = driver
            .and_then(|d| chosen.get(&d).copied())
            .filter(|&s| sites[s].kind == SiteKind::Eclksync)
            .ok_or_else(|| {
                format!(
                    "'{}' must be driven by an edge clock synchronizer (ECLKSYNC)",
                    c.path
                )
            })?;
        let index = sites[sync].name.rsplit('_').next().unwrap_or("0");
        let name = format!("ECLKDIVB{}_{index}", sites[sync].bank.unwrap_or(0));
        let site = find(SiteKind::Eclkdiv, &|s| s.name == name)
            .filter(|i| !used.contains(i))
            .ok_or_else(|| format!("'{}': no free edge clock divider {name}", c.path))?;
        used.insert(site);
        chosen.insert(id, site);
    }
    // DLLs: nearest to the DQS buffers they feed
    for &id in &blocks.cells {
        let c = cell(id)?;
        if prims.of(c).map(|p| p.site) != Some(SiteKind::Ddrdll) {
            continue;
        }
        let fed: Vec<usize> = c
            .outputs
            .iter()
            .flat_map(|net| sinks.get(net).into_iter().flatten())
            .filter_map(|s| chosen.get(s).copied())
            .collect();
        let site = sites
            .iter()
            .enumerate()
            .filter(|(i, s)| s.kind == SiteKind::Ddrdll && !used.contains(i) && accepts(id, *i))
            .min_by_key(|(_, s)| {
                fed.iter()
                    .map(|&f| {
                        u64::from(sites[f].x.abs_diff(s.x)) + u64::from(sites[f].y.abs_diff(s.y))
                    })
                    .sum::<u64>()
            })
            .map(|(i, _)| i)
            .ok_or_else(|| {
                format!(
                    "'{}': no free DDR DLL in the device data that its clock reaches",
                    c.path
                )
            })?;
        used.insert(site);
        chosen.insert(id, site);
    }
    for &id in &blocks.cells {
        if !chosen.contains_key(&id) {
            return Err(format!("'{}' has no place to go", cell(id)?.path));
        }
    }
    Ok(chosen)
}

/// The settings a placed primitive cell makes, as (name, option) enums and
/// (name, value) words, names without the block's prefix. `driver_site`
/// gives the name of the block driving a cell input, for `pins_from`.
pub fn cell_settings(
    prims: &Primitives,
    cell: &Cell,
    driver_site: &dyn Fn(usize) -> Option<String>,
) -> Result<CellSettings, String> {
    let prim = prims
        .of(cell)
        .ok_or_else(|| format!("'{}' is not a DDR primitive", cell.path))?;
    let mut enums: Vec<String> = prim.settings.clone();
    let mut words = Vec::new();
    // Each connected pin's settings
    let mut index = 0;
    while let Some(port) = cell.blackbox_pin(false, index) {
        if let Some(pin) = prim.pins.get(&port_key(port)) {
            enums.extend(pin.settings.iter().cloned());
        }
        index += 1;
    }
    index = 0;
    while let Some(port) = cell.blackbox_pin(true, index) {
        if let Some(pin) = prim.pins.get(&port_key(port)) {
            enums.extend(pin.settings.iter().cloned());
        }
        index += 1;
    }
    // Pin buses driven by a particular block
    for (bus, by_block) in &prim.pins_from {
        let mut drivers: Vec<Option<String>> = Vec::new();
        let mut index = 0;
        while let Some(port) = cell.blackbox_pin(false, index) {
            if port_key(port)
                .strip_prefix(bus.as_str())
                .is_some_and(|bit| bit.chars().all(|c| c.is_ascii_digit()))
            {
                drivers.push(driver_site(index));
            }
            index += 1;
        }
        if let Some(Some(block)) = drivers.first() {
            if drivers.iter().all(|d| d.as_ref() == Some(block)) {
                if let Some(settings) = by_block.get(block) {
                    enums.extend(settings.iter().cloned());
                }
            }
        }
    }
    // Parameters
    for (name, param) in &prim.parameters {
        let value = cell
            .parameters
            .get(name)
            .or(param.default.as_ref())
            .map(String::as_str);
        let Some(value) = value else {
            continue;
        };
        if let Some(values) = &param.values {
            let settings = values.get(value).ok_or_else(|| {
                format!(
                    "'{}': {name} = \"{value}\" is not supported (the device data has {})",
                    cell.path,
                    values.keys().cloned().collect::<Vec<_>>().join(", ")
                )
            })?;
            enums.extend(settings.iter().cloned());
        }
        if let Some(word) = &param.word {
            let number: u64 = value
                .parse()
                .map_err(|_| format!("'{}': {name} = \"{value}\" is not a number", cell.path))?;
            let bits = param.bits.unwrap_or(64);
            if bits < 64 && number >> bits != 0 {
                return Err(format!(
                    "'{}': {name} = {number} does not fit its {bits} bits",
                    cell.path
                ));
            }
            words.push((word.clone(), number));
        }
    }
    let mut out = Vec::new();
    for setting in enums {
        let (name, option) = setting
            .rsplit_once('.')
            .ok_or_else(|| format!("device data setting '{setting}' has no option"))?;
        out.push((name.to_string(), option.to_string()));
    }
    out.sort();
    out.dedup();
    Ok((out, words))
}

/// A site's pins: pin name -> (tile column, tile row, wire name).
pub type SitePins = BTreeMap<String, (u32, u32, String)>;

/// The pad site (column, row, PIO index) of a pad cell.
pub type PadSiteOf<'a> = &'a dyn Fn(&Cell) -> Option<(u32, u32, u32)>;

/// The strobe pad (column, row) of the DQS group of a pad at (column, row).
pub type StrobeOf<'a> = &'a dyn Fn(u32, u32) -> Option<(u32, u32)>;

/// Whether a block (cell) may take a site (an index into the device's
/// [`Site`]s), for [`choose_sites`].
pub type Accepts<'a> = &'a dyn Fn(CellId, usize) -> bool;

/// A placed primitive's settings: (name, option) enums and (name, value) words.
pub type CellSettings = (Vec<(String, String)>, Vec<(String, u64)>);

#[cfg(test)]
mod tests {
    use super::*;

    const DATA: &str = r#"{"primitives": {
        "ODDRX4": {"site": "IOLOGIC", "pad": {"role": "output", "ports": ["Q"]},
            "pins": {"D0": {"pin": "TXDATA0"}, "ECLK": {"pin": "ECLK", "clock": true},
                     "SCLK": {"pin": "SCLKOUT", "clock": true, "settings": ["SCLKOUTMUX.SCLKOUT"]}},
            "settings": ["ODDRXN.DDRMODE.ODDRX4"],
            "parameters": {"GSR": {"default": "ENABLED", "values": {"ENABLED": ["GSR.ENABLED"],
                "DISABLED": ["GSR.DISABLED"]}}}},
        "DELAYB": {"site": "IOLOGIC", "pad": {"role": "output_delay", "ports": ["A", "Z"]},
            "pins": {}, "settings": ["OUTMUX.DELAY"],
            "parameters": {"DEL_VALUE": {"default": "0", "word": "DELAY.DEL_VALUE", "bits": 7}}},
        "ECLKSYNC": {"site": "ECLKSYNC",
            "pins": {"ECLKIN": {"pin": "ECLKIN", "clock": true}, "STOP": {"pin": "STOP", "unrouted": 0},
                     "ECLKOUT": {"pin": "ECLKOUT", "clock": true}}},
        "DQSBUF": {"site": "DQSBUF", "pad": {"role": "strobe", "ports": ["DQSI"]},
            "pins": {"DLLCODE0": {"pin": "DLLCODE0", "dedicated": "DDRDLL.CODE"},
                     "RST": {"pin": "RST"}},
            "pins_from": {"DLLCODE": {"DDRDLL_CORE_R72C158": ["DLLCODE.DDRDLL"]}}}
    }}"#;

    fn blackbox(kind: &str, inputs: &[&str], outputs: &[&str], params: &[(&str, &str)]) -> Cell {
        let mut cell = Cell::new_comb(
            CellId(0),
            format!("BLACKBOX_{kind}"),
            "test".to_string(),
            0.0,
            format!("top.{kind}"),
            Vec::new(),
            Vec::new(),
        );
        cell.parameters.insert(
            skalp_lir::gate_netlist::BLACKBOX_INPUTS_PARAM.to_string(),
            inputs.join(","),
        );
        cell.parameters.insert(
            skalp_lir::gate_netlist::BLACKBOX_OUTPUTS_PARAM.to_string(),
            outputs.join(","),
        );
        for (k, v) in params {
            cell.parameters.insert(k.to_string(), v.to_string());
        }
        cell
    }

    #[test]
    fn ports_map_to_router_pins_from_the_device_data() {
        let prims = Primitives::from_json(DATA).unwrap();
        let oddr = blackbox("ODDRX4", &["D0", "ECLK", "SCLK"], &["Q"], &[]);
        let d0 = prims.input_router_pin(&oddr, 0).unwrap();
        let eclk = prims.input_router_pin(&oddr, 1).unwrap();
        assert!(!crate::device::is_clock_pin(d0));
        assert!(crate::device::is_clock_pin(eclk));
        assert_eq!(prims.pin_of_router_pin(d0), Some("TXDATA0"));
        assert_eq!(prims.pin_of_router_pin(eclk), Some("ECLK"));
        // The pad-side output and a dedicated input are not routed
        assert_eq!(prims.output_router_pin(&oddr, 0), None);
        let dqs = blackbox("DQSBUF", &["DQSI", "RST", "DLLCODE[0]"], &[], &[]);
        assert_eq!(prims.input_router_pin(&dqs, 0), None);
        assert!(prims.input_router_pin(&dqs, 1).is_some());
        assert_eq!(prims.input_router_pin(&dqs, 2), None);
        assert!(prims.port_problems(&dqs).is_none());
        let odd = blackbox("ODDRX4", &["D0", "NOPE"], &["Q"], &[]);
        assert!(prims.port_problems(&odd).unwrap().contains("NOPE"));
    }

    #[test]
    fn unrouted_values_come_from_the_device_data() {
        let prims = Primitives::from_json(DATA).unwrap();
        let sync = blackbox("ECLKSYNC", &["ECLKIN", "STOP"], &["ECLKOUT"], &[]);
        assert_eq!(prims.input_unrouted_value(&sync, 0), None);
        assert_eq!(prims.input_unrouted_value(&sync, 1), Some(false));
        // A port the data does not describe has no value
        assert_eq!(prims.input_unrouted_value(&sync, 2), None);
    }

    #[test]
    fn settings_come_from_the_device_data() {
        let prims = Primitives::from_json(DATA).unwrap();
        let oddr = blackbox(
            "ODDRX4",
            &["D0", "ECLK", "SCLK"],
            &["Q"],
            &[("GSR", "DISABLED")],
        );
        let (enums, words) = cell_settings(&prims, &oddr, &|_| None).unwrap();
        let has = |n: &str, o: &str| enums.contains(&(n.to_string(), o.to_string()));
        assert!(has("ODDRXN.DDRMODE", "ODDRX4"));
        assert!(has("SCLKOUTMUX", "SCLKOUT"));
        assert!(has("GSR", "DISABLED"));
        assert!(words.is_empty());

        let delay = blackbox("DELAYB", &["A"], &["Z"], &[("DEL_VALUE", "8")]);
        let (_, words) = cell_settings(&prims, &delay, &|_| None).unwrap();
        assert_eq!(words, [("DELAY.DEL_VALUE".to_string(), 8)]);
        let too_big = blackbox("DELAYB", &["A"], &["Z"], &[("DEL_VALUE", "200")]);
        assert!(cell_settings(&prims, &too_big, &|_| None)
            .unwrap_err()
            .contains("7 bits"));

        // The DLL code bus from the right-side DLL sets its bit
        let dqs = blackbox("DQSBUF", &["DQSI", "DLLCODE[0]", "DLLCODE[1]"], &[], &[]);
        let right = |i: usize| (i > 0).then(|| "DDRDLL_CORE_R72C158".to_string());
        let (enums, _) = cell_settings(&prims, &dqs, &right).unwrap();
        assert!(enums.contains(&("DLLCODE".to_string(), "DDRDLL".to_string())));
        let left = |i: usize| (i > 0).then(|| "DDRDLL_CORE_R68C1".to_string());
        let (enums, _) = cell_settings(&prims, &dqs, &left).unwrap();
        assert!(!enums.iter().any(|(n, _)| n == "DLLCODE"));
    }

    const TIMED: &str = r#"{"primitives": {
        "ECLKDIV": {"site": "ECLKDIV",
            "pins": {"ECLKIN": {"pin": "ECLKIN", "clock": true},
                     "DIVOUT": {"pin": "DIVOUT", "clock": true,
                                "clock_from": {"port": "ECLKIN", "divide_by": "ECLK_DIV"}}},
            "parameters": {"ECLK_DIV": {"default": "DISABLE",
                "values": {"2": ["ECLK_DIV.2"], "4": ["ECLK_DIV.4"]}}}},
        "ECLKSYNC": {"site": "ECLKSYNC",
            "pins": {"ECLKIN": {"pin": "ECLKIN", "clock": true},
                     "ECLKOUT": {"pin": "ECLKOUT", "clock": true, "clock_from": {"port": "ECLKIN"}}}},
        "IDDRX4": {"site": "IOLOGIC", "pad": {"role": "input", "ports": ["D"]},
            "pins": {"Q0": {"pin": "RXDATA0", "timing": {"clock": "SCLK",
                        "launch": {"cell": "IOLOGIC:IDDRXN", "from": "SCLKIN", "to": "RXDATA"}}},
                     "RST": {"pin": "LSRIN", "timing": {"clock": "SCLK",
                        "setup": {"cell": "IOLOGIC:IDDRXN", "pin": "LSRIN", "clock": "SCLKIN"}}},
                     "SCLK": {"pin": "SCLKIN", "clock": true}}}
    }}"#;

    /// Clock outputs follow the input the device data names, divided by the
    /// ratio a parameter sets; a divider that is off drives no known clock
    #[test]
    fn clock_outputs_follow_their_inputs() {
        use skalp_lir::gate_netlist::GateNetId;
        let prims = Primitives::from_json(TIMED).unwrap();
        let known = |net: GateNetId| (net == GateNetId(1)).then_some(500.0);
        let mut div = blackbox("ECLKDIV", &["ECLKIN"], &["DIVOUT"], &[("ECLK_DIV", "4")]);
        div.inputs = vec![GateNetId(1)];
        div.outputs = vec![GateNetId(2)];
        assert_eq!(prims.driven_frequencies(&div, &known), [(0, 125.0)]);
        div.parameters.shift_remove("ECLK_DIV");
        assert!(prims.driven_frequencies(&div, &known).is_empty());
        let mut sync = blackbox("ECLKSYNC", &["ECLKIN"], &["ECLKOUT"], &[]);
        sync.inputs = vec![GateNetId(1)];
        sync.outputs = vec![GateNetId(3)];
        assert_eq!(prims.driven_frequencies(&sync, &known), [(0, 500.0)]);
        // An input with no known frequency gives none
        sync.inputs = vec![GateNetId(9)];
        assert!(prims.driven_frequencies(&sync, &known).is_empty());
    }

    /// A pin timed against a clock names that clock's input
    #[test]
    fn timed_pins_name_their_clock_input() {
        let prims = Primitives::from_json(TIMED).unwrap();
        let iddr = blackbox("IDDRX4", &["D", "RST", "SCLK"], &["Q0"], &[]);
        assert_eq!(prims.timing_clock(&iddr, true, 0), Some(2));
        assert_eq!(prims.timing_clock(&iddr, false, 1), Some(2));
        // Pad and clock inputs are not timed against a clock
        assert_eq!(prims.timing_clock(&iddr, false, 0), None);
        assert_eq!(prims.timing_clock(&iddr, false, 2), None);
    }

    /// The device's DDR blocks come from its tile types: on the LFCPNX-100,
    /// I/O logic at both sides of a bottom pad tile (A's settings in it, B's
    /// in the next), a DQS buffer per group with its settings over five
    /// tiles, the right-side DLL and the bank 3 edge clock blocks.
    #[test]
    fn blocks_come_from_the_tile_types() {
        let Some(db) = super::super::prjoxide_load::find_database() else {
            eprintln!("skipping: no prjoxide database");
            return;
        };
        let variant = super::super::NexusVariant::Lfcpnx100;
        let tiles = super::super::prjoxide_load::load_tilegrid(&db, variant).unwrap();
        if Primitives::load(&db, "LFCPNX").unwrap().pins().is_empty() {
            eprintln!("skipping: the database has no DDR primitives");
            return;
        }
        let sites = sites(&db, "LFCPNX", &tiles);
        let find = |kind: SiteKind, name: &str, x: u32| {
            sites
                .iter()
                .find(|s| s.kind == kind && s.name == name && (x == 0 || s.x == x))
        };
        // DQ0 of the Versa board's LPDDR4 (AA6) is bank 3's pad at R74C114
        let a = find(SiteKind::Iologic, "IOLOGIC_A", 114).expect("IOLOGIC A at C114");
        assert_eq!(a.bank, Some(3));
        assert!(a.pins.contains_key("TXDATA0") && a.pins.contains_key("DQSW270"));
        assert_eq!(a.settings_tiles.len(), 1);
        let b = find(SiteKind::Iologic, "IOLOGIC_B", 114).expect("IOLOGIC B at C114");
        assert!(
            b.settings_tiles[0].contains("C115"),
            "{:?}",
            b.settings_tiles
        );
        let dqs = sites
            .iter()
            .find(|s| s.kind == SiteKind::Dqsbuf && s.x.abs_diff(114) <= 1 && s.y == 74)
            .expect("the DQS buffer of the C114 group");
        assert_eq!(dqs.settings_tiles.len(), 5, "{:?}", dqs.settings_tiles);
        assert!(dqs.pins.contains_key("DQSW270") && dqs.pins.contains_key("RST"));
        let dll = find(SiteKind::Ddrdll, "DDRDLL_CORE_R72C158", 0).expect("right DLL");
        assert!(dll.pins.contains_key("CODE8"));
        let sync = find(SiteKind::Eclksync, "ECLKSYNC3_0", 0).expect("bank 3 ECLKSYNC");
        assert_eq!(sync.bank, Some(3));
        assert!(sync.pins.contains_key("ECLKIN") && sync.pins.contains_key("ECLKOUT"));
        let div = find(SiteKind::Eclkdiv, "ECLKDIVB3_0", 0).expect("bank 3 ECLKDIV");
        assert_eq!(div.prefix, "ECLKDIV3A");
    }

    #[test]
    fn bus_ports_name_their_bit() {
        assert_eq!(port_key("RDCLKSEL[3]"), "RDCLKSEL3");
        assert_eq!(port_key("Q"), "Q");
    }

    #[test]
    fn pin_wires_are_recognised_by_kind() {
        assert_eq!(
            pin_wire(
                SiteKind::Iologic,
                "JTXDATA0_IOLOGIC_CORE_I_GEARING_PIC_TOP_B"
            ),
            Some(("TXDATA0", "B"))
        );
        assert_eq!(
            pin_wire(SiteKind::Dqsbuf, "N1W5:JRST_DQSBUFA_CORE_I_DQS_TOP"),
            Some(("RST", ""))
        );
        assert_eq!(
            pin_wire(SiteKind::Eclksync, "N1:JECLKIN_ECLKSYNC_CORE_ECLKSYNC3_0"),
            Some(("ECLKIN", "3_0"))
        );
        assert_eq!(
            pin_wire(SiteKind::Eclkdiv, "N1:JECLKIN_ECLKSYNC_CORE_ECLKSYNC3_0"),
            None
        );
    }
}
