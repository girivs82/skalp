//! M1b: build SKALP's routing graph from the prjoxide tiletype bit database.
//!
//! prjoxide stores wires per tile-type in *normalized* form — names are relative
//! to the tile that owns them. To assemble a single global routing graph we must
//! invert that normalization: a normalized wire seen from tile `(tx, ty)` resolves
//! to a canonical **node** that other tiles may also reference under a different
//! relative name. Unifying tile-wires that resolve to the same node *is* the
//! routing graph's connectivity.
//!
//! This module implements the inverse of `prjoxide/src/wires.rs::normalize_wire`.
//! See that file for the forward direction and the prefix grammar.

/// A canonical routing node — the identity under which tile-wires are unified.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NodeKey {
    /// A fabric wire whose nominal home is tile `(x, y)`, named `name` there.
    Local { x: u32, y: u32, name: String },
    /// A device-global wire (`G:` prefix, or VCC) — one node regardless of where seen.
    Global(String),
    /// A clock-network / special wire (BRANCH/SPINE/HROW/DQSG/BANK/TAP).
    ///
    /// M1b unifies these by their full normalized string so the clock spine is one
    /// node device-wide. This is coarse (it ignores the per-region structure of the
    /// real clock tree) but correct for fabric routing; M1c refines the clock model.
    Special(String),
}

/// Resolve a normalized wire name, as seen from tile `(tx, ty)`, to its canonical node.
///
/// Inverse of `normalize_wire`: a rel-prefix `N{d}`/`S{d}`/`E{d}`/`W{d}` shifts the
/// node's home tile (N = up/−y, S = down/+y, E = right/+x, W = left/−x); a bare name
/// (no `:`) is local to `(tx, ty)`; recognized special prefixes map to `Global`/`Special`.
///
/// NOTE (edges): near the device border, `normalize_wire` applies `handle_edge_name`
/// renaming to keep nominal coordinates in bounds. This inverse does not yet undo that,
/// so a handful of border wires may resolve to a slightly-off home tile. Interior fabric
/// (where compute logic is placed) is exact. Tracked for M1c. Returns `None` only for a
/// rel-offset that would underflow the grid (border artifact), letting callers drop it.
pub fn resolve_wire(tx: u32, ty: u32, normalized: &str) -> Option<NodeKey> {
    // Device-global and clock-network prefixes (checked before rel-prefix parsing).
    if let Some(rest) = normalized.strip_prefix("G:") {
        return Some(NodeKey::Global(rest.to_string()));
    }
    if normalized == "G:VCC" || normalized == "VCC" {
        return Some(NodeKey::Global("VCC".to_string()));
    }
    for sp in [
        "BRANCH_L:",
        "BRANCH_R:",
        "BRANCH:",
        "SPINE:",
        "HROW:",
        "DQSG:",
        "BANK:",
    ] {
        if normalized.starts_with(sp) {
            return Some(NodeKey::Special(normalized.to_string()));
        }
    }

    // Split optional rel-prefix from the base name on the first ':'.
    let Some(colon) = normalized.find(':') else {
        // No prefix → local to this tile.
        return Some(NodeKey::Local {
            x: tx,
            y: ty,
            name: normalized.to_string(),
        });
    };
    let (prefix, base) = normalized.split_at(colon);
    let base = &base[1..]; // skip ':'

    // Parse the rel-prefix grammar: ([NS]\d+)?([EW]\d+)? .
    let (mut dx, mut dy): (i64, i64) = (0, 0);
    let bytes = prefix.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        let dir = bytes[i] as char;
        i += 1;
        let start = i;
        while i < bytes.len() && bytes[i].is_ascii_digit() {
            i += 1;
        }
        if start == i {
            // Unrecognized prefix shape — treat the whole thing as a special node.
            return Some(NodeKey::Special(normalized.to_string()));
        }
        let n: i64 = prefix[start..i].parse().ok()?;
        match dir {
            'N' => dy = -n,
            'S' => dy = n,
            'E' => dx = n,
            'W' => dx = -n,
            _ => return Some(NodeKey::Special(normalized.to_string())),
        }
    }

    let hx = tx as i64 + dx;
    let hy = ty as i64 + dy;
    if hx < 0 || hy < 0 {
        return None; // border artifact (see edge note above)
    }
    Some(NodeKey::Local {
        x: hx as u32,
        y: hy as u32,
        name: base.to_string(),
    })
}

// ---------------------------------------------------------------------------
// RON tiletype bit-database parsing + per-tile edge resolution
// ---------------------------------------------------------------------------

use super::PrjoxideTile;
use serde::Deserialize;
use std::collections::BTreeMap;
use std::path::Path;

/// One configuration bit, tile-relative (as stored in the `.ron`).
#[derive(Debug, Clone, Deserialize)]
pub struct ConfigBit {
    pub frame: usize,
    pub bit: usize,
    #[serde(default)]
    pub invert: bool,
}

#[derive(Debug, Clone, Deserialize)]
struct PipData {
    from_wire: String,
    #[serde(default)]
    bits: Vec<ConfigBit>,
}

#[derive(Debug, Clone, Deserialize)]
struct ConnData {
    from_wire: String,
}

/// The per-tile-type bit database (`tiletypes/<TYPE>.ron`).
///
/// Only the routing-relevant sections (`pips`, `conns`) are deserialized; `words`
/// and `enums` (BEL config, used by M1c) are present in the file but ignored here —
/// serde drops unknown fields.
#[derive(Debug, Clone, Deserialize)]
pub struct TileBitsDb {
    #[serde(default)]
    pips: BTreeMap<String, Vec<PipData>>,
    #[serde(default)]
    conns: BTreeMap<String, Vec<ConnData>>,
}

/// A configuration bit in *global* (whole-bitstream) coordinates.
///
/// Computed as `tile.start_frame + cb.frame`, `tile.start_bit + cb.bit`. This is
/// what M2 sets in the packed frames when the owning PIP is used.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlobalBit {
    pub frame: u32,
    pub bit: u32,
    pub invert: bool,
}

/// A resolved routing edge: a connection from `src` node to `dst` node.
///
/// `configurable` PIPs carry the `bits` that enable them; fixed `conns` are
/// always-on (empty `bits`). Endpoints are canonical [`NodeKey`]s, already
/// unified across tiles.
#[derive(Debug, Clone)]
pub struct ResolvedEdge {
    pub src: NodeKey,
    pub dst: NodeKey,
    pub configurable: bool,
    pub bits: Vec<GlobalBit>,
}

/// Load and parse one tiletype's `.ron` bit database.
///
/// `db_root` is the prjoxide database root; `family` is e.g. `"LFCPNX"`. Returns
/// `Ok(None)` if no `.ron` exists for `tiletype` (some grid tiletypes have no
/// routing database of their own).
pub fn load_tiletype(
    db_root: &Path,
    family: &str,
    tiletype: &str,
) -> Result<Option<TileBitsDb>, String> {
    let path = db_root
        .join(family)
        .join("tiletypes")
        .join(format!("{tiletype}.ron"));
    if !path.exists() {
        return Ok(None);
    }
    let text =
        std::fs::read_to_string(&path).map_err(|e| format!("reading {}: {}", path.display(), e))?;
    let db: TileBitsDb =
        ron::from_str(&text).map_err(|e| format!("parsing {}: {}", path.display(), e))?;
    Ok(Some(db))
}

/// Resolve every pip and fixed connection in `tile` (of type `db`) into global
/// routing edges, dropping any whose endpoint falls outside the grid (border
/// artifact — see [`resolve_wire`]).
pub fn tile_edges(tile: &PrjoxideTile, db: &TileBitsDb) -> Vec<ResolvedEdge> {
    let (tx, ty) = (tile.x, tile.y);
    let mut edges = Vec::new();

    let to_global = |cb: &ConfigBit| GlobalBit {
        frame: tile.start_frame + cb.frame as u32,
        bit: tile.start_bit + cb.bit as u32,
        invert: cb.invert,
    };

    for (dst_name, arcs) in &db.pips {
        let Some(dst) = resolve_wire(tx, ty, dst_name) else {
            continue;
        };
        for arc in arcs {
            let Some(src) = resolve_wire(tx, ty, &arc.from_wire) else {
                continue;
            };
            edges.push(ResolvedEdge {
                src,
                dst: dst.clone(),
                configurable: true,
                bits: arc.bits.iter().map(to_global).collect(),
            });
        }
    }
    for (dst_name, conns) in &db.conns {
        let Some(dst) = resolve_wire(tx, ty, dst_name) else {
            continue;
        };
        for conn in conns {
            let Some(src) = resolve_wire(tx, ty, &conn.from_wire) else {
                continue;
            };
            edges.push(ResolvedEdge {
                src,
                dst: dst.clone(),
                configurable: false,
                bits: Vec::new(),
            });
        }
    }
    edges
}

// ---------------------------------------------------------------------------
// M1b-part3: full-device graph assembly (interned, compact)
// ---------------------------------------------------------------------------

use super::super::{Pip, PipId, Wire, WireId, WireType};

/// The assembled routing graph: flat, integer-indexed arrays plus the lookup maps
/// the `Device` trait serves, and the `PipId → GlobalBit` reverse map M2 consumes.
///
/// Memory note: the LFCPNX-100 fabric is ~24M+ edges. Wires are interned (one per
/// canonical [`NodeKey`]); pips are a flat `Vec` indexed by `PipId`. Build a bounded
/// region for tests/bring-up via the `bbox` arg to keep this cheap.
#[derive(Default)]
pub struct RoutingGraph {
    pub wires: Vec<Wire>,
    pub pips: Vec<Pip>,
    pub wire_names: std::collections::HashMap<String, WireId>,
    /// Pips driving each wire (indexed by destination) — `Device::wire_pips`.
    pub wire_to_pips: std::collections::HashMap<WireId, Vec<PipId>>,
    /// Pips sourced by each wire — `Device::wire_src_pips`.
    pub wire_src_pips: std::collections::HashMap<WireId, Vec<PipId>>,
    /// Wires whose home is each grid cell — `Device::tile_wires`.
    pub tile_wires: std::collections::HashMap<(u32, u32), Vec<WireId>>,
    /// Per-pip config bits in global bitstream coordinates (parallel to `pips`).
    pub pip_bits: Vec<Vec<GlobalBit>>,
    intern: std::collections::HashMap<NodeKey, WireId>,
}

/// Classify a canonical fabric wire name into SKALP's `WireType` (informational;
/// routing cost is carried by pip delay). Nexus span naming: `H01/V01` (neighbour),
/// `H02/V02` (span-2 ≈ Span4), `H06/V06` (span-6 ≈ Span12); `J…COUT/CIN` carry;
/// `J…` CIB/site signals → BelPin; else Local.
fn classify_wire(node: &NodeKey) -> WireType {
    let name = match node {
        NodeKey::Global(_) => return WireType::Global(0),
        NodeKey::Special(_) => return WireType::Global(0),
        NodeKey::Local { name, .. } => name.as_str(),
    };
    if name.contains("FCOUT") || name.contains("FCIN") {
        return WireType::CarryChain;
    }
    let b = name.as_bytes();
    match &name[..name.len().min(3)] {
        "H01" => WireType::Neighbour,
        "V01" => WireType::Neighbour,
        "H02" => WireType::Span4H(0),
        "V02" => WireType::Span4V(0),
        "H06" => WireType::Span12H(0),
        "V06" => WireType::Span12V(0),
        _ if b.first() == Some(&b'J') => WireType::BelPin,
        _ => WireType::Local(0),
    }
}

/// Synthesize the canonical (Lattice-style) global name for a node.
pub fn node_name(node: &NodeKey) -> String {
    match node {
        NodeKey::Local { x, y, name } => format!("R{y}C{x}_{name}"),
        NodeKey::Global(n) => format!("G:{n}"),
        NodeKey::Special(s) => s.clone(),
    }
}

impl RoutingGraph {
    /// Intern a node to a `WireId`, creating the `Wire` (and tile/name index) once.
    fn intern(&mut self, node: NodeKey) -> WireId {
        if let Some(&id) = self.intern.get(&node) {
            return id;
        }
        let id = WireId(self.wires.len() as u32);
        let name = node_name(&node);
        let wire_type = classify_wire(&node);
        let (tx, ty) = match &node {
            NodeKey::Local { x, y, .. } => (*x, *y),
            _ => (0, 0),
        };
        // Intrinsic wire delay (ps) by span length.
        let t = &super::data::TIMING_GRADE10;
        let delay = match wire_type {
            WireType::Span12H(_) | WireType::Span12V(_) => (t.span6_delay * 1000.0) as u32,
            WireType::Span4H(_) | WireType::Span4V(_) => (t.span2_delay * 1000.0) as u32,
            _ => (t.span0_delay * 1000.0) as u32,
        };
        self.wires.push(Wire {
            id,
            name: name.clone(),
            wire_type,
            tile_x: tx,
            tile_y: ty,
            delay,
        });
        self.wire_names.insert(name, id);
        if matches!(node, NodeKey::Local { .. }) {
            self.tile_wires.entry((tx, ty)).or_default().push(id);
        }
        self.intern.insert(node, id);
        id
    }
}

/// Assemble the routing graph from `tiles`. If `bbox = Some((x0,y0,x1,y1))`, only
/// tiles within that inclusive grid window contribute (cheap subset for tests/bring-up);
/// `None` builds the full device. Tiletype `.ron`s are parsed once and cached.
pub fn build_routing_graph(
    db_root: &Path,
    family: &str,
    tiles: &[PrjoxideTile],
    bbox: Option<(u32, u32, u32, u32)>,
) -> Result<RoutingGraph, String> {
    let mut g = RoutingGraph::default();
    let mut cache: std::collections::HashMap<String, Option<TileBitsDb>> =
        std::collections::HashMap::new();
    let t = &super::data::TIMING_GRADE10;
    let pip_delay = (t.pip_local_to_local * 1000.0) as u32;

    for tile in tiles {
        if let Some((x0, y0, x1, y1)) = bbox {
            if tile.x < x0 || tile.x > x1 || tile.y < y0 || tile.y > y1 {
                continue;
            }
        }
        let db = cache.entry(tile.tiletype.clone()).or_insert_with(|| {
            load_tiletype(db_root, family, &tile.tiletype)
                .ok()
                .flatten()
        });
        let Some(db) = db.clone() else { continue };

        for edge in tile_edges(tile, &db) {
            let src = g.intern(edge.src);
            let dst = g.intern(edge.dst);
            let pid = PipId(g.pips.len() as u32);
            g.pips.push(Pip {
                id: pid,
                src_wire: src,
                dst_wire: dst,
                delay: if edge.configurable { pip_delay } else { 0 },
                configurable: edge.configurable,
                tile_x: tile.x,
                tile_y: tile.y,
            });
            g.pip_bits.push(edge.bits);
            g.wire_to_pips.entry(dst).or_default().push(pid);
            g.wire_src_pips.entry(src).or_default().push(pid);
        }
    }
    Ok(g)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn local(x: u32, y: u32, n: &str) -> NodeKey {
        NodeKey::Local {
            x,
            y,
            name: n.to_string(),
        }
    }

    #[test]
    fn bare_name_is_local_to_tile() {
        // From PLC.ron: site/local wires carry no prefix.
        assert_eq!(resolve_wire(20, 10, "JF0"), Some(local(20, 10, "JF0")));
        assert_eq!(
            resolve_wire(20, 10, "H02E0001"),
            Some(local(20, 10, "H02E0001"))
        );
    }

    #[test]
    fn cardinal_prefixes_shift_home_tile() {
        // N3: → 3 tiles up (−y). From PLC.ron "N3:V06S0003".
        assert_eq!(
            resolve_wire(20, 10, "N3:V06S0003"),
            Some(local(20, 7, "V06S0003"))
        );
        // W3: → 3 tiles left (−x). From PLC.ron "W3:H06E0003".
        assert_eq!(
            resolve_wire(20, 10, "W3:H06E0003"),
            Some(local(17, 10, "H06E0003"))
        );
        // N1: → 1 up. From PLC.ron "N1:V01S0000".
        assert_eq!(
            resolve_wire(20, 10, "N1:V01S0000"),
            Some(local(20, 9, "V01S0000"))
        );
    }

    #[test]
    fn south_and_east_directions() {
        assert_eq!(resolve_wire(20, 10, "S2:FOO"), Some(local(20, 12, "FOO")));
        assert_eq!(resolve_wire(20, 10, "E4:BAR"), Some(local(24, 10, "BAR")));
    }

    #[test]
    fn combined_prefix_ns_then_ew() {
        // Grammar is ([NS]\d+)?([EW]\d+)? — N then E.
        assert_eq!(
            resolve_wire(20, 10, "S2E1:WIRE"),
            Some(local(21, 12, "WIRE"))
        );
        assert_eq!(
            resolve_wire(20, 10, "N1W2:WIRE"),
            Some(local(18, 9, "WIRE"))
        );
    }

    #[test]
    fn unification_two_tiles_same_node() {
        // A wire 3 tiles north of (20,10) is the SAME node as one 3 tiles south
        // of (20,4): both are home (20,7)/V06S0003.
        let a = resolve_wire(20, 10, "N3:V06S0003").unwrap();
        let b = resolve_wire(20, 4, "S3:V06S0003").unwrap();
        assert_eq!(a, b);
    }

    #[test]
    fn global_and_special_prefixes() {
        assert_eq!(
            resolve_wire(20, 10, "G:HFSN0000"),
            Some(NodeKey::Global("HFSN0000".to_string()))
        );
        assert_eq!(
            resolve_wire(20, 10, "VCC"),
            Some(NodeKey::Global("VCC".to_string()))
        );
        assert!(matches!(
            resolve_wire(20, 10, "SPINE:VPSX0500").unwrap(),
            NodeKey::Special(_)
        ));
        assert!(matches!(
            resolve_wire(20, 10, "BRANCH_L:HPBX0100").unwrap(),
            NodeKey::Special(_)
        ));
    }

    #[test]
    fn border_underflow_returns_none() {
        // 3 tiles north of row 1 underflows — dropped as a border artifact.
        assert_eq!(resolve_wire(20, 1, "N3:V06S0003"), None);
    }

    // --- M1b-part2: real RON parse + edge resolution on actual silicon data ---

    use super::super::prjoxide_load::{find_database, load_tilegrid};
    use super::super::NexusVariant;

    #[test]
    fn real_plc_pip_resolves_with_global_bits() {
        let Some(db) = find_database() else {
            eprintln!("PRJOXIDE_DB not found — skipping");
            return;
        };
        let variant = NexusVariant::Lfcpnx100;
        let tiles = load_tilegrid(&db, variant).expect("tilegrid");
        // An interior PLC tile (avoid borders so resolution is exact).
        let plc = tiles
            .iter()
            .find(|t| t.tiletype == "PLC" && t.x > 5 && t.y > 5)
            .expect("a PLC tile");
        let bits_db = load_tiletype(&db, variant.prjoxide_family(), "PLC")
            .expect("load PLC.ron")
            .expect("PLC.ron exists");

        let edges = tile_edges(plc, &bits_db);
        assert!(!edges.is_empty(), "PLC has routing edges");

        // The canonical first pip from PLC.ron: dst "E1:H02E0001" <- "JF0",
        // bits [(f25,b6),(f26,b7)]. dst resolves +1 in x; src is local.
        let dst = NodeKey::Local {
            x: plc.x + 1,
            y: plc.y,
            name: "H02E0001".to_string(),
        };
        let src = NodeKey::Local {
            x: plc.x,
            y: plc.y,
            name: "JF0".to_string(),
        };
        let edge = edges
            .iter()
            .find(|e| e.dst == dst && e.src == src)
            .expect("E1:H02E0001 <- JF0 pip present");

        assert!(edge.configurable);
        assert!(edge.bits.contains(&GlobalBit {
            frame: plc.start_frame + 25,
            bit: plc.start_bit + 6,
            invert: false,
        }));
        assert!(edge.bits.contains(&GlobalBit {
            frame: plc.start_frame + 26,
            bit: plc.start_bit + 7,
            invert: false,
        }));

        // Fixed conns resolve too (always-on, no bits). PLC.ron: "JA0_SLICEA" <- "JA0".
        let has_conn = edges.iter().any(|e| {
            !e.configurable
                && e.dst
                    == NodeKey::Local {
                        x: plc.x,
                        y: plc.y,
                        name: "JA0_SLICEA".to_string(),
                    }
        });
        assert!(has_conn, "fixed connection JA0_SLICEA resolved");

        eprintln!(
            "M1b-part2 OK: PLC@({},{}) → {} edges ({} configurable pips + fixed conns), \
             global bits offset by frame+{} bit+{}",
            plc.x,
            plc.y,
            edges.len(),
            edges.iter().filter(|e| e.configurable).count(),
            plc.start_frame,
            plc.start_bit,
        );
    }

    #[test]
    fn build_region_graph_is_consistent() {
        let Some(db) = find_database() else {
            eprintln!("PRJOXIDE_DB not found — skipping");
            return;
        };
        let variant = NexusVariant::Lfcpnx100;
        let tiles = load_tilegrid(&db, variant).expect("tilegrid");
        // A small interior window keeps the build cheap and edges in-bounds.
        let g = build_routing_graph(&db, variant.prjoxide_family(), &tiles, Some((6, 6, 10, 10)))
            .expect("build graph");

        assert!(!g.wires.is_empty() && !g.pips.is_empty(), "graph populated");

        // pip_bits is parallel to pips.
        assert_eq!(g.pip_bits.len(), g.pips.len(), "pip_bits parallel to pips");

        // Every pip's endpoints are valid wire indices, and the adjacency maps
        // agree with the flat pip array.
        for pip in &g.pips {
            assert!((pip.src_wire.0 as usize) < g.wires.len());
            assert!((pip.dst_wire.0 as usize) < g.wires.len());
            assert!(
                g.wire_to_pips[&pip.dst_wire].contains(&pip.id),
                "dst adjacency contains pip"
            );
            assert!(
                g.wire_src_pips[&pip.src_wire].contains(&pip.id),
                "src adjacency contains pip"
            );
        }

        // Interning is sound: name → id → same name round-trips, and each name unique.
        for (name, &id) in &g.wire_names {
            assert_eq!(&g.wires[id.0 as usize].name, name);
        }
        assert_eq!(
            g.wire_names.len(),
            g.wires.len(),
            "every wire interned exactly once"
        );

        // Configurable pips carry bits; fixed conns don't.
        let cfg_with_bits = g
            .pips
            .iter()
            .zip(&g.pip_bits)
            .filter(|(p, b)| p.configurable && !b.is_empty())
            .count();
        let conns = g.pips.iter().filter(|p| !p.configurable).count();
        assert!(cfg_with_bits > 0, "some configurable pips carry bits");

        eprintln!(
            "M1b-part3 OK: region (6,6)-(10,10) → {} wires, {} pips ({} cfg+bits, {} fixed conns), \
             {} total config bits",
            g.wires.len(),
            g.pips.len(),
            cfg_with_bits,
            conns,
            g.pip_bits.iter().map(|b| b.len()).sum::<usize>(),
        );
    }
}
