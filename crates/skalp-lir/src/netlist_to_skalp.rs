//! A synthesized gate netlist written back as a SKALP entity, so that it can
//! be simulated by the behavioural simulator next to the rest of a design.
//!
//! Gate-level simulation of a design that uses vendor primitives needs those
//! primitives' simulation models, and the synthesized netlist only has black
//! boxes for them. Written as SKALP, each library cell becomes the expression
//! or clocked assignment the gate-level simulator evaluates for it, and each
//! black box becomes an instance of the vendor primitive, whose body is its
//! model. The design's own logic is then simulated as the gates synthesis
//! made, and the primitives as their models: a mixed-level simulation.
//!
//! The cell semantics follow `primitive_eval::evaluate_primitive` and the
//! gate-level simulator's sequential rules: a flip-flop's own reset is
//! synchronous and loads 0 whatever its enable says, a flip-flop with
//! `INIT = 1` (or a reset value tied high) powers up high, and a
//! distributed RAM writes on its clock edge and reads asynchronously.

use crate::gate_netlist::{Cell, GateNetId, GateNetlist, FF_INIT_PARAM};
use crate::lir::PrimitiveType;
use crate::primitive_eval::primitive_for_cell;
use crate::tech_library::CellFunction;
use indexmap::IndexMap;
use std::collections::{BTreeMap, HashSet};
use std::fmt::Write as _;

/// A port of the entity the netlist is written as: the netlist's top-level
/// nets are its bits, named `top.<name>` or `top.<name>[<bit>]`
#[derive(Debug, Clone)]
pub struct EntityPort {
    pub name: String,
    pub width: usize,
    pub output: bool,
    pub clock: bool,
}

/// The netlist as the source of a SKALP entity `entity_name` with `ports`.
/// `generics_of` names the generic parameters of the vendor primitive a black
/// box stands for (`None` when the design has no such primitive): those of
/// the cell's parameters become the instance's generic arguments.
pub fn netlist_to_skalp(
    netlist: &GateNetlist,
    entity_name: &str,
    ports: &[EntityPort],
    generics_of: &dyn Fn(&str) -> Option<Vec<String>>,
) -> Result<String, String> {
    Writer::new(netlist).write(entity_name, ports, generics_of)
}

struct Writer<'a> {
    netlist: &'a GateNetlist,
    /// net name -> net, for the port bits
    by_name: IndexMap<&'a str, GateNetId>,
    /// nets that need a signal (canonical nets only)
    used: HashSet<GateNetId>,
    /// canonical nets driven by a cell or a port
    driven: HashSet<GateNetId>,
    /// truth tables the LUTs read, one constant per distinct (inputs, INIT)
    tables: BTreeMap<(usize, u64), String>,
}

/// Signal name of a (canonical) net
fn net(id: GateNetId) -> String {
    format!("n{}", id.0)
}

impl<'a> Writer<'a> {
    fn new(netlist: &'a GateNetlist) -> Self {
        Self {
            netlist,
            by_name: netlist
                .nets
                .iter()
                .map(|n| (n.name.as_str(), n.id))
                .collect(),
            used: HashSet::new(),
            driven: HashSet::new(),
            tables: BTreeMap::new(),
        }
    }

    /// The constant holding a `k`-input LUT's truth table
    fn table(&mut self, k: usize, init: u64) -> String {
        let w = 1usize << k;
        let init = if w >= 64 {
            init
        } else {
            init & ((1u64 << w) - 1)
        };
        self.tables
            .entry((k, init))
            .or_insert_with(|| format!("lut{k}_{init:X}"))
            .clone()
    }

    fn canon(&mut self, id: GateNetId) -> GateNetId {
        let c = self.netlist.resolve_alias(id);
        self.used.insert(c);
        c
    }

    fn sig(&mut self, id: GateNetId) -> String {
        net(self.canon(id))
    }

    /// `{msb, ..., lsb}` of the nets, LSB first in `bits`
    fn concat(&mut self, bits: &[GateNetId]) -> String {
        if bits.len() == 1 {
            return self.sig(bits[0]);
        }
        let parts: Vec<String> = bits.iter().rev().map(|&b| self.sig(b)).collect();
        format!("{{{}}}", parts.join(", "))
    }

    /// The net carrying bit `bit` of port `name`, if the netlist has one
    fn port_bit(&self, name: &str, width: usize, bit: usize) -> Option<GateNetId> {
        let indexed = format!("top.{name}[{bit}]");
        let plain = format!("top.{name}");
        let found = self.by_name.get(indexed.as_str());
        if width == 1 {
            found.or_else(|| self.by_name.get(plain.as_str())).copied()
        } else {
            found.copied()
        }
    }

    fn write(
        mut self,
        entity_name: &str,
        ports: &[EntityPort],
        generics_of: &dyn Fn(&str) -> Option<Vec<String>>,
    ) -> Result<String, String> {
        let nl = self.netlist;
        let mut body = String::new();

        // Input ports drive their bits' nets
        for port in ports.iter().filter(|p| !p.output) {
            for bit in 0..port.width {
                let Some(n) = self.port_bit(&port.name, port.width, bit) else {
                    continue;
                };
                let c = self.canon(n);
                if !self.driven.insert(c) {
                    continue;
                }
                let src = if port.width == 1 {
                    port.name.clone()
                } else {
                    format!("{}[{bit}]", port.name)
                };
                writeln!(body, "    {} = {src}", net(c)).unwrap();
            }
        }

        // Cells
        let mut seq: BTreeMap<(u32, bool), Vec<String>> = BTreeMap::new();
        let mut ff_init: Vec<(GateNetId, bool)> = Vec::new();
        let mut memories: Vec<String> = Vec::new();
        for cell in &nl.cells {
            if let Some(prim) = cell.cell_type.strip_prefix("BLACKBOX_") {
                self.write_blackbox(&mut body, cell, prim, generics_of)?;
                continue;
            }
            let outputs: Vec<GateNetId> = cell.outputs.iter().map(|&o| self.canon(o)).collect();
            for &o in &outputs {
                if !self.driven.insert(o) {
                    return Err(format!(
                        "net '{}' has more than one driver (cell '{}')",
                        nl.nets[o.0 as usize].name, cell.path
                    ));
                }
            }
            let ins: Vec<String> = cell.inputs.iter().map(|&i| self.sig(i)).collect();
            let pin = |i: usize| -> Result<&String, String> {
                ins.get(i).ok_or_else(|| {
                    format!("cell '{}' ({}) has no input {i}", cell.path, cell.cell_type)
                })
            };

            if cell.function.as_ref() == Some(&CellFunction::DistRam16x4) {
                // [DI0..3, WAD0..3, WRE, RAD0..3] -> [DO0..3]
                if ins.len() != 13 || outputs.len() != 4 {
                    return Err(format!(
                        "distributed RAM '{}' has {} inputs / {} outputs, expected 13 / 4",
                        cell.path,
                        ins.len(),
                        outputs.len()
                    ));
                }
                if let Some(init) = cell.parameters.get("INITVAL") {
                    let v = init.trim_start_matches("0x").trim_start_matches("0X");
                    if v.chars().any(|c| c != '0') {
                        return Err(format!(
                            "distributed RAM '{}' has initial contents ({init}), which the \
                             netlist writer does not carry yet",
                            cell.path
                        ));
                    }
                }
                let clock = cell
                    .clock
                    .ok_or_else(|| format!("distributed RAM '{}' has no clock", cell.path))?;
                let clock = self.canon(clock);
                let mem = format!("m{}", cell.id.0);
                memories.push(format!("    signal {mem}: [bit[4]; 16]"));
                let rd = format!("{mem}_rd");
                memories.push(format!("    signal {rd}: bit[4]"));
                let wad = format!("{{{}, {}, {}, {}}}", ins[7], ins[6], ins[5], ins[4]);
                let di = format!("{{{}, {}, {}, {}}}", ins[3], ins[2], ins[1], ins[0]);
                seq.entry((clock.0, true)).or_default().push(format!(
                    "        if {} == 1 {{\n            {mem}[{wad}] = {di}\n        }}",
                    ins[8]
                ));
                let rad = format!("{{{}, {}, {}, {}}}", ins[12], ins[11], ins[10], ins[9]);
                writeln!(body, "    {rd} = {mem}[{rad}]").unwrap();
                for (i, o) in outputs.iter().enumerate() {
                    writeln!(body, "    {} = {rd}[{i}]", net(*o)).unwrap();
                }
                continue;
            }

            let ptype = primitive_for_cell(cell);
            let out = |i: usize| -> Result<String, String> {
                outputs.get(i).map(|&o| net(o)).ok_or_else(|| {
                    format!(
                        "cell '{}' ({}) has no output {i}",
                        cell.path, cell.cell_type
                    )
                })
            };
            let comb = |expr: String| -> Result<String, String> {
                Ok(format!("    {} = {expr}\n", out(0)?))
            };
            let n_ary = |op: &str, n: u8| -> Result<String, String> {
                let parts: Result<Vec<&String>, String> = (0..n as usize).map(pin).collect();
                Ok(parts?
                    .iter()
                    .map(|s| s.as_str())
                    .collect::<Vec<_>>()
                    .join(op))
            };
            let line = match &ptype {
                PrimitiveType::Constant { value } => comb(format!("{}", *value as u8))?,
                PrimitiveType::Buf | PrimitiveType::ClkBuf => comb(pin(0)?.clone())?,
                PrimitiveType::Inv => comb(format!("~{}", pin(0)?))?,
                PrimitiveType::And { inputs } => comb(n_ary(" & ", *inputs)?)?,
                PrimitiveType::Or { inputs } => comb(n_ary(" | ", *inputs)?)?,
                PrimitiveType::Nand { inputs } => comb(format!("~({})", n_ary(" & ", *inputs)?))?,
                PrimitiveType::Nor { inputs } => comb(format!("~({})", n_ary(" | ", *inputs)?))?,
                PrimitiveType::Xor => comb(format!("{} ^ {}", pin(0)?, pin(1)?))?,
                PrimitiveType::Xnor => comb(format!("~({} ^ {})", pin(0)?, pin(1)?))?,
                PrimitiveType::AndNot => comb(format!("{} & ~{}", pin(0)?, pin(1)?))?,
                PrimitiveType::OrNot => comb(format!("{} | ~{}", pin(0)?, pin(1)?))?,
                PrimitiveType::Aoi21 => {
                    comb(format!("~(({} & {}) | {})", pin(0)?, pin(1)?, pin(2)?))?
                }
                PrimitiveType::Aoi22 => comb(format!(
                    "~(({} & {}) | ({} & {}))",
                    pin(0)?,
                    pin(1)?,
                    pin(2)?,
                    pin(3)?
                ))?,
                PrimitiveType::Oai21 => {
                    comb(format!("~(({} | {}) & {})", pin(0)?, pin(1)?, pin(2)?))?
                }
                PrimitiveType::Oai22 => comb(format!(
                    "~(({} | {}) & ({} | {}))",
                    pin(0)?,
                    pin(1)?,
                    pin(2)?,
                    pin(3)?
                ))?,
                PrimitiveType::Mux2 => comb(format!(
                    "if {} == 1 {{ {} }} else {{ {} }}",
                    pin(0)?,
                    pin(2)?,
                    pin(1)?
                ))?,
                PrimitiveType::Lut4 { init } => {
                    let table = self.table(4, *init as u64);
                    comb(format!("{table}[{}]", select(4, &ins)))?
                }
                PrimitiveType::Lut6 { init } => {
                    // a LUT5 is a 6-input LUT with five inputs
                    let k = ins.len().clamp(5, 6);
                    let table = self.table(k, *init);
                    comb(format!("{table}[{}]", select(k, &ins)))?
                }
                PrimitiveType::Ccu2 { init0, init1 } => {
                    if ins.len() != 9 || outputs.len() != 3 {
                        return Err(format!(
                            "carry cell '{}' has {} inputs / {} outputs, expected 9 / 3",
                            cell.path,
                            ins.len(),
                            outputs.len()
                        ));
                    }
                    // each half: the LUT's output P propagates the carry in
                    // (sum = P ^ carry in), else the LUT on A and B alone
                    // generates the carry out
                    let t0 = self.table(4, *init0 as u64);
                    let t1 = self.table(4, *init1 as u64);
                    let (p0, c1, p1) = (
                        format!("k{}_p0", cell.id.0),
                        format!("k{}_c1", cell.id.0),
                        format!("k{}_p1", cell.id.0),
                    );
                    for k in [&p0, &c1, &p1] {
                        memories.push(format!("    signal {k}: bit"));
                    }
                    format!(
                        "    {p0} = {t0}[{{{d0}, {cc0}, {b0}, {a0}}}]\n\
                         \x20   {s0} = {p0} ^ {cin}\n\
                         \x20   {c1} = if {p0} == 1 {{ {cin} }} else {{ {t0}[{{{b0}, {a0}}}] }}\n\
                         \x20   {p1} = {t1}[{{{d1}, {cc1}, {b1}, {a1}}}]\n\
                         \x20   {s1} = {p1} ^ {c1}\n\
                         \x20   {co} = if {p1} == 1 {{ {c1} }} else {{ {t1}[{{{b1}, {a1}}}] }}\n",
                        a0 = ins[0],
                        b0 = ins[1],
                        cc0 = ins[2],
                        d0 = ins[3],
                        a1 = ins[4],
                        b1 = ins[5],
                        cc1 = ins[6],
                        d1 = ins[7],
                        cin = ins[8],
                        s0 = out(0)?,
                        s1 = out(1)?,
                        co = out(2)?,
                    )
                }
                PrimitiveType::DffP
                | PrimitiveType::DffN
                | PrimitiveType::DffNeg
                | PrimitiveType::DffE
                | PrimitiveType::DffSR => {
                    let clock = cell
                        .clock
                        .ok_or_else(|| format!("flip-flop '{}' has no clock", cell.path))?;
                    let clock = self.canon(clock);
                    let rising = !matches!(ptype, PrimitiveType::DffN | PrimitiveType::DffNeg);
                    let q = out(0)?;
                    let d = pin(0)?;
                    let enable = if matches!(ptype, PrimitiveType::DffE) {
                        Some(pin(1)?.clone())
                    } else {
                        None
                    };
                    let reset_index = match ptype {
                        PrimitiveType::DffSR => Some(1),
                        PrimitiveType::DffE => Some(2),
                        _ => None,
                    };
                    let reset = match reset_index.and_then(|i| ins.get(i)) {
                        Some(r) => Some(r.clone()),
                        None if reset_index.is_some() => cell.reset.map(|r| self.sig(r)),
                        None => None,
                    };
                    let load = match &enable {
                        Some(en) => format!("if {en} == 1 {{\n            {q} = {d}\n        }}"),
                        None => format!("{q} = {d}"),
                    };
                    let stmt = match reset {
                        Some(r) => format!(
                            "        if {r} == 1 {{\n            {q} = 0\n        }} else {}",
                            if enable.is_some() {
                                load
                            } else {
                                format!("{{\n            {load}\n        }}")
                            }
                        ),
                        None => format!("        {load}"),
                    };
                    seq.entry((clock.0, rising)).or_default().push(stmt);
                    ff_init.push((outputs[0], self.starts_high(cell)));
                    String::new()
                }
                other => {
                    return Err(format!(
                        "cell '{}' ({}, {:?}) has no SKALP form in the netlist writer",
                        cell.path, cell.cell_type, other
                    ))
                }
            };
            body.push_str(&line);
        }

        // Output ports
        let mut out_lines = String::new();
        for port in ports.iter().filter(|p| p.output) {
            let mut bits = Vec::with_capacity(port.width);
            for bit in 0..port.width {
                let n = self
                    .port_bit(&port.name, port.width, bit)
                    .ok_or_else(|| format!("output port '{}' bit {bit} has no net", port.name))?;
                bits.push(n);
            }
            let value = self.concat(&bits);
            writeln!(out_lines, "    {} = {value}", port.name).unwrap();
        }

        // Clocked blocks: one signal of clock type per clock net
        let mut seq_text = String::new();
        let mut clock_sigs: BTreeMap<u32, String> = BTreeMap::new();
        for ((clock, rising), stmts) in &seq {
            let ck = clock_sigs
                .entry(*clock)
                .or_insert_with(|| format!("c{clock}"))
                .clone();
            writeln!(
                seq_text,
                "    on({ck}.{}) {{\n{}\n    }}",
                if *rising { "rise" } else { "fall" },
                stmts.join("\n")
            )
            .unwrap();
        }

        // Declarations
        let mut decl = String::new();
        let init: IndexMap<GateNetId, bool> = ff_init.into_iter().collect();
        let mut used: Vec<GateNetId> = self.used.iter().copied().collect();
        used.sort_by_key(|n| n.0);
        for n in &used {
            match init.get(n) {
                Some(high) => {
                    writeln!(decl, "    signal {}: bit = {}", net(*n), *high as u8).unwrap()
                }
                None => writeln!(decl, "    signal {}: bit", net(*n)).unwrap(),
            }
        }
        // nets nothing drives read as 0, as in the gate-level simulator
        let mut tied = String::new();
        for n in &used {
            if !self.driven.contains(n) && !init.contains_key(n) {
                writeln!(tied, "    {} = 0", net(*n)).unwrap();
            }
        }
        for (clock, ck) in &clock_sigs {
            writeln!(decl, "    signal {ck}: clock").unwrap();
            writeln!(tied, "    {ck} = {}", net(GateNetId(*clock))).unwrap();
        }
        for m in &memories {
            writeln!(decl, "{m}").unwrap();
        }
        for ((k, init), name) in &self.tables {
            let w = 1usize << k;
            writeln!(
                decl,
                "    let {name}: bit[{w}] = {w}'h{init:0digits$X}",
                digits = w / 4
            )
            .unwrap();
        }

        let mut src = String::new();
        writeln!(
            src,
            "// Gate-level netlist of {entity_name} ({} cells, library {})",
            nl.cells.len(),
            nl.library_name
        )
        .unwrap();
        writeln!(src, "entity {entity_name} {{").unwrap();
        for p in ports {
            let ty = if p.clock {
                "clock".to_string()
            } else if p.width == 1 {
                "bit".to_string()
            } else {
                format!("bit[{}]", p.width)
            };
            writeln!(
                src,
                "    {} {}: {ty}",
                if p.output { "out" } else { "in" },
                p.name
            )
            .unwrap();
        }
        writeln!(src, "}}\n\nimpl {entity_name} {{").unwrap();
        src.push_str(&decl);
        src.push_str(&tied);
        src.push_str(&body);
        src.push_str(&seq_text);
        src.push_str(&out_lines);
        src.push_str("}\n");
        Ok(src)
    }

    /// Whether a flip-flop powers up high: `INIT = 1`, or a reset value tied
    /// high through the reset multiplexer synthesis put before it
    fn starts_high(&self, cell: &Cell) -> bool {
        let nl = self.netlist;
        if cell.parameters.get(FF_INIT_PARAM).map(String::as_str) == Some("1") {
            return true;
        }
        let Some(&d) = cell.inputs.first() else {
            return false;
        };
        let Some(mux) = nl
            .cells
            .iter()
            .find(|c| c.outputs.contains(&d) && c.source_op.as_deref() == Some("ResetMux"))
        else {
            return false;
        };
        let Some(&value) = mux.inputs.get(2) else {
            return false;
        };
        nl.cells.iter().any(|t| {
            t.outputs.contains(&value)
                && t.source_op.as_deref() == Some("ResetValue")
                && (t.cell_type.contains("HIGH") || t.cell_type.contains("HI"))
        })
    }

    fn write_blackbox(
        &mut self,
        body: &mut String,
        cell: &Cell,
        prim: &str,
        generics_of: &dyn Fn(&str) -> Option<Vec<String>>,
    ) -> Result<(), String> {
        let nl = self.netlist;
        let generics = generics_of(prim).ok_or_else(|| {
            format!(
                "black box '{}' ({prim}) has no simulation model in this design",
                cell.path
            )
        })?;

        // pins by port, bits in index order
        let ports_of =
            |output: bool, count: usize| -> Result<IndexMap<String, Vec<(usize, usize)>>, String> {
                let mut ports: IndexMap<String, Vec<(usize, usize)>> = IndexMap::new();
                for i in 0..count {
                    let name = cell.blackbox_pin(output, i).ok_or_else(|| {
                        format!("black box '{}' has no name for pin {i}", cell.path)
                    })?;
                    let (port, bit) = match name.split_once('[') {
                        Some((p, rest)) => (
                            p.to_string(),
                            rest.trim_end_matches(']')
                                .parse::<usize>()
                                .map_err(|_| format!("black box pin '{name}'"))?,
                        ),
                        None => (name.to_string(), 0),
                    };
                    ports.entry(port).or_default().push((bit, i));
                }
                for bits in ports.values_mut() {
                    bits.sort();
                }
                Ok(ports)
            };
        let ins = ports_of(false, cell.inputs.len())?;
        let outs = ports_of(true, cell.outputs.len())?;

        let inst = format!("b{}", cell.id.0);
        let args: Vec<String> = generics
            .iter()
            .filter_map(|g| cell.parameters.get(g).map(|v| format!("{g}: {v}")))
            .collect();
        let mut conns = Vec::new();
        for (port, bits) in &ins {
            let nets: Vec<GateNetId> = bits.iter().map(|&(_, i)| cell.inputs[i]).collect();
            conns.push(format!("        {port}: {}", self.concat(&nets)));
        }
        writeln!(
            body,
            "    inst {inst} = {prim}{} {{\n{}\n    }}",
            if args.is_empty() {
                String::new()
            } else {
                format!("<{}>", args.join(", "))
            },
            conns.join(",\n")
        )
        .unwrap();
        for (port, bits) in &outs {
            let bus = bits.len() > 1
                || bits[0].0 != 0
                || cell
                    .blackbox_pin(true, bits[0].1)
                    .is_some_and(|n| n.contains('['));
            let width = bits.iter().map(|&(b, _)| b + 1).max().unwrap_or(1);
            let wire = format!("{inst}_{port}");
            if bus {
                writeln!(body, "    signal {wire}: bit[{width}]").unwrap();
            } else {
                writeln!(body, "    signal {wire}: bit").unwrap();
            }
            writeln!(body, "    {wire} = {inst}.{port}").unwrap();
            for &(bit, i) in bits {
                let o = self.canon(cell.outputs[i]);
                if !self.driven.insert(o) {
                    return Err(format!(
                        "net '{}' has more than one driver (black box '{}')",
                        nl.nets[o.0 as usize].name, cell.path
                    ));
                }
                if bus {
                    writeln!(body, "    {} = {wire}[{bit}]", net(o)).unwrap();
                } else {
                    writeln!(body, "    {} = {wire}", net(o)).unwrap();
                }
            }
        }
        Ok(())
    }
}

/// The index a `k`-input LUT reads its table at: `{i[k-1], ..., i[0]}`
/// (input 0 is the LSB); a missing input reads 0
fn select(k: usize, ins: &[String]) -> String {
    let sel: Vec<String> = (0..k)
        .rev()
        .map(|i| ins.get(i).cloned().unwrap_or_else(|| "0".to_string()))
        .collect();
    format!("{{{}}}", sel.join(", "))
}
