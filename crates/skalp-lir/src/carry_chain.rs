//! Lattice `CCU2` carry chains (Nexus `CCU2`, ECP5 `CCU2C`).
//!
//! Adders, subtractors and magnitude comparators of at least
//! [`CARRY_CHAIN_MIN_WIDTH`] bits are mapped onto the dedicated carry chain
//! instead of ripple logic built from LUTs. This module holds the functional
//! model of the cell (shared by gate-level simulation and AIG conversion) and
//! the mapping from LIR arithmetic to a chain of cells.
//!
//! # Cell model (`INJECT = "NO"`)
//!
//! A `CCU2` has two halves. Half `i` has a LUT4 `P = INITi(Ai, Bi, Ci, Di)`
//! (propagate) and a LUT2 `G = INITi[3:0](Ai, Bi)` (generate):
//!
//! ```text
//! S_i     = P_i ^ c_i
//! c_(i+1) = P_i ? c_i : G_i        c_0 = CIN, COUT = c_2
//! ```
//!
//! # Mapping
//!
//! Every bit of an arithmetic operation computes `x + y + c` where `x` and `y`
//! are the operand bits, each possibly inverted (`a - b = a + ~b + 1`). With
//! `Ci = 0` and `Di = 1` the LUT4 reads nibble 2 of INIT, which holds
//! `P = x ^ y`; nibble 0 (the LUT2) holds `G = x` (when `P = 0`, `x == y` and
//! the carry out is `x & y = x`). Constant operand bits are folded into the
//! truth tables.
//!
//! A chain starts with a half whose `P = 0` and `G` = the carry-in constant,
//! so the chain never depends on the value at the first cell's `CIN` pin.
//! A carry out (adder carry, subtractor borrow, comparator result) leaves the
//! chain through an extra half with `P` = 0 or 1, whose sum output is `c` or
//! `!c`: the dedicated `COUT` → `CIN` path cannot reach general routing.

use crate::gate_netlist::{Cell, CellFailureMode, CellId, GateNet, GateNetId, GateNetlist};
use crate::lir::LirOp;
use crate::tech_library::{CellFunction, TechLibrary};

/// Narrowest operation mapped onto a carry chain. Below this, LUT logic is as
/// fast and avoids the chain's fixed half-cell overhead.
pub const CARRY_CHAIN_MIN_WIDTH: u32 = 4;

/// Evaluate a `CCU2` cell.
///
/// `inputs` are `[A0, B0, C0, D0, A1, B1, C1, D1, CIN]` (missing pins read 0);
/// the result is `[S0, S1, COUT]`.
pub fn ccu2_eval(init0: u16, init1: u16, inputs: &[bool]) -> [bool; 3] {
    let pin = |i: usize| inputs.get(i).copied().unwrap_or(false);
    let half = |init: u16, a: bool, b: bool, c: bool, d: bool, cin: bool| -> (bool, bool) {
        let idx = (a as u16) | ((b as u16) << 1) | ((c as u16) << 2) | ((d as u16) << 3);
        let p = (init >> idx) & 1 == 1;
        let g = (init >> ((a as u16) | ((b as u16) << 1))) & 1 == 1;
        (p ^ cin, if p { cin } else { g })
    };
    let (s0, c1) = half(init0, pin(0), pin(1), pin(2), pin(3), pin(8));
    let (s1, c2) = half(init1, pin(4), pin(5), pin(6), pin(7), c1);
    [s0, s1, c2]
}

/// Parse a Lattice INIT parameter value (`"0x96AA"`, `"96AA"` or `"16'h96AA"`).
pub fn parse_init(value: &str) -> Option<u16> {
    let v = value.trim().trim_matches('"');
    let hex = v
        .strip_prefix("0x")
        .or_else(|| v.strip_prefix("0X"))
        .or_else(|| v.split_once("'h").map(|(_, h)| h))
        .or_else(|| v.split_once("'H").map(|(_, h)| h))
        .unwrap_or(v);
    u16::from_str_radix(hex, 16).ok()
}

/// The `(INIT0, INIT1)` truth tables of a `CCU2` cell, from its parameters.
pub fn ccu2_inits(cell: &Cell) -> Option<(u16, u16)> {
    let init0 = parse_init(cell.parameters.get("INIT0")?)?;
    let init1 = parse_init(cell.parameters.get("INIT1")?)?;
    Some((init0, init1))
}

/// Format a truth table as a Lattice INIT parameter value.
pub fn format_init(init: u16) -> String {
    format!("0x{:04X}", init)
}

/// Whether `op` is arithmetic that maps onto a carry chain at `min_width`.
pub fn is_carry_chain_op(op: &LirOp, min_width: u32) -> bool {
    match op {
        LirOp::Add { width, .. }
        | LirOp::Sub { width, .. }
        | LirOp::Lt { width }
        | LirOp::Le { width }
        | LirOp::Gt { width }
        | LirOp::Ge { width }
        | LirOp::Slt { width }
        | LirOp::Sle { width }
        | LirOp::Sgt { width }
        | LirOp::Sge { width } => *width >= min_width,
        _ => false,
    }
}

/// One operand bit: a net, or a constant folded into the truth table.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperandBit {
    /// Driven by a net
    Net(GateNetId),
    /// Known constant
    Const(bool),
}

/// An operand: its nets (LSB first) and, if it is a constant, its value.
#[derive(Debug, Clone)]
pub struct Operand {
    /// Nets of the operand signal
    pub nets: Vec<GateNetId>,
    /// Value when the operand is a constant (bits at and above 64 are 0)
    pub constant: Option<u64>,
}

impl Operand {
    /// Bit `i`, zero-extended past the operand width.
    pub fn bit(&self, i: u32) -> OperandBit {
        if let Some(value) = self.constant {
            return OperandBit::Const(i < 64 && (value >> i) & 1 == 1);
        }
        match self.nets.get(i as usize) {
            Some(&net) => OperandBit::Net(net),
            None => OperandBit::Const(false),
        }
    }
}

/// One half of a `CCU2`.
#[derive(Debug, Clone)]
struct Half {
    /// Net on the A pin (None: tied low)
    a: Option<GateNetId>,
    /// Net on the B pin (None: tied low)
    b: Option<GateNetId>,
    /// Truth table (C is tied low and D high, so nibble 2 is the LUT4 output)
    init: u16,
    /// Net driven by the half's sum output (None: a fresh unused net)
    sum: Option<GateNetId>,
}

/// Truth table from `p(A, B)` (propagate, nibbles 1–3) and `g(A, B)` (generate, nibble 0).
fn truth_table(p: impl Fn(bool, bool) -> bool, g: impl Fn(bool, bool) -> bool) -> u16 {
    let mut init = 0u16;
    for idx in 0..16u16 {
        let a = idx & 1 == 1;
        let b = (idx >> 1) & 1 == 1;
        let low_nibble = idx >> 2 == 0;
        let v = if low_nibble { g(a, b) } else { p(a, b) };
        if v {
            init |= 1 << idx;
        }
    }
    init
}

/// The half for one bit of `x + y + c`, with `x = a ^ invert_a`, `y = b ^ invert_b`.
fn sum_half(
    a: OperandBit,
    invert_a: bool,
    b: OperandBit,
    invert_b: bool,
    sum: Option<GateNetId>,
) -> Half {
    let (a_net, a_const) = match a {
        OperandBit::Net(n) => (Some(n), None),
        OperandBit::Const(v) => (None, Some(v)),
    };
    let (b_net, b_const) = match b {
        OperandBit::Net(n) => (Some(n), None),
        OperandBit::Const(v) => (None, Some(v)),
    };
    let x = move |pin_a: bool| a_const.unwrap_or(pin_a) ^ invert_a;
    let y = move |pin_b: bool| b_const.unwrap_or(pin_b) ^ invert_b;
    Half {
        a: a_net,
        b: b_net,
        init: truth_table(|pa, pb| x(pa) ^ y(pb), |pa, _| x(pa)),
        sum,
    }
}

/// The first half of a chain: forces the carry to `cin`.
fn carry_in_half(cin: bool) -> Half {
    Half {
        a: None,
        b: None,
        init: truth_table(|_, _| false, |_, _| cin),
        sum: None,
    }
}

/// A half that brings the incoming carry out to general routing: `sum = c ^ invert`.
fn carry_out_half(invert: bool, sum: GateNetId) -> Half {
    Half {
        a: None,
        b: None,
        init: truth_table(|_, _| invert, |_, _| false),
        sum: Some(sum),
    }
}

/// Map a carry-chain LIR operation (see [`is_carry_chain_op`]) to `CCU2` cells.
///
/// `operands` are the op's two inputs; `outputs` the nets of its result. Result
/// bits the operation does not compute are driven low, as in the AIG lowering.
/// Returns false (and adds nothing) when the library has no `CCU2` cell or the
/// op is not a carry-chain operation.
pub fn map_carry_chain(
    netlist: &mut GateNetlist,
    library: &TechLibrary,
    op: &LirOp,
    operands: &[Operand],
    outputs: &[GateNetId],
    path: &str,
) -> bool {
    let Some(lib_cell) = library.find_best_cell(&CellFunction::Ccu2) else {
        return false;
    };
    if operands.len() < 2 {
        return false;
    }
    let (a, b) = (&operands[0], &operands[1]);

    /// Each bit computes `x ^ invert_x` + `y ^ invert_y` + carry, starting from `cin`.
    /// `signed` flips both MSBs (a signed compare as an unsigned one);
    /// `carry_out = Some((invert, bit))` drives output `bit` with the final carry.
    struct Plan<'o> {
        width: u32,
        x: &'o Operand,
        y: &'o Operand,
        invert_y: bool,
        invert_x: bool,
        cin: bool,
        signed: bool,
        sums_to_outputs: bool,
        carry_out: Option<(bool, usize)>,
    }
    let compare = |width: u32, swap: bool, invert: bool, signed: bool| Plan {
        width,
        // a < b  ⇔  no carry out of a + ~b + 1
        x: if swap { b } else { a },
        y: if swap { a } else { b },
        invert_x: false,
        invert_y: true,
        cin: true,
        signed,
        sums_to_outputs: false,
        carry_out: Some((invert, 0)),
    };
    let plan = match op {
        LirOp::Add {
            width, has_carry, ..
        } => Plan {
            width: *width,
            x: a,
            y: b,
            invert_x: false,
            invert_y: false,
            cin: false,
            signed: false,
            sums_to_outputs: true,
            carry_out: (*has_carry && outputs.len() > *width as usize)
                .then_some((false, *width as usize)),
        },
        LirOp::Sub { width, has_borrow } => Plan {
            width: *width,
            x: a,
            y: b,
            invert_x: false,
            invert_y: true,
            cin: true,
            signed: false,
            sums_to_outputs: true,
            // borrow = !carry
            carry_out: (*has_borrow && outputs.len() > *width as usize)
                .then_some((true, *width as usize)),
        },
        LirOp::Lt { width } => compare(*width, false, true, false),
        LirOp::Ge { width } => compare(*width, false, false, false),
        LirOp::Gt { width } => compare(*width, true, true, false),
        LirOp::Le { width } => compare(*width, true, false, false),
        LirOp::Slt { width } => compare(*width, false, true, true),
        LirOp::Sge { width } => compare(*width, false, false, true),
        LirOp::Sgt { width } => compare(*width, true, true, true),
        LirOp::Sle { width } => compare(*width, true, false, true),
        _ => return false,
    };
    if plan.width == 0 {
        return false;
    }

    let mut halves = vec![carry_in_half(plan.cin)];
    for i in 0..plan.width {
        // Signed compare: flipping both MSBs turns it into an unsigned compare.
        let msb_flip = plan.signed && i == plan.width - 1;
        let sum = if plan.sums_to_outputs {
            outputs.get(i as usize).copied()
        } else {
            None
        };
        halves.push(sum_half(
            plan.x.bit(i),
            plan.invert_x ^ msb_flip,
            plan.y.bit(i),
            plan.invert_y ^ msb_flip,
            sum,
        ));
    }
    let mut driven: Vec<usize> = if plan.sums_to_outputs {
        (0..(plan.width as usize).min(outputs.len())).collect()
    } else {
        Vec::new()
    };
    if let Some((invert, bit)) = plan.carry_out {
        if let Some(&net) = outputs.get(bit) {
            halves.push(carry_out_half(invert, net));
            driven.push(bit);
        }
    }

    // Shared constant nets
    let tie_low = get_or_create_tie(netlist, false);
    let tie_high = get_or_create_tie(netlist, true);

    let failure_modes: Vec<_> = lib_cell
        .failure_modes
        .iter()
        .map(|fm| CellFailureMode {
            name: fm.name.clone(),
            fit: fm.fit,
            fault_type: fm.fault_type,
        })
        .collect();

    let mut carry = tie_low;
    for (cell_idx, pair) in halves.chunks(2).enumerate() {
        let lo = &pair[0];
        let unused = Half {
            a: None,
            b: None,
            init: 0,
            sum: None,
        };
        let hi = pair.get(1).unwrap_or(&unused);
        let new_net = |netlist: &mut GateNetlist, suffix: &str| {
            netlist.add_net(GateNet::new(
                GateNetId(0),
                format!("{}.ccu2_{}.{}", path, cell_idx, suffix),
            ))
        };
        let s0 = lo.sum.unwrap_or_else(|| new_net(netlist, "s0"));
        let s1 = hi.sum.unwrap_or_else(|| new_net(netlist, "s1"));
        let cout = new_net(netlist, "cout");
        let inputs = vec![
            lo.a.unwrap_or(tie_low),
            lo.b.unwrap_or(tie_low),
            tie_low,
            tie_high,
            hi.a.unwrap_or(tie_low),
            hi.b.unwrap_or(tie_low),
            tie_low,
            tie_high,
            carry,
        ];
        let mut cell = Cell::new_comb(
            CellId(0),
            lib_cell.name.clone(),
            library.name.clone(),
            lib_cell.fit,
            format!("{}.ccu2_{}", path, cell_idx),
            inputs,
            vec![s0, s1, cout],
        );
        cell.function = Some(CellFunction::Ccu2);
        cell.failure_modes = failure_modes.clone();
        cell.source_op = Some(format!("{:?}", std::mem::discriminant(op)));
        cell.parameters
            .insert("INIT0".to_string(), format_init(lo.init));
        cell.parameters
            .insert("INIT1".to_string(), format_init(hi.init));
        cell.parameters
            .insert("INJECT".to_string(), "NO".to_string());
        netlist.add_cell(cell);
        carry = cout;
    }

    // Result bits the operation does not produce read as 0.
    for (bit, &net) in outputs.iter().enumerate() {
        if !driven.contains(&bit) {
            drive_low(netlist, library, net, tie_low, path, bit);
        }
    }
    true
}

/// The netlist's shared constant net (`tie_low` / `tie_high`), created on first use.
fn get_or_create_tie(netlist: &mut GateNetlist, high: bool) -> GateNetId {
    let (name, alt) = if high {
        ("tie_high", "vdd")
    } else {
        ("tie_low", "gnd")
    };
    if let Some(id) = netlist.get_net_id(name).or_else(|| netlist.get_net_id(alt)) {
        return id;
    }
    netlist.add_tie_cell(name, high as u64);
    netlist
        .get_net_id(name)
        .expect("add_tie_cell creates the named net")
}

/// Drive `net` from the constant-low net through a buffer.
fn drive_low(
    netlist: &mut GateNetlist,
    library: &TechLibrary,
    net: GateNetId,
    tie_low: GateNetId,
    path: &str,
    bit: usize,
) {
    let (name, fit) = library
        .find_best_cell(&CellFunction::Buf)
        .map(|c| (c.name.clone(), c.fit))
        .unwrap_or_else(|| ("BUF".to_string(), 0.1));
    let mut cell = Cell::new_comb(
        CellId(0),
        name,
        library.name.clone(),
        fit,
        format!("{}.zero{}", path, bit),
        vec![tie_low],
        vec![net],
    );
    cell.function = Some(CellFunction::Buf);
    netlist.add_cell(cell);
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Evaluate a chain of halves bit-serially with the cell model.
    fn run_chain(halves: &[(u16, bool, bool)]) -> Vec<bool> {
        // (init, A, B) per half; C = 0, D = 1
        let mut carry = false;
        let mut sums = Vec::new();
        for pair in halves.chunks(2) {
            let lo = pair[0];
            let hi = pair.get(1).copied().unwrap_or((0, false, false));
            let out = ccu2_eval(
                lo.0,
                hi.0,
                &[lo.1, lo.2, false, true, hi.1, hi.2, false, true, carry],
            );
            sums.push(out[0]);
            sums.push(out[1]);
            carry = out[2];
        }
        sums
    }

    fn plan_add(a: u64, b: u64, width: u32, sub: bool) -> (u64, bool) {
        let mut halves = vec![(carry_in_half(sub).init, false, false)];
        for i in 0..width {
            let h = sum_half(
                OperandBit::Net(GateNetId(0)),
                false,
                OperandBit::Net(GateNetId(0)),
                sub,
                None,
            );
            halves.push((h.init, (a >> i) & 1 == 1, (b >> i) & 1 == 1));
        }
        halves.push((carry_out_half(false, GateNetId(0)).init, false, false));
        let sums = run_chain(&halves);
        let mut result = 0;
        for i in 0..width as usize {
            if sums[i + 1] {
                result |= 1 << i;
            }
        }
        (result, sums[width as usize + 1])
    }

    #[test]
    fn ccu2_adds_and_subtracts_exhaustively_at_5_bits() {
        let mask = (1u64 << 5) - 1;
        for a in 0..32u64 {
            for b in 0..32u64 {
                let (sum, carry) = plan_add(a, b, 5, false);
                assert_eq!(sum, (a + b) & mask, "{a} + {b}");
                assert_eq!(carry, a + b > mask, "carry of {a} + {b}");
                let (diff, no_borrow) = plan_add(a, b, 5, true);
                assert_eq!(diff, a.wrapping_sub(b) & mask, "{a} - {b}");
                assert_eq!(no_borrow, a >= b, "borrow of {a} - {b}");
            }
        }
    }

    #[test]
    fn constant_operand_is_folded_into_the_truth_table() {
        for k in [false, true] {
            let h = sum_half(
                OperandBit::Net(GateNetId(0)),
                false,
                OperandBit::Const(k),
                false,
                None,
            );
            assert!(h.b.is_none(), "a constant bit needs no B net");
            for a in [false, true] {
                for cin in [false, true] {
                    let out = ccu2_eval(
                        h.init,
                        0,
                        &[a, false, false, true, false, false, false, true, cin],
                    );
                    assert_eq!(out[0], a ^ k ^ cin);
                }
            }
        }
    }

    #[test]
    fn yosys_add_init_matches_the_model() {
        // Yosys's nexus/ecp5 arith map uses INIT 0x96AA with C = 0, D = 1: P = A ^ B, G = A.
        for a in [false, true] {
            for b in [false, true] {
                for cin in [false, true] {
                    // The high half has P = 0, so S1 shows the low half's carry out.
                    let out = ccu2_eval(
                        0x96AA,
                        0x0000,
                        &[a, b, false, true, false, false, false, true, cin],
                    );
                    assert_eq!(out[0], a ^ b ^ cin);
                    assert_eq!(out[1], (a & b) | (cin & (a ^ b)));
                }
            }
        }
    }

    #[test]
    fn parse_init_accepts_lattice_forms() {
        assert_eq!(parse_init("0x96AA"), Some(0x96AA));
        assert_eq!(parse_init("96aa"), Some(0x96AA));
        assert_eq!(parse_init("16'h96AA"), Some(0x96AA));
        assert_eq!(parse_init("\"0x0001\""), Some(1));
        assert_eq!(parse_init("zz"), None);
    }
}
