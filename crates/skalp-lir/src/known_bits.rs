//! Known-zero upper bits of LIR signals.
//!
//! A word-level operation often sees operands whose upper bits are constant
//! zero: `{24'b0, x} * {24'b0, y}` is a 48-bit multiply whose operands only
//! have 24 significant bits. [`LeadingZeros`] finds how many of a signal's top
//! bits are known to be zero, so that mapping can size hard blocks (DSP
//! multipliers) by the significant width instead of the declared one.
//!
//! The analysis is conservative: an unknown driver (a primary input, a
//! register, a physical block) has no known-zero bits.

use crate::lir::{Lir, LirOp, LirSignalId};
use std::cell::RefCell;
use std::collections::HashMap;

/// Deepest chain of drivers followed before giving up (returns "unknown").
const MAX_DEPTH: u32 = 64;

/// Memoized known-leading-zero analysis over one [`Lir`].
pub struct LeadingZeros<'a> {
    lir: &'a Lir,
    /// Signal id -> index of the node driving it.
    driver: HashMap<u32, usize>,
    memo: RefCell<HashMap<u32, u32>>,
}

impl<'a> LeadingZeros<'a> {
    pub fn new(lir: &'a Lir) -> Self {
        let driver = lir
            .nodes
            .iter()
            .enumerate()
            .map(|(idx, node)| (node.output.0, idx))
            .collect();
        Self {
            lir,
            driver,
            memo: RefCell::new(HashMap::new()),
        }
    }

    /// Number of top bits of `sig` known to be zero (at most its width).
    pub fn of(&self, sig: LirSignalId) -> u32 {
        self.at_depth(sig, 0)
    }

    /// Bits of `sig` that may be non-zero: its width minus the known-zero
    /// top bits.
    pub fn significant_width(&self, sig: LirSignalId) -> u32 {
        self.width(sig) - self.of(sig)
    }

    fn width(&self, sig: LirSignalId) -> u32 {
        self.lir
            .signals
            .get(sig.0 as usize)
            .map(|s| s.width)
            .unwrap_or(0)
    }

    fn at_depth(&self, sig: LirSignalId, depth: u32) -> u32 {
        if let Some(&known) = self.memo.borrow().get(&sig.0) {
            return known;
        }
        if depth > MAX_DEPTH {
            return 0;
        }
        let width = self.width(sig);
        let known = self.from_driver(sig, depth).min(width);
        self.memo.borrow_mut().insert(sig.0, known);
        known
    }

    /// Known-zero top bits of `sig`, computed from its driving node.
    fn from_driver(&self, sig: LirSignalId, depth: u32) -> u32 {
        let width = self.width(sig);
        let Some(&idx) = self.driver.get(&sig.0) else {
            return 0;
        };
        let node = &self.lir.nodes[idx];
        let input = |i: usize| node.inputs.get(i).copied();
        // Known-zero top bits of input `i`, re-expressed against this
        // signal's width (an input narrower than the output is implicitly
        // zero-extended; a wider one is truncated).
        let input_lz = |i: usize| -> u32 {
            let Some(inp) = input(i) else { return 0 };
            let in_width = self.width(inp);
            let lz = self.at_depth(inp, depth + 1);
            let significant = in_width - lz;
            width.saturating_sub(significant)
        };

        match &node.op {
            LirOp::Constant { value, .. } => width.saturating_sub(64 - value.leading_zeros()),

            LirOp::Buf { .. } | LirOp::Buffer { .. } => input_lz(0),

            // The first operand is the most significant.
            LirOp::Concat { widths } => {
                let mut known = 0;
                for (i, &w) in widths.iter().enumerate() {
                    let Some(inp) = input(i) else { break };
                    let in_width = self.width(inp);
                    let lz = self.at_depth(inp, depth + 1);
                    // Part bits above the operand's own width read as zero.
                    let part_lz = (w.saturating_sub(in_width) + lz).min(w);
                    known += part_lz;
                    if part_lz < w {
                        break;
                    }
                }
                // Parts may sum to more or less than this signal's width.
                let total: u32 = widths.iter().sum();
                if total >= width {
                    known.saturating_sub(total - width)
                } else {
                    known + (width - total)
                }
            }

            LirOp::ZeroExtend { .. } => input_lz(0),

            LirOp::And { .. } => input_lz(0).max(input_lz(1)),

            LirOp::Or { .. } | LirOp::Xor { .. } => input_lz(0).min(input_lz(1)),

            // Inputs: [sel, else, then]
            LirOp::Mux2 { .. } => input_lz(1).min(input_lz(2)),

            LirOp::RangeSelect { high, low, .. } => {
                let Some(inp) = input(0) else { return 0 };
                let in_width = self.width(inp);
                let significant = in_width - self.at_depth(inp, depth + 1);
                // Selected bits at or above `significant` are zero.
                let selected = high + 1 - low;
                let zero_from = significant.max(*low).min(high + 1);
                let zero_selected = high + 1 - zero_from;
                width.saturating_sub(selected) + zero_selected.min(selected)
            }

            // Shifting right by a constant clears its top bits.
            LirOp::Shr { .. } => {
                let shift = input(1).and_then(|s| self.constant_value(s));
                match shift {
                    Some(amount) => {
                        let amount = amount.min(u64::from(width)) as u32;
                        (input_lz(0) + amount).min(width)
                    }
                    None => input_lz(0),
                }
            }

            _ => 0,
        }
    }

    /// The value of a constant-driven signal.
    fn constant_value(&self, sig: LirSignalId) -> Option<u64> {
        let &idx = self.driver.get(&sig.0)?;
        match self.lir.nodes[idx].op {
            LirOp::Constant { value, .. } => Some(value),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn constant(lir: &mut Lir, width: u32, value: u64) -> LirSignalId {
        let out = lir.add_signal(format!("c{value}_{width}"), width);
        lir.add_node(LirOp::Constant { width, value }, vec![], out, "c".into());
        out
    }

    #[test]
    fn concat_of_zero_constant_and_input() {
        let mut lir = Lir::new("t".into());
        let x = lir.add_input("x".into(), 24);
        let zero = constant(&mut lir, 24, 0);
        let wide = lir.add_signal("wide".into(), 48);
        lir.add_node(
            LirOp::Concat {
                widths: vec![24, 24],
            },
            vec![zero, x],
            wide,
            "cat".into(),
        );
        let lz = LeadingZeros::new(&lir);
        assert_eq!(lz.of(wide), 24);
        assert_eq!(lz.significant_width(wide), 24);
        assert_eq!(lz.of(x), 0);
    }

    #[test]
    fn constant_buffer_and_mask() {
        let mut lir = Lir::new("t".into());
        let x = lir.add_input("x".into(), 32);
        let mask = constant(&mut lir, 32, 0xFF);
        let masked = lir.add_signal("masked".into(), 32);
        lir.add_node(
            LirOp::And { width: 32 },
            vec![x, mask],
            masked,
            "and".into(),
        );
        let buffered = lir.add_signal("buffered".into(), 32);
        lir.add_node(
            LirOp::Buffer { width: 32 },
            vec![masked],
            buffered,
            "buf".into(),
        );
        let lz = LeadingZeros::new(&lir);
        assert_eq!(lz.of(mask), 24);
        assert_eq!(lz.of(buffered), 24);
    }

    #[test]
    fn zero_extend_and_range_select() {
        let mut lir = Lir::new("t".into());
        let x = lir.add_input("x".into(), 10);
        let ext = lir.add_signal("ext".into(), 40);
        lir.add_node(
            LirOp::ZeroExtend { from: 10, to: 40 },
            vec![x],
            ext,
            "zext".into(),
        );
        // ext[19:4]: bits 10..19 are zero, so the top 10 of 16 bits are.
        let slice = lir.add_signal("slice".into(), 16);
        lir.add_node(
            LirOp::RangeSelect {
                width: 40,
                high: 19,
                low: 4,
            },
            vec![ext],
            slice,
            "sel".into(),
        );
        let lz = LeadingZeros::new(&lir);
        assert_eq!(lz.of(ext), 30);
        assert_eq!(lz.of(slice), 10);
    }

    #[test]
    fn concat_stops_at_first_unknown_part() {
        let mut lir = Lir::new("t".into());
        let x = lir.add_input("x".into(), 8);
        let y = lir.add_input("y".into(), 8);
        let zero = constant(&mut lir, 4, 0);
        let cat = lir.add_signal("cat".into(), 20);
        lir.add_node(
            LirOp::Concat {
                widths: vec![4, 8, 8],
            },
            vec![zero, x, y],
            cat,
            "cat".into(),
        );
        let lz = LeadingZeros::new(&lir);
        assert_eq!(lz.of(cat), 4);
    }
}
