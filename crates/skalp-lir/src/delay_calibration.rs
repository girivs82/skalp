//! Delay calibration: a device's delays on silicon as factors of one of its
//! model's corners, by routing switch class and by cell type.
//!
//! A vendor's fast and slow corners bracket silicon, but where a part sits
//! between them differs by kind of delay: on one device measured with ring
//! oscillators, logic sat near the slow corner, short and long wires well
//! below it, and medium wires at it. A calibration file gives each kind its
//! own factor, fitted from rings measured on the chip, and
//! [`DelayAnnotation::calibrated`] turns a routed design's delays at one
//! corner into delays at the silicon so measured. A device database can
//! hold one for its family (place and route then keeps it with the build),
//! or a file can be given. It reads JSON, or TOML such as:
//!
//! ```toml
//! # the corner the factors scale
//! corner = "slow"
//! # routing switches by delay class: the first pattern that matches wins
//! wires = [
//!     { class = "*span6*", factor = 0.76 },
//!     { class = "*", factor = 0.8 },
//! ]
//! # each cell type's own delays (arcs, clock to out, setup, hold)
//! cells = [
//!     { cell = "CCU2", between = 0.49 },
//!     { cell = "*", factor = 0.54 },
//! ]
//! # the clock wires
//! clock = { between = 0.95 }
//! ```
//!
//! `factor = F` scales the corner's delay by F; `between = K` puts a delay
//! the fraction K of the way from the `from` corner (default `fast`) to the
//! calibrated one, for delays fitted that way. A wire's switches are scaled
//! class by class, as place and route split the wire's delay by class
//! ([`CellDelays::wire_classes`]); a wire without that split is scaled as
//! the class `""`. Wire classes take factors only. A delay nothing matches
//! keeps the corner's value.

use crate::gate_netlist::GateNetlist;
use crate::glob::glob_matches;
use crate::timed_sim::{CellDelays, DelayAnnotation};
use serde::{Deserialize, Serialize};

/// Silicon's delays as factors of one corner's (see the module docs)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct DelayCalibration {
    /// The corner the factors scale
    pub corner: String,
    /// The corner `between` fractions start from
    #[serde(default = "fast_corner")]
    pub from: String,
    /// Routing switches by delay class; the first matching pattern wins
    #[serde(default)]
    pub wires: Vec<WireScale>,
    /// Cells' own delays by cell type; the first matching pattern wins
    #[serde(default)]
    pub cells: Vec<CellScale>,
    /// The clock wires
    #[serde(default)]
    pub clock: Option<Scale>,
    /// What the calibration is, for people
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub comment: Option<String>,
    /// How and on what it was measured, for people
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub measured: Option<serde_json::Value>,
}

fn fast_corner() -> String {
    "fast".to_string()
}

/// The factor of the routing switches of the delay classes `class` matches
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct WireScale {
    /// A pattern of delay classes, with `*` wildcards
    pub class: String,
    /// The factor of the corner's delay
    pub factor: f64,
}

/// How the own delays of the cell types `cell` matches are scaled
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct CellScale {
    /// A pattern of cell types, with `*` wildcards
    pub cell: String,
    /// The factor of the corner's delay
    #[serde(default)]
    pub factor: Option<f64>,
    /// The fraction of the way from the `from` corner to this one
    #[serde(default)]
    pub between: Option<f64>,
}

/// A factor of the corner's delay or a fraction of the way to it
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Scale {
    /// The factor of the corner's delay
    #[serde(default)]
    pub factor: Option<f64>,
    /// The fraction of the way from the `from` corner to this one
    #[serde(default)]
    pub between: Option<f64>,
}

/// A checked [`Scale`]
#[derive(Debug, Clone, Copy, PartialEq)]
enum Rule {
    Factor(f64),
    Between(f64),
}

impl Rule {
    fn new(factor: Option<f64>, between: Option<f64>, what: &str) -> Result<Self, String> {
        match (factor, between) {
            (Some(f), None) if f.is_finite() && f >= 0.0 => Ok(Rule::Factor(f)),
            (None, Some(k)) if k.is_finite() => Ok(Rule::Between(k)),
            (Some(_), None) | (None, Some(_)) => Err(format!(
                "{what}: the factor or fraction is not a usable number"
            )),
            _ => Err(format!("{what}: give one of factor and between")),
        }
    }

    /// The delay `at` the calibrated corner becomes; `from` is the same
    /// delay at the `from` corner
    fn apply(self, at: i64, from: Option<i64>) -> i64 {
        match self {
            Rule::Factor(f) => (at as f64 * f).round() as i64,
            Rule::Between(k) => {
                let from = from.unwrap_or(at);
                from + ((at - from) as f64 * k).round() as i64
            }
        }
    }
}

impl DelayCalibration {
    /// Read a calibration in TOML
    pub fn from_toml(text: &str) -> Result<Self, String> {
        toml::from_str::<Self>(text)
            .map_err(|e| e.to_string())?
            .checked()
    }

    /// Read a calibration in JSON
    pub fn from_json(text: &str) -> Result<Self, String> {
        serde_json::from_str::<Self>(text)
            .map_err(|e| e.to_string())?
            .checked()
    }

    /// Check every rule gives one usable number
    fn checked(self) -> Result<Self, String> {
        for wire in &self.wires {
            Rule::new(
                Some(wire.factor),
                None,
                &format!("wire class {}", wire.class),
            )?;
        }
        for cell in &self.cells {
            Rule::new(cell.factor, cell.between, &format!("cell {}", cell.cell))?;
        }
        if let Some(clock) = self.clock {
            Rule::new(clock.factor, clock.between, "clock")?;
        }
        Ok(self)
    }

    /// Whether any rule is a fraction from the `from` corner, which then
    /// must be given
    pub fn needs_from_corner(&self) -> bool {
        self.cells.iter().any(|c| c.between.is_some())
            || self.clock.is_some_and(|c| c.between.is_some())
    }

    fn wire_factor(&self, class: &str) -> f64 {
        self.wires
            .iter()
            .find(|w| glob_matches(&w.class, class))
            .map_or(1.0, |w| w.factor)
    }

    fn cell_rule(&self, cell_type: &str) -> Option<Rule> {
        self.cells
            .iter()
            .find(|c| glob_matches(&c.cell, cell_type))
            .and_then(|c| Rule::new(c.factor, c.between, &c.cell).ok())
    }
}

/// What a calibration did to a design's delays
#[derive(Debug, Clone, Default, PartialEq)]
pub struct CalibrationReport {
    /// Delay classes no wire pattern matched, kept at the corner's delay
    pub unmatched_classes: Vec<String>,
    /// Cell types no cell pattern matched, kept at the corner's delay
    pub unmatched_cells: Vec<String>,
    /// Pins whose wire has no split by class (scaled as the class `""`)
    pub unsplit_wires: usize,
}

impl DelayAnnotation {
    /// The delays of `netlist` at `corner` (this calibration's corner)
    /// scaled as `calibration` gives (see [`crate::delay_calibration`]);
    /// `from` is its `from` corner, needed where a rule is a fraction from
    /// it.
    pub fn calibrated(
        corner: &Self,
        from: Option<&Self>,
        calibration: &DelayCalibration,
        netlist: &GateNetlist,
    ) -> Result<(Self, CalibrationReport), String> {
        if corner.cells.len() != netlist.cells.len() {
            return Err(format!(
                "the {} corner has {} cells but the netlist has {}",
                corner.corner,
                corner.cells.len(),
                netlist.cells.len()
            ));
        }
        if let Some(from) = from {
            if from.cells.len() != corner.cells.len() {
                return Err(format!(
                    "the {} and {} corners have {} and {} cells",
                    from.corner,
                    corner.corner,
                    from.cells.len(),
                    corner.cells.len()
                ));
            }
        } else if calibration.needs_from_corner() {
            return Err(format!(
                "the calibration has fractions from the {} corner, which the build lacks",
                calibration.from
            ));
        }
        let class_factor: Vec<f64> = corner
            .classes
            .iter()
            .map(|c| calibration.wire_factor(c))
            .collect();
        let mut report = CalibrationReport {
            unmatched_classes: corner
                .classes
                .iter()
                .filter(|c| !calibration.wires.iter().any(|w| glob_matches(&w.class, c)))
                .cloned()
                .collect(),
            ..Default::default()
        };
        let unsplit_factor = calibration.wire_factor("");
        let clock = calibration
            .clock
            .map(|c| Rule::new(c.factor, c.between, "clock"))
            .transpose()?;
        let scale = |rule: Option<Rule>, at: i64, from: Option<i64>| match rule {
            Some(rule) => rule.apply(at, from),
            None => at,
        };
        let mut cells = Vec::with_capacity(corner.cells.len());
        for (index, (delays, cell)) in corner.cells.iter().zip(&netlist.cells).enumerate() {
            let from_cell = from.and_then(|f| f.cells.get(index));
            let own = calibration.cell_rule(&cell.cell_type);
            if own.is_none() && !report.unmatched_cells.contains(&cell.cell_type) {
                report.unmatched_cells.push(cell.cell_type.clone());
            }
            let wire_ps = delays
                .wire_ps
                .iter()
                .enumerate()
                .map(|(pin, &ps)| match delays.wire_classes.get(pin) {
                    Some(split) if !split.is_empty() => split
                        .iter()
                        .map(|&(class, ps)| {
                            let factor = class_factor.get(class as usize).copied().unwrap_or(1.0);
                            (ps as f64 * factor).round() as i64
                        })
                        .sum(),
                    _ => {
                        if ps != 0 {
                            report.unsplit_wires += 1;
                        }
                        (ps as f64 * unsplit_factor).round() as i64
                    }
                })
                .collect();
            let from_at = |pick: &dyn Fn(&CellDelays) -> Option<i64>| from_cell.and_then(pick);
            cells.push(CellDelays {
                wire_ps,
                wire_classes: Vec::new(),
                clock_wire_ps: scale(
                    clock,
                    delays.clock_wire_ps,
                    from_at(&|c| Some(c.clock_wire_ps)),
                ),
                arcs: delays
                    .arcs
                    .iter()
                    .enumerate()
                    .map(|(input, outputs)| {
                        outputs
                            .iter()
                            .enumerate()
                            .map(|(output, arc)| {
                                arc.map(|ps| {
                                    scale(
                                        own,
                                        ps,
                                        from_at(&|c| {
                                            c.arcs.get(input)?.get(output).copied().flatten()
                                        }),
                                    )
                                })
                            })
                            .collect()
                    })
                    .collect(),
                clock_to_out_ps: scale(
                    own,
                    delays.clock_to_out_ps,
                    from_at(&|c| Some(c.clock_to_out_ps)),
                ),
                launches: delays.launches.clone(),
                setup_ps: delays
                    .setup_ps
                    .iter()
                    .enumerate()
                    .map(|(pin, &ps)| scale(own, ps, from_at(&|c| c.setup_ps.get(pin).copied())))
                    .collect(),
                hold_ps: delays
                    .hold_ps
                    .iter()
                    .enumerate()
                    .map(|(pin, &ps)| scale(own, ps, from_at(&|c| c.hold_ps.get(pin).copied())))
                    .collect(),
            });
        }
        report.unmatched_classes.sort();
        report.unmatched_cells.sort();
        Ok((
            Self {
                corner: format!("{} calibrated", corner.corner),
                cells,
                classes: Vec::new(),
            },
            report,
        ))
    }

    /// Every delay multiplied by `factor`, as when the whole die runs
    /// slower by that much (supply droop under load)
    pub fn derated(&self, factor: f64) -> Self {
        let by = |ps: i64| (ps as f64 * factor).round() as i64;
        Self {
            corner: format!("{} derated by {factor}", self.corner),
            cells: self
                .cells
                .iter()
                .map(|c| CellDelays {
                    wire_ps: c.wire_ps.iter().map(|&ps| by(ps)).collect(),
                    wire_classes: c
                        .wire_classes
                        .iter()
                        .map(|split| split.iter().map(|&(class, ps)| (class, by(ps))).collect())
                        .collect(),
                    clock_wire_ps: by(c.clock_wire_ps),
                    arcs: c
                        .arcs
                        .iter()
                        .map(|outputs| outputs.iter().map(|arc| arc.map(by)).collect())
                        .collect(),
                    clock_to_out_ps: by(c.clock_to_out_ps),
                    launches: c.launches.clone(),
                    setup_ps: c.setup_ps.iter().map(|&ps| by(ps)).collect(),
                    hold_ps: c.hold_ps.iter().map(|&ps| by(ps)).collect(),
                })
                .collect(),
            classes: self.classes.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gate_netlist::{Cell, CellId};

    const FILE: &str = r#"
        corner = "slow"
        wires = [
            { class = "*long*", factor = 0.5 },
            { class = "*", factor = 0.8 },
        ]
        cells = [
            { cell = "CARRY", between = 0.25 },
            { cell = "LUT*", factor = 0.5 },
        ]
        clock = { between = 0.5 }
    "#;

    fn netlist() -> GateNetlist {
        let mut netlist = GateNetlist::new("t".into(), "lib".into());
        for kind in ["LUT4", "CARRY", "DFF"] {
            netlist.add_cell(Cell::new_comb(
                CellId(0),
                kind.into(),
                "lib".into(),
                0.0,
                format!("top.{kind}"),
                vec![],
                vec![],
            ));
        }
        netlist
    }

    fn cell(wires: Vec<i64>, split: Vec<Vec<(u32, i64)>>, arc: i64, clock: i64) -> CellDelays {
        CellDelays {
            wire_ps: wires,
            wire_classes: split,
            clock_wire_ps: clock,
            arcs: vec![vec![Some(arc)]],
            clock_to_out_ps: arc,
            launches: vec![false],
            setup_ps: vec![arc],
            hold_ps: vec![],
        }
    }

    /// Each class of a wire's switches takes the factor of the first
    /// pattern matching it, a wire without a split the factor of `""`;
    /// cells take their type's rule, factor or fraction from the fast
    /// corner, and what nothing matches keeps the corner's delay
    #[test]
    fn delays_are_scaled_by_switch_class_and_cell_type() {
        let calibration = DelayCalibration::from_toml(FILE).unwrap();
        let slow = DelayAnnotation {
            corner: "slow".into(),
            classes: vec!["span_long".into(), "span_short".into()],
            cells: vec![
                // 1000 ps of long switches and 100 of short, then a pin
                // with no split
                cell(
                    vec![1100, 200],
                    vec![vec![(0, 1000), (1, 100)], vec![]],
                    400,
                    0,
                ),
                cell(vec![], vec![], 400, 0),
                cell(vec![], vec![], 400, 600),
            ],
        };
        let fast = DelayAnnotation {
            corner: "fast".into(),
            classes: Vec::new(),
            cells: vec![
                cell(vec![500, 100], vec![], 200, 0),
                cell(vec![], vec![], 200, 0),
                cell(vec![], vec![], 200, 200),
            ],
        };
        let (cal, report) =
            DelayAnnotation::calibrated(&slow, Some(&fast), &calibration, &netlist()).unwrap();
        assert_eq!(cal.cells[0].wire_ps, vec![500 + 80, 160]);
        assert_eq!(cal.cells[0].arcs, vec![vec![Some(200)]]);
        assert_eq!(cal.cells[0].setup_ps, vec![200]);
        // CARRY a quarter of the way from 200 to 400
        assert_eq!(cal.cells[1].arcs, vec![vec![Some(250)]]);
        assert_eq!(cal.cells[1].clock_to_out_ps, 250);
        // DFF: no rule for its own delays; the clock half way 200 -> 600
        assert_eq!(cal.cells[2].arcs, vec![vec![Some(400)]]);
        assert_eq!(cal.cells[2].clock_wire_ps, 400);
        assert_eq!(report.unmatched_cells, vec!["DFF".to_string()]);
        assert!(report.unmatched_classes.is_empty());
        assert_eq!(report.unsplit_wires, 1);
        // A fraction from the fast corner needs it
        assert!(DelayAnnotation::calibrated(&slow, None, &calibration, &netlist()).is_err());
        // The whole die slower by 2%
        let derated = cal.derated(1.02);
        assert_eq!(derated.cells[0].wire_ps, vec![592, 163]);
        assert_eq!(derated.cells[2].clock_wire_ps, 408);
    }

    #[test]
    fn calibration_files_are_checked() {
        let one_of = "corner = \"slow\"\ncells = [{ cell = \"*\", factor = 1.0, between = 0.5 }]\n";
        assert!(DelayCalibration::from_toml(one_of).is_err());
        let neither = "corner = \"slow\"\nclock = {}\n";
        assert!(DelayCalibration::from_toml(neither).is_err());
        let negative = "corner = \"slow\"\nwires = [{ class = \"*\", factor = -1.0 }]\n";
        assert!(DelayCalibration::from_toml(negative).is_err());
        let unknown = "corner = \"slow\"\nwire = []\n";
        assert!(DelayCalibration::from_toml(unknown).is_err());
        let calibration = DelayCalibration::from_toml(FILE).unwrap();
        assert_eq!(calibration.from, "fast");
        assert!(calibration.needs_from_corner());
        // JSON, as a device database holds it, with notes for people
        let json = r#"{ "comment": "one chip", "corner": "slow",
            "wires": [{ "class": "*", "factor": 0.8 }],
            "measured": { "date": "2026-11-25" } }"#;
        let calibration = DelayCalibration::from_json(json).unwrap();
        assert_eq!(calibration.wires[0].factor, 0.8);
        assert!(DelayCalibration::from_json(r#"{ "corner": "slow", "clock": {} }"#).is_err());
    }
}
