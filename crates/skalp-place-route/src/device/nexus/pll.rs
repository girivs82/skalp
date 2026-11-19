//! PLL (`PLL_CORE`) sites of a Nexus device.
//!
//! A device has a PLL in each of up to four corners, in a `GPLL_<corner>`
//! tile. Its pins are wires named `J<pin>_PLL_CORE_PLL_LMMI` one tile into
//! the fabric from that tile; most reach general routing through the
//! neighbouring CIB, the reference clock comes through the tile's REFMUX
//! (which takes a CIB clock input), and the outputs feed the clock network's
//! centre muxes. The PLL's settings are registers on the configuration bus
//! at the corner's `PLL_<corner>` base address (`baseaddr.json`), described
//! by the `PLL_CORE` iptype; the tile holds only the feedback and LMMI muxes.

use super::prjoxide_load::PrjoxideTile;
use crate::collections::{HashMap, HashMapExt};
use skalp_lir::gate_netlist::{Cell, CellId, GateNetId, GateNetlist};

/// A PLL's input pins as the device names them, in router pin order.
const INPUT_PINS: &[&str] = &[
    "REFCK",
    "FBKCK",
    "PLLRESET",
    "PLLPDN",
    "ENCLKOP",
    "ENCLKOS",
    "ENCLKOS2",
    "ENCLKOS3",
    "ENCLKOS4",
    "ENCLKOS5",
    "ENEXT",
    "LEGACY",
    "STDBY",
    "REFDETRESET",
    "DIRDEL",
    "DIRDELP1",
    "ROTDEL",
    "ROTDELP1",
    "CIBDIR",
    "CIBDSEL0",
    "CIBDSEL1",
    "CIBDSEL2",
    "CIBLDREG",
    "CIBROT",
    "LMMICLK",
    "LMMIRESETN",
    "LMMIREQUEST",
    "LMMIWRRDN",
    "LMMIOFFSET0",
    "LMMIOFFSET1",
    "LMMIOFFSET2",
    "LMMIOFFSET3",
    "LMMIOFFSET4",
    "LMMIOFFSET5",
    "LMMIOFFSET6",
    "LMMIWDATA0",
    "LMMIWDATA1",
    "LMMIWDATA2",
    "LMMIWDATA3",
    "LMMIWDATA4",
    "LMMIWDATA5",
    "LMMIWDATA6",
    "LMMIWDATA7",
    "OPCGLDCK",
    "SCANCLK",
    "SCANRST",
    "ZRSEL3",
];

/// A PLL's output pins as the device names them, in router pin order.
const OUTPUT_PINS: &[&str] = &[
    "CLKOP",
    "CLKOS",
    "CLKOS2",
    "CLKOS3",
    "CLKOS4",
    "CLKOS5",
    "LOCK",
    "INTLOCK",
    "LEGRDYN",
    "CLKOUTDL",
    "REFMUXCK",
    "REFDETLOS",
    "PFDDN",
    "PFDUP",
    "LMMIRDATA0",
    "LMMIRDATA1",
    "LMMIRDATA2",
    "LMMIRDATA3",
    "LMMIRDATA4",
    "LMMIRDATA5",
    "LMMIRDATA6",
    "LMMIRDATA7",
    "LMMIRDATAVALID",
    "LMMIREADY",
];

/// Primitive port names that differ from the device's pin names.
const PORT_ALIASES: &[(&str, &str)] = &[
    ("RST", "PLLRESET"),
    ("PLLPOWERDOWN_N", "PLLPDN"),
    ("LMMIWRRD_N", "LMMIWRRDN"),
    ("LMMIRESET_N", "LMMIRESETN"),
];

/// Whether a cell is a PLL: a `PLL_CORE` blackbox instance.
pub fn is_pll(cell: &Cell) -> bool {
    cell.cell_type == "PLL_CORE" || cell.cell_type == "BLACKBOX_PLL_CORE"
}

/// The device pin a primitive port names.
fn device_pin(port: &str) -> &str {
    PORT_ALIASES
        .iter()
        .find(|(alias, _)| *alias == port)
        .map_or(port, |(_, pin)| pin)
}

/// Router pin number of a PLL's `index`-th input.
pub fn input_router_pin(cell: &Cell, index: usize) -> Option<usize> {
    let port = device_pin(cell.blackbox_pin(false, index)?);
    INPUT_PINS.iter().position(|&p| p == port)
}

/// Router pin number of a PLL's `index`-th output.
pub fn output_router_pin(cell: &Cell, index: usize) -> Option<usize> {
    let port = device_pin(cell.blackbox_pin(true, index)?);
    OUTPUT_PINS.iter().position(|&p| p == port)
}

/// The device name of the input pin a cell input names, if it is a PLL pin.
pub fn input_pin_name(cell: &Cell, index: usize) -> Option<&'static str> {
    input_router_pin(cell, index).map(|n| INPUT_PINS[n])
}

/// Input pins by router pin number.
pub fn input_pins() -> &'static [&'static str] {
    INPUT_PINS
}

/// Output pins by router pin number.
pub fn output_pins() -> &'static [&'static str] {
    OUTPUT_PINS
}

/// Every port a cell names that the PLL does not have, for an error.
pub fn unknown_ports(cell: &Cell) -> Vec<String> {
    let mut unknown = Vec::new();
    for output in [false, true] {
        let known = if output { OUTPUT_PINS } else { INPUT_PINS };
        let mut index = 0;
        while let Some(port) = cell.blackbox_pin(output, index) {
            if !known.contains(&device_pin(port)) {
                unknown.push(port.to_string());
            }
            index += 1;
        }
    }
    unknown
}

/// The corner (`ULC`, `URC`, `LLC` or `LRC`) of a `GPLL_<corner>` tile type
/// (a CrossLink-NX tile type may carry a device suffix, as `GPLL_LLC_15K`).
pub fn corner_of_tile(tiletype: &str) -> Option<&str> {
    let corner = tiletype.strip_prefix("GPLL_")?.get(..3)?;
    ["ULC", "URC", "LLC", "LRC"]
        .contains(&corner)
        .then_some(corner)
}

/// A block's clock pin that a PLL output drives and that only some corners'
/// PLLs reach over the dedicated clock network (an edge clock synchronizer's
/// input, the ADC's converter clock).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClockNeed {
    /// The PLL output (`CLKOP`, `CLKOS4`, ...)
    pub output: String,
    /// What it drives, for messages: the block, its pin and the cell
    pub sink: String,
    /// The corners whose PLL's `output` reaches it
    pub corners: Vec<String>,
}

/// The wire nodes a corner's PLL drives from its output `pin`: its pins sit
/// one tile into the fabric from the PLL tile at `(x, y)`, beside it or
/// diagonally, and the network says which of those is the PLL's.
pub fn output_nodes(
    network: &super::prjoxide_graph::ClockNetwork,
    x: u32,
    y: u32,
    pin: &str,
) -> Vec<super::NodeKey> {
    ["E1", "W1", "S1", "N1", "S1E1", "N1E1", "S1W1", "N1W1"]
        .into_iter()
        .filter_map(|step| super::resolve_wire(x, y, &site_wire(step, pin)))
        .filter(|node| network.drives(node))
        .collect()
}

/// The corners (name, tile x, y) of a device's PLLs, in name order.
pub fn corners(tiles: &[PrjoxideTile]) -> Vec<(&str, u32, u32)> {
    let mut corners: Vec<(&str, u32, u32)> = tiles
        .iter()
        .filter_map(|t| Some((corner_of_tile(&t.tiletype)?, t.x, t.y)))
        .collect();
    corners.sort_unstable();
    corners
}

/// The PLL site (tile x, y) of every PLL of a design: the corner its `LOC`
/// names (`LLC` or `PLL_LLC`), or else the free corner nearest the design's
/// pins (`pads`) among those that reach what the PLL's outputs must
/// (`needs`, per PLL). Every port must be one the PLL has.
pub fn choose_sites(
    tiles: &[PrjoxideTile],
    netlist: &GateNetlist,
    pads: &[(u32, u32)],
    needs: &HashMap<CellId, Vec<ClockNeed>>,
) -> Result<HashMap<CellId, (u32, u32)>, String> {
    let corners = corners(tiles);
    let mut sites = HashMap::new();
    let no_needs = Vec::new();
    let needs_of = |cell: &Cell| needs.get(&cell.id).unwrap_or(&no_needs);
    // Whether a corner's PLL reaches everything a PLL's outputs drive
    let serves = |corner: &str, cell: &Cell| {
        needs_of(cell)
            .iter()
            .all(|n| n.corners.iter().any(|c| c == corner))
    };
    let plls: Vec<&Cell> = netlist.cells.iter().filter(|c| is_pll(c)).collect();
    // PLLs with a corner named take theirs first, then those with the fewest
    // corners that serve them
    let mut ordered = plls.clone();
    ordered.sort_by_key(|c| {
        let serving = corners.iter().filter(|k| serves(k.0, c)).count();
        (c.parameters.get("LOC").is_none(), serving, c.id.0)
    });
    // What a PLL's outputs drive that a corner's PLL does not reach
    let unreached = |corner: &str, cell: &Cell| -> Vec<String> {
        needs_of(cell)
            .iter()
            .filter(|n| !n.corners.iter().any(|c| c == corner))
            .map(|n| {
                format!(
                    "its {} drives {}, which only the {} corner's PLL reaches",
                    n.output,
                    n.sink,
                    n.corners.join(" or ")
                )
            })
            .collect()
    };
    let mut taken: Vec<&str> = Vec::new();
    for cell in ordered {
        let unknown = unknown_ports(cell);
        if !unknown.is_empty() {
            return Err(format!(
                "PLL '{}' has ports the device's PLL does not: {}",
                cell.path,
                unknown.join(", ")
            ));
        }
        let site = match cell.parameters.get("LOC") {
            Some(loc) => {
                let wanted = loc.strip_prefix("PLL_").unwrap_or(loc);
                let site = corners
                    .iter()
                    .find(|(corner, _, _)| *corner == wanted)
                    .copied()
                    .ok_or_else(|| {
                        let known: Vec<&str> = corners.iter().map(|c| c.0).collect();
                        format!(
                            "PLL '{}': no PLL in corner {loc}; this device has {}",
                            cell.path,
                            known.join(", ")
                        )
                    })?;
                let missing = unreached(site.0, cell);
                if !missing.is_empty() {
                    let serving: Vec<&str> = corners
                        .iter()
                        .filter(|c| serves(c.0, cell))
                        .map(|c| c.0)
                        .collect();
                    return Err(format!(
                        "PLL '{}' is placed in the {} corner (LOC), but {}; {}",
                        cell.path,
                        site.0,
                        missing.join("; "),
                        if serving.is_empty() {
                            "no corner's PLL reaches all of them".to_string()
                        } else {
                            format!("set its LOC to {} or leave it out", serving.join(" or "))
                        }
                    ));
                }
                site
            }
            None => {
                let (cx, cy) = if pads.is_empty() {
                    (0.0, 0.0)
                } else {
                    let n = pads.len() as f64;
                    (
                        pads.iter().map(|p| p.0 as f64).sum::<f64>() / n,
                        pads.iter().map(|p| p.1 as f64).sum::<f64>() / n,
                    )
                };
                let distance = |&(_, x, y): &(&str, u32, u32)| {
                    (x as f64 - cx).powi(2) + (y as f64 - cy).powi(2)
                };
                corners
                    .iter()
                    .filter(|c| !taken.contains(&c.0) && serves(c.0, cell))
                    .min_by(|a, b| distance(a).total_cmp(&distance(b)))
                    .copied()
                    .ok_or_else(|| {
                        let serving: Vec<&str> = corners
                            .iter()
                            .filter(|c| serves(c.0, cell))
                            .map(|c| c.0)
                            .collect();
                        if serving.is_empty() {
                            let missing: Vec<String> = corners
                                .iter()
                                .flat_map(|c| unreached(c.0, cell))
                                .collect::<std::collections::BTreeSet<_>>()
                                .into_iter()
                                .collect();
                            format!(
                                "PLL '{}': no corner's PLL reaches everything its outputs \
                                 drive: {}",
                                cell.path,
                                missing.join("; ")
                            )
                        } else if needs_of(cell).is_empty() {
                            format!("PLL '{}': no PLL corner left", cell.path)
                        } else {
                            format!(
                                "PLL '{}': only the {} corner's PLL reaches what its outputs \
                                 drive, and another PLL has it; set the PLLs' LOCs",
                                cell.path,
                                serving.join(" or ")
                            )
                        }
                    })?
            }
        };
        if taken.contains(&site.0) {
            return Err(format!(
                "PLL '{}': the {} corner's PLL is already used",
                cell.path, site.0
            ));
        }
        taken.push(site.0);
        sites.insert(cell.id, (site.1, site.2));
    }
    Ok(sites)
}

/// Name of a pin's site wire from a PLL tile: the pins sit one tile into the
/// fabric, in direction `step` (such as `E1`) from the tile.
pub fn site_wire(step: &str, pin: &str) -> String {
    format!("{step}:J{pin}_PLL_CORE_PLL_LMMI")
}

/// Settings `PLL_CORE` instances take unless they give their own: the
/// primitive's defaults, as Lattice's primitive library declares them (and
/// Yosys's `cells_bb_nexus.v` repeats). Left alone, they give a PLL that
/// runs from its CLKOP output with every divider at 1 and its outputs off.
pub const DEFAULT_SETTINGS: &[(&str, &str)] = &[
    ("BW_CTL_BIAS", "0b0101"),
    ("CLKOP_TRIM", "0b0000"),
    ("CLKOS_TRIM", "0b0000"),
    ("CLKOS2_TRIM", "0b0000"),
    ("CLKOS3_TRIM", "0b0000"),
    ("CLKOS4_TRIM", "0b0000"),
    ("CLKOS5_TRIM", "0b0000"),
    ("CRIPPLE", "5P"),
    ("CSET", "40P"),
    ("DELAY_CTRL", "200PS"),
    ("DELA", "0"),
    ("DELB", "0"),
    ("DELC", "0"),
    ("DELD", "0"),
    ("DELE", "0"),
    ("DELF", "0"),
    ("DIRECTION", "DISABLED"),
    ("DIVA", "0"),
    ("DIVB", "0"),
    ("DIVC", "0"),
    ("DIVD", "0"),
    ("DIVE", "0"),
    ("DIVF", "0"),
    ("DYN_SEL", "0b000"),
    ("DYN_SOURCE", "STATIC"),
    ("ENCLK_CLKOP", "DISABLED"),
    ("ENCLK_CLKOS", "DISABLED"),
    ("ENCLK_CLKOS2", "DISABLED"),
    ("ENCLK_CLKOS3", "DISABLED"),
    ("ENCLK_CLKOS4", "DISABLED"),
    ("ENCLK_CLKOS5", "DISABLED"),
    ("ENABLE_SYNC", "DISABLED"),
    ("FAST_LOCK_EN", "ENABLED"),
    ("V2I_1V_EN", "DISABLED"),
    ("FBK_CUR_BLE", "0b00000000"),
    ("FBK_EDGE_SEL", "POSITIVE"),
    ("FBK_IF_TIMING_CTL", "0b00"),
    ("FBK_INTEGER_MODE", "DISABLED"),
    ("FBK_MASK", "0b00001000"),
    ("FBK_MMD_DIG", "8"),
    ("FBK_MMD_PULS_CTL", "0b0000"),
    ("FBK_MODE", "0b00"),
    ("FBK_PI_BYPASS", "NOT_BYPASSED"),
    ("FBK_PI_RC", "0b1100"),
    ("FBK_PR_CC", "0b0000"),
    ("FBK_PR_IC", "0b1000"),
    ("FLOAT_CP", "DISABLED"),
    ("FLOCK_CTRL", "2X"),
    ("FLOCK_EN", "ENABLED"),
    ("FLOCK_SRC_SEL", "REFCLK"),
    ("FORCE_FILTER", "DISABLED"),
    ("I_CTRL", "10UA"),
    ("IPI_CMP", "0b1000"),
    ("IPI_CMPN", "0b0011"),
    ("IPI_COMP_EN", "DISABLED"),
    ("IPP_CTRL", "0b1000"),
    ("IPP_SEL", "0b1111"),
    ("KP_VCO", "0b11001"),
    ("LDT_INT_LOCK_STICKY", "DISABLED"),
    ("LDT_LOCK", "1536CYC"),
    ("LDT_LOCK_SEL", "U_FREQ"),
    ("LEGACY_ATT", "DISABLED"),
    ("LOAD_REG", "DISABLED"),
    ("OPENLOOP_EN", "DISABLED"),
    ("PHIA", "0"),
    ("PHIB", "0"),
    ("PHIC", "0"),
    ("PHID", "0"),
    ("PHIE", "0"),
    ("PHIF", "0"),
    ("PLLPDN_EN", "DISABLED"),
    ("PLLPD_N", "UNUSED"),
    ("PLLRESET_ENA", "DISABLED"),
    ("REF_INTEGER_MODE", "DISABLED"),
    ("REF_MASK", "0b00000000"),
    ("REF_MMD_DIG", "8"),
    ("REF_MMD_IN", "0b00001000"),
    ("REF_MMD_PULS_CTL", "0b0000"),
    ("REF_TIMING_CTL", "0b00"),
    ("REFIN_RESET", "SET"),
    ("RESET_LF", "DISABLED"),
    ("ROTATE", "DISABLED"),
    ("SEL_OUTA", "DISABLED"),
    ("SEL_OUTB", "DISABLED"),
    ("SEL_OUTC", "DISABLED"),
    ("SEL_OUTD", "DISABLED"),
    ("SEL_OUTE", "DISABLED"),
    ("SEL_OUTF", "DISABLED"),
    ("SLEEP", "DISABLED"),
    ("SSC_DITHER", "DISABLED"),
    ("SSC_EN_CENTER_IN", "DOWN_TRIANGLE"),
    ("SSC_EN_SDM", "DISABLED"),
    ("SSC_EN_SSC", "DISABLED"),
    ("SSC_F_CODE", "0b000000000000000"),
    ("SSC_N_CODE", "0b000010100"),
    ("SSC_ORDER", "SDM_ORDER2"),
    ("SSC_PI_BYPASS", "NOT_BYPASSED"),
    ("SSC_REG_WEIGHTING_SEL", "0b000"),
    ("SSC_SQUARE_MODE", "DISABLED"),
    ("SSC_STEP_IN", "0b0000000"),
    ("SSC_TBASE", "0b000000000000"),
    ("STDBY_ATT", "DISABLED"),
    ("TRIMOP_BYPASS_N", "BYPASSED"),
    ("TRIMOS_BYPASS_N", "BYPASSED"),
    ("TRIMOS2_BYPASS_N", "BYPASSED"),
    ("TRIMOS3_BYPASS_N", "BYPASSED"),
    ("TRIMOS4_BYPASS_N", "BYPASSED"),
    ("TRIMOS5_BYPASS_N", "BYPASSED"),
    ("V2I_KVCO_SEL", "85"),
    ("V2I_PP_ICTRL", "0b00110"),
    ("V2I_PP_RES", "10K"),
    ("DIV_DEL", "0b0000001"),
];

/// Registers no primitive setting reaches (manufacturing test, reserved and
/// spread-spectrum calibration fields): they stay cleared unless a cell
/// names them.
pub const RESERVED_SETTINGS: &[&str] = &[
    "FBK_RSV",
    "MFGOUT1_SEL",
    "MFGOUT2_SEL",
    "MFG_CTRL",
    "MFG_SEL",
    "SSC_DELTA",
    "SSC_DELTA_CTL",
];

/// Name of the iptype describing a PLL's registers.
pub const IPTYPE: &str = "PLL_CORE";

/// Input pins that must read high when the design leaves them unconnected:
/// the output enables, power-down (active low) and the LMMI reset (active
/// low).
pub const FLOAT_HIGH_PINS: &[&str] = &[
    "PLLPDN",
    "ENCLKOP",
    "ENCLKOS",
    "ENCLKOS2",
    "ENCLKOS3",
    "ENCLKOS4",
    "ENCLKOS5",
    "LMMIRESETN",
];

/// Configuration bits outside the PLL's own tile that a device sets
/// whenever a PLL is used, as (tile type, frame, bit). CertusPro-NX sets two
/// bits in its oscillator tile for a PLL in any corner, found by fuzzing
/// Radiant; what they do is not known.
pub fn device_enable_bits(variant: super::NexusVariant) -> &'static [(&'static str, u32, u32)] {
    match variant.prjoxide_family() {
        "LFCPNX" => &[("EFB_1_OSC", 41, 1), ("EFB_1_OSC", 105, 0)],
        _ => &[],
    }
}

/// Settings that live in the PLL's tile rather than its registers.
pub const TILE_SETTINGS: &[(&str, &str)] = &[
    ("CLKMUX_FB", "CMUX_CLKOP"),
    ("LMMICLKMUX", "LMMICLK"),
    ("LMMIRESETNMUX", "LMMIRESETN"),
];

/// A setting's value as a number: decimal, or binary with a `0b` prefix.
pub fn parse_number(value: &str) -> Option<u64> {
    let value = value.trim();
    match value
        .strip_prefix("0b")
        .or_else(|| value.strip_prefix("0B"))
    {
        Some(bits) => u64::from_str_radix(&bits.replace('_', ""), 2).ok(),
        None => match value
            .strip_prefix("0x")
            .or_else(|| value.strip_prefix("0X"))
        {
            Some(hex) => u64::from_str_radix(&hex.replace('_', ""), 16).ok(),
            None => value.parse().ok(),
        },
    }
}

/// The value a PLL cell gives a setting, or its default.
pub fn setting<'a>(cell: &'a Cell, name: &str) -> Option<&'a str> {
    cell.parameters.get(name).map(String::as_str).or_else(|| {
        DEFAULT_SETTINGS
            .iter()
            .chain(TILE_SETTINGS)
            .find(|(n, _)| *n == name)
            .map(|(_, v)| *v)
    })
}

/// Output frequencies of a PLL in MHz for a reference of `refclk_mhz`, by
/// output pin, when it runs from CLKOP feedback (the VCO is
/// `ref × FBK_MMD_DIG × (DIVA + 1) / REF_MMD_DIG`, each output
/// `VCO / (DIVx + 1)`).
pub fn output_frequencies(cell: &Cell, refclk_mhz: f64) -> Option<Vec<(&'static str, f64)>> {
    let number = |name: &str| setting(cell, name).and_then(parse_number);
    if setting(cell, "CLKMUX_FB")? != "CMUX_CLKOP" {
        return None;
    }
    let reference_divider = number("REF_MMD_DIG")?.max(1) as f64;
    let feedback_divider = number("FBK_MMD_DIG")?.max(1) as f64;
    let diva = number("DIVA")? as f64 + 1.0;
    let vco = refclk_mhz * feedback_divider * diva / reference_divider;
    let outputs = ["CLKOP", "CLKOS", "CLKOS2", "CLKOS3", "CLKOS4", "CLKOS5"];
    let dividers = ["DIVA", "DIVB", "DIVC", "DIVD", "DIVE", "DIVF"];
    Some(
        outputs
            .iter()
            .zip(dividers)
            .filter_map(|(&out, div)| Some((out, vco / (number(div)? as f64 + 1.0))))
            .collect(),
    )
}

/// Frequencies a PLL drives, by cell output index, once its reference
/// clock's frequency is known (`known` gives a net's frequency in MHz).
pub fn driven_frequencies(
    cell: &Cell,
    known: &dyn Fn(GateNetId) -> Option<f64>,
) -> Vec<(usize, f64)> {
    if !is_pll(cell) {
        return Vec::new();
    }
    let reference = (0..cell.inputs.len())
        .find(|&index| input_pin_name(cell, index) == Some("REFCK"))
        .and_then(|index| known(cell.inputs[index]));
    let Some(outputs) = reference.and_then(|mhz| output_frequencies(cell, mhz)) else {
        return Vec::new();
    };
    (0..cell.outputs.len())
        .filter_map(|index| {
            let pin = OUTPUT_PINS[output_router_pin(cell, index)?];
            let (_, mhz) = outputs.iter().find(|(name, _)| *name == pin)?;
            Some((index, *mhz))
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use skalp_lir::gate_netlist::{CellId, BLACKBOX_INPUTS_PARAM, BLACKBOX_OUTPUTS_PARAM};

    fn pll(params: &[(&str, &str)]) -> Cell {
        let mut cell = Cell::new_comb(
            CellId(0),
            "BLACKBOX_PLL_CORE".into(),
            "blackbox".into(),
            0.0,
            "top.pll".into(),
            vec![],
            vec![],
        );
        cell.parameters.insert(
            BLACKBOX_INPUTS_PARAM.into(),
            "REFCK,RST,PLLPOWERDOWN_N".into(),
        );
        cell.parameters
            .insert(BLACKBOX_OUTPUTS_PARAM.into(), "CLKOP,LOCK".into());
        for (k, v) in params {
            cell.parameters.insert(k.to_string(), v.to_string());
        }
        cell
    }

    #[test]
    fn ports_map_to_device_pins() {
        let cell = pll(&[]);
        assert_eq!(input_pin_name(&cell, 0), Some("REFCK"));
        assert_eq!(input_pin_name(&cell, 1), Some("PLLRESET"));
        assert_eq!(input_pin_name(&cell, 2), Some("PLLPDN"));
        assert_eq!(output_router_pin(&cell, 0), Some(0));
        assert_eq!(output_router_pin(&cell, 1), Some(6));
        assert!(unknown_ports(&cell).is_empty());
        assert!(is_pll(&cell));
    }

    #[test]
    fn frequencies_follow_the_dividers() {
        // 125 MHz in, VCO 1600 MHz, CLKOP 100 MHz (Radiant's settings)
        let cell = pll(&[("REF_MMD_DIG", "5"), ("FBK_MMD_DIG", "4"), ("DIVA", "15")]);
        let outputs = output_frequencies(&cell, 125.0).unwrap();
        assert_eq!(outputs[0], ("CLKOP", 100.0));
        assert_eq!(parse_number("0b0101"), Some(5));
        assert_eq!(corner_of_tile("GPLL_LLC_15K"), Some("LLC"));
        assert_eq!(corner_of_tile("GPLL_URC"), Some("URC"));
    }

    #[test]
    fn a_pll_drives_its_outputs_once_its_reference_is_known() {
        // 125 MHz in, CLKOP 137.5 MHz (VCO 1512.5 MHz)
        let mut cell = pll(&[("REF_MMD_DIG", "10"), ("FBK_MMD_DIG", "11"), ("DIVA", "10")]);
        cell.inputs = vec![GateNetId(7), GateNetId(8), GateNetId(9)];
        cell.outputs = vec![GateNetId(10), GateNetId(11)];
        let reference = |net: GateNetId| (net == GateNetId(7)).then_some(125.0);
        let driven = driven_frequencies(&cell, &reference);
        assert_eq!(driven.len(), 1, "CLKOP only, not LOCK: {driven:?}");
        assert_eq!(driven[0].0, 0);
        assert!((driven[0].1 - 137.5).abs() < 1e-9);
        assert!(driven_frequencies(&cell, &|_| None).is_empty());
    }

    fn tile(tiletype: &str, x: u32, y: u32) -> PrjoxideTile {
        PrjoxideTile {
            name: format!("R{y}C{x}:{tiletype}"),
            tiletype: tiletype.into(),
            x,
            y,
            start_frame: 0,
            frames: 0,
            start_bit: 0,
            bits: 0,
            skalp_type: crate::device::TileType::Pll,
        }
    }

    /// Four corners on a 100 x 100 grid
    fn four_corners() -> Vec<PrjoxideTile> {
        vec![
            tile("GPLL_ULC", 0, 0),
            tile("GPLL_URC", 99, 0),
            tile("GPLL_LLC", 0, 99),
            tile("GPLL_LRC", 99, 99),
        ]
    }

    fn one_pll(loc: Option<&str>) -> (GateNetlist, CellId) {
        let mut netlist = GateNetlist::new("top".into(), "nexus".into());
        let mut cell = pll(&[]);
        if let Some(loc) = loc {
            cell.parameters.insert("LOC".into(), loc.into());
        }
        let id = cell.id;
        netlist.cells.push(cell);
        (netlist, id)
    }

    fn need(id: CellId, corners: &[&str]) -> HashMap<CellId, Vec<ClockNeed>> {
        let mut needs = HashMap::new();
        needs.insert(
            id,
            vec![ClockNeed {
                output: "CLKOP".into(),
                sink: "ECLKSYNC4_0 ECLKIN".into(),
                corners: corners.iter().map(|c| c.to_string()).collect(),
            }],
        );
        needs
    }

    /// Unconstrained, a PLL goes to the corner nearest the pins; when its
    /// output drives a clock pin only some corners reach, it goes to the
    /// nearest of those, however far the pins pull
    #[test]
    fn a_pll_goes_where_its_outputs_reach() {
        let tiles = four_corners();
        let (netlist, id) = one_pll(None);
        let pads = [(95, 95), (90, 99)];
        let free = choose_sites(&tiles, &netlist, &pads, &HashMap::new()).unwrap();
        assert_eq!(free[&id], (99, 99));
        let bound = choose_sites(&tiles, &netlist, &pads, &need(id, &["LLC", "ULC"])).unwrap();
        assert_eq!(bound[&id], (0, 99));
    }

    /// A LOC whose PLL cannot reach what its outputs drive is refused,
    /// naming the corners that can
    #[test]
    fn a_loc_that_cannot_reach_is_refused() {
        let tiles = four_corners();
        let (netlist, id) = one_pll(Some("LRC"));
        let err = choose_sites(&tiles, &netlist, &[], &need(id, &["LLC"])).unwrap_err();
        assert!(err.contains("LRC corner (LOC)"), "{err}");
        assert!(err.contains("ECLKSYNC4_0 ECLKIN"), "{err}");
        assert!(err.contains("set its LOC to LLC"), "{err}");
        let (netlist, id) = one_pll(Some("PLL_LLC"));
        let ok = choose_sites(&tiles, &netlist, &[], &need(id, &["LLC"])).unwrap();
        assert_eq!(ok[&id], (0, 99));
    }

    /// Two PLLs that both need the one corner that reaches their sinks
    /// cannot both have it; a PLL with fewer serving corners chooses first
    #[test]
    fn constrained_plls_choose_first() {
        let tiles = four_corners();
        let mut netlist = GateNetlist::new("top".into(), "nexus".into());
        let a = pll(&[]);
        let mut b = pll(&[]);
        b.id = CellId(1);
        b.path = "top.pll_b".into();
        netlist.cells.push(a);
        netlist.cells.push(b);
        // The first PLL would take LLC (nearest the pins) if it chose first
        let mut needs = need(CellId(1), &["LLC"]);
        needs.insert(CellId(0), Vec::new());
        let sites = choose_sites(&tiles, &netlist, &[(1, 98)], &needs).unwrap();
        assert_eq!(sites[&CellId(1)], (0, 99));
        assert_ne!(sites[&CellId(0)], (0, 99));
        // Both bound to LLC: the second is refused, naming it
        needs.insert(CellId(0), need(CellId(0), &["LLC"])[&CellId(0)].clone());
        let err = choose_sites(&tiles, &netlist, &[], &needs).unwrap_err();
        assert!(err.contains("only the LLC corner"), "{err}");
    }
}

#[cfg(all(test, feature = "nexus-bitstream"))]
mod device_tests {
    /// Over the CertusPro-NX's dedicated clock network, each of the bottom
    /// middle bank's edge clock synchronizers takes CLKOP from one of the
    /// two lower PLLs only: synchronizers 0 and 1 from the lower left one,
    /// 2 and 3 from the lower right one. Neither upper PLL reaches them.
    #[test]
    fn certuspro_edge_clocks_take_clkop_from_one_corner() {
        use crate::device::nexus::ddr;
        use crate::device::nexus::prjoxide_graph::ClockNetwork;
        let Some(db) = find_database() else { return };
        let variant = NexusVariant::Lfcpnx100;
        let family = variant.prjoxide_family();
        let tiles = load_tilegrid(&db, variant).unwrap();
        let network = ClockNetwork::load(&db, family, &tiles).unwrap();
        let corners = super::corners(&tiles);
        let reaching = |pin: &str, x: u32, y: u32, wire: &str| -> Vec<&str> {
            let node = crate::device::nexus::resolve_wire(x, y, wire).unwrap();
            corners
                .iter()
                .filter(|&&(_, cx, cy)| {
                    network.reaches(&super::output_nodes(&network, cx, cy, pin), &node)
                })
                .map(|c| c.0)
                .collect()
        };
        let syncs: Vec<ddr::Site> = ddr::sites(&db, family, &tiles)
            .into_iter()
            .filter(|s| s.kind == ddr::SiteKind::Eclksync && s.bank == Some(4))
            .collect();
        assert_eq!(syncs.len(), 4, "{syncs:?}");
        let clkop: Vec<(&str, Vec<&str>)> = syncs
            .iter()
            .map(|sync| {
                let (x, y, wire) = &sync.pins["ECLKIN"];
                (sync.name.as_str(), reaching("CLKOP", *x, *y, wire))
            })
            .collect();
        assert_eq!(
            clkop,
            [
                ("ECLKSYNC4_0", vec!["LLC"]),
                ("ECLKSYNC4_1", vec!["LLC"]),
                ("ECLKSYNC4_2", vec!["LRC"]),
                ("ECLKSYNC4_3", vec!["LRC"]),
            ]
        );
        // Every corner's PLL drives something on the network
        for &(corner, x, y) in &corners {
            assert!(
                !super::output_nodes(&network, x, y, "CLKOP").is_empty(),
                "{corner}"
            );
        }
    }

    use crate::device::nexus::prjoxide_load::{find_database, load_tilegrid};
    use crate::device::nexus::{NexusDevice, NexusVariant};
    use crate::device::Device;

    /// Every PLL corner of the CertusPro-NX has its pins in the fabric: the
    /// reset from a set/reset line, the reference clock through its mux,
    /// and CLKOP into the clock muxes of the two edges it meets.
    #[test]
    fn every_certuspro_pll_corner_reaches_the_fabric() {
        let Some(db) = find_database() else { return };
        let variant = NexusVariant::Lfcpnx100;
        let tiles = load_tilegrid(&db, variant).unwrap();
        let mut corners: Vec<(&str, u32, u32)> = tiles
            .iter()
            .filter_map(|t| Some((super::corner_of_tile(&t.tiletype)?, t.x, t.y)))
            .collect();
        corners.sort_unstable();
        let names: Vec<&str> = corners.iter().map(|c| c.0).collect();
        assert_eq!(names, ["LLC", "LRC", "ULC", "URC"]);
        for (corner, x, y) in corners {
            let bbox = (x.saturating_sub(4), y.saturating_sub(4), x + 4, y + 4);
            let device = NexusDevice::from_prjoxide_bbox(variant, &db, bbox).unwrap();
            let name = |w| device.wire(w).map(|x| x.name.clone()).unwrap_or_default();
            let pin = |p: &str| {
                device
                    .bel_pin_wire_by_name(x, y, p)
                    .unwrap_or_else(|| panic!("{corner}: no {p} wire"))
            };
            let reset_sources: Vec<String> = device
                .wire_pips(pin("PLL_PLLRESET"))
                .into_iter()
                .map(|p| name(device.pip(p).unwrap().src_wire))
                .collect();
            assert!(
                reset_sources.iter().any(|s| s.contains("_JLSR")),
                "{corner}: reset from {reset_sources:?}"
            );
            assert!(
                !device.wire_pips(pin("PLL_REFCK")).is_empty(),
                "{corner}: nothing drives REFCK"
            );
            let clkop: Vec<String> = device
                .wire_src_pips(pin("PLL_CLKOP"))
                .iter()
                .map(|&p| name(device.pip(p).unwrap().dst_wire))
                .collect();
            let prefix = format!("G:J{}CLKOP_", &corner[..2]);
            assert!(
                clkop.iter().filter(|w| w.starts_with(&prefix)).count() >= 2,
                "{corner}: CLKOP feeds {clkop:?}"
            );
        }
    }
}
