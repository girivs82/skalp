//! On-chip logic analyzer: adds a capture core to a design and reads it back.
//!
//! Signals marked `#[trace]` in the top entity become probes, and its
//! `#[breakpoint]` signals form the trigger. `skalp synth --debug` adds the
//! standard library's `logic_analyzer` to the design, wired to the board's
//! UART, and writes a probe map; `skalp debug` reads a capture over the UART and
//! writes it as a waveform.

use crate::mir::{DataType, Module, PortDirection};
use anyhow::{anyhow, bail, Context, Result};
use serde::{Deserialize, Serialize};
use skalp_frontend::parse::parse_with_errors;
use skalp_frontend::syntax::{SyntaxKind, SyntaxNode};
use std::path::Path;

/// The standard library's capture core, added to the design's source.
const ANALYZER_SOURCE: &str = include_str!("../../skalp-stdlib/components/logic_analyzer.sk");

/// Name of the output port the capture leaves on.
const TX_PORT: &str = "skalp_debug_tx";
/// Samples recorded by default: 2^DEPTH_BITS, half before the trigger.
pub const DEPTH_BITS: u32 = 10;
/// UART rate. 1 Mbaud is exact on an FT2232H and divides common clocks.
pub const BAUD: u32 = 1_000_000;

/// Where the analyzer sends its capture, and the clock it runs on.
pub struct DebugLink {
    /// Package pin of the UART line the board routes to the host
    pub tx_pin: &'static str,
    /// IO standard of that pin
    pub io_standard: &'static str,
    /// Frequency of the design's clock, in hertz
    pub clock_hz: f64,
    /// Samples recorded: 2^depth_bits, half before the trigger
    pub depth_bits: u32,
    /// UART rate
    pub baud: u32,
}

/// One recorded signal.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Probe {
    pub name: String,
    pub width: usize,
    /// First bit within a sample
    pub offset: usize,
    #[serde(default)]
    pub radix: Option<String>,
    #[serde(default)]
    pub group: Option<String>,
}

/// What `skalp debug` needs to read a capture.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProbeMap {
    pub design: String,
    pub baud: u32,
    pub clock_hz: f64,
    pub depth: usize,
    pub probe_bytes: usize,
    /// Sample number of the trigger
    pub trigger_index: usize,
    pub trigger: String,
    pub probes: Vec<Probe>,
}

/// File the probe map is written to, next to the bitstream.
pub const PROBE_MAP_FILE: &str = "debug_probes.json";

fn name_of(node: &SyntaxNode) -> Option<String> {
    node.children_with_tokens()
        .filter_map(|e| e.into_token())
        .find(|t| t.kind() == SyntaxKind::Ident)
        .map(|t| t.text().to_string())
}

/// Offset just before the closing brace of a node's outermost brace pair.
fn before_closing_brace(node: &SyntaxNode) -> Option<usize> {
    node.descendants_with_tokens()
        .filter_map(|e| e.into_token())
        .filter(|t| t.kind() == SyntaxKind::RBrace)
        .last()
        .map(|t| usize::from(t.text_range().start()))
}

/// The design's source with the analyzer added, and its probe map.
pub fn add_analyzer(source: &str, top: &Module, link: &DebugLink) -> Result<(String, ProbeMap)> {
    let clock = top
        .ports
        .iter()
        .find(|p| {
            matches!(p.port_type, DataType::Clock { .. }) && p.direction == PortDirection::Input
        })
        .ok_or_else(|| anyhow!("'{}' has no clock input for the logic analyzer", top.name))?;

    let mut probes = Vec::new();
    let mut offset = 0;
    for signal in &top.signals {
        let Some(trace) = &signal.trace_config else {
            continue;
        };
        let width = crate::type_width::get_type_width(&signal.signal_type);
        probes.push(Probe {
            name: trace
                .display_name
                .clone()
                .unwrap_or_else(|| signal.name.clone()),
            width,
            offset,
            radix: Some(format!("{:?}", trace.radix).to_lowercase()),
            group: trace.group.clone(),
        });
        offset += width;
    }
    if probes.is_empty() {
        bail!(
            "'{}' has no #[trace] signals for the logic analyzer to record",
            top.name
        );
    }
    let probe_bits = offset;
    let probe_bytes = probe_bits.div_ceil(8);

    let mut triggers = Vec::new();
    for signal in &top.signals {
        let Some(breakpoint) = &signal.breakpoint_config else {
            continue;
        };
        match &breakpoint.condition {
            Some(condition) => triggers.push(format!("({condition})")),
            None if crate::type_width::get_type_width(&signal.signal_type) == 1 => {
                triggers.push(format!("({} == 1)", signal.name))
            }
            None => bail!(
                "#[breakpoint] on '{}' needs a condition for the logic analyzer, such as \
                 #[breakpoint(condition = \"{} == 3\")]",
                signal.name,
                signal.name
            ),
        }
    }
    let trigger = if triggers.is_empty() {
        "1'b1".to_string()
    } else {
        triggers.join(" || ")
    };

    let baud = f64::from(link.baud);
    let divider = (link.clock_hz / baud).round();
    let error = (link.clock_hz / divider - baud).abs() / baud;
    if divider < 4.0 || error > 0.02 {
        bail!(
            "A {:.1} MHz clock cannot run the debug UART at {} baud",
            link.clock_hz / 1e6,
            link.baud
        );
    }

    // Probes are packed with the first traced signal in the lowest bits. They
    // are wired by signal name; a display name only changes the waveform.
    let padding = probe_bytes * 8 - probe_bits;
    let packed: Vec<String> = top
        .signals
        .iter()
        .filter(|s| s.trace_config.is_some())
        .rev()
        .map(|s| s.name.clone())
        .collect();
    let mut packed = packed;
    if padding > 0 {
        packed.insert(0, format!("{padding}'b0"));
    }

    let tree = parse_with_errors(source).0;
    let entity = tree
        .descendants()
        .find(|n| n.kind() == SyntaxKind::EntityDecl && name_of(n).as_deref() == Some(&top.name))
        .ok_or_else(|| anyhow!("Cannot find entity '{}' in the source", top.name))?;
    let implementation = tree
        .descendants()
        .find(|n| n.kind() == SyntaxKind::ImplBlock && name_of(n).as_deref() == Some(&top.name))
        .ok_or_else(|| anyhow!("Cannot find the implementation of '{}'", top.name))?;
    // The entity's own closing brace ends its port list.
    let ports_end = before_closing_brace(&entity)
        .ok_or_else(|| anyhow!("Cannot find the end of '{}'s ports", top.name))?;
    let impl_end = before_closing_brace(&implementation)
        .ok_or_else(|| anyhow!("Cannot find the end of '{}'s implementation", top.name))?;

    let port = format!(
        "\n    out {TX_PORT}: bit @ {{ pin: \"{}\", io_standard: \"{}\" }}\n",
        link.tx_pin, link.io_standard
    );
    let instance = format!(
        "\n    signal skalp_debug_probes: bit[{probe_bits_padded}]\n    \
         signal skalp_debug_trigger: bit\n    \
         skalp_debug_probes = {{{packed}}}\n    \
         skalp_debug_trigger = {trigger}\n    \
         inst skalp_debug = logic_analyzer<{probe_bytes}, {depth_bits}, {depth}, {divider}> {{\n        \
         clk: {clock},\n        probes: skalp_debug_probes,\n        trigger: skalp_debug_trigger\n    }}\n    \
         {TX_PORT} = skalp_debug.tx\n",
        probe_bits_padded = probe_bytes * 8,
        packed = packed.join(", "),
        depth_bits = link.depth_bits,
        depth = 1usize << link.depth_bits,
        divider = divider as u64,
        clock = clock.name,
    );

    let mut edited = String::with_capacity(source.len() + ANALYZER_SOURCE.len() + 512);
    edited.push_str(&source[..ports_end]);
    edited.push_str(&port);
    edited.push_str(&source[ports_end..impl_end]);
    edited.push_str(&instance);
    edited.push_str(&source[impl_end..]);
    edited.push_str("\n\n");
    edited.push_str(ANALYZER_SOURCE);

    let depth = 1usize << link.depth_bits;
    let map = ProbeMap {
        design: top.name.clone(),
        baud: link.baud,
        clock_hz: link.clock_hz,
        depth,
        probe_bytes,
        trigger_index: depth / 2,
        trigger,
        probes,
    };
    Ok((edited, map))
}

/// Find a capture in a UART stream and split it into samples.
pub fn parse_capture(stream: &[u8], map: &ProbeMap) -> Result<Vec<Vec<u8>>> {
    let header = [
        0xA5,
        0x5A,
        map.probe_bytes as u8,
        map.depth.trailing_zeros() as u8,
    ];
    let start = stream
        .windows(header.len())
        .position(|w| w == header)
        .ok_or_else(|| anyhow!("No capture header in {} bytes from the board", stream.len()))?;
    let body = &stream[start + header.len()..];
    let needed = map.depth * map.probe_bytes;
    if body.len() < needed {
        bail!(
            "The capture is incomplete: {} of {} bytes arrived",
            body.len(),
            needed
        );
    }
    Ok(body[..needed]
        .chunks(map.probe_bytes)
        .map(<[u8]>::to_vec)
        .collect())
}

/// The value of one probe in a sample, as little-endian bytes.
pub fn probe_value(sample: &[u8], probe: &Probe) -> Vec<u8> {
    let mut value = vec![0u8; probe.width.div_ceil(8)];
    for bit in 0..probe.width {
        let source = probe.offset + bit;
        if (sample[source / 8] >> (source % 8)) & 1 == 1 {
            value[bit / 8] |= 1 << (bit % 8);
        }
    }
    value
}

/// Read the probe map a debug build wrote.
pub fn read_probe_map(dir: &Path) -> Result<ProbeMap> {
    let path = dir.join(PROBE_MAP_FILE);
    let text = std::fs::read_to_string(&path).with_context(|| {
        format!(
            "No probe map at {}. Build with skalp synth --debug first",
            path.display()
        )
    })?;
    serde_json::from_str(&text).with_context(|| format!("Cannot parse {}", path.display()))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn map(probes: Vec<Probe>, probe_bytes: usize) -> ProbeMap {
        ProbeMap {
            design: "t".to_string(),
            baud: BAUD,
            clock_hz: 18e6,
            depth: 2,
            probe_bytes,
            trigger_index: 1,
            trigger: String::new(),
            probes,
        }
    }

    #[test]
    fn captures_split_into_samples_after_the_header() {
        let probe = Probe {
            name: "a".to_string(),
            width: 12,
            offset: 0,
            radix: None,
            group: None,
        };
        let map = map(vec![probe], 2);
        let stream = [0x00, 0xA5, 0x5A, 2, 1, 0x34, 0x12, 0x78, 0x56, 0xFF];
        let samples = parse_capture(&stream, &map).unwrap();
        assert_eq!(samples, vec![vec![0x34, 0x12], vec![0x78, 0x56]]);
    }

    #[test]
    fn probes_are_cut_from_their_bit_offsets() {
        let probe = Probe {
            name: "b".to_string(),
            width: 4,
            offset: 6,
            radix: None,
            group: None,
        };
        assert_eq!(
            probe_value(&[0b1100_0000, 0b0000_0011], &probe),
            vec![0b1111]
        );
    }
}
