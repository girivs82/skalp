//! Hard-block inputs a design must tie to a constant, from the device data.
//!
//! Some hard blocks have fabric inputs that the vendor's primitive does not
//! expose and that the vendor's tools tie to a constant. A design built
//! without them must do the same: an unconnected fabric input reads 1, which
//! can leave the block misbehaving (on the LFCPNX, an ADC with `RSVDH`/`RSVDL`
//! high converts at mid-scale whatever it measures).
//!
//! These are device facts, so they live in the device data, not here:
//! `<family>/tied_inputs.json` maps a block type as the device data names it
//! (`ADC_CORE`) to its tied pins, each with the constant (`"value": 0` or
//! `1`) and any evidence fields. Keys starting with `_` are comments. The
//! backend drives the pins tied low from a constant-zero LUT; a pin tied
//! high needs nothing, since an unconnected input reads 1.

use std::collections::BTreeMap;
use std::path::Path;

/// The device data file, in the family's directory.
const FILE: &str = "tied_inputs.json";

/// One tied input, as the device data describes it.
#[derive(serde::Deserialize)]
struct TiedInput {
    /// The constant the vendor's tools tie it to
    value: u8,
}

/// The pins of each block type that must be driven low, by block type, each
/// list in name order. A family without the file has none.
pub fn low_pins(db_root: &Path, family: &str) -> Result<BTreeMap<String, Vec<String>>, String> {
    let path = db_root.join(family).join(FILE);
    let text = match std::fs::read_to_string(&path) {
        Ok(text) => text,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(BTreeMap::new()),
        Err(e) => return Err(format!("{}: {e}", path.display())),
    };
    let file: BTreeMap<String, serde_json::Value> =
        serde_json::from_str(&text).map_err(|e| format!("{}: {e}", path.display()))?;
    let mut blocks = BTreeMap::new();
    for (block, pins) in file {
        if block.starts_with('_') {
            continue;
        }
        let pins: BTreeMap<String, TiedInput> = serde_json::from_value(pins)
            .map_err(|e| format!("{}: block {block}: {e}", path.display()))?;
        let mut low = Vec::new();
        for (pin, input) in pins {
            match input.value {
                0 => low.push(pin),
                1 => {}
                v => {
                    return Err(format!(
                        "{}: {block} input {pin} is tied to {v}, not 0 or 1",
                        path.display()
                    ))
                }
            }
        }
        blocks.insert(block, low);
    }
    Ok(blocks)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tied_inputs_come_from_the_device_data() {
        let dir = std::env::temp_dir().join(format!("skalp_tied_{}", std::process::id()));
        std::fs::create_dir_all(dir.join("FAM")).unwrap();
        std::fs::write(
            dir.join("FAM/tied_inputs.json"),
            r#"{"_comment": "x", "ADC_CORE": {"RSVDL": {"value": 0, "pin_wire": "w"},
                "RSVDH": {"value": 0}}, "OTHER_CORE": {"X": {"value": 1}, "Y": {"value": 0}}}"#,
        )
        .unwrap();
        let blocks = low_pins(&dir, "FAM").unwrap();
        assert_eq!(blocks["ADC_CORE"], ["RSVDH", "RSVDL"]);
        assert_eq!(blocks["OTHER_CORE"], ["Y"]);
        assert!(!blocks.contains_key("_comment"));
        assert!(low_pins(&dir, "NONE").unwrap().is_empty());

        std::fs::write(
            dir.join("FAM/tied_inputs.json"),
            r#"{"ADC_CORE": {"RSVDH": {"value": 2}}}"#,
        )
        .unwrap();
        assert!(low_pins(&dir, "FAM").unwrap_err().contains("not 0 or 1"));

        std::fs::remove_dir_all(&dir).unwrap();
    }
}
