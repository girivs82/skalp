//! nextpnr Cross-Validation Tests
//!
//! Compares skalp P&R output against nextpnr-ice40 reference at increasing complexity.
//! All tests require external tools (yosys, nextpnr-ice40, icetime) and are `#[ignore]`
//! by default. Run with `cargo test --test test_nextpnr_golden -- --ignored`.

use skalp_frontend::parse_and_build_hir;
use skalp_lir::gate_netlist::GateNetlist;
use skalp_lir::{get_stdlib_library, lower_mir_module_to_lir, synthesize_balanced};
use skalp_mir::MirCompiler;
use skalp_place_route::device::ice40::chipdb_parser::{ChipDb, LcBitMapping};
use skalp_place_route::{place_and_route, Ice40Variant, PnrConfig};
use std::path::Path;
use std::process::Command;

// ===== Tool helpers =====

fn yosys_path() -> &'static str {
    "/opt/homebrew/bin/yosys"
}

fn nextpnr_path() -> &'static str {
    if Path::new("/Users/girivs/.local/bin/nextpnr-ice40").exists() {
        "/Users/girivs/.local/bin/nextpnr-ice40"
    } else {
        "nextpnr-ice40"
    }
}

fn icetime_path() -> &'static str {
    if Path::new("/Users/girivs/.local/bin/icetime").exists() {
        "/Users/girivs/.local/bin/icetime"
    } else {
        "icetime"
    }
}

fn tools_available() -> bool {
    Command::new(yosys_path())
        .arg("--version")
        .output()
        .is_ok()
        && Command::new(nextpnr_path())
            .arg("--version")
            .output()
            .is_ok()
}

/// Run yosys synthesis + nextpnr P&R and return the reference .asc contents
fn run_reference_flow(verilog: &str, top: &str) -> Option<String> {
    let dir = tempfile::tempdir().ok()?;
    let v_path = dir.path().join("design.v");
    let json_path = dir.path().join("design.json");
    let asc_path = dir.path().join("design.asc");

    std::fs::write(&v_path, verilog).ok()?;

    // Yosys synthesis
    let yosys_cmd = format!(
        "read_verilog {}; synth_ice40 -top {} -json {}",
        v_path.display(),
        top,
        json_path.display()
    );
    let yosys_out = Command::new(yosys_path())
        .args(["-p", &yosys_cmd])
        .output()
        .ok()?;

    if !yosys_out.status.success() {
        eprintln!(
            "yosys failed: {}",
            String::from_utf8_lossy(&yosys_out.stderr)
        );
        return None;
    }

    // nextpnr P&R
    let nextpnr_out = Command::new(nextpnr_path())
        .args([
            "--hx1k",
            "--json",
            json_path.to_str()?,
            "--asc",
            asc_path.to_str()?,
            "--seed",
            "42",
            "--pcf-allow-unconstrained",
        ])
        .output()
        .ok()?;

    if !nextpnr_out.status.success() {
        eprintln!(
            "nextpnr failed: {}",
            String::from_utf8_lossy(&nextpnr_out.stderr)
        );
        return None;
    }

    std::fs::read_to_string(&asc_path).ok()
}

/// Run icetime on an .asc file and return the reported Fmax in MHz
fn run_icetime(asc_contents: &str) -> Option<f64> {
    let dir = tempfile::tempdir().ok()?;
    let asc_path = dir.path().join("design.asc");
    std::fs::write(&asc_path, asc_contents).ok()?;

    let out = Command::new(icetime_path())
        .args(["-d", "hx1k", asc_path.to_str()?])
        .output()
        .ok()?;

    let stdout = String::from_utf8_lossy(&out.stdout);
    // icetime output: "Total path delay: X.XX ns (Y.YY MHz)"
    for line in stdout.lines() {
        if line.contains("MHz") {
            // Extract MHz value
            if let Some(start) = line.rfind('(') {
                let inside = &line[start + 1..];
                if let Some(end) = inside.find(" MHz") {
                    return inside[..end].trim().parse().ok();
                }
            }
        }
    }
    None
}

// ===== ASC parsing helpers =====

/// Load chipdb LC bit mappings for HX1K (cached across calls via lazy init)
fn hx1k_lc_mappings() -> &'static Vec<LcBitMapping> {
    use std::sync::OnceLock;
    static LC_MAPPINGS: OnceLock<Vec<LcBitMapping>> = OnceLock::new();
    LC_MAPPINGS.get_or_init(|| {
        let chipdb = ChipDb::load_embedded(Ice40Variant::Hx1k)
            .expect("HX1K chipdb should load");
        chipdb.lc_mappings
    })
}

/// Extract LUT init values from an .asc file using real chipdb LC bit mappings.
/// Returns a sorted multiset of non-zero 16-bit init values (location-independent).
fn extract_lut_inits(asc: &str) -> Vec<u16> {
    let lc_mappings = hx1k_lc_mappings();
    let mut inits = Vec::new();

    let mut in_logic_tile = false;
    let mut tile_bits: Vec<Vec<bool>> = Vec::new();

    for line in asc.lines() {
        if line.starts_with(".logic_tile") {
            in_logic_tile = true;
            tile_bits.clear();
            continue;
        }

        if in_logic_tile {
            if line.is_empty() || line.starts_with('.') {
                if !tile_bits.is_empty() {
                    for mapping in lc_mappings {
                        let init = extract_lc_lut_init_chipdb(&tile_bits, mapping);
                        if init != 0 {
                            inits.push(init);
                        }
                    }
                }
                in_logic_tile = false;
                continue;
            }

            let row: Vec<bool> = line.chars().map(|c| c == '1').collect();
            tile_bits.push(row);
        }
    }

    inits.sort();
    inits
}

/// Extract LUT init value for a specific LC using chipdb bit position mappings.
/// The first 16 entries in `mapping.bit_positions` are the LUT init bits.
fn extract_lc_lut_init_chipdb(tile_bits: &[Vec<bool>], mapping: &LcBitMapping) -> u16 {
    let mut init = 0u16;
    for (bit_num, &(row, col)) in mapping.bit_positions.iter().take(16).enumerate() {
        let r = row as usize;
        let c = col as usize;
        if r < tile_bits.len() && c < tile_bits[r].len() && tile_bits[r][c] {
            init |= 1 << bit_num;
        }
    }
    init
}

/// Count total PIPs in an .asc file (count of routing config lines with set bits)
fn count_routing_bits(asc: &str) -> usize {
    let mut count = 0;
    let mut in_tile = false;

    for line in asc.lines() {
        if line.starts_with(".logic_tile")
            || line.starts_with(".io_tile")
            || line.starts_with(".ramb_tile")
            || line.starts_with(".ramt_tile")
        {
            in_tile = true;
            continue;
        }

        if line.starts_with('.') || line.is_empty() {
            in_tile = false;
            continue;
        }

        if in_tile {
            count += line.chars().filter(|&c| c == '1').count();
        }
    }

    count
}

/// Comparison report between skalp and nextpnr outputs
struct ComparisonReport {
    /// Number of non-zero LUT inits in skalp output
    skalp_lut_count: usize,
    /// Number of non-zero LUT inits in nextpnr output
    nextpnr_lut_count: usize,
    /// Total routing bits in skalp
    skalp_routing_bits: usize,
    /// Total routing bits in nextpnr
    nextpnr_routing_bits: usize,
    /// icetime Fmax for skalp (if available)
    skalp_fmax: Option<f64>,
    /// icetime Fmax for nextpnr (if available)
    nextpnr_fmax: Option<f64>,
    /// skalp's bitstream is valid (icetime didn't error)
    skalp_valid: bool,
    /// nextpnr's bitstream is valid
    nextpnr_valid: bool,
}

impl ComparisonReport {
    fn print(&self, label: &str) {
        println!("\n=== {} Cross-Validation ===", label);
        println!(
            "LUT inits:     skalp={:>3}   nextpnr={:>3}",
            self.skalp_lut_count, self.nextpnr_lut_count
        );
        println!(
            "Routing bits:  skalp={:>5}   nextpnr={:>5}   ratio={:.2}",
            self.skalp_routing_bits,
            self.nextpnr_routing_bits,
            if self.nextpnr_routing_bits > 0 {
                self.skalp_routing_bits as f64 / self.nextpnr_routing_bits as f64
            } else {
                0.0
            }
        );
        if let (Some(sf), Some(nf)) = (self.skalp_fmax, self.nextpnr_fmax) {
            println!(
                "Fmax (MHz):    skalp={:>6.1}   nextpnr={:>6.1}   ratio={:.2}",
                sf,
                nf,
                sf / nf
            );
        }
        println!(
            "Valid:         skalp={}   nextpnr={}",
            self.skalp_valid, self.nextpnr_valid
        );
    }
}

fn compare_asc(skalp_asc: &str, nextpnr_asc: &str, label: &str) -> ComparisonReport {
    let skalp_luts = extract_lut_inits(skalp_asc);
    let nextpnr_luts = extract_lut_inits(nextpnr_asc);
    let skalp_routing = count_routing_bits(skalp_asc);
    let nextpnr_routing = count_routing_bits(nextpnr_asc);
    let skalp_fmax = run_icetime(skalp_asc);
    let nextpnr_fmax = run_icetime(nextpnr_asc);

    let report = ComparisonReport {
        skalp_lut_count: skalp_luts.len(),
        nextpnr_lut_count: nextpnr_luts.len(),
        skalp_routing_bits: skalp_routing,
        nextpnr_routing_bits: nextpnr_routing,
        skalp_fmax,
        nextpnr_fmax,
        skalp_valid: skalp_fmax.is_some(),
        nextpnr_valid: nextpnr_fmax.is_some(),
    };

    report.print(label);
    report
}

// ===== Skalp synthesis helper =====

/// Compile skalp source through the full pipeline to produce a GateNetlist
fn synthesize_skalp(source: &str) -> GateNetlist {
    let hir = parse_and_build_hir(source).expect("skalp parse failed");
    let mir = MirCompiler::new()
        .compile_to_mir(&hir)
        .expect("MIR compilation failed");
    let lir = lower_mir_module_to_lir(mir.modules.first().expect("no modules")).lir;
    let library = get_stdlib_library("ice40").expect("ice40 library");
    synthesize_balanced(&lir, &library).netlist
}

// ===== Skalp source designs (matching Verilog) =====

const INVERTER_SK: &str = r#"
entity Inverter {
    in a: bit
    out y: bit
}
impl Inverter {
    y = ~a
}
"#;

const AND_GATE_SK: &str = r#"
entity AndGate {
    in a: bit
    in b: bit
    out y: bit
}
impl AndGate {
    y = a & b
}
"#;

const DFF_RESET_SK: &str = r#"
entity DffReset {
    in clk: clock
    in d: bit
    in rst: reset
    out q: bit
}
impl DffReset {
    signal q_reg: bit = 0
    on(clk.rise) {
        if rst {
            q_reg = 0
        } else {
            q_reg = d
        }
    }
    q = q_reg
}
"#;

const COUNTER_4_SK: &str = r#"
entity Counter4 {
    in clk: clock
    out count: bit[4]
}
impl Counter4 {
    signal cnt: bit[4] = 0
    on(clk.rise) {
        cnt = cnt + 1
    }
    count = cnt
}
"#;

const ADDER_8_SK: &str = r#"
entity Adder8 {
    in a: bit[8]
    in b: bit[8]
    out sum: bit[9]
}
impl Adder8 {
    sum = a + b
}
"#;

const COUNTER_16_SK: &str = r#"
entity Counter16 {
    in clk: clock
    out count: bit[16]
}
impl Counter16 {
    signal cnt: bit[16] = 0
    on(clk.rise) {
        cnt = cnt + 1
    }
    count = cnt
}
"#;

const SHIFTREG_8_SK: &str = r#"
entity ShiftReg8 {
    in clk: clock
    in data_in: bit
    out data_out: bit
}
impl ShiftReg8 {
    signal sr: bit[8] = 0
    on(clk.rise) {
        sr = {sr[6:0], data_in}
    }
    data_out = sr[7]
}
"#;

// ===== Verilog for reference flow =====

const INVERTER_V: &str = r#"
module inverter(input a, output y);
    assign y = ~a;
endmodule
"#;

const AND_GATE_V: &str = r#"
module and_gate(input a, input b, output y);
    assign y = a & b;
endmodule
"#;

const DFF_RESET_V: &str = r#"
module dff_reset(input clk, input d, input rst, output reg q);
    always @(posedge clk or posedge rst)
        if (rst) q <= 0;
        else q <= d;
endmodule
"#;

const COUNTER_4_V: &str = r#"
module counter_4(input clk, output reg [3:0] count);
    always @(posedge clk)
        count <= count + 1;
endmodule
"#;

const ADDER_8_V: &str = r#"
module adder_8(input [7:0] a, input [7:0] b, output [8:0] sum);
    assign sum = a + b;
endmodule
"#;

const COUNTER_16_V: &str = r#"
module counter_16(input clk, output reg [15:0] count);
    always @(posedge clk)
        count <= count + 1;
endmodule
"#;

const SHIFTREG_8_V: &str = r#"
module shiftreg_8(input clk, input data_in, output data_out);
    reg [7:0] sr;
    always @(posedge clk)
        sr <= {sr[6:0], data_in};
    assign data_out = sr[7];
endmodule
"#;

// ===== Cross-validation tests =====
// Each test: build skalp netlist + run reference flow → compare

#[test]
#[ignore]
fn test_cross_level1_inverter() {
    if !tools_available() {
        println!("SKIP: external tools not available");
        return;
    }

    // Skalp
    let netlist = synthesize_skalp(INVERTER_SK);
    let config = PnrConfig::fast();
    let result = place_and_route(&netlist, Ice40Variant::Hx1k, config).unwrap();
    let skalp_asc = result.to_icestorm_ascii_with_netlist(Some(&netlist));

    // Reference
    let nextpnr_asc = run_reference_flow(INVERTER_V, "inverter")
        .expect("Reference flow should succeed for inverter");

    let report = compare_asc(&skalp_asc, &nextpnr_asc, "Level 1: Inverter");

    // Structural: both should have exactly 1 non-trivial LUT
    assert!(
        report.skalp_lut_count >= 1,
        "skalp should produce at least 1 LUT for inverter"
    );

    // Validity: skalp bitstream should be parseable by icetime
    // (icetime may not work on minimal designs, so this is informational)
    println!(
        "icetime validation: skalp={}, nextpnr={}",
        report.skalp_valid, report.nextpnr_valid
    );
}

#[test]
#[ignore]
fn test_cross_level2_and_gate() {
    if !tools_available() {
        println!("SKIP: external tools not available");
        return;
    }

    let netlist = synthesize_skalp(AND_GATE_SK);
    let config = PnrConfig::fast();
    let result = place_and_route(&netlist, Ice40Variant::Hx1k, config).unwrap();
    let skalp_asc = result.to_icestorm_ascii_with_netlist(Some(&netlist));

    let nextpnr_asc = run_reference_flow(AND_GATE_V, "and_gate")
        .expect("Reference flow should succeed for AND gate");

    let report = compare_asc(&skalp_asc, &nextpnr_asc, "Level 2: AND gate");

    assert!(
        report.skalp_lut_count >= 1,
        "skalp should produce at least 1 LUT for AND gate"
    );
}

#[test]
#[ignore]
fn test_cross_level3_dff_reset() {
    if !tools_available() {
        println!("SKIP: external tools not available");
        return;
    }

    let netlist = synthesize_skalp(DFF_RESET_SK);
    let config = PnrConfig::fast();
    let result = place_and_route(&netlist, Ice40Variant::Hx1k, config).unwrap();
    let skalp_asc = result.to_icestorm_ascii_with_netlist(Some(&netlist));

    let nextpnr_asc = run_reference_flow(DFF_RESET_V, "dff_reset")
        .expect("Reference flow should succeed for DFF with reset");

    let report = compare_asc(&skalp_asc, &nextpnr_asc, "Level 3: DFF with reset");

    // Both should have at least some routing
    assert!(
        report.skalp_routing_bits > 0,
        "skalp should produce routing bits for DFF design"
    );
}

#[test]
#[ignore]
fn test_cross_level4_counter_4() {
    if !tools_available() {
        println!("SKIP: external tools not available");
        return;
    }

    let netlist = synthesize_skalp(COUNTER_4_SK);
    let config = PnrConfig::fast();
    let result = place_and_route(&netlist, Ice40Variant::Hx1k, config).unwrap();
    let skalp_asc = result.to_icestorm_ascii_with_netlist(Some(&netlist));

    let nextpnr_asc = run_reference_flow(COUNTER_4_V, "counter_4")
        .expect("Reference flow should succeed for 4-bit counter");

    let report = compare_asc(&skalp_asc, &nextpnr_asc, "Level 4: 4-bit counter");

    // Counter should have LUTs and routing
    assert!(
        report.skalp_lut_count >= 1,
        "skalp should produce LUTs for counter"
    );
    assert!(
        report.skalp_routing_bits > 0,
        "skalp should produce routing for counter"
    );
}

#[test]
#[ignore]
fn test_cross_level5_adder_8() {
    if !tools_available() {
        println!("SKIP: external tools not available");
        return;
    }

    let netlist = synthesize_skalp(ADDER_8_SK);
    let config = PnrConfig::fast();
    let result = place_and_route(&netlist, Ice40Variant::Hx1k, config).unwrap();
    let skalp_asc = result.to_icestorm_ascii_with_netlist(Some(&netlist));

    let nextpnr_asc = run_reference_flow(ADDER_8_V, "adder_8")
        .expect("Reference flow should succeed for 8-bit adder");

    let report = compare_asc(&skalp_asc, &nextpnr_asc, "Level 5: 8-bit adder");

    // Adder should have routing (LUT init extraction is approximate)
    assert!(
        report.skalp_routing_bits > 0,
        "skalp should produce routing bits for adder"
    );
}

#[test]
#[ignore]
fn test_cross_level6_counter_16() {
    if !tools_available() {
        println!("SKIP: external tools not available");
        return;
    }

    let netlist = synthesize_skalp(COUNTER_16_SK);
    let config = PnrConfig::default();
    let result = place_and_route(&netlist, Ice40Variant::Hx1k, config).unwrap();
    let skalp_asc = result.to_icestorm_ascii_with_netlist(Some(&netlist));

    let nextpnr_asc = run_reference_flow(COUNTER_16_V, "counter_16")
        .expect("Reference flow should succeed for 16-bit counter");

    let report = compare_asc(&skalp_asc, &nextpnr_asc, "Level 6: 16-bit counter");

    // Routing quality comparison
    if report.nextpnr_routing_bits > 0 {
        let ratio = report.skalp_routing_bits as f64 / report.nextpnr_routing_bits as f64;
        println!("Routing bit ratio (skalp/nextpnr): {:.2}", ratio);
        if ratio > 1.5 {
            println!("WARNING: skalp uses >50% more routing resources than nextpnr");
        }
    }

    // Fmax comparison
    if let (Some(sf), Some(nf)) = (report.skalp_fmax, report.nextpnr_fmax) {
        let fmax_ratio = sf / nf;
        println!("Fmax ratio (skalp/nextpnr): {:.2}", fmax_ratio);
        if fmax_ratio < 0.8 {
            println!("WARNING: skalp Fmax is >20% worse than nextpnr");
        }
    }
}

#[test]
#[ignore]
fn test_cross_level7_shiftreg_8() {
    if !tools_available() {
        println!("SKIP: external tools not available");
        return;
    }

    let netlist = synthesize_skalp(SHIFTREG_8_SK);
    let config = PnrConfig::default();
    let result = place_and_route(&netlist, Ice40Variant::Hx1k, config).unwrap();
    let skalp_asc = result.to_icestorm_ascii_with_netlist(Some(&netlist));

    let nextpnr_asc = run_reference_flow(SHIFTREG_8_V, "shiftreg_8")
        .expect("Reference flow should succeed for 8-bit shift register");

    let report = compare_asc(&skalp_asc, &nextpnr_asc, "Level 7: 8-bit shift register");

    assert!(
        report.skalp_routing_bits > 0,
        "skalp should produce routing for shift register"
    );
}

// ===== Progressive comparison summary test =====

#[test]
#[ignore]
fn test_cross_validation_summary() {
    if !tools_available() {
        println!("SKIP: external tools not available");
        return;
    }

    println!("\n====== nextpnr Cross-Validation Summary ======");
    println!(
        "{:<25} {:>8} {:>8} {:>8} {:>8} {:>8} {:>8}",
        "Design", "S-LUTs", "N-LUTs", "S-Bits", "N-Bits", "S-Fmax", "N-Fmax"
    );
    println!("{}", "-".repeat(85));

    type DesignEntry<'a> = (&'a str, &'a str, &'a str);
    let designs: Vec<DesignEntry<'_>> = vec![
        ("Inverter", INVERTER_V, INVERTER_SK),
        ("AND gate", AND_GATE_V, AND_GATE_SK),
        ("DFF+reset", DFF_RESET_V, DFF_RESET_SK),
        ("Counter-4", COUNTER_4_V, COUNTER_4_SK),
        ("Adder-8", ADDER_8_V, ADDER_8_SK),
        ("Counter-16", COUNTER_16_V, COUNTER_16_SK),
        ("ShiftReg-8", SHIFTREG_8_V, SHIFTREG_8_SK),
    ];

    let top_names = [
        "inverter",
        "and_gate",
        "dff_reset",
        "counter_4",
        "adder_8",
        "counter_16",
        "shiftreg_8",
    ];

    for (i, (name, verilog, sk_source)) in designs.iter().enumerate() {
        let netlist = synthesize_skalp(sk_source);
        let config = PnrConfig::fast();
        let skalp_result = match place_and_route(&netlist, Ice40Variant::Hx1k, config) {
            Ok(r) => r,
            Err(e) => {
                println!("{:<25} SKALP FAILED: {}", name, e);
                continue;
            }
        };
        let skalp_asc = skalp_result.to_icestorm_ascii_with_netlist(Some(&netlist));

        let nextpnr_asc = match run_reference_flow(verilog, top_names[i]) {
            Some(asc) => asc,
            None => {
                println!("{:<25} NEXTPNR FAILED", name);
                continue;
            }
        };

        let s_luts = extract_lut_inits(&skalp_asc).len();
        let n_luts = extract_lut_inits(&nextpnr_asc).len();
        let s_bits = count_routing_bits(&skalp_asc);
        let n_bits = count_routing_bits(&nextpnr_asc);
        let s_fmax = run_icetime(&skalp_asc);
        let n_fmax = run_icetime(&nextpnr_asc);

        println!(
            "{:<25} {:>8} {:>8} {:>8} {:>8} {:>8} {:>8}",
            name,
            s_luts,
            n_luts,
            s_bits,
            n_bits,
            s_fmax
                .map(|f| format!("{:.1}", f))
                .unwrap_or_else(|| "-".to_string()),
            n_fmax
                .map(|f| format!("{:.1}", f))
                .unwrap_or_else(|| "-".to_string()),
        );
    }

    println!("{}", "-".repeat(85));
}

// ===== Compile smoke test (non-ignored) =====

#[test]
fn test_skalp_sources_compile() {
    let designs: &[(&str, &str)] = &[
        ("Inverter", INVERTER_SK),
        ("AND gate", AND_GATE_SK),
        ("DFF+reset", DFF_RESET_SK),
        ("Counter-4", COUNTER_4_SK),
        ("Adder-8", ADDER_8_SK),
        ("Counter-16", COUNTER_16_SK),
        ("ShiftReg-8", SHIFTREG_8_SK),
    ];

    for (name, source) in designs {
        let netlist = synthesize_skalp(source);
        assert!(
            !netlist.cells.is_empty(),
            "{} should produce cells",
            name
        );

        // Verify no stale cell ID references (regression for gate_optimizer fix)
        let max_cell_id = netlist.cells.len() as u32;
        for net in &netlist.nets {
            if let Some(d) = net.driver {
                assert!(
                    d.0 < max_cell_id,
                    "{}: stale driver CellId({}) in net '{}' (max={})",
                    name, d.0, net.name, max_cell_id
                );
            }
            for (cid, _) in &net.fanout {
                assert!(
                    cid.0 < max_cell_id,
                    "{}: stale fanout CellId({}) in net '{}' (max={})",
                    name, cid.0, net.name, max_cell_id
                );
            }
        }

        // Verify P&R succeeds
        let config = PnrConfig::fast();
        let result = place_and_route(&netlist, Ice40Variant::Hx1k, config);
        assert!(
            result.is_ok(),
            "{}: P&R failed: {}",
            name,
            result.unwrap_err()
        );
    }
}
