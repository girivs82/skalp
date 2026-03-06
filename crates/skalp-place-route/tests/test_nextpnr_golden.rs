//! nextpnr Cross-Validation Tests
//!
//! Compares skalp P&R output against nextpnr-ice40 reference at increasing complexity.
//! All tests require external tools (yosys, nextpnr-ice40, icetime) and are `#[ignore]`
//! by default. Run with `cargo test --test test_nextpnr_golden -- --ignored`.

use skalp_lir::gate_netlist::{Cell, CellId, GateNet, GateNetId, GateNetlist};
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

/// Extract LUT init values from an .asc file (multiset of init values, location-independent)
fn extract_lut_inits(asc: &str) -> Vec<u16> {
    let mut inits = Vec::new();

    // Parse each logic tile and extract LUT init bits
    // In iCE40, each logic tile has 8 LCs, each with 16-bit LUT init
    // The LUT init bits are stored in specific rows/columns per LC within the tile
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
                // End of tile — extract LUT inits from tile_bits
                if !tile_bits.is_empty() {
                    for lc in 0..8 {
                        let init = extract_lc_lut_init(&tile_bits, lc);
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

/// Extract LUT init value for a specific LC within a logic tile bit array
/// iCE40 LUT init bits are stored in rows 0-15, columns depend on LC index
fn extract_lc_lut_init(tile_bits: &[Vec<bool>], lc: usize) -> u16 {
    // iCE40 logic tile column layout per LC:
    // LC0: cols 45-30 (lut_init[15:0])
    // LC1: cols 29-14
    // ...etc (each LC uses 16 columns for LUT init)
    // The exact mapping varies — use chipdb for precision.
    // For cross-validation we compare multisets, not exact positions.

    let mut init = 0u16;
    // Simplified: check if any bits are set in the LC's region
    // The actual mapping comes from the chipdb lc_mapping
    // For now, check rows across the LC's column range
    let cols_per_lc = 2; // Each LC occupies 2 columns in the 54-col tile
    let base_col = lc * cols_per_lc;

    for bit in 0..16 {
        let row = bit;
        if row < tile_bits.len()
            && base_col < tile_bits[row].len()
            && tile_bits[row][base_col]
        {
            init |= 1 << bit;
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

// ===== Netlist builders (matching Verilog designs) =====

/// Build a single inverter netlist: 1 LUT + 2 IO
fn build_inverter() -> GateNetlist {
    let mut netlist = GateNetlist::new("inverter".to_string(), "ice40".to_string());
    let mut nid = 0u32;
    let mut next = || {
        let id = nid;
        nid += 1;
        id
    };

    let in_net = netlist.add_net(GateNet::new_input(GateNetId(next()), "a".to_string()));
    let out_net = netlist.add_net(GateNet::new_output(GateNetId(next()), "y".to_string()));

    // LUT4 configured as inverter: init = 0x5555 (output = ~input[0])
    let mut lut = Cell::new_comb(
        CellId(0),
        "SB_LUT4".to_string(),
        "ice40".to_string(),
        0.0,
        "top.inv_lut".to_string(),
        vec![in_net],
        vec![out_net],
    );
    lut.lut_init = Some(0x5555);
    let lut_id = netlist.add_cell(lut);
    netlist.nets[out_net.0 as usize].driver = Some(lut_id);
    netlist.nets[in_net.0 as usize].fanout.push((lut_id, 0));

    // Input IO buffer
    let io_in = Cell::new_comb(
        CellId(0),
        "SB_IO".to_string(),
        "ice40".to_string(),
        0.0,
        "io.a".to_string(),
        vec![],
        vec![in_net],
    );
    let io_in_id = netlist.add_cell(io_in);
    netlist.nets[in_net.0 as usize].driver = Some(io_in_id);

    netlist
}

/// Build a 2-input AND gate: 1 LUT + 3 IO
fn build_and_gate() -> GateNetlist {
    let mut netlist = GateNetlist::new("and_gate".to_string(), "ice40".to_string());
    let mut nid = 0u32;
    let mut next = || {
        let id = nid;
        nid += 1;
        id
    };

    let a_net = netlist.add_net(GateNet::new_input(GateNetId(next()), "a".to_string()));
    let b_net = netlist.add_net(GateNet::new_input(GateNetId(next()), "b".to_string()));
    let y_net = netlist.add_net(GateNet::new_output(GateNetId(next()), "y".to_string()));

    // LUT4 configured as AND: init = 0x8888 (output = a & b)
    let mut lut = Cell::new_comb(
        CellId(0),
        "SB_LUT4".to_string(),
        "ice40".to_string(),
        0.0,
        "top.and_lut".to_string(),
        vec![a_net, b_net],
        vec![y_net],
    );
    lut.lut_init = Some(0x8888);
    let lut_id = netlist.add_cell(lut);
    netlist.nets[y_net.0 as usize].driver = Some(lut_id);
    netlist.nets[a_net.0 as usize].fanout.push((lut_id, 0));
    netlist.nets[b_net.0 as usize].fanout.push((lut_id, 1));

    // Input IO buffers
    let io_a = Cell::new_comb(
        CellId(0),
        "SB_IO".to_string(),
        "ice40".to_string(),
        0.0,
        "io.a".to_string(),
        vec![],
        vec![a_net],
    );
    let io_a_id = netlist.add_cell(io_a);
    netlist.nets[a_net.0 as usize].driver = Some(io_a_id);

    let io_b = Cell::new_comb(
        CellId(0),
        "SB_IO".to_string(),
        "ice40".to_string(),
        0.0,
        "io.b".to_string(),
        vec![],
        vec![b_net],
    );
    let io_b_id = netlist.add_cell(io_b);
    netlist.nets[b_net.0 as usize].driver = Some(io_b_id);

    netlist
}

/// Build a D flip-flop with async reset: 1 DFF + 2 LUT + IO
fn build_dff_with_reset() -> GateNetlist {
    let mut netlist = GateNetlist::new("dff_reset".to_string(), "ice40".to_string());
    let mut nid = 0u32;
    let mut next = || {
        let id = nid;
        nid += 1;
        id
    };

    let clk_net = netlist.add_net({
        let mut net = GateNet::new_input(GateNetId(next()), "clk".to_string());
        net.is_clock = true;
        net
    });
    let d_net = netlist.add_net(GateNet::new_input(GateNetId(next()), "d".to_string()));
    let rst_net = netlist.add_net(GateNet::new_input(GateNetId(next()), "rst".to_string()));
    let q_net = netlist.add_net(GateNet::new_output(GateNetId(next()), "q".to_string()));
    let lut_out = netlist.add_net(GateNet::new(GateNetId(next()), "lut_out".to_string()));

    // LUT passes data through (buffer): init = 0xAAAA
    let mut buf_lut = Cell::new_comb(
        CellId(0),
        "SB_LUT4".to_string(),
        "ice40".to_string(),
        0.0,
        "top.buf_lut".to_string(),
        vec![d_net],
        vec![lut_out],
    );
    buf_lut.lut_init = Some(0xAAAA);
    let lut_id = netlist.add_cell(buf_lut);
    netlist.nets[lut_out.0 as usize].driver = Some(lut_id);
    netlist.nets[d_net.0 as usize].fanout.push((lut_id, 0));

    // DFF with async reset (SB_DFFR)
    let mut dff = Cell::new_seq(
        CellId(0),
        "SB_DFFR".to_string(),
        "ice40".to_string(),
        0.0,
        "top.dff0".to_string(),
        vec![lut_out],
        vec![q_net],
        clk_net,
        Some(rst_net),
    );
    dff.clock = Some(clk_net);
    let dff_id = netlist.add_cell(dff);
    netlist.nets[q_net.0 as usize].driver = Some(dff_id);
    netlist.nets[lut_out.0 as usize].fanout.push((dff_id, 0));

    // IO cells
    let io_clk = Cell::new_comb(
        CellId(0),
        "SB_IO".to_string(),
        "ice40".to_string(),
        0.0,
        "io.clk".to_string(),
        vec![],
        vec![clk_net],
    );
    let io_clk_id = netlist.add_cell(io_clk);
    netlist.nets[clk_net.0 as usize].driver = Some(io_clk_id);

    netlist
}

/// Build a 4-bit counter (blinky): 4 LUTs + 4 DFFs + 4 carry + clock + IO
fn build_blinky() -> GateNetlist {
    build_counter(4)
}

/// Build an N-bit counter: N LUTs + N DFFs + N carry cells + clock + IO
fn build_counter(n: usize) -> GateNetlist {
    let mut netlist = GateNetlist::new(format!("counter_{}", n), "ice40".to_string());
    let mut nid = 0u32;
    let mut next = || {
        let id = nid;
        nid += 1;
        id
    };

    // Clock net
    let clock_net = netlist.add_net({
        let mut net = GateNet::new_input(GateNetId(next()), "clk".to_string());
        net.is_clock = true;
        net
    });

    // Carry chain
    let carry_in = netlist.add_net(GateNet::new(GateNetId(next()), "carry_in".to_string()));
    let mut carry_nets = vec![carry_in];
    for i in 0..n {
        carry_nets.push(netlist.add_net(GateNet::new(
            GateNetId(next()),
            format!("carry_{}", i),
        )));
    }

    // DFF feedback and LUT output nets
    let mut dff_out_nets = Vec::new();
    let mut lut_out_nets = Vec::new();
    for i in 0..n {
        dff_out_nets.push(netlist.add_net(GateNet::new(
            GateNetId(next()),
            format!("q_{}", i),
        )));
        lut_out_nets.push(netlist.add_net(GateNet::new(
            GateNetId(next()),
            format!("lut_{}", i),
        )));
    }

    // Build counter stages
    for i in 0..n {
        // LUT: XOR toggle
        let mut lut = Cell::new_comb(
            CellId(0),
            "SB_LUT4".to_string(),
            "ice40".to_string(),
            0.0,
            format!("cnt.lut{}", i),
            vec![dff_out_nets[i], carry_nets[i]],
            vec![lut_out_nets[i]],
        );
        lut.lut_init = Some(0x6666); // XOR
        let lut_id = netlist.add_cell(lut);
        netlist.nets[lut_out_nets[i].0 as usize].driver = Some(lut_id);
        netlist.nets[dff_out_nets[i].0 as usize]
            .fanout
            .push((lut_id, 0));
        netlist.nets[carry_nets[i].0 as usize]
            .fanout
            .push((lut_id, 1));

        // CARRY
        let carry = Cell::new_comb(
            CellId(0),
            "SB_CARRY".to_string(),
            "ice40".to_string(),
            0.0,
            format!("cnt.carry{}", i),
            vec![lut_out_nets[i], carry_nets[i]],
            vec![carry_nets[i + 1]],
        );
        let carry_id = netlist.add_cell(carry);
        netlist.nets[carry_nets[i + 1].0 as usize].driver = Some(carry_id);
        netlist.nets[lut_out_nets[i].0 as usize]
            .fanout
            .push((carry_id, 0));
        netlist.nets[carry_nets[i].0 as usize]
            .fanout
            .push((carry_id, 1));

        // DFF
        let mut dff = Cell::new_seq(
            CellId(0),
            "SB_DFF".to_string(),
            "ice40".to_string(),
            0.0,
            format!("cnt.dff{}", i),
            vec![lut_out_nets[i]],
            vec![dff_out_nets[i]],
            clock_net,
            None,
        );
        dff.clock = Some(clock_net);
        let dff_id = netlist.add_cell(dff);
        netlist.nets[dff_out_nets[i].0 as usize].driver = Some(dff_id);
        netlist.nets[lut_out_nets[i].0 as usize]
            .fanout
            .push((dff_id, 0));
    }

    // Clock IO buffer
    let clk_io = Cell::new_comb(
        CellId(0),
        "SB_IO".to_string(),
        "ice40".to_string(),
        0.0,
        "io.clk".to_string(),
        vec![],
        vec![clock_net],
    );
    let clk_io_id = netlist.add_cell(clk_io);
    netlist.nets[clock_net.0 as usize].driver = Some(clk_io_id);

    netlist
}

/// Build an 8-bit adder with IO
fn build_adder_with_io(n: usize) -> GateNetlist {
    let mut netlist = GateNetlist::new(format!("adder_{}", n), "ice40".to_string());
    let mut nid = 0u32;
    let mut next = || {
        let id = nid;
        nid += 1;
        id
    };

    // Input nets
    let mut a_nets = Vec::new();
    let mut b_nets = Vec::new();
    for i in 0..n {
        a_nets.push(netlist.add_net(GateNet::new_input(
            GateNetId(next()),
            format!("a_{}", i),
        )));
        b_nets.push(netlist.add_net(GateNet::new_input(
            GateNetId(next()),
            format!("b_{}", i),
        )));
    }

    // Carry chain
    let carry_in = netlist.add_net(GateNet::new(GateNetId(next()), "cin".to_string()));
    let mut carry_nets = vec![carry_in];
    for i in 0..n {
        carry_nets.push(netlist.add_net(GateNet::new(
            GateNetId(next()),
            format!("carry_{}", i),
        )));
    }

    // Sum outputs
    let mut sum_nets = Vec::new();
    for i in 0..n {
        sum_nets.push(netlist.add_net(GateNet::new_output(
            GateNetId(next()),
            format!("sum_{}", i),
        )));
    }

    // Build adder stages
    for i in 0..n {
        // LUT: XOR for sum
        let mut lut = Cell::new_comb(
            CellId(0),
            "SB_LUT4".to_string(),
            "ice40".to_string(),
            0.0,
            format!("add.lut{}", i),
            vec![a_nets[i], b_nets[i], carry_nets[i]],
            vec![sum_nets[i]],
        );
        lut.lut_init = Some(0x9696); // a ^ b ^ cin
        let lut_id = netlist.add_cell(lut);
        netlist.nets[sum_nets[i].0 as usize].driver = Some(lut_id);
        netlist.nets[a_nets[i].0 as usize].fanout.push((lut_id, 0));
        netlist.nets[b_nets[i].0 as usize].fanout.push((lut_id, 1));
        netlist.nets[carry_nets[i].0 as usize]
            .fanout
            .push((lut_id, 2));

        // CARRY
        let carry = Cell::new_comb(
            CellId(0),
            "SB_CARRY".to_string(),
            "ice40".to_string(),
            0.0,
            format!("add.carry{}", i),
            vec![a_nets[i], b_nets[i], carry_nets[i]],
            vec![carry_nets[i + 1]],
        );
        let carry_id = netlist.add_cell(carry);
        netlist.nets[carry_nets[i + 1].0 as usize].driver = Some(carry_id);
        netlist.nets[a_nets[i].0 as usize]
            .fanout
            .push((carry_id, 0));
        netlist.nets[b_nets[i].0 as usize]
            .fanout
            .push((carry_id, 1));
        netlist.nets[carry_nets[i].0 as usize]
            .fanout
            .push((carry_id, 2));
    }

    netlist
}

/// Build an 8-bit shift register
fn build_shift_register(n: usize) -> GateNetlist {
    let mut netlist = GateNetlist::new(format!("shiftreg_{}", n), "ice40".to_string());
    let mut nid = 0u32;
    let mut next = || {
        let id = nid;
        nid += 1;
        id
    };

    let clock_net = netlist.add_net({
        let mut net = GateNet::new_input(GateNetId(next()), "clk".to_string());
        net.is_clock = true;
        net
    });

    let data_in = netlist.add_net(GateNet::new_input(
        GateNetId(next()),
        "data_in".to_string(),
    ));

    let mut dff_out_nets = Vec::new();
    for i in 0..n {
        dff_out_nets.push(netlist.add_net(GateNet::new(
            GateNetId(next()),
            format!("q_{}", i),
        )));
    }

    let mut prev_net = data_in;
    #[allow(clippy::needless_range_loop)]
    for i in 0..n {
        let mut dff = Cell::new_seq(
            CellId(0),
            "SB_DFF".to_string(),
            "ice40".to_string(),
            0.0,
            format!("sr.dff{}", i),
            vec![prev_net],
            vec![dff_out_nets[i]],
            clock_net,
            None,
        );
        dff.clock = Some(clock_net);
        let dff_id = netlist.add_cell(dff);
        netlist.nets[dff_out_nets[i].0 as usize].driver = Some(dff_id);
        netlist.nets[prev_net.0 as usize].fanout.push((dff_id, 0));
        prev_net = dff_out_nets[i];
    }

    // Clock IO
    let clk_io = Cell::new_comb(
        CellId(0),
        "SB_IO".to_string(),
        "ice40".to_string(),
        0.0,
        "io.clk".to_string(),
        vec![],
        vec![clock_net],
    );
    let clk_io_id = netlist.add_cell(clk_io);
    netlist.nets[clock_net.0 as usize].driver = Some(clk_io_id);

    netlist
}

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
    let netlist = build_inverter();
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

    let netlist = build_and_gate();
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

    let netlist = build_dff_with_reset();
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

    let netlist = build_blinky();
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

    let netlist = build_adder_with_io(8);
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

    let netlist = build_counter(16);
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

    let netlist = build_shift_register(8);
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

    type DesignEntry<'a> = (&'a str, &'a str, Box<dyn Fn() -> GateNetlist>);
    let designs: Vec<DesignEntry<'_>> = vec![
        ("Inverter", INVERTER_V, Box::new(build_inverter)),
        ("AND gate", AND_GATE_V, Box::new(build_and_gate)),
        (
            "DFF+reset",
            DFF_RESET_V,
            Box::new(build_dff_with_reset),
        ),
        ("Counter-4", COUNTER_4_V, Box::new(build_blinky)),
        (
            "Adder-8",
            ADDER_8_V,
            Box::new(|| build_adder_with_io(8)),
        ),
        ("Counter-16", COUNTER_16_V, Box::new(|| build_counter(16))),
        (
            "ShiftReg-8",
            SHIFTREG_8_V,
            Box::new(|| build_shift_register(8)),
        ),
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

    for (i, (name, verilog, builder)) in designs.iter().enumerate() {
        let netlist = builder();
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
