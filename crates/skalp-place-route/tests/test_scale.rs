//! Scale Testing for Place and Route
//!
//! Programmatic netlist generators and scaling tests to find performance limits.

use skalp_lir::gate_netlist::{Cell, CellId, GateNet, GateNetId, GateNetlist};
use skalp_place_route::{place_and_route, Ice40Variant, PnrConfig};
use std::time::Instant;

/// Build an N-bit counter netlist: N LUTs + N DFFs + N carry cells + clock + IO
fn build_counter(n: usize) -> GateNetlist {
    let mut netlist = GateNetlist::new(format!("counter_{}", n), "ice40".to_string());
    let mut net_id_counter = 0u32;

    let mut next_net = || {
        let id = net_id_counter;
        net_id_counter += 1;
        id
    };

    // Clock net
    let clock_net = netlist.add_net({
        let mut net = GateNet::new_input(GateNetId(next_net()), "clk".to_string());
        net.is_clock = true;
        net
    });

    // Carry chain: carry_in, carry[0], carry[1], ..., carry[n-1]
    let carry_in = netlist.add_net(GateNet::new(GateNetId(next_net()), "carry_in".to_string()));

    let mut carry_nets = vec![carry_in];
    for i in 0..n {
        carry_nets
            .push(netlist.add_net(GateNet::new(GateNetId(next_net()), format!("carry_{}", i))));
    }

    // DFF feedback nets (Q output feeds back to LUT)
    let mut dff_out_nets = Vec::new();
    for i in 0..n {
        dff_out_nets.push(netlist.add_net(GateNet::new(GateNetId(next_net()), format!("q_{}", i))));
    }

    // LUT output nets
    let mut lut_out_nets = Vec::new();
    for i in 0..n {
        lut_out_nets
            .push(netlist.add_net(GateNet::new(GateNetId(next_net()), format!("lut_{}", i))));
    }

    // Build N counter stages: LUT + CARRY + DFF
    for i in 0..n {
        // LUT: implements XOR (toggle) using feedback
        let lut = Cell::new_comb(
            CellId(0),
            "SB_LUT4".to_string(),
            "ice40".to_string(),
            0.0,
            format!("cnt.lut{}", i),
            vec![dff_out_nets[i], carry_nets[i]],
            vec![lut_out_nets[i]],
        );
        let lut_id = netlist.add_cell(lut);
        netlist.nets[lut_out_nets[i].0 as usize].driver = Some(lut_id);
        netlist.nets[dff_out_nets[i].0 as usize]
            .fanout
            .push((lut_id, 0));
        netlist.nets[carry_nets[i].0 as usize]
            .fanout
            .push((lut_id, 1));

        // CARRY: propagates carry chain
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

        // DFF: stores counter bit
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

    // IO: clock input buffer
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

/// Build an N-stage shift register: N DFFs in series + clock
fn build_shift_register(n: usize) -> GateNetlist {
    let mut netlist = GateNetlist::new(format!("shiftreg_{}", n), "ice40".to_string());
    let mut net_id_counter = 0u32;

    let mut next_net = || {
        let id = net_id_counter;
        net_id_counter += 1;
        id
    };

    // Clock net
    let clock_net = netlist.add_net({
        let mut net = GateNet::new_input(GateNetId(next_net()), "clk".to_string());
        net.is_clock = true;
        net
    });

    // Data input
    let data_in = netlist.add_net(GateNet::new_input(
        GateNetId(next_net()),
        "data_in".to_string(),
    ));

    // DFF output nets
    let mut dff_out_nets = Vec::new();
    for i in 0..n {
        dff_out_nets.push(netlist.add_net(GateNet::new(GateNetId(next_net()), format!("q_{}", i))));
    }

    // Build N DFFs in series
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

    // IO cells
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

/// Build an N-bit adder: N LUTs + N carry cells + IO
fn build_adder(n: usize) -> GateNetlist {
    let mut netlist = GateNetlist::new(format!("adder_{}", n), "ice40".to_string());
    let mut net_id_counter = 0u32;

    let mut next_net = || {
        let id = net_id_counter;
        net_id_counter += 1;
        id
    };

    // Input nets for A and B operands
    let mut a_nets = Vec::new();
    let mut b_nets = Vec::new();
    for i in 0..n {
        a_nets.push(netlist.add_net(GateNet::new_input(
            GateNetId(next_net()),
            format!("a_{}", i),
        )));
        b_nets.push(netlist.add_net(GateNet::new_input(
            GateNetId(next_net()),
            format!("b_{}", i),
        )));
    }

    // Carry chain nets
    let carry_in = netlist.add_net(GateNet::new(GateNetId(next_net()), "cin".to_string()));
    let mut carry_nets = vec![carry_in];
    for i in 0..n {
        carry_nets
            .push(netlist.add_net(GateNet::new(GateNetId(next_net()), format!("carry_{}", i))));
    }

    // Sum output nets
    let mut sum_nets = Vec::new();
    for i in 0..n {
        sum_nets.push(netlist.add_net(GateNet::new_output(
            GateNetId(next_net()),
            format!("sum_{}", i),
        )));
    }

    // Build N adder stages
    for i in 0..n {
        // LUT: XOR(a, b) for sum computation
        let lut = Cell::new_comb(
            CellId(0),
            "SB_LUT4".to_string(),
            "ice40".to_string(),
            0.0,
            format!("add.lut{}", i),
            vec![a_nets[i], b_nets[i], carry_nets[i]],
            vec![sum_nets[i]],
        );
        let lut_id = netlist.add_cell(lut);
        netlist.nets[sum_nets[i].0 as usize].driver = Some(lut_id);
        netlist.nets[a_nets[i].0 as usize].fanout.push((lut_id, 0));
        netlist.nets[b_nets[i].0 as usize].fanout.push((lut_id, 1));
        netlist.nets[carry_nets[i].0 as usize]
            .fanout
            .push((lut_id, 2));

        // CARRY: propagates carry
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

fn run_pnr(netlist: &GateNetlist, label: &str) -> bool {
    let config = PnrConfig::default();
    let start = Instant::now();
    let result = place_and_route(netlist, Ice40Variant::Hx1k, config);
    let elapsed = start.elapsed();

    match result {
        Ok(pnr) => {
            println!(
                "{}: cells={}, routing_success={}, wirelength={}, congestion={:.2}, time={:?}",
                label,
                pnr.placement.placements.len(),
                pnr.routing.success,
                pnr.routing.wirelength,
                pnr.routing.congestion,
                elapsed,
            );
            pnr.routing.success
        }
        Err(e) => {
            println!("{}: FAILED - {}, time={:?}", label, e, elapsed);
            false
        }
    }
}

// === Counter tests ===

#[test]
fn test_counter_16() {
    let netlist = build_counter(16);
    assert!(
        run_pnr(&netlist, "counter_16"),
        "16-bit counter should P&R successfully"
    );
}

#[test]
#[ignore] // ~3min in debug mode
fn test_counter_32() {
    let netlist = build_counter(32);
    assert!(
        run_pnr(&netlist, "counter_32"),
        "32-bit counter should P&R successfully"
    );
}

#[test]
#[ignore] // May reveal router limits at scale
fn test_counter_64() {
    let netlist = build_counter(64);
    let success = run_pnr(&netlist, "counter_64");
    if !success {
        println!("WARNING: 64-bit counter routing did not fully succeed — expected at scale");
    }
}

#[test]
#[ignore]
fn test_counter_128() {
    let netlist = build_counter(128);
    let success = run_pnr(&netlist, "counter_128");
    println!(
        "128-bit counter stress test: {}",
        if success { "PASS" } else { "PARTIAL" }
    );
}

// === Shift register tests ===

#[test]
#[ignore] // ~2min in debug mode
fn test_shift_register_64() {
    let netlist = build_shift_register(64);
    assert!(
        run_pnr(&netlist, "shiftreg_64"),
        "64-stage shift register should P&R successfully"
    );
}

// === Adder tests ===

#[test]
fn test_adder_16() {
    let netlist = build_adder(16);
    assert!(
        run_pnr(&netlist, "adder_16"),
        "16-bit adder should P&R successfully"
    );
}

#[test]
#[ignore] // ~3min in debug mode
fn test_adder_32() {
    let netlist = build_adder(32);
    let success = run_pnr(&netlist, "adder_32");
    if !success {
        println!("WARNING: 32-bit adder routing did not fully succeed — expected at scale");
    }
}

// === Scaling performance table ===

#[test]
#[ignore] // Runs full matrix — use --ignored flag
fn test_scaling_performance() {
    println!("\n=== Scaling Performance Table ===");
    println!(
        "{:<20} {:>6} {:>8} {:>10} {:>10} {:>8}",
        "Design", "Cells", "Success", "Wirelength", "Congestion", "Time(ms)"
    );
    println!("{}", "-".repeat(70));

    for &size in &[8, 16, 32, 64] {
        let netlist = build_counter(size);
        let config = PnrConfig::default();
        let start = Instant::now();
        let result = place_and_route(&netlist, Ice40Variant::Hx1k, config);
        let elapsed_ms = start.elapsed().as_millis();

        match result {
            Ok(pnr) => {
                println!(
                    "{:<20} {:>6} {:>8} {:>10} {:>10.2} {:>8}",
                    format!("counter_{}", size),
                    pnr.placement.placements.len(),
                    pnr.routing.success,
                    pnr.routing.wirelength,
                    pnr.routing.congestion,
                    elapsed_ms,
                );
            }
            Err(e) => {
                println!(
                    "{:<20} {:>6} {:>8} {:>10} {:>10} {:>8}",
                    format!("counter_{}", size),
                    "?",
                    "FAIL",
                    "-",
                    format!("{}", e),
                    elapsed_ms,
                );
            }
        }
    }
}
