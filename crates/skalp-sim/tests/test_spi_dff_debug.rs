use skalp_frontend::parse_and_build_hir_from_file;
use skalp_lir::{get_stdlib_library, lower_mir_hierarchical_with_top, synthesize_hierarchical};
use skalp_mir::MirCompiler;
use skalp_sim::{convert_gate_netlist_to_sir, gate_simulator::GateLevelSimulator};
/// Debug test: trace gate-level DFF wiring and behavior for SPI Master shift_reg[0]
///
/// Run: cargo test --package skalp-sim --test test_spi_dff_debug -- --nocapture
use std::path::Path;

#[test]
fn test_spi_dff_sir_structure() {
    let hir = parse_and_build_hir_from_file(
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../examples/spi_master.sk")
            .as_path(),
    )
    .expect("parse");
    let compiler = MirCompiler::new();
    let mir = compiler.compile_to_mir(&hir).expect("mir");
    let library = get_stdlib_library("generic_asic").expect("lib");
    let hier_lir = lower_mir_hierarchical_with_top(&mir, Some("SpiMaster"));
    let hier_netlist =
        synthesize_hierarchical(&hier_lir, &library, skalp_lir::synth::SynthPreset::Quick);
    let netlist = hier_netlist.flatten();

    println!(
        "Gate netlist: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    // Dump cells related to shift_reg
    println!("\n=== CELLS related to shift_reg ===");
    for cell in &netlist.cells {
        let path = &cell.path;
        if path.contains("shift_reg") || path.contains("_t21") || path.contains("_t17") {
            println!(
                "Cell: {} type={} func={:?} path={}",
                cell.id.0, cell.cell_type, cell.function, path
            );
            println!("  inputs: {:?}", cell.inputs);
            println!("  outputs: {:?}", cell.outputs);
            println!(
                "  sequential: {} reset: {:?}",
                cell.is_sequential(),
                cell.reset
            );
            println!("  source_op: {:?}", cell.source_op);
        }
    }

    // Dump nets related to shift_reg
    println!("\n=== NETS related to shift_reg ===");
    for net in &netlist.nets {
        if net.name.contains("shift_reg") || net.name.contains("_t21") {
            println!(
                "Net {}: name={} is_output={} driver={:?} alias_of={:?}",
                net.id.0, net.name, net.is_output, net.driver, net.alias_of
            );
        }
    }

    // Dump LIR nodes for sclk_reg to find duplicate registers
    let lir = hier_lir.flatten();
    println!("\n=== LIR NODES for sclk_reg ===");
    for (i, node) in lir.nodes.iter().enumerate() {
        let out_name = lir
            .signals
            .get(node.output.0 as usize)
            .map(|s| s.name.as_str())
            .unwrap_or("?");
        if out_name.contains("sclk") || format!("{:?}", node.op).contains("sclk") {
            let inp_names: Vec<&str> = node
                .inputs
                .iter()
                .map(|id| {
                    lir.signals
                        .get(id.0 as usize)
                        .map(|s| s.name.as_str())
                        .unwrap_or("?")
                })
                .collect();
            println!(
                "  Node {}: op={:?} inputs={:?} output={} ({})",
                i, node.op, inp_names, out_name, node.path
            );
        }
    }
    // Find sequential nodes (those with clock set)
    println!("\n=== ALL SEQUENTIAL NODES ===");
    for (i, node) in lir.nodes.iter().enumerate() {
        if node.clock.is_some() {
            let out_name = lir
                .signals
                .get(node.output.0 as usize)
                .map(|s| s.name.as_str())
                .unwrap_or("?");
            let inp_names: Vec<&str> = node
                .inputs
                .iter()
                .map(|id| {
                    lir.signals
                        .get(id.0 as usize)
                        .map(|s| s.name.as_str())
                        .unwrap_or("?")
                })
                .collect();
            println!(
                "  Node {}: op={:?} inputs={:?} output={} ({})",
                i, node.op, inp_names, out_name, node.path
            );
        }
    }

    // Dump the MIR process body structure
    println!("\n=== MIR PROCESS BODY ===");
    let target = mir.modules.iter().find(|m| m.name == "SpiMaster").unwrap();
    for (pi, proc) in target.processes.iter().enumerate() {
        println!("Process {}: {} statements", pi, proc.body.statements.len());
        for (si, stmt) in proc.body.statements.iter().enumerate() {
            println!("  stmt[{}]: {:?}", si, std::mem::discriminant(stmt));
            match stmt {
                skalp_mir::mir::Statement::If(if_stmt) => {
                    println!("    then: {} stmts", if_stmt.then_block.statements.len());
                    if let Some(ref eb) = if_stmt.else_block {
                        println!("    else: {} stmts", eb.statements.len());
                        for (ei, es) in eb.statements.iter().enumerate() {
                            println!("      else_stmt[{}]: {:?}", ei, std::mem::discriminant(es));
                        }
                    }
                }
                skalp_mir::mir::Statement::Case(_) => println!("    (Case)"),
                _ => {}
            }
        }
    }

    let sir_result = convert_gate_netlist_to_sir(&netlist);
    let sir = &sir_result.sir;

    // Dump SIR signals related to shift_reg
    println!("\n=== SIR SIGNALS related to shift_reg ===");
    for signal in &sir.top_module.signals {
        if signal.name.contains("shift_reg") || signal.name.contains("_t21") {
            println!(
                "Signal {}: name={} width={} type={:?} initial={:?}",
                signal.id.0, signal.name, signal.width, signal.signal_type, signal.initial_value
            );
        }
    }

    // Dump sequential block operations for shift_reg DFFs
    println!("\n=== SEQUENTIAL BLOCK OPERATIONS (shift_reg DFFs) ===");
    for block in &sir.top_module.seq_blocks {
        println!(
            "SeqBlock: clock={:?} edge={:?} reset={:?}",
            block.clock, block.clock_edge, block.reset
        );
        for op in &block.operations {
            if let skalp_sim::sir::SirOperation::Primitive {
                id,
                ptype,
                inputs,
                outputs,
                path,
            } = op
            {
                {
                    // Print ALL DFFs so we can see duplicates
                    println!(
                        "  DFF {}: ptype={:?} inputs={:?} outputs={:?} path={}",
                        id.0, ptype, inputs, outputs, path
                    );
                    for inp in inputs {
                        if let Some(sig) = sir.top_module.signals.iter().find(|s| s.id == *inp) {
                            println!("    INPUT signal {}: name={}", sig.id.0, sig.name);
                        }
                    }
                    for outp in outputs {
                        if let Some(sig) = sir.top_module.signals.iter().find(|s| s.id == *outp) {
                            println!("    OUTPUT signal {}: name={}", sig.id.0, sig.name);
                        }
                    }
                }
            }
        }
    }

    // Also dump the combinational operations that drive _t21
    println!("\n=== COMBINATIONAL OPERATIONS (shift_reg cone) ===");
    for block in &sir.top_module.comb_blocks {
        for op in &block.operations {
            if let skalp_sim::sir::SirOperation::Primitive {
                id,
                ptype,
                inputs,
                outputs,
                path,
            } = op
            {
                if path.contains("shift_reg")
                    || path.contains("_t21")
                    || path.contains("_t17")
                    || path.contains("_t16")
                    || path.contains("_t20")
                    || path.contains("sclk")
                {
                    println!(
                        "  Op {}: ptype={:?} inputs={:?} outputs={:?} path={}",
                        id.0, ptype, inputs, outputs, path
                    );
                    for inp in inputs {
                        if let Some(sig) = sir.top_module.signals.iter().find(|s| s.id == *inp) {
                            println!("    INPUT signal {}: name={}", sig.id.0, sig.name);
                        }
                    }
                    for outp in outputs {
                        if let Some(sig) = sir.top_module.signals.iter().find(|s| s.id == *outp) {
                            println!("    OUTPUT signal {}: name={}", sig.id.0, sig.name);
                        }
                    }
                }
            }
        }
    }

    // Now simulate step by step — replicate EXACT EC flow
    let mut sim = GateLevelSimulator::new(&sir_result.sir);

    // EC uses with_reset("reset", 2) — "reset" does NOT match "reset" in the gate netlist!
    // So set_input_u64("reset", 1) silently fails.
    // Replicate this: DON'T drive reset, use set_input_u64 like EC does

    // Helper to print key signals
    let print_key = |sim: &GateLevelSimulator, label: &str| {
        let snap = sim.snapshot_signals();
        let get = |name: &str| -> String {
            snap.get(name)
                .map(|b| format!("{}", b[0] as u8))
                .unwrap_or("?".into())
        };
        println!(
            "  [{}] sclk_reg={} state={} reset={} start={}",
            label,
            get("top.sclk_reg"),
            get("top.state"),
            get("reset"),
            get("start")
        );
    };

    print_key(&sim, "init");

    // 2 reset cycles
    for i in 0..2 {
        sim.set_input_u64("reset", 1);
        sim.set_input_u64("clk", 0);
        sim.step();
        print_key(&sim, &format!("rst{} clkL", i));
        sim.set_input_u64("clk", 1);
        sim.step();
        print_key(&sim, &format!("rst{} clkH", i));
    }

    // Release reset + init all inputs to 0
    sim.set_input_u64("reset", 0);
    sim.set_input_u64("start", 0);
    for i in 0..8 {
        sim.set_input_u64(&format!("data_in[{}]", i), 0);
    }
    sim.set_input_u64("miso", 0);

    // One quiet cycle
    sim.set_input_u64("clk", 0);
    sim.step();
    print_key(&sim, "quiet0 clkL");
    sim.set_input_u64("clk", 1);
    sim.step();
    print_key(&sim, "quiet0 clkH");

    // 5 more reset cycles
    for i in 0..5 {
        sim.set_input_u64("reset", 1);
        sim.set_input_u64("clk", 0);
        sim.step();
        sim.set_input_u64("clk", 1);
        sim.step();
        print_key(&sim, &format!("rst2_{} clkH", i));
    }

    // Release reset again
    sim.set_input_u64("reset", 0);
    for i in 0..8 {
        sim.set_input_u64(&format!("data_in[{}]", i), 0);
    }
    sim.set_input_u64("start", 0);
    sim.set_input_u64("miso", 0);

    // 3 quiet cycles
    for i in 0..3 {
        sim.set_input_u64("clk", 0);
        sim.step();
        sim.set_input_u64("clk", 1);
        sim.step();
        print_key(&sim, &format!("quiet2_{} clkH", i));
    }

    println!("\n=== After reset + quiet cycle ===");
    let snap = sim.snapshot_signals();
    for (name, bits) in &snap {
        if name.contains("shift_reg") || name.contains("state") {
            println!("  {} = {:?}", name, bits);
        }
    }

    // Now apply EC cycle 0 inputs: start=1, data_in=102, miso=1
    // Use set_input_u64 like the EC does
    sim.set_input_u64("start", 1);
    sim.set_input_u64("miso", 1);
    // data_in = 102 = 0b01100110, set bit by bit like EC does
    let data_in_val: u64 = 102;
    for i in 0..8 {
        let bit_val = (data_in_val >> i) & 1;
        sim.set_input_u64(&format!("data_in[{}]", i), bit_val);
    }

    // Clock low phase
    sim.set_input_u64("clk", 0);
    sim.step();

    println!("\n=== After clk LOW (pre-edge, inputs applied) ===");
    let snap = sim.snapshot_signals();
    for (name, bits) in &snap {
        if name.contains("shift_reg")
            || name.contains("_t21")
            || name.contains("_t17")
            || name.contains("_t16")
            || name.contains("_t20")
            || name.contains("state")
            || name.contains("sclk_reg")
            || name.contains("data_in")
            || name.contains("start")
            || name.contains("miso")
            || name.contains("data_out")
        {
            let val: u64 = bits
                .iter()
                .enumerate()
                .map(|(i, &b)| if b { 1u64 << i } else { 0 })
                .sum();
            println!("  {} = {} (bits={:?})", name, val, bits);
        }
    }

    // Clock high phase (rising edge)
    sim.set_input_u64("clk", 1);
    sim.step();

    println!("\n=== After clk HIGH (post-edge) ===");
    let snap = sim.snapshot_signals();
    for (name, bits) in &snap {
        if name.contains("shift_reg")
            || name.contains("_t21")
            || name.contains("_t17")
            || name.contains("_t16")
            || name.contains("_t20")
            || name.contains("state")
            || name.contains("sclk_reg")
            || name.contains("data_in")
            || name.contains("start")
            || name.contains("miso")
            || name.contains("data_out")
        {
            let val: u64 = bits
                .iter()
                .enumerate()
                .map(|(i, &b)| if b { 1u64 << i } else { 0 })
                .sum();
            println!("  {} = {} (bits={:?})", name, val, bits);
        }
    }

    // Verify shift_reg[0]
    for (name, bits) in &snap {
        if name == "top.shift_reg[0]" {
            let val = bits[0];
            println!(
                "\n*** shift_reg[0] = {} (expected 0, data_in[0]=0) ***",
                val
            );
            assert!(!val, "shift_reg[0] should be 0 (data_in[0]=0), got 1");
        }
    }
}
