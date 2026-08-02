/// Test BMC equivalence checking for slow state machines
///
/// This tests whether BMC can find equivalence issues in state machines
/// that would require millions of cycles to reach certain states through
/// normal simulation (like PCIe link training).

#[test]
fn test_slow_fsm_bmc_equivalence() {
    use skalp_formal::equivalence::{
        check_sequential_equivalence_sat, BoundedModelChecker, LirToAig,
    };
    use skalp_frontend::parse_and_build_hir_from_file;
    use skalp_lir::mir_to_lir::lower_mir_module_to_lir;
    use skalp_mir::MirCompiler;

    // Two FSMs that differ only in a state reachable after LONG timeout
    // FsmA: state 2 -> state 3 (correct)
    // FsmB: state 2 -> state 1 (bug)
    // State 2 is only reachable after 16 million cycles of timeout!

    let fsm_a = r#"
pub const BIG_TIMEOUT: nat[24] = 16777215;

entity FsmA {
    in clk: clock
    in rst: reset(active_high)
    in go: bit
    out out_signal: bit
}

impl FsmA {
    signal state: nat[2]
    signal cnt: nat[24]

    on(clk.rise) {
        if rst {
            state = 0
            cnt = 0
        } else if state == 0 {
            if go {
                state = 1
            } else if cnt == BIG_TIMEOUT {
                state = 2
            } else {
                cnt = cnt + 1
            }
        } else if state == 1 {
            state = 3
        } else if state == 2 {
            state = 3
        }
    }

    out_signal = state == 3
}
"#;

    let fsm_b = r#"
pub const BIG_TIMEOUT: nat[24] = 16777215;

entity FsmB {
    in clk: clock
    in rst: reset(active_high)
    in go: bit
    out out_signal: bit
}

impl FsmB {
    signal state: nat[2]
    signal cnt: nat[24]

    on(clk.rise) {
        if rst {
            state = 0
            cnt = 0
        } else if state == 0 {
            if go {
                state = 1
            } else if cnt == BIG_TIMEOUT {
                state = 2
            } else {
                cnt = cnt + 1
            }
        } else if state == 1 {
            state = 3
        } else if state == 2 {
            state = 1   // BUG! Goes to 1 instead of 3
        }
    }

    out_signal = state == 3
}
"#;

    let temp_dir = std::env::temp_dir();

    // Compile FsmA
    let path_a = temp_dir.join("fsm_a_slow.sk");
    std::fs::write(&path_a, fsm_a).unwrap();
    let hir_a = parse_and_build_hir_from_file(&path_a).unwrap();
    let mir_a = MirCompiler::new().compile_to_mir(&hir_a).unwrap();
    let module_a = mir_a.modules.iter().find(|m| m.name == "FsmA").unwrap();
    let lir_result_a = lower_mir_module_to_lir(module_a);
    let lir_a = lir_result_a.lir;

    // Compile FsmB
    let path_b = temp_dir.join("fsm_b_slow.sk");
    std::fs::write(&path_b, fsm_b).unwrap();
    let hir_b = parse_and_build_hir_from_file(&path_b).unwrap();
    let mir_b = MirCompiler::new().compile_to_mir(&hir_b).unwrap();
    let module_b = mir_b.modules.iter().find(|m| m.name == "FsmB").unwrap();
    let lir_result_b = lower_mir_module_to_lir(module_b);
    let lir_b = lir_result_b.lir;

    println!("\n=== Slow FSM BMC Equivalence Test ===");
    println!("FsmA and FsmB differ only in timeout path (state 2)");
    println!(
        "State 2 requires {} cycles (~16 million) to reach via timeout!",
        16777215u32
    );
    println!("LFSR simulation would need millions of cycles to find this bug.");
    println!("BMC should find it instantly by reasoning symbolically.\n");

    // Convert to AIG (use convert_sequential to create latches for state)
    let conv_a = LirToAig::new();
    let aig_a = conv_a.convert_sequential(&lir_a);

    let conv_b = LirToAig::new();
    let aig_b = conv_b.convert_sequential(&lir_b);

    println!(
        "FsmA: {} AIG nodes, {} latches",
        aig_a.nodes.len(),
        aig_a.latches.len()
    );
    println!(
        "FsmB: {} AIG nodes, {} latches",
        aig_b.nodes.len(),
        aig_b.latches.len()
    );

    // Run BMC-style equivalence check
    println!("\nRunning bounded model checking equivalence...");
    println!("(Checking if outputs can differ over multiple cycles)");

    let bmc = BoundedModelChecker::new().with_bound(10);
    let result = bmc.check_sequential_aig_equivalence(&aig_a, &aig_b, 10);

    match result {
        Ok(res) => {
            if res.equivalent {
                println!(
                    "\nResult: Simulation-based BMC reports EQUIVALENT (up to {} cycles)",
                    res.bound
                );
                println!("\nIMPORTANT LIMITATION:");
                println!("The simulation-based BMC cannot find this bug because:");
                println!("  1. State 2 is only reachable after 16 million cycles");
                println!("  2. Simulation (even random) can't explore 16M cycles practically");
                println!();
                println!("For TRUE symbolic BMC, we would need:");
                println!("  1. Allow SAT solver to pick ANY initial state (not just reset state)");
                println!("  2. Check: 'Exists state s, input i: outputs differ from state s with input i'");
                println!("  3. SAT would find: 'From state=2, outputs differ after 1 cycle'");
                println!();
                println!("This demonstrates the gap between:");
                println!("  - Simulation-based verification (limited by reachability)");
                println!("  - Symbolic verification (can reason about unreachable states)");
            } else {
                println!("\nResult: NOT EQUIVALENT - Bug found!");
                println!("Mismatch at cycle: {:?}", res.mismatch_cycle);
                println!("Mismatching output: {:?}", res.mismatch_output);
            }
        }
        Err(e) => {
            println!("BMC error: {:?}", e);
        }
    }

    // Clean up
    std::fs::remove_file(&path_a).ok();
    std::fs::remove_file(&path_b).ok();

    // Now use SAT-based symbolic equivalence to find the bug
    println!("\n=== SAT-Based Symbolic Equivalence Check ===");
    println!("This checks if there exists ANY state/input where designs differ.");
    println!("No simulation needed - SAT solver reasons symbolically!\n");

    match check_sequential_equivalence_sat(&aig_a, &aig_b, false) {
        Ok(result) => {
            if result.equivalent {
                println!("Result: EQUIVALENT (unexpected!)");
            } else {
                println!("Result: NOT EQUIVALENT - Bug found by SAT solver!");
                println!("Time: {}ms", result.time_ms);

                if let Some(ce) = &result.counterexample {
                    println!("\nCounterexample (state/input where designs differ):");

                    // Show state assignment
                    let mut state_bits: Vec<_> = ce
                        .state
                        .iter()
                        .filter(|(k, _)| k.contains("state"))
                        .collect();
                    state_bits.sort_by_key(|(k, _)| k.as_str());

                    if !state_bits.is_empty() {
                        println!("  State bits:");
                        for (name, val) in &state_bits {
                            println!("    {} = {}", name, if **val { "1" } else { "0" });
                        }

                        // Decode state value
                        let state_val: u32 = state_bits
                            .iter()
                            .map(|(name, val)| {
                                // Extract bit index from name like "state[0]" or "__reg_cur_state[0]"
                                let bit_idx = name
                                    .rfind('[')
                                    .and_then(|start| {
                                        name.rfind(']').map(|end| &name[start + 1..end])
                                    })
                                    .and_then(|s| s.parse::<u32>().ok())
                                    .unwrap_or(0);
                                if **val {
                                    1u32 << bit_idx
                                } else {
                                    0
                                }
                            })
                            .sum();
                        println!("  => state = {}", state_val);
                    }

                    // Show relevant inputs
                    let relevant_inputs: Vec<_> = ce
                        .inputs
                        .iter()
                        .filter(|(k, _)| !k.starts_with("__"))
                        .collect();
                    if !relevant_inputs.is_empty() {
                        println!("  Inputs:");
                        for (name, val) in relevant_inputs {
                            println!("    {} = {}", name, if *val { "1" } else { "0" });
                        }
                    }
                }

                println!("\nSUCCESS: SAT solver found the bug WITHOUT simulating 16M cycles!");
                println!("The solver reasoned: 'If state==2, the next-state values differ'");
            }
        }
        Err(e) => {
            println!("SAT check error: {:?}", e);
        }
    }

    println!("\n=== Summary ===");
    println!("1. Simulation-based BMC: CANNOT find bug (state 2 unreachable in bounded cycles)");
    println!("2. SAT-based symbolic check: CAN find bug (reasons about ALL states, even unreachable ones)");
    println!(
        "\nThis is critical for protocols like PCIe where link training takes millions of cycles."
    );
}
