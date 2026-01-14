//! End-to-end safety analysis with technology mapping
//!
//! Demonstrates ISO 26262 functional safety flow:
//! 1. Define automotive motor controller design
//! 2. Apply technology mapping to get gate-level FIT rates
//! 3. Define safety goals and ASIL requirements
//! 4. Run FMEDA analysis
//! 5. Calculate safety metrics (SPFM, LFM, PMHF)
//!
//! This example shows how technology mapping provides accurate FIT rates
//! for realistic safety analysis rather than generic estimates.

use indexmap::IndexMap;
use skalp_frontend::parse_and_build_hir;
use skalp_lir::{
    gate_netlist::GateNetlist, get_stdlib_library, lower_mir_module_to_word_lir,
    tech_mapper::TechMapper,
};
use skalp_mir::MirCompiler;
use skalp_safety::{
    asil::{AsilLevel, HardwareMetrics},
    design_resolver::{AnnotationLevel, SafetyAnnotation},
    fault_simulation::{EffectAnalysis, SimulationCampaignResults},
    fmeda_library::FitBreakdown,
    gate_netlist_integration::{gate_netlist_to_fmea, GateToFmeaConfig},
    hierarchy::{DesignRef, InstancePath, Severity},
};
use skalp_sim::convert_gate_netlist_to_sir;

// ============================================================================
// Test Infrastructure
// ============================================================================

/// Compile source to GateNetlist via technology mapping
fn compile_to_gate_netlist(source: &str) -> Vec<GateNetlist> {
    let hir = parse_and_build_hir(source).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
        .expect("Failed to compile to MIR");

    let library = get_stdlib_library("generic_asic").expect("Failed to load library");

    mir.modules
        .iter()
        .map(|module| {
            let word_lir_result = lower_mir_module_to_word_lir(module);
            let mut mapper = TechMapper::new(&library);
            mapper.map(&word_lir_result.lir).netlist
        })
        .collect()
}

/// Calculate FIT breakdown from gate netlist for FMEDA
fn calculate_fit_breakdown_from_netlist(netlist: &GateNetlist) -> FitBreakdown {
    let total_fit = netlist.total_fit();

    // Estimate failure class distribution based on cell types
    // In real FMEDA, this would come from library failure mode data
    let mut safe_fit = 0.0;
    let mut spf_fit = 0.0;
    let mut latent_fit = 0.0;

    for cell in &netlist.cells {
        let cell_fit = cell.fit;
        // Sequential cells (DFF) have higher SPF contribution
        if cell.is_sequential() {
            spf_fit += cell_fit * 0.7; // 70% SPF
            latent_fit += cell_fit * 0.2; // 20% latent
            safe_fit += cell_fit * 0.1; // 10% safe
        } else {
            // Combinational cells
            spf_fit += cell_fit * 0.5; // 50% SPF
            latent_fit += cell_fit * 0.3; // 30% latent
            safe_fit += cell_fit * 0.2; // 20% safe
        }
    }

    FitBreakdown {
        total_fit,
        safe_fit,
        spf_fit,
        residual_fit: 0.0, // No residual without safety mechanisms
        latent_fit,
        dc_applied: 0.0,
    }
}

/// Calculate FIT breakdown with safety mechanisms applied
fn apply_safety_mechanisms(fit: &FitBreakdown, dc_psm: f64, dc_lsm: f64) -> FitBreakdown {
    // PSM detects single-point faults
    let detected_spf = fit.spf_fit * dc_psm / 100.0;
    let undetected_spf = fit.spf_fit - detected_spf;

    // LSM detects latent faults
    let detected_latent = fit.latent_fit * dc_lsm / 100.0;
    let undetected_latent = fit.latent_fit - detected_latent;

    // Detected faults become safe
    let new_safe = fit.safe_fit + detected_spf + detected_latent;

    FitBreakdown {
        total_fit: fit.total_fit,
        safe_fit: new_safe,
        spf_fit: undetected_spf,
        residual_fit: 0.0,
        latent_fit: undetected_latent,
        dc_applied: dc_psm,
    }
}

// ============================================================================
// Automotive Motor Controller Design
// ============================================================================

const MOTOR_CONTROLLER_SOURCE: &str = r#"
// Automotive Electric Motor Controller
// Safety-critical component requiring ASIL D compliance

entity MotorController {
    // Clock and reset
    in clk: clock
    in rst: reset

    // Sensor inputs (from position encoders)
    in encoder_a: nat[12]    // Primary encoder
    in encoder_b: nat[12]    // Secondary encoder (redundant)

    // Current sensing (from ADC)
    in current_u: nat[12]    // Phase U current
    in current_v: nat[12]    // Phase V current
    in current_w: nat[12]    // Phase W current

    // Torque command from higher-level controller
    in torque_cmd: nat[16]
    in enable: bool

    // PWM outputs to motor driver
    out pwm_u: nat[10]
    out pwm_v: nat[10]
    out pwm_w: nat[10]

    // Safety outputs
    out fault_detected: bool
    out safe_state: bool
}

impl MotorController {
    // Internal signals for FOC algorithm
    signal position: nat[12] = 0
    signal velocity: nat[16] = 0
    signal id_current: nat[16] = 0  // d-axis current
    signal iq_current: nat[16] = 0  // q-axis current

    // Safety monitoring signals
    signal encoder_mismatch: bool = false
    signal overcurrent: bool = false
    signal position_error: nat[12] = 0

    // PWM generation registers
    signal pwm_u_reg: nat[10] = 0
    signal pwm_v_reg: nat[10] = 0
    signal pwm_w_reg: nat[10] = 0

    // Safety state machine
    signal fault_state: bool = false

    // Main control loop - clocked process
    on(clk.rise) {
        if rst {
            // Reset all registers
            position <= 0
            velocity <= 0
            id_current <= 0
            iq_current <= 0
            pwm_u_reg <= 0
            pwm_v_reg <= 0
            pwm_w_reg <= 0
            fault_state <= false
            encoder_mismatch <= false
            overcurrent <= false
        } else if enable && !fault_state {
            // Position estimation (use primary encoder)
            position <= encoder_a

            // Encoder comparison for redundancy check
            encoder_mismatch <= (encoder_a > encoder_b + 10) || (encoder_b > encoder_a + 10)

            // Current limit check (overcurrent protection)
            overcurrent <= (current_u > 3000) || (current_v > 3000) || (current_w > 3000)

            // Enter fault state if any safety violation
            fault_state <= encoder_mismatch || overcurrent

            // Simplified FOC calculation (actual FOC is more complex)
            // Park transform approximation
            id_current <= current_u
            iq_current <= current_v

            // PI controller output (simplified)
            pwm_u_reg <= torque_cmd[9:0]
            pwm_v_reg <= torque_cmd[9:0]
            pwm_w_reg <= torque_cmd[9:0]
        } else {
            // Safe state - disable outputs
            pwm_u_reg <= 0
            pwm_v_reg <= 0
            pwm_w_reg <= 0
        }
    }

    // Output assignments
    pwm_u = pwm_u_reg
    pwm_v = pwm_v_reg
    pwm_w = pwm_w_reg
    fault_detected = encoder_mismatch || overcurrent
    safe_state = fault_state || !enable
}
"#;

// TMR Voting Module for safety mechanisms
const TMR_VOTER_SOURCE: &str = r#"
// Triple Modular Redundancy Voter
// Primary Safety Mechanism for ASIL D compliance

entity TmrVoter {
    in clk: clock
    in a: nat[16]
    in b: nat[16]
    in c: nat[16]
    out voted: nat[16]
    out disagreement: bool
}

impl TmrVoter {
    signal result: nat[16] = 0
    signal error: bool = false

    on(clk.rise) {
        // 2-of-3 voting logic
        if a == b {
            result <= a
            error <= !(a == c)
        } else if a == c {
            result <= a
            error <= true
        } else if b == c {
            result <= b
            error <= true
        } else {
            // All three disagree - use first input and flag error
            result <= a
            error <= true
        }
    }

    voted = result
    disagreement = error
}
"#;

// Watchdog Timer
const WATCHDOG_SOURCE: &str = r#"
// Hardware Watchdog Timer
// Latent Safety Mechanism for monitoring processor liveness

entity Watchdog {
    in clk: clock
    in rst: reset
    in kick: bool          // Pulse to reset timer
    in timeout_val: nat[16] // Timeout value
    out expired: bool
}

impl Watchdog {
    signal counter: nat[16] = 0
    signal timed_out: bool = false

    on(clk.rise) {
        if rst {
            counter <= 0
            timed_out <= false
        } else if kick {
            counter <= 0
            timed_out <= false
        } else if counter >= timeout_val {
            timed_out <= true
        } else {
            counter <= counter + 1
        }
    }

    expired = timed_out
}
"#;

// ============================================================================
// Safety Analysis Tests
// ============================================================================

#[test]
fn test_motor_controller_tech_mapping() {
    println!("\n=== Motor Controller Technology Mapping ===\n");

    let netlists = compile_to_gate_netlist(MOTOR_CONTROLLER_SOURCE);
    assert_eq!(netlists.len(), 1, "Should have one module");

    let netlist = &netlists[0];

    println!("Design: {}", netlist.name);
    println!("Total cells: {}", netlist.cells.len());
    println!("Total nets: {}", netlist.nets.len());
    println!("Clock nets: {}", netlist.clocks.len());
    println!("Reset nets: {}", netlist.resets.len());

    // Count cell types
    let mut cell_counts: IndexMap<String, usize> = IndexMap::new();
    for cell in &netlist.cells {
        *cell_counts.entry(cell.cell_type.clone()).or_insert(0) += 1;
    }

    println!("\nCell type distribution:");
    let mut sorted_cells: Vec<_> = cell_counts.iter().collect();
    sorted_cells.sort_by(|a, b| b.1.cmp(a.1));
    for (cell_type, count) in sorted_cells.iter().take(10) {
        println!("  {}: {}", cell_type, count);
    }

    // FIT analysis
    let total_fit = netlist.total_fit();
    println!("\n=== FIT Rate Analysis ===");
    println!("Total FIT: {:.4}", total_fit);

    // Sequential vs combinational FIT
    let seq_fit: f64 = netlist
        .cells
        .iter()
        .filter(|c| c.is_sequential())
        .map(|c| c.fit)
        .sum();
    let comb_fit: f64 = netlist
        .cells
        .iter()
        .filter(|c| !c.is_sequential())
        .map(|c| c.fit)
        .sum();

    println!(
        "Sequential FIT: {:.4} ({:.1}%)",
        seq_fit,
        seq_fit / total_fit * 100.0
    );
    println!(
        "Combinational FIT: {:.4} ({:.1}%)",
        comb_fit,
        comb_fit / total_fit * 100.0
    );

    // Convert to SIR for simulation
    let sir_result = convert_gate_netlist_to_sir(netlist);
    println!("\nSIR conversion:");
    println!("  Signals: {}", sir_result.stats.signals_created);
    println!("  Primitives: {}", sir_result.stats.primitives_created);
    println!("  Total FIT preserved: {:.4}", sir_result.stats.total_fit);
}

#[test]
fn test_safety_mechanisms_tech_mapping() {
    println!("\n=== Safety Mechanisms Technology Mapping ===\n");

    // TMR Voter
    let tmr_netlists = compile_to_gate_netlist(TMR_VOTER_SOURCE);
    let tmr_netlist = &tmr_netlists[0];
    println!("TMR Voter:");
    println!("  Cells: {}", tmr_netlist.cells.len());
    println!("  FIT: {:.4}", tmr_netlist.total_fit());

    // Watchdog
    let wd_netlists = compile_to_gate_netlist(WATCHDOG_SOURCE);
    let wd_netlist = &wd_netlists[0];
    println!("\nWatchdog Timer:");
    println!("  Cells: {}", wd_netlist.cells.len());
    println!("  FIT: {:.4}", wd_netlist.total_fit());
}

#[test]
fn test_fmeda_analysis_with_tech_mapping() {
    println!("\n=== FMEDA Analysis with Technology Mapping ===\n");

    // Compile all components
    let motor_netlists = compile_to_gate_netlist(MOTOR_CONTROLLER_SOURCE);
    let tmr_netlists = compile_to_gate_netlist(TMR_VOTER_SOURCE);
    let wd_netlists = compile_to_gate_netlist(WATCHDOG_SOURCE);

    let motor_netlist = &motor_netlists[0];
    let tmr_netlist = &tmr_netlists[0];
    let wd_netlist = &wd_netlists[0];

    // Step 1: Calculate base FIT breakdown without safety mechanisms
    println!("Step 1: Base FIT analysis (no safety mechanisms)");
    let motor_fit_base = calculate_fit_breakdown_from_netlist(motor_netlist);
    println!("Motor Controller base FIT breakdown:");
    println!("  Total FIT: {:.4}", motor_fit_base.total_fit);
    println!("  Safe FIT: {:.4}", motor_fit_base.safe_fit);
    println!("  SPF FIT: {:.4}", motor_fit_base.spf_fit);
    println!("  Latent FIT: {:.4}", motor_fit_base.latent_fit);

    let spfm_base = motor_fit_base.spfm();
    let lfm_base = motor_fit_base.lfm();
    println!("\nBase metrics (no mechanisms):");
    println!("  SPFM: {:.1}%", spfm_base);
    println!("  LFM: {:.1}%", lfm_base);

    // Step 2: Apply TMR (Primary Safety Mechanism) - 99.5% diagnostic coverage
    println!("\nStep 2: Apply TMR (PSM with 99.5% DC)");
    let dc_psm = 99.5; // TMR diagnostic coverage

    // Step 3: Apply Watchdog (Latent Safety Mechanism) - 90% coverage
    println!("Step 3: Apply Watchdog (LSM with 90% DC)");
    let dc_lsm = 90.0;

    // Calculate with safety mechanisms
    let motor_fit_protected = apply_safety_mechanisms(&motor_fit_base, dc_psm, dc_lsm);

    println!("\nMotor Controller with safety mechanisms:");
    println!("  Total FIT: {:.4}", motor_fit_protected.total_fit);
    println!("  Safe FIT: {:.4}", motor_fit_protected.safe_fit);
    println!("  SPF FIT: {:.4}", motor_fit_protected.spf_fit);
    println!("  Latent FIT: {:.4}", motor_fit_protected.latent_fit);

    let spfm_protected = motor_fit_protected.spfm();
    let lfm_protected = motor_fit_protected.lfm();

    println!("\nProtected metrics:");
    println!("  SPFM: {:.1}% (target: 99% for ASIL D)", spfm_protected);
    println!("  LFM: {:.1}% (target: 90% for ASIL D)", lfm_protected);

    // Step 4: Calculate PMHF
    let pmhf = motor_fit_protected.spf_fit + motor_fit_protected.residual_fit;
    println!("\nPMHF: {:.2} FIT (target: <10 FIT for ASIL D)", pmhf);

    // Step 5: Evaluate ASIL compliance
    println!("\n=== ASIL Compliance Evaluation ===");
    let asil_d_requirements = AsilLevel::D.requirements();
    println!(
        "ASIL D targets: SPFM >= {:.0}%, LFM >= {:.0}%, PMHF <= 10 FIT",
        asil_d_requirements.spfm_target.unwrap_or(0.0),
        asil_d_requirements.lf_target.unwrap_or(0.0)
    );

    let hw_metrics = HardwareMetrics {
        spfm: spfm_protected,
        lf: lfm_protected,
        pmhf,
    };

    if hw_metrics.meets_asil_requirements(&AsilLevel::D) {
        println!("\n[PASS] Design meets ASIL D requirements");
    } else {
        println!("\n[INFO] Design does not meet ASIL D requirements");

        // Check what ASIL level we can achieve
        for level in &[AsilLevel::C, AsilLevel::B, AsilLevel::A, AsilLevel::QM] {
            if hw_metrics.meets_asil_requirements(level) {
                println!("Design meets {:?} requirements", level);
                break;
            }
        }
    }

    // Add safety mechanism overhead
    println!("\n=== Safety Mechanism Overhead ===");
    let tmr_fit = tmr_netlist.total_fit();
    let wd_fit = wd_netlist.total_fit();
    println!("TMR Voter FIT: {:.4}", tmr_fit);
    println!("Watchdog FIT: {:.4}", wd_fit);
    println!("Total safety mechanism FIT: {:.4}", tmr_fit + wd_fit);
    println!(
        "Percentage of total: {:.1}%",
        (tmr_fit + wd_fit) / motor_fit_base.total_fit * 100.0
    );
}

#[test]
fn test_asil_decomposition_analysis() {
    println!("\n=== ASIL Decomposition Analysis ===\n");

    // Demonstrate ASIL decomposition per ISO 26262
    // ASIL D can be decomposed to:
    //   - ASIL B + ASIL B (redundant channels)
    //   - ASIL C + ASIL A

    println!("ASIL D decomposition options:");
    for (a, b) in AsilLevel::D.decompose() {
        println!("  {:?} + {:?}", a, b);
    }

    // For our motor controller with TMR, we effectively have:
    // - 3 redundant channels each at ASIL B
    // - Voting logic handles disagreement

    let channel_source = r#"
        entity Channel {
            in clk: clock
            in input: nat[16]
            out output: nat[16]
        }

        impl Channel {
            signal reg: nat[16] = 0
            on(clk.rise) {
                reg <= input
            }
            output = reg
        }
    "#;

    let channel_netlists = compile_to_gate_netlist(channel_source);
    let channel_netlist = &channel_netlists[0];

    let channel_fit = channel_netlist.total_fit();
    let channel_fit_breakdown = calculate_fit_breakdown_from_netlist(channel_netlist);

    println!("\nSingle channel analysis:");
    println!("  FIT: {:.4}", channel_fit);
    println!("  SPFM: {:.1}%", channel_fit_breakdown.spfm());
    println!("  LFM: {:.1}%", channel_fit_breakdown.lfm());

    // With TMR, probability of failure = 3 * P^2 (2 of 3 must fail)
    // For small P, this is approximately 3 * P^2
    let single_channel_failure_rate = channel_fit / 1e9; // Convert FIT to failures/hour
    let tmr_failure_rate = 3.0 * single_channel_failure_rate.powi(2);
    let tmr_equivalent_fit = tmr_failure_rate * 1e9;

    println!("\nTMR combined analysis (3 channels):");
    println!(
        "  Equivalent FIT (with voting): {:.6} FIT",
        tmr_equivalent_fit
    );
    println!(
        "  Improvement factor: {:.0}x",
        channel_fit / tmr_equivalent_fit
    );
}

#[test]
fn test_comprehensive_safety_report() {
    println!("\n");
    println!("╔══════════════════════════════════════════════════════════════════╗");
    println!("║         ISO 26262 SAFETY ANALYSIS REPORT                         ║");
    println!("║         Automotive Motor Controller - ASIL D                     ║");
    println!("╚══════════════════════════════════════════════════════════════════╝");
    println!();

    // Compile design
    let motor_netlists = compile_to_gate_netlist(MOTOR_CONTROLLER_SOURCE);
    let tmr_netlists = compile_to_gate_netlist(TMR_VOTER_SOURCE);
    let wd_netlists = compile_to_gate_netlist(WATCHDOG_SOURCE);

    let motor_netlist = &motor_netlists[0];
    let tmr_netlist = &tmr_netlists[0];
    let wd_netlist = &wd_netlists[0];

    println!("1. DESIGN SUMMARY");
    println!("─────────────────────────────────────────────────────────────────────");
    println!("Component               | Cells | Seq | Comb | FIT");
    println!("─────────────────────────────────────────────────────────────────────");

    let motor_seq = motor_netlist
        .cells
        .iter()
        .filter(|c| c.is_sequential())
        .count();
    let motor_comb = motor_netlist.cells.len() - motor_seq;
    println!(
        "Motor Controller        | {:5} | {:3} | {:4} | {:.4}",
        motor_netlist.cells.len(),
        motor_seq,
        motor_comb,
        motor_netlist.total_fit()
    );

    let tmr_seq = tmr_netlist
        .cells
        .iter()
        .filter(|c| c.is_sequential())
        .count();
    let tmr_comb = tmr_netlist.cells.len() - tmr_seq;
    println!(
        "TMR Voter (PSM)         | {:5} | {:3} | {:4} | {:.4}",
        tmr_netlist.cells.len(),
        tmr_seq,
        tmr_comb,
        tmr_netlist.total_fit()
    );

    let wd_seq = wd_netlist
        .cells
        .iter()
        .filter(|c| c.is_sequential())
        .count();
    let wd_comb = wd_netlist.cells.len() - wd_seq;
    println!(
        "Watchdog Timer (LSM)    | {:5} | {:3} | {:4} | {:.4}",
        wd_netlist.cells.len(),
        wd_seq,
        wd_comb,
        wd_netlist.total_fit()
    );

    let total_cells = motor_netlist.cells.len() + tmr_netlist.cells.len() + wd_netlist.cells.len();
    let total_fit = motor_netlist.total_fit() + tmr_netlist.total_fit() + wd_netlist.total_fit();
    println!("─────────────────────────────────────────────────────────────────────");
    println!(
        "TOTAL                   | {:5} |     |      | {:.4}",
        total_cells, total_fit
    );

    println!("\n2. SAFETY GOALS");
    println!("─────────────────────────────────────────────────────────────────────");
    println!("SG-001: Prevent unintended motor torque");
    println!("  ASIL Level: D");
    println!("  SPFM Target: >= 99%");
    println!("  LFM Target: >= 90%");
    println!("  PMHF Target: <= 10 FIT");

    println!("\n3. HARDWARE SAFETY REQUIREMENTS (HSR)");
    println!("─────────────────────────────────────────────────────────────────────");
    println!("HSR-001: Detect encoder failures within 10ms");
    println!("  Implementation: Dual encoder comparison");
    println!("  PSM: TMR Voter (DC = 99.5%)");
    println!();
    println!("HSR-002: Detect overcurrent conditions");
    println!("  Implementation: Current threshold comparison");
    println!("  PSM: Built-in comparators (DC = 99%)");
    println!();
    println!("HSR-003: Monitor processor liveness");
    println!("  Implementation: Hardware watchdog");
    println!("  LSM: Watchdog Timer (DC = 90%)");

    println!("\n4. FMEDA RESULTS");
    println!("─────────────────────────────────────────────────────────────────────");

    let motor_fit_base = calculate_fit_breakdown_from_netlist(motor_netlist);
    let motor_fit_protected = apply_safety_mechanisms(&motor_fit_base, 99.5, 90.0);

    println!("                        | Base      | Protected");
    println!("─────────────────────────────────────────────────────────────────────");
    println!(
        "Total FIT               | {:9.4} | {:9.4}",
        motor_fit_base.total_fit, motor_fit_protected.total_fit
    );
    println!(
        "Safe Fault FIT          | {:9.4} | {:9.4}",
        motor_fit_base.safe_fit, motor_fit_protected.safe_fit
    );
    println!(
        "Single Point Fault FIT  | {:9.4} | {:9.4}",
        motor_fit_base.spf_fit, motor_fit_protected.spf_fit
    );
    println!(
        "Latent Fault FIT        | {:9.4} | {:9.4}",
        motor_fit_base.latent_fit, motor_fit_protected.latent_fit
    );

    println!("\n5. SAFETY METRICS");
    println!("─────────────────────────────────────────────────────────────────────");

    let spfm = motor_fit_protected.spfm();
    let lfm = motor_fit_protected.lfm();
    let pmhf = motor_fit_protected.spf_fit + motor_fit_protected.residual_fit;

    println!("Metric | Value   | Target  | Status");
    println!("─────────────────────────────────────────────────────────────────────");

    let spfm_status = if spfm >= 99.0 { "PASS" } else { "FAIL" };
    println!("SPFM   | {:5.1}%  | >= 99%  | {}", spfm, spfm_status);

    let lfm_status = if lfm >= 90.0 { "PASS" } else { "FAIL" };
    println!("LFM    | {:5.1}%  | >= 90%  | {}", lfm, lfm_status);

    let pmhf_status = if pmhf <= 10.0 { "PASS" } else { "FAIL" };
    println!("PMHF   | {:5.2} FIT | <= 10   | {}", pmhf, pmhf_status);

    println!("\n6. COMPLIANCE ASSESSMENT");
    println!("─────────────────────────────────────────────────────────────────────");

    let hw_metrics = HardwareMetrics {
        spfm,
        lf: lfm,
        pmhf,
    };

    let mut achieved_asil = AsilLevel::QM;
    for level in &[AsilLevel::D, AsilLevel::C, AsilLevel::B, AsilLevel::A] {
        if hw_metrics.meets_asil_requirements(level) {
            achieved_asil = *level;
            break;
        }
    }

    println!("Target ASIL: D");
    println!("Achieved ASIL: {:?}", achieved_asil);

    if achieved_asil >= AsilLevel::D {
        println!("\n[COMPLIANT] Design meets ISO 26262 ASIL D requirements");
    } else {
        println!(
            "\n[GAP IDENTIFIED] Design achieves {:?}, improvements needed for ASIL D",
            achieved_asil
        );

        // Calculate gaps
        let requirements = AsilLevel::D.requirements();
        if spfm < requirements.spfm_target.unwrap_or(0.0) {
            println!(
                "  - SPFM gap: {:.1}% needed",
                requirements.spfm_target.unwrap_or(0.0) - spfm
            );
        }
        if lfm < requirements.lf_target.unwrap_or(0.0) {
            println!(
                "  - LFM gap: {:.1}% needed",
                requirements.lf_target.unwrap_or(0.0) - lfm
            );
        }
        if pmhf > 10.0 {
            println!("  - PMHF reduction needed: {:.2} FIT", pmhf - 10.0);
        }
    }

    println!("\n7. TECHNOLOGY MAPPING TRACEABILITY");
    println!("─────────────────────────────────────────────────────────────────────");
    println!("Technology Library: generic_asic");
    println!("Process Node: 7nm equivalent");
    println!("FIT Source: Library default values (conservative estimates)");
    println!();
    println!("Top 5 FIT contributors:");

    let mut cell_fits: Vec<_> = motor_netlist
        .cells
        .iter()
        .map(|c| (&c.cell_type, c.fit, &c.path))
        .collect();
    cell_fits.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());

    for (i, (cell_type, fit, path)) in cell_fits.iter().take(5).enumerate() {
        println!("  {}. {} ({:.4} FIT) at {}", i + 1, cell_type, fit, path);
    }

    println!("\n═══════════════════════════════════════════════════════════════════");
    println!("                    END OF SAFETY REPORT");
    println!("═══════════════════════════════════════════════════════════════════\n");
}

#[test]
fn test_fit_improvement_with_mechanisms() {
    println!("\n=== FIT Rate Improvement Analysis ===\n");

    let motor_netlists = compile_to_gate_netlist(MOTOR_CONTROLLER_SOURCE);
    let motor_netlist = &motor_netlists[0];

    let base_fit = calculate_fit_breakdown_from_netlist(motor_netlist);

    println!("Diagnostic Coverage Sweep:");
    println!("DC_PSM | DC_LSM | SPFM    | LFM     | PMHF");
    println!("─────────────────────────────────────────────────────────────────────");

    for dc_psm in [0.0, 50.0, 90.0, 95.0, 99.0, 99.5, 99.9] {
        for dc_lsm in [0.0, 50.0, 90.0] {
            let protected = apply_safety_mechanisms(&base_fit, dc_psm, dc_lsm);
            let spfm = protected.spfm();
            let lfm = protected.lfm();
            let pmhf = protected.spf_fit + protected.residual_fit;

            let spfm_ok = if spfm >= 99.0 { "+" } else { " " };
            let lfm_ok = if lfm >= 90.0 { "+" } else { " " };
            let pmhf_ok = if pmhf <= 10.0 { "+" } else { " " };

            println!(
                "{:5.1}% | {:5.1}% | {:5.1}%{} | {:5.1}%{} | {:5.2}{}",
                dc_psm, dc_lsm, spfm, spfm_ok, lfm, lfm_ok, pmhf, pmhf_ok
            );
        }
    }

    println!("\nLegend: + = meets ASIL D target");
}

// ============================================================================
// New Integration Tests with gate_netlist_to_fmea
// ============================================================================

#[test]
fn test_gate_netlist_to_fmea_with_measured_dc() {
    println!("\n=== Gate Netlist to FMEA with Measured DC ===\n");

    // Step 1: Compile motor controller to gate netlist
    let motor_netlists = compile_to_gate_netlist(MOTOR_CONTROLLER_SOURCE);
    let motor_netlist = &motor_netlists[0];
    println!(
        "Motor Controller: {} cells, {:.4} FIT",
        motor_netlist.cells.len(),
        motor_netlist.total_fit()
    );

    // Step 2: Create safety annotations
    // These represent #[implements(...)] attributes in the source code
    // Note: Path must match cell paths (e.g., "MotorController.xxx" not "top.MotorController.xxx")
    let annotations = vec![
        // Encoder monitoring covers the entire MotorController
        SafetyAnnotation {
            design_ref: DesignRef::instance(InstancePath::parse("MotorController")),
            goal_name: "MotorSafety".to_string(),
            mechanism_name: "EncoderMonitoring".to_string(),
            level: AnnotationLevel::Instance,
        },
    ];
    println!(
        "Safety annotations: {} mechanism(s) declared",
        annotations.len()
    );

    // Step 3: Create mock fault injection results with measured DC
    // In a real flow, this comes from running fault simulation
    let mut sim_results = SimulationCampaignResults::new("MotorSafety", "MotorController");

    // Simulate fault injection results: 98.5% of faults were detected
    let mut effect = EffectAnalysis::new("motor_torque_error", Severity::S3, 99.0);
    effect.total_faults_causing = 1000;
    effect.faults_detected = 985; // 98.5% detection
    effect.update_dc(); // This sets measured_dc = 0.985
    effect
        .detection_by_mechanism
        .insert("EncoderMonitoring".to_string(), 985);
    sim_results
        .effect_analyses
        .insert("motor_torque_error".to_string(), effect);

    println!(
        "Fault injection results: {:.1}% DC measured",
        sim_results.effect_analyses["motor_torque_error"].measured_dc * 100.0
    );

    // Step 4: Run gate_netlist_to_fmea conversion
    let config = GateToFmeaConfig {
        library_name: "generic_asic".to_string(),
        base_path: "top.MotorController".to_string(),
        fallback_dc: 0.0, // Conservative fallback
    };

    let result = gate_netlist_to_fmea(motor_netlist, &annotations, Some(&sim_results), &config);

    // Step 5: Verify results
    println!("\n=== FMEA Conversion Results ===");
    println!("Total cells: {}", result.coverage_summary.total_cells);
    println!("Covered cells: {}", result.coverage_summary.covered_cells);
    println!(
        "DC from simulation: {}",
        result.coverage_summary.dc_from_simulation
    );
    println!("Raw FIT: {:.4}", result.coverage_summary.total_fit_raw);
    println!(
        "Residual FIT (after DC): {:.4}",
        result.coverage_summary.total_fit_residual
    );
    println!(
        "FMEA components generated: {}",
        result.fmea_data.components.len()
    );

    if !result.warnings.is_empty() {
        println!("\nWarnings:");
        for warning in &result.warnings {
            println!("  - {}", warning);
        }
    }

    // Assertions
    assert!(
        result.coverage_summary.dc_from_simulation,
        "DC should come from simulation results"
    );
    assert_eq!(
        result.coverage_summary.total_cells,
        motor_netlist.cells.len()
    );
    assert!(
        result.coverage_summary.total_fit_residual < result.coverage_summary.total_fit_raw,
        "Residual FIT should be less than raw FIT when DC is applied"
    );

    // With 98.5% DC, residual should be ~1.5% of raw for covered cells
    // But not all cells may be covered, so just verify it's reduced
    let reduction_factor =
        result.coverage_summary.total_fit_residual / result.coverage_summary.total_fit_raw;
    println!(
        "\nFIT reduction factor: {:.3} (lower is better)",
        reduction_factor
    );
    assert!(
        reduction_factor < 1.0,
        "With DC > 0, residual FIT should be reduced"
    );
}

#[test]
fn test_gate_netlist_to_fmea_without_simulation() {
    println!("\n=== Gate Netlist to FMEA Without Simulation ===\n");

    // Compile motor controller
    let motor_netlists = compile_to_gate_netlist(MOTOR_CONTROLLER_SOURCE);
    let motor_netlist = &motor_netlists[0];

    // Create annotations but no simulation results
    // Note: Path must match cell paths (e.g., "MotorController" not "top.MotorController")
    let annotations = vec![SafetyAnnotation {
        design_ref: DesignRef::instance(InstancePath::parse("MotorController")),
        goal_name: "MotorSafety".to_string(),
        mechanism_name: "EncoderMonitoring".to_string(),
        level: AnnotationLevel::Instance,
    }];

    let config = GateToFmeaConfig::default();

    // Run WITHOUT simulation results - should use fallback DC
    let result = gate_netlist_to_fmea(motor_netlist, &annotations, None, &config);

    println!(
        "DC from simulation: {}",
        result.coverage_summary.dc_from_simulation
    );
    println!("Raw FIT: {:.4}", result.coverage_summary.total_fit_raw);
    println!(
        "Residual FIT: {:.4}",
        result.coverage_summary.total_fit_residual
    );

    // Assertions
    assert!(
        !result.coverage_summary.dc_from_simulation,
        "Without simulation, dc_from_simulation should be false"
    );

    // With fallback DC of 0%, residual = raw
    assert!(
        (result.coverage_summary.total_fit_residual - result.coverage_summary.total_fit_raw).abs()
            < 0.001,
        "With 0% fallback DC, residual FIT should equal raw FIT"
    );

    // Should have a warning about missing simulation
    assert!(
        result
            .warnings
            .iter()
            .any(|w| w.contains("fault injection") || w.contains("simulation")),
        "Should warn about missing fault injection results"
    );
}

#[test]
fn test_comprehensive_fmeda_with_gate_netlist() {
    println!("\n");
    println!("╔══════════════════════════════════════════════════════════════════╗");
    println!("║   COMPREHENSIVE FMEDA WITH GATE NETLIST INTEGRATION             ║");
    println!("╚══════════════════════════════════════════════════════════════════╝");
    println!();

    // Compile all components
    let motor_netlists = compile_to_gate_netlist(MOTOR_CONTROLLER_SOURCE);
    let tmr_netlists = compile_to_gate_netlist(TMR_VOTER_SOURCE);
    let wd_netlists = compile_to_gate_netlist(WATCHDOG_SOURCE);

    let motor_netlist = &motor_netlists[0];
    let tmr_netlist = &tmr_netlists[0];
    let wd_netlist = &wd_netlists[0];

    println!("1. COMPILED NETLISTS");
    println!("─────────────────────────────────────────────────────────────────────");
    println!(
        "   Motor Controller: {} cells, {:.4} FIT",
        motor_netlist.cells.len(),
        motor_netlist.total_fit()
    );
    println!(
        "   TMR Voter:        {} cells, {:.4} FIT",
        tmr_netlist.cells.len(),
        tmr_netlist.total_fit()
    );
    println!(
        "   Watchdog:         {} cells, {:.4} FIT",
        wd_netlist.cells.len(),
        wd_netlist.total_fit()
    );

    // Create safety annotations for each component
    // Note: Paths must match cell paths (module name without "top." prefix)
    let motor_annotations = vec![SafetyAnnotation {
        design_ref: DesignRef::instance(InstancePath::parse("MotorController")),
        goal_name: "MotorSafety".to_string(),
        mechanism_name: "EncoderComparison".to_string(),
        level: AnnotationLevel::Instance,
    }];

    let tmr_annotations = vec![SafetyAnnotation {
        design_ref: DesignRef::instance(InstancePath::parse("TmrVoter")),
        goal_name: "MotorSafety".to_string(),
        mechanism_name: "TMRVoting".to_string(),
        level: AnnotationLevel::Instance,
    }];

    let wd_annotations = vec![SafetyAnnotation {
        design_ref: DesignRef::instance(InstancePath::parse("Watchdog")),
        goal_name: "MotorSafety".to_string(),
        mechanism_name: "WatchdogTimer".to_string(),
        level: AnnotationLevel::Instance,
    }];

    println!("\n2. SAFETY ANNOTATIONS (from #[implements(...)])");
    println!("─────────────────────────────────────────────────────────────────────");
    println!("   Motor Controller: implements MotorSafety::EncoderComparison");
    println!("   TMR Voter:        implements MotorSafety::TMRVoting");
    println!("   Watchdog:         implements MotorSafety::WatchdogTimer");

    // Create simulation results with realistic DC values
    // TMR typically achieves very high DC
    let mut tmr_sim = SimulationCampaignResults::new("MotorSafety", "TmrVoter");
    let mut tmr_effect = EffectAnalysis::new("voter_error", Severity::S3, 99.5);
    tmr_effect.total_faults_causing = 10000;
    tmr_effect.faults_detected = 9950; // 99.5% DC
    tmr_effect.update_dc();
    tmr_effect
        .detection_by_mechanism
        .insert("TMRVoting".to_string(), 9950);
    tmr_sim
        .effect_analyses
        .insert("voter_error".to_string(), tmr_effect);

    // Watchdog achieves lower DC (only detects timing failures)
    let mut wd_sim = SimulationCampaignResults::new("MotorSafety", "Watchdog");
    let mut wd_effect = EffectAnalysis::new("timeout_failure", Severity::S2, 90.0);
    wd_effect.total_faults_causing = 1000;
    wd_effect.faults_detected = 900; // 90% DC
    wd_effect.update_dc();
    wd_effect
        .detection_by_mechanism
        .insert("WatchdogTimer".to_string(), 900);
    wd_sim
        .effect_analyses
        .insert("timeout_failure".to_string(), wd_effect);

    // Motor controller with encoder comparison
    let mut motor_sim = SimulationCampaignResults::new("MotorSafety", "MotorController");
    let mut motor_effect = EffectAnalysis::new("encoder_error", Severity::S3, 95.0);
    motor_effect.total_faults_causing = 5000;
    motor_effect.faults_detected = 4750; // 95% DC
    motor_effect.update_dc();
    motor_effect
        .detection_by_mechanism
        .insert("EncoderComparison".to_string(), 4750);
    motor_sim
        .effect_analyses
        .insert("encoder_error".to_string(), motor_effect);

    println!("\n3. FAULT INJECTION RESULTS (measured DC)");
    println!("─────────────────────────────────────────────────────────────────────");
    println!(
        "   Motor Controller: {:.1}% DC measured",
        motor_sim.effect_analyses["encoder_error"].measured_dc * 100.0
    );
    println!(
        "   TMR Voter:        {:.1}% DC measured",
        tmr_sim.effect_analyses["voter_error"].measured_dc * 100.0
    );
    println!(
        "   Watchdog:         {:.1}% DC measured",
        wd_sim.effect_analyses["timeout_failure"].measured_dc * 100.0
    );

    // Run FMEA conversion for each component
    let config = GateToFmeaConfig::default();

    let motor_fmea =
        gate_netlist_to_fmea(motor_netlist, &motor_annotations, Some(&motor_sim), &config);
    let tmr_fmea = gate_netlist_to_fmea(tmr_netlist, &tmr_annotations, Some(&tmr_sim), &config);
    let wd_fmea = gate_netlist_to_fmea(wd_netlist, &wd_annotations, Some(&wd_sim), &config);

    println!("\n4. FMEA RESULTS (with measured DC applied)");
    println!("─────────────────────────────────────────────────────────────────────");
    println!("Component           | Raw FIT | Residual FIT | Reduction");
    println!("─────────────────────────────────────────────────────────────────────");

    let motor_reduction =
        motor_fmea.coverage_summary.total_fit_residual / motor_fmea.coverage_summary.total_fit_raw;
    let tmr_reduction =
        tmr_fmea.coverage_summary.total_fit_residual / tmr_fmea.coverage_summary.total_fit_raw;
    let wd_reduction =
        wd_fmea.coverage_summary.total_fit_residual / wd_fmea.coverage_summary.total_fit_raw;

    println!(
        "Motor Controller    | {:7.4} | {:12.4} | {:.1}%",
        motor_fmea.coverage_summary.total_fit_raw,
        motor_fmea.coverage_summary.total_fit_residual,
        (1.0 - motor_reduction) * 100.0
    );
    println!(
        "TMR Voter           | {:7.4} | {:12.4} | {:.1}%",
        tmr_fmea.coverage_summary.total_fit_raw,
        tmr_fmea.coverage_summary.total_fit_residual,
        (1.0 - tmr_reduction) * 100.0
    );
    println!(
        "Watchdog            | {:7.4} | {:12.4} | {:.1}%",
        wd_fmea.coverage_summary.total_fit_raw,
        wd_fmea.coverage_summary.total_fit_residual,
        (1.0 - wd_reduction) * 100.0
    );

    // Total system analysis
    let total_raw = motor_fmea.coverage_summary.total_fit_raw
        + tmr_fmea.coverage_summary.total_fit_raw
        + wd_fmea.coverage_summary.total_fit_raw;
    let total_residual = motor_fmea.coverage_summary.total_fit_residual
        + tmr_fmea.coverage_summary.total_fit_residual
        + wd_fmea.coverage_summary.total_fit_residual;

    println!("─────────────────────────────────────────────────────────────────────");
    println!(
        "TOTAL               | {:7.4} | {:12.4} | {:.1}%",
        total_raw,
        total_residual,
        (1.0 - total_residual / total_raw) * 100.0
    );

    println!("\n5. ASIL COMPLIANCE CHECK");
    println!("─────────────────────────────────────────────────────────────────────");
    println!("Total residual FIT: {:.4}", total_residual);
    println!("ASIL D PMHF target: 10 FIT");
    if total_residual <= 10.0 {
        println!("[PASS] System meets ASIL D PMHF requirement");
    } else {
        println!(
            "[FAIL] System does not meet ASIL D PMHF requirement (gap: {:.2} FIT)",
            total_residual - 10.0
        );
    }

    // Verify the results are sensible
    assert!(
        motor_fmea.coverage_summary.dc_from_simulation,
        "Motor should have DC from simulation"
    );
    assert!(
        tmr_fmea.coverage_summary.dc_from_simulation,
        "TMR should have DC from simulation"
    );
    assert!(
        wd_fmea.coverage_summary.dc_from_simulation,
        "Watchdog should have DC from simulation"
    );

    // TMR with 99.5% DC should have ~0.5% residual
    assert!(
        tmr_reduction < 0.1,
        "TMR with 99.5% DC should have very low residual ratio"
    );

    println!("\n═══════════════════════════════════════════════════════════════════");
    println!("           INTEGRATION TEST COMPLETE");
    println!("═══════════════════════════════════════════════════════════════════\n");
}
