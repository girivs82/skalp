//! Built-in Technology Libraries
//!
//! Provides pre-defined technology libraries for common use cases:
//! - `generic_asic` - Conservative FIT rates for generic ASIC (default)
//! - `asic_7nm` - 7nm process node with optimized cells
//! - `asic_28nm` - 28nm process node
//! - `fpga_lut4` - FPGA with 4-input LUTs
//! - `fpga_lut6` - FPGA with 6-input LUTs

use crate::gate_netlist::FaultType;
use crate::tech_library::{
    CellFunction, DecompConnectivity, DecompSource, DecompositionRule, LibraryCell,
    LibraryFailureMode, TechLibrary,
};

/// Get a built-in library by name
pub fn get_builtin_library(name: &str) -> Option<TechLibrary> {
    match name {
        "generic_asic" | "generic" | "default" => Some(builtin_generic_asic()),
        "asic_7nm" | "7nm" => Some(builtin_asic_7nm()),
        "asic_28nm" | "28nm" => Some(builtin_asic_28nm()),
        "fpga_lut4" | "fpga" => Some(builtin_fpga_lut4()),
        "fpga_lut6" => Some(builtin_fpga_lut6()),
        _ => None,
    }
}

/// List all available built-in library names
pub fn list_builtin_libraries() -> Vec<&'static str> {
    vec![
        "generic_asic",
        "asic_7nm",
        "asic_28nm",
        "fpga_lut4",
        "fpga_lut6",
    ]
}

/// Generic ASIC library with conservative FIT rates
///
/// This is the default library for when no specific technology is known.
/// FIT rates are conservative estimates suitable for ISO 26262 FMEDA.
pub fn builtin_generic_asic() -> TechLibrary {
    let mut lib = TechLibrary::new("generic_asic");
    lib.process_node = Some(28);
    lib.version = Some("1.0.0".to_string());
    lib.reference_temperature = Some(85.0);
    lib.reference_voltage = Some(0.9);

    // === Basic Gates ===

    // Inverter
    lib.add_cell(make_cell("INV_X1", CellFunction::Inv, 0.05, 2, 0.4));
    lib.add_cell(make_cell("INV_X2", CellFunction::Inv, 0.06, 4, 0.6));

    // Buffer
    lib.add_cell(make_cell("BUF_X1", CellFunction::Buf, 0.05, 4, 0.5));
    lib.add_cell(make_cell("BUF_X2", CellFunction::Buf, 0.06, 8, 0.8));

    // NAND gates
    lib.add_cell(make_cell("NAND2_X1", CellFunction::Nand2, 0.10, 4, 0.8));
    lib.add_cell(make_cell("NAND3_X1", CellFunction::Nand3, 0.15, 6, 1.2));
    lib.add_cell(make_cell("NAND4_X1", CellFunction::Nand4, 0.20, 8, 1.6));

    // NOR gates
    lib.add_cell(make_cell("NOR2_X1", CellFunction::Nor2, 0.10, 4, 0.8));
    lib.add_cell(make_cell("NOR3_X1", CellFunction::Nor3, 0.15, 6, 1.2));
    lib.add_cell(make_cell("NOR4_X1", CellFunction::Nor4, 0.20, 8, 1.6));

    // AND gates
    lib.add_cell(make_cell("AND2_X1", CellFunction::And2, 0.12, 6, 1.0));
    lib.add_cell(make_cell("AND3_X1", CellFunction::And3, 0.18, 8, 1.4));
    lib.add_cell(make_cell("AND4_X1", CellFunction::And4, 0.24, 10, 1.8));

    // OR gates
    lib.add_cell(make_cell("OR2_X1", CellFunction::Or2, 0.12, 6, 1.0));
    lib.add_cell(make_cell("OR3_X1", CellFunction::Or3, 0.18, 8, 1.4));
    lib.add_cell(make_cell("OR4_X1", CellFunction::Or4, 0.24, 10, 1.8));

    // XOR gates
    lib.add_cell(make_cell("XOR2_X1", CellFunction::Xor2, 0.15, 8, 1.6));
    lib.add_cell(make_cell("XNOR2_X1", CellFunction::Xnor2, 0.15, 8, 1.6));

    // === Complex Gates ===

    // AOI (AND-OR-Invert)
    lib.add_cell(make_cell("AOI21_X1", CellFunction::Aoi21, 0.14, 6, 1.2));
    lib.add_cell(make_cell("AOI22_X1", CellFunction::Aoi22, 0.18, 8, 1.6));

    // OAI (OR-AND-Invert)
    lib.add_cell(make_cell("OAI21_X1", CellFunction::Oai21, 0.14, 6, 1.2));
    lib.add_cell(make_cell("OAI22_X1", CellFunction::Oai22, 0.18, 8, 1.6));

    // === Multiplexers ===
    lib.add_cell(make_cell("MUX2_X1", CellFunction::Mux2, 0.18, 12, 2.0));
    lib.add_cell(make_cell("MUX4_X1", CellFunction::Mux4, 0.36, 28, 4.0));

    // === Arithmetic ===
    lib.add_cell(make_cell("HA_X1", CellFunction::HalfAdder, 0.20, 10, 2.0));
    lib.add_cell(make_cell("FA_X1", CellFunction::FullAdder, 0.30, 14, 3.0));

    // === Sequential ===
    lib.add_cell(make_seq_cell("DFF_X1", CellFunction::Dff, 0.25, 20, 4.0));
    lib.add_cell(make_seq_cell("DFFR_X1", CellFunction::DffR, 0.30, 24, 5.0));
    lib.add_cell(make_seq_cell("DFFE_X1", CellFunction::DffE, 0.30, 24, 5.0));
    lib.add_cell(make_seq_cell(
        "DFFRE_X1",
        CellFunction::DffRE,
        0.35,
        28,
        6.0,
    ));
    lib.add_cell(make_seq_cell(
        "LATCH_X1",
        CellFunction::Latch,
        0.15,
        12,
        2.5,
    ));

    // === Tristate ===
    lib.add_cell(make_cell("TBUF_X1", CellFunction::Tristate, 0.12, 6, 1.2));

    // === Decomposition Rules ===
    add_standard_decomposition_rules(&mut lib);

    lib
}

/// 7nm ASIC library
///
/// Optimized cells with lower FIT rates due to smaller transistors
/// but with higher soft error sensitivity.
pub fn builtin_asic_7nm() -> TechLibrary {
    let mut lib = TechLibrary::new("asic_7nm");
    lib.process_node = Some(7);
    lib.version = Some("1.0.0".to_string());
    lib.reference_temperature = Some(85.0);
    lib.reference_voltage = Some(0.75);

    // 7nm has ~3x improvement in area but ~1.5x increase in soft error rate
    // Net FIT is roughly similar but scaled differently

    // Basic gates
    lib.add_cell(make_cell("INV_X1", CellFunction::Inv, 0.04, 2, 0.15));
    lib.add_cell(make_cell("BUF_X1", CellFunction::Buf, 0.04, 4, 0.2));
    lib.add_cell(make_cell("NAND2_X1", CellFunction::Nand2, 0.08, 4, 0.3));
    lib.add_cell(make_cell("NOR2_X1", CellFunction::Nor2, 0.08, 4, 0.3));
    lib.add_cell(make_cell("AND2_X1", CellFunction::And2, 0.10, 6, 0.4));
    lib.add_cell(make_cell("OR2_X1", CellFunction::Or2, 0.10, 6, 0.4));
    lib.add_cell(make_cell("XOR2_X1", CellFunction::Xor2, 0.12, 8, 0.6));
    lib.add_cell(make_cell("XNOR2_X1", CellFunction::Xnor2, 0.12, 8, 0.6));

    // Multiplexers
    lib.add_cell(make_cell("MUX2_X1", CellFunction::Mux2, 0.15, 12, 0.8));

    // Arithmetic
    lib.add_cell(make_cell("HA_X1", CellFunction::HalfAdder, 0.16, 10, 0.8));
    lib.add_cell(make_cell("FA_X1", CellFunction::FullAdder, 0.24, 14, 1.2));

    // Sequential (higher soft error rate in 7nm)
    lib.add_cell(make_seq_cell("DFF_X1", CellFunction::Dff, 0.30, 20, 1.5));
    lib.add_cell(make_seq_cell("DFFR_X1", CellFunction::DffR, 0.35, 24, 2.0));
    lib.add_cell(make_seq_cell(
        "LATCH_X1",
        CellFunction::Latch,
        0.18,
        12,
        1.0,
    ));

    add_standard_decomposition_rules(&mut lib);

    lib
}

/// 28nm ASIC library
///
/// Mature process with well-characterized FIT rates.
pub fn builtin_asic_28nm() -> TechLibrary {
    let mut lib = TechLibrary::new("asic_28nm");
    lib.process_node = Some(28);
    lib.version = Some("1.0.0".to_string());
    lib.reference_temperature = Some(85.0);
    lib.reference_voltage = Some(0.9);

    // 28nm is mature with good reliability
    lib.add_cell(make_cell("INV_X1", CellFunction::Inv, 0.05, 2, 0.4));
    lib.add_cell(make_cell("BUF_X1", CellFunction::Buf, 0.05, 4, 0.5));
    lib.add_cell(make_cell("NAND2_X1", CellFunction::Nand2, 0.10, 4, 0.8));
    lib.add_cell(make_cell("NOR2_X1", CellFunction::Nor2, 0.10, 4, 0.8));
    lib.add_cell(make_cell("AND2_X1", CellFunction::And2, 0.12, 6, 1.0));
    lib.add_cell(make_cell("OR2_X1", CellFunction::Or2, 0.12, 6, 1.0));
    lib.add_cell(make_cell("XOR2_X1", CellFunction::Xor2, 0.15, 8, 1.6));
    lib.add_cell(make_cell("XNOR2_X1", CellFunction::Xnor2, 0.15, 8, 1.6));

    lib.add_cell(make_cell("MUX2_X1", CellFunction::Mux2, 0.18, 12, 2.0));

    lib.add_cell(make_cell("HA_X1", CellFunction::HalfAdder, 0.20, 10, 2.0));
    lib.add_cell(make_cell("FA_X1", CellFunction::FullAdder, 0.30, 14, 3.0));

    lib.add_cell(make_seq_cell("DFF_X1", CellFunction::Dff, 0.25, 20, 4.0));
    lib.add_cell(make_seq_cell("DFFR_X1", CellFunction::DffR, 0.30, 24, 5.0));
    lib.add_cell(make_seq_cell(
        "LATCH_X1",
        CellFunction::Latch,
        0.15,
        12,
        2.5,
    ));

    add_standard_decomposition_rules(&mut lib);

    lib
}

/// FPGA library with 4-input LUTs
///
/// Higher FIT rates due to configuration memory (SRAM) susceptibility.
pub fn builtin_fpga_lut4() -> TechLibrary {
    let mut lib = TechLibrary::new("fpga_lut4");
    lib.version = Some("1.0.0".to_string());
    lib.reference_temperature = Some(85.0);
    lib.reference_voltage = Some(1.0);

    // FPGA LUTs have higher FIT due to configuration SRAM
    // Each LUT4 can implement any 4-input function

    // LUT4 mapped to common functions
    lib.add_cell(make_fpga_cell("LUT4_INV", CellFunction::Inv, 0.20));
    lib.add_cell(make_fpga_cell("LUT4_BUF", CellFunction::Buf, 0.20));
    lib.add_cell(make_fpga_cell("LUT4_NAND2", CellFunction::Nand2, 0.20));
    lib.add_cell(make_fpga_cell("LUT4_NOR2", CellFunction::Nor2, 0.20));
    lib.add_cell(make_fpga_cell("LUT4_AND2", CellFunction::And2, 0.20));
    lib.add_cell(make_fpga_cell("LUT4_OR2", CellFunction::Or2, 0.20));
    lib.add_cell(make_fpga_cell("LUT4_XOR2", CellFunction::Xor2, 0.20));
    lib.add_cell(make_fpga_cell("LUT4_XNOR2", CellFunction::Xnor2, 0.20));
    lib.add_cell(make_fpga_cell("LUT4_MUX2", CellFunction::Mux2, 0.20));
    lib.add_cell(make_fpga_cell("LUT4_AND3", CellFunction::And3, 0.20));
    lib.add_cell(make_fpga_cell("LUT4_OR3", CellFunction::Or3, 0.20));
    lib.add_cell(make_fpga_cell("LUT4_AND4", CellFunction::And4, 0.20));
    lib.add_cell(make_fpga_cell("LUT4_OR4", CellFunction::Or4, 0.20));

    // FPGA carry chain (optimized arithmetic)
    lib.add_cell(make_fpga_cell("CARRY_HA", CellFunction::HalfAdder, 0.25));
    lib.add_cell(make_fpga_cell("CARRY_FA", CellFunction::FullAdder, 0.30));

    // FPGA flip-flops (embedded in CLB)
    lib.add_cell(make_fpga_seq_cell("FF", CellFunction::Dff, 0.30));
    lib.add_cell(make_fpga_seq_cell("FFR", CellFunction::DffR, 0.35));
    lib.add_cell(make_fpga_seq_cell("FFE", CellFunction::DffE, 0.35));

    add_standard_decomposition_rules(&mut lib);

    lib
}

/// FPGA library with 6-input LUTs
///
/// More capable LUTs that can implement larger functions.
pub fn builtin_fpga_lut6() -> TechLibrary {
    let mut lib = TechLibrary::new("fpga_lut6");
    lib.version = Some("1.0.0".to_string());
    lib.reference_temperature = Some(85.0);
    lib.reference_voltage = Some(1.0);

    // LUT6 has more SRAM bits, so slightly higher FIT
    lib.add_cell(make_fpga_cell("LUT6_INV", CellFunction::Inv, 0.25));
    lib.add_cell(make_fpga_cell("LUT6_BUF", CellFunction::Buf, 0.25));
    lib.add_cell(make_fpga_cell("LUT6_NAND2", CellFunction::Nand2, 0.25));
    lib.add_cell(make_fpga_cell("LUT6_NOR2", CellFunction::Nor2, 0.25));
    lib.add_cell(make_fpga_cell("LUT6_AND2", CellFunction::And2, 0.25));
    lib.add_cell(make_fpga_cell("LUT6_OR2", CellFunction::Or2, 0.25));
    lib.add_cell(make_fpga_cell("LUT6_XOR2", CellFunction::Xor2, 0.25));
    lib.add_cell(make_fpga_cell("LUT6_XNOR2", CellFunction::Xnor2, 0.25));
    lib.add_cell(make_fpga_cell("LUT6_MUX2", CellFunction::Mux2, 0.25));

    // LUT6 can implement MUX4 in one LUT
    lib.add_cell(make_fpga_cell("LUT6_MUX4", CellFunction::Mux4, 0.25));

    // LUT6 can implement up to 6-input gates
    lib.add_cell(make_fpga_cell("LUT6_AND3", CellFunction::And3, 0.25));
    lib.add_cell(make_fpga_cell("LUT6_OR3", CellFunction::Or3, 0.25));
    lib.add_cell(make_fpga_cell("LUT6_AND4", CellFunction::And4, 0.25));
    lib.add_cell(make_fpga_cell("LUT6_OR4", CellFunction::Or4, 0.25));

    // Carry chain
    lib.add_cell(make_fpga_cell("CARRY_HA", CellFunction::HalfAdder, 0.30));
    lib.add_cell(make_fpga_cell("CARRY_FA", CellFunction::FullAdder, 0.35));

    // Flip-flops
    lib.add_cell(make_fpga_seq_cell("FF", CellFunction::Dff, 0.35));
    lib.add_cell(make_fpga_seq_cell("FFR", CellFunction::DffR, 0.40));
    lib.add_cell(make_fpga_seq_cell("FFE", CellFunction::DffE, 0.40));

    add_standard_decomposition_rules(&mut lib);

    lib
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Create a combinational cell with realistic failure mode distributions
///
/// Failure mode distribution for combinational logic:
/// - Stuck-at faults: 60% (symmetric between 0 and 1)
/// - Transient (soft error): 15% - particle strikes, voltage glitches
/// - Bridge (short): 10% - manufacturing defects, electromigration
/// - Open circuit: 8% - via/contact failures, electromigration
/// - Delay: 7% - process variation, aging effects
///
/// These distributions are based on typical ASIC failure mechanisms
/// and are suitable for ISO 26262 FMEDA analysis.
fn make_cell(
    name: &str,
    function: CellFunction,
    fit: f64,
    transistor_count: u32,
    area: f64,
) -> LibraryCell {
    let (inputs, outputs) = function.default_pins();
    LibraryCell {
        name: name.to_string(),
        function,
        fit,
        area: Some(area),
        transistor_count: Some(transistor_count),
        inputs,
        outputs,
        failure_modes: vec![
            LibraryFailureMode::new("stuck_at_0", fit * 0.30, FaultType::StuckAt0)
                .with_mechanism("oxide_breakdown"),
            LibraryFailureMode::new("stuck_at_1", fit * 0.30, FaultType::StuckAt1)
                .with_mechanism("electromigration"),
            LibraryFailureMode::new("transient", fit * 0.15, FaultType::Transient)
                .with_mechanism("radiation_seu")
                .with_recovery_time_ns(1.0),
            LibraryFailureMode::new("bridge", fit * 0.10, FaultType::Bridge)
                .with_mechanism("metal_bridge"),
            LibraryFailureMode::new("open", fit * 0.08, FaultType::Open)
                .with_mechanism("via_failure"),
            LibraryFailureMode::new("delay", fit * 0.07, FaultType::Delay)
                .with_mechanism("process_variation"),
        ],
    }
}

/// Create a sequential cell with realistic failure mode distributions
///
/// Failure mode distribution for sequential elements (flip-flops, latches):
/// - Stuck-at faults: 40% (data path failures)
/// - Timing (setup/hold): 20% - critical for sequential elements
/// - Clock path: 15% - clock buffer, routing failures
/// - Data retention: 10% - soft errors in storage node
/// - Transient: 8% - particle strikes
/// - Reset path: 7% - reset buffer, routing failures (if applicable)
///
/// Sequential cells have more complex failure mechanisms due to
/// feedback paths, clock/data timing relationships, and state retention.
fn make_seq_cell(
    name: &str,
    function: CellFunction,
    fit: f64,
    transistor_count: u32,
    area: f64,
) -> LibraryCell {
    let (inputs, outputs) = function.default_pins();
    LibraryCell {
        name: name.to_string(),
        function,
        fit,
        area: Some(area),
        transistor_count: Some(transistor_count),
        inputs,
        outputs,
        failure_modes: vec![
            LibraryFailureMode::new("stuck_at_0", fit * 0.20, FaultType::StuckAt0)
                .with_mechanism("oxide_breakdown"),
            LibraryFailureMode::new("stuck_at_1", fit * 0.20, FaultType::StuckAt1)
                .with_mechanism("electromigration"),
            LibraryFailureMode::new("setup_violation", fit * 0.10, FaultType::Timing)
                .with_mechanism("setup_slack_violation"),
            LibraryFailureMode::new("hold_violation", fit * 0.10, FaultType::Timing)
                .with_mechanism("hold_slack_violation"),
            LibraryFailureMode::new("clock_path", fit * 0.15, FaultType::ClockPath)
                .with_mechanism("clock_buffer_failure"),
            LibraryFailureMode::new("data_retention", fit * 0.10, FaultType::DataRetention)
                .with_mechanism("sram_bit_flip")
                .with_soft_error_cross_section(1.2e-15),
            LibraryFailureMode::new("transient", fit * 0.08, FaultType::Transient)
                .with_mechanism("single_event_upset")
                .with_recovery_time_ns(1.0),
            LibraryFailureMode::new("reset_path", fit * 0.07, FaultType::ResetPath)
                .with_mechanism("reset_buffer_failure"),
        ],
    }
}

/// Create an FPGA LUT cell with realistic failure mode distributions
///
/// FPGA failure modes are dominated by SRAM configuration memory upsets:
/// - Configuration upset: 50% - radiation-induced bit flips in SRAM
/// - Stuck-at: 20% - permanent logic failures
/// - Transient: 15% - single-event transients in logic
/// - Routing: 10% - interconnect failures (open/bridge)
/// - Delay: 5% - timing degradation
///
/// FPGAs have higher soft error rates than ASICs due to SRAM configuration,
/// but failures can often be recovered by scrubbing/reconfiguration.
fn make_fpga_cell(name: &str, function: CellFunction, fit: f64) -> LibraryCell {
    let (inputs, outputs) = function.default_pins();
    LibraryCell {
        name: name.to_string(),
        function,
        fit,
        area: None,
        transistor_count: None,
        inputs,
        outputs,
        failure_modes: vec![
            LibraryFailureMode::new("config_upset", fit * 0.50, FaultType::DataRetention)
                .with_mechanism("sram_soft_error")
                .with_soft_error_cross_section(2.5e-14),
            LibraryFailureMode::new("stuck_at_0", fit * 0.10, FaultType::StuckAt0)
                .with_mechanism("oxide_breakdown"),
            LibraryFailureMode::new("stuck_at_1", fit * 0.10, FaultType::StuckAt1)
                .with_mechanism("electromigration"),
            LibraryFailureMode::new("transient", fit * 0.15, FaultType::Transient)
                .with_mechanism("single_event_transient")
                .with_recovery_time_ns(1.0),
            LibraryFailureMode::new("routing_open", fit * 0.05, FaultType::Open)
                .with_mechanism("interconnect_failure"),
            LibraryFailureMode::new("routing_bridge", fit * 0.05, FaultType::Bridge)
                .with_mechanism("interconnect_bridge"),
            LibraryFailureMode::new("delay", fit * 0.05, FaultType::Delay)
                .with_mechanism("process_variation"),
        ],
    }
}

/// Create an FPGA flip-flop cell with realistic failure mode distributions
///
/// FPGA sequential elements have unique failure modes:
/// - Configuration upset: 35% - SRAM control bit flips
/// - Data retention: 20% - storage node upsets
/// - Timing: 20% - setup/hold violations
/// - Stuck-at: 15% - permanent logic failures
/// - Clock routing: 10% - clock distribution failures
fn make_fpga_seq_cell(name: &str, function: CellFunction, fit: f64) -> LibraryCell {
    let (inputs, outputs) = function.default_pins();
    LibraryCell {
        name: name.to_string(),
        function,
        fit,
        area: None,
        transistor_count: None,
        inputs,
        outputs,
        failure_modes: vec![
            LibraryFailureMode::new("config_upset", fit * 0.35, FaultType::DataRetention)
                .with_mechanism("sram_soft_error")
                .with_soft_error_cross_section(2.5e-14),
            LibraryFailureMode::new("data_retention", fit * 0.20, FaultType::DataRetention)
                .with_mechanism("storage_node_upset")
                .with_soft_error_cross_section(1.5e-14),
            LibraryFailureMode::new("timing", fit * 0.20, FaultType::Timing)
                .with_mechanism("setup_hold_violation"),
            LibraryFailureMode::new("stuck_at_0", fit * 0.07, FaultType::StuckAt0)
                .with_mechanism("oxide_breakdown"),
            LibraryFailureMode::new("stuck_at_1", fit * 0.08, FaultType::StuckAt1)
                .with_mechanism("electromigration"),
            LibraryFailureMode::new("clock_path", fit * 0.10, FaultType::ClockPath)
                .with_mechanism("clock_routing_failure"),
        ],
    }
}

/// Add standard decomposition rules to a library
fn add_standard_decomposition_rules(lib: &mut TechLibrary) {
    // XOR to NAND decomposition (if library lacks XOR)
    // XOR = NAND(NAND(a, NAND(a,b)), NAND(b, NAND(a,b)))
    lib.add_decomposition_rule(DecompositionRule {
        name: "xor_to_nand".to_string(),
        source: DecompSource::Xor,
        targets: vec![
            CellFunction::Nand2,
            CellFunction::Nand2,
            CellFunction::Nand2,
            CellFunction::Nand2,
        ],
        connectivity: DecompConnectivity::Custom("xor_nand".to_string()),
        fit_multiplier: 0.9,
    });

    // AND to NAND + INV
    lib.add_decomposition_rule(DecompositionRule {
        name: "and_to_nand_inv".to_string(),
        source: DecompSource::And,
        targets: vec![CellFunction::Nand2, CellFunction::Inv],
        connectivity: DecompConnectivity::Chain,
        fit_multiplier: 1.0,
    });

    // OR to NOR + INV
    lib.add_decomposition_rule(DecompositionRule {
        name: "or_to_nor_inv".to_string(),
        source: DecompSource::Or,
        targets: vec![CellFunction::Nor2, CellFunction::Inv],
        connectivity: DecompConnectivity::Chain,
        fit_multiplier: 1.0,
    });

    // Adder decomposition
    lib.add_decomposition_rule(DecompositionRule {
        name: "add_to_fa_chain".to_string(),
        source: DecompSource::Add,
        targets: vec![CellFunction::HalfAdder, CellFunction::FullAdder],
        connectivity: DecompConnectivity::RippleCarry,
        fit_multiplier: 1.0,
    });

    // Mux2 decomposition (if no MUX cell)
    // MUX2 = OR(AND(sel, d1), AND(NOT(sel), d0))
    lib.add_decomposition_rule(DecompositionRule {
        name: "mux_to_aoi".to_string(),
        source: DecompSource::Mux2,
        targets: vec![
            CellFunction::Inv,
            CellFunction::And2,
            CellFunction::And2,
            CellFunction::Or2,
        ],
        connectivity: DecompConnectivity::Custom("mux_aoi".to_string()),
        fit_multiplier: 0.95,
    });
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generic_asic_library() {
        let lib = builtin_generic_asic();
        assert_eq!(lib.name, "generic_asic");
        assert_eq!(lib.process_node, Some(28));

        // Should have basic cells
        assert!(lib.has_function(&CellFunction::Inv));
        assert!(lib.has_function(&CellFunction::Nand2));
        assert!(lib.has_function(&CellFunction::FullAdder));
        assert!(lib.has_function(&CellFunction::Dff));

        // Cell count sanity check
        assert!(lib.cell_count() > 20);
    }

    #[test]
    fn test_7nm_library() {
        let lib = builtin_asic_7nm();
        assert_eq!(lib.name, "asic_7nm");
        assert_eq!(lib.process_node, Some(7));

        // 7nm should have lower FIT for logic but similar for sequential
        let inv = lib.find_best_cell(&CellFunction::Inv).unwrap();
        assert!(inv.fit < 0.05);
    }

    #[test]
    fn test_fpga_library() {
        let lib = builtin_fpga_lut4();
        assert_eq!(lib.name, "fpga_lut4");

        // FPGA should have higher FIT due to config memory
        let inv = lib.find_best_cell(&CellFunction::Inv).unwrap();
        assert!(inv.fit >= 0.15);

        // Should have config_upset failure mode
        assert!(inv.failure_modes.iter().any(|f| f.name == "config_upset"));
    }

    #[test]
    fn test_get_builtin() {
        assert!(get_builtin_library("generic_asic").is_some());
        assert!(get_builtin_library("default").is_some());
        assert!(get_builtin_library("7nm").is_some());
        assert!(get_builtin_library("fpga").is_some());
        assert!(get_builtin_library("nonexistent").is_none());
    }

    #[test]
    fn test_list_libraries() {
        let libs = list_builtin_libraries();
        assert!(libs.contains(&"generic_asic"));
        assert!(libs.contains(&"fpga_lut4"));
    }

    #[test]
    fn test_failure_modes() {
        let lib = builtin_generic_asic();
        let nand = lib.find_best_cell(&CellFunction::Nand2).unwrap();

        // Should have failure modes
        assert!(!nand.failure_modes.is_empty());

        // Total failure mode FIT should equal cell FIT
        let total_fm_fit: f64 = nand.failure_modes.iter().map(|f| f.fit).sum();
        assert!((total_fm_fit - nand.fit).abs() < 0.001);
    }

    #[test]
    fn test_decomposition_rules() {
        let lib = builtin_generic_asic();
        assert!(!lib.decomposition_rules.is_empty());

        // Should have XOR decomposition
        let xor_rule = lib
            .decomposition_rules
            .iter()
            .find(|r| r.source == DecompSource::Xor);
        assert!(xor_rule.is_some());
    }
}
