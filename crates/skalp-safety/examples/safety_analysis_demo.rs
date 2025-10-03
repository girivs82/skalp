//! Safety Analysis Demo for SKALP
//!
//! Demonstrates comprehensive ISO 26262 safety analysis capabilities

use skalp_safety::asil::AsilLevel;
use skalp_safety::fmea::{AnalysisDepth, FmeaConfig, FmeaGenerator};
use skalp_safety::mechanisms::{
    MechanismCategory, MechanismType, SafetyMechanism, SafetyMechanismManager,
};
use skalp_safety::metrics::{CalculationConfig, ComplianceStatus, SafetyMetricsCalculator};
use skalp_safety::power_domains::{
    self, IsolationRequirements, PowerDomain, PowerDomainManager, PowerDomainType, PowerSourceType,
    PowerState, PowerSupply,
};
use skalp_safety::requirements::{
    RequirementCategory, SafetyRequirement, SafetyRequirementManager,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== SKALP Safety Framework Demo ===\n");

    // 1. Create safety requirements with ASIL allocation
    println!("1. Safety Requirements Analysis");
    let mut req_manager = SafetyRequirementManager::new();

    let safety_req = SafetyRequirement::new(
        "REQ-SAFETY-001".to_string(),
        "System shall detect and respond to processor failures within 10ms".to_string(),
        AsilLevel::D,
        RequirementCategory::Functional,
    );

    req_manager.add_requirement(safety_req);

    let coverage_report = req_manager.generate_coverage_report();
    println!(
        "   Total requirements: {}",
        coverage_report.total_requirements
    );
    println!(
        "   ASIL D requirements: {:?}\n",
        coverage_report.asil_breakdown.get(&AsilLevel::D)
    );

    // 2. Define safety mechanisms (PSM/LSM)
    println!("2. Safety Mechanisms");
    let mut mechanism_manager = SafetyMechanismManager::new();

    // Create ECC mechanism
    let mut ecc_mechanism = SafetyMechanism::new(
        "ECC-001".to_string(),
        "Error Correcting Code".to_string(),
        "SECDED ECC for memory protection".to_string(),
        MechanismType::Primary,
        MechanismCategory::ErrorDetection,
    );
    ecc_mechanism.set_fault_coverage(99.9);
    ecc_mechanism.set_diagnostic_coverage(95.0);

    // Create watchdog mechanism
    let mut watchdog_mechanism = SafetyMechanism::new(
        "WATCHDOG-001".to_string(),
        "Hardware Watchdog".to_string(),
        "Independent watchdog for processor monitoring".to_string(),
        MechanismType::Latent,
        MechanismCategory::Monitoring,
    );
    watchdog_mechanism.set_diagnostic_coverage(98.0);

    mechanism_manager.add_mechanism(ecc_mechanism);
    mechanism_manager.add_mechanism(watchdog_mechanism);

    // Assign mechanisms to components
    mechanism_manager.assign_mechanism("memory_controller".to_string(), "ECC-001".to_string());
    mechanism_manager.assign_mechanism("processor_core".to_string(), "WATCHDOG-001".to_string());

    let mechanism_report = mechanism_manager.generate_report();
    println!("   Total mechanisms: {}", mechanism_report.total_mechanisms);
    println!(
        "   Average fault coverage: {:.1}%",
        mechanism_report.avg_fault_coverage
    );
    println!(
        "   Average diagnostic coverage: {:.1}%\n",
        mechanism_report.avg_diagnostic_coverage
    );

    // 3. Generate FMEA
    println!("3. FMEA Generation");
    let fmea_config = FmeaConfig {
        include_common_modes: true,
        quantitative_analysis: true,
        asil_decomposition: false,
        include_software: false,
        analysis_depth: AnalysisDepth::Component,
    };

    let fmea_generator = FmeaGenerator::new(fmea_config);

    // In a real scenario, this would use actual design entities and intents
    let fmea_analysis = fmea_generator.generate_from_design(&[], &[], &[])?;

    println!("   FMEA ID: {}", fmea_analysis.metadata.id);
    println!("   Target ASIL: {:?}", fmea_analysis.metadata.target_asil);
    println!(
        "   Total FMEA entries: {}",
        fmea_analysis.summary.total_entries
    );
    println!(
        "   Analysis date: {}\n",
        fmea_analysis.metadata.analysis_date.format("%Y-%m-%d")
    );

    // 4. Calculate safety metrics
    println!("4. Safety Metrics Calculation");
    let metrics_config = CalculationConfig::default();
    let metrics_calculator = SafetyMetricsCalculator::new(metrics_config);

    let mechanisms: Vec<SafetyMechanism> = mechanism_manager.get_all_mechanisms();
    let safety_metrics = metrics_calculator.calculate_metrics(&fmea_analysis, &mechanisms)?;

    println!("   SPFM: {:.1}%", safety_metrics.hardware_metrics.spfm);
    println!("   LF: {:.1}%", safety_metrics.hardware_metrics.lf);
    println!("   PMHF: {:.1} FIT", safety_metrics.hardware_metrics.pmhf);

    match safety_metrics.compliance_assessment.overall_compliance {
        ComplianceStatus::Compliant => println!("   ✅ ASIL compliance: PASSED"),
        ComplianceStatus::CompliantWithMargins => {
            println!("   ✅ ASIL compliance: PASSED with margins")
        }
        _ => println!("   ❌ ASIL compliance: FAILED"),
    }
    println!();

    // 5. Power domain analysis
    println!("5. Power Domain Safety Analysis");
    let mut power_manager = PowerDomainManager::new();

    // Create safety-critical power domain
    let safety_domain = PowerDomain {
        id: "safety_domain".to_string(),
        name: "Safety Critical Domain".to_string(),
        domain_type: PowerDomainType::SafetyCritical,
        safety_level: AsilLevel::D,
        power_supply: PowerSupply {
            nominal_voltage: 3.3,
            voltage_tolerance: 5.0,
            max_current: 2.0,
            source_type: PowerSourceType::LinearRegulator,
            redundancy_level: 1,
            brownout_threshold: 2.8,
            power_good_available: true,
        },
        components: vec!["processor_core".to_string(), "safety_monitor".to_string()],
        clock_domains: vec!["clk_safety".to_string()],
        power_states: vec![PowerState::Active, PowerState::Standby],
        current_state: PowerState::Active,
        isolation_requirements: IsolationRequirements {
            electrical_isolation: true,
            galvanic_isolation: false,
            optical_isolation: false,
            min_isolation_voltage: 8000.0,
            isolation_tests: vec![],
            signal_constraints: Default::default(),
        },
        status: power_domains::DomainStatus::Operational,
        created_at: chrono::Utc::now(),
    };

    power_manager.add_domain(safety_domain)?;

    let power_report = power_manager.get_power_consumption_report();
    println!(
        "   Total power consumption: {:.1} W",
        power_report.total_power_consumption
    );
    println!(
        "   Power efficiency: {:.1}%",
        power_report.efficiency_metrics.power_efficiency
    );

    // Try to power off safety domain (should fail)
    match power_manager.change_power_state("safety_domain", PowerState::PoweredOff) {
        Ok(_) => println!("   ❌ Safety violation: Critical domain powered off"),
        Err(_) => println!("   ✅ Safety protection: Critical domain power-off prevented"),
    }

    println!("\n=== Safety Analysis Complete ===");
    println!("SKALP Safety Framework successfully demonstrates:");
    println!("• ISO 26262 ASIL requirements management");
    println!("• Comprehensive safety mechanisms (PSM/LSM)");
    println!("• Automated FMEA generation");
    println!("• Hardware safety metrics calculation");
    println!("• Power domain safety isolation");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_safety_demo() {
        // Test that the demo runs without panicking
        assert!(main().is_ok());
    }
}
