//! Power domain support for safety isolation in ISO 26262 designs
//!
//! Provides power domain management and isolation analysis for safety-critical systems.
//! Supports multiple power domains with controlled cross-domain communication.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use chrono::{DateTime, Utc};
use crate::asil::AsilLevel;
use petgraph::Graph;
use petgraph::graph::NodeIndex;

/// Power domain management system
#[derive(Debug, Clone)]
pub struct PowerDomainManager {
    /// All power domains in the system
    pub domains: HashMap<String, PowerDomain>,
    /// Domain hierarchy graph
    pub hierarchy: DomainHierarchy,
    /// Isolation barriers between domains
    pub isolation_barriers: Vec<IsolationBarrier>,
    /// Cross-domain communication channels
    pub communication_channels: Vec<CrossDomainChannel>,
    /// Power management policies
    pub power_policies: Vec<PowerPolicy>,
}

/// Individual power domain definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PowerDomain {
    /// Domain identifier
    pub id: String,
    /// Human-readable name
    pub name: String,
    /// Domain type
    pub domain_type: PowerDomainType,
    /// Safety criticality level
    pub safety_level: AsilLevel,
    /// Power supply characteristics
    pub power_supply: PowerSupply,
    /// Components in this domain
    pub components: Vec<String>,
    /// Clock domains within this power domain
    pub clock_domains: Vec<String>,
    /// Power states supported
    pub power_states: Vec<PowerState>,
    /// Current power state
    pub current_state: PowerState,
    /// Isolation requirements
    pub isolation_requirements: IsolationRequirements,
    /// Domain status
    pub status: DomainStatus,
    /// Creation timestamp
    pub created_at: DateTime<Utc>,
}

/// Types of power domains
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum PowerDomainType {
    /// Always-on domain (never powered down)
    AlwaysOn,
    /// Safety-critical domain
    SafetyCritical,
    /// Mission-critical domain
    MissionCritical,
    /// Performance domain (can be power-gated)
    Performance,
    /// Low-power domain
    LowPower,
    /// Test/Debug domain
    TestDebug,
    /// External interface domain
    ExternalInterface,
}

/// Power supply characteristics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PowerSupply {
    /// Nominal voltage (V)
    pub nominal_voltage: f64,
    /// Voltage tolerance (percentage)
    pub voltage_tolerance: f64,
    /// Maximum current (A)
    pub max_current: f64,
    /// Power source type
    pub source_type: PowerSourceType,
    /// Redundancy level
    pub redundancy_level: u32,
    /// Brown-out detection threshold (V)
    pub brownout_threshold: f64,
    /// Power-good signal availability
    pub power_good_available: bool,
}

/// Types of power sources
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum PowerSourceType {
    /// Primary battery
    PrimaryBattery,
    /// Secondary (rechargeable) battery
    SecondaryBattery,
    /// External power supply
    ExternalSupply,
    /// Switched-mode power supply
    SwitchedMode,
    /// Linear regulator
    LinearRegulator,
    /// Low-dropout regulator
    LowDropoutRegulator,
    /// Capacitor backup
    CapacitorBackup,
}

/// Power states for domains
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum PowerState {
    /// Fully powered and operational
    Active,
    /// Reduced power with clock gating
    ClockGated,
    /// Power gated but retention
    Retention,
    /// Completely powered off
    PoweredOff,
    /// Standby mode
    Standby,
    /// Sleep mode with wake capability
    Sleep,
    /// Deep sleep mode
    DeepSleep,
    /// Emergency shutdown
    EmergencyShutdown,
}

/// Isolation requirements for power domains
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IsolationRequirements {
    /// Electrical isolation required
    pub electrical_isolation: bool,
    /// Galvanic isolation required
    pub galvanic_isolation: bool,
    /// Optical isolation required
    pub optical_isolation: bool,
    /// Minimum isolation voltage (V)
    pub min_isolation_voltage: f64,
    /// Isolation test requirements
    pub isolation_tests: Vec<IsolationTest>,
    /// Cross-domain signal constraints
    pub signal_constraints: SignalConstraints,
}

/// Isolation test specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IsolationTest {
    /// Test type
    pub test_type: IsolationTestType,
    /// Test voltage (V)
    pub test_voltage: f64,
    /// Test duration (seconds)
    pub test_duration: f64,
    /// Pass criteria
    pub pass_criteria: String,
    /// Test frequency
    pub test_frequency: TestFrequency,
}

/// Types of isolation tests
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum IsolationTestType {
    /// Dielectric withstand test
    DielectricWithstand,
    /// Insulation resistance test
    InsulationResistance,
    /// High-potential test
    HighPotential,
    /// Partial discharge test
    PartialDischarge,
    /// Surge test
    Surge,
}

/// Test frequency requirements
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TestFrequency {
    /// Test during manufacturing only
    Manufacturing,
    /// Test at startup
    Startup,
    /// Periodic testing during operation
    Periodic(u64), // Test interval in seconds
    /// Continuous monitoring
    Continuous,
    /// On-demand testing
    OnDemand,
}

/// Signal constraints for cross-domain communication
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SignalConstraints {
    /// Maximum voltage levels
    pub max_voltage_levels: HashMap<String, f64>,
    /// Signal rise/fall time limits
    pub timing_constraints: TimingConstraints,
    /// EMI/EMC requirements
    pub emi_requirements: EmiRequirements,
    /// Safety integrity requirements
    pub safety_integrity: SafetyIntegrityConstraints,
}

/// Timing constraints for cross-domain signals
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimingConstraints {
    /// Maximum rise time (ns)
    pub max_rise_time: f64,
    /// Maximum fall time (ns)
    pub max_fall_time: f64,
    /// Setup time (ns)
    pub setup_time: f64,
    /// Hold time (ns)
    pub hold_time: f64,
    /// Maximum skew (ns)
    pub max_skew: f64,
}

/// EMI/EMC requirements
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EmiRequirements {
    /// Maximum emission levels (dBµV/m)
    pub max_emission_levels: HashMap<String, f64>, // Frequency -> Level
    /// Immunity requirements (V/m)
    pub immunity_levels: HashMap<String, f64>, // Frequency -> Level
    /// Filtering requirements
    pub filtering_required: bool,
    /// Shielding requirements
    pub shielding_required: bool,
}

/// Safety integrity constraints
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SafetyIntegrityConstraints {
    /// Required ASIL level for communication
    pub required_asil: AsilLevel,
    /// Error detection requirements
    pub error_detection_required: bool,
    /// Error correction requirements
    pub error_correction_required: bool,
    /// Redundancy requirements
    pub redundancy_required: bool,
    /// Diagnostic coverage required (percentage)
    pub diagnostic_coverage_required: f64,
}

/// Domain status tracking
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum DomainStatus {
    /// Domain is operational
    Operational,
    /// Domain is in startup sequence
    Starting,
    /// Domain is shutting down
    Shutting,
    /// Domain is in fault state
    Faulted,
    /// Domain is isolated due to fault
    Isolated,
    /// Domain is under maintenance
    Maintenance,
    /// Domain is disabled
    Disabled,
}

/// Hierarchical structure of power domains
#[derive(Debug, Clone)]
pub struct DomainHierarchy {
    /// Domain dependency graph
    pub dependency_graph: Graph<String, DependencyType>,
    /// Node indices for domains
    pub domain_nodes: HashMap<String, NodeIndex>,
    /// Parent-child relationships
    pub parent_child: HashMap<String, Vec<String>>,
    /// Root domains (no dependencies)
    pub root_domains: Vec<String>,
}

/// Types of dependencies between domains
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum DependencyType {
    /// Power dependency (child needs parent powered)
    PowerDependency,
    /// Clock dependency
    ClockDependency,
    /// Communication dependency
    CommunicationDependency,
    /// Safety dependency
    SafetyDependency,
    /// Temporal dependency (ordering)
    TemporalDependency,
}

/// Isolation barrier between domains
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IsolationBarrier {
    /// Barrier identifier
    pub id: String,
    /// Source domain
    pub source_domain: String,
    /// Target domain
    pub target_domain: String,
    /// Barrier type
    pub barrier_type: BarrierType,
    /// Isolation specifications
    pub specifications: IsolationSpecifications,
    /// Barrier status
    pub status: BarrierStatus,
    /// Test results
    pub test_results: Vec<IsolationTestResult>,
}

/// Types of isolation barriers
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum BarrierType {
    /// Electrical isolation (transformers, optocouplers)
    Electrical,
    /// Galvanic isolation
    Galvanic,
    /// Optical isolation
    Optical,
    /// Magnetic isolation
    Magnetic,
    /// Capacitive isolation
    Capacitive,
    /// Digital isolation
    Digital,
}

/// Isolation barrier specifications
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IsolationSpecifications {
    /// Isolation voltage rating (V)
    pub isolation_voltage: f64,
    /// Working voltage (V)
    pub working_voltage: f64,
    /// Creepage distance (mm)
    pub creepage_distance: f64,
    /// Clearance distance (mm)
    pub clearance_distance: f64,
    /// Common mode transient immunity (kV/µs)
    pub cmti: f64,
    /// Surge immunity (kV)
    pub surge_immunity: f64,
    /// Temperature rating (°C)
    pub temperature_rating: (f64, f64), // (min, max)
}

/// Barrier operational status
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum BarrierStatus {
    /// Barrier is functional
    Functional,
    /// Barrier is degraded but operational
    Degraded,
    /// Barrier has failed
    Failed,
    /// Barrier is under test
    UnderTest,
    /// Barrier is bypassed (unsafe)
    Bypassed,
}

/// Results from isolation testing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IsolationTestResult {
    /// Test timestamp
    pub timestamp: DateTime<Utc>,
    /// Test type performed
    pub test_type: IsolationTestType,
    /// Test passed
    pub passed: bool,
    /// Measured values
    pub measured_values: HashMap<String, f64>,
    /// Test notes
    pub notes: String,
}

/// Cross-domain communication channel
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CrossDomainChannel {
    /// Channel identifier
    pub id: String,
    /// Source domain
    pub source_domain: String,
    /// Target domain
    pub target_domain: String,
    /// Communication protocol
    pub protocol: CommunicationProtocol,
    /// Data integrity mechanisms
    pub integrity_mechanisms: Vec<IntegrityMechanism>,
    /// Channel characteristics
    pub characteristics: ChannelCharacteristics,
    /// Safety requirements
    pub safety_requirements: ChannelSafetyRequirements,
    /// Channel status
    pub status: ChannelStatus,
}

/// Communication protocols for cross-domain channels
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum CommunicationProtocol {
    /// Simple digital I/O
    DigitalIO,
    /// SPI with isolation
    IsolatedSPI,
    /// I2C with isolation
    IsolatedI2C,
    /// CAN with isolation
    IsolatedCAN,
    /// Ethernet with isolation
    IsolatedEthernet,
    /// Custom isolated protocol
    CustomIsolated,
    /// Optical communication
    Optical,
    /// Wireless communication
    Wireless,
}

/// Data integrity mechanisms
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum IntegrityMechanism {
    /// Cyclic Redundancy Check
    CRC,
    /// Error Correcting Code
    ECC,
    /// Parity checking
    Parity,
    /// Checksum verification
    Checksum,
    /// Message authentication
    MessageAuth,
    /// Sequence numbering
    SequenceNumber,
    /// Timeout detection
    Timeout,
    /// Heartbeat monitoring
    Heartbeat,
}

/// Channel characteristics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChannelCharacteristics {
    /// Data rate (bps)
    pub data_rate: u64,
    /// Latency (ns)
    pub latency: f64,
    /// Jitter (ns)
    pub jitter: f64,
    /// Bit error rate
    pub bit_error_rate: f64,
    /// Bandwidth (Hz)
    pub bandwidth: f64,
    /// Power consumption (mW)
    pub power_consumption: f64,
}

/// Safety requirements for communication channels
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChannelSafetyRequirements {
    /// Required ASIL level
    pub required_asil: AsilLevel,
    /// Error detection coverage (percentage)
    pub error_detection_coverage: f64,
    /// Maximum error propagation time (ns)
    pub max_error_propagation_time: f64,
    /// Fail-safe behavior required
    pub fail_safe_required: bool,
    /// Diagnostic requirements
    pub diagnostic_requirements: Vec<String>,
}

/// Channel operational status
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ChannelStatus {
    /// Channel is operational
    Operational,
    /// Channel has errors but functional
    ErrorsDetected,
    /// Channel is degraded
    Degraded,
    /// Channel has failed
    Failed,
    /// Channel is disabled
    Disabled,
    /// Channel is under test
    UnderTest,
}

/// Power management policy
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PowerPolicy {
    /// Policy identifier
    pub id: String,
    /// Policy name
    pub name: String,
    /// Target domains
    pub target_domains: Vec<String>,
    /// Policy type
    pub policy_type: PolicyType,
    /// Trigger conditions
    pub triggers: Vec<PolicyTrigger>,
    /// Actions to take
    pub actions: Vec<PolicyAction>,
    /// Safety constraints
    pub safety_constraints: PolicySafetyConstraints,
    /// Policy priority
    pub priority: u32,
    /// Policy status
    pub status: PolicyStatus,
}

/// Types of power management policies
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum PolicyType {
    /// Power conservation policy
    PowerConservation,
    /// Safety shutdown policy
    SafetyShutdown,
    /// Emergency response policy
    EmergencyResponse,
    /// Performance optimization policy
    PerformanceOptimization,
    /// Thermal management policy
    ThermalManagement,
    /// Battery management policy
    BatteryManagement,
}

/// Policy trigger conditions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PolicyTrigger {
    /// Trigger type
    pub trigger_type: TriggerType,
    /// Trigger condition
    pub condition: String,
    /// Threshold value
    pub threshold: Option<f64>,
    /// Trigger priority
    pub priority: u32,
}

/// Types of policy triggers
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TriggerType {
    /// Battery level trigger
    BatteryLevel,
    /// Temperature trigger
    Temperature,
    /// Power consumption trigger
    PowerConsumption,
    /// Safety event trigger
    SafetyEvent,
    /// Time-based trigger
    TimeBased,
    /// External signal trigger
    ExternalSignal,
    /// Fault detection trigger
    FaultDetection,
}

/// Policy actions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PolicyAction {
    /// Action type
    pub action_type: ActionType,
    /// Target domain
    pub target_domain: String,
    /// New power state
    pub new_power_state: Option<PowerState>,
    /// Action parameters
    pub parameters: HashMap<String, String>,
    /// Action timeout (seconds)
    pub timeout: Option<f64>,
}

/// Types of policy actions
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ActionType {
    /// Change power state
    ChangePowerState,
    /// Isolate domain
    IsolateDomain,
    /// Reset domain
    ResetDomain,
    /// Enable/disable communication
    ConfigureCommunication,
    /// Trigger diagnostic
    TriggerDiagnostic,
    /// Send notification
    SendNotification,
    /// Emergency shutdown
    EmergencyShutdown,
}

/// Safety constraints for policies
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PolicySafetyConstraints {
    /// Maximum execution time (seconds)
    pub max_execution_time: f64,
    /// Safety state requirements
    pub safety_state_requirements: Vec<String>,
    /// Prohibited combinations
    pub prohibited_combinations: Vec<String>,
    /// Required confirmations
    pub required_confirmations: Vec<String>,
}

/// Policy execution status
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum PolicyStatus {
    /// Policy is active
    Active,
    /// Policy is inactive
    Inactive,
    /// Policy is executing
    Executing,
    /// Policy execution completed
    Completed,
    /// Policy execution failed
    Failed,
    /// Policy is suspended
    Suspended,
}

impl PowerDomainManager {
    /// Create a new power domain manager
    pub fn new() -> Self {
        Self {
            domains: HashMap::new(),
            hierarchy: DomainHierarchy {
                dependency_graph: Graph::new(),
                domain_nodes: HashMap::new(),
                parent_child: HashMap::new(),
                root_domains: vec![],
            },
            isolation_barriers: vec![],
            communication_channels: vec![],
            power_policies: vec![],
        }
    }

    /// Add a power domain
    pub fn add_domain(&mut self, domain: PowerDomain) -> Result<(), PowerDomainError> {
        let domain_id = domain.id.clone();

        // Add node to dependency graph
        let node_index = self.hierarchy.dependency_graph.add_node(domain_id.clone());
        self.hierarchy.domain_nodes.insert(domain_id.clone(), node_index);

        // Check if this is a root domain (no dependencies)
        // This would be determined by the domain type and configuration
        if matches!(domain.domain_type, PowerDomainType::AlwaysOn) {
            self.hierarchy.root_domains.push(domain_id.clone());
        }

        self.domains.insert(domain_id, domain);
        Ok(())
    }

    /// Add dependency between domains
    pub fn add_dependency(
        &mut self,
        source: &str,
        target: &str,
        dependency_type: DependencyType,
    ) -> Result<(), PowerDomainError> {
        let source_node = self.hierarchy.domain_nodes.get(source)
            .ok_or_else(|| PowerDomainError::DomainNotFound(source.to_string()))?;
        let target_node = self.hierarchy.domain_nodes.get(target)
            .ok_or_else(|| PowerDomainError::DomainNotFound(target.to_string()))?;

        self.hierarchy.dependency_graph.add_edge(*source_node, *target_node, dependency_type);

        // Update parent-child relationships
        self.hierarchy.parent_child
            .entry(source.to_string())
            .or_insert_with(Vec::new)
            .push(target.to_string());

        Ok(())
    }

    /// Add isolation barrier
    pub fn add_isolation_barrier(&mut self, barrier: IsolationBarrier) -> Result<(), PowerDomainError> {
        // Validate that both domains exist
        if !self.domains.contains_key(&barrier.source_domain) {
            return Err(PowerDomainError::DomainNotFound(barrier.source_domain.clone()));
        }
        if !self.domains.contains_key(&barrier.target_domain) {
            return Err(PowerDomainError::DomainNotFound(barrier.target_domain.clone()));
        }

        self.isolation_barriers.push(barrier);
        Ok(())
    }

    /// Add communication channel
    pub fn add_communication_channel(&mut self, channel: CrossDomainChannel) -> Result<(), PowerDomainError> {
        // Validate that both domains exist
        if !self.domains.contains_key(&channel.source_domain) {
            return Err(PowerDomainError::DomainNotFound(channel.source_domain.clone()));
        }
        if !self.domains.contains_key(&channel.target_domain) {
            return Err(PowerDomainError::DomainNotFound(channel.target_domain.clone()));
        }

        self.communication_channels.push(channel);
        Ok(())
    }

    /// Change domain power state
    pub fn change_power_state(
        &mut self,
        domain_id: &str,
        new_state: PowerState,
    ) -> Result<(), PowerDomainError> {
        // Check if domain exists and get supported states
        let domain_exists = self.domains.contains_key(domain_id);
        if !domain_exists {
            return Err(PowerDomainError::DomainNotFound(domain_id.to_string()));
        }

        let supported_states = self.domains.get(domain_id).unwrap().power_states.clone();
        if !supported_states.contains(&new_state) {
            return Err(PowerDomainError::UnsupportedPowerState(new_state));
        }

        // Check safety constraints
        self.validate_power_state_change(domain_id, &new_state)?;

        // Update the state
        let domain = self.domains.get_mut(domain_id).unwrap();
        domain.current_state = new_state;
        Ok(())
    }

    /// Validate power state change for safety
    fn validate_power_state_change(
        &self,
        domain_id: &str,
        new_state: &PowerState,
    ) -> Result<(), PowerDomainError> {
        let domain = self.domains.get(domain_id).unwrap();

        // Safety-critical domains cannot be powered off
        if domain.safety_level != AsilLevel::QM && matches!(new_state, PowerState::PoweredOff) {
            return Err(PowerDomainError::SafetyViolation(
                "Safety-critical domain cannot be powered off".to_string()
            ));
        }

        // Check dependencies - dependent domains cannot be powered when parent is off
        if let Some(children) = self.hierarchy.parent_child.get(domain_id) {
            for child_id in children {
                if let Some(child_domain) = self.domains.get(child_id) {
                    if matches!(new_state, PowerState::PoweredOff | PowerState::EmergencyShutdown) &&
                       !matches!(child_domain.current_state, PowerState::PoweredOff) {
                        return Err(PowerDomainError::DependencyViolation(
                            format!("Cannot power off domain {} while {} is active", domain_id, child_id)
                        ));
                    }
                }
            }
        }

        Ok(())
    }

    /// Perform isolation analysis
    pub fn analyze_isolation(&self) -> IsolationAnalysisResult {
        let mut violations = vec![];
        let mut warnings = vec![];
        let mut compliant_barriers = 0;

        for barrier in &self.isolation_barriers {
            let source_domain = self.domains.get(&barrier.source_domain).unwrap();
            let target_domain = self.domains.get(&barrier.target_domain).unwrap();

            // Check if isolation is adequate for safety levels
            if source_domain.safety_level != target_domain.safety_level {
                let min_isolation_voltage = self.calculate_min_isolation_voltage(
                    source_domain.safety_level,
                    target_domain.safety_level,
                );

                if barrier.specifications.isolation_voltage < min_isolation_voltage {
                    violations.push(IsolationViolation {
                        barrier_id: barrier.id.clone(),
                        violation_type: ViolationType::InsufficientIsolation,
                        description: format!(
                            "Isolation voltage {} V is below required {} V",
                            barrier.specifications.isolation_voltage,
                            min_isolation_voltage
                        ),
                        severity: ViolationSeverity::High,
                    });
                } else {
                    compliant_barriers += 1;
                }
            } else {
                // Same safety level, always compliant
                compliant_barriers += 1;
            }

            // Check barrier status
            if barrier.status != BarrierStatus::Functional {
                warnings.push(IsolationWarning {
                    barrier_id: barrier.id.clone(),
                    warning_type: WarningType::BarrierDegraded,
                    description: format!("Barrier status: {:?}", barrier.status),
                });
            }
        }

        let compliance_percentage = if !self.isolation_barriers.is_empty() {
            (compliant_barriers as f64 / self.isolation_barriers.len() as f64) * 100.0
        } else {
            100.0
        };

        IsolationAnalysisResult {
            total_barriers: self.isolation_barriers.len(),
            compliant_barriers,
            compliance_percentage,
            violations,
            warnings,
            analysis_timestamp: Utc::now(),
        }
    }

    /// Calculate minimum isolation voltage based on safety levels
    fn calculate_min_isolation_voltage(&self, level1: AsilLevel, level2: AsilLevel) -> f64 {
        let max_level = if level1 > level2 { level1 } else { level2 };

        match max_level {
            AsilLevel::QM => 1000.0,  // 1 kV
            AsilLevel::A => 2500.0,   // 2.5 kV
            AsilLevel::B => 4000.0,   // 4 kV
            AsilLevel::C => 6000.0,   // 6 kV
            AsilLevel::D => 8000.0,   // 8 kV
        }
    }

    /// Get power consumption report
    pub fn get_power_consumption_report(&self) -> PowerConsumptionReport {
        let mut total_power = 0.0;
        let mut power_by_domain = HashMap::new();
        let mut power_by_state = HashMap::new();

        for domain in self.domains.values() {
            // Simplified power calculation based on state
            let domain_power = match domain.current_state {
                PowerState::Active => domain.power_supply.max_current * domain.power_supply.nominal_voltage,
                PowerState::ClockGated => domain.power_supply.max_current * domain.power_supply.nominal_voltage * 0.7,
                PowerState::Retention => domain.power_supply.max_current * domain.power_supply.nominal_voltage * 0.1,
                PowerState::PoweredOff => 0.0,
                PowerState::Standby => domain.power_supply.max_current * domain.power_supply.nominal_voltage * 0.5,
                PowerState::Sleep => domain.power_supply.max_current * domain.power_supply.nominal_voltage * 0.2,
                PowerState::DeepSleep => domain.power_supply.max_current * domain.power_supply.nominal_voltage * 0.05,
                PowerState::EmergencyShutdown => 0.0,
            };

            total_power += domain_power;
            power_by_domain.insert(domain.id.clone(), domain_power);
            *power_by_state.entry(domain.current_state.clone()).or_insert(0.0) += domain_power;
        }

        PowerConsumptionReport {
            total_power_consumption: total_power,
            power_by_domain,
            power_by_state,
            efficiency_metrics: EfficiencyMetrics {
                power_efficiency: 85.0, // Example value
                thermal_efficiency: 90.0,
                energy_density: 150.0, // Wh/kg
            },
            recommendations: vec![
                "Consider power gating unused domains".to_string(),
                "Optimize communication channel power consumption".to_string(),
            ],
        }
    }
}

/// Results from isolation analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IsolationAnalysisResult {
    /// Total number of barriers analyzed
    pub total_barriers: usize,
    /// Number of compliant barriers
    pub compliant_barriers: usize,
    /// Compliance percentage
    pub compliance_percentage: f64,
    /// Isolation violations found
    pub violations: Vec<IsolationViolation>,
    /// Warnings issued
    pub warnings: Vec<IsolationWarning>,
    /// Analysis timestamp
    pub analysis_timestamp: DateTime<Utc>,
}

/// Isolation violation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IsolationViolation {
    /// Barrier ID
    pub barrier_id: String,
    /// Type of violation
    pub violation_type: ViolationType,
    /// Description
    pub description: String,
    /// Severity
    pub severity: ViolationSeverity,
}

/// Types of isolation violations
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ViolationType {
    /// Insufficient isolation voltage
    InsufficientIsolation,
    /// Missing isolation barrier
    MissingBarrier,
    /// Barrier failure
    BarrierFailure,
    /// Safety level mismatch
    SafetyLevelMismatch,
    /// Signal integrity violation
    SignalIntegrityViolation,
}

/// Violation severity levels
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum ViolationSeverity {
    /// Low severity
    Low,
    /// Medium severity
    Medium,
    /// High severity
    High,
    /// Critical severity
    Critical,
}

/// Isolation warning
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IsolationWarning {
    /// Barrier ID
    pub barrier_id: String,
    /// Type of warning
    pub warning_type: WarningType,
    /// Description
    pub description: String,
}

/// Types of isolation warnings
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum WarningType {
    /// Barrier is degraded
    BarrierDegraded,
    /// Test overdue
    TestOverdue,
    /// Marginal isolation
    MarginalIsolation,
    /// Communication degraded
    CommunicationDegraded,
}

/// Power consumption report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PowerConsumptionReport {
    /// Total system power consumption (W)
    pub total_power_consumption: f64,
    /// Power consumption by domain (W)
    pub power_by_domain: HashMap<String, f64>,
    /// Power consumption by state (W)
    pub power_by_state: HashMap<PowerState, f64>,
    /// Efficiency metrics
    pub efficiency_metrics: EfficiencyMetrics,
    /// Optimization recommendations
    pub recommendations: Vec<String>,
}

/// Efficiency metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EfficiencyMetrics {
    /// Power efficiency (percentage)
    pub power_efficiency: f64,
    /// Thermal efficiency (percentage)
    pub thermal_efficiency: f64,
    /// Energy density (Wh/kg)
    pub energy_density: f64,
}

/// Errors that can occur in power domain management
#[derive(Debug, thiserror::Error)]
pub enum PowerDomainError {
    #[error("Domain not found: {0}")]
    DomainNotFound(String),
    #[error("Unsupported power state: {0:?}")]
    UnsupportedPowerState(PowerState),
    #[error("Safety violation: {0}")]
    SafetyViolation(String),
    #[error("Dependency violation: {0}")]
    DependencyViolation(String),
    #[error("Isolation failure: {0}")]
    IsolationFailure(String),
    #[error("Communication error: {0}")]
    CommunicationError(String),
}

impl Default for PowerDomainManager {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for SignalConstraints {
    fn default() -> Self {
        Self {
            max_voltage_levels: HashMap::new(),
            timing_constraints: TimingConstraints {
                max_rise_time: 10.0,
                max_fall_time: 10.0,
                setup_time: 2.0,
                hold_time: 1.0,
                max_skew: 0.5,
            },
            emi_requirements: EmiRequirements {
                max_emission_levels: HashMap::new(),
                immunity_levels: HashMap::new(),
                filtering_required: false,
                shielding_required: false,
            },
            safety_integrity: SafetyIntegrityConstraints {
                required_asil: AsilLevel::QM,
                error_detection_required: false,
                error_correction_required: false,
                redundancy_required: false,
                diagnostic_coverage_required: 0.0,
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_power_domain_creation() {
        let mut manager = PowerDomainManager::new();

        let domain = PowerDomain {
            id: "domain1".to_string(),
            name: "Safety Domain".to_string(),
            domain_type: PowerDomainType::SafetyCritical,
            safety_level: AsilLevel::D,
            power_supply: PowerSupply {
                nominal_voltage: 3.3,
                voltage_tolerance: 5.0,
                max_current: 1.0,
                source_type: PowerSourceType::LinearRegulator,
                redundancy_level: 1,
                brownout_threshold: 2.8,
                power_good_available: true,
            },
            components: vec!["cpu".to_string(), "memory".to_string()],
            clock_domains: vec!["clk_main".to_string()],
            power_states: vec![PowerState::Active, PowerState::Standby],
            current_state: PowerState::Active,
            isolation_requirements: IsolationRequirements {
                electrical_isolation: true,
                galvanic_isolation: false,
                optical_isolation: false,
                min_isolation_voltage: 2500.0,
                isolation_tests: vec![],
                signal_constraints: SignalConstraints {
                    max_voltage_levels: HashMap::new(),
                    timing_constraints: TimingConstraints {
                        max_rise_time: 10.0,
                        max_fall_time: 10.0,
                        setup_time: 2.0,
                        hold_time: 1.0,
                        max_skew: 0.5,
                    },
                    emi_requirements: EmiRequirements {
                        max_emission_levels: HashMap::new(),
                        immunity_levels: HashMap::new(),
                        filtering_required: false,
                        shielding_required: false,
                    },
                    safety_integrity: SafetyIntegrityConstraints {
                        required_asil: AsilLevel::D,
                        error_detection_required: true,
                        error_correction_required: false,
                        redundancy_required: false,
                        diagnostic_coverage_required: 95.0,
                    },
                },
            },
            status: DomainStatus::Operational,
            created_at: Utc::now(),
        };

        let result = manager.add_domain(domain);
        assert!(result.is_ok());
        assert!(manager.domains.contains_key("domain1"));
    }

    #[test]
    fn test_power_state_change() {
        let mut manager = PowerDomainManager::new();

        let domain = PowerDomain {
            id: "test_domain".to_string(),
            name: "Test Domain".to_string(),
            domain_type: PowerDomainType::Performance,
            safety_level: AsilLevel::QM,
            power_supply: PowerSupply {
                nominal_voltage: 1.8,
                voltage_tolerance: 3.0,
                max_current: 0.5,
                source_type: PowerSourceType::SwitchedMode,
                redundancy_level: 0,
                brownout_threshold: 1.5,
                power_good_available: true,
            },
            components: vec![],
            clock_domains: vec![],
            power_states: vec![PowerState::Active, PowerState::PoweredOff],
            current_state: PowerState::Active,
            isolation_requirements: IsolationRequirements {
                electrical_isolation: false,
                galvanic_isolation: false,
                optical_isolation: false,
                min_isolation_voltage: 0.0,
                isolation_tests: vec![],
                signal_constraints: SignalConstraints {
                    max_voltage_levels: HashMap::new(),
                    timing_constraints: TimingConstraints {
                        max_rise_time: 5.0,
                        max_fall_time: 5.0,
                        setup_time: 1.0,
                        hold_time: 0.5,
                        max_skew: 0.2,
                    },
                    emi_requirements: EmiRequirements {
                        max_emission_levels: HashMap::new(),
                        immunity_levels: HashMap::new(),
                        filtering_required: false,
                        shielding_required: false,
                    },
                    safety_integrity: SafetyIntegrityConstraints {
                        required_asil: AsilLevel::QM,
                        error_detection_required: false,
                        error_correction_required: false,
                        redundancy_required: false,
                        diagnostic_coverage_required: 0.0,
                    },
                },
            },
            status: DomainStatus::Operational,
            created_at: Utc::now(),
        };

        manager.add_domain(domain).unwrap();

        let result = manager.change_power_state("test_domain", PowerState::PoweredOff);
        assert!(result.is_ok());

        let updated_domain = manager.domains.get("test_domain").unwrap();
        assert_eq!(updated_domain.current_state, PowerState::PoweredOff);
    }

    #[test]
    fn test_safety_critical_domain_protection() {
        let mut manager = PowerDomainManager::new();

        let domain = PowerDomain {
            id: "safety_domain".to_string(),
            name: "Safety Critical Domain".to_string(),
            domain_type: PowerDomainType::SafetyCritical,
            safety_level: AsilLevel::D,
            power_supply: PowerSupply {
                nominal_voltage: 5.0,
                voltage_tolerance: 2.0,
                max_current: 2.0,
                source_type: PowerSourceType::LinearRegulator,
                redundancy_level: 2,
                brownout_threshold: 4.5,
                power_good_available: true,
            },
            components: vec![],
            clock_domains: vec![],
            power_states: vec![PowerState::Active, PowerState::Standby, PowerState::PoweredOff],
            current_state: PowerState::Active,
            isolation_requirements: IsolationRequirements {
                electrical_isolation: true,
                galvanic_isolation: true,
                optical_isolation: false,
                min_isolation_voltage: 8000.0,
                isolation_tests: vec![],
                signal_constraints: SignalConstraints {
                    max_voltage_levels: HashMap::new(),
                    timing_constraints: TimingConstraints {
                        max_rise_time: 2.0,
                        max_fall_time: 2.0,
                        setup_time: 0.5,
                        hold_time: 0.3,
                        max_skew: 0.1,
                    },
                    emi_requirements: EmiRequirements {
                        max_emission_levels: HashMap::new(),
                        immunity_levels: HashMap::new(),
                        filtering_required: true,
                        shielding_required: true,
                    },
                    safety_integrity: SafetyIntegrityConstraints {
                        required_asil: AsilLevel::D,
                        error_detection_required: true,
                        error_correction_required: true,
                        redundancy_required: true,
                        diagnostic_coverage_required: 99.0,
                    },
                },
            },
            status: DomainStatus::Operational,
            created_at: Utc::now(),
        };

        manager.add_domain(domain).unwrap();

        // Attempt to power off safety-critical domain should fail
        let result = manager.change_power_state("safety_domain", PowerState::PoweredOff);
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), PowerDomainError::SafetyViolation(_)));
    }

    #[test]
    fn test_isolation_analysis() {
        let mut manager = PowerDomainManager::new();

        // Add domains
        let domain1 = PowerDomain {
            id: "domain1".to_string(),
            name: "Domain 1".to_string(),
            domain_type: PowerDomainType::SafetyCritical,
            safety_level: AsilLevel::C,
            power_supply: PowerSupply {
                nominal_voltage: 3.3,
                voltage_tolerance: 5.0,
                max_current: 1.0,
                source_type: PowerSourceType::LinearRegulator,
                redundancy_level: 1,
                brownout_threshold: 2.8,
                power_good_available: true,
            },
            components: vec![],
            clock_domains: vec![],
            power_states: vec![PowerState::Active],
            current_state: PowerState::Active,
            isolation_requirements: IsolationRequirements {
                electrical_isolation: true,
                galvanic_isolation: false,
                optical_isolation: false,
                min_isolation_voltage: 6000.0,
                isolation_tests: vec![],
                signal_constraints: SignalConstraints {
                    max_voltage_levels: HashMap::new(),
                    timing_constraints: TimingConstraints {
                        max_rise_time: 10.0,
                        max_fall_time: 10.0,
                        setup_time: 2.0,
                        hold_time: 1.0,
                        max_skew: 0.5,
                    },
                    emi_requirements: EmiRequirements {
                        max_emission_levels: HashMap::new(),
                        immunity_levels: HashMap::new(),
                        filtering_required: false,
                        shielding_required: false,
                    },
                    safety_integrity: SafetyIntegrityConstraints {
                        required_asil: AsilLevel::C,
                        error_detection_required: true,
                        error_correction_required: false,
                        redundancy_required: false,
                        diagnostic_coverage_required: 90.0,
                    },
                },
            },
            status: DomainStatus::Operational,
            created_at: Utc::now(),
        };

        let domain2 = PowerDomain {
            id: "domain2".to_string(),
            name: "Domain 2".to_string(),
            domain_type: PowerDomainType::Performance,
            safety_level: AsilLevel::QM,
            power_supply: PowerSupply {
                nominal_voltage: 1.8,
                voltage_tolerance: 3.0,
                max_current: 0.5,
                source_type: PowerSourceType::SwitchedMode,
                redundancy_level: 0,
                brownout_threshold: 1.5,
                power_good_available: true,
            },
            components: vec![],
            clock_domains: vec![],
            power_states: vec![PowerState::Active, PowerState::PoweredOff],
            current_state: PowerState::Active,
            isolation_requirements: IsolationRequirements {
                electrical_isolation: false,
                galvanic_isolation: false,
                optical_isolation: false,
                min_isolation_voltage: 0.0,
                isolation_tests: vec![],
                signal_constraints: SignalConstraints {
                    max_voltage_levels: HashMap::new(),
                    timing_constraints: TimingConstraints {
                        max_rise_time: 5.0,
                        max_fall_time: 5.0,
                        setup_time: 1.0,
                        hold_time: 0.5,
                        max_skew: 0.2,
                    },
                    emi_requirements: EmiRequirements {
                        max_emission_levels: HashMap::new(),
                        immunity_levels: HashMap::new(),
                        filtering_required: false,
                        shielding_required: false,
                    },
                    safety_integrity: SafetyIntegrityConstraints {
                        required_asil: AsilLevel::QM,
                        error_detection_required: false,
                        error_correction_required: false,
                        redundancy_required: false,
                        diagnostic_coverage_required: 0.0,
                    },
                },
            },
            status: DomainStatus::Operational,
            created_at: Utc::now(),
        };

        manager.add_domain(domain1).unwrap();
        manager.add_domain(domain2).unwrap();

        // Add isolation barrier with insufficient isolation
        let barrier = IsolationBarrier {
            id: "barrier1".to_string(),
            source_domain: "domain1".to_string(),
            target_domain: "domain2".to_string(),
            barrier_type: BarrierType::Electrical,
            specifications: IsolationSpecifications {
                isolation_voltage: 2500.0, // Insufficient for ASIL C
                working_voltage: 24.0,
                creepage_distance: 4.0,
                clearance_distance: 3.0,
                cmti: 50.0,
                surge_immunity: 4.0,
                temperature_rating: (-40.0, 125.0),
            },
            status: BarrierStatus::Functional,
            test_results: vec![],
        };

        manager.add_isolation_barrier(barrier).unwrap();

        let analysis = manager.analyze_isolation();
        assert_eq!(analysis.total_barriers, 1);
        assert_eq!(analysis.compliant_barriers, 0);
        assert!(!analysis.violations.is_empty());
        assert_eq!(analysis.violations[0].violation_type, ViolationType::InsufficientIsolation);
    }

    #[test]
    fn test_power_consumption_report() {
        let mut manager = PowerDomainManager::new();

        let domain = PowerDomain {
            id: "test_domain".to_string(),
            name: "Test Domain".to_string(),
            domain_type: PowerDomainType::Performance,
            safety_level: AsilLevel::QM,
            power_supply: PowerSupply {
                nominal_voltage: 3.3,
                voltage_tolerance: 5.0,
                max_current: 2.0,
                source_type: PowerSourceType::SwitchedMode,
                redundancy_level: 0,
                brownout_threshold: 2.8,
                power_good_available: true,
            },
            components: vec![],
            clock_domains: vec![],
            power_states: vec![PowerState::Active, PowerState::PoweredOff],
            current_state: PowerState::Active,
            isolation_requirements: IsolationRequirements {
                electrical_isolation: false,
                galvanic_isolation: false,
                optical_isolation: false,
                min_isolation_voltage: 0.0,
                isolation_tests: vec![],
                signal_constraints: SignalConstraints {
                    max_voltage_levels: HashMap::new(),
                    timing_constraints: TimingConstraints {
                        max_rise_time: 10.0,
                        max_fall_time: 10.0,
                        setup_time: 2.0,
                        hold_time: 1.0,
                        max_skew: 0.5,
                    },
                    emi_requirements: EmiRequirements {
                        max_emission_levels: HashMap::new(),
                        immunity_levels: HashMap::new(),
                        filtering_required: false,
                        shielding_required: false,
                    },
                    safety_integrity: SafetyIntegrityConstraints {
                        required_asil: AsilLevel::QM,
                        error_detection_required: false,
                        error_correction_required: false,
                        redundancy_required: false,
                        diagnostic_coverage_required: 0.0,
                    },
                },
            },
            status: DomainStatus::Operational,
            created_at: Utc::now(),
        };

        manager.add_domain(domain).unwrap();

        let report = manager.get_power_consumption_report();
        assert_eq!(report.total_power_consumption, 6.6); // 3.3V * 2A
        assert!(report.power_by_domain.contains_key("test_domain"));
        assert!(report.power_by_state.contains_key(&PowerState::Active));
    }
}