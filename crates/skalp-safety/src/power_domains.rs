//! Power domain support for safety isolation in ISO 26262 designs
//!
//! Provides power domain management and isolation analysis for safety-critical systems.
//! Supports multiple power domains with controlled cross-domain communication.

use crate::asil::AsilLevel;
use crate::common_cause::{CcfCause, CcfGroup};
use chrono::{DateTime, Utc};
use indexmap::IndexMap;
use petgraph::graph::NodeIndex;
use petgraph::Graph;
use serde::{Deserialize, Serialize};

/// Power domain management system
#[derive(Debug, Clone)]
pub struct PowerDomainManager {
    /// All power domains in the system
    pub domains: IndexMap<String, PowerDomain>,
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
    pub max_voltage_levels: IndexMap<String, f64>,
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
    pub max_emission_levels: IndexMap<String, f64>, // Frequency -> Level
    /// Immunity requirements (V/m)
    pub immunity_levels: IndexMap<String, f64>, // Frequency -> Level
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
    pub domain_nodes: IndexMap<String, NodeIndex>,
    /// Parent-child relationships
    pub parent_child: IndexMap<String, Vec<String>>,
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
    pub measured_values: IndexMap<String, f64>,
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
    pub parameters: IndexMap<String, String>,
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
            domains: IndexMap::new(),
            hierarchy: DomainHierarchy {
                dependency_graph: Graph::new(),
                domain_nodes: IndexMap::new(),
                parent_child: IndexMap::new(),
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
        self.hierarchy
            .domain_nodes
            .insert(domain_id.clone(), node_index);

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
        let source_node = self
            .hierarchy
            .domain_nodes
            .get(source)
            .ok_or_else(|| PowerDomainError::DomainNotFound(source.to_string()))?;
        let target_node = self
            .hierarchy
            .domain_nodes
            .get(target)
            .ok_or_else(|| PowerDomainError::DomainNotFound(target.to_string()))?;

        self.hierarchy
            .dependency_graph
            .add_edge(*source_node, *target_node, dependency_type);

        // Update parent-child relationships
        self.hierarchy
            .parent_child
            .entry(source.to_string())
            .or_default()
            .push(target.to_string());

        Ok(())
    }

    /// Add isolation barrier
    pub fn add_isolation_barrier(
        &mut self,
        barrier: IsolationBarrier,
    ) -> Result<(), PowerDomainError> {
        // Validate that both domains exist
        if !self.domains.contains_key(&barrier.source_domain) {
            return Err(PowerDomainError::DomainNotFound(
                barrier.source_domain.clone(),
            ));
        }
        if !self.domains.contains_key(&barrier.target_domain) {
            return Err(PowerDomainError::DomainNotFound(
                barrier.target_domain.clone(),
            ));
        }

        self.isolation_barriers.push(barrier);
        Ok(())
    }

    /// Add communication channel
    pub fn add_communication_channel(
        &mut self,
        channel: CrossDomainChannel,
    ) -> Result<(), PowerDomainError> {
        // Validate that both domains exist
        if !self.domains.contains_key(&channel.source_domain) {
            return Err(PowerDomainError::DomainNotFound(
                channel.source_domain.clone(),
            ));
        }
        if !self.domains.contains_key(&channel.target_domain) {
            return Err(PowerDomainError::DomainNotFound(
                channel.target_domain.clone(),
            ));
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
                "Safety-critical domain cannot be powered off".to_string(),
            ));
        }

        // Check dependencies - dependent domains cannot be powered when parent is off
        if let Some(children) = self.hierarchy.parent_child.get(domain_id) {
            for child_id in children {
                if let Some(child_domain) = self.domains.get(child_id) {
                    if matches!(
                        new_state,
                        PowerState::PoweredOff | PowerState::EmergencyShutdown
                    ) && !matches!(child_domain.current_state, PowerState::PoweredOff)
                    {
                        return Err(PowerDomainError::DependencyViolation(format!(
                            "Cannot power off domain {} while {} is active",
                            domain_id, child_id
                        )));
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
                            barrier.specifications.isolation_voltage, min_isolation_voltage
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
            AsilLevel::QM => 1000.0, // 1 kV
            AsilLevel::A => 2500.0,  // 2.5 kV
            AsilLevel::B => 4000.0,  // 4 kV
            AsilLevel::C => 6000.0,  // 6 kV
            AsilLevel::D => 8000.0,  // 8 kV
        }
    }

    /// Get power consumption report
    pub fn get_power_consumption_report(&self) -> PowerConsumptionReport {
        let mut total_power = 0.0;
        let mut power_by_domain = IndexMap::new();
        let mut power_by_state = IndexMap::new();

        for domain in self.domains.values() {
            // Simplified power calculation based on state
            let domain_power = match domain.current_state {
                PowerState::Active => {
                    domain.power_supply.max_current * domain.power_supply.nominal_voltage
                }
                PowerState::ClockGated => {
                    domain.power_supply.max_current * domain.power_supply.nominal_voltage * 0.7
                }
                PowerState::Retention => {
                    domain.power_supply.max_current * domain.power_supply.nominal_voltage * 0.1
                }
                PowerState::PoweredOff => 0.0,
                PowerState::Standby => {
                    domain.power_supply.max_current * domain.power_supply.nominal_voltage * 0.5
                }
                PowerState::Sleep => {
                    domain.power_supply.max_current * domain.power_supply.nominal_voltage * 0.2
                }
                PowerState::DeepSleep => {
                    domain.power_supply.max_current * domain.power_supply.nominal_voltage * 0.05
                }
                PowerState::EmergencyShutdown => 0.0,
            };

            total_power += domain_power;
            power_by_domain.insert(domain.id.clone(), domain_power);
            *power_by_state
                .entry(domain.current_state.clone())
                .or_insert(0.0) += domain_power;
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
    pub power_by_domain: IndexMap<String, f64>,
    /// Power consumption by state (W)
    pub power_by_state: IndexMap<PowerState, f64>,
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
            max_voltage_levels: IndexMap::new(),
            timing_constraints: TimingConstraints {
                max_rise_time: 10.0,
                max_fall_time: 10.0,
                setup_time: 2.0,
                hold_time: 1.0,
                max_skew: 0.5,
            },
            emi_requirements: EmiRequirements {
                max_emission_levels: IndexMap::new(),
                immunity_levels: IndexMap::new(),
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

// ============================================================================
// CCF Integration for Power Domain Failures
// ============================================================================

use crate::common_cause::CcfAnalysisResults;

/// Power domain information extracted from gate netlist analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PowerDomainInfo {
    /// Domain name
    pub name: String,
    /// Nominal voltage (V)
    pub voltage: f64,
    /// Cell paths in this domain
    pub cells: Vec<String>,
    /// Total FIT for cells in this domain
    pub total_fit: f64,
    /// Level shifter cell paths (at domain boundaries)
    pub level_shifters: Vec<String>,
    /// Isolation cell paths (at domain boundaries)
    pub isolation_cells: Vec<String>,
}

/// Results from power domain analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PowerDomainAnalysis {
    /// Identified power domains
    pub domains: Vec<PowerDomainInfo>,
    /// CCF groups created from power domain analysis
    pub ccf_groups: Vec<CcfGroup>,
    /// Level shifter coverage (number of boundary crossings with level shifters)
    pub level_shifter_coverage: usize,
    /// Isolation cell coverage
    pub isolation_cell_coverage: usize,
    /// Total boundary crossings identified
    pub total_boundary_crossings: usize,
    /// Unprotected boundary crossings
    pub unprotected_crossings: Vec<UnprotectedCrossing>,
    /// Summary statistics
    pub summary: PowerDomainSummary,
}

/// Unprotected boundary crossing warning
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnprotectedCrossing {
    /// Source domain
    pub source_domain: String,
    /// Target domain
    pub target_domain: String,
    /// Signal path
    pub signal_path: String,
    /// Crossing type (voltage level mismatch, etc.)
    pub crossing_type: CrossingType,
    /// Risk level
    pub risk_level: CrossingRisk,
}

/// Type of domain boundary crossing
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum CrossingType {
    /// Voltage level mismatch
    VoltageMismatch,
    /// Safety level mismatch
    SafetyLevelMismatch,
    /// Missing isolation
    MissingIsolation,
    /// Clock domain crossing
    ClockDomainCrossing,
}

/// Risk level for unprotected crossings
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum CrossingRisk {
    Low,
    Medium,
    High,
    Critical,
}

/// Summary statistics for power domain analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PowerDomainSummary {
    /// Number of domains identified
    pub domain_count: usize,
    /// Total cells across all domains
    pub total_cells: usize,
    /// Total FIT across all domains
    pub total_fit: f64,
    /// Total CCF contribution from power domains
    pub total_ccf_fit: f64,
    /// Coverage percentage (protected crossings / total crossings)
    pub boundary_protection_percentage: f64,
}

/// Create CCF groups from power domain analysis
///
/// Each power domain becomes a CCF group because a power supply failure
/// affects all cells in that domain simultaneously (common cause failure).
pub fn power_domain_ccf_groups(domains: &[PowerDomainInfo]) -> Vec<CcfGroup> {
    domains
        .iter()
        .map(|domain| {
            // Calculate CCF FIT using beta factor model
            // Beta factor for power supply failure is typically high (~10-30%)
            // because all cells share the same power rail
            let beta_factor = 0.2; // 20% beta factor for power domain failures

            let mut group = CcfGroup::new(
                &format!("power_domain_{}", domain.name),
                CcfCause::SharedPower,
                beta_factor,
            );
            group.members = domain.cells.clone();
            group.description = Some(format!(
                "Power domain {} with {} cells at {:.2}V. \
                 Power supply failure affects all cells in domain.",
                domain.name,
                domain.cells.len(),
                domain.voltage
            ));
            group
        })
        .collect()
}

/// Analyze power domains from hierarchical cell paths
///
/// This function identifies power domains by analyzing cell paths for
/// common prefixes that indicate power domain boundaries.
/// In real designs, power domain annotations would come from design tools.
pub fn analyze_power_domains_from_paths(
    cell_paths: &[(String, f64)], // (path, fit)
    domain_patterns: &[PowerDomainPattern],
) -> PowerDomainAnalysis {
    let mut domains: IndexMap<String, PowerDomainInfo> = IndexMap::new();
    let mut unassigned_cells = Vec::new();

    // Classify cells by domain based on path patterns
    for (path, fit) in cell_paths {
        let mut assigned = false;
        for pattern in domain_patterns {
            if path.contains(&pattern.path_pattern) {
                let domain =
                    domains
                        .entry(pattern.name.clone())
                        .or_insert_with(|| PowerDomainInfo {
                            name: pattern.name.clone(),
                            voltage: pattern.voltage,
                            cells: Vec::new(),
                            total_fit: 0.0,
                            level_shifters: Vec::new(),
                            isolation_cells: Vec::new(),
                        });
                domain.cells.push(path.clone());
                domain.total_fit += fit;
                assigned = true;
                break;
            }
        }
        if !assigned {
            unassigned_cells.push((path.clone(), *fit));
        }
    }

    // Identify level shifters and isolation cells from naming conventions
    for domain in domains.values_mut() {
        for cell in &domain.cells {
            let lower = cell.to_lowercase();
            if lower.contains("level_shift") || lower.contains("lvl_shift") || lower.contains("ls_")
            {
                domain.level_shifters.push(cell.clone());
            }
            if lower.contains("isolation") || lower.contains("iso_") || lower.contains("isolate") {
                domain.isolation_cells.push(cell.clone());
            }
        }
    }

    // Create CCF groups from domains
    let domain_list: Vec<PowerDomainInfo> = domains.into_values().collect();
    let ccf_groups = power_domain_ccf_groups(&domain_list);

    // Calculate summary statistics
    let total_cells: usize = domain_list.iter().map(|d| d.cells.len()).sum();
    let total_fit: f64 = domain_list.iter().map(|d| d.total_fit).sum();
    // CCF FIT = total FIT * average beta factor (approximation)
    let total_ccf_fit: f64 = domain_list.iter().map(|d| d.total_fit * 0.2).sum(); // 20% beta
    let level_shifter_coverage: usize = domain_list.iter().map(|d| d.level_shifters.len()).sum();
    let isolation_cell_coverage: usize = domain_list.iter().map(|d| d.isolation_cells.len()).sum();

    PowerDomainAnalysis {
        domains: domain_list,
        ccf_groups,
        level_shifter_coverage,
        isolation_cell_coverage,
        total_boundary_crossings: 0, // Would need connectivity analysis
        unprotected_crossings: Vec::new(),
        summary: PowerDomainSummary {
            domain_count: 0,
            total_cells,
            total_fit,
            total_ccf_fit,
            boundary_protection_percentage: 100.0, // Placeholder
        },
    }
}

/// Pattern for identifying power domains from cell paths
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PowerDomainPattern {
    /// Domain name
    pub name: String,
    /// Path pattern to match
    pub path_pattern: String,
    /// Nominal voltage
    pub voltage: f64,
    /// Is this a safety-critical domain?
    pub safety_critical: bool,
}

impl PowerDomainPattern {
    /// Create a new power domain pattern
    pub fn new(name: &str, path_pattern: &str, voltage: f64, safety_critical: bool) -> Self {
        Self {
            name: name.to_string(),
            path_pattern: path_pattern.to_string(),
            voltage,
            safety_critical,
        }
    }
}

/// Merge power domain CCF results into existing CCF analysis
pub fn merge_power_domain_ccf(
    existing: &mut CcfAnalysisResults,
    power_analysis: &PowerDomainAnalysis,
) {
    for group in &power_analysis.ccf_groups {
        existing.groups.push(group.clone());
    }
    // Add total CCF contribution from power domains
    existing.total_common_cause_fit += power_analysis.summary.total_ccf_fit;
}

/// Generate human-readable power domain analysis report
pub fn format_power_domain_report(analysis: &PowerDomainAnalysis) -> String {
    let mut output = String::new();

    output.push_str("Power Domain Analysis Report\n");
    output.push_str("═══════════════════════════════════════════════════════════════\n\n");

    // Summary
    output.push_str("SUMMARY\n");
    output.push_str("───────────────────────────────────────────────────────────────\n");
    output.push_str(&format!(
        "Power domains identified: {}\n",
        analysis.domains.len()
    ));
    output.push_str(&format!(
        "Total cells analyzed: {}\n",
        analysis.summary.total_cells
    ));
    output.push_str(&format!(
        "Total FIT: {:.4} FIT\n",
        analysis.summary.total_fit
    ));
    output.push_str(&format!(
        "CCF contribution (λDPF_power): {:.4} FIT\n",
        analysis.summary.total_ccf_fit
    ));
    output.push_str(&format!(
        "Level shifters: {}\n",
        analysis.level_shifter_coverage
    ));
    output.push_str(&format!(
        "Isolation cells: {}\n\n",
        analysis.isolation_cell_coverage
    ));

    // Per-domain breakdown
    output.push_str("PER-DOMAIN BREAKDOWN\n");
    output.push_str("───────────────────────────────────────────────────────────────\n");

    for domain in &analysis.domains {
        output.push_str(&format!("{}:\n", domain.name));
        output.push_str(&format!("  Voltage: {:.2}V\n", domain.voltage));
        output.push_str(&format!("  Cells: {}\n", domain.cells.len()));
        output.push_str(&format!("  Total FIT: {:.4} FIT\n", domain.total_fit));
        output.push_str(&format!(
            "  Level shifters: {}\n",
            domain.level_shifters.len()
        ));
        output.push_str(&format!(
            "  Isolation cells: {}\n\n",
            domain.isolation_cells.len()
        ));
    }

    // CCF groups
    output.push_str("CCF GROUPS CREATED\n");
    output.push_str("───────────────────────────────────────────────────────────────\n");

    for group in &analysis.ccf_groups {
        // Calculate CCF FIT for display (we don't have direct access to cell FIT here)
        output.push_str(&format!(
            "{}: β={:.2} ({} cells)\n",
            group.name,
            group.beta_factor,
            group.members.len()
        ));
    }

    // Unprotected crossings
    if !analysis.unprotected_crossings.is_empty() {
        output.push_str("\nWARNINGS: UNPROTECTED BOUNDARY CROSSINGS\n");
        output.push_str("───────────────────────────────────────────────────────────────\n");
        for crossing in &analysis.unprotected_crossings {
            output.push_str(&format!(
                "  {} -> {}: {:?} (Risk: {:?})\n",
                crossing.source_domain,
                crossing.target_domain,
                crossing.crossing_type,
                crossing.risk_level
            ));
        }
    }

    output
}

// ============================================================================
// GateNetlist-Based Power Domain Extraction
// ============================================================================

/// Extract power domain information from a GateNetlist
///
/// This function analyzes the gate-level netlist to identify power domains
/// and calculate CCF (Common Cause Failure) contributions for ISO 26262.
pub fn extract_power_domains_from_gate_netlist(
    netlist: &skalp_lir::gate_netlist::GateNetlist,
) -> PowerDomainAnalysis {
    // For now, assume a single default power domain containing all cells
    // Future: parse power domain annotations from cell attributes

    let total_cells = netlist.cells.len();
    let total_fit = netlist.total_fit();

    // Collect all cell paths
    let cell_paths: Vec<String> = netlist.cells.iter().map(|c| c.path.clone()).collect();

    // Create a single default power domain
    let default_domain = PowerDomainInfo {
        name: "VDD_CORE".to_string(),
        voltage: 1.0,
        cells: cell_paths.clone(),
        total_fit,
        level_shifters: vec![],
        isolation_cells: vec![],
    };

    // CCF contribution from power domain
    // Per ISO 26262-9: β factor typically 2-10% for power supply failures
    let beta_factor = 0.05; // 5% beta factor for power domain CCF
    let ccf_fit = total_fit * beta_factor;

    let ccf_group = CcfGroup {
        name: "power_domain_vdd_core".to_string(),
        members: cell_paths,
        beta_factor,
        cause: CcfCause::SharedPower,
        description: Some("All cells share VDD_CORE power domain".to_string()),
    };

    PowerDomainAnalysis {
        domains: vec![default_domain],
        ccf_groups: vec![ccf_group],
        level_shifter_coverage: 0,
        isolation_cell_coverage: 0,
        total_boundary_crossings: 0,
        unprotected_crossings: vec![],
        summary: PowerDomainSummary {
            domain_count: 1,
            total_cells,
            total_fit,
            total_ccf_fit: ccf_fit,
            boundary_protection_percentage: 100.0, // No crossings = fully protected
        },
    }
}

/// Infer voltage from power domain name
fn infer_voltage_from_name(name: &str) -> f64 {
    let lower = name.to_lowercase();

    // Check for explicit voltage indicators
    if lower.contains("5v") || lower.contains("5_0v") {
        return 5.0;
    }
    if lower.contains("3v3") || lower.contains("3_3v") {
        return 3.3;
    }
    if lower.contains("2v5") || lower.contains("2_5v") {
        return 2.5;
    }
    if lower.contains("1v8") || lower.contains("1_8v") {
        return 1.8;
    }
    if lower.contains("1v2") || lower.contains("1_2v") {
        return 1.2;
    }
    if lower.contains("1v") || lower.contains("1_0v") || lower.contains("core") {
        return 1.0;
    }
    if lower.contains("io") || lower.contains("pad") {
        return 3.3; // Typical I/O voltage
    }
    if lower.contains("analog") {
        return 5.0; // Typical analog supply
    }

    // Default to core voltage
    1.0
}

// ============================================================================
// Power Domain Isolation Verification (ISO 26262 Compliance)
// ============================================================================

/// Result of isolation cell verification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IsolationCellVerification {
    /// Total domain boundary crossings
    pub total_crossings: usize,
    /// Crossings with proper isolation cells
    pub protected_crossings: usize,
    /// Crossings missing isolation cells
    pub missing_isolation: Vec<MissingIsolationCell>,
    /// Verification status
    pub compliant: bool,
    /// Coverage percentage
    pub coverage_percentage: f64,
    /// Recommendations
    pub recommendations: Vec<String>,
}

/// Missing isolation cell warning
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MissingIsolationCell {
    /// Source domain
    pub source_domain: String,
    /// Target domain
    pub target_domain: String,
    /// Signal path affected
    pub signal_path: String,
    /// Source domain voltage
    pub source_voltage: f64,
    /// Target domain voltage
    pub target_voltage: f64,
    /// Safety impact assessment
    pub safety_impact: SafetyImpact,
}

/// Safety impact level for missing isolation
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum SafetyImpact {
    /// Low impact - same voltage, same safety level
    Low,
    /// Medium impact - voltage mismatch or different safety levels
    Medium,
    /// High impact - significant voltage mismatch or ASIL mismatch
    High,
    /// Critical - safety critical signal without isolation
    Critical,
}

/// Result of level shifter verification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LevelShifterVerification {
    /// Total voltage level crossings
    pub total_voltage_crossings: usize,
    /// Crossings with proper level shifters
    pub shifters_present: usize,
    /// Missing level shifters
    pub missing_shifters: Vec<MissingLevelShifter>,
    /// Verification status
    pub compliant: bool,
    /// Coverage percentage
    pub coverage_percentage: f64,
    /// Voltage domain pairs analyzed
    pub voltage_pairs: Vec<VoltageDomainPair>,
}

/// Missing level shifter warning
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MissingLevelShifter {
    /// Source domain
    pub source_domain: String,
    /// Target domain
    pub target_domain: String,
    /// Source voltage (V)
    pub source_voltage: f64,
    /// Target voltage (V)
    pub target_voltage: f64,
    /// Affected signals
    pub affected_signals: Vec<String>,
    /// Risk of damage (overvoltage to target)
    pub damage_risk: bool,
    /// Risk of incorrect operation (undervoltage)
    pub logic_risk: bool,
}

/// Voltage domain pair analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VoltageDomainPair {
    /// Domain A name
    pub domain_a: String,
    /// Domain B name
    pub domain_b: String,
    /// Voltage A (V)
    pub voltage_a: f64,
    /// Voltage B (V)
    pub voltage_b: f64,
    /// Number of crossings between them
    pub crossing_count: usize,
    /// Level shifters present
    pub shifters_present: usize,
    /// Direction-aware analysis
    pub directions: Vec<CrossingDirection>,
}

/// Direction of cross-domain signal
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CrossingDirection {
    /// From high to low voltage
    pub high_to_low: bool,
    /// Signal count in this direction
    pub signal_count: usize,
    /// Level shifter present
    pub shifter_present: bool,
}

/// Complete cross-domain signal analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CrossDomainSignalAnalysis {
    /// Total signals crossing domains
    pub total_cross_domain_signals: usize,
    /// Signals with proper protection
    pub properly_protected: usize,
    /// Detailed analysis per crossing
    pub crossings: Vec<CrossDomainSignalInfo>,
    /// Safety-critical signals crossing domains
    pub safety_critical_crossings: Vec<CrossDomainSignalInfo>,
    /// Timing violations found
    pub timing_issues: Vec<TimingIssue>,
    /// Overall compliance status
    pub compliance_status: ComplianceStatus,
}

/// Information about a cross-domain signal
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CrossDomainSignalInfo {
    /// Signal path
    pub signal_path: String,
    /// Source domain
    pub source_domain: String,
    /// Target domain
    pub target_domain: String,
    /// Is this a safety-critical signal
    pub safety_critical: bool,
    /// Protection mechanisms present
    pub protection: CrossingProtection,
    /// Compliance status for this signal
    pub compliant: bool,
    /// Issues found
    pub issues: Vec<String>,
}

/// Protection mechanisms for cross-domain signals
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CrossingProtection {
    /// Isolation cell present
    pub isolation_cell: bool,
    /// Level shifter present
    pub level_shifter: bool,
    /// Synchronizer present (for clock domain crossing)
    pub synchronizer: bool,
    /// ESD protection present
    pub esd_protection: bool,
    /// Glitch filter present
    pub glitch_filter: bool,
}

/// Timing issue for cross-domain signal
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimingIssue {
    /// Signal path
    pub signal_path: String,
    /// Issue description
    pub description: String,
    /// Maximum allowed delay
    pub max_delay_ns: Option<f64>,
    /// Actual delay (if known)
    pub actual_delay_ns: Option<f64>,
}

/// Overall compliance status
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ComplianceStatus {
    /// Fully compliant with all requirements
    Compliant,
    /// Minor issues, acceptable with documentation
    AcceptableWithWarnings,
    /// Major issues requiring resolution
    NonCompliant,
    /// Cannot determine compliance (missing information)
    Unknown,
}

/// Verify isolation cells exist at all domain boundaries
///
/// Per ISO 26262-5 Section 7.4.11, power domain boundaries must have proper
/// isolation to prevent fault propagation between domains.
pub fn verify_isolation_cells(
    domains: &[PowerDomainInfo],
    connectivity: &IndexMap<String, Vec<String>>,
) -> IsolationCellVerification {
    let mut total_crossings = 0;
    let mut protected_crossings = 0;
    let mut missing_isolation = Vec::new();
    let mut recommendations = Vec::new();

    // Build domain lookup
    let domain_lookup: IndexMap<&str, &PowerDomainInfo> = domains
        .iter()
        .flat_map(|d| d.cells.iter().map(move |c| (c.as_str(), d)))
        .collect();

    // Analyze connectivity for cross-domain signals
    for (source_cell, targets) in connectivity {
        let source_domain = domain_lookup.get(source_cell.as_str());

        for target_cell in targets {
            let target_domain = domain_lookup.get(target_cell.as_str());

            // Check if this is a cross-domain signal
            if let (Some(src_domain), Some(tgt_domain)) = (source_domain, target_domain) {
                if src_domain.name != tgt_domain.name {
                    total_crossings += 1;

                    // Check if there's an isolation cell on this path
                    let has_isolation = src_domain.isolation_cells.iter().any(|ic| {
                        connectivity.get(ic).is_some_and(|ic_targets| {
                            ic_targets.contains(target_cell)
                                || ic_targets.iter().any(|t| t == source_cell)
                        })
                    }) || tgt_domain.isolation_cells.iter().any(|ic| {
                        connectivity
                            .get(source_cell)
                            .is_some_and(|src_targets| src_targets.contains(ic))
                    });

                    if has_isolation
                        || !src_domain.isolation_cells.is_empty()
                            && !tgt_domain.isolation_cells.is_empty()
                    {
                        protected_crossings += 1;
                    } else {
                        let safety_impact = assess_safety_impact(src_domain, tgt_domain);
                        missing_isolation.push(MissingIsolationCell {
                            source_domain: src_domain.name.clone(),
                            target_domain: tgt_domain.name.clone(),
                            signal_path: format!("{} -> {}", source_cell, target_cell),
                            source_voltage: src_domain.voltage,
                            target_voltage: tgt_domain.voltage,
                            safety_impact,
                        });
                    }
                }
            }
        }
    }

    let coverage_percentage = if total_crossings > 0 {
        (protected_crossings as f64 / total_crossings as f64) * 100.0
    } else {
        100.0 // No crossings = fully compliant
    };

    // Generate recommendations
    if !missing_isolation.is_empty() {
        let critical_count = missing_isolation
            .iter()
            .filter(|m| m.safety_impact == SafetyImpact::Critical)
            .count();
        if critical_count > 0 {
            recommendations.push(format!(
                "CRITICAL: {} safety-critical domain crossings lack isolation cells",
                critical_count
            ));
        }

        let voltage_mismatches: Vec<_> = missing_isolation
            .iter()
            .filter(|m| (m.source_voltage - m.target_voltage).abs() > 0.5)
            .collect();
        if !voltage_mismatches.is_empty() {
            recommendations.push(format!(
                "Add isolation cells for {} voltage-mismatched crossings to prevent damage",
                voltage_mismatches.len()
            ));
        }
    }

    let compliant = missing_isolation
        .iter()
        .all(|m| m.safety_impact != SafetyImpact::Critical)
        && coverage_percentage >= 90.0;

    IsolationCellVerification {
        total_crossings,
        protected_crossings,
        missing_isolation,
        compliant,
        coverage_percentage,
        recommendations,
    }
}

/// Assess safety impact of missing isolation
fn assess_safety_impact(source: &PowerDomainInfo, target: &PowerDomainInfo) -> SafetyImpact {
    let voltage_diff = (source.voltage - target.voltage).abs();

    // High voltage difference is critical
    if voltage_diff > 1.5 {
        return SafetyImpact::Critical;
    }

    // Check based on number of cells (larger domains = higher risk)
    let total_cells = source.cells.len() + target.cells.len();
    if total_cells > 100 {
        SafetyImpact::High
    } else if voltage_diff > 0.5 || total_cells > 20 {
        SafetyImpact::Medium
    } else {
        SafetyImpact::Low
    }
}

/// Verify level shifters exist for voltage level crossings
///
/// Level shifters are required when signals cross between domains with
/// different voltage levels to ensure proper signal integrity.
pub fn verify_level_shifters(
    domains: &[PowerDomainInfo],
    connectivity: &IndexMap<String, Vec<String>>,
) -> LevelShifterVerification {
    let mut total_voltage_crossings = 0;
    let mut shifters_present = 0;
    let mut missing_shifters = Vec::new();
    let mut voltage_pairs: IndexMap<(String, String), VoltageDomainPair> = IndexMap::new();

    // Build domain lookup
    let domain_lookup: IndexMap<&str, &PowerDomainInfo> = domains
        .iter()
        .flat_map(|d| d.cells.iter().map(move |c| (c.as_str(), d)))
        .collect();

    // Level shifter cell paths
    let shifter_cells: std::collections::HashSet<&str> = domains
        .iter()
        .flat_map(|d| d.level_shifters.iter().map(|s| s.as_str()))
        .collect();

    // Analyze voltage crossings
    for (source_cell, targets) in connectivity {
        let source_domain = domain_lookup.get(source_cell.as_str());

        for target_cell in targets {
            let target_domain = domain_lookup.get(target_cell.as_str());

            if let (Some(src_domain), Some(tgt_domain)) = (source_domain, target_domain) {
                // Check for voltage mismatch (>0.1V difference)
                if (src_domain.voltage - tgt_domain.voltage).abs() > 0.1 {
                    total_voltage_crossings += 1;

                    // Check if there's a level shifter on this path
                    let has_shifter = shifter_cells.contains(source_cell.as_str())
                        || shifter_cells.contains(target_cell.as_str())
                        || connectivity.get(source_cell).is_some_and(|t| {
                            t.iter().any(|cell| shifter_cells.contains(cell.as_str()))
                        });

                    if has_shifter {
                        shifters_present += 1;
                    } else {
                        let damage_risk = src_domain.voltage > tgt_domain.voltage + 0.3;
                        let logic_risk = src_domain.voltage < tgt_domain.voltage - 0.3;

                        // Find existing entry or create new
                        let key = if src_domain.name < tgt_domain.name {
                            (src_domain.name.clone(), tgt_domain.name.clone())
                        } else {
                            (tgt_domain.name.clone(), src_domain.name.clone())
                        };

                        let pair =
                            voltage_pairs
                                .entry(key.clone())
                                .or_insert_with(|| VoltageDomainPair {
                                    domain_a: key.0.clone(),
                                    domain_b: key.1.clone(),
                                    voltage_a: if src_domain.name < tgt_domain.name {
                                        src_domain.voltage
                                    } else {
                                        tgt_domain.voltage
                                    },
                                    voltage_b: if src_domain.name < tgt_domain.name {
                                        tgt_domain.voltage
                                    } else {
                                        src_domain.voltage
                                    },
                                    crossing_count: 0,
                                    shifters_present: 0,
                                    directions: Vec::new(),
                                });
                        pair.crossing_count += 1;

                        missing_shifters.push(MissingLevelShifter {
                            source_domain: src_domain.name.clone(),
                            target_domain: tgt_domain.name.clone(),
                            source_voltage: src_domain.voltage,
                            target_voltage: tgt_domain.voltage,
                            affected_signals: vec![format!("{} -> {}", source_cell, target_cell)],
                            damage_risk,
                            logic_risk,
                        });
                    }
                }
            }
        }
    }

    let coverage_percentage = if total_voltage_crossings > 0 {
        (shifters_present as f64 / total_voltage_crossings as f64) * 100.0
    } else {
        100.0
    };

    let compliant = missing_shifters.iter().all(|m| !m.damage_risk) && coverage_percentage >= 95.0;

    LevelShifterVerification {
        total_voltage_crossings,
        shifters_present,
        missing_shifters,
        compliant,
        coverage_percentage,
        voltage_pairs: voltage_pairs.into_values().collect(),
    }
}

/// Analyze all cross-domain signals comprehensively
pub fn analyze_cross_domain_signals(
    domains: &[PowerDomainInfo],
    connectivity: &IndexMap<String, Vec<String>>,
    safety_critical_paths: &[String],
) -> CrossDomainSignalAnalysis {
    let mut crossings = Vec::new();
    let mut safety_critical_crossings = Vec::new();
    let timing_issues = Vec::new();
    let mut properly_protected = 0;

    // Build domain lookup
    let domain_lookup: IndexMap<&str, &PowerDomainInfo> = domains
        .iter()
        .flat_map(|d| d.cells.iter().map(move |c| (c.as_str(), d)))
        .collect();

    // Level shifter and isolation cell lookups
    let shifter_cells: std::collections::HashSet<&str> = domains
        .iter()
        .flat_map(|d| d.level_shifters.iter().map(|s| s.as_str()))
        .collect();
    let isolation_cells: std::collections::HashSet<&str> = domains
        .iter()
        .flat_map(|d| d.isolation_cells.iter().map(|s| s.as_str()))
        .collect();

    for (source_cell, targets) in connectivity {
        let source_domain = domain_lookup.get(source_cell.as_str());

        for target_cell in targets {
            let target_domain = domain_lookup.get(target_cell.as_str());

            if let (Some(src_domain), Some(tgt_domain)) = (source_domain, target_domain) {
                if src_domain.name != tgt_domain.name {
                    let signal_path = format!("{} -> {}", source_cell, target_cell);
                    let is_safety_critical = safety_critical_paths
                        .iter()
                        .any(|p| signal_path.contains(p) || p.contains(&signal_path));

                    let voltage_mismatch = (src_domain.voltage - tgt_domain.voltage).abs() > 0.1;

                    let protection = CrossingProtection {
                        isolation_cell: isolation_cells.contains(source_cell.as_str())
                            || isolation_cells.contains(target_cell.as_str()),
                        level_shifter: !voltage_mismatch
                            || shifter_cells.contains(source_cell.as_str())
                            || shifter_cells.contains(target_cell.as_str()),
                        synchronizer: false,   // Would need clock domain info
                        esd_protection: false, // Would need pad info
                        glitch_filter: false,
                    };

                    let mut issues = Vec::new();
                    if !protection.isolation_cell {
                        issues.push("Missing isolation cell".to_string());
                    }
                    if voltage_mismatch && !protection.level_shifter {
                        issues.push(format!(
                            "Missing level shifter ({:.1}V -> {:.1}V)",
                            src_domain.voltage, tgt_domain.voltage
                        ));
                    }

                    let compliant = issues.is_empty() || (!is_safety_critical && issues.len() == 1);

                    if compliant {
                        properly_protected += 1;
                    }

                    let info = CrossDomainSignalInfo {
                        signal_path: signal_path.clone(),
                        source_domain: src_domain.name.clone(),
                        target_domain: tgt_domain.name.clone(),
                        safety_critical: is_safety_critical,
                        protection,
                        compliant,
                        issues,
                    };

                    if is_safety_critical {
                        safety_critical_crossings.push(info.clone());
                    }
                    crossings.push(info);
                }
            }
        }
    }

    let total_cross_domain_signals = crossings.len();
    let compliance_status = if safety_critical_crossings.iter().all(|c| c.compliant) {
        if crossings.iter().all(|c| c.compliant) {
            ComplianceStatus::Compliant
        } else {
            ComplianceStatus::AcceptableWithWarnings
        }
    } else {
        ComplianceStatus::NonCompliant
    };

    CrossDomainSignalAnalysis {
        total_cross_domain_signals,
        properly_protected,
        crossings,
        safety_critical_crossings,
        timing_issues,
        compliance_status,
    }
}

/// Format isolation verification report
pub fn format_isolation_verification_report(
    isolation: &IsolationCellVerification,
    level_shifters: &LevelShifterVerification,
    signals: &CrossDomainSignalAnalysis,
) -> String {
    let mut output = String::new();

    output.push_str("Power Domain Isolation Verification Report\n");
    output.push_str("═══════════════════════════════════════════════════════════════\n\n");

    // Overall status
    let overall_status = match (
        &isolation.compliant,
        &level_shifters.compliant,
        &signals.compliance_status,
    ) {
        (true, true, ComplianceStatus::Compliant) => "PASS",
        (true, true, ComplianceStatus::AcceptableWithWarnings) => "PASS (with warnings)",
        _ => "FAIL",
    };
    output.push_str(&format!("Overall Status: {}\n\n", overall_status));

    // Isolation cell verification
    output.push_str("ISOLATION CELL VERIFICATION\n");
    output.push_str("───────────────────────────────────────────────────────────────\n");
    output.push_str(&format!(
        "Total domain crossings: {}\n",
        isolation.total_crossings
    ));
    output.push_str(&format!(
        "Protected crossings: {}\n",
        isolation.protected_crossings
    ));
    output.push_str(&format!(
        "Coverage: {:.1}%\n",
        isolation.coverage_percentage
    ));
    output.push_str(&format!(
        "Status: {}\n\n",
        if isolation.compliant {
            "COMPLIANT"
        } else {
            "NON-COMPLIANT"
        }
    ));

    if !isolation.missing_isolation.is_empty() {
        output.push_str("Missing isolation cells:\n");
        for missing in &isolation.missing_isolation {
            output.push_str(&format!(
                "  {} -> {} ({:.1}V -> {:.1}V) [{:?}]\n",
                missing.source_domain,
                missing.target_domain,
                missing.source_voltage,
                missing.target_voltage,
                missing.safety_impact
            ));
        }
        output.push('\n');
    }

    // Level shifter verification
    output.push_str("LEVEL SHIFTER VERIFICATION\n");
    output.push_str("───────────────────────────────────────────────────────────────\n");
    output.push_str(&format!(
        "Total voltage crossings: {}\n",
        level_shifters.total_voltage_crossings
    ));
    output.push_str(&format!(
        "Shifters present: {}\n",
        level_shifters.shifters_present
    ));
    output.push_str(&format!(
        "Coverage: {:.1}%\n",
        level_shifters.coverage_percentage
    ));
    output.push_str(&format!(
        "Status: {}\n\n",
        if level_shifters.compliant {
            "COMPLIANT"
        } else {
            "NON-COMPLIANT"
        }
    ));

    if !level_shifters.missing_shifters.is_empty() {
        output.push_str("Missing level shifters:\n");
        for missing in level_shifters.missing_shifters.iter().take(10) {
            output.push_str(&format!(
                "  {} -> {} ({:.1}V -> {:.1}V) {}{}\n",
                missing.source_domain,
                missing.target_domain,
                missing.source_voltage,
                missing.target_voltage,
                if missing.damage_risk {
                    "[DAMAGE RISK] "
                } else {
                    ""
                },
                if missing.logic_risk {
                    "[LOGIC RISK]"
                } else {
                    ""
                }
            ));
        }
        if level_shifters.missing_shifters.len() > 10 {
            output.push_str(&format!(
                "  ... and {} more\n",
                level_shifters.missing_shifters.len() - 10
            ));
        }
        output.push('\n');
    }

    // Cross-domain signal analysis
    output.push_str("CROSS-DOMAIN SIGNAL ANALYSIS\n");
    output.push_str("───────────────────────────────────────────────────────────────\n");
    output.push_str(&format!(
        "Total signals: {}\n",
        signals.total_cross_domain_signals
    ));
    output.push_str(&format!(
        "Properly protected: {}\n",
        signals.properly_protected
    ));
    output.push_str(&format!(
        "Safety-critical signals: {}\n",
        signals.safety_critical_crossings.len()
    ));
    output.push_str(&format!("Compliance: {:?}\n\n", signals.compliance_status));

    if !signals.safety_critical_crossings.is_empty() {
        output.push_str("Safety-critical crossings:\n");
        for crossing in &signals.safety_critical_crossings {
            let status = if crossing.compliant { "[OK]" } else { "[FAIL]" };
            output.push_str(&format!(
                "  {} {} {} -> {}\n",
                status, crossing.signal_path, crossing.source_domain, crossing.target_domain
            ));
            for issue in &crossing.issues {
                output.push_str(&format!("    - {}\n", issue));
            }
        }
    }

    // Recommendations
    if !isolation.recommendations.is_empty() {
        output.push_str("\nRECOMMENDATIONS\n");
        output.push_str("───────────────────────────────────────────────────────────────\n");
        for rec in &isolation.recommendations {
            output.push_str(&format!("• {}\n", rec));
        }
    }

    output
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
                    max_voltage_levels: IndexMap::new(),
                    timing_constraints: TimingConstraints {
                        max_rise_time: 10.0,
                        max_fall_time: 10.0,
                        setup_time: 2.0,
                        hold_time: 1.0,
                        max_skew: 0.5,
                    },
                    emi_requirements: EmiRequirements {
                        max_emission_levels: IndexMap::new(),
                        immunity_levels: IndexMap::new(),
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
                    max_voltage_levels: IndexMap::new(),
                    timing_constraints: TimingConstraints {
                        max_rise_time: 5.0,
                        max_fall_time: 5.0,
                        setup_time: 1.0,
                        hold_time: 0.5,
                        max_skew: 0.2,
                    },
                    emi_requirements: EmiRequirements {
                        max_emission_levels: IndexMap::new(),
                        immunity_levels: IndexMap::new(),
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
            power_states: vec![
                PowerState::Active,
                PowerState::Standby,
                PowerState::PoweredOff,
            ],
            current_state: PowerState::Active,
            isolation_requirements: IsolationRequirements {
                electrical_isolation: true,
                galvanic_isolation: true,
                optical_isolation: false,
                min_isolation_voltage: 8000.0,
                isolation_tests: vec![],
                signal_constraints: SignalConstraints {
                    max_voltage_levels: IndexMap::new(),
                    timing_constraints: TimingConstraints {
                        max_rise_time: 2.0,
                        max_fall_time: 2.0,
                        setup_time: 0.5,
                        hold_time: 0.3,
                        max_skew: 0.1,
                    },
                    emi_requirements: EmiRequirements {
                        max_emission_levels: IndexMap::new(),
                        immunity_levels: IndexMap::new(),
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
        assert!(matches!(
            result.unwrap_err(),
            PowerDomainError::SafetyViolation(_)
        ));
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
                    max_voltage_levels: IndexMap::new(),
                    timing_constraints: TimingConstraints {
                        max_rise_time: 10.0,
                        max_fall_time: 10.0,
                        setup_time: 2.0,
                        hold_time: 1.0,
                        max_skew: 0.5,
                    },
                    emi_requirements: EmiRequirements {
                        max_emission_levels: IndexMap::new(),
                        immunity_levels: IndexMap::new(),
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
                    max_voltage_levels: IndexMap::new(),
                    timing_constraints: TimingConstraints {
                        max_rise_time: 5.0,
                        max_fall_time: 5.0,
                        setup_time: 1.0,
                        hold_time: 0.5,
                        max_skew: 0.2,
                    },
                    emi_requirements: EmiRequirements {
                        max_emission_levels: IndexMap::new(),
                        immunity_levels: IndexMap::new(),
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
        assert_eq!(
            analysis.violations[0].violation_type,
            ViolationType::InsufficientIsolation
        );
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
                    max_voltage_levels: IndexMap::new(),
                    timing_constraints: TimingConstraints {
                        max_rise_time: 10.0,
                        max_fall_time: 10.0,
                        setup_time: 2.0,
                        hold_time: 1.0,
                        max_skew: 0.5,
                    },
                    emi_requirements: EmiRequirements {
                        max_emission_levels: IndexMap::new(),
                        immunity_levels: IndexMap::new(),
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

    // ======================================================================
    // CCF Integration Tests
    // ======================================================================

    #[test]
    fn test_power_domain_ccf_groups() {
        let domains = vec![
            PowerDomainInfo {
                name: "core_1v".to_string(),
                voltage: 1.0,
                cells: vec!["top.core.cell1".to_string(), "top.core.cell2".to_string()],
                total_fit: 0.5,
                level_shifters: vec![],
                isolation_cells: vec![],
            },
            PowerDomainInfo {
                name: "io_3v3".to_string(),
                voltage: 3.3,
                cells: vec!["top.io.cell1".to_string()],
                total_fit: 0.3,
                level_shifters: vec![],
                isolation_cells: vec![],
            },
        ];

        let ccf_groups = power_domain_ccf_groups(&domains);

        assert_eq!(ccf_groups.len(), 2);

        // Check first CCF group
        let core_group = ccf_groups
            .iter()
            .find(|g| g.name.contains("core_1v"))
            .unwrap();
        assert_eq!(core_group.members.len(), 2);
        assert_eq!(core_group.cause, CcfCause::SharedPower);
        assert!((core_group.beta_factor - 0.2).abs() < 0.001);
    }

    #[test]
    fn test_analyze_power_domains_from_paths() {
        let cell_paths = vec![
            ("top.core_1v.alu.add".to_string(), 0.1),
            ("top.core_1v.alu.mul".to_string(), 0.15),
            ("top.io_3v3.uart.tx".to_string(), 0.08),
            ("top.io_3v3.uart.rx".to_string(), 0.07),
            ("top.io_3v3.lvl_shift_1".to_string(), 0.02),
            ("top.misc.other".to_string(), 0.05),
        ];

        let patterns = vec![
            PowerDomainPattern::new("core_1v", "core_1v", 1.0, true),
            PowerDomainPattern::new("io_3v3", "io_3v3", 3.3, false),
        ];

        let analysis = analyze_power_domains_from_paths(&cell_paths, &patterns);

        // Should identify 2 domains
        assert_eq!(analysis.domains.len(), 2);

        // Check core domain
        let core = analysis
            .domains
            .iter()
            .find(|d| d.name == "core_1v")
            .unwrap();
        assert_eq!(core.cells.len(), 2);
        assert!((core.total_fit - 0.25).abs() < 0.001);

        // Check IO domain
        let io = analysis
            .domains
            .iter()
            .find(|d| d.name == "io_3v3")
            .unwrap();
        assert_eq!(io.cells.len(), 3);
        assert_eq!(io.level_shifters.len(), 1); // lvl_shift_1

        // CCF groups should be created
        assert_eq!(analysis.ccf_groups.len(), 2);
    }

    #[test]
    fn test_power_domain_pattern() {
        let pattern = PowerDomainPattern::new("vdd_core", "top.vdd_core", 0.8, true);

        assert_eq!(pattern.name, "vdd_core");
        assert!((pattern.voltage - 0.8).abs() < 0.001);
        assert!(pattern.safety_critical);
    }

    #[test]
    fn test_power_domain_report_format() {
        let domains = vec![PowerDomainInfo {
            name: "test_domain".to_string(),
            voltage: 1.2,
            cells: vec!["cell1".to_string()],
            total_fit: 0.1,
            level_shifters: vec![],
            isolation_cells: vec![],
        }];

        let ccf_groups = power_domain_ccf_groups(&domains);

        let analysis = PowerDomainAnalysis {
            domains,
            ccf_groups,
            level_shifter_coverage: 0,
            isolation_cell_coverage: 0,
            total_boundary_crossings: 0,
            unprotected_crossings: vec![],
            summary: PowerDomainSummary {
                domain_count: 1,
                total_cells: 1,
                total_fit: 0.1,
                total_ccf_fit: 0.02,
                boundary_protection_percentage: 100.0,
            },
        };

        let report = format_power_domain_report(&analysis);

        assert!(report.contains("Power Domain Analysis Report"));
        assert!(report.contains("test_domain"));
        assert!(report.contains("CCF GROUPS CREATED"));
    }
}
