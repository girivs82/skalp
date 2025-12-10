//! FMEDA Library System for ISO 26262 Functional Safety
//!
//! Provides library-based component failure data for FMEDA calculations.
//! Components can specify base FIT rates, failure mode distributions,
//! and default diagnostic coverages for common safety mechanisms.

use crate::asil::AsilLevel;
use crate::hierarchy::{DesignRef, FailureClass, FailureMode, MechanismType, Severity};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;

// ============================================================================
// FMEDA Library Core Types
// ============================================================================

/// FMEDA Library - collection of component failure data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FmedaLibrary {
    /// Library name (e.g., "automotive_grade")
    pub name: String,
    /// Library version
    pub version: String,
    /// Library description
    pub description: String,
    /// Library source (manufacturer, standard, etc.)
    pub source: String,
    /// Components in library
    pub components: HashMap<String, LibraryComponent>,
    /// Default temperature for FIT rates (°C)
    pub reference_temperature: f64,
    /// Library metadata
    pub metadata: LibraryMetadata,
}

impl FmedaLibrary {
    /// Create a new empty library
    pub fn new(name: String) -> Self {
        Self {
            name,
            version: "1.0.0".to_string(),
            description: String::new(),
            source: String::new(),
            components: HashMap::new(),
            reference_temperature: 55.0, // Default junction temp
            metadata: LibraryMetadata::default(),
        }
    }

    /// Add a component to the library
    pub fn add_component(&mut self, component: LibraryComponent) {
        self.components.insert(component.part_number.clone(), component);
    }

    /// Get a component by part number
    pub fn get_component(&self, part_number: &str) -> Option<&LibraryComponent> {
        self.components.get(part_number)
    }

    /// Calculate total FIT for a component with mechanisms applied
    pub fn calculate_fit(&self, part_number: &str, mechanisms: &[MechanismType]) -> Option<FitBreakdown> {
        let component = self.get_component(part_number)?;
        Some(component.calculate_fit(mechanisms))
    }
}

/// Library metadata
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct LibraryMetadata {
    /// Creation date
    pub created: Option<String>,
    /// Last modified date
    pub modified: Option<String>,
    /// Author/creator
    pub author: Option<String>,
    /// Standard compliance (e.g., "IEC 62380", "SN 29500")
    pub standard: Option<String>,
    /// Notes
    pub notes: Option<String>,
}

/// Component failure data in library
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LibraryComponent {
    /// Part number (unique identifier)
    pub part_number: String,
    /// Component name/description
    pub name: String,
    /// Component category
    pub category: ComponentCategory,
    /// Base total FIT rate (failures per 10^9 hours)
    pub base_fit: f64,
    /// Temperature derating factor
    pub derating_factor: Option<f64>,
    /// Failure mode distribution
    pub failure_modes: Vec<LibraryFailureMode>,
    /// Default diagnostic coverages for various mechanisms
    pub mechanism_coverages: HashMap<MechanismType, f64>,
    /// Component quality level
    pub quality_level: QualityLevel,
    /// Reliability data source
    pub data_source: String,
}

impl LibraryComponent {
    /// Create a new component
    pub fn new(part_number: String, name: String, base_fit: f64) -> Self {
        Self {
            part_number,
            name,
            category: ComponentCategory::Digital,
            base_fit,
            derating_factor: None,
            failure_modes: Vec::new(),
            mechanism_coverages: HashMap::new(),
            quality_level: QualityLevel::Commercial,
            data_source: String::new(),
        }
    }

    /// Add a failure mode
    pub fn add_failure_mode(&mut self, mode: LibraryFailureMode) {
        self.failure_modes.push(mode);
    }

    /// Set diagnostic coverage for a mechanism
    pub fn set_mechanism_coverage(&mut self, mechanism: MechanismType, dc: f64) {
        self.mechanism_coverages.insert(mechanism, dc);
    }

    /// Calculate FIT breakdown with mechanisms applied
    pub fn calculate_fit(&self, mechanisms: &[MechanismType]) -> FitBreakdown {
        let mut safe_fit = 0.0;
        let mut spf_fit = 0.0;
        let mut residual_fit = 0.0;
        let mut latent_fit = 0.0;

        // Get applicable diagnostic coverages
        let dc = self.get_effective_dc(mechanisms);

        for mode in &self.failure_modes {
            let mode_fit = self.base_fit * mode.distribution_pct / 100.0;

            match mode.default_class {
                FailureClass::Safe => {
                    safe_fit += mode_fit;
                }
                FailureClass::SinglePointFault => {
                    // Apply DC from mechanisms
                    let detected = mode_fit * dc / 100.0;
                    let undetected = mode_fit - detected;
                    safe_fit += detected; // Detected faults become safe
                    spf_fit += undetected;
                }
                FailureClass::Residual => {
                    residual_fit += mode_fit;
                }
                FailureClass::Latent => {
                    latent_fit += mode_fit;
                }
                FailureClass::MultiPoint => {
                    // MPFs contribute to latent
                    latent_fit += mode_fit;
                }
            }
        }

        FitBreakdown {
            total_fit: self.base_fit,
            safe_fit,
            spf_fit,
            residual_fit,
            latent_fit,
            dc_applied: dc,
        }
    }

    /// Get effective DC considering multiple mechanisms
    fn get_effective_dc(&self, mechanisms: &[MechanismType]) -> f64 {
        if mechanisms.is_empty() {
            return 0.0;
        }

        // For multiple mechanisms, use 1 - (1-DC1)(1-DC2)... formula
        let mut undetected = 1.0;
        for mech in mechanisms {
            if let Some(&dc) = self.mechanism_coverages.get(mech) {
                undetected *= 1.0 - dc / 100.0;
            }
        }
        (1.0 - undetected) * 100.0
    }
}

/// Component categories
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ComponentCategory {
    /// Digital logic (processors, FPGAs, ASICs)
    Digital,
    /// Analog circuits
    Analog,
    /// Memory (SRAM, DRAM, Flash)
    Memory,
    /// Power management
    Power,
    /// Sensors
    Sensor,
    /// Communication interfaces
    Communication,
    /// Clock/timing
    Clock,
    /// Passive components
    Passive,
    /// Mixed signal
    MixedSignal,
}

/// Quality levels for components
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum QualityLevel {
    /// Commercial grade
    Commercial,
    /// Industrial grade
    Industrial,
    /// Automotive grade (AEC-Q100, etc.)
    Automotive,
    /// Military/aerospace grade
    MilAero,
    /// Space grade
    Space,
}

/// Failure mode in library (template)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LibraryFailureMode {
    /// Failure mode name
    pub name: String,
    /// Description
    pub description: String,
    /// Distribution percentage of total FIT
    pub distribution_pct: f64,
    /// Default failure class
    pub default_class: FailureClass,
    /// Default severity
    pub default_severity: Severity,
    /// Typical detection mechanisms
    pub typical_detectors: Vec<MechanismType>,
}

impl LibraryFailureMode {
    /// Create a new library failure mode
    pub fn new(name: String, distribution_pct: f64, default_class: FailureClass) -> Self {
        Self {
            name,
            description: String::new(),
            distribution_pct,
            default_class,
            default_severity: Severity::S2,
            typical_detectors: Vec::new(),
        }
    }
}

/// FIT breakdown after safety analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FitBreakdown {
    /// Total base FIT
    pub total_fit: f64,
    /// Safe failure FIT
    pub safe_fit: f64,
    /// Single point fault FIT
    pub spf_fit: f64,
    /// Residual fault FIT
    pub residual_fit: f64,
    /// Latent fault FIT
    pub latent_fit: f64,
    /// Diagnostic coverage applied (percentage)
    pub dc_applied: f64,
}

impl FitBreakdown {
    /// Calculate SPFM (Single Point Fault Metric)
    pub fn spfm(&self) -> f64 {
        let dangerous = self.total_fit - self.safe_fit;
        if dangerous <= 0.0 {
            return 100.0;
        }
        (1.0 - self.spf_fit / dangerous) * 100.0
    }

    /// Calculate LFM (Latent Fault Metric)
    pub fn lfm(&self) -> f64 {
        let dangerous = self.total_fit - self.safe_fit;
        if dangerous <= 0.0 {
            return 100.0;
        }
        (1.0 - self.latent_fit / dangerous) * 100.0
    }
}

// ============================================================================
// FMEDA Library Manager
// ============================================================================

/// Manager for loading and accessing FMEDA libraries
#[derive(Debug, Clone, Default)]
pub struct FmedaLibraryManager {
    /// Loaded libraries by name
    libraries: HashMap<String, FmedaLibrary>,
    /// Search paths for library files
    search_paths: Vec<String>,
}

impl FmedaLibraryManager {
    /// Create a new library manager
    pub fn new() -> Self {
        Self {
            libraries: HashMap::new(),
            search_paths: Vec::new(),
        }
    }

    /// Add a search path for library files
    pub fn add_search_path(&mut self, path: &str) {
        self.search_paths.push(path.to_string());
    }

    /// Register a library
    pub fn register_library(&mut self, library: FmedaLibrary) {
        self.libraries.insert(library.name.clone(), library);
    }

    /// Get a library by name
    pub fn get_library(&self, name: &str) -> Option<&FmedaLibrary> {
        self.libraries.get(name)
    }

    /// Lookup a component across all libraries
    pub fn lookup_component(&self, library: &str, part_number: &str) -> Option<&LibraryComponent> {
        self.libraries
            .get(library)
            .and_then(|lib| lib.get_component(part_number))
    }

    /// Load built-in standard libraries
    pub fn load_standard_libraries(&mut self) {
        // Register automotive digital library
        self.register_library(create_automotive_digital_library());
        // Register automotive sensor library
        self.register_library(create_automotive_sensor_library());
        // Register digital logic library
        self.register_library(create_digital_logic_library());
    }
}

// ============================================================================
// Design Binding Types
// ============================================================================

/// Binding of library data to design component
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FmedaBinding {
    /// Design reference
    pub design_ref: DesignRef,
    /// Library name
    pub library: String,
    /// Part number in library
    pub part_number: String,
    /// Mechanisms applied to this component
    pub mechanisms: Vec<MechanismType>,
    /// Override values
    pub overrides: FmedaOverrides,
}

/// Override values for FMEDA binding
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct FmedaOverrides {
    /// Override base FIT
    pub base_fit: Option<f64>,
    /// Override DC for specific mechanisms
    pub mechanism_dc_overrides: HashMap<MechanismType, f64>,
    /// Override failure mode distributions
    pub mode_overrides: HashMap<String, FailureModeOverride>,
}

/// Override for a specific failure mode
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FailureModeOverride {
    /// Override distribution percentage
    pub distribution_pct: Option<f64>,
    /// Override failure class
    pub failure_class: Option<FailureClass>,
    /// Override DC
    pub dc: Option<f64>,
}

// ============================================================================
// FMEDA Analysis Results
// ============================================================================

/// Complete FMEDA analysis results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FmedaAnalysis {
    /// Component analyses
    pub components: Vec<ComponentFmedaResult>,
    /// Aggregated metrics
    pub summary: FmedaSummary,
}

/// FMEDA result for single component
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComponentFmedaResult {
    /// Design reference
    pub design_ref: DesignRef,
    /// Library component used
    pub part_number: String,
    /// FIT breakdown
    pub fit_breakdown: FitBreakdown,
    /// Individual failure mode results
    pub failure_modes: Vec<FailureModeResult>,
}

/// Result for individual failure mode
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FailureModeResult {
    /// Failure mode name
    pub name: String,
    /// FIT contribution
    pub fit: f64,
    /// Resulting failure class after mechanisms
    pub final_class: FailureClass,
    /// DC achieved
    pub dc_achieved: f64,
    /// Detecting mechanism(s)
    pub detectors: Vec<MechanismType>,
}

/// FMEDA summary metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FmedaSummary {
    /// Total system FIT
    pub total_fit: f64,
    /// Safe failure FIT
    pub safe_fit: f64,
    /// Single point fault FIT
    pub spf_fit: f64,
    /// Residual fault FIT
    pub residual_fit: f64,
    /// Latent fault FIT
    pub latent_fit: f64,
    /// System SPFM (percentage)
    pub spfm: f64,
    /// System LFM (percentage)
    pub lfm: f64,
    /// System PMHF (FIT)
    pub pmhf: f64,
    /// ASIL achievable based on metrics
    pub achievable_asil: AsilLevel,
}

impl FmedaSummary {
    /// Check if metrics meet target ASIL requirements
    pub fn meets_asil(&self, target: AsilLevel) -> bool {
        let requirements = target.requirements();

        if let Some(spfm_target) = requirements.spfm_target {
            if self.spfm < spfm_target {
                return false;
            }
        }

        if let Some(lf_target) = requirements.lf_target {
            if self.lfm < lf_target {
                return false;
            }
        }

        // Check PMHF
        let pmhf_target = match target {
            AsilLevel::QM => f64::INFINITY,
            AsilLevel::A => 1000.0,
            AsilLevel::B => 100.0,
            AsilLevel::C => 100.0,
            AsilLevel::D => 10.0,
        };

        self.pmhf <= pmhf_target
    }

    /// Get gaps for achieving target ASIL
    pub fn get_gaps(&self, target: AsilLevel) -> Vec<MetricGap> {
        let mut gaps = Vec::new();
        let requirements = target.requirements();

        if let Some(spfm_target) = requirements.spfm_target {
            if self.spfm < spfm_target {
                gaps.push(MetricGap {
                    metric: "SPFM".to_string(),
                    current: self.spfm,
                    target: spfm_target,
                    shortfall: spfm_target - self.spfm,
                });
            }
        }

        if let Some(lf_target) = requirements.lf_target {
            if self.lfm < lf_target {
                gaps.push(MetricGap {
                    metric: "LFM".to_string(),
                    current: self.lfm,
                    target: lf_target,
                    shortfall: lf_target - self.lfm,
                });
            }
        }

        let pmhf_target = match target {
            AsilLevel::QM => f64::INFINITY,
            AsilLevel::A => 1000.0,
            AsilLevel::B => 100.0,
            AsilLevel::C => 100.0,
            AsilLevel::D => 10.0,
        };

        if self.pmhf > pmhf_target {
            gaps.push(MetricGap {
                metric: "PMHF".to_string(),
                current: self.pmhf,
                target: pmhf_target,
                shortfall: self.pmhf - pmhf_target,
            });
        }

        gaps
    }
}

/// Gap in meeting a metric target
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MetricGap {
    /// Metric name
    pub metric: String,
    /// Current value
    pub current: f64,
    /// Target value
    pub target: f64,
    /// Shortfall
    pub shortfall: f64,
}

// ============================================================================
// FMEDA Calculator
// ============================================================================

/// FMEDA calculation engine
pub struct FmedaCalculator {
    /// Library manager
    library_manager: FmedaLibraryManager,
}

impl FmedaCalculator {
    /// Create a new calculator
    pub fn new(library_manager: FmedaLibraryManager) -> Self {
        Self { library_manager }
    }

    /// Calculate FMEDA for a set of bindings
    pub fn calculate(&self, bindings: &[FmedaBinding]) -> FmedaAnalysis {
        let mut components = Vec::new();
        let mut total_fit = 0.0;
        let mut safe_fit = 0.0;
        let mut spf_fit = 0.0;
        let mut residual_fit = 0.0;
        let mut latent_fit = 0.0;

        for binding in bindings {
            if let Some(result) = self.calculate_component(binding) {
                total_fit += result.fit_breakdown.total_fit;
                safe_fit += result.fit_breakdown.safe_fit;
                spf_fit += result.fit_breakdown.spf_fit;
                residual_fit += result.fit_breakdown.residual_fit;
                latent_fit += result.fit_breakdown.latent_fit;
                components.push(result);
            }
        }

        // Calculate system metrics
        let dangerous = total_fit - safe_fit;
        let spfm = if dangerous > 0.0 {
            (1.0 - spf_fit / dangerous) * 100.0
        } else {
            100.0
        };
        let lfm = if dangerous > 0.0 {
            (1.0 - latent_fit / dangerous) * 100.0
        } else {
            100.0
        };
        let pmhf = spf_fit + residual_fit; // PMHF = SPF + Residual (simplified)

        // Determine achievable ASIL
        let achievable_asil = if pmhf <= 10.0 && spfm >= 99.0 && lfm >= 90.0 {
            AsilLevel::D
        } else if pmhf <= 100.0 && spfm >= 97.0 && lfm >= 80.0 {
            AsilLevel::C
        } else if pmhf <= 100.0 && spfm >= 90.0 && lfm >= 80.0 {
            AsilLevel::B
        } else if pmhf <= 1000.0 && spfm >= 90.0 && lfm >= 60.0 {
            AsilLevel::A
        } else {
            AsilLevel::QM
        };

        FmedaAnalysis {
            components,
            summary: FmedaSummary {
                total_fit,
                safe_fit,
                spf_fit,
                residual_fit,
                latent_fit,
                spfm,
                lfm,
                pmhf,
                achievable_asil,
            },
        }
    }

    /// Calculate FMEDA for a single component
    fn calculate_component(&self, binding: &FmedaBinding) -> Option<ComponentFmedaResult> {
        let component = self
            .library_manager
            .lookup_component(&binding.library, &binding.part_number)?;

        let fit_breakdown = component.calculate_fit(&binding.mechanisms);

        let failure_modes = component
            .failure_modes
            .iter()
            .map(|mode| {
                let mode_fit = component.base_fit * mode.distribution_pct / 100.0;
                let dc = component.get_effective_dc(&binding.mechanisms);

                let final_class = if mode.default_class == FailureClass::SinglePointFault && dc > 0.0
                {
                    if dc >= 99.0 {
                        FailureClass::Safe
                    } else {
                        FailureClass::Residual
                    }
                } else {
                    mode.default_class
                };

                FailureModeResult {
                    name: mode.name.clone(),
                    fit: mode_fit,
                    final_class,
                    dc_achieved: dc,
                    detectors: binding.mechanisms.clone(),
                }
            })
            .collect();

        Some(ComponentFmedaResult {
            design_ref: binding.design_ref.clone(),
            part_number: binding.part_number.clone(),
            fit_breakdown,
            failure_modes,
        })
    }
}

// ============================================================================
// Standard Libraries
// ============================================================================

/// Create automotive digital component library
fn create_automotive_digital_library() -> FmedaLibrary {
    let mut library = FmedaLibrary::new("automotive_digital".to_string());
    library.description = "Automotive-grade digital components (AEC-Q100)".to_string();
    library.source = "IEC 62380 / SN 29500".to_string();

    // ARM Cortex-M processor
    let mut cortex_m = LibraryComponent::new(
        "ARM_CORTEX_M7".to_string(),
        "ARM Cortex-M7 Processor".to_string(),
        50.0,
    );
    cortex_m.category = ComponentCategory::Digital;
    cortex_m.quality_level = QualityLevel::Automotive;
    cortex_m.set_mechanism_coverage(MechanismType::Lockstep, 99.0);
    cortex_m.set_mechanism_coverage(MechanismType::Ecc, 99.5);
    cortex_m.add_failure_mode(LibraryFailureMode::new(
        "logic_upset".to_string(),
        60.0,
        FailureClass::SinglePointFault,
    ));
    cortex_m.add_failure_mode(LibraryFailureMode::new(
        "register_stuck".to_string(),
        30.0,
        FailureClass::Residual,
    ));
    cortex_m.add_failure_mode(LibraryFailureMode::new(
        "clock_failure".to_string(),
        10.0,
        FailureClass::Safe,
    ));
    library.add_component(cortex_m);

    // SRAM memory
    let mut sram = LibraryComponent::new(
        "SRAM_64KB".to_string(),
        "64KB SRAM Memory".to_string(),
        100.0,
    );
    sram.category = ComponentCategory::Memory;
    sram.quality_level = QualityLevel::Automotive;
    sram.set_mechanism_coverage(MechanismType::Ecc, 99.5);
    sram.set_mechanism_coverage(MechanismType::Bist, 90.0);
    sram.add_failure_mode(LibraryFailureMode::new(
        "bit_flip".to_string(),
        70.0,
        FailureClass::SinglePointFault,
    ));
    sram.add_failure_mode(LibraryFailureMode::new(
        "address_decode".to_string(),
        20.0,
        FailureClass::SinglePointFault,
    ));
    sram.add_failure_mode(LibraryFailureMode::new(
        "stuck_cell".to_string(),
        10.0,
        FailureClass::Latent,
    ));
    library.add_component(sram);

    // CRC checker
    let mut crc = LibraryComponent::new(
        "CRC8_CHECKER".to_string(),
        "CRC-8 Checker".to_string(),
        5.0,
    );
    crc.category = ComponentCategory::Digital;
    crc.quality_level = QualityLevel::Automotive;
    crc.add_failure_mode(LibraryFailureMode::new(
        "false_positive".to_string(),
        70.0,
        FailureClass::Safe,
    ));
    crc.add_failure_mode(LibraryFailureMode::new(
        "false_negative".to_string(),
        30.0,
        FailureClass::Residual,
    ));
    library.add_component(crc);

    // Watchdog timer
    let mut watchdog = LibraryComponent::new(
        "WATCHDOG_TIMER".to_string(),
        "Watchdog Timer".to_string(),
        10.0,
    );
    watchdog.category = ComponentCategory::Digital;
    watchdog.quality_level = QualityLevel::Automotive;
    watchdog.add_failure_mode(LibraryFailureMode::new(
        "timeout_stuck".to_string(),
        50.0,
        FailureClass::SinglePointFault,
    ));
    watchdog.add_failure_mode(LibraryFailureMode::new(
        "counter_stuck".to_string(),
        30.0,
        FailureClass::Latent,
    ));
    watchdog.add_failure_mode(LibraryFailureMode::new(
        "false_timeout".to_string(),
        20.0,
        FailureClass::Safe,
    ));
    library.add_component(watchdog);

    library
}

/// Create automotive sensor library
fn create_automotive_sensor_library() -> FmedaLibrary {
    let mut library = FmedaLibrary::new("automotive_sensors".to_string());
    library.description = "Automotive-grade sensors".to_string();
    library.source = "Manufacturer data".to_string();

    // Pressure sensor
    let mut pressure = LibraryComponent::new(
        "PRESSURE_SENSOR".to_string(),
        "Automotive Pressure Sensor".to_string(),
        25.0,
    );
    pressure.category = ComponentCategory::Sensor;
    pressure.quality_level = QualityLevel::Automotive;
    pressure.set_mechanism_coverage(MechanismType::Tmr, 99.5);
    pressure.add_failure_mode(LibraryFailureMode::new(
        "stuck_high".to_string(),
        25.0,
        FailureClass::SinglePointFault,
    ));
    pressure.add_failure_mode(LibraryFailureMode::new(
        "stuck_low".to_string(),
        25.0,
        FailureClass::SinglePointFault,
    ));
    pressure.add_failure_mode(LibraryFailureMode::new(
        "drift".to_string(),
        30.0,
        FailureClass::Latent,
    ));
    pressure.add_failure_mode(LibraryFailureMode::new(
        "open_circuit".to_string(),
        20.0,
        FailureClass::Safe,
    ));
    library.add_component(pressure);

    // Temperature sensor
    let mut temp = LibraryComponent::new(
        "TEMP_SENSOR".to_string(),
        "Temperature Sensor".to_string(),
        15.0,
    );
    temp.category = ComponentCategory::Sensor;
    temp.quality_level = QualityLevel::Automotive;
    temp.set_mechanism_coverage(MechanismType::Tmr, 99.0);
    temp.add_failure_mode(LibraryFailureMode::new(
        "stuck".to_string(),
        40.0,
        FailureClass::SinglePointFault,
    ));
    temp.add_failure_mode(LibraryFailureMode::new(
        "out_of_range".to_string(),
        40.0,
        FailureClass::Safe,
    ));
    temp.add_failure_mode(LibraryFailureMode::new(
        "drift".to_string(),
        20.0,
        FailureClass::Latent,
    ));
    library.add_component(temp);

    // ADC
    let mut adc = LibraryComponent::new(
        "ADC_12BIT".to_string(),
        "12-bit ADC".to_string(),
        20.0,
    );
    adc.category = ComponentCategory::MixedSignal;
    adc.quality_level = QualityLevel::Automotive;
    adc.add_failure_mode(LibraryFailureMode::new(
        "stuck_code".to_string(),
        30.0,
        FailureClass::SinglePointFault,
    ));
    adc.add_failure_mode(LibraryFailureMode::new(
        "offset".to_string(),
        40.0,
        FailureClass::Latent,
    ));
    adc.add_failure_mode(LibraryFailureMode::new(
        "missing_code".to_string(),
        30.0,
        FailureClass::Residual,
    ));
    library.add_component(adc);

    library
}

/// Create digital logic library
fn create_digital_logic_library() -> FmedaLibrary {
    let mut library = FmedaLibrary::new("digital_logic".to_string());
    library.description = "Standard digital logic components".to_string();
    library.source = "IEC 62380".to_string();

    // Register
    let mut reg = LibraryComponent::new(
        "REG_8BIT".to_string(),
        "8-bit Register".to_string(),
        2.0,
    );
    reg.category = ComponentCategory::Digital;
    reg.set_mechanism_coverage(MechanismType::Ecc, 99.0);
    reg.set_mechanism_coverage(MechanismType::Tmr, 99.5);
    reg.add_failure_mode(LibraryFailureMode::new(
        "stuck_at".to_string(),
        60.0,
        FailureClass::SinglePointFault,
    ));
    reg.add_failure_mode(LibraryFailureMode::new(
        "seu".to_string(),
        40.0,
        FailureClass::SinglePointFault,
    ));
    library.add_component(reg);

    // Comparator
    let mut comp = LibraryComponent::new(
        "COMPARATOR".to_string(),
        "Digital Comparator".to_string(),
        3.0,
    );
    comp.category = ComponentCategory::Digital;
    comp.add_failure_mode(LibraryFailureMode::new(
        "stuck_match".to_string(),
        50.0,
        FailureClass::SinglePointFault,
    ));
    comp.add_failure_mode(LibraryFailureMode::new(
        "stuck_mismatch".to_string(),
        50.0,
        FailureClass::Safe,
    ));
    library.add_component(comp);

    library
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_library_creation() {
        let library = create_automotive_digital_library();
        assert_eq!(library.name, "automotive_digital");
        assert!(library.get_component("ARM_CORTEX_M7").is_some());
        assert!(library.get_component("SRAM_64KB").is_some());
    }

    #[test]
    fn test_component_fit_calculation() {
        let library = create_automotive_digital_library();
        let component = library.get_component("ARM_CORTEX_M7").unwrap();

        // Without mechanisms
        let fit_no_mech = component.calculate_fit(&[]);
        assert_eq!(fit_no_mech.total_fit, 50.0);
        assert!(fit_no_mech.spf_fit > 0.0);

        // With lockstep
        let fit_with_lockstep = component.calculate_fit(&[MechanismType::Lockstep]);
        assert!(fit_with_lockstep.spf_fit < fit_no_mech.spf_fit);
    }

    #[test]
    fn test_spfm_calculation() {
        let fit = FitBreakdown {
            total_fit: 100.0,
            safe_fit: 10.0,
            spf_fit: 9.0,   // 10% of dangerous
            residual_fit: 0.0,
            latent_fit: 0.0,
            dc_applied: 90.0,
        };

        // SPFM = 1 - SPF/dangerous = 1 - 9/90 = 90%
        assert!((fit.spfm() - 90.0).abs() < 0.1);
    }

    #[test]
    fn test_lfm_calculation() {
        let fit = FitBreakdown {
            total_fit: 100.0,
            safe_fit: 10.0,
            spf_fit: 0.0,
            residual_fit: 0.0,
            latent_fit: 18.0,  // 20% of dangerous
            dc_applied: 0.0,
        };

        // LFM = 1 - latent/dangerous = 1 - 18/90 = 80%
        assert!((fit.lfm() - 80.0).abs() < 0.1);
    }

    #[test]
    fn test_library_manager() {
        let mut manager = FmedaLibraryManager::new();
        manager.load_standard_libraries();

        let component = manager.lookup_component("automotive_digital", "ARM_CORTEX_M7");
        assert!(component.is_some());

        let component = manager.lookup_component("automotive_sensors", "PRESSURE_SENSOR");
        assert!(component.is_some());
    }

    #[test]
    fn test_fmeda_calculator() {
        let mut manager = FmedaLibraryManager::new();
        manager.load_standard_libraries();

        let bindings = vec![
            FmedaBinding {
                design_ref: DesignRef::from_str("top.cpu"),
                library: "automotive_digital".to_string(),
                part_number: "ARM_CORTEX_M7".to_string(),
                mechanisms: vec![MechanismType::Lockstep],
                overrides: FmedaOverrides::default(),
            },
            FmedaBinding {
                design_ref: DesignRef::from_str("top.pressure"),
                library: "automotive_sensors".to_string(),
                part_number: "PRESSURE_SENSOR".to_string(),
                mechanisms: vec![MechanismType::Tmr],
                overrides: FmedaOverrides::default(),
            },
        ];

        let calculator = FmedaCalculator::new(manager);
        let analysis = calculator.calculate(&bindings);

        assert_eq!(analysis.components.len(), 2);
        assert!(analysis.summary.total_fit > 0.0);
        assert!(analysis.summary.spfm > 0.0);
        assert!(analysis.summary.lfm > 0.0);
    }

    #[test]
    fn test_asil_gap_detection() {
        let summary = FmedaSummary {
            total_fit: 100.0,
            safe_fit: 10.0,
            spf_fit: 5.0,
            residual_fit: 5.0,
            latent_fit: 20.0,
            spfm: 94.4,  // Below ASIL D target
            lfm: 77.8,   // Below ASIL D target
            pmhf: 10.0,
            achievable_asil: AsilLevel::B,
        };

        let gaps = summary.get_gaps(AsilLevel::D);
        assert!(!gaps.is_empty());
        assert!(gaps.iter().any(|g| g.metric == "SPFM"));
        assert!(gaps.iter().any(|g| g.metric == "LFM"));
    }

    #[test]
    fn test_effective_dc_combination() {
        let mut component = LibraryComponent::new(
            "TEST".to_string(),
            "Test Component".to_string(),
            100.0,
        );
        component.set_mechanism_coverage(MechanismType::Lockstep, 99.0);
        component.set_mechanism_coverage(MechanismType::Ecc, 99.0);

        // Combined DC = 1 - (1-0.99)(1-0.99) = 1 - 0.0001 = 99.99%
        let dc = component.get_effective_dc(&[MechanismType::Lockstep, MechanismType::Ecc]);
        assert!((dc - 99.99).abs() < 0.01);
    }
}
