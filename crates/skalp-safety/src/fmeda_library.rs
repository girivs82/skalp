//! FMEDA Library System for ISO 26262 Functional Safety
//!
//! Provides library-based component failure data for FMEDA calculations.
//! Components can specify base FIT rates, failure mode distributions,
//! and default diagnostic coverages for common safety mechanisms.

use crate::asil::AsilLevel;
use crate::hierarchy::{DesignRef, FailureClass, FailureMode, MechanismType, Severity};
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
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
    pub components: IndexMap<String, LibraryComponent>,
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
            components: IndexMap::new(),
            reference_temperature: 55.0, // Default junction temp
            metadata: LibraryMetadata::default(),
        }
    }

    /// Add a component to the library
    pub fn add_component(&mut self, component: LibraryComponent) {
        self.components
            .insert(component.part_number.clone(), component);
    }

    /// Get a component by part number
    pub fn get_component(&self, part_number: &str) -> Option<&LibraryComponent> {
        self.components.get(part_number)
    }

    /// Calculate total FIT for a component with mechanisms applied
    pub fn calculate_fit(
        &self,
        part_number: &str,
        mechanisms: &[MechanismType],
    ) -> Option<FitBreakdown> {
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
    pub mechanism_coverages: IndexMap<MechanismType, f64>,
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
            mechanism_coverages: IndexMap::new(),
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
    libraries: IndexMap<String, FmedaLibrary>,
    /// Search paths for library files
    search_paths: Vec<String>,
}

impl FmedaLibraryManager {
    /// Create a new library manager
    pub fn new() -> Self {
        Self {
            libraries: IndexMap::new(),
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
// Technology Primitive Library (Gate-level FIT data)
// ============================================================================

/// Technology library with primitive-level FIT data
/// Based on foundry/process data (gates, flops, muxes, etc.)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TechLibrary {
    /// Technology name (e.g., "tsmc7nm", "intel14nm")
    pub name: String,
    /// Process node (nm)
    pub process_node: u32,
    /// Library version
    pub version: String,
    /// Primitives in this library
    pub primitives: IndexMap<String, TechPrimitive>,
    /// Default temperature (°C)
    pub reference_temperature: f64,
    /// Voltage reference (V)
    pub reference_voltage: f64,
}

impl TechLibrary {
    /// Create a new technology library
    pub fn new(name: String, process_node: u32) -> Self {
        Self {
            name,
            process_node,
            version: "1.0.0".to_string(),
            primitives: IndexMap::new(),
            reference_temperature: 85.0, // Junction temp
            reference_voltage: 0.75,
        }
    }

    /// Add a primitive to the library
    pub fn add_primitive(&mut self, primitive: TechPrimitive) {
        self.primitives.insert(primitive.name.clone(), primitive);
    }

    /// Get a primitive by name
    pub fn get_primitive(&self, name: &str) -> Option<&TechPrimitive> {
        self.primitives.get(name)
    }
}

/// Technology primitive (gate, flop, etc.)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TechPrimitive {
    /// Primitive name (e.g., "DFF", "NAND2", "MUX2")
    pub name: String,
    /// Primitive type
    pub prim_type: PrimitiveType,
    /// Base FIT rate per instance
    pub base_fit: f64,
    /// Failure modes for this primitive
    pub failure_modes: Vec<PrimitiveFailureMode>,
    /// Area in um^2 (for density calculations)
    pub area: Option<f64>,
    /// Number of transistors
    pub transistor_count: Option<u32>,
}

impl TechPrimitive {
    /// Create a new primitive
    pub fn new(name: String, prim_type: PrimitiveType, base_fit: f64) -> Self {
        Self {
            name,
            prim_type,
            base_fit,
            failure_modes: Vec::new(),
            area: None,
            transistor_count: None,
        }
    }

    /// Add a failure mode
    pub fn add_failure_mode(&mut self, mode: PrimitiveFailureMode) {
        self.failure_modes.push(mode);
    }

    /// Get total FIT for this primitive
    pub fn total_fit(&self) -> f64 {
        self.failure_modes.iter().map(|m| m.fit).sum()
    }
}

/// Primitive types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum PrimitiveType {
    /// D Flip-Flop
    Dff,
    /// Latch
    Latch,
    /// Inverter
    Inv,
    /// NAND gate
    Nand,
    /// NOR gate
    Nor,
    /// AND gate
    And,
    /// OR gate
    Or,
    /// XOR gate
    Xor,
    /// Multiplexer
    Mux,
    /// Buffer
    Buf,
    /// Tri-state buffer
    Tribuf,
    /// Clock buffer
    ClkBuf,
    /// Clock gate
    ClkGate,
    /// Memory bit cell
    MemCell,
    /// Custom/other
    Custom,
}

/// Failure mode for a primitive
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PrimitiveFailureMode {
    /// Failure mode name
    pub name: String,
    /// FIT rate for this mode
    pub fit: f64,
    /// Failure class
    pub class: FailureClass,
    /// Description
    pub description: String,
}

impl PrimitiveFailureMode {
    /// Create a new primitive failure mode
    pub fn new(name: String, fit: f64, class: FailureClass) -> Self {
        Self {
            name,
            fit,
            class,
            description: String::new(),
        }
    }

    /// Set description
    pub fn with_description(mut self, desc: String) -> Self {
        self.description = desc;
        self
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

/// Primitive-level binding (maps design instance to tech primitive)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PrimitiveBinding {
    /// Design hierarchy path to the primitive instance
    pub design_ref: DesignRef,
    /// Technology library name
    pub tech_library: String,
    /// Primitive type name in library
    pub primitive_name: String,
    /// Instance count (for arrays/vectors)
    pub instance_count: u32,
    /// Mechanisms covering this primitive
    pub mechanisms: Vec<MechanismType>,
    /// DC overrides per mechanism
    pub dc_overrides: IndexMap<MechanismType, f64>,
}

// ============================================================================
// Multi-Contributor Failure Mode (Primitive-Level)
// ============================================================================

/// Multi-contributor failure mode referencing primitive instances
/// Used when a failure mode has contributions from multiple primitives
/// in the design hierarchy
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MultiContributorFailureMode {
    /// Unique identifier
    pub id: String,
    /// Failure mode name
    pub name: String,
    /// Total FIT rate for this failure mode
    pub total_fit: f64,
    /// Severity
    pub severity: Severity,
    /// Failure class
    pub class: FailureClass,
    /// Contributors (primitive instances with weights)
    pub contributors: Vec<PrimitiveContributor>,
    /// Detecting mechanism
    pub detector: Option<DetectorBinding>,
    /// Description
    pub description: String,
}

/// A primitive instance contributing to a failure mode
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PrimitiveContributor {
    /// Design hierarchy path to the primitive instance
    pub design_ref: DesignRef,
    /// Contribution weight (0.0 to 1.0)
    pub weight: f64,
    /// Description of how this primitive contributes
    pub contribution_desc: Option<String>,
}

impl PrimitiveContributor {
    /// Create a new primitive contributor
    pub fn new(design_ref: DesignRef, weight: f64) -> Self {
        Self {
            design_ref,
            weight,
            contribution_desc: None,
        }
    }

    /// Add description
    pub fn with_description(mut self, desc: String) -> Self {
        self.contribution_desc = Some(desc);
        self
    }

    /// Get FIT contribution given total FIT
    pub fn get_fit(&self, total_fit: f64) -> f64 {
        total_fit * self.weight
    }
}

/// Detector binding for a failure mode
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DetectorBinding {
    /// Mechanism type (PSM or LSM)
    pub mechanism_type: DetectorType,
    /// Mechanism name
    pub mechanism_name: String,
    /// Achieved diagnostic coverage
    pub dc: f64,
}

/// Type of detector
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum DetectorType {
    /// Primary Safety Mechanism
    Psm,
    /// Latent Safety Mechanism
    Lsm,
}

impl MultiContributorFailureMode {
    /// Create a new multi-contributor failure mode
    pub fn new(
        id: String,
        name: String,
        total_fit: f64,
        severity: Severity,
        class: FailureClass,
    ) -> Self {
        Self {
            id,
            name,
            total_fit,
            severity,
            class,
            contributors: Vec::new(),
            detector: None,
            description: String::new(),
        }
    }

    /// Add a contributor
    pub fn add_contributor(&mut self, contributor: PrimitiveContributor) {
        self.contributors.push(contributor);
    }

    /// Set contributors
    pub fn with_contributors(mut self, contributors: Vec<PrimitiveContributor>) -> Self {
        self.contributors = contributors;
        self
    }

    /// Set detector
    pub fn with_detector(mut self, detector: DetectorBinding) -> Self {
        self.detector = Some(detector);
        self
    }

    /// Set description
    pub fn with_description(mut self, desc: String) -> Self {
        self.description = desc;
        self
    }

    /// Validate contributor weights sum to 1.0
    pub fn validate(&self) -> Result<(), String> {
        if self.contributors.is_empty() {
            return Err("Multi-contributor failure mode must have contributors".to_string());
        }

        let total_weight: f64 = self.contributors.iter().map(|c| c.weight).sum();
        if (total_weight - 1.0).abs() > 0.01 {
            return Err(format!(
                "Contributor weights sum to {:.2}, expected 1.0",
                total_weight
            ));
        }

        for c in &self.contributors {
            if c.weight < 0.0 {
                return Err(format!(
                    "Negative weight {} for contributor {}",
                    c.weight, c.design_ref
                ));
            }
        }

        Ok(())
    }

    /// Get FIT contribution for a specific primitive
    pub fn get_fit_for_primitive(&self, design_ref: &DesignRef) -> Option<f64> {
        self.contributors
            .iter()
            .find(|c| &c.design_ref == design_ref)
            .map(|c| c.get_fit(self.total_fit))
    }

    /// Get all primitives involved
    pub fn get_involved_primitives(&self) -> Vec<&DesignRef> {
        self.contributors.iter().map(|c| &c.design_ref).collect()
    }
}

// ============================================================================
// FMEDA with Primitive-Level Bindings
// ============================================================================

/// Complete FMEDA specification with primitive-level data
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PrimitiveFmeda {
    /// Design name
    pub design_name: String,
    /// Technology library used
    pub tech_library: String,
    /// Primitive bindings (single-primitive failure modes)
    pub primitive_bindings: Vec<PrimitiveBinding>,
    /// Multi-contributor failure modes (cross-primitive)
    pub multi_contributor_modes: Vec<MultiContributorFailureMode>,
}

impl PrimitiveFmeda {
    /// Create a new primitive FMEDA
    pub fn new(design_name: String, tech_library: String) -> Self {
        Self {
            design_name,
            tech_library,
            primitive_bindings: Vec::new(),
            multi_contributor_modes: Vec::new(),
        }
    }

    /// Add a primitive binding
    pub fn add_primitive_binding(&mut self, binding: PrimitiveBinding) {
        self.primitive_bindings.push(binding);
    }

    /// Add a multi-contributor failure mode
    pub fn add_multi_contributor_mode(&mut self, mode: MultiContributorFailureMode) {
        self.multi_contributor_modes.push(mode);
    }

    /// Calculate total FIT for a specific primitive instance
    /// Includes both single-primitive and multi-contributor modes
    pub fn calculate_primitive_fit(&self, design_ref: &DesignRef, tech_lib: &TechLibrary) -> f64 {
        let mut total_fit = 0.0;

        // FIT from primitive bindings
        for binding in &self.primitive_bindings {
            if &binding.design_ref == design_ref {
                if let Some(prim) = tech_lib.get_primitive(&binding.primitive_name) {
                    total_fit += prim.base_fit * binding.instance_count as f64;
                }
            }
        }

        // FIT from multi-contributor modes
        for mode in &self.multi_contributor_modes {
            if let Some(fit) = mode.get_fit_for_primitive(design_ref) {
                total_fit += fit;
            }
        }

        total_fit
    }

    /// Calculate total system FIT
    pub fn calculate_total_fit(&self, tech_lib: &TechLibrary) -> f64 {
        let mut total_fit = 0.0;

        // Sum primitive bindings
        for binding in &self.primitive_bindings {
            if let Some(prim) = tech_lib.get_primitive(&binding.primitive_name) {
                total_fit += prim.base_fit * binding.instance_count as f64;
            }
        }

        // Sum multi-contributor modes (total, not per-contributor)
        for mode in &self.multi_contributor_modes {
            total_fit += mode.total_fit;
        }

        total_fit
    }

    /// Validate all multi-contributor modes
    pub fn validate(&self) -> Result<(), Vec<String>> {
        let mut errors = Vec::new();

        for mode in &self.multi_contributor_modes {
            if let Err(e) = mode.validate() {
                errors.push(format!("Mode '{}': {}", mode.id, e));
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Get all failure modes affecting a specific primitive
    pub fn get_modes_for_primitive(
        &self,
        design_ref: &DesignRef,
    ) -> Vec<&MultiContributorFailureMode> {
        self.multi_contributor_modes
            .iter()
            .filter(|m| m.get_involved_primitives().contains(&design_ref))
            .collect()
    }
}

/// Override values for FMEDA binding
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct FmedaOverrides {
    /// Override base FIT
    pub base_fit: Option<f64>,
    /// Override DC for specific mechanisms
    pub mechanism_dc_overrides: IndexMap<MechanismType, f64>,
    /// Override failure mode distributions
    pub mode_overrides: IndexMap<String, FailureModeOverride>,
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

                let final_class =
                    if mode.default_class == FailureClass::SinglePointFault && dc > 0.0 {
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
    let mut crc =
        LibraryComponent::new("CRC8_CHECKER".to_string(), "CRC-8 Checker".to_string(), 5.0);
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
    let mut adc = LibraryComponent::new("ADC_12BIT".to_string(), "12-bit ADC".to_string(), 20.0);
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
    let mut reg = LibraryComponent::new("REG_8BIT".to_string(), "8-bit Register".to_string(), 2.0);
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

/// Create a standard technology primitive library (gate-level)
pub fn create_tech_primitive_library() -> TechLibrary {
    let mut library = TechLibrary::new("tech_standard".to_string(), 7);
    library.version = "1.0.0".to_string();

    // D Flip-Flop
    let mut dff = TechPrimitive::new("DFF".to_string(), PrimitiveType::Dff, 0.5);
    dff.area = Some(2.0);
    dff.transistor_count = Some(20);
    dff.add_failure_mode(
        PrimitiveFailureMode::new(
            "stuck_at_0".to_string(),
            0.2,
            FailureClass::SinglePointFault,
        )
        .with_description("Output stuck at logic 0".to_string()),
    );
    dff.add_failure_mode(
        PrimitiveFailureMode::new(
            "stuck_at_1".to_string(),
            0.2,
            FailureClass::SinglePointFault,
        )
        .with_description("Output stuck at logic 1".to_string()),
    );
    dff.add_failure_mode(
        PrimitiveFailureMode::new("setup_violation".to_string(), 0.05, FailureClass::Residual)
            .with_description("Setup time violation".to_string()),
    );
    dff.add_failure_mode(
        PrimitiveFailureMode::new("hold_violation".to_string(), 0.05, FailureClass::Residual)
            .with_description("Hold time violation".to_string()),
    );
    library.add_primitive(dff);

    // Latch
    let mut latch = TechPrimitive::new("LATCH".to_string(), PrimitiveType::Latch, 0.4);
    latch.area = Some(1.5);
    latch.transistor_count = Some(12);
    latch.add_failure_mode(PrimitiveFailureMode::new(
        "stuck_at_0".to_string(),
        0.15,
        FailureClass::SinglePointFault,
    ));
    latch.add_failure_mode(PrimitiveFailureMode::new(
        "stuck_at_1".to_string(),
        0.15,
        FailureClass::SinglePointFault,
    ));
    latch.add_failure_mode(PrimitiveFailureMode::new(
        "transparent_stuck".to_string(),
        0.1,
        FailureClass::Residual,
    ));
    library.add_primitive(latch);

    // NAND2 gate
    let mut nand2 = TechPrimitive::new("NAND2".to_string(), PrimitiveType::Nand, 0.1);
    nand2.area = Some(0.5);
    nand2.transistor_count = Some(4);
    nand2.add_failure_mode(PrimitiveFailureMode::new(
        "stuck_at_0".to_string(),
        0.05,
        FailureClass::SinglePointFault,
    ));
    nand2.add_failure_mode(PrimitiveFailureMode::new(
        "stuck_at_1".to_string(),
        0.05,
        FailureClass::SinglePointFault,
    ));
    library.add_primitive(nand2);

    // NOR2 gate
    let mut nor2 = TechPrimitive::new("NOR2".to_string(), PrimitiveType::Nor, 0.1);
    nor2.area = Some(0.5);
    nor2.transistor_count = Some(4);
    nor2.add_failure_mode(PrimitiveFailureMode::new(
        "stuck_at_0".to_string(),
        0.05,
        FailureClass::SinglePointFault,
    ));
    nor2.add_failure_mode(PrimitiveFailureMode::new(
        "stuck_at_1".to_string(),
        0.05,
        FailureClass::SinglePointFault,
    ));
    library.add_primitive(nor2);

    // Inverter
    let mut inv = TechPrimitive::new("INV".to_string(), PrimitiveType::Inv, 0.05);
    inv.area = Some(0.25);
    inv.transistor_count = Some(2);
    inv.add_failure_mode(PrimitiveFailureMode::new(
        "stuck_at_0".to_string(),
        0.025,
        FailureClass::SinglePointFault,
    ));
    inv.add_failure_mode(PrimitiveFailureMode::new(
        "stuck_at_1".to_string(),
        0.025,
        FailureClass::SinglePointFault,
    ));
    library.add_primitive(inv);

    // 2-to-1 Multiplexer
    let mut mux2 = TechPrimitive::new("MUX2".to_string(), PrimitiveType::Mux, 0.15);
    mux2.area = Some(1.0);
    mux2.transistor_count = Some(8);
    mux2.add_failure_mode(
        PrimitiveFailureMode::new(
            "select_stuck_0".to_string(),
            0.05,
            FailureClass::SinglePointFault,
        )
        .with_description("Select line stuck, always selects input 0".to_string()),
    );
    mux2.add_failure_mode(
        PrimitiveFailureMode::new(
            "select_stuck_1".to_string(),
            0.05,
            FailureClass::SinglePointFault,
        )
        .with_description("Select line stuck, always selects input 1".to_string()),
    );
    mux2.add_failure_mode(PrimitiveFailureMode::new(
        "output_stuck".to_string(),
        0.05,
        FailureClass::SinglePointFault,
    ));
    library.add_primitive(mux2);

    // Clock buffer
    let mut clkbuf = TechPrimitive::new("CLKBUF".to_string(), PrimitiveType::ClkBuf, 0.2);
    clkbuf.area = Some(1.5);
    clkbuf.transistor_count = Some(8);
    clkbuf.add_failure_mode(
        PrimitiveFailureMode::new("stuck_low".to_string(), 0.1, FailureClass::SinglePointFault)
            .with_description("Clock output stuck low".to_string()),
    );
    clkbuf.add_failure_mode(
        PrimitiveFailureMode::new(
            "stuck_high".to_string(),
            0.05,
            FailureClass::SinglePointFault,
        )
        .with_description("Clock output stuck high".to_string()),
    );
    clkbuf.add_failure_mode(
        PrimitiveFailureMode::new("jitter".to_string(), 0.05, FailureClass::Residual)
            .with_description("Excessive clock jitter".to_string()),
    );
    library.add_primitive(clkbuf);

    // Memory bit cell
    let mut memcell = TechPrimitive::new("MEMCELL".to_string(), PrimitiveType::MemCell, 0.001);
    memcell.area = Some(0.1);
    memcell.transistor_count = Some(6);
    memcell.add_failure_mode(PrimitiveFailureMode::new(
        "stuck_at_0".to_string(),
        0.0004,
        FailureClass::SinglePointFault,
    ));
    memcell.add_failure_mode(PrimitiveFailureMode::new(
        "stuck_at_1".to_string(),
        0.0004,
        FailureClass::SinglePointFault,
    ));
    memcell.add_failure_mode(
        PrimitiveFailureMode::new("seu".to_string(), 0.0002, FailureClass::SinglePointFault)
            .with_description("Single event upset (soft error)".to_string()),
    );
    library.add_primitive(memcell);

    library
}

// ============================================================================
// Technology Derating Model (ISO 26262 Compliant)
// ============================================================================

/// Boltzmann constant in eV/K
const BOLTZMANN_EV: f64 = 8.617e-5;

/// Operating conditions for FIT derating
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OperatingConditions {
    /// Junction temperature in °C
    pub temperature_c: f64,
    /// Supply voltage in V
    pub voltage: f64,
    /// Voltage specification (nominal) in V
    pub voltage_nominal: f64,
}

impl Default for OperatingConditions {
    fn default() -> Self {
        Self {
            temperature_c: 85.0, // Typical automotive junction temp
            voltage: 0.9,
            voltage_nominal: 0.9,
        }
    }
}

/// Technology derating parameters
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeratingParameters {
    /// Activation energy in eV (typical 0.3-0.7 for semiconductors)
    pub activation_energy_ev: f64,
    /// Reference temperature in °C (FIT data temperature)
    pub reference_temperature_c: f64,
    /// Voltage acceleration exponent (typically 2-4)
    pub voltage_exponent: f64,
    /// Process corner: -3σ to +3σ (0.0 = typical)
    pub process_sigma: f64,
    /// Process variation coefficient (σ/mean for FIT)
    pub process_cv: f64,
}

impl Default for DeratingParameters {
    fn default() -> Self {
        Self {
            activation_energy_ev: 0.7, // Typical for oxide breakdown
            reference_temperature_c: 55.0,
            voltage_exponent: 2.5,
            process_sigma: 0.0, // Typical corner
            process_cv: 0.15,   // 15% coefficient of variation
        }
    }
}

impl DeratingParameters {
    /// Create parameters for CMOS gate oxide failures
    /// Activation energy ~0.7 eV, high voltage sensitivity
    pub fn gate_oxide() -> Self {
        Self {
            activation_energy_ev: 0.7,
            reference_temperature_c: 55.0,
            voltage_exponent: 3.0,
            process_sigma: 0.0,
            process_cv: 0.20,
        }
    }

    /// Create parameters for electromigration failures
    /// Activation energy ~0.5-0.9 eV
    pub fn electromigration() -> Self {
        Self {
            activation_energy_ev: 0.6,
            reference_temperature_c: 55.0,
            voltage_exponent: 2.0, // Current density driven
            process_sigma: 0.0,
            process_cv: 0.25,
        }
    }

    /// Create parameters for hot carrier injection
    /// Activation energy ~0.1-0.3 eV (negative temp dependence)
    pub fn hot_carrier() -> Self {
        Self {
            activation_energy_ev: -0.2, // Negative = increases at low temp
            reference_temperature_c: 55.0,
            voltage_exponent: 4.0, // High voltage sensitivity
            process_sigma: 0.0,
            process_cv: 0.30,
        }
    }

    /// Create parameters for soft errors (SEU)
    /// Minimal temperature dependence
    pub fn soft_error() -> Self {
        Self {
            activation_energy_ev: 0.0, // No Arrhenius dependence
            reference_temperature_c: 55.0,
            voltage_exponent: 0.0, // No voltage dependence
            process_sigma: 0.0,
            process_cv: 0.50, // High variation due to environment
        }
    }

    /// Create worst-case parameters (+3σ)
    pub fn worst_case() -> Self {
        Self {
            activation_energy_ev: 0.7,
            reference_temperature_c: 55.0,
            voltage_exponent: 2.5,
            process_sigma: 3.0, // +3σ
            process_cv: 0.15,
        }
    }

    /// Create best-case parameters (-3σ)
    pub fn best_case() -> Self {
        Self {
            activation_energy_ev: 0.7,
            reference_temperature_c: 55.0,
            voltage_exponent: 2.5,
            process_sigma: -3.0, // -3σ
            process_cv: 0.15,
        }
    }
}

/// Technology derating calculator
#[derive(Debug, Clone)]
pub struct DeratingCalculator {
    /// Derating parameters
    pub params: DeratingParameters,
    /// Operating conditions
    pub conditions: OperatingConditions,
}

impl DeratingCalculator {
    /// Create a new derating calculator
    pub fn new(params: DeratingParameters, conditions: OperatingConditions) -> Self {
        Self { params, conditions }
    }

    /// Calculate the Arrhenius temperature acceleration factor
    ///
    /// AF_temp = exp(Ea/k * (1/T_ref - 1/T_use))
    ///
    /// Where:
    /// - Ea = Activation energy (eV)
    /// - k = Boltzmann constant (8.617×10⁻⁵ eV/K)
    /// - T_ref = Reference temperature (K)
    /// - T_use = Operating temperature (K)
    pub fn arrhenius_factor(&self) -> f64 {
        let t_ref_k = self.params.reference_temperature_c + 273.15;
        let t_use_k = self.conditions.temperature_c + 273.15;

        let exponent =
            self.params.activation_energy_ev / BOLTZMANN_EV * (1.0 / t_ref_k - 1.0 / t_use_k);

        exponent.exp()
    }

    /// Calculate voltage stress factor
    ///
    /// AF_voltage = (V_use / V_nom)^n
    ///
    /// Where n is the voltage acceleration exponent
    pub fn voltage_factor(&self) -> f64 {
        if self.conditions.voltage_nominal <= 0.0 {
            return 1.0;
        }

        let voltage_ratio = self.conditions.voltage / self.conditions.voltage_nominal;
        voltage_ratio.powf(self.params.voltage_exponent)
    }

    /// Calculate process corner factor
    ///
    /// AF_process = 1 + σ * CV
    ///
    /// Where:
    /// - σ = Process corner (-3 to +3)
    /// - CV = Coefficient of variation
    pub fn process_factor(&self) -> f64 {
        1.0 + self.params.process_sigma * self.params.process_cv
    }

    /// Calculate combined acceleration factor
    ///
    /// AF_total = AF_temp × AF_voltage × AF_process
    pub fn total_acceleration_factor(&self) -> f64 {
        self.arrhenius_factor() * self.voltage_factor() * self.process_factor()
    }

    /// Apply derating to a base FIT value
    pub fn derate_fit(&self, base_fit: f64) -> f64 {
        base_fit * self.total_acceleration_factor()
    }

    /// Calculate FIT bounds for uncertainty analysis
    ///
    /// Returns (typical, worst_case, best_case) FIT values
    pub fn fit_bounds(&self, base_fit: f64) -> FitBounds {
        // Typical (current corner)
        let typical = self.derate_fit(base_fit);

        // Worst case (+3σ, max temp in range)
        let mut worst_calc = self.clone();
        worst_calc.params.process_sigma = 3.0;
        let worst_case = worst_calc.derate_fit(base_fit);

        // Best case (-3σ)
        let mut best_calc = self.clone();
        best_calc.params.process_sigma = -3.0;
        let best_case = best_calc.derate_fit(base_fit).max(0.0); // Clamp to non-negative

        FitBounds {
            typical,
            worst_case,
            best_case,
            acceleration_factor: self.total_acceleration_factor(),
        }
    }
}

/// FIT bounds from derating analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FitBounds {
    /// Typical (nominal) FIT value
    pub typical: f64,
    /// Worst-case FIT (+3σ, high temp)
    pub worst_case: f64,
    /// Best-case FIT (-3σ)
    pub best_case: f64,
    /// Total acceleration factor applied
    pub acceleration_factor: f64,
}

impl FitBounds {
    /// Calculate the uncertainty range as a percentage
    pub fn uncertainty_percent(&self) -> f64 {
        if self.typical > 0.0 {
            (self.worst_case - self.best_case) / self.typical * 100.0
        } else {
            0.0
        }
    }

    /// Get the 90% confidence upper bound (approximation)
    pub fn upper_90_percent(&self) -> f64 {
        // Assuming normal distribution, 90% is ~1.645σ
        self.typical + (self.worst_case - self.typical) * 1.645 / 3.0
    }
}

/// Apply derating to a technology library
pub fn derate_tech_library(
    library: &TechLibrary,
    operating_temp_c: f64,
    operating_voltage: f64,
) -> IndexMap<String, FitBounds> {
    let conditions = OperatingConditions {
        temperature_c: operating_temp_c,
        voltage: operating_voltage,
        voltage_nominal: library.reference_voltage,
    };

    let params = DeratingParameters {
        reference_temperature_c: library.reference_temperature,
        ..Default::default()
    };

    let calculator = DeratingCalculator::new(params, conditions);

    library
        .primitives
        .iter()
        .map(|(name, prim)| {
            let bounds = calculator.fit_bounds(prim.base_fit);
            (name.clone(), bounds)
        })
        .collect()
}

/// Apply derating to a component library
pub fn derate_component_library(
    library: &FmedaLibrary,
    operating_temp_c: f64,
) -> IndexMap<String, FitBounds> {
    let conditions = OperatingConditions {
        temperature_c: operating_temp_c,
        voltage: 1.0, // Assume nominal
        voltage_nominal: 1.0,
    };

    let params = DeratingParameters {
        reference_temperature_c: library.reference_temperature,
        ..Default::default()
    };

    let calculator = DeratingCalculator::new(params, conditions);

    library
        .components
        .iter()
        .map(|(name, comp)| {
            let bounds = calculator.fit_bounds(comp.base_fit);
            (name.clone(), bounds)
        })
        .collect()
}

/// Temperature sensitivity analysis
///
/// Calculates FIT at multiple temperature points for plotting
pub fn temperature_sensitivity(
    base_fit: f64,
    params: &DeratingParameters,
    temp_range: std::ops::Range<f64>,
    num_points: usize,
) -> Vec<(f64, f64)> {
    let step = (temp_range.end - temp_range.start) / (num_points - 1) as f64;

    (0..num_points)
        .map(|i| {
            let temp = temp_range.start + step * i as f64;
            let conditions = OperatingConditions {
                temperature_c: temp,
                ..Default::default()
            };
            let calc = DeratingCalculator::new(params.clone(), conditions);
            (temp, calc.derate_fit(base_fit))
        })
        .collect()
}

/// Generate derating report
pub fn format_derating_report(
    base_fit: f64,
    params: &DeratingParameters,
    conditions: &OperatingConditions,
) -> String {
    let calc = DeratingCalculator::new(params.clone(), conditions.clone());
    let bounds = calc.fit_bounds(base_fit);

    let mut report = String::new();
    report.push_str("=== Technology Derating Report ===\n\n");

    report.push_str("Operating Conditions:\n");
    report.push_str(&format!(
        "  Temperature: {:.1}°C (ref: {:.1}°C)\n",
        conditions.temperature_c, params.reference_temperature_c
    ));
    report.push_str(&format!(
        "  Voltage: {:.3}V (nom: {:.3}V)\n",
        conditions.voltage, conditions.voltage_nominal
    ));
    report.push_str(&format!(
        "  Process Corner: {:.1}σ\n\n",
        params.process_sigma
    ));

    report.push_str("Derating Parameters:\n");
    report.push_str(&format!(
        "  Activation Energy: {:.2} eV\n",
        params.activation_energy_ev
    ));
    report.push_str(&format!(
        "  Voltage Exponent: {:.1}\n",
        params.voltage_exponent
    ));
    report.push_str(&format!(
        "  Process CV: {:.1}%\n\n",
        params.process_cv * 100.0
    ));

    report.push_str("Acceleration Factors:\n");
    report.push_str(&format!(
        "  Arrhenius (temp): {:.3}x\n",
        calc.arrhenius_factor()
    ));
    report.push_str(&format!(
        "  Voltage stress: {:.3}x\n",
        calc.voltage_factor()
    ));
    report.push_str(&format!(
        "  Process corner: {:.3}x\n",
        calc.process_factor()
    ));
    report.push_str(&format!(
        "  Total: {:.3}x\n\n",
        calc.total_acceleration_factor()
    ));

    report.push_str("FIT Values:\n");
    report.push_str(&format!("  Base FIT: {:.2}\n", base_fit));
    report.push_str(&format!("  Derated FIT (typical): {:.2}\n", bounds.typical));
    report.push_str(&format!("  Worst case (+3σ): {:.2}\n", bounds.worst_case));
    report.push_str(&format!("  Best case (-3σ): {:.2}\n", bounds.best_case));
    report.push_str(&format!(
        "  Uncertainty: ±{:.1}%\n",
        bounds.uncertainty_percent() / 2.0
    ));

    report
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
            spf_fit: 9.0, // 10% of dangerous
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
            latent_fit: 18.0, // 20% of dangerous
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
                design_ref: DesignRef::parse("top.cpu"),
                library: "automotive_digital".to_string(),
                part_number: "ARM_CORTEX_M7".to_string(),
                mechanisms: vec![MechanismType::Lockstep],
                overrides: FmedaOverrides::default(),
            },
            FmedaBinding {
                design_ref: DesignRef::parse("top.pressure"),
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
            spfm: 94.4, // Below ASIL D target
            lfm: 77.8,  // Below ASIL D target
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
        let mut component =
            LibraryComponent::new("TEST".to_string(), "Test Component".to_string(), 100.0);
        component.set_mechanism_coverage(MechanismType::Lockstep, 99.0);
        component.set_mechanism_coverage(MechanismType::Ecc, 99.0);

        // Combined DC = 1 - (1-0.99)(1-0.99) = 1 - 0.0001 = 99.99%
        let dc = component.get_effective_dc(&[MechanismType::Lockstep, MechanismType::Ecc]);
        assert!((dc - 99.99).abs() < 0.01);
    }

    // =========================================================================
    // Technology Primitive Library Tests
    // =========================================================================

    #[test]
    fn test_tech_library_creation() {
        let library = create_tech_primitive_library();
        assert_eq!(library.name, "tech_standard");
        assert_eq!(library.process_node, 7);
        assert!(library.get_primitive("DFF").is_some());
        assert!(library.get_primitive("NAND2").is_some());
        assert!(library.get_primitive("MUX2").is_some());
    }

    #[test]
    fn test_tech_primitive_fit() {
        let library = create_tech_primitive_library();
        let dff = library.get_primitive("DFF").unwrap();

        assert_eq!(dff.base_fit, 0.5);
        assert_eq!(dff.prim_type, PrimitiveType::Dff);

        // Sum of failure mode FITs should equal base_fit
        let total: f64 = dff.failure_modes.iter().map(|m| m.fit).sum();
        assert!((total - dff.base_fit).abs() < 0.001);
    }

    #[test]
    fn test_primitive_binding() {
        let binding = PrimitiveBinding {
            design_ref: DesignRef::parse("top.voter.ff_a_reg"),
            tech_library: "tech_standard".to_string(),
            primitive_name: "DFF".to_string(),
            instance_count: 1,
            mechanisms: vec![MechanismType::Tmr],
            dc_overrides: IndexMap::new(),
        };

        assert_eq!(binding.instance_count, 1);
        assert_eq!(binding.primitive_name, "DFF");
    }

    // =========================================================================
    // Multi-Contributor Failure Mode Tests (Primitive-Level)
    // =========================================================================

    #[test]
    fn test_multi_contributor_mode_creation() {
        let mode = MultiContributorFailureMode::new(
            "MCF-001".to_string(),
            "timing_path_violation".to_string(),
            1.0,
            Severity::S2,
            FailureClass::SinglePointFault,
        )
        .with_contributors(vec![
            PrimitiveContributor::new(DesignRef::parse("top.voter.ff_a_reg"), 0.3)
                .with_description("Source flop".to_string()),
            PrimitiveContributor::new(DesignRef::parse("top.voter.comb.u1"), 0.2)
                .with_description("NAND in path".to_string()),
            PrimitiveContributor::new(DesignRef::parse("top.voter.comb.u2"), 0.2)
                .with_description("Another gate".to_string()),
            PrimitiveContributor::new(DesignRef::parse("top.voter.mux_sel"), 0.15)
                .with_description("Mux in path".to_string()),
            PrimitiveContributor::new(DesignRef::parse("top.voter.ff_out_reg"), 0.15)
                .with_description("Dest flop".to_string()),
        ])
        .with_detector(DetectorBinding {
            mechanism_type: DetectorType::Psm,
            mechanism_name: "TimingMonitor".to_string(),
            dc: 95.0,
        })
        .with_description("Timing path failure across multiple primitives".to_string());

        assert!(mode.validate().is_ok());
        assert_eq!(mode.contributors.len(), 5);

        // Check FIT contribution
        let ff_a_ref = DesignRef::parse("top.voter.ff_a_reg");
        assert_eq!(mode.get_fit_for_primitive(&ff_a_ref), Some(0.3));
    }

    #[test]
    fn test_multi_contributor_validation_error() {
        // Weights don't sum to 1.0
        let mode = MultiContributorFailureMode::new(
            "MCF-BAD".to_string(),
            "bad_mode".to_string(),
            1.0,
            Severity::S1,
            FailureClass::Safe,
        )
        .with_contributors(vec![
            PrimitiveContributor::new(DesignRef::parse("top.a"), 0.5),
            PrimitiveContributor::new(DesignRef::parse("top.b"), 0.3),
        ]);

        let result = mode.validate();
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("sum to 0.80"));
    }

    #[test]
    fn test_primitive_fmeda() {
        let tech_lib = create_tech_primitive_library();
        let mut fmeda = PrimitiveFmeda::new("top.voter".to_string(), "tech_standard".to_string());

        // Add primitive bindings
        fmeda.add_primitive_binding(PrimitiveBinding {
            design_ref: DesignRef::parse("top.voter.ff_a_reg"),
            tech_library: "tech_standard".to_string(),
            primitive_name: "DFF".to_string(),
            instance_count: 1,
            mechanisms: vec![],
            dc_overrides: IndexMap::new(),
        });

        fmeda.add_primitive_binding(PrimitiveBinding {
            design_ref: DesignRef::parse("top.voter.ff_b_reg"),
            tech_library: "tech_standard".to_string(),
            primitive_name: "DFF".to_string(),
            instance_count: 1,
            mechanisms: vec![],
            dc_overrides: IndexMap::new(),
        });

        // Add multi-contributor mode (timing path spanning both flops)
        fmeda.add_multi_contributor_mode(
            MultiContributorFailureMode::new(
                "MCF-001".to_string(),
                "timing_violation".to_string(),
                0.2, // Total FIT for this mode
                Severity::S2,
                FailureClass::SinglePointFault,
            )
            .with_contributors(vec![
                PrimitiveContributor::new(DesignRef::parse("top.voter.ff_a_reg"), 0.5),
                PrimitiveContributor::new(DesignRef::parse("top.voter.ff_b_reg"), 0.5),
            ]),
        );

        assert!(fmeda.validate().is_ok());

        // Total FIT: 2x DFF (0.5 each) + 1x MCF (0.2) = 1.2
        let total_fit = fmeda.calculate_total_fit(&tech_lib);
        assert!((total_fit - 1.2).abs() < 0.001);

        // FIT for ff_a_reg: 0.5 (DFF) + 0.1 (50% of MCF) = 0.6
        let ff_a_ref = DesignRef::parse("top.voter.ff_a_reg");
        let ff_a_fit = fmeda.calculate_primitive_fit(&ff_a_ref, &tech_lib);
        assert!((ff_a_fit - 0.6).abs() < 0.001);
    }

    #[test]
    fn test_get_modes_for_primitive() {
        let mut fmeda = PrimitiveFmeda::new("design".to_string(), "lib".to_string());

        let ref_a = DesignRef::parse("top.a");
        let ref_b = DesignRef::parse("top.b");
        let ref_c = DesignRef::parse("top.c");

        // Mode affecting A and B
        fmeda.add_multi_contributor_mode(
            MultiContributorFailureMode::new(
                "MCF-1".to_string(),
                "mode1".to_string(),
                1.0,
                Severity::S2,
                FailureClass::SinglePointFault,
            )
            .with_contributors(vec![
                PrimitiveContributor::new(ref_a.clone(), 0.5),
                PrimitiveContributor::new(ref_b.clone(), 0.5),
            ]),
        );

        // Mode affecting B and C
        fmeda.add_multi_contributor_mode(
            MultiContributorFailureMode::new(
                "MCF-2".to_string(),
                "mode2".to_string(),
                1.0,
                Severity::S2,
                FailureClass::Residual,
            )
            .with_contributors(vec![
                PrimitiveContributor::new(ref_b.clone(), 0.6),
                PrimitiveContributor::new(ref_c.clone(), 0.4),
            ]),
        );

        // A is in 1 mode
        let modes_a = fmeda.get_modes_for_primitive(&ref_a);
        assert_eq!(modes_a.len(), 1);

        // B is in 2 modes
        let modes_b = fmeda.get_modes_for_primitive(&ref_b);
        assert_eq!(modes_b.len(), 2);

        // C is in 1 mode
        let modes_c = fmeda.get_modes_for_primitive(&ref_c);
        assert_eq!(modes_c.len(), 1);
    }

    #[test]
    fn test_primitive_contributor() {
        let design_ref = DesignRef::parse("top.voter.ff_out_reg");
        let contributor = PrimitiveContributor::new(design_ref.clone(), 0.3)
            .with_description("Output register in timing path".to_string());

        assert_eq!(contributor.weight, 0.3);
        assert_eq!(contributor.get_fit(100.0), 30.0);
        assert!(contributor.contribution_desc.is_some());
    }

    #[test]
    fn test_detector_binding() {
        let detector = DetectorBinding {
            mechanism_type: DetectorType::Psm,
            mechanism_name: "TimingMonitor".to_string(),
            dc: 95.0,
        };

        assert_eq!(detector.mechanism_type, DetectorType::Psm);
        assert_eq!(detector.dc, 95.0);
    }

    // === Technology Derating Tests ===

    #[test]
    fn test_arrhenius_factor_higher_temp() {
        // Higher temperature than reference should increase FIT
        let params = DeratingParameters::default();
        let conditions = OperatingConditions {
            temperature_c: 105.0, // 50°C above 55°C reference
            voltage: 0.9,
            voltage_nominal: 0.9,
        };

        let calc = DeratingCalculator::new(params, conditions);
        let af = calc.arrhenius_factor();

        // With Ea=0.7eV, temp increase should significantly increase failure rate
        assert!(af > 1.0, "Higher temp should increase AF");
        assert!(af < 100.0, "AF should be reasonable");
    }

    #[test]
    fn test_arrhenius_factor_lower_temp() {
        // Lower temperature than reference should decrease FIT
        let params = DeratingParameters::default();
        let conditions = OperatingConditions {
            temperature_c: 25.0, // 30°C below 55°C reference
            voltage: 0.9,
            voltage_nominal: 0.9,
        };

        let calc = DeratingCalculator::new(params, conditions);
        let af = calc.arrhenius_factor();

        assert!(af < 1.0, "Lower temp should decrease AF");
        assert!(af > 0.01, "AF should be reasonable");
    }

    #[test]
    fn test_arrhenius_factor_same_temp() {
        // Same temperature as reference should give AF = 1.0
        let params = DeratingParameters {
            reference_temperature_c: 85.0,
            ..Default::default()
        };
        let conditions = OperatingConditions {
            temperature_c: 85.0,
            ..Default::default()
        };

        let calc = DeratingCalculator::new(params, conditions);
        let af = calc.arrhenius_factor();

        assert!((af - 1.0).abs() < 0.001, "Same temp should give AF=1.0");
    }

    #[test]
    fn test_voltage_factor() {
        let params = DeratingParameters {
            voltage_exponent: 2.5,
            ..Default::default()
        };

        // 10% overvoltage
        let conditions = OperatingConditions {
            voltage: 0.99,
            voltage_nominal: 0.9,
            ..Default::default()
        };

        let calc = DeratingCalculator::new(params, conditions);
        let vf = calc.voltage_factor();

        // (1.1)^2.5 ≈ 1.27
        assert!(vf > 1.0, "Overvoltage should increase factor");
        assert!((vf - 1.27).abs() < 0.05, "Voltage factor calculation");
    }

    #[test]
    fn test_process_corner_factor() {
        // +3σ worst case
        let params = DeratingParameters {
            process_sigma: 3.0,
            process_cv: 0.15, // 15% CV
            ..Default::default()
        };
        let conditions = OperatingConditions::default();

        let calc = DeratingCalculator::new(params, conditions);
        let pf = calc.process_factor();

        // 1 + 3 * 0.15 = 1.45
        assert!((pf - 1.45).abs() < 0.001, "Process factor calculation");
    }

    #[test]
    fn test_fit_bounds() {
        let params = DeratingParameters::default();
        let conditions = OperatingConditions::default();

        let calc = DeratingCalculator::new(params, conditions);
        let bounds = calc.fit_bounds(100.0);

        // Worst case should be higher than typical
        assert!(bounds.worst_case > bounds.typical);
        // Best case should be lower than typical
        assert!(bounds.best_case < bounds.typical);
        // Bounds should be in reasonable range
        assert!(bounds.best_case > 0.0);
    }

    #[test]
    fn test_derating_presets() {
        // Gate oxide should have high Ea
        let oxide = DeratingParameters::gate_oxide();
        assert_eq!(oxide.activation_energy_ev, 0.7);

        // Electromigration
        let em = DeratingParameters::electromigration();
        assert_eq!(em.activation_energy_ev, 0.6);

        // Hot carrier has negative Ea
        let hci = DeratingParameters::hot_carrier();
        assert!(hci.activation_energy_ev < 0.0);

        // Soft error has no temperature dependence
        let seu = DeratingParameters::soft_error();
        assert_eq!(seu.activation_energy_ev, 0.0);
    }

    #[test]
    fn test_derate_fit() {
        let params = DeratingParameters::default();
        let conditions = OperatingConditions {
            temperature_c: 105.0, // Higher than 55°C reference
            voltage: 0.9,
            voltage_nominal: 0.9,
        };

        let calc = DeratingCalculator::new(params, conditions);
        let base_fit = 10.0;
        let derated = calc.derate_fit(base_fit);

        // Higher temp should increase FIT
        assert!(derated > base_fit);
    }

    #[test]
    fn test_temperature_sensitivity() {
        let params = DeratingParameters::default();
        let base_fit = 100.0;

        let sensitivity = temperature_sensitivity(base_fit, &params, 25.0..125.0, 5);

        assert_eq!(sensitivity.len(), 5);
        // FIT should increase with temperature
        for i in 1..sensitivity.len() {
            assert!(
                sensitivity[i].1 > sensitivity[i - 1].1,
                "FIT should increase with temp"
            );
        }
    }

    #[test]
    fn test_derating_report() {
        let params = DeratingParameters::default();
        let conditions = OperatingConditions {
            temperature_c: 85.0,
            voltage: 0.9,
            voltage_nominal: 0.9,
        };

        let report = format_derating_report(100.0, &params, &conditions);

        assert!(report.contains("Derating Report"));
        assert!(report.contains("Arrhenius"));
        assert!(report.contains("Voltage"));
        assert!(report.contains("FIT"));
    }
}
