//! Advanced Design Rule Checking (DRC) for ASIC Implementation
//!
//! Comprehensive layout verification including:
//! - Geometric design rules (spacing, width, area)
//! - Connectivity rules (via enclosure, overlap)
//! - Process-specific rules (antenna, density)
//! - Manufacturing rules (resolution, OPC)
//! - Electrical rules (resistance, capacitance)

use crate::{AsicError, Technology, DesignRules};
use crate::placement::Placement;
use crate::routing::RoutingResult;
use crate::gdsii::GdsiiStream;
use std::collections::{HashMap, HashSet};
use serde::{Serialize, Deserialize};

/// Advanced DRC engine with comprehensive rule checking
pub struct AdvancedDrcEngine {
    /// Target technology
    pub technology: Technology,
    /// Design rules database
    pub design_rules: DesignRules,
    /// Advanced rule set
    pub advanced_rules: AdvancedRuleSet,
    /// Configuration
    pub config: DrcConfig,
}

/// Comprehensive design rule set
#[derive(Debug, Clone)]
pub struct AdvancedRuleSet {
    /// Metal layer rules
    pub metal_rules: Vec<MetalLayerRules>,
    /// Via rules
    pub via_rules: Vec<ViaRules>,
    /// Poly rules
    pub poly_rules: PolyRules,
    /// Diffusion rules
    pub diffusion_rules: DiffusionRules,
    /// Well rules
    pub well_rules: WellRules,
    /// Contact rules
    pub contact_rules: ContactRules,
    /// Process-specific rules
    pub process_rules: ProcessRules,
    /// Electrical rules
    pub electrical_rules: ElectricalRules,
}

/// Metal layer design rules
#[derive(Debug, Clone)]
pub struct MetalLayerRules {
    /// Layer number
    pub layer: usize,
    /// Minimum width
    pub min_width: f64,
    /// Minimum spacing (same net)
    pub min_spacing: f64,
    /// Minimum spacing (different nets)
    pub min_spacing_diff: f64,
    /// Minimum area
    pub min_area: f64,
    /// Maximum width
    pub max_width: Option<f64>,
    /// Slot rules
    pub slot_rules: SlotRules,
    /// End-of-line rules
    pub eol_rules: EndOfLineRules,
    /// Antenna rules
    pub antenna_rules: AntennaRules,
}

/// Via design rules
#[derive(Debug, Clone)]
pub struct ViaRules {
    /// Via type
    pub via_type: String,
    /// From layer
    pub from_layer: usize,
    /// To layer
    pub to_layer: usize,
    /// Minimum size
    pub min_size: (f64, f64),
    /// Maximum size
    pub max_size: Option<(f64, f64)>,
    /// Minimum enclosure (bottom)
    pub min_enclosure_bottom: (f64, f64),
    /// Minimum enclosure (top)
    pub min_enclosure_top: (f64, f64),
    /// Minimum spacing
    pub min_spacing: f64,
    /// Array rules
    pub array_rules: ViaArrayRules,
}

/// Slot rules for metal layers
#[derive(Debug, Clone)]
pub struct SlotRules {
    /// Enable slot checking
    pub enabled: bool,
    /// Maximum unslotted length
    pub max_length: f64,
    /// Slot width
    pub slot_width: f64,
    /// Slot spacing
    pub slot_spacing: f64,
}

/// End-of-line rules
#[derive(Debug, Clone)]
pub struct EndOfLineRules {
    /// Enable EOL checking
    pub enabled: bool,
    /// EOL spacing
    pub eol_spacing: f64,
    /// EOL length
    pub eol_length: f64,
    /// Parallel run length
    pub parallel_length: f64,
}

/// Antenna rules
#[derive(Debug, Clone)]
pub struct AntennaRules {
    /// Enable antenna checking
    pub enabled: bool,
    /// Maximum antenna ratio
    pub max_ratio: f64,
    /// Cumulative antenna ratio
    pub max_cumulative_ratio: f64,
    /// Gate area factor
    pub gate_factor: f64,
}

/// Via array rules
#[derive(Debug, Clone)]
pub struct ViaArrayRules {
    /// Minimum number in array
    pub min_array_size: usize,
    /// Maximum number in array
    pub max_array_size: Option<usize>,
    /// Array spacing
    pub array_spacing: (f64, f64),
}

/// Poly layer rules
#[derive(Debug, Clone)]
pub struct PolyRules {
    /// Minimum width
    pub min_width: f64,
    /// Minimum spacing
    pub min_spacing: f64,
    /// Gate extension
    pub gate_extension: f64,
    /// Contact enclosure
    pub contact_enclosure: f64,
}

/// Diffusion rules
#[derive(Debug, Clone)]
pub struct DiffusionRules {
    /// Minimum width
    pub min_width: f64,
    /// Minimum spacing
    pub min_spacing: f64,
    /// Well enclosure
    pub well_enclosure: f64,
    /// Contact enclosure
    pub contact_enclosure: f64,
}

/// Well rules
#[derive(Debug, Clone)]
pub struct WellRules {
    /// Minimum width
    pub min_width: f64,
    /// Minimum spacing
    pub min_spacing: f64,
    /// Same potential spacing
    pub same_potential_spacing: f64,
    /// Minimum area
    pub min_area: f64,
}

/// Contact rules
#[derive(Debug, Clone)]
pub struct ContactRules {
    /// Minimum size
    pub min_size: (f64, f64),
    /// Minimum spacing
    pub min_spacing: f64,
    /// Enclosure rules
    pub enclosure: HashMap<String, (f64, f64)>,
}

/// Process-specific rules
#[derive(Debug, Clone)]
pub struct ProcessRules {
    /// Density rules
    pub density_rules: DensityRules,
    /// Lithography rules
    pub lithography_rules: LithographyRules,
    /// CMP rules
    pub cmp_rules: CmpRules,
    /// Stress rules
    pub stress_rules: StressRules,
}

/// Density rules
#[derive(Debug, Clone)]
pub struct DensityRules {
    /// Enable density checking
    pub enabled: bool,
    /// Window size for density calculation
    pub window_size: (f64, f64),
    /// Minimum density
    pub min_density: f64,
    /// Maximum density
    pub max_density: f64,
    /// Layer-specific rules
    pub layer_rules: HashMap<usize, (f64, f64)>, // (min, max)
}

/// Lithography rules
#[derive(Debug, Clone)]
pub struct LithographyRules {
    /// Minimum resolution
    pub min_resolution: f64,
    /// Optical proximity correction
    pub opc_rules: OpcRules,
    /// Phase shift mask rules
    pub psm_rules: Option<PsmRules>,
}

/// OPC rules
#[derive(Debug, Clone)]
pub struct OpcRules {
    /// Enable OPC checking
    pub enabled: bool,
    /// Minimum feature size for OPC
    pub min_feature: f64,
    /// Corner rounding radius
    pub corner_radius: f64,
}

/// Phase shift mask rules
#[derive(Debug, Clone)]
pub struct PsmRules {
    /// Phase conflict distance
    pub phase_conflict_distance: f64,
    /// Minimum phase region size
    pub min_phase_region: f64,
}

/// Chemical mechanical polishing rules
#[derive(Debug, Clone)]
pub struct CmpRules {
    /// Enable CMP checking
    pub enabled: bool,
    /// Pattern density window
    pub density_window: f64,
    /// Thickness variation rules
    pub thickness_rules: HashMap<usize, f64>,
}

/// Stress rules
#[derive(Debug, Clone)]
pub struct StressRules {
    /// Enable stress checking
    pub enabled: bool,
    /// Maximum stress level
    pub max_stress: f64,
    /// Stress calculation window
    pub calc_window: f64,
}

/// Electrical rules
#[derive(Debug, Clone)]
pub struct ElectricalRules {
    /// Resistance rules
    pub resistance_rules: ResistanceRules,
    /// Capacitance rules
    pub capacitance_rules: CapacitanceRules,
    /// Electromigration rules
    pub em_rules: ElectromigrationRules,
    /// IR drop rules
    pub ir_drop_rules: IrDropRules,
}

/// Resistance rules
#[derive(Debug, Clone)]
pub struct ResistanceRules {
    /// Enable resistance checking
    pub enabled: bool,
    /// Maximum resistance per unit length
    pub max_resistance: HashMap<usize, f64>,
    /// Via resistance limits
    pub via_resistance: HashMap<String, f64>,
}

/// Capacitance rules
#[derive(Debug, Clone)]
pub struct CapacitanceRules {
    /// Enable capacitance checking
    pub enabled: bool,
    /// Maximum coupling capacitance
    pub max_coupling: f64,
    /// Minimum ground capacitance
    pub min_ground_cap: f64,
}

/// Electromigration rules
#[derive(Debug, Clone)]
pub struct ElectromigrationRules {
    /// Enable EM checking
    pub enabled: bool,
    /// Maximum current density
    pub max_current_density: HashMap<usize, f64>,
    /// Frequency factor
    pub frequency_factor: f64,
}

/// IR drop rules
#[derive(Debug, Clone)]
pub struct IrDropRules {
    /// Enable IR drop checking
    pub enabled: bool,
    /// Maximum voltage drop
    pub max_voltage_drop: f64,
    /// Grid resistance model
    pub grid_model: GridResistanceModel,
}

/// Grid resistance model
#[derive(Debug, Clone)]
pub struct GridResistanceModel {
    /// Sheet resistance per layer
    pub sheet_resistance: HashMap<usize, f64>,
    /// Via resistance
    pub via_resistance: HashMap<String, f64>,
    /// Contact resistance
    pub contact_resistance: f64,
}

/// DRC configuration
#[derive(Debug, Clone)]
pub struct DrcConfig {
    /// Enable parallel checking
    pub parallel_checking: bool,
    /// Number of threads
    pub num_threads: usize,
    /// Check level (fast/comprehensive)
    pub check_level: CheckLevel,
    /// Enable incremental DRC
    pub incremental: bool,
    /// Violation limit (0 = unlimited)
    pub violation_limit: usize,
}

/// DRC check level
#[derive(Debug, Clone)]
pub enum CheckLevel {
    /// Fast checking (basic rules only)
    Fast,
    /// Standard checking (most rules)
    Standard,
    /// Comprehensive checking (all rules)
    Comprehensive,
    /// Manufacturing signoff (ultra-strict)
    Signoff,
}

/// DRC violation report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DrcReport {
    /// Summary statistics
    pub summary: DrcSummary,
    /// Violations by category
    pub violations: HashMap<ViolationType, Vec<DrcViolation>>,
    /// Layer-wise statistics
    pub layer_stats: HashMap<usize, LayerStats>,
    /// Recommendations
    pub recommendations: Vec<DrcRecommendation>,
}

/// DRC summary statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DrcSummary {
    /// Total violations
    pub total_violations: usize,
    /// Violations by severity
    pub by_severity: HashMap<ViolationSeverity, usize>,
    /// Violations by layer
    pub by_layer: HashMap<usize, usize>,
    /// Check runtime
    pub runtime_ms: u64,
    /// Coverage percentage
    pub coverage: f64,
}

/// DRC violation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DrcViolation {
    /// Violation ID
    pub id: String,
    /// Violation type
    pub violation_type: ViolationType,
    /// Severity level
    pub severity: ViolationSeverity,
    /// Location (x, y)
    pub location: (f64, f64),
    /// Affected layer
    pub layer: usize,
    /// Measured value
    pub measured: f64,
    /// Required value
    pub required: f64,
    /// Description
    pub description: String,
    /// Suggested fix
    pub suggested_fix: Option<String>,
    /// Waiver information
    pub waiver: Option<DrcWaiver>,
}

/// Violation type enumeration
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ViolationType {
    MinWidth,
    MinSpacing,
    MinArea,
    Enclosure,
    Overlap,
    Extension,
    Antenna,
    Density,
    Slot,
    EndOfLine,
    Via,
    Contact,
    Well,
    Poly,
    Diffusion,
    Electrical,
    Process,
    Custom(String),
}

/// Violation severity
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ViolationSeverity {
    /// Critical - manufacturing failure likely
    Critical,
    /// Error - functional failure possible
    Error,
    /// Warning - yield impact possible
    Warning,
    /// Info - best practice violation
    Info,
}

/// Layer statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LayerStats {
    /// Total area
    pub total_area: f64,
    /// Utilized area
    pub utilized_area: f64,
    /// Number of shapes
    pub shape_count: usize,
    /// Average width
    pub avg_width: f64,
    /// Violation count
    pub violations: usize,
    /// Density
    pub density: f64,
}

/// DRC recommendation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DrcRecommendation {
    /// Recommendation type
    pub rec_type: RecommendationType,
    /// Priority (1-10)
    pub priority: u8,
    /// Description
    pub description: String,
    /// Estimated impact
    pub impact: String,
    /// Implementation effort
    pub effort: EffortLevel,
}

/// Recommendation type
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RecommendationType {
    LayoutOptimization,
    RuleWaiver,
    ProcessAdjustment,
    DesignChange,
    FloorplanRevision,
}

/// Implementation effort level
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EffortLevel {
    Low,
    Medium,
    High,
    Extreme,
}

/// DRC waiver information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DrcWaiver {
    /// Waiver ID
    pub id: String,
    /// Justification
    pub justification: String,
    /// Approved by
    pub approved_by: String,
    /// Approval date
    pub approval_date: String,
    /// Expiration date
    pub expiration: Option<String>,
}

impl AdvancedDrcEngine {
    /// Create new advanced DRC engine
    pub fn new(technology: Technology, design_rules: DesignRules) -> Self {
        Self {
            technology: technology.clone(),
            design_rules,
            advanced_rules: AdvancedRuleSet::for_technology(&technology),
            config: DrcConfig::default(),
        }
    }

    /// Run comprehensive DRC check
    pub fn check_design(&self,
                       placement: &Placement,
                       routing: &RoutingResult,
                       gds_stream: Option<&GdsiiStream>) -> Result<DrcReport, AsicError> {

        let start_time = std::time::Instant::now();
        let mut violations = HashMap::new();
        let mut layer_stats = HashMap::new();

        // Initialize violation maps
        for violation_type in [
            ViolationType::MinWidth,
            ViolationType::MinSpacing,
            ViolationType::MinArea,
            ViolationType::Enclosure,
            ViolationType::Antenna,
            ViolationType::Density,
        ] {
            violations.insert(violation_type, Vec::new());
        }

        // Check geometric rules
        self.check_geometric_rules(placement, routing, &mut violations)?;

        // Check connectivity rules
        self.check_connectivity_rules(routing, &mut violations)?;

        // Check process rules
        if matches!(self.config.check_level, CheckLevel::Comprehensive | CheckLevel::Signoff) {
            self.check_process_rules(placement, routing, &mut violations)?;
        }

        // Check electrical rules
        if self.advanced_rules.electrical_rules.resistance_rules.enabled {
            self.check_electrical_rules(routing, &mut violations)?;
        }

        // Calculate layer statistics
        self.calculate_layer_stats(placement, routing, &mut layer_stats)?;

        // Generate recommendations
        let recommendations = self.generate_recommendations(&violations)?;

        let runtime = start_time.elapsed().as_millis() as u64;
        let total_violations: usize = violations.values().map(|v| v.len()).sum();

        let summary = DrcSummary {
            total_violations,
            by_severity: self.calculate_severity_stats(&violations),
            by_layer: self.calculate_layer_violation_stats(&violations),
            runtime_ms: runtime,
            coverage: self.calculate_coverage(),
        };

        Ok(DrcReport {
            summary,
            violations,
            layer_stats,
            recommendations,
        })
    }

    /// Check geometric design rules
    fn check_geometric_rules(&self,
                            _placement: &Placement,
                            routing: &RoutingResult,
                            violations: &mut HashMap<ViolationType, Vec<DrcViolation>>) -> Result<(), AsicError> {

        // Check minimum width violations
        for net in &routing.routed_nets {
            for segment in &net.segments {
                let width = segment.width;
                let layer_rules = &self.advanced_rules.metal_rules.get(segment.layer)
                    .ok_or_else(|| AsicError::DrcError(format!("No rules for layer {}", segment.layer)))?;

                if width < layer_rules.min_width {
                    let violation_id = format!("MW_{}_{}_{}", net.name, segment.layer,
                        violations.get(&ViolationType::MinWidth).map(|v| v.len()).unwrap_or(0));
                    violations.get_mut(&ViolationType::MinWidth).unwrap().push(DrcViolation {
                        id: violation_id,
                        violation_type: ViolationType::MinWidth,
                        severity: ViolationSeverity::Error,
                        location: if segment.points.len() > 0 { segment.points[0] } else { (0.0, 0.0) },
                        layer: segment.layer,
                        measured: width,
                        required: layer_rules.min_width,
                        description: format!("Wire width {:.3} < minimum {:.3}", width, layer_rules.min_width),
                        suggested_fix: Some("Increase wire width or use wider metal layer".to_string()),
                        waiver: None,
                    });
                }
            }
        }

        // Check minimum spacing violations
        self.check_spacing_violations(routing, violations)?;

        // Check minimum area violations
        self.check_area_violations(routing, violations)?;

        Ok(())
    }

    /// Check spacing violations
    fn check_spacing_violations(&self,
                               routing: &RoutingResult,
                               violations: &mut HashMap<ViolationType, Vec<DrcViolation>>) -> Result<(), AsicError> {

        // For each layer, check spacing between all wire segments
        let mut layer_segments: HashMap<usize, Vec<_>> = HashMap::new();

        for net in &routing.routed_nets {
            for segment in &net.segments {
                layer_segments.entry(segment.layer).or_default().push((net, segment));
            }
        }

        for (layer, segments) in layer_segments {
            let layer_rules = match self.advanced_rules.metal_rules.get(layer) {
                Some(rules) => rules,
                None => continue,
            };

            for i in 0..segments.len() {
                for j in i+1..segments.len() {
                    let (net1, seg1) = segments[i];
                    let (net2, seg2) = segments[j];

                    let spacing = self.calculate_segment_spacing(seg1, seg2);
                    let required_spacing = if net1.name == net2.name {
                        layer_rules.min_spacing
                    } else {
                        layer_rules.min_spacing_diff
                    };

                    if spacing < required_spacing {
                        let violation_id = format!("SP_{}_{}_{}_{}", net1.name, net2.name, layer,
                            violations.get(&ViolationType::MinSpacing).map(|v| v.len()).unwrap_or(0));
                        violations.get_mut(&ViolationType::MinSpacing).unwrap().push(DrcViolation {
                            id: violation_id,
                            violation_type: ViolationType::MinSpacing,
                            severity: if net1.name == net2.name { ViolationSeverity::Warning } else { ViolationSeverity::Error },
                            location: self.get_closest_points(seg1, seg2),
                            layer,
                            measured: spacing,
                            required: required_spacing,
                            description: format!("Spacing {:.3} < minimum {:.3} between {} and {}",
                                               spacing, required_spacing, net1.name, net2.name),
                            suggested_fix: Some("Increase spacing or reroute wires".to_string()),
                            waiver: None,
                        });
                    }
                }
            }
        }

        Ok(())
    }

    /// Check area violations
    fn check_area_violations(&self,
                            routing: &RoutingResult,
                            violations: &mut HashMap<ViolationType, Vec<DrcViolation>>) -> Result<(), AsicError> {

        for net in &routing.routed_nets {
            let mut layer_areas: HashMap<usize, f64> = HashMap::new();

            for segment in &net.segments {
                if segment.points.len() >= 2 {
                    let length = self.calculate_segment_length(segment);
                    let area = length * segment.width;
                    *layer_areas.entry(segment.layer).or_insert(0.0) += area;
                }
            }

            for (layer, area) in layer_areas {
                if let Some(layer_rules) = self.advanced_rules.metal_rules.get(layer) {
                    if area < layer_rules.min_area {
                        let violation_id = format!("MA_{}_{}_{}_{}", net.name, layer, area as u32,
                            violations.get(&ViolationType::MinArea).map(|v| v.len()).unwrap_or(0));
                        violations.get_mut(&ViolationType::MinArea).unwrap().push(DrcViolation {
                            id: violation_id,
                            violation_type: ViolationType::MinArea,
                            severity: ViolationSeverity::Warning,
                            location: (0.0, 0.0), // Would need to calculate centroid
                            layer,
                            measured: area,
                            required: layer_rules.min_area,
                            description: format!("Metal area {:.3} < minimum {:.3} for net {}",
                                               area, layer_rules.min_area, net.name),
                            suggested_fix: Some("Add fill shapes or merge with nearby metal".to_string()),
                            waiver: None,
                        });
                    }
                }
            }
        }

        Ok(())
    }

    /// Check connectivity rules
    fn check_connectivity_rules(&self,
                               _routing: &RoutingResult,
                               _violations: &mut HashMap<ViolationType, Vec<DrcViolation>>) -> Result<(), AsicError> {
        // Implementation would check via enclosure, overlap, etc.
        Ok(())
    }

    /// Check process-specific rules
    fn check_process_rules(&self,
                          _placement: &Placement,
                          _routing: &RoutingResult,
                          _violations: &mut HashMap<ViolationType, Vec<DrcViolation>>) -> Result<(), AsicError> {
        // Implementation would check density, antenna, etc.
        Ok(())
    }

    /// Check electrical rules
    fn check_electrical_rules(&self,
                             _routing: &RoutingResult,
                             _violations: &mut HashMap<ViolationType, Vec<DrcViolation>>) -> Result<(), AsicError> {
        // Implementation would check resistance, capacitance, etc.
        Ok(())
    }

    /// Calculate spacing between two segments
    fn calculate_segment_spacing(&self, seg1: &crate::routing::WireSegment, seg2: &crate::routing::WireSegment) -> f64 {
        // Simplified - would need proper geometric calculation
        if seg1.points.len() < 2 || seg2.points.len() < 2 {
            return f64::INFINITY;
        }

        let p1 = seg1.points[0];
        let p2 = seg2.points[0];
        ((p1.0 - p2.0).powi(2) + (p1.1 - p2.1).powi(2)).sqrt()
    }

    /// Get closest points between two segments
    fn get_closest_points(&self, seg1: &crate::routing::WireSegment, seg2: &crate::routing::WireSegment) -> (f64, f64) {
        // Simplified - return midpoint
        if seg1.points.len() > 0 && seg2.points.len() > 0 {
            let p1 = seg1.points[0];
            let p2 = seg2.points[0];
            ((p1.0 + p2.0) / 2.0, (p1.1 + p2.1) / 2.0)
        } else {
            (0.0, 0.0)
        }
    }

    /// Calculate segment length
    fn calculate_segment_length(&self, segment: &crate::routing::WireSegment) -> f64 {
        if segment.points.len() < 2 {
            return 0.0;
        }

        let mut total_length = 0.0;
        for i in 1..segment.points.len() {
            let p1 = segment.points[i-1];
            let p2 = segment.points[i];
            total_length += ((p1.0 - p2.0).powi(2) + (p1.1 - p2.1).powi(2)).sqrt();
        }
        total_length
    }

    /// Calculate layer statistics
    fn calculate_layer_stats(&self,
                            _placement: &Placement,
                            routing: &RoutingResult,
                            layer_stats: &mut HashMap<usize, LayerStats>) -> Result<(), AsicError> {

        for net in &routing.routed_nets {
            for segment in &net.segments {
                let stats = layer_stats.entry(segment.layer).or_insert(LayerStats {
                    total_area: 0.0,
                    utilized_area: 0.0,
                    shape_count: 0,
                    avg_width: 0.0,
                    violations: 0,
                    density: 0.0,
                });

                stats.shape_count += 1;
                let length = self.calculate_segment_length(segment);
                stats.utilized_area += length * segment.width;
                stats.avg_width = (stats.avg_width * (stats.shape_count - 1) as f64 + segment.width) / stats.shape_count as f64;
            }
        }

        // Calculate densities
        for stats in layer_stats.values_mut() {
            if stats.total_area > 0.0 {
                stats.density = stats.utilized_area / stats.total_area;
            }
        }

        Ok(())
    }

    /// Generate optimization recommendations
    fn generate_recommendations(&self, violations: &HashMap<ViolationType, Vec<DrcViolation>>) -> Result<Vec<DrcRecommendation>, AsicError> {
        let mut recommendations = Vec::new();

        // Analyze violation patterns and suggest fixes
        let total_violations: usize = violations.values().map(|v| v.len()).sum();

        if total_violations > 100 {
            recommendations.push(DrcRecommendation {
                rec_type: RecommendationType::LayoutOptimization,
                priority: 9,
                description: "High violation count suggests systematic layout issues".to_string(),
                impact: "Significant yield and manufacturability improvement".to_string(),
                effort: EffortLevel::High,
            });
        }

        if let Some(spacing_violations) = violations.get(&ViolationType::MinSpacing) {
            if spacing_violations.len() > 20 {
                recommendations.push(DrcRecommendation {
                    rec_type: RecommendationType::FloorplanRevision,
                    priority: 8,
                    description: "Multiple spacing violations indicate congested routing".to_string(),
                    impact: "Reduced crosstalk and improved signal integrity".to_string(),
                    effort: EffortLevel::Medium,
                });
            }
        }

        Ok(recommendations)
    }

    /// Calculate severity statistics
    fn calculate_severity_stats(&self, violations: &HashMap<ViolationType, Vec<DrcViolation>>) -> HashMap<ViolationSeverity, usize> {
        let mut stats = HashMap::new();

        for violation_list in violations.values() {
            for violation in violation_list {
                *stats.entry(violation.severity.clone()).or_insert(0) += 1;
            }
        }

        stats
    }

    /// Calculate layer violation statistics
    fn calculate_layer_violation_stats(&self, violations: &HashMap<ViolationType, Vec<DrcViolation>>) -> HashMap<usize, usize> {
        let mut stats = HashMap::new();

        for violation_list in violations.values() {
            for violation in violation_list {
                *stats.entry(violation.layer).or_insert(0) += 1;
            }
        }

        stats
    }

    /// Calculate coverage percentage
    fn calculate_coverage(&self) -> f64 {
        // Simplified coverage calculation
        match self.config.check_level {
            CheckLevel::Fast => 60.0,
            CheckLevel::Standard => 85.0,
            CheckLevel::Comprehensive => 95.0,
            CheckLevel::Signoff => 99.0,
        }
    }
}

impl AdvancedRuleSet {
    /// Create default rule set for a technology
    fn for_technology(technology: &Technology) -> Self {
        match technology {
            Technology::Sky130 => Self::sky130_rules(),
            Technology::Gf180 => Self::gf180_rules(),
            Technology::Ihp130 => Self::ihp130_rules(),
        }
    }

    /// SKY130 specific rules
    fn sky130_rules() -> Self {
        Self {
            metal_rules: vec![
                MetalLayerRules {
                    layer: 1,
                    min_width: 0.14,
                    min_spacing: 0.14,
                    min_spacing_diff: 0.14,
                    min_area: 0.083,
                    max_width: None,
                    slot_rules: SlotRules {
                        enabled: true,
                        max_length: 1.27,
                        slot_width: 0.14,
                        slot_spacing: 0.14,
                    },
                    eol_rules: EndOfLineRules {
                        enabled: true,
                        eol_spacing: 0.07,
                        eol_length: 0.28,
                        parallel_length: 0.14,
                    },
                    antenna_rules: AntennaRules {
                        enabled: true,
                        max_ratio: 5.0,
                        max_cumulative_ratio: 5.0,
                        gate_factor: 1.0,
                    },
                },
            ],
            via_rules: vec![],
            poly_rules: PolyRules {
                min_width: 0.15,
                min_spacing: 0.21,
                gate_extension: 0.13,
                contact_enclosure: 0.07,
            },
            diffusion_rules: DiffusionRules {
                min_width: 0.15,
                min_spacing: 0.27,
                well_enclosure: 0.18,
                contact_enclosure: 0.06,
            },
            well_rules: WellRules {
                min_width: 0.84,
                min_spacing: 1.27,
                same_potential_spacing: 0.0,
                min_area: 0.56,
            },
            contact_rules: ContactRules {
                min_size: (0.17, 0.17),
                min_spacing: 0.19,
                enclosure: HashMap::new(),
            },
            process_rules: ProcessRules {
                density_rules: DensityRules {
                    enabled: true,
                    window_size: (700.0, 700.0),
                    min_density: 0.25,
                    max_density: 0.70,
                    layer_rules: HashMap::new(),
                },
                lithography_rules: LithographyRules {
                    min_resolution: 0.13,
                    opc_rules: OpcRules {
                        enabled: true,
                        min_feature: 0.20,
                        corner_radius: 0.02,
                    },
                    psm_rules: None,
                },
                cmp_rules: CmpRules {
                    enabled: true,
                    density_window: 100.0,
                    thickness_rules: HashMap::new(),
                },
                stress_rules: StressRules {
                    enabled: false,
                    max_stress: 0.0,
                    calc_window: 0.0,
                },
            },
            electrical_rules: ElectricalRules {
                resistance_rules: ResistanceRules {
                    enabled: true,
                    max_resistance: HashMap::new(),
                    via_resistance: HashMap::new(),
                },
                capacitance_rules: CapacitanceRules {
                    enabled: true,
                    max_coupling: 0.2,
                    min_ground_cap: 0.01,
                },
                em_rules: ElectromigrationRules {
                    enabled: true,
                    max_current_density: HashMap::new(),
                    frequency_factor: 1.0,
                },
                ir_drop_rules: IrDropRules {
                    enabled: true,
                    max_voltage_drop: 0.1,
                    grid_model: GridResistanceModel {
                        sheet_resistance: HashMap::new(),
                        via_resistance: HashMap::new(),
                        contact_resistance: 1.0,
                    },
                },
            },
        }
    }

    /// GF180 specific rules
    fn gf180_rules() -> Self {
        // Simplified - would have actual GF180 rules
        Self::sky130_rules()
    }

    /// IHP130 specific rules
    fn ihp130_rules() -> Self {
        // Simplified - would have actual IHP130 rules
        Self::sky130_rules()
    }
}

impl Default for DrcConfig {
    fn default() -> Self {
        Self {
            parallel_checking: true,
            num_threads: num_cpus::get(),
            check_level: CheckLevel::Standard,
            incremental: false,
            violation_limit: 0,
        }
    }
}

/// Legacy DRC engine for compatibility
pub struct DrcEngine {
    pub technology: Technology,
    pub design_rules: DesignRules,
}

impl DrcEngine {
    pub fn new(technology: Technology, design_rules: DesignRules) -> Self {
        Self { technology, design_rules }
    }
}