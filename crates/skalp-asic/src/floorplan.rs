//! Floorplan Editor and Macro Placement
//!
//! Interactive floorplanning with hierarchical design support

use crate::AsicError;
use serde::{Deserialize, Serialize};
use skalp_lir::CellFunction;

/// Floorplan manager
pub struct FloorplanManager {
    /// Die dimensions
    pub die: DieSpecification,
    /// Floorplan regions
    pub regions: Vec<FloorplanRegion>,
    /// Macro blocks
    pub macros: Vec<MacroBlock>,
    /// Power domains
    pub power_domains: Vec<PowerDomain>,
    /// IO pad ring
    pub io_ring: IOConfiguration,
    /// Hierarchical blocks
    pub hierarchies: Vec<HierarchicalBlock>,
    /// Placement constraints
    pub constraints: FloorplanConstraints,
    /// Voltage areas
    pub voltage_areas: Vec<VoltageArea>,
}

/// Die specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DieSpecification {
    /// Die width (microns)
    pub width: f64,
    /// Die height (microns)
    pub height: f64,
    /// Core area
    pub core: CoreArea,
    /// Aspect ratio
    pub aspect_ratio: f64,
    /// Utilization target
    pub utilization: f64,
}

/// Core area definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoreArea {
    /// Left offset from die edge
    pub left_offset: f64,
    /// Right offset
    pub right_offset: f64,
    /// Top offset
    pub top_offset: f64,
    /// Bottom offset
    pub bottom_offset: f64,
}

/// Floorplan region
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FloorplanRegion {
    /// Region name
    pub name: String,
    /// Bounds
    pub bounds: Rectangle,
    /// Region type
    pub region_type: RegionType,
    /// Module assignment
    pub module: Option<String>,
    /// Placement density
    pub density: f64,
    /// Routing blockages
    pub routing_blockages: Vec<RoutingBlockage>,
    /// Attributes
    pub attributes: RegionAttributes,
}

/// Rectangle bounds
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Rectangle {
    pub x1: f64,
    pub y1: f64,
    pub x2: f64,
    pub y2: f64,
}

/// Region types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RegionType {
    /// Standard cell placement
    StandardCell,
    /// Macro placement area
    MacroPlacement,
    /// Hard blockage
    Blockage,
    /// Partial blockage
    PartialBlockage { density: f64 },
    /// Exclusive region
    Exclusive,
    /// Rectilinear region
    Rectilinear { vertices: Vec<(f64, f64)> },
}

/// Region attributes
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegionAttributes {
    /// Is soft (movable)
    pub soft: bool,
    /// Is fence (cells must stay inside)
    pub fence: bool,
    /// Is guide (preferred but not required)
    pub guide: bool,
}

/// Routing blockage
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RoutingBlockage {
    /// Blocked layers
    pub layers: Vec<usize>,
    /// Blockage area
    pub area: Rectangle,
    /// Partial blockage percentage
    pub partial: Option<f64>,
}

/// Macro block
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MacroBlock {
    /// Macro name
    pub name: String,
    /// Cell type
    pub cell_type: String,
    /// Dimensions
    pub width: f64,
    pub height: f64,
    /// Position
    pub position: Option<(f64, f64)>,
    /// Orientation
    pub orientation: Orientation,
    /// Fixed placement
    pub fixed: bool,
    /// Power pins
    pub power_pins: Vec<PowerPin>,
    /// Halos/keepouts
    pub halo: Option<Halo>,
    /// Timing criticality
    pub criticality: f64,
}

/// Orientation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Orientation {
    N,  // North (0 degrees)
    S,  // South (180 degrees)
    E,  // East (270 degrees)
    W,  // West (90 degrees)
    FN, // Flipped North
    FS, // Flipped South
    FE, // Flipped East
    FW, // Flipped West
}

/// Power pin
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PowerPin {
    pub name: String,
    pub pin_type: PowerPinType,
    pub position: (f64, f64),
    pub layer: usize,
}

/// Power pin types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PowerPinType {
    VDD,
    VSS,
    VDDIO,
    VSSIO,
    Custom(String),
}

/// Macro halo/keepout
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Halo {
    /// Halo distance
    pub distance: f64,
    /// Different distances per side
    pub per_side: Option<HaloPerSide>,
}

/// Per-side halo distances
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HaloPerSide {
    pub left: f64,
    pub right: f64,
    pub top: f64,
    pub bottom: f64,
}

/// Power domain
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PowerDomain {
    /// Domain name
    pub name: String,
    /// Voltage
    pub voltage: f64,
    /// Power switches
    pub switches: Vec<PowerSwitch>,
    /// Always-on
    pub always_on: bool,
    /// Regions
    pub regions: Vec<String>,
}

/// Power switch
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PowerSwitch {
    pub name: String,
    pub switch_type: String,
    pub position: (f64, f64),
    pub enable_signal: String,
}

/// IO configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IOConfiguration {
    /// IO pads
    pub pads: Vec<IOPad>,
    /// Corner pads
    pub corners: Vec<CornerPad>,
    /// Ring width
    pub ring_width: f64,
    /// Power/ground pads
    pub power_pads: Vec<PowerPad>,
}

/// IO pad
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IOPad {
    pub name: String,
    pub pad_type: IOPadType,
    pub side: Side,
    pub position: f64,
    pub orientation: Orientation,
}

/// IO pad types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum IOPadType {
    Input,
    Output,
    Bidirectional,
    Power,
    Ground,
    Analog,
    /// Low-jitter clock input pad
    Clock,
    /// Power pad with integrated Low Dropout Regulator
    PowerLdo,
}

impl IOPadType {
    /// Convert from CellFunction to IOPadType
    /// Returns None if the CellFunction is not an I/O pad type
    pub fn from_cell_function(func: &CellFunction) -> Option<Self> {
        match func {
            CellFunction::InputPad => Some(IOPadType::Input),
            CellFunction::OutputPad => Some(IOPadType::Output),
            CellFunction::BidirPad => Some(IOPadType::Bidirectional),
            CellFunction::ClockPad => Some(IOPadType::Clock),
            CellFunction::PowerPad => Some(IOPadType::Power),
            CellFunction::GroundPad => Some(IOPadType::Ground),
            CellFunction::AnalogPad => Some(IOPadType::Analog),
            CellFunction::PowerPadLdo => Some(IOPadType::PowerLdo),
            _ => None,
        }
    }
}

/// Die sides
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Side {
    Top,
    Bottom,
    Left,
    Right,
}

/// Corner pad
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CornerPad {
    pub corner: Corner,
    pub pad_type: String,
}

/// Corner positions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Corner {
    TopLeft,
    TopRight,
    BottomLeft,
    BottomRight,
}

/// Power pad
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PowerPad {
    pub name: String,
    pub net: String,
    pub side: Side,
    pub position: f64,
}

/// Hierarchical block
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HierarchicalBlock {
    /// Block name
    pub name: String,
    /// Instance path
    pub instance_path: String,
    /// Sub-blocks
    pub children: Vec<String>,
    /// Block bounds
    pub bounds: Option<Rectangle>,
    /// Soft or hard block
    pub block_type: BlockType,
    /// Pin assignments
    pub pins: Vec<BlockPin>,
}

/// Block types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BlockType {
    /// Soft block (can be modified)
    Soft,
    /// Hard block (fixed)
    Hard { gds_file: Option<String> },
    /// Black box
    BlackBox,
}

/// Block pin
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BlockPin {
    pub name: String,
    pub side: Side,
    pub position: f64,
    pub layer: usize,
}

/// Voltage area
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VoltageArea {
    pub name: String,
    pub voltage: f64,
    pub region: Rectangle,
    pub level_shifters: Vec<LevelShifter>,
}

/// Level shifter
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LevelShifter {
    pub name: String,
    pub from_voltage: f64,
    pub to_voltage: f64,
    pub position: (f64, f64),
}

/// Floorplan constraints
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FloorplanConstraints {
    /// Relative placement constraints
    pub relative_placement: Vec<RelativePlacement>,
    /// Alignment constraints
    pub alignments: Vec<Alignment>,
    /// Spacing constraints
    pub spacing: Vec<SpacingConstraint>,
    /// Clustering constraints
    pub clusters: Vec<Cluster>,
}

/// Relative placement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RelativePlacement {
    pub reference: String,
    pub target: String,
    pub relation: PlacementRelation,
    pub distance: Option<f64>,
}

/// Placement relations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PlacementRelation {
    Above,
    Below,
    LeftOf,
    RightOf,
    Near,
}

/// Alignment constraint
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Alignment {
    pub objects: Vec<String>,
    pub alignment_type: AlignmentType,
}

/// Alignment types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AlignmentType {
    Left,
    Right,
    Top,
    Bottom,
    CenterH,
    CenterV,
}

/// Spacing constraint
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpacingConstraint {
    pub object1: String,
    pub object2: String,
    pub min_spacing: f64,
    pub max_spacing: Option<f64>,
}

/// Cluster definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Cluster {
    pub name: String,
    pub members: Vec<String>,
    pub cluster_type: ClusterType,
}

/// Cluster types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ClusterType {
    Soft,
    Hard,
    Columnar,
}

/// Interactive floorplan editor
pub struct FloorplanEditor {
    /// Current floorplan
    pub floorplan: FloorplanManager,
    /// Edit history
    pub history: Vec<FloorplanEdit>,
    /// Undo stack
    pub undo_stack: Vec<FloorplanManager>,
    /// Validation engine
    pub validator: FloorplanValidator,
}

/// Floorplan edit operation
#[derive(Debug, Clone)]
pub enum FloorplanEdit {
    MoveRegion {
        name: String,
        new_bounds: Rectangle,
    },
    ResizeRegion {
        name: String,
        new_bounds: Rectangle,
    },
    AddRegion {
        region: FloorplanRegion,
    },
    RemoveRegion {
        name: String,
    },
    PlaceMacro {
        name: String,
        position: (f64, f64),
    },
    MoveMacro {
        name: String,
        new_position: (f64, f64),
    },
    RotateMacro {
        name: String,
        orientation: Orientation,
    },
}

/// Floorplan validator
pub struct FloorplanValidator {
    /// Validation rules
    pub rules: Vec<ValidationRule>,
}

/// Validation rule
#[derive(Debug, Clone)]
pub enum ValidationRule {
    NoOverlap,
    MinSpacing { distance: f64 },
    MaxUtilization { threshold: f64 },
    PowerDomainIntegrity,
    IOPlacement,
}

impl FloorplanManager {
    /// Create new floorplan
    pub fn new(width: f64, height: f64) -> Self {
        Self {
            die: DieSpecification {
                width,
                height,
                core: CoreArea {
                    left_offset: 10.0,
                    right_offset: 10.0,
                    top_offset: 10.0,
                    bottom_offset: 10.0,
                },
                aspect_ratio: width / height,
                utilization: 0.7,
            },
            regions: Vec::new(),
            macros: Vec::new(),
            power_domains: Vec::new(),
            io_ring: IOConfiguration {
                pads: Vec::new(),
                corners: Vec::new(),
                ring_width: 50.0,
                power_pads: Vec::new(),
            },
            hierarchies: Vec::new(),
            constraints: FloorplanConstraints {
                relative_placement: Vec::new(),
                alignments: Vec::new(),
                spacing: Vec::new(),
                clusters: Vec::new(),
            },
            voltage_areas: Vec::new(),
        }
    }

    /// Add floorplan region
    pub fn add_region(&mut self, region: FloorplanRegion) {
        self.regions.push(region);
    }

    /// Add macro
    pub fn add_macro(&mut self, macro_block: MacroBlock) {
        self.macros.push(macro_block);
    }

    /// Calculate area utilization
    pub fn calculate_utilization(&self) -> f64 {
        let core_area = (self.die.width - self.die.core.left_offset - self.die.core.right_offset)
            * (self.die.height - self.die.core.top_offset - self.die.core.bottom_offset);

        let mut used_area = 0.0;
        for macro_block in &self.macros {
            used_area += macro_block.width * macro_block.height;
        }

        used_area / core_area
    }

    /// Generate placement constraints
    pub fn generate_placement_constraints(&self) -> Vec<String> {
        let mut constraints = Vec::new();

        // Fixed macro positions
        for macro_block in &self.macros {
            if macro_block.fixed {
                if let Some(pos) = macro_block.position {
                    constraints.push(format!(
                        "set_cell_location {} {:.2} {:.2} -fixed",
                        macro_block.name, pos.0, pos.1
                    ));
                }
            }
        }

        // Region constraints
        for region in &self.regions {
            if let Some(ref module) = region.module {
                constraints.push(format!(
                    "create_region {} {:.2} {:.2} {:.2} {:.2}",
                    region.name,
                    region.bounds.x1,
                    region.bounds.y1,
                    region.bounds.x2,
                    region.bounds.y2
                ));
                constraints.push(format!("add_to_region {} {}", region.name, module));
            }
        }

        constraints
    }

    /// Automatic macro placement
    pub fn auto_place_macros(&mut self) -> Result<(), AsicError> {
        // Simple placement algorithm - place macros around periphery
        let mut placed_macros = Vec::new();
        let mut current_x = self.die.core.left_offset;
        let mut current_y = self.die.core.bottom_offset;

        for macro_block in &mut self.macros {
            if !macro_block.fixed {
                // Check if it fits
                if current_x + macro_block.width <= self.die.width - self.die.core.right_offset {
                    macro_block.position = Some((current_x, current_y));
                    current_x += macro_block.width + 10.0; // Add spacing
                } else {
                    // Move to next row
                    current_x = self.die.core.left_offset;
                    current_y += 100.0; // Assume row height
                    macro_block.position = Some((current_x, current_y));
                    current_x += macro_block.width + 10.0;
                }
                placed_macros.push(macro_block.name.clone());
            }
        }

        Ok(())
    }

    /// Export to DEF format
    pub fn export_def(&self, path: &std::path::Path) -> Result<(), AsicError> {
        use std::fs::File;
        use std::io::Write;

        let mut file = File::create(path)
            .map_err(|e| AsicError::PlacementError(format!("Failed to create DEF: {}", e)))?;

        writeln!(file, "VERSION 5.8 ;")?;
        writeln!(file, "DESIGN top ;")?;
        writeln!(file, "UNITS DISTANCE MICRONS 1000 ;")?;
        writeln!(file)?;

        // Die area
        writeln!(
            file,
            "DIEAREA ( 0 0 ) ( {:.0} {:.0} ) ;",
            self.die.width * 1000.0,
            self.die.height * 1000.0
        )?;
        writeln!(file)?;

        // Rows
        writeln!(file, "# ROWS")?;
        // TODO: Generate rows
        writeln!(file)?;

        // Components (macros)
        if !self.macros.is_empty() {
            writeln!(file, "COMPONENTS {} ;", self.macros.len())?;
            for macro_block in &self.macros {
                write!(file, "  - {} {}", macro_block.name, macro_block.cell_type)?;
                if let Some(pos) = macro_block.position {
                    write!(
                        file,
                        " + PLACED ( {:.0} {:.0} ) {:?}",
                        pos.0 * 1000.0,
                        pos.1 * 1000.0,
                        macro_block.orientation
                    )?;
                }
                if macro_block.fixed {
                    write!(file, " + FIXED")?;
                }
                writeln!(file, " ;")?;
            }
            writeln!(file, "END COMPONENTS")?;
        }

        writeln!(file)?;
        writeln!(file, "END DESIGN")?;

        Ok(())
    }
}

impl FloorplanEditor {
    /// Create new editor
    pub fn new(floorplan: FloorplanManager) -> Self {
        Self {
            floorplan,
            history: Vec::new(),
            undo_stack: Vec::new(),
            validator: FloorplanValidator {
                rules: vec![
                    ValidationRule::NoOverlap,
                    ValidationRule::MinSpacing { distance: 10.0 },
                    ValidationRule::MaxUtilization { threshold: 0.85 },
                ],
            },
        }
    }

    /// Apply edit operation
    pub fn apply_edit(&mut self, edit: FloorplanEdit) -> Result<(), AsicError> {
        // Save current state for undo
        self.undo_stack.push(self.floorplan.clone());

        // Apply edit
        match edit.clone() {
            FloorplanEdit::MoveRegion { name, new_bounds } => {
                if let Some(region) = self.floorplan.regions.iter_mut().find(|r| r.name == name) {
                    region.bounds = new_bounds;
                }
            }
            FloorplanEdit::PlaceMacro { name, position } => {
                if let Some(macro_block) = self.floorplan.macros.iter_mut().find(|m| m.name == name)
                {
                    macro_block.position = Some(position);
                }
            }
            _ => {}
        }

        // Validate
        if !self.validate() {
            // Rollback
            if let Some(prev) = self.undo_stack.pop() {
                self.floorplan = prev;
            }
            return Err(AsicError::PlacementError("Validation failed".to_string()));
        }

        self.history.push(edit);
        Ok(())
    }

    /// Validate floorplan
    fn validate(&self) -> bool {
        for rule in &self.validator.rules {
            match rule {
                ValidationRule::NoOverlap => {
                    // Check macro overlaps
                    for i in 0..self.floorplan.macros.len() {
                        for j in i + 1..self.floorplan.macros.len() {
                            if self.macros_overlap(
                                &self.floorplan.macros[i],
                                &self.floorplan.macros[j],
                            ) {
                                return false;
                            }
                        }
                    }
                }
                ValidationRule::MaxUtilization { threshold } => {
                    if self.floorplan.calculate_utilization() > *threshold {
                        return false;
                    }
                }
                _ => {}
            }
        }
        true
    }

    /// Check if macros overlap
    fn macros_overlap(&self, m1: &MacroBlock, m2: &MacroBlock) -> bool {
        if let (Some(p1), Some(p2)) = (m1.position, m2.position) {
            let m1_rect = Rectangle {
                x1: p1.0,
                y1: p1.1,
                x2: p1.0 + m1.width,
                y2: p1.1 + m1.height,
            };
            let m2_rect = Rectangle {
                x1: p2.0,
                y1: p2.1,
                x2: p2.0 + m2.width,
                y2: p2.1 + m2.height,
            };

            !(m1_rect.x2 < m2_rect.x1
                || m1_rect.x1 > m2_rect.x2
                || m1_rect.y2 < m2_rect.y1
                || m1_rect.y1 > m2_rect.y2)
        } else {
            false
        }
    }

    /// Undo last operation
    pub fn undo(&mut self) -> bool {
        if let Some(prev) = self.undo_stack.pop() {
            self.floorplan = prev;
            self.history.pop();
            true
        } else {
            false
        }
    }
}

impl Clone for FloorplanManager {
    fn clone(&self) -> Self {
        Self {
            die: self.die.clone(),
            regions: self.regions.clone(),
            macros: self.macros.clone(),
            power_domains: self.power_domains.clone(),
            io_ring: self.io_ring.clone(),
            hierarchies: self.hierarchies.clone(),
            constraints: self.constraints.clone(),
            voltage_areas: self.voltage_areas.clone(),
        }
    }
}
