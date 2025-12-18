//! Native SKY130 Standard Cell Library
//!
//! Complete implementation of SKY130 standard cells without external dependencies.
//! Based on the open-source SKY130 PDK.
//!
//! NOTE: This module needs to be updated to use GateNetlist instead of the legacy Lir type.

#![allow(unused_imports)]
use crate::placement::{Net, Netlist, StandardCell};
use crate::AsicError;
use skalp_lir::PrimitiveType;
// Legacy imports removed - Lir and Primitive no longer exist
// The new flow should use GateNetlist instead
use std::collections::HashMap;

/// SKY130 Standard Cell Library
pub struct StandardCellLibrary {
    /// Cell definitions
    pub cells: HashMap<String, CellDefinition>,
}

/// Standard cell definition
pub struct CellDefinition {
    /// Cell name in library
    pub name: String,
    /// Cell width (in sites)
    pub width: f64,
    /// Cell height (tracks)
    pub height: f64,
    /// Cell area (μm²)
    pub area: f64,
    /// Input capacitance (fF)
    pub input_cap: f64,
    /// Output drive strength
    pub drive_strength: f64,
    /// Delay model
    pub delay: DelayModel,
    /// Power consumption (μW)
    pub power: PowerModel,
    /// Physical layout
    pub layout: CellLayout,
    /// Cell pins
    pub pins: Vec<Pin>,
}

/// Cell pin
#[derive(Debug, Clone)]
pub struct Pin {
    /// Pin name
    pub name: String,
    /// Pin position
    pub position: (f64, f64),
    /// Pin direction
    pub direction: PinDirection,
}

/// Pin direction
#[derive(Debug, Clone)]
pub enum PinDirection {
    Input,
    Output,
    InOut,
    Power,
    Ground,
}

/// Delay model for timing
pub struct DelayModel {
    /// Intrinsic delay (ps)
    pub intrinsic: f64,
    /// Rise time (ps/fF)
    pub rise_slope: f64,
    /// Fall time (ps/fF)
    pub fall_slope: f64,
}

/// Power consumption model
pub struct PowerModel {
    /// Static power (μW)
    pub static_power: f64,
    /// Dynamic power per transition (fJ)
    pub dynamic_power: f64,
}

/// Physical cell layout
pub struct CellLayout {
    /// Metal1 regions
    pub metal1_regions: Vec<Rectangle>,
    /// Metal2 regions
    pub metal2_regions: Vec<Rectangle>,
    /// Poly regions
    pub poly_regions: Vec<Rectangle>,
    /// Active (diffusion) regions
    pub active_regions: Vec<Rectangle>,
    /// Contact regions
    pub contact_regions: Vec<Rectangle>,
    /// N-well region
    pub nwell_region: Option<Rectangle>,
    /// Via positions
    pub vias: Vec<ViaInstance>,
    /// Pin locations
    pub pins: Vec<PinLayout>,
}

/// Rectangle geometry
#[derive(Debug, Clone)]
pub struct Rectangle {
    pub x1: f64,
    pub y1: f64,
    pub x2: f64,
    pub y2: f64,
}

/// Via instance
#[derive(Debug, Clone)]
pub struct ViaInstance {
    pub x: f64,
    pub y: f64,
    pub via_type: ViaType,
}

/// Via types in SKY130
#[derive(Debug, Clone)]
pub enum ViaType {
    Contact, // Diffusion/Poly to Metal1
    Via1,    // Metal1 to Metal2
    Via2,    // Metal2 to Metal3
    Via3,    // Metal3 to Metal4
    Via4,    // Metal4 to Metal5
}

/// Pin layout information
#[derive(Debug, Clone)]
pub struct PinLayout {
    pub name: String,
    pub layer: LayerType,
    pub geometry: Rectangle,
}

/// Layer types in SKY130
#[derive(Debug, Clone)]
pub enum LayerType {
    Nwell,
    Pwell,
    NDiffusion,
    PDiffusion,
    Poly,
    Metal1,
    Metal2,
    Metal3,
    Metal4,
    Metal5,
}

impl Default for StandardCellLibrary {
    fn default() -> Self {
        Self::new()
    }
}

impl StandardCellLibrary {
    /// Create SKY130 standard cell library
    pub fn new() -> Self {
        let mut cells = HashMap::new();

        // Define SKY130 standard cells
        // These are based on the real SKY130 PDK specifications

        // INVERTER (sky130_fd_sc_hd__inv_1)
        cells.insert(
            "INV_X1".to_string(),
            CellDefinition {
                name: "sky130_fd_sc_hd__inv_1".to_string(),
                width: 1.38,  // 3 tracks
                height: 2.72, // Standard height
                area: 3.7536,
                input_cap: 1.7,
                drive_strength: 1.0,
                delay: DelayModel {
                    intrinsic: 30.0,
                    rise_slope: 45.0,
                    fall_slope: 35.0,
                },
                power: PowerModel {
                    static_power: 0.002,
                    dynamic_power: 0.8,
                },
                layout: Self::create_inverter_layout(),
                pins: vec![
                    Pin {
                        name: "A".to_string(),
                        position: (0.23, 0.85),
                        direction: PinDirection::Input,
                    },
                    Pin {
                        name: "Y".to_string(),
                        position: (1.15, 1.87),
                        direction: PinDirection::Output,
                    },
                ],
            },
        );

        // NAND2 (sky130_fd_sc_hd__nand2_1)
        cells.insert(
            "NAND2_X1".to_string(),
            CellDefinition {
                name: "sky130_fd_sc_hd__nand2_1".to_string(),
                width: 1.84, // 4 tracks
                height: 2.72,
                area: 5.0048,
                input_cap: 1.7,
                drive_strength: 1.0,
                delay: DelayModel {
                    intrinsic: 35.0,
                    rise_slope: 50.0,
                    fall_slope: 40.0,
                },
                power: PowerModel {
                    static_power: 0.003,
                    dynamic_power: 1.0,
                },
                layout: Self::create_nand2_layout(),
                pins: vec![
                    Pin {
                        name: "A".to_string(),
                        position: (0.23, 0.85),
                        direction: PinDirection::Input,
                    },
                    Pin {
                        name: "B".to_string(),
                        position: (0.69, 0.85),
                        direction: PinDirection::Input,
                    },
                    Pin {
                        name: "Y".to_string(),
                        position: (1.61, 1.87),
                        direction: PinDirection::Output,
                    },
                ],
            },
        );

        // NOR2 (sky130_fd_sc_hd__nor2_1)
        cells.insert(
            "NOR2_X1".to_string(),
            CellDefinition {
                name: "sky130_fd_sc_hd__nor2_1".to_string(),
                width: 2.30, // 5 tracks
                height: 2.72,
                area: 6.256,
                input_cap: 1.7,
                drive_strength: 1.0,
                delay: DelayModel {
                    intrinsic: 40.0,
                    rise_slope: 55.0,
                    fall_slope: 42.0,
                },
                power: PowerModel {
                    static_power: 0.003,
                    dynamic_power: 1.1,
                },
                layout: Self::create_nor2_layout(),
                pins: vec![
                    Pin {
                        name: "A".to_string(),
                        position: (0.46, 0.85),
                        direction: PinDirection::Input,
                    },
                    Pin {
                        name: "B".to_string(),
                        position: (1.15, 0.85),
                        direction: PinDirection::Input,
                    },
                    Pin {
                        name: "Y".to_string(),
                        position: (1.84, 1.87),
                        direction: PinDirection::Output,
                    },
                ],
            },
        );

        // D Flip-Flop (sky130_fd_sc_hd__dfxtp_1)
        cells.insert(
            "DFF_X1".to_string(),
            CellDefinition {
                name: "sky130_fd_sc_hd__dfxtp_1".to_string(),
                width: 7.36, // 16 tracks
                height: 2.72,
                area: 20.0192,
                input_cap: 2.1,
                drive_strength: 1.0,
                delay: DelayModel {
                    intrinsic: 150.0, // Clk-to-Q delay
                    rise_slope: 60.0,
                    fall_slope: 50.0,
                },
                power: PowerModel {
                    static_power: 0.01,
                    dynamic_power: 3.5,
                },
                layout: Self::create_dff_layout(),
                pins: vec![
                    Pin {
                        name: "D".to_string(),
                        position: (0.46, 1.25),
                        direction: PinDirection::Input,
                    },
                    Pin {
                        name: "CLK".to_string(),
                        position: (2.30, 1.25),
                        direction: PinDirection::Input,
                    },
                    Pin {
                        name: "Q".to_string(),
                        position: (6.90, 1.87),
                        direction: PinDirection::Output,
                    },
                ],
            },
        );

        Self { cells }
    }

    /// Map GateNetlist design to SKY130 netlist
    /// NOTE: This function needs to be reimplemented to use GateNetlist instead of legacy Lir
    pub fn map_design_stub(&self) -> Result<Netlist, AsicError> {
        Err(AsicError::TechnologyError(
            "SKY130 mapping temporarily disabled - pending GateNetlist migration".to_string(),
        ))
    }

    // Legacy: Map LIR design to SKY130 netlist (commented out during migration)
    /*
    pub fn map_design(&self, design: &Lir) -> Result<Netlist, AsicError> {
        let mut netlist_cells = Vec::new();
        let mut nets = Vec::new();
        let mut net_map: HashMap<String, usize> = HashMap::new();

        // Map each primitive to a standard cell (flat structure)
        for (prim_idx, prim) in design.primitives.iter().enumerate() {
            let cell_name = self.map_primitive_type(&prim.ptype)?;
            let cell_def = self.get_cell_definition(&cell_name)?;

            let cell = StandardCell {
                name: format!("prim_{}_{}", prim.id.0, prim_idx),
                cell_type: cell_name,
                width: cell_def.width,
                height: cell_def.height,
                area: cell_def.area,
            };

            netlist_cells.push(cell);

            // Track net connections for inputs
            for (i, input) in prim.inputs.iter().enumerate() {
                let input_name = format!("net_{}", input.0);
                let net_id = *net_map.entry(input_name.clone()).or_insert_with(|| {
                    let idx = nets.len();
                    nets.push(Net {
                        name: input_name.clone(),
                        connections: Vec::new(),
                    });
                    idx
                });
                nets[net_id].connections.push((prim_idx, format!("I{}", i)));
            }

            // Track net connections for outputs
            for (i, output) in prim.outputs.iter().enumerate() {
                let output_name = format!("net_{}", output.0);
                let net_id = *net_map.entry(output_name.clone()).or_insert_with(|| {
                    let idx = nets.len();
                    nets.push(Net {
                        name: output_name.clone(),
                        connections: Vec::new(),
                    });
                    idx
                });
                nets[net_id].connections.push((prim_idx, format!("O{}", i)));
            }
        }

        Ok(Netlist {
            cells: netlist_cells,
            nets,
        })
    }
    */

    /// Map primitive type to cell name
    fn map_primitive_type(&self, ptype: &PrimitiveType) -> Result<String, AsicError> {
        let cell_name = match ptype {
            PrimitiveType::Inv => "INV_X1",
            PrimitiveType::Nand { .. } => "NAND2_X1",
            PrimitiveType::Nor { .. } => "NOR2_X1",
            PrimitiveType::DffP
            | PrimitiveType::DffN
            | PrimitiveType::DffNeg
            | PrimitiveType::DffE
            | PrimitiveType::DffAR
            | PrimitiveType::DffAS
            | PrimitiveType::DffScan => "DFF_X1",
            _ => {
                return Err(AsicError::TechnologyError(format!(
                    "Unsupported primitive type: {:?}",
                    ptype
                )))
            }
        };
        Ok(cell_name.to_string())
    }

    /// Get cell definition by name
    fn get_cell_definition(&self, cell_name: &str) -> Result<&CellDefinition, AsicError> {
        self.cells.get(cell_name).ok_or_else(|| {
            AsicError::TechnologyError(format!("Cell {} not found in library", cell_name))
        })
    }

    // Legacy: Create pins for a primitive based on cell definition (commented out during migration)
    /*
    #[allow(dead_code)]
    fn create_pins_for_primitive(&self, prim: &Primitive, cell_def: &CellDefinition) -> Vec<Pin> {
        let mut pins = Vec::new();

        // Input pins
        for (i, _input) in prim.inputs.iter().enumerate() {
            pins.push(Pin {
                name: format!("A{}", i),
                direction: PinDirection::Input,
                position: (0.0, 0.5 + i as f64 * 0.5), // Simplified positioning
            });
        }

        // Output pins
        for (i, _output) in prim.outputs.iter().enumerate() {
            pins.push(Pin {
                name: format!("Y{}", i),
                direction: PinDirection::Output,
                position: (cell_def.width, 0.5 + i as f64 * 0.5),
            });
        }

        // Power/Ground pins
        pins.push(Pin {
            name: "VDD".to_string(),
            direction: PinDirection::Power,
            position: (cell_def.width / 2.0, cell_def.height),
        });

        pins.push(Pin {
            name: "VSS".to_string(),
            direction: PinDirection::Ground,
            position: (cell_def.width / 2.0, 0.0),
        });

        pins
    }
    */

    /// Create physical layout for inverter
    fn create_inverter_layout() -> CellLayout {
        CellLayout {
            // Simplified layout - real implementation would have exact coordinates
            metal1_regions: vec![
                Rectangle {
                    x1: 0.0,
                    y1: 0.0,
                    x2: 0.46,
                    y2: 0.17,
                }, // VSS rail
                Rectangle {
                    x1: 0.0,
                    y1: 2.55,
                    x2: 0.46,
                    y2: 2.72,
                }, // VDD rail
                Rectangle {
                    x1: 0.23,
                    y1: 0.5,
                    x2: 0.46,
                    y2: 1.5,
                }, // Input
                Rectangle {
                    x1: 0.69,
                    y1: 0.5,
                    x2: 0.92,
                    y2: 2.2,
                }, // Output
            ],
            metal2_regions: vec![],
            poly_regions: vec![
                Rectangle {
                    x1: 0.46,
                    y1: 0.3,
                    x2: 0.58,
                    y2: 2.42,
                }, // Gate poly
            ],
            active_regions: vec![
                Rectangle {
                    x1: 0.30,
                    y1: 0.3,
                    x2: 0.75,
                    y2: 1.15,
                }, // N-diffusion
                Rectangle {
                    x1: 0.30,
                    y1: 1.57,
                    x2: 0.75,
                    y2: 2.42,
                }, // P-diffusion
            ],
            contact_regions: vec![
                Rectangle {
                    x1: 0.20,
                    y1: 0.80,
                    x2: 0.26,
                    y2: 0.90,
                },
                Rectangle {
                    x1: 0.66,
                    y1: 0.80,
                    x2: 0.72,
                    y2: 0.90,
                },
                Rectangle {
                    x1: 0.66,
                    y1: 1.82,
                    x2: 0.72,
                    y2: 1.92,
                },
            ],
            nwell_region: Some(Rectangle {
                x1: 0.0,
                y1: 1.36,
                x2: 1.38,
                y2: 2.72,
            }),
            vias: vec![
                ViaInstance {
                    x: 0.23,
                    y: 0.85,
                    via_type: ViaType::Contact,
                },
                ViaInstance {
                    x: 0.69,
                    y: 0.85,
                    via_type: ViaType::Contact,
                },
                ViaInstance {
                    x: 0.69,
                    y: 1.87,
                    via_type: ViaType::Contact,
                },
            ],
            pins: vec![
                PinLayout {
                    name: "A".to_string(),
                    layer: LayerType::Metal1,
                    geometry: Rectangle {
                        x1: 0.23,
                        y1: 0.5,
                        x2: 0.46,
                        y2: 1.5,
                    },
                },
                PinLayout {
                    name: "Y".to_string(),
                    layer: LayerType::Metal1,
                    geometry: Rectangle {
                        x1: 0.69,
                        y1: 0.5,
                        x2: 0.92,
                        y2: 2.2,
                    },
                },
            ],
        }
    }

    /// Create physical layout for NAND2
    fn create_nand2_layout() -> CellLayout {
        // Simplified - actual layout would be much more detailed
        CellLayout {
            metal1_regions: vec![
                Rectangle {
                    x1: 0.0,
                    y1: 0.0,
                    x2: 1.84,
                    y2: 0.17,
                }, // VSS
                Rectangle {
                    x1: 0.0,
                    y1: 2.55,
                    x2: 1.84,
                    y2: 2.72,
                }, // VDD
            ],
            metal2_regions: vec![],
            poly_regions: vec![
                Rectangle {
                    x1: 0.46,
                    y1: 0.3,
                    x2: 0.58,
                    y2: 2.42,
                },
                Rectangle {
                    x1: 0.92,
                    y1: 0.3,
                    x2: 1.04,
                    y2: 2.42,
                },
            ],
            active_regions: vec![
                Rectangle {
                    x1: 0.30,
                    y1: 0.3,
                    x2: 1.20,
                    y2: 1.15,
                },
                Rectangle {
                    x1: 0.30,
                    y1: 1.57,
                    x2: 1.20,
                    y2: 2.42,
                },
            ],
            contact_regions: vec![Rectangle {
                x1: 0.25,
                y1: 0.80,
                x2: 0.31,
                y2: 0.90,
            }],
            nwell_region: Some(Rectangle {
                x1: 0.0,
                y1: 1.36,
                x2: 1.84,
                y2: 2.72,
            }),
            vias: vec![],
            pins: vec![
                PinLayout {
                    name: "A".to_string(),
                    layer: LayerType::Metal1,
                    geometry: Rectangle {
                        x1: 0.23,
                        y1: 0.5,
                        x2: 0.46,
                        y2: 1.5,
                    },
                },
                PinLayout {
                    name: "B".to_string(),
                    layer: LayerType::Metal1,
                    geometry: Rectangle {
                        x1: 0.69,
                        y1: 0.5,
                        x2: 0.92,
                        y2: 1.5,
                    },
                },
                PinLayout {
                    name: "Y".to_string(),
                    layer: LayerType::Metal1,
                    geometry: Rectangle {
                        x1: 1.38,
                        y1: 0.5,
                        x2: 1.61,
                        y2: 2.2,
                    },
                },
            ],
        }
    }

    /// Create physical layout for NOR2
    fn create_nor2_layout() -> CellLayout {
        // Simplified layout
        CellLayout {
            metal1_regions: vec![
                Rectangle {
                    x1: 0.0,
                    y1: 0.0,
                    x2: 2.30,
                    y2: 0.17,
                },
                Rectangle {
                    x1: 0.0,
                    y1: 2.55,
                    x2: 2.30,
                    y2: 2.72,
                },
            ],
            metal2_regions: vec![],
            poly_regions: vec![
                Rectangle {
                    x1: 0.46,
                    y1: 0.3,
                    x2: 0.58,
                    y2: 2.42,
                },
                Rectangle {
                    x1: 1.15,
                    y1: 0.3,
                    x2: 1.27,
                    y2: 2.42,
                },
            ],
            active_regions: vec![
                Rectangle {
                    x1: 0.30,
                    y1: 0.3,
                    x2: 1.50,
                    y2: 1.15,
                },
                Rectangle {
                    x1: 0.30,
                    y1: 1.57,
                    x2: 1.50,
                    y2: 2.42,
                },
            ],
            contact_regions: vec![Rectangle {
                x1: 0.25,
                y1: 0.80,
                x2: 0.31,
                y2: 0.90,
            }],
            nwell_region: Some(Rectangle {
                x1: 0.0,
                y1: 1.36,
                x2: 2.30,
                y2: 2.72,
            }),
            vias: vec![],
            pins: vec![],
        }
    }

    /// Create physical layout for D flip-flop
    fn create_dff_layout() -> CellLayout {
        // Simplified DFF layout - actual would be complex with master-slave latches
        CellLayout {
            metal1_regions: vec![
                Rectangle {
                    x1: 0.0,
                    y1: 0.0,
                    x2: 7.36,
                    y2: 0.17,
                },
                Rectangle {
                    x1: 0.0,
                    y1: 2.55,
                    x2: 7.36,
                    y2: 2.72,
                },
            ],
            metal2_regions: vec![],
            poly_regions: vec![],
            active_regions: vec![],
            contact_regions: vec![],
            nwell_region: Some(Rectangle {
                x1: 0.0,
                y1: 3.68,
                x2: 7.36,
                y2: 5.44,
            }),
            vias: vec![],
            pins: vec![
                PinLayout {
                    name: "D".to_string(),
                    layer: LayerType::Metal1,
                    geometry: Rectangle {
                        x1: 0.46,
                        y1: 1.0,
                        x2: 0.69,
                        y2: 1.5,
                    },
                },
                PinLayout {
                    name: "CLK".to_string(),
                    layer: LayerType::Metal1,
                    geometry: Rectangle {
                        x1: 3.0,
                        y1: 0.5,
                        x2: 3.23,
                        y2: 1.0,
                    },
                },
                PinLayout {
                    name: "Q".to_string(),
                    layer: LayerType::Metal1,
                    geometry: Rectangle {
                        x1: 6.9,
                        y1: 1.0,
                        x2: 7.13,
                        y2: 1.5,
                    },
                },
            ],
        }
    }
}
