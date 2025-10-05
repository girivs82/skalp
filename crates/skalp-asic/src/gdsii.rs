//! Native GDSII Generation for ASIC Implementation
//!
//! Implements direct GDSII stream format generation:
//! - Layer mapping for technology
//! - Polygon and path generation
//! - Cell hierarchy
//! - Text labels and properties

use crate::cts::ClockTree;
use crate::placement::Placement;
use crate::routing::RoutingResult;
use crate::sky130::StandardCellLibrary;
use crate::{AsicError, DesignRules, Technology};
use std::collections::HashSet;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::Path;

/// GDSII generator
pub struct GdsiiGenerator {
    /// Technology node
    pub technology: Technology,
    /// Design rules
    pub design_rules: DesignRules,
    /// Layer mapping
    pub layers: LayerMap,
    /// Database unit in meters
    pub db_unit: f64,
    /// User unit
    pub user_unit: f64,
}

/// Layer mapping for technology
#[derive(Debug, Clone)]
pub struct LayerMap {
    /// N-well layer
    pub nwell: (i16, i16),
    /// P-well layer
    pub pwell: (i16, i16),
    /// Active (diffusion) layer
    pub active: (i16, i16),
    /// Polysilicon layer
    pub poly: (i16, i16),
    /// Metal layers
    pub metal: Vec<(i16, i16)>,
    /// Via layers
    pub via: Vec<(i16, i16)>,
    /// N+ implant
    pub nplus: (i16, i16),
    /// P+ implant
    pub pplus: (i16, i16),
    /// Contact layer
    pub contact: (i16, i16),
    /// Pad layer
    pub pad: (i16, i16),
    /// Text layer
    pub text: (i16, i16),
}

/// GDSII stream file
pub struct GdsiiStream {
    /// Design name
    pub name: String,
    /// Creation time
    pub timestamp: [i16; 6],
    /// Libraries
    pub libraries: Vec<GdsiiLibrary>,
}

/// GDSII library
#[derive(Debug, Clone)]
pub struct GdsiiLibrary {
    /// Library name
    pub name: String,
    /// Structures (cells)
    pub structures: Vec<GdsiiStructure>,
}

/// GDSII structure (cell)
#[derive(Debug, Clone)]
pub struct GdsiiStructure {
    /// Structure name
    pub name: String,
    /// Elements in structure
    pub elements: Vec<GdsiiElement>,
}

/// GDSII element
#[derive(Debug, Clone)]
pub enum GdsiiElement {
    /// Boundary (polygon)
    Boundary {
        layer: i16,
        datatype: i16,
        points: Vec<(i32, i32)>,
    },
    /// Path (wire)
    Path {
        layer: i16,
        datatype: i16,
        width: i32,
        points: Vec<(i32, i32)>,
        pathtype: i16,
    },
    /// Structure reference (instance)
    Sref {
        name: String,
        position: (i32, i32),
        angle: f64,
        mag: f64,
        reflect: bool,
    },
    /// Array reference
    Aref {
        name: String,
        cols: i16,
        rows: i16,
        spacing: (i32, i32),
        position: (i32, i32),
    },
    /// Text label
    Text {
        layer: i16,
        datatype: i16,
        text: String,
        position: (i32, i32),
        height: i32,
    },
    /// Node (electrical)
    Node {
        layer: i16,
        nodetype: i16,
        points: Vec<(i32, i32)>,
    },
    /// Box
    Box {
        layer: i16,
        boxtype: i16,
        points: Vec<(i32, i32)>,
    },
}

/// GDSII record types
#[derive(Debug, Clone, Copy)]
#[repr(u8)]
enum RecordType {
    Header = 0x00,
    BgnLib = 0x01,
    LibName = 0x02,
    Units = 0x03,
    EndLib = 0x04,
    BgnStr = 0x05,
    StrName = 0x06,
    EndStr = 0x07,
    Boundary = 0x08,
    Path = 0x09,
    Sref = 0x0A,
    Aref = 0x0B,
    Text = 0x0C,
    Layer = 0x0D,
    DataType = 0x0E,
    Width = 0x0F,
    Xy = 0x10,
    EndEl = 0x11,
    SName = 0x12,
    ColRow = 0x13,
    Node = 0x15,
    TextType = 0x16,
    Presentation = 0x17,
    String = 0x19,
    Strans = 0x1A,
    Mag = 0x1B,
    Angle = 0x1C,
    RefLibs = 0x1F,
    Fonts = 0x20,
    PathType = 0x21,
    Generations = 0x22,
    AttrTable = 0x23,
    ElFlags = 0x26,
    NodeType = 0x2A,
    PropAttr = 0x2B,
    PropValue = 0x2C,
    Box = 0x2D,
    BoxType = 0x2E,
    Plex = 0x2F,
    BgnExtn = 0x30,
    EndExtn = 0x31,
}

/// GDSII data types
#[derive(Debug, Clone, Copy)]
#[repr(u8)]
enum DataType {
    NoData = 0x00,
    BitArray = 0x01,
    TwoByteInt = 0x02,
    FourByteInt = 0x03,
    FourByteReal = 0x04,
    EightByteReal = 0x05,
    AsciiString = 0x06,
}

impl GdsiiGenerator {
    /// Create new GDSII generator
    pub fn new(technology: Technology, design_rules: DesignRules) -> Self {
        let layers = match &technology {
            Technology::Sky130 => LayerMap::sky130(),
            Technology::Gf180 => LayerMap::gf180(),
            Technology::Ihp130 => LayerMap::ihp130(),
        };

        Self {
            technology,
            design_rules,
            layers,
            db_unit: 1e-9,   // 1nm database unit
            user_unit: 1e-6, // 1um user unit
        }
    }

    /// Generate GDSII from placed and routed design
    pub fn generate(
        &self,
        design_name: &str,
        placement: &Placement,
        routing: &RoutingResult,
        clock_tree: Option<&ClockTree>,
        output_path: &Path,
    ) -> Result<(), AsicError> {
        // Create top-level structure
        let mut top_structure = GdsiiStructure {
            name: design_name.to_string(),
            elements: Vec::new(),
        };

        // Add placed cells
        self.add_placed_cells(&mut top_structure, placement)?;

        // Add routing
        self.add_routing(&mut top_structure, routing)?;

        // Add clock tree if present
        if let Some(cts) = clock_tree {
            self.add_clock_tree(&mut top_structure, cts)?;
        }

        // Add power/ground grid
        self.add_power_grid(&mut top_structure, placement)?;

        // Add die boundary
        self.add_die_boundary(&mut top_structure, placement)?;

        // Create library
        let library = GdsiiLibrary {
            name: format!("{}_lib", design_name),
            structures: vec![top_structure],
        };

        // Add standard cell structures
        let std_cell_structures = self.create_std_cell_structures()?;

        // Create GDSII stream
        let stream = GdsiiStream {
            name: design_name.to_string(),
            timestamp: self.current_timestamp(),
            libraries: vec![library],
        };

        // Write to file
        self.write_stream(&stream, output_path)?;

        Ok(())
    }

    /// Add placed cells to structure
    fn add_placed_cells(
        &self,
        structure: &mut GdsiiStructure,
        placement: &Placement,
    ) -> Result<(), AsicError> {
        for cell in &placement.cells {
            let position = placement
                .positions
                .get(&cell.instance_name)
                .ok_or_else(|| {
                    AsicError::PlacementError(format!(
                        "No position for cell {}",
                        cell.instance_name
                    ))
                })?;

            // Convert to database units
            let x = (position.0 / self.db_unit) as i32;
            let y = (position.1 / self.db_unit) as i32;

            // Add cell reference
            structure.elements.push(GdsiiElement::Sref {
                name: cell.cell_type.clone(),
                position: (x, y),
                angle: 0.0,
                mag: 1.0,
                reflect: false,
            });
        }

        Ok(())
    }

    /// Add routing to structure
    fn add_routing(
        &self,
        structure: &mut GdsiiStructure,
        routing: &RoutingResult,
    ) -> Result<(), AsicError> {
        for net in &routing.routed_nets {
            // Add each segment as a path
            for segment in &net.segments {
                let points: Vec<(i32, i32)> = segment
                    .points
                    .iter()
                    .map(|p| ((p.0 / self.db_unit) as i32, (p.1 / self.db_unit) as i32))
                    .collect();

                // Get layer for this metal level
                let (layer, datatype) = self.get_metal_layer(segment.layer)?;

                structure.elements.push(GdsiiElement::Path {
                    layer,
                    datatype,
                    width: (segment.width / self.db_unit) as i32,
                    points,
                    pathtype: 0, // Flush ends
                });
            }

            // Add vias
            for via in &net.vias {
                let (layer, datatype) = self.get_via_layer(via.from_layer, via.to_layer)?;

                // Via as a box
                let half_size = (via.size / 2.0 / self.db_unit) as i32;
                let x = (via.position.0 / self.db_unit) as i32;
                let y = (via.position.1 / self.db_unit) as i32;

                let points = vec![
                    (x - half_size, y - half_size),
                    (x + half_size, y - half_size),
                    (x + half_size, y + half_size),
                    (x - half_size, y + half_size),
                    (x - half_size, y - half_size), // Close polygon
                ];

                structure.elements.push(GdsiiElement::Boundary {
                    layer,
                    datatype,
                    points,
                });
            }
        }

        Ok(())
    }

    /// Add clock tree to structure
    fn add_clock_tree(
        &self,
        structure: &mut GdsiiStructure,
        clock_tree: &ClockTree,
    ) -> Result<(), AsicError> {
        // Add clock buffers
        for buffer in &clock_tree.buffers {
            let x = (buffer.position.0 / self.db_unit) as i32;
            let y = (buffer.position.1 / self.db_unit) as i32;

            structure.elements.push(GdsiiElement::Sref {
                name: buffer.cell_type.clone(),
                position: (x, y),
                angle: 0.0,
                mag: 1.0,
                reflect: false,
            });
        }

        // Add clock nets (typically on higher metal layers)
        for net in &clock_tree.nets {
            for segment in &net.segments {
                let points: Vec<(i32, i32)> = vec![
                    (
                        (segment.start.0 / self.db_unit) as i32,
                        (segment.start.1 / self.db_unit) as i32,
                    ),
                    (
                        (segment.end.0 / self.db_unit) as i32,
                        (segment.end.1 / self.db_unit) as i32,
                    ),
                ];

                let (layer, datatype) = self.get_metal_layer(segment.layer)?;

                structure.elements.push(GdsiiElement::Path {
                    layer,
                    datatype,
                    width: (segment.width / self.db_unit) as i32,
                    points,
                    pathtype: 0,
                });
            }
        }

        Ok(())
    }

    /// Add power/ground grid
    fn add_power_grid(
        &self,
        structure: &mut GdsiiStructure,
        placement: &Placement,
    ) -> Result<(), AsicError> {
        let (min_x, min_y, max_x, max_y) = self.get_die_bounds(placement)?;

        // Power rails on M1 (horizontal)
        let rail_pitch = 10.0; // um
        let rail_width = 2.0; // um
        let (layer, datatype) = self.layers.metal[0]; // M1

        let mut y = min_y;
        let mut is_power = true;

        while y <= max_y {
            let points = vec![
                ((min_x / self.db_unit) as i32, (y / self.db_unit) as i32),
                ((max_x / self.db_unit) as i32, (y / self.db_unit) as i32),
            ];

            structure.elements.push(GdsiiElement::Path {
                layer,
                datatype: if is_power { 0 } else { 1 }, // 0 for VDD, 1 for VSS
                width: (rail_width / self.db_unit) as i32,
                points,
                pathtype: 0,
            });

            // Add text label
            let text = if is_power { "VDD" } else { "VSS" };
            structure.elements.push(GdsiiElement::Text {
                layer: self.layers.text.0,
                datatype: self.layers.text.1,
                text: text.to_string(),
                position: (
                    ((min_x + 10.0) / self.db_unit) as i32,
                    (y / self.db_unit) as i32,
                ),
                height: (1.0 / self.db_unit) as i32,
            });

            y += rail_pitch;
            is_power = !is_power;
        }

        // Power straps on M2 (vertical)
        let strap_pitch = 50.0; // um
        let strap_width = 3.0; // um
        let (layer, datatype) = self.layers.metal[1]; // M2

        let mut x = min_x;
        is_power = true;

        while x <= max_x {
            let points = vec![
                ((x / self.db_unit) as i32, (min_y / self.db_unit) as i32),
                ((x / self.db_unit) as i32, (max_y / self.db_unit) as i32),
            ];

            structure.elements.push(GdsiiElement::Path {
                layer,
                datatype: if is_power { 0 } else { 1 },
                width: (strap_width / self.db_unit) as i32,
                points,
                pathtype: 0,
            });

            x += strap_pitch;
            is_power = !is_power;
        }

        Ok(())
    }

    /// Add die boundary
    fn add_die_boundary(
        &self,
        structure: &mut GdsiiStructure,
        placement: &Placement,
    ) -> Result<(), AsicError> {
        let (min_x, min_y, max_x, max_y) = self.get_die_bounds(placement)?;

        // Add boundary on a special layer
        let points = vec![
            ((min_x / self.db_unit) as i32, (min_y / self.db_unit) as i32),
            ((max_x / self.db_unit) as i32, (min_y / self.db_unit) as i32),
            ((max_x / self.db_unit) as i32, (max_y / self.db_unit) as i32),
            ((min_x / self.db_unit) as i32, (max_y / self.db_unit) as i32),
            ((min_x / self.db_unit) as i32, (min_y / self.db_unit) as i32),
        ];

        structure.elements.push(GdsiiElement::Boundary {
            layer: 0, // Boundary layer
            datatype: 0,
            points,
        });

        Ok(())
    }

    /// Create standard cell structures
    fn create_std_cell_structures(&self) -> Result<Vec<GdsiiStructure>, AsicError> {
        let std_cells = StandardCellLibrary::new();
        let mut structures = Vec::new();

        // Create a structure for each unique cell type
        let mut cell_types = HashSet::new();
        for name in std_cells.cells.keys() {
            cell_types.insert(name.clone());
        }

        for cell_type in cell_types {
            if let Some(cell_def) = std_cells.cells.get(&cell_type) {
                let mut structure = GdsiiStructure {
                    name: cell_type.clone(),
                    elements: Vec::new(),
                };

                // Add cell layout
                self.add_cell_layout(&mut structure, cell_def)?;

                structures.push(structure);
            }
        }

        Ok(structures)
    }

    /// Add cell layout to structure
    fn add_cell_layout(
        &self,
        structure: &mut GdsiiStructure,
        cell_def: &crate::sky130::CellDefinition,
    ) -> Result<(), AsicError> {
        // Add active regions
        for rect in &cell_def.layout.active_regions {
            let points = self.rect_to_points(rect);
            structure.elements.push(GdsiiElement::Boundary {
                layer: self.layers.active.0,
                datatype: self.layers.active.1,
                points,
            });
        }

        // Add poly gates
        for rect in &cell_def.layout.poly_regions {
            let points = self.rect_to_points(rect);
            structure.elements.push(GdsiiElement::Boundary {
                layer: self.layers.poly.0,
                datatype: self.layers.poly.1,
                points,
            });
        }

        // Add metal1 connections
        for rect in &cell_def.layout.metal1_regions {
            let points = self.rect_to_points(rect);
            structure.elements.push(GdsiiElement::Boundary {
                layer: self.layers.metal[0].0,
                datatype: self.layers.metal[0].1,
                points,
            });
        }

        // Add contacts
        for rect in &cell_def.layout.contact_regions {
            let points = self.rect_to_points(rect);
            structure.elements.push(GdsiiElement::Boundary {
                layer: self.layers.contact.0,
                datatype: self.layers.contact.1,
                points,
            });
        }

        // Add n-well if present
        if let Some(rect) = &cell_def.layout.nwell_region {
            let points = self.rect_to_points(rect);
            structure.elements.push(GdsiiElement::Boundary {
                layer: self.layers.nwell.0,
                datatype: self.layers.nwell.1,
                points,
            });
        }

        // Add pins as text labels
        for pin in &cell_def.pins {
            structure.elements.push(GdsiiElement::Text {
                layer: self.layers.text.0,
                datatype: self.layers.text.1,
                text: pin.name.clone(),
                position: (
                    (pin.position.0 / self.db_unit) as i32,
                    (pin.position.1 / self.db_unit) as i32,
                ),
                height: (0.5 / self.db_unit) as i32,
            });
        }

        Ok(())
    }

    /// Write GDSII stream to file
    fn write_stream(&self, stream: &GdsiiStream, path: &Path) -> Result<(), AsicError> {
        let file = File::create(path)
            .map_err(|e| AsicError::GdsError(format!("Failed to create file: {}", e)))?;
        let mut writer = BufWriter::new(file);

        // Write header
        self.write_header(&mut writer)?;

        // Write libraries
        for library in &stream.libraries {
            self.write_library(&mut writer, library)?;
        }

        Ok(())
    }

    /// Write GDSII header
    fn write_header<W: Write>(&self, writer: &mut W) -> Result<(), AsicError> {
        // HEADER record
        self.write_record(writer, RecordType::Header, DataType::TwoByteInt, &[0x0002])?;

        Ok(())
    }

    /// Write GDSII library
    fn write_library<W: Write>(
        &self,
        writer: &mut W,
        library: &GdsiiLibrary,
    ) -> Result<(), AsicError> {
        // BGNLIB record
        self.write_record(
            writer,
            RecordType::BgnLib,
            DataType::TwoByteInt,
            &self.current_timestamp(),
        )?;

        // LIBNAME record
        self.write_string_record(writer, RecordType::LibName, &library.name)?;

        // UNITS record (database unit and user unit)
        let units = [
            self.float_to_gds_real(self.db_unit / self.user_unit),
            self.float_to_gds_real(self.db_unit),
        ];
        self.write_record(
            writer,
            RecordType::Units,
            DataType::EightByteReal,
            &units.concat(),
        )?;

        // Write structures
        for structure in &library.structures {
            self.write_structure(writer, structure)?;
        }

        // ENDLIB record
        self.write_record(writer, RecordType::EndLib, DataType::NoData, &[])?;

        Ok(())
    }

    /// Write GDSII structure
    fn write_structure<W: Write>(
        &self,
        writer: &mut W,
        structure: &GdsiiStructure,
    ) -> Result<(), AsicError> {
        // BGNSTR record
        self.write_record(
            writer,
            RecordType::BgnStr,
            DataType::TwoByteInt,
            &self.current_timestamp(),
        )?;

        // STRNAME record
        self.write_string_record(writer, RecordType::StrName, &structure.name)?;

        // Write elements
        for element in &structure.elements {
            self.write_element(writer, element)?;
        }

        // ENDSTR record
        self.write_record(writer, RecordType::EndStr, DataType::NoData, &[])?;

        Ok(())
    }

    /// Write GDSII element
    fn write_element<W: Write>(
        &self,
        writer: &mut W,
        element: &GdsiiElement,
    ) -> Result<(), AsicError> {
        match element {
            GdsiiElement::Boundary {
                layer,
                datatype,
                points,
            } => {
                self.write_record(writer, RecordType::Boundary, DataType::NoData, &[])?;
                self.write_record(writer, RecordType::Layer, DataType::TwoByteInt, &[*layer])?;
                self.write_record(
                    writer,
                    RecordType::DataType,
                    DataType::TwoByteInt,
                    &[*datatype],
                )?;
                self.write_xy(writer, points)?;
                self.write_record(writer, RecordType::EndEl, DataType::NoData, &[])?;
            }
            GdsiiElement::Path {
                layer,
                datatype,
                width,
                points,
                pathtype,
            } => {
                self.write_record(writer, RecordType::Path, DataType::NoData, &[])?;
                self.write_record(writer, RecordType::Layer, DataType::TwoByteInt, &[*layer])?;
                self.write_record(
                    writer,
                    RecordType::DataType,
                    DataType::TwoByteInt,
                    &[*datatype],
                )?;
                self.write_record(
                    writer,
                    RecordType::PathType,
                    DataType::TwoByteInt,
                    &[*pathtype],
                )?;
                self.write_record(
                    writer,
                    RecordType::Width,
                    DataType::FourByteInt,
                    &[*width as i16],
                )?;
                self.write_xy(writer, points)?;
                self.write_record(writer, RecordType::EndEl, DataType::NoData, &[])?;
            }
            GdsiiElement::Sref {
                name,
                position,
                angle,
                mag,
                reflect,
            } => {
                self.write_record(writer, RecordType::Sref, DataType::NoData, &[])?;
                self.write_string_record(writer, RecordType::SName, name)?;

                if *angle != 0.0 || *mag != 1.0 || *reflect {
                    let strans = if *reflect { 0x8000u16 } else { 0 };
                    self.write_record(
                        writer,
                        RecordType::Strans,
                        DataType::TwoByteInt,
                        &[strans as i16],
                    )?;

                    if *mag != 1.0 {
                        let mag_data = self.float_to_gds_real(*mag);
                        self.write_record(
                            writer,
                            RecordType::Mag,
                            DataType::EightByteReal,
                            &mag_data,
                        )?;
                    }

                    if *angle != 0.0 {
                        let angle_data = self.float_to_gds_real(*angle);
                        self.write_record(
                            writer,
                            RecordType::Angle,
                            DataType::EightByteReal,
                            &angle_data,
                        )?;
                    }
                }

                self.write_xy(writer, &[*position])?;
                self.write_record(writer, RecordType::EndEl, DataType::NoData, &[])?;
            }
            GdsiiElement::Text {
                layer,
                datatype,
                text,
                position,
                height,
            } => {
                self.write_record(writer, RecordType::Text, DataType::NoData, &[])?;
                self.write_record(writer, RecordType::Layer, DataType::TwoByteInt, &[*layer])?;
                self.write_record(
                    writer,
                    RecordType::TextType,
                    DataType::TwoByteInt,
                    &[*datatype],
                )?;
                self.write_xy(writer, &[*position])?;
                self.write_string_record(writer, RecordType::String, text)?;
                self.write_record(writer, RecordType::EndEl, DataType::NoData, &[])?;
            }
            _ => {} // Handle other element types as needed
        }

        Ok(())
    }

    /// Write record to GDSII
    fn write_record<W: Write>(
        &self,
        writer: &mut W,
        record_type: RecordType,
        data_type: DataType,
        data: &[i16],
    ) -> Result<(), AsicError> {
        let length = 4 + data.len() * 2; // Header + data
        let header = [
            (length >> 8) as u8,
            (length & 0xFF) as u8,
            record_type as u8,
            data_type as u8,
        ];

        writer
            .write_all(&header)
            .map_err(|e| AsicError::GdsError(format!("Write error: {}", e)))?;

        for value in data {
            let bytes = value.to_be_bytes();
            writer
                .write_all(&bytes)
                .map_err(|e| AsicError::GdsError(format!("Write error: {}", e)))?;
        }

        Ok(())
    }

    /// Write string record
    fn write_string_record<W: Write>(
        &self,
        writer: &mut W,
        record_type: RecordType,
        text: &str,
    ) -> Result<(), AsicError> {
        let mut bytes = text.as_bytes().to_vec();
        #[allow(clippy::manual_is_multiple_of)]
        if bytes.len() % 2 != 0 {
            bytes.push(0); // Pad to even length
        }

        let length = 4 + bytes.len();
        let header = [
            (length >> 8) as u8,
            (length & 0xFF) as u8,
            record_type as u8,
            DataType::AsciiString as u8,
        ];

        writer
            .write_all(&header)
            .map_err(|e| AsicError::GdsError(format!("Write error: {}", e)))?;
        writer
            .write_all(&bytes)
            .map_err(|e| AsicError::GdsError(format!("Write error: {}", e)))?;

        Ok(())
    }

    /// Write XY coordinates
    fn write_xy<W: Write>(&self, writer: &mut W, points: &[(i32, i32)]) -> Result<(), AsicError> {
        let mut data = Vec::new();
        for (x, y) in points {
            data.push((x >> 16) as i16);
            data.push((x & 0xFFFF) as i16);
            data.push((y >> 16) as i16);
            data.push((y & 0xFFFF) as i16);
        }

        self.write_record(writer, RecordType::Xy, DataType::FourByteInt, &data)?;

        Ok(())
    }

    /// Convert float to GDSII 8-byte real format
    fn float_to_gds_real(&self, value: f64) -> Vec<i16> {
        // Simplified conversion - in production, use proper IEEE to GDSII conversion
        let bytes = value.to_be_bytes();
        let mut result = Vec::new();
        for chunk in bytes.chunks(2) {
            let val = ((chunk[0] as u16) << 8) | (chunk[1] as u16);
            result.push(val as i16);
        }
        result
    }

    /// Get current timestamp
    fn current_timestamp(&self) -> [i16; 6] {
        // Return dummy timestamp - in production, use actual time
        [2024, 1, 1, 0, 0, 0]
    }

    /// Convert rectangle to points
    fn rect_to_points(&self, rect: &crate::sky130::Rectangle) -> Vec<(i32, i32)> {
        let x1 = (rect.x1 / self.db_unit) as i32;
        let y1 = (rect.y1 / self.db_unit) as i32;
        let x2 = (rect.x2 / self.db_unit) as i32;
        let y2 = (rect.y2 / self.db_unit) as i32;

        vec![
            (x1, y1),
            (x2, y1),
            (x2, y2),
            (x1, y2),
            (x1, y1), // Close polygon
        ]
    }

    /// Get die bounds from placement
    fn get_die_bounds(&self, placement: &Placement) -> Result<(f64, f64, f64, f64), AsicError> {
        if placement.positions.is_empty() {
            return Err(AsicError::PlacementError("No placed cells".to_string()));
        }

        let min_x = placement
            .positions
            .values()
            .map(|p| p.0)
            .fold(f64::INFINITY, f64::min)
            - 10.0;
        let min_y = placement
            .positions
            .values()
            .map(|p| p.1)
            .fold(f64::INFINITY, f64::min)
            - 10.0;
        let max_x = placement
            .positions
            .values()
            .map(|p| p.0)
            .fold(0.0, f64::max)
            + 10.0;
        let max_y = placement
            .positions
            .values()
            .map(|p| p.1)
            .fold(0.0, f64::max)
            + 10.0;

        Ok((min_x, min_y, max_x, max_y))
    }

    /// Get metal layer mapping
    fn get_metal_layer(&self, level: usize) -> Result<(i16, i16), AsicError> {
        self.layers
            .metal
            .get(level)
            .copied()
            .ok_or_else(|| AsicError::GdsError(format!("Invalid metal layer: {}", level)))
    }

    /// Get via layer mapping
    fn get_via_layer(&self, from: usize, to: usize) -> Result<(i16, i16), AsicError> {
        let via_index = from.min(to);
        self.layers
            .via
            .get(via_index)
            .copied()
            .ok_or_else(|| AsicError::GdsError(format!("Invalid via layer: {}-{}", from, to)))
    }
}

impl LayerMap {
    /// SKY130 layer mapping
    pub fn sky130() -> Self {
        Self {
            nwell: (64, 20),
            pwell: (64, 13),
            active: (65, 20),
            poly: (66, 20),
            metal: vec![
                (68, 20), // Metal1
                (69, 20), // Metal2
                (70, 20), // Metal3
                (71, 20), // Metal4
                (72, 20), // Metal5
            ],
            via: vec![
                (68, 44), // Contact
                (69, 44), // Via1
                (70, 44), // Via2
                (71, 44), // Via3
                (72, 44), // Via4
            ],
            nplus: (93, 44),
            pplus: (94, 20),
            contact: (66, 44),
            pad: (76, 20),
            text: (83, 44),
        }
    }

    /// GF180 layer mapping
    pub fn gf180() -> Self {
        Self {
            nwell: (21, 0),
            pwell: (22, 0),
            active: (22, 0),
            poly: (30, 0),
            metal: vec![
                (34, 0), // Metal1
                (36, 0), // Metal2
                (42, 0), // Metal3
                (46, 0), // Metal4
                (81, 0), // Metal5
                (82, 0), // Metal6
            ],
            via: vec![
                (33, 0), // Contact
                (35, 0), // Via1
                (38, 0), // Via2
                (40, 0), // Via3
                (41, 0), // Via4
                (82, 0), // Via5
            ],
            nplus: (32, 0),
            pplus: (31, 0),
            contact: (33, 0),
            pad: (37, 0),
            text: (63, 0),
        }
    }

    /// IHP130 layer mapping
    pub fn ihp130() -> Self {
        Self {
            nwell: (31, 0),
            pwell: (32, 0),
            active: (1, 0),
            poly: (19, 0),
            metal: vec![
                (8, 0),  // Metal1
                (10, 0), // Metal2
                (30, 0), // Metal3
                (33, 0), // Metal4
                (34, 0), // Metal5
            ],
            via: vec![
                (7, 0),  // Contact
                (9, 0),  // Via1
                (29, 0), // Via2
                (51, 0), // Via3
                (52, 0), // Via4
            ],
            nplus: (3, 0),
            pplus: (5, 0),
            contact: (7, 0),
            pad: (39, 0),
            text: (63, 63),
        }
    }
}
