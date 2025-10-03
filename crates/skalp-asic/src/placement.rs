//! Native ASIC Placement Engine
//!
//! Implements analytical placement with simulated annealing optimization
//! for standard cell placement without external tools.

use crate::sky130::StandardCellLibrary;
use crate::{AsicError, DesignRules, Technology};
use nalgebra::{DMatrix, DVector};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Netlist representation
#[derive(Debug, Clone)]
pub struct Netlist {
    /// Cells in the netlist
    pub cells: Vec<StandardCell>,
    /// Nets connecting cells
    pub nets: Vec<Net>,
}

/// Standard cell instance
#[derive(Debug, Clone)]
pub struct StandardCell {
    /// Instance name
    pub name: String,
    /// Cell type
    pub cell_type: String,
    /// Cell area
    pub area: f64,
    /// Cell width
    pub width: f64,
    /// Cell height
    pub height: f64,
}

/// Net connecting cells
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Net {
    /// Net name
    pub name: String,
    /// Connected cell indices and pin names
    pub connections: Vec<(usize, String)>,
}

/// Placement result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Placement {
    /// Cell positions (x, y)
    pub cell_positions: Vec<(f64, f64)>,
    /// Cell orientations
    pub orientation: Vec<Orientation>,
    /// Placement rows
    pub rows: Vec<PlacementRow>,
    /// Mapping from instance name to position
    pub positions: HashMap<String, (f64, f64)>,
    /// Placed cells
    pub cells: Vec<CellInstance>,
}

/// Cell orientation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Orientation {
    R0,   // No rotation
    R90,  // 90 degree rotation
    R180, // 180 degree rotation
    R270, // 270 degree rotation
    MX,   // Mirror X
    MY,   // Mirror Y
}

/// Placement row
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlacementRow {
    /// Y coordinate
    pub y: f64,
    /// Row height
    pub height: f64,
    /// Site width
    pub site_width: f64,
}

/// Cell instance
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CellInstance {
    /// Instance name
    pub instance_name: String,
    /// Cell type
    pub cell_type: String,
}

/// Floorplan
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Floorplan {
    /// Die area (width, height)
    pub die_area: (f64, f64),
    /// Core area (width, height)
    pub core_area: (f64, f64),
    /// Power grid
    pub power_grid: PowerGrid,
    /// I/O ring
    pub io_ring: IoRing,
}

/// Power grid
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PowerGrid {
    /// VDD rails
    pub vdd_rails: Vec<PowerRail>,
    /// GND rails
    pub gnd_rails: Vec<PowerRail>,
}

/// Power rail
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PowerRail {
    /// Metal layer
    pub layer: usize,
    /// Rail width
    pub width: f64,
    /// Rail pitch
    pub pitch: f64,
}

/// I/O ring
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IoRing {
    /// I/O pads
    pub pads: Vec<IoPad>,
}

/// I/O pad
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IoPad {
    /// Pad name
    pub name: String,
    /// Pad position
    pub position: (f64, f64),
    /// Pad type
    pub pad_type: PadType,
}

/// Pad type
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PadType {
    Input,
    Output,
    Bidirectional,
    Power,
    Ground,
}

/// Move operation for simulated annealing
#[derive(Debug, Clone)]
enum MoveOperation {
    /// Swap two cells
    Swap(usize, usize),
    /// Displace cell by (dx, dy)
    Displace(usize, f64, f64),
}

/// Native ASIC placer
pub struct Placer {
    /// Design rules
    design_rules: DesignRules,
    /// Placement configuration
    config: PlacementConfig,
}

/// Placement configuration
#[derive(Debug, Clone)]
pub struct PlacementConfig {
    /// Target utilization (0.0 - 1.0)
    pub utilization: f64,
    /// Row height in tracks
    pub row_height: f64,
    /// Site width
    pub site_width: f64,
    /// Number of placement iterations
    pub iterations: usize,
    /// Temperature schedule for annealing
    pub temperature: TemperatureSchedule,
}

/// Temperature schedule for simulated annealing
#[derive(Debug, Clone)]
pub struct TemperatureSchedule {
    /// Initial temperature
    pub initial: f64,
    /// Final temperature
    pub final_temp: f64,
    /// Cooling rate
    pub cooling_rate: f64,
}

impl Default for PlacementConfig {
    fn default() -> Self {
        Self {
            utilization: 0.7,
            row_height: 2.72, // SKY130 standard row height
            site_width: 0.46, // SKY130 site width
            iterations: 10000,
            temperature: TemperatureSchedule {
                initial: 1000.0,
                final_temp: 1.0,
                cooling_rate: 0.95,
            },
        }
    }
}

impl Placer {
    /// Create a new placer
    pub fn new(design_rules: &DesignRules) -> Self {
        Self {
            design_rules: design_rules.clone(),
            config: PlacementConfig::default(),
        }
    }

    /// Place standard cells
    pub fn place(&self, netlist: &Netlist, floorplan: &Floorplan) -> Result<Placement, AsicError> {
        println!("   Starting analytical placement...");

        // Step 1: Initial placement using quadratic placement
        let initial_placement = self.quadratic_placement(netlist, floorplan)?;

        // Step 2: Legalize placement to valid sites
        let legal_placement = self.legalize_placement(&initial_placement, netlist, floorplan)?;

        // Step 3: Optimize with simulated annealing
        let optimized = self.simulated_annealing(legal_placement, netlist, floorplan)?;

        // Step 4: Final legalization and alignment
        let final_placement = self.final_legalization(&optimized, floorplan)?;

        println!(
            "   Placement complete: {} cells placed",
            final_placement.cell_positions.len()
        );

        Ok(final_placement)
    }

    /// Quadratic placement for initial solution
    fn quadratic_placement(
        &self,
        netlist: &Netlist,
        floorplan: &Floorplan,
    ) -> Result<Placement, AsicError> {
        let n = netlist.cells.len();

        // Build connectivity matrix (Laplacian)
        let mut laplacian = DMatrix::<f64>::zeros(n, n);
        let mut bx = DVector::<f64>::zeros(n);
        let mut by = DVector::<f64>::zeros(n);

        // Build Laplacian matrix from netlist connectivity
        for net in &netlist.nets {
            let connections = &net.connections;
            let weight = 1.0 / connections.len().max(2) as f64;

            for i in 0..connections.len() {
                for j in i + 1..connections.len() {
                    let ci = connections[i].0;
                    let cj = connections[j].0;

                    laplacian[(ci, ci)] += weight;
                    laplacian[(cj, cj)] += weight;
                    laplacian[(ci, cj)] -= weight;
                    laplacian[(cj, ci)] -= weight;
                }
            }
        }

        // Add fixed points (I/O pads) as boundary conditions
        for (i, cell) in netlist.cells.iter().enumerate() {
            if self.is_io_cell(&cell.cell_type) {
                // Fix I/O cells at the boundary
                let (x, y) = self.get_io_position(i, floorplan);
                laplacian[(i, i)] += 1000.0; // Large weight to fix position
                bx[i] += 1000.0 * x;
                by[i] += 1000.0 * y;
            }
        }

        // Solve the linear system for X coordinates
        let x_positions = self.solve_linear_system(&laplacian, &bx)?;

        // Solve for Y coordinates
        let y_positions = self.solve_linear_system(&laplacian, &by)?;

        // Create placement from solutions
        let mut cell_positions = Vec::new();
        let mut orientation = Vec::new();

        for i in 0..n {
            cell_positions.push((x_positions[i], y_positions[i]));
            orientation.push(Orientation::R0);
        }

        // Create placement rows
        let rows = self.create_placement_rows(floorplan);

        Ok(Placement {
            cell_positions: cell_positions.clone(),
            orientation,
            rows,
            positions: cell_positions
                .iter()
                .enumerate()
                .map(|(i, pos)| (netlist.cells[i].name.clone(), *pos))
                .collect(),
            cells: netlist
                .cells
                .iter()
                .map(|c| CellInstance {
                    instance_name: c.name.clone(),
                    cell_type: c.cell_type.clone(),
                })
                .collect(),
        })
    }

    /// Solve linear system using conjugate gradient
    fn solve_linear_system(
        &self,
        a: &DMatrix<f64>,
        b: &DVector<f64>,
    ) -> Result<Vec<f64>, AsicError> {
        let n = b.len();
        let mut x = DVector::<f64>::zeros(n);
        let mut r = b - a * &x;
        let mut p = r.clone();

        for _ in 0..n {
            let ap = a * &p;
            let alpha = r.dot(&r) / p.dot(&ap);
            x += alpha * &p;
            let r_new = &r - alpha * ap;

            if r_new.norm() < 1e-6 {
                break;
            }

            let beta = r_new.dot(&r_new) / r.dot(&r);
            p = &r_new + beta * p;
            r = r_new;
        }

        Ok(x.iter().copied().collect())
    }

    /// Legalize placement to valid sites
    fn legalize_placement(
        &self,
        placement: &Placement,
        netlist: &Netlist,
        floorplan: &Floorplan,
    ) -> Result<Placement, AsicError> {
        let mut legal_positions = Vec::new();
        let mut orientation = Vec::new();
        let mut occupied = HashMap::new();

        // Sort cells by X position for left-to-right legalization
        let mut cell_order: Vec<_> = (0..netlist.cells.len()).collect();
        cell_order.sort_by(|&a, &b| {
            placement.cell_positions[a]
                .0
                .partial_cmp(&placement.cell_positions[b].0)
                .unwrap()
        });

        // Place cells row by row
        for &cell_idx in &cell_order {
            let cell = &netlist.cells[cell_idx];
            let (desired_x, desired_y) = placement.cell_positions[cell_idx];

            // Find nearest valid site
            let (legal_x, legal_y, row_idx) =
                self.find_nearest_site(desired_x, desired_y, cell, &occupied, &placement.rows)?;

            // Mark sites as occupied
            let width_in_sites =
                (cell.area / self.config.row_height / self.config.site_width).ceil() as usize;
            for site in 0..width_in_sites {
                let site_x = legal_x + site as f64 * self.config.site_width;
                occupied.insert((row_idx, site_x.to_bits()), cell_idx);
            }

            legal_positions.push((legal_x, legal_y));

            // Determine orientation (flip every other row for power rail sharing)
            let orient = if row_idx % 2 == 0 {
                Orientation::R0
            } else {
                Orientation::MY // Mirror Y for alternating rows
            };
            orientation.push(orient);
        }

        Ok(Placement {
            cell_positions: legal_positions.clone(),
            orientation,
            rows: placement.rows.clone(),
            positions: legal_positions
                .iter()
                .enumerate()
                .map(|(i, pos)| (netlist.cells[i].name.clone(), *pos))
                .collect(),
            cells: netlist
                .cells
                .iter()
                .map(|c| CellInstance {
                    instance_name: c.name.clone(),
                    cell_type: c.cell_type.clone(),
                })
                .collect(),
        })
    }

    /// Find nearest valid site for a cell
    fn find_nearest_site(
        &self,
        x: f64,
        y: f64,
        cell: &StandardCell,
        occupied: &HashMap<(usize, u64), usize>,
        rows: &[PlacementRow],
    ) -> Result<(f64, f64, usize), AsicError> {
        // Find nearest row
        let row_idx = ((y / self.config.row_height).round() as usize).min(rows.len() - 1);
        let row = &rows[row_idx];

        // Snap to site grid
        let site_x = (x / self.config.site_width).round() * self.config.site_width;
        let width_in_sites =
            (cell.area / self.config.row_height / self.config.site_width).ceil() as usize;

        // Search for free space
        let mut best_x = site_x;
        let mut min_displacement = f64::INFINITY;

        for offset in 0..100 {
            for &direction in &[1.0, -1.0] {
                let test_x = site_x + offset as f64 * direction * self.config.site_width;

                // Check if position is valid and unoccupied
                if test_x >= 0.0 && self.is_position_free(test_x, row_idx, width_in_sites, occupied)
                {
                    let displacement = (test_x - x).abs();
                    if displacement < min_displacement {
                        min_displacement = displacement;
                        best_x = test_x;
                    }
                }
            }

            if min_displacement < f64::INFINITY {
                break;
            }
        }

        Ok((best_x, row.y, row_idx))
    }

    /// Check if position is free
    fn is_position_free(
        &self,
        x: f64,
        row: usize,
        width: usize,
        occupied: &HashMap<(usize, u64), usize>,
    ) -> bool {
        for site in 0..width {
            let site_x = x + site as f64 * self.config.site_width;
            if occupied.contains_key(&(row, site_x.to_bits())) {
                return false;
            }
        }
        true
    }

    /// Optimize placement with simulated annealing
    fn simulated_annealing(
        &self,
        mut placement: Placement,
        netlist: &Netlist,
        floorplan: &Floorplan,
    ) -> Result<Placement, AsicError> {
        use rand::Rng;

        let mut rng = rand::thread_rng();
        let mut temperature = self.config.temperature.initial;
        let mut best_cost = self.calculate_cost(&placement, netlist);
        let mut current_cost = best_cost;

        println!("   Starting simulated annealing optimization...");
        println!("   Initial cost: {:.2}", best_cost);

        for iteration in 0..self.config.iterations {
            // Generate a move
            let move_op = self.generate_move(&placement, netlist);

            // Apply move temporarily
            let new_placement = self.apply_move(&placement, &move_op)?;
            let new_cost = self.calculate_cost(&new_placement, netlist);

            // Accept or reject move
            let delta = new_cost - current_cost;
            let accept = if delta < 0.0 {
                true
            } else {
                let probability = (-delta / temperature).exp();
                rng.gen::<f64>() < probability
            };

            if accept {
                placement = new_placement;
                current_cost = new_cost;

                if current_cost < best_cost {
                    best_cost = current_cost;
                }
            }

            // Cool down
            if iteration % 100 == 0 {
                temperature *= self.config.temperature.cooling_rate;

                if iteration % 1000 == 0 {
                    println!(
                        "   Iteration {}: cost = {:.2}, temp = {:.2}",
                        iteration, current_cost, temperature
                    );
                }
            }

            if temperature < self.config.temperature.final_temp {
                break;
            }
        }

        println!("   Annealing complete. Final cost: {:.2}", best_cost);

        Ok(placement)
    }

    /// Calculate placement cost (HPWL - Half Perimeter Wire Length)
    fn calculate_cost(&self, placement: &Placement, netlist: &Netlist) -> f64 {
        let mut total_hpwl = 0.0;

        for net in &netlist.nets {
            if net.connections.is_empty() {
                continue;
            }

            let mut min_x = f64::INFINITY;
            let mut max_x = f64::NEG_INFINITY;
            let mut min_y = f64::INFINITY;
            let mut max_y = f64::NEG_INFINITY;

            for conn in &net.connections {
                let (cell_idx, _pin) = conn;
                let (x, y) = placement.cell_positions[*cell_idx];
                min_x = min_x.min(x);
                max_x = max_x.max(x);
                min_y = min_y.min(y);
                max_y = max_y.max(y);
            }

            total_hpwl += (max_x - min_x) + (max_y - min_y);
        }

        total_hpwl
    }

    /// Generate a random move for annealing
    fn generate_move(&self, placement: &Placement, netlist: &Netlist) -> MoveOperation {
        use rand::Rng;
        let mut rng = rand::thread_rng();

        // Choose move type
        if rng.gen::<f64>() < 0.7 {
            // Swap two cells
            let cell1 = rng.gen_range(0..netlist.cells.len());
            let cell2 = rng.gen_range(0..netlist.cells.len());
            MoveOperation::Swap(cell1, cell2)
        } else {
            // Displace a cell
            let cell = rng.gen_range(0..netlist.cells.len());
            let dx = rng.gen_range(-5..=5) as f64 * self.config.site_width;
            let dy = rng.gen_range(-2..=2) as f64 * self.config.row_height;
            MoveOperation::Displace(cell, dx, dy)
        }
    }

    /// Apply a move operation
    fn apply_move(
        &self,
        placement: &Placement,
        move_op: &MoveOperation,
    ) -> Result<Placement, AsicError> {
        let mut new_placement = placement.clone();

        match move_op {
            MoveOperation::Swap(i, j) => {
                new_placement.cell_positions.swap(*i, *j);
                new_placement.orientation.swap(*i, *j);
            }
            MoveOperation::Displace(i, dx, dy) => {
                let (x, y) = new_placement.cell_positions[*i];
                new_placement.cell_positions[*i] = (x + dx, y + dy);
            }
        }

        Ok(new_placement)
    }

    /// Final legalization and alignment
    fn final_legalization(
        &self,
        placement: &Placement,
        floorplan: &Floorplan,
    ) -> Result<Placement, AsicError> {
        let mut final_placement = placement.clone();

        // Ensure all cells are on valid sites
        for i in 0..final_placement.cell_positions.len() {
            let (x, y) = final_placement.cell_positions[i];

            // Snap to site grid
            let site_x = (x / self.config.site_width).round() * self.config.site_width;
            let row_y = (y / self.config.row_height).round() * self.config.row_height;

            final_placement.cell_positions[i] = (site_x, row_y);
        }

        Ok(final_placement)
    }

    /// Check if a cell is an I/O cell
    fn is_io_cell(&self, cell_type: &str) -> bool {
        cell_type.contains("IO") || cell_type.contains("PAD")
    }

    /// Get I/O position for boundary cells
    fn get_io_position(&self, index: usize, floorplan: &Floorplan) -> (f64, f64) {
        // Place I/O cells around the perimeter
        let total_ios = floorplan.io_ring.pads.len();
        let side = index * 4 / total_ios;
        let position_on_side = index % (total_ios / 4);

        match side {
            0 => (position_on_side as f64 * 10.0, 0.0), // Bottom
            1 => (floorplan.core_area.0, position_on_side as f64 * 10.0), // Right
            2 => (position_on_side as f64 * 10.0, floorplan.core_area.1), // Top
            _ => (0.0, position_on_side as f64 * 10.0), // Left
        }
    }

    /// Create placement rows
    fn create_placement_rows(&self, floorplan: &Floorplan) -> Vec<PlacementRow> {
        let num_rows = (floorplan.core_area.1 / self.config.row_height) as usize;
        let mut rows = Vec::new();

        for i in 0..num_rows {
            rows.push(PlacementRow {
                y: i as f64 * self.config.row_height,
                height: self.config.row_height,
                site_width: self.config.site_width,
            });
        }

        rows
    }
}

/// Floorplanner for creating initial floorplan
pub struct Floorplanner {
    design_rules: DesignRules,
}

impl Floorplanner {
    /// Create a new floorplanner
    pub fn new(design_rules: &DesignRules) -> Self {
        Self {
            design_rules: design_rules.clone(),
        }
    }

    /// Create floorplan from netlist
    pub fn create_floorplan(&self, netlist: &Netlist) -> Result<Floorplan, AsicError> {
        // Calculate required area
        let total_cell_area: f64 = netlist.cells.iter().map(|c| c.area).sum();
        let target_utilization = 0.7;
        let required_area = total_cell_area / target_utilization;

        // Create square die
        let die_size = required_area.sqrt() * 1.1; // 10% margin
        let core_size = die_size * 0.9; // Core is 90% of die

        // Create power grid
        let power_grid = self.create_power_grid(die_size);

        // Create I/O ring
        let io_ring = self.create_io_ring(die_size, netlist);

        Ok(Floorplan {
            die_area: (die_size, die_size),
            core_area: (core_size, core_size),
            power_grid,
            io_ring,
        })
    }

    /// Create power grid
    fn create_power_grid(&self, die_size: f64) -> PowerGrid {
        PowerGrid {
            vdd_rails: vec![
                PowerRail {
                    layer: 1,
                    width: 0.48,
                    pitch: 10.0,
                },
                PowerRail {
                    layer: 2,
                    width: 0.48,
                    pitch: 20.0,
                },
                PowerRail {
                    layer: 3,
                    width: 0.96,
                    pitch: 40.0,
                },
            ],
            gnd_rails: vec![
                PowerRail {
                    layer: 1,
                    width: 0.48,
                    pitch: 10.0,
                },
                PowerRail {
                    layer: 2,
                    width: 0.48,
                    pitch: 20.0,
                },
                PowerRail {
                    layer: 3,
                    width: 0.96,
                    pitch: 40.0,
                },
            ],
        }
    }

    /// Create I/O ring
    fn create_io_ring(&self, die_size: f64, netlist: &Netlist) -> IoRing {
        let mut pads = Vec::new();

        // Simplified - create pads for primary I/Os
        let num_pads = 40; // Example
        let pad_pitch = die_size * 4.0 / num_pads as f64;

        for i in 0..num_pads {
            let (x, y) = match i * 4 / num_pads {
                0 => (i as f64 * pad_pitch, 0.0),                         // Bottom
                1 => (die_size, (i % (num_pads / 4)) as f64 * pad_pitch), // Right
                2 => ((num_pads - i) as f64 * pad_pitch, die_size),       // Top
                _ => (0.0, (num_pads - i) as f64 * pad_pitch),            // Left
            };

            pads.push(IoPad {
                name: format!("pad_{}", i),
                position: (x, y),
                pad_type: if i % 3 == 0 {
                    PadType::Input
                } else if i % 3 == 1 {
                    PadType::Output
                } else {
                    PadType::Bidirectional
                },
            });
        }

        IoRing { pads }
    }
}

// Clone derives are handled by derive macros above
