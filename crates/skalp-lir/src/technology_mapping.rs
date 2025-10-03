use crate::lir::{Gate, GateType, Lir, Net};
use std::collections::HashMap;

/// Technology mapping for different target platforms
pub struct TechnologyMapper {
    /// Target technology
    pub target: TechnologyTarget,
    /// Resource utilization tracking
    pub resource_usage: ResourceUsage,
}

/// Target technology platforms
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TechnologyTarget {
    /// Generic gates (no specific technology)
    Generic,
    /// FPGA with LUTs (Look-Up Tables)
    FpgaLut4, // 4-input LUTs
    FpgaLut6, // 6-input LUTs
    /// ASIC with standard cells
    AsicStandardCell,
}

/// Resource utilization tracking
#[derive(Debug, Clone, Default)]
pub struct ResourceUsage {
    /// Number of LUTs used (FPGA)
    pub luts: usize,
    /// Number of flip-flops used
    pub flip_flops: usize,
    /// Number of DSP blocks used
    pub dsp_blocks: usize,
    /// Number of memory blocks used
    pub memory_blocks: usize,
    /// Estimated area (technology units)
    pub area: f64,
}

/// Result of technology mapping
#[derive(Debug, Clone)]
pub struct TechnologyMappingResult {
    /// Mapped LIR with technology-specific gates
    pub mapped_lir: Lir,
    /// Resource utilization
    pub resource_usage: ResourceUsage,
    /// Mapping efficiency (0.0 to 1.0)
    pub efficiency: f64,
    /// Technology-specific notes
    pub notes: Vec<String>,
}

impl TechnologyMapper {
    /// Create a new technology mapper
    pub fn new(target: TechnologyTarget) -> Self {
        Self {
            target,
            resource_usage: ResourceUsage::default(),
        }
    }

    /// Map LIR to target technology
    pub fn map(&mut self, lir: &Lir) -> TechnologyMappingResult {
        let mut mapped_lir = lir.clone();
        let mut notes = Vec::new();

        match self.target {
            TechnologyTarget::Generic => {
                // No mapping needed for generic
                notes.push("Generic technology - no mapping required".to_string());
            }
            TechnologyTarget::FpgaLut4 => {
                self.map_to_fpga_lut4(&mut mapped_lir, &mut notes);
            }
            TechnologyTarget::FpgaLut6 => {
                self.map_to_fpga_lut6(&mut mapped_lir, &mut notes);
            }
            TechnologyTarget::AsicStandardCell => {
                self.map_to_asic_standard_cells(&mut mapped_lir, &mut notes);
            }
        }

        let efficiency = self.calculate_efficiency(&mapped_lir);

        TechnologyMappingResult {
            mapped_lir,
            resource_usage: self.resource_usage.clone(),
            efficiency,
            notes,
        }
    }

    /// Map to FPGA with 4-input LUTs
    fn map_to_fpga_lut4(&mut self, lir: &mut Lir, notes: &mut Vec<String>) {
        let mut lut_count = 0;
        let mut ff_count = 0;

        for gate in &mut lir.gates {
            match gate.gate_type {
                // Simple gates can be packed into LUTs
                GateType::And
                | GateType::Or
                | GateType::Not
                | GateType::Xor
                | GateType::Nand
                | GateType::Nor
                | GateType::Xnor => {
                    if gate.inputs.len() <= 4 {
                        // Fits in one LUT4
                        gate.gate_type = GateType::Buffer; // Placeholder for LUT4
                        lut_count += 1;
                    } else {
                        // Need multiple LUTs - decompose
                        notes.push(format!("Gate {} requires decomposition for LUT4", gate.id));
                        lut_count += 2; // Rough estimate
                    }
                }
                // Flip-flops map directly
                GateType::DFF => {
                    ff_count += 1;
                }
                // Buffers can often be optimized away in FPGA
                GateType::Buffer => {
                    // May not need a LUT at all
                    notes.push(format!("Buffer {} may be optimized away", gate.id));
                }
                _ => {
                    lut_count += 1; // Default assumption
                }
            }
        }

        self.resource_usage.luts = lut_count;
        self.resource_usage.flip_flops = ff_count;
        self.resource_usage.area = (lut_count as f64 * 1.0) + (ff_count as f64 * 0.5);

        notes.push(format!(
            "FPGA LUT4 mapping: {} LUTs, {} FFs",
            lut_count, ff_count
        ));
    }

    /// Map to FPGA with 6-input LUTs
    fn map_to_fpga_lut6(&mut self, lir: &mut Lir, notes: &mut Vec<String>) {
        let mut lut_count = 0;
        let mut ff_count = 0;

        for gate in &mut lir.gates {
            match gate.gate_type {
                // More gates can fit in LUT6
                GateType::And
                | GateType::Or
                | GateType::Not
                | GateType::Xor
                | GateType::Nand
                | GateType::Nor
                | GateType::Xnor => {
                    if gate.inputs.len() <= 6 {
                        // Fits in one LUT6
                        gate.gate_type = GateType::Buffer; // Placeholder for LUT6
                        lut_count += 1;
                    } else {
                        // Still need decomposition for very wide gates
                        lut_count += 2;
                    }
                }
                GateType::DFF => {
                    ff_count += 1;
                }
                GateType::Buffer => {
                    // May be optimized away
                }
                _ => {
                    lut_count += 1;
                }
            }
        }

        self.resource_usage.luts = lut_count;
        self.resource_usage.flip_flops = ff_count;
        self.resource_usage.area = (lut_count as f64 * 1.2) + (ff_count as f64 * 0.5);

        notes.push(format!(
            "FPGA LUT6 mapping: {} LUTs, {} FFs",
            lut_count, ff_count
        ));
    }

    /// Map to ASIC standard cells
    fn map_to_asic_standard_cells(&mut self, lir: &mut Lir, notes: &mut Vec<String>) {
        let mut area = 0.0;
        let mut ff_count = 0;

        // Standard cell area estimates (normalized units)
        let gate_areas: HashMap<GateType, f64> = [
            (GateType::And, 1.0),
            (GateType::Or, 1.0),
            (GateType::Not, 0.5),
            (GateType::Nand, 0.8),
            (GateType::Nor, 0.8),
            (GateType::Xor, 1.5),
            (GateType::Xnor, 1.5),
            (GateType::Buffer, 0.5),
            (GateType::DFF, 2.0),
        ]
        .iter()
        .cloned()
        .collect();

        for gate in &lir.gates {
            if let Some(&gate_area) = gate_areas.get(&gate.gate_type) {
                area += gate_area;
                if matches!(gate.gate_type, GateType::DFF) {
                    ff_count += 1;
                }
            } else {
                area += 1.0; // Default area
            }
        }

        self.resource_usage.area = area;
        self.resource_usage.flip_flops = ff_count;

        notes.push(format!(
            "ASIC standard cell mapping: {:.1} area units, {} FFs",
            area, ff_count
        ));
    }

    /// Calculate mapping efficiency
    fn calculate_efficiency(&self, lir: &Lir) -> f64 {
        if lir.gates.is_empty() {
            return 1.0;
        }

        match self.target {
            TechnologyTarget::FpgaLut4 | TechnologyTarget::FpgaLut6 => {
                // For FPGA, efficiency is LUT utilization
                let total_inputs: usize = lir.gates.iter().map(|g| g.inputs.len()).sum();
                let max_lut_inputs = match self.target {
                    TechnologyTarget::FpgaLut4 => 4,
                    TechnologyTarget::FpgaLut6 => 6,
                    _ => 4,
                };
                let theoretical_luts = (total_inputs + max_lut_inputs - 1) / max_lut_inputs;
                if theoretical_luts == 0 {
                    1.0
                } else {
                    theoretical_luts as f64 / self.resource_usage.luts as f64
                }
            }
            TechnologyTarget::AsicStandardCell => {
                // For ASIC, efficiency is area utilization
                let ideal_area = lir.gates.len() as f64 * 0.8; // Ideal standard cell area
                if self.resource_usage.area == 0.0 {
                    1.0
                } else {
                    ideal_area / self.resource_usage.area
                }
            }
            TechnologyTarget::Generic => 1.0,
        }
    }

    /// Get target-specific optimization recommendations
    pub fn get_optimization_recommendations(&self, lir: &Lir) -> Vec<String> {
        let mut recommendations = Vec::new();

        match self.target {
            TechnologyTarget::FpgaLut4 => {
                // Look for wide gates that need decomposition
                for gate in &lir.gates {
                    if gate.inputs.len() > 4 {
                        recommendations.push(format!(
                            "Gate {} has {} inputs, consider decomposition for LUT4",
                            gate.id,
                            gate.inputs.len()
                        ));
                    }
                }
            }
            TechnologyTarget::FpgaLut6 => {
                // Look for opportunities to pack more logic
                let simple_gates = lir
                    .gates
                    .iter()
                    .filter(|g| {
                        matches!(g.gate_type, GateType::And | GateType::Or | GateType::Not)
                            && g.inputs.len() < 3
                    })
                    .count();

                if simple_gates > 2 {
                    recommendations.push(format!(
                        "Found {} simple gates that could be packed together in LUT6",
                        simple_gates
                    ));
                }
            }
            TechnologyTarget::AsicStandardCell => {
                // Look for area optimization opportunities
                let buffer_count = lir
                    .gates
                    .iter()
                    .filter(|g| matches!(g.gate_type, GateType::Buffer))
                    .count();

                if buffer_count > 0 {
                    recommendations.push(format!(
                        "Found {} buffers that may be eliminated in ASIC",
                        buffer_count
                    ));
                }
            }
            TechnologyTarget::Generic => {
                recommendations
                    .push("Use specific technology target for detailed optimization".to_string());
            }
        }

        recommendations
    }
}
