//! Technology mapping for different targets
//!
//! Maps generic gates to technology-specific implementations

use crate::lir::{Gate, GateType, Lir};
use crate::technology::{Technology, TechnologyKind};
use std::collections::HashMap;

/// Technology mapper
pub struct TechMapper {
    /// Target technology
    technology: Technology,
    /// Mapping rules
    rules: HashMap<GateType, MappingRule>,
}

/// Mapping rule for a gate type
#[derive(Debug, Clone)]
pub struct MappingRule {
    /// Target primitive name
    pub primitive: String,
    /// Area cost
    pub area: f64,
    /// Delay
    pub delay: f64,
    /// Power
    pub power: f64,
}

impl TechMapper {
    /// Create a mapper for a specific technology
    pub fn new(technology: Technology) -> Self {
        let rules = match technology.kind {
            TechnologyKind::FPGA => Self::fpga_rules(),
            TechnologyKind::ASIC => Self::asic_rules(),
            TechnologyKind::Generic => Self::generic_rules(),
        };

        Self { technology, rules }
    }

    /// FPGA mapping rules (LUT-based)
    fn fpga_rules() -> HashMap<GateType, MappingRule> {
        let mut rules = HashMap::new();

        // Map basic gates to LUTs
        rules.insert(
            GateType::And,
            MappingRule {
                primitive: "LUT2".to_string(),
                area: 1.0,
                delay: 0.5,
                power: 0.1,
            },
        );

        rules.insert(
            GateType::Or,
            MappingRule {
                primitive: "LUT2".to_string(),
                area: 1.0,
                delay: 0.5,
                power: 0.1,
            },
        );

        rules.insert(
            GateType::Not,
            MappingRule {
                primitive: "LUT1".to_string(),
                area: 0.5,
                delay: 0.3,
                power: 0.05,
            },
        );

        rules.insert(
            GateType::Xor,
            MappingRule {
                primitive: "LUT2".to_string(),
                area: 1.0,
                delay: 0.6,
                power: 0.12,
            },
        );

        rules.insert(
            GateType::DFF,
            MappingRule {
                primitive: "FDRE".to_string(), // Xilinx flip-flop
                area: 2.0,
                delay: 1.0,
                power: 0.2,
            },
        );

        rules.insert(
            GateType::Buffer,
            MappingRule {
                primitive: "BUF".to_string(),
                area: 0.5,
                delay: 0.2,
                power: 0.05,
            },
        );

        rules
    }

    /// ASIC mapping rules (standard cell)
    fn asic_rules() -> HashMap<GateType, MappingRule> {
        let mut rules = HashMap::new();

        rules.insert(
            GateType::And,
            MappingRule {
                primitive: "AND2X1".to_string(),
                area: 2.0,
                delay: 0.05,
                power: 0.01,
            },
        );

        rules.insert(
            GateType::Or,
            MappingRule {
                primitive: "OR2X1".to_string(),
                area: 2.0,
                delay: 0.05,
                power: 0.01,
            },
        );

        rules.insert(
            GateType::Not,
            MappingRule {
                primitive: "INVX1".to_string(),
                area: 1.0,
                delay: 0.03,
                power: 0.005,
            },
        );

        rules.insert(
            GateType::Nand,
            MappingRule {
                primitive: "NAND2X1".to_string(),
                area: 1.5,
                delay: 0.04,
                power: 0.008,
            },
        );

        rules.insert(
            GateType::Nor,
            MappingRule {
                primitive: "NOR2X1".to_string(),
                area: 1.5,
                delay: 0.04,
                power: 0.008,
            },
        );

        rules.insert(
            GateType::Xor,
            MappingRule {
                primitive: "XOR2X1".to_string(),
                area: 3.0,
                delay: 0.06,
                power: 0.012,
            },
        );

        rules.insert(
            GateType::DFF,
            MappingRule {
                primitive: "DFFX1".to_string(),
                area: 6.0,
                delay: 0.1,
                power: 0.02,
            },
        );

        rules.insert(
            GateType::Buffer,
            MappingRule {
                primitive: "BUFX1".to_string(),
                area: 1.0,
                delay: 0.02,
                power: 0.005,
            },
        );

        rules
    }

    /// Generic mapping rules
    fn generic_rules() -> HashMap<GateType, MappingRule> {
        let mut rules = HashMap::new();

        for gate_type in &[
            GateType::And,
            GateType::Or,
            GateType::Not,
            GateType::Nand,
            GateType::Nor,
            GateType::Xor,
            GateType::Xnor,
            GateType::Buffer,
            GateType::DFF,
            GateType::Latch,
        ] {
            rules.insert(
                gate_type.clone(),
                MappingRule {
                    primitive: format!("{:?}", gate_type),
                    area: 1.0,
                    delay: 1.0,
                    power: 1.0,
                },
            );
        }

        rules
    }

    /// Map LIR to technology-specific gates
    pub fn map(&self, lir: &mut Lir) -> MappingReport {
        let mut report = MappingReport::new();

        for gate in &mut lir.gates {
            if let Some(rule) = self.rules.get(&gate.gate_type) {
                // Update gate with technology-specific info
                report.mapped_gates += 1;
                report.total_area += rule.area;
                report.total_power += rule.power;
                report.max_delay = report.max_delay.max(rule.delay);

                // Record mapping
                report.mappings.push(GateMapping {
                    original: gate.id.clone(),
                    gate_type: gate.gate_type.clone(),
                    primitive: rule.primitive.clone(),
                    area: rule.area,
                    delay: rule.delay,
                    power: rule.power,
                });
            } else {
                report.unmapped_gates += 1;
            }
        }

        report
    }

    /// Optimize for area (technology-specific)
    pub fn optimize_area(&self, lir: &mut Lir) {
        // FPGA: Pack logic into LUTs
        if matches!(self.technology.kind, TechnologyKind::FPGA) {
            self.pack_luts(lir);
        }

        // ASIC: Use complex gates (AOI, OAI)
        if matches!(self.technology.kind, TechnologyKind::ASIC) {
            self.use_complex_gates(lir);
        }
    }

    /// Pack small gates into LUTs (FPGA optimization)
    fn pack_luts(&self, lir: &mut Lir) {
        // Find chains of logic that can fit in a single LUT
        // This is simplified - real packing is more complex

        let mut gates_to_remove = Vec::new();
        let mut new_gates = Vec::new();

        // Look for AND-OR patterns that can be packed
        for i in 0..lir.gates.len() {
            if lir.gates[i].gate_type == GateType::And {
                for j in 0..lir.gates.len() {
                    if lir.gates[j].gate_type == GateType::Or {
                        // Check if OR takes AND output
                        if lir.gates[j].inputs.contains(&lir.gates[i].outputs[0]) {
                            // Can pack into a single LUT3
                            gates_to_remove.push(lir.gates[i].id.clone());
                            gates_to_remove.push(lir.gates[j].id.clone());

                            // Create packed LUT
                            let mut inputs = lir.gates[i].inputs.clone();
                            for input in &lir.gates[j].inputs {
                                if input != &lir.gates[i].outputs[0] {
                                    inputs.push(input.clone());
                                }
                            }

                            new_gates.push(Gate {
                                id: format!("lut3_{}", i),
                                gate_type: GateType::Buffer, // Represents LUT3
                                inputs,
                                outputs: lir.gates[j].outputs.clone(),
                            });
                            break;
                        }
                    }
                }
            }
        }

        // Apply packing
        lir.gates.retain(|g| !gates_to_remove.contains(&g.id));
        lir.gates.extend(new_gates);
    }

    /// Use complex gates for ASIC
    fn use_complex_gates(&self, lir: &mut Lir) {
        // Look for patterns that map to AOI/OAI gates
        // This is simplified - real synthesis uses pattern matching

        let mut gates_to_remove = Vec::new();
        let mut new_gates = Vec::new();

        // Look for (A AND B) OR C pattern -> AOI21
        for i in 0..lir.gates.len() {
            if lir.gates[i].gate_type == GateType::And {
                for j in 0..lir.gates.len() {
                    if lir.gates[j].gate_type == GateType::Or {
                        if lir.gates[j].inputs.contains(&lir.gates[i].outputs[0]) {
                            // Found AOI pattern
                            gates_to_remove.push(lir.gates[i].id.clone());
                            gates_to_remove.push(lir.gates[j].id.clone());

                            // Create AOI gate (represented as Buffer for simplicity)
                            let mut inputs = lir.gates[i].inputs.clone();
                            for input in &lir.gates[j].inputs {
                                if input != &lir.gates[i].outputs[0] {
                                    inputs.push(input.clone());
                                }
                            }

                            new_gates.push(Gate {
                                id: format!("aoi21_{}", i),
                                gate_type: GateType::Buffer, // Represents AOI21
                                inputs,
                                outputs: lir.gates[j].outputs.clone(),
                            });
                            break;
                        }
                    }
                }
            }
        }

        // Apply complex gate mapping
        lir.gates.retain(|g| !gates_to_remove.contains(&g.id));
        lir.gates.extend(new_gates);
    }
}

/// Mapping report
#[derive(Debug)]
pub struct MappingReport {
    /// Number of mapped gates
    pub mapped_gates: usize,
    /// Number of unmapped gates
    pub unmapped_gates: usize,
    /// Total area
    pub total_area: f64,
    /// Total power
    pub total_power: f64,
    /// Maximum delay
    pub max_delay: f64,
    /// Individual gate mappings
    pub mappings: Vec<GateMapping>,
}

impl MappingReport {
    fn new() -> Self {
        Self {
            mapped_gates: 0,
            unmapped_gates: 0,
            total_area: 0.0,
            total_power: 0.0,
            max_delay: 0.0,
            mappings: Vec::new(),
        }
    }

    /// Print the report
    pub fn print(&self) {
        println!("=== Technology Mapping Report ===");
        println!("Mapped gates: {}", self.mapped_gates);
        println!("Unmapped gates: {}", self.unmapped_gates);
        println!("Total area: {:.2}", self.total_area);
        println!("Total power: {:.2} mW", self.total_power);
        println!("Critical path delay: {:.2} ns", self.max_delay);
    }
}

/// Individual gate mapping
#[derive(Debug)]
pub struct GateMapping {
    /// Original gate ID
    pub original: String,
    /// Gate type
    pub gate_type: GateType,
    /// Mapped primitive
    pub primitive: String,
    /// Area cost
    pub area: f64,
    /// Delay
    pub delay: f64,
    /// Power
    pub power: f64,
}
