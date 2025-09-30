use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
pub struct SirModule {
    pub name: String,
    pub inputs: Vec<SirPort>,
    pub outputs: Vec<SirPort>,
    pub signals: Vec<SirSignal>,
    pub combinational_nodes: Vec<SirNode>,
    pub sequential_nodes: Vec<SirNode>,
    pub state_elements: HashMap<String, StateElement>,
    pub clock_domains: HashMap<String, ClockDomain>,
}

#[derive(Debug, Clone)]
pub struct SirPort {
    pub name: String,
    pub width: usize,
    pub direction: PortDirection,
    pub clock_domain: Option<String>,
}

#[derive(Debug, Clone)]
pub enum PortDirection {
    Input,
    Output,
}

#[derive(Debug, Clone)]
pub struct SirSignal {
    pub name: String,
    pub width: usize,
    pub driver_node: Option<usize>,
    pub fanout_nodes: Vec<usize>,
    pub is_state: bool,
}

#[derive(Debug, Clone)]
pub struct SirNode {
    pub id: usize,
    pub kind: SirNodeKind,
    pub inputs: Vec<SignalRef>,
    pub outputs: Vec<SignalRef>,
    pub clock_domain: Option<String>,
}

#[derive(Debug, Clone)]
pub enum SirNodeKind {
    // Combinational operations
    BinaryOp(BinaryOperation),
    UnaryOp(UnaryOperation),
    Mux,
    Concat,
    Slice { start: usize, end: usize },
    Constant { value: u64, width: usize },
    SignalRef { signal: String }, // Read from signal/state

    // Sequential operations
    FlipFlop { clock_edge: ClockEdge },
    Latch { enable: SignalRef },
    Memory { depth: usize, width: usize },

    // Special nodes
    ClockGate,
    Reset,
}

#[derive(Debug, Clone)]
pub enum BinaryOperation {
    Add, Sub, Mul, Div, Mod,
    And, Or, Xor,
    Eq, Neq, Lt, Lte, Gt, Gte,
    Shl, Shr,
}

#[derive(Debug, Clone)]
pub enum UnaryOperation {
    Not,
    Neg,
    RedAnd,
    RedOr,
    RedXor,
}

#[derive(Debug, Clone)]
pub struct SignalRef {
    pub signal_id: String,
    pub bit_range: Option<(usize, usize)>,
}

#[derive(Debug, Clone)]
pub struct StateElement {
    pub name: String,
    pub width: usize,
    pub reset_value: Option<u64>,
    pub clock: String,
    pub reset: Option<String>,
}

#[derive(Debug, Clone)]
pub struct ClockDomain {
    pub name: String,
    pub frequency_hz: Option<u64>,
    pub phase_offset: f32,
    pub state_elements: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ClockEdge {
    Rising,
    Falling,
    Both,
}

impl SirModule {
    pub fn new(name: String) -> Self {
        SirModule {
            name,
            inputs: Vec::new(),
            outputs: Vec::new(),
            signals: Vec::new(),
            combinational_nodes: Vec::new(),
            sequential_nodes: Vec::new(),
            state_elements: HashMap::new(),
            clock_domains: HashMap::new(),
        }
    }

    pub fn extract_combinational_cones(&self) -> Vec<CombinationalCone> {
        let mut cones = Vec::new();
        let mut visited = HashSet::new();

        // Start from each sequential element (flip-flop) and trace backwards
        // to find all combinational logic that feeds it
        for seq_node in &self.sequential_nodes {
            // Skip clock inputs, focus on data inputs
            for input in &seq_node.inputs {
                if input.signal_id.contains("clk") || input.signal_id.contains("clock") {
                    continue;
                }

                // Find all combinational nodes that feed this sequential input
                let mut cone_nodes = Vec::new();
                let mut to_visit = vec![];

                // Find the driver of this input signal
                if let Some(driver) = self.get_signal_driver(&input.signal_id) {
                    if self.is_combinational_node(driver) && !visited.contains(&driver) {
                        to_visit.push(driver);
                    }
                }

                // Traverse backwards through all combinational logic
                while let Some(node_id) = to_visit.pop() {
                    if !visited.contains(&node_id) {
                        visited.insert(node_id);
                        cone_nodes.push(node_id);

                        // Add all combinational inputs of this node
                        if let Some(node) = self.get_node(node_id) {
                            for node_input in &node.inputs {
                                if let Some(driver) = self.get_signal_driver(&node_input.signal_id) {
                                    if self.is_combinational_node(driver) && !visited.contains(&driver) {
                                        to_visit.push(driver);
                                    }
                                }
                            }
                        }
                    }
                }

                // If we found combinational nodes, create a cone
                if !cone_nodes.is_empty() {
                    cones.push(CombinationalCone {
                        nodes: cone_nodes.clone(),
                        inputs: self.get_cone_inputs(&cone_nodes),
                        outputs: self.get_cone_outputs(&cone_nodes),
                    });
                }
            }
        }

        // If there are no sequential nodes, treat all combinational logic as one cone
        if self.sequential_nodes.is_empty() && !self.combinational_nodes.is_empty() {
            let all_comb_nodes: Vec<usize> = self.combinational_nodes.iter().map(|n| n.id).collect();
            cones.push(CombinationalCone {
                nodes: all_comb_nodes.clone(),
                inputs: self.get_cone_inputs(&all_comb_nodes),
                outputs: self.get_cone_outputs(&all_comb_nodes),
            });
        }

        cones
    }

    fn get_node(&self, id: usize) -> Option<&SirNode> {
        self.combinational_nodes.iter()
            .chain(self.sequential_nodes.iter())
            .find(|n| n.id == id)
    }

    fn get_signal_driver(&self, signal_name: &str) -> Option<usize> {
        self.signals.iter()
            .find(|s| s.name == signal_name)
            .and_then(|s| s.driver_node)
    }

    fn is_combinational_node(&self, node_id: usize) -> bool {
        self.combinational_nodes.iter().any(|n| n.id == node_id)
    }

    fn get_cone_inputs(&self, node_ids: &[usize]) -> Vec<String> {
        let mut inputs = HashSet::new();

        for &node_id in node_ids {
            if let Some(node) = self.get_node(node_id) {
                for input in &node.inputs {
                    if let Some(driver) = self.get_signal_driver(&input.signal_id) {
                        if !node_ids.contains(&driver) {
                            inputs.insert(input.signal_id.clone());
                        }
                    } else {
                        inputs.insert(input.signal_id.clone());
                    }
                }
            }
        }

        inputs.into_iter().collect()
    }

    fn get_cone_outputs(&self, node_ids: &[usize]) -> Vec<String> {
        let mut outputs = HashSet::new();

        for &node_id in node_ids {
            if let Some(node) = self.get_node(node_id) {
                for output in &node.outputs {
                    outputs.insert(output.signal_id.clone());
                }
            }
        }

        outputs.into_iter().collect()
    }
}

#[derive(Debug, Clone)]
pub struct CombinationalCone {
    pub nodes: Vec<usize>,
    pub inputs: Vec<String>,
    pub outputs: Vec<String>,
}