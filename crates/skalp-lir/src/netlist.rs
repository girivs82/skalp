//! Netlist representation for SKALP LIR

use serde::{Deserialize, Serialize};

/// Netlist representation of a hardware design
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Netlist {
    /// Top-level module name
    pub top_module: String,
    /// All modules in the design
    pub modules: Vec<Module>,
    /// Global nets
    pub global_nets: Vec<GlobalNet>,
}

/// Hardware module in the netlist
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Module {
    /// Module name
    pub name: String,
    /// Module ports
    pub ports: Vec<Port>,
    /// Module instances
    pub instances: Vec<Instance>,
    /// Internal nets
    pub nets: Vec<InternalNet>,
}

/// Module port
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Port {
    /// Port name
    pub name: String,
    /// Port direction
    pub direction: PortDirection,
    /// Port width
    pub width: usize,
}

/// Port direction
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PortDirection {
    Input,
    Output,
    Inout,
}

/// Module instance
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Instance {
    /// Instance name
    pub name: String,
    /// Module type being instantiated
    pub module_type: String,
    /// Port connections
    pub connections: Vec<Connection>,
}

/// Port connection
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Connection {
    /// Port name on the instance
    pub port: String,
    /// Net connected to this port
    pub net: String,
}

/// Internal net within a module
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InternalNet {
    /// Net name
    pub name: String,
    /// Net width
    pub width: usize,
}

/// Global net spanning multiple modules
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GlobalNet {
    /// Net name
    pub name: String,
    /// Net width
    pub width: usize,
    /// Modules this net spans
    pub modules: Vec<String>,
}

impl Netlist {
    /// Create a new empty netlist
    pub fn new(top_module: String) -> Self {
        Self {
            top_module,
            modules: Vec::new(),
            global_nets: Vec::new(),
        }
    }
}
