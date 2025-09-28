//! Verilog code generation utilities

use crate::BackendError;
use skalp_lir::LirModule;

pub fn generate_verilog(module: &LirModule) -> Result<String, BackendError> {
    let mut verilog = String::new();

    // Module declaration
    verilog.push_str(&format!("module {} (\n", module.name));

    // Add ports (simplified)
    for (i, signal) in module.signals.iter().enumerate() {
        if i > 0 {
            verilog.push_str(",\n");
        }
        let direction = if signal.is_input { "input" } else { "output" };
        verilog.push_str(&format!("  {} {}", direction, signal.name));
    }

    verilog.push_str("\n);\n\n");

    // Add signal declarations
    for signal in &module.signals {
        if signal.is_register {
            verilog.push_str(&format!("  reg {};\n", signal.name));
        }
    }

    // Add gate instantiations (simplified)
    for gate in &module.gates {
        verilog.push_str(&format!(
            "  {} {} (.in({}), .out({}));\n",
            gate.gate_type.to_string().to_lowercase(),
            gate.id,
            gate.inputs.join(", "),
            gate.outputs.join(", ")
        ));
    }

    verilog.push_str("\nendmodule\n");

    Ok(verilog)
}