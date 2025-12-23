//! MIR Invariant Validation
//!
//! This module validates that MIR data structures satisfy required invariants
//! after HIR→MIR transformation.
//!
//! **Purpose:** Catch bugs early by validating that transformation passes
//! produce well-formed MIR.
//!
//! **Key Invariants:**
//! 1. **Synthesis-Friendly Types:** All ports and signals must be synthesis-friendly
//!    (scalars or arrays of scalars, no Struct/Vec*/Enum/Union/Array-of-composites)
//! 2. **Type Flattening Complete:** Composite types (except arrays of scalars) should
//!    be flattened during HIR→MIR transformation
//! 3. **Array Preservation:** Arrays of scalar types are intentionally preserved
//!    to allow synthesis tools to choose optimal implementation
//!
//! **Usage:**
//! Call `validate_mir()` after HIR→MIR transformation to verify invariants.
//! If validation fails, the function panics with a detailed error message.

use crate::mir::{DataType, Mir, Module};
use crate::type_width::is_composite_type;

/// Validation error types
#[derive(Debug, Clone)]
pub enum ValidationError {
    /// Port has a composite type that should have been flattened
    CompositePortType {
        module_name: String,
        port_name: String,
        port_type: String,
    },
    /// Signal has a composite type that should have been flattened
    CompositeSignalType {
        module_name: String,
        signal_name: String,
        signal_type: String,
    },
}

impl std::fmt::Display for ValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValidationError::CompositePortType {
                module_name,
                port_name,
                port_type,
            } => {
                write!(
                    f,
                    "MIR Invariant Violation: Port '{}' in module '{}' has composite type '{}'\n\
                     All composite types should be flattened during HIR→MIR transformation.\n\
                     This indicates a bug in hir_to_mir.rs.",
                    port_name, module_name, port_type
                )
            }
            ValidationError::CompositeSignalType {
                module_name,
                signal_name,
                signal_type,
            } => {
                write!(
                    f,
                    "MIR Invariant Violation: Signal '{}' in module '{}' has composite type '{}'\n\
                     All composite types should be flattened during HIR→MIR transformation.\n\
                     This indicates a bug in hir_to_mir.rs.",
                    signal_name, module_name, signal_type
                )
            }
        }
    }
}

impl std::error::Error for ValidationError {}

/// Validate that all MIR invariants are satisfied
///
/// # Arguments
/// * `mir` - The MIR to validate
///
/// # Returns
/// Ok(()) if all invariants are satisfied, Err(ValidationError) otherwise
///
/// # Example
/// ```ignore
/// let mir = transform_hir_to_mir(&hir);
/// validate_mir(&mir)?;  // Panics if invariants are violated
/// ```
pub fn validate_mir(mir: &Mir) -> Result<(), ValidationError> {
    // Validate each module
    for module in &mir.modules {
        validate_module(module)?;
    }

    Ok(())
}

/// Validate a single module's invariants
fn validate_module(module: &Module) -> Result<(), ValidationError> {
    // Validate port types
    for port in &module.ports {
        if is_composite_type(&port.port_type) {
            return Err(ValidationError::CompositePortType {
                module_name: module.name.clone(),
                port_name: port.name.clone(),
                port_type: format_type(&port.port_type),
            });
        }
    }

    // Validate signal types
    for signal in &module.signals {
        if is_composite_type(&signal.signal_type) {
            return Err(ValidationError::CompositeSignalType {
                module_name: module.name.clone(),
                signal_name: signal.name.clone(),
                signal_type: format_type(&signal.signal_type),
            });
        }
    }

    Ok(())
}

/// Format a DataType for error messages
fn format_type(data_type: &DataType) -> String {
    match data_type {
        DataType::Struct(st) => format!("Struct({})", st.name),
        DataType::Vec2(elem) => format!("Vec2({})", format_type(elem)),
        DataType::Vec3(elem) => format!("Vec3({})", format_type(elem)),
        DataType::Vec4(elem) => format!("Vec4({})", format_type(elem)),
        DataType::Array(elem, size) => format!("Array({}, {})", format_type(elem), size),
        DataType::Bit(width) => format!("bit[{}]", width),
        DataType::Logic(width) => format!("logic[{}]", width),
        DataType::Int(width) => format!("int[{}]", width),
        DataType::Nat(width) => format!("nat[{}]", width),
        DataType::Bool => "bool".to_string(),
        DataType::Clock { .. } => "clock".to_string(),
        DataType::Reset { .. } => "reset".to_string(),
        DataType::Event => "event".to_string(),
        DataType::Float16 => "float16".to_string(),
        DataType::Float32 => "float32".to_string(),
        DataType::Float64 => "float64".to_string(),
        DataType::Enum(et) => format!("Enum({})", et.name),
        DataType::Union(ut) => format!("Union({})", ut.name),
        DataType::BitParam { .. } => "bit[N]".to_string(),
        DataType::LogicParam { .. } => "logic[N]".to_string(),
        DataType::IntParam { .. } => "int[N]".to_string(),
        DataType::NatParam { .. } => "nat[N]".to_string(),
        DataType::BitExpr { .. } => "bit[expr]".to_string(),
        DataType::LogicExpr { .. } => "logic[expr]".to_string(),
        DataType::IntExpr { .. } => "int[expr]".to_string(),
        DataType::NatExpr { .. } => "nat[expr]".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mir::{Module, ModuleId, Port, PortDirection, PortId, Signal, SignalId};
    use skalp_frontend::safety_attributes::ModuleSafetyDefinitions;

    #[test]
    fn test_validate_scalar_types_ok() {
        let module = Module {
            id: ModuleId(0),
            name: "TestModule".to_string(),
            parameters: vec![],
            ports: vec![
                Port {
                    id: PortId(0),
                    name: "clk".to_string(),
                    port_type: DataType::Clock { domain: None },
                    direction: PortDirection::Input,
                    physical_constraints: None,
                    span: None,
                    detection_config: None,
                },
                Port {
                    id: PortId(1),
                    name: "data".to_string(),
                    port_type: DataType::Bit(32),
                    direction: PortDirection::Input,
                    physical_constraints: None,
                    span: None,
                    detection_config: None,
                },
            ],
            signals: vec![Signal {
                id: SignalId(0),
                name: "result".to_string(),
                signal_type: DataType::Bit(32),
                initial: None,
                clock_domain: None,
                span: None,
                memory_config: None,
                trace_config: None,
                cdc_config: None,
                breakpoint_config: None,
                power_config: None,
                safety_context: None,
                detection_config: None,
                power_domain: None,
            }],
            variables: vec![],
            processes: vec![],
            assignments: vec![],
            instances: vec![],
            clock_domains: vec![],
            generate_blocks: vec![],
            assertions: vec![],
            span: None,
            pipeline_config: None,
            vendor_ip_config: None,
            compiled_ip_config: None,
            power_domains: vec![],
            power_domain_config: None,
            safety_context: None,
        };

        let mir = Mir {
            name: "test_design".to_string(),
            modules: vec![module],
            safety_definitions: ModuleSafetyDefinitions::default(),
        };

        assert!(validate_mir(&mir).is_ok());
    }

    #[test]
    fn test_validate_composite_port_type_fails() {
        use crate::mir::{StructField, StructType};

        let struct_type = StructType {
            name: "Point".to_string(),
            fields: vec![
                StructField {
                    name: "x".to_string(),
                    field_type: DataType::Bit(32),
                },
                StructField {
                    name: "y".to_string(),
                    field_type: DataType::Bit(32),
                },
            ],
            packed: false,
        };

        let module = Module {
            id: ModuleId(0),
            name: "BadModule".to_string(),
            parameters: vec![],
            ports: vec![Port {
                id: PortId(0),
                name: "point".to_string(),
                port_type: DataType::Struct(Box::new(struct_type)),
                direction: PortDirection::Input,
                physical_constraints: None,
                span: None,
                detection_config: None,
            }],
            signals: vec![],
            variables: vec![],
            processes: vec![],
            assignments: vec![],
            instances: vec![],
            clock_domains: vec![],
            generate_blocks: vec![],
            assertions: vec![],
            span: None,
            pipeline_config: None,
            vendor_ip_config: None,
            compiled_ip_config: None,
            power_domains: vec![],
            power_domain_config: None,
            safety_context: None,
        };

        let mir = Mir {
            name: "test_design".to_string(),
            modules: vec![module],
            safety_definitions: ModuleSafetyDefinitions::default(),
        };

        let result = validate_mir(&mir);
        assert!(result.is_err());

        if let Err(ValidationError::CompositePortType {
            module_name,
            port_name,
            ..
        }) = result
        {
            assert_eq!(module_name, "BadModule");
            assert_eq!(port_name, "point");
        } else {
            panic!("Expected CompositePortType error");
        }
    }

    #[test]
    fn test_validate_composite_signal_type_fails() {
        let module = Module {
            id: ModuleId(0),
            name: "BadModule".to_string(),
            parameters: vec![],
            ports: vec![],
            signals: vec![Signal {
                id: SignalId(0),
                name: "vec_signal".to_string(),
                signal_type: DataType::Vec3(Box::new(DataType::Float32)),
                initial: None,
                clock_domain: None,
                span: None,
                memory_config: None,
                trace_config: None,
                cdc_config: None,
                breakpoint_config: None,
                power_config: None,
                safety_context: None,
                detection_config: None,
                power_domain: None,
            }],
            variables: vec![],
            processes: vec![],
            assignments: vec![],
            instances: vec![],
            clock_domains: vec![],
            generate_blocks: vec![],
            assertions: vec![],
            span: None,
            pipeline_config: None,
            vendor_ip_config: None,
            compiled_ip_config: None,
            power_domains: vec![],
            power_domain_config: None,
            safety_context: None,
        };

        let mir = Mir {
            name: "test_design".to_string(),
            modules: vec![module],
            safety_definitions: ModuleSafetyDefinitions::default(),
        };

        let result = validate_mir(&mir);
        assert!(result.is_err());

        if let Err(ValidationError::CompositeSignalType {
            module_name,
            signal_name,
            ..
        }) = result
        {
            assert_eq!(module_name, "BadModule");
            assert_eq!(signal_name, "vec_signal");
        } else {
            panic!("Expected CompositeSignalType error");
        }
    }

    #[test]
    fn test_validate_array_of_scalars_ok() {
        // Arrays of scalar types are intentionally preserved
        let module = Module {
            id: ModuleId(0),
            name: "GoodModule".to_string(),
            parameters: vec![],
            ports: vec![Port {
                id: PortId(0),
                name: "data_array".to_string(),
                port_type: DataType::Array(Box::new(DataType::Bit(8)), 16),
                direction: PortDirection::Input,
                physical_constraints: None,
                span: None,
                detection_config: None,
            }],
            signals: vec![Signal {
                id: SignalId(0),
                name: "memory".to_string(),
                signal_type: DataType::Array(Box::new(DataType::Bit(32)), 4),
                initial: None,
                clock_domain: None,
                span: None,
                memory_config: None,
                trace_config: None,
                cdc_config: None,
                breakpoint_config: None,
                power_config: None,
                safety_context: None,
                detection_config: None,
                power_domain: None,
            }],
            variables: vec![],
            processes: vec![],
            assignments: vec![],
            instances: vec![],
            clock_domains: vec![],
            generate_blocks: vec![],
            assertions: vec![],
            span: None,
            pipeline_config: None,
            vendor_ip_config: None,
            compiled_ip_config: None,
            power_domains: vec![],
            power_domain_config: None,
            safety_context: None,
        };

        let mir = Mir {
            name: "test_design".to_string(),
            modules: vec![module],
            safety_definitions: ModuleSafetyDefinitions::default(),
        };

        // Should pass - arrays of scalars are allowed
        assert!(validate_mir(&mir).is_ok());
    }

    #[test]
    fn test_validate_array_of_composites_fails() {
        // Arrays of composite types (like Vec3) should still be flattened
        let module = Module {
            id: ModuleId(0),
            name: "BadModule".to_string(),
            parameters: vec![],
            ports: vec![Port {
                id: PortId(0),
                name: "vec_array".to_string(),
                port_type: DataType::Array(
                    Box::new(DataType::Vec3(Box::new(DataType::Float32))),
                    4,
                ),
                direction: PortDirection::Input,
                physical_constraints: None,
                span: None,
                detection_config: None,
            }],
            signals: vec![],
            variables: vec![],
            processes: vec![],
            assignments: vec![],
            instances: vec![],
            clock_domains: vec![],
            generate_blocks: vec![],
            assertions: vec![],
            span: None,
            pipeline_config: None,
            vendor_ip_config: None,
            compiled_ip_config: None,
            power_domains: vec![],
            power_domain_config: None,
            safety_context: None,
        };

        let mir = Mir {
            name: "test_design".to_string(),
            modules: vec![module],
            safety_definitions: ModuleSafetyDefinitions::default(),
        };

        let result = validate_mir(&mir);
        assert!(result.is_err());

        if let Err(ValidationError::CompositePortType {
            module_name,
            port_name,
            ..
        }) = result
        {
            assert_eq!(module_name, "BadModule");
            assert_eq!(port_name, "vec_array");
        } else {
            panic!("Expected CompositePortType error");
        }
    }
}
