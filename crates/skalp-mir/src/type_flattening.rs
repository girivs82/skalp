//! Type Flattening Utilities
//!
//! This module provides the single source of truth for flattening composite types
//! (structs, vectors, arrays, enums, unions) into scalar components.
//!
//! **Purpose:** Ensure consistent flattening logic across all compiler passes.
//! All struct/vector expansion should use this module, not duplicate the logic.
//!
//! **Usage:**
//! - HIR→MIR transformation: Use to flatten ports and signals
//! - Assignment expansion: Use to expand struct assignments into field assignments
//! - Expression resolution: Use to resolve field access paths
//!
//! **Invariant:** After HIR→MIR transformation using this module, MIR should contain
//! ONLY scalar types in ports and signals (no DataType::Struct, etc.).

use crate::mir::{
    Assignment, AssignmentKind, ClockDomainId, DataType, Expression, ExpressionKind, LValue, Port,
    PortDirection, PortId, Signal, SignalId, StructType, Value,
};
use skalp_frontend::span::SourceSpan;

/// Information about a flattened field
#[derive(Debug, Clone)]
pub struct FlattenedField {
    /// Unique ID for this flattened field (PortId or SignalId)
    pub id: u32,
    /// Path from root to this field (e.g., ["position", "x"])
    pub field_path: Vec<String>,
    /// The leaf scalar type of this field
    pub leaf_type: DataType,
}

/// Type flattener - handles all composite type expansion
pub struct TypeFlattener {
    next_id: u32,
}

impl TypeFlattener {
    /// Create a new type flattener with starting ID
    pub fn new(starting_id: u32) -> Self {
        Self {
            next_id: starting_id,
        }
    }

    /// Check if a type is a scalar type (no composite structure)
    ///
    /// Scalar types include: Bit, Bool, Logic, Int, Nat, Clock, Reset, Event, Float*
    /// These can be used as array elements without further flattening.
    fn is_scalar_type(data_type: &DataType) -> bool {
        matches!(
            data_type,
            DataType::Bit(_)
                | DataType::Bool
                | DataType::Logic(_)
                | DataType::Int(_)
                | DataType::Nat(_)
                | DataType::Clock { .. }
                | DataType::Reset { .. }
                | DataType::Event
                | DataType::Float16
                | DataType::Float32
                | DataType::Float64
                | DataType::BitParam { .. }
                | DataType::LogicParam { .. }
                | DataType::IntParam { .. }
                | DataType::NatParam { .. }
        )
    }

    /// Check if we should preserve an array structure instead of flattening it
    ///
    /// Arrays of scalar types should be preserved to allow synthesis tools
    /// to choose optimal implementation (MUX trees, distributed RAM, block RAM, etc.)
    fn should_preserve_array(element_type: &DataType) -> bool {
        Self::is_scalar_type(element_type)
    }

    /// Flatten a port with composite type into multiple scalar ports
    ///
    /// # Arguments
    /// * `base_name` - Base name for the port (e.g., "vertex")
    /// * `port_type` - Type to flatten (may be composite or scalar)
    /// * `direction` - Port direction (Input/Output/InOut)
    /// * `physical_constraints` - Optional physical constraints
    ///
    /// # Returns
    /// * Vector of flattened ports (all scalar types)
    /// * Vector of flattened field metadata
    ///
    /// # Example
    /// ```ignore
    /// struct Point { x: bit[32], y: bit[32] }
    /// Input: base_name="pos", port_type=Struct(Point)
    /// Output: [Port("pos_x", Bit(32)), Port("pos_y", Bit(32))]
    /// ```
    pub fn flatten_port(
        &mut self,
        base_name: &str,
        port_type: &DataType,
        direction: PortDirection,
        physical_constraints: Option<skalp_frontend::hir::PhysicalConstraints>,
    ) -> (Vec<Port>, Vec<FlattenedField>) {
        self.flatten_port_with_span(base_name, port_type, direction, physical_constraints, None)
    }

    /// Flatten a port with composite type into multiple scalar ports, with source span
    ///
    /// # Arguments
    /// * `base_name` - Base name for the port (e.g., "vertex")
    /// * `port_type` - Type to flatten (may be composite or scalar)
    /// * `direction` - Port direction (Input/Output/InOut)
    /// * `physical_constraints` - Optional physical constraints
    /// * `span` - Optional source span (propagated to all flattened ports)
    ///
    /// # Returns
    /// * Vector of flattened ports (all scalar types)
    /// * Vector of flattened field metadata
    pub fn flatten_port_with_span(
        &mut self,
        base_name: &str,
        port_type: &DataType,
        direction: PortDirection,
        physical_constraints: Option<skalp_frontend::hir::PhysicalConstraints>,
        span: Option<SourceSpan>,
    ) -> (Vec<Port>, Vec<FlattenedField>) {
        let mut ports = Vec::new();
        let mut fields = Vec::new();
        self.flatten_port_recursive(
            base_name,
            port_type,
            direction,
            physical_constraints.as_ref(),
            span,
            vec![],
            &mut ports,
            &mut fields,
        );
        (ports, fields)
    }

    /// Flatten a signal with composite type into multiple scalar signals
    ///
    /// # Arguments
    /// * `base_name` - Base name for the signal
    /// * `signal_type` - Type to flatten
    /// * `initial` - Optional initial value
    /// * `clock_domain` - Optional clock domain
    /// * `span` - Optional source span (propagated to all flattened signals)
    ///
    /// # Returns
    /// * Vector of flattened signals (all scalar types)
    /// * Vector of flattened field metadata
    pub fn flatten_signal(
        &mut self,
        base_name: &str,
        signal_type: &DataType,
        initial: Option<Value>,
        clock_domain: Option<ClockDomainId>,
    ) -> (Vec<Signal>, Vec<FlattenedField>) {
        self.flatten_signal_with_span(base_name, signal_type, initial, clock_domain, None)
    }

    /// Flatten a signal with composite type into multiple scalar signals, with source span
    ///
    /// # Arguments
    /// * `base_name` - Base name for the signal
    /// * `signal_type` - Type to flatten
    /// * `initial` - Optional initial value
    /// * `clock_domain` - Optional clock domain
    /// * `span` - Optional source span (propagated to all flattened signals)
    ///
    /// # Returns
    /// * Vector of flattened signals (all scalar types)
    /// * Vector of flattened field metadata
    pub fn flatten_signal_with_span(
        &mut self,
        base_name: &str,
        signal_type: &DataType,
        initial: Option<Value>,
        clock_domain: Option<ClockDomainId>,
        span: Option<SourceSpan>,
    ) -> (Vec<Signal>, Vec<FlattenedField>) {
        let mut signals = Vec::new();
        let mut fields = Vec::new();
        self.flatten_signal_recursive(
            base_name,
            signal_type,
            initial,
            clock_domain,
            span,
            vec![],
            &mut signals,
            &mut fields,
        );
        (signals, fields)
    }

    /// Get the flattened field name for a field access path
    ///
    /// # Arguments
    /// * `base_name` - Base signal/port name
    /// * `field_chain` - Chain of field accesses (e.g., ["position", "x"])
    ///
    /// # Returns
    /// Flattened name following naming convention (e.g., "vertex_position_x")
    ///
    /// # Example
    /// ```ignore
    /// get_field_path("vertex", &["position", "x"]) → "vertex_position_x"
    /// ```
    pub fn get_field_path(base_name: &str, field_chain: &[String]) -> String {
        if field_chain.is_empty() {
            base_name.to_string()
        } else {
            format!("{}_{}", base_name, field_chain.join("_"))
        }
    }

    /// Expand a struct assignment into multiple field-level assignments
    ///
    /// # Arguments
    /// * `lhs` - Left-hand side (must be struct-typed)
    /// * `rhs` - Right-hand side expression
    /// * `struct_type` - The struct type being assigned
    /// * `kind` - Assignment kind (blocking/non-blocking)
    /// * `flattened_fields` - Metadata about flattened fields
    ///
    /// # Returns
    /// Vector of field-level assignments
    ///
    /// # Example
    /// ```ignore
    /// // Input: point = other_point
    /// // Output: [point_x = other_point_x, point_y = other_point_y]
    /// ```
    pub fn expand_struct_assignment(
        lhs: &LValue,
        rhs: &Expression,
        struct_type: &StructType,
        kind: AssignmentKind,
        flattened_fields: &[FlattenedField],
    ) -> Vec<Assignment> {
        let mut assignments = Vec::new();

        // Get base names for LHS and RHS
        let lhs_base = Self::get_lvalue_base_name(lhs);
        let rhs_base = Self::get_expression_base_name(rhs);

        // Create assignment for each flattened field
        for field_info in flattened_fields {
            let field_path_str = field_info.field_path.join("_");

            // Create LHS for this field
            let lhs_field_name = if field_path_str.is_empty() {
                lhs_base.clone()
            } else {
                format!("{}_{}", lhs_base, field_path_str)
            };

            // Create RHS for this field
            let rhs_field_name = if field_path_str.is_empty() {
                rhs_base.clone()
            } else {
                format!("{}_{}", rhs_base, field_path_str)
            };

            // Create the field assignment
            // Note: We create LValue::Signal with the computed SignalId
            // This assumes the signal has already been created during signal flattening
            let lhs_field = LValue::Signal(SignalId(field_info.id));

            // For RHS, we create a reference to the flattened RHS signal
            // TODO(Bug #76): Use proper type from field_info.leaf_type instead of Unknown
            let rhs_field = Expression::with_unknown_type(ExpressionKind::Ref(LValue::Signal(
                SignalId(field_info.id),
            )));

            assignments.push(Assignment {
                lhs: lhs_field,
                rhs: rhs_field,
                kind,
                span: None,
            });
        }

        assignments
    }

    // ========================================================================
    // Private helper methods
    // ========================================================================

    /// Recursively flatten a port into scalar components
    #[allow(clippy::too_many_arguments)]
    fn flatten_port_recursive(
        &mut self,
        name: &str,
        port_type: &DataType,
        direction: PortDirection,
        physical_constraints: Option<&skalp_frontend::hir::PhysicalConstraints>,
        span: Option<SourceSpan>,
        field_path: Vec<String>,
        ports: &mut Vec<Port>,
        fields: &mut Vec<FlattenedField>,
    ) {
        match port_type {
            DataType::Struct(struct_type) => {
                // Recursively flatten each struct field
                for field in &struct_type.fields {
                    let field_name = format!("{}_{}", name, field.name);
                    let mut new_path = field_path.clone();
                    new_path.push(field.name.clone());
                    self.flatten_port_recursive(
                        &field_name,
                        &field.field_type,
                        direction,
                        physical_constraints,
                        span.clone(),
                        new_path,
                        ports,
                        fields,
                    );
                }
            }
            DataType::Vec2(element_type)
            | DataType::Vec3(element_type)
            | DataType::Vec4(element_type) => {
                // Expand vector types into components
                let components = match port_type {
                    DataType::Vec2(_) => vec!["x", "y"],
                    DataType::Vec3(_) => vec!["x", "y", "z"],
                    DataType::Vec4(_) => vec!["x", "y", "z", "w"],
                    _ => unreachable!(),
                };

                for component in components {
                    let comp_name = format!("{}_{}", name, component);
                    let mut new_path = field_path.clone();
                    new_path.push(component.to_string());
                    self.flatten_port_recursive(
                        &comp_name,
                        element_type,
                        direction,
                        physical_constraints,
                        span.clone(),
                        new_path,
                        ports,
                        fields,
                    );
                }
            }
            DataType::Array(element_type, size) => {
                // NEW: Preserve arrays of scalar types for synthesis flexibility
                // Arrays of scalar types become packed arrays in SystemVerilog,
                // allowing synthesis tools to choose optimal implementation
                // (MUX trees, distributed RAM, block RAM, etc.)
                if Self::should_preserve_array(element_type) {
                    // Keep array intact - create port with array type
                    self.create_leaf_port(
                        name,
                        port_type,
                        direction,
                        physical_constraints,
                        span,
                        field_path,
                        ports,
                        fields,
                    );
                } else {
                    // Arrays of composite types (structs, vectors) still get flattened
                    // Example: [Vec3; 4] → elem_0_x, elem_0_y, elem_0_z, elem_1_x, ...
                    for i in 0..*size {
                        let elem_name = format!("{}_{}", name, i);
                        let mut new_path = field_path.clone();
                        new_path.push(i.to_string());
                        self.flatten_port_recursive(
                            &elem_name,
                            element_type,
                            direction,
                            physical_constraints,
                            span.clone(),
                            new_path,
                            ports,
                            fields,
                        );
                    }
                }
            }
            DataType::Enum(enum_type) => {
                // Enums expand to their base type
                self.flatten_port_recursive(
                    name,
                    &enum_type.base_type,
                    direction,
                    physical_constraints,
                    span,
                    field_path,
                    ports,
                    fields,
                );
            }
            DataType::Union(union_type) => {
                // Unions expand to largest field
                // For now, just use the first field type
                // TODO: Proper union handling with tag field
                if let Some(first_field) = union_type.fields.first() {
                    self.flatten_port_recursive(
                        name,
                        &first_field.field_type,
                        direction,
                        physical_constraints,
                        span,
                        field_path,
                        ports,
                        fields,
                    );
                } else {
                    // Empty union - treat as 1-bit
                    self.create_leaf_port(
                        name,
                        &DataType::Bit(1),
                        direction,
                        physical_constraints,
                        span,
                        field_path,
                        ports,
                        fields,
                    );
                }
            }
            _ => {
                // Leaf scalar type - create actual port
                self.create_leaf_port(
                    name,
                    port_type,
                    direction,
                    physical_constraints,
                    span,
                    field_path,
                    ports,
                    fields,
                );
            }
        }
    }

    /// Create a leaf port (scalar type)
    #[allow(clippy::too_many_arguments)]
    fn create_leaf_port(
        &mut self,
        name: &str,
        port_type: &DataType,
        direction: PortDirection,
        physical_constraints: Option<&skalp_frontend::hir::PhysicalConstraints>,
        span: Option<SourceSpan>,
        field_path: Vec<String>,
        ports: &mut Vec<Port>,
        fields: &mut Vec<FlattenedField>,
    ) {
        let port_id = PortId(self.next_id);
        self.next_id += 1;

        let port = Port {
            id: port_id,
            name: name.to_string(),
            direction,
            port_type: port_type.clone(),
            physical_constraints: physical_constraints.cloned(),
            span,
        };
        ports.push(port);

        fields.push(FlattenedField {
            id: port_id.0,
            field_path,
            leaf_type: port_type.clone(),
        });
    }

    /// Recursively flatten a signal into scalar components
    #[allow(clippy::too_many_arguments)]
    fn flatten_signal_recursive(
        &mut self,
        name: &str,
        signal_type: &DataType,
        initial: Option<Value>,
        clock_domain: Option<ClockDomainId>,
        span: Option<SourceSpan>,
        field_path: Vec<String>,
        signals: &mut Vec<Signal>,
        fields: &mut Vec<FlattenedField>,
    ) {
        match signal_type {
            DataType::Struct(struct_type) => {
                // Recursively flatten each struct field
                for field in &struct_type.fields {
                    let field_name = format!("{}_{}", name, field.name);
                    let mut new_path = field_path.clone();
                    new_path.push(field.name.clone());
                    self.flatten_signal_recursive(
                        &field_name,
                        &field.field_type,
                        None, // Don't propagate initial value for struct fields
                        clock_domain,
                        span.clone(),
                        new_path,
                        signals,
                        fields,
                    );
                }
            }
            DataType::Vec2(element_type)
            | DataType::Vec3(element_type)
            | DataType::Vec4(element_type) => {
                // Expand vector types into components
                let components = match signal_type {
                    DataType::Vec2(_) => vec!["x", "y"],
                    DataType::Vec3(_) => vec!["x", "y", "z"],
                    DataType::Vec4(_) => vec!["x", "y", "z", "w"],
                    _ => unreachable!(),
                };

                for component in components {
                    let comp_name = format!("{}_{}", name, component);
                    let mut new_path = field_path.clone();
                    new_path.push(component.to_string());
                    self.flatten_signal_recursive(
                        &comp_name,
                        element_type,
                        None,
                        clock_domain,
                        span.clone(),
                        new_path,
                        signals,
                        fields,
                    );
                }
            }
            DataType::Array(element_type, size) => {
                // NEW: Preserve arrays of scalar types for synthesis flexibility
                // Arrays of scalar types become packed arrays in SystemVerilog,
                // allowing synthesis tools to choose optimal implementation
                let should_preserve = Self::should_preserve_array(element_type);
                if should_preserve {
                    // Keep array intact - create signal with array type
                    self.create_leaf_signal(
                        name,
                        signal_type,
                        initial,
                        clock_domain,
                        span,
                        field_path,
                        signals,
                        fields,
                    );
                } else {
                    // Arrays of composite types still get flattened
                    for i in 0..*size {
                        let elem_name = format!("{}_{}", name, i);
                        let mut new_path = field_path.clone();
                        new_path.push(i.to_string());
                        self.flatten_signal_recursive(
                            &elem_name,
                            element_type,
                            None,
                            clock_domain,
                            span.clone(),
                            new_path,
                            signals,
                            fields,
                        );
                    }
                }
            }
            DataType::Enum(enum_type) => {
                // Enums expand to their base type
                self.flatten_signal_recursive(
                    name,
                    &enum_type.base_type,
                    initial,
                    clock_domain,
                    span,
                    field_path,
                    signals,
                    fields,
                );
            }
            DataType::Union(union_type) => {
                // Unions expand to largest field
                // For now, just use the first field type
                // TODO: Proper union handling with tag field
                if let Some(first_field) = union_type.fields.first() {
                    self.flatten_signal_recursive(
                        name,
                        &first_field.field_type,
                        initial,
                        clock_domain,
                        span,
                        field_path,
                        signals,
                        fields,
                    );
                } else {
                    // Empty union - treat as 1-bit
                    self.create_leaf_signal(
                        name,
                        &DataType::Bit(1),
                        initial,
                        clock_domain,
                        span,
                        field_path,
                        signals,
                        fields,
                    );
                }
            }
            _ => {
                // Leaf scalar type - create actual signal
                self.create_leaf_signal(
                    name,
                    signal_type,
                    initial,
                    clock_domain,
                    span,
                    field_path,
                    signals,
                    fields,
                );
            }
        }
    }

    /// Create a leaf signal (scalar type)
    #[allow(clippy::too_many_arguments)]
    fn create_leaf_signal(
        &mut self,
        name: &str,
        signal_type: &DataType,
        initial: Option<Value>,
        clock_domain: Option<ClockDomainId>,
        span: Option<SourceSpan>,
        field_path: Vec<String>,
        signals: &mut Vec<Signal>,
        fields: &mut Vec<FlattenedField>,
    ) {
        let signal_id = SignalId(self.next_id);
        self.next_id += 1;

        let signal = Signal {
            id: signal_id,
            name: name.to_string(),
            signal_type: signal_type.clone(),
            initial,
            clock_domain,
            span,
            memory_config: None,
            trace_config: None,
            cdc_config: None,
            breakpoint_config: None,
            power_config: None,
        };
        signals.push(signal);

        fields.push(FlattenedField {
            id: signal_id.0,
            field_path,
            leaf_type: signal_type.clone(),
        });
    }

    /// Extract base name from an LValue
    fn get_lvalue_base_name(lvalue: &LValue) -> String {
        match lvalue {
            LValue::Signal(id) => format!("signal_{}", id.0),
            LValue::Variable(id) => format!("var_{}", id.0),
            LValue::Port(id) => format!("port_{}", id.0),
            LValue::BitSelect { base, .. } => Self::get_lvalue_base_name(base),
            LValue::RangeSelect { base, .. } => Self::get_lvalue_base_name(base),
            LValue::Concat(_) => "concat".to_string(),
        }
    }

    /// Extract base name from an Expression
    fn get_expression_base_name(expr: &Expression) -> String {
        match &expr.kind {
            ExpressionKind::Ref(lvalue) => Self::get_lvalue_base_name(lvalue),
            ExpressionKind::Literal(_) => "literal".to_string(),
            ExpressionKind::Binary { .. } => "binary".to_string(),
            ExpressionKind::Unary { .. } => "unary".to_string(),
            ExpressionKind::Conditional { .. } => "cond".to_string(),
            ExpressionKind::Concat(_) => "concat".to_string(),
            ExpressionKind::Replicate { .. } => "replicate".to_string(),
            ExpressionKind::FunctionCall { name, .. } => name.clone(),
            ExpressionKind::Cast { expr, .. } => Self::get_expression_base_name(expr),
            // BUG FIX #85: Handle tuple/field access
            ExpressionKind::TupleFieldAccess { base, index } => {
                format!("{}.{}", Self::get_expression_base_name(base), index)
            }
            ExpressionKind::FieldAccess { base, field } => {
                format!("{}.{}", Self::get_expression_base_name(base), field)
            }
        }
    }
}

impl Default for TypeFlattener {
    fn default() -> Self {
        Self::new(0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_flatten_simple_struct() {
        let mut flattener = TypeFlattener::new(0);

        let struct_type = StructType {
            name: "Point".to_string(),
            fields: vec![
                crate::mir::StructField {
                    name: "x".to_string(),
                    field_type: DataType::Bit(32),
                },
                crate::mir::StructField {
                    name: "y".to_string(),
                    field_type: DataType::Bit(32),
                },
            ],
            packed: false,
        };

        let (ports, fields) = flattener.flatten_port(
            "pos",
            &DataType::Struct(Box::new(struct_type)),
            PortDirection::Input,
            None,
        );

        assert_eq!(ports.len(), 2);
        assert_eq!(ports[0].name, "pos_x");
        assert_eq!(ports[1].name, "pos_y");
        assert_eq!(fields.len(), 2);
        assert_eq!(fields[0].field_path, vec!["x"]);
        assert_eq!(fields[1].field_path, vec!["y"]);
    }

    #[test]
    fn test_flatten_vec3() {
        let mut flattener = TypeFlattener::new(0);

        let (signals, fields) = flattener.flatten_signal(
            "velocity",
            &DataType::Vec3(Box::new(DataType::Bit(32))),
            None,
            None,
        );

        assert_eq!(signals.len(), 3);
        assert_eq!(signals[0].name, "velocity_x");
        assert_eq!(signals[1].name, "velocity_y");
        assert_eq!(signals[2].name, "velocity_z");
        assert_eq!(fields.len(), 3);
    }

    #[test]
    fn test_field_path() {
        let path =
            TypeFlattener::get_field_path("vertex", &["position".to_string(), "x".to_string()]);
        assert_eq!(path, "vertex_position_x");

        let path2 = TypeFlattener::get_field_path("data", &[]);
        assert_eq!(path2, "data");
    }
}
