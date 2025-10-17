//! Type Width Calculation Utilities
//!
//! This module provides the single source of truth for calculating the bit width
//! of MIR data types.
//!
//! **Purpose:** Ensure consistent width calculation across all compiler passes.
//! All type width queries should use this module, not duplicate the logic.
//!
//! **Usage:**
//! - MIR construction: Use to determine signal/port widths
//! - MIR→SIR transformation: Use to create SIR signals with correct widths
//! - SystemVerilog codegen: Use to emit correct bit widths
//! - Metal codegen: Use to determine buffer sizes
//!
//! **Invariant:** Width calculations are deterministic and consistent across
//! all passes.

use crate::mir::{DataType, EnumType, StructType, UnionType};

/// Calculate the bit width of a MIR data type
///
/// # Arguments
/// * `data_type` - The data type to measure
///
/// # Returns
/// Bit width of the type
///
/// # Panics
/// Panics if the type is a composite type (Struct/Enum/Union) that should have
/// been flattened during HIR→MIR transformation.
///
/// # Example
/// ```ignore
/// let width = get_type_width(&DataType::Bit(32));  // Returns 32
/// let width = get_type_width(&DataType::Float32);  // Returns 32
/// ```
pub fn get_type_width(data_type: &DataType) -> usize {
    match data_type {
        // Scalar bit types
        DataType::Bit(width) => *width,
        DataType::Logic(width) => *width,
        DataType::Int(width) => *width,
        DataType::Nat(width) => *width,

        // Boolean
        DataType::Bool => 1,

        // Control signal types
        DataType::Clock { .. } => 1,
        DataType::Reset { .. } => 1,
        DataType::Event => 1,

        // Floating-point types
        DataType::Float16 => 16,
        DataType::Float32 => 32,
        DataType::Float64 => 64,

        // Parametric types (use default widths)
        DataType::BitParam { default, .. }
        | DataType::LogicParam { default, .. }
        | DataType::IntParam { default, .. }
        | DataType::NatParam { default, .. } => *default,

        // Vector types - these should be flattened, but we can calculate width
        DataType::Vec2(element_type) => get_type_width(element_type) * 2,
        DataType::Vec3(element_type) => get_type_width(element_type) * 3,
        DataType::Vec4(element_type) => get_type_width(element_type) * 4,

        // Array types - these should be flattened, but we can calculate width
        DataType::Array(element_type, size) => get_type_width(element_type) * size,

        // Composite types - these MUST be flattened before reaching here
        DataType::Struct(st) => {
            panic!(
                "INTERNAL ERROR: Attempted to get width of struct type '{}' which should have been \
                 flattened during HIR→MIR transformation. This indicates a bug in hir_to_mir.rs.\n\
                 Struct types should never appear in MIR port/signal types after transformation.\n\
                 Use get_struct_total_width() during flattening if you need the total width.",
                st.name
            );
        }
        DataType::Enum(et) => {
            panic!(
                "INTERNAL ERROR: Attempted to get width of enum type '{}' which should have been \
                 flattened during HIR→MIR transformation. This indicates a bug in hir_to_mir.rs.\n\
                 Enum types should never appear in MIR port/signal types after transformation.",
                et.name
            );
        }
        DataType::Union(ut) => {
            panic!(
                "INTERNAL ERROR: Attempted to get width of union type '{}' which should have been \
                 flattened during HIR→MIR transformation. This indicates a bug in hir_to_mir.rs.\n\
                 Union types should never appear in MIR port/signal types after transformation.",
                ut.name
            );
        }
    }
}

/// Calculate the total bit width of a flattened struct
///
/// **Note:** This function is for use DURING HIR→MIR transformation when
/// computing struct widths for flattening. After transformation is complete,
/// no struct types should exist in MIR.
///
/// # Arguments
/// * `struct_type` - The struct type to measure
///
/// # Returns
/// Total width of all fields combined
///
/// # Example
/// ```ignore
/// struct Point { x: bit[32], y: bit[32] }
/// get_struct_total_width(&point_struct)  // Returns 64
/// ```
pub fn get_struct_total_width(struct_type: &StructType) -> usize {
    struct_type
        .fields
        .iter()
        .map(|field| get_type_width(&field.field_type))
        .sum()
}

/// Calculate the bit width of an enum type
///
/// **Note:** This function is for use DURING HIR→MIR transformation.
/// After transformation, enums should be flattened to their base type.
///
/// # Arguments
/// * `enum_type` - The enum type to measure
///
/// # Returns
/// Width of the enum's base type
pub fn get_enum_width(enum_type: &EnumType) -> usize {
    get_type_width(&enum_type.base_type)
}

/// Calculate the bit width of a union type
///
/// **Note:** This function is for use DURING HIR→MIR transformation.
/// After transformation, unions should be flattened.
///
/// # Arguments
/// * `union_type` - The union type to measure
///
/// # Returns
/// Width of the largest field in the union
pub fn get_union_width(union_type: &UnionType) -> usize {
    union_type
        .fields
        .iter()
        .map(|field| get_type_width(&field.field_type))
        .max()
        .unwrap_or(1) // Empty union defaults to 1 bit
}

/// Calculate the bit offset of a field within a struct
///
/// **Note:** This function is for use DURING HIR→MIR transformation when
/// computing field offsets for packed structs or value extraction.
///
/// # Arguments
/// * `struct_type` - The struct containing the field
/// * `field_name` - Name of the field to locate
///
/// # Returns
/// Bit offset from the start of the struct, or None if field not found
///
/// # Example
/// ```ignore
/// struct Point { x: bit[32], y: bit[32] }
/// get_field_offset(&point_struct, "y")  // Returns Some(32)
/// ```
pub fn get_field_offset(struct_type: &StructType, field_name: &str) -> Option<usize> {
    let mut offset = 0;
    for field in &struct_type.fields {
        if field.name == field_name {
            return Some(offset);
        }
        offset += get_type_width(&field.field_type);
    }
    None
}

/// Check if a data type is a scalar (leaf) type
///
/// Scalar types are those that don't need flattening.
///
/// # Returns
/// true if the type is scalar (Bit, Int, Nat, Float*, Clock, Reset, Event, Bool)
/// false if the type is composite (Struct, Enum, Union, Vec*, Array)
pub fn is_scalar_type(data_type: &DataType) -> bool {
    matches!(
        data_type,
        DataType::Bit(_)
            | DataType::Logic(_)
            | DataType::Int(_)
            | DataType::Nat(_)
            | DataType::Bool
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

/// Check if a data type is composite and needs flattening
///
/// Arrays of scalar types are intentionally preserved (not considered composite)
/// to allow synthesis tools to choose optimal implementation.
///
/// # Returns
/// true if the type needs flattening (Struct, Enum, Union, Vec*, Array of composites)
/// false if the type is synthesis-friendly (scalar or array of scalars)
pub fn is_composite_type(data_type: &DataType) -> bool {
    match data_type {
        // Arrays of scalars are preserved - not composite
        DataType::Array(element_type, _) => {
            // Only consider array composite if its element type is composite
            // This allows `[bit[32]; 16]` but rejects `[Vec3; 4]`
            is_composite_type(element_type)
        }
        // All other cases: scalar types return false, composite types return true
        _ => !is_scalar_type(data_type),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mir::StructField;

    #[test]
    fn test_scalar_widths() {
        assert_eq!(get_type_width(&DataType::Bit(8)), 8);
        assert_eq!(get_type_width(&DataType::Bit(32)), 32);
        assert_eq!(get_type_width(&DataType::Int(16)), 16);
        assert_eq!(get_type_width(&DataType::Bool), 1);
        assert_eq!(get_type_width(&DataType::Clock { domain: None }), 1);
        assert_eq!(get_type_width(&DataType::Float32), 32);
        assert_eq!(get_type_width(&DataType::Float64), 64);
    }

    #[test]
    fn test_vector_widths() {
        assert_eq!(
            get_type_width(&DataType::Vec2(Box::new(DataType::Float32))),
            64
        );
        assert_eq!(
            get_type_width(&DataType::Vec3(Box::new(DataType::Bit(32)))),
            96
        );
        assert_eq!(
            get_type_width(&DataType::Vec4(Box::new(DataType::Float16))),
            64
        );
    }

    #[test]
    fn test_array_widths() {
        assert_eq!(
            get_type_width(&DataType::Array(Box::new(DataType::Bit(8)), 4)),
            32
        );
    }

    #[test]
    fn test_struct_total_width() {
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

        assert_eq!(get_struct_total_width(&struct_type), 64);
    }

    #[test]
    fn test_field_offset() {
        let struct_type = StructType {
            name: "Vertex".to_string(),
            fields: vec![
                StructField {
                    name: "position".to_string(),
                    field_type: DataType::Vec3(Box::new(DataType::Float32)),
                },
                StructField {
                    name: "color".to_string(),
                    field_type: DataType::Bit(32),
                },
            ],
            packed: false,
        };

        assert_eq!(get_field_offset(&struct_type, "position"), Some(0));
        assert_eq!(get_field_offset(&struct_type, "color"), Some(96)); // After 3 * 32 bits
        assert_eq!(get_field_offset(&struct_type, "nonexistent"), None);
    }

    #[test]
    #[should_panic(expected = "should have been flattened")]
    fn test_struct_width_panics() {
        let struct_type = StructType {
            name: "BadStruct".to_string(),
            fields: vec![],
            packed: false,
        };

        // This should panic because structs shouldn't reach get_type_width after flattening
        get_type_width(&DataType::Struct(Box::new(struct_type)));
    }

    #[test]
    fn test_is_scalar_type() {
        assert!(is_scalar_type(&DataType::Bit(32)));
        assert!(is_scalar_type(&DataType::Bool));
        assert!(is_scalar_type(&DataType::Float32));
        assert!(is_scalar_type(&DataType::Clock { domain: None }));

        assert!(!is_scalar_type(&DataType::Vec2(Box::new(DataType::Bit(
            32
        )))));
        assert!(!is_scalar_type(&DataType::Array(
            Box::new(DataType::Bit(8)),
            4
        )));
    }

    #[test]
    fn test_is_composite_type() {
        // Scalar types are not composite
        assert!(!is_composite_type(&DataType::Bit(32)));
        assert!(!is_composite_type(&DataType::Float32));

        // Vector types are composite
        assert!(is_composite_type(&DataType::Vec3(Box::new(
            DataType::Float32
        ))));

        // Arrays of scalars are NOT composite (intentionally preserved)
        assert!(!is_composite_type(&DataType::Array(
            Box::new(DataType::Bit(8)),
            4
        )));

        // Arrays of composites ARE composite (need flattening)
        assert!(is_composite_type(&DataType::Array(
            Box::new(DataType::Vec3(Box::new(DataType::Float32))),
            4
        )));
    }
}
