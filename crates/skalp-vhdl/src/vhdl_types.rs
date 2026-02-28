use crate::builtins::{clog2, BuiltinScope};
use crate::syntax::{SyntaxKind, SyntaxNode};
use indexmap::IndexMap;
use skalp_frontend::hir::{
    HirEnumType, HirEnumVariant, HirStructField, HirStructType, HirType,
};

/// Range direction and bounds
#[derive(Debug, Clone)]
pub struct RangeInfo {
    pub left: i64,
    pub right: i64,
    pub direction: RangeDirection,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RangeDirection {
    To,
    Downto,
}

impl RangeInfo {
    pub fn width(&self) -> u32 {
        let size = match self.direction {
            RangeDirection::Downto => (self.left - self.right + 1).unsigned_abs() as u32,
            RangeDirection::To => (self.right - self.left + 1).unsigned_abs() as u32,
        };
        if size == 0 { 1 } else { size }
    }
}

/// Resolve a VHDL type name with optional constraint to an HirType
pub fn resolve_vhdl_type(
    type_name: &str,
    range: Option<&RangeInfo>,
    builtin_scope: &BuiltinScope,
    user_types: &IndexMap<String, HirType>,
) -> Option<HirType> {
    let lower = type_name.to_ascii_lowercase();

    // Check user-defined types first
    if let Some(ty) = user_types.get(&lower) {
        return Some(ty.clone());
    }

    match lower.as_str() {
        "std_logic" | "std_ulogic" => Some(HirType::Logic(1)),
        "std_logic_vector" | "std_ulogic_vector" => {
            let width = range.map(|r| r.width()).unwrap_or(1);
            Some(HirType::Logic(width))
        }
        "unsigned" => {
            let width = range.map(|r| r.width()).unwrap_or(1);
            Some(HirType::Nat(width))
        }
        "signed" => {
            let width = range.map(|r| r.width()).unwrap_or(1);
            Some(HirType::Int(width))
        }
        "boolean" => Some(HirType::Bool),
        "integer" => {
            if let Some(r) = range {
                // integer range 0 to M -> Nat(clog2(M+1))
                if r.left >= 0 {
                    let max_val = std::cmp::max(r.left, r.right) as u64;
                    Some(HirType::Nat(clog2(max_val + 1)))
                } else {
                    let max_abs = std::cmp::max(r.left.unsigned_abs(), r.right.unsigned_abs());
                    Some(HirType::Int(clog2(max_abs + 1) + 1))
                }
            } else {
                Some(HirType::Nat(32))
            }
        }
        "natural" => Some(HirType::Nat(32)),
        "positive" => Some(HirType::Nat(32)),
        "bit" => Some(HirType::Logic(1)),
        "bit_vector" => {
            let width = range.map(|r| r.width()).unwrap_or(1);
            Some(HirType::Logic(width))
        }
        _ => {
            // Try builtin scope
            builtin_scope.resolve_builtin_type(&lower)
        }
    }
}

/// Create an HirType::Enum from VHDL enum variants
pub fn make_enum_type(name: &str, variants: &[String]) -> HirType {
    let width = clog2(variants.len() as u64);
    HirType::Enum(Box::new(HirEnumType {
        name: name.to_string(),
        variants: variants
            .iter()
            .map(|v| HirEnumVariant {
                name: v.clone(),
                value: None,
                associated_data: None,
            })
            .collect(),
        base_type: Box::new(HirType::Nat(width)),
    }))
}

/// Create an HirType::Struct from VHDL record fields
pub fn make_struct_type(name: &str, fields: &[(String, HirType)]) -> HirType {
    HirType::Struct(HirStructType {
        name: name.to_string(),
        fields: fields
            .iter()
            .map(|(n, t)| HirStructField {
                name: n.clone(),
                field_type: t.clone(),
            })
            .collect(),
        packed: true,
    })
}

/// Create an HirType::Array from VHDL array type definition
pub fn make_array_type(element_type: HirType, size: u32) -> HirType {
    HirType::Array(Box::new(element_type), size)
}
