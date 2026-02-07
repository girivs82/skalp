//! Shared type definitions for code generation backends
//!
//! This module contains type mappings, width calculations, and alignment rules
//! that are shared between Metal and C++ backends.

use crate::sir::{SirModule, SirType};
use std::collections::HashMap;

/// Configuration for which backend is being generated
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BackendTarget {
    /// Metal Shading Language (macOS GPU)
    Metal,
    /// Standard C++ (CPU simulation)
    Cpp,
}

/// Type mapping information for a signal
#[derive(Debug, Clone)]
pub struct TypeInfo {
    /// Base type name (e.g., "uint", "uint2", "float")
    pub base_type: String,
    /// Array size if this is an array type
    pub array_size: Option<usize>,
    /// Whether the signal is decomposed into multiple parts (for >256-bit signals)
    pub is_decomposed: bool,
    /// Number of decomposed parts (only relevant if is_decomposed is true)
    pub num_parts: usize,
    /// Width per part in bits (only relevant if is_decomposed is true)
    pub part_width: usize,
}

/// Type mapper that handles conversions between SIR types and target language types
pub struct TypeMapper {
    /// The backend target (Metal or Cpp)
    pub target: BackendTarget,
    /// Track signals that need to be decomposed (width > 256 bits)
    /// Maps signal name to (total_width, num_parts, part_width)
    pub wide_signal_decomposition: HashMap<String, (usize, usize, usize)>,
}

impl TypeMapper {
    pub fn new(target: BackendTarget) -> Self {
        Self {
            target,
            wide_signal_decomposition: HashMap::new(),
        }
    }

    /// Sanitize signal names by replacing dots with underscores
    /// This handles hierarchical signal names like "stage1.reg" -> "stage1_reg"
    pub fn sanitize_name(&self, name: &str) -> String {
        name.replace('.', "_")
    }

    /// Get the target language type for a given bit width
    /// Returns (base_type, optional_array_size)
    pub fn get_type_for_width(&self, width: usize) -> (String, Option<usize>) {
        match self.target {
            BackendTarget::Metal => self.get_metal_type_for_width(width),
            BackendTarget::Cpp => self.get_cpp_type_for_width(width),
        }
    }

    /// Get Metal type for a given bit width
    fn get_metal_type_for_width(&self, width: usize) -> (String, Option<usize>) {
        match width {
            0 => ("uint".to_string(), None), // Default to uint for zero-width
            1..=32 => ("uint".to_string(), None),
            33..=64 => ("uint2".to_string(), None),
            65..=128 => ("uint4".to_string(), None),
            _ => {
                // For very wide signals (>128 bits), use arrays
                let array_size = width.div_ceil(32);
                ("uint".to_string(), Some(array_size))
            }
        }
    }

    /// Get C++ type for a given bit width
    fn get_cpp_type_for_width(&self, width: usize) -> (String, Option<usize>) {
        match width {
            0 => ("uint32_t".to_string(), None),
            1..=32 => ("uint32_t".to_string(), None),
            33..=64 => ("uint64_t".to_string(), None),
            _ => {
                // For very wide signals (>64 bits), use arrays of uint32_t
                let array_size = width.div_ceil(32);
                ("uint32_t".to_string(), Some(array_size))
            }
        }
    }

    /// Get struct field type parts for a SIR type
    /// Returns (base_type, array_suffix) for struct field declarations
    pub fn get_struct_field_parts(&self, sir_type: &SirType) -> (String, String) {
        match self.target {
            BackendTarget::Metal => self.get_metal_struct_field_parts(sir_type),
            BackendTarget::Cpp => self.get_cpp_struct_field_parts(sir_type),
        }
    }

    /// Get Metal struct field type parts
    fn get_metal_struct_field_parts(&self, sir_type: &SirType) -> (String, String) {
        match sir_type {
            SirType::Bits(width) | SirType::SignedBits(width) => {
                let (base, arr) = self.get_metal_type_for_width(*width);
                if let Some(size) = arr {
                    (base, format!("[{}]", size))
                } else {
                    (base, String::new())
                }
            }
            SirType::Float16 => ("half".to_string(), String::new()),
            SirType::Float32 => ("float".to_string(), String::new()),
            SirType::Float64 => ("double".to_string(), String::new()),
            SirType::Vec2(elem) => {
                let elem_type = match elem.as_ref() {
                    SirType::Float32 => "float2",
                    SirType::Float16 => "half2",
                    _ => "uint2",
                };
                (elem_type.to_string(), String::new())
            }
            SirType::Vec3(elem) => {
                let elem_type = match elem.as_ref() {
                    SirType::Float32 => "float3",
                    SirType::Float16 => "half3",
                    _ => "uint3",
                };
                (elem_type.to_string(), String::new())
            }
            SirType::Vec4(elem) => {
                let elem_type = match elem.as_ref() {
                    SirType::Float32 => "float4",
                    SirType::Float16 => "half4",
                    _ => "uint4",
                };
                (elem_type.to_string(), String::new())
            }
            SirType::Array(elem_type, size) => {
                let (elem_base, elem_suffix) = self.get_metal_struct_field_parts(elem_type);
                let suffix = format!("[{}]{}", size, elem_suffix);
                (elem_base, suffix)
            }
        }
    }

    /// Get C++ struct field type parts
    fn get_cpp_struct_field_parts(&self, sir_type: &SirType) -> (String, String) {
        match sir_type {
            SirType::Bits(width) | SirType::SignedBits(width) => {
                let (base, arr) = self.get_cpp_type_for_width(*width);
                if let Some(size) = arr {
                    (base, format!("[{}]", size))
                } else {
                    (base, String::new())
                }
            }
            SirType::Float16 => ("uint16_t".to_string(), String::new()), // C++ uses uint16_t for fp16 storage
            SirType::Float32 => ("float".to_string(), String::new()),
            SirType::Float64 => ("double".to_string(), String::new()),
            SirType::Vec2(elem) => {
                let elem_width = elem.width();
                let total_width = elem_width * 2;
                let (base, arr) = self.get_cpp_type_for_width(total_width);
                if let Some(size) = arr {
                    (base, format!("[{}]", size))
                } else {
                    (base, String::new())
                }
            }
            SirType::Vec3(elem) => {
                let elem_width = elem.width();
                let total_width = elem_width * 3;
                let (base, arr) = self.get_cpp_type_for_width(total_width);
                if let Some(size) = arr {
                    (base, format!("[{}]", size))
                } else {
                    (base, String::new())
                }
            }
            SirType::Vec4(elem) => {
                let elem_width = elem.width();
                let total_width = elem_width * 4;
                let (base, arr) = self.get_cpp_type_for_width(total_width);
                if let Some(size) = arr {
                    (base, format!("[{}]", size))
                } else {
                    (base, String::new())
                }
            }
            SirType::Array(elem_type, size) => {
                let (elem_base, elem_suffix) = self.get_cpp_struct_field_parts(elem_type);
                let suffix = format!("[{}]{}", size, elem_suffix);
                (elem_base, suffix)
            }
        }
    }

    /// Get signal width from SIR module
    pub fn get_signal_width(&self, sir: &SirModule, signal_name: &str) -> usize {
        // Check outputs first (explicit type declarations)
        if let Some(output) = sir.outputs.iter().find(|o| o.name == signal_name) {
            return output.sir_type.width();
        }
        // Check inputs
        if let Some(input) = sir.inputs.iter().find(|i| i.name == signal_name) {
            return input.sir_type.width();
        }
        // Check signals
        if let Some(sig) = sir.signals.iter().find(|s| s.name == signal_name) {
            return sig.sir_type.width();
        }
        // Check state elements
        if let Some(state) = sir.state_elements.get(signal_name) {
            return state.width;
        }
        // Default
        32
    }

    /// Get signal SIR type from module
    pub fn get_signal_type(&self, sir: &SirModule, signal_name: &str) -> Option<SirType> {
        // Check signals
        if let Some(sig) = sir.signals.iter().find(|s| s.name == signal_name) {
            return Some(sig.sir_type.clone());
        }
        // Check inputs
        if let Some(input) = sir.inputs.iter().find(|i| i.name == signal_name) {
            return Some(input.sir_type.clone());
        }
        // Check outputs
        if let Some(output) = sir.outputs.iter().find(|o| o.name == signal_name) {
            return Some(output.sir_type.clone());
        }
        None
    }

    /// Check if a signal should be decomposed (>256 bits)
    /// and register its decomposition info if so
    pub fn check_decomposition(&mut self, name: &str, width: usize) -> bool {
        let sanitized = self.sanitize_name(name);

        if width > 256 {
            let num_parts = width.div_ceil(256);
            let part_width = 256;
            self.wide_signal_decomposition
                .insert(sanitized, (width, num_parts, part_width));
            true
        } else {
            false
        }
    }

    /// Get type info for a signal including decomposition details
    pub fn get_type_info(&self, sir: &SirModule, signal_name: &str) -> TypeInfo {
        let width = self.get_signal_width(sir, signal_name);
        let sanitized = self.sanitize_name(signal_name);

        if let Some((_total_width, num_parts, part_width)) =
            self.wide_signal_decomposition.get(&sanitized)
        {
            TypeInfo {
                base_type: "uint".to_string(),
                array_size: Some(8), // 256 bits / 32 = 8 elements per part
                is_decomposed: true,
                num_parts: *num_parts,
                part_width: *part_width,
            }
        } else {
            let (base_type, array_size) = self.get_type_for_width(width);
            TypeInfo {
                base_type,
                array_size,
                is_decomposed: false,
                num_parts: 1,
                part_width: width,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_metal_type_mapping() {
        let mapper = TypeMapper::new(BackendTarget::Metal);

        assert_eq!(mapper.get_type_for_width(1), ("uint".to_string(), None));
        assert_eq!(mapper.get_type_for_width(32), ("uint".to_string(), None));
        assert_eq!(mapper.get_type_for_width(64), ("uint2".to_string(), None));
        assert_eq!(mapper.get_type_for_width(128), ("uint4".to_string(), None));
        assert_eq!(mapper.get_type_for_width(256), ("uint".to_string(), Some(8)));
    }

    #[test]
    fn test_cpp_type_mapping() {
        let mapper = TypeMapper::new(BackendTarget::Cpp);

        assert_eq!(mapper.get_type_for_width(1), ("uint32_t".to_string(), None));
        assert_eq!(mapper.get_type_for_width(32), ("uint32_t".to_string(), None));
        assert_eq!(mapper.get_type_for_width(64), ("uint64_t".to_string(), None));
        assert_eq!(mapper.get_type_for_width(128), ("uint32_t".to_string(), Some(4)));
    }

    #[test]
    fn test_sanitize_name() {
        let mapper = TypeMapper::new(BackendTarget::Metal);
        assert_eq!(mapper.sanitize_name("stage1.reg"), "stage1_reg");
        assert_eq!(mapper.sanitize_name("simple_name"), "simple_name");
    }
}
