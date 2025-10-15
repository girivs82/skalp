//! Signal and Port Naming Utilities
//!
//! This module provides the single source of truth for naming conventions used
//! when flattening composite types and creating hierarchical instances.
//!
//! **Purpose:** Ensure consistent naming across all compiler passes.
//! All signal/port name generation should use this module for consistency.
//!
//! **Usage:**
//! - HIR→MIR transformation: Use to create flattened signal/port names
//! - Instance elaboration: Use to create hierarchical signal names
//! - Codegen: Use to ensure consistent naming in generated code
//!
//! **Naming Conventions:**
//! - Struct fields: `base_field` (e.g., `vertex_position`)
//! - Nested fields: `base_field1_field2` (e.g., `vertex_position_x`)
//! - Array elements: `base_index` (e.g., `data_0`, `data_1`)
//! - Hierarchical instances: `instance.signal` (e.g., `fifo.wr_ptr`)

/// Standard naming for flattened struct fields
///
/// # Arguments
/// * `base` - Base signal/port name
/// * `field` - Field name
///
/// # Returns
/// Flattened field name following convention
///
/// # Example
/// ```
/// use skalp_mir::signal_naming::make_field_name;
/// assert_eq!(make_field_name("vertex", "position"), "vertex_position");
/// ```
pub fn make_field_name(base: &str, field: &str) -> String {
    format!("{}_{}", base, field)
}

/// Standard naming for nested struct field paths
///
/// # Arguments
/// * `base` - Base signal/port name
/// * `field_path` - Chain of field names
///
/// # Returns
/// Fully qualified field name
///
/// # Example
/// ```
/// use skalp_mir::signal_naming::make_nested_field_name;
/// assert_eq!(
///     make_nested_field_name("vertex", &["position", "x"]),
///     "vertex_position_x"
/// );
/// ```
pub fn make_nested_field_name(base: &str, field_path: &[&str]) -> String {
    if field_path.is_empty() {
        base.to_string()
    } else {
        format!("{}_{}", base, field_path.join("_"))
    }
}

/// Standard naming for array elements
///
/// # Arguments
/// * `base` - Base array name
/// * `index` - Element index
///
/// # Returns
/// Array element name following convention
///
/// # Example
/// ```
/// use skalp_mir::signal_naming::make_array_element_name;
/// assert_eq!(make_array_element_name("data", 0), "data_0");
/// assert_eq!(make_array_element_name("data", 42), "data_42");
/// ```
pub fn make_array_element_name(base: &str, index: usize) -> String {
    format!("{}_{}", base, index)
}

/// Standard naming for hierarchical instance signals
///
/// # Arguments
/// * `instance` - Instance name
/// * `signal` - Signal name within the instance
///
/// # Returns
/// Hierarchical signal name following convention
///
/// # Example
/// ```
/// use skalp_mir::signal_naming::make_instance_signal_name;
/// assert_eq!(make_instance_signal_name("fifo", "wr_ptr"), "fifo.wr_ptr");
/// ```
pub fn make_instance_signal_name(instance: &str, signal: &str) -> String {
    format!("{}.{}", instance, signal)
}

/// Standard naming for nested instance signals (multi-level hierarchy)
///
/// # Arguments
/// * `instance_path` - Chain of instance names (outermost to innermost)
/// * `signal` - Signal name
///
/// # Returns
/// Fully qualified hierarchical signal name
///
/// # Example
/// ```
/// use skalp_mir::signal_naming::make_nested_instance_signal_name;
/// assert_eq!(
///     make_nested_instance_signal_name(&["stage1", "fifo"], "wr_ptr"),
///     "stage1.fifo.wr_ptr"
/// );
/// ```
pub fn make_nested_instance_signal_name(instance_path: &[&str], signal: &str) -> String {
    if instance_path.is_empty() {
        signal.to_string()
    } else {
        format!("{}.{}", instance_path.join("."), signal)
    }
}

/// Parse a flattened field name back into components
///
/// # Arguments
/// * `flattened_name` - Flattened name (e.g., "vertex_position_x")
///
/// # Returns
/// (base_name, field_path) tuple
///
/// # Example
/// ```
/// use skalp_mir::signal_naming::parse_field_name;
/// let (base, fields) = parse_field_name("vertex_position_x");
/// assert_eq!(base, "vertex");
/// assert_eq!(fields, vec!["position", "x"]);
/// ```
///
/// # Note
/// This is a best-effort parsing and assumes the naming convention was followed.
/// For complex names with underscores in the base or field names, parsing may be ambiguous.
pub fn parse_field_name(flattened_name: &str) -> (String, Vec<String>) {
    let parts: Vec<&str> = flattened_name.split('_').collect();
    if parts.is_empty() {
        return (String::new(), Vec::new());
    }

    let base = parts[0].to_string();
    let fields: Vec<String> = parts[1..].iter().map(|s| s.to_string()).collect();

    (base, fields)
}

/// Parse a hierarchical signal name into instance and signal components
///
/// # Arguments
/// * `hierarchical_name` - Hierarchical name (e.g., "stage1.fifo.wr_ptr")
///
/// # Returns
/// (instance_path, signal_name) tuple
///
/// # Example
/// ```
/// use skalp_mir::signal_naming::parse_instance_signal_name;
/// let (instances, signal) = parse_instance_signal_name("stage1.fifo.wr_ptr");
/// assert_eq!(instances, vec!["stage1", "fifo"]);
/// assert_eq!(signal, "wr_ptr");
/// ```
pub fn parse_instance_signal_name(hierarchical_name: &str) -> (Vec<String>, String) {
    let parts: Vec<&str> = hierarchical_name.split('.').collect();
    if parts.is_empty() {
        return (Vec::new(), String::new());
    }

    let signal = parts[parts.len() - 1].to_string();
    let instances: Vec<String> = parts[..parts.len() - 1].iter().map(|s| s.to_string()).collect();

    (instances, signal)
}

/// Check if a name follows the flattened field convention
///
/// # Arguments
/// * `name` - Name to check
///
/// # Returns
/// true if the name contains underscores (likely a flattened field)
/// but is NOT a hierarchical name (containing dots)
///
/// # Example
/// ```
/// use skalp_mir::signal_naming::is_flattened_name;
/// assert!(is_flattened_name("vertex_position_x"));
/// assert!(!is_flattened_name("clk"));
/// assert!(!is_flattened_name("fifo.wr_ptr")); // hierarchical, not flattened
/// ```
pub fn is_flattened_name(name: &str) -> bool {
    // Hierarchical names (with dots) take precedence over flattened names
    !name.contains('.') && name.contains('_')
}

/// Check if a name follows the hierarchical convention
///
/// # Arguments
/// * `name` - Name to check
///
/// # Returns
/// true if the name contains dots (hierarchical instance signal)
///
/// # Example
/// ```
/// use skalp_mir::signal_naming::is_hierarchical_name;
/// assert!(is_hierarchical_name("fifo.wr_ptr"));
/// assert!(!is_hierarchical_name("wr_ptr"));
/// ```
pub fn is_hierarchical_name(name: &str) -> bool {
    name.contains('.')
}

/// Escape signal names for use in generated code
///
/// Some target languages (e.g., SystemVerilog) have restrictions on identifier names.
/// This function ensures names are valid for the target.
///
/// # Arguments
/// * `name` - Signal/port name
///
/// # Returns
/// Escaped name safe for use in generated code
///
/// # Example
/// ```
/// use skalp_mir::signal_naming::escape_name;
/// // Dots are replaced with underscores for SystemVerilog compatibility
/// assert_eq!(escape_name("fifo.wr_ptr"), "fifo_wr_ptr");
/// ```
pub fn escape_name(name: &str) -> String {
    // Replace dots with underscores for SystemVerilog compatibility
    name.replace('.', "_")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_make_field_name() {
        assert_eq!(make_field_name("vertex", "position"), "vertex_position");
        assert_eq!(make_field_name("data", "x"), "data_x");
    }

    #[test]
    fn test_make_nested_field_name() {
        assert_eq!(
            make_nested_field_name("vertex", &["position", "x"]),
            "vertex_position_x"
        );
        assert_eq!(
            make_nested_field_name("vertex", &["color", "r"]),
            "vertex_color_r"
        );
        assert_eq!(make_nested_field_name("vertex", &[]), "vertex");
    }

    #[test]
    fn test_make_array_element_name() {
        assert_eq!(make_array_element_name("data", 0), "data_0");
        assert_eq!(make_array_element_name("buffer", 42), "buffer_42");
    }

    #[test]
    fn test_make_instance_signal_name() {
        assert_eq!(
            make_instance_signal_name("fifo", "wr_ptr"),
            "fifo.wr_ptr"
        );
        assert_eq!(
            make_instance_signal_name("processor", "busy"),
            "processor.busy"
        );
    }

    #[test]
    fn test_make_nested_instance_signal_name() {
        assert_eq!(
            make_nested_instance_signal_name(&["stage1", "fifo"], "wr_ptr"),
            "stage1.fifo.wr_ptr"
        );
        assert_eq!(
            make_nested_instance_signal_name(&[], "signal"),
            "signal"
        );
    }

    #[test]
    fn test_parse_field_name() {
        let (base, fields) = parse_field_name("vertex_position_x");
        assert_eq!(base, "vertex");
        assert_eq!(fields, vec!["position", "x"]);

        let (base2, fields2) = parse_field_name("clk");
        assert_eq!(base2, "clk");
        assert_eq!(fields2, Vec::<String>::new());
    }

    #[test]
    fn test_parse_instance_signal_name() {
        let (instances, signal) = parse_instance_signal_name("stage1.fifo.wr_ptr");
        assert_eq!(instances, vec!["stage1", "fifo"]);
        assert_eq!(signal, "wr_ptr");

        let (instances2, signal2) = parse_instance_signal_name("signal");
        assert_eq!(instances2, Vec::<String>::new());
        assert_eq!(signal2, "signal");
    }

    #[test]
    fn test_is_flattened_name() {
        assert!(is_flattened_name("vertex_position_x"));
        assert!(is_flattened_name("data_0"));
        assert!(!is_flattened_name("clk"));
        assert!(!is_flattened_name("fifo.wr_ptr"));
    }

    #[test]
    fn test_is_hierarchical_name() {
        assert!(is_hierarchical_name("fifo.wr_ptr"));
        assert!(is_hierarchical_name("stage1.fifo.wr_ptr"));
        assert!(!is_hierarchical_name("wr_ptr"));
        assert!(!is_hierarchical_name("vertex_position_x"));
    }

    #[test]
    fn test_escape_name() {
        assert_eq!(escape_name("fifo.wr_ptr"), "fifo_wr_ptr");
        assert_eq!(escape_name("stage1.fifo.rd_ptr"), "stage1_fifo_rd_ptr");
        assert_eq!(escape_name("simple_name"), "simple_name");
    }
}
