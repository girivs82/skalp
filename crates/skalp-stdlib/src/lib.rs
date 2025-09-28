//! SKALP Standard Library
//!
//! This crate contains the SKALP source files for standard library components.
//! The actual components are written in SKALP and stored in the components/ directory.

/// Get the list of all standard library components
pub fn get_components() -> Vec<(&'static str, &'static str)> {
    vec![
        ("counter", COUNTER_SK),
        ("fifo", FIFO_SK),
        ("shift_register", SHIFT_REGISTER_SK),
        ("uart", UART_SK),
        ("adder", ADDER_SK),
        ("multiplier", MULTIPLIER_SK),
    ]
}

// Include the SKALP component source files as string constants
// These will be created next

pub const COUNTER_SK: &str = include_str!("../components/counter.sk");
pub const FIFO_SK: &str = include_str!("../components/fifo.sk");
pub const SHIFT_REGISTER_SK: &str = include_str!("../components/shift_register.sk");
pub const UART_SK: &str = include_str!("../components/uart.sk");
pub const ADDER_SK: &str = include_str!("../components/adder.sk");
pub const MULTIPLIER_SK: &str = include_str!("../components/multiplier.sk");

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_components_available() {
        let components = get_components();
        assert!(!components.is_empty());
        assert!(components.iter().any(|(name, _)| *name == "counter"));
        assert!(components.iter().any(|(name, _)| *name == "fifo"));
    }
}
