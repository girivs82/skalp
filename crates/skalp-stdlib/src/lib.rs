#![allow(dead_code, unused_variables, unused_imports)]
//! SKALP Standard Library
//!
//! Components are context-agnostic: they work in both sync and async (NCL)
//! contexts. The parent entity's context determines interpretation:
//!
//!   - `async entity` parent: barriers become NCL completion detection points.
//!     Each stage between barriers is a self-timed async pipeline stage.
//!   - Sync clocked parent: barriers become pipeline registers.
//!   - Sync combinational parent: barriers are ignored.
//!
//! Arithmetic operators (`+`, `-`) lower to `std_adder` which maps to
//! CARRY4/CCU2 hard chains. The `*` operator lowers to `std_multiplier`
//! which decomposes into a chain of adder stages — no circular dependency.

/// Get the list of all standard library components
pub fn get_components() -> Vec<(&'static str, &'static str)> {
    vec![
        // Arithmetic (context-agnostic, barrier-annotated)
        ("adder", ADDER_SK),
        ("multiplier", MULTIPLIER_SK),
        ("comparator", COMPARATOR_SK),
        ("barrel_shifter", BARREL_SHIFTER_SK),
        // Stateful (context-agnostic, barrier-annotated)
        ("counter", COUNTER_SK),
        ("shift_register", SHIFT_REGISTER_SK),
        // Infrastructure (sync-only for now)
        ("fifo", FIFO_SK),
        ("uart", UART_SK),
        ("axi4_lite", AXI4_LITE_SK),
    ]
}

// Context-agnostic components (work in both sync and async)
pub const ADDER_SK: &str = include_str!("../components/adder.sk");
pub const MULTIPLIER_SK: &str = include_str!("../components/multiplier.sk");
pub const COMPARATOR_SK: &str = include_str!("../components/comparator.sk");
pub const BARREL_SHIFTER_SK: &str = include_str!("../components/barrel_shifter.sk");
pub const COUNTER_SK: &str = include_str!("../components/counter.sk");
pub const SHIFT_REGISTER_SK: &str = include_str!("../components/shift_register.sk");

// Sync-only components (need async variants later)
pub const FIFO_SK: &str = include_str!("../components/fifo.sk");
pub const UART_SK: &str = include_str!("../components/uart.sk");
pub const AXI4_LITE_SK: &str = include_str!("../components/axi4_lite.sk");

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
