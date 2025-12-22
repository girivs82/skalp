//! Constraint validation logic

use crate::constraints::devices::Device;
use crate::hir::{HirType, PhysicalConstraints, PinLocation};
use std::collections::HashSet;
use thiserror::Error;

/// Constraint validation errors
#[derive(Debug, Error)]
pub enum ConstraintError {
    #[error("Invalid pin '{pin}' for device '{device}'")]
    InvalidPin { pin: String, device: String },

    #[error("Pin '{pin}' is used multiple times")]
    DuplicatePin { pin: String },

    #[error("I/O standard '{standard}' is not supported on device '{device}'")]
    UnsupportedIoStandard { standard: String, device: String },

    #[error("Pin count mismatch: expected {expected} pins for bus width, got {actual}")]
    PinCountMismatch { expected: usize, actual: usize },

    #[error("Invalid differential pair: {positive} and {negative} are not a valid pair")]
    InvalidDifferentialPair { positive: String, negative: String },

    #[error("Bank voltage mismatch: pins in bank {bank} have conflicting voltage requirements")]
    BankVoltageMismatch { bank: u32 },

    #[error("Missing differential pair: {which} pin specified without {missing}")]
    IncompleteDifferentialPair { which: String, missing: String },
}

/// Validator for physical constraints
pub struct ConstraintValidator<'a> {
    device: &'a Device,
    used_pins: HashSet<String>,
}

impl<'a> ConstraintValidator<'a> {
    /// Create a new constraint validator for a device
    pub fn new(device: &'a Device) -> Self {
        ConstraintValidator {
            device,
            used_pins: HashSet::new(),
        }
    }

    /// Validate physical constraints for a port
    pub fn validate_port_constraints(
        &mut self,
        port_name: &str,
        port_type: &HirType,
        constraints: &PhysicalConstraints,
    ) -> Result<(), ConstraintError> {
        // Validate pin location
        if let Some(ref pin_loc) = constraints.pin_location {
            self.validate_pin_location(port_type, pin_loc)?;
        }

        // Validate I/O standard
        if let Some(ref io_std) = constraints.io_standard {
            self.validate_io_standard(io_std)?;
        }

        Ok(())
    }

    /// Validate pin location against device and port type
    fn validate_pin_location(
        &mut self,
        port_type: &HirType,
        pin_location: &PinLocation,
    ) -> Result<(), ConstraintError> {
        match pin_location {
            PinLocation::Single(pin) => {
                self.validate_single_pin(pin)?;
            }
            PinLocation::Multiple(pins) => {
                // Check if port type is an array
                if let HirType::Array(_, size) = port_type {
                    if pins.len() != *size as usize {
                        return Err(ConstraintError::PinCountMismatch {
                            expected: *size as usize,
                            actual: pins.len(),
                        });
                    }
                }

                // Validate each pin
                for pin in pins {
                    self.validate_single_pin(pin)?;
                }
            }
            PinLocation::Differential { positive, negative } => {
                self.validate_differential_pair(positive, negative)?;
            }
        }

        Ok(())
    }

    /// Validate a single pin
    fn validate_single_pin(&mut self, pin: &str) -> Result<(), ConstraintError> {
        // Check if pin exists on device
        if !self.device.has_pin(pin) {
            return Err(ConstraintError::InvalidPin {
                pin: pin.to_string(),
                device: self.device.name.clone(),
            });
        }

        // Check if pin is already used
        if self.used_pins.contains(pin) {
            return Err(ConstraintError::DuplicatePin {
                pin: pin.to_string(),
            });
        }

        // Mark pin as used
        self.used_pins.insert(pin.to_string());

        Ok(())
    }

    /// Validate a differential pair
    fn validate_differential_pair(
        &mut self,
        positive: &str,
        negative: &str,
    ) -> Result<(), ConstraintError> {
        // Validate both pins exist
        self.validate_single_pin(positive)?;
        self.validate_single_pin(negative)?;

        // Check if they form a valid differential pair
        if !self.device.is_valid_differential_pair(positive, negative) {
            return Err(ConstraintError::InvalidDifferentialPair {
                positive: positive.to_string(),
                negative: negative.to_string(),
            });
        }

        Ok(())
    }

    /// Validate I/O standard
    fn validate_io_standard(&self, standard: &str) -> Result<(), ConstraintError> {
        if !self.device.supports_io_standard(standard) {
            return Err(ConstraintError::UnsupportedIoStandard {
                standard: standard.to_string(),
                device: self.device.name.clone(),
            });
        }

        Ok(())
    }

    /// Get all used pins
    pub fn used_pins(&self) -> &HashSet<String> {
        &self.used_pins
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::constraints::devices::Device;
    use crate::hir::{HirType, PhysicalConstraints, PinLocation};

    #[test]
    fn test_validate_single_pin_valid() {
        let device = Device::ice40_hx1k();
        let mut validator = ConstraintValidator::new(&device);

        let constraints = PhysicalConstraints {
            pin_location: Some(PinLocation::Single("A1".to_string())),
            io_standard: Some("LVCMOS33".to_string()),
            drive_strength: None,
            slew_rate: None,
            termination: None,
            schmitt_trigger: None,
            bank: None,
            diff_term: None,
            pad_type: None,
            pad_cell: None,
            ldo_config: None,
        };

        let port_type = HirType::Bit(1);

        assert!(validator
            .validate_port_constraints("clk", &port_type, &constraints)
            .is_ok());
    }

    #[test]
    fn test_validate_invalid_pin() {
        let device = Device::ice40_hx1k();
        let mut validator = ConstraintValidator::new(&device);

        let constraints = PhysicalConstraints {
            pin_location: Some(PinLocation::Single("Z99".to_string())),
            io_standard: None,
            drive_strength: None,
            slew_rate: None,
            termination: None,
            schmitt_trigger: None,
            bank: None,
            diff_term: None,
            pad_type: None,
            pad_cell: None,
            ldo_config: None,
        };

        let port_type = HirType::Bit(1);

        let result = validator.validate_port_constraints("clk", &port_type, &constraints);
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err(),
            ConstraintError::InvalidPin { .. }
        ));
    }

    #[test]
    fn test_validate_duplicate_pin() {
        let device = Device::ice40_hx1k();
        let mut validator = ConstraintValidator::new(&device);

        let constraints1 = PhysicalConstraints {
            pin_location: Some(PinLocation::Single("A1".to_string())),
            io_standard: None,
            drive_strength: None,
            slew_rate: None,
            termination: None,
            schmitt_trigger: None,
            bank: None,
            diff_term: None,
            pad_type: None,
            pad_cell: None,
            ldo_config: None,
        };

        let port_type = HirType::Bit(1);

        // First use should succeed
        assert!(validator
            .validate_port_constraints("clk", &port_type, &constraints1)
            .is_ok());

        // Second use of same pin should fail
        let result = validator.validate_port_constraints("rst", &port_type, &constraints1);
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err(),
            ConstraintError::DuplicatePin { .. }
        ));
    }

    #[test]
    fn test_validate_pin_array() {
        let device = Device::ice40_hx1k();
        let mut validator = ConstraintValidator::new(&device);

        let constraints = PhysicalConstraints {
            pin_location: Some(PinLocation::Multiple(vec![
                "C1".to_string(),
                "C2".to_string(),
                "C3".to_string(),
                "C4".to_string(),
            ])),
            io_standard: Some("LVCMOS33".to_string()),
            drive_strength: None,
            slew_rate: None,
            termination: None,
            schmitt_trigger: None,
            bank: None,
            diff_term: None,
            pad_type: None,
            pad_cell: None,
            ldo_config: None,
        };

        let port_type = HirType::Array(Box::new(HirType::Bit(1)), 4);

        assert!(validator
            .validate_port_constraints("data", &port_type, &constraints)
            .is_ok());
    }

    #[test]
    fn test_validate_pin_count_mismatch() {
        let device = Device::ice40_hx1k();
        let mut validator = ConstraintValidator::new(&device);

        let constraints = PhysicalConstraints {
            pin_location: Some(PinLocation::Multiple(vec![
                "C1".to_string(),
                "C2".to_string(),
            ])),
            io_standard: None,
            drive_strength: None,
            slew_rate: None,
            termination: None,
            schmitt_trigger: None,
            bank: None,
            diff_term: None,
            pad_type: None,
            pad_cell: None,
            ldo_config: None,
        };

        let port_type = HirType::Array(Box::new(HirType::Bit(1)), 4);

        let result = validator.validate_port_constraints("data", &port_type, &constraints);
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err(),
            ConstraintError::PinCountMismatch { .. }
        ));
    }

    #[test]
    fn test_validate_unsupported_io_standard() {
        let device = Device::ice40_hx1k();
        let mut validator = ConstraintValidator::new(&device);

        let constraints = PhysicalConstraints {
            pin_location: Some(PinLocation::Single("A1".to_string())),
            io_standard: Some("INVALID_STANDARD".to_string()),
            drive_strength: None,
            slew_rate: None,
            termination: None,
            schmitt_trigger: None,
            bank: None,
            diff_term: None,
            pad_type: None,
            pad_cell: None,
            ldo_config: None,
        };

        let port_type = HirType::Bit(1);

        let result = validator.validate_port_constraints("clk", &port_type, &constraints);
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err(),
            ConstraintError::UnsupportedIoStandard { .. }
        ));
    }
}
