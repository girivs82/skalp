//! Device database for FPGA pin and I/O standard definitions

use std::collections::HashMap;

/// FPGA device family
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FpgaFamily {
    /// Lattice iCE40 family
    Ice40,
    /// Lattice ECP5 family
    Ecp5,
    /// Xilinx 7-Series family
    Xilinx7Series,
    /// Intel Stratix family
    IntelStratix,
}

/// FPGA device with pin and I/O standard definitions
#[derive(Debug, Clone)]
pub struct Device {
    /// Device name (e.g., "iCE40HX8K-CT256")
    pub name: String,
    /// Device family
    pub family: FpgaFamily,
    /// Available pins
    pub pins: Vec<Pin>,
    /// I/O banks
    pub banks: Vec<Bank>,
    /// Supported I/O standards
    pub supported_io_standards: Vec<String>,
}

/// Physical pin definition
#[derive(Debug, Clone)]
pub struct Pin {
    /// Pin name (e.g., "A1", "B2")
    pub name: String,
    /// I/O bank this pin belongs to
    pub bank: u32,
    /// Differential pair partner (if any)
    pub differential_pair: Option<String>,
}

/// I/O bank definition
#[derive(Debug, Clone)]
pub struct Bank {
    /// Bank identifier
    pub id: u32,
    /// Supported voltages (e.g., ["1.2V", "3.3V"])
    pub supported_voltages: Vec<String>,
}

impl Device {
    /// Create an iCE40HX1K-TQ144 device definition
    pub fn ice40_hx1k() -> Self {
        // Basic iCE40HX1K with common pins
        // In a real implementation, this would have all 144 pins
        let pins = vec![
            Pin {
                name: "A1".to_string(),
                bank: 0,
                differential_pair: None,
            },
            Pin {
                name: "A2".to_string(),
                bank: 0,
                differential_pair: None,
            },
            Pin {
                name: "B1".to_string(),
                bank: 0,
                differential_pair: None,
            },
            Pin {
                name: "B2".to_string(),
                bank: 0,
                differential_pair: None,
            },
            Pin {
                name: "C1".to_string(),
                bank: 1,
                differential_pair: None,
            },
            Pin {
                name: "C2".to_string(),
                bank: 1,
                differential_pair: None,
            },
            Pin {
                name: "C3".to_string(),
                bank: 1,
                differential_pair: None,
            },
            Pin {
                name: "C4".to_string(),
                bank: 1,
                differential_pair: None,
            },
            Pin {
                name: "D1".to_string(),
                bank: 1,
                differential_pair: None,
            },
            Pin {
                name: "D2".to_string(),
                bank: 1,
                differential_pair: None,
            },
            Pin {
                name: "D3".to_string(),
                bank: 1,
                differential_pair: None,
            },
            Pin {
                name: "D4".to_string(),
                bank: 1,
                differential_pair: None,
            },
        ];

        let banks = vec![
            Bank {
                id: 0,
                supported_voltages: vec![
                    "1.2V".to_string(),
                    "1.8V".to_string(),
                    "2.5V".to_string(),
                    "3.3V".to_string(),
                ],
            },
            Bank {
                id: 1,
                supported_voltages: vec![
                    "1.2V".to_string(),
                    "1.8V".to_string(),
                    "2.5V".to_string(),
                    "3.3V".to_string(),
                ],
            },
        ];

        Device {
            name: "iCE40HX1K-TQ144".to_string(),
            family: FpgaFamily::Ice40,
            pins,
            banks,
            supported_io_standards: vec![
                "LVCMOS33".to_string(),
                "LVCMOS25".to_string(),
                "LVCMOS18".to_string(),
                "LVCMOS12".to_string(),
                "LVDS_25".to_string(),
                "SB_LVCMOS".to_string(),
            ],
        }
    }

    /// Create an iCE40HX8K-CT256 device definition
    pub fn ice40_hx8k() -> Self {
        // Basic iCE40HX8K with common pins
        // In a real implementation, this would have all 256 pins
        let pins = vec![
            Pin {
                name: "A1".to_string(),
                bank: 0,
                differential_pair: None,
            },
            Pin {
                name: "A2".to_string(),
                bank: 0,
                differential_pair: None,
            },
            Pin {
                name: "B1".to_string(),
                bank: 0,
                differential_pair: None,
            },
            Pin {
                name: "B2".to_string(),
                bank: 0,
                differential_pair: None,
            },
            Pin {
                name: "C1".to_string(),
                bank: 1,
                differential_pair: None,
            },
            Pin {
                name: "C2".to_string(),
                bank: 1,
                differential_pair: None,
            },
            // Add more pins as needed
        ];

        let banks = vec![
            Bank {
                id: 0,
                supported_voltages: vec![
                    "1.2V".to_string(),
                    "1.8V".to_string(),
                    "2.5V".to_string(),
                    "3.3V".to_string(),
                ],
            },
            Bank {
                id: 1,
                supported_voltages: vec![
                    "1.2V".to_string(),
                    "1.8V".to_string(),
                    "2.5V".to_string(),
                    "3.3V".to_string(),
                ],
            },
        ];

        Device {
            name: "iCE40HX8K-CT256".to_string(),
            family: FpgaFamily::Ice40,
            pins,
            banks,
            supported_io_standards: vec![
                "LVCMOS33".to_string(),
                "LVCMOS25".to_string(),
                "LVCMOS18".to_string(),
                "LVCMOS12".to_string(),
                "LVDS_25".to_string(),
                "SB_LVCMOS".to_string(),
            ],
        }
    }

    /// Check if a pin exists on this device
    pub fn has_pin(&self, pin_name: &str) -> bool {
        self.pins.iter().any(|p| p.name == pin_name)
    }

    /// Get a pin by name
    pub fn get_pin(&self, pin_name: &str) -> Option<&Pin> {
        self.pins.iter().find(|p| p.name == pin_name)
    }

    /// Check if an I/O standard is supported
    pub fn supports_io_standard(&self, standard: &str) -> bool {
        self.supported_io_standards.iter().any(|s| s == standard)
    }

    /// Check if two pins form a valid differential pair
    pub fn is_valid_differential_pair(&self, positive: &str, negative: &str) -> bool {
        if let Some(pin) = self.get_pin(positive) {
            if let Some(ref pair) = pin.differential_pair {
                return pair == negative;
            }
        }
        false
    }

    /// Get the bank a pin belongs to
    pub fn get_pin_bank(&self, pin_name: &str) -> Option<u32> {
        self.get_pin(pin_name).map(|p| p.bank)
    }
}

/// Device database for looking up FPGA devices
pub struct DeviceDatabase {
    devices: HashMap<String, Device>,
}

impl DeviceDatabase {
    /// Create a new device database with built-in devices
    pub fn new() -> Self {
        let mut devices = HashMap::new();

        // Add iCE40 devices
        let hx1k = Device::ice40_hx1k();
        devices.insert(hx1k.name.clone(), hx1k);

        let hx8k = Device::ice40_hx8k();
        devices.insert(hx8k.name.clone(), hx8k);

        DeviceDatabase { devices }
    }

    /// Get a device by name
    pub fn get_device(&self, name: &str) -> Option<&Device> {
        self.devices.get(name)
    }

    /// Add a custom device definition
    pub fn add_device(&mut self, device: Device) {
        self.devices.insert(device.name.clone(), device);
    }
}

impl Default for DeviceDatabase {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ice40_hx1k_has_pins() {
        let device = Device::ice40_hx1k();
        assert!(device.has_pin("A1"));
        assert!(device.has_pin("B2"));
        assert!(!device.has_pin("Z99"));
    }

    #[test]
    fn test_ice40_supports_io_standards() {
        let device = Device::ice40_hx1k();
        assert!(device.supports_io_standard("LVCMOS33"));
        assert!(device.supports_io_standard("LVDS_25"));
        assert!(!device.supports_io_standard("INVALID_STANDARD"));
    }

    #[test]
    fn test_device_database() {
        let db = DeviceDatabase::new();
        assert!(db.get_device("iCE40HX1K-TQ144").is_some());
        assert!(db.get_device("iCE40HX8K-CT256").is_some());
        assert!(db.get_device("NonExistent").is_none());
    }

    #[test]
    fn test_get_pin_bank() {
        let device = Device::ice40_hx1k();
        assert_eq!(device.get_pin_bank("A1"), Some(0));
        assert_eq!(device.get_pin_bank("C1"), Some(1));
        assert_eq!(device.get_pin_bank("Z99"), None);
    }
}
