//! Assertion generation and management

use crate::property::Property;
use serde::{Deserialize, Serialize};

/// Hardware assertion
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Assertion {
    pub name: String,
    pub condition: String,
    pub message: String,
    pub severity: AssertionSeverity,
    pub location: Option<SourceLocation>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AssertionSeverity {
    Info,
    Warning,
    Error,
    Fatal,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SourceLocation {
    pub file: String,
    pub line: u32,
    pub column: u32,
}

impl Assertion {
    pub fn from_property(property: &Property) -> Self {
        Self {
            name: property.name.clone(),
            condition: format!("{:?}", property.formula),
            message: format!("Property {} failed", property.name),
            severity: match property.severity {
                crate::property::Severity::Info => AssertionSeverity::Info,
                crate::property::Severity::Warning => AssertionSeverity::Warning,
                crate::property::Severity::Error => AssertionSeverity::Error,
                crate::property::Severity::Critical => AssertionSeverity::Fatal,
            },
            location: None,
        }
    }

    /// Convert to SystemVerilog assertion
    pub fn to_sv_assertion(&self) -> String {
        format!(
            "assert property (@(posedge clk) {}) else $error(\"{}\");",
            self.condition, self.message
        )
    }

    /// Convert to PSL assertion
    pub fn to_psl_assertion(&self) -> String {
        format!(
            "assert always {} report \"{}\";",
            self.condition, self.message
        )
    }
}
