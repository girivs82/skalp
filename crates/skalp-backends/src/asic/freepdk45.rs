//! FreePDK45 ASIC synthesis flow
//!
//! Implements synthesis for the FreePDK45 open-source process.

use crate::{BackendResult, SynthesisResults};
use crate::asic::AsicConfig;
use std::path::Path;

/// Synthesize design for FreePDK45 process
pub async fn synthesize_freepdk45(
    verilog: &str,
    temp_dir: &Path,
    config: &AsicConfig,
) -> BackendResult<SynthesisResults> {
    // Delegate to generic flow with FreePDK45-specific parameters
    crate::asic::generic::synthesize_generic(
        verilog,
        "NangateOpenCellLibrary",
        "45nm",
        temp_dir,
        config,
    ).await
}