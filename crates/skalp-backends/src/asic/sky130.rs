//! SkyWater 130nm ASIC synthesis flow
//!
//! Implements synthesis for the open-source SkyWater 130nm process.

use crate::{BackendResult, SynthesisResults, LogMessage, LogLevel};
use crate::asic::AsicConfig;
use std::path::Path;

/// Synthesize design for SkyWater 130nm process
pub async fn synthesize_sky130(
    verilog: &str,
    temp_dir: &Path,
    config: &AsicConfig,
) -> BackendResult<SynthesisResults> {
    // Delegate to generic flow with Sky130-specific parameters
    crate::asic::generic::synthesize_generic(
        verilog,
        "sky130_fd_sc_hd",
        "130nm",
        temp_dir,
        config,
    ).await
}