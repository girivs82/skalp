# SKALP TODO

## Deferred Tasks

### Physical Routing (Third-party toolchain integration)

These tests are ignored as they require deep integration with specific third-party FPGA tools that we likely won't use extensively:

1. **VTR Routing** (`tests/test_native_place_route.rs:1183`)
   - Test: `test_vtr_routing_flow`
   - Status: Ignored - "VTR routing implementation incomplete"
   - Notes: Requires VTR (Verilog-to-Routing) toolchain integration

2. **OpenFPGA Routing** (`tests/test_native_place_route.rs:1363`)
   - Test: `test_openfpga_routing_flow`
   - Status: Ignored - "OpenFPGA routing implementation incomplete"
   - Notes: Requires OpenFPGA toolchain integration

**Priority**: Low - These are specific to third-party FPGA backend tools. Will revisit if/when we need deep integration with these specific toolchains.
