# Timing Constraints for iCE40 FPGA
#
# Format: SDC (Synopsys Design Constraints)
# Tool: nextpnr-ice40 with --sdc option
#
# Usage:
#   nextpnr-ice40 --hx8k --sdc timing.sdc --json design.json

# ============================================================================
# Clock Definitions
# ============================================================================

# System clock: 100 MHz (10ns period)
create_clock -period 10.0 -name sys_clk [get_ports sys_clk]

# AXI clock: 50 MHz (20ns period)
# (May be same as sys_clk or divided)
create_clock -period 20.0 -name axi_clk [get_ports axi_clk]

# Video pixel clock: 25 MHz (40ns period)
# Generated from PLL
create_generated_clock -name video_pix_clk \
    -source [get_ports sys_clk] \
    -divide_by 4 \
    [get_pins pll/video_clk]

# Geometry processor clock: 100 MHz
# (Usually same as sys_clk)
create_clock -period 10.0 -name geom_clk [get_nets geom_clk]

# Rasterizer clock: 50 MHz
create_clock -period 20.0 -name rast_clk [get_nets rast_clk]

# ============================================================================
# Clock Groups (Asynchronous Clock Domains)
# ============================================================================

# Define asynchronous clock groups
set_clock_groups -asynchronous \
    -group [get_clocks sys_clk] \
    -group [get_clocks axi_clk] \
    -group [get_clocks video_pix_clk]

# Geometry and rasterizer clocks are synchronous to sys_clk
set_clock_groups -synchronous \
    -group [get_clocks sys_clk] \
    -group [get_clocks geom_clk] \
    -group [get_clocks rast_clk]

# ============================================================================
# Input Delays
# ============================================================================

# AXI interface input delays
# Assume 2ns setup time relative to AXI clock
set_input_delay -clock axi_clk -min 0.5 [get_ports axi_*]
set_input_delay -clock axi_clk -max 2.0 [get_ports axi_*]

# Reset input (asynchronous, no constraint needed)
set_false_path -from [get_ports sys_rst]

# ============================================================================
# Output Delays
# ============================================================================

# Video output delays
# Account for output buffer delay and PCB trace delay
set_output_delay -clock video_pix_clk -min 1.0 [get_ports video_*]
set_output_delay -clock video_pix_clk -max 5.0 [get_ports video_*]

# AXI response delays
set_output_delay -clock axi_clk -min 0.5 [get_ports axi_*ready]
set_output_delay -clock axi_clk -max 2.0 [get_ports axi_*ready]

# ============================================================================
# Clock Domain Crossing Constraints
# ============================================================================

# Async FIFO between geometry and rasterizer
# Gray code synchronizers have 2-3 flip-flop stages
set_max_delay -from [get_pins geometry_to_raster_fifo/wr_ptr_gray_reg*/C] \
              -to [get_pins geometry_to_raster_fifo/wr_ptr_gray_sync_reg*/D] \
              -datapath_only 20.0

set_max_delay -from [get_pins geometry_to_raster_fifo/rd_ptr_gray_reg*/C] \
              -to [get_pins geometry_to_raster_fifo/rd_ptr_gray_sync_reg*/D] \
              -datapath_only 10.0

# Video FIFO (rasterizer to video output)
set_max_delay -from [get_cells raster_to_video_fifo/wr_ptr_gray_reg*] \
              -to [get_cells raster_to_video_fifo/rd_ptr_gray_sync_reg*] \
              -datapath_only 40.0

# ============================================================================
# False Paths
# ============================================================================

# Reset paths are asynchronous
set_false_path -from [get_ports *rst*]
set_false_path -to [get_ports *rst*]

# Debug signals (LEDs) don't need tight timing
set_false_path -to [get_ports led*]

# UART debug output (low speed)
set_false_path -to [get_ports uart_tx]

# Static configuration registers
set_false_path -from [get_cells config_reg*]

# ============================================================================
# Multi-cycle Paths
# ============================================================================

# Geometry processor matrix multiply takes 4 cycles
# (If using pipelined multiply)
set_multicycle_path -setup 4 -from [get_cells geom_proc/matrix_mult*] \
                                -to [get_cells geom_proc/result_reg*]
set_multicycle_path -hold 3 -from [get_cells geom_proc/matrix_mult*] \
                               -to [get_cells geom_proc/result_reg*]

# Rasterizer interpolation (2 cycle path)
set_multicycle_path -setup 2 -from [get_cells rasterizer/interp*] \
                                -to [get_cells rasterizer/pixel_out_reg*]
set_multicycle_path -hold 1 -from [get_cells rasterizer/interp*] \
                               -to [get_cells rasterizer/pixel_out_reg*]

# ============================================================================
# Maximum Fanout Constraints
# ============================================================================

# Limit fanout for critical nets
set_max_fanout 16 [get_nets sys_clk]
set_max_fanout 8 [get_nets geom_proc/pipeline_valid*]

# ============================================================================
# Area Constraints (iCE40 specific)
# ============================================================================

# iCE40-HX8K has:
# - 7,680 logic cells
# - 128 Kbit RAM
# - 32 multipliers (DSP blocks)

# Target utilization: 70% of logic, 80% of RAM, 90% of DSPs
# (Leave headroom for routing)

# ============================================================================
# Special Constraints for Specific Paths
# ============================================================================

# FIFO full/empty flags are critical
set_max_delay 5.0 -from [get_cells *_fifo/wr_full_reg] \
                   -to [get_ports *_ready]

set_max_delay 5.0 -from [get_cells *_fifo/rd_empty_reg] \
                   -to [get_cells *_valid_reg]

# Pipeline stall logic must be fast
set_max_delay 8.0 -from [get_cells */pipeline_stall] \
                   -to [get_cells */pipeline_enable]

# ============================================================================
# Clock Uncertainty
# ============================================================================

# Add clock uncertainty for jitter and skew
set_clock_uncertainty -setup 0.5 [get_clocks sys_clk]
set_clock_uncertainty -hold 0.2 [get_clocks sys_clk]

set_clock_uncertainty -setup 0.3 [get_clocks video_pix_clk]
set_clock_uncertainty -hold 0.1 [get_clocks video_pix_clk]

# ============================================================================
# Exception Paths for Formal Verification
# ============================================================================

# When running formal verification, some paths can be ignored
# These are handled by the formal tool, not synthesis

# Coverage counters (don't affect functionality)
# set_false_path -to [get_cells *_coverage_count*]

# ============================================================================
# Notes
# ============================================================================

# iCE40 Timing Characteristics:
# - Logic delay: ~0.5-1.0ns per LUT level
# - Routing delay: ~1-3ns depending on distance
# - FF setup time: ~0.1ns
# - FF clock-to-q: ~0.5ns
# - DSP multiply: ~4ns
# - Block RAM access: ~2-3ns
#
# For 100 MHz operation (10ns period):
# - Typical path budget: 8-9ns (after clock uncertainty)
# - Allows ~6-8 logic levels
# - Or 2-3 DSP operations
#
# Optimization tips:
# - Use pipelining for long paths
# - Register DSP outputs
# - Use block RAM with output registers
# - Minimize fanout on critical nets
# - Use local routing where possible
