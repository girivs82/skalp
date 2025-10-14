# Timing Constraints for Xilinx FPGA
#
# Target: Xilinx 7-series (Artix-7, Kintex-7, Virtex-7)
# Format: XDC (Xilinx Design Constraints)
# Tool: Vivado
#
# Usage:
#   read_xdc timing.xdc
#   link_design -top graphics_pipeline_top -part xc7a100tcsg324-1

# ============================================================================
# Clock Definitions
# ============================================================================

# System clock: 100 MHz (10ns period)
create_clock -period 10.000 -name sys_clk [get_ports sys_clk_p]
set_property CLOCK_DEDICATED_ROUTE BACKBONE [get_nets sys_clk]

# AXI clock: 100 MHz (same as system clock in this design)
create_generated_clock -name axi_clk \
    -source [get_pins pll/CLKIN1] \
    -multiply_by 1 \
    [get_pins pll/CLKOUT0]

# Video pixel clock: 74.25 MHz (13.468ns period) for 720p
create_generated_clock -name video_pix_clk \
    -source [get_pins pll/CLKIN1] \
    -multiply_by 37 \
    -divide_by 50 \
    [get_pins pll/CLKOUT1]

# Alternative: 148.5 MHz for 1080p
# create_generated_clock -name video_pix_clk_1080p \
#     -source [get_pins pll/CLKIN1] \
#     -multiply_by 37 \
#     -divide_by 25 \
#     [get_pins pll/CLKOUT1]

# Geometry processor clock: 200 MHz (5ns period) for high performance
create_generated_clock -name geom_clk \
    -source [get_pins pll/CLKIN1] \
    -multiply_by 2 \
    [get_pins pll/CLKOUT2]

# Rasterizer clock: 150 MHz (6.667ns period)
create_generated_clock -name rast_clk \
    -source [get_pins pll/CLKIN1] \
    -multiply_by 3 \
    -divide_by 2 \
    [get_pins pll/CLKOUT3]

# DDR3 controller clock (typically 333 MHz for DDR3-1333)
# This would be defined by MIG IP
# create_clock -period 3.000 -name ddr3_ui_clk [get_pins mig_7series_0/ui_clk]

# ============================================================================
# Clock Groups
# ============================================================================

# Asynchronous clock domains
set_clock_groups -asynchronous \
    -group [get_clocks sys_clk] \
    -group [get_clocks axi_clk] \
    -group [get_clocks video_pix_clk] \
    -group [get_clocks geom_clk] \
    -group [get_clocks rast_clk]

# If using DDR3
# set_clock_groups -asynchronous \
#     -group [get_clocks ddr3_ui_clk] \
#     -group [get_clocks sys_clk]

# ============================================================================
# Input Delays
# ============================================================================

# AXI interface: Assume 2ns board delay
set_input_delay -clock axi_clk -min 0.5 [get_ports axi_*]
set_input_delay -clock axi_clk -max 3.0 [get_ports axi_*]

# Reset is asynchronous
set_false_path -from [get_ports sys_rst]

# External memory interface (if not using MIG)
# set_input_delay -clock sys_clk -min 1.0 [get_ports mem_data*]
# set_input_delay -clock sys_clk -max 4.0 [get_ports mem_data*]

# ============================================================================
# Output Delays
# ============================================================================

# Video outputs: Account for serializer and cable delay
set_output_delay -clock video_pix_clk -min -0.5 [get_ports hdmi_tx*]
set_output_delay -clock video_pix_clk -max 2.0 [get_ports hdmi_tx*]

# AXI response signals
set_output_delay -clock axi_clk -min 0.5 [get_ports axi_*ready]
set_output_delay -clock axi_clk -max 3.0 [get_ports axi_*ready]
set_output_delay -clock axi_clk -min 0.5 [get_ports axi_rdata*]
set_output_delay -clock axi_clk -max 3.0 [get_ports axi_rdata*]

# ============================================================================
# Clock Domain Crossing Constraints
# ============================================================================

# Async FIFO: Geometry to rasterizer
set_max_delay -from [get_pins geometry_to_raster_fifo/wr_ptr_gray_reg*/C] \
              -to [get_pins geometry_to_raster_fifo/wr_ptr_gray_sync_reg*/D] \
              -datapath_only 6.667

set_max_delay -from [get_pins geometry_to_raster_fifo/rd_ptr_gray_reg*/C] \
              -to [get_pins geometry_to_raster_fifo/rd_ptr_gray_sync_reg*/D] \
              -datapath_only 5.000

# Rasterizer to video FIFO
set_max_delay -from [get_pins raster_to_video_fifo/wr_ptr_gray_reg*/C] \
              -to [get_pins raster_to_video_fifo/rd_ptr_gray_sync_reg*/D] \
              -datapath_only 13.468

# AXI to geometry processor CDC
set_max_delay -from [get_pins axi_ctrl/config_reg*/C] \
              -to [get_pins geom_proc/config_sync_reg*/D] \
              -datapath_only 10.000

# ============================================================================
# False Paths
# ============================================================================

# Reset synchronizers
set_false_path -to [get_pins */rst_sync_reg*/PRE]
set_false_path -to [get_pins */rst_sync_reg*/CLR]

# Debug LEDs (not performance critical)
set_false_path -to [get_ports led*]

# UART debug interface (low speed)
set_false_path -to [get_ports uart_tx]
set_false_path -from [get_ports uart_rx]

# Static configuration (set once at startup)
set_false_path -from [get_pins config/static_config_reg*/C]

# Test/debug signals
set_false_path -to [get_pins debug_ila/probe*]

# ============================================================================
# Multicycle Paths
# ============================================================================

# Matrix multiplication in geometry processor: 4-cycle operation
set_multicycle_path -setup 4 \
    -from [get_pins geom_proc/matrix_mult_stage1_reg*/C] \
    -to [get_pins geom_proc/matrix_mult_result_reg*/D]
set_multicycle_path -hold 3 \
    -from [get_pins geom_proc/matrix_mult_stage1_reg*/C] \
    -to [get_pins geom_proc/matrix_mult_result_reg*/D]

# Lighting calculation: 3-cycle path
set_multicycle_path -setup 3 \
    -from [get_pins geom_proc/lighting_stage1_reg*/C] \
    -to [get_pins geom_proc/lighting_result_reg*/D]
set_multicycle_path -hold 2 \
    -from [get_pins geom_proc/lighting_stage1_reg*/C] \
    -to [get_pins geom_proc/lighting_result_reg*/D]

# Rasterizer interpolation: 2-cycle path
set_multicycle_path -setup 2 \
    -from [get_pins rasterizer/interp_reg*/C] \
    -to [get_pins rasterizer/pixel_out_reg*/D]
set_multicycle_path -hold 1 \
    -from [get_pins rasterizer/interp_reg*/C] \
    -to [get_pins rasterizer/pixel_out_reg*/D]

# ============================================================================
# Maximum Delay Constraints
# ============================================================================

# Critical FIFO flags must update quickly
set_max_delay 4.0 -from [get_pins *_fifo/count_reg*/C] \
                   -to [get_pins *_fifo/full_flag_reg/D]
set_max_delay 4.0 -from [get_pins *_fifo/count_reg*/C] \
                   -to [get_pins *_fifo/empty_flag_reg/D]

# Pipeline control signals
set_max_delay 5.0 -from [get_pins */pipeline_stall_reg/C] \
                   -to [get_pins */pipeline_enable_reg*/CE]

# ============================================================================
# Physical Constraints
# ============================================================================

# Place PLL close to clock input
set_property LOC MMCME2_ADV_X0Y0 [get_cells pll/MMCME2_ADV_inst]

# Constrain async FIFO to single clock region
set_property USER_CLOCK_ROOT X0Y0 [get_nets geom_clk]
set_property USER_CLOCK_ROOT X0Y1 [get_nets rast_clk]

# Place critical logic in same region to minimize routing delay
create_pblock pblock_geometry_proc
add_cells_to_pblock pblock_geometry_proc [get_cells geom_proc]
resize_pblock pblock_geometry_proc -add {SLICE_X50Y50:SLICE_X99Y99}
resize_pblock pblock_geometry_proc -add {DSP48_X2Y20:DSP48_X3Y39}
resize_pblock pblock_geometry_proc -add {RAMB36_X2Y10:RAMB36_X3Y19}

# ============================================================================
# DSP Constraints
# ============================================================================

# Use DSP48E1 for multiplications
set_property USE_DSP48 yes [get_cells geom_proc/matrix_mult*]
set_property USE_DSP48 yes [get_cells geom_proc/vector_dot*]

# Pipeline DSP blocks for better performance
set_property REG_CONFIG "AREG1 BREG1 CREG1 MREG1 PREG1" [get_cells geom_proc/dsp*]

# ============================================================================
# Block RAM Constraints
# ============================================================================

# Use block RAM for FIFOs
set_property RAM_STYLE block [get_cells *_fifo/mem_reg]

# Enable output registers on BRAMs
set_property RAM_EXTENSION_A BLOCK [get_cells frame_buffer/mem_reg]
set_property RAM_EXTENSION_B BLOCK [get_cells z_buffer/mem_reg]

# ============================================================================
# Power Optimization
# ============================================================================

# Enable clock gating for power savings
set_property CLOCK_GATING yes [current_design]

# Use low-power RAM modes when possible
set_property POWER_SAVE block_ram [get_cells *_buffer/mem_reg]

# ============================================================================
# Timing Exceptions for Specific Scenarios
# ============================================================================

# When running at high speed, relax timing on non-critical paths
# (This is design-dependent, adjust as needed)

# Configuration register updates (infrequent)
set_false_path -from [get_pins axi_ctrl/matrix_config_reg*/C] \
               -to [get_pins geom_proc/model_matrix_reg*/D]

# ============================================================================
# Waveform Capture (ILA Debug Core)
# ============================================================================

# If using Integrated Logic Analyzer for debugging:
# set_property MARK_DEBUG true [get_nets geom_proc/pipeline_valid*]
# set_property MARK_DEBUG true [get_nets rasterizer/pixel_out*]

# ============================================================================
# Timing Summary Goals
# ============================================================================

# Target timing margins:
# - Setup: WNS (Worst Negative Slack) >= 0.5ns
# - Hold: WHS (Worst Hold Slack) >= 0.2ns
# - Pulse width: WPWS >= 0.5ns

# These can be checked with:
#   report_timing_summary -file timing_summary.rpt
#   report_timing -max_paths 100 -file timing_detail.rpt

# ============================================================================
# Optimization Directives
# ============================================================================

# Synthesis strategy
set_property STRATEGY Flow_PerfOptimized_high [get_runs synth_1]

# Implementation strategy
set_property STRATEGY Performance_ExplorePostRoutePhysOpt [get_runs impl_1]

# Enable physical optimization
set_property STEPS.PHYS_OPT_DESIGN.IS_ENABLED true [get_runs impl_1]
set_property STEPS.POST_ROUTE_PHYS_OPT_DESIGN.IS_ENABLED true [get_runs impl_1]

# ============================================================================
# Notes
# ============================================================================

# Xilinx 7-series Timing Characteristics:
# - Logic delay: ~0.2-0.5ns per LUT level
# - Routing delay: ~0.5-2ns depending on distance
# - FF setup time: ~0.05ns
# - FF clock-to-q: ~0.1ns
# - DSP48 multiply-add: ~3.5ns (single cycle) or pipelined
# - Block RAM access: ~1.8ns
#
# For 200 MHz geometry processor (5ns period):
# - Typical path budget: ~4.5ns (after clock uncertainty)
# - Allows ~10-15 logic levels or 1 DSP + logic
# - Requires careful pipelining for complex operations
#
# For 150 MHz rasterizer (6.667ns period):
# - More relaxed timing, easier to close
# - Can afford more logic levels between registers
#
# Timing closure tips:
# - Use DSP48 blocks for arithmetic (faster than LUTs)
# - Pipeline long combinatorial paths
# - Place related logic close together
# - Use BUFG for high-fanout clocks
# - Enable retiming in synthesis
# - Use physical optimization post-route
