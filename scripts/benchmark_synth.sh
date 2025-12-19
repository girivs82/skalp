#!/bin/bash
# SKALP Synthesis Benchmark Script
# Compares gate counts across different optimization presets

SKALP_BIN="${SKALP_BIN:-./target/release/skalp}"
export SKALP_STDLIB_PATH="${SKALP_STDLIB_PATH:-./crates/skalp-stdlib}"
OUTPUT_DIR="/tmp/skalp_benchmark"

# Create output directory
mkdir -p "$OUTPUT_DIR"

echo "============================================"
echo "      SKALP Synthesis Benchmark Suite      "
echo "============================================"
echo ""

# Check if skalp binary exists
if [ ! -f "$SKALP_BIN" ]; then
    echo "Error: SKALP binary not found at $SKALP_BIN"
    echo "Please run 'cargo build --release' first"
    exit 1
fi

# Header
printf "%-20s | %-8s | %-10s | %-8s | %-10s\n" "Design" "Quick" "Balanced" "Full" "Reduction"
printf "%s\n" "------------------------------------------------------------------------"

# Function to run synthesis and get cell count
run_synth() {
    local design_path=$1
    local design_name=$2
    local preset=$3
    local out_dir="$OUTPUT_DIR/${design_name}_${preset}"

    output=$($SKALP_BIN build -s "$design_path" -o "$out_dir" --target gates --optimize "$preset" 2>&1) || return 1

    # Extract cell count from output
    cells=$(echo "$output" | grep -o 'Cells: [0-9]*' | grep -o '[0-9]*' | tail -1)

    if [ -z "$cells" ]; then
        cells="0"
    fi

    echo "$cells"
}

# Benchmark each design
benchmark_design() {
    local design_path=$1
    local design_name=$2

    if [ ! -f "$design_path" ]; then
        return
    fi

    cells_quick=$(run_synth "$design_path" "$design_name" "quick" 2>/dev/null || echo "ERR")
    cells_balanced=$(run_synth "$design_path" "$design_name" "balanced" 2>/dev/null || echo "ERR")
    cells_full=$(run_synth "$design_path" "$design_name" "full" 2>/dev/null || echo "ERR")

    # Calculate reduction (quick vs full)
    if [ "$cells_quick" != "ERR" ] && [ "$cells_full" != "ERR" ] && [ "$cells_quick" -gt 0 ] 2>/dev/null; then
        reduction=$(awk "BEGIN {printf \"%.1f\", (1 - $cells_full / $cells_quick) * 100}")
        reduction_str="${reduction}%"
    else
        reduction_str="N/A"
    fi

    printf "%-20s | %-8s | %-10s | %-8s | %-10s\n" \
        "$design_name" "$cells_quick" "$cells_balanced" "$cells_full" "$reduction_str"
}

# Run benchmarks on each design
benchmark_design "examples/counter.sk" "Counter"
benchmark_design "examples/alu.sk" "ALU"
benchmark_design "examples/spi_master.sk" "SPIMaster"
benchmark_design "examples/fifo.sk" "FIFO"
benchmark_design "examples/cdc_synchronizer.sk" "CDCSync"
benchmark_design "examples/real_world/02_uart_tx/uart_tx.sk" "UartTx"
benchmark_design "examples/real_world/04_i2c_master/i2c_master.sk" "I2CMaster"
benchmark_design "examples/safety/tmr_counter.sk" "TMRCounter"
benchmark_design "examples/async_fifo.sk" "AsyncFIFO"

echo ""
echo "============================================"
echo "              Benchmark Complete            "
echo "============================================"
echo ""
echo "Output files saved to: $OUTPUT_DIR"
echo ""
echo "To compare with Yosys:"
echo "  brew install yosys  # or apt install yosys"
echo "  yosys -p 'read_verilog <file>.v; synth; stat'"
echo ""
