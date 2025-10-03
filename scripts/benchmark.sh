#!/bin/bash
# SKALP Performance Benchmark Suite
# Tests compilation time, memory usage, and GPU simulation performance

set -e

echo "🚀 SKALP Performance Benchmark Suite"
echo "===================================="
echo

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Ensure we're in the right directory
cd "$(dirname "$0")/.."

# Build SKALP in release mode
echo -e "${BLUE}Building SKALP in release mode...${NC}"
cargo build --release --quiet
echo -e "${GREEN}✅ Build complete${NC}"
echo

# Test designs of various sizes
declare -a DESIGNS=(
    "examples/counter.sk:Small Counter:100"
    "examples/uart.sk:UART Module:1000"
    "examples/axi_lite.sk:AXI-Lite:2000"
    "examples/processor.sk:Simple Processor:5000"
)

RESULTS_FILE="benchmark_results_$(date +%Y%m%d_%H%M%S).txt"
echo "Benchmark Results - $(date)" > "$RESULTS_FILE"
echo "=================================" >> "$RESULTS_FILE"
echo

# Function to measure time and memory
measure_performance() {
    local cmd="$1"
    local name="$2"

    echo -e "${YELLOW}Testing: $name${NC}"
    echo "Command: $cmd"

    # Use time to measure performance
    if command -v gtime &> /dev/null; then
        TIME_CMD="gtime"
    else
        TIME_CMD="time"
    fi

    # Run the command and capture metrics
    TIME_OUTPUT=$( { $TIME_CMD -f "Time: %E, Memory: %M KB, CPU: %P" $cmd; } 2>&1 )
    EXIT_CODE=$?

    if [ $EXIT_CODE -eq 0 ]; then
        echo -e "${GREEN}✅ $name: PASSED${NC}"
        echo "$TIME_OUTPUT" | grep -E "(Time:|Memory:|CPU:)" || echo "Performance data not available"
    else
        echo -e "${RED}❌ $name: FAILED${NC}"
        echo "Exit code: $EXIT_CODE"
    fi

    echo "$name: $TIME_OUTPUT" >> "$RESULTS_FILE"
    echo
}

# Test 1: Compilation Performance
echo -e "${BLUE}📊 Testing Compilation Performance${NC}"
echo "=================================="

# Create test files of varying sizes
create_test_design() {
    local size=$1
    local filename=$2

    cat > "$filename" << EOF
// Auto-generated test design with $size entities
entity test_entity_0 {
    input clock clk;
    input reset rst;
    output bit[32] result;
}

impl test_entity_0 {
    signal bit[32] counter = 0;

    event clk {
        when rst {
            counter <= 0;
        } else {
            counter <= counter + 1;
        }
    }

    result = counter;
}
EOF

    # Add more entities for larger designs
    for ((i=1; i<$size; i++)); do
        cat >> "$filename" << EOF

entity test_entity_$i {
    input clock clk;
    input reset rst;
    input bit[32] data_in;
    output bit[32] data_out;
}

impl test_entity_$i {
    signal bit[32] reg_$i = 0;

    event clk {
        when rst {
            reg_$i <= 0;
        } else {
            reg_$i <= data_in + $i;
        }
    }

    data_out = reg_$i;
}
EOF
    done
}

# Test compilation with different design sizes
echo "Creating test designs..."
create_test_design 1 "/tmp/small_design.sk"
create_test_design 10 "/tmp/medium_design.sk"
create_test_design 50 "/tmp/large_design.sk"

measure_performance "./target/release/skalp build -s /tmp/small_design.sk -o /tmp/small_output" "Small Design (1 entity)"
measure_performance "./target/release/skalp build -s /tmp/medium_design.sk -o /tmp/medium_output" "Medium Design (10 entities)"
measure_performance "./target/release/skalp build -s /tmp/large_design.sk -o /tmp/large_output" "Large Design (50 entities)"

# Test 2: Standard Library Performance
echo -e "${BLUE}📊 Testing Standard Library Compilation${NC}"
echo "======================================="

if [ -f "crates/skalp-stdlib/components/counter.sk" ]; then
    measure_performance "./target/release/skalp build -s crates/skalp-stdlib/components/counter.sk" "Counter Component"
fi

if [ -f "crates/skalp-stdlib/components/fifo.sk" ]; then
    measure_performance "./target/release/skalp build -s crates/skalp-stdlib/components/fifo.sk" "FIFO Component"
fi

if [ -f "crates/skalp-stdlib/components/uart.sk" ]; then
    measure_performance "./target/release/skalp build -s crates/skalp-stdlib/components/uart.sk" "UART Component"
fi

# Test 3: Simulation Performance (if examples exist)
echo -e "${BLUE}📊 Testing Simulation Performance${NC}"
echo "================================="

# Test GPU simulation performance
if [ -f "/tmp/small_design.sk" ]; then
    echo "Testing GPU simulation performance..."
    START_TIME=$(date +%s.%N)
    timeout 30 ./target/release/skalp sim -s /tmp/small_design.sk || true
    END_TIME=$(date +%s.%N)
    DURATION=$(echo "$END_TIME - $START_TIME" | bc 2>/dev/null || echo "N/A")
    echo "GPU simulation duration: ${DURATION}s" | tee -a "$RESULTS_FILE"
    echo
fi

# Test 4: Memory Usage Patterns
echo -e "${BLUE}📊 Testing Memory Usage Patterns${NC}"
echo "================================="

if command -v valgrind &> /dev/null; then
    echo "Running memory analysis with Valgrind..."
    valgrind --tool=massif --massif-out-file=massif.out ./target/release/skalp build -s /tmp/small_design.sk > /dev/null 2>&1
    if [ -f "massif.out" ]; then
        MAX_MEMORY=$(grep "mem_heap_B" massif.out | sed 's/mem_heap_B=//g' | sort -n | tail -1)
        echo "Peak memory usage: $MAX_MEMORY bytes" | tee -a "$RESULTS_FILE"
        rm -f massif.out
    fi
else
    echo "Valgrind not available, skipping detailed memory analysis"
fi

# Test 5: LSP Server Performance
echo -e "${BLUE}📊 Testing LSP Server Performance${NC}"
echo "=================================="

echo "Starting LSP server performance test..."
LSP_START=$(date +%s.%N)
timeout 5 ./target/release/skalp-lsp < /dev/null || true
LSP_END=$(date +%s.%N)
LSP_DURATION=$(echo "$LSP_END - $LSP_START" | bc 2>/dev/null || echo "N/A")
echo "LSP server startup time: ${LSP_DURATION}s" | tee -a "$RESULTS_FILE"

# Cleanup
echo -e "${BLUE}Cleaning up test files...${NC}"
rm -f /tmp/small_design.sk /tmp/medium_design.sk /tmp/large_design.sk

# Summary
echo
echo -e "${GREEN}🎯 Benchmark Complete!${NC}"
echo "=================================="
echo "Results saved to: $RESULTS_FILE"
echo

# Display summary
echo -e "${BLUE}Performance Summary:${NC}"
echo "===================="
cat "$RESULTS_FILE" | grep -E "(Time:|Memory:|Duration:|usage:)" | head -10

echo
echo -e "${YELLOW}Performance Targets (Phase 11):${NC}"
echo "- Compilation: <1s for 10K lines, <10s for 100K lines"
echo "- Memory: <1GB for large designs"
echo "- GPU Simulation: >100MHz equivalent throughput"
echo "- LSP Response: <100ms for all operations"
echo

# Check if we meet targets
echo -e "${BLUE}Target Analysis:${NC}"
echo "=================="

# Simple analysis of results
if grep -q "Time: 0:0[0-5]" "$RESULTS_FILE"; then
    echo -e "${GREEN}✅ Compilation speed target met (<5s for test designs)${NC}"
else
    echo -e "${YELLOW}⚠️  Compilation speed may need optimization${NC}"
fi

if grep -q "LSP server startup time.*[0-4]\." "$RESULTS_FILE"; then
    echo -e "${GREEN}✅ LSP startup performance acceptable${NC}"
else
    echo -e "${YELLOW}⚠️  LSP startup time may need optimization${NC}"
fi

echo
echo -e "${GREEN}Benchmark suite completed successfully!${NC}"