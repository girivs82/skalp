#!/bin/bash
# Collect training data from design corpus for ML model training
#
# Usage: ./scripts/collect_training_data.sh [output_dir]
#
# This script runs the synthesis engine on a diverse set of designs
# and collects training data for the ML pass ordering model.

set -e

# Configuration
SKALP_BIN="${SKALP_BIN:-./target/release/skalp}"
STDLIB_PATH="${SKALP_STDLIB_PATH:-./crates/skalp-stdlib}"
OUTPUT_DIR="${1:-./training_data}"
TEMP_DIR="/tmp/skalp_training_$$"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Ensure build is up to date
echo -e "${YELLOW}Building SKALP...${NC}"
cargo build --release --quiet

# Create output directories
mkdir -p "$OUTPUT_DIR"
mkdir -p "$TEMP_DIR"

# Define design corpus - diverse set of designs
# Format: name:path
DESIGNS=(
    # Basic designs
    "counter:examples/counter.sk"
    "adder:examples/adder.sk"
    "alu:examples/alu.sk"
    "fifo:examples/fifo.sk"

    # Real-world designs
    "uart_tx:examples/real_world/02_uart_tx/uart_tx.sk"
    "spi_master:examples/real_world/03_spi_master/spi_master.sk"
    "i2c_master:examples/real_world/04_i2c_master/i2c_master.sk"
    "mem_arbiter:examples/real_world/05_memory_arbiter/mem_arbiter.sk"
    "regfile:examples/real_world/07_register_file/regfile.sk"
    "alu_rw:examples/real_world/08_alu/alu.sk"

    # Complex designs
    "hierarchical_alu:examples/hierarchical_alu.sk"
    "pipelined_processor:examples/pipelined_processor.sk"

    # Safety designs (smaller)
    "tmr_counter:examples/safety/tmr_counter.sk"
)

echo -e "${GREEN}Collecting training data from ${#DESIGNS[@]} designs${NC}"
echo "Output directory: $OUTPUT_DIR"
echo ""

# Track statistics
SUCCESSFUL=0
FAILED=0
TOTAL_EPISODES=0
TOTAL_DECISIONS=0

# Process each design
for design_entry in "${DESIGNS[@]}"; do
    IFS=':' read -r name path <<< "$design_entry"

    if [ ! -f "$path" ]; then
        echo -e "${RED}[SKIP] $name - file not found: $path${NC}"
        ((FAILED++))
        continue
    fi

    echo -e "${YELLOW}[PROCESSING] $name${NC}"

    # Create temp output for this design
    design_output="$TEMP_DIR/$name"
    design_training="$TEMP_DIR/${name}_training"
    mkdir -p "$design_output"
    mkdir -p "$design_training"

    # Run synthesis with training data collection
    if SKALP_STDLIB_PATH="$STDLIB_PATH" "$SKALP_BIN" build \
        -s "$path" \
        -o "$design_output" \
        --target gates \
        --ml-guided \
        --collect-training-data "$design_training" \
        2>/dev/null; then

        # Check if training data was generated
        if [ -f "$design_training/dataset.json" ]; then
            echo -e "${GREEN}  ✓ Training data collected${NC}"
            ((SUCCESSFUL++))

            # Extract stats
            episodes=$(jq -r '.stats.total_episodes // 0' "$design_training/stats.json" 2>/dev/null || echo "0")
            decisions=$(jq -r '.stats.total_pass_decisions // 0' "$design_training/stats.json" 2>/dev/null || echo "0")

            echo "    Episodes: $episodes, Decisions: $decisions"
            TOTAL_EPISODES=$((TOTAL_EPISODES + episodes))
            TOTAL_DECISIONS=$((TOTAL_DECISIONS + decisions))

            # Copy training data with design prefix
            cp "$design_training/dataset.json" "$OUTPUT_DIR/${name}_dataset.json"
        else
            echo -e "${RED}  ✗ No training data generated${NC}"
            ((FAILED++))
        fi
    else
        echo -e "${RED}  ✗ Synthesis failed${NC}"
        ((FAILED++))
    fi
done

echo ""
echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}Training Data Collection Complete${NC}"
echo -e "${GREEN}========================================${NC}"
echo ""
echo "Successful: $SUCCESSFUL / ${#DESIGNS[@]}"
echo "Failed: $FAILED"
echo "Total episodes: $TOTAL_EPISODES"
echo "Total pass decisions: $TOTAL_DECISIONS"
echo ""
echo "Training data saved to: $OUTPUT_DIR"

# Merge all datasets into a single file
echo ""
echo -e "${YELLOW}Merging datasets...${NC}"

# Create merged dataset using Python (if available) or jq
if command -v python3 &> /dev/null; then
    python3 << 'PYTHON_SCRIPT'
import json
import os
import sys
from datetime import datetime

output_dir = sys.argv[1] if len(sys.argv) > 1 else "./training_data"

merged = {
    "metadata": {
        "version": "1.0",
        "created_at": datetime.utcnow().isoformat() + "Z",
        "num_designs": 0,
        "libraries": ["7nm"],
        "config": {
            "collect_passes": True,
            "collect_cuts": True,
            "collect_cells": True,
            "max_cuts_per_node": 10,
            "record_action_probs": True,
            "compute_optimal_labels": False
        }
    },
    "episodes": [],
    "stats": {
        "total_episodes": 0,
        "total_pass_decisions": 0,
        "total_cut_decisions": 0,
        "total_cell_decisions": 0,
        "avg_improvement": 0.0,
        "best_improvement": 0.0,
        "pass_action_counts": {},
        "cell_type_counts": {}
    }
}

design_count = 0
for filename in os.listdir(output_dir):
    if filename.endswith("_dataset.json"):
        filepath = os.path.join(output_dir, filename)
        try:
            with open(filepath) as f:
                data = json.load(f)
                merged["episodes"].extend(data.get("episodes", []))
                design_count += 1

                # Merge stats
                stats = data.get("stats", {})
                merged["stats"]["total_episodes"] += stats.get("total_episodes", 0)
                merged["stats"]["total_pass_decisions"] += stats.get("total_pass_decisions", 0)
                merged["stats"]["total_cut_decisions"] += stats.get("total_cut_decisions", 0)
                merged["stats"]["total_cell_decisions"] += stats.get("total_cell_decisions", 0)

                # Merge pass action counts
                for action, count in stats.get("pass_action_counts", {}).items():
                    merged["stats"]["pass_action_counts"][action] = \
                        merged["stats"]["pass_action_counts"].get(action, 0) + count

        except Exception as e:
            print(f"Warning: Could not process {filename}: {e}", file=sys.stderr)

merged["metadata"]["num_designs"] = design_count

# Write merged dataset
output_path = os.path.join(output_dir, "merged_dataset.json")
with open(output_path, "w") as f:
    json.dump(merged, f, indent=2)

print(f"Merged {design_count} datasets into {output_path}")
print(f"Total episodes: {merged['stats']['total_episodes']}")
print(f"Total pass decisions: {merged['stats']['total_pass_decisions']}")
PYTHON_SCRIPT
else
    echo "Python3 not available, skipping merge step"
fi

# Cleanup temp directory
rm -rf "$TEMP_DIR"

echo ""
echo -e "${GREEN}Done!${NC}"
