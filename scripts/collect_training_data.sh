#!/bin/bash
# Collect training data from design corpus for ML model training
#
# Usage: ./scripts/collect_training_data.sh [output_dir] [episodes_per_design]
#
# This script runs the synthesis engine on a diverse set of designs
# and collects training data for the ML pass ordering model.

set -e

# Configuration
SKALP_BIN="${SKALP_BIN:-./target/release/skalp}"
STDLIB_PATH="${SKALP_STDLIB_PATH:-./crates/skalp-stdlib}"
OUTPUT_DIR="${1:-./training_data}"
EPISODES_PER_DESIGN="${2:-15}"  # Run multiple episodes per design for more diversity
TEMP_DIR="/tmp/skalp_training_$$"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Ensure build is up to date
echo -e "${YELLOW}Building SKALP...${NC}"
cargo build --release --quiet

# Create output directories
mkdir -p "$OUTPUT_DIR"
mkdir -p "$TEMP_DIR"

# Define comprehensive design corpus
# Format: name:path
DESIGNS=(
    # Basic designs - foundational circuits
    "counter:examples/counter.sk"
    "adder:examples/adder.sk"
    "alu:examples/alu.sk"
    "fifo:examples/fifo.sk"
    "async_fifo:examples/async_fifo.sk"
    "cdc_sync:examples/cdc_synchronizer.sk"

    # Real-world designs - practical circuits
    "uart_tx:examples/real_world/02_uart_tx/uart_tx.sk"
    "spi_master:examples/real_world/03_spi_master/spi_master.sk"
    "i2c_master:examples/real_world/04_i2c_master/i2c_master.sk"
    "mem_arbiter:examples/real_world/05_memory_arbiter/mem_arbiter.sk"
    "regfile:examples/real_world/07_register_file/regfile.sk"
    "alu_rw:examples/real_world/08_alu/alu.sk"

    # Complex designs - multi-module circuits
    "hierarchical_alu:examples/hierarchical_alu.sk"
    "pipelined_processor:examples/pipelined_processor.sk"
    "advanced_types:examples/advanced_types.sk"

    # Safety designs - redundant circuits
    "tmr_counter:examples/safety/tmr_counter.sk"

    # Stdlib components - reusable modules
    "stdlib_adder:crates/skalp-stdlib/components/adder.sk"
    "stdlib_counter:crates/skalp-stdlib/components/counter.sk"
    "stdlib_multiplier:crates/skalp-stdlib/components/multiplier.sk"
    "stdlib_bitops:crates/skalp-stdlib/components/bitops_entity.sk"

    # Numeric/math examples
    "fp_sqrt:tests/fixtures/numeric/fp_sqrt_simple.sk"
    "fp_arithmetic:tests/fixtures/numeric/fp_arithmetic.sk"
    "fp_quadratic:tests/fixtures/numeric/fp_quadratic.sk"
    "fp_distance:tests/fixtures/numeric/fp_distance.sk"
    "fp_comparison:tests/fixtures/numeric/fp_comparison.sk"

    # Function test fixtures - varied logic patterns
    "fn_add:tests/fixtures/functions/simple_add.sk"
    "fn_and:tests/fixtures/functions/simple_and.sk"
    "fn_multiply:tests/fixtures/functions/simple_multiply.sk"
    "fn_shift:tests/fixtures/functions/simple_shift.sk"
    "fn_sub:tests/fixtures/functions/simple_sub.sk"
    "fn_let_single:tests/fixtures/functions/let_single.sk"
    "fn_let_multiple:tests/fixtures/functions/let_multiple.sk"
    "fn_let_chain:tests/fixtures/functions/let_chain.sk"
    "fn_match:tests/fixtures/functions/match_expr.sk"
    "fn_if:tests/fixtures/functions/if_expr.sk"
    "fn_if_nested:tests/fixtures/functions/if_nested.sk"

    # Intent-driven designs - optimization intent annotations
    "intent_latency:tests/fixtures/intent/intent_latency.sk"
    "intent_area:tests/fixtures/intent/intent_area.sk"
    "intent_throughput:tests/fixtures/intent/intent_throughput.sk"
    "intent_conditional:tests/fixtures/intent/intent_conditional.sk"

    # Struct/tuple tests - complex data types
    "struct_field:tests/fixtures/test_struct_field_seq.sk"
    "nested_field:tests/fixtures/test_nested_field.sk"
    "tuple_field:tests/fixtures/test_tuple_field_add.sk"
    "tuple_complete:tests/fixtures/test_tuple_complete.sk"
    "tuple_sim:tests/fixtures/test_tuple_sim.sk"

    # Control flow tests
    "for_loop:tests/fixtures/test_for_loop.sk"
    "match_expr:tests/fixtures/test_match_expr.sk"
    "match_guards:tests/fixtures/test_match_guards_unique.sk"
    "match_state_machine:tests/fixtures/test_match_guards_state_machine.sk"

    # Array/memory tests
    "array_write:tests/fixtures/test_array_write_simple.sk"
    "output_fifo:tests/fixtures/test_output_fifo_simple.sk"

    # Pipeline tests
    "pipeline_annot:tests/fixtures/test_pipeline_annotation.sk"
    "struct_outputs:tests/fixtures/test_struct_outputs.sk"

    # ML Training designs - varied complexity for training
    "barrel_shifter:examples/training/barrel_shifter.sk"
    "mac_unit:examples/training/mac_unit.sk"
    "priority_encoder:examples/training/priority_encoder.sk"
    "popcount:examples/training/popcount.sk"
    "crc16:examples/training/crc16.sk"
    "comparator_tree:examples/training/comparator_tree.sk"
    "gray_counter:examples/training/gray_counter.sk"
)

NUM_DESIGNS=${#DESIGNS[@]}
TOTAL_RUNS=$((NUM_DESIGNS * EPISODES_PER_DESIGN))

echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}Training Data Collection${NC}"
echo -e "${GREEN}========================================${NC}"
echo ""
echo "Designs: $NUM_DESIGNS"
echo "Episodes per design: $EPISODES_PER_DESIGN"
echo "Total runs: $TOTAL_RUNS"
echo "Output directory: $OUTPUT_DIR"
echo ""

# Track statistics
SUCCESSFUL=0
FAILED=0
TOTAL_EPISODES=0
TOTAL_DECISIONS=0
RUN_COUNT=0

# Process each design
for design_entry in "${DESIGNS[@]}"; do
    IFS=':' read -r name path <<< "$design_entry"

    if [ ! -f "$path" ]; then
        echo -e "${RED}[SKIP] $name - file not found: $path${NC}"
        ((FAILED += EPISODES_PER_DESIGN))
        continue
    fi

    # Run multiple episodes for this design
    for episode in $(seq 1 $EPISODES_PER_DESIGN); do
        ((RUN_COUNT++))
        progress_pct=$((RUN_COUNT * 100 / TOTAL_RUNS))

        echo -e "${BLUE}[$RUN_COUNT/$TOTAL_RUNS ${progress_pct}%]${NC} ${YELLOW}$name (episode $episode)${NC}"

        # Create temp output for this run
        run_name="${name}_ep${episode}"
        design_output="$TEMP_DIR/$run_name"
        design_training="$TEMP_DIR/${run_name}_training"
        mkdir -p "$design_output"
        mkdir -p "$design_training"

        # Run synthesis with training data collection
        # The ML advisor's exploration will make each run different
        if SKALP_STDLIB_PATH="$STDLIB_PATH" "$SKALP_BIN" build \
            -s "$path" \
            -o "$design_output" \
            --target gates \
            --ml-guided \
            --collect-training-data "$design_training" \
            2>/dev/null; then

            # Check if training data was generated
            if [ -f "$design_training/stats.json" ]; then
                ((SUCCESSFUL++))

                # Extract stats
                episodes=$(jq -r '.total_episodes // 0' "$design_training/stats.json" 2>/dev/null || echo "0")
                decisions=$(jq -r '.total_pass_decisions // 0' "$design_training/stats.json" 2>/dev/null || echo "0")

                echo -e "  ${GREEN}✓${NC} decisions: $decisions"
                TOTAL_EPISODES=$((TOTAL_EPISODES + episodes))
                TOTAL_DECISIONS=$((TOTAL_DECISIONS + decisions))

                # Copy training data with run prefix
                if [ -f "$design_training/dataset.json" ]; then
                    cp "$design_training/dataset.json" "$OUTPUT_DIR/${run_name}_dataset.json"
                fi
                if [ -f "$design_training/pass_decisions.csv" ]; then
                    cp "$design_training/pass_decisions.csv" "$OUTPUT_DIR/${run_name}_pass_decisions.csv"
                fi
            else
                echo -e "  ${RED}✗ No training data${NC}"
                ((FAILED++))
            fi
        else
            echo -e "  ${RED}✗ Synthesis failed${NC}"
            ((FAILED++))
        fi
    done
done

echo ""
echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}Training Data Collection Complete${NC}"
echo -e "${GREEN}========================================${NC}"
echo ""
echo "Successful runs: $SUCCESSFUL / $TOTAL_RUNS"
echo "Failed runs: $FAILED"
echo "Total episodes: $TOTAL_EPISODES"
echo -e "${YELLOW}Total pass decisions: $TOTAL_DECISIONS${NC}"
echo ""
echo "Training data saved to: $OUTPUT_DIR"

# Merge all datasets into a single file
echo ""
echo -e "${YELLOW}Merging datasets...${NC}"

# Create merged dataset using Python
if command -v python3 &> /dev/null; then
    python3 - "$OUTPUT_DIR" << 'PYTHON_SCRIPT'
import json
import os
import sys
from datetime import datetime
from collections import defaultdict

output_dir = sys.argv[1] if len(sys.argv) > 1 else "./training_data"

merged = {
    "metadata": {
        "version": "1.0",
        "created_at": datetime.utcnow().isoformat() + "Z",
        "num_designs": 0,
        "num_runs": 0,
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

design_names = set()
run_count = 0

for filename in sorted(os.listdir(output_dir)):
    if filename.endswith("_dataset.json"):
        filepath = os.path.join(output_dir, filename)
        try:
            with open(filepath) as f:
                data = json.load(f)
                merged["episodes"].extend(data.get("episodes", []))
                run_count += 1

                # Extract design name (remove _epN_dataset.json suffix)
                design_name = filename.rsplit("_ep", 1)[0]
                design_names.add(design_name)

                # Merge stats
                stats = data.get("stats", {})
                merged["stats"]["total_episodes"] += stats.get("total_episodes", 0)
                merged["stats"]["total_pass_decisions"] += stats.get("total_pass_decisions", 0)
                merged["stats"]["total_cut_decisions"] += stats.get("total_cut_decisions", 0)
                merged["stats"]["total_cell_decisions"] += stats.get("total_cell_decisions", 0)

                # Track best improvement
                best = stats.get("best_improvement", 0.0)
                if best > merged["stats"]["best_improvement"]:
                    merged["stats"]["best_improvement"] = best

                # Merge pass action counts
                for action, count in stats.get("pass_action_counts", {}).items():
                    merged["stats"]["pass_action_counts"][action] = \
                        merged["stats"]["pass_action_counts"].get(action, 0) + count

                # Merge cell type counts
                for cell, count in stats.get("cell_type_counts", {}).items():
                    merged["stats"]["cell_type_counts"][cell] = \
                        merged["stats"]["cell_type_counts"].get(cell, 0) + count

        except Exception as e:
            print(f"Warning: Could not process {filename}: {e}", file=sys.stderr)

merged["metadata"]["num_designs"] = len(design_names)
merged["metadata"]["num_runs"] = run_count

# Compute average improvement
if merged["stats"]["total_episodes"] > 0:
    # We'd need to track this during collection, skip for now
    pass

# Write merged dataset
output_path = os.path.join(output_dir, "merged_dataset.json")
with open(output_path, "w") as f:
    json.dump(merged, f, indent=2)

# Also merge CSV files
csv_header = None
csv_rows = []
for filename in sorted(os.listdir(output_dir)):
    if filename.endswith("_pass_decisions.csv"):
        filepath = os.path.join(output_dir, filename)
        try:
            with open(filepath) as f:
                lines = f.readlines()
                if lines:
                    if csv_header is None:
                        csv_header = lines[0]
                    csv_rows.extend(lines[1:])  # Skip header after first file
        except Exception as e:
            print(f"Warning: Could not process {filename}: {e}", file=sys.stderr)

if csv_header and csv_rows:
    csv_path = os.path.join(output_dir, "merged_pass_decisions.csv")
    with open(csv_path, "w") as f:
        f.write(csv_header)
        f.writelines(csv_rows)
    print(f"Merged CSV: {len(csv_rows)} rows")

print(f"")
print(f"Merged {run_count} runs from {len(design_names)} unique designs")
print(f"Total episodes: {merged['stats']['total_episodes']}")
print(f"Total pass decisions: {merged['stats']['total_pass_decisions']}")
print(f"")
print(f"Pass action distribution:")
total_actions = sum(merged['stats']['pass_action_counts'].values())
for action, count in sorted(merged['stats']['pass_action_counts'].items(), key=lambda x: -x[1]):
    pct = count * 100.0 / total_actions if total_actions > 0 else 0
    print(f"  {action}: {count} ({pct:.1f}%)")

PYTHON_SCRIPT
else
    echo "Python3 not available, skipping merge step"
fi

# Cleanup temp directory
rm -rf "$TEMP_DIR"

echo ""
echo -e "${GREEN}Done!${NC}"
