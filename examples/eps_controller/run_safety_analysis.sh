#!/usr/bin/env bash
# ============================================================================
# EPS Controller Safety Analysis Runner
# ============================================================================
#
# This script runs the complete safety analysis workflow for the EPS controller.
#
# Usage:
#   ./run_safety_analysis.sh [options]
#
# Options:
#   --quick         Run quick analysis (limited faults)
#   --full          Run full analysis (all faults)
#   --gpu           Enable GPU acceleration
#   --asil LEVEL    Target ASIL level (A, B, C, D) [default: D]
#   --report FILE   Output report file [default: safety_report.md]
#   --help          Show this help message

set -e

# Default configuration
ASIL="D"
REPORT="safety_report.md"
GPU_FLAG=""
FAULT_LIMIT=""
WORKPRODUCTS="all"

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --quick)
            FAULT_LIMIT="--max-faults 1000"
            shift
            ;;
        --full)
            FAULT_LIMIT=""
            shift
            ;;
        --gpu)
            GPU_FLAG="--gpu"
            shift
            ;;
        --asil)
            ASIL="$2"
            shift 2
            ;;
        --report)
            REPORT="$2"
            shift 2
            ;;
        --help)
            cat << EOF
EPS Controller Safety Analysis Runner

Usage:
  ./run_safety_analysis.sh [options]

Options:
  --quick         Run quick analysis (1000 random faults)
  --full          Run full analysis (all faults, may take 10-20 min)
  --gpu           Enable GPU acceleration (requires Metal on macOS)
  --asil LEVEL    Target ASIL level: A, B, C, or D [default: D]
  --report FILE   Output report filename [default: safety_report.md]
  --help          Show this help message

Examples:
  # Quick test (for iteration)
  ./run_safety_analysis.sh --quick

  # Full analysis with GPU
  ./run_safety_analysis.sh --full --gpu

  # ASIL-B analysis
  ./run_safety_analysis.sh --asil B --report report_asil_b.md

EOF
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

# Print configuration
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "EPS Controller Safety Analysis"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Target ASIL:       ASIL-$ASIL"
echo "Report file:       $REPORT"
echo "GPU acceleration:  $([ -n "$GPU_FLAG" ] && echo "Enabled" || echo "Disabled")"
echo "Fault limit:       $([ -n "$FAULT_LIMIT" ] && echo "1000 (quick mode)" || echo "All (full mode)")"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Check if SKALP is in path
if ! command -v skalp &> /dev/null; then
    echo "Error: skalp command not found"
    echo "Please ensure SKALP is installed and in your PATH"
    exit 1
fi

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Build command
SKALP_CMD="skalp build \
    -s src/eps_controller.sk \
    --safety \
    --asil $ASIL \
    --safety-report $REPORT \
    --workproducts $WORKPRODUCTS \
    --gate-level \
    $GPU_FLAG \
    $FAULT_LIMIT"

echo "Running: $SKALP_CMD"
echo ""

# Run SKALP
eval $SKALP_CMD

# Check exit status
if [ $? -eq 0 ]; then
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "✅ Safety analysis complete!"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo ""
    echo "Generated files:"
    echo "  - $REPORT (main safety report)"
    echo "  - fmea_eps_controller.md (FMEA)"
    echo "  - traceability_matrix.csv (requirement traceability)"
    echo ""
    echo "Next steps:"
    echo "  1. Review $REPORT for compliance status"
    echo "  2. Check gap analysis for undetected faults"
    echo "  3. Iterate on design if targets not met"
    echo ""
else
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "❌ Safety analysis failed"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo ""
    echo "Please check the error messages above and:"
    echo "  - Ensure all .sk files are syntactically correct"
    echo "  - Verify all safety goal signals are bound"
    echo "  - Check that safety mechanisms are properly annotated"
    echo ""
    exit 1
fi
