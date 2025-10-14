# SKALP Graphics Pipeline - Build System
# ======================================

.PHONY: all clean help sim synth test lint docs

# Configuration
SKALP = skalp
BUILD_DIR = build
SIM_DIR = sim
SRC_DIR = src
VERIF_DIR = verif

# Main targets
all: build-top

help:
	@echo "SKALP Graphics Pipeline - Build Targets"
	@echo "========================================"
	@echo ""
	@echo "Building:"
	@echo "  make all              - Build complete design"
	@echo "  make build-top        - Build top-level only"
	@echo "  make build-geometry   - Build geometry module"
	@echo "  make build-fifo       - Build FIFO library"
	@echo ""
	@echo "Simulation:"
	@echo "  make sim              - Run all testbenches"
	@echo "  make test-fifo        - Test FIFO module"
	@echo "  make test-geometry    - Test geometry processor"
	@echo "  make test-top         - Test complete system"
	@echo ""
	@echo "Synthesis:"
	@echo "  make synth-ice40      - Synthesize for iCE40"
	@echo "  make synth-xilinx     - Synthesize for Xilinx"
	@echo "  make reports          - Generate synthesis reports"
	@echo ""
	@echo "Utilities:"
	@echo "  make clean            - Remove build artifacts"
	@echo "  make lint             - Run code quality checks"
	@echo "  make docs             - Build documentation"
	@echo "  make coverage         - Generate coverage report"

clean:
	@echo "Cleaning build artifacts..."
	@rm -rf $(BUILD_DIR) $(SIM_DIR)
	@echo "Done."

# Build targets
build-top:
	@echo "Building top-level design..."
	@mkdir -p $(BUILD_DIR)/systemverilog
	$(SKALP) build -s $(SRC_DIR)/main.sk -o $(BUILD_DIR)/systemverilog

build-geometry:
	@echo "Building geometry processor..."
	@mkdir -p $(BUILD_DIR)/systemverilog
	$(SKALP) build -s $(SRC_DIR)/geometry/processor.sk -o $(BUILD_DIR)/systemverilog

build-fifo:
	@echo "Building FIFO library..."
	@mkdir -p $(BUILD_DIR)/systemverilog
	$(SKALP) build -s lib/fifo/async_fifo.sk -o $(BUILD_DIR)/systemverilog

# Simulation targets
sim: test-fifo test-geometry test-top
	@echo "All simulations complete!"

test-fifo:
	@echo "Testing FIFO module..."
	@echo "  [Testbench not yet implemented]"

test-geometry:
	@echo "Testing geometry processor..."
	@echo "  [Testbench not yet implemented]"

test-top:
	@echo "Testing complete system..."
	@echo "  [Testbench not yet implemented]"

# Synthesis targets
synth-ice40:
	@echo "Synthesizing for iCE40..."
	@echo "  [Synthesis flow not yet implemented]"
	@echo "  Requires: yosys, nextpnr-ice40"

synth-xilinx:
	@echo "Synthesizing for Xilinx..."
	@echo "  [Synthesis flow not yet implemented]"
	@echo "  Requires: Vivado"

reports:
	@echo "Generating synthesis reports..."
	@mkdir -p $(BUILD_DIR)/reports
	@echo "  [Report generation not yet implemented]"

# Utility targets
lint:
	@echo "Running code quality checks..."
	@echo "  Checking SKALP syntax..."
	@find $(SRC_DIR) -name "*.sk" -exec $(SKALP) check {} \;

docs:
	@echo "Building documentation..."
	@echo "  Documentation files are in docs/"
	@echo "  View README.md for getting started"

coverage:
	@echo "Generating coverage report..."
	@echo "  [Coverage collection not yet implemented]"

# Example targets (for reference)
example-speed:
	@echo "Building speed-optimized configuration..."
	$(SKALP) build -s examples/optimized_for_speed.sk -o $(BUILD_DIR)/speed

example-area:
	@echo "Building area-optimized configuration..."
	$(SKALP) build -s examples/optimized_for_area.sk -o $(BUILD_DIR)/area
