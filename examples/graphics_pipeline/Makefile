# SKALP Graphics Pipeline - Build System
# ======================================

.PHONY: all clean help sim synth test lint docs build verilator

# Configuration
SKALP = ../../target/release/skalp
BUILD_DIR = build
SIM_DIR = sim
SYNTH_DIR = synth
SRC_DIR = src
VERIF_DIR = verif
TOP_MODULE = main.sk

# Derived paths
TOP_SRC = $(SRC_DIR)/$(TOP_MODULE)
SV_OUTPUT = $(BUILD_DIR)/design.sv

# Main targets
all: build

help:
	@echo "SKALP Graphics Pipeline - Build Targets"
	@echo "========================================"
	@echo ""
	@echo "Building:"
	@echo "  make build            - Build complete design (HIR → MIR → SystemVerilog)"
	@echo "  make verilator        - Lint generated SystemVerilog with Verilator"
	@echo ""
	@echo "Simulation:"
	@echo "  make sim              - Run simulation with SKALP simulator (MIR)"
	@echo "  make sim-lir          - Run simulation on LIR format"
	@echo ""
	@echo "Synthesis:"
	@echo "  make synth            - Synthesize for iCE40 FPGA"
	@echo "  make synth-ice40      - Synthesize for iCE40 (detailed)"
	@echo "  make synth-full       - Full synthesis flow (place, route, bitstream)"
	@echo ""
	@echo "Utilities:"
	@echo "  make clean            - Remove build artifacts"
	@echo "  make lint             - Run all linting (SKALP + Verilator)"
	@echo "  make test             - Run Rust test suite for graphics pipeline"
	@echo "  make help             - Show this help message"

clean:
	@echo "🧹 Cleaning build artifacts..."
	@rm -rf $(BUILD_DIR) $(SIM_DIR) $(SYNTH_DIR)
	@echo "✅ Done."

# Build targets
build:
	@echo "🔨 Building graphics pipeline design..."
	@mkdir -p $(BUILD_DIR)
	@echo "   Source: $(TOP_SRC)"
	@echo "   Output: $(BUILD_DIR)/"
	$(SKALP) build -s $(TOP_SRC) -o $(BUILD_DIR)
	@echo "✅ Build complete!"
	@echo "   Generated: $(SV_OUTPUT)"

verilator: build
	@echo "🔍 Running Verilator lint on generated SystemVerilog..."
	@if [ ! -f $(SV_OUTPUT) ]; then \
		echo "❌ Error: $(SV_OUTPUT) not found. Run 'make build' first."; \
		exit 1; \
	fi
	@cd $(BUILD_DIR) && verilator --lint-only -Wall design.sv 2>&1 | tee verilator.log
	@echo ""
	@echo "📊 Verilator Summary:"
	@grep -c "^%Error" $(BUILD_DIR)/verilator.log > /dev/null && \
		echo "❌ Errors found (see $(BUILD_DIR)/verilator.log)" || \
		echo "✅ No critical errors"
	@grep -c "^%Warning" $(BUILD_DIR)/verilator.log > /dev/null && \
		(echo "⚠️  Warnings found:"; grep "^%Warning" $(BUILD_DIR)/verilator.log | head -10) || \
		echo "✅ No warnings"

# Simulation targets
sim:
	@echo "🎮 Running SKALP simulation..."
	@mkdir -p $(BUILD_DIR) $(SIM_DIR)
	@echo "   Step 1: Building design to MIR..."
	$(SKALP) build -s $(TOP_SRC) -o $(BUILD_DIR) -t mir
	@echo "   Step 2: Simulating MIR (1000 cycles)..."
	$(SKALP) sim $(BUILD_DIR)/design.mir --duration 1000 2>&1 | tee $(SIM_DIR)/simulation.log
	@echo "✅ Simulation complete!"
	@echo "   Log: $(SIM_DIR)/simulation.log"
	@if [ -f simulation.vcd ]; then \
		mv simulation.vcd $(SIM_DIR)/; \
		echo "   VCD: $(SIM_DIR)/simulation.vcd"; \
	fi

sim-lir:
	@echo "🎮 Running simulation on LIR..."
	@mkdir -p $(BUILD_DIR) $(SIM_DIR)
	@echo "   Step 1: Building design to LIR..."
	$(SKALP) build -s $(TOP_SRC) -o $(BUILD_DIR) -t lir
	@echo "   Step 2: Simulating LIR (1000 cycles)..."
	$(SKALP) sim $(BUILD_DIR)/design.lir --duration 1000 2>&1 | tee $(SIM_DIR)/simulation_lir.log
	@echo "✅ Simulation complete!"
	@echo "   Log: $(SIM_DIR)/simulation_lir.log"

# Synthesis targets
synth: synth-ice40

synth-ice40: build
	@echo "🏗️  Synthesizing for iCE40 FPGA..."
	@mkdir -p $(SYNTH_DIR)
	@echo "   Device: iCE40-HX8K"
	@echo "   Source: $(TOP_SRC)"
	$(SKALP) synth $(TOP_SRC) --device ice40-hx8k | tee $(SYNTH_DIR)/synthesis.log
	@echo "✅ Synthesis complete!"
	@echo "   Results: $(SYNTH_DIR)/"

synth-full: build
	@echo "🏗️  Running full synthesis flow (place, route, bitstream)..."
	@mkdir -p $(SYNTH_DIR)
	@echo "   Device: iCE40-HX8K"
	$(SKALP) synth $(TOP_SRC) --device ice40-hx8k --full-flow | tee $(SYNTH_DIR)/synthesis_full.log
	@echo "✅ Full synthesis flow complete!"
	@echo "   Bitstream: $(SYNTH_DIR)/"

# Utility targets
lint: verilator
	@echo "✅ Linting complete!"

test:
	@echo "🧪 Running graphics pipeline test suite..."
	cd ../.. && cargo test --test test_graphics_pipeline --all-features -- --nocapture

# Development targets
watch:
	@echo "👀 Watching for changes (requires entr)..."
	@find $(SRC_DIR) -name "*.sk" | entr -c make build

info:
	@echo "📋 Graphics Pipeline Project Info"
	@echo "=================================="
	@echo "Top module:    $(TOP_SRC)"
	@echo "Build dir:     $(BUILD_DIR)/"
	@echo "Sim dir:       $(SIM_DIR)/"
	@echo "Synth dir:     $(SYNTH_DIR)/"
	@echo "SKALP binary:  $(SKALP)"
	@echo ""
	@echo "Design stats:"
	@find $(SRC_DIR) -name "*.sk" -exec wc -l {} + | tail -1
	@echo ""
