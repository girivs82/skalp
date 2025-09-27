# Makefile for simulation tests

# Simulator (use iverilog by default, can switch to verilator)
SIM ?= iverilog
SIMRUN = vvp
WAVE_VIEWER = gtkwave

# Source files
SV_FILES = counter.sv
TB_FILES = counter_tb.sv

# Output files
SIM_OUT = counter_sim
VCD_FILE = counter.vcd

# Default target
all: simulate

# Compile and run with iverilog
simulate: $(SV_FILES) $(TB_FILES)
	$(SIM) -g2012 -o $(SIM_OUT) $(SV_FILES) $(TB_FILES)
	$(SIMRUN) $(SIM_OUT)

# View waveforms
wave: $(VCD_FILE)
	$(WAVE_VIEWER) $(VCD_FILE) &

# Verilator simulation (C++ based)
verilator: $(SV_FILES) $(TB_FILES)
	verilator --binary --trace -Wall $(SV_FILES) $(TB_FILES)
	./obj_dir/Vcounter_tb

# Clean generated files
clean:
	rm -f $(SIM_OUT) $(VCD_FILE)
	rm -rf obj_dir

.PHONY: all simulate wave verilator clean