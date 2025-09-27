#!/bin/bash
# Script to compile and run Verilator simulation

# Compile with Verilator
echo "Compiling with Verilator..."
verilator --cc --exe --trace --build \
    -Wno-lint \
    counter.sv \
    counter_tb.cpp

if [ $? -ne 0 ]; then
    echo "Verilator compilation failed!"
    exit 1
fi

# Run the simulation
echo "Running simulation..."
./obj_dir/Vcounter

# Check if VCD file was created
if [ -f "counter.vcd" ]; then
    echo "VCD trace file created: counter.vcd"
    echo "You can view it with: gtkwave counter.vcd"
else
    echo "Warning: VCD file not created"
fi

echo "Simulation complete!"