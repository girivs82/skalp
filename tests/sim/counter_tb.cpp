// Verilator C++ testbench for counter module

#include <verilated.h>
#include <verilated_vcd_c.h>
#include "Vcounter.h"
#include <iostream>

int main(int argc, char** argv) {
    // Initialize Verilator
    Verilated::commandArgs(argc, argv);
    Verilated::traceEverOn(true);

    // Create instance of our module
    Vcounter* dut = new Vcounter;

    // Create VCD trace file
    VerilatedVcdC* tfp = new VerilatedVcdC;
    dut->trace(tfp, 99);
    tfp->open("counter.vcd");

    // Initialize signals
    dut->clk = 0;
    dut->reset = 1;
    dut->enable = 0;

    // Simulation time
    vluint64_t sim_time = 0;

    // Run simulation
    while (sim_time < 1000 && !Verilated::gotFinish()) {
        // Toggle clock
        dut->clk = !dut->clk;

        // Apply test stimulus
        if (sim_time == 20) {
            dut->reset = 0;  // Release reset
        }
        if (sim_time == 30) {
            dut->enable = 1;  // Start counting
        }
        if (sim_time == 200) {
            dut->enable = 0;  // Stop counting
        }
        if (sim_time == 250) {
            dut->enable = 1;  // Resume counting
        }
        if (sim_time == 350) {
            dut->reset = 1;  // Reset again
        }
        if (sim_time == 370) {
            dut->reset = 0;  // Release reset
        }

        // Evaluate model
        dut->eval();

        // Dump trace
        tfp->dump(sim_time);

        // Print status every 10 time units on rising edge
        if (dut->clk && sim_time % 10 == 0) {
            std::cout << "Time: " << sim_time
                      << " Reset: " << (int)dut->reset
                      << " Enable: " << (int)dut->enable
                      << " Count: " << (int)dut->count
                      << std::endl;
        }

        // Advance time
        sim_time++;
    }

    // Cleanup
    tfp->close();
    delete tfp;
    delete dut;

    std::cout << "Simulation completed!" << std::endl;
    return 0;
}