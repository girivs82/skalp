// Testbench for counter module

module counter_tb;
    // Testbench signals
    reg clk;
    reg reset;
    reg enable;
    wire [7:0] count;

    // Instantiate the counter
    counter dut (
        .clk(clk),
        .reset(reset),
        .enable(enable),
        .count(count)
    );

    // Clock generation
    initial begin
        clk = 0;
        forever #5 clk = ~clk; // 10 time unit period
    end

    // Test stimulus
    initial begin
        $dumpfile("counter.vcd");
        $dumpvars(0, counter_tb);

        // Initialize signals
        reset = 1;
        enable = 0;

        // Release reset
        #20 reset = 0;

        // Enable counting
        #10 enable = 1;

        // Let it count for a while
        #200;

        // Disable counting
        enable = 0;
        #50;

        // Re-enable
        enable = 1;
        #100;

        // Reset again
        reset = 1;
        #20;
        reset = 0;
        #50;

        $display("Test completed!");
        $finish;
    end

    // Monitor output
    initial begin
        $monitor("Time=%0d: reset=%b enable=%b count=%d",
                 $time, reset, enable, count);
    end

endmodule