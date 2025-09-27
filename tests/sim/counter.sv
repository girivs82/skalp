// Counter module in SystemVerilog (hand-written for comparison)

module counter (
    input  wire       clk,
    input  wire       reset,
    input  wire       enable,
    output logic [7:0] count
);

    logic [7:0] count_reg;

    always_ff @(posedge clk) begin
        if (reset) begin
            count_reg <= 8'b0;
        end else if (enable) begin
            count_reg <= count_reg + 1;
        end
    end

    assign count = count_reg;

endmodule
