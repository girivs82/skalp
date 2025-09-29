module Counter (
    input clk,
    input rst,
    output [31:0] count
);

    reg [31:0] counter;

    assign count = counter;

    always_ff @(posedge clk) begin
        if (rst) begin
            counter <= 0;
        end else begin
            counter <= (counter + 1);
        end
    end

endmodule
