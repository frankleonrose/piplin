module piplin_module(
  input clock,
);
  input wire clock;
  //Input and output declarations

  //Registers
  reg [7:0] x8822 = 8'd0;

  // Primitives
  SB_IO #(
    .PIN_TYPE(6'b000000),
    .PULLUP(1'b0),
    .IO_STANDARD("SB_LVCMO"))
    SB_IO_8821 (
    .D_OUT_0(x8822));

  //Main code
  wire [7:0] G__8823 = x8822 + 8'd1;

  //Assignments to outputs

  always @(posedge clock) begin
    x8822 <= G__8823;
  end
endmodule
