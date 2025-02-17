/**
 * BASIC TESTBENCH FOR IKAOPM
 */
`timescale 1ns/1ns
module IKAOPM_tb;

//BUS IO wires
reg             EMUCLK = 1'b1;
reg             IC_n = 1'b1;
reg             CS_n = 1'b1;
reg             WR_n = 1'b1;
reg             A0 = 1'b0;
reg     [7:0]   DIN = 8'h0;

reg             block_cen = 1'b0;

wire            phi1, sh1, sh2, sd;


//generate clock
always #35 EMUCLK = ~EMUCLK;        // EMUCLK period = 70ns =  14.285714 Mhz

reg     [2:0]   clkdiv = 3'b000;
reg             phiMref = 1'b0;
wire            phiM_PCEN_n = ~(clkdiv[1:0] == 2'b11);

// generate phiMref as EMUCLK div 8
always @(posedge EMUCLK) begin
    if(clkdiv == 3'b111) begin clkdiv <= 3'b000; phiMref <= 1'b1; end
    else clkdiv <= clkdiv + 3'b001;

    if(clkdiv[1:0] == 2'b01) phiMref <= 1'b0;
end


//async reset
initial begin
    #(30*35) IC_n <= 1'b0;
    #(1300*35) IC_n <= 1'b1;
end

wire            emu_r_sample, emu_l_sample;
wire signed     [15:0]  emu_r_ex, emu_l_ex;
wire signed     [15:0]  emu_r, emu_l;


//main chip
IKAOPM #(
    .FULLY_SYNCHRONOUS          (1                          ),
    .FAST_RESET                 (1                          ),
    .USE_BRAM                   (1                          )
) main (
    // phyM expected 3.58MHz.
    // Due to phiM_PCEN_n division by 4 below, the EMUCLK shall be 4 * 3.58Mhz = 14.32Mhz, i.e. Tperiod=69.832ns=70ns
    .i_EMUCLK                   (EMUCLK                     ),

    .i_phiM_PCEN_n              (~(clkdiv == 3'd0 || clkdiv == 3'd4) | block_cen),  // phiM n_enable as EMUCLK div 4 (they internaly divide by 2)
    // .i_phi1_PCEN_n              (~(clkdiv == 3'd0) | block_cen),
    // .i_phi1_NCEN_n              (~(clkdiv == 3'd4) | block_cen),


    .i_IC_n                     (IC_n                       ),

    .o_phi1                     (phi1                       ),

    .i_CS_n                     (CS_n                       ),
    .i_RD_n                     (1'b1                       ),
    .i_WR_n                     (WR_n                       ),
    .i_A0                       (A0                         ),

    .i_D                        (DIN                        ),
    .o_D                        (                           ),
    .o_D_OE                     (                           ),

    .o_CT1                      (                           ),
    .o_CT2                      (                           ),

    .o_IRQ_n                    (                           ),

    .o_SH1                      (sh1                        ),
    .o_SH2                      (sh2                        ),

    .o_SO                       (sd                         ),

    .o_EMU_R_SAMPLE             (emu_r_sample               ),
    .o_EMU_L_SAMPLE             (emu_l_sample               ),
    .o_EMU_R_EX                 (emu_r_ex                   ),
    .o_EMU_L_EX                 (emu_l_ex                   ),
    .o_EMU_R                    (emu_r                      ),
    .o_EMU_L                    (emu_l                      )
);

YM3012 u_dac (
    .i_phi1                     (phi1                       ),
    .i_SH1                      (sh1                        ),
    .i_SH2                      (sh2                        ),
    .i_DI                       (sd                         ),
    .o_R                        (                           ),
    .o_L                        (                           )
);


task automatic IKAOPM_write (
    input       [7:0]   i_TARGET_ADDR,
    input       [7:0]   i_WRITE_DATA
); begin
    #0   CS_n = 1'b0; WR_n = 1'b1; A0 = 1'b0; DIN = i_TARGET_ADDR;
    #(30*35)  CS_n = 1'b0; WR_n = 1'b0; A0 = 1'b0; DIN = i_TARGET_ADDR;
    #(40*35)  CS_n = 1'b1; WR_n = 1'b1; A0 = 1'b0; DIN = i_TARGET_ADDR;
    #(30*35)  CS_n = 1'b0; WR_n = 1'b1; A0 = 1'b1; DIN = i_WRITE_DATA;
    #(30*35)  CS_n = 1'b0; WR_n = 1'b0; A0 = 1'b1; DIN = i_WRITE_DATA;
    #(40*35)  CS_n = 1'b1; WR_n = 1'b1; A0 = 1'b1; DIN = i_WRITE_DATA;
end endtask

initial begin
    #(2100*35);

    //KC
    #(600*35) IKAOPM_write(8'h28, {4'h4, 4'h2}); //ch1

    //MUL
    #(600*35) IKAOPM_write(8'h40, {1'b0, 3'd0, 4'd2}); 
    #(600*35) IKAOPM_write(8'h50, {1'b0, 3'd0, 4'd1});

    //TL
    #(600*35) IKAOPM_write(8'h60, {8'd21});
    #(600*35) IKAOPM_write(8'h70, {8'd1});
    #(600*35) IKAOPM_write(8'h68, {8'd127});
    #(600*35) IKAOPM_write(8'h78, {8'd127});

    //AR
    #(600*35) IKAOPM_write(8'h80, {2'd0, 1'b0, 5'd31}); 
    #(600*35) IKAOPM_write(8'h90, {2'd0, 1'b0, 5'd30});

    //AMEN/D1R(DR)
    #(600*35) IKAOPM_write(8'hA0, {1'b0, 2'b00, 5'd5});
    #(600*35) IKAOPM_write(8'hB0, {1'b0, 2'b00, 5'd18});

    //D2R(SR)
    #(600*35) IKAOPM_write(8'hC0, {2'd0, 1'b0, 5'd0});
    #(600*35) IKAOPM_write(8'hD0, {2'd0, 1'b0, 5'd7});

    //D1L(SL)RR
    #(600*35) IKAOPM_write(8'hE0, {4'd0, 4'd0});
    #(600*35) IKAOPM_write(8'hF0, {4'd1, 4'd4});

    //RL/FL/ALG
    #(600*35) IKAOPM_write(8'h20, {2'b11, 3'd7, 3'd4});

    //KON
    #(600*35) IKAOPM_write(8'h08, {1'b0, 4'b0011, 3'd0}); //write 0x7F, 0x08(KON)=
end


    // ------------------------------------------------------

    initial
    begin
        // time in 1ns-units
        // # 100_000_000;      // 100ms
        # 10_000_000;      // 10ms
        // # 10000000_00;
        $finish;
    end

    // Do this in your test bench to generate VCD waves
    initial
    begin
        $dumpfile("IKAOPM_tb.vcd");
        $dumpvars(0,IKAOPM_tb);
    end


endmodule


// ////////////////////////////////////////////////////////////////////////////////


module YM3012 (
    input   wire                i_phi1,
    input   wire                i_SH1, //right
    input   wire                i_SH2, //left
    input   wire                i_DI,
    output  wire signed [15:0]  o_R,
    output  wire signed [15:0]  o_L
);

reg             sh1_z, sh1_zz, sh2_z, sh2_zz;
always @(posedge i_phi1) begin
    sh1_z <= i_SH1;
    sh2_z <= i_SH2;
end
always @(posedge i_phi1) begin
    sh1_zz <= sh1_z;
    sh2_zz <= sh2_z;
end

wire            right_ld = ~(sh1_z | ~sh1_zz);
wire            left_ld = ~(sh2_z | ~sh2_zz);

reg     [13:0]  right_sr, left_sr;
always @(posedge i_phi1) begin
    right_sr[13] <= i_DI;
    right_sr[12:0] <= right_sr[13:1];

    left_sr[13] <= i_DI;
    left_sr[12:0] <= left_sr[13:1];
end

reg     [12:0]  right_latch, left_latch;
// always @(*) begin
//     if(right_ld) right_latch = right_sr[12:0];
//     if(left_ld) left_latch = left_sr[12:0];
// end

always @(posedge i_phi1) begin
    if (right_ld) right_latch <= right_sr[12:0];
    if (left_ld) left_latch <= left_sr[12:0];
end

reg signed  [15:0]  right_output, left_output;
always @(*) begin
    case(right_latch[12:10])
        3'd0: right_output = 16'dx;
        3'd1: right_output = right_latch[9] ? {1'b0, 6'b000000, right_latch[8:0]     } : ~{1'b0, 6'b000000, ~right_latch[8:0]     } + 16'd1;
        3'd2: right_output = right_latch[9] ? {1'b0, 5'b00000, right_latch[8:0], 1'b0} : ~{1'b0, 5'b00000, ~right_latch[8:0], 1'b0} + 16'd1;
        3'd3: right_output = right_latch[9] ? {1'b0, 4'b0000, right_latch[8:0], 2'b00} : ~{1'b0, 4'b0000, ~right_latch[8:0], 2'b00} + 16'd1;
        3'd4: right_output = right_latch[9] ? {1'b0, 3'b000, right_latch[8:0], 3'b000} : ~{1'b0, 3'b000, ~right_latch[8:0], 3'b000} + 16'd1;
        3'd5: right_output = right_latch[9] ? {1'b0, 2'b00, right_latch[8:0], 4'b0000} : ~{1'b0, 2'b00, ~right_latch[8:0], 4'b0000} + 16'd1;
        3'd6: right_output = right_latch[9] ? {1'b0, 1'b0, right_latch[8:0], 5'b00000} : ~{1'b0, 1'b0, ~right_latch[8:0], 5'b00000} + 16'd1;
        3'd7: right_output = right_latch[9] ? {1'b0,      right_latch[8:0], 6'b000000} : ~{1'b0,      ~right_latch[8:0], 6'b000000} + 16'd1;
    endcase
end


endmodule