//============================================================================
//  Oric Telestrat
//  rampa@encomix.org
//
//============================================================================
//`default_nettype none

module guest_top
  #
  (
   parameter  TOT_DISKS = 1
   )
(
	input         CLOCK_27,
`ifdef USE_CLOCK_50
	input         CLOCK_50,
`endif

	output        LED,
	output [VGA_BITS-1:0] VGA_R,
	output [VGA_BITS-1:0] VGA_G,
	output [VGA_BITS-1:0] VGA_B,
	output        VGA_HS,
	output        VGA_VS,

`ifdef USE_HDMI
	output        HDMI_RST,
	output  [7:0] HDMI_R,
	output  [7:0] HDMI_G,
	output  [7:0] HDMI_B,
	output        HDMI_HS,
	output        HDMI_VS,
	output        HDMI_PCLK,
	output        HDMI_DE,
	inout         HDMI_SDA,
	inout         HDMI_SCL,
	input         HDMI_INT,
`endif

	input         SPI_SCK,
	inout         SPI_DO,
	input         SPI_DI,
	input         SPI_SS2,    // data_io
	input         SPI_SS3,    // OSD
	input         CONF_DATA0, // SPI_SS for user_io

`ifdef USE_QSPI
	input         QSCK,
	input         QCSn,
	inout   [3:0] QDAT,
`endif
`ifndef NO_DIRECT_UPLOAD
	input         SPI_SS4,
`endif

	output [12:0] SDRAM_A,
	inout  [15:0] SDRAM_DQ,
	output        SDRAM_DQML,
	output        SDRAM_DQMH,
	output        SDRAM_nWE,
	output        SDRAM_nCAS,
	output        SDRAM_nRAS,
	output        SDRAM_nCS,
	output  [1:0] SDRAM_BA,
	output        SDRAM_CLK,
	output        SDRAM_CKE,

`ifdef DUAL_SDRAM
	output [12:0] SDRAM2_A,
	inout  [15:0] SDRAM2_DQ,
	output        SDRAM2_DQML,
	output        SDRAM2_DQMH,
	output        SDRAM2_nWE,
	output        SDRAM2_nCAS,
	output        SDRAM2_nRAS,
	output        SDRAM2_nCS,
	output  [1:0] SDRAM2_BA,
	output        SDRAM2_CLK,
	output        SDRAM2_CKE,
`endif

`ifdef USE_EXTBUS
  inout  [7:0]  CH_D ,
  output        CH_RD,
  output        CH_WR,
  output        CH_A0,
  output        CH_CS,
`endif



	output        AUDIO_L,
	output        AUDIO_R,
`ifdef I2S_AUDIO
	output        I2S_BCK,
	output        I2S_LRCK,
	output        I2S_DATA,
`endif
`ifdef I2S_AUDIO_HDMI
	output        HDMI_MCLK,
	output        HDMI_BCK,
	output        HDMI_LRCK,
	output        HDMI_SDATA,
`endif
`ifdef SPDIF_AUDIO
	output        SPDIF,
`endif
`ifdef USE_AUDIO_IN
	input         AUDIO_IN,
`endif
	input         UART_RX,
	output        UART_TX

);

`ifdef NO_DIRECT_UPLOAD
localparam bit DIRECT_UPLOAD = 0;
wire SPI_SS4 = 1;
`else
localparam bit DIRECT_UPLOAD = 1;
`endif

`ifdef USE_QSPI
localparam bit QSPI = 1;
assign QDAT = 4'hZ;
`else
localparam bit QSPI = 0;
`endif

`ifdef VGA_8BIT
localparam VGA_BITS = 8;
`else
localparam VGA_BITS = 6;
`endif

`ifdef USE_HDMI
localparam bit HDMI = 1;
assign HDMI_RST = 1'b1;
`else
localparam bit HDMI = 0;
`endif

`ifdef BIG_OSD
localparam bit BIG_OSD = 1;
`define SEP "-;",
`else
localparam bit BIG_OSD = 0;
`define SEP
`endif

`ifdef USE_AUDIO_IN
localparam bit USE_AUDIO_IN = 1;
wire TAPE_SOUND=AUDIO_IN;
`else
localparam bit USE_AUDIO_IN = 0;
wire TAPE_SOUND=UART_RX;
`endif


`include "build_id.v"
localparam CONF_STR = {
	"X65;;",
	"F0,ROM,Load ROM;",
	"O23,Mode,6502,65c02,65C816;",
//	"O1,RAM,bram,sdram;",
	"-;",
	"O45,Scandoubler Fx,None,CRT 25%,CRT 50%,CRT 75%;",
	"O89,Stereo,Off,ABC (West Europe),ACB (East Europe);",
	"-;",
	"T0,Reset & Apply;",
	"V,v",`BUILD_DATE
};


/////////////////////// CLOCKS ////////////////////////////

wire        clk_video; // 25 Mhz
wire        clk_sys;   // 50 Mhz
wire        clk_ram;   // 100 Mhz
wire        locked;

wire FPGACLK; //temporal

pll pll
(
	.inclk0(CLOCK_50),
	.c0(clk_sys),
	.c1(clk_ram),
	.c2(clk_video),	
	.locked(locked)
);


reg        reset = 0;

assign reset= status[0] || buttons[1]|| ~locked || ioctl_download ;

//////////////////// HPS ///////////////////////////////


wire [15:0] joy0;
wire [15:0] joy1;
wire  [4:0] joy_t0={joy0[3],joy0[2],joy0[4],joy0[1],joy0[0]}; //Up,Down,Fire,Left,Right
wire  [4:0] joy_t1={joy1[3],joy1[2],joy1[4],joy1[1],joy1[0]};
wire        fire2_t1=joy1[5]; 
wire        fire3_t1=joy1[6]; 
wire        forced_scandoubler;
wire [127:0] status;

wire [31:0] sd_lba[TOT_DISKS];
wire  [TOT_DISKS-1:0] sd_rd;
wire  [TOT_DISKS-1:0] sd_wr;
wire  [TOT_DISKS-1:0] sd_ack;
wire  [8:0] sd_buff_addr;
wire  [7:0] sd_buff_dout;
wire  [7:0] sd_buff_din[TOT_DISKS];
wire        sd_buff_wr;
wire        sd_ack_conf;
wire        sd_conf;
wire        sd_sdhc = 1'b1;

wire  [TOT_DISKS-1:0] img_mounted;
wire [31:0] img_size;
wire        img_readonly;

wire        ioctl_wr;
wire [24:0] ioctl_addr;
wire  [7:0] ioctl_dout;
wire        ioctl_download;
wire  [7:0] ioctl_index;

reg         status_set;
reg  [31:0] status_out;

wire [21:0] gamma_bus;
wire        no_csync;
wire [63:0] rtc;



`ifdef USE_HDMI
wire        i2c_start;
wire        i2c_read;
wire  [6:0] i2c_addr;
wire  [7:0] i2c_subaddr;
wire  [7:0] i2c_dout;
wire  [7:0] i2c_din;
wire        i2c_ack;
wire        i2c_end;
`endif

wire  [1:0] buttons, switches;
wire			ypbpr;
wire        scandoubler_disable;
wire        key_pressed;
wire [7:0]  key_code;
wire        key_strobe;
wire        key_extended;


user_io #(.STRLEN($size(CONF_STR)>>3), .SD_IMAGES(TOT_DISKS), .FEATURES(32'h0 | (BIG_OSD << 13) | (HDMI << 14))) user_io
(	
	.clk_sys        	(clk_sys         	),
	.clk_sd           (clk_sys           ),
	.conf_str       	(CONF_STR       	),
	.SPI_CLK        	(SPI_SCK        	),
	.SPI_SS_IO      	(CONF_DATA0     	),
	.SPI_MISO       	(SPI_DO        	),
	.SPI_MOSI       	(SPI_DI         	),

`ifdef USE_HDMI
	.i2c_start      (i2c_start      ),
	.i2c_read       (i2c_read       ),
	.i2c_addr       (i2c_addr       ),
	.i2c_subaddr    (i2c_subaddr    ),
	.i2c_dout       (i2c_dout       ),
	.i2c_din        (i2c_din        ),
	.i2c_ack        (i2c_ack        ),
	.i2c_end        (i2c_end        ),
`endif

	.buttons        	(buttons        	),
	.switches       	(switches      	),
	.scandoubler_disable (scandoubler_disable	),
	.ypbpr          	(ypbpr          	),

	.joystick_0       ( joy0      ),
	.joystick_1       ( joy1      ),
	.status         	(status     ),
	
	.ps2_kbd_clk      (PS2K_CLK   ),
	.ps2_kbd_data     (PS2K_DATA  ),
	.ps2_mouse_clk    (PS2M_CLK   ),
	.ps2_mouse_data   (PS2M_DATA  ),

	.rtc              (rtc),
	.no_csync         (no_csync         ),
	// SD CARD
   .sd_lba                      (sd_lba[0]     ),
	.sd_rd                       (sd_rd         ),
	.sd_wr                       (sd_wr         ),
	.sd_ack                      (sd_ack        ),
	.sd_ack_conf                 (sd_ack_conf   ),
	.sd_conf                     (sd_conf       ),
	.sd_sdhc                     (sd_sdhc       ),
	.sd_dout                     (sd_buff_dout  ),
	.sd_dout_strobe              (sd_buff_wr    ),
	.sd_din                      (sd_buff_din[0]),
//	.sd_din_strobe               (sd_din_strobe ),
	.sd_buff_addr                (sd_buff_addr  ),
	.img_mounted                 (img_mounted   ),
	.img_size                    (img_size      )
);

data_io data_io(
	.clk_sys       ( clk_sys      ),
	.SPI_SCK       ( SPI_SCK       ),
	.SPI_SS2       ( SPI_SS2       ),
	.SPI_DI        ( SPI_DI        ),
	.clkref_n      ( 1'b0          ),
	.ioctl_download( ioctl_download),
	.ioctl_index   ( ioctl_index   ),
	.ioctl_wr      ( ioctl_wr      ),
	.ioctl_addr    ( ioctl_addr    ),
	.ioctl_dout    ( ioctl_dout    )
);


//////////////////////// ROM CARTRIDGE ////////////

wire[19:0] rom_address;
wire[7:0]  rom_data;
wire[7:0]  rom_q;
reg        rom_rd;
reg        rom_wr;
wire       isRAM = rom_address[19];


altddio_out
#(
        .extend_oe_disable("OFF"),
        .intended_device_family("Cyclone 10 LP"),
        .invert_output("OFF"),
        .lpm_hint("UNUSED"),
        .lpm_type("altddio_out"),
        .oe_reg("UNREGISTERED"),
        .power_up_high("OFF"),
        .width(1)
)


sdramclk_ddr
(
        .datain_h(1'b0),
        .datain_l(1'b1),
        .outclock(clk_ram),
        .dataout(SDRAM_CLK),
        .aclr(1'b0),
        .aset(1'b0),
        .oe(1'b1),
        .outclocken(1'b1),
        .sclr(1'b0),
        .sset(1'b0)
);


sdram sdram
(
   .*,
   .init(~locked),
   .clk(clk_ram),

   .wtbt(0),
   .addr(ioctl_download ? ioctl_addr : rom_address),
   .rd(rom_rd),
   .dout(rom_data),
   .din (ioctl_download? ioctl_dout:rom_q),
   .we  (ioctl_download? ioctl_wr : isRAM?rom_wr:0),
   .ready()
);


// ICD SPI-slave interface
wire ICD_CSn;
wire ICD_MOSI;
wire ICD_MISO;
wire ICD_SCK;

sd_card sd_card (
	.clk_sys         ( clk_sys        ),   // at least 2xsd_sck
	// connection to io controller
	.sd_lba          ( sd_lba[0]      ),
	.sd_rd           ( sd_rd          ),
	.sd_wr           ( sd_wr          ),
	.sd_ack          ( sd_ack         ),
	.sd_conf         ( sd_conf        ),
	.sd_ack_conf     ( sd_ack_conf    ),
	.sd_sdhc         ( sd_sdhc        ),
	.allow_sdhc      (1'b0            ),
	.sd_buff_dout    ( sd_buff_dout   ),
	.sd_buff_wr      ( sd_buff_wr     ),
	.sd_buff_din     ( sd_buff_din[0] ),
	.sd_buff_addr    ( sd_buff_addr   ),

   .img_mounted   (img_mounted),
	.img_size      (img_size),
	
	// connection to local CPU
	.sd_cs   		( ICD_CSn  ),
	.sd_sck  		( ICD_SCK  ),
	.sd_sdi  		( ICD_MOSI ),
	.sd_sdo  		( ICD_MISO )
);

///////////////////////////////////////////////////



wire  [11:0] psg_a;
wire  [11:0] psg_b;
wire  [11:0] psg_c;
wire  [13:0] psg_out;

wire  [1:0] stereo = status [9:8];

wire [3:0]  R, G, B; 
wire        HSync, VSync, HBlank, VBlank;


wire PS2K_CLK;
wire PS2K_DATA;
wire PS2M_CLK;
wire PS2M_DATA;

    // Instance of the "top" module
    top_nora u_top (
        .FPGACLK(clk_sys),

        // CPU interface
        .CD(CD),
        .CA(CA),

        .CRESn(CRESn),
        .CIRQn(CIRQn),
        .CNMIn(CNMIn),
        .CABORTn(CABORTn),
        .CPHI2(CPHI2),
        .CBE(CBE),

        .CRDY(CRDY),
        .CSOB_MX(CSOB_MX),

        .CSYNC_VPA(CSYNC_VPA),
        .CMLn(CMLn),
        .CVPn(CVPn),
        .CVDA(CVDA),
        .CEF(CEF),
        .CRWn(CRWn),

        // Memory bus
        .MAH(MAH),
        .MAL(MAL),
        .MD(MD),
        .M1CSn(M1CSn),
        .MRDn(MRDn),
        .MWRn(MWRn),

        // I2C bus
        .I2C_SCL(I2C_SCL),
        .I2C_SDA(I2C_SDA),

        // SNES
//        .NESLATCH(NESLATCH),
//        .NESCLOCK(NESCLOCK),
//        .NESDATA1(NESDATA1),
//        .NESDATA0(NESDATA0),

//        // LEDs
//        .CPULED0(CPULED0),
//        .CPULED1(CPULED1),
//        .DIPLED0(DIPLED0),
//        .DIPLED1(DIPLED1),

        // CPUTYPE strap
        .CPUTYPE02(0), //// board assembly CPU type: 0 => 65C816 (16b), 1 => 65C02 (8b)

        // PS2 ports
        .PS2K_CLK(PS2K_CLK),
        .PS2K_DATA(PS2K_DATA),
        .PS2M_CLK(PS2M_CLK),
        .PS2M_DATA(PS2M_DATA),

        // UART port
//        .UART_CTS(UART_CTS),
//        .UART_RTS(UART_RTS),
        .UART_TX(UART_TX),
        .UART_RX(UART_RX),

        // Chip-Selects
        .VCS0n(VCS0n),
        .ACS1n(ACS1n),
        .ECS2n(ECS2n),
        .UE_CS3n(UE_CS3n),

        // IRQ
        .VIRQn(VIRQn),
        .AIRQn(AIRQn),
        .EIRQn(EIRQn),

        // Resets
        .ERSTn(ERSTn),

        // ICD SPI-slave interface
        .ICD_CSn(ICD_CSn),
        .ICD_MOSI(ICD_MOSI),
        .ICD_MISO(ICD_MISO),
        .ICD_SCK(ICD_SCK),

        // Button input
        .ATTBTN(buttons[1]),

        // Master SPI interface for SPI-flash access
//        .FMOSI(FMOSI),
//        .FMISO(FMISO),
//        .FSCK(FSCK),
//        .FLASHCSn(FLASHCSn)
    );

/////////////////// CPU ////////////////////////////////
// Instance of the "P65C816" processor module
////////////////////////////////////////////////////////
// Wire declarations for shared signals

wire [23:0] CA;
wire [7:0]  CD;
wire [7:0]  CD_i;
wire [7:0]  CD_o;
wire [20:12] MAH;
wire [11:0] MAL;
wire [7:0] MD;
wire CRESn;
wire CIRQn;
wire CNMIn;
wire CABORTn;
wire CBE;
wire CSYNC_VPA;
wire CMLn;
wire CVPn;
wire CVDA;
wire CRDY;
wire CRWn;


 
    P65C816 u_P65C816 (
        .CLK(clk_sys),
        .RST_N(CRESn),
        .CE(CBE),
        .RDY_IN(CRDY),
        .NMI_N(CNMIn),
        .IRQ_N(CIRQn),
        .ABORT_N(CABORTn),
        .D_IN(CD_i),
        .D_OUT(CD_o),
        .A_OUT(CA),
        .WE(CRWn),
        .RDY_OUT(CRDY),
        .VPA(CSYNC_VPA),
        .VDA(CVDA),
        .MLB(CMLn),
        .VPB(CVPn)
    );

/////////////////// Audio ////////////////////////////////
////////////////////////////////////////////////////////
// Instance of the "aura" audio chip module

wire VAUDIO_LRCK;
wire VAUDIO_BCK;
wire VAUDIO_DATA;


    aura u_aura (
        .ASYSCLK(clk_video),
        .AB(CA[4:0]),
        .DB(CD),
        .ACS1N(ACS1n),
        .VCS0N(VCS0n),
        .MRDN(MRDn),
        .MWRN(MWRn),
        .AIRQN(AIRQn),
        .IOCSN(),
        .VAUDIO_LRCK(VAUDIO_LRCK),
        .VAUDIO_DATA(VAUDIO_DATA),
        .VAUDIO_BCK(VAUDIO_BCK),
        .AUDIO_BCK(),
        .AUDIO_DATA(),
        .AUDIO_LRCK(),
        .SD_SSELN(),
        .AURALED(),
        .ASPI_MOSI(),
        .ASPI_MISO(),
        .ASPI_SCK(),
        .AFLASH_SSELN()
    );

    top u_vga_audio (
        .clk25(clk_video),

        // External bus interface
        .extbus_cs_n(ACS1n),
        .extbus_rd_n(MRDn),
        .extbus_wr_n(MWRn),
        .extbus_a(CA[4:0]),
        .extbus_d(MD),
        .extbus_irq_n(AIRQn),

        // VGA interface
        .vga_r(R),
        .vga_g(G),
        .vga_b(B),
        .vga_hsync(HSync),
        .vga_vsync(VSync),

        // SPI interface
        .spi_sck(ICD_SCK),
        .spi_mosi(ICD_MOSI),
        .spi_miso(ICD_MISO),
        .spi_ssel_n_sd(ICD_CSn),

        // Audio output
        .audio_lrck(VAUDIO_LRCK),
        .audio_bck(VAUDIO_BCK),
        .audio_data(VAUDIO_DATA)
    );


/////////////////// VIDEO PROCESSING ////////////////////////////////

mist_dual_video 
   #(.COLOR_DEPTH(4),
	.SD_HCNT_WIDTH(11),
	.OUT_COLOR_DEPTH(VGA_BITS),
	.USE_BLANKS(1'b1),
	.BIG_OSD(BIG_OSD)
	)
mist_video
(
	.clk_sys        ( clk_hdmi         ),
	.SPI_SCK        ( SPI_SCK          ),
	.SPI_SS3        ( SPI_SS3          ),
	.SPI_DI         ( SPI_DI           ),
	.R              ( R                ),
	.G              ( g                ),
	.B              ( b                ),
	.HBlank         ( HBlank           ),
	.VBlank         ( VBlank           ),
	.HSync          ( HSync            ),
	.VSync          ( VSync            ),
	.VGA_R          ( VGA_R            ),
	.VGA_G          ( VGA_G            ),
	.VGA_B          ( VGA_B            ),
	.VGA_VS         ( VGA_VS           ),
	.VGA_HS         ( VGA_HS           ),
`ifdef USE_HDMI
	.HDMI_R         ( HDMI_R           ),
	.HDMI_G         ( HDMI_G           ),
	.HDMI_B         ( HDMI_B           ),
	.HDMI_VS        ( HDMI_VS          ),
	.HDMI_HS        ( HDMI_HS          ),
	.HDMI_DE        ( HDMI_DE          ),
`endif

	.ce_divider     ( 4'h3             ),
	.rotate         ( 2'b00            ),
	.rotate_screen  ( 1'b0             ),
	.rotate_hfilter ( 1'b0             ),
	.rotate_vfilter ( 1'b0             ),
	.blend          (                  ),
	.scandoubler_disable(scandoubler_disable),
	.scanlines      (status[5:4]),
	.ypbpr          (ypbpr             ),
	.no_csync       (no_csync          )
	);

	
//`ifdef USE_HDMI
//i2c_master #(28_000_000) i2c_master (
//	.CLK         (clk_hdmi),
//	.I2C_START   (i2c_start),
//	.I2C_READ    (i2c_read),
//	.I2C_ADDR    (i2c_addr),
//	.I2C_SUBADDR (i2c_subaddr),
//	.I2C_WDATA   (i2c_dout),
//	.I2C_RDATA   (i2c_din),
//	.I2C_END     (i2c_end),
//	.I2C_ACK     (i2c_ack),
//
//	//I2C bus
//	.I2C_SCL     (HDMI_SCL),
//	.I2C_SDA     (HDMI_SDA)
//);	
//assign HDMI_PCLK = clk_hdmi;
//
//`endif

////////////////////// AUDIO MIXING /////////////////////////////
wire [15:0] psg_l;
wire [15:0] psg_r;
reg [12:0] psg_ab;
reg [12:0] psg_ac;
reg [12:0] psg_bc;


always @ (clk_sys,psg_a,psg_b,psg_c) begin
 psg_ab <= {{1'b0,psg_a} + {1'b0,psg_b}};
 psg_ac <= {{1'b0,psg_a} + {1'b0,psg_c}};
 psg_bc <= {{1'b0,psg_b} + {1'b0,psg_c}};
end

assign psg_l = (stereo == 2'b00) ? {psg_out,2'b0} : (stereo == 2'b01) ? {psg_ab,3'b0}: {psg_ac,3'b0};
assign psg_r = (stereo == 2'b00) ? {psg_out,2'b0} : (stereo == 2'b01) ? {psg_bc,3'b0}: {psg_bc,3'b0};

wire [15:0] dac_in_l,dac_in_r;

assign dac_in_l = psg_l;
assign dac_in_r = psg_r;


dac #(
   .c_bits	(16))
audiodac_l(
   .clk_i	(clk_sys	),
   .res_n_i	(1	),
   .dac_i	(dac_in_l),
   .dac_o	(AUDIO_L)
  );

dac #(
   .c_bits	(16))
audiodac_r(
   .clk_i	(clk_sys	),
   .res_n_i	(1	),
   .dac_i	(dac_in_r),
   .dac_o	(AUDIO_R)
  );

wire [31:0] clk_rate =  32'd24_000_000;

`ifdef I2S_AUDIO
i2s i2s (
	.reset(1'b0),
	.clk(clk_sys),
	.clk_rate(clk_rate),

	.sclk(I2S_BCK),
	.lrclk(I2S_LRCK),
	.sdata(I2S_DATA),

	.left_chan ({~dac_in_l[15],dac_in_l[14:0]}),
	.right_chan({~dac_in_r[15],dac_in_r[14:0]})
);
`ifdef I2S_AUDIO_HDMI
assign HDMI_MCLK = 0;
always @(posedge clk_sys) begin
	HDMI_BCK <= I2S_BCK;
	HDMI_LRCK <= I2S_LRCK;
	HDMI_SDATA <= I2S_DATA;
end
`endif
`endif

`ifdef SPDIF_AUDIO
spdif spdif (
	.clk_i(clk_sys),
	.rst_i(1'b0),
	.clk_rate_i(clk_rate),
	.spdif_o(SPDIF),
	.sample_i({dac_in_r, dac_in_l})
);
`endif


wire tape_in;
wire tape_out;
wire cas_relay;

assign tape_in = TAPE_SOUND;

endmodule


