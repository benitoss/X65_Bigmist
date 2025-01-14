NORA Registers
===============

Two special registers at the beginning of the CPU zero page ARE ONLY AVAILABLE IF THE BIT [5] MIRROR_ZP IN RMBCTRL REG. IS SET.
Otherwise, normal memory is displayed.

    Address         Reg.name            Bits        Description
    $0000           RAMBLOCK            [7:0]       MIRROR OF THE REGISTER RamBLOCK_AB at $9F50.
                                                    This 8-bit register specifies which 8kB-BLOCK of the 2MB SRAM
                                                    is mapped into the 8kB RAM-BLOCK FRAME visible at the CPU address $A000 to $BFFF.
                                                    See description of RamBLOCK_AB below.
    
    $0001           ROMBLOCK                        MIRROR OF THE REGISTER ROMBLOCK at $9F51.
                                        [4:0]       This 5-bit register specifies which 16kB-BLOCK from the SRAM's 
                                                    512kB area 0x08_0000 to 0x0F_FFFF is mapped into the 16kB ROM-BLOCK FRAME
                                                    visible at the CPU address $C000 to $FFFF.
                                                    See description of ROMBLOCK below.


NORA's main register block starts at $00_9F50 (65816 address), that is $9F50 in 6502.
The first two registers support *RAM-Block mapping* (at $A000) and *system reset* functions:


    Address         Reg.name            Bits        Description
    $9F50           RamBLOCK_AB         [7:0]       This 8-bit register specifies which 8kB-BLOCK of the 2MB SRAM
                                                    is mapped into the 8kB RAM-BLOCK FRAME visible at the CPU address $A000 to $BFFF. The contents 
                                                    of this 8b register is bit-wise ANDed with the register RAMBMASK (at $9F52) and the result 
                                                    is the SRAM RAM-block number.
                                                    (As described in memory-map, the 2MB SRAM has 256 of 8kB BLOCKs).
                                                    These RAM-Blocks are numbered in the 2MB SRAM starting in the MIDDLE and wrapping around: 
                                                        RAM-Block #0   is from SRAM 0x10_0000 to 0x10_1FFF,
                                                        RAM-Block #1   is from SRAM 0x10_2000 to 0x10_3FFF, etc.,
                                                        RAM-Block #127 is from SRAM 0x1F_E000 to 0x1F_FFFF,
                                                        RAM-Block #128 is from SRAM 0x00_0000 to 0x00_1FFF,
                                                        RAM-Block #129 is from SRAM 0x00_2000 to 0x00_2FFF, etc.,
                                                        RAM-Block #255 is from SRAM 0x0F_E000 to 0x0F_FFFF.
                                                    Note 1: in CX16 parlance this register is called "RAMBANK", but the function is (basically) the same.
                                                    Note 2: RAM-Blocks 128 to 132 are always mapped to the CPU "low-memory" addresses from $0000 to $9EFF. 
                                                    (65816: from $00_0000 to $00_9EFF => in Bank 0.)
                                                    RAM-Blocks 192 to 255 are also available as ROM-Blocks, see below.
    
    $9F51           RamBLOCK_CD                     [SHARED ADDRESS WITH ROMBLOCK reg (SEE BELOW). Must be enabled in bit [3] ENABLE_RA_CD of RMBCTRL]
                                                    This 8-bit register specifies which 8kB-BLOCK of the 2MB SRAM
                                                    is mapped into the 8kB RAM-BLOCK FRAME visible at the CPU address $C000 to $DFFF. 
                                                    The contents  of this 8b register is bit-wise ANDed with the register RAMBMASK (at $9F52) 
                                                    and the result is the SRAM RAM-block number. The mapping to SRAM is the same as for the $A000 area
                                                    -> see RamBLOCK_AB description above.
                                                    Bit [3] ENABLE_RA_CD of RMBCTRL must be set to 1 to enable this register.

    $9F51           ROMBLOCK                        [SHARED ADDRESS WITH RamBLOCK_CD reg (SEE ABOVE). Must be enabled in bit [4] ENABLE_RO_CDEF of RMBCTRL]
                                        
                                        [7:5]   reserved, write 0
                                        
                                        [4:0]   This 5-bit register specifies which 16kB-BLOCK from the SRAM's 
                                                512kB area 0x08_0000 to 0x0F_FFFF is mapped into the 16kB ROM-BLOCK FRAME
                                                visible at the CPU address $C000 to $FFFF.
                                                There are 32 x 16kB ROM-Blocks in SRAM:
                                                    ROM-Block #0  is from SRAM 0x08_0000 to 0x08_3FFF (= also known as the 8kB RAM-Blocks #192 and #193),
                                                    ROM-Block #1  is from SRAM 0x08_4000 to 0x08_7FFF (= also known as the 8kB RAM-Blocks #194 and #195), etc.,
                                                    ROM-Block #31 is from SRAM 0x0F_C000 to 0x0F_FFFF (= also known as the 8kB RAM-Blocks #254 and #255).
                                                The ROMBLOCK register allows addressing up to 64 ROM-Blocks, but only the first 32 are available in the SRAM.

    $9F52           RamBMASK            [7:0]       Mask register for RamBLOCK_AB and RamBLOCK_CD effective address calculation. 
                                                    The effective RAM-Block number is = RAMBLOCK & RAMBMASK.
                                                    For CX16 ROMs this register should be set to 0x7F to limit the RAMBLOCK addressing to 1MB.
                                                    Otherwise, RAM-Blocks 128-132, mapped to CPU "low-memory", get overwriten by the OS.
                                                    For software aware of X65 memory map this register could be set to 0xFF to allow full SRAM access even 
                                                    from 8-bit (6502) code.
                                                    (For 16-bit code (65816) using 24-bit native linear addressing the RAMBMASK is irrelevant.)
    
    $9F53           RMBCTRL                         Controls the Ram/ROM Block region mapping between $A000 to $FFFF and the function of register $9F51.

                                        [7] bit MAP_BOOTROM:
                                                When set to 1, NORA's BootRom is displayed in the ROM-Block Frame from $D000 to $FFFF, and any ROMBLOCK setting is ignored.
                                                When cleared to 0, the ROMBLOCK, if enabled in bit 4, define the contents of ROM-Block Frame.
                                                BootRom is 512B and mirrored over the 8kB frame.

                                        [6] bit AUTO_UNMAP_BOOTROM:
                                                When set to 1 together with bit [7], then the next RTI instruction (Return From Interrupt)
                                                will automatically clear bits [7] and [6], thus taking the BootRom out of the ROM-Block Frame.
                                                NMI is automatically blocked while bit [6] is set, and all other interrupts should be blocked in Sw.
                                                This is used by special handlers like ISAFIX.

                                        [5] bit MIRROR_ZP:
                                                When set to 1 the registers $9F50 and $9F51 are _also_ displayed in the zero page at the absolute 
                                                addresses $00_0000 and $00_0001. This is useful for 6502 code that needs to access these registers often
                                                and it is necessary for compatibility with CX16 code (= their "RAMBANK" and "ROMBANK" registers).
                                                When cleared to 0, the registers are not mirrored to the zero page.
                                                For 65816 systems in Native 16-bit mode, it is recommended _not_ to mirror these registers to the zero page,
                                                i.e. clear this bit to 0.
                                        
                                        [4] bit ENABLE_ROM_CDEF:
                                                When set to 1, the 16kB ROM-Block Frame from $C000 to $FFFF is enabled, 
                                                and the CPU sees the ROM-Block selected by the ROMBLOCK register at $9F51 (and $01 if MIRROR_ZP is set).
                                                When cleared to 0, the ROM-Block Frame is disabled, and the CPU sees the SRAM directly at $C000 to $FFFF.
                                                This is useful to enable for generic 6502 code and it is necessary for compatibility with CX16 code.
                                                For 65816 systems in Native 16-bit mode, it is recommended to clear this bit to 0 and not use ROM-Blocks.
                                        
                                        [3] bit ENABLE_RAM_CD:
                                                When set to 1, the second 8kB RAM-Block Frame between $C000 to $DFFF is enabled, and the CPU sees 
                                                the RAM-Block selected by the RamBLOCK_CD ($9F51) register.
                                                When cleared to 0, the second RAM-Block Frame is disabled, and the CPU sees the SRAM directly at $C000 to $DFFF.
                                                For 6502 system (8-bit) the ROM-Block Frame should be enabled (bit ENABLE_RO_CDEF=1) and this bit should be cleared to 0;
                                                this is also the necessary setting for compatibility with CX16 code.

                                                Note: it is not allowed to simultaneosly set ENABE_RAM_CD and ENABLE_ROM_CDEF bits to 1 !!
                                        
                                        [2] bit RDONLY_EF:
                                                When set (1), Read-only protect the area from $E000 to $FFFF.
                                            
                                        [1] bit RDONLY_CD:
                                                When set (1), Read-only protect the area from $C000 to $DFFF.

                                        [0] reserved, write 0


    $9F54           SYSCTRL                         System control / reset trigger.
                                        [7] bit UNLOCK: to prevent unintended system resets, the SYSCTRL must be first unlocked by writing 0x80 into the register.
                                            This bit always reads 0.
                                        [6] bit ABRT02: writing 1 (after UNLOCKing) will enable ABORTing of 65C02-only opcodes
                                            encountered during the Emulation mode of the 65816 processor: BBR, BBS, RMB, SMB.
                                            Software should then emulate the behaviour of the 65C02 instructions in the ABORT handler.
                                        [5] unused, write 0
                                        [4] unused, write 0
                                        [3] unused, write 0
                                        [2] bit NORARESET: writing 1 (after UNLOCKing) will trigger a hard NORA reset - bitstream reloading.
                                            This bit always reads 0.
                                        [1] bit CPUSTOP: writing 1 (after UNLOCKing) will stop the CPU forever.
                                            This bit always reads 0.
                                        [0] bit CPURESET: writing 1 (after UNLOCKing) will trigger CPU reset sequence. 
                                            Note: ROMBANK or any other registers are not affected!
                                            This bit always reads 0.


The next three registers control the *USB_UART* periphery:

    Address         Reg.name            Bits        Description
    $9F55           USB_UART_CTRL
                                        [2:0] = Baud Rate
                                                000 = reserved
                                                001 = reserved
                                                010 = 9600 Bd
                                                011 = 57600 Bd
                                                100 = 115200 Bd = DEFAULT
                                                101 = 230400 Bd
                                                110 = 1000000 Bd (non-standard freq)
                                                111 = 3000000 Bd (non-standard freq)
                                        [3] = Enable Parity generation/checking, DEFAULT=0
                                        [4] = Parity Configuration: 0=even (DEFAULT), 1=odd.
                                        [5] = Enable HW-FlowCtrl (RTS/CTS) - honoring the CTS signal.
                                                When the CTS input is active (low), transmission from FIFO is allowed.
                                                When the CTS input is inactive (high), transmissions are blocked.
                                                DEFAULT = OFF.
                                        [6] = Enable IRQ activation on not(RX-FIFO-empty)
                                        [7] = Enable IRQ activation on not(TX-FIFO-full)

    $9F56           USB_UART_STAT                   USB(FTDI)-UART Status
                                        [0] = reserved, 0.
                                        [1] = Is RX FIFO full?
                                        [2] = Is TX FIFO empty AND all chars were sent?
                                        [3] = Is TX FIFO full (no more TX chars can be accepted)?
                                        [4] = Framing Error Flag, latching, clear by writing 0.
                                        [5] = Parity Error Flag, latching, clear by writing 0.
                                        [6] = CTS signal Status: 1=CTS inactive (voltage=high), 0=CTS active (voltage=low).
                                        [7] = Is RX FIFO empty?

    $9F57           USB_UART_DATA       [7:0]       Reading dequeues data from the RX FIFO.
                                                    Writing enqueues to the TX FIFO.

The next three registers control the *UEXT_UART* periphery, currently NOT IMPLEMENTED:

    Address         Reg.name            Bits        Description
    $9F58           UEXT_UART_CTRL                  Same as USB_UART_CTRL but for UEXT UART. 
                                                    RTS/CTS Hw-Flow-Control is not supported on UEXT UART,
    $9F59           UEXT_UART_STAT                  Same as USB_UART_STAT but for UEXT UART. 
    $9F5A           UEXT_UART_DATA                  Same as USB_UART_DATA but for UEXT UART. 

The next three registers control the *I2C Master* periphery:

    Address         Reg.name            Bits        Description
    $9F5B           I2C_CTRL
                                        [2:0] = I2C_Command field:
                                                000 = idle/no operation.
                                                001 = START_ADDRESS: generate START condition, send ADDRESS (passed via the DATA register),
                                                        then read the ACK bit, which gets written into the DATA register.
                                                010 = SEND_DATA_READ_ACK: send data to I2C from the DATA reg., read ACK bit and write it to the DATA reg.
                                                011 = RECV_DATA: receive data from the I2C bus into DATA reg
                                                100 = WRITE_ACK
                                                101 = WRITE_NACK
                                                111 = STOP: generate stop condition
                                        
                                        [6:3] = reserved, 0
                                        [7] = Enable IRQ when not(BUSY)

    $9F5C           I2C_STAT            [3:0] = view of internal FSM state; see verilog
                                        [4] = SCL link state (high/low)
                                        [5] = SDA link state (high/low)
                                        [6] = TIMEOUT flag: reads 1 if last operation had to be cancelled due to I2C bus timeout.
                                              The flag is automatically cleared when a new operation is started.
                                        [7] = BUSY flag: reads 1 if operation in progress, otherwise 0.

    $9F5D           I2C_DATA            [7:0]       Reads/writes the DATA register.

The next five registers control the dual-PS/2 periphery through the SMC:

    Address         Reg.name            Bits        Description
    $9F5E           PS2_CTRL                        PS2 Control register
                                        [7:2] = reserved, 0
                                        [1] = Enable IRQ when kbd buffer has a byte, or mouse buffer has >= 3 bytes
                                        [0] = Disable scancode-to-keycode translation in HW. 
                                              Default is 0 = translation enabled = PS2K_BUF reads IBM key-codes.
                                              If 1 = translation disabled = PS2K_BUF reads PS/2 scan-codes.

    $9F5F           PS2_STAT                        PS2 Status Register
                                        [7] = KBD Buffer FIFO is non-empty
                                        [6] = Mouse Buffer RX-FIFO is non-empty
                                        [5] = Mouse buffer RX-FIFO has 3 or more bytes [3B = mouse movement packet]
                                        [4:0] = unused, 0

    $9F60           PS2K_BUF            [7:0]       Keyboard buffer (FIFO output).
                                                    Reading gets the next keycode (scancode) received from the keyboard,
                                                    or 0x00 if the buffer was empty.
                                                    Writing will enqueue a 1-byte command for the keyboard.
                                                    See https://wiki.osdev.org/PS/2_Keyboard#Scan_Code_Sets for the hints.

    $9F61           PS2K_RSTAT          [7:0]       Reply status from keyboard (in response from a command).
                                                    Possible values:
                                                        0x00 => idle (no transmission started)
                                                        0x01 => transmission pending
                                                        0xFA => ACK received
                                                        0xFE => ERR received
                                                    Special case: Writing will enqueue a 2-byte command for the keyboard
                                                    (both bytes must be written in PS2K_RSTAT consequtevely.)

    $9F62           PS2M_BUF            [7:0]       Mouse buffer (FIFO output).
   

The next three registers control the *SPI-Master* periphery in NORA.
The SPI-Master can access NORA's UNIFIED ROM (really the SPI-Flash primarilly for NORA bitstream), and the SPI bus on UEXT port:

    Address         Reg.name            Bits        Description
    $9F63           N_SPI_CTRL
                                        [7:6] = reserved, 0
                                        TBD: IRQ???
                                        [5:3] = set the SPI speed - SCK clock frequency:
                                                000 = 100kHz
                                                001 = 400kHz
                                                010 = 1MHz
                                                011 = 8MHz
                                                100 = 24MHz
                                                other = reserved.
                   
                                        [2:0] = set the target SPI slave (1-7), or no slave (CS high) when 0.
                                                000 = no slave (all deselect)
                                                001 = UNIFIED ROM = NORA's SPI-Flash
                                                010 = UEXT SPI-Bus
                                                other = reserved.

    $9F64           N_SPI_STAT                      SPI Status
                                        [0] = Is RX FIFO empty?
                                        [1] = Is RX FIFO full?
                                        [2] = Is TX FIFO empty?
                                        [3] = Is TX FIFO full?
                                        [4] = reserved, 0
                                        [5] = reserved, 0
                                        [6] = reserved, 0
                                        [7] = Is BUSY - is TX/RX in progress?

    $9F65           N_SPI_DATA                      Reading dequeues data from the RX FIFO.
                                                    Writing enqueues to the TX FIFO.



The next register controls global IRQ/NMI masking:

    $9F63           IRQCTRL         
                                        [7] = VERAIRQ_EN
                                        [6] = AURAIRQ_EN
                                        [5] = ETHIRQ_EN
                                        [4] = UEXTIRQ_EN
                                        [3] = NORAIRQ_EN (PS2, UART, IRQ, SPI)
                                        [2] = unused
                                        [1] = unused
                                        [0] = unused
    
    $9F64           IRQSTAT





