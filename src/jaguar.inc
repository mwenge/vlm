;****************************************************************************
;
; JAGUAR.INC  Hardware Equates for JAGUAR System
;
;     COPYRIGHT 1994 Atari Computer Corporation
;          UNATHORIZED REPRODUCTION, ADAPTATION, DISTRIBUTION,   
;          PERFORMANCE OR DISPLAY OF THIS COMPUTER PROGRAM OR    
;        THE ASSOCIATED AUDIOVISUAL WORK IS STRICTLY PROHIBITED. 
;                            ALL RIGHTS RESERVED.   
;
;****************************************************************************
;
;
; JAGUAR REGISTERS
;
BASE        equ $F00000 ;TOM Internal Register Base
;
;
;            size R/W description
MEMCON1     equ $F00000 ;w RW Memory Configuration Register One
MEMCON2     equ $F00002 ;w RW Memory Configuration Register Two
;
HC          equ $F00004 ;w RW Horizontal Count
VC          equ $F00006 ;w RW Vertical Count
;
LPH         equ $F00008 ;w RO Horizontal Lightpen
LPV         equ $F0000A ;w RO Vertical Lightpen
;
;   equ $F0000C ;w (unused)
;   equ $F0000E ;w (unused)
;
;
OB0         equ $F00010 ;w RO Current Object Phrase
OB1         equ $F00012 ;w RO
OB2         equ $F00014 ;w RO
OB3         equ $F00016 ;w RO
;
; $F00018-$F0001E (reserved)
;
OLP         equ $F00020 ;l WO Object List Pointer (little endian)
ODP         equ $F00024 ;w    Object Data Pointer
OBF         equ $F00026 ;w WO Object Processor Flag
;
VMODE       equ $F00028 ;w WO Video Mode
;
BORD1       equ $F0002A ;w WO Border Color (Red & Green)
BORD2       equ $F0002C ;w WO Border Color (Blue)
;
HP          equ $F0002E ;w WO Horizontal Period
HBB         equ $F00030 ;w WO Horizontal Blanking Begin
HBE         equ $F00032 ;w WO Horizontal Blanking End
HS          equ $F00034 ;w WO Horizontal Sync
HVS         equ $F00036 ;w WO Horizontal Vertical Sync
HDB1        equ $F00038 ;w WO Horizontal Display Begin One
HDB2        equ $F0003A ;w WO Horizontal Display Begin Two
HDE         equ $F0003C ;w WO Horizontal Display End
VP          equ $F0003E ;w WO Vertical Period
VBB         equ $F00040 ;w WO Vertical Blanking Begin
VBE         equ $F00042 ;w WO Vertical Blanking End
VS          equ $F00044 ;w WO Vertical Sync
VDB         equ $F00046 ;w WO Vertical Display Begin
VDE         equ $F00048 ;w WO Vertical Display End
VEB         equ $F0004A ;w WO Vertical equalization Begin
VEE         equ $F0004C ;w WO Vertical equalization End
VI          equ $F0004E ;w WO Vertical Interrupt
PIT0        equ $F00050 ;w WO Programmable Interrupt Timer (Lo)
PIT1        equ $F00052 ;w WO Programmable Interrupt Timer (Hi)
HEQ         equ $F00054 ;w WO Horizontal equalization End
TEST1       equ $F00056 ;w    Undocumented Register - DO NOT USE
BG          equ $F00058 ;w WO Background Color
;
; $F0005A-$F000DE  (reserved)
;
INT1        equ $F000E0 ;w RW CPU Interrupt Control Register
INT2        equ $F000E2 ;w WO CPU Interrupt Resume Register
;
; $F000E4-$F003FE  (reserved)
;
CLUT        equ $F00400 ;w RW -$F005FE Color Lookup Table (A)
;       ;w RW -$F007FE Color Lookup Table (B)
LBUFA       equ $F00800 ;w RW -$F00D9E Line Buffer A
; $F00DA0-$F00FFE  (reserved)
LBUFB       equ $F01000 ;w RW -$F0159E Line Buffer B
; $F015A0-$F017FE  (reserved)
LBUFC       equ $F01800 ;w RW -$F01D9E Line Buffer Current
;
; $F01DA0-$F01FFE  (reserved)
;
; $F02000-$F020FE  (reserved)
;
; GPU REGISTERS
;
G_FLAGS     equ $F02100 ;l RW GPU Flags
G_MTXC      equ $F02104 ;l WO GPU Matrix Control
G_MTXA      equ $F02108 ;l WO GPU Matrix Address
G_END       equ $F0210C ;l WO GPU Data Organization
G_PC        equ $F02110 ;l RW GPU Program Counter
G_CTRL      equ $F02114 ;l RW GPU Operation Control/Status
G_HIDATA    equ $F02118 ;l RW GPU Bus Interface high data
G_REMAIN    equ $F0211C ;l RO GPU Division Remainder
G_DIVCTRL   equ $F0211C ;l WO DSP Divider control
;
; $F02120-$F021FE  (reserved)
;
; BLTTER REGISTERS
;
A1_BASE     equ $F02200 ;l WO A1 Base Address
A1_FLAGS    equ $F02204 ;l WO A1 Control Flags
A1_CLIP     equ $F02208 ;l WO A1 Clipping Size
A1_PIXEL    equ $F0220C ;l RW A1 Pixel Pointer
A1_STEP     equ $F02210 ;l WO A1 Step (Integer Part)
A1_FSTEP    equ $F02214 ;l WO A1 Step (Fractional Part)
A1_FPIXEL   equ $F02218 ;l RW A1 Pixel Pointer (Fractional)
A1_INC      equ $F0221C ;l WO A1 Increment (Integer Part)
A1_FINC     equ $F02220 ;l WO A1 Increment (Fractional Part)
A2_BASE     equ $F02224 ;l WO A2 Base Address
A2_FLAGS    equ $F02228 ;l WO A2 Control Flags
A2_MASK     equ $F0222C ;l WO A2 Address Mask
A2_PIXEL    equ $F02230 ;l RW A2 PIXEL
A2_STEP     equ $F02234 ;l WO A2 Step (Integer)
;
B_CMD       equ $F02238 ;l WO Command
        ;l RO Status
;
B_COUNT     equ $F0223C ;l WO Counters
B_SRCD      equ $F02240 ;2xl WO Source Data
B_DSTD      equ $F02248 ;2xl WO Destination Data
B_DSTZ      equ $F02250 ;2xl WO Destination Z
B_SRCZ1     equ $F02258 ;2xl WO Source Z (Integer)
B_SRCZ2     equ $F02260 ;2xl WO Source Z (Fractional)
B_PATD      equ $F02268 ;2xl WO Pattern Data
B_IINC      equ $F02270 ;l WO Intensity Increment
B_ZINC      equ $F02274 ;l WO Z Increment
B_STOP      equ $F02278 ;l WO Collision stop control
;
B_I3        equ $F0227C ;l WO Blitter Intensity 3
B_I2        equ $F02280 ;l WO Blitter Intensity 2
B_I1        equ $F02284 ;l WO Blitter Intensity 1
B_I0        equ $F02288 ;l WO Blitter Intensity 0
;
B_Z3        equ $F0228C ;l WO Blitter Z 3
B_Z2        equ $F02290 ;l WO Blitter Z 2
B_Z1        equ $F02294 ;l WO Blitter Z 1
B_Z0        equ $F02298 ;l WO Blitter Z 0
;
; $F0229C-$F02FFC  (reserved)
;
;
G_RAM       equ $F03000 ;l RW GPU Internal sRAM
G_ENDRAM    equ $F04000 ;  end GPU internal sRAM (+1), 4K bytes
;
; $F04000-$F09FFC (reserved)
;
; $F0A000-$F0A0FC (reserved)
;
;
; $F0A100-$F0BFFC (hi speed WO access of $F02100-$F03FFC, longwords only) 
;
;
; $F0C000-$F0FFFC (reserved)
;
; JERRY TIMERS
;
JPIT1       equ $F10000 ;w WO Timer 1 Pre-Scaler
JPIT2       equ $F10002 ;w WO Timer 1 Divider
JPIT3       equ $F10004 ;w WO Timer 2 Pre-Scaler
JPIT4       equ $F10006 ;w WO Timer 2 Divider
;
; $F10008-$F1000E  (reserved)
;
CLK1        equ $F10010 ;w WO Processor Clock Frequency
CLK2        equ $F10012 ;w WO Video Clock Frequency
CHRO_CLK    equ $F10014 ;w WO Chroma clock control
;
; $F10016-$F1001E  (reserved)
;
J_INT       equ $F10020 ;w RW Jerry's Interrupt control Register
;
; $F10022-$F1002E  (reserved)
;
ASIDATA     equ $F10030 ;w RW Asynchronous Serial Data
ASICTRL     equ $F10032 ;w WO Asynchronous Serial Control
ASISTAT     equ $F10032 ;w RO Asynchronous Serial Status
ASICLK      equ $F10034 ;w RW Asynchronous Serial Interface Clock
;
;
; JERRY TIMERS READ ACCESS
;
;     $F10036 ;w RO Timer 1 Pre-Scaler
;     $F10038 ;w RO Timer 1 Divider
;     $F1003A ;w RO Timer 2 Pre-Scaler
;     $F1003C ;w RO Timer 2 Divider
;
; $F10040-$F13FFC  (reserved)
;
JOY1        equ $F14000 ;w RW Joystick register
JOY2        equ $F14002 ;w RW Button register
;
; $F14004-$F147FC  (reserved)
;
;
; General Purpose IO
;
GPIO_0      equ $F14800 ;w -$F14FFE Used by EEPROM code
GPIO_1      equ $F15000 ;w -$F15FFE
GPIO_2      equ $F16000 ;w -$F16FFE
GPIO_3      equ $F17000 ;w -$F177FE
GPIO_4      equ $F17800 ;w -$F17BFE
GPIO_5      equ $F17C00 ;w -$F17FFE
;
; $F18000-$F1A0FC  (reserved)
;
;
; JERRY Registers (DSP)
;
D_FLAGS     equ $F1A100 ;l RW DSP Flags
D_MTXC      equ $F1A104 ;l WO DSP Matrix Control
D_MTXA      equ $F1A108 ;l WO DSP Matrix Address
D_END       equ $F1A10C ;l WO DSP Data Organization
D_PC        equ $F1A110 ;l RW DSP Program Counter
D_CTRL      equ $F1A114 ;l RW DSP Operation Control/Status
D_HIDATA    equ $F1A118 ;l WO DSP Bus Interface high data
D_REMAIN    equ $F1A11C ;l RO DSP Division Remainder
D_DIVCTRL   equ $F1A11C ;l WO DSP Divider control
;
;
; $F1A120-$F1A13C  (reserved)
;
; PULSE WIDTH MODULATION DACs
;
DAC1        equ $F1A140 ;l WO Left 14-bit DAC
DAC2        equ $F1A144 ;l WO Right 14-bit DAC
;
; I2S transmit/receive data
LTXD        equ $F1A148 ;l WO Left 16-bit transmit data
LRXD        equ $F1A148 ;l RO Left 16-bit receive data
;
RTXD        equ $F1A14C ;l WO Right 16-bit transmit data
RRXD        equ $F1A14C ;l RO Right 16-bit receive data
;
;
; $F1A148-$F1A14C  (reserved)
;
;
SCLK        equ $F1A150 ;l WO SSI Clock Frequency
SSTAT       equ $F1A150 ;l RO Serial Status
SMODE       equ $F1A154 ;l WO SSI Control
;
;
; $F1A158-$F1AFFC  (reserved)
;
D_RAM       equ $F1B000 ; DSP Internal RAM
D_ENDRAM    equ $F1D000 ; End of sRAM (+1) 8K bytes
;
;
; ROM Tables built into Jerry 128 samples
; 16 bit samples sign extended to 32
;
ROM_TABLE   equ $F1D000 ; Base of tables
;
; These are NEW CHANGED DIFFERENT equates they should cause less confusion
;
ROM_TRI     equ $F1D000 ; A triangle wave
ROM_SINE    equ $F1D200 ; Full amplitude SINE
ROM_AMSINE  equ $F1D400 ; Linear (?) ramp SINE
ROM_SINE12W equ $F1D600 ; SINE(X)+SINE(2*X)
ROM_CHIRP16 equ $F1D800 ; SHORT SWEEP
ROM_NTRI    equ $F1DA00 ; Triangle w/NOISE
ROM_DELTA   equ $F1DC00 ; Positive spike
ROM_NOISE   equ $F1DE00 ; Guess
; vim:ft=asm68k ts=2
