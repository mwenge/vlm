;==========================================================
;
;
;       CD Front Panel Module
;
;       This module manages the Audio CD control functions and interface
;       to the Virtual Light Machine
;
;       Programmer: Dave Staugas
;
;
;
serial  equ     113             ;serial # of this software revision
;
;
;
        .nlist
        .include        "../jaguar.inc"
        .include        "../blitter.inc"
        .include        "../cd.inc"
        .list
; 
;
; Butch's hardware registers
;
;
BUTCH     equ  $DFFF00          ;base of Butch=interrupt control register, R/W
;
;  When written (Long):
;
;  bit0 - set to enable interrupts
;  bit1 - enable CD data FIFO half full interrupt
;  bit2 - enable CD subcode frame-time interrupt (@ 2x spped = 7ms.)
;  bit3 - enable pre-set subcode time-match found interrupt
;  bit4 - CD module command transmit buffer empty interrupt
;  bit5 - CD module command receive buffer full
;  bit6 - CIRC failure interrupt
;
;  bit7-31  reserved, set to 0 
;
;
;  When read (Long):
;
;  bit0-8 reserved
;
;  bit9  - CD data FIFO half-full flag pending
;  bit10 - Frame pending
;  bit11 - Subcode data pending
;  bit12 - Command to CD drive pending (trans buffer empty if 1)
;  bit13 - Response from CD drive pending (rec buffer full if 1)
;  bit14 - CD uncorrectable data error pending
;
;
;   Offsets from BUTCH
;
DSCNTRL   equ  4                ; DSA control register, R/W
DS_DATA   equ  $A               ; DSA TX/RX data, R/W
;
;
;
I2CNTRL   equ  $10              ; i2s bus control register, R/W
;
;  When read:
;
;  b0 - I2S data from drive to BUTCH is ON if 1
;  b1 - I2S path from BUTCH to Jerry is ON if 1
;  b2 - reserved
;  b3 - host bus width is 16 if 1, else 32
;  b4 - FIFO state is not empty if 1
;
;
;
;
SBCNTRL   equ  $14              ; CD subcode control register, R/W
SUBDATA   equ  $18              ; Subcode data register A
SUBDATB   equ  $1C              ; Subcode data register B
SB_TIME   equ  $20              ; Subcode time and compare enable (D24)
FIFODAT   equ  $24              ; i2s FIFO data
I2SDAT2   equ  $28              ; i2s FIFO data (old)
;
;
;  Some symbols imported from Jeff's code (10-Aug-94)
;
        .include        "vlm.equ"
;
VOLUME  equ     $F1BE04         ;.l DSP volume constant (0..$7FFF)
;
;
;
DSPqcode        equ     $2800   ;DSP deposits polled q_subcodes here (32 bytes)
;DSPqcode       equ     $f1c000 ;DSP deposits polled q_subcodes here (32 bytes)
realTOC         equ     $2c00   ;here's where we put the directory
;
;
;
;
        bra     RanGetEE                ;+0 dispatch for table for 
        bra     RanPutEE                ;+4  get/put random data from EEprom
        dc.l    0                       ;+8  cdboot1's address for randf routine
;
start:                                  ;+C
        lea     $200000,sp              ;set stack at top of memory
        move.w  #-1,Pradip
        move.w  #3,TvChnlAv
;
        bsr     getEE                   ;get EEprom data
;
        lea     defseqm,a0              ;this code added to init 
        lea     seqmode,a1              ;modes that used to be in the
        moveq   #2,d0                   ;EEprom block (moved 13-Apr-95)
setdefs:
        move.w  (a0)+,(a1)+
        dbra    d0,setdefs

;       jsr     CD_stop                 ;stop the CD
;
        move.l  $f1003a,seed            ;get JPIT1 for random seed
;
        lea     hexbcd,a0               ;build quick hex2bcd conversion table
        moveq   #1,d1
        moveq   #99,d2
        moveq   #0,d0
        lsr.w   #1,d0                   ;clear x
hexbuild:
        move.b  d0,(a0)+
        abcd    d1,d0
        dbra    d2,hexbuild
;
        bsr     arrbuild        ;build trk grid excess arrow indicators
;
        move.w  #-1,CLUT+2
;
        lea     BUTCH,a4
        lea     return,a3
;
;       tst.l   realTOC
;       beq.s   .notext                 ;check for external--br if so
;
        move.l  #$f,I2CNTRL(a4)         ;5 = code, f= audio
;
        move.l  #$f2,SBCNTRL(a4)
        move.l  #$2f2,SBCNTRL(a4)
;
.notext:
;
;  align all graphics forms
;
;
;  .CRY form header is:
;
;       +0 width
;       +2 height
;       +4 D/IWIDTH
;       +6 unused
;       +8 # colors used
;       +A pixel depth
;       +C file size (long)
;
;   Convert to:
;
;       +0 offset to base of graphics form
;       +2 width
;       +4 height
;       +6 D/IWIDTH
;       +8 pixel depth
;
;       +A Palette (4,4,16,256 words for pixdepth 0,1,2,3 respectively)
;
;       +
;
;       
;
formdo:
        lea     allforms(pc),a3
formloop:
        move.l  (a3)+,d0
        beq     formxit
;
        move.l  d0,a0
        move.l  a0,a2           ;save base ptr
;
        move.w  (a0)+,d1        ;retrieve width
        move.w  (a0),d2         ;retrieve height
        move.w  d1,(a0)+        ;save width
        move.w  (a0),d3         ;retrieve D/IWIDTH
        move.w  d2,(a0)+        ;save height
        move.w  d3,(a0)+        ;save D/IWIDTH
        move.w  2(a0),d4
        move.w  d4,(a0)+        ;save pixel depth
        add.w   d4,d4
        move.w  palsize(pc,d4.w),d1
        lea     6(a0),a1
        bra.s   palmovi
palsize:
        dc.w    4,4,16,256
;
;  here are all the forms we transform (followed by long 0)
;
allforms:
;       dc.l    cdstatus
        dc.l    onepage3
        dc.l    0
;
;
palmove:
        move.w  (a1)+,(a0)+
palmovi:
        dbra    d1,palmove
;
        move.l  a0,d0
        addq.l  #7,d0
        andi.w  #~7,d0
        move.l  d0,d1
        sub.l   a0,d0           ;d0 = adjust value
        move.l  d1,a0
        sub.l   a2,d1
        move.w  d1,(a2)
        cmpi.w  #6,d0
        beq.s   formover
        mulu    d2,d3
        lsl.l   #2,d3           ;# of words in graphics form
;
        bra.s   grafupi
grafup0:
        swap    d3
grafup:
        move.w  (a1)+,(a0)+
grafupi:
        dbra    d3,grafup
        swap    d3
        dbra    d3,grafup0
formover:
        bra     formloop
formxit:
;
;  Load up the big palette
;
repaller:
;       lea     cdstatus+10,a0
        lea     onepage3+10,a0
        lea     CLUT,a1
        move.w  #255,d0 
fullpal:
        move.w  (a0)+,(a1)+
        dbra    d0,fullpal
;
        move.w  #255,d0
chkp:
        move.w  -(a1),d1
        cmp.w   -(a0),d1
        bne     repaller
        dbra    d0,chkp
;
;
;
;
;
;
;   let's expand (bit->pixel) cdnumb2.cry
;
bit2pix:
        lea     cnumber+$18,a0
        lea     $60000,a1       ;primary digits
        lea     $61000,a2       ;alternate digits
;
        moveq   #16,d5          ;17 lines hi
.loop0:
        moveq   #7,d2
.loop1:
        moveq   #15,d3
        move.w  (a0)+,d4
.loop2:
        moveq   #4,d0           ;background color (black)
        moveq   #4,d1           ;background color (black)
;
        rol.w   #1,d4
        bcc.s   .notfore
        moveq   #1,d0           ;foreground color (bright green)
        move.w  #$fe,d1         ;foreground color (??)
.notfore:
        move.b  d0,(a1)+
        move.b  d1,(a2)+
;
        dbra    d3,.loop2
        dbra    d2,.loop1
        dbra    d5,.loop0
;
;
VOLbar  equ     $12000
;
        lea     VOLbar,a0
        move.l  #$00000011,d0
        move.l  #$10000000,d1
        moveq   #0,d3
        move.w  #64-1,d2
VOLlp:
        move.l  d0,(a0)+
        move.l  d1,(a0)+
        move.l  d0,(a0)+
        move.l  d1,(a0)+
;
        move.l  d3,(a0)+
        move.l  d3,(a0)+
        dbra    d2,VOLlp
;
        move.l  #$22222222,d0
        move.l  #$22222220,d1
        move.l  d0,(a0)+
        move.l  d1,(a0)+
        move.l  d0,(a0)+
        move.l  d1,(a0)+
;
        move.l  d3,(a0)+
        move.l  d3,(a0)+

        move.l  #$03333333,d0
        move.l  #$33333300,d1
        move.w  #64-1,d2
VOLlp1:
        move.l  d0,(a0)+
        move.l  d1,(a0)+
        move.l  d0,(a0)+
        move.l  d1,(a0)+
;
        move.l  d3,(a0)+
        move.l  d3,(a0)+
        dbra    d2,VOLlp1
;
;
;       
here:
        clr.w   modecnt                 ;start at 0th index for our_mode
        clr.w   boxleft
        clr.w   cancelC                 ;cancel C button once if non-zero
        clr.w   scan                    ;no scan mode
        clr.w   play                    ;indicate stopped
        clr.w   pause                   ;not paused
        clr.w   track
        clr.w   audvlm                  ;set for audio mode
        clr.w   keytime
        clr.w   distrack
        clr.w   progenty
        clr.w   m3opt                   ;set for normal option on mode3
        clr.w   gridoff
        clr.w   vlmrand
        move.b  #-1,retcode+1           ;wait til found comes in on that stop
        bclr.b  #0,cdgflags
;
;
        lea     realTOC+2,a0            ;start at min track #
        move.b  (a0)+,d0
        move.w  d0,d2
        move.b  (a0)+,d0
        lsl.w   #8,d2
        move.b  d0,d2
        move.w  d2,maxmin
;
        move.l  (a0),d3
        andi.l  #$00ffffff,d3
        moveq   #3,d2
nextBCD:
        moveq   #0,d0
        move.b  d3,d0
        cmpi.w  #100,d0
        bcc     do99
        divu    #10,d0
        move.l  d0,d1
        swap    d1
        lsl.w   #4,d0
        or.w    d1,d0
        bra.s   itsBCD
do99:
        moveq   #$99,d0
itsBCD:
        move.b  d0,d3
        ror.l   #8,d3
        dbra    d2,nextBCD
BCDdone:
        move.l  d3,fineEND
;
        addq.l  #1,a0
        moveq   #0,d2
        moveq   #2,d3
buildf:
        move.b  (a0)+,d0
;       bsr     bcd2hex
        lsl.l   #8,d2
        move.b  d0,d2
        dbra    d3,buildf
        lsl.l   #8,d2
        move.l  d2,origEND
;
        lea     SubCode,a1
;
        movem.l a1/a4,ourreg
        move.w  #-1,oneshot             ;here's an init after Jeff
        clr.l   joyprev                 ;clear previous joystick
        clr.l   m3prev                  ;clear mode3 previous joystick
;
        clr.w   m3col
        move.w  #1,m3row
;
;
        move.l  #service,davesvec
;
        bsr     buildseq        ;build normal sequence of tracks
        bsr     bildprog        ;set user-programmable seq to standard
;
        move.w  maxmin,d0
        andi.w  #$ff,d0
        move.w  d0,gridsize     ;set total # of tracks for grid
;
;
        move.w  Sbankeff,d0
        move.w  d0,d1
        lsr.w   #4,d1
        andi.w  #$f,d1
        andi.w  #$f,d0
        move.w  d0,skid         ;set effect
        move.w  d1,imatrix      ;& bank too
;
        ; Turn off the demo mode.
        move.w  #2,freerun              ;shut down GPU
        move.w  frames,d1
fwait:
        cmp.w   frames,d1       ;we need to wait
        beq     fwait
;
        jmp     goag            ;this entry point is better
;
;
;
;   The following will be called as a subroutine from Jeff's main loop...
;
service:
        movem.l ourreg,a1/a4    ;get a1=subcode, a4=BUTCH
        tst.w   oneshot
        beq     onetime
;
        move.w  #$8003,our_mode         ;set mode 3, init bit set
        move.w  #0,vlm_mode     ;start in "audio" mode
;
        clr.w   oneshot
        clr.w   voltimr         ;ignore turn-off of volume
        clr.w   volfcnt         ;ignore volume raise/lower repeat time
;
        bset.b  #7,no_ksel+8    ;disable jeff's vlm edit mode
;
        tst.l   realTOC         ;are we external?
        bne.s   notextrn        ;br if not
;
;  else, set-up for external I2S
;
        move.l  #$37ded4a0,$f1bc58      ;this ugly patch in DSP is needed
;
        move.l  #0,BUTCH+I2CNTRL        ;*************test
        move.l  #9,SCLK
        move.l  #$15,SMODE              ;*************end test
notextrn:
;
;  set-up volume object
;
        lea     davesobj+$80,a0
        moveq   #-1,d1
        move.w  d1,$c(a0)       ;turn object off while we're working on 'em
        move.w  #30,(a0)        ;horz position
        move.w  #38*2,d0        ;assume NTSC
        tst.w   pal
        beq.s   .nopala
        add.w   #48*8,davelist+6        ;adjust cd+g screen YPOS
        move.w  #(38+27)*2,d0
.nopala:
        move.w  d0,$4(a0)       ;vert position
        move.w  #1,$14(a0)      ;rmw or transparent
        move.w  #$3e,$16(a0)    ;palette index
        move.w  #1,$18(a0)      ;DWIDTH
        move.w  #192,$1A(a0)    ;height
        move.w  #2,$1C(a0)      ;pixel depth
;
        lea     voltab,a3
        moveq   #0,d0
        move.w  volptr,d0
        add.w   d0,d0
        moveq   #0,d1
        move.w  (a3,d0.w),d1

        move.l  d1,VOLUME
;       move.l  #$7fff,VOLUME

        mulu    #12,d0
        add.l   #VOLbar,d0
        move.l  d0,$10(a0)
;
;       move.w  #0,CLUT+(128*2)
;       move.w  #$5678,CLUT+(129*2)     ;top color (thin)
;       move.w  #$7ff,CLUT+(130*2)      ;mid-line color
;       move.w  #$7fc0,CLUT+(131*2)     ;bottom color (fat)
;
        move.w  #-1,loceff
        move.w  #-1,locbank             ;set our local copies to force prn
;
;  Form upside-down vlm logo
;
        bsr      fliplogo
;
;
onetime:
        move.w  voltimr,d0      ;check volume display turn-off
        beq.s   notvol
        move.w  frames,d1
        sub.w   d1,d0
        bgt.s   notvol
        cmpi.w  #3,our_mode     ;are we in mode 3 (upper panel)?
        bne.s   offv            ;br if not
        tst.w   boxleft         ;are we on the volume bar?
        beq.s   offv
;
        addq.w  #8,d1           ;we're on the volume bar in mode3
        bne.s   .volset         ;keep it displaying
        addq.w  #1,d1           
.volset:
        move.w  d1,voltimr
        bra.s   notvol
offv:
        clr.w   voltimr         ;turn off display
        move.w  #-1,davesobj+$8c
;
        bsr     chngdisp        ;any need for RunBeast?
;
        moveq   #(volptr-workEE),d1
        bsr     setEE           ;set new volume in EEprom
notvol:
;
;  now check on VLM-random
;
        tst.w   cdgmode         ;we must be in vlm mode
        bne     notvlmr         ;br if not              
        tst.w   vlmrand         ;and random flag must be engaged
        beq     notvlmr         ;br if not
;
;  we're doing the random thang
        move.w  frames,d0
        sub.w   vlmrfrm,d0
        move.w  #20*60,d1       ;every x seconds
        tst.w   pal
        beq.s   intchk
        move.w  #20*50,d1
intchk:
        cmp.w   d1,d0           ;we'll do every x seconds
        bcs.s   notvlmr         ;br if not time yet
;
        move.l  $80008,a0       ;get cdboot1's randf
        move.l  #backhome,-(sp)
        jmp     (a0)
backhome:
        lsr.l   #8,d0           ;condition random value
        andi.l  #$ffff,d0
        move.w  frames,vlmrfrm  ;reset random timer
;
        move.w  vlmrdcnt,d1     ;get rand transition counter
        addq.w  #1,d1           ;advance
        cmpi.w  #15,d1          ;time to do bank switch?
        bcs.s   notrbank        ;br if not bank switch time
        moveq   #0,d1
notrbank:
        move.w  d1,vlmrdcnt
        beq.s   jabanks
;
jaeffs:
        divu    #9,d0
        swap    d0
        addq.w  #1,d0
        move.w  d0,skid
        move.l  #skidoo,action  ;this should change effect
        bra.s   notvlmr
jabanks:
        move.w  imatrix,d1      ;are we in funny bank?
        cmpi.w  #8,d1
        beq     jaeffs          ;if so, stay there
;
        divu    #8,d0           ;banks 0-7 please
        swap    d0
        move.w  d0,imatrix
        move.l  #gm,action      ;this should change bank
notvlmr:
;       .if     0
        move.w  vlm_mode,d0     ;check for Jeff in his edit mode
        cmpi.w  #3,d0           ;if he's editting, give him everything
        bne.s   notJedit
;
        tst.w   cancelC         ;have we already set cancelC?
        bmi.s   notJedit        ;br if so
;
;  We've entered vlm-edit mode
;
        moveq   #-1,d0
        move.w  d0,CLUT+2
        move.w  d0,cancelC      ;set to cancel one C button
        move.w  #$8000,our_mode ;revert to VLM mode
        move.w  #2,modecnt      ;tell indirect layer too
;       .endif
;
;  Check our mode for display option
;
;   0 - VLM only
;   1 - help controller diagram
;   2 - RMW digits only
;   3 - top full panel
;
;
notJedit:
;
;
        move.w  our_mode,d0     
        bpl     samode          ;br if no need to change mode
;
;
;  A mode change has occured--display should be changed
;
;   Modes for the time being:
;
;xxxx   0 - vlm mode
;xxxx   1 - controller diagram (help)
;xxxx   2 - digits only
;xxxx   3 - top full panel
;
        moveq   #-1,d1
        move.l  a1,a0
        move.l  d1,(a0)+        ;force update of track/time data
        move.l  d1,(a0)+
        move.l  d1,(a0)+

        lea     davesobj,a0

;
        btst.l  #14,d0          ;have we turned off previous mode's
        bne.s   offwait         ; grafix yet?  (br if so)
;
        bset.l  #14,d0          ;indicate we're waiting for mode change
        move.w  d0,our_mode     ;  to take effect
        moveq   #-1,d1
        move.w  d1,$c(a0)       ;turn objects off while we're working on 'em
        move.w  d1,$4c(a0)
;
        move.w  frames,d0
        addq.w  #1,d0           ;1 frame should be enuf to take effect
        move.w  d0,offrame
;
        move.w  frames,voltimr  ;turn off volume bar (if on)
;
        bsr     chngdisp
        bra     retvlm          ;back to vlm
;       rts
;
offwait:
        move.w  frames,d1
        cmp.w   offrame,d1
        bge.s   offwait1
        bra     retvlm
;
;       rts
;
offwait1:
        andi.w  #$f,d0
        move.w  d0,our_mode     ;here's our new mode    
;
;  ****added 17-Jan-95
        beq.s   jeffgo          ;br if entered vlm mode
        bset.b  #7,no_ksel+8    ;disable jeff's vlm edit mode
        bra.s   jeffoh
jeffgo:
        bclr.b  #7,no_ksel+8    ;disable jeff's vlm edit mode
jeffoh: 
;***end addition
;
        moveq   #0,d1           ;assume audio mode
        cmpi.w  #3,d0           ;did we just enter upper panel mode?
        beq.s   setvlmm
        tst.w   audvlm
        beq.s   setvlmm
        moveq   #1,d1
setvlmm:
        move.w  vlm_mode,d3     ;before we change vlm_mode
        cmpi.w  #2,d3           ;check to see if one of Jeff's special
        bcc.s   setvlmx         ;br if Jeff's edit mode--skip vlm_mode change
        move.w  d1,vlm_mode
setvlmx:
;
        move.w  #-1,CLUT+2
;
;
        add.w   d0,d0
        beq     samode          ;br if so--we need no objects
;
        clr.w   vlmtim          ;turn off vlm logo if present
;
        subq.w  #2,d0

        bne.s   skiphelp
        bsr     onhelp          ;bring up the joypad help screen
        bra     turnon
skiphelp:
        move.w  horztab(pc,d0.w),d1
        move.w  vertab(pc,d0.w),d2
        tst.w   pal
        beq.s   .nopadj
;
        move.w  #$1f0,davesobj+$c4      ;adjust vlmlogo for PAL

        move.w  verptab(pc,d0.w),d2     ;fetch from pal
.nopadj:
        move.w  rmwtab(pc,d0.w),d3
        move.w  palindx(pc,d0.w),d4
        bra     ovtabz
;
vertdig equ     22
;
frntbase        equ     $4000   ;graphics base addr for front panel     
frnthite        equ     48+15
;
gridbase        equ     $7f00   ;graphics base addr for lower buttons & grid    
;
;
;   graphics for front panel are 256w x 48h
;   @ $4000
;
;   graphics for trk-grid & select boxes is 256w x 96h
;   @ $7000
;
;
;
;
;  Set-up objects for our new mode (1..3)
;
;  Horz positions
;
horztab:
        dc.w    $32             ;1
        dc.w    $32             ;2
        dc.w    $32             ;3
;
;  Vert positions
;
vertab:
        dc.w    $180            ;1
        dc.w    $180            ;2
        dc.w    $40             ;3
verptab:
        dc.w    $180+$60        ;1
        dc.w    $180+$60        ;2
        dc.w    $40+$10         ;3
;
;  RMW or transparent
;
rmwtab:
        dc.w    1               ;1
        dc.w    0               ;2
        dc.w    1               ;3
;
;  Palette index
;
palindx:
        dc.w    0               ;1
        dc.w    0               ;2
        dc.w    0               ;3
;
;  DWIDTH/IWIDTH
;
dwidtab:
        dc.w    $20             ;1
        dc.w    $20             ;2
        dc.w    $20             ;3
;
;  Height
;
hitab:
        dc.w    $30             ;1
        dc.w    $30             ;2

;       dc.w    $50             ;3
        dc.w    frnthite        ;3
;
;  Pixel depth
;
pixdtab:
        dc.w    3               ;1
        dc.w    3               ;2
        dc.w    3               ;3
;
;  Back to code
;
ovtabz:
        move.w  dwidtab(pc,d0.w),d5
        move.w  hitab(pc,d0.w),d6
        move.w  pixdtab(pc,d0.w),d7
;
        tst.w   cdgmode                 ;are we in cd+g mode?
        beq.s   ovtabzz                 ;br if not
;
;  cd+g mode.. let's do a few adjustments
;
        cmpi.w  #2,d0                   ;in mode 2 ( (mode-1)*2 ) ?
        bne.s   ovtabzz
        addq.w  #8,d2                   ;lower mode 2 digits
        moveq   #1,d3                   ;no rmw
        tst.w   pal
        beq.s   ovtabzz
        sub.w   #$28,d2
ovtabzz:
;
        move.w  d1,(a0)                 ;horz position, obj #0
        move.w  d1,$40(a0)              ;obj #1
;
        move.w  d2,$4(a0)               ;vert position, obj #0
        move.w  d2,$44(a0)              ;obj #1
;
        move.w  d3,$14(a0)              ;rmw or transparent, obj #0
        move.w  d3,$54(a0)              ;obj #1
;
        move.w  d4,$16(a0)              ;palette index, obj #0
        move.w  d4,$56(a0)              ;obj #1
;
        move.w  d5,$18(a0)              ;DWIDTH
        move.w  d5,$58(a0)              ;DWIDTH
;
        cmpi.w  #4,d0                   ;in mode 3?
        bne.s   .nm3
        tst.w   m3opt
        beq.s   .nm3
        subi.w  #15,d6
.nm3:
        move.w  d6,$1A(a0)              ;height
        move.w  d6,$5A(a0)              ;height
        move.w  d7,$1C(a0)              ;pixel depth
        move.w  d7,$5C(a0)              ;pixel depth
;
        move.l  #frntbase,$50(a0)       ;this one might get munged
;
;   Now blit init graphics into display buffer
;
        bsr     screnclr                ;clear the screen
;
;
        move.w  #$e0,d0                 ;get rmw colors
        move.w  #$a47f,d4               ;foreground
;
        tst.w   cdgmode                 ;are we in cd+g mode?
        beq.s   colzok                  ;br if not
        move.w  #0,d0
        move.w  #$30,d4
colzok:
        move.w  d0,CLUT+8               ;these are for RMW digits only
        move.w  d4,CLUT+2
;
        move.w  our_mode,d0
        cmpi.w  #2,d0
        beq     setcolin                ;br if RMW digits only
;
;
;
bigfront:
;
;  Mode 1 here--put up the front panel
;
;       clr.w   vlmtim          ;force off any lingering VLM logo
;
        bsr     frontup         ;blit up the front panel on a cleen screen
;
;       move.w  seqmode,d0      ;are we in prog/rand seq modes?
;       cmpi.w  #1,d0           ;br if in normal mode
;       beq.s   seqnoevl
;
        bsr     progup          ;put up seq grid
;
        moveq   #3,d0
        cmp.w   our_mode,d0     ;
        bne.s   setcolin
;
;       move.w  #-1,blinkon
;       bsr     ringit          ;put up select box if mode 3
;
        moveq   #1,d0
        moveq   #-1,d1
        bsr     radiob
;
        moveq   #4+1,d0
        bsr     radiob
;
        move.w  vlmrand,-(sp)
        clr.w   vlmrand         ;we need this clear for 1st draw
        moveq   #8,d0
        bsr     radiob
        move.w  (sp)+,vlmrand   ;restore vlm
;
        moveq   #0,d1
        move.w  seqmode,d0
        bsr     radiob          ;put up sequence mode
;
        tst.w   seqmode         ;check for program entry
        bne.s   .nproge

        moveq   #3,d0
        cmp.w   m3row,d0        ;
        bhi.s   .nproge
        move.w  #-1,progenty
.nproge:
        tst.w   m3opt
        beq.s   .nm3
;       
        move.w  #-1,blinkon
        bsr     ringit          ;put up select box if mode 3
.nm3:
;
seqnoevl:
        moveq   #0,d1
;
        move.w  reptmode,d0
        addq.w  #4,d0
        bsr     radiob          ;put up repeat mode
;
        move.w  cdgmode,d0
        addq.w  #8,d0
        bsr     radiob          ;put up vlm/cd+g mode
;
;  arrive here if modes 1 or 2 or 3
;
setcolin:
        bsr     colons          ;put up the min:sec colon
        bsr     bankneg         ;put up dash between bank-effect
        bsr     bankout         ;display bank-effect numbers if mode=2
;
        tst.w   play            
        bne.s   ovstop  
;
        bsr     stopout         ;display stop digits
ovstop:
;
;   And turn objects on
;
turnon:
        lea     davesobj,a0
        moveq   #6,d1
        move.w  our_mode,d0     ;check help mode
        cmpi.w  #1,d0
        beq.s   justhel
;       
        move.w  d1,$c(a0)       ;modes 2 & 3 require object #0
;
        cmpi.w  #2,d0           ;2 objects?
        bne.s   just1           ;br if not
justhel:
        move.w  d1,$4c(a0)              ;else, better turn on 2nd
just1:  
        moveq   #-1,d0
        move.b  d0,1(a1)        
        move.b  d0,3(a1)        
        move.b  d0,4(a1)        
        move.b  d0,7(a1)        
        move.b  d0,8(a1)        
;
        bsr     chngdisp
        bra     retvlm                  ;we've done enough, go back to Jeff     
;
;
;
;   We're continuing in same mode as before...
;
samode:
        lea     $2810,a5                ;copy q-subcodes
        lea     $f1c010,a0
        move.l  (a0)+,(a5)+
        move.l  (a0)+,(a5)+
        move.l  (a0)+,(a5)+
;
;  indicate bank/effect if necessary
;
        move.w  imatrix,d0
        cmp.w   locbank,d0
        bne.s   baefx
        move.w  skid,d0
        cmp.w   loceff,d0
        beq.s   nobaefx
;
;  bank or effect has changed--put it up..
;
baefx:
        move.w  imatrix,d0
        move.w  d0,locbank
        move.w  d0,disbank
        move.w  skid,d0
        move.w  d0,loceff
        move.w  d0,diseff
        bsr     bankout
;
;  tell EEprom too
;
        tst.w   vlmrand         ;better not EE save if in random mode
        bne.s   nobaefx         ;since we change so much
;
        move.w  imatrix,d0
        lsl.w   #4,d0
        or.w    skid,d0
        move.w  d0,Sbankeff
        move.w  #(Sbankeff-workEE),d1
        bsr     setEE
;
nobaefx:
;
;
        tst.w   play                    ;when in play mode
        beq.s   nochkend                ;we need to check for off the end stuff
;
        lea     fineEND+1,a5
        lea     7(a1),a6
        move.b  (a6)+,d0                ;are we at max minutes?
        cmp.b   (a5)+,d0
        bne.s   nochkend
        move.b  (a6)+,d0                ;are we at max secs?
        cmp.b   (a5)+,d0
        bne.s   nochkend
;
;  we've reached end of disc--check for repeat option
;
        move.w  reptmode,d0     ;check repet mode (0-rept trk, 1-no, 2-rept dsc)
        beq     starplay        ;br if repeat last track
;
;  we're at end of last physical track, check next sequence
;
        move.w  trksptr,d5
        addq.w  #1,d5
        lea     trkseq,a0
        moveq   #0,d6
        move.b  (a0,d5.w),d6    ;get next track in sequence
        bne.s   moseq           ;br if more tracks in sequence
;
;  reached end of sequence and end of disc
;
        moveq   #0,d5
        move.w  d5,trksptr
        move.b  (a0),d6         ;get repeat track # if needed
        cmpi.w  #2,d0           ;repeat disc mode?
        bne     dostop          ;if not, just stop
;
;
;  More tracks in sequence, but we must seek to them
;
moseq:
        move.w  d5,trksptr      ;save new sequence ptr
        move.w  d6,track
        bra     starplay        
;
;
nochkend:
;
;
        tst.b   retcode+1               ;we can't display if waiting for found
        bmi     checkjoy
;
;   check display update
;
;
        lea     DSPqcode+$10,a5         ;here's the DSP polled Q-subcodes
        moveq   #4,d2                   ;we just want to check 5
        moveq   #1,d1
        cmp.w   scan,d1                 ;are we in active scan?
        bcc     subupd                  ;br if not
;
;  in scan mode, we use the seek pointer
;
        lea     hexbcd,a5
;
        moveq   #0,d3
        move.l  scantime,d2     ;scan mode seek pointer
;
        moveq   #0,d4
        lsr.l   #8,d2           ;d2.b = Asecs
        move.b  d2,d4
        move.b  (a5,d4.w),d4
        cmp.b   8(a1),d4        ;have Aseconds changed?
        beq.s   noAsec          ;br if no change
;
        move.b  d4,8(a1)        ;update Aseconds
        addq.l  #1,d3           ;Asec update indicator
noAsec:
        lsr.w   #8,d2           ;d2.b = Amins
        move.b  d2,d4
        move.b  (a5,d4.w),d4
        cmp.b   7(a1),d4        ;have Amins changed?
        beq.s   noAmin          ;br if no change
;
        move.b  d4,7(a1)        
        addq.l  #2,d3
noAmin:
        tst.w   d3
        bne     doAchg          ;do change if one found
        tst.w   scanrel         ;do we need to process scan relative display
        beq     checkjoy        ;br if not
;
        clr.w   scanrel         ;tell 'em we're updating
;
;  compute relative scan
;
        lea     realTOC,a5
        move.l  scantime,d2
        moveq   #-1,d5          ;indicate if track has changed
        move.l  #$ffffff,d6
;
        moveq   #0,d0
        move.w  track,d0        ;get track #
;       
        lsl.w   #3,d0
        adda.w  d0,a5

retrack:
        move.l  (a5),d3         ;base addr of current track
        addq.l  #1,d5           ;d5=0 (track not changed)
        and.l   d6,d3
        cmp.l   d3,d2           ;reality check on track # 
        bcs.s   trkhi           ;br if track # too hi
;
;  see if we have passed into next track
;
        move.l  8(a5),d4        ;check for next track
        and.l   d6,d4
        beq.s   trkgood         ;br if we're in last track
;
        cmp.l   d4,d2
        bcs.s   trkgood         ;br if our track is ok
;
;  track # is too low
;
        addq.l  #8,a5
        addq.w  #1,track
        bra     retrack
trkhi:
        subq.l  #8,a5
        subq.w  #1,track
        beq     dostop  
        bra     retrack
;
trkgood:
        tst.w   d5              ;did we get a track update?
        beq.s   trksame         ;br if not
        move.w  track,d0
        bsr     hex2bcd
        move.b  d0,SubCode+1    ;save new track
        moveq   #1,d1           ;set offset for track
        movem.l d2-d4,-(sp)
        bsr     printout
        movem.l (sp)+,d2-d4
;
;   d2 = current spot
;   d3 = base of current track
;   d4 = base of current track +1
;
trksame:
        lsr.l   #8,d2
        lsr.l   #8,d3
;
        sub.b   d3,d2
        bcc.s   trksec
        add.b   #60,d2
        ori     #$10,ccr                ;set x
trksec:
        ror.w   #8,d2
        ror.w   #8,d3                   ;d3 = Sec Min
        subx.b  d3,d2
        lea     hexbcd,a5
        moveq   #0,d0
        move.b  d2,d0
        move.b  (a5,d0.w),3(a1)         ;BCD relative minutes
        lsr.w   #8,d2
        move.b  d2,d0
        move.b  (a5,d0.w),4(a1)         ;BCD relative seconds
        moveq   #1,d2
subupdz:
        moveq   #0,d1
        moveq   #0,d0
        move.b  suboffs+1(pc,d2.w),d1   ;get next subcode offset
        move.b  (a1,d1.w),d0            ;get new data for display

        movem.l a5/d2,-(sp)
        bsr     printout
;       bsr     prout
        movem.l (sp)+,a5/d2
nosubdz:
        dbra    d2,subupdz
        bra     checkjoy
;
;
;   Do Abs min/sec change here
;
doAchg:
        moveq   #1,d2
        move.w  d2,scanrel      ;set for rel change next time thru
subupdx:
        moveq   #0,d1
        moveq   #0,d0
        lsr.w   #1,d3
        bcc.s   nosubdx
        move.b  suboffs+3(pc,d2.w),d1   ;get next subcode offset
        move.b  (a1,d1.w),d0            ;get new data for display

        movem.l a5/d2-d3,-(sp)
        bsr     printout
;       bsr     prout
        movem.l (sp)+,a5/d2-d3
nosubdx:
        dbra    d2,subupdx
        bra     checkjoy
;
suboffs:
        dc.b    1,3,4,7,8,9
;
;  During normal play, we need to filter q_subcodes
;
;
subupd:
        tst.w   pause           ;check pause mode
        beq.s   subupd0         ;br if not in pause mode
        tst.b   1(a1)           ;do we need a single update?
        bmi.s   subupd0         ;br if so
        bra     checkjoy
subupd0:
;
        bset.b  #0,cdgflags     ;normal run mode, make sure cdg is updated
        moveq   #1,d4           ;we need for abcd
        moveq   #0,d2           ;flags for redraw
;
;   Check out Asec
;
        moveq   #0,d0
        move.b  8(a5),d0        ;get proposed Asec from DSP
        move.b  8(a1),d3        ;get previously displayed Asec
        bmi.s   Asecdo
        cmp.b   d3,d0           ;need to update display?
        beq.s   noAsecd         ;br if not
;
Asecdo:
        addq.l  #1,d2           ;set Asec needs display flag 
        move.b  d0,8(a1)        ;save new
noAsecd:
;
;    Now check out Amin
;
        moveq   #0,d0
        move.b  7(a5),d0        ;get proposed Amin from DSP
        move.b  7(a1),d3        ;get previously displayed Amin
        bmi.s   Amindo
        cmp.b   d3,d0           ;need to update display?
        beq.s   noAmind         ;br if not
;
Amindo:
        addq.l  #2,d2           ;set Amin flag
        move.b  d0,7(a1)        ;save new
noAmind:
;
;   Next, do Rsec (relative seconds)
;
        moveq   #0,d0
        move.b  4(a5),d0        ;get proposed Rsec from DSP
        tst.b   2(a5)
        bne.s   negadj
        addq.b  #1,d0
negadj:
        move.b  4(a1),d3        ;get previously displayed Rsec
        bmi.s   Rsecdo
        cmp.b   d3,d0           ;need to update display?
        beq.s   noRsecd         ;br if not
;
Rsecdo:
        addq.l  #4,d2           ;set Rsec needs display flag 
        move.b  d0,4(a1)        ;save new
noRsecd:
;
;    Now check out Rmin
;
        moveq   #0,d0
        move.b  3(a5),d0        ;get proposed Rmin from DSP
        move.b  3(a1),d3        ;get previously displayed Rmin
        bmi.s   Rmindo
        cmp.b   d3,d0           ;need to update display?
        beq.s   noRmind         ;br if not
;
Rmindo:
        addq.l  #8,d2           ;set Rmin flag
        move.b  d0,3(a1)        ;save new
noRmind:
;
;    Now check out Track
;
        moveq   #0,d0
        move.b  1(a5),d0        ;get proposed Track from DSP
        move.b  1(a1),d3        ;get previously displayed Track
        cmpi.b  #-1,d3          ;check for minus 1
        beq     Tdo             ; always display if -1
        cmp.b   d3,d0           ;need to update display?
        beq     noTdo           ;br if not
;
;  track change has occured during play
;
;***********test
;       move.w  #$1501,$dfff0a
;***********end test
;
;
        move.w  reptmode,d4     ;check repet mode (0-rept trk, 1-no, 2-rept dsc)
        bne.s   ntrkrep         ;br if not repeat track
;
;  we want to repeat track just completed
;
        moveq   #0,d0
        move.b  d3,d0
        bsr     bcd2hex
        move.w  d0,track
        bra     starplay        ;go repeat the track
;
;  check next track in sequence
;
ntrkrep:
        move.w  trksptr,d5
        addq.w  #1,d5
        lea     trkseq,a0
        move.b  (a0,d5.w),d6    ;get next track in sequence
        beq.s   endseq          ;br if no more tracks in sequence
;
        move.w  d5,trksptr      ;save new sequence ptr
        movem.l d0-d1,-(sp)
        bsr     bcd2hex         ;change new track bcd to hex
        move.w  d0,d7           ;copy to d7
        movem.l (sp)+,d0-d1
        cmp.b   d6,d7           ;is sequence same as new track
        bne.s   dscrep          ;br if not
;
        movem.l d0-d2/a5,-(sp)
        bsr     uprantrk
        tst.w   m3opt
        beq.s   .skipr
        move.w  #-1,blinkon
        bsr     ringit
.skipr:
        movem.l (sp)+,d0-d2/a5
;
        bra.s   Tdo             ;br if so--just roll over to next track
;
;  we have a custom sequence here, better seek to next sequence track
;
dscrep:
        ext.w   d6
        move.w  d6,track
        bra     starplay        
;
;  reached end of sequence
;
endseq:         
        clr.w   trksptr         ;set for start of sequence next time
        cmpi.w  #2,d4           ;repeat disc mode?
        bne     dostop          ;br if not--just stop
;
        move.b  (a0),d6         ;get repeat track # if needed
        cmp.w   seqmode,d4      ;sequence mode = 2 (random)?
        bne     dscrep          ;br if not random to repeat disc
;
        move.l  a0,-(sp)
        bsr     randf           ;re-randomize track sequence before repeating
        moveq   #3,d0
        cmp.w   our_mode,d0     ;we'll need display update if in mode=3
        bne.s   .nrandup
        clr.w   gridoff         ;29-Mar-95
        bsr     progup          ;update display if necessary
.nrandup:
        move.l  (sp)+,a0
        clr.w   trksptr         ;set for start of sequence next time
        move.b  (a0),d6         ;get repeat track # if needed
        bra     dscrep
;
Tdo:
        bset.l  #4,d2           ;set Track flag
        move.b  d0,1(a1)        ;save new Track
        bsr     bcd2hex
        move.w  d0,track
;
        move.b  4(a5),4(a1)
        move.b  3(a5),3(a1)
        ori.w   #$c,d2
noTdo:
;
;    Now check out Sign bit
;
        moveq   #0,d0
        move.b  2(a5),d0        ;get proposed Index from DSP
        move.b  2(a1),d3        ;get previously displayed Sign
        bmi.s   Sbitdo
        cmp.b   d3,d0           ;need to update display?
        beq.s   noSbit          ;br if not
;
Sbitdo:
        bset.l  #5,d2           ;set Sign-bit flag
        move.b  d0,2(a1)        ;save new
        bra.s   noSbit
;
;
suboffz:
        dc.b    1,3,4,7,8,9
;
;
;
noSbit:
;
        moveq   #4,d3
subqlp:
        lsr.w   #1,d2
        bcc.s   nosubd
        moveq   #0,d1
        moveq   #0,d0
        move.b  suboffz(pc,d3.w),d1     ;get next subcode offset
        move.b  (a1,d1.w),d0
        movem.l d2-d3,-(sp)
        bsr     printout
        movem.l (sp)+,d2-d3
nosubd:
        dbra    d3,subqlp
        tst.w   d2                      ;only sign bit is left
        beq     checkjoy                ;br if no change
;
;  set or clear sign-bit
;
        lea     A1_BASE,a0
        move.w  #206,d1                 ;dst
        swap    d1
        move.w  #112,d1                 ;src
;
        tst.b   2(a1)
        beq     .blwait
;
        move.w  #$64,d1                 ;use blank for "positive"
;
.blwait:
        move.l  B_CMD-A1_BASE(a0),d2    ;wait til blitter free
        lsr.w   #1,d2
        bcc     .blwait

        move.l  #$60000,A2_BASE-A1_BASE(a0)     ;set SRC base
        move.l  #frntbase,(a0)                  ;set DST base

        move.l  #PITCH1|WID128|XADDPIX|PIXEL8,A2_FLAGS-A1_BASE(a0)      ;src
        move.l  #PITCH1|WID256|XADDPIX|PIXEL8,A1_FLAGS-A1_BASE(a0)      ;dst
;
        moveq   #0,d2           ;ypos
        move.w  d1,d2           ;src y=0, x 
        move.l  d2,A2_PIXEL-A1_BASE(a0)
;
        moveq   #vertdig,d2             ;ypos
        swap    d2
        swap    d1
        move.w  d1,d2           ;dst y=0, x 
        move.l  d2,A1_PIXEL-A1_BASE(a0)
;
        move.l  #(17*$10000)+6,B_COUNT-A1_BASE(a0)      ;w:6, h:17
        move.l  #(2*$10000)-6,d2
        move.l  d2,A1_STEP-A1_BASE(a0)
        move.l  d2,A2_STEP-A1_BASE(a0)
        move.l  #UPDA2|UPDA1|SRCEN|LFU_S|DSTEN,B_CMD-A1_BASE(a0)
;
;       bra     checkjoy
;
;
;
;
;   New joystick bit assignments (we now follow Jeff's)
;
;       pad_now (longword):
;
;        3         2         1         0
;       10987654321098765432109876543210
;       xxAPxxBxRLDU147*xxCxxxOx2580369#
;
;
;
Abit    equ     29      ;fireA bit
Pbit    equ     28      ;pause bit
Ubit    equ     20      ;   Up bit
Dbit    equ     21      ; Down bit
Lbit    equ     22      ; Left bit
Rbit    equ     23      ; Rite bit
Bbit    equ     25      ;fireB bit
Cbit    equ     13      ;fireC bit
Obit    equ     9       ;Option bit
;
checkjoy:
        bsr     m3trans                 ;do translation or mode3
;
        move.l  dave_pad,d3             ;get current joystick
;
;       tst.w   our_mode                ;are we in Jeff only mode?
;       bne.s   noignor
;                  3         2         1         0
;                 10987654321098765432109876543210
;                 xxAPxxBxRLDU147*xxCxxxOx2580369#
;       andi.l  #%00110000000000000000001000000000,d3   ;only want these
;noignor:
        move.l  joyprev,d1              ;and previous one too
        move.l  d3,joyprev              ;save new previous
        move.l  d3,d0
        eor.l   d1,d0                   ;check for edges
;
;
;                  3         2         1         0
;                 10987654321098765432109876543210
;                 xxAPxxBxRLDU147*xxCxxxOx2580369#
        andi.l  #%00110010111100000010001000000000,d0   ;only want these
        bne     butedge
;
;  No transitions..
;
checkvol:
        move.w  volfcnt,d0      ;are we in VOL raise/lower auto repeat?
        beq.s   novauto         ;br if not
        sub.w   frames,d0       ; else, time to move up/down?
        bge.s   novauto         ; br if not yet
;
;  time to raise/lower volume
;
        move.w  frames,d0       ;
        addi.w  #5,d0           ;next adjust in 5 frames please
        bne.s   yeavauto
        addq.w  #1,d0           ;make it 6 if volfcnt would be 0 otherwise
yeavauto:
        move.w  d0,volfcnt
        movem.l d1-d2,-(sp)
        move.l  d3,d2
        bsr     volupdwn        ;this routine looks at d2.l for Ubit/Dbit
        movem.l (sp)+,d1-d2     
;
;  done with raise/lower volume
;
novauto:
        move.w  scan,d0         ;are we in wait for scan mode?
        beq     nojoy           ;br if not
stilldwn:
;
;  some type of scan stuff here..
;
        cmpi.w  #1,d0           ;waiting for min hold?
        bne     scammin         ;br if we're actively scanning
;
;  waiting for min hold time on scan
;
        move.w  frames,d0
        sub.w   scanfcnt,d0
        cmpi.w  #40,d0          ;wait 2/3 sec for scan
        bcs     nojoy
;
;  Reached minimum time--start up a scan
;
        bclr.b  #0,cdgflags
;
        moveq   #2,d0           ;assume forward 
        btst.l  #Lbit,d3        ;right (forward) scan?
        bne.s   setscan
        moveq   #3,d0           ;assume backward
        btst.l  #Rbit,d3        ;left (backward) scan?
        bne     setscan
;
;  hey--what happened to our scan!!!
;
        clr.w   scan
        bra     nojoy
;       illegal                 ;shouldn't get here
;
;  start up a scan
;
setscan:
        move.w  d0,d3           ;save fore/back scan indicator
;
        move.b  DSPqcode+$17,d0
;       move.b  7(a1),d0        ;fetch abs Mins (BCD)
        bsr     bcd2hex         ;convert to hex
        bne     nojoy
        moveq   #0,d2
        move.b  d0,d2           ;save in d2
        move.b  DSPqcode+$18,d0
;       move.b  8(a1),d0        ;now do Asec
        bsr     bcd2hex
        bne     nojoy
        lsl.l   #8,d2
        move.b  d0,d2           ;save secs
        move.b  DSPqcode+$19,d0
;       move.b  9(a1),d0
        bsr     bcd2hex
        bne     nojoy
        lsl.l   #8,d2
        move.b  d0,d2
        move.l  d2,scantime
        move.w  d3,scan
        move.w  frames,scanfcnt
        clr.w   scancnt         ;count of goto time commands issued
;
;  do our scan thing
;
scammin:
        move.w  frames,d0
        cmp.w   scanfcnt,d0     ;is it time for next goto time command?
        bcs     nojoy           ;br if not
;
        tst.b   retcode+1       ;are we still waiting for "found"?
        bne     nojoy           ;br if so
;
; compute next goto time
;
        moveq   #0,d3
        move.w  rate,d3         ;vid_frames/tick
        add.l   d3,d0
        move.w  d0,scanfcnt
        subq.l  #2,d3
        lsl.w   #2,d3
        move.l  slowrate(pc,d3.w),d1
        move.w  scancnt,d0
        cmpi.w  #30,d0
        bcs.s   audfok
        move.l  fastrate(pc,d3.w),d1
        bra.s   audfok
slowrate:
        dc.l    $019    ;2
        dc.l    $026    ;3
        dc.l    $032    ;4
        dc.l    $03F    ;5
        dc.l    $100    ;6
        dc.l    $10D    ;7
        dc.l    $119    ;8
fastrate:
        dc.l    $200    ;2
        dc.l    $300    ;3
        dc.l    $400    ;4
        dc.l    $500    ;5
        dc.l    $600    ;6
        dc.l    $700    ;7
        dc.l    $800    ;8
rate:
        dc.w    6               ;2..8
audfok:

        move.l  scantime,d3
;
;  d1 = increment
;  d3 = last goto time point
;
        move.w  scan,d0         ;advance forward or reverse?
        lsr.w   #1,d0
        bcc.s   scanundr        ;br if reverse
;
;  else, compute forward scan
;
        add.b   d1,d3                   ;advance our time by frame increment
        cmp.b   #75,d3                  ;d3= 00 Am As Af
        bcs.s   frok
        sub.b   #75,d3  
        ori     #$10,ccr                ;set x
frok:
        ror.l   #8,d1
        ror.l   #8,d3                   ;d3 = Af 00 Am As
        addx.b  d1,d3
        cmp.b   #60,d3
        bcs.s   secok
        sub.b   #60,d3
        ori     #$10,ccr                ;set x
secok:
        ror.l   #8,d1
        ror.l   #8,d3                   ;d3 = As Af 00 Am
        addx.b  d1,d3
        ror.l   #8,d3                   ;d3 = Am As Af 00
        cmp.l   origEND,d3              ;check for exceeding end `o time
        bcs.s   dopoint                 ;now advance to our setpoint
        bra     outbndz                 ;br to out of bounds
;
;  compute reverse scanpoint
;
scanundr:
        sub.b   d1,d3                   ;backup our time by frame decrement
        bcc.s   frokx                   ;frames ok
        add.b   #75,d3
        ori     #$10,ccr                ;set x
frokx:
        ror.l   #8,d1
        ror.l   #8,d3                   ;d3 = Af 00 Am As
        subx.b  d1,d3
        bcc.s   secokx
        add.b   #60,d3
        ori     #$10,ccr                ;set x
secokx:
        ror.l   #8,d1
        ror.l   #8,d3                   ;d3 = As Af 00 Am
        subx.b  d1,d3
        ror.l   #8,d3                   ;d3 = Am As Af 00
        moveq   #0,d4
        negx.w  d4
        beq.s   dopoint
;
;  a fast forward/backward request puts us out-of-bounds...
;
outbndz:
        tst.w   play                    ;are we in play mode?
        bne     dostop                  ;stop if so
        bra     nojoy                   ;else, ignore further advancement
;
;   d3.l = Am As Af 00
;
dopoint:
        move.l  #$1000,d4
        moveq   #2,d6
        bra.s   ovwaits
gotolp:
.waitset:
        move.l  (a4),d5
        btst.l  #12,d5                  ;wait for DSA tx to be set
        beq     .waitset
        move.w  DS_DATA(a4),d5
        tst.l   DSCNTRL(a4)
ovwaits:
        rol.l   #8,d3
        move.b  d3,d4
;
        move.w  d4,DS_DATA(a4)          ;send min goto time command word
;
        addi.w  #$100,d4
        dbra    d6,gotolp
;
;
        moveq   #1,d1
        move.b  d1,retcode+1            ;we are waiting for found
;
        move.l  d3,scantime
        addq.w  #1,scancnt              ;advance count of scans
        bra     nojoy
;
;
;  Button edge transition here...
;
;  d3.l = pad_now (joycur)
;  d1.l = joyprev
;  d0.l = edges
;
butedge:
        move.l  d0,d2
        and.l   d3,d2           ;we're only interested in positive edges here
;                  3         2         1         0
;                 10987654321098765432109876543210
;                 xxAPxxBxRLDU147*xxCxxxOx2580369#
        andi.l  #%00110010000000000010001000000000,d2   ;only want these
;
        tst.w   cdgmode
        bne.s   nOption
;
        btst.l  #Obit,d2        ;positive edge on Option?
        beq.s   nOption         ;br if not
;
        moveq   #1,d6
        not.w   audvlm
        bne.s   stuffvlm
        moveq   #0,d6
stuffvlm:
        move.w  d6,vlm_mode
;
        bsr     fliplogo
;
        move.w  #7,beasties+(7*$40)+$C  ;put up new for a few secs
        move.w  #$40,vlmtim
;
;
nOption:
        btst.l  #Abit,d2        ;positive edge on FireA?
        beq.s   nfireAtr        ;go start something
;
        move.w  modecnt,d6
        addq.w  #1,d6
;
;       .if     0
        cmpi.w  #3,d6           ;this logic skips the ugly help screen
        bcs.s   .shortwr
        moveq   #0,d6
.shortwr:
;       .endif
;       andi.w  #3,d6

        move.w  d6,modecnt
        move.b  rmseq(pc,d6.w),d6
        bne.s   nfAtrx          ;br if not the VLM-only mode
        bra.s   ovrmseq
rmseq:
        dc.b    3,2,0,1         ;richard miller's sequence order
ovrmseq:
;
;  "VLM-only" mode--actually we could be in CD+G mode
;
        tst.w   cdgmode         ;are we in cd+g?
        bne.s   nfAtrx          ;br if so--skip vlm prep stuff
;
;  Just entered VLM only mode..indicate whether "audio" or "vlm"
;
;       tst.w   vlm_mode        ;else, we might need
;       bne.s   nfAtrx          ;to let
;       move.w  #1,vlm_mode     ; Jeff field some joypad here 
;
;  Put up logo for ~1 sec
;
        move.w  #7,beasties+(7*$40)+$C
        move.w  #$40,vlmtim
nfAtrx:
        ori.w   #$8000,d6
        move.w  d6,our_mode
nfireAtr:
        btst.l  #Bbit,d2        ;positive edge on FireB?
        bne     fireBtr         ;go start something
;
        btst.l  #Cbit,d2        ;how about fireC?
        bne     fireCtr
;
        btst.l  #Pbit,d2        ;any pause leading edge?
        beq     nofireA         ;br if not to check other transitions
;
;  leading edge of Pause
;
        tst.w   play            ;are we in play mode?
        beq     nofireA         ;br if not
        tst.w   pause           ;in pause mode?
        beq     setpause
;
;  release pause
;
relpause:
        bset.b  #0,cdgflags
        moveq   #4,d0
        moveq   #0,d1
        bsr     depressd
;
        clr.w   pause
        move.w  #$500,d0        ;pause release
        bra.s   paws
setpause:
        bclr.b  #0,cdgflags
        moveq   #4,d0
        moveq   #1,d1
        bsr     depressd
;
        move.w  #-1,pause
        move.w  #$400,d0
paws:
        moveq   #0,d1                   ;# of return words
        bsr     DSA_tx
;
        bra     nojoy
;       
;
;  positive push on fireB
;
fireBtr:
        clr.w   scan            ;exit scan if needed
        tst.w   play            ;are we already playing?
        beq.s   fireBx          ;br if not--we can enter now
;
        tst.w   pause           ;check for pause mode
        beq     nojoy           ;if not in pause, we can ignore
        bra     relpause        ; else, just release pause
;
fireBx:
        move.w  #-1,play        ;set
        bra     starplay
;
;
fireCtr:
        moveq   #0,d0
        moveq   #1,d1
        bsr     depressd                ;stop button pushed
;
        clr.w   scan
        tst.w   play            ;are we already stopped?
        beq     nojoy           ;if so--we're done
;
;  stop a play
;
dostopx:
        tst.w   cancelC         ;is there a cancelC in effect
        beq.s   dostop          ;br if not
        clr.w   cancelC         ;only do it once
        bra     nojoy
dostop:
        clr.w   cancelC         ;only do it once
        clr.w   play
        clr.w   track
        clr.w   trksptr
;
        bsr     uprantrk
        tst.w   m3opt
        beq.s   .rering
        move.w  #-1,blinkon
        bsr     ringit
.rering:
;
        tst.w   pause
        beq.s   unpawzd
;
        moveq   #4,d0
        moveq   #0,d1
        bsr     depressd
;
        clr.w   pause
unpawzd:
;
        moveq   #2,d0
        moveq   #0,d1
        bsr     depressd                ;play button raised
;
        bsr     stopout
;
        move.w  #$200,d0                ;
        moveq   #0,d1                   ;# of return words
        bra     exitplay
;
;  start a play
;
starplay:
        bsr     uprantrk

        tst.w   m3opt
        beq.s   .skipr
        move.w  #-1,blinkon
        bsr     ringit
.skipr:

;
        moveq   #0,d0
        moveq   #0,d1
        bsr     depressd                ;reset button raised
;
        moveq   #2,d0
        moveq   #1,d1
        bsr     depressd                ;play button depressed
;
        move.w  trksptr,d0
        bpl.s   .starts
        moveq   #0,d0
        move.w  d0,trksptr
.starts:
        lea     trkseq,a0
        move.b  (a0,d0.w),d0
;
;       move.w  track,d0
;       bmi.s   set1
;       bne.s   set0    
;set1:
;       move.w  #1,d0
;
        move.w  d0,track
set0:
        bsr     string          ;start of play digit display
;
        move.w  track,d0
        ori.w   #$100,d0
        moveq   #0,d1
exitplay:
        bsr     DSA_tx
        moveq   #-1,d0
        move.b  d0,retcode+1            ;we are waiting for found
        move.b  d0,1(a1)        
        move.b  d0,3(a1)        
        move.b  d0,4(a1)        
        move.b  d0,7(a1)        
        move.b  d0,8(a1)
        bclr.b  #0,cdgflags     
        bra     nojoy
;
;
;   Button transition, but not fireB or Pause
;
;  d3.l = pad_now (joycur)
;  d1.l = joyprev
;  d0.l = edges
;
;
;   Not fireA, fireB, or fireC or pause, check track inc/dec
;
nofireA:
        move.l  d0,d2
;                  3         2         1         0
;                 10987654321098765432109876543210
;                 xxAPxxBxRLDU147*xxCxxxOx2580369#
        andi.l  #%00000000000000000010001000000000,d2   ;check release of C
        and.l   d1,d2
        beq.s   hangfire
;
        btst.l  #Cbit,d2
        bne.s   Crels                   ;br if release of C
;
;  must be release of Option key
;
;       bsr     offhelp
;       bsr     chngdisp
;
        bra.s   hangfire
;
;  Release of C 
;
Crels:
        moveq   #0,d0                   ;release "C" (stop) button
        moveq   #0,d1
        bsr     depressd
        bra     nojoy   
hangfire:
        move.l  d0,d2
        
;
;                  3         2         1         0
;                 10987654321098765432109876543210
;                 xxAPxxBxRLDU147*xxCxxxOx2580369#
        andi.l  #%00000000111100000000000000000000,d2   ;only want LRUD
        and.l   d3,d2
        bne     scanbeg                         ;check br if leading edges
;
        move.l  d0,d2
;
;                  3         2         1         0
;                 10987654321098765432109876543210
;                 xxAPxxBxRLDU147*xxCxxxOx2580369#
        andi.l  #%00000000111100000000000000000000,d2   ;only want LRUD
        and.l   d1,d2                   ;check trailing edge
        beq     nojoy                   ;exit if not LRUD
;
        btst.l  #Dbit,d2                ;check for release of Up/Down
        bne.s   offvol                  ;we got a release
        btst.l  #Ubit,d2
        beq.s   skipvol                 ;no release detected
;
;  Up/Down was released...set timer for 3 seconds til display shut-down
;
offvol:
        move.w  frames,d0
        addi.w  #180,d0
        bne.s   timeok
        addq.w  #1,d0
timeok:
        move.w  d0,voltimr
        clr.w   volfcnt         ;turn off possible repeat raise/lower
skipvol:
;
;  R,L trailing edge
;
;   d2 has trailing edge of L or R
;
        moveq   #1,d0
        btst.l  #Lbit,d2        ;check for left release
        bne.s   lrelez  
        btst.l  #Rbit,d2        ;right release
        beq     nojoy
        moveq   #3,d0
lrelez:
        moveq   #0,d1
        move.l  d2,-(sp)
        bsr     depressd
        move.l  (sp)+,d2
;
        move.w  scan,d0
        clr.w   scan            ;clear any scan
        moveq   #2,d1
        cmp.w   d1,d0
        bcs     unscand         ;br if no scan going on--we track up/down
;
;  else--a scan is being terminated, no track up/down
;
        bset.b  #0,cdgflags
;
        bsr     trkptrsm        ;smart track ptr restore: d2 has R L 
        bsr     uprantrk
        bra     nojoy
;
; R, L trailing edge in d2.l without scan--this indicates a seek track up/down
;
unscand:
        .if     0
        moveq   #0,d0
        move.b  1(a1),d0        ;get TRK from subcode

        bsr     bcd2hex
        beq.s   trkng
        move.w  track,d0
trkng:
        move.w  maxmin,d1
        cmp.b   d0,d1
        bcc     trkgoo
        move.w  track,d0
trkgoo: 
        tst.w   d0
        bne.s   trkgoo1
        tst.w   play
        bne.s   play0
        btst.l  #Rbit,d2
        bne.s   trkgoo1
play0:
        moveq   #1,d0
trkgoo1:
        move.w  d0,track
        .endif
;
        lea     trkseq,a0               ;ptr to track sequence
        move.w  trksptr,d0              ;get current track offset in seq
;       bpl.s   .trkpos
;       moveq   #0,d0
;       move.w  d0,trksptr
;.trkpos:
;
        btst.l  #Rbit,d2                ;is it Right (track increment)?
        bne.s   truckup                 ;br if so
;
;  else, left (track decrement)
;
        tst.w   play            ;are we in play mode?
        beq.s   playno          ;br if not
        tst.b   retcode+1
        bne.s   playno
;
;  in play mode...
;
        cmp.b   #1,3(a1)        ;check if close to start of track
;       
        bcc.s   newtrkgd
        cmp.b   #2,4(a1)        ;if within 2 seconds, we back to previous
        bcc.s   newtrkgd
playno:
        subq.w  #1,d0
        bmi     nojoy           ;ignore if track is too low
        bra     newtrkgd
;
;   track increment
;
truckup:
        tst.w   play
        bne.s   trucky          ;skip the following if in play
;
;  not in play mode..
;
        tst.w   track           ;is this the first time we've hit right?
        beq.s   trucku          ;br if so
trucky:
        addq.w  #1,d0
trucku:
        tst.b   (a0,d0.w)       ;are we at the end
        beq     nojoy           ;br if so--can't go this hi
newtrkgd:
        move.w  d0,trksptr
        move.b  (a0,d0.w),d0    ;fetch physical track #
        move.w  d0,track
;
        bsr     string          ;blank out time-code
;
        bclr.b  #0,cdgflags
;
        tst.w   play
        beq     nojoy           ;if not playing, just display new & exit
        tst.w   pause           ;were we paused?
        beq.s   noprel          ;if not, skip pause release
;
        clr.w   pause
        move.w  #$500,d0                ;pause release
        moveq   #1,d1                   ;# of return words
        bsr     DSA_tx
;
noprel:
        move.w  track,d0
        bra     starplay        ;if playing, seek new track
;
;  Leading edge on R or L -- enter waiting for scan mode  
;
scanbeg:
        btst.l  #Ubit,d2        ;check for up/down
        bne.s   updownon        
        btst.l  #Dbit,d2
        beq.s   scanbeg1
;
;   Turn on volume indicator
;
updownon:
        move.w  frames,d0
        addi.w  #25,d0          ;1/2 sec before we start auto raise/lower
        bne.s   rltimok
        addq.w  #1,d0
rltimok:        
        move.w  d0,volfcnt      ;save frame count when auto repeat begins
;
        tst.w   voltimr         ;are we already on?
        beq.s   udset
;
        bsr     volupdwn
udset:
        move.w  #6,davesobj+$8c ;turn on volume object
;
        bsr     chngdisp
;
;
;
scanbeg1:
        moveq   #1,d0
        btst.l  #Lbit,d2
        bne.s   lsetez
        btst.l  #Rbit,d2
        beq     nojoy
        moveq   #3,d0
lsetez:
        moveq   #1,d1
        bsr     depressd
;
        move.w  #1,scan
        move.w  frames,scanfcnt ;save time when wait for hold 1st entered
;
;  fall thru to nojoy
;
nojoy:
        move.l  (a4),d2                 ;check receive buffer full
        btst.l  #13,d2
        beq     retvlm
;
;
gotone:
        move.w  DS_DATA(a4),d2          ;else, get bogus receive stuff
        tst.l   DSCNTRL(a4)             ;read to clear interrupt flag
        lea     retcode,a5
        move.w  d2,d0
        lsr.w   #8,d0
        move.b  d2,(a5,d0.w)            ;save return code
;
;  *****The following 5 lines added 28-Mar-95
        cmpi.b  #4,d0                   ;are we error code
        bne.s   retvlm                  ;br if not
;
        tst.b   1(a5)                   ;check for scan error
        bpl.s   retvlm                  ;must be scan
        bra     set0                    ;retry this track seek
;  ********end addition
;
;   go back to vlm
;
retvlm:
        tst.w   cdgmode                 ;return to vlm if not in cd+g
        bne.s   mustbcdg        
        rts
;
mustbcdg:
        bsr     subcode                 ;call Pradip for service
        tst.w   Pradip
        bne.s   mbcdg
        bsr     dispsub
        move.w  #-1,Pradip
        jsr     readpad
mbcdg:
;       bra     mustbcdg
        bra     service
;
;
;
;   d0.b is BCD to convert to hex
;   d1 is trashed (returns 0 if good BCD else nz)
;
;
bcd2hex:
        andi.w  #$ff,d0
        move.w  d0,d1
        andi.w  #$f,d1
        cmpi.w  #10,d1
        bcc.s   bcdbad
        lsr.w   #4,d0
        cmpi.w  #10,d0
        bcc.s   bcdbad
        mulu    #10,d0
        add.w   d1,d0
        moveq   #0,d1
        rts
bcdbad:
        moveq   #-1,d1
        rts
;
;
hex2bcd:
        andi.l  #$ff,d0
        divu    #10,d0
        move.l  d0,d1
        swap    d1
        lsl.w   #4,d0
        or.b    d1,d0
        rts
;
;
;  rotate vlm logo
;
fliplogo:
        .if     1
        lea     vlmlogo,a0      ;do a temp swap
        move.l  a0,a3
        adda.w  #(32*10*2),a3
        move.w  #(32*5)-1,d7
.fliplp:
        move.w  (a0),d6
        move.w  -(a3),(a0)+
        move.w  d6,(a3)
        dbra    d7,.fliplp
        .else
;
;  exchange lite VLM w/ Dark
;
        lea     vlmlogo,a0
        lea     vlmdk(pc),a3
        move.w  #(16*10)-1,d7
.exglp:
        move.l  (a0),d6
        move.l  (a3),(a0)+
        move.l  d6,(a3)+
        dbra    d7,.exglp       
;
        .endif
        rts
;
;
;  Blit up the Front panel on a cleen screen
;
frontup:
        lea     A1_BASE,a0
        lea     onepage3,a3
.bwait:
        move.l  B_CMD-A1_BASE(a0),d0    ;wait til blitter free
        lsr.w   #1,d0
        bcc     .bwait
;
        moveq   #0,d0
        move.w  (a3),d0
        add.l   a3,d0
        move.l  d0,A2_BASE-A1_BASE(a0)  ;set SRC base
        move.l  #frntbase,(a0)          ;set DST base
;
        move.l  #PITCH1|WID256|XADDPIX|PIXEL8,A2_FLAGS-A1_BASE(a0)      ;src
        move.l  #PITCH1|WID256|XADDPIX|PIXEL8,A1_FLAGS-A1_BASE(a0)      ;dst
;
        moveq   #0,d0
        move.l  d0,A2_PIXEL-A1_BASE(a0) ;src = (0,0)
        move.l  d0,A1_PIXEL-A1_BASE(a0) ;dst
;
        move.l  #(48*$10000)+256,B_COUNT-A1_BASE(a0)    ;w:256, h:48
        move.l  #(2*$10000)-256,d0
        move.l  d0,A1_STEP-A1_BASE(a0)
        move.l  d0,A2_STEP-A1_BASE(a0)
        move.l  #UPDA2|UPDA1|SRCEN|LFU_S|DSTEN,B_CMD-A1_BASE(a0)
;
;  Now blit up the bottom sub panel ("C" "Left" "B" "Right" "Pause")
;
subtbwid        equ     172
;
        lea     tabuild(pc),a5
        moveq   #2,d1
tablop:
.bwait:
        move.l  B_CMD-A1_BASE(a0),d0    ;wait til blitter free
        lsr.w   #1,d0
        bcc     .bwait
;
        moveq   #0,d0
        move.w  (a5)+,d0                ;get src Y
        swap    d0
        move.l  d0,A2_PIXEL-A1_BASE(a0) ;src = (0,0)

        moveq   #0,d0
        move.w  (a5)+,d0                ;get dst Y
        swap    d0
        move.l  d0,A1_PIXEL-A1_BASE(a0) ;dst
;
        move.w  (a5)+,d0                ;get height
        swap    d0
        move.w  #subtbwid,d0
        move.l  d0,B_COUNT-A1_BASE(a0)  ;w:256, h:48
        move.l  #(2*$10000)-subtbwid,d0
        move.l  d0,A1_STEP-A1_BASE(a0)
        move.l  d0,A2_STEP-A1_BASE(a0)
        move.l  #UPDA2|UPDA1|SRCEN|LFU_S|DSTEN,B_CMD-A1_BASE(a0)
;
        dbra    d1,tablop
        bra.s   ovtbtab
;
tabuild:
        dc.w    1               ;src Y
        dc.w    47              ;dst Y
        dc.w    13              ;height
;
        dc.w    2               ;src Y
        dc.w    60              ;dst Y
        dc.w    1               ;height
;
        dc.w    46              ;src Y
        dc.w    61              ;dst Y
        dc.w    2               ;height
;
;
ovtbtab:
;
;   Now, stuff the buttons
;
        lea     butbuild(pc),a5
        moveq   #3,d1
butlop:
.bwait:
        move.l  B_CMD-A1_BASE(a0),d0    ;wait til blitter free
        lsr.w   #1,d0
        bcc     .bwait
;
        move.l  (a5)+,d0                ;get src Y,X
        move.l  d0,A2_PIXEL-A1_BASE(a0) ;src = (0,0)

        move.w  #51,d0                  ;Ypos = constant
        swap    d0
        move.w  (a5)+,d0                ;get dst X
        move.l  d0,A1_PIXEL-A1_BASE(a0) ;dst
;
        moveq   #8,d0                   ;height is constant
        swap    d0
        move.w  (a5)+,d0                ;get width
        move.l  d0,B_COUNT-A1_BASE(a0)  ;w:xx, h:8
        ext.l   d0
        sub.l   #(2*$10000),d0
        neg.l   d0
        move.l  d0,A1_STEP-A1_BASE(a0)
        move.l  d0,A2_STEP-A1_BASE(a0)
        move.l  #UPDA2|UPDA1|SRCEN|LFU_S|DSTEN,B_CMD-A1_BASE(a0)
;
        dbra    d1,butlop
        bra.s   ovbttab
;
butbuild:
        dc.l    (100*$10000)+0  ;src Y, X       C
        dc.w    4               ;dst X
        dc.w    28              ;width
;       
        dc.l    (49*$10000)+218 ;src Y, X       Left
        dc.w    34              ;dst X
        dc.w    36              ;width
;       
        dc.l    (100*$10000)+28 ;src Y, X       B
        dc.w    72              ;dst X
        dc.w    28              ;width
;       
        dc.l    (58*$10000)+218 ;src Y, X       Right
        dc.w    101             ;dst X
        dc.w    38              ;width
;       
;
;
ovbttab:
;
;
        tst.w   play
        beq.s   notdep
;
        moveq   #2,d0
        moveq   #1,d1
        bsr     depressd                ;play button depressed
notdep:
        tst.w   pause
        beq.s   notpaws
;
        moveq   #4,d0
        moveq   #1,d1
        bsr     depressd                ;pause button depressed
notpaws:
        lea     onepage3+10,a3
        tst.w   cdgmode         ;are we in RGB?
        beq.s   notrgb          ;br if not
        lea     rgbpal,a3
notrgb:
        move.l  a3,a5
        lea     CLUT,a0
        move.w  #20,d0
paller:
        move.w  (a3)+,(a0)+     ;stuff 1st 128 palette entries
        dbra    d0,paller
;
        move.l  a5,a3
        lea     CLUT,a0
        move.w  #20,d0
pallerx:
        move.w  (a3)+,d1
        cmp.w   (a0),d1
        beq.s   gopall
        move.w  d1,(a0)+
        dbra    d0,pallerx
        rts
gopall:
        addq.l  #2,a0
        dbra    d0,pallerx
;
;
        rts
;
;
;
;=================================================
;
;
;
;   this routine will translate the joypad for mode 3
;
m3trans:
        move.l  pad_now,d1
;
;**added 17-Jan-95
        moveq   #3,d0
        cmp.w   vlm_mode,d0     ;check on this idiot edit mode
        bne.s   vlmnoe
;                  3         2         1         0
;                 10987654321098765432109876543210
;                 xxAPxxBxRLDU147*xxCxxxOx2580369#
        andi.l  #%00010000000000000000000000000000,d1   ;only allow pause
        bra     m3tx
vlmnoe:
;***end addition
;
        move.l  m3prev,d0
        move.l  d1,d2
        eor.l   d0,d2                   ;any changes in m3 buttons?
;
; arbitrate multiple RLDU edges here (we're better off doing only 1 at a time)
        move.l  d2,d3
        swap    d3
        andi.w  #%11110000,d3           ;edges here
        move.l  d1,d4
        swap    d4
        lsr.w   #4,d4
        andi.w  #%00001111,d4           ;current
        or.w    d4,d3
        moveq   #0,d4
        move.b  arbtab(pc,d3.w),d4
        beq     samRLDU
;
;                   3         2         1         0
;                  10987654321098765432109876543210
;                  xxAPxxBxRLDU147*xxCxxxOx2580369#
        andi.l  #~%00000000111100000000000000000000,d1  ;clear any RLDU
        lsl.w   #4,d4                   ;align to RLDU
        swap    d4
        or.l    d4,d1                   ;put in our custom RLDU
        move.l  d1,d2
        eor.l   d0,d2
        bra     samRLDU
;          U   D       L               R
arbtab:
 dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ;no edges
 dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ;U
 dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ;D
 dc.b $80,$80,$80,$80,$84,$84,$84,$84,$88,$88,$88,$88,$8c,$8c,$8c,$8c ;D&U
 dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ;L
 dc.b $80,$80,$82,$82,$80,$80,$82,$82,$88,$88,$8a,$8a,$88,$88,$88,$88 ;L&U
 dc.b $80,$81,$80,$81,$80,$81,$80,$81,$88,$89,$88,$89,$88,$89,$88,$89 ;L&D
 dc.b $80,$80,$80,$80,$80,$80,$80,$80,$88,$88,$88,$88,$88,$88,$88,$88 ;L&D&U
 dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ;R
 dc.b $80,$80,$82,$82,$84,$84,$86,$86,$80,$80,$82,$82,$84,$84,$86,$86 ;R&U
 dc.b $80,$81,$80,$81,$84,$85,$84,$85,$80,$81,$80,$81,$84,$85,$84,$85 ;R&D
 dc.b $80,$80,$80,$80,$84,$84,$84,$84,$80,$80,$80,$80,$84,$84,$84,$84 ;R&D&U
 dc.b $80,$81,$82,$83,$80,$81,$82,$83,$80,$81,$82,$83,$80,$81,$82,$83 ;R&L
 dc.b $80,$80,$82,$82,$80,$80,$82,$82,$80,$80,$82,$82,$80,$80,$82,$82 ;R&L&U
 dc.b $80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81,$80,$81 ;R&L&D
 dc.b $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80 ;R&L&D&U

samRLDU:
        move.l  d1,m3prev

        move.l  d2,d3
        and.l   d1,d3                   ;positive edges in d3
;
        move.w  keytime,d4              ;is a key pending?
        beq.s   keychk0
        cmp.w   frames,d4
        bge.s   keychk0
        clr.w   keytime
        moveq   #-1,d5
        move.w  distrack,d4
        bne     seeker
        tst.w   progenty                ;were we in program entry mode?
        bne     wedele                  ;br if so
;******test
        move.b  #-1,1(a1)               ;set track for auto update
;****end test
keychk0:
        move.l  d3,d4                   ;check for numeric key entires

;                  3         2         1         0
;                 10987654321098765432109876543210
;                 xxAPxxBxRLDU147*xxCxxxOx2580369#
        andi.l  #%00000000000011100000000011111111,d4   ;check numeric
        beq.s   ignkey
;
;  positive edge on numeric key
        btst.l  #16,d1                  ;is it one of Jeff's (*)?
        beq     savekey                 ;br if not
;
;
ignkey:
        btst.l  #16,d3                  ;leading edge on *?
        beq.s   ignkeyx                 ;br if not
        move.w  cdgmode,d4              ;are we in cd+g?
        beq.s   ignkeyx                 ;br if not
;
;  In cd+g mode and a # key was pressed...cycle cd+g channels
;
        move.w  TvChnlAv,d5     ;check avail channels on cd+g
        ori.w   #2,d5           ;always force ch1
cdglp:
        addq.w  #1,d4           ;cdgmode
cdglp0:
        btst.l  d4,d5           ;are we avail?
        bne.s   cdgavail        ;br if so
        cmpi.w  #15,d4
        bcs     cdglp
        moveq   #0,d4
        bra     cdglp
cdgavail:
        move.w  d4,cdgmode
        move.b  d4,TvChnlNo     
        addq    #8,d4
        move.w  d4,d0
        moveq   #0,d1
        moveq   #3,d4           ;check for front panel
        cmp.w   our_mode,d4
        bne.s   .cdgav
        bsr     radiob
.cdgav:
        bsr     bankout
        moveq   #0,d1
        bra     m3tx    
;       
;
ignkeyx:        
        moveq   #3,d4                   ;check for mode3 ("User in-your-face")
        cmp.w   our_mode,d4
        beq     dom3            ;br if mode 3, we need fancy translation
;
;  Not mode3, so just pass joystick thru (except if in vlm_mode)
;
        tst.w   cdgmode         ;ignore vlm mode stuff if in cd+g
        bne.s   m3tx
;
        tst.w   vlm_mode        ;0=audio mode, 1=vlm mode, 3=vlm edit mode
        beq.s   m3tx            ;br if audio mode
m3txx:
;
;  else, we're in vlm mode--we must ignore B & UDLR
;
;       
;                   3         2         1         0
;                  10987654321098765432109876543210
;                  xxAPxxBxRLDU147*xxCxxxOx2580369#
;       andi.l  #~%00000010111100000000000000000000,d1  ;cancel B & UDLR
        andi.l  #~%00000010111100000010000000000000,d1  ;cancel B, C & UDLR
m3tx:
        clr.l   dave_pad
notm3:
        or.l    d1,dave_pad
        rts
;
;
;  Numeric keypad check
;
savekey:
;  got a positive edge on a numeric key (or #)
;
        moveq   #7,d5
keylp:
        lsr.w   #1,d4
        bcs.s   keygot
        dbra    d5,keylp
        swap    d4
        lsr.w   #1,d4
;
        moveq   #2,d5
keylp1:
        lsr.w   #1,d4
        bcs.s   keygot1
        dbra    d5,keylp1
        bra     ignkey
;
;
keytab:
        dc.b    2,5,8,0,3,6,9,-1
        dc.b    1,4,7,0
;
;
keygot1:
        addq    #8,d5
keygot:
        move.b  keytab(pc,d5),d5
        move.w  keytime,d4              ;get frame count when last key came in
        bne     secnkey
        tst.b   d5                      ;are we terminator without a key?
        bmi     ignkey                  ;br if so
        move.w  d5,distrack             ;display track during key entry
        move.w  frames,d0
        add.w   #120,d0                 ;2 seconds til forced update
        bne.s   .not0
        addq.w  #1,d0
.not0:
        move.w  d0,keytime
;
;  display track in d5
;
        tst.w   progenty                ;are we in program entry mode?
        beq     .stand                  ;br if not
;
;  Program entry mode..  open up a place in the sequence
;
;       add.w   #1,gridsize     ;# of tracks for trk grid purposes
;
        move.w  gridsize,d0     ;need a limit on this
        addq.w  #1,d0
        cmpi.w  #191,d0         ;no more than "x" tracks allowed
        bcs     .advtrk
        clr.w   keytime
        bra.s   .noarad
.advtrk:
        move.w  d0,gridsize
;
;
        move.w  m3row,d0
        subq.w  #3,d0
        mulu    #10,d0
        add.w   m3col,d0
        add.w   gridoff,d0      ;d0 = offset into trkseq for new entry
        lea     trkseq,a5
        adda.w  d0,a5
        move.l  a5,a0
        move.l  a0,d1           ;save target
.endgam:
        tst.b   (a0)+           ;find end
        bne     .endgam
;
        lea     1(a0),a5
.openho:
        move.b  -(a0),-(a5)     ;expand trkseq to open up a hole
        cmp.l   a0,d1
        bcs     .openho
;
        move.w  distrack,d0     ;get current entry
        bne.s   .singe
        moveq   #-1,d0          ;set special tag for gridigit
.singe:
        move.b  d0,(a0)
        move.l  a0,holeptr      ;save our ptr
;
        move.b  d0,d4           ;29-Mar-95
        bsr     listinst
;       bsr     uprantrk        ;display the new tracks
;
        move.w  #-1,blinkon
        bsr     ringit          ;restore select box
;
        move.w  gridsize,d0
        sub.w   gridoff,d0
        cmpi.w  #21,d0
        bcs.s   .noarad
;
        bsr     plantard        ;turn on down arrows
.noarad:
        moveq   #0,d1                   
        bra     m3tx
;
;
;  Not in Program entry mode--display digit in standard trk window
;
.stand:
        move.w  d5,d0
;
        move.w  our_mode,d5
        cmpi.w  #3,d5           ;on mode3 screen
        beq.s   .novlmst        ;br if so
;       
        tst.w   vlm_mode        ;are we in Jeff mode?
        beq.s   .novlmst        ;br if not
;
;  in VLM mode on non mode3 screen
;
        tst.w   d0              ;don't allow effect 0
        beq.s   .yevlmst
        move.w  d0,diseff       ;set effect
        bsr     bankout
        bra.s   .yevlmst
;
.novlmst:
        moveq   #1,d1
        bsr     printout
.yevlmst:
        moveq   #0,d1                   
        bra     m3tx
;
secnkey:
        move.w  distrack,d4
        tst.b   d5                      ;are we a terminator with 1 key?
        bmi.s   seeker
        mulu    #10,d4
        add.w   d5,d4
seeker:
        clr.w   keytime
;
;  d4 has completed hex track #
;
        tst.w   progenty                ;are we in program entry mode?
        beq     .doseeky                ;br if not
;
        tst.w   d4
        beq     wedele          ;user wants trk 00, deny him
;
        move.w  maxmin,d0
        andi.w  #$ff,d0
        cmp.w   d4,d0           ;does track exist?
        bcs     wedele          ;br if not
;
        move.l  holeptr,a0
        move.b  d4,(a0)         ;install final track in trkseq
;
        move.l  a0,d0
        sub.l   #trkseq,d0
;
        move.w  trksptr,d1      ;check for current not on list
        bpl.s   .onlist
;
        cmp.w   track,d4
        bne.s   .noinc
;
        move.w  d0,trksptr
;       bra.s   .noinc
;
.onlist:
.noinc:
        bsr     uprantrk        ;display the new tracks
;
;       bsr     listinst
;
        clr.w   blinkon
        bsr     ringit                  ;turn off current selector box
;
        move.w  m3col,d0                ;move select box to right
        add.w   #1,d0
        cmpi.w  #10,d0
        bcs.s   .gotrite
;
        move.w  m3row,d0
        add.w   #1,d0
        cmpi.w  #4,d0                   ;move down row if you have to
        beq.s   .gotdown
        add.w   #10,gridoff
;
        bsr     uprantrk        ;display the new tracks
;
        bsr     plantaru                ;add up arrows          
        move.w  gridsize,d2
        sub.w   gridoff,d2
        cmpi.w  #21,d2
        bcc.s   .notdof
        bsr     planoffd        ;turn off down arrows
.notdof:
        move.w  #4,d0
.gotdown:
        move.w  d0,m3row
        move.w  #0,d0
.gotrite:
        move.w  d0,m3col
;
        move.w  #-1,blinkon
        bsr     ringit          ;restore select box
;
        moveq   #0,d1
        bra     m3tx
;
;  d4 has track or bank/effect
;
.doseeky:
        move.w  our_mode,d6
        cmpi.w  #3,d6           ;on mode3 screen
        beq     .unJeff         ;br if so
;       
        tst.w   vlm_mode        ;are we in Jeff mode?
        beq     .unJeff         ;br if not
;
;  in VLM mode on non mode3 screen--d4 is bank/effect to use
;
;       move.w  imatrix,d6
;       addq.w  #1,d6           ;assume 1 character
;
        tst.b   d5              ;negative if only 1 char
        bpl.s   .twochar        ;br if 2 chars entered
;
;  Only 1 char entered--can only be effect
;
        tst.w   d4
        beq.s   .dobout         ;if effect 0, just ignore
;
;  just change effect
;
.onechar:
        cmp.w   skid,d4
        beq.s   .dobout
;
        move.w  d4,skid
        move.l  #skidoo,action  ;jeffs recommendation for just effect change
        bra.s   .waiteff
;
;  2 chars entered for bank/effect change
;       
.twochar:
        ext.l   d4
        divu    #10,d4
        move.w  d4,d6           ;d6.w = bank (+1)
        swap    d4              ;d4.w = effect
.unchar:
        tst.w   d4
        beq.s   .dobout         ;if effect 0, just ignore
        subq.w  #1,d6
        bmi.s   .dobout         ;if bank 0, just ignore
        cmp.w   imatrix,d6      ;not a new bank?
        beq     .onechar        ;br if not new--just do effect
;
;  d4 = effect
;  d6 = bank
;
        move.w  d6,imatrix
        move.w  d4,skid
        move.l  #gm,action      ;switch bank/effect
        bra.s   .waiteff        
.dobout:
        move.w  imatrix,d6
        move.w  d6,locbank
        move.w  d6,disbank
        move.w  skid,d6
        move.w  d6,loceff
        move.w  d6,diseff
        bsr     bankout
.waiteff:
        moveq   #0,d1                   
        bra     m3tx
;
;
.unJeff:
        tst.w   d4              ;are we requesting track 0?
        bne.s   .unZero
        move.b  #-1,1(a1)       ;force redisplay of track
        bra     oit
.unZero:
        move.w  track,d5        ;remember current track
        move.w  d4,track
        move.w  trksptr,d4
        bsr     trkptrfx
        tst.b   d2
        bmi.s   .notfnd
        tst.w   play                    ;are we in play mode?
        bne.s   .doplay
        move.w  trksptr,d4
        bra.s   .jusdis                 ;just display & continue
;
.doplay:        
        addq.l  #4,sp                   ;adjust for subroutine
        bra     starplay
.notfnd:
        move.w  d4,trksptr              ;restore previous track ptr
        bra.s   .m3txx
.jusdis:
        lea     trkseq,a3
        move.b  (a3,d4.w),d5
.m3txx:
        move.w  d5,track
        move.w  d5,d0
;
        bne.s   .ninpla                 ;13-Apr-95 if not in play
        move.w  maxmin,d0
        andi.w  #$ff,d0                 ;end 13-Apr-95 addition 
.ninpla:
;
        bsr     hex2bcd
        moveq   #1,d1
        bsr     printout
oit:
        moveq   #0,d1
        bra     m3tx
;
;  We're in mode 3--do it
;
dom3:

        move.l  d1,d0
;
        btst.l  #9,d3                   ;positive edge on option?
        beq.s   dom3z
;
        move.w  #frnthite-15,d0
        move.w  #-1,blinkon             ;assume turn on entering select box
        not.w   m3opt                   ;toggle upper/lower mode
        bne.s   setlo                   ;br if lower
;
;  else, upper
;
;*******test
        tst.w   keytime                 ;are we in data entry?
        beq.s   keynoe                  ;br if not
        clr.w   keytime
        tst.w   distrack
        bne.s   keynoe
        move.w  d0,-(sp)
        bsr     wedele
        move.w  #-1,blinkon             ;assume turn on entering select box
        move.w  (sp)+,d0
keynoe:
;
;*******end test
        clr.w   progenty                ;force no program entry
        addi.w  #15,d0
        not.w   blinkon                 ;else, turn on ring
        bra.s   doring
setlo:
        move.w  m3row,d5
        cmpi.w  #3,d5
        bcs.s   doring
;
        tst.w   seqmode                 ;are we in program mode?
        bne.s   doring
        move.w  #-1,progenty            ;set up program entry mode
doring:
        move.w  d0,davesobj+$1a         ;set new hieght
;
        bsr     ringit
        bsr     chngdisp
        bra     bd2clr                  ;exit
;
dom3z:
        btst.l  #Abit,d3                ;Abit exits mode--let main do it now
        beq.s   nom3chg                 ;br if not exiting mode
;
;  Positive edge on fireA--ignore other stuff & exit mode
;
        moveq   #0,d1
        bset.l  #Abit,d1        ;set fireA bit so main code can do dirty work
        clr.l   m3prev          ;prev m3 buttons cleaned up
;
;       clr.w   m3col           ;reset to "play" mode (fireB)
;       clr.w   m3row           ; on row too
;
        clr.w   progenty        ;exit program entry mode (if any)
;
        clr.w   boxleft
        move.w  frames,voltimr  ;turn off volume bar (if on)
        bra     notm3           ;exit to change modes
;
;
nom3chg:
        move.l  d0,d4
;                  3         2         1         0
;                 10987654321098765432109876543210
;                 xxAPxxBxRLDU147*xxCxxxOx2580369#
        andi.l  #%00010010111100000010000000000000,d4   ;PBC,RLUD pass-thru
        tst.w   m3opt
        bne     dom3zz                  ;br if on bottom
        move.l  d4,dave_pad             ;else, exit with pass thru
        rts
;
;
dom3zz:
;                  3         2         1         0
;                 10987654321098765432109876543210
;                 xxAPxxBxRLDU147*xxCxxxOx2580369#
        andi.l  #%00010000000000000010000000000000,d0   ;P & C pass-thru
        move.l  d0,dave_pad
;
dom33z:
;                  3         2         1         0
;                 10987654321098765432109876543210
;                 xxAPxxBxRLDU147*xxCxxxOx2580369#
        andi.l  #%00100010111100000000000000000000,d1   ;only want these
;
;  No mode change requested--check RLDU for any m3sel ("selection") changes
;
;       
;                  3         2         1         0
;                 10987654321098765432109876543210
;                 xxAPxxBxRLDU147*xxCxxxOx2580369#
        andi.l  #%00000000111100000000000000000000,d3   ;positive edges?
        beq     nom3sel         ;br if no joypad maneouverings
;
;  joypad maneouvering..
;
        tst.w   keytime         ;are we awaiting another keypad entry?
        beq.s   .nokey          ;br if not
        tst.w   progenty        ;are we in program mode
        beq.s   .nokey          ;br if not
;
;  we have to cancel any further key entry..
        clr.w   keytime         ;turn off await timer
        move.w  distrack,d4     ;check legit track request
        beq.s   .cankey
;
        move.w  maxmin,d5
        andi.w  #$ff,d5
        cmp.w   d4,d5
        bcs.s   .cankey         ;cancel previous key entry
;
;  legit partial track..  do final install in list
;       movem.l d0-d7/a0-a6,-(sp)
;       bsr     listinst        ;29-Mar-95 update display, trksptr
;       movem.l (sp)+,d0-d7/a0-a6
;
;       move.l  holeptr,a0
;       move.b  d4,(a0)         ;install partial track as final
;
;       bsr     uprantrk        ;***29-Mar-95
;
;       move.w  #-1,blinkon
;       bsr     ringit          ;restore select box

        bra.s   .nokey
;
.cankey:
        movem.l m3col,d4
        movem.l d0-d4,-(sp)
        bsr     wedele          ;delete entry

        clr.w   blinkon
        bsr     ringit          ;turn off select box
        
        movem.l (sp)+,d0-d4
        move.l  d4,m3col        ;restore original row/col
        
.nokey: 
        tst.w   boxleft         ;check for box off grid (on volume bar)
        beq.s   nrmbox          ;br if not
;
;                  3         2         1         0
;                 10987654321098765432109876543210
;                 xxAPxxBxRLDU147*xxCxxxOx2580369#
        andi.l  #%00000000110000000000000000000000,d3   ;edge on L/R?
        bne.s   boxon
;
volux:
;                  3         2         1         0
;                 10987654321098765432109876543210
;                 xxAPxxBxRLDU147*xxCxxxOx2580369#
        andi.l  #%00000000001100000000000000000000,d1   ;pass thru only up/down
        bra     notm3

boxon:
        move.w  m3row,d2
        bne.s   rowan
        moveq   #2,d2
        btst.l  #Lbit,d3
        bne.s   set3col
        moveq   #3,d2
        bra.s   set3col
rowan:
        moveq   #2,d2
        btst.l  #Lbit,d3
        bne.s   set3col
        moveq   #0,d2
set3col:
        move.w  d2,m3col
        move.w  frames,voltimr          ;turn off volume bar
;
;
;
nrmbox:
        moveq   #-1,d2
        btst.l  #Lbit,d3        ;leading edge on Left? 
        bne     chgcol
        moveq   #1,d2
        btst.l  #Rbit,d3
        bne     chgcol
        btst.l  #Dbit,d3
        bne.s   chgrow
        moveq   #-1,d2
        btst.l  #Ubit,d3
        beq     nom3sel
;
;
;  0 - Front panel
;  1 - seq/repeat/cd+g buttons
;  2 - trk selector box         (only in program mode)
;  3 - top row, trk grid        (only in prog & random modes)
;  4 - bottom row, trk grid     (only in prog & random modes)
;
;
;
chgrow:
        add.w   m3row,d2
        beq     bd2clr          ;don't allow access to old row #0
;       bpl.s   nomwrap
;       moveq   #0,d2           ;can't go above (neg) top row
nomwrap:
        cmpi.w  #2,d2           ;max +1 row # in normal mode
        bcs     nomwrap1        ;br if ok within all modes
        move.w  seqmode,d4      ;are we in program or random modes?
;
        beq.s   randrow         ;br if program mode--always allow grid access
;
        cmpi.w  #1,d4           ;normal mode?
        beq     bd2clr          ;br if so--at limit, can't change rows
;
;  must be random mode..(see if overflo allows us scrolling)
;
        move.w  gridsize,d4
        cmpi.w  #21,d4          ;more than 20 tracks?
        bcs     bd2clr          ;if not, can't select any grid box
;
;
;  Prog/random mode..
;
randrow:
        cmpi.w  #5,d2           ;max on random/prog
        bcs.s   .rannwrp        ;br if not max on random mode
;
        move.w  gridsize,d4
        cmpi.w  #21,d4
        bcs     .skipwrp
        sub.w   gridoff,d4
        cmpi.w  #21,d4
        bcs     .skipwrp
;
;  Scroll data in track grid down 1 row
;
        addi.w  #10,gridoff
        bsr     uprantrk
        bsr     plantaru        ;set up arrows
        move.w  gridsize,d2
        sub.w   gridoff,d2
        cmpi.w  #21,d2
        bcc.s   .notdof
        bsr     planoffd        ;turn off down arrows
.notdof:
        moveq   #4,d2           ;set max select row position
;
.rannwrp:
        cmpi.w  #2,d2           ;in random mode, did we land on phantom row2?
        bne.s   nomwrap1        ;br if not
;
;  We have no row2 in random mode--better jump over it
;
        btst.l  #31,d2          ;check neg increment (moving up a row)
        bne.s   .upmove         ;br if moving up
;
;  we were moving down and landed on phantom row 2
;
        move.w  seqmode,d2      ;are we in program mode?
        bne.s   .notprg         ;br if random mode
        move.w  #-1,progenty    ;set flag for program entry mode
;
        clr.w   keytime         ;29-Mar-95              
        move.b  #-1,1(a1)       ;force track update
;
.notprg:
        move.w  #3,d2           ;set for row 3
        bra     nomwrap1
.upmove:
;
;  we may have to hold here to Scroll track data if gridoff is non-zero
;
        move.w  gridoff,d4      ;get grid offset
        beq.s   .scroff
        subi.w  #10,d4
        move.w  d4,gridoff
        bne.s   .stillgrd
        bsr     planoffu        ;remove up arrows
.stillgrd:
        move.w  gridsize,d0
        sub.w   gridoff,d0
        cmpi.w  #21,d0
        bcs.s   .noarad
;
        bsr     plantard        ;turn on down arrows
.noarad:

;       bsr     plantard
;
;
        bsr     uprantrk
        moveq   #3,d2
        bra.s   nomwrap1
.scroff:
        clr.w   progenty        ;always clear if moving up
        moveq   #-1,d4
.adjop:
        add.w   d4,d2
        bra.s   nomwrap1
.skipwrp:
        moveq   #-1,d4
        btst.l  #31,d2
        bne.s   .adjopx
        moveq   #1,d4
.adjopx:
        sub.w   d4,d2
nomwrap1:
;
;  set column for new row
;
        move.w  d2,d4
        swap    d2
        move.w  m3row,d2
        add.w   d4,d2
        cmpi.w  #6,d2
        bcs.s   offgrid 
;
;  same col as before if 3<->4
;
samebf:
        move.w  m3col,d2
        tst.w   progenty        ;are we in program entry?
        beq     nomwrapx        ;br if not
;
; we may need to prevent landing on a blank..
;
        swap    d2
        move.w  d2,d4           ;get proposed row
        swap    d2
        subq.w  #3,d4           ;adjust for top row of grid (+10)
        mulu    #10,d4
        add.w   gridoff,d4
        sub.w   gridsize,d4     ;
        neg.w   d4
        bmi     bd2clr          ;this occurs if no legit entries on new row
        cmp.w   d2,d4
        bcc     nomwrapx
;
        move.w  d4,d2           ;this is max col for this row
        bra     nomwrapx
;
samesam:
        move.w  m3row,d2
        swap    d2
        bra     samebf
;
offgrid:
        sub.w   m3row,d4        ;new row - old row
        beq     samebf
        bpl.s   .setmid         ;going down
;
;  moving up
;
        move.w  m3row,d4
        cmp.w   #3,d4           ;coming in from row #3?
        bne.s   .setmid
;
        moveq   #0,d4
        move.w  m3col,d4
        subq    #1,d4
        bpl.s   .loc
        moveq   #0,d4
.loc:
        divu    #3,d4
        btst.l  #17,d2          ;are we going to row #2?
        beq.s   .loca
        moveq   #0,d4
.loca:
        move.w  d4,d2
        bra     nomwrapx
.setmid:
        swap    d2
        move.w  d2,d4
        swap    d2
        clr.w   d2
        move.b  midtab(pc,d4.w),d2
        cmpi.w  #3,d4           ;are we entering grid?
        bne     nomwrapx
        cmp.w   gridsize,d2     ;if so--can't be beyond a short program here
        bcs     nomwrapx
        move.w  gridsize,d2
        bra     nomwrapx
midtab:
        dc.b    0,1,0,5
;
;
;
chgcol:
        move.w  m3row,d3
        cmpi.w  #3,d3                   ;are we on trk grid?
        bcs.s   .notgrid                ;br if not
;
        swap    d3
        move.w  seqmode,d3
        cmpi.w  #2,d3                   ;are we in random mode?
        beq     bd2clr                  ;if so, ignore left/right selects
        swap    d3
.notgrid:
        add.w   m3col,d2
        bpl.s   nomwrap3
;
        cmpi.w  #3,d3                   ;are we row #3?
        bcs.s   .nongri
;
        bne.s   .nongrii
;
;  moved left on top row--may need to scroll up
;
        tst.w   gridoff                 ;any scroll room?
        beq     staytop                 ;br if not--stay put
;
        subi.w  #10,gridoff
        bsr     uprantrk

        move.w  gridsize,d0
        sub.w   gridoff,d0
        cmpi.w  #21,d0
        bcs.s   .noarad
;
        bsr     plantard        ;turn on down arrows
.noarad:
;       bsr     plantard        ;set up down arrows

        tst.w   gridoff
        bne.s   .notdof
        bsr     planoffu        ;turn off up arrows
.notdof:

;
.nongrii:
        move.w  #3,d3
.nongri:
        moveq   #0,d2
        move.b  maxcolx(pc,d3.w),d2
        bra.s   nomwrap3
;
;  max col #'s, each row
;
maxcolx:
        dc.b    4,2,0,9,9,0
;
nomwrap3:
        btst.l  #31,d2                  ;moved to left?
        bne.s   nomwrap2                ;br if so--we can always do
;
        cmpi.w  #3,d3                   ;just check below if on trk grid
        bcs.s   .notforb
;
;  moving to right--did we enter forbidden zone?
;
        move.w  d3,d4           ;get proposed row
        subq.w  #3,d4           ;adjust for top row of grid (+10)
        mulu    #10,d4
        add.w   gridoff,d4
        sub.w   gridsize,d4     ;
        neg.w   d4
        cmp.w   d2,d4
        bcc     .notforb
        move.w  d4,d2
.notforb:
;       
;  need to check bounds
;       
        cmp.b   maxcolx(pc,d3.w),d2
        bls.s   nomwrap2
;
        cmpi.w  #3,d3
        bcs.s   staytop
;
;  we want to wrap on right--check rules for how to do..
;
        addq.w  #1,d3           ;advance row count
        cmpi.w  #5,d3           ;exceeded rows allowed?
        bcs.s   staytop         ;br if not
        moveq   #4,d3           ;got to stay on bottom row
;
;  advance to next row
;
        addi.w  #10,gridoff
        bsr     uprantrk
        bsr     plantaru        ;set up arrows
        move.w  gridsize,d4
        sub.w   gridoff,d4
        cmpi.w  #21,d4
        bcc.s   .notdof
        bsr     planoffd        ;turn off down arrows
.notdof:
        moveq   #4,d3           ;set max select row position
staytop:
        moveq   #0,d2
nomwrap2:
        swap    d2
        move.w  d3,d2
        swap    d2
;
;  d2 = new row[31:16] col[15:00]
;
nomwrapx:
        move.l  d2,-(sp)
        clr.w   blinkon         ;force previous off
        bsr     ringit
        move.l  (sp)+,d2
        move.w  d2,d4
        add.w   m3col,d4        ;d4=5 if we wrap
;
        tst.w   boxleft         ;are we on volume bar
        beq.s   novobar
        clr.w   boxleft
        bra.s   entbox
;
transtab:
        dc.b    5,2
;
;  not on volume bar
;
novobar:
        bra.s   nowind
;
        swap    d2
        move.w  d2,d5
        swap    d2
        cmp.w   m3row,d5
        bne.s   nowind
        cmp.b   transtab(pc,d5.w),d4    ;check horz wrap indicator
        bne.s   nowind          ;br if no wrap here
        move.w  #-1,boxleft
        moveq   #0,d1
        bset    #Ubit,d1
        bra     notm3           ;exit with up bit set
;
nowind:

        move.w  d2,m3col        ;set new mode 3 selection
        swap    d2
        move.w  d2,m3row
entbox:
        bsr     ringit
;
;
;  Now set translated bit determined by selection from fireB bit
;
nom3sel:
        tst.w   boxleft         ;if we're on the volume bar, just do up/down
        bne     volux           ;exit with up/down only
;
        btst.l  #Bbit,d1
        beq.s   bd2clr
        tst.w   m3row
        bne     nonBset
        moveq   #0,d2
        move.w  m3col,d3
        move.b  btrantab(pc,d3.w),d3
        btst.l  #Bbit,d1
        beq.s   bclear
        bset.l  d3,d2
        bra.s   bclear
btrantab:
        dc.b    Bbit,Rbit,Pbit,Cbit,Lbit,0      
;
;   We translate the B button & RLDU into virtual buttons the rest of the code
;  can understand.
;
;   selections are:
;
;   m3sel:- Action:             State of fireB translated to:
;
;       0 - play (default)      fireB
;       1 - FF                  Right
;       2 - pause               pause
;       3 - Stop                fireC
;       4 - Rew                 Left
;
bd2clr:
        moveq   #0,d2
bclear:
        or.l    d2,dave_pad             ;save our response
        rts                     ; else, we have nothing more to do
;
;
;  We have a B button but we're not on top row
;
nonBset:
        move.l  d2,d3
        and.l   d1,d3
        moveq   #0,d2
        or.l    d2,dave_pad     ;no response for dave
        btst.l  #Bbit,d3        ;leading edge only
        beq.s   nonBsetx
        move.w  m3row,d2
        cmpi.w  #1,d2           ;only have row #0 & #1 for now
        beq.s   werow1
;
;  B-button on row #3/#4--are we in data entry mode?
;
        tst.w   progenty        ;if active, B clears current trk in grid
        bne     werowB          ;if so, go do it
;
nonBsetx:
        rts
;
;
;
;
maxmodz:
        dc.b    3,3,16,0                ;max entries for each col on row #1
;
;
;  B button on row #1
;
werow1:
        move.w  m3col,d2
        moveq   #0,d3
        move.b  maxmodz(pc,d2.w),d3
        add.w   d2,d2
        lea     seqmode,a3
        move.w  (a3,d2.w),d1
;
        addq.w  #1,d1           ;advance seqmode/reptmode/cdgmode
        cmpi.w  #4,d2           ;are we advancing cd+g mode?
        bne.s   scdgm           ;br if not
;
;  we're in cdgmode change
;
;******24-Jan-95
        cmpi.w  #1,d1
        bne.s   scgmlp
        clr.w   vlmrdcnt        ;29-Mar-95
        move.w  frames,vlmrfrm  ;set frame count
        not.w   vlmrand
        bne.s   scdgmx
;*******end addition    
scgmlp:
        move.w  TvChnlAv,d4     ;check avail channels on cd+g
        btst.l  d1,d4           ;are we avail?
        bne.s   scdgm           ;br if so
        addq.w  #1,d1
        cmp.w   d3,d1
        bcs     scgmlp
scdgm:
        cmp.w   d3,d1
        bcs.s   werow1x 
scdgmx:
        moveq   #0,d1
werow1x:
        move.w  d1,(a3,d2.w)
        lsl.w   #1,d2
        add.w   d2,d1
        move.w  d1,d0
        moveq   #0,d1
        bsr     radiob
;
;
;
        move.w  m3col,d0        ;are we changing sequence stuff?
        bne     noseqm          ;br if not
;
;  sequence mode was changed
;
        move.w  seqmode,d0      ;(program-0, normal-1, random-2)
        beq.s   progseqm        ;br if program
        cmpi.w  #1,d0           ;normal mode?
        beq     setnorm
;
;  must be seqmode=2-- entering random sequnce
;
        move.w  maxmin,d0
        andi.w  #$ff,d0
        move.w  d0,gridsize     ;set total # of tracks for grid
        bsr     randf           ;form random sequence of tracks
        clr.w   gridoff         ;29-Mar-95
        bsr     progup
        rts
;
;
;
;
;
listinst:
        move.l  holeptr,a0
        move.b  d4,(a0)         ;install final track in trkseq
;
listins1:
        move.l  a0,d0
        sub.l   #trkseq,d0
;
        move.w  trksptr,d1      ;check for current not on list
        bpl.s   .onlist
;
        cmp.w   track,d4
        bne.s   .noinc
;
        move.w  d0,trksptr
        bra.s   .noinc
;
.onlist:
        cmp.w   d1,d0
        bhi.s   .noinc
        addi.w  #1,trksptr      ;just inc by 1
.noinc:
        bsr     uprantrk        ;display the new tracks
        rts
;
;
;
;
;  Entering User programable sequence mode
;
progseqm:
        lea     progseq,a0
        lea     trkseq,a1
        moveq   #-1,d0
.coprog:
        addq.w  #1,d0
        move.b  (a0)+,(a1)+
        bne     .coprog
;
        move.w  d0,gridsize     ;this is the # of tracks for grid purposes
;
        move.w  trksptrp,trksptr        ;get track ptr used last time in program

        clr.w   gridoff         ;29-Mar-95
        bsr     progup          ;put up user programmable seq
        bra     settrkp         ;exit by setting trksptr
;
;
;  Entering Normal sequential sequence
;
setnorm:
;       move.w  #-1,davesobj+$4c        ;turn off
;
;  previous mode was program--need to preserve user's program sequence
        lea     trkseq,a0
        lea     progseq,a1
.coprog:
        move.b  (a0)+,(a1)+
        bne     .coprog
;
        move.w  trksptr,trksptrp        ;save program mode's trksptr too        
;

        bsr     buildseq        ;normal mode just does straight seq
;
        move.w  maxmin,d0
        andi.w  #$ff,d0
        move.w  d0,gridsize     ;set total # of tracks for grid
;
        clr.w   gridoff         ;29-Mar-95
        bsr     progup
settrkp:
        tst.w   play            ;if we're playing, we better fix trksptr
        beq.s   setnormx        ;br if not playing--we're set   
;
        bsr     trkptrfx        ;find proper trksptr for track we're playing
setnormx:
        bsr     chngdisp        ;may need for cd+g
        rts
;
;  Not in sequence mode, check cd+g
;
noseqm:
        cmpi.w  #2,d0           ;did we change cd+g mode?
        beq.s   docdg           ;br if so       
        rts
;
;
docdg:
        tst.w   cdgmode         ;did we just enter cd+g mode?
        bne     cdgdo           ; br to do it if so
;
;  may be re-entering vlm
;
        tst.w   vlmrand         ;are we just shifting to vlm random?
        beq.s   revlm           ;br if no--we're starting up vlm again
        rts
revlm:
;
;******1-Aug-95
        move.w  frames,d0
frsync:
        cmp.w   frames,d0
        beq     frsync
;***end 1-Aug-95
;
        move.l  #Frame,$100
;
        bsr     stopcdg
;
        lea     davesobj,a0     ;copy daves objects into local
        lea     objcopi,a3
        moveq   #47,d0
daveloc:
        move.l  (a0)+,(a3)+
        dbra    d0,daveloc
;
        move.l  #davesres,davesvec
;
        move.w  frames,reframe  ;save this 'cause vlm trashes
;
.gpuwait:
        move.l  G_CTRL,d0
        lsr.w   #1,d0
        bcs     .gpuwait
;       
        lea     $200000,sp
        jmp     goag
;
;
;
;
davesres:
        move.w  reframe,frames
;
        lea     davesobj,a0     ;copy daves objects into local
        lea     objcopi,a3
        moveq   #47,d0
daveloc1:
        move.l  (a3)+,(a0)+
        dbra    d0,daveloc1
;
;  restore CRY palette
;
.repalle:
;       lea     cdstatus+10,a0
        lea     onepage3+10,a0
        lea     CLUT,a1
        move.w  #255,d0 
.fullpal:
        move.w  (a0)+,(a1)+
        dbra    d0,.fullpal
;
        move.w  #255,d0
.chkp:
        move.w  -(a1),d1
        cmp.w   -(a0),d1
        bne     .repalle
        dbra    d0,.chkp
        move.w  #-1,CLUT+(2*$ff)
;
        move.l  #service,davesvec
        bra     service
;               
;
;
cdgdo:
        move.w  cdgmode,d0
        cmpi.w  #1,d0
        bne     nvlmkill                ;only ch #1 will be kill of VLM
;
;
        move.l  #$9800317e,$f03160      ;this cheap magick will shut-down GPU
;
;
.gpuwait:
        move.l  G_CTRL,d0
        lsr.w   #1,d0
        bcs     .gpuwait
;
        move.l  #$980030be,$f03160      ;restore to former self
;
;       move.l  #$c00,d2        ;compute screen addr
;       add.l   cscreen,d2      ;get base addr here
;       add.l   hango,d2        ;add offset
;
cdg_scrn        equ     $100000
;
;       move.l  #GPU_S,a0
;       move.l  #GPU_E,d0
;       sub.l   a0,d0
;       asr.l   #2,d0
;       lea     $f03000,a1
;.copy:
;       move.l  (a0)+,(a1)+
;       dbra    d0,.copy
;
;       move.l  #cdg_scrn,G_RAM+4
;
;       move.l  #gpustart,G_PC
;
;       move.l  #1,G_CTRL
;
;  Set-up davelist
;
        tst.w   daveonce        ;only do this once
        bne.s   only1
;
        moveq   #$60,d1
        add.l   dlist,d1        ;get link ptr for davelist
        lea     davelist,a0
;
        move.l  #cdg_scrn,d2
        andi.l  #$00fffff8,d2   
        lsl.l   #8,d2           ;get image ptr
        move.l  d2,(a0)
;
        move.l  2(a0),d2
        andi.w  #$ff,d2
        lsl.l   #5,d1
        andi.l  #$07ffff00,d1
        or.l    d1,d2
        move.l  d2,2(a0)
;
only1:
;
;
        bsr     initgpu         ;set-up Pradip
;
;  Need to tell GPU the following addresses before we start-up..
;
;       davelist
;       dlist
;       blist
;       Pradip
;       frames
;
        move.l  #davelist,d0
        move.w  d0,d1                   ;must be long-word aligned
        andi.w  #3,d1
        beq.s   islongw
        move.l  d0,d1
        andi.w  #~3,d1
        move.l  d1,a3
        move.l  d0,a0
        move.l  d1,d0
        tst.w   daveonce
        bne.s   islongw
;
        move.w  #-1,daveonce            ;this should only happen once
;       
        move.l  (a0)+,(a3)+
        move.l  (a0)+,(a3)+
        move.l  (a0)+,(a3)+
        move.l  (a0)+,(a3)+
islongw:
        lea     $f03020,a0              ;GPU addresses here
        move.l  d0,(a0)+                ;long-aligned davelist
        move.l  dlist,(a0)+
        move.l  blist,(a0)+
        move.l  #Pradip,(a0)+
        move.l  #frames,(a0)+
;
        move.l  #1,G_CTRL               ;start up GPU
;
        bsr     initcdg
;
;
.repalle:
        lea     rgbpal,a0
        lea     CLUT,a1
        move.w  #255,d0 
.fullpal:
        move.w  (a0)+,(a1)+
        dbra    d0,.fullpal
;
        move.w  #255,d0
.chkp:
        move.w  -(a1),d1
        cmp.w   -(a0),d1
        bne     .repalle
        dbra    d0,.chkp
;
;
;
        move.l  #Dframe,$100    ;use Dave's v_blank
;
        move.w  #$6c7,VMODE             ;go into RGB mode
nvlmkill:
        move.w  cdgmode,d0
        move.b  d0,TvChnlNo     ;set our channel here

        rts
;
daveonce:
        dc.w    0
;
;  arrive here if erroneous entry needs deleting
;
wedele:
        moveq   #-1,d5
        bra.s   werowBi
;
;  arrive here if B-button on trk grid in program mode...
;
werowB:
        moveq   #0,d5           ;indicate normal "B" button delete
werowBi:
;*********added 22-Jan-95
        move.w  gridsize,d2     ;see if we're trying to delete last
        cmpi.w  #2,d2
        bcs     trkfnx          ;br if attempt to delete last--don't allow
;*********end addition
;
        move.w  m3row,d2        ;find entry in trkseq we are deleting
        subq.w  #3,d2
        mulu    #10,d2
        add.w   gridoff,d2
        add.w   m3col,d2        ;d2 = entry in trkseq to delete
        lea     trkseq,a0
        bra.s   trkfni
;
trkfnlp:
        tst.b   (a0)+
        beq     trkfnx          ;exit if we went beyond end
trkfni:
        dbra    d2,trkfnlp
;
        tst.b   (a0)            ;were we just pointing to endblank?
        beq.s   trkback         ;if so, just back up the box
;
        sub.w   #1,gridsize     ;# of tracks for trk grid purposes
;
        moveq   #1,d0
        tst.w   trksptr
        bmi.s   .negtrkp
;
        move.l  a0,d0
        sub.l   #trkseq,d0      ;check for trksptr update
        sub.w   trksptr,d0
.negtrkp:
;
        move.l  a0,a5
        addq.l  #1,a5
        moveq   #-1,d2
trkfnlp1:
        addq.w  #1,d2
        move.b  (a5)+,(a0)+
        bne.s   trkfnlp1
;
        tst.w   d0
        bmi.s   .decptr
        bne.s   .nodec
        movem.l d2/d5,-(sp)
        bsr     trkptrfx        ;we deleted current trk-- find new one
        movem.l (sp)+,d2/d5
        bra.s   .nodec
.decptr:
        subi.w  #1,trksptr      ;just decrement by 1
.nodec:
;
        tst.w   d2              ;did we delete last in list?
        bne.s   trknfol         ;br if not
;
        tst.w   d5
        bne.s   trknfol         ;are we just cancelling bad entry?
;
;  deleted last in list--move select box to new last
;
trkback:
        clr.w   blinkon
        bsr     ringit          ;turn off select box
;
        move.w  m3col,d2        ;move box to left
        subq.w  #1,d2
        bpl.s   .boxleft
;
        move.w  m3row,d2        ;see if we can move up a row?
        subi.w  #1,d2
        cmpi.w  #3,d2
        beq.s   .rowup          ;br if was on bottom row
;
; we were on top row
;
        move.w  gridoff,d2
        sub.w   #10,d2
        bmi.s   trknfol
        move.w  d2,gridoff
        bne.s   .rownup
        bsr     planoffu        ;turn off up arrows
        bra.s   .rownup
.rowup:
        move.w  d2,m3row
.rownup:
        move.w  #9,d2
.boxleft:
        move.w  d2,m3col        
trknfol:
        bsr     uprantrk        ;display the new tracks
;
        move.w  #-1,blinkon
        bsr     ringit          ;restore select box
;
        move.w  gridsize,d0
        sub.w   gridoff,d0
        cmpi.w  #21,d0
        bcc.s   trkfnx
;
        bsr     planoffd        ;turn off down arrows
trkfnx:
        rts
;---------------------------------------------------
;
;  put up the program mode screen
;
;rectv  equ     42
rectv   equ     27
;
gridv:
        dc.w    $140-rectv      ;NTSC
        dc.w    $140-rectv+$60  ;PAL
;
;
;
progup:
        movem.l a0/a5/d0-d5,-(sp)
;
;  Now install the object
;
        lea     davesobj+$40,a0
        move.w  #$38,(a0)               ;horz position
;       move.w  #$140-rectv,$4(a0)      ;vert position
        move.w  pal,d0
        add.w   d0,d0
        move.w  gridv(pc,d0.w),$4(a0)   ;vert position
;
;       move.w  #$140-rectv,$4(a0)      ;vert position
;
        move.w  #1,$14(a0)              ;rmw or transparent
        move.w  #0,$16(a0)              ;palette index
        move.w  #$20,$18(a0)            ;DWIDTH
        move.w  #$60,$1A(a0)            ;height
        move.w  #3,$1C(a0)              ;pixel depth
;
        move.l  #gridbase,$10(a0)       ;this one might get munged
        move.w  #6,$c(a0)               ;turn on the object
;
        moveq   #0,d0                   ;fill data set for clear
;       move.l  #$12121212,d0           ;fill data set for color

        move.w  #0,d1                   ;x
;
;       move.w  #rectv,d2               ;y
        move.w  #17,d2
;
        move.w  #256,d3                 ;w
        move.w  #$60-17,d4              ;h
        bsr     fillbox                 ;clear buffer
;
        move.l  #$04040404,d0           ;fill data set for opaque black
        move.w  #0,d1                   ;x
        move.w  #rectv,d2               ;y
        move.w  #243,d3                 ;w
        move.w  #63,d4                  ;h
        bsr     fillbox                 ;clear buffer
;
        move.l  #$02020202,d0           ;fill data set for lite tan
        move.w  #1,d1                   ;x
        move.w  #rectv+1,d2             ;y
        move.w  #1,d3                   ;w
        move.w  #61,d4                  ;h
        moveq   #10,d5
.vlines:
        bsr     fillbox                 ;clear buffer
        add.w   #24,d1
        dbra    d5,.vlines
;
        move.w  #1,d1                   ;x
        move.w  #rectv+1,d2             ;y
        move.w  #240,d3                 ;w
        move.w  #1,d4                   ;h
        moveq   #2,d5
.hlines:
        bsr     fillbox                 ;clear buffer
        add.w   #30,d2
        dbra    d5,.hlines
;
        bsr     trkptrfx        ;find current track seq ptr
;
        bsr     uprantrk        ;display the tracks
;
        move.w  gridsize,d0
        cmpi.b  #21,d0
        bcs.s   nodwnar
;
        sub.w   gridoff,d0
        cmpi.w  #21,d0
        bcs.s   .skipdwn
        bsr     plantard        ;down arrows
.skipdwn:
        tst.w   gridoff
        beq.s   nodwnar
        bsr     plantaru        ;do up arrows
nodwnar:
;29-Mar-95      clr.w   gridoff         ;grid offset is at top
        bsr     chngdisp
        movem.l (sp)+,a0/a5/d0-d5
        rts     
;
;
;
;  Update track sequence display
;
uprantrk:
        moveq   #3,d4
        cmp.w   our_mode,d4
        bne.s   outuptrk        ;skip if not in mode=3

        lea     trkseq,a5
        adda.w  gridoff,a5
;
        moveq   #19,d4
        move.l  #gridbase+((rectv-13)*256),d3
;
        moveq   #4,d2
uptrker:
        moveq   #0,d0
        move.b  (a5)+,d0
        beq     uptrkx
;
        cmpi.b  #-1,d0          ;special way to get a zero thru
        bne.s   uptrxx
        moveq   #0,d0
uptrxx:
        bsr     hex2bcd 
        moveq   #$b,d1
        move.w  d2,randtrk
        cmpi.b  #10,d0
        bcc.s   uptrkr
        sub.w   #2,randtrk
uptrkr:
        bsr     gridigit
        add.w   #24,d2
        cmpi.w  #10,d4
        bne.s   not2ndr
        addi.l  #30*256,d3
        moveq   #4,d2
not2ndr:
        dbra    d4,uptrker
outuptrk:
        rts
;
;  got some blanks to put in at the end..
;
uptrkx:
        moveq   #19,d5
        sub.w   d4,d5

        move.l  #$04040404,d0           ;fill data set for opaque black
        move.w  #22,d3                  ;w
.blnklp:
        move.w  d5,d1
        move.w  #rectv+2,d2             ;y assume top row
        cmpi.w  #10,d5
        bcs.s   .botrow
        add.w   #30,d2                  ;y
        sub.w   #10,d1
.botrow:
        mulu    #24,d1
        add.w   #2,d1                   ;x
        movem.l d0/d3-d5,-(sp)
        move.w  #28,d4                  ;h
        bsr     fillbox                 ;clear buffer
        movem.l (sp)+,d0/d3-d5
        addq.w  #1,d5
        dbra    d4,.blnklp
        rts
;
;
;
;  d0 = fill data
;  d1 = x
;  d2 = y
;  d3 = w
;  d4 = h
;
fillbox:
        move.l  d5,-(sp)
        lea     A1_BASE,a0
.bwait:
        move.l  B_CMD-A1_BASE(a0),d5    ;wait til blitter free
        lsr.w   #1,d5
        bcc     .bwait
;
        move.l  d0,B_PATD-A1_BASE(a0)   ;set pattern regs w/fill data
        move.l  d0,B_PATD+4-A1_BASE(a0)
;
        move.l  #gridbase,(a0)          ;set DST base
;
        move.l  #PITCH1|WID256|XADDPIX|PIXEL8,A1_FLAGS-A1_BASE(a0)      ;dst
        move.w  d2,d5
        swap    d5
        move.w  d1,d5
        move.l  d5,A1_PIXEL-A1_BASE(a0) ;dst (y,x)
;
        move.w  d4,d5
        swap    d5
        move.w  d3,d5
        move.l  d5,B_COUNT-A1_BASE(a0)          ;h:xx, w:1
        moveq   #0,d5
        move.w  d3,d5
        neg.w   d5
        bset.l  #16,d5
        move.l  d5,A1_STEP-A1_BASE(a0)
        move.l  #PATDSEL|UPDA1,B_CMD-A1_BASE(a0)        ;write opaque black
        move.l  (sp)+,d5
        rts
;
;
;
;  Build some displayable arrows
;
;   16 bytes horz x 7 lines high
;
arrbuild:
        lea     arrowfrm,a0
        move.l  a0,d5
        add.l   #12,d5
        andi.w  #~7,d5
        move.l  d5,(a0)
        move.l  d5,a0
        lea     uparrow,a5
;
        moveq   #6,d3
arcollp:
        move.l  (a5)+,d0
        move.w  #15,d1          
arowlp:
        rol.l   #2,d0
        move.l  d0,d2
        andi.w  #3,d2
        move.b  arrcol(pc,d2.w),(a0)+
        dbra    d1,arowlp
        dbra    d3,arcollp
;
        move.l  a0,a5
        suba.w  #16,a0
        moveq   #6,d0
arup:
        move.l  (a0)+,(a5)+
        move.l  (a0)+,(a5)+
        move.l  (a0)+,(a5)+
        move.l  (a0)+,(a5)+
        suba.w  #32,a0
        dbra    d0,arup
;
        rts
;
;
;               trans,fg,n/a, black
arrcol:
        dc.b    0,$22,0,4
;
;
;                v v v v v v v v v v v v v
uparrow:
        dc.l    %00000000000011000000000000
        dc.l    %00000000001101110000000000
        dc.l    %00000000110101011100000000
        dc.l    %00000011010101010111000000
        dc.l    %00001101010101010101110000
        dc.l    %00110101010101010101011100
        dc.l    %11111111111111111111111111
;
;  Turn off down arrows
;
planoffd:
        movem.l d0-d4,-(sp)
        move.w  #rectv+63,d2            ;y
        bra.s   filby
;
;  Turn off down arrows
;
planoffu:
        movem.l d0-d4,-(sp)
        move.w  #rectv-6,d2             ;y
filby:
        moveq   #0,d0                   ;fill data set for transparent
        move.w  #0,d1                   ;x
        move.w  #256,d3                 ;w
        move.w  #6,d4                   ;h
        bsr     fillbox
        movem.l (sp)+,d0-d4
        rts
;
;
plantard:
        move.w  #rectv+63,d0            ;vert pos
        move.l  #8*16,d2
        bra.s   plant   
;
plantaru:
        move.w  #rectv-6,d0             ;vert pos
        moveq   #0,d2
plant:
        swap    d0
        move.w  #5,d0
        lea     A1_BASE,a0
.bwait:
        move.l  B_CMD-A1_BASE(a0),d1    ;wait til blitter free
        lsr.w   #1,d1
        bcc     .bwait
;
        lea     arrowfrm,a3
        add.l   (a3),d2
        move.l  d2,A2_BASE-A1_BASE(a0)          ;set SRC base
        move.l  #gridbase,(a0)                  ;set DST base
;
        move.l  #PITCH1|WID16|XADDPIX|PIXEL8,A2_FLAGS-A1_BASE(a0)       ;src
        move.l  #PITCH1|WID256|XADDPIX|PIXEL8,A1_FLAGS-A1_BASE(a0)      ;dst
;
        moveq   #9,d2
multar:
        move.l  #0,A2_PIXEL-A1_BASE(a0)         ;src = (x,y)
        move.l  d0,A1_PIXEL-A1_BASE(a0)         ;dst = (x,y)
        addi.w  #$18,d0
        move.l  #(6*$10000)+16,B_COUNT-A1_BASE(a0)      ;h:xx, w:xx
        move.l  #2*$10000-16,d1
        move.l  d1,A1_STEP-A1_BASE(a0)
        move.l  d1,A2_STEP-A1_BASE(a0)
        move.l  #UPDA2|UPDA1|SRCEN|LFU_S|DSTEN,B_CMD-A1_BASE(a0)
;
.bwait1:
        move.l  B_CMD-A1_BASE(a0),d1    ;wait til blitter free
        lsr.w   #1,d1
        bcc     .bwait1
;
        dbra    d2,multar
        rts
;
;====================================================================
;
ringit:
;
;  test code
        move.w  m3row,d0
        cmpi.w  #2,d0
        bne.s   .ring0
.ringo:
        illegal
.ring0:
        cmpi.w  #5,d0
        bcc     .ringo
;
        moveq   #0,d2
        move.b  mcolx(pc,d0.w),d2
        cmp.w   m3col,d2
        bcs     .ringo
        bra     ring1
mcolx:
        dc.b    4,2,0,9,9,0
;
ring1:
;
;  end test
;
;
        lea     A1_BASE,a0
.bwait:
        move.l  B_CMD-A1_BASE(a0),d0    ;wait til blitter free
        lsr.w   #1,d0
        bcc     .bwait
;
        move.l  #$04040404,d0
        not.w   blinkon
        bmi.s   .blone
        move.l  #$ffffffff,d0
.blone:
        move.l  d0,B_PATD-A1_BASE(a0)   ;set pattern regs
        move.l  d0,B_PATD+4-A1_BASE(a0)
;
        move.l  #frntbase,(a0)          ;set DST base
;
        move.l  #PITCH1|WID256|XADDPIX|PIXEL8,A1_FLAGS-A1_BASE(a0)      ;dst
;
        move.w  m3row,d0                ;check row
        cmpi.w  #2,d0
        bcs.s   .uppr
        beq.s   .selbox
;
;  on track grid
;
        moveq   #6,d0
        add.w   m3col,d0
        bra.s   .tgrid
.selbox:
        move.l  #20*$10000+24,d0
        bra.s   .selboxy
.uppr:
        lsl.w   #4,d0
        or.w    m3col,d0
.tgrid:
        lsl.w   #2,d0
        move.l  DSTXW(pc,d0.w),d0       ;get DST (x,w)
.selboxy:
        move.w  m3row,d2
        lsl.w   #2,d2
        move.l  DSTYH(pc,d2.w),d2
;
        move.l  d2,d3
        swap    d0
        move.w  d0,d3                   ;d3 = (y,x)
        swap    d0
        swap    d2
        move.w  d0,d2                   ;d2 = (h,w)
        bra.s   owdata
;
butseq  equ     24-7
butrept equ     93-7
butcdg  equ     180-7   
;
;butseq equ     24
;butrept        equ     93
;butcdg equ     180     
;
;  Dst xpos,width
;
DSTXW:
        dc.w    70,29   ;0 play
        dc.w    99,39   ;1 FF
        dc.w    138,31  ;2 pause
        dc.w    2,30    ;3 stop
        dc.w    32,38   ;4 Rew
        dc.w    0,0
;
        dc.w    (0*24)+2,22     ;bottom row (trk grid)
        dc.w    (1*24)+2,22
        dc.w    (2*24)+2,22
        dc.w    (3*24)+2,22
        dc.w    (4*24)+2,22
        dc.w    (5*24)+2,22
        dc.w    (6*24)+2,22
        dc.w    (7*24)+2,22
        dc.w    (8*24)+2,22
        dc.w    (9*24)+2,22
;
        dc.w    butseq+3,56-6   
        dc.w    butrept+3,73-6
        dc.w    butcdg+3,56-6
;
;
DSTYH:
        dc.l    (14*$10000)+32          ;get DST (y,h) constants
;       dc.l    (57*$10000)+17-4
;       dc.l    (60*$10000)+17-4
        dc.l    ((frnthite+2)*$10000)+17-4

        dc.l    0       
        dc.l    ((frnthite+rectv+2)*$10000)+29
        dc.l    ((frnthite+rectv+32)*$10000)+29
;
;
owdata:
        move.l  d3,A1_PIXEL-A1_BASE(a0) ;dst (y,x)
        move.l  d2,d0                   ;dst (h,w)
        move.w  #1,d0
;
        move.l  d0,B_COUNT-A1_BASE(a0)  ;h:xx, w:1
        ext.l   d0
        neg.w   d0
        bset.l  #16,d0
        move.l  d0,A1_STEP-A1_BASE(a0)

        move.l  #PATDSEL|UPDA1,B_CMD-A1_BASE(a0)
;       
.bwait1:
        move.l  B_CMD-A1_BASE(a0),d0    ;wait til blitter free
        lsr.w   #1,d0
        bcc     .bwait1
;
        move.l  d3,A1_PIXEL-A1_BASE(a0) ;dst (y,x)
        move.l  d2,d0                   ;dst (h,w)
        ext.l   d0
        bset.l  #16,d0
;
        move.l  d0,B_COUNT-A1_BASE(a0)  ;h:1, w:xx
        ext.l   d0
        neg.w   d0
        bset.l  #16,d0
        move.l  d0,A1_STEP-A1_BASE(a0)

        move.l  #PATDSEL|UPDA1,B_CMD-A1_BASE(a0)
;       
.bwait2:
        move.l  B_CMD-A1_BASE(a0),d0    ;wait til blitter free
        lsr.w   #1,d0
        bcc     .bwait2
;
        swap    d3
        swap    d2
        add.w   d2,d3
        subq.w  #1,d3
        swap    d3
        swap    d2
        move.l  d3,A1_PIXEL-A1_BASE(a0) ;dst (y+h-1,x)
        move.l  d2,d0                   ;dst (h,w)
        ext.l   d0
        bset.l  #16,d0
        move.l  d0,B_COUNT-A1_BASE(a0)  ;h:1, w:xx
        ext.l   d0
        neg.w   d0
        bset.l  #16,d0
        move.l  d0,A1_STEP-A1_BASE(a0)

        move.l  #PATDSEL|UPDA1,B_CMD-A1_BASE(a0)
;       
.bwait3:
        move.l  B_CMD-A1_BASE(a0),d0    ;wait til blitter free
        lsr.w   #1,d0
        bcc     .bwait3
;
        swap    d2
        swap    d3
        sub.w   d2,d3
        add.w   #1,d3
        swap    d2
        swap    d3
        add.w   d2,d3
        subq.w  #1,d3
        move.l  d3,A1_PIXEL-A1_BASE(a0) ;dst (y,x+w-1)
        move.l  d2,d0                   ;dst (h,w)
        move.w  #1,d0
        move.l  d0,B_COUNT-A1_BASE(a0)  ;h:1, w:xx
        ext.l   d0
        neg.w   d0
        bset.l  #16,d0
        move.l  d0,A1_STEP-A1_BASE(a0)

        move.l  #PATDSEL|UPDA1,B_CMD-A1_BASE(a0)
;
        rts
;
;
;
;
;   call this to put up stop display (max track, max time)
;
stopout:
        move.w  maxmin,d0
        andi.w  #$ff,d0
        bsr     hex2bcd
        moveq   #1,d1
        bsr     printout        ;put up max track
;
        moveq   #0,d0
        move.b  fineEND+1,d0    ;put up max mins
        moveq   #3,d1
        bsr     printout
;
        moveq   #0,d0
        move.b  fineEND+2,d0    ;put up max secs
        moveq   #4,d1
        bsr     printout
        rts
;
;
;
;
;
;
;  Raise or Lower volume one tick based on Ubit set in d2.l
;    if set, raise volume, else lower
;    uses d0.l & d1.l
;
volupdwn:
        clr.w   voltimr         ;we were already on
        moveq   #24,d0
        btst.l  #Ubit,d2
        bne.s   ud1
        neg.l   d0
ud1:
        add.l   d0,davesobj+$90
        moveq   #0,d0
        move.w  volptr,d0
        addq.w  #1,d0
        btst.l  #Ubit,d2
        bne.s   weup
        subq.w  #2,d0
        bpl.s   weup
        moveq   #0,d0
weup:
        cmpi.w  #65,d0
        bcs.s   not2hi
        moveq   #64,d0
not2hi:
        move.w  d0,volptr
        lea     voltab,a0
        add.w   d0,d0
        moveq   #0,d1
        move.w  (a0,d0.w),d1
        move.l  d1,VOLUME
        mulu    #12,d0
        add.l   #VOLbar,d0
        move.l  d0,davesobj+$90
        bsr     chngdisp
        rts
;
;  Entry point to put in Bank/Effect or cd+g Channel # on rmw digit screen
;
;   Put out the bank & effect
;
bankout:
        movem.l d0-d4/a1/a5,-(sp)
        lea     frntbase,a5             ;base address of dst form               
        move.w  our_mode,d2             ;check mode 1st
        cmpi.w  #2,d2                   ;only rmw-digit mode
        bne     printx
;
        tst.w   cdgmode                 ;are we in cd+g mode?
        beq.s   banko                   ;br if not
;
        moveq   #0,d0
        move.b  TvChnlNo,d0             ;else, put up the cd+g channel #
        cmpi.w  #10,d0                  ;convert binary nibble to bcd
        bcs.s   cdgbcd
        subi.w  #10,d0
        addi.w  #$10,d0
cdgbcd:
        moveq   #5,d1
        moveq   #10,d4  
        bra.s   blnumb
banko:
        move.w  disbank,d4
        addq.w  #1,d4
        andi.w  #$f,d4
        lsl.w   #4,d4
        move.w  diseff,d0
        andi.w  #$f,d0
        or.w    d4,d0
        moveq   #0,d1

        moveq   #17,d4                  ;digit spacing
        bra.s   blnumb
;
;  Entry point for putting track digits in Random/Program grid
;
;  d0.b = bcd to print
;  d1   = offset in subcodes (if odd, do leading zero suppression)
;
gridigit:
        movem.l d0-d4/a1/a5,-(sp)
        lea     trkseq+1,a0
        adda.w  trksptr,a0
        lea     $61000,a1               ;use lite green
        cmp.l   a0,a5
        bne.s   .grf
        tst.w   play                    ;are we in play mode?
        beq.s   .grf                    ;br if not
        lea     $60000,a1               ;use standard green
.grf:
        move.l  d3,a5
        bra.s   pox
;
;  Entry point for putting track/min/sec digits in trk/time display
;
;  d0.b = bcd to print
;  d1   = offset in subcodes (if odd, do leading zero suppression)
;
;       
printout:
        movem.l d0-d4/a1/a5,-(sp)
        lea     frntbase,a5             ;base address of dst form               
        lea     $60000,a1               ;use standard green
pox:
        moveq   #10,d4                  ;digit spacing
;
;       move.w  our_mode,d2             ;check mode 1st
;       beq     printx
;
        tst.w   our_mode
        beq     printx
;
        bra.s   blnumbb
;
; blit up the numbers
;
blnumb:
        lea     $60000,a1
blnumbb:        
        btst.l  #0,d1
        beq.s   prinx
        cmp.b   #10,d0
        bcc.s   prinx
        or.b    #$a0,d0
prinx:
        lsl.w   #1,d1
        move.w  offline(pc,d1.w),d1     ;get dst xpos
        beq     printx
;
        swap    d1                      ;dst in high word
        lea     A1_BASE,a0
;
        moveq   #1,d3
reblit:
        ror.b   #4,d0
        move.w  #0,d1
        move.b  d0,d1
        andi.w  #$f,d1
        add.w   d1,d1
        move.w  srcxtab(pc,d1.w),d1     ;d1 = src xpos in lo word
        bra.s   pout
;
;       
sbasex  equ     0
swid    equ     10
swidx   equ     10
shite   equ     17
;
;
;
srcxtab:
        dc.w    3               ;0
        dc.w    $b              ;1
        dc.w    $15             ;2
        dc.w    $1e             ;3
        dc.w    $29             ;4
        dc.w    $33             ;5
        dc.w    $3d             ;6
        dc.w    $46             ;7
        dc.w    $50             ;8
        dc.w    $5a             ;9
        dc.w    $64             ;<sp>
;
;   DST xpos
;
offline:
;
;
        dc.w    34      ;0 Cont/Adr (bank/effect xpos)
        dc.w    173     ;1 TNO
        dc.w    0       ;2 Index
        dc.w    205     ;3 Min
        dc.w    231     ;4 Sec
        dc.w    31      ;5 Frame (cd+g channel)
        dc.w    0       ;6 Zero
        dc.w    0       ;7 Amin
        dc.w    0       ;8 Asec

        dc.w    0       ;9 Aframe
        dc.w    0       ;A CRC
randtrk:
        dc.w    0       ;B CRC (track #'s in rand/prog grid)
        dc.w    0       ;C
        dc.w    0       ;D
        dc.w    0       ;E
        dc.w    0       ;F
;
pout:
;
;       lea     $60000,a1
bltloop:
        move.l  B_CMD-A1_BASE(a0),d2    ;wait til blitter free
        lsr.w   #1,d2
        bcc     bltloop

        move.l  a1,A2_BASE-A1_BASE(a0)  ;set SRC base
        move.l  a5,(a0)                 ;set DST base
;
        move.l  #PITCH1|WID128|XADDPIX|PIXEL8,A2_FLAGS-A1_BASE(a0)      ;src
        move.l  #PITCH1|WID256|XADDPIX|PIXEL8,A1_FLAGS-A1_BASE(a0)      ;dst
;
        moveq   #0,d2           ;ypos
        move.w  d1,d2           ;src y=0, x 
        move.l  d2,A2_PIXEL-A1_BASE(a0)
;
        moveq   #vertdig,d2             ;ypos
        swap    d2
        swap    d1
        move.w  d1,d2           ;dst y=0, x 
        move.l  d2,A1_PIXEL-A1_BASE(a0)
;
        move.l  #(17*$10000)+10,B_COUNT-A1_BASE(a0)     ;w:10, h:17
        move.l  #(2*$10000)-10,d2
        move.l  d2,A1_STEP-A1_BASE(a0)
        move.l  d2,A2_STEP-A1_BASE(a0)
        move.l  #UPDA2|UPDA1|SRCEN|LFU_S|DSTEN,B_CMD-A1_BASE(a0)
;
        add.w   d4,d1           ;advance dst x
        swap    d1

        dbra    d3,reblit
printx:
        movem.l (sp)+,d0-d4/a1/a5
        rts
;
;
;
bankneg:
        move.w  our_mode,d2             ;check mode 1st
        cmpi.w  #2,d2                   ;only rmw-digit mode
        beq     bankneg0
        rts
bankneg0:
        lea     A1_BASE,a0
        move.w  #44,d1                  ;dst
        swap    d1
;       move.w  #112,d1                 ;src
        move.w  #111,d1                 ;src
;
.blwait:
        move.l  B_CMD-A1_BASE(a0),d2    ;wait til blitter free
        lsr.w   #1,d2
        bcc     .blwait

        move.l  #$60000,A2_BASE-A1_BASE(a0)     ;set SRC base
        move.l  #frntbase,(a0)          ;set DST base

        move.l  #PITCH1|WID128|XADDPIX|PIXEL8,A2_FLAGS-A1_BASE(a0)      ;src
        move.l  #PITCH1|WID256|XADDPIX|PIXEL8,A1_FLAGS-A1_BASE(a0)      ;dst
;
        moveq   #0,d2           ;ypos
        move.w  d1,d2           ;src y=0, x 
        move.l  d2,A2_PIXEL-A1_BASE(a0)
;
        moveq   #vertdig,d2             ;ypos
        swap    d2
        swap    d1
        move.w  d1,d2           ;dst y=0, x 
        move.l  d2,A1_PIXEL-A1_BASE(a0)
;
        move.l  #(17*$10000)+7,B_COUNT-A1_BASE(a0)      ;w:6, h:17
        move.l  #(2*$10000)-7,d2
        move.l  d2,A1_STEP-A1_BASE(a0)
        move.l  d2,A2_STEP-A1_BASE(a0)
        move.l  #UPDA2|UPDA1|SRCEN|LFU_S|DSTEN,B_CMD-A1_BASE(a0)
        rts
;
;
;
;
colona  equ     225
;
colons:
        movem.l d0-d4/a1/a5,-(sp)
        lea     A1_BASE,a0
        move.w  #colona,d1                      ;dst
        swap    d1
        move.w  #118,d1 ;src
;
        lea     $60000,a1
        moveq   #1,d3
blwait:
        move.l  B_CMD-A1_BASE(a0),d2    ;wait til blitter free
        lsr.w   #1,d2
        bcc     blwait

        move.l  a1,A2_BASE-A1_BASE(a0)  ;set SRC base
        move.l  #frntbase,(a0)          ;set DST base

        move.l  #PITCH1|WID128|XADDPIX|PIXEL8,A2_FLAGS-A1_BASE(a0)      ;src
        move.l  #PITCH1|WID256|XADDPIX|PIXEL8,A1_FLAGS-A1_BASE(a0)      ;dst
;
        moveq   #0,d2           ;ypos
        move.w  d1,d2           ;src y=0, x 
        move.l  d2,A2_PIXEL-A1_BASE(a0)
;
        moveq   #vertdig,d2             ;ypos
        swap    d2
        swap    d1
        move.w  d1,d2           ;dst y=0, x 
        move.l  d2,A1_PIXEL-A1_BASE(a0)
;
        move.l  #(17*$10000)+6,B_COUNT-A1_BASE(a0)      ;w:6, h:17
        move.l  #(2*$10000)-6,d2
        move.l  d2,A1_STEP-A1_BASE(a0)
        move.l  d2,A2_STEP-A1_BASE(a0)
        move.l  #UPDA2|UPDA1|SRCEN|LFU_S|DSTEN,B_CMD-A1_BASE(a0)
;
        movem.l (sp)+,d0-d4/a1/a5
        rts
;
;
;
;   Use blitter to zero maximum screen memory (@ $4000)..256x50
;
screnclr:
        movem.l a0/d0,-(sp)
        lea     A1_BASE,a0
.bwait:
        move.l  B_CMD-A1_BASE(a0),d0    ;wait til blitter free
        lsr.w   #1,d0
        bcc     .bwait
;
        moveq   #0,d0
        move.l  #frntbase,(a0)          ;set DST base
;
        move.l  d0,B_PATD-A1_BASE(a0)   ;clear pattern regs
        move.l  d0,B_PATD+4-A1_BASE(a0)
;
        move.l  d0,A1_PIXEL-A1_BASE(a0)         ;start at top/left corner

        move.l  #((frnthite+rectv)*$10000)+256,B_COUNT-A1_BASE(a0)

        move.l  #(2*$10000)-256,A1_STEP-A1_BASE(a0)
        move.l  #PITCH1|WID256|XADDPHR|PIXEL8,A1_FLAGS-A1_BASE(a0)
        move.l  #PATDSEL|UPDA1,B_CMD-A1_BASE(a0)
;
.bwait1:
        move.l  B_CMD-A1_BASE(a0),d0    ;wait til blitter free
        lsr.w   #1,d0
        bcc     .bwait1
;
        movem.l (sp)+,a0/d0
        rts
;
;
;
;
;
;
;
;
string:
        move.w  track,d0
        bsr     hex2bcd
        moveq   #1,d1
        bsr     printout        
stringb:
        move.l  a1,-(sp)
        move.w  #$aa,d0
        lea     bxlist,a1
restn:
        moveq   #0,d1
        move.b  (a1)+,d1
        beq.s   stringx
        bsr     printout
        bra     restn
stringx:
        move.l  (sp)+,a1
        rts
;
bxlist:
        dc.b    3,4,7,8,0,0
;
;
;
;
;
;
;   Blit from cdpanin or cdpanout to front panel to effect push button
;
;  0 - RESET
;  1 - REW
;  2 - PLAY
;  3 - FF
;  4 - PAUSE
;
;   d0 = 0..4 for the various functions
;   d1 = 0 for out, 1 for in
;
;pansrc:
;       dc.l    cpanout
;       dc.l    cpanin
;
;
panx:
        dc.w    3
        dc.w    33
        dc.w    71
        dc.w    100
        dc.w    139
panwid:
        dc.w    28      
        dc.w    36
        dc.w    27
        dc.w    37
        dc.w    29
;
;
depressd:
        move.w  our_mode,d2
        cmpi.w  #2,d2
        bne.s   depresok
        rts
depresok:
        move.w  #49,d2          ;assume panel "IN" (SRC ypos)
        tst.w   d1
        bne.s   itspanin
        move.w  #15,d2          ;else, its panel "OUT"
itspanin:
        swap    d1
        move.w  d2,d1
        swap    d1
;
        lea     onepage3,a3
;
        lea     A1_BASE,a0
        add.w   d0,d0
        move.w  panx(pc,d0.w),d1        ;srcx, dstx in d1
        move.w  panwid(pc,d0.w),d0      ;width in d0    
.bwait:
        move.l  B_CMD-A1_BASE(a0),d2    ;wait til blitter free
        lsr.w   #1,d2
        bcc     .bwait
;
        moveq   #0,d2
        move.w  (a3),d2
        add.l   a3,d2
        move.l  d2,A2_BASE-A1_BASE(a0)  ;set SRC base
        move.l  #frntbase,(a0)          ;set DST base
;
        move.l  #PITCH1|WID256|XADDPIX|PIXEL8,A2_FLAGS-A1_BASE(a0)      ;src
        move.l  #PITCH1|WID256|XADDPIX|PIXEL8,A1_FLAGS-A1_BASE(a0)      ;dst
;
        move.l  d1,A2_PIXEL-A1_BASE(a0) ;src = (0,0)
        swap    d1
        move.w  #15,d1                  ;set srcy, dsty
        swap    d1
        move.l  d1,A1_PIXEL-A1_BASE(a0) ;dst
;
        move.w  #30,d1
        swap    d1
        move.w  d0,d1
        move.l  d1,B_COUNT-A1_BASE(a0)  ;w:xx, h:31
;
        move.l  #(2*$10000),d1
        ext.l   d0
        sub.l   d0,d1
        move.l  d1,A1_STEP-A1_BASE(a0)
        move.l  d1,A2_STEP-A1_BASE(a0)
        move.l  #UPDA2|UPDA1|SRCEN|LFU_S|DSTEN,B_CMD-A1_BASE(a0)
;
        rts
;
;
;
;   put up a radio button
;
;
;  d0.w = button #
;  d1.w = 0 if normal, -1 if full button
;
;
xadj    equ     4       ;subtract from dstx,srcx for full
yadj    equ     5       ;subtract from dsty,srcy for full
wadj    equ     8       ;add to width for full
hadj    equ     9       ;add to height for full
;
;
radiob:
        moveq   #3,d2
        cmp.w   our_mode,d2
        beq.s   radiok
        rts
;
;
;
;
; Butn#
;               srcx    srcy    dstx    dsty    wid     hite
; 0 - program   0       82      24      55      56      17
; 1 - normal    64      82      24      55      56      17
; 2 - random    128     82      24      55      56      17
; 3 - 
;
; 4 - rept trk  176     48      100     55      73      17
; 5 - no rept   176     65      100     55      73      17
;
bsrcx:
        dc.w    170     ;0 program (sequence mode)
        dc.w    0+xadj  ;1 normal
        dc.w    170     ;2 random
        dc.w    0       ;3 n/a
;
        dc.w    185     ;4 rep trk (repeat mode)
        dc.w    55+xadj ;5 no rep
        dc.w    185     ;6 rep disc
        dc.w    0       ;7 n/a
;
        dc.w    129+xadj ;8 vlm (vlm/cd+g mode)
        dc.w    170     ;9 cd+g

        dc.w    129+xadj+8 ;a vlm-random (vlm/cd+g mode)
;
;
bsrcy:
        dc.w    49      ;0 program (sequence mode)
        dc.w    82+yadj ;1 normal
        dc.w    58      ;2 random
        dc.w    0       ;3 n/a
;
        dc.w    76      ;4 rep trk (repeat mode)
        dc.w    82+yadj ;5 no rep
        dc.w    85      ;6 rep disc
        dc.w    0       ;7 n/a
;
        dc.w    82+yadj ;8 vlm (vlm/cd+g mode)
        dc.w    67      ;9 cd+g
;
        dc.w    82+yadj ;A vlm-random (vlm/cd+g mode)
;
bdstx:
        dc.w    butseq+xadj,butrept+xadj,butcdg+xadj
bdsty:
;       dc.w    55+yadj,55+yadj,55+yadj
;       dc.w    10+yadj,10+yadj,10+yadj
        dc.w    yadj,yadj,yadj
bwid:
        dc.w    56-wadj,73-wadj,56-wadj
bhite:
        dc.w    17-hadj,17-hadj,17-hadj
;
;
radiok:
        lea     onepage3,a3
        moveq   #0,d4                   ;assume no ch # to append
        cmpi.w  #8,d0                   ;are we cd+g with channels?
        bcs.s   .radb                   ;br if not
        bne.s   .radbx
        tst.w   vlmrand
        beq.s   .radb
        moveq   #-1,d4                  ;tell them post-processing for vlm R
        moveq   #10,d0                  ;this is for vlm-random
        bra.s   .radb
.radbx:
        move.w  d0,d4
        sub.w   #8,d4                   ;d4 = channel # to append
        moveq   #9,d0                   ;else, just set for ch 1
.radb:
        lea     A1_BASE,a0
        add.w   d0,d0
.bwait:
        move.l  B_CMD-A1_BASE(a0),d2    ;wait til blitter free
        lsr.w   #1,d2
        bcc     .bwait
;
        moveq   #0,d2
        move.w  (a3),d2
        add.l   a3,d2
        move.l  d2,A2_BASE-A1_BASE(a0)  ;set SRC base
        move.l  #gridbase,(a0)          ;set DST base
;
        lea     bsrcx(pc),a3
;
        move.l  #PITCH1|WID256|XADDPIX|PIXEL8,A2_FLAGS-A1_BASE(a0)      ;src
        move.l  #PITCH1|WID256|XADDPIX|PIXEL8,A1_FLAGS-A1_BASE(a0)      ;dst
;

        move.w  bsrcy-bsrcx(a3,d0.w),d3
        swap    d3
        move.w  bsrcx-bsrcx(a3,d0.w),d3
        tst.w   d1
        beq.s   .normsxy
        sub.w   #xadj,d3
        swap    d3
        sub.w   #yadj,d3
        swap    d3
.normsxy:
        move.l  d3,A2_PIXEL-A1_BASE(a0) ;src = (x,y)
;
        lsr.w   #3,d0
        add.w   d0,d0
;
        move.w  bdsty-bsrcx(a3,d0.w),d3
        swap    d3
        move.w  bdstx-bsrcx(a3,d0.w),d3
        tst.w   d1
        beq.s   .normdxy
        sub.w   #xadj,d3
        swap    d3
        sub.w   #yadj,d3
        swap    d3
.normdxy:
        move.l  d3,A1_PIXEL-A1_BASE(a0) ;dst = (x,y)
;
        move.w  bhite-bsrcx(a3,d0.w),d3
        swap    d3
        move.w  bwid-bsrcx(a3,d0.w),d3
        tst.w   d1
        beq.s   .normwh
        add.w   #wadj,d3
        swap    d3
        add.w   #hadj,d3
        swap    d3
.normwh:
        move.l  d3,B_COUNT-A1_BASE(a0)  ;h:xx, w:xx
;
        move.l  #(2*$10000),d0
        ext.l   d3
        sub.l   d3,d0
        move.l  d0,A1_STEP-A1_BASE(a0)
        move.l  d0,A2_STEP-A1_BASE(a0)
        move.l  #UPDA2|UPDA1|SRCEN|LFU_S|DSTEN,B_CMD-A1_BASE(a0)
;
        tst.w   d4
        bne.s   appcha
        rts
;
;  Append channel #, given in d4
;
appcha:
        bmi     appR            ;br if we append vlm-random's "R"
; 
        move.l  #(5*$10000)+butcdg+37,d1        ;dst = (x,y)
.bwait:
        move.l  B_CMD-A1_BASE(a0),d2    ;wait til blitter free
        lsr.w   #1,d2
        bcc     .bwait
;
        move.w  d4,d3
        moveq   #-1,d4
        cmpi.w  #10,d3                  ;>= 10?
        bcs.s   .not10
        subi.w  #10,d3
        move.w  d3,d4
        moveq   #1,d3
.not10:
        mulu    #6,d3
        add.w   #185,d3
        swap    d3
        move.w  #94,d3                  ;get srcy
        swap    d3
;
        move.l  d3,A2_PIXEL-A1_BASE(a0) ;src = (x,y)
        move.l  d1,A1_PIXEL-A1_BASE(a0) ;dst = (x,y)
        move.l  #(8*$10000)+5,B_COUNT-A1_BASE(a0)       ;h:xx, w:xx
;
        move.l  #(2*$10000)-5,d0
        move.l  d0,A1_STEP-A1_BASE(a0)
        move.l  d0,A2_STEP-A1_BASE(a0)
        move.l  #UPDA2|UPDA1|SRCEN|LFU_S|DSTEN,B_CMD-A1_BASE(a0)
        addq.l  #6,d1
        tst.w   d4
        bpl     .bwait
        rts
;
;  Append an "R" to the VLM message
;
appR:
.bwait:
        move.l  B_CMD-A1_BASE(a0),d2    ;wait til blitter free
        lsr.w   #1,d2
        bcc     .bwait
;
        move.l  #(58*$10000)+173,A2_PIXEL-A1_BASE(a0)   ;src = (x,y)
        move.l  #(5*$10000)+butcdg+37,A1_PIXEL-A1_BASE(a0)      ;dst = (x,y)
        move.l  #(8*$10000)+10,B_COUNT-A1_BASE(a0)      ;h:xx, w:xx
;
        move.l  #(2*$10000)-10,d0
        move.l  d0,A1_STEP-A1_BASE(a0)
        move.l  d0,A2_STEP-A1_BASE(a0)
        move.l  #UPDA2|UPDA1|SRCEN|LFU_S|DSTEN,B_CMD-A1_BASE(a0)

.bwaitx:
        move.l  B_CMD-A1_BASE(a0),d2    ;wait til blitter free
        lsr.w   #1,d2
        bcc     .bwaitx
;
        move.l  #((82+yadj)*$10000)+129+xadj+38,A2_PIXEL-A1_BASE(a0)    ;src = (x,y)
        move.l  #(5*$10000)+butcdg+47,A1_PIXEL-A1_BASE(a0)      ;dst = (x,y)
        move.l  #(8*$10000)+5,B_COUNT-A1_BASE(a0)       ;h:xx, w:xx
;
        move.l  #(2*$10000)-5,d0
        move.l  d0,A1_STEP-A1_BASE(a0)
        move.l  d0,A2_STEP-A1_BASE(a0)
        move.l  #UPDA2|UPDA1|SRCEN|LFU_S|DSTEN,B_CMD-A1_BASE(a0)
;
        rts

;
;  Turn on help or turn it off
;
onhelp:
        moveq   #6,d6
        bra.s   onffx
offhelp:
        moveq   #-1,d6
onffx:
        moveq   #1,d5
        cmp.w   our_mode,d5
        beq.s   onoffh
        rts
onoffh:
;       lea     cdstatus,a0

        lea     onepage3,a0             ;test

        moveq   #0,d5
        move.w  (a0),d5
        add.l   a0,d5
        lea     davesobj+$40,a0
;
        move.l  d5,$10(a0)              ;set new form
;
        move.w  #$42,(a0)       ;horz pos
        move.w  #200,4(a0)      ;vert pos
;
;       move.w  #28,$18(a0)     ;DWIDTH
;       move.w  #85,$1a(a0)     ;height
;
        move.w  #32,$18(a0)     ;DWIDTH
        move.w  #108,$1a(a0)    ;height

        move.w  #1,$14(a0)      ;transparent
        move.w  d6,$c(a0)       ;turn on/off
        rts
;
;----------------------------------------------------
;
;  Send command to DSA bus
;   and receive 0,1 or multiple words in return
;
;       
;
;  entry:
;    d0 = command code to send
;    d1 = # of returned words expected
;
;    a3 -> buffer for returned words
;
;  exit:
;    d2 = last of returned words
;
;
DSA_tx:
        move.l  a3,-(sp)
        lea     return,a3
        move.l  (a4),d2                 ;check receive buffer full
        btst.l  #13,d2
        beq     .txready                ;br if receive buffer clear
;
        move.w  DS_DATA(a4),d2          ;else, get bogus receive stuff
        tst.l   DSCNTRL(a4)             ;read to clear interrupt flag
.txready:
        move.w  d0,DS_DATA(a4)          ;send command word
.txwait:
        bra.s   .txcount                
.txwait1:
        moveq   #15,d2                  ;this delay helps
.txdelay0:
        dbra    d2,.txdelay0
;
        move.l  (a4),d2                 ;wait til receive buffer full
        btst.l  #13,d2
        beq     .txwait1
;
        move.w  DS_DATA(a4),d2
        move.w  d2,(a3)+                ;get response, stuff in return buffer
        tst.l   DSCNTRL(a4)             ;read to clear interrupt flag
.txcount:
        dbra    d1,.txwait1
        move.l  (sp)+,a3
;
        rts
;
;
;  Build normal sequential track order
;
buildseq:
        move.w  maxmin,d6
        move.w  d6,d0
        lsr.w   #8,d0                   ;starting track #
        andi.w  #$ff,d6                 ;max track #
;
        lea     trkseq,a0               ;put in sequential track #'s
        move.l  a0,a3                   ;save for later
randlp:
        move.b  d0,(a0)+
        addq.w  #1,d0
        cmp.w   d6,d0
        bls     randlp
        clr.b   (a0)                    ;put in a terminator
;
        clr.w   trksptr                 ;current track is 0th entry
        rts
;
;  Build normal sequential track order for user programmable sequence too
;
bildprog:
        move.w  maxmin,d6
        move.w  d6,d0
        lsr.w   #8,d0                   ;starting track #
        andi.w  #$ff,d6                 ;max track #
;
        lea     progseq,a0              ;put in sequential track #'s
        move.l  a0,a3                   ;save for later
proglp:
        move.b  d0,(a0)+
        addq.w  #1,d0
        cmp.w   d6,d0
        bls     proglp
        clr.b   (a0)                    ;put in a terminator
;
        rts
;
;
;   Build a shuffle track sequence from a random function
;
;
randf:
        bsr     buildseq
        tst.w   d6              ;0 tracks are possible in extern mode
        bne.s   randy
        rts
randy:
;
;   now, shuffle these tracks
;       
;  The following random generator provides exactly the same
;   results as Landon Dyer's version in the ST extended BIOS.
;
        movem.l seed(pc),d0-d1          ;get seed & constant
randloop:
        moveq   #0,d2           ;clear neg flag
rerand:
        tst.l   d0              ;check seed
        bgt.s   ov              ;br if pos, non-zero (seed ok)
        bne.s   ovx             ;br if neg, non-zero (need positive)
;
;  we have a zero seed--can't let this happen
;
        move.w  frames,d0
        swap    d0
        move.w  frames,d0
        not.w   d0
        bra     rerand          ;guaranteed non-zero now                
ovx:
        neg.l   d0              ;make positive
        addq    #1,d2           ;set neg flag
ov:
        move.l  d0,d3
        mulu    d1,d3
;
        move.l  d0,d4
        swap    d4
        mulu    d1,d4
;
        move.l  d1,d5
        swap    d5
        mulu    d5,d0
;
        add.w   d4,d0
        swap    d0
        clr.w   d0
        add.l   d3,d0
        tst.w   d2
        bne.s   ov1
        neg.l   d0
ov1:
        addq.l  #1,d0
        move.l  d0,d5
        lsr.l   #8,d5
        lsr.l   #3,d5
        swap    d5
        clr.w   d5
        swap    d5
;
        divu    d6,d5
        swap    d5
        move.b  (a3,d5.w),d4
        move.b  (a3),(a3,d5.w)
        move.b  d4,(a3)+
        subq.w  #1,d6
        cmpi.w  #1,d6
;
;       bne     randloop
        bhi     randloop        ;**29-Mar-95 because we can have 1 trk CD's
;
        move.l  d0,seed         ;save seed for next time
;
        moveq   #0,d0
        tst.w   play            ;are we playing?
        beq.s   trkplay         ;br if not--just start at beginning
;
;  We're playing--we need to find track we're on in sequence
;
;  on exit, d2 = -1 means we can't find
trkptrfx:
        moveq   #0,d0
        lea     trkseq,a0
        move.w  track,d1
        beq.s   trkplay
;******added 22-Jan-95
;
        move.w  trksptr,d0
        cmp.w   gridsize,d0
        bcc.s   cantuse
        cmp.b   (a0,d0.w),d1            ;was old trksptr good enuf?
        beq.s   trkplay                 ;br if so
cantuse:
        moveq   #-1,d0
;*****end addition
;
chkloop:
        addq.w  #1,d0
        move.b  (a0)+,d2
        beq.s   cantf           
        cmp.b   d2,d1
        bne     chkloop
trkplay:
        move.w  d0,trksptr
        rts
;
cantf:
        moveq   #-1,d2
        moveq   #-1,d0
        bra     trkplay
;
;  Smart track ptr fix: d2 has L or R set according to direction to look
;
trkptrsm:
        lea     trkseq,a0       
        move.w  track,d1
        beq.s   trkplay
        btst.l  #Lbit,d2                ;are we looking left (decrementing)?
        bne.s   trkdeci
        btst.l  #Rbit,d2                ;looking right (incrementing)?
        bne.s   trkinci
        bra     trkptrfx
trkdeci:
        moveq   #-1,d2
        bra.s   trksm
trkinci:
        moveq   #1,d2
trksm:
        move.w  trksptr,d0
        moveq   #0,d3
trksmlp:
        move.b  (a0,d0.w),d3
        beq     trkptrfx
        cmp.b   d3,d1
        beq.s   trkplay
        add.w   d2,d0
        bpl     trksmlp
        bra     trkptrfx
;
;
seed:
        dc.l    $33ba0359               ;seed
constant:
        dc.l    $44bf19d3               ;constant
;
;
;---------------------------------------------------
;
;  Get random long saved in EEprom and return in d2.l
;
RanGetEE:
        move.w  #((defauEEx-defauEE)/2)+2,d1
        bsr     eeread
        move.w  d0,d2
        swap    d2
        subq    #1,d1
        bsr     eeread
        move.w  d0,d2
        rts
;
;  Save random long in d2.l in EEprom
;
RanPutEE:
        move.w  d2,d0
        move.w  #((defauEEx-defauEE)/2)+1,d1
        bsr     eewrite
        swap    d2
        move.w  d2,d0
        addq    #1,d1
        bsr     eewrite
        rts
;
;  EEprom read/write routines
;
;
;   Get all of the EEprom data and copy to our local ram
;     If data is not valid, just use defaults & init EEprom
;
getEE:
        lea     workEE,a3
        moveq   #0,d1                   ;word address to fetch
        moveq   #0,d2                   ;here is our running checksum
.getE:
        bsr     eeread
        move.w  d0,(a3)+
        add.w   d0,d2                   ;add to checksum
        addq.w  #1,d1
        cmpi.w  #(defauEEx-defauEE+2)/2,d1
        bcs     .getE
;
;  we got all of the EEprom
;
        tst.w   d2                      ;good checksum?
        bne.s   .resetE                 ;br if not (set-up as if cold init)
        move.w  #serial,d2
        cmp.w   vers,d2
        bne.s   .resetE
        rts
;
;
.resetE:
        lea     workEE,a3
        lea     defauEE,a1
        moveq   #(defauEEx-defauEE)/2-1,d1
        moveq   #0,d0                   ;checksum
.setE:
        move.w  (a1)+,d2
        move.w  d2,(a3)+
        add.w   d2,d0
        dbra    d1,.setE
;
        neg.w   d0
        move.w  d0,(a3)+
;
;   Now, copy to EEprom (include computed checksum)
;
        lea     workEE,a3
        moveq   #0,d1
        moveq   #(defauEEx-defauEE+2)/2-1,d2
.settE:
        move.w  (a3)+,d0
        bsr     eewrite
        bne.s   .errwr                  ;give-up if time-out
        addq.w  #1,d1                   ;next word-address
        dbra    d2,.settE
.errwr: 
        rts
;
;
;---------------------------------------------------
;
;   Set EEprom data specified by d1.w to current working value
;    and adjust checksum accordingly 
;
;
;  example:
;     To update value for seqmode, call setEE with #(seqmode-workEE) in d1.w
;
setEE:
        lea     workEE,a0
        move.w  (a0,d1.w),d2            ;get data to refresh
        lsr.w   #1,d1                   ;form word-address in EEprom
        bsr     eeread                  ;see if it has changed
        cmp.w   d0,d2
        beq     .setEx                  ;br to exit if no change
;
        move.w  d2,d0
        bsr     eewrite
        bne.s   .setEx                  ;exit if time-out (nothing we can do)
;
;  re-compute checksum
;
        moveq   #(defauEEx-defauEE)/2-1,d1
        moveq   #0,d0                   ;checksum
.setE:
        add.w   (a0)+,d0
        dbra    d1,.setE
;
        neg.w   d0
        move.w  d0,(a0)
;
        moveq   #(defauEEx-defauEE)/2,d1
        bsr     eewrite
.setEx:
        rts
;
;       
;
;  The BUTCH interface for the CD-ROM module is a long-word register,
;   where only the least signifigant 4 bits are used
;
eeprom  equ     $DFFF2c                 ;interface to CD-eeprom
;
;  bit3 - busy if 0 after write cmd, or Data In after read cmd 
;  bit2 - Data Out
;  bit1 - clock
;  bit0 - Chip Select (CS)
;
;
;   Commands specific to the National Semiconductor NM93C14
;
;
;  9-bit commands..
;                876543210
eREAD   equ     %110000000              ;read from EEPROM
eEWEN   equ     %100110000              ;Erase/write Enable
eERASE  equ     %111000000              ;Erase selected register
eWRITE  equ     %101000000              ;Write selected register
eERAL   equ     %100100000              ;Erase all registers
eWRAL   equ     %100010000              ;Writes all registers
eEWDS   equ     %100000000              ;Erase/Write disable (default)
;
;
;*****************************************************************
;
;  Write a word to EEPROM
;
;  entry: d0.w = data to be written
;         d1.w = least signifigant 6 bits specify write address (0-63)  
;
;   exit: d0 = 0 for successful write, -1 for time-out error
;              all other registers preserved
;       
;
;
eewrite:
        movem.l a0/d1-d6,-(sp)
        lea     eeprom,a0       ;set ptr to EEPROM i/o address
;
        move.w  #eEWEN,d2
        bsr     out9bits        ;enable write
;
        andi.w  #$3f,d1         ;force write addr to be legit (0-63)
        move.w  #eWRITE,d2
        or.w    d1,d2
        bsr     out9bits        ;issue WRITE command with write address
;
        move.w  d0,d2
        bsr     out16bit
;
;  strobe Chip Select (before check of busy)
;
        moveq   #1,d3
        moveq   #0,d4
        move.l  d4,(a0)         ;CS=0
        move.l  d3,(a0)         ;CS=1
        move.l  d4,(a0)         ;CS=0
;
        moveq   #8,d4
;
;       move.w  #1000,d5        ;this time-out is hairy edge on lite system
        move.w  #$8000,d5
busywait:
        move.l  (a0),d2         ;check busy
        and.w   d4,d2
        bne.s   eewrfin
        dbra    d5,busywait
;
;  time-out on write, set indicator & exit
;
        moveq   #-1,d0          ;indicate time out
        bra.s   eewrx   
eewrfin:
        move.w  #eEWDS,d2       ;get erase/write disable command
        bsr     out9bits        ;send it
;
        moveq   #0,d0           ;indicate no timeout
eewrx:
        movem.l (sp)+,a0/d1-d6
        tst.l   d0
        rts                     ;we're done
;
;
;
;******************************************************
;
;
;  Read a word from EEPROM
;
;  entry:  d1.w = least signif 6 bits specify read address (0-63)  
;
;   exit:  d0.w = data as read from EEPROM
;                 all other registers preserved
;
eeread:
        movem.l a0/d1-d4,-(sp)
        lea     eeprom,a0       ;set ptr to EEPROM i/o address
;
        andi.w  #$3f,d1         ;force legit read addr
        move.w  #eREAD,d2
        or.w    d1,d2
        bsr     out9bits
;
        moveq   #0,d0
        moveq   #15,d3          ;pick up 16 bits
        moveq   #0,d4
        moveq   #2,d5
inlp:
        move.l  d4,(a0)         ;Clk=0
        move.l  d5,(a0)         ;Clk=1
        move.l  d4,(a0)         ;Clk=0
;
        move.l  (a0),d1
        lsr.w   #4,d1
        addx.w  d0,d0
        dbra    d3,inlp
;
        movem.l (sp)+,a0/d1-d4
        rts
;
;**************************************************************
;
;  Serial data sent to device is written to DI, bit2 of $DFFF2C
;
; entry:
;  a0 -> eeprom ($DFFF2C)
;  d2.w = 16-bit data word to write
;
; exit:
;  d2.w, d3.l-d6.l destroyed
;
out16bit:
        rol.w   #3,d2           ;align MSbit to data bit position (bit2)
        moveq   #15,d6          ;send 15
        bra.s   outxb
;
; entry:
;  a0 -> eeprom ($DFFF2C)
;  d2.w = 9-bit command to write
;
out9bits:
;
;  strobe Chip Select (always needed for command)
;
        moveq   #1,d3
        moveq   #0,d4
;
        move.l  d4,(a0)         ;CS=0
        move.l  d3,(a0)         ;CS=1
        move.l  d4,(a0)         ;CS=0
;
        ror.w   #6,d2           ;align MSbit to data bit position (bit2)
        moveq   #8,d6           ;send 9
;
;
;
outxb:
        moveq   #4,d3           ;mask for data bit
        moveq   #2,d4           ;clock bit
;
bitloop:
        move.w  d2,d5
        and.l   d3,d5
;
        move.l  d5,(a0)         ;data is ready, CLK=0
        eor.w   d4,d5
        move.l  d5,(a0)         ;same data, CLK=1
        eor.w   d4,d5
        move.l  d5,(a0)         ;CLK=0
        rol.w   #1,d2
        dbra    d6,bitloop
;
;
        rts     
;
;
;  Our v-blank irq routine
;
;
Dframe:
;       ori.l   #4,G_CTRL               ;do GPU interrupt
        move.l  #5,G_CTRL               ;****1-Aug-95
;
        move.w  #$0101,INT1             ;clear interrupt
        move.w  #0,INT2
        rte
;
;
        .if     0
        movem.l d0/a0-a1,-(a7)
;
        lea     davelist,a0
        movea.l dlist,a1
;
        move.l  (a0)+,(a1)+             ;put up full CD+G 8-bit/pixel screen
        move.l  (a0)+,(a1)+
        move.l  (a0)+,(a1)+
        move.l  (a0)+,(a1)+
;
        adda.w  #$50,a1
;
        movea.l blist,a0                ;now do rest of Jeff's stuff
        adda.w  #$60,a0
;
        moveq   #$10,d0
;
        move.l  (a0)+,(a1)+             ;daves obj #0
        move.l  (a0)+,(a1)+
        move.l  (a0)+,(a1)+
        move.l  (a0)+,(a1)+
;
        adda.w  d0,a0
        adda.w  d0,a1
;
        move.l  (a0)+,(a1)+             ;daves obj #1
        move.l  (a0)+,(a1)+
        move.l  (a0)+,(a1)+
        move.l  (a0)+,(a1)+
;
        adda.w  d0,a0
        adda.w  d0,a1
;
        move.l  (a0)+,(a1)+             ;daves obj #2
        move.l  (a0)+,(a1)+
        move.l  (a0)+,(a1)+
        move.l  (a0)+,(a1)+
;
        adda.w  d0,a0
        adda.w  d0,a1
;
        move.l  (a0)+,(a1)+
        move.l  (a0)+,(a1)+
        move.l  (a0)+,(a1)+
        move.l  (a0)+,(a1)+
;
;       .endif
;
;       addi.w  #$1,frames              ;advance frame count
        clr.w   Pradip                  ;tell Pradip he can blit to display
notv:
        movem.l (a7)+,d0/a0-a1
;       move.w  #$0101,INT1             ;clear interrupt
;       move.w  #0,INT2

        move.w  #$0101,INT1             ;clear interrupt
        move.w  #0,INT2
        rte
        .endif
;
;
;  Every time display congfiguration changes, we may need to RunBeast
;
chngdisp:
        tst.w   cdgmode
        beq.s   .norun
        movem.l a0-a6/d0-d7,-(sp)
        jsr     RunBeast
        movem.l (sp)+,a0-a6/d0-d7
.norun:
        rts
;
;
;
        dc.l    0       ;space to back-up if we need to achieve long alignment
;
davelist:
;
;
;
;  Object type (Bits 0-2 in 1st 64-bit phrase of object)
; 
BMAPTYP         equ     0       ;Bit-Map object
SCALTYP         equ     1       ;Scaled Bit-Map object
GPUTYP          equ     2       ;Graphics Processor interrupt Object
BRTYP           equ     3       ;Branch Object
STOPTYP         equ     4       ;Stop Object
;
;
OBJTYPE         set     BMAPTYP ;1st object is a Scaled Bit-Map object
HEIGHT          set     192*2   ;fill the screen
YPOS            set     25*2+1  ;from the top
XPOS            set     35      ;from the left edge
FIRSTPIX        set     0       ;no left edge clipping
RELEASE         set     0       ;release bus between image data fetches
TRANSPAR        set     0       ;make logical color 0 transparent
RMW             set     0       ;no read-modify-write
REFLECT         set     0       ;no horizontal reflect
INDEX           set     0       ;palette offset if 1-4 bits/pix
IWIDTH          set     36      ;36 phrases in displayed image
DWIDTH          set     40      ;40 phrases in source form
PITCH           set     1       ;increment by 1 phrase for next fetch
DEPTH           set     3       ;8 bits/pixel (256 color)
;
;  1st Object is a vanilla Bit-map, so it needs a 2 phrase header
;
;  1st phrase
 dc.l   0
 dc.l   (HEIGHT << 13) + (YPOS << 3) + OBJTYPE 
;
;  2nd phrase
 dc.w (FIRSTPIX << 1) + RELEASE
 dc.w (TRANSPAR << 15) + (RMW << 14) + (REFLECT <<13) + (INDEX << 6) + (IWIDTH >> 4)
 dc.w ((IWIDTH & $0F) << 12) + (DWIDTH << 2) + (PITCH & $06)
 dc.w ((PITCH & 1) << 15) + (DEPTH << 12) + XPOS
;
 dc.l   0,STOPTYP,0,STOPTYP
;
;
;       
;
;   Default EEprom data
;
; This is the setting copied into EEprom if no valid data is found there
;
;
;
defauEE:
defVOL:
        dc.w    36      ;+0 default volume control ptr (0..64)
;defseqm:
;       dc.w    1       ;+2 default sequence mode (normal=1)
;defreptm:
;       dc.w    1       ;+4 default repeat mode (no repeat=1)
;defcdgm:
;       dc.w    0       ;+6 default cd+g mode (vlm = 0)
defBnkE:
        dc.w    $04     ;+8 default bank(-1)/effect
defVnbr:
        dc.w    serial  ;+A default version number
defauEEx:
;       dc.w    0       ;+C checksum (computed)
;
;
;
;
;
;  End eeprom data
;
;
;
defseqm:
        dc.w    1       ;+2 default sequence mode (normal=1)
defreptm:
        dc.w    1       ;+4 default repeat mode (no repeat=1)
defcdgm:
        dc.w    0       ;+6 default cd+g mode (vlm = 0)
;
;
;
voltab:
        dc.w    0       ;this is off
;
;  Exponential Volume Table (courtesy of Tim Dunn)
;
;       
        dc.w      32
        dc.w      36
        dc.w      40
        dc.w      45
        dc.w      50
        dc.w      55
        dc.w      62
        dc.w      69
        dc.w      77
        dc.w      86
        dc.w      96
        dc.w     107
        dc.w     120
        dc.w     134
        dc.w     149
        dc.w     167
        dc.w     186
        dc.w     208
        dc.w     232
        dc.w     259
        dc.w     289
        dc.w     323
        dc.w     360
        dc.w     402
        dc.w     449
        dc.w     501
        dc.w     559
        dc.w     624
        dc.w     697
        dc.w     778
        dc.w     868
        dc.w     969
        dc.w    1082
        dc.w    1208
        dc.w    1348
        dc.w    1505
        dc.w    1680
        dc.w    1875
        dc.w    2094
        dc.w    2337
        dc.w    2609
        dc.w    2912
        dc.w    3251
        dc.w    3629
        dc.w    4051
        dc.w    4522
        dc.w    5048
        dc.w    5635
        dc.w    6291
        dc.w    7022
        dc.w    7839
        dc.w    8751
        dc.w    9769
        dc.w   10905
        dc.w   12173
        dc.w   13589
        dc.w   15169
        dc.w   16933
        dc.w   18903
        dc.w   21101
        dc.w   23555
        dc.w   26295
        dc.w   29353
        dc.w   32767
;
vlmdk:
        .include        "vlmdark.s"
;
        .bss
;
;
;
;  The following block is copied from EEprom
;    and any changes are maintained in the EEprom
;
;
workEE:
volptr:
        ds.w    1       ;+0 init position of volume (1..64)
;seqmode:
;       ds.w    1       ;+2 sequence mode (program-0, normal-1, random-2)
;reptmode:
;       ds.w    1       ;+4 repeat mode (repeat track-0, no repeat-1) 
;cdgmode:
;       ds.w    1       ;+6 cd+g mode (vlm-0 , cd+g-1)
Sbankeff:
        ds.w    1       ;+8 bank/effect to start-up with
vers:
        ds.w    1       ;+A version #
chksum:
        ds.w    1       ;+C checksum of above block
;
;
;
;  These aren't saved so keep them outside EEprom block
;
seqmode:
        ds.w    1       ;+2 sequence mode (program-0, normal-1, random-2)
reptmode:
        ds.w    1       ;+4 repeat mode (repeat track-0, no repeat-1) 
cdgmode:
        ds.w    1       ;+6 cd+g mode (vlm-0 , cd+g-1)
;
reframe:
        ds.w    1       ;copy of frame counter when re-initing vlm
;
;
getTram:
play:
        ds.w    1       ;0 = stopped, ff = play
scan:
        ds.w    1       ;b1,0: 00-off, 01-wait min hold, 10-scan up, 11-scan down 
pause:
        ds.w    1       ;0 if not paused, else paused
scanfcnt:
        ds.w    1       ;frame count when joy R,L first pressed
scantime:
        ds.l    1       ;Amin,Asec,Aframe of last goto time command
scancnt:
        ds.w    1       ;# of times gototime command has been issued,this scan
track:
        ds.w    1       ;real track (hex)
maxmin:
        ds.w    1       ;hex min, max track #
SubCode:
        ds.b    16      ;12 q-subcode bytes (16-bytes allocated)
;
;
;        0 - Control/Addr
;        1 - TNO (track number)
;        2 - Index (point, pause between track indicator 0=minus, 1=plus)
;        3 - Min
;        4 - Sec
;        5 - Frame
;        6 - Zero
;        7 - AMin
;        8 - ASec
;        9 - AFrame
;       10 - CRC1 (validated by DSP)
;       11 - CRC2 (validated by DSP)
;
;
;
dave_pad:
        ds.l    1       ;copy of pad_now for modes 1&2
joyprev:
        ds.l    1       ;previous dave_pad from last service call
m3prev:
        ds.l    1       ;previous pad_now when in mode3
m3col:
        ds.w    1       ;current mode 3 column selection
m3row:
        ds.w    1       ;current mode 3 row selection
m3opt:
        ds.w    1       ;mode 3 option (=0 normal func, <>0 rows 1,3,4)
blinkon:
        ds.w    1       ;blink off/on
ourreg:
        ds.l    2       ;here's where we keep a1=subcode/a4=BUTCH
oneshot:
        ds.w    1       ;=0 if no init, else do one time init on service call
return:
        ds.w    256     ;data returned from a DSA_tx call
;
origEND:
        ds.l    1       ;ABS time End of CD    BCD (Am:As:Af:00)
fineEND:
        ds.l    1       ;ABS time End of CD binary (00:Am:As:Af)
retcode:
        ds.w    256     ;return code from DSA call, high byte is index here
hexbcd:
        ds.b    128     ;quick hex to BCD conversion table
scanrel:
        ds.w    1       ;update rel codes during scan if <>0
our_mode:
        ds.w    1       ;our pseudo vlm_mode (0..3)
modecnt:
        ds.w    1       ;our layer of indirection on our_mode (for RM seq)
audvlm:
        ds.w    1       ;=0 for audio mode, -1 for vlm mode
boxleft:
        ds.w    1       ;=0 if norm, -1 if on volume bar in mode 3
vlmrand:
        ds.w    1       ;0=normal vlm mode, -1 = random vlm mode
vlmrfrm:
        ds.w    1       ;frmcount when next change is to be made
cancelC:
        ds.w    1       ;=0 if norm, -1 if cancel "C" button once
offrame:
        ds.w    1       ;frame count when mode change has taken effect
volfcnt:
        ds.w    1       ;repeat volume frame counter (else 0)
voltimr:
        ds.w    1       ;frame count when we shut-off vol ind (else 0)
locbank:
        ds.w    1       ;local copy of imatrix (to refresh when changed)
loceff:
        ds.w    1       ;local copy of skid (to refresh when changed)
disbank:
        ds.w    1       ;display copy of imatrix
diseff:
        ds.w    1       ;display copy of skid
distrack:
        ds.w    1       ;partial track # entered via keypad
keytime:
        ds.w    1       ;timer for key entries
trksptr:
        ds.w    1       ;offset to current track in sequence (-1 if not on seq)
trksptrp:
        ds.w    1       ;trksptr when we left program mode (to be restored)
trkseq:
        ds.b    200     ;track sequence in use (may be randomized)
progseq:
        ds.b    200     ;user programmable sequence
gridoff:
        ds.w    1       ;offset into trkseq currently displaying on trk grid
progenty:
        ds.w    1       ;0=norm, -1=program mode data entry on trk grid
gridsize:
        ds.w    1       ;# of tracks for trk grid purposes
holeptr:
        ds.l    1       ;when program insert active, -> insert in trkseq
Pradip:
        ds.w    1
vlmrdcnt:
        ds.w    1       ;vlm random counter (for bank switch time)
;
objcopi:
        ds.b    3*$40
arrowfrm:
        ds.l    1       ;ptr to arrow form
        ds.l    2
        ds.b    16*7    ;7 lines high (up arrow)
        ds.b    16*7    ;7 lines high (down arrow)
