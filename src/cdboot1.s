;**************************************************************************
; (C)1994 ATARI CORP.       SECRET & CONFIDENTIAL       ALL RIGHTS RESERVED
;
;	This is a 1st stab at the CD boot code and is derived from cd_samp
;
;
;                                REVISION HISTORY
;
; REV.  DATE       BY            DESCRIPTION OF EDIT
; """"  """"       ""            """""""""""""""""""
; 0.01   8 Jun 94  DJS		 First Release
; 0.02  15 Jun 94  DJS		 Now boots a bootable CD
;
;****************************************************************************
;
;The 5 following conditionals must all be false (0) for production release
fail2ill	equ	0	;conditional for CD-ROM failures -> "illegal"
erreport	equ	0	;conditional for "endless" boot,errors to Alpine
errdRAM		equ	0	;send erreport errors to dRAM instead of Alpine
seqblock	equ	0	;sequential block (rand order) instead of rand
noecrypt	equ	0	;conditional for skip encrypt stuff
;
;
backfrm		equ	6	;frames to back-up before seek
maxretry	equ	5	;CD ERR or seek errors warrant this # of retries
;
;
;
	.nlist
  .include 'jaguar.inc'
	.include 'blitter.inc'
	.list
	.include 'cd.inc'	; CD-related equates

	.extern	end		; needed for GPU program load by DB
	.extern GPU_done
	.extern GPU_state
	.extern	SETUP
                                ; These define the main DRAM CD data buf area
	.extern	GPUSTART
	.extern	GPUEND

	.extern	SETUP
	.extern	CDREADER
;
;
;   Useful Kart locations
;
ROMCONFIG	equ	$800400		;4 identical bytes of 000XXXX0 for MEMCON2
JUMPVEC		equ	$800404		;desired start-up (usually $802000)
FLAGS		equ	$800408		;skip intro if bit0=1, (others reserved, 0)
;
;
;  some fixed locations we need in cdfront.s...
;
RanGetEE	equ	$80000	
RanPutEE	equ	$80004
randfAD		equ	$80008
CDfront		equ	$8000C
;
;
;
CDtoc		equ	$2c00	;CD directory goes here
GameInit	equ	$4000	;CD boot sector is initially loaded here
GameOver	equ	$24000	;end of CD boot sector buffer		
relocad		equ	$50000	;final org address of this module
;
; Butch's hardware registers
;
;
BUTCH     equ  $DFFF00		;base of Butch=interrupt control register, R/W
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
DSCNTRL   equ  4		; DSA control register, R/W
DS_DATA   equ  $A		; DSA TX/RX data, R/W
;
;
;
I2CNTRL   equ  $10		; i2s bus control register, R/W
;
;  When read:
;
;  b0 - I2S data from drive is ON if 1
;  b1 - I2S path to Jerry is ON if 1
;  b2 - reserved
;  b3 - host bus width is 16 if 1, else 32
;  b4 - FIFO state is not empty if 1
;
;
;
;
SBCNTRL   equ  $14		; CD subcode control register, R/W
SUBDATA   equ  $18		; Subcode data register A
SUBDATB   equ  $1C		; Subcode data register B
SB_TIME   equ  $20		; Subcode time and compare enable (D24)
FIFODAT   equ  $24		; i2s FIFO data
I2SDAT2   equ  $28		; i2s FIFO data (old)
;
;
;
;
start:
	move.l	#$200000,A7	; set stack pointer up high
reladj	equ	start-relocad
;
;
;  Memory map for 16-May-95
;
;  magic   - $2400-$2403   $12345678 is here so Karts know CD drive present
;  Q_codes - $2800-$281F   Only if audio CD is this used for copy of Q-subcodes
;
;  cdbios  - $3000-$39E7   v.4.04 
; sysload  - $3A00-$3Cxx
; err_flag - $3E00-$3E01   cdbios error code
;
; user     - $4000-$FAFFF  safe area for CDroms to load their boot sector
;
;cdboot1.bss - $FB000-$FF4F7
;  cdboot1   - $50000-$5212F
;  cdfront   - $80000-$8B368
;
;  screens   - $100000-$191FFF
;  vlm	     - $192000-$1ACFAC (upper half of dRAM allocated for vlm)
;  stack     - $1FFFxx-$1FFFFF
;
;
	moveq	#-1,d0			;start timer, just in case it's 0'd
	move.l	d0,$f10004		; for use in selecting start-up screen
;
;  Load the VLM
;
	lea	vlm,a0
	lea	$192000,a1
	lea	vlmx,a2
vlmset:
	move.l	(a0)+,(a1)+
	cmp.l	a2,a0
	bcs	vlmset
;
;
;  Some symbols imported from Jeff's code
;
	.nlist
	.include	"vlm.equ"
	.list
;
;
;  Load cdfront
;
	lea	cdfront,a0
	lea	$80000,a1
	lea	cdfrontx,a2
cdfset:
	move.l	(a0)+,(a1)+
	cmp.l	a2,a0
	bcs	cdfset
;
;  Now, load the cdbios
;
	lea	cd_bios,a0
	lea	$3000,a1
	lea	cd_biosx,a2
cdbioset:
	move.l	(a0)+,(a1)+
	cmp.l	a2,a0
	bcs	cdbioset
;
;  Copy cdboot1 to dRAM--we need to execute out of there
;
	bsr	intCDboo
;
;
	lea	readpad,a0
	move.w	(a0),padstart		;save 1st word of joystick routine
	move.w	#$4e75,(a0)		;cancel joystick reads until needed
;
	lea	newstart-reladj,a0
jumper:
	jmp	(a0)			;leave this 8-bit behind
;
;
;
;

intCDboo:
	lea	start,a0
	lea	relocad,a1
	lea	cd1,a2				;this is our last addr
reloop:
	move.l	(a0)+,(a1)+
	cmp.l	a2,a0
	bcs	reloop
;
;  Get error Graphics in dRAM too
;
	lea	cd1ram,a1			;copy in the error graphics
	lea	ergrafx(pc),a3
	lea	cd1ptr,a4
	moveq	#3,d1
nextgr:
	move.l	(a3)+,a0			;src start
	move.l	(a3)+,a2			;src end (+1)
	move.l	a1,(a4)+			;save base ptr
ergraflp:
	move.l	(a0)+,(a1)+
	cmp.l	a2,a0
	bcs	ergraflp
;
	move.l	a1,d0			;do a phrase alignment
	addq.l	#7,d0
	andi.w	#~7,d0
	move.l	d0,a1
	dbra	d1,nextgr		;go for next graphics form
;
	rts

ergrafx:
	dc.l	cd1
	dc.l	cd1x
;
	dc.l	quest
	dc.l	questx
;
	dc.l	cdback
	dc.l	cdbackx
;
	dc.l	arrow
	dc.l	arrowx
;
;overgraf:
;
;
;
;
newstart:	
;	.if	0
	lea	stopobj(pc),a0
	move.l	a0,d0
	addi.l	#15,d0
	andi.w	#~15,d0
	swap	d0
	move.l	d0,OLP

	lea	NullFrm(pc),a0
	move.l	a0,$100

	moveq	#0,d0
	move.l	d0,BORD1

;	move.w	#$96a0,BG		;flesh screen
	move.w	d0,BG			;black screen

	move.w	#-1,VDE
	move.w	#-1,VI
	move.w	#$100,INT1
	move.w	#$6c1,VMODE
;
	clr.l	failcode
;
;	.endif
;
	lea	randf(pc),a0	;cdfront would like this address
	move.l	a0,randfAD	;so we'll make it avail
;
	move.l	#$70007,G_END	;Data organisation register
	move.l	#$70007,D_END	;Data organisation register
;
	move.w	#$1865,MEMCON1
;*NotYetmove.w	#$1875,MEMCON1	;let's try 6 (not 10) ROM cycles, 32-bit
;
	move.l	#0,BUTCH	;let us get at the Kart
;
;
;---------------------------------------------
;  check cart stuff
;
;
;   Load up the cdbios...
;
	move.l	#0,G_CTRL	;Stop the GPU
	move.l	#0,G_FLAGS	;GPU Flags register
;
;
;
;
	move.w	#1,vlm_mode
;
;
;
	jsr	RanGetEE	;get previous random long in d2.l
	lea	seed(pc),a1
	move.l	d2,(a1)			;set our random seed
;
	move.l	d2,randinit
	swap	d2
	moveq	#0,d1
	move.w	d2,d1
	divu	#9,d1
	swap	d1
	addq.w	#1,d1	
	move.w	d1,skid			;set for this screen
;	move.w	#3,skid			;****test
	jsr	free			;put up the free run
;
	.if	0
	bra	skipKart
	.endif
;
; Copy over the DSP program to qualify a Kart
;
	lea	RSAgpu,a1
;
	move.l	#RSA_S,a0
	move.l	#RSA_E,a2
;
	move.l	#reladj,d7
	sub.l	d7,a0
	sub.l	d7,a2
	move.l	a1,D_PC
gloop:
	move.l	(a0)+,(a1)+
	cmp.l	a2,a0
	bcs	gloop
;
	lea	Public(pc),a1		;get Kart's Public Key
	lea	D_RAM,a2
	moveq	#68/4-1,d0
keyloop:
	move.l	(a1)+,(a2)+		;stuff in our Public key
	dbra	d0,keyloop		
;
;
	move.l	#$12345678,$2400	;set cookie location for JSR Karts 
;
;
	lea	BASE,a0
	move.l	ROMCONFIG,d0		;get ROM config byte
	andi.w	#%11110,d0		;just allow a few bits
	ori.w	#$1861,d0
;
	move.w	d0,(a0)			;set MEMCON1 for our Kart
;
	nop
	nop
	nop
;
	move.l	FLAGS,d3		;check flags for cart-no-boot
	btst.l	#2,d3			; from CD module flag
	bne	skipKart		;br to skip if we request cart-no-boot
;	
	lea	D_CTRL,a1
	move.l	#1,(a1)		;sick the DSP on the Kart (deRSA & MD5)
;
wait:
	move.l	(a1),d0		;wait here
	btst.l	#0,d0		; til DSP is done
	bne	wait
;
	move.l	D_RAM,d1		;DSP is done qualifying Kart
	cmpi.l	#$3D0DEAD,d1
	beq.s	.doKart
	cmpi.l	#$12345678,d1
	beq	skipKart
	bra	wait
;
;  We have a Kart that passes encryption
;
.doKart:
	move.l	#$180000,BUTCH	;set lid-up & cart-pull reset
;
	btst.l	#1,d3			;check for JSR Kart
	beq.s	.notJSR			;br if not JSR Kart
;
;  A JSR kart can do stuff without using display if "option" not depressed
;
	lea	JOY1,a3
	move.w	#$8007,(a3)+		;check option key
	move.w	(a3),d0			;bit1=0 if option down
	btst.l	#1,d0
	beq.s	.JSRdisd	;display shut-down & .bss save if option
;
;  Just leave display running if no option key
;
	move.l	JUMPVEC,a0		;get start vector
	move.l	#skipKart-reladj,-(sp)
	jmp	(a0)			;return after completion
;
.JSRdisd:
	lea	$FB000,a0		;25-May-95
	lea	$100000,a1
	lea	$4000,a2
.bsscop:
	move.l	(a0)+,(a2)+		;save .bss since NVMROM cart clobbers
	cmp.l	a1,a0
	bcs	.bsscop
;
.notJSR:
	move.w	#2,freerun		;stop GPU

.waitg:
	move.l	G_CTRL,d0		;wait here
	lsr.l	#1,d0			; til GPU is done
	bcs	.waitg
;
loram	equ	$1000
;
	lea	loram,a0
	lea	Kartblk(pc),a1
	lea	Kartblke(pc),a2
setKblk:
	move.l	(a1)+,(a0)+
	cmp.l	a2,a1
	bcs	setKblk
	jmp	loram
;
Kartblk:
	move.w	frames,d0
fzwait:
	move.w	frames,d1		;this one's more bullet proof
	sub.w	d0,d1			;added 7-Jun-95
;	cmpi.w	#5,d1
	cmpi.w	#2,d1
	bcs	fzwait
;
;*****6-Aug-95
	move.w	#-1,frames
	lea	videstop(pc),a0
	move.l	a0,$100
	bra.s	ovrids
;
videstop:
	move.l	#Frame,$100
	move.l	#4,$1ae824	;stop obj here
	move.w	#$21,VI
	clr.w	frames
	move.w	#$100,INT1
	move.w	#$0,INT2
	rte
;
ovrids:
	tst.w	frames
	bne	ovrids
;
	lea	stopo(pc),a0
	move.l	a0,d0
	addi.l	#15,d0
	andi.w	#~15,d0
	swap	d0
	move.l	d0,OLP
;
	lea	NFrm(pc),a0
	move.l	a0,$100
;
;	move.w	#-1,VI	;***21-Feb-95  Raiden (NTSC only) needs running
;
;	move.w	#$100,INT1
;	move.w	#$101,INT1		;*** Flashback (PAL only) likes
;
	moveq	#0,d0
	move.l	d0,BORD1
	move.w	d0,BG			;black screen
;
	move.l	JUMPVEC,a0		;get start vector
	move.l	FLAGS,d0		;check for JUMP or JSR option
	btst.l	#1,d0			;we assign this to bit 1
	bne.s	dojsr
;
;
;
;  ***4-Aug-95 this bit of madness for a piece of shit game
;	lea	$800004,a1
;	cmpi.l	#$749084f7,(a1)+	;Flashback?
;	bne.s	nflash			;br if not
;	cmpi.l	#$c99607f4,(a1)		;really, truely Flashback?
;	bne.s	nflash
;
;	moveq	#-1,d0			;these 3 instructions
;flashbut:
;	dbra	d0,flashbut		;help FlashBack (PAL only)
;	move.w	#$100,INT1		;function
;
;
nflash:
;	
	jmp	(a0)			;Jump II Kart (wherever it wants us)
;
dojsr:
	move.l	#KartRest-reladj,-(sp)
	jmp	(a0)			;return after completion
;
;
stopo:
	dc.w	4,4,4,4
	dc.w	4,4,4,4
	dc.w	4,4,4,4
	dc.w	4,4,4,4
NFrm:
	move.w	#$100,INT1
;	move.w	#$101,INT1		;*** Flashback (PAL only) likes
	move.w	#$0,INT2
	rte
Kartblke:
;
;  Restart the display if Kart used its own
;
KartRest:
	lea	$FB000,a0		;25-May-95
	lea	$100000,a1
	lea	$4000,a2
.bssrest:
	move.l	(a2)+,(a0)+		;restore .bss since NVMROM cart clobbers
	cmp.l	a1,a0
	bcs	.bssrest
;

	jsr	free		;restart the display
;
skipKart:
;-------------------------
	move.w	#maxretry,retryz	;init Directory read retry count
TOCagin:

	move.w	#$1875,MEMCON1	;let's try 6 (not 10) ROM cycles, 32-bit
;
	lea	BUTCH,a0	;added 8-May-95
	move.l	#$20000,(a0)	;reset CD module
	bsr	delay
	move.l	#0,(a0)
;
	move.l	#0,4(a0)	
	bsr	delay
;
	move.w	#0,JOY1		;mute this thing
;
Start_CD:                       ; Beginning of CD_related code
;
	bsr	chkCD		;talk to BUTCH and check for proper function
	bne	powup		;br if not proper function (power out)
;
	move.w	#3,d0		; Go to double speed, CDROM mode
	jsr	CD_mode
;
;***********************************************
;
;  The following is for Boot-ROM diagnostics only..
;
;   error report goes to Alpine:
;
;   900000 - total count
;   900004 - #1 failure count
;   900008 - #2
;   90000C - #3
;   900010 - #4
;   900014 - #5
;   900018 - #6
;
;   900020 - block #0 passes
;   900024 - failures
;
;   900028 - block #1 passes
;   90002C - failures
;
;   etc.
;
;   900818 - block #ff passes
;   90081C - failures
;
	.if	errdRAM
	move.l	#$20000,errBASE
	.else
	move.l	#$900000,errBASE
	.endif
;
	.if	seqblock
	clr.l	$90000			;seqblock allocation
	.endif
;
;
	.if	erreport
;
;
;
	move.l	errBASE,a0
	moveq	#0,d0
	move.w	#$4000-1,d1	;init diag
clrdiag:
	move.l	d0,(a0)+
	dbra	d1,clrdiag
;
	move.l	a0,d0		;init ptr to seq recorder
	addq.l	#4,d0
	move.l	d0,(a0)
;
;****************Dave's output folly here
	lea	brdata(pc),a0
	lea	beasties+$80,a1
	moveq	#6,d0
setboard:
	move.l	(a0)+,(a1)+
	dbra	d0,setboard
;
	suba.w	#16,a1
	move.w	#-1,CLUT+2
	move.w	#1,(a1)
;
	lea	board,a0
	moveq	#0,d0
	move.w	#$400,d1
clrboar:
	move.l	d0,(a0)+
	dbra	d1,clrboar
;
;
	move.w	#5,cursx
	move.w	#21,cursy

	lea	amsg(pc),a0
	bsr	printd

	move.w	#23,cursy
	bsr	outdata
	bra	overmsg
;
brdata:
	dc.l	$18fff8,$3cff7c,$240024,$ffff0001
	dc.l	board,$10000,$500f0
amsg:
	dc.b	"Hashes                    Fails",0
numstr:
	dc.b	"00000000",0
	.even
outdata:
	move.l	errBASE,a1
	move.l	(a1),d0
	bsr	makestr
	move.w	#5,cursx
	bsr	printd
	move.l	errBASE,a1
	adda.w	#$18,a1
	move.l	(a1),d0
	bsr	makestr
	move.w	#28,cursx
	bsr	printd
	rts
;
;  a0 -> output string, cursx/cursy is start position
;
printd:
	movem.l	a0-a4/d0-d2,-(sp)
	lea	board,a1
	lea	myFont,a2
	move.w	cursy,d0
	mulu	#40*8,d0
	moveq	#0,d1
	move.w	cursx,d1
	add.l	d1,d0
	adda.w	d0,a1
printdll:
	moveq	#0,d0
	move.b	(a0)+,d0
	beq.s	printdx
	add.w	d0,d0
	move.w	4(a2,d0.w),d0
	lea	(a2,d0.w),a3
	move.l	a1,a4
	moveq	#7,d1
printdl:
	move.b	(a3)+,(a4)
	adda.w	#40,a4
	dbra	d1,printdl
	addq.l	#1,a1
	bra	printdll
printdx:
	movem.l	(sp)+,a0-a4/d0-d2
	rts
;
;
makestr:
	lea	numstr(pc),a0
	moveq	#7,d2
	moveq	#0,d3
makestlp:
	rol.l	#4,d0
	move.w	d0,d1
	andi.w	#$f,d1
	bne.s	makest
	tst.w	d3
	bmi.s	makest
	tst.w	d2
	beq.s	makest
	move.b	#" ",(a0)+
	bra.s	makestx	
makest:
	moveq	#-1,d3
	move.b	hextab(pc,d1.w),(a0)+
makestx:
	dbra	d2,makestlp
	subq.l	#8,a0
	rts
hextab:
	dc.b	"0123456789ABCDEF"	
overmsg:
;*****************end Dave's output folly
	.endif
;
;   Now, let's read the directory
;
DSP_wait:
	move.l	D_CTRL,d0
	lsr.l	#1,d0
	bcs	DSP_wait
;
; Load, & initialize DSP "gettoc" program
;
Do_DSP:
	move.l	#DSP_S,a0
	move.l	#reladj,d7
	sub.l	d7,a0
	move.l	#DSP_E,a1
	sub.l	d7,a1
;
	move.l	a0,d1
	move.l	a1,d0
	sub.l	d1,d0		; Size in bytes
	asr.l	#2,d0
	move.l	#D_RAM,a1
x4loop:
	move.l	(a0)+,(a1)+
        dbra    d0,x4loop
;
	move.l	#D_RAM,D_PC	; Set GPU PC to start of code in SRAM
;
;
	lea	$f1c000,a0
	moveq	#0,d0
	moveq	#63,d1
clearsub:
	move.l	d0,(a0)+
	dbra	d1,clearsub
;
	move.l	#1,D_CTRL	; start GPU
;
;
;   Calculate the XMODEM quick look-up table
;
;xmdmtab	equ	$2a00
xmdmtab		equ	$f1c400
;
XMDMtab:
	lea	xmdmtab,a1
	moveq	#0,d1
	move.w	#$1021,d2
.tablp:
	move.b	d1,d4
	moveq	#7,d3
	moveq	#0,d0
.bytelp:
	lsl.w	#1,d0
	bcc.s	.hiclr
	lsl.b	#1,d4
	bcc.s	.xit
	bra.s	.noxit
.hiclr:
	lsl.b	#1,d4
	bcc.s	.noxit
.xit:
	eor.w	d2,d0
.noxit:
	dbra	d3,.bytelp
	move.l	d0,(a1)+
	addq.w	#1,d1
	cmpi.w	#256,d1
	bne	.tablp
;
;
	lea	D_RAM,a0
dwait:
	move.l	(a0),d0		;wait til dsp stuffs 0 here
	bne	dwait
;
;
	move.l	$f1003a,d2	;use JPIT as a random
	jsr	RanPutEE	;get a new random seed
;
	lea	CDtoc,a1
	move.l	(a1)+,d5	;check for "Disc not present or timeout
	bpl	notTO
;
;  Error condition detected...
;
;  d5.l = $FFFF0000 if time-out (format may be CD-I or other un-recognized)
;	= $FFFFFFFF if disc not present (status = $402 lid closed, no disc)
;						= $42c lid open
;
;
;
checkout:
	moveq	#-1,d0
	cmp.l	d0,d5		;check for invalid format or no disc type
	bne	termdis0	;br if invalid format (may be CD-I or CD-ROM)
;
;  No disc present or lid up error...
;
	move.w	BUTCH+DS_DATA,d0
	cmpi.w	#$42c,d0	;check for tray error (only recoverable)
	bne.s	termdis2	;br if not tray--it's terminal
;
;  Lid up error (only recoverable one)
;
	move.l	#$100000,BUTCH	;cart pull reset enable only (lid down disable)
	moveq	#0,d5		;tell them to check for error clearing
	bra	ntermd
;
termdis2:
	cmpi.w	#$402,d0	;was it focus error? (no disc)
	bne.s	termdis3	;br if not
;
;  No disc means we may want to to VLM external
;
	move.w	padstart,readpad	;restore joystick routine
	bra	termdis1		;continue
;
;  Some other DSA error has been detected--Let's retry
;
termdis3:
;
	subq.w	#1,retryz
	bne	TOCagin		;retry with reset of BUTCH
;
	bra	termdis0	;we give up
;	
;  come here if a CD fails authentication
;
failCD:
	.if	erreport
	move.l	d0,d1
	lsl.l	#2,d1
	move.l	errBASE,a0
	addq.l	#1,(a0)		;advance total count
	addq.l	#1,(a0,d1.l)	;advance error failure count
	cmp.w	#6,d0		;are we a block failure?
	bne.s	notmd5
;
	cmpi.w	#2,d6		;are we block #1?
	
	addq.l	#1,$2c(a0)	;increment block # failure

notmd5:
	move.l	d0,-(sp)
	move.w	#12,cursx
	move.w	#5,cursy
	bsr	makestr
	bsr	printd
	move.w	#8,cursx
	move.w	#3,cursy
	lea	fmsg(pc),a0
	bsr	printd
	bra.s	ovfmsg
fmsg:
	.dc.b	"Failcode:",0
	.even
ovfmsg:
	move.l	(sp)+,d0

	.endif
	move.l	d0,failcode	;stuff the failure code here
termdis0:
	moveq	#0,d0
	jsr	CD_stop		;stop CD 
termdis1:

	moveq	#-1,d5		;say its terminal
	move.l	#$180000,BUTCH	;set lid-up & cart-pull reset
;	move.l	#$80000,BUTCH	;set lid-up & cart-pull reset
;
;
;
ntermd:
	move.l	randinit,d2	;*** added 11-Apr-95
	rol.l	#5,d2
	move.w	failcode+2,d2	;save failure code
	andi.w	#$f,d2
	ori.w	#$DAF0,d2
	jsr	RanPutEE	;*** end 11-Apr-95 addition
;
	lea	davesobj,a0
	move.l	cd1ptr,a1	;get data from ram
;
;
;  a1 ->
;	+0  .w WIDTH
;	+2  .w HEIGHT
;	+4  .w D/IWIDTH
;	+6  .w unused, reserved
;	+8  .w COLORS USED (0 if PIXEL DEPTH = 4)
;	+A  .w PIXEL DEPTH   
;	+C  .l TOTAL FILE SIZE
;
;	+10  N Palette Words (N=0,2,4,16,256 for PIXEL DEPTHS 4,0,1,2,3)
;
;	+10+(2N) Start of Bit-Map
;
;	
	moveq	#-1,d1
	move.w	d1,$c(a0)	;turn object off while we're working on 'em
	move.w	#$90,(a0)	;set davesobj horz position
	move.w	#$150,$4(a0)	;vert position
	move.w	2(a1),$1A(a0)	;height
	move.w	#1,$14(a0)	;rmw or transparent
	move.w	#0,$16(a0)	;palette index
	move.w	$4(a1),$18(a0)	;DWIDTH
	move.w	$A(a1),$1C(a0)	;pixel depth
;
	lea	$210(a1),a2
	move.l	a2,$10(a0)	;set graphics base addr
;
;  load the palette
;
repaller:
	lea	$10(a1),a2
	lea	CLUT,a3
	move.w	$82(a3),d2
	move.w	#255,d0	
fullpal:
	move.w	(a2)+,(a3)+
	dbra	d0,fullpal
;
	move.w	#255,d0
chkp:
	move.w	-(a3),d1
	cmp.w	-(a2),d1
	bne	repaller
	dbra	d0,chkp
;
	move.w	d2,CLUT+$82
;
	.if	erreport
	move.w	#-1,CLUT+2
	.endif
;
	move.w	#6,$c(a0)	;turn it on
;
;
	lea	davesobj+$40,a0
	move.l	questptr,a1	;get data from ram
;
;
	moveq	#-1,d1
	move.w	d1,$c(a0)	;turn object off while we're working on 'em
	move.w	#$ac,(a0)	;set davesobj horz position
;
	move.w	#$170,$4(a0)	;vert position
	move.w	2(a1),$1A(a0)	;height
	move.w	#1,$14(a0)	;rmw or transparent
	move.w	#0,$16(a0)	;palette index
	move.w	$4(a1),$18(a0)	;DWIDTH
	move.w	$A(a1),$1C(a0)	;pixel depth
;
	lea	$210(a1),a2
	move.l	a2,$10(a0)	;set graphics base addr
	moveq	#-1,d7
donemiss:
	lea	davesobj+$40,a0
ques2:
	moveq	#-1,d1		;assume off
	bchg	#0,d7
	beq.s	offer
	moveq	#6,d1
offer:
;	
	move.w	d1,$c(a0)	;turn it on/off
;
;		   3         2         1         0
;		  10987654321098765432109876543210
;		  xxAPxxBxRLDU147*xxCxxxOx2580369#
	move.l	#%00100000000000010000000000000001,d1	;simultaneous A * #
	move.w	frames,d0
;7-Jun	add.w	#40,d0
.frwait:
	tst.l	d5		;are we terminal?
	beq.s	.noxchk
	cmp.l	pad_now,d1	;check joystick
	bne.s	.noxchk		;br if A * # not pressed together
;
	lea	CDtoc,a1
	clr.l	(a1)+
	clr.l	(a1)+
	move.l	#$01000000,(a1)
	bra	notTO
;
;	clr.l	CDtoc
;	clr.l	CDtoc+4
;	move.l	#$80000,BUTCH	;set-up lid-up reset
;	move.w	#-1,davesobj+$c
;	move.w	#-1,davesobj+$4c	;turn off error graphics, if any
;	move.w	#2,freerun	;stop GPU
;	jmp	$80000		;else, just start-up VLM in external
;
.noxchk:
;7-Jun	cmp.w	frames,d0
;7-Jun	bne	.frwait
;
	move.w	frames,d2	;7-Jun-95
	sub.w	d0,d2		;this makes the flashing "?" more bullet proof
	cmpi.w	#40,d2
	bcs	.frwait
;
;
	tst.l	d5		;are we terminal (must wait for reset)?
	bne	donemiss	;br if so
;
;  now we can wait for the whip (lid) to come down..
;
	lea	BUTCH,a4
	move.w	#$1700,d0	;clear error
	bsr	DSAcmd
;
	move.w	#$b00,d0
	bsr	DSAcmd		;tray closed
;
	andi.w	#$ff00,d0
	cmpi.w	#$400,d0
	beq	donemiss	;wait for error to go away
;
	lea	davesobj,a0
	move.w	#-1,$c(a0)
	move.w	#-1,$4c(a0)	;turn off error stuff
;
	move.w	frames,d0	;make sure door is closed
;7-Jun	add.w	#40,d0
.frwaitx:
;7-Jun	cmp.w	frames,d0
;7-Jun	bne	.frwaitx
;
	move.w	frames,d2	;7-Jun
	sub.w	d0,d2		;Make this more bullet proof
	cmpi.w	#5,d2
	bcs	.frwaitx
;
;
	bra	TOCagin		;retry
;	bra	DSP_wait		;retry
;
;
;  Make a multi-session CD into a VLM playable one
;
CDplus:
	moveq	#0,d0
	jsr	CD_jeri		;turn off jerry I2S irq's
;
	lea	SEMIPTR,a3
	move.l	#0,(a3)		;shut down DSP program so we can load another
;
	lea	D_CTRL,a3
.shutDSP:
	move.l	(a3),d0		;wait til it happens
	lsr.w	#1,d0
	bcs	.shutDSP
;
;	lea	D_RAM,a1	;clear out DSP just for test
;	move.l	#$0,d0
;	move.w	#$800-1,d1
;.DSPclr:
;	move.l	d0,(a1)+
;	dbra	d1,.DSPclr
;
	lea	CDtoc,a0
	moveq	#4,d0
multisc:
	addq	#8,d0
	move.l	(a0,d0.w),d1	;check for session #1
	beq	weAudio		;this br should never be taken
;
	rol.l	#8,d1
	cmpi.b	#1,d1		;have we reached session #1
	bne	multisc		;br if not--still scanning
;
	subq	#4,d0
	move.w	d0,d1		;d1 = offset where we start zeroing
	subq	#8,d0		;d0 = offset of last track, session #0
;
	move.l	(a0,d0.w),d2	;
	rol.l	#8,d2		;get track #
	move.l	(a0),d3
	move.b	d2,d3		;copy to track range (last track #)
	move.l	d3,(a0)		;save
	lsr.l	#8,d2		;clear off track # (leave just start time)
	move.l	4(a0,d0.w),d3	;get track duration (session # is always 0)
;
	moveq	#0,d4		;assume no frame carry
	add.b	d3,d2		;sum frames
	cmpi.b	#75,d2
	bcs.s	framsum		;
	subi.b	#75,d2
	moveq	#1,d4
framsum:
	ror.l	#8,d2
	ror.l	#8,d3
	add.b	d3,d2		;add seconds
	add.b	d4,d2		;and carry from frames
;
	moveq	#0,d4		;assume no seconds carry
	cmpi.b	#60,d2
	bcs.s	secsum
	subi.b	#60,d2
	moveq	#1,d4
secsum:
	ror.l	#8,d2
	ror.l	#8,d3
	add.b	d3,d2		;add minutes
	add.b	d4,d2		;and carry from seconds
;
	ori.w	#$100,d2	;put in session count (must always be 1)
	swap	d2		;now we have "SESS" MM:SS:FF
	move.l	d2,4(a0)
;
;  now clear higher sessions
;
	moveq	#0,d0
sessclr:
	move.l	d0,(a0,d1.w)	;clear next upper session stuff
	addq.w	#4,d1
	tst.l	(a0,d1.w)	;next long non-zero?
	bne	sessclr		;br if so--clear it
;	
;	bra	weAudio		;now ready for VLM
;
	lea	CDtoc+4,a1
	bra	notTO
;
; send a command to DSA and wait for single response
;
; Entry:
;  a4 -> BUTCH
;  d0 = command to send
;
; Exit:
;  d0 = DSA's reply
;
DSAcmd:
	move.l	(a4),d2			;check receive buffer full
	btst.l	#13,d2
	beq	.txready		;br if receive buffer clear
;
	move.w	DS_DATA(a4),d2		;else, get bogus receive stuff
	tst.l	DSCNTRL(a4)		;read to clear interrupt flag
.txready:
	move.w	d0,DS_DATA(a4)		;send command word
.txwait1:
	moveq	#15,d2			;this delay helps
.txdelay0:
	dbra	d2,.txdelay0
;
	move.l	(a4),d2			;wait til receive buffer full
	btst.l	#13,d2
	beq	.txwait1
;
	move.w	DS_DATA(a4),d0
	tst.l	DSCNTRL(a4)		;read to clear interrupt flag
;
	rts

;----------------------------------
;
;
powup:
	lea	davesobj,a0
	move.l	cdbackpt,a1	;get data from ram
;
;  a1 ->
;	+0  .w WIDTH
;	+2  .w HEIGHT
;	+4  .w D/IWIDTH
;	+6  .w unused, reserved
;	+8  .w COLORS USED (0 if PIXEL DEPTH = 4)
;	+A  .w PIXEL DEPTH   
;	+C  .l TOTAL FILE SIZE
;
;	+10  N Palette Words (N=0,2,4,16,256 for PIXEL DEPTHS 4,0,1,2,3)
;
;	+10+(2N) Start of Bit-Map
;
	moveq	#-1,d1
	move.w	d1,$c(a0)	;turn object off while we're working on 'em
	move.w	#$88,(a0)	;set davesobj horz position
	move.w	#$160,$4(a0)	;vert position
	move.w	2(a1),$1A(a0)	;height
	move.w	#1,$14(a0)	;rmw or transparent
	move.w	#0,$16(a0)	;palette index
	move.w	$4(a1),$18(a0)	;DWIDTH
	move.w	$A(a1),$1C(a0)	;pixel depth
;
	lea	$210(a1),a2
	move.l	a2,$10(a0)	;set graphics base addr
;
;  load the palette
;
.repaller:
	lea	$10(a1),a2
	lea	CLUT,a3
	move.w	#255,d0	
.fullpal:
	move.w	(a2)+,(a3)+
	dbra	d0,.fullpal
;
	move.w	#255,d0
.chkp:
	move.w	-(a3),d1
	cmp.w	-(a2),d1
	bne	.repaller
	dbra	d0,.chkp
;

	move.w	#6,$c(a0)	;turn it on
;
;
	lea	davesobj+$40,a0
	move.l	arrowptr,a1	;get data from ram
;
	moveq	#-1,d1
	move.w	d1,$c(a0)	;turn object off while we're working on 'em
	move.w	#$c4,d3
;	move.w	#$ec,d4
;	move.w	#$dc,d4
	move.w	d3,(a0)		;set davesobj horz position
;
	move.w	#$198,$4(a0)	;vert position
	move.w	2(a1),$1A(a0)	;height
	move.w	#1,$14(a0)	;rmw or transparent
	move.w	#0,$16(a0)	;palette index
	move.w	$4(a1),$18(a0)	;DWIDTH
	move.w	$A(a1),$1C(a0)	;pixel depth
;
	lea	$210(a1),a2
	move.l	a2,$10(a0)	;set graphics base addr
	move.w	#6,$c(a0)	;turn it on
	move.w	#6,d3
	move.w	#-1,d4
.testwt:
	move.w	frames,d5
.testwt1:
	movem.l	a0/d3-d5,-(sp)
	bsr	chkCD		;check for power coming on
	movem.l	(sp)+,a0/d3-d5
	tst.w	d0
	beq.s	gotpow
	move.w	frames,d0
	sub.w	d5,d0
	cmp.w	#40,d0
	bcs	.testwt1
;
	move.w	d4,$c(a0)
	exg	d4,d3
	bra	.testwt
;
;  We got power--let's start the boot up again
;
gotpow:
	lea	davesobj,a0
	moveq	#-1,d0
	move.w	d0,$c(a0)	;turn off the CD module arse
	move.w	d0,$4c(a0)
;
	lea	BUTCH,a0	;
	move.l	#$20000,(a0)	;reset CD module
	bsr	delay
	move.l	#0,(a0)
;
	move.l	#0,4(a0)	
	bsr	delay
;
	jsr	CD_setup	;1st time we talk to BUTCH
;
	move.w	#3,d0		; Go to double speed, CDROM mode
	jsr	CD_mode
;
;  we got power now..
;
	bra	DSP_wait	;finish up the boot
;
;
;
delay:
	moveq	#-1,d0
del0:
;	nop
;	nop
	dbra	d0,del0
	rts
;
;  Talk to CD (1st time) and set-up if proper funtion
;
;
chkCD:	
	jsr	CD_setup	;1st time we talk to BUTCH
;
	lea	$2c00,a0
	moveq	#0,d0		;clear directory space
	move.w	#$100-1,d1	;while U wait
cleartoc:
	move.l	d0,(a0)+
	dbra	d1,cleartoc
;
;
	move.w	#$8000,d1
	lea	BUTCH,a0
chkCDpow:
	move.l	(a0),d0
	andi.w	#$2000,d0
	bne.s	powok
	dbra	d1,chkCDpow
;
;  timed out..
;
powBAD:
	move.l	#$20000,BUTCH	;reset CD module
	move.l	#0,BUTCH+4	;clear DSA

	moveq	#-1,d0		;exit with error
	rts
powok:
	move.w	DS_DATA(a0),d0
	cmpi.w	#$7001,d0	;this indicates proper function
	bne	powBAD		;if not--put up power message
;
	tst.l	DSCNTRL(a0)	;clear DSA_rx
;
	moveq	#0,d0		;exit good
	rts
;
;
notTO:
	move.l	#$180000,BUTCH	;set lid-up & cart-pull reset
;	move.l	#$80000,BUTCH	;set lid-up & cart-pull reset
;
	move.w	#-1,davesobj+$c
	move.w	#-1,davesobj+$4c	;turn off error graphics, if any
;
	moveq	#0,d0
	move.l	d0,relocad	;we can use this ram	
;
	move.l	(a1),d0		;pick up session count & lead-out time
;
	rol.l	#8,d0		;get session count in d0.b
	cmpi.b	#2,d0		;session count 2 or greater?
	bcc	GPU_init	;br if so, multi-session disk
	cmpi.b	#1,d0		;only 1 session?
	bne	pauz		;br if not even 1 session
;
	move.l	#$208,relocad	;we use this time if checking CD-rom
;
weAudio:
	move.w	#0,d0		; Go to single speed, audio mode
	jsr	CD_mode
;
	jsr	CD_stop
;
	move.w	#2,freerun	;stop GPU
;
	move.w	padstart,readpad	;restart joystick reading
;
	jmp	CDfront		;jump to cdfront
;
;
pauz:
;	move.w	#$96a0,BG
	bra	pauz
;
;******************************************************************
;
;  Multi-session disk-- this will need our special blessing before it can run
;
GPU_init:
;
	clr.w	retrycnt	;init total CD ERR retry count (diag use only)
;
JER_wait:
	move.l	D_CTRL,d0	;wait til GPU is finished
	lsr.l	#1,d0
	bcs	JER_wait
;
;
;
;
; Load, & initialize host DSP program
;
Do_JER:
	.if	noecrypt

	lea	D_RAM,a1
;
	move.l	#MD5_S,a0
	move.l	#MD5_E,a2
;
	move.l	#reladj,d7
	sub.l	d7,a0
	sub.l	d7,a2
	move.l	#MD5start,D_PC
loamd5:
	move.l	(a0)+,(a1)+
	cmp.l	a2,a0
	bcs	loamd5
;
	move.l	#Hashoffs,sramo+4	;set DST in burp routine
;
	move.l	#$14,SMODE
;
	moveq	#1,d0
	jsr	CD_jeri			;send data to Jerry
;
	lea	D_CTRL,a1
	move.l	#1,(a1)		;start DSP
;
;
;*******************test
;	lea	startT0,a0
;	move.l	#$4252c,(a0)+	;start time
;	move.l	#"11111111",(a0)+
;	move.l	#$800,(a0)+	;length
;	bra	testhas
;*******************endtest
;
	bra	WePass
;
	.endif
;
;
;	lea	D_RAM,a1	;clear out DSP just for test
;	move.l	#$12345678,d0
;	move.w	#$800-1,d1
;DSPclr:
;	move.l	d0,(a1)+
;	dbra	d1,DSPclr
;
;  Load up READRSA.DAS module into DSP
;
	move.l	#reladj,d7
	move.l	#RDRSA_S,a0
	sub.l	d7,a0
	move.l	#RDRSA_E,a1
	sub.l	d7,a1
;
	move.l	a0,d1
	move.l	a1,d0
	sub.l	d1,d0		; Size in bytes
	asr.l	#2,d0
	move.l	#D_RAM,a1
xferloop:
	move.l	(a0)+,(a1)+
        dbra    d0,xferloop
;
;******test
	move.l	#SETUP,D_PC
	move.l	#1,D_CTRL	;start it up
;******end test
;
	bsr	clrRSAop	;clear RSA operands & install Public (CD) key
;
	move.l	#$14,SMODE	;get Jerry ready to read data
;
	moveq	#1,d0
	jsr	CD_jeri		;send data to Jerry
;
	.if	erreport
	moveq	#-1,d0		;do this for testing
	lea	$f1c000,a0
	move.w	#$2ff,d1
fillFF:
	move.l	d0,(a0)+
	dbra	d1,fillFF
	.endif
;
;test	move.l	#SETUP,D_PC
;test	move.l	#1,D_CTRL	;start it up
;
;   --------------------------
;  We need to get Hashblock records track (authentication code)
;    This must always be the last track on the disk..
;
	lea	CDtoc+8,a0	;find the last track
findhash:
	tst.l	(a0)+
	bne	findhash
;
	suba.w	#12,a0		;a0 -> hash track
	move.l	(a0),d0		;get hashtable start time in d0
;
	addq	#4,a0		;find out how many tracks since start of
	moveq	#-1,d1		;session #1
;
uptoc:
	addq	#1,d1		;patch ATARI APPROVED...
	subq.l	#8,a0
	tst.b	(a0)
	bne	uptoc
;
	addi.b	#$20,d1		;add to " " to form ATARI APPROVED ...ATRI"x"
	lea	ATstrng+$1f(pc),a1
	move.b	d1,(a1)
;
	bsr	backup6		;back up d0.l 6 frames
;
	bset.l	#31,d0
	move.l	d0,Hsynctim	;save for repeat later
;
	move.w	#maxretry,retryz	;we will try n times before giving up
firshash:
	lea	hashoffs,a3	;stuff desired offset(s)

	moveq	#0,d0
	move.l	d0,(a3)+	;tell DSP to get first 34 longs after "ATRI"

	moveq	#17,d0
	move.l	d0,(a3)+	; on the hash track

	moveq	#-1,d0
	move.l	d0,(a3)
;
;
;  3-May-95  start up DSP before seek (it waits for ACK now)
;
	lea	SEMIPTR,a3	;DSP ram ptr
	moveq	#1,d0
	move.l	d0,(a3)		;start up the frag read
;
;  end 3-May-95 addition
;
	move.l	Hsynctim,d0	;
	jsr	CD_read		;go seek & play some hash
;
;
;	jsr	CD_ack		;wait for "found"
;	cmpi.w	#$100,d1	;is our seek found?
;	bne	.fnderr		;br if not found
;
;	tst.l	$DFFF10		;clear CD ERR, if any
;
;	lea	SEMIPTR,a3	;DSP ram ptr
;	moveq	#1,d0
;	move.l	d0,(a3)		;start up the frag read
;
.check:
	move.l	(a3),d0		;wait til DSP is finished
	bpl.s	.check
;

;	move.w	#$ff07,$f14000	;*************test code
;
	tst.l	BUTCH+DSCNTRL	;****22-May-95 clear DSA_rx if any
;
	move.l	BUTCH,d0	;check for CD ERR
	move.l	d0,BUTCHcop	;save for diag
	btst.l	#14,d0		;did servo detect read error?
	beq.s	.noCDerr	;continue if no error
;
.fnderr:
	addq.w	#1,retrycnt	;increment global counter (for diag only)
	tst.l	$DFFF10		;clear CD ERR
	subq.w	#1,retryz	;dec retry count and retry
	bne	firshash
;
	moveq	#8,d0		;retry failure on 1st read of hash track
	bra	failCD
.noCDerr:

	moveq	#0,d0
	jsr	CD_jeri		;shut down jerry I2S irq's
;
	lea	DSP2RAM,a1	;fetch the hash offsets table
	move.l	#hashoffs,(a1)+	;set SRC
	move.l	#Hashoffs,(a1)+	;& DST
	move.l	#8,(a1)+	;count
	move.l	#-1,(a1)	;semiphore
;
	moveq	#5,d0
	move.l	d0,D_CTRL	;burp stuff from DSP's sRAM to dRAM where 
;				 we can look at it
.semiw:
	tst.l	(a1)		;wait till stuff is burped in
	bmi	.semiw		;br if not yet
;
;
	lea	Hashoffs,a1	;check for "ATRI" not found
	moveq	#-1,d1		;-1 at 1st offset indicates DSP time-out
	moveq	#1,d0		;fail code #1
	cmp.l	(a1),d1		;did we fail to find "ATRI"?
;
	.if	fail2ill

	bne	.nofail0		;
	illegal
.nofail0:
	.else

;1-Aug-95	beq	failCD		;if so--fail 'em
	beq	CDplus		;this is a CD+ disk
	.endif
;
	moveq	#-2,d1		;3-May-95 check for seek error
	cmp.l	(a1),d1		;we now check in DSP
	beq	.fnderr		;br if seek error
twofrags:
;
;  Copy 1st two hashtable frags as requested into dRAM..
;
	lea	DSP2RAM,a1
	move.l	#SEMIPTR+$3bc,(a1)+	;set SRC (rawHASH)
	move.l	#$4000,(a1)+	;& DST
	move.l	#34,(a1)+	;count
	move.l	#-1,(a1)
;
	moveq	#5,d0
	move.l	d0,D_CTRL	;burp it in
;
.semiw:
	tst.l	(a1)		;wait till stuff is burped in
	bmi.s	.semiw		;br until we got it
;
;
;  check for valid ATARI data header
;
	moveq	#2,d0		;assume "ATARI APPROVED.." error
;
	lea	$4000,a0
	moveq	#31,d2
	lea	ATstrng(pc),a1
.mustb:
	move.b	(a1)+,d1
	cmp.b	(a0)+,d1
;
	.if	fail2ill
	beq	.nofail1
	illegal
.nofail1:
	.else
	bne	failCD
	.endif
	dbra	d2,.mustb
;
;  a0-> base addr of data immediately following Hash track header
;
	lea	SEMIPTR,a1
	move.l	a0,(a1)		;tell GPU to deRSA 0th RSA block
;
;  now wait for GPU to finish
;
	moveq	#4,d0		;assume deRSA error
.RSAwait:
	move.l	(a1),d1		;=$FFFFFFFF when done ($FFFEFFFF if error)
	bpl	.RSAwait
;
	btst.l	#16,d1		;did we get the deRSA error?
;
;
	.if	fail2ill
	bne	.notf0		;br if not
	illegal
.notf0:
	.else
	beq	failCD		;br if so
	.endif
;
RSAd:
	lea	DSP2RAM,a1
	move.l	#SEMIPTR+$2f0,(a1)+	;set SRC (bootIIG)
	move.l	#daRSA,(a1)+	;& DST
	move.l	#16,(a1)+	;count
	move.l	#-1,(a1)
;
	moveq	#5,d0
	move.l	d0,D_CTRL	;burp it in
;
.semiw:
	tst.l	(a1)		;wait till stuff is burped in
	bmi	.semiw		;br if we got it
;
;
	lea	Hblkcnt,a1
	lea	daRSA,a2	;this is dRAM copied from "bootIIG"
	move.l	(a2)+,(a1)+	;pick up # of hashblocks
	addq	#8,a2
	moveq	#3,d0		;we'll take 4 longs of TOC hash
.TOChg:
	move.l	(a2)+,(a1)+
	dbra	d0,.TOChg
;
;  our 1st selection is always boot sector
;
	moveq	#6,d0
.selclp:
	move.l	(a2)+,(a1)+		;get start time,pattern,length, & hash
	dbra	d0,.selclp
;
;  Now, pick 2 more random Hash blocks
;
	.if	seqblock
;
; test mode--we want all blocks with minimum delay
;
	lea	$90000,a0
	move.l	(a0)+,d0
	bne.s	fetchan			;br if we don't need to generate
;
;  we need to generate 1st time
;
	move.l	a0,-4(a0)		
	move.l	Hblkcnt,d1		;get total block count
	move.l	a0,a3
	moveq	#2,d2
seqloop:
	move.w	d2,(a3)+		;fill with sequential 2..n
	addq.w	#1,d2
	cmp.w	d1,d2
	bcs	seqloop
	moveq	#-1,d3
	move.w	d3,(a3)+		;put in terminator
;
	move.l	a0,a3
;
	subq	#3,d2
rexrand:
	bsr	randf
	swap	d0
	clr.w	d0
	swap	d0
	divu	d2,d0
	swap	d0
	add.w	d0,d0
	move.w	(a3,d0.w),d4
	move.w	(a3),(a3,d0.w)
	move.w	d4,(a3)+
	subq.w	#1,d2
	cmpi.w	#1,d2
	bne	rexrand
	move.l	a0,d0
fetchan:
	move.l	a0,-(sp)
	lea	prevcmp(pc),a0
	tst.b	(a0)
	bne.s	.incomp		;br if not complete
	move.w	#$200,$DFFF0A	;stop the disk
	move.l	#0,BUTCH	;allow lid up
.wethru:
	bra	.wethru
.incomp:
	move.l	(sp)+,a0
;
	move.l	d0,a3
	move.w	(a3)+,d2
	bpl.s	fetch2
	bsr	complt		;indicate complete on screen
	move.l	a0,a3
	move.w	(a3)+,d2
fetch2:
	move.w	(a3)+,d3
	bpl.s	fetch3
	bsr	complt		;indicate complete on screen
	move.l	a0,a3
	move.w	(a3)+,d3
fetch3:
	move.l	a3,-(a0)
	bra.s	ovcomp
;
complt:
	movem.l	d0-d1/a0,-(sp)
	lea	prevcmp(pc),a0
	clr.b	(a0)		;set complete flag
	move.w	cursx,d0
	move.w	cursy,d1
	move.w	#22,cursy
	move.w	#18,cursx
	lea	compmsg(pc),a0
	bsr	printd
	move.w	d0,cursx
	move.w	d1,cursy
	movem.l	(sp)+,d0-d1/a0
	rts
prevcmp:
	dc.b	-1		;-1 if not complete
compmsg:
	dc.b	"Complete",0
	.even
ovcomp:
;
;
;
; end seqblock stuff
;
	.else
;
	bsr	randf			;d0.l = next random
	move.l	Hblkcnt,d1		;get total # of Hash blocks
	subq.l	#2,d1			;we must exclude TOC hash & 0th block
	bhi.s	gooBLK			;br if 1 or greater
failBLK:
	moveq	#3,d0			;error code if division by zero
	bra	failCD			; or negative will occur
gooBLK:
	lsr.l	#1,d0			;this makes distribution better
;
	andi.l	#$ffff,d0
	divu	d1,d0
	swap	d0			;get remainder
	addq.w	#2,d0			;we picked #0,1 already
	move.w	d0,d2			;d2 is first pick
;
	subq.l	#1,d1			;one less to pick from
	bls	failBLK
;
	bsr	randf
	lsr.l	#3,d0			;this helps too

	andi.l	#$ffff,d0
	divu	d1,d0
	swap	d0
	move.w	d0,d3
	addq.w	#2,d3
	cmp.w	d2,d3
	bcs.s	notless
	addq.w	#1,d3
notless:
	.endif
;
;   d2,d3 = 2 blocks, randomly selected
;
	cmp.w	d2,d3		;these must be in numeric order
	bcc.s	neednot
	exg	d2,d3
neednot:
	move.w	d2,d7
	swap	d7
	move.w	d3,d7	
	move.l	d7,blockpic	;save for use later
;
	lea	Blksgot,a3	;find plaintext block #'s we need
	lea	plainoff,a0	;plaintxt offset
	lea	Cryptgot,a5	;Cryptotext offsets for each
	lea	hashoffs,a6	; and expressed as long offsets too
	move.l	a6,a1
;
	moveq	#-1,d4		;set previous block # requested to impossible
loop2:
	moveq	#28,d0		;find offset for 1st random hashblock
	mulu	d2,d0		;d0 is offset into hashblock records table 
	move.l	d0,(a0)+	;we need for later
	moveq	#27,d1
	add.l	d0,d1		;d1 is offset of last byte of record
	lsr.l	#6,d0		;/64--get record # for 1st byte
	lsr.l	#6,d1		;/64--record # for last byte
	cmp.w	d0,d4		;are we same as previous?
	beq.s	notpre		;br if so--needn't request same
nothblk:
	move.w	d0,(a3)+	;else, we need this block #
	moveq	#65,d5
	mulu	d0,d5		;find cryptotext offset
	move.l	d5,(a5)+	;save
	lsr.l	#2,d5
	addq.l	#8,d5		;
	cmp.l	a6,a1		;is this first offset request?
	beq.s	firreq		;br if so
;
	moveq	#17,d6		;add 17 to previous
	add.l	-4(a6),d6
	cmp.l	d6,d5
	bcc.s	firreq
	move.l	d6,d5
firreq:
	move.l	d5,(a6)+
notpre:
	cmp.w	d0,d1		;check for block straddle
	beq.s	notdoub		;br if no straddle
	move.w	d1,d0		;else, need another
	bra	nothblk		;go for it
notdoub:
	move.w	d0,d4		;d0 is last block requested
	cmp.w	d3,d2		;are we done?
	beq.s	hfragx		;exit if so
	move.w	d3,d2		;else, just do other now
	bra	loop2
hfragx:
	moveq	#-1,d0
	move.l	d0,(a6)		;stuff terminator
;
	moveq	#1,d0
	jsr	CD_jeri		;turn on jerry I2S irq's
;
	move.w	#maxretry,retryz	;we will try n times before giving up
secnhash:
;
;  3-May-95
;
	lea	SEMIPTR,a3
	moveq	#1,d0
	move.l	d0,(a3)		;start up
;
;  *** end 3-May-95 addition
;
;
	move.l	Hsynctim,d0	;get original hash track seek time (w/bit31 set)
	jsr	CD_read		;go play some hash
;
;	jsr	CD_ack		;but wait for "found"
;	cmpi.w	#$100,d1	;is it found?
;	bne	.fnderr		;br if not found
;
;	tst.l	$DFFF10		;clear CD ERR, if any
;
;	lea	SEMIPTR,a3
;	moveq	#1,d0
;	move.l	d0,(a3)		;start up
;
.check:
	move.l	(a3),d0
	bpl.s	.check
;
	tst.l	BUTCH+DSCNTRL	;22-May-95 clear DSA_rx if it needs it
;	
	move.l	BUTCH,d0	;check for CD ERR
	move.l	d0,BUTCHcop	;save for diag
	btst.l	#14,d0		;did servo detect read error?
	beq.s	.noCDerr	;continue if no error
;
.fnderr:
	addq.w	#1,retrycnt	;increment global counter (for diag only)
	tst.l	$DFFF10		;clear CD ERR
	subq.w	#1,retryz	;dec retry count and retry
	bne	secnhash
;
	moveq	#9,d0		;retry failure on 2nd read of hash track
	bra	failCD
.noCDerr:
;
;
	moveq	#0,d0
	jsr	CD_jeri		;turn off jerry I2S irq's
;
	lea	DSP2RAM,a1
	move.l	#hashoffs,(a1)+	;set SRC (hashoffs)
	move.l	#Hashoffs,(a1)+	;& DST
	move.l	#8,(a1)+	;count
	move.l	#-1,(a1)	;semiphore
;
	moveq	#5,d0
	move.l	d0,D_CTRL	;burp in actual hashoffs used
;
.semiw:
	tst.l	(a1)		;wait till stuff is burped in
	bmi	.semiw		;br til we got it
;
;
	moveq	#1,d0		;assume error code
	lea	Hashoffs,a1	;check for "ATRI" not found
	moveq	#-1,d1		;-1 is DSP's time-out indication
	cmp.l	(a1),d1		;did we fail to find "ATRI"?
;
	.if	fail2ill
	bne	.notfffx
	illegal
.notfffx:
	.else
	beq	failCD		;if so--fail 'em
	.endif
;
	moveq	#-2,d1
	cmp.l	(a1),d1
	beq	.fnderr
;
;
;  Copy hashtable frags into dRAM..
;
RSAx:
	lea	DSP2RAM,a1
	move.l	#SEMIPTR+$3bc,(a1)+	;set SRC (rawHASH)
	move.l	#$4020,(a1)+	;& DST
	move.l	#17*4,(a1)+	;long count (maximum posssible)
	move.l	#-1,(a1)	;semiphore for job completion
;
	moveq	#5,d0
	move.l	d0,D_CTRL	;burp it in
;
.semiw:
	tst.l	(a1)		;wait till stuff is burped in
	bmi	.semiw		;br if we got it
;
;  Now we need to deRSA up to 4 RSA blocks
;
	lea	SEMIPTR,a1	;DSP semiphore
	lea	Cryptgot,a2	;crypto blocks offsets (in bytes)
	lea	Hashoffs,a3	;offsets requested (in longs)
	lea	$4020,a4	;crypto data as retrieved
	lea	plaintxt,a5	;place to deposit plain text blocks
;
	move.l	(a3)+,d0	;get long where we picked up first block
;
deRloop:
	subq.l	#8,d0		;adjust for 8 longs of header
	lsl.l	#2,d0		;x4 for bytes
	move.l	(a2)+,d1	;get RSA offset
	sub.l	d0,d1		;get 0..3
	lea	(a4,d1.l),a6
.contin:
	move.l	a6,(a1)		;deRSA this block
;
;  now wait for DSP to finish
;
.RSAwait:
	move.l	(a1),d5
	bpl	.RSAwait
;
	moveq	#4,d0		;assume deRSA error
	btst.l	#16,d5

	.if	fail2ill
	bne	.notff0
	illegal
.notff0:
	.else
	beq	failCD		;br if error
	.endif
;
	lea	DSP2RAM,a0	;no error--just get plaintext into dRAM
	move.l	#SEMIPTR+$2f0,(a0)+	;set SRC (bootIIG)
	move.l	a5,(a0)+	;& DST (plain text buffer)
	move.l	#16,(a0)+	;count
	move.l	#-1,(a0)	;semiphore for completion
;
	moveq	#5,d0
	move.l	d0,D_CTRL	;burp it in
;
.semiw:
	tst.l	(a0)		;wait till stuff is burped in
	bmi	.semiw		;br til we got it
;
;
;  check for continuation
;
	adda.l	#17*4,a4	;advance to next crypto-blob to read in
	adda.l	#16*4,a5	;advance to next plaintxt block
	move.l	(a3)+,d0
	bpl	deRloop
;
;  Now copy plain text to our hash entries
;
	lea	plainoff,a2
	lea	startT1,a4
	moveq	#1,d6
plan2h:
	move.l	(a2)+,d0
	move.l	d0,d1
	andi.w	#%111111,d1	;just interested in block offset
	lsr.l	#6,d0		;get block #
;
	lea	plaintxt,a1	;find block we're looking for
	lea	Blksgot,a3
plloop:
	cmp.w	(a3)+,d0
	beq.s	gotpll		;found it
	adda.w	#$40,a1
	bra	plloop		
gotpll:
	lea	(a1,d1.w),a5
	moveq	#6,d5
fetchone:
	move.l	(a5)+,(a4)+
	dbra	d5,fetchone	
	dbra	d6,plan2h
;
;
;  we now have the three hashblocks that need verification
;
	lea	SEMIPTR,a3
	move.l	#0,(a3)		;shut down DSP program so we can load another
;
	lea	D_CTRL,a3
shutrsa:
	move.l	(a3),d0		;wait til it happens
	lsr.w	#1,d0
	bcs	shutrsa
;
;
;   Now, load MD5 stuff into DSP
;
gotpats:
	lea	D_RAM,a1
;
	move.l	#MD5_S,a0
	move.l	#MD5_E,a2
;
	move.l	#reladj,d7
	sub.l	d7,a0
	sub.l	d7,a2
	move.l	#MD5start,D_PC
loadmd5:
	move.l	(a0)+,(a1)+
	cmp.l	a2,a0
	bcs	loadmd5
;
	move.l	#Hashoffs,sramo+4	;set DST in burp routine
;
	move.l	#$14,SMODE
;
;	moveq	#1,d0
;	jsr	CD_jeri			;send data to Jerry
;
	lea	D_CTRL,a1
	move.l	#1,(a1)		;start DSP
;
;
; But first, let's deMD5 the CDtoc
;
	lea	$f1b800,a1	;we need to stuff the TOC inside
	move.l	a1,d1
	lea	CDtoc,a2	; jerry before this can be done
	move.w	#255,d0
injerrlp:
	move.l	(a2)+,(a1)+
	dbra	d0,injerrlp
;
	lea	MD5head,a1
	move.l	d1,(a1)+
	addi.l	#$400,d1
	move.l	d1,(a1)
	subq	#8,a1
	moveq	#1,d0		;this just does Sram based blocks
	move.l	d0,(a1)		;start
;
.waiTOC:
	tst.l	(a1)		;wait til done
	bpl	.waiTOC
;
	lea	sramo+12,a0
	move.l	#-1,(a0)
;
	moveq	#5,d0
	move.l	d0,D_CTRL	;burp DSP to deliver MD5result data to Hashoffs
;
.semiw:
	tst.l	(a0)		;wait till stuff is burped in
	bmi	.semiw		;br til we got it
;
;
	lea	TOChash,a0
;
;	lea	MD5result,a1
	lea	Hashoffs,a1
;
	moveq	#5,d0		;error code for bad TOC hash
	moveq	#3,d2
chkTOCh:
	move.l	(a0)+,d1
	cmp.l	(a1)+,d1

	.if	fail2ill
	beq	chkTOCg
	illegal
chkTOCg:
	.else
	bne	failCD		;fail if CDtoc hash doesn't match
	.endif

	dbra	d2,chkTOCh
;
;
;   Now for the hard part..
;      let's MD5 the CD block containing the boot segment, then 2 random ones
;
;testhas:
	lea	startT0,a0
	moveq	#2,d6		;3 MD5's to do, including 0th
threhash:
	move.l	(a0)+,d0	;start time
	move.l	(a0)+,d1	;start pattern
	move.l	(a0)+,d2	;block count
	lea	MD5semi,a1
;
	move.l	d1,4(a1)
	move.l	d2,8(a1)	
;
;	subq	#4,a1
;
	bsr	backup6		;back up d0.l 6 frames

	move.l	d0,-(sp)
	moveq	#1,d0
	jsr	CD_jeri		;send data to Jerry
	move.l	(sp)+,d0
;
	bset.l	#31,d0
	move.l	d0,Hsynctim	;save for retry use
	move.w	#maxretry,retryz	;n attempts for retry
rehasht:
;
;
;  *** 3-May-95  (wait for "FOUND" in DSP) 
	moveq	#0,d1
	move.l	d1,(a1)		;start up Jerry
;  *** end 3-May-95 addition
;
	move.l	Hsynctim,d0
	jsr	CD_read		;just do read
;
;	jsr	CD_ack		;wait for found
;	cmpi.w	#$100,d1	;is it found?
;	bne	.fnderr		;br if not found
;
;------
;	tst.l	$DFFF10		;clear CD ERR, if any
;-------
;
;	moveq	#0,d1
;	move.l	d1,(a1)		;start up Jerry
;
;
.wao:
	move.l	(a1),d1		;wait for completion
	bpl	.wao
;
	tst.l	BUTCH+DSCNTRL	;22-May-95 clear DSA_rx if it needs it

	move.l	BUTCH,d0	;check for CD ERR
	move.l	d0,BUTCHcop	;save for diag
	btst.l	#14,d0		;did servo detect read error?
	beq.s	.noCDerr	;continue if no error
;
.fnderr:
	addq.w	#1,retrycnt	;increment global counter (for diag only)
	tst.l	$DFFF10		;clear CD ERR
	subq.w	#1,retryz	;dec retry count and retry
	bne	rehasht
;
	moveq	#10,d0		;retry failure on 2nd read of hash track
	add.w	d6,d0		;10 for 3rd, 11 for 2nd & 12 for 1st block
	bra	failCD
.noCDerr:
	btst.l	#17,d1		;seek error?
	beq	.fnderr		;br if so
;
	moveq	#7,d0		;assume start pattern time-out error (code 7)
;
	btst.l	#16,d1		;did we time-out looking for start pattern?

	.if	fail2ill
	bne.s	.hashpb	
	illegal
.hashpb:
	.else
	beq	failCD
	.endif
;
	moveq	#0,d0
	jsr	CD_jeri		;stop data to Jerry
;
	lea	sramo+12,a1
	move.l	#-1,(a1)
;
	moveq	#5,d0
	move.l	d0,D_CTRL	;burp DSP to deliver MD5result data to Hashoffs
;
.semiw:
	tst.l	(a1)		;wait till stuff is burped in
	bmi	.semiw		;br til we got it
;
;
	move.w	d6,d0		;put 0, 1, or 2
	swap	d0		;in high word to tell which hash
	move.w	#6,d0		;set error code in case bad hash
;	lea	MD5result,a1
	lea	Hashoffs,a1
	moveq	#3,d2
chkCDh:
	move.l	(a1)+,d1
	cmp.l	(a0)+,d1
;
	beq.s	gooCD

	.if	fail2ill
	illegal
	.else

	.if	!erreport
	bra	failCD		;fail if hash doesn't match
	.endif
	.endif
;
;
;
	lsl.l	#2,d2		;advance over bad hash
	adda.l	d2,a0

;
;  Error report stuff
;	
	move.l	blockpic,d2
	cmpi.w	#1,d6
	bcs.s	.later
	bne.s	.booty
	swap	d2
	bra.s	.later
.booty:
	moveq	#1,d2
.later:
	lsl.w	#3,d2
	move.l	errBASE,a1
	adda.w	#$20,a1
	addq.l	#1,4(a1,d2.w)		;advance error count
	subq.l	#1,(a1,d2.w)		;decrement pass (it gets inc'd later)
	lsl.w	#1,d2

	move.l	errBASE,a1
;	adda.w	#$4000,a1
	adda.l	#$8000,a1

	adda.w	d2,a1
	lea	Hashoffs,a2
	moveq	#3,d2
.badh:
	move.l	(a2)+,(a1)+		;copy bad hash
	dbra	d2,.badh
	move.l	errBASE,a1
	adda.w	#6*4,a1
	addq.l	#1,(a1)		;advance error #6 counter
	moveq	#0,d2
;
;
gooCD:
	dbra	d2,chkCDh
;
	dbra	d6,threhash
;
;
;
;  We pass!
;
;
	.if	erreport
;
	moveq	#0,d0
	jsr	CD_jeri		;shut down jerry I2S irq's
;
	move.l	errBASE,a0
	addq.l	#3,(a0)		;advance total count
;
	move.l	blockpic,d1
	addq.l	#1,$28(a0)	;advance fixed block #1
;
	lsl.w	#3,d1
	addq.l	#1,$20(a0,d1.w)	;advance pass count 1st rand block
	swap	d1
	lsl.w	#3,d1
	addq.l	#1,$20(a0,d1.w)	;advance pass count 2nd rand block
;
	moveq	#-1,d0
Dlay:
	dbra	d0,Dlay
;
	moveq	#3,d0
	move.l	d0,MD5semi	;turn off DSP	
;
;	move.l	D_FLAGS,d0	;force DSP reg set back to bank 0
;	bclr.l	#14,d0
;	move.l	d0,D_FLAGS
;
	move.l	errBASE,a1
	adda.l	#$10000,a1
	move.l	(a1),a0
	move.l	a1,d0
	addi.l	#$10000,d0
	cmp.l	d0,a0
	bcc.s	attop
	move.l	blockpic,(a0)+
attop:
	move.l	a0,(a1)
;
	bsr	outdata
;
	bra	JER_wait
;
	.endif
;
	moveq	#1,d0
	jsr	CD_jeri		;send data to Jerry
;
;
;
WePass:
	move.w	#2,freerun	;stop the GPU
;
	move.w	frames,d0
fxwait:
	move.w	frames,d1
	sub.w	d0,d1
	cmpi.w	#2,d1
	bcs	fxwait
;
;16-May-95 test
;
	lea	vidstop(pc),a0
	move.l	a0,$100
	bra.s	ovvids
;
vidstop:
	move.l	#Frame,$100
	move.l	#4,$1ae824	;stop obj here
	move.w	#$100,INT1
	move.w	#$0,INT2
	rte
;
ovvids:
; *******end 16-May-95 test
;
;**	move.w	#$100,INT1	;turn off display this way
;
	lea	CDtoc+8,a0	;now find start time of boot track
waitfor1:
	move.l	(a0)+,d0
	move.l	(a0)+,d1
	rol.l	#8,d1
	cmpi.b	#1,d1		;1st session #1 track?
	bne	waitfor1
seekit:
	bsr	backup6		;back up d0.l 6 frames
;
;
;
;  We pass!
;
;lastlow	equ	$3e80
;
lastlow	equ	$3a00
;
hsynctm	equ	$3fc0		;we need this for lastlow temp ram
Retryz	equ	$3fc4
;
;
	lea	lastlow,a0
	lea	CDnext(pc),a1
	move.l	#(CDnexte-CDnext)/4,d1
cdloop:
	move.l	(a1)+,(a0)+
	dbra	d1,cdloop
;
	jmp	lastlow
;
;
;  bring display to a reasonable state
;
CDnext:
	bset.l	#31,d0
;
	move.l	d0,hsynctm	;save for retry use
	move.w	#maxretry,Retryz	;n attempts for retry
Btretry:
	move.l	hsynctm,d0
	jsr	CD_read		;just do read
;
	jsr	CD_ack		;wait for found
	cmpi.w	#$100,d1	;is it found?
	bne	booerr		;br if not found
;
	tst.l	$DFFF10		;clear CD ERR, if any

	lea	MD5semi,a1
	moveq	#2,d1		;start up a boot thing

	move.l	d1,(a1)		;start up Jerry
;
;
bootwait:
	move.l	(a1),d0
	bpl	bootwait	
;
	move.l	BUTCH,d0	;check for CD ERR
;17-Apr	move.l	d0,BUTCHcop	;save for diag
	btst.l	#14,d0		;did servo detect read error?
	beq.s	nobooerr	;continue if no error
;
booerr:
;17-Apr	addq.w	#1,retrycnt	;increment global counter (for diag only)
	tst.l	$DFFF10		;clear CD ERR
	subq.w	#1,Retryz	;dec retry count and retry
	bne	Btretry		;go for another
;
	move.l	#$40000,BUTCH
	move.w	#$1861,MEMCON1	;set MEMCON for 8-bit
;
	jsr	intCDboo	;reload cdboot1 from 8-bit CD Bootrom

	move.w	#$1865,MEMCON1
	move.l	#0,BUTCH
;
	moveq	#13,d0		;retry failure on load of boot block
	move.w	#$101,INT1	;turn display back on
	lea	failCD,a0
	sub.l	#reladj,a0
	jmp	(a0)
;
nobooerr:
	moveq	#3,d0
	move.l	d0,(a1)		;shut-down DSP command
;
	lea	D_CTRL,a0
notfin:
	move.l	(a0),d0
	move.l	d0,d1
	lsr.w	#1,d1		;wait for DSP to shut itself off
	bcs	notfin
;
;
	lea	stopobj(pc),a0
	move.l	a0,d0
	addi.l	#15,d0
	andi.w	#~15,d0
	swap	d0
	move.l	d0,OLP
;
	lea	NullFrm(pc),a0
	move.l	a0,$100
;
	moveq	#0,d0
	move.l	d0,BORD1
	move.w	d0,BG			;black screen
;
	move.w	#-1,VI		;turn display off for real
	move.w	#$101,INT1
;
	moveq	#0,d0
	jsr	CD_jeri		;turn off jerry I2S irq's
;
go4it:
	move.l	MD5result,a0
	jmp	(a0)
;
stopobj:
	dc.w	4,4,4,4
	dc.w	4,4,4,4
	dc.w	4,4,4,4
	dc.w	4,4,4,4
NullFrm:
	move.w	#$101,INT1
	move.w	#$0,INT2
	rte

CDnexte:
;
;
;-----------------------------------------------
;
;  Load Public Key and clear all other operands in DSP's deRSA
;
clrRSAop:
	movem.l	d0-d1/a1/a2,-(sp)
	lea	PublicCD(pc),a1
	lea	SEMIPTR+4,a2
	moveq	#68/4-1,d0
.keyloop:
	move.l	(a1)+,(a2)+		;stuff in our Public key
	dbra	d0,.keyloop		
;
	moveq	#0,d1
	move.w	#200,d0
oper0:
	move.l	d1,(a2)+
	dbra	d0,oper0		;zero out operands
	movem.l	(sp)+,d0-d1/a1/a2

	rts
;
;  ----------------------------------------
;
;
;    Get a random long in d0 after each call
;
randf:
	movem.l	a0/d1-d5,-(sp)
;	
;  The following random generator provides exactly the same
;   results as Landon Dyer's version in the ST extended BIOS.
;
	movem.l	seed(pc),d0-d1		;get seed & constant
	moveq	#0,d2		;clear neg flag
rerand:
	tst.l	d0		;check seed
	bgt.s	ov		;br if pos, non-zero (seed ok)
	bne.s	ovx		;br if neg, non-zero (need positive)
;
;  we have a zero seed--can't let this happen
;
	move.l	$f1003a,d0		;use JPIT as a random
	swap	d0
	bra	rerand		;guaranteed non-zero now		
ovx:
	neg.l	d0		;make positive
	addq	#1,d2		;set neg flag
ov:
	move.l	d0,d3
	mulu	d1,d3
;
	move.l	d0,d4
	swap	d4
	mulu	d1,d4
;
	move.l	d1,d5
	swap	d5
	mulu	d5,d0
;
	add.w	d4,d0
	swap	d0
	clr.w	d0
	add.l	d3,d0
	tst.w	d2
	bne.s	ov1
	neg.l	d0
ov1:
	addq.l	#1,d0
	lea	seed(pc),a0
	move.l	d0,(a0)		;save seed for next time
;
	movem.l	(sp)+,a0/d1-d5
	rts
;
;
;
seed:
	dc.l	$33ba0359		;seed
constant:
	dc.l	$44bf19d3		;constant
;
;
;
	.if	0
;
;  --------  Search buffer for header block "ATRI" ------------
;
; Entry:
;  a0 -> start of buffer to scan
;  a1 -> end of buffer to scan
;
; Exit:
;  a0 -> start of data block immediately after partition mark found
;
matchead:
	move.l	#"ATRI",d0
	rol.l	#8,d0
newchk:
	move.l	d0,d1
	moveq	#64-2,d2
;	moveq	#4-2,d2
wait1st:
	cmp.b	(a0)+,d1
	beq.s	wait4
	cmp.l	a1,a0
	bcs	wait1st
wait4:
	rol.l	#8,d1
wait4AT:
	cmp.b	(a0)+,d1
	beq.s	waiti
	cmp.l	a1,a0
	bcs	newchk
	moveq	#-1,d1		;reached end without match
	rts
waiti:
	dbra	d2,wait4
;
	moveq	#0,d1		;good match found
	rts
;
	.endif
;-------------------------------------------------
;
; Entry:
;  d0.l = seek time (mm:ss:ff)
;
; Exit:
;  d0.l = 6 frames before seek time
;
backup6:
	andi.l	#$ffffff,d0	;strip track # off [31:24]
;
;	subq.b	#6,d0		;back up 6 frames
;	subi.b	#9,d0		;back up 9 frames
	subi.b	#backfrm,d0	;back up x frames

	bcc.s	doseek

	addi.b	#75,d0		;adjust for frame underflo
	ror.l	#8,d0		;need to borrow from seconds
	subq.b	#1,d0		;subtract 1 from seconds
	bcc.s	doseek1
;
	addi.b	#60,d0		;adjust for seconds underflo
	ror.l	#8,d0		;need to borrow from minutes
	subq.b	#1,d0		;there better be minutes
	rol.l	#8,d0		; since we don't check min underflo
doseek1:
	rol.l	#8,d0
doseek:
	rts
;
;
;
;******************************************************
;
	.if	0
;
;  Need to do the funny de-scramble thang
;
CDromchk:
	lea	GameInit,a0
;	lea	GameOver,a1
;
nextsec:
	move.l	a0,a1
	adda.w	#5000,a1
	moveq	#-1,d0
	moveq	#3,d1
headchk:
	cmp.w	(a0)+,d0
	beq.s	head1
	cmp.l	a1,a0
	bcs	headchk
	bra	weA
head1:
	dbra	d1,headchk
;
goodhead:
	addq	#2,a0
	move.l	a0,a2
;
;  found next start
;
	move.w	#2-1,d3		;just do 2 bytes for id
	moveq	#1,d0
	moveq	#0,d5
;
;
	move.l	#$8000,d2

blklp:
	move.w	(a0),d1
	moveq	#15,d4
	moveq	#0,d7
wordlp:
	or.w	d7,d5
	lsr.w	#1,d5
;
	move.w	d1,d7
	eor.w	d0,d7
	ror.w	#1,d7
	and.w	d2,d7
;
	move.w	d0,d6
	lsl.w	#1,d0
	eor.w	d0,d6
	roxr.w	#2,d6
	roxr.w	#2,d0
	lsr.w	#1,d1
	dbra	d4,wordlp
;
	or.w	d7,d5
	rol.w	#8,d5
	move.w	d5,(a0)+
	dbra	d3,blklp
;
	swap	d2		;have we done full sector?
	tst.w	d2
	bne	secdone		;if so, quit
;
	move.l	-4(a0),d3	;check this sector id
	andi.l	#$ffffff00,d3
	move.l	d3,d4
	swap	d4
	cmp.w	#2,d4		;is it CDrom style sector id?
	bne	weA		;br if not
;
	cmpi.l	#$00021600,d3	;reached desired sector id?
	bne	nextsec		;keep looking
;
	move.w	#1160-1,d3	;yes--this is one of interest
	move.w	#$ffff,d2	;so finish all bytes
	swap	d2
	bra	blklp
;
secdone:
	illegal
;
weA:
	move.l	#1,G_CTRL
	bra	weAudio
;
;	
;
;bail:
;	move.w	#$96a0,BG
	.endif
;
;
;
;		
ATstrng:	;32103210321032103210321032103210
	.dc.b	"ATARI APPROVED DATA HEADER ATRI "
;
	.even
Public:
	dc.b	0,0,0
	.include	"public.key"
PublicCD:
	dc.b	0,0,0
	.include	"publicd.key"
;
;
;
	.bss
;
;  The following block of memory is used by de-cryption code..
;
Hblkcnt:
	ds.l	1		;# of Hash blocks, this CD
TOChash:
	ds.l	4		;CDtoc MD5 hash (for $2C00-$2FFF)
;
;  We select three Hash blocks to authenticate...
;
;
;	#0
startT0:
	ds.l	1		;start time for hash block #0
startP0:
	ds.l	1		;start partition marker pattern
size0:
	ds.l	1		;# of 64-byte blocks to hash
HBhash0:
	ds.l	4		;hash for selected hash block #0
;
;
;	#1
startT1:
	ds.l	1		;start time for hash block #1
startP1:
	ds.l	1		;start partition marker pattern
size1:
	ds.l	1		;# of 64-byte blocks to hash
HBhash1:
	ds.l	4		;hash for selected hash block #1
;
;
;	#2
startT2:
	ds.l	1		;start time for hash block #2
startP2:
	ds.l	1		;start partition marker pattern
size2:
	ds.l	1		;# of 64-byte blocks to hash
HBhash2:
	ds.l	4		;hash for selected hash block #2
;
;
;
blockpic:
	ds.l	1	;Hi & Lo words contain randomly selected frag #'s
Hsynctim:
	ds.l	1	;sync time to CD_read to when rereading hash track
;
failcode:
	ds.l	1	;CD-ROM failure code goes here
;			1 - "ATRI" header not found on hash track
;			2 - "ATARI APPROVED.." not found
;			3 - Hash entry count below minimum
;			4 - Bad RSA block on hash track
;			5 - Bad TOC hash
;			6 - Bad segment hash
;			7 - Time-out looking for sync before MD5 hash
;			8 - retry failure (CD ERR) on 1st read of hash track
;			9 - retry failure (CD ERR) on 2nd read of hash track
;			10- retry failure (CD ERR) on 3rd (random) hash block
;			11- retry failure (CD ERR) on 2nd (random) hash block 
;			12- retry failure (CD ERR) on 1st (boot) hash block 
;			13- retry failure on load of boot block (before exec)
randinit:
	ds.l	1	;random initialization value
errBASE:
	ds.l	1	;ptr to base addr of error report
padstart:
	ds.w	1	;save 1st word of joystick routine (to restart)
retrycnt:
	ds.w	1	;total count of CD ERR retries attempted
retryz:
	ds.w	1	;retry count for CD ERR
	ds.w	1	
BUTCHcop:
	ds.l	2	;heres a copy of BUTCH before we error out
Blksgot:
	ds.w	4	;plaintext block #'s we need (64-byte blocks, #0..n)
plainoff:
	ds.l	2	;offset for each of 2 blocks
;
Cryptgot:
	ds.l	4	;cryptext deRSA offsets for above blocks
;
plaintxt:
	ds.l	16	;up to 4 plaintext blocks need a home
	ds.l	16
	ds.l	16
	ds.l	16

;
;   The following locations are used as copies of DSP sRAM 
;
SEMptr:
	ds.l	1	;readrsa module results dRAM
daRSA:
Hashoffs:
	ds.l	8
Hfrags:
	ds.l	68	
;
;   end de-cryption code memory
;
errorlog:
	ds.l	1	;here's where errors are recorded
	ds.l	1
;
cd1ptr:
	ds.l	1
questptr:
	ds.l	1
cdbackpt:
	ds.l	1
arrowptr:
	ds.l	1
cd1ram:
	ds.w	$f0000
