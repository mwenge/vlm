;************************************************************************
;* TITLE	: PACK.S						*
;* Function	: CD+G processing routines				*
;*									*
;* Project #	: JAGUAR CD+G						*
;* Programmer	: Pradip K Fatehpuria	09/22/94			*
;*									*
;* COPYRIGHT(c) 1992,1993 Atari Computer Corporation			*
;* UNATHORIZED REPRODUCTION, ADAPTATION, DISTRIBUTION, PERFORMANCE OR	*
;* DISPLAY OF THIS COMPUTER PROGRAM OR THE ASSOCIATED AUDIOVISUAL WORK	*
;* IS STRICTLY PROHIBITED.						*
;* ALL RIGHTS RESERVED.							*
;************************************************************************
;*									*
;* INSTRUCTIONS FOR INTEGRATION 					*
;*									*
;*======================================================================*
;*									*
;* initgpu()	: should be called before the audio playback starts	*
;*									*
;* initcdg()	: should be called to initialise the CD+G valrables	*
;*		  and interrupts					*
;*									*
;* stopcdg()	: stops the GPU program and disables CD+G		*
;*									*
;* subcode()	: reads subcode data from GPU memory and processes them	*
;*									*
;* dispsub()	: updates the screen for CD+G, CLUT and border color,	*
;*		  needed.  This routine should	be called once for	*
;*		  each video frame					*
;*									*
;*======================================================================*
;*									*
;* TvChnlAv(.w)	: the TV channel numbers available.  bit set means	*
;*		  the particular channel is available.			*
;*									*
;* TvChnlNo(.b) : the current CD+G channel to be displayed.  User	*
;*		  should be allowed to change this value (0-15).	*
;*									*
;* cdgflags(.b)	: set/clear bit 0 in this byte variable to start	*
;*		  processing for subcode or skipping subcode data	*
;*									*
;************************************************************************

	.title	"PROCESS CD+G PACKS"

;*======================================================================*
;* HEADER FILES								*
;*======================================================================*

	.include 'jaguar.inc'
	.include 'blitter.inc'
	.include 'objlist.inc'
	.include 'defs.inc'

;*======================================================================*
;* LOCAL EQUATES							*
;*======================================================================*

SYMB00		equ	$00			; 00
SYMB01		equ	$42			; 01
SYMB02		equ	$7D			; 02
SYMB03		equ	$BF			; 03
SYMB04		equ	$64			; 04
SYMB05		equ	$32			; 05
SYMB06		equ	$96			; 06
SYMB07		equ	$AF			; 07
SYMB08		equ	$08			; 08
SYMB09		equ	$21			; 09
SYMB10		equ	$3A			; 0A
SYMB11		equ	$53			; 0B
SYMB12		equ	$6C			; 0C
SYMB13		equ	$85			; 0D
SYMB14		equ	$9E			; 0E
SYMB15		equ	$B7			; 0F
SYMB16		equ	$10			; 10
SYMB17		equ	$29			; 11
SYMB18		equ	$19			; 12
SYMB19		equ	$5B			; 13
SYMB20		equ	$74			; 14
SYMB21		equ	$8D			; 15
SYMB22		equ	$A6			; 16
SYMB23		equ	$4B			; 17

;*==============================================*
;*==============================================*

;J_INT		equ	$F10020			; JERRY interrupt control register

BUTCH		equ	$DFFF00			; BASE OF BUTCH - interrupt control register, R/W
SBCNTRL		equ	BUTCH+$14		; BUTCH - CD subcode control register, R/W

;*==============================================*
;*==============================================*

GPU_stop	equ	$F03500			; stop-GPU semaphore from M68K

Haf_flg		equ	$F03504			; buffer half flag
Ful_flg		equ	$F03508			; buffer full flag

Buf_ptr		equ	$F03600			; subcode buffer in GPU memory
Mid_ptr		equ	$F03660			; midway pointer in subcode buffer

;*==============================================*
;*==============================================*

tvdspmem	equ	$100000			; graphics memory for TV-Graphics
tvgrfmm1	equ	tvdspmem+(320*192)	; graphics memory for TV-Graphics
tvgrfmm2	equ	tvgrfmm1+(320*240)	; graphics memory for TV-Graphics
CDGwords	equ	tvgrfmm2+(320*240)	; subcode data (288 bytes)
fontdata	equ	CDGwords+(288*1)	; memory for font data (72 words)
fontblit	equ	fontdata+(144*1)	; font data for bit to pixel expansion (12 bytes)
cdgmmend	equ	fontblit+(16*1)		; end of buffer used by CD+G stuff

;*==============================================*
;*==============================================*

CDG_DISP	equ	0			; should the subcodes be processed
TVDSPFLG	equ	1			; should the TV-graphics display be updated
TVCOLFLG	equ	2			; should the colour table be updated
BRCOLFLG	equ	3			; border colour flag

;*==============================================*
;*==============================================*

TVCOLOFF	equ	$00E0			; starting offset in CLUT

;*======================================================================*
;* EXTERNAL DATA							*
;*======================================================================*

	.extern	gpuend				; GPU code and data

;*======================================================================*
;* PUBLIC ROUTINES							*
;*======================================================================*

	.globl	initgpu
	.globl	initcdg
	.globl	stopcdg
	.globl	subcode
	.globl	dispsub

;*======================================================================*
;* PUBLIC DATA								*
;*======================================================================*

	.globl	TvChnlAv
	.globl	TvChnlNo
	.globl	cdgflags

;************************************************************************
;* SUBROUTINES AREA							*
;************************************************************************

	.text

;************************************************************************
;************************************************************************

;*======================================================================*
;* load and run GPU program						*
;*======================================================================*

initgpu:
	movem.l	d0-d1/a0-a1,-(sp)	; save registers

	move.l	#0,GPU_stop		; semaphore to tell GPU to stop itself
.lodgpu:
	move.l	#gpuend,a0		; a0 = ptr to DSP code and data
	move.l	(a0)+,d0		; d0 = final location of GPU code
	move.l	d0,a1			; a1 = final location of GPU code
	move.l	d0,d1			; d1 = save the start of the GPU code
	move.l	(a0)+,d0		; d0 = size in bytes
	asr.l	#2,d0			; d0 = size in longs
.xferg:	move.l	(a0)+,(a1)+		; copy data to GPU internal RAM
	dbra	d0,.xferg		; loop until copy finished
.rungpu:				; initialize & run GPU program
;*Dave	move.l	d1,G_PC			; set GPU PC to start of code in SRAM
	move.l	#$f03034,G_PC		; set GPU PC to start of code in SRAM
;*Dave	move.l	#1,G_CTRL		; set GPU GO bit to start running

.ret:	movem.l	(sp)+,d0-d1/a0-a1	; restore registers
	rts

;*======================================================================*
;* initcdg	: initialise a CDG track player				*
;*======================================================================*

initcdg:
	movem.l	d0-d1/a0-a3,-(sp)	; save registers

.iniflgs:
;	move.w	#$0003,TvChnlAv		; channel 0 and 1 are always available
	move.w	#$0000,TvChnlNo		; channel number zero to be displayed

	move.b	#0,cdgflags		; initialise the CD+G flag
	bset.b	#CDG_DISP,cdgflags
;;;	bclr.b	#TVDSPFLG,cdgflags
;;;	bclr.b	#TVCOLFLG,cdgflags
;;;	bclr.b	#BRCOLFLG,cdgflags

.iniptrs:
	move.l	#tvgrfmm1,tvgrfmem	; init ptr to TV graphics memory
	move.l	#tvgrfmm2,tvotherm	; init ptr to alternate TV graphics memory
	move.l	#0,subcount		; subcode count in buffer

.clrscrs:
	move.w	#$0000,d1		; set the color to BLUE
	bsr	clrtvdsp		; clear TV-Graphics Display memory
	bsr	clrtvgrf		; clear TV-Graphics memory

.inicolr:				; initialise color tables
	clr.l	d0
	move.l	#TvColRed,a0		; primary memory - red component
	move.l	#TvColGrn,a1		; primary memory - green component
	move.l	#TvColBlu,a2		; primary memory - blue component
	move.l	#TransFac,a3		; transparency factor
	move.w	#4,d1			; total of 16 colors
.colloop:
	move.l	d0,(a0)+		; primary - red component
	move.l	d0,(a1)+		; primary - green component
	move.l	d0,(a2)+		; primary - blue component
	move.l	d0,(a3)+		; transparency factor
	dbra	d1,.colloop

.brdcolr:
	move.l	d0,BrdColNo		; border color  number

.tvscrol:
	move.w	d0,tvPH			; scroll factors for TV graphics
	move.w	d0,tvPV
	move.w	d0,tvCOPH
	move.w	d0,tvCOPV

.cdginit:
;	move.l	#%100000,G_FLAGS	; allow DSP interrupts to TOM
	move.w	#1,J_INT		; tell JERRY to pass them to TOM
	move.l	SBCNTRL,d1		; read to clear int flag

	move.l	BUTCH,d0		;do it this way
	ori.l	#%1001,d0
	move.l	d0,BUTCH

;	move.l	#%1001,BUTCH		; BUTCH enable 

.ret:	movem.l	(sp)+,d0-d1/a0-a3	; restore registers
	rts

;*======================================================================*
;* stop CDG application							*
;*======================================================================*

stopcdg:
	move.l	#1,GPU_stop		; semaphore to tell GPU to stop itself
	move.l	#0,G_FLAGS		; clear GPU interrupt mask register
	move.w	#0,J_INT		; stop interrupt from Jerry to Tom
;	move.l	#0,BUTCH		; BUTCH disable

	move.l	BUTCH,d0
	andi.l	#~%1001,d0		; BUTCH disable
	move.l	d0,BUTCH

.ret:
	rts

;*======================================================================*
;* Fill up a DRAM block with CD subcode					*
;*======================================================================*

subcode:
	movem.l	d0-d7/a0-a6,-(sp)	; save registers

.gpustop:
;	move.l	GPU_stop,d0		; get GPU stop semaphore
;	beq.s	.ishaf			; GPU is still running - get data
;
;	bsr	stopcdg			; stop the GPU and DSP
;	move.l	#$DEADC0DE,d7		; error on half flag
;	illegal				; and bail out to db
;
.ishaf:	move.l	#Haf_flg,a3		; pointer to half buffer flag in DSP
	tst.l	(a3)			; check half buffer flag
	beq.s	.isful
	move.l	#Buf_ptr,a1		; pointer to start of buffer in DSP
	bra.s	.getdata

.isful:	move.l	#Ful_flg,a3		; pointer to full buffer flag in DSP
	tst.l	(a3)			; check full buffer flag
	beq	.ret
	move.l	#Mid_ptr,a1		; pointer to mid way pointer in DSP

.getdata:
	move.l	#CDGwords,a0		; pointer to symbol list
	add.l	subcount,a0		; position in subcode buffer

	move.l	#11,d7			; get 12 * 8 symbols

.descrmb:
	move.l	d7,-(sp)		; save d7

	move.l	(a1)+,d0		; d0 = SRQx
	move.l	(a1)+,d2		; d2 = WVUT

	move.l	d0,d1			; d0 = Srxx	- Used for S
	swap	d1			; d1 = xxsR	- Used for R
	move.l	d2,d3			; d2 = Wvut	- Used for W
	swap	d3			; d3 = utwV	- Used for V
	move.l	d2,d4			; d4 = wvUt	- Used for U
	move.l	d2,d5			; d5 = wvuT	- Used for T

	move.w	#7,d7			; 8 symbols per subcode read
.subsmbl:
	clr.w	d6			; make symbol in d6
	roxl.b	#1,d1			; get the R bit
	roxl.b	#1,d6			; result = 0000000r
	roxl.l	#1,d0			; get the S bit
	roxl.b	#1,d6			; result = 000000rs
	roxl.b	#1,d5			; get the T bit
	roxl.b	#1,d6			; result = 00000rst
	roxl.w	#1,d4			; get the U bit
	roxl.b	#1,d6			; result = 0000rstu
	roxl.b	#1,d3			; get the V bit
	roxl.b	#1,d6			; result = 000rstuv
	roxl.l	#1,d2			; get the W bit
	roxl.b	#1,d6			; result = 00rstuvw
	move.b	d6,(a0)+		; save it in buffer
	dbra	d7,.subsmbl		; get next symbol

.subnext:
	move.l	(sp)+,d7		; restore d7
	dbra	d7,.descrmb		; wait for next subcode

.clrflgs:
	move.l	#0,(a3)			; clear the half/full buffer flag in DSP
	add.l	#96,subcount		; 4 more packs (96 more symbols)
.subproc:
	cmp.l	#96*3,subcount		; do we have enough subcodes
	bne.s	.ret			; no, so quit

	jsr	procsub			; process the subcodes
	sub.l	#96,subcount		; 96 less symbols left in buffer

.wait:	move.l	B_CMD,d0		; wait for
	lsr.w	#1,d0			;  blitter to
.loop:	bcc	.wait			;  stop

.blit:	move.l	#CDGwords,A1_BASE
	move.l	#CDGwords+96,A2_BASE
	move.l	#0,A1_PIXEL
	move.l	#0,A2_PIXEL
	move.l	#(PITCH1|PIXEL32|WID256|XADDPHR),A1_FLAGS
	move.l	#(PITCH1|PIXEL32|WID256|XADDPHR),A2_FLAGS
	move.l	#$00010030,B_COUNT	; h = 1, w = 48
	move.l	#(SRCEN|LFU_AN|LFU_A|BUSHI),B_CMD

.ret:	movem.l	(sp)+,d0-d7/a0-a6	; restore registers
	rts

;************************************************************************
;************************************************************************

;*======================================================================*
;* process the four packets collected from the CD			*
;*======================================================================*

procsub:
	btst.b	#CDG_DISP,cdgflags	; should we process CD+G data
	beq.s	.ret			; no, so quit

.pack1:	move.l	#CDGwords+$00,a6	; a6 = pointer to 1st pack
	bsr.s	procpack		; process it

.pack2:	move.l	#CDGwords+$18,a6	; a6 = pointer to 2nd pack
	bsr.s	procpack		; process it

.pack3:	move.l	#CDGwords+$30,a6	; a6 = pointer to 3rd pack
	bsr.s	procpack		; process it

.pack4:	move.l	#CDGwords+$48,a6	; a6 = pointer to 4th pack
	bsr.s	procpack		; process it

.ret:	rts

;*======================================================================*
;* process the given pack						*
;*======================================================================*

procpack:
	cmp.b	#$09,SYMB00(a6)		; is it TV-graphics pack
	bne.s	.ret			; no, quit

.proc:	move.l	#proctabl,a0		; pointer to jump table
	move.b	SYMB01(a6),d0		; d0 = instruction type
	and.w	#$3F,d0			; mask off unnecessary bits
	lsl.w	#$02,d0			; d0 = byte offset in jump table
	move.l	(a0,d0),d0		; pointer to the required function
	beq.s	.ret			; quit, if function is NULL

	move.l	d0,a0			; jump to the required function
	jsr	(a0)			; which processes the given pack
.ret:	rts

;*======================================================================*
;* The TV-GRAPHICS mode PRESET-MEMORY instruction (1)			*
;*======================================================================*

tvsetmem:
	move.w	#0,tvPH			; initialise horz scroll pointer
	move.w	#0,tvPV			; initialise vert scroll pointer
.getcolr:
	move.b	SYMB04(a6),d0		; d1 = symbol 4
	andi.w	#$000F,d1		; d1 = 0000 0000 0000 cccc
	add.w	#TVCOLOFF,d1
	move.w	d1,d0			; d0 = d1
	lsl.w	#8,d0			; d0 = 0000 cccc 0000 0000
	or.w	d0,d1			; d1 = 0000 cccc 0000 cccc
	bsr	clrtvgrf		; set the TV graphics work memory to background color

.disp:	bset.b	#TVDSPFLG,cdgflags	; update TV-graphics display
.ret:	rts

;*======================================================================*
;* The TV-GRAPHICS mode PRESET-BORDER instruction (2)			*
;*======================================================================*

tvsetbrd:
	move.b	SYMB04(a6),d0		; d0 = symbol 4
	andi.l	#$000F,d0		; d0 = 0000 0000 0000 cccc
	move.l	d0,BrdColNo		; save border color number

.disp:	bset.b	#BRCOLFLG,cdgflags	; update border color
.ret:	rts

;*======================================================================*
;* The TV-GRAPHICS mode WRITE-FONT FOREGROUND/BACKGROUND instruction (6)*
;*======================================================================*

tvfntfbg:
	move.l	#(LFU_AN|LFU_A),d5	; REPLACE mode
	bsr.s	tvprpfnt		; blit the font
.ret:	rts

;*======================================================================*
;* The TV-GRAPHICS mode EXCLUSIVE-OR FONT instruction (38)		*
;*======================================================================*

tvxorfnt:
	move.l	#(LFU_AN|LFU_NA),d5	; XOR mode
	bsr.s	tvprpfnt		; blit the font
.ret:	rts

;*======================================================================*
;* Prepares FONT data to be blitted in REPLACE/XOR format		*
;*======================================================================*

tvprpfnt:
.ch0:	move.b	SYMB04(a6),d0		; d0 = symbol 4
	lsr.b	#$02,d0			; d0 = (d0 >> 2)
	andi.b	#$0C,d0			; d0 = (d0 & 0x000C)
.ch1:	move.b	SYMB05(a6),d1		; d1 = symbol 5
	lsr.b	#$04,d1			; d1 = (d1 >> 4)
	andi.b	#$03,d1			; d1 = (d1 & 0x0003)
.chno:	or.b	d1,d0			; d0 = channel number

.chkch:	cmpi.b	#1,d0			; (chno > 1) ?
	bgt.s	.usrch			; yes, check for the user channel
	bra.s	.chok			; no, go blit the font
.usrch:	move.w	TvChnlAv,d1		; d1 = available channel flags
	bset.l	d0,d1			; d1 = set the new channel flag
	move.w	d1,TvChnlAv		; save channel available flags
	cmp.b	TvChnlNo,d0		; is font from the user channel ?
	bne	.ret			; no, quit
.chok:

.xpos:	move.b	SYMB07(a6),d3		; d3 = symbol 7
	andi.w	#$003F,d3		; d3 = 00xx xxxx = column position
	cmpi.b	#49,d3			; if (x > 49)
	bgt	.ret			; yes, quit

.ypos:	move.b	SYMB06(a6),d4		; d4 = symbol 6
	andi.w	#$001F,d4		; d4 = 000y yyyy = row position
	cmpi.b	#17,d4			; if (y > 17)
	bgt	.ret			; yes, quit

.prep:	move.l	#fontblit,a0		; a0 = pointer to font data
	move.l	#offstabl,a1		; a1 = offset table in pack data
	add.l	#$04,a1			; a1 = offset to 8th symbol
	move.w	#$02,d7			; d7 = 2 (total of 12 symbols)

.loop:	move.w	#0,d6			; clear d6

	move.b	(a1)+,d6		; d6 = offset in pack
	move.b	(a6,d6),d0		; d0 = 1st symbol
	lsl.b	#2,d0			; 1st byte
	lsl.l	#8,d0			; d0 = xxxx11xx

	move.b	(a1)+,d6		; d6 = offset in pack
	move.b	(a6,d6),d0		; d0 = 2nd symbol
	lsl.b	#2,d0			; 2nd byte
	lsl.l	#8,d0			; d0 = xx1122xx

	move.b	(a1)+,d6		; d6 = offset in pack
	move.b	(a6,d6),d0		; d0 = 3rd symbol
	lsl.b	#2,d0			; 3rd byte
	lsl.l	#8,d0			; d0 = 112233xx

	move.b	(a1)+,d6		; d6 = offset in pack
	move.b	(a6,d6),d0		; d0 = 4th symbol
	lsl.b	#2,d0			; 4th byte

	move.l	d0,(a0)+		; d0 = 11223344 -> save 4 bytes
.next:	dbra	d7,.loop		; check next symbol

.bit2pix:
	bsr	blitidle

.bgcol:	move.b	SYMB04(a6),d1		; d1 = symbol 4 = xxxxxx0B = background color
	andi.w	#$000F,d1
	cmp.l	#(LFU_AN|LFU_NA),d5	; is XOR mode ?
	beq.s	.skpbg			; yes, no need for offset
	add.w	#TVCOLOFF,d1		; no, add offset
.skpbg:	move.b	d1,d0			; d0 = xxxxxx0B
	lsl.w	#8,d0			; d0 = xxxx0B00
	or.b	d1,d0			; d0 = xxxx0B0B
	move.w	d0,d1			; d1 = xxxx0B0B
	swap	d1			; d1 = 0B0Bxxxx
	move.w	d0,d1			; d1 = 0B0B0B0B
	move.l	d1,B_DSTD		; initialise destination data
	move.l	d1,B_DSTD+4		; register with background color

.fgcol:	move.b	SYMB05(a6),d2		; d2 = symbol 5 = xxxxxx0B = foreground color
	andi.w	#$000F,d2
	cmp.l	#(LFU_AN|LFU_NA),d5	; is XOR mode ?
	beq.s	.skpfg			; yes, no need for offset
	add.w	#TVCOLOFF,d2		; no, add offset
.skpfg:	move.b	d2,d0			; d0 = xxxxxx0B
	lsl.w	#8,d0			; d0 = xxxx0B00
	or.b	d2,d0			; d0 = xxxx0B0B
	move.w	d0,d2			; d2 = xxxx0B0B
	swap	d2			; d2 = 0B0Bxxxx
	move.w	d0,d2			; d2 = 0B0B0B0B
	move.l	d2,B_PATD		; initialise pattern data
	move.l	d2,B_PATD+4		; register with foreground color

.blit:	move.l	#0,A1_PIXEL
	move.l	#0,A2_PIXEL

	move.l	#$0001FFF8,A1_STEP
	move.l	#$0001FFF8,A2_STEP

	move.l	#((12<<16)|(8)),B_COUNT

	move.l	#fontdata,A1_BASE
	move.l	#fontblit,A2_BASE

	move.l	#(PITCH1|PIXEL8|WID8|XADDPIX),A1_FLAGS
	move.l	#(PITCH1|PIXEL1|WID8|XADDPIX),A2_FLAGS

	move.l	#(SRCEN|UPDA1|UPDA2|PATDSEL|BCOMPEN|BKGWREN|BUSHI),B_CMD

.font:	move.l	d5,d2			; REPLACE/XOR mode
	bsr	bltfnt08		; blit font on offscreen buffer

.disp:	bset.b	#TVDSPFLG,cdgflags	; update TV-graphics display
.ret:	rts

;*======================================================================*
;* The TV-GRAPHICS mode SCROLL SCREEN WITH PRESET instruction (20)	*
;*======================================================================*

tvscrlst:
.color:	move.b	SYMB04(a6),d0		; d0 = symbol 4
	andi.w	#$000F,d0		; d0 = 0000 0000 0000 cccc
	add.w	#TVCOLOFF,d0		; offset for colour table
	move.w	d0,d1			; d1 = d0
	lsl.w	#8,d1			; d1 = 0000 cccc 0000 0000
	or.w	d1,d0			; d0 = 0000 cccc 0000 cccc
	move.w	d0,ScrlColr		; save scroll with preset color

.scrol:	move.w	#FALSE,ScrlType		; reset to scroll with preset
	bsr.s	tvscroll		; scroll screen
.ret:	rts

;*======================================================================*
;* The TV-GRAPHICS mode SCROLL SCREEN WITH COPY instruction (24)	*
;*======================================================================*

tvscrlcp:
.scrol:	move.w	#TRUE,ScrlType		; scroll with copy
	bsr.s	tvscroll		; scroll screen
.ret:	rts

;*======================================================================*
;* scroll the TV-GRAPHICS screen					* 
;*======================================================================*

tvscroll:
	move.w	#0,d0			; clear d0.w
.tvph:	move.b	SYMB05(a6),d1		; d1 = symbol 5
	move.b	d1,d0			; d0 = symbol 5
	andi.b	#$07,d0			; d0 = tvPH
	cmpi.b	#5,d0			; if (tvPH > 5)
	bgt	.ret			; yes, quit
	move.w	d0,tvPH			; save tvPH

.tvpv:	move.b	SYMB06(a6),d2		; d2 = symbol 6
	move.b	d2,d0			; d0 = symbol 6
	andi.b	#$0F,d0			; d0 = tvPV
	cmpi.b	#11,d0			; if (tvPV > 11)
	bgt	.ret			; yes, quit
	move.w	d0,tvPV			; save tvPV

.chkrt:	btst.l	#4,d1			; copy right ?
	beq.s	.chklt			; no, check further
.cpyrt:	move.w	#6,d3			; yes, copy right
	bra.s	.ocoph
.chklt:	btst.l	#5,d1			; copy left ?
	beq.s	.cpyh0			; no, check further
.cpylt:	move.w	#-6,d3			; yes, copy left
	bra.s	.ocoph
.cpyh0:	move.w	#0,d3			; no horizontal copy

.ocoph:	move.w	d3,tvCOPH		; save tvCOPH

.chkdn:	btst.l	#4,d2			; copy down ?
	beq.s	.chkup			; no, check further
.cpydn:	move.w	#12,d4			; yes, copy down
	bra.s	.ocopv
.chkup:	btst.l	#5,d2			; copy up ?
	beq.s	.cpyv0			; no, check further
.cpyup:	move.w	#-12,d4			; yes, copy up
	bra.s	.ocopv
.cpyv0:	move.w	#0,d4			; no vertical copy

.ocopv:	move.w	d4,tvCOPV		; save tvCOPV

.scrol:	or.w	d3,d4			; check if any one of them set
	beq.s	.disp			; no, so no need for scrolling
	bsr	updtvgrf		; update TV graphics memory
	move.w	#0,tvCOPH		; clear tvCOPH
	move.w	#0,tvCOPV		; clear tvCOPV

.disp:	bset.b	#TVDSPFLG,cdgflags	; update TV-graphics display
.ret:	rts

;************************************************************************
;* ROUTINES FOR BLITTING TV-GRAPHICS					*
;************************************************************************

;*======================================================================*
;* BLITIDLE	: waits for the BLITTER IDLE				*
;*======================================================================*

blitidle:
.wait:	move.l	B_CMD,d0
	lsr.b	#1,d0
	bcc.s	.wait
.ret:	rts

;*======================================================================*
;* CLRTVDSP	: clear the display memory for TV Graphics		*
;* INPUT	: d1.w = color to be displayed (0c0c)			*
;*======================================================================*

clrtvdsp:
	bsr	blitidle		; wait for blitter idle

	move.w	d1,B_PATD		; select the color
	move.w	d1,B_PATD+2
	move.w	d1,B_PATD+4
	move.w	d1,B_PATD+6

	move.l	#tvdspmem,A1_BASE	; point to the buffer
	move.l	#0,A1_PIXEL		; pointer to (0,0)
	move.l	#$0001FEC0,A1_STEP	; step y = 1, x = -320
	move.l	#((192<<16)|(320)),B_COUNT ; do h = 192, w = 320
	move.l	#(PITCH1|PIXEL8|WID320|XADDPHR),A1_FLAGS ; flags

	move.l	#(PATDSEL|UPDA1),B_CMD	; start the BLTTER
.ret:	rts

;*======================================================================*
;* CLRTVGRF	: clear the graphic memory for TV-Graphics		*
;* INPUT	: d1.w = color to be displayed (0c0c)			*
;*======================================================================*

clrtvgrf:
	bsr	blitidle		; wait for blitter idle

	move.w	d1,B_PATD		; select the color
	move.w	d1,B_PATD+2
	move.w	d1,B_PATD+4
	move.w	d1,B_PATD+6

	move.l	tvgrfmem,A1_BASE	; point to the buffer
	move.l	#0,A1_PIXEL		; pointer to (0,0)
	move.l	#$0001FEC0,A1_STEP	; step y = 1, x = -320
	move.l	#((240<<16)|(320)),B_COUNT ; do h = 240, w = 320
	move.l	#(PITCH1|PIXEL8|XADDPHR|WID320),A1_FLAGS ; flags

	move.l	#(PATDSEL|UPDA1),B_CMD	; start the BLTTER
.ret:	rts

;*======================================================================*
;* bltfnt08(x, y)							*
;* INPUT :	d2.l = replace/xor/or					*
;*		d3.w = x position					*
;*		d4.w = y position					*
;*									*
;* Blit a font with 8 bits per pixel to TV-Graf Memory at (x,y)		*
;*======================================================================*

bltfnt08:
	bsr	blitidle		; wait for blitter idle

	mulu	#$6,d3			; calculate x position in pixel
	addi.w	#$6,d3			; add extra 6 bit offset on left to keep scope for scrolling with copy

	mulu	#$C,d4			; calculate y position in pixel
	addi.w	#$C,d4			; add extra 12 bit offset on top to keep scope for scrolling with copy

.a1pix:
	move.w	d4,d0			; y position
	swap	d0			; set the destination pixel pointer 
	move.w	d3,d0			; must be y|x (save x position in d3)
	move.l	d0,A1_PIXEL		; offset into draw buffer

	move.l	#((1<<16)|(6)),d1	; step increment y = 1, x = (-6 + delta)
	add.w	d1,d0			; d0 = end pixel in blit
	andi.w	#$7,d0			; check if end pixel on phrase boundary
	beq.s	.a1step			; yes, set step increment
	addi.w	#8,d1			; now add an offset of (8-d0) to the end pixel,
	sub.w	d0,d1			;   so that it reaches the phrase boundary
.a1step:
	neg.w	d1
	move.l	d1,A1_STEP		; step increment y = 1, x = -6+delta

.a2pix:
	move.l	#0,A2_PIXEL		; source is always from 0,0
	cmp.w	#2,d0
	beq.s	.a2step2
	cmp.w	#4,d0
	beq.s	.a2step2
.a2step1:
	move.l	#$0001FFF8,A2_STEP	; step increment y = 1, x = -6+delta(-2) = -8
	bra.s	.bcount
.a2step2:
	move.l	#$0001FFF0,A2_STEP	; step increment y = 1, x = -6+delta(-10) = -16

.bcount:
	move.l	#((12<<16)|(6)),B_COUNT	; inner/outer loop counters

.aflags:
	move.l	#(PITCH1|PIXEL8|WID320|XADDPHR),A1_FLAGS ; source flags
	move.l	#(PITCH1|PIXEL8|WID8|XADDPHR),A2_FLAGS	 ; destination flags

.abases:
	move.l	tvgrfmem,A1_BASE	; pointer to destination buffer
	move.l	#fontdata,A2_BASE	; pointer to character data

.bcmd:	or.l	#(SRCEN|UPDA1|UPDA2|DSTEN|BUSHI),d2
	move.l	d2,B_CMD		; set up the blitter commands
	rts

;*======================================================================*
;* updtvgrf	: updates the graphics memory for commands like, screen	*
;*		  update with preset/copy				*
;*									*
;* SCROLL GRAPHICS MEMORY, IF NECESSARY					*
;*									*
;* in this case copy the bit-blit from TV-graphics memory left/right	*
;* and up/down according to scroll with preset or copy instruction	*
;*									*
;* arrange source/destination pixel pointers as well as the blitter	*
;* flags so that the overlapping memory area problem is taken care of	*
;*======================================================================*

updtvgrf:
	bsr	blitidle		; wait for blitter idle

	move.l	#(SRCEN|UPDA1|UPDA2|LFU_A|LFU_AN),d2
.a1pix:
	move.w	#12,d0
	add.w	tvCOPV,d0		; destination y
	swap	d0
	move.w	#6,d0
	add.w	tvCOPH,d0		; destination x
	move.l	d0,A1_PIXEL		; prepare the destination pixel pointer
	move.w	d0,d3			; d3 = save x position

	move.l	#((1<<16)|(300)),d1	; step y = 1, x = -(300 + delta)
	add.w	d1,d0			; end pixel in blit
	andi.w	#$7,d0			; check if end pixel on phrase boundary
	beq.s	.a1step			; yes, set step increment
	addi.w	#8,d1			; now add an offset of (8-d0) to the end pixel,
	sub.w	d0,d1			;   so that it reaches the phrase boundary
.a1step:
	neg.w	d1
	move.l	d1,A1_STEP		; setup the destination step increment
.a2pix:
	move.l	#((12<<16)|(6)),A2_PIXEL ; source y = 12, x = 6
	cmp.w	#$6,d3			; are the starting x position same
	beq.s	.a2step			; no, then no need to make two source reads
	or.l	#SRCENX,d2		; the command flags for phrase boundary
.a2step:
	move.l	#$0001FECE,A2_STEP	; step increment : y = 1, x = -306
.bcount:
	move.l	#((216<<16)|(300)),B_COUNT ; inner/outer loop counters
.aflags:
	move.l	#(PITCH1|PIXEL8|WID320|XADDPHR),A1_FLAGS ; setup the source flags
	move.l	#(PITCH1|PIXEL8|WID320|XADDPHR),A2_FLAGS ; setup the destination flags
.abases:
	move.l	tvotherm,A1_BASE	; base adress of destination buffer
	move.l	tvgrfmem,A2_BASE	; base of address of source buffer

.bcmd:	move.l	d2,B_CMD		; let blitter go

;****************************************
;* SWAP TV graphics memory with it's
;* alternater memory buffer
;****************************************

.swaptvg:
	move.l	tvgrfmem,d0
	move.l	tvotherm,d1

	move.l	d0,tvotherm
	move.l	d1,tvgrfmem

;****************************************
;* PRESET/COPY the border according to
;* scroll instruction
;****************************************

.hz:	move.w	tvCOPH,d0
	beq.s	.vt
	bmi.s	.hzneg
.hzpos:	move.l	#((0<<16)|(6)),d1	; destination (y|x)
	move.l	#((0<<16)|(306)),d2	; source (y|x)
	bra.s	.chblt
.hzneg:	move.l	#((0<<16)|(300)),d1	; destination (y|x)
	move.l	#((0<<16)|(0)),d2	; source (y|x)
.chblt:	move.l	#((240<<16)|(6)),d3	; (height|width)
	bsr	blttvgrf

.vt:	move.w	tvCOPV,d0
	beq.s	.ret
	bmi.s	.vtneg
.vtpos:	move.l	#((12<<16)|(0)),d1	; destination (y|x)
	move.l	#((228<<16)|(0)),d2	; source y
	bra.s	.cvblt
.vtneg:	move.l	#((216<<16)|(0)),d1	; destination y
	move.l	#((0<<16)|(0)),d2	; source y
.cvblt:	move.l	#((12<<16)|(312)),d3	; height
	bsr	blttvgrf

.ret:	rts

;*======================================================================*
;* BLTTVGRF	: blit one portion of tv graphics memory to another	*
;*									*
;* INPUT :								*
;*	D1.l : Destination Y|X						*
;*	D2.l : Source Y|X						*
;*	D3.l : Height|Width						*
;*======================================================================*

blttvgrf:
	bsr	blitidle		; wait for blitter idle

	move.w	ScrlType,d0		; check if copy with preset or copy
	beq.s	scrlset
scrlcpy:				; SCROLL WITH COPY

; COPY SOURCE BLOCK TO DESTINATION BLOCK

	move.l	d1,A1_PIXEL		; destination pixel pointer (y|x)
	move.l	d2,A2_PIXEL		; source pixel pointer (y|x)

	move.l	#$00010000,d0
	move.w	d3,d0
	neg.w	d0
	move.l	d0,A1_STEP		; source step increment
	move.l	d0,A2_STEP		; destination step increment

	move.l	d3,B_COUNT		; set the inner\outer loop counters

	move.l	tvgrfmem,A1_BASE	; pointer to destination buffer
	move.l	tvgrfmem,A2_BASE	; pointer to source buffer

	move.l	#(PITCH1|PIXEL8|WID320|XADDPIX),A1_FLAGS ; source flags
	move.l	#(PITCH1|PIXEL8|WID320|XADDPIX),A2_FLAGS ; destination flags

	move.l	#(SRCEN|UPDA1|UPDA2|LFU_A|LFU_AN),B_CMD ; let it go
.ret:	rts

scrlset:				; SCROLL WITH PRESET

; CLEAR THE DESTINATION BLOCK

	bsr	blitidle		; wait for blitter idle

	move.w	ScrlColr,d0		; set the fill pattern and color
	move.w	d0,B_PATD		; select the color
	move.w	d0,B_PATD+2
	move.w	d0,B_PATD+4
	move.w	d0,B_PATD+6

	move.l	d1,A1_PIXEL		; source pixel pointer (y|x)

	move.l	#$00010000,d0
	move.w	d3,d0
	neg.w	d0
	move.l	d0,A1_STEP		; step -width x, +1 y each line

	move.l	d3,B_COUNT		; set the inner/outer loop counters

	move.l	tvgrfmem,A1_BASE	; point to the buffer

	move.l	#(PITCH1|PIXEL8|WID320|XADDPIX),A1_FLAGS ; set the flags

	move.l	#(PATDSEL|UPDA1),B_CMD	; start the BLTTER
.ret:	rts

;************************************************************************
;************************************************************************

;*======================================================================*
;* The TV-GRAPHICS mode	LOAD CLUT Colour-0..7 instruction (30)		*
;*			LOAD CLUT Colour-8..15 instruction (31)		*
;*									*
;* The TV-GRAPHICS mode DEFINE COLOUR TRANSPARENCY instruction (28)	*
;*======================================================================*

tvldclt0:
	move.l	#0,d0
	bsr	ldclut1
.ret:	rts

tvldclt1:
	move.l	#8,d0
	bsr	ldclut1
.ret:	rts

tvcoltrn:
;	move.l	#0,d0
;	bsr	ldtrans
.ret:	rts

;*======================================================================*
;* Load the CLUT given the starting position in table and the pack	*
;*									*
;* This routine loads a 12 bit colour composition as used in TV-grapics	*
;* mode (when load clut is called)					*
;*									*
;*	RED table	- 000rrrr0					*
;*	GREEn table	- 00gggg00					*
;*	BLUE table	- 000bbbb0					*
;*									*
;* input	: d0.l - the starting position in table			*
;*======================================================================*

ldclut1:
	move.b	#FALSE,d5		; d5.b = change colour flag

	move.l	#TvColRed,a3		; a3 = table for RED component
	move.l	#TvColGrn,a4		; a4 = table for GREEN component
	move.l	#TvColBlu,a5		; a5 = table for BLUE component

	add.l	d0,a3			; a3 = starting position in RED table
	add.l	d0,a4			; a4 = starting position in GREEN table
	add.l	d0,a5			; a5 = the starting position in BLUE table

	move.l	#offstabl,a0		; a0 = offset table for descrabling pack data
	move.l	#0,d0			; clear d0
	move.w	#7,d7			; load 8 colours from pack

.loop:	move.b	(a0)+,d0		; d0 = offset in pack data
	move.b	(a6,d0),d1		; d1 = current symbol
	move.b	d1,d2			; d2 = d1

	lsr.b	#$02,d1			; red = ((color >> 2) & 0x0F)
	andi.b	#$0F,d1			; d1 = 0000 rrrr
	lsl.b	#$02,d2			; grn = ((color << 2) & 0x0C)
	andi.b	#$0C,d2			; d2 = 0000 gg00

	move.b	(a0)+,d0		; d0 = offset in pack data
	move.b	(a6,d0),d0		; d0 = current symbol
	move.b	d0,d3			; d3 = d0

	lsr.b	#$04,d0			; grn = ((color >> 4) & 0x03)
	andi.b	#$03,d0			; d0 = 0000 00gg
	or.b	d0,d2			; d2 = 0000 gggg
	andi.b	#$0F,d3			; d3 = 0000 bbbb

.chkR:	cmp.b	(a3),d1			; check if RED component changed
	beq.s	.noR			; no, so no need to set new RED
	move.b	d1,(a3)			; save RED   (0000 rrrr)
	move.b	#TRUE,d5		; change colour table
.noR:	addq.l	#1,a3

.chkG:	cmp.b	(a4),d2			; check if GREEN component changed
	beq.s	.noG			; no, so no need to set new GREEN
	move.b	d2,(a4)			; save GREEN (0000 gggg)
	move.b	#TRUE,d5		; change colour table
.noG:	addq.l	#1,a4

.chkB:	cmp.b	(a5),d3			; check if BLUE component changed
	beq.s	.noB			; no, so no need to set new BLUE
	move.b	d3,(a5)			; save BLUE  (0000 bbbb)
	move.b	#TRUE,d5		; change colour table
.noB:	addq.l	#1,a5

	dbra	d7,.loop		; continue for next symbols

.setflgs:
	tst.b	d5
	beq.s	.ret

	bset.b	#TVCOLFLG,cdgflags	; update the colour table
	bset.b	#BRCOLFLG,cdgflags	; update border oclour
.ret:	rts

;*======================================================================*
;* Load the transparency factor for different color components and	*
;* update the colour table						*
;*									*
;* input	: d0.l - the starting position in table			*
;*======================================================================*

ldtrans:
	move.b	#FALSE,d5		; d5.b = change colour flag

	move.l	#TransFac,a3		; transparency factor
	add.l	d0,a3			; a3 = starting position in table

	move.l	#offstabl,a0		; a0 = offset table for descrabling pack data
	move.l	#0,d0			; clear d0
	move.w	#15,d7			; load 15 additional colour bits from pack

.loop:	move.b	(a0)+,d0		; d0 = offset in current pack
	move.b	(a6,d0),d0		; d0 = TRANSn factor
	and.b	#$3F,d0			; d0 = 6 bits of Transparency factor

.chkT:	move.b	(a3),d1			; d1.b = old TRANs component
	cmp.b	d0,d1			; check if transparency flag changed
	beq.s	.noT			; no, so no need to set new TRANs
	move.b	d0,(a3)			; save transparency factor
	move.b	#TRUE,d5		; change colour table
.noT:	addq.l	#1,a3

	dbra	d7,.loop		; continue for next symbols

.setflgs:
	tst.b	d5
	beq.s	.ret

	bset.b	#TVCOLFLG,cdgflags	; update the colour table
	bset.b	#BRCOLFLG,cdgflags	; update border oclour
.ret:	rts

;************************************************************************
;************************************************************************

;*======================================================================*
;* DISPSUB	: diaplay at frame interrupt				*
;*======================================================================*

dispsub:
	movem.l	d0-d7/a1-a5,-(sp)	; save registers
	move.b	cdgflags,d6		; d6 = CD+G flags

.updcl:	btst.l	#TVCOLFLG,d6		; update system color table ?
	beq.s	.ovrcl			; no, quit
	bsr	setcolor		; yes, update it
	bclr.l	#TVCOLFLG,d6		; CLUT updated
.ovrcl:

.updbr:	btst.l	#BRCOLFLG,d6		; update border color ?
	beq.s	.ovrbr			; no, quit
	bsr	setbordr		; set border color
	bclr.l	#BRCOLFLG,d6		; Border color updated
.ovrbr:

.updtv:	btst.l	#TVDSPFLG,d6		; display TV-graphics ?
	beq.s	.ovrtv			; no, quit
	bsr	cp2tvdsp		; yes, update it
	bclr.l	#TVDSPFLG,d6		; TV-display updated
.ovrtv:

.ret:	move.b	d6,cdgflags		; save CD+G flags
	movem.l	(sp)+,d0-d7/a1-a5	; restore registers
	rts

;*======================================================================*
;* setcolor	: set color table from primary/secondary color table	*
;*		  depending upon the memory coltrol instruction, if in	*
;*		  extended TV-graphics mode, of from primary memory	*
;*		  if it is TV-graphics mode.				*
;*									*
;*		  Also, apply the transparency factor, if in		*
;*		  TV-graphics mode					*
;*======================================================================*

setcolor:
	move.l	#CLUT,a1		; a1 = pointer to system CLUT
	add.l	#(TVCOLOFF*2),a1	; offset for colour table
	move.l	#TransFac,a2		; a2 = table for transparency factor
	move.l	#TvColRed,a3		; a3 = table for RED component
	move.l	#TvColGrn,a4		; a4 = table for GREEN component
	move.l	#TvColBlu,a5		; a5 = table for BLUE component

	move.w	#$0F,d7			; load system colour table (15 colors)

.loop:	move.l	#$3F,d0			; d0 = TRANs factor = 63
	move.l	#$00,d1			; d1 = RED component
	move.l	#$00,d2			; d2 = GREEN component
	move.l	#$00,d3			; d3 = BLUE component

	sub.b	(a2)+,d0		; d0 = TRANs factor = 63-TRANSn
	move.b	(a3)+,d1		; d1 = RED component
	move.b	(a4)+,d2		; d2 = GREEN component
	move.b	(a5)+,d3		; d3 = BLUE component

	mulu	d0,d1			; d1 = (63-TRANSn)*r
	mulu	d0,d2			; d2 = (63-TRANSn)*g
	mulu	d0,d3			; d3 = (63-TRANSn)*b

	divu	#$3F,d1			; d1 = ((63-TRANSn)*r)/63
	divu	#$3F,d2			; d2 = ((63-TRANSn)*g)/63
	divu	#$3F,d3			; d3 = ((63-TRANSn)*b)/63

	lsl.w	#$8,d1			; d1 = 0000 rrrr 0000 0000
	lsl.w	#$4,d1			; d1 = rrrr 0000 0000 0000
	lsl.w	#$7,d3			; d3 = 0000 0bbb b000 0000
	lsl.w	#$2,d2			; d2 = 0000 0000 00gg gg00

	or.w	d1,d2			; d2 = rrrr 0bbb b000 0000
	or.w	d3,d2			; d2 = rrrr 0bbb b0gg gg00

.clup:	move.w	d2,(a1)			; save colour in CLUT
	cmp.w	(a1),d2			; is it written properly ?
.cbak:	bne.s	.clup			; no, keep trying
	addq.w	#2,a1

	dbra	d7,.loop		; continue for next symbols
.ret:	rts

;*======================================================================*
;* set the border colour from colour table				*
;*======================================================================*

setbordr:
	move.l	BrdColNo,d5		; get border colour number

	move.l	#CLUT,a1		; pointer to colour lookup table
	add.l	#(TVCOLOFF*2),a1	; offset for colour table
	move.l	#TransFac,a2		; a2 = table for transparency factor
	move.l	#TvColRed,a3		; a3 = table for RED component
	move.l	#TvColGrn,a4		; a4 = table for GREEN component
	move.l	#TvColBlu,a5		; a5 = table for BLUE component

	move.l	#$3F,d0			; d0 = TRANs factor = 63
	move.l	#$00,d1			; d1 = RED component
	move.l	#$00,d2			; d2 = GREEN component
	move.l	#$00,d3			; d3 = BLUE component

	sub.b	(a2,d5),d0		; d0 = TRANs factor = 63-TRANSn
	move.b	(a3,d5),d1		; d1 = RED component
	move.b	(a4,d5),d2		; d2 = GREEN component
	move.b	(a5,d5),d3		; d3 = BLUE component

	mulu	d0,d1			; d1 = (63-TRANSn)*r
	mulu	d0,d2			; d2 = (63-TRANSn)*g
	mulu	d0,d3			; d3 = (63-TRANSn)*b

	divu	#$3F,d1			; d1 = ((63-TRANSn)*r)/63
	divu	#$3F,d2			; d2 = ((63-TRANSn)*g)/63
	divu	#$3F,d3			; d3 = ((63-TRANSn)*b)/63

	and.l	#$0F,d1			; d1 = 0000 0000 0000 0000 0000 0000 0000 rrrr
	and.l	#$0F,d2			; d2 = 0000 0000 0000 0000 0000 0000 0000 gggg
	and.l	#$0F,d3			; d3 = 0000 0000 0000 0000 0000 0000 0000 bbbb

	move.l	#20,d0
	lsl.l	d0,d1			; d1 = 0000 0000 rrrr 0000 0000 0000 0000 0000
	move.l	#28,d0
	lsl.l	d0,d2			; d2 = gggg 0000 0000 0000 0000 0000 0000 0000
	move.l	#4,d0
	lsl.l	d0,d3			; d3 = 0000 0000 0000 0000 0000 0000 bbbb 0000

	or.l	d2,d1			; d1 = 0000 0000 rrrr 0000 0000 0000 bbbb 0000
	or.l	d3,d1			; d1 = gggg 0000 rrrr 0000 0000 0000 bbbb 0000
	move.l	d1,BORD1		; set the border color

	lsl.l	#1,d5			; multiply by 2 for words
	move.w	(a1,d5),BG		; colour (16 bit RGB)
.ret:	rts

;*======================================================================*
;* cp2tvdsp	: Copies the graphics memory to display memory when a	*
;*		  update of graphics memory occurs			*
;*======================================================================*

cp2tvdsp:

;****************************************
;* UPDATE DISPLAY FROM GRAPHICS MEMORY -
;*
;* copy the graphic memory to the display
;* memory depending upon (tvPH, tvPV) and
;* (tvCOPH, TVCOPV) starting from FONT
;* at position (1,1)
;*
;* Also, there is an extra one-font 
;* border around TV-graphics memory, 
;* which is used for Scrolling with COPY
;*
;****************************************

	bsr	blitidle		; wait for blitter idle

	move.l	#(SRCEN|UPDA1|UPDA2|LFU_A|LFU_AN),d2
.a1pix:
	move.l	#0,A1_PIXEL		; initialise the destination pixel pointer
.a1step:
	move.l	#$0001FEE0,A1_STEP	; step increment : y = 1, x = -288

.a2pix:
	move.w	#24,d0			; initialise the source pixel pointer
	add.w	tvPV,d0
	sub.w	tvCOPV,d0
	swap	d0
	move.w	#12,d0
	add.w	tvPH,d0
	sub.w	tvCOPH,d0
	move.l	d0,A2_PIXEL
	move.w	d0,d3			; d3 = save source starting x

	move.l	#((1<<16)|(288)),d1	; y = 1, x = -(288 + delta)
	add.w	d1,d0			; d0 = end pixel in blit
	andi.w	#$7,d0			; check if end pixel on phrase boundary
	beq.s	.a2flag			; yes, set step increment
	addi.w	#8,d1			; now add an offset of (8-d0) to the end pixel,
	sub.w	d0,d1			;   so that it reaches the phrase boundary
.a2flag:
	and.w	#$7,d3			; check if starts from a phrase boundary
	beq.s	.a2step			; yes, then no need for two source reads
	or.l	#SRCENX,d2		; the command flags for phrase boundary
.a2step:
	neg.w	d1			; x step value
	move.l	d1,A2_STEP		; step for A2 1 = +1 y|-288 x

.bcount:
	move.l	#((192<<16)|(288)),B_COUNT ; height|width of playfield

.aflags:
	move.l	#(PITCH1|PIXEL8|WID320|XADDPHR),A1_FLAGS ; source flags
	move.l	#(PITCH1|PIXEL8|WID320|XADDPHR),A2_FLAGS ; destination flags

.abases:
	move.l	#tvdspmem,A1_BASE	; base address of destination buffer
	move.l	tvgrfmem,A2_BASE	; base address of character data

.bcmd:	move.l	d2,B_CMD		; set up the blitter commands

.ret:	rts

;************************************************************************
;*	CONSTANT DATA AREA						*
;************************************************************************

		.data

;*======================================================================*
;*======================================================================*

;*
;* MAIN JUMP TABLE FOR TV-GRAPHICS PROCESSING
;*
		.phrase

proctabl:	.dc.l	$00000000	; 00 
		.dc.l	tvsetmem	; 01 - TV GRAPHICS - PRESET MEMORY
		.dc.l	tvsetbrd	; 02 - TV HRAPHICS - PRESET BORDER
		.dc.l	$00000000	; 03 
		.dc.l	$00000000	; 04 
		.dc.l	$00000000	; 05 
		.dc.l	tvfntfbg	; 06 - TV GRAPHICS - FONT FOREGROUND/BACKGROUND
		.dc.l	$00000000	; 07 
		.dc.l	$00000000	; 08 
		.dc.l	$00000000	; 09 
		.dc.l	$00000000	; 0A 
		.dc.l	$00000000	; 0B 
		.dc.l	$00000000	; 0C 
		.dc.l	$00000000	; 0D 
		.dc.l	$00000000	; 0E 
		.dc.l	$00000000	; 0F 
		.dc.l	$00000000	; 10 
		.dc.l	$00000000	; 11 
		.dc.l	$00000000	; 12 
		.dc.l	$00000000	; 13 
		.dc.l	tvscrlst	; 14 - TV GRAPHICS - SOFT SCROLL SCREEN WITH PRESET
		.dc.l	$00000000	; 15 
		.dc.l	$00000000	; 16 
		.dc.l	$00000000	; 17 
		.dc.l	tvscrlcp	; 18 - TV GRAPHICS - SOFT SCROLL SCREEN WITH COPY
		.dc.l	$00000000	; 19 
		.dc.l	$00000000	; 1A 
		.dc.l	$00000000	; 1B 
		.dc.l	tvcoltrn	; 1C - TV GRAPHICS - DEFINE COLOUR TRANSPARENCY
		.dc.l	$00000000	; 1D 
		.dc.l	tvldclt0	; 1E - TV GRAPHICS - LOAD CLUT - COLOUR 0..7
		.dc.l	tvldclt1	; 1F - TV GRAPHICS - LOAD CLUT - COLOUR 8- 15
		.dc.l	$00000000	; 20 
		.dc.l	$00000000	; 21 
		.dc.l	$00000000	; 22 
		.dc.l	$00000000	; 23 
		.dc.l	$00000000	; 24 
		.dc.l	$00000000	; 25 
		.dc.l	tvxorfnt	; 26 - TV GRAPHICS - EXCLUSIVE OR FONT WITH 2 COLOURS
		.dc.l	$00000000	; 27 
		.dc.l	$00000000	; 28 
		.dc.l	$00000000	; 29 
		.dc.l	$00000000	; 2A 
		.dc.l	$00000000	; 2B 
		.dc.l	$00000000	; 2C 
		.dc.l	$00000000	; 2D 
		.dc.l	$00000000	; 2E 
		.dc.l	$00000000	; 2F 
		.dc.l	$00000000	; 30 
		.dc.l	$00000000	; 31 
		.dc.l	$00000000	; 32 
		.dc.l	$00000000	; 33 
		.dc.l	$00000000	; 34 
		.dc.l	$00000000	; 35 
		.dc.l	$00000000	; 36 
		.dc.l	$00000000	; 37 
		.dc.l	$00000000	; 38 
		.dc.l	$00000000	; 39 
		.dc.l	$00000000	; 3A 
		.dc.l	$00000000	; 3B 
		.dc.l	$00000000	; 3C 
		.dc.l	$00000000	; 3D 
		.dc.l	$00000000	; 3E 
		.dc.l	$00000000	; 3F 

;*
;* OFFSET INTO PACK DATA FOR DE-SCRAMBLING
;*
		.phrase

offstabl:	.dc.b	SYMB04		; symbol #4
		.dc.b	SYMB05		; symbol #5
		.dc.b	SYMB06		; symbol #6
		.dc.b	SYMB07		; symbol #7
		.dc.b	SYMB08		; symbol #8
		.dc.b	SYMB09		; symbol #9
		.dc.b	SYMB10		; symbol #10
		.dc.b	SYMB11		; symbol #11
		.dc.b	SYMB12		; symbol #12
		.dc.b	SYMB13		; symbol #13
		.dc.b	SYMB14		; symbol #14
		.dc.b	SYMB15		; symbol #15
		.dc.b	SYMB16		; symbol #16
		.dc.b	SYMB17		; symbol #17
		.dc.b	SYMB18		; symbol #18
		.dc.b	SYMB19		; symbol #19

		.phrase

;************************************************************************
;* VARIABLE DATA AREA							*
;************************************************************************

		.bss

;*======================================================================*
;*======================================================================*

		.phrase

TvChnlAv:	.ds.w	1		; the channel numbers available
TvChnlNo:	.ds.b	1		; the channel number the user wants to view 
cdgflags:	.ds.b	1		; if CD+G being displayed

tvgrfmem:	.ds.l	1		; pointer to Primary TV graphics memory
tvotherm:	.ds.l	1		; pointer to alternate TV graphics memory

subcount:	.ds.l	1		; number of subcodes in data buffer 

TvColRed:	.ds.b	16		; red component
TvColGrn:	.ds.b	16		; green component
TvColBlu:	.ds.b	16		; blue component
TransFac:	.ds.b	16		; transparency factor

BrdColNo:	.ds.l	1		; border color number

tvPH:		.ds.w	1		; horizontal screen pointer for TV-Graphics
tvPV:		.ds.w	1		; vertical screen pointer for TV-Graphics
tvCOPH:		.ds.w	1		; horizontal shift (left or right) in TV-Graphics
tvCOPV:		.ds.w	1		; vertical shift (up or down) in TV-Graphics

ScrlType:	.ds.w	1		; scroll with copy (TRUE) or preset (FALSE)
ScrlColr:	.ds.w	1		; background colour while scrolling with preset

		.phrase

;************************************************************************
;* END OF FILE								*
;************************************************************************

	.end
