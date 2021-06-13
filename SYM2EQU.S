*
*  Build a file of EQUATES for all symbols given in "symlist"
*  to be extracted from a .SYM file
*  include-able for .S programs
*
*
PRN	equ	0
AUX	equ	1
CON	equ	2
*
INSTAT	equ	1
CHARIN	equ	2
CHAROUT	equ	3
*
XON	equ	$13
XOFF	equ	$11
*
bufsize	equ	4096
*
*
*
*
start:
	move.w	#0,-(sp)
	move.l	#inpath,-(sp)		;open VLM.SYM file name
	move.w	#$3d,-(sp)
	trap	#1
	addq.l	#8,sp
*
	move.w	d0,ihandle
	bpl	goodin			;br if this one was found
*
*  File name missing or read error on: 
*
baddload:
	pea	filerr(pc)
	move.w	#9,-(sp)
	trap	#1
	addq.l	#6,sp
*
quit:
	move.w	#-1,-(sp)
	move.w	#$4c,-(sp)
	trap	#1
*
goodin:
	move.l	#inbuf,-(sp)		;get pointer to currently avail memory
	move.l	#$20000,-(sp)		;read in a buffer's full
	move.w	ihandle,-(sp)
	move.w	#$3f,-(sp)
	trap	#1
	adda.w	#12,sp
;
	tst.l	d0
	bmi	baddload
;
	addi.l	#inbuf,d0
	move.l	d0,inend
;
	move.w	ihandle,-(sp)
	move.w	#$3e,-(sp)
	trap	#1			;close input file
	addq.l	#4,sp
;
	move.w	#0,-(sp)
	move.l	#outpath,-(sp)
	move.w	#$3c,-(sp)	;create .EQU file
	trap	#1
	addq.l	#8,sp

	move.w	d0,ohandle
	bmi	baddload
;
;
;
goodwr:
	lea	symlist,a0
	move.l	inend,a2
	lea	outbuf,a3
nxtshort:
	lea	inbuf+$24,a1
symloop:
	move.l	a0,a4		;target symbol name
	move.l	a1,a5		;entry in symbol table
	moveq	#7,d0		;max symbol length
symchk:
	move.b	(a4)+,d1	;get desired symbol char
	cmp.b	(a5)+,d1	;check against symbol table entry
	bne.s	nextsym
	tst.b	d1
	beq.s	gotmatch
	dbra	d0,symchk
	bra	gotmatch
nextsym:
	adda.w	#14,a1		;advance to next symbol table entry
	cmp.l	a2,a1		;if not reached end, continue
	bcs	symloop
;
;  couldn't find this one in the table...
;
	move.l	a0,-(sp)
	pea	symerr(pc)
	move.w	#9,-(sp)
	trap	#1
	addq.l	#6,sp
	move.l	(sp)+,a0
;
	clr.b	8(a0)
	move.l	a0,-(sp)
	move.w	#9,-(sp)
	trap	#1
	addq.l	#6,sp
	bra	quit
;
;
gotmatch:
	moveq	#0,d0
copname:
	addq.w	#1,d0
	move.b	(a0)+,(a3)+	;copy symbol name to output buffer
	bne	copname
	subq.l	#1,a3		;we don't want null in text file
	move.b	#9,(a3)+
	cmpi.b	#9,d0
	bcc.s	skiptab
	move.b	#9,(a3)+
skiptab:
	move.b	#"E",(a3)+
	move.b	#"Q",(a3)+
	move.b	#"U",(a3)+
	move.b	#9,(a3)+
	move.b	#"$",(a3)+
;
	adda.w	#10,a1
	moveq	#3,d1
symer:
	moveq	#0,d0
	move.b	(a1)+,d0
	move.w	d0,d2
	lsr.w	#4,d2
	move.b	hextab(pc,d2.w),(a3)+
	andi.w	#$F,d0
	move.b	hextab(pc,d0.w),(a3)+
	dbra	d1,symer
	bra.s	ovhex
hextab:
	dc.b	"0123456789ABCDEF"
ovhex:
	move.b	(a0)+,(a3)+
	bne	ovhex
;
	subq.l	#1,a3		;we don't want null in text file
;
	move.b	#13,(a3)+
	move.b	#10,(a3)+
;
	tst.b	(a0)		;reached end of our short list?
	bne	nxtshort
;
;  Write & close output file
;
	move.l	#outbuf,d0
	sub.l	d0,a3

	move.l	d0,-(sp)		;get pointer to currently avail memory
	move.l	a3,-(sp)		;write next bufer's full
	move.w	ohandle,-(sp)
	move.w	#$40,-(sp)
	trap	#1
	adda.w	#12,sp
;
	cmp.l	-8(sp),d0
	bne	baddload

	move.w	ohandle,-(sp)
	move.w	#$3e,-(sp)
	trap	#1			;close file
	addq.l	#4,sp
;
	move.w	#0,-(sp)
	move.w	#$4c,-(sp)
	trap	#1
*
;
;
esc	equ	$1b
*
*
;
inpath:
	dc.b	"VLM.SYM",0
outpath:
	dc.b	"VLM.EQU",0
;
symlist:
	dc.b	"free",0," ; start of Free run code",0
	dc.b	"audio",0," ; start of audio reactive",0
	dc.b	"goag",0," ; real start of audio reactive",0
	dc.b	"board",0," ; base of 1-bit/pixel 320x240 display",0 
	dc.b	"frames",0," ;.w Jeff's frame count",0
	dc.b	"pad_now",0," ;.w Jeff's joystick word",0
	dc.b	"freerun",0," ;.w put a 2 in here to stop GPU in free run",0
	dc.b	"davesvec",0," ;.l vector for dave's mainloop",0
	dc.b	"cursimg",0," ; cursor image (to be cleared)",0
	dc.b	"davesobj",0," ; base addr of dave's overlay obj",0
	dc.b	"vlm_mode",0," ;.w 0=no VLM controls, 1=VLM controls active",0
	dc.b	"skid",0," ;.w 0..9 setting for free running screen",0
	dc.b	"ObTypes",0," ; ptr to object type definitions",0
	dc.b	"skidoo",0," ; value to stuff in 'action'",0
	dc.b	"imatrix",0," ;.w 0..9 bank number",0
	dc.b	"action",0," ; vector to initiate bank/effect switch",0
	dc.b	"gm",0," ; value to stuff in 'action'",0
	dc.b	"beasties",0," ; start of object list (davesobj is subset)",0
	dc.b	"print",0," ; routine to print errors",0
	dc.b	"cursx",0," ;.w horz position associated with 'print'",0
	dc.b	"cursy",0," ;.w vert position associated with 'print'",0
	dc.b	"iansdoit",0," ; entry point for DSP init routine",0
	dc.b	"vlmtim",0," ;.w turn-off timer for VLM logo",0
	dc.b	"blist",0," ;.l ptr to build (shadow) display list",0
	dc.b	"dlist",0," ;.l ptr to hardware display list",0
	dc.b	"RunBeast",0," ; Jeff's object build routine",0
	dc.b	"Frame",0," ; Jeff's v_blank routine",0
	dc.b	"readpad",0," ; Jeff's joystick routine",0
	dc.b	"vlmlogo",0," ; Base addr of VLM logo grafix",0
	dc.b	"no_ksel",0," ;vlm label,@ +8.w bit15 set disables vlmedit",0
	dc.b	"pal",0," ;.w =1 if on PAL system, =0 for NTSC",0
	dc.b	"myFont",0," ;base address of internal font",0
	dc.b	0
;
filerr:
	dc.b	13,10,"A file error has occurred...",0
symerr:
	dc.b	13,10,"Symbol not found: ",0
*
;
;
	.bss
;
inend:
	ds.l	1
;
;
ohandle:
	ds.w	1
ihandle:
	ds.w	1
;
;
outbuf:
	.ds.b	$1000
inbuf:
	.ds.b	$20000
