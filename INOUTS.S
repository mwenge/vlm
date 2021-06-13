;
D_END	equ	$f1a10c
D_CTRL	equ	$f1a114
D_PC	equ	$f1a110
SMODE	equ	$f1a154
JOY1	equ	$f14000
;
;   org at iansdoit
;
doit:
	move.w	sinshot(pc),d0		;have we run before?
	beq.s	realdot			;br if not
	rts
realdot:
	move.w	#-1,sinshot		;the is just a single shot
;
	move.l	#$70007,D_END
	move.l	#DSP_S,a0
	move.l	#DSP_E,d0
	sub.l	a0,d0
	asr.l	#2,d0
	lea	realorg,a1
copy:
	move.l	(a0)+,(a1)+
	dbra	d0,copy
;
	move.l	#startup,D_PC
;	move.l	#testp,D_PC
;
tpt:
	move.l	#1,D_CTRL
	move.l	#$14,SMODE
	move.w	#$100,JOY1
	rts
sinshot:
	dc.w	0		;0 means we can run, -1 means not
