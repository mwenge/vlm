;
D_END	equ	$f1a10c
D_CTRL	equ	$f1a114
D_PC	equ	$f1a110
SMODE	equ	$f1a154
JOY1	equ	$f14000
;
;   org at $1Ab328
;
doit:
	move.l	#$70007,D_END
	move.l	#DSP_S,a0
	move.l	#DSP_E,d0
	sub.l	a0,d0
	asr.l	#2,d0
	lea	$f1b000,a1
copy:
	move.l	(a0)+,(a1)+
	dbra	d0,copy
;
	move.l	#$f1b000,D_PC
;	move.l	#testp,D_PC
;
tpt:
	move.l	#1,D_CTRL
	move.l	#$14,SMODE
	move.w	#$100,JOY1
	rts
