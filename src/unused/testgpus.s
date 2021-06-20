;
;
;
;
	.include	"JAGUAR.INC"
;
doit:
	move.l	#GPU_S,a0
	move.l	#GPU_E,d0
	sub.l	a0,d0
	asr.l	#2,d0
	lea	$f03000,a1
copy:
	move.l	(a0)+,(a1)+
	dbra	d0,copy
;
	move.l	#gpustart,G_PC
;
tpt:
	move.l	#1,G_CTRL

	illegal

