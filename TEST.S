;
;
;
;
;
	moveq	#5,d6
	lea	result,a1
loop:
	bsr	randf
	move.l	d0,(a1)+
	dbra	d6,loop
;
;
;
	moveq	#5,d6
	lea	result1,a1
loop1:
	movem.l	a1/d6,-(sp)
	move.w	#17,-(sp)
	trap	#14
	addq.l	#2,sp
	movem.l	(sp)+,a1/d6
	move.l	d0,(a1)+
	dbra	d6,loop1	
	
	illegal
;
;
randf:
	moveq	#0,d2
;
	movem.l	seed(pc),d0-d1		;get seed, constant
	tst.l	d0
	bge.s	ov
	neg.l	d0
	addq	#1,d2
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
	move.l	d0,seed
	lsr.l	#8,d0
	rts
;
;
seed:
	dc.l	$33ba0359		;seed
constant:
	dc.l	$44bf19d3		;constant
;
	.bss
;
result:
	ds.l	6
result1:
	ds.l	6
