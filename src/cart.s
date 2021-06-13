;
	.include	'jaguar.inc'
;
start:
	lea	NFrm(pc),a0
	move.l	a0,$100
;
	lea	stopo(pc),a0
	move.l	a0,d0
	addi.l	#15,d0
	andi.w	#~15,d0
	swap	d0
	move.l	d0,OLP
;
;
;	move.w	#-1,VI	;***21-Feb-95  Raiden (NTSC only) needs running
;
	move.w	#$100,INT1
;	move.w	#$101,INT1		;*** Flashback (PAL only) likes
;
	moveq	#0,d0
	move.l	d0,BORD1
	move.w	d0,BG			;black screen
;
	moveq	#-1,d0
delay:
	dbra	d0,delay
;
	move.w	#$100,INT1
;
	jmp	$802000
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
