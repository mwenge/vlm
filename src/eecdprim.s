;************************************************
;						*
;	CD EEPROM Write & Read Primitives	*
;						*
;************************************************
;
;	CD-ROM module primitives for non-volatile memory (eeprom)
;	for use by Jaguar CD-ROM game programmers
;
;
;	Last Update: 7-Sep-94
;	 Programmer: Dave Staugas
;		     Atari, Sunnyvale
;		     [408] 745-8802
;
;
;
;  The BUTCH interface for the CD-ROM module is a long-word register,
;   where only the least signifigant 4 bits are used
;
eeprom	equ	$DFFF2c			;interface to CD-eeprom
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
;		 876543210
eREAD	equ	%110000000		;read from EEPROM
eEWEN	equ	%100110000		;Erase/write Enable
eERASE	equ	%111000000		;Erase selected register
eWRITE	equ	%101000000		;Write selected register
eERAL	equ	%100100000		;Erase all registers
eWRAL	equ	%100010000		;Writes all registers
eEWDS	equ	%100000000		;Erase/Write disable (default)
;
;
;
;
;
;   Test the EEPROM eREAD and eWRITE primitives
;
;
start:
	moveq	#127,d7
restart:
	lea	chkdata,a1
	moveq	#0,d1
wrloop:
	move.w	(a1)+,d0
	bsr	eewrite
	beq.s	goowr
wrtimeo:
	illegal
goowr:
	addq	#1,d1
	cmpi.w	#64,d1
	bcs	wrloop
;
	lea	result,a0
	moveq	#0,d1
rdloop:
	bsr	eeread
	move.w	d0,(a0)+
	addq	#1,d1
	cmpi.w	#64,d1
	bcs	rdloop
;
	lea	chkdata,a0
	lea	result,a1
	moveq	#63,d1
chkloop:
	move.w	(a0)+,d0
	cmp.w	(a1)+,d0
	bne	fail
	dbra	d1,chkloop
;
;
;
	lea	result,a0
	lea	chkdata,a1
	move.w	(a0)+,126(a1)
;
	moveq	#62,d1
shifloop:	
	move.w	(a0)+,(a1)+
	dbra	d1,shifloop
	dbra	d7,restart
pass:
	illegal
;	bra	start
;
;
;
fail:
	illegal
;
	lea	result,a0
	lea	chkdata,a1
	moveq	#63,d0
forclp:	
	move.w	(a1)+,(a0)+
	dbra	d0,forclp
		
	move.w	(a0)+,126(a1)
;
	lea	result,a0
	lea	chkdata,a1
	moveq	#62,d1
	bra	shifloop
;
;
;
;*****************************************************************
;
;  Write a word to EEPROM
;
;  entry: d0.w = data to be written
;	  d1.w = least signifigant 6 bits specify write address (0-63)  
;
;   exit: d0 = 0 for successful write, -1 for time-out error
;              all other registers preserved
;	
;
;
eewrite:
	movem.l	a0/d1-d6,-(sp)
	lea	eeprom,a0	;set ptr to EEPROM i/o address
;
	move.w	#eEWEN,d2
	bsr	out9bits	;enable write
;
;	moveq	#0,d2
;	bsr	out16bit	;flush it out
;
	andi.w	#$3f,d1		;force write addr to be legit (0-63)
	move.w	#eWRITE,d2
	or.w	d1,d2
	bsr	out9bits	;issue WRITE command with write address
;
	move.w	d0,d2
	bsr	out16bit
;
;  strobe Chip Select (before check of busy)
;
	moveq	#1,d3
	moveq	#0,d4
	move.l	d4,(a0)		;CS=0
	move.l	d3,(a0)		;CS=1
	move.l	d4,(a0)		;CS=0
;
	moveq	#8,d4
;
;	move.w	#1000,d5	;this time-out is hairy edge on lite system
	move.w	#$8000,d5
busywait:
	move.l	(a0),d2		;check busy
	and.w	d4,d2
	bne.s	eewrfin
	dbra	d5,busywait
;
;  time-out on write, set indicator & exit
;
	moveq	#-1,d0		;indicate time out
	bra.s	eewrx	
eewrfin:
	move.w	#eEWDS,d2	;get erase/write disable command
	bsr	out9bits	;send it
;
	moveq	#0,d0		;indicate no timeout
eewrx:
	movem.l	(sp)+,a0/d1-d6
	tst.l	d0
	rts			;we're done
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
;		  all other registers preserved
;
eeread:
	movem.l	a0/d1-d4,-(sp)
	lea	eeprom,a0	;set ptr to EEPROM i/o address
;
	andi.w	#$3f,d1		;force legit read addr
	move.w	#eREAD,d2
	or.w	d1,d2
	bsr	out9bits
;
	moveq	#0,d0
	moveq	#15,d3		;pick up 16 bits
	moveq	#0,d4
	moveq	#2,d5
inlp:
	move.l	d4,(a0)		;Clk=0
	move.l	d5,(a0)		;Clk=1
	move.l	d4,(a0)		;Clk=0
;
	move.l	(a0),d1
	lsr.w	#4,d1
	addx.w	d0,d0
	dbra	d3,inlp
;
	movem.l	(sp)+,a0/d1-d4
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
	rol.w	#3,d2		;align MSbit to data bit position (bit2)
	moveq	#15,d6		;send 15
	bra.s	outxb
;
; entry:
;  a0 -> eeprom ($DFFF2C)
;  d2.w = 9-bit command to write
;
out9bits:
;
;  strobe Chip Select (always needed for command)
;
	moveq	#1,d3
	moveq	#0,d4
;
	move.l	d4,(a0)		;CS=0
	move.l	d3,(a0)		;CS=1
	move.l	d4,(a0)		;CS=0
;
	ror.w	#6,d2		;align MSbit to data bit position (bit2)
	moveq	#8,d6		;send 9
;
;
;
outxb:
	moveq	#4,d3		;mask for data bit
	moveq	#2,d4		;clock bit
;
bitloop:
	move.w	d2,d5
	and.l	d3,d5
;
	move.l	d5,(a0)		;data is ready, CLK=0
	eor.w	d4,d5
	move.l	d5,(a0)		;same data, CLK=1
	eor.w	d4,d5
	move.l	d5,(a0)		;CLK=0
	rol.w	#1,d2
	dbra	d6,bitloop
;
;
	rts	
;	
;
;	
chkdata:
;ickify
          dc.l      $F4292244
          dc.l      $432AFF97
          dc.l      $AB9423A7
          dc.l      $FC93A039
          dc.l      $655B59C3
          dc.l      $8F0CCC92
          dc.l      $FFEFF47D
          dc.l      $85845DD1
          dc.l      $6FA87E4F
          dc.l      $FE2CE6E0
          dc.l      $A3014314
          dc.l      $4E0811A1
          dc.l      $F7537E82
          dc.l      $BD3AF235
          dc.l      $2AD7D2BB
          dc.l      $EB86D391
;
;hack
          dc.l      $FFFA3942
          dc.l      $8771F681
          dc.l      $6D9D6122
          dc.l      $FDE5380C
          dc.l      $A4BEEA44
          dc.l      $4BDECFA9
          dc.l      $F6BB4B60
          dc.l      $BEBFBC70
          dc.l      $289B7EC6
          dc.l      $EAA127FA
          dc.l      $D4EF3085
          dc.l      $04881D05
          dc.l      $D9D4D039
          dc.l      $E6DB99E5
          dc.l      $1FA27CF8
          dc.l      $C4AC5665
	.bss
result:
	.ds.w	64
