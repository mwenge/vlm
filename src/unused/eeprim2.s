;************************************************
;						*
;	EEPROM Write & Read Primitives		*
;						*
;************************************************
;
;	Hi-Score on-board cartridge EEPROM primitives 
;	for use by Jaguar game cartridge programmers
;
;
;	Last Update: 9-Nov-93
;	 Programmer: Dave Staugas
;		     Atari, Sunnyvale
;		     [408] 745-8802
;
;
;   Equates needed that may already be defined in the JAGUAR.INC file..
;
JOY1		equ	$f14000		;this we'll use as our I/O base address
GPIO_0		equ	$f14800		;General purpose I/O #0
GPIO_1		equ	$f15000		;General purpose I/O #1
;
;   Equates derived from the above
;    to allow indirect with 16-bit displacement addressing
;
GPIO_0of	equ	GPIO_0-JOY1	;offset to GPIO_0 (when addr reg Ax -> JOY1) 
GPIO_1of	equ	GPIO_1-JOY1	;offset to GPIO_1 (when addr reg Ax -> JOY1) 
;
;   Commands specific to the National Semiconductor NM93C14
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
;  DO (data out)	- is read on bit0 of JOY1
;  DI (data in) 	- is written on bit0 of GPIO_0
;  CS (chip select)	- is pulsed low by any access to GPIO_1
;
;
;   Test the EEPROM eREAD and eWRITE primitives
;
;
start:
	lea	chkdata,a1
	moveq	#0,d1
wrloop:
	move.w	(a1)+,d0
	bsr	eewrite
	addq	#1,d1
	cmpi.w	#64,d1
;test	bcs	wrloop
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
pass:
	lea	result,a0
	lea	chkdata,a1
	move.w	(a0)+,126(a1)
;
	moveq	#62,d1
shifloop:	
	move.w	(a0)+,(a1)+
	dbra	d1,shifloop
	bra	start
;
;
;
fail:
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
;*****************************************************************
;
;  Write all 128 words to EEPROM
;
;  entry: a1 -> 128-word buffer of data to write
;
;   exit: d0 = 0 for successful write, -1 for time-out error
;              all other registers preserved
;	
;
eewral:
	movem.l	a0/d1-d4,-(sp)
	lea	JOY1,a0		;set ptr to EEPROM i/o addresses
;
	tst.w	GPIO_1of(a0)	;strobe ChipSelect
;
	move.w	#eEWEN,d2	;erase/write enable command
	bsr	out9bits	;send it to EEPROM
;	
	tst.w	GPIO_1of(a0)	;strobe ChipSelect
;
	move.w	#eWRAL,d2	;get WRITE ALL command
	bsr	out9bits	;send it to EEPROM
;
	moveq	#127,d1		;we do 128 words here
wrAlloop:
	move.w	(a1)+,d2	;get next 16-bit data word to send
	bsr	out16bit	;  & send it
	dbra	d1,wrAlloop
;
	tst.w	GPIO_1of(a0)	;strobe ChipSelect
;
	nop			;1 us required after CS for status valid
	nop
	moveq	#0,d4		;assume no time-out error
	moveq	#-1,d2		;load time-out counter
wrAwait:
	move.w	(a0),d3		;wait until write is complete
	lsr.w	#1,d3
	bcs.s	wrAdone		;exit if write complete is indicated
	dbra	d2,wrAwait
;
;  time-out error, set indication...
;
	moveq	#-1,d4		;set time-out error
wrAdone:	
;	
	move.w	#eEWDS,d2	;get erase/write disable command
	bsr	out9bits	;send it
;
	tst.w	GPIO_1of(a0)	;strobe ChipSelect
;
	move.l	d4,d0		;indicate success or time-out on write

	movem.l	(sp)+,a0/d1-d4
	rts			;we're done
;
;
;******************************************************
;
;
;  Read all 128-words to buffer from EEPROM
;
;  entry:  a1 -> buffer where 128-words from EEPROM are to be deposited
;
;
eereall:
	movem.l	a0/d1-d4,-(sp)
	lea	JOY1,a0		;set ptr to EEPROM i/o address
;
	tst.w	GPIO_1of(a0)	;strobe ChipSelect
;
	move.w	#eREAD,d2
	bsr	out9bits
;
	moveq	#127,d2
inword:
	moveq	#0,d0
	moveq	#15,d3		;pick up 17 bits (1st is dummy)
inAlp:	
	tst.w	GPIO_0of(a0)
	nop
	move.w	(a0),d1
	lsr.w	#1,d1
	addx.w	d0,d0
	nop
	nop
	nop
	nop
	nop
	nop
	dbra	d3,inAlp
;
	move.w	d0,(a1)+
	dbra	d2,inword
;
	movem.l	(sp)+,a0/d1-d4
	rts
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
	movem.l	a0/d1-d4,-(sp)
	lea	JOY1,a0		;set ptr to EEPROM i/o addresses
;
	tst.w	GPIO_1of(a0)	;strobe ChipSelect
;
	move.w	#eEWEN,d2	;erase/write enable command
	bsr	out9bits	;send it to EEPROM
;	
	tst.w	GPIO_1of(a0)	;strobe ChipSelect
;
	andi.w	#$3f,d1		;force write addr to be legit (0-63)
	ori.w	#eWRITE,d1	;form WRITE command
	move.w	d1,d2
;
;
	move.w	#eWRAL,d2	;*****  test
;
;
	bsr	out9bits	;send it to EEPROM
;
	moveq	#127,d4
wrallp:
	move.w	(a1)+,d2
;	move.w	d0,d2		;get 16-bit data word to send
	bsr	out16bit	;  & send it
;
	dbra	d4,wrallp
;
;
	tst.w	GPIO_1of(a0)	;strobe ChipSelect
;
	nop			;1 us required after CS for status valid
	nop
	moveq	#0,d4		;assume no time-out error
	moveq	#-1,d2		;load time-out counter
wrwait:
	move.w	(a0),d3		;wait until write is complete
	lsr.w	#1,d3
	bcs.s	wrdone		;exit if write complete is indicated
	dbra	d2,wrwait
;
;  time-out error, set indication...
;
	moveq	#-1,d4		;set time-out error
wrdone:	
;	
	move.w	#eEWDS,d2	;get erase/write disable command
	bsr	out9bits	;send it
;
	tst.w	GPIO_1of(a0)	;strobe ChipSelect
;
	move.l	d4,d0		;indicate success or time-out on write

	movem.l	(sp)+,a0/d1-d4
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
	lea	JOY1,a0		;set ptr to EEPROM i/o address
;
	tst.w	GPIO_1of(a0)	;strobe ChipSelect
;
	andi.w	#$3f,d1		;force legit read addr
	ori.w	#eREAD,d1
	move.w	d1,d2
	bsr	out9bits
;
	moveq	#0,d0
	moveq	#15,d3		;pick up 17 bits (1st is dummy)
inlp:	
	tst.w	GPIO_0of(a0)
	nop
	move.w	(a0),d1
	lsr.w	#1,d1
	addx.w	d0,d0
	nop
	nop
	nop
	nop
	nop
	nop
	dbra	d3,inlp
;
	movem.l	(sp)+,a0/d1-d4
	rts
;
;**************************************************************
;
;  Serial data sent to device is written to DI, bit0 of GPIO_0
;
; entry:
;  a0 -> JOY1
;  d2.w = 16-bit data word to write
;
; exit:
;  d2.w, d3.l destroyed
;
out16bit:
	rol.w	#1,d2		;align 1st serial data bit (bit15) to bit0
	moveq	#15,d3		;send 16 bits
	bra.s	out9lp
;
; entry:
;  a0 -> JOY1
;  d2.w = 9-bit command to write
;
out9bits:
	rol.w	#8,d2		;align 1st serial data bit (bit8) to bit0
	moveq	#8,d3		;send 9
out9lp:
	move.w	d2,GPIO_0of(a0)		;write next bit
	nop
	nop
	nop			;delay next write
	nop
	nop
	nop
	rol.w	#1,d2		;adjust bit0 for next datum
	dbra	d3,out9lp	;go for all 9 or all 16
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
