;**************************************************************************
; (C)1994 ATARI CORP.       SECRET & CONFIDENTIAL       ALL RIGHTS RESERVED
;
;				ee.s
;
;       This program sets up, writes then reads the serial eeprom on the 
; 	Jaguar CD Module.
;
;	The makefile is named 'ee.mak'.
;
;                                REVISION HISTORY
;
; REV.  DATE       BY            DESCRIPTION OF EDIT
; """"  """"       ""            """""""""""""""""""
; 100  27 Jun 94  DMS	Combine eewr and eerd.s
; 103  28 Jun 94  DMS	Does "Ok" write-all fill and read-back
; 304  23 Aug 94  DMS	Almost clean.  Need to readback twice to get 1st 8
; 305  23 Aug 94  DMS	Mod for use in Test1.s
; xxx  07 Sep 94  DJS
;***************************************************************************
VDE	equ	$F00048
BUTCH	equ	$DFFF00
eeprom	 equ	$DFFF2C		; eeprom bits are the low 4 here
;
;
;
;
;
Start_it:			; start of code
	move.w	#$FFFF,VDE	; video hack
	move.l	#$1ff000,A7	; set stack pointer up high

;				*****************************************
;				*    Clear memory range & init Butch  	*
;				*****************************************
Clearmem:
	move.l	#Eblokbeg,a0	; our lowest DRAM address
	move.l	#Eblokend,d1	; our highest DRAM address

Clermem1:			; clear data buffer area
	move.l	#0,(a0)+
	move.l	a0,d0	
	cmp.l	d0,d1
	bne	Clermem1

Do_butch:			; enable Butch
	move.l	#0,BUTCH

;				*****************************************
;				*    Fill eeprom word by word		*
;				*****************************************
SetupWR:			; setup for write
	move.l	#$1234,d4	; first word to write
	move.l	#0,a4		; base address of EEPROM
	bsr	ENABcmnd	; enable writing

RITEmore:
	move.l	a4,d1		; get address
	bsr	RITEcmnd	; send command with address
	move.l	d4,d1		; get data
	bsr	TX_word		; send data

RITEdone:
	cmp.l	#63,a4
	beq	Read_EE		; jump to readback if done, else...
	bsr	Chekbusy	; see if ready for next command
	add.l	#1,d4		; increment data
	add.l	#1,a4		; inc address
	bra	RITEmore	; loop

Stoprite:
	bsr	EWDScmnd	; send disable-programming command

;				*****************************************
;				*    read the whole EEPROM back		*
;				*****************************************
Read_EE:
	clr.l	d5		; need to read twice to get 1st 8 right
	move.w	#2,d5		; count of complete read-backs

Setup_RX:			; setup to receive array of words
	move.l	#Eblokbeg,a3	; base of storage array in DRAM
	move.l	#0,a4		; base of EEPROM addresses

Get_more:			; word-reading loop
	move.l	a4,d1		; address to read into d1
	bsr	READcmnd	; do command with address
	bsr	RX_word		; read 16 bits back

saveword:			; word done, so store it to DRAM array
	move.l	d1,(a3)+
	clr.l	d1		; ready for next word
	clr.l	d0

Readdone:			; see if all the words are done
	add.l	#1,a4
	cmp.l	#64,a4
	bne	Get_more	; not done yet, loop
				; done with 1 read of whole EEPROM, so...
Readagin:
	sub.w	#1,d5		; get repeat counter and decrement it
	cmp.w	#0,d5		; done yet?
	bne	Setup_RX	; no, loop
				; yes, repeats finished, so...

Fini:				; exit to db
	illegal


;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;			Subroutines
;""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
;				*****************************************
;				*    Write one 16-bit word to eeprom	*
;				*****************************************
Rite_one:			; for example...
	bsr	ENABcmnd	; enable writing to eeprom
	move.l	#0,d1		; address in EEPROM
	bsr	RITEcmnd	; send write command with address
	move.l	#$4F6B,d1	; load d1 with "Ok" (the data to send)
	bsr	TX_word		; send it
	bsr	Chekbusy
	rts
;				*****************************************
;				*    Command to write to address in d1	*
;				*****************************************
RITEcmnd:			
				; init state
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#1,eeprom	; clock low, CS*, data 0
	move.l	#0,eeprom	; clock low, CS, data 0

;				; leading zero
;	move.l	#2,eeprom	; clock hi, CS, data 0
;	move.l	#0,eeprom	; clock low, CS, data 0

				; start bit
	move.l	#4,eeprom	; clock low, CS, data 1
	move.l	#6,eeprom	; clock hi, CS, data 1
	move.l	#4,eeprom	; clock low, CS, data 1 

				; make OP code of 01
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#4,eeprom	; clock low, CS, data 1
	move.l	#6,eeprom	; clock hi, CS, data 1
	move.l	#4,eeprom	; clock low, CS, data 1

.ADRS_TX:			; Transmit a 6-bit address from d1 MSB first
	move.w	#6,d2		; number of bits to TX
	lsl.w	#8,d1		; shift 6-bit address to top of register
	lsl.w	#2,d1		; 10 bit shift, in all

.TX_bit:			; send a 0 or 1
	btst	#15,d1		; look at bit 15
	beq	Send_0		; yes, it was zero, so...
	bra	Send_1		; no, it was 1

.Nextbit:
	lsl.w	#1,d1		; shift d1 to get next bit
	subq	#1,d2		; decrement bit counter
	cmp.w	#0,d2
	bne	.TX_bit		; not done with word, loop
	rts			; done, return to main, with clock low

.Send_1:			; send a 1
	move.l	#4,eeprom	; clock low, CS, data 1 
	move.l	#6,eeprom	; clock hi, CS, data 1
	move.l	#4,eeprom	; clock low, CS, data 1 
	bra	.Nextbit	; done sending, return via Nextbit

.Send_0:			; send a 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	bra	.Nextbit	; done sending, return via Nextbit

;				*****************************************
;				* Subroutine to read 1 word into d1	*
;				*****************************************
RX_word:
	move.l	#16,d2		; number of bits in word

RX_a_bit:			; receive one bit
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	eeprom,d0	; get data into d0
	and.l	#$0000000F,d0	; mask for low nibble
	lsr.l	#3,d0		; move data bit to LSB position
	or.l	d0,d1		; or in the data bit

worddone:			; see if word is done
	sub.l	#1,d2
	cmp.l	#0,d2
	beq	wordfini	; done jump to rts
	lsl.l	#1,d1		; not done, shift left. ready for next bit
	bra	RX_a_bit	; loop

wordfini:
	rts					


;				*****************************************
;				*	Send write-enable command	*
;				*****************************************
ENABcmnd:			; write-enable sequence of 0, 1, 0, 0, 1, 1
				; init state
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#1,eeprom	; clock low, CS*, data 0
	move.l	#0,eeprom	; clock low, CS, data 0

				; leading zero
;	move.l	#2,eeprom	; clock hi, CS, data 0
;	move.l	#0,eeprom	; clock low, CS, data 0
				; start bit
	move.l	#4,eeprom	; clock low, CS, data 1
	move.l	#6,eeprom	; clock hi, CS, data 1
	move.l	#4,eeprom	; clock low, CS, data 1 
				; make OP code of 00
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
				; Make address of 11
	move.l	#4,eeprom	; clock low, CS, data 1 
	move.l	#6,eeprom	; clock hi, CS, data 1
	move.l	#4,eeprom	; clock low, CS, data 1 
	move.l	#6,eeprom	; clock hi, CS, data 1
	move.l	#4,eeprom	; clock low, CS, data 1 
				; padding clocks
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0

	rts			; return to caller

;				*****************************************
;				*	Send write-all command		*
;				*****************************************
WALLcmnd:			; write-all enable sequence of 0, 1, 0, 0, 0, 1
				; init state
	move.l	#1,eeprom	; clock low, CS*, data 0
	move.l	#0,eeprom	; clock low, CS, data 0

				; leading zero
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
				; start bit
	move.l	#4,eeprom	; clock low, CS, data 1
	move.l	#6,eeprom	; clock hi, CS, data 1
	move.l	#4,eeprom	; clock low, CS, data 1 

				; make OP code of 00
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0

				; Make address of 01
	move.l	#0,eeprom	; clock low, CS, data 0 
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0 
	move.l	#4,eeprom	; clock low, CS, data 1 
	move.l	#6,eeprom	; clock hi, CS, data 1
	move.l	#4,eeprom	; clock low, CS, data 1 

				; padding clocks
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	rts			; return to caller

;				*****************************************
;				*  Send disable-programming command	*
;				*****************************************
EWDScmnd:			; disable sequence = 1, 0, 0, 0, 0
				; init state
	move.l	#1,eeprom	; clock low, CS*, data 0
	move.l	#0,eeprom	; clock low, CS, data 0

				; start bit
	move.l	#4,eeprom	; clock low, CS, data 1
	move.l	#6,eeprom	; clock hi, CS, data 1
	move.l	#4,eeprom	; clock low, CS, data 1 

				; make OP code of 00
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0

				; Make address of 00
	move.l	#0,eeprom	; clock low, CS, data 0 
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0 
	move.l	#0,eeprom	; clock low, CS, data 0 
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0 

				; padding clocks
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	rts			; return to caller

;				*****************************************
;				*   See if eeprom is ready to receive   *
;				*****************************************
Chekbusy:			; see if chip is busy
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#1,eeprom	; clock low, CS*, data 0
;	move.l	#3,eeprom	; clock hi, CS*, data 0	
;	move.l	#1,eeprom	; clock low, CS*, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	nop
	nop
	nop
	nop

Busywait:
;	move.l	#0,eeprom	; clock low, CS, data 0
;	move.l	#2,eeprom	; clock hi, CS, data 0 
	move.l	eeprom,d0	; get Butch's reg
	and.l	#$00000008,d0	; mask for low nibble
	cmp.l	#$8,d0		; should be $A when EEP is ready for next word
	bne	Busywait	; make clocks until not busy
	rts			; OK, not busy, return or hang forever

;				*****************************************
;				*	Transmit 1 data word from d1	*
;				*****************************************
TX_word:			; send MSB first
	move.w	#16,d2		; number of bits to TX
				
TX_a_bit:			; send a 0 or 1
	btst	#15,d1		; look at bit 15
	beq	Sendzero	; yes, it was zero, so...
	bra	Sendaone	; no, it was 1

Next_bit:
	lsl.w	#1,d1		; shift d1 to get next bit
	subq	#1,d2		; decrement bit counter
	cmp.w	#0,d2
	bne	TX_a_bit	; not done with word, loop
	rts			; done, return to main, with clock low

Sendaone:			; send a 1
	move.l	#4,eeprom	; clock low, CS, data 1 
	move.l	#6,eeprom	; clock hi, CS, data 1
	move.l	#4,eeprom	; clock low, CS, data 1 
	bra	Next_bit	; done sending, return via Next_bit

Sendzero:			; send a 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	bra	Next_bit	; done sending, return via Next_bit

;				*****************************************
;				* Send command to read at address in d1	*
;				*****************************************
READcmnd:			; send read Opcode sequence of 0, 1, 1, 0
				; init state
	move.l	#1,eeprom	; clock low, CS*, data 0
	move.l	#0,eeprom	; clock low, CS, data 0

				; start bit
	move.l	#4,eeprom	; clock low, CS, data 1
	move.l	#6,eeprom	; clock hi, CS, data 1
	move.l	#4,eeprom	; clock low, CS, data 1 
				; make OP code of 10
	move.l	#4,eeprom	; clock low, CS, data 1
	move.l	#6,eeprom	; clock hi, CS, data 1
	move.l	#4,eeprom	; clock low, CS, data 1
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0

ADRS_TX:			; Transmit a 6-bit address from d1 MSB first
	move.w	#6,d2		; number of bits to TX
	lsl.w	#8,d1		; shift 6-bit address to top of register
	lsl.w	#2,d1		; 10 bit shift, in all

TX_bit:				; send a 0 or 1
	btst	#15,d1		; look at bit 15
	beq	Send_0		; yes, it was zero, so...
	bra	Send_1		; no, it was 1

Nextbit:
	lsl.w	#1,d1		; shift d1 to get next bit
	subq	#1,d2		; decrement bit counter
	cmp.w	#0,d2
	bne	TX_bit		; not done with word, loop
	rts			; done, return to main, with clock low

Send_1:				; send a 1
	move.l	#4,eeprom	; clock low, CS, data 1 
	move.l	#6,eeprom	; clock hi, CS, data 1
	move.l	#4,eeprom	; clock low, CS, data 1 
	bra	Nextbit		; done sending, return via Nextbit

Send_0:				; send a 0
	move.l	#0,eeprom	; clock low, CS, data 0
	move.l	#2,eeprom	; clock hi, CS, data 0
	move.l	#0,eeprom	; clock low, CS, data 0
	bra	Nextbit		; done sending, return via Nextbit

				; end of code
				; END OF FILE

	.bss
Eblokbeg:
	ds.b	$1000		; DRAM buffer
Eblokend:


	
