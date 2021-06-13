	PLEN	95
;**************************************************************************
; (C)1993 ATARI CORP.       SECRET & CONFIDENTIAL       ALL RIGHTS RESERVED
;
;
;                                gettoc.das
;
;       This program reads the CD directory as the previous 68k code
;       did.  The function is now performed on the DSP for higher bus priority.
;
;
;                                REVISION HISTORY
;
; REV.  DATE       BY            DESCRIPTION OF EDIT
; """"  """"       ""            """""""""""""""""""
; 0.01  30 Jun 94  DJS		Initial version
;
;
;
;**************************************************************************
	NOLIST
	.include	"jaguar.inc"
	.include	"gettoc.xdf"
	LIST
;
;
BUTCH		equ	$DFFF00		;Butch chip base register
;
;
BUTCHREG	REGEQU	R0		;-> base address of butch chip
DATAREG		REGEQU	R1		;-> Butch Data Reg (.w)
CTRLREG		REGEQU	R2		;-> Butch Control Reg (.l)
SUBRET		REGEQU	R3		;subroutine return addr
;
;  The following register assignments must be contiguous
;
TRKMIN		REGEQU	R4		;minimum track number, current session
TRKMAX		REGEQU	R5		;maximum track number, current session
SRUNOUTM	REGEQU	R6		;minute, start of run-out, this session
SRUNOUTS	REGEQU	R7		;second, start of run-out, this session
SRUNOUTF	REGEQU	R8		;frame, start of run-out, this session
;
SESSION		REGEQU	R9		;current session # (0..99)
SESSMAX		REGEQU	R10		;maximum session # (-1 if unknown)
ATRKMIN		REGEQU	R11		;minimum track, all sessions
ATRKMAX		REGEQU	R12		;maximum track, all sessions
;
MASKFF		REGEQU	R13	;=000000FF
TRKCOUNT	REGEQU	R15	;# of tracks left to process, this session
;
;
;
	ORG	D_RAM			; put this program at start of DSP SRAM
;
;
cpuirq:
	movei	#d_start,r0		; (interrupt 0  - DSP Control)
	jump	(r0)
	nop
	nop
	nop
	nop
i2sirq:
	movei	#halt_DSP,r29		; (interrupt 1 - I2S)
	jump	(r29)			; should never get here
	nop
	nop
	nop
	nop
;
;
;
;
d_start:
	movei	#BUTCH,BUTCHREG		;base address of BUTCH
	moveq	#$A,DATAREG
	add	BUTCHREG,DATAREG	;form data reg
	moveq	#$4,CTRLREG
	add	BUTCHREG,CTRLREG	;and CTRL reg
	movei	#$FF,MASKFF	;mask for argument to identifier

;
;   clear local TOC space
;
	movei	#LOCALTOC,R20		;let's clear the local TOC
	moveq	#0,R21
	movei	#256,R22
clearlp:
	store	R21,(R20)
	subq	#1,R22
	jr	NE,clearlp
	addq	#4,R20
;
;
	moveq	#0,ATRKMAX	;set global track max to 0
	move	ATRKMAX,ATRKMIN
	not	ATRKMIN		;set global track min to $FFFFFFFF

	moveq	#0,SESSION	;start with session 0
sessloop:
	movei	#$300,R20	;prepare read session TOC command
	moveq	#$1f,R24	;put in min return code
	addq	#1,R24		;R24 = $20 (minimum return opcode)
	movei	#DSA_TX,R29	;call DSA_TX
	move	R24,R25
	moveq	#5,R21		;we want 5 args
	addq	#4,R25		;R25 = $24 (maximum return opcode)

	move	PC,SUBRET	;return back here +6
	jump	(R29)
	or	SESSION,R20	;	
;
;
;
;  we return here...R22 points to return codes buffer
;
;   Load up TRKMIN,TRKMAX,SRUNOUTM,SRUNOUTS, & SRUNOUTF
;  with self modifying code
;
sess1:
	load	(R22),R20	;check for error return (session doesn't exist)
	moveq	#4,R23
	shrq	#8,R20
	cmp	R20,R23
	jr	NE,sessgoo
	nop
	movei	#endsess,R29
	jump	(R29)
	nop
sessgoo:
	movei	#$20,R20	;1st data identifier we expect
	movei	#selfmod,R25	;cute register increment
	moveq	#1,R26	
	moveq	#20,R29
	movei	#sessloop,R30
	add	R22,R29		;end of RETBUF +1
	btst	#1,R25
	jr	NE,sess2
	bclr	#1,R25
	shlq	#16,R26
sess2:	
	load	(R25),R27
sess3:
	load	(R22),R23
	move	R23,R24
	and	MASKFF,R23
	shrq	#8,R24
	addq	#4,R22
	cmp	R20,R24
	addqt	#1,R20
	jr	EQ,selfmod
	load	(R25),R28
;
	jump	(R30)
	store	R27,(R25)	;restore self modified code to vanilla
;
selfmod:
	move	R23,TRKMIN
	add	R26,R28
	cmp	R29,R22
	jr	CS,sess3
	store	R28,(R25)
;
;
;
;   TRKMIN,TRKMAX,SRUNOUTM,SRUNOUTS, & SRUNOUTF now loaded
;
;  lets pack leadout times
;
	shlq	#8,SRUNOUTS
	shlq	#16,SRUNOUTM
	or	SRUNOUTS,SRUNOUTF
	or	SRUNOUTM,SRUNOUTF	;SRUNOUTF is packed lead-out time
;
;     adjust global min and max for new session values..
;
	move	TRKMAX,TRKCOUNT
;
	cmp	TRKMAX,ATRKMAX
	jr	CC,sess4
	store	R27,(R25)	;restore self mod to pristine
;
	move	TRKMAX,ATRKMAX
sess4:
	cmp	ATRKMIN,TRKMIN
	jr	CC,sess5
	nop
	move	TRKMIN,ATRKMIN
;
;   Global min & max now set
;
sess5:
	sub	TRKMIN,TRKCOUNT	;we need this many tracks before done
;
	movei	#$1400,R20	;prepare full TOC command
	or	SESSION,R20	;	
	storew	R20,(DATAREG)	;ask for the works, this session
;
	moveq	#0,R16		;current track index
	movei	#LOCALTOC,R14	;	
	movei	#$60,R23	;track ident
;
;	movei	#40,R17		;max DSA words after a track until we give up
	movei	#400,R17	;max DSA words after a track until we give up
;
	move	R17,R29
	movei	#tocloop,R28
tocloop:
	subq	#1,R29
	jr	NE,sess6
	load	(BUTCHREG),R20
;
	movei	#postsess,R29
	jump	(R29)
	or	R20,R20		;make sure R20 is safe after load above
sess6:
	btst	#13,R20
	jr	NE,gotdat
	nop

	moveq	#20,R30
;
	shlq	#5,R30
deLAY:
	jr	NE,deLAY
	subq	#1,R30
;
	jr	sess6
	load	(BUTCHREG),R20	
gotdat:
;
;	btst	#13,R20		;wait for next data
;	jr	EQ,sess6	;wait until data is ready
;	load	(BUTCHREG),R20
;
	loadw	(DATAREG),R22	;fetch next DSA data
;
	move	R22,R24
	shrq	#8,R22	
	load	(CTRLREG),R20	;clear interrupt flag	
	sub	R23,R22		;see if its a track
	jr	NE,nottrack
	and	MASKFF,R24	;mask for argument
;
;  it's a track
;
	cmp	TRKMIN,R24
	jr	CS,bumtrack
	moveq	#0,R16
;
	cmp	R24,TRKMAX
	jr	CS,bumtrack
	nop
;
	move	R17,R29		;reload time-out
	move	R24,R16		;current track index
	shlq	#3,R16		;*8 for index
;
	move	R24,R25		;save track #
	shlq	#24,R25		;place session in MSByte position SSxxxxxx
;
bumtrack:
	jump	(R28)		;loop around to "tocloop"
;
;
;
nottrack:
	subq	#2,R22
	jump	MI,(R28)
	moveq	#3,R20
;
	cmp	R20,R22
	jump	CC,(r28)
;
;  R22= 0-minutes, 1-seconds, 2-frames
;
	shlq	#3,R22		;put data in 2nd MSbyte (xxDDxxxx)
	rorq	#16,R24
	ror	R22,R24
	btst	#4,R22		;check for frames
	jump	EQ,(R28)
	or	R24,R25
;
	or	R16,R16		;invalid TOC index?
	jump	EQ,(R28)	;br if so--can't save this
	load	(R14+R16),R20	;have we stuffed here before?
	or	R20,R20
	jump	NE,(R28)
	nop
;
	store	R25,(R14+R16)	;save TrMiSeFr (Track,Minute,Second,Frame)
	moveq	#0,R16		;invalidate TOC index
gtest:
	subq	#1,TRKCOUNT
	jump	PL,(R28)
	nop
;
;  We got this session's track start times, now compute durations
;
sinfoin:
;
	move	MASKFF,SRUNOUTM
	not	SRUNOUTM	;we need this mask too
	moveq	#0,SRUNOUTS	;use this for CY
;
	move	TRKMIN,R20
	movei	#(LOCALTOC+4),R29
	shlq	#3,R20
	add	R20,R29		;here's where we put session,duration info
;
	move	TRKMIN,TRKCOUNT
durloop:
	move	TRKCOUNT,R20	;copy current track #
	addq	#1,TRKCOUNT
	move	TRKCOUNT,R21
	cmp	R21,TRKMAX	;have we gone beyond?
	jr	CC,notlastd	;br if not
	shlq	#3,R20		;form index
;
;  we've gone beyond the end---use session lead-out for duration
;
	jr	gendtime
	move	SRUNOUTF,R21
;
notlastd:
	shlq	#3,R21		;index for track +1 too
	load	(R14+R21),R21	
gendtime:
	load	(R14+R20),R20
	move	R20,R25
	move	R21,R26
	and	MASKFF,R25
	and	MASKFF,R26
;
	and	SRUNOUTM,R21	;clear LSByte for the result
;
	sub	R25,R26		;subtract early Frames from late Frames
	jr	CC,fclear
	nop
;
	movei	#75,R27
	add	R27,R26		;this will set CY
fclear:
	addc	SRUNOUTS,SRUNOUTS	;preserve CY
	or	R26,R21
	rorq	#8,R20
	rorq	#8,R21
;
	move	R20,R25
	move	R21,R26
	and	MASKFF,R25
	and	MASKFF,R26
;
	and	SRUNOUTM,R21	;clear LSByte for the result
;
	shrq	#1,SRUNOUTS	;release CY
	subc	R25,R26		;subtract early Frames from late Frames
	jr	CC,sclear
	nop
;
	movei	#60,R27
	add	R27,R26		;this will set CY
sclear:
	addc	SRUNOUTS,SRUNOUTS	;preserve CY
	or	R26,R21
	rorq	#8,R20
	rorq	#8,R21
;
	move	R20,R25
	move	R21,R26
	and	MASKFF,R25
	and	MASKFF,R26
;
	and	SRUNOUTM,R21	;clear LSByte for the result
;
	shrq	#1,SRUNOUTS	;release CY
	subc	R25,R26		;subtract early Frames from late Frames
	or	R26,R21
	rorq	#8,R21		;MMSSFFxx
	and	SRUNOUTM,R21	;clear out track# remnant
	or	SESSION,R21
	rorq	#8,R21		;pack the session in too
;
	cmp	TRKCOUNT,TRKMAX	;have we gone beyond?
	jr	CS,nextsess	;br if not
	store	R21,(R29)	;save next session/duration
;
	movei	#durloop,R20	;loop for next
	jump	(R20)
	addq	#8,R29		;advance TOC ptr
;
;
;  Finished this session, check on the next...
;
nextsess:
	addq	#1,SESSION
;
	movei	#sessloop,R29
	jump	(R29)		;else, go for next session
	nop
;
endsess:
	rorq	#24,SRUNOUTF	;last session's run out: MMSSFFxx
	rorq	#24,ATRKMIN
	movei	#LOCALTOC,R20	;stuff some final info in 1st TOC record
	or	ATRKMAX,ATRKMIN
	or	SESSION,SRUNOUTF
	store	ATRKMIN,(R20)
	rorq	#8,SRUNOUTF
	addq	#4,R20
	store	SRUNOUTF,(R20)
;
;  Now copy complete TOC to dRAM
;
copyTOC:
	movei	#$2c00,R20
	movei	#LOCALTOC,R21
	movei	#$100,R22
	move	R21,R25		;save LOCALTOC ptr in R25
	addq	#4,R25		;we will want session count @ LOCALTOC+4
;
;
coploop:
	load	(R21),R23
	rorq	#16,R23
	storew	R23,(R20)
	rorq	#16,R23
	addq	#2,R20
	addq	#4,R21
	storew	R23,(R20)
	subq	#1,R22
	jr	NE,coploop
	addq	#2,R20
;
;  let's set-up audio CD here if we are a single session CD...
;
	.if	0
	load	(R25),R26	;fetch session count, etc
	shrq	#24,R26		;how many sessions here?
	cmpq	#1,R26		;check for 1 session (audio CD)
;
;	jr	EQ,audset	;br if do audio
;	moveq	#$17,R24	;put in min return code for audio
;
;
;  else, we'd like to set single speed/audio mode
;
	.endif
;
halt_DSP:
	moveq	#0,r25
	movei	#D_RAM,R27
	store	r25,(r27)
	movei	#D_CTRL,r26	; and halt it
verydead:			; if the DSP is not stopped now
	store	r25,(r26)	; try turning the power off
	nop
	nop
	nop
	jr	verydead
	nop
;
;  We get here if timed-out..
;
postsess:
	movei	#LOCALTOC,R21
	moveq	#0,R20
	load	(R21),R22
	not	R20
	shlq	#16,R20
	or	R20,R22
	movei	#copyTOC,R30
	jump	(R30)
	store	R22,(R21)
;
;
;
	.if	0
audset:
	movei	#$1501,R20	;prepare SetMode command 
	movei	#DSA_TX,R29	;call DSA_TX
	moveq	#1,R21		;# of return words
	move	PC,SUBRET	;return back here +6
	jump	(R29)
	move	R24,R25		;R25 is max return code (same as min
;
	movei	#$200,R20	;stop spindle if audio
	movei	#DSA_TX,R29	;call DSA_TX
	moveq	#0,R21		;# of return words
	moveq	#2,R24
;	movei	#(halt_DSP-6),SUBRET	;return back here +6
	move	PC,SUBRET
	jump	(R29)
	move	R24,R25		;R25 is max return code (same as min
;
	load	(BUTCHREG),R23
	or	R23,R23	
	movei	#halt_DSP,R29
	jump	(R29)
	nop
	.endif
;
;
;  R20 = command code to send
;  R21 = # of return words expected
;  R22 -> internal buffer for returned words (extended to longs)
;  R24 = LSbyte is set to expected return opcode min
;  R25 = LSbyte is set to expected return opcode max
;
;  R23,R26 R27 R28 trashed 
;
DSA_TX:
	addq	#6,SUBRET
	movei	#RETBUF,R22	;set to our buffer
;
	load	(BUTCHREG),R23	;check receive buffer full
	btst	#13,R23		;
	jr	EQ,tx_rdy	;br if receive is clear
	nop
	loadw	(DATAREG),R23	;flush receive with dummy fetch
	load	(CTRLREG),R23	;clear interrupt flag
;
tx_rdy:
	storew	R20,(DATAREG)	;stuff command
;
	subq	#1,R21		;check for Null return
	jr	PL,tx_loop
	moveq	#0,R23
;
	movei	#RETBUF,R22	;set to our buffer
	jump	(SUBRET)	;now return to caller
	nop
;
tx_loop:
;	btst	#13,R23		;any received?
;	jr	EQ,tx_loop	;wait til received
;	load	(BUTCHREG),R23	;check receive buffer full
;
	btst	#13,R23
	jr	NE,gotDAT
	nop

;	nop
;	nop
;	nop
;	nop
;	or	R23,R23
;
;
;
	moveq	#20,R30
;
	shlq	#5,R30
DElay:
	jr	NE,DElay
	subq	#1,R30
;
tx_loopx:
	jr	tx_loop
	load	(BUTCHREG),R23	
gotDAT:

	loadw	(DATAREG),R26	;pick up return code
	load	(CTRLREG),R29	;clear interrupt flag
	move	R26,R23
	shrq	#8,R23
;
	cmp	R24,R23		;check agains min
	jr	CS,tx_err	;br if less than min
	cmp	R23,R25
	jr	CS,tx_err	;br if greater than max
	nop
;
	store	R26,(R22)	;save in internal
	addq	#4,R22
tx_ret:
	subq	#1,R21
	jr	PL,tx_loopx	
	nop
;
	movei	#RETBUF,R22	;set to our buffer
	jump	(SUBRET)	;now return to caller
	nop
;
tx_err:
	moveq	#4,R27
	cmp	R23,R27		;did we get error code?
	jr	EQ,tx_errx	;br if so
	nop
;
tx_wait:
;	jr	tx_wait
;	nop
	movei	#tx_loop,R27
	jump	(R27)
	nop
tx_errx:
	movei	#RETBUF,R22	;set to our buffer
	store	R26,(R22)
	jump	(SUBRET)	;now return to caller
	nop
;
;
;
	.align	long
RETBUF:
	ds.l	6		;return words (extended to longs)
LOCALTOC:
	ds.l	1		;local copy of our CD directoy

	end                                        

